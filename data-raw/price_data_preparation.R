# --- Code to prepare internal and external package datasets ---

# Loading required architecture libraries
library(readr)
library(tidyverse)
library(dplyr)
library(comtradr)
library(readxl)
library(rdbnomics)
library(openxlsx)
library(sbtools)
library(stringi)
library(here)
library(fredr)

# --- Configuration & Setup ---
ref_yr <- 2026
h <- 10        # Horizon length for historical analysis
price_version_comparison <- FALSE # Set to TRUE if you want to perform year-over-year comparison
download_fresh_data <- TRUE #set to 'TRUE' to download latest data

# Ensure API keys or environment variables are loaded if necessary
# comtradr key is handled inside its own retrieval function now

# Loading master metadata file
master_data_path <- here("data-raw",
                         paste0("db_comm_master_", ref_yr, ".xlsx"))

master_data <- read_excel(master_data_path)

# Loading auxiliary configuration parameters
um_imf <- read_xlsx(here("data-raw/um.xlsx"), sheet = "imf")[, c("imf_code", "um")]
um_p   <- read_xlsx(here("data-raw/um.xlsx"), sheet = "p") # Measurement unit conversion factors

# --- 1. Sourcing Isolated Retrieval Modules ---
functions_path <- here("data-raw", "functions")

functions <- list.files(functions_path, full.names = TRUE)

walk(
  functions,
  \(x) source(x)
)

# Creting a local cache folder if missing

cache_dir <- here("data-raw", "cache_raw_data")

if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# --- 2. Running Workers to Fetch Raw/Defensive Prices ---
if(isTRUE(download_fresh_data)) {


message("Executing modular data extraction workers...")

# USGS Module (returns a structured list, tracking prices)
usgs_output <- get_usgs_prices(ref_yr = ref_yr,
                                 output_dir = here::here("data-raw", "usgs"),
                                 historical_file = "usgs_historical.xlsx",
                                 usgs_corr_table = "usgs_corr_table.xlsx",
                                 latest_wollastonite = NA_real_,
                                 usgs_um_legend = "usgs_um.xlsx",
                                 verbose = TRUE,
                                 save_logs = TRUE)

price_usgs_def <- usgs_output$prices |>
  select(year, code, price, source, cur)



# IMF, Comtrade, and Comext Modules

comtrade_ref_yr <- ref_yr - 1

price_imf_def      <- get_imf_prices(master_data = master_data_path)
price_comtrade_def <- get_comtrade_prices(master_data = master_data_path,
                                          ref_yr = comtrade_ref_yr, horizon = h) # ref_yr in this case is the year before the actual ref_yr
price_comext_def   <- get_comext_prices(ref_yr = ref_yr, horizon = h)

# Saving data in the cache folder

save(price_usgs_def, price_imf_def, price_comtrade_def, price_comext_def,
     file = file.path(cache_dir, paste0("raw_prices_snapshot_", ref_yr, ".RData")))

} else {

  message("🟢 Offline Cache Mode Active: Loading last session raw snapshots...")
  cache_file <- file.path(cache_dir, paste0("raw_prices_snapshot_", ref_yr, ".RData"))

  if (!file.exists(cache_file)) {
    stop("Critical error: No local cache file found. You must set download_fresh_data <- TRUE at least once.")
  }
  load(cache_file)

}

# --- 3. Downloading Macroeconomic Deflators & Exchange Rates ---
message("Fetching GDP deflators and exchange rates...")

# Downloading US GDP deflator from FRED (for USD denominated sources: Comtrade, IMF, USGS)
gdp_defl_raw_usd <- fredr(series_id = "GDPDEF") |>
  mutate(year = year(date), cur = "usd") |>
  group_by(year, cur) |>
  summarise(defl = mean(value), .groups = "drop")

# Downloading Euro Area GDP deflator from Eurostat (for EUR denominated sources: Comext)
gdp_defl_raw_eur <- eurostat::get_eurostat(
  "nama_10_gdp",
  filters = list(GEO = "EA", NA_ITEM = "B1GQ", UNIT = "PD20_EUR")
) |>
  mutate(
    year = year(time),
    cur = "eur",
    defl = values
  ) |>
  select(year, cur, defl) |>
  drop_na()

# Combining deflators into a unified look-up reference table
gdp_defl <- bind_rows(gdp_defl_raw_usd, gdp_defl_raw_eur)

# Downloading USD-EUR exchange rate from the European Central Bank (ECB) via DBnomics
exc_rate_raw <- rdb("ECB/EXR/A.USD.EUR.SP00.A")
exc_rate <- exc_rate_raw[, c("original_period", "value")]
names(exc_rate) <- c("year", "exc_rate")
exc_rate$year <- as.numeric(exc_rate$year)

common_max_yr <- intersect(gdp_defl$year, exc_rate$year) |> max() # Finding the lastest common year between deflator and exchange rate

# Extracting the reference year exchange rate (in any case the last available year) for currency conversions
exc_rate_ref <- exc_rate |> filter(year == common_max_yr) |> pull(exc_rate)

# --- 4. Central Consolidation & Currency/Inflation Adjustments ---
message("Consolidating datasets and applying currency conversions (USD -> EUR)...")

# Bind all separate sources together
prices_all <- bind_rows(price_comtrade_def, price_comext_def,
                        price_imf_def, price_usgs_def) |>
  as_tibble() |>
  filter(year >= (max(year) - 10) & year <= max(year)) #keeping an extra year to be able to compute previous' year average prices

if (max(prices_all$year) > max(gdp_defl$year))
  stop("Critical: GDP deflator data do not cover the latest year of available price data. Please verify FRED/Eurostat updates.")

# Complete the time series matrix grid to handle potential missing combinations
prices_grid <- expand_grid(
  year = unique(prices_all$year),
  unique(select(prices_all, code, source, cur))
)

# Adjusting for inflation (constant reference year prices) and converting USD to EUR
prices_all <- prices_grid |>
  left_join(prices_all, by = c("year", "code", "source", "cur")) |>
  left_join(gdp_defl, by = c("year", "cur")) |>
  group_by(code, source, cur) |>
    mutate(
    defl_safe = if_else(is.na(defl), defl[year == common_max_yr], defl),
    price_k   = price / defl_safe * defl[year == common_max_yr]
  ) |>
  ungroup() |>
  mutate(price_eur = if_else(cur == "usd", price_k / exc_rate_ref, price_k)) |>
  select(!c(price, price_k, defl, defl_safe, cur))

message(paste0(
  "Prices are expressed at ", common_max_yr, " price levels, and converted into euros using the ",
  common_max_yr, " official USD/EUR exchange rate provided by the European Central Bank"
))

# Computing historical summary metrics over the selected horizon (10 years)
ref_prices <- prices_all |>
  filter(year >= (max(year) - 9) & year <= max(year)) |>
  group_by(code, source) |>
  summarise(
    mean  = mean(price_eur, na.rm = TRUE),
    min   = min(price_eur, na.rm = TRUE),
    max   = max(price_eur, na.rm = TRUE),
    n_obs = sum(!is.na(price_eur)), # Number of years validating the sample average
    .groups = "drop"
  )

# Computing previous year's averages


prices_last_year <- prices_all |>
  filter(year >= (max(year) - 10) & year <= max(year) - 1) |>
  group_by(code, source) |>
  summarise(
    mean_previous_year  = mean(price_eur, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(code, source, mean_previous_year)


ref_prices <- ref_prices |>
  left_join(prices_last_year, by = c("code", "source"))

# --- 5. Tidying Commodity Keys and Mapping Configurations ---
message("Mapping processed prices back to master layout structure...")

comm_key_tidy <- master_data |>
  mutate(across(c(comtrade_code, comext_code, usitc_code), as.character)) |>
  select(-any_of("quandl_code")) |>
  pivot_longer(
    cols = ends_with("code"),
    names_to = "source_code",
    values_to = "code",
    names_pattern = "(.*)_code"
  ) |>
  mutate(
    source_code = if_else(source == "none", "none", source_code),
    code = if_else(source == "none", NA_character_, code)
  ) |>
  unique()

comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))
comm_key_tidy$source_code <- NULL

# Merging reference summaries into final structured package matrix dataset
clcc_prices_ref <- comm_key_tidy |>
  left_join(ref_prices, by = c("code", "source")) |>
  mutate(
    across(mean:max, ~ if_else(is.na(.x), 0, .x)),
    update_yr = ref_yr,
    defl_year = common_max_yr
  )

# Saving price file for next year's comparison

saveRDS(clcc_prices_ref,
        here("data-raw", "old_data_functions",
             paste0("clcc_prices_ref_", ref_yr, ".rds")))

# --- 6. Post-Run Quality Assurance Validation ---
check_comtrade_failures <- clcc_prices_ref |>
  filter(source == "comtrade" & n_obs != h)

if (nrow(check_comtrade_failures) > 0) {
  stop("Quality Check Failed: The Comtrade queries did not return continuous data timelines for all commodities.")
}

# --- 7. Year-Over-Year Price Evaluation Logs (Optional Workbook) ---
if (isTRUE(price_version_comparison)) {
  message("Generating price version comparison sheets...")

  last_year_prices <- paste0("clcc_prices_ref_", ref_yr - 1)

  prices_previous_version <- readRDS(here("data-raw", "old_data_functions", paste0(last_year_prices, ".rds")))

  if (isFALSE(identical(prices_previous_version$comm, clcc_prices_ref$comm))) {
    stop("Mismatch detected between historical commodity index mappings and current matrices.")
  }

  price_changes <- map(c("mean", "min", "max"), \(x) {
    new <- clcc_prices_ref[, c("comm", x)]
    old <- prices_previous_version[, c("comm", x)]
    names(old)[2] <- paste0(x, "_old")

    change <- new |> left_join(old, by = "comm")
    change[["change"]] <- ifelse(
      change[[x]] == change[[paste0(x, "_old")]], 0,
      change[[x]] / change[[paste0(x, "_old")]] * 100 - 100
    )
    return(arrange(change, desc(change)))
  }) |> setNames(c("mean", "min", "max"))

  wb <- createWorkbook()
  for (name in names(price_changes)) {
    addWorksheet(wb, name)
    writeData(wb, sheet = name, price_changes[[name]])
  }
  saveWorkbook(wb, here("data-raw",
                        "price_comparisons",
                        paste0("price_comparison_", ref_yr, ".xlsx")), overwrite = TRUE)
}

# --- 8. Processing SimaPro Template Dependencies ---
message("Loading SimaPro infrastructure metadata templates...")

simapro_codes <- read_excel(here(paste0("data-raw/db_comm_master_", ref_yr, ".xlsx")), sheet = "simapro_codes") |>
  mutate(formula = noquote(formula))

template_names <- c("top", "mid_eu", "mid_iea", "mid2", "bottom", "gruppi_top", "gruppi_bottom",
                    "gruppi_critical_top", "gruppi_critical_eu_bottom", "gruppi_critical_iea_bottom")

simapro_template <- lapply(template_names, function(x) {
  read_delim(here(paste0("data-raw/simapro_template_", x, ".csv")), delim = ";", col_names = FALSE, show_col_types = FALSE)
}) |> setNames(template_names)

# --- 9. Writing Data Back into Package Environment Binary Storage ---
message("Writing updated elements to package internal environment storage...")

# Save public user-facing datasets
usethis::use_data(clcc_prices_ref, overwrite = TRUE)

# Save package internal environment datasets (hidden trackers)
usethis::use_data(simapro_template, simapro_codes, overwrite = TRUE, internal = TRUE)

if (isTRUE(download_fresh_data)) {

# Backup historical text artifacts generated during the current session execution
openxlsx::write.xlsx(
  list(historical = usgs_output$prices_raw),
  file = here("data-raw", "usgs", "usgs_historical_backup.xlsx"),
  overwrite = TRUE
)

}

message("Pipeline execution completed successfully. Package internal repositories are updated.")
