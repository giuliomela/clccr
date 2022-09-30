## code to prepare `DATASET` dataset goes here

# loading libraries

pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate)

# loading measurement units of IMF prices and performing measurement unit conversion

um_imf <- read_xlsx(here("data-raw/um.xlsx"), sheet = "imf")[, c("imf_code", "um")]

um_p <- read_xlsx(here("data-raw/um.xlsx"), sheet = "p") # oading measurement unit conversion factors for price data

# defining reference year and horizon length

ref_yr <- 2021

h <- 10

# downloading GDP deflators (Euro area for Eurostat and Comext data and US for Comtrade and USITC data)

gdp_defl_raw <- rdb(c("OECD/MEI/EA19.NAGIGP01.IXOBSA.A",
      "OECD/MEI/USA.NAGIGP01.IXOBSA.A"))

gdp_defl <- gdp_defl_raw[, c("original_period", "LOCATION", "value")]

names(gdp_defl) <- c("year", "cur", "defl")

gdp_defl$year <- as.numeric(gdp_defl$year)

gdp_defl$cur <- ifelse(gdp_defl$cur == "USA", "usd", "eur")


# downloading USD-EUR exchange rate from the European Central Bank database

exc_rate_raw <- rdb("ECB/EXR/A.USD.EUR.SP00.A")

exc_rate <- exc_rate_raw[, c("original_period", "value")]

names(exc_rate) <- c("year", "exc_rate")

exc_rate$year <- as.numeric(exc_rate$year)

# loading master file

master_data <- read_excel(here("data-raw/db_comm_master_2022.xlsx"))

#### downloading comtrade data ####

comtrade_codes <- subset(master_data, !is.na(comtrade_code))$comtrade_code

comtrade_codes <- unique(as.character(comtrade_codes)) # list of all comtrade codes

comtrade_codes_l <- split(comtrade_codes, ceiling(seq_along(comtrade_codes)/4))

years_l <- list(c(start_yr = ref_yr - h + 1, end_yr = ref_yr - h/2),
                c(start_yr = ref_yr - h/2 + 1, end_yr = ref_yr))

if (ct_get_remaining_hourly_queries() < length(comtrade_codes_l) * length(years_l)) stop("Superato limite massimo di query orarie")

# defining a function to download comtrade data without infringing API limits

comtrade_raw <- NULL

for (i in 1:length(comtrade_codes_l)) {

  for (j in 1:length(years_l)) {

  comtrade_raw[[paste0(i, "_", j)]] <- ct_search(
    reporters = "All",
    partners = "World",
    trade_direction = "export",
    start_date = years_l[[j]][["start_yr"]],
    end_date = years_l[[j]][["end_yr"]],
    commod_codes = comtrade_codes_l[[i]]
  )

  Sys.sleep(5)

  }

  Sys.sleep(5)

}


# joining databases

comtrade_raw <- do.call(rbind, comtrade_raw)

# tidying data

trade_data_tidy <- subset(comtrade_raw,  # removes flows of less than 100 kg
                          select = c(year, reporter_iso, partner_iso, commodity_code,
                                     commodity, netweight_kg, trade_value_usd)) %>%
  as_tibble()

if (!setequal(unique(trade_data_tidy$commodity_code), comtrade_codes)) stop ("The Comtrade query did not return all selected codes")


# removing rows with missing values (either quantity or value)

# trade_data_tidy <- trade_data_tidy[!with(trade_data_tidy,
#                       is.na(netweight_kg) | is.na(trade_value_usd)), ]

# Calculating unit values

price_comtrade_def <- trade_data_tidy %>%
  group_by(year, commodity_code) %>%
  summarize(across(c(netweight_kg, trade_value_usd), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(price = trade_value_usd / netweight_kg) %>%
  select(year, code = commodity_code, price)

# Adding missing rows (some commodities might not have values for every year)

uv_grid <- expand_grid(year = seq((ref_yr - h + 1), ref_yr, 1),
                              code = unlist(comtrade_codes_l))


price_comtrade_def <- uv_grid %>%
  left_join(price_comtrade_def) %>%
  mutate(source = "comtrade",
         cur = "usd")

#### Downloading data from the IMF commodity price database using the rdbnomics database####

codes_imf <- subset(master_data, source == "imf")$imf_code %>%
  unique()

codes_imf <- paste0("IMF/PCPS/A.W00.", codes_imf, ".USD") # creating codes compatible with the rdbnomics package

price_imf_raw <- rdb(codes_imf) # prices in raw data form

str(price_imf_raw)

price_imf_raw <- price_imf_raw[, c("original_period", "COMMODITY", "value")]

names(price_imf_raw) <- c("year", "code", "price_usd")

# preparing the final tibble (mu conversion)

price_imf_def <- price_imf_raw %>%
  left_join(um_imf,
            by = c("code" = "imf_code")) %>%
  rename(um_from = um) %>%
  left_join(um_p) # adding original measurement units to price series in USD

price_imf_def <- price_imf_def %>%
  mutate(price = price_usd / fct,
         year = as.numeric(year),
         source = "imf",
         cur = "usd") %>%
  select(!c(um_from, um_to, fct, price_usd))

#### Downloading electric energy prices from Eurostat  - THESE PRICES ARE NOT USED IN THE 202 VERSION OF THE INDICATOR####

# Downloading prices for non-household electricity consumption at various annual consumption levels

# eurostat_indicators <- rdb_dimensions(provider_code = "Eurostat", dataset_code = "nrg_pc_205")
#
# cons_band_codes <- eurostat_indicators$Eurostat$nrg_pc_205$consom$consom # codes to retrieve data for different consumption bands
#
# cons_band_eurostat <- eurostat_indicators$Eurostat$nrg_pc_205$consom # data table with consumption bands codes and labels
#
# cons_band_eurostat <- add_column(cons_band_eurostat, min_band = c(0, 20, 500, 2000, 20000, 70000, 150000),
#            max_band = c(19, 499, 1999, 19999, 69999, 149999, Inf)) # consumption bands legend
#
# # building codes for extraction
# cons_band_codes <- paste0("Eurostat/nrg_pc_205/S.6000.KWH.", cons_band_codes, ".X_TAX.EUR.EU27_2020")
#
# # retrieving data
#
# price_eurostat_raw <- rdb(cons_band_codes)
#
# price_eurostat <- price_eurostat_raw[, c("period", "consom", "Consumption", "value")]
#
# price_eurostat$Consumption <- str_sub(price_eurostat$Consumption, 1, 7)
#
# price_eurostat$period <- lubridate::year(price_eurostat$period)
#
# names(price_eurostat) <- c("year", "cons_code", "cons_label", "price")
#
# price_eurostat_def <- price_eurostat %>%
#   group_by(year, cons_code, cons_label) %>%
#   summarise(price = mean(price)) %>%
#   ungroup() %>%
#   mutate(um_from = "kwh") %>%
#   left_join(um_p) %>%
#   mutate(price = price / fct,
#          fct = NULL,
#          cur = "eur",
#          um_from = NULL,
#          um_to = NULL) # tibble with all average yearly prices by consumption band

#### Loading Comext data ####

comext <- read_delim(here("data-raw/comext.csv"))

# selecting imports and flows regarding the EU as a whole

comext_raw <- dplyr::rename_with(comext, tolower) # variable names in lowercase

comext_raw <- subset(comext_raw, flow == 1 &
                           reporter == "EU" & partner == "EU_EXTRA") # selecting imports

comext_raw <- within(comext_raw, {
  year <- as.numeric(stringr::str_sub(as.character(period), 1, 4))
  period <- NULL
})

# Comext prices expressed in euro/kg (current)

comext_raw <- comext_raw[, c("product", "indicators",
                                     "indicator_value", "year")]

comext_raw <- subset(comext_raw, year > ref_yr - 10)

comext_raw <- tidyr::pivot_wider(comext_raw,
                                     names_from = indicators,
                                     values_from = indicator_value)

price_comext_def <- comext_raw %>%
  mutate(price = VALUE_IN_EUROS / QUANTITY_IN_100KG / 100,
         code = as.character(product),
         source = "comext",
         cur = "eur",
         VALUE_IN_EUROS = NULL,
         QUANTITY_IN_100KG = NULL,
         product = NULL
  )

#### Loading USITC data ####

# data retrieved manually from https://dataweb.usitc.gov/

usitc_raw <- read_xlsx(here("data-raw/usitc.xlsx"), sheet = 2, skip = 2)

usitc_raw <- usitc_raw %>%
  mutate(`HTS Number` = str_remove_all(`HTS Number`, "\\."),
         across(starts_with("Year"), ~ as.numeric(str_replace(.x, "N/A", "NA")))) %>% # coverting values in numeric
  pivot_longer(!`Data Type`:`Quantity Description`, names_to = "year",
               values_to = "price") %>%
  mutate(year = as.numeric(str_remove(year, "Year "))) %>%
  select(year,
         code = `HTS Number`,
         um_from = `Quantity Description`,
         price)

# renaming measurement units

usitc_raw <- usitc_raw %>%
  mutate(um_from = case_when(
    um_from == "metric tons" ~ "t",
    um_from == "kilograms" ~ "kg",
    um_from == "component kilograms" ~ "kg",
    um_from == "component grams" ~ "g"
  ))

# converting measurement units

usitc_raw <- usitc_raw %>%
  left_join(um_p) %>%
  mutate(price = price / fct,
         fct = NULL,
         um_from = NULL,
         um_to = NULL)

price_usitc_def <- usitc_raw %>%
  mutate(source = "usitc",
         cur = "usd")

#### CREATING AN UNIQUE DATABASE OF REFERENCE PRICES ####

prices_all <- bind_rows(price_comtrade_def, price_comext_def,
          price_imf_def, price_usitc_def) %>%
  as_tibble()

# adjusting for inflation

prices_all <- prices_all %>%
  left_join(gdp_defl) %>%
  group_by(code, source) %>%
  mutate(price_k = price / defl * defl[year == ref_yr]) %>%
  ungroup()

# converting price into euros

exc_rate_ref <- subset(exc_rate, year == ref_yr)$exc_rate

prices_all <- prices_all %>%
  mutate(price_eur = if_else(cur == "usd",
                             price_k / exc_rate_ref,
                             price_k)) %>%
  select(!c(price, price_k, cur, defl))

# Computing average and minimum and maximum prices of the last 10 years

prices_all <- prices_all %>%
  filter(year > ref_yr - h)

ref_prices <-  prices_all %>%
  group_by(code, source) %>%
  summarise(mean = mean(price_eur, na.rm = T),
            min = min(price_eur, na.rm = T),
            max = max(price_eur, na.rm = T),
            n_obs = sum(!is.na(price_eur)) # number of years over which the average is actually computed
          ) %>%
  ungroup()

#### PREPRARING FINAL TIBBLE WITH COMMODITY NAMES AND REFERENCE PRICES

# manipulating the comm_key tibble to make it tidy and ready to be joined with prices

comm_key_tidy <- mutate(master_data,
                               across(c(comtrade_code,
                                               comext_code, usitc_code), as.character))

comm_key_tidy$quandl_code <- NULL


comm_key_tidy <- tidyr::pivot_longer(comm_key_tidy, tidyselect::ends_with("code"),
                                     names_to = "source_code", values_to = "code",
                                     names_pattern = "(.*)_code")

comm_key_tidy <- within(comm_key_tidy, {
  source_code <- ifelse(source == "none", "none", source_code)
})

comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))

comm_key_tidy$source_code <- NULL

# Joining price_ref and comm_key tibbles to get the final dataset to be used in the analysis

clcc_prices_ref <- comm_key_tidy%>%
  left_join(ref_prices)

clcc_prices_ref <- clcc_prices_ref %>%
  mutate(across(mean:max, ~ if_else(is.na(.x), 0, .x)),
         ref_yr = ref_yr)

# checking if the call retrieved data for all years (comtrade data only)

check_comtrade_l <- clcc_prices_ref %>%
  filter(source == "comtrade") %>%
  group_by(no_comm) %>%
  group_split()

length_check <- sapply(check_comtrade_l, function (x) x[["n_obs"]] != h)

if (sum(length_check != 0)) stop ("The comtrade query did not return data for all years selected for at least one commodity")


usethis::use_data(clcc_prices_ref, overwrite = TRUE)
