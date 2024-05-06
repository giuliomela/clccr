## code to prepare `DATASET` dataset goes here

# loading libraries

pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate, httr, censusapi, jsonlite)


# loading master file

master_data <- read_excel(here("data-raw/db_comm_master_2024.xlsx"))

# loading USGS prices

usgs_prices <- read_excel(here("data-raw/db_comm_master_2024.xlsx"),
                          sheet = "prices_USGS") %>%
  filter(!is.na(code)) %>%
  pivot_longer(
    !c(comm, description_original, um, source, code),
    names_to = "year",
    values_to = "price"
  )

# provisional - untill contradr will be restored, the untrader package is used to retrieve trade data

comtradr::set_primary_comtrade_key("2b9783161f3342eda6ec85e4af0487db")

# loading measurement units of IMF prices and performing measurement unit conversion

um_imf <- read_xlsx(here("data-raw/um.xlsx"), sheet = "imf")[, c("imf_code", "um")]

um_p <- read_xlsx(here("data-raw/um.xlsx"), sheet = "p") # loading measurement unit conversion factors for price data

# defining reference year and horizon length

ref_yr <- 2022

h <- 10

# downloading GDP deflators (Euro area for Eurostat and Comext data and US for Comtrade and USITC data)

gdp_defl_raw <- rdb(c("OECD/MEI/EA20.NAGIGP01.IXOBSA.A",
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

#### downloading comtrade data ####

comtrade_codes <- master_data %>%
  filter(source == "comtrade" & comm != "Spodumene") %>% # spodumene is treated separately (only AUS exports)
  pull(comtrade_code) %>%
  unique() # codes to download

comtrade_raw <- ct_get_data(
  flow_direction = "export",
  start_date = ref_yr - 9,
  end_date = ref_yr,
  commodity_code = comtrade_codes
)

# downloading spodumene data

spodumene_code <- master_data %>%
  filter(comm == "Spodumene") %>% # spodumene is treated separately (only AUS exports)
  pull(comtrade_code) %>%
  unique()

spodumene_raw <- ct_get_data(
  flow_direction = "export",
  start_date = ref_yr - 9,
  end_date = ref_yr,
  reporter = "AUS",
  commodity_code = spodumene_code
)

comtrade_raw <- bind_rows(
  comtrade_raw, spodumene_raw
) %>%
  mutate(qty = if_else(
    is.na(qty) | qty == 0,
    altQty,
    qty
  ))


# tidying data

trade_data_tidy <- subset(comtrade_raw,  # removes flows of less than 100 kg
                          select = c(refYear, reporterISO, partnerISO, cmdCode,
                                     cmdDesc, qty, fobvalue)) %>%
  as_tibble()

names(trade_data_tidy) <- c("year", "reporter_iso", "partner_iso", "commodity_code",
                            "commodity", "netweight_kg", "trade_value_usd")

if (!setequal(unique(trade_data_tidy$commodity_code),
              c(spodumene_code, comtrade_codes))) stop ("The Comtrade query did not return all selected codes")

# Calculating unit values

price_comtrade_def <- trade_data_tidy %>%
  group_by(year, commodity_code) %>%
  drop_na() %>%
  filter(netweight_kg != 0) %>%
  summarize(across(c(netweight_kg, trade_value_usd), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(price = trade_value_usd / netweight_kg) %>%
  select(year, code = commodity_code, price)

# Adding missing rows (some commodities might not have values for every year)

uv_grid <- expand_grid(year = seq((ref_yr - h + 1), ref_yr, 1),
                       code = as.character(c(spodumene_code, comtrade_codes)))


price_comtrade_def <- uv_grid %>%
  left_join(price_comtrade_def) %>%
  mutate(source = "comtrade",
         cur = "usd")

### USGS Prices

# Converting prices to USD/kg

price_usgs_def <- usgs_prices %>%
  left_join(um_p, by = c("um" = "um_from")) %>%
  mutate(price = price / fct,
         source = "usgs",
         cur = "usd",
         year = as.numeric(year),
         um = NULL,
         um_to = NULL,
         fct = NULL,
         description_original = NULL,
         comm = NULL)

#### Downloading data from the IMF commodity price database using the rdbnomics database####

codes_imf <- subset(master_data, source == "imf")$imf_code %>%
  unique()

codes_imf <- paste0("IMF/PCPS/A.W00.", codes_imf, ".USD") # creating codes compatible with the rdbnomics package

price_imf_raw <- rdb(codes_imf) # prices in raw data form

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

#### Loading Comext data ####

comext <- read_delim(here("data-raw/comext.csv"))

# selecting imports and flows regarding the EU as a whole

comext_raw <- dplyr::rename_with(comext, tolower) # variable names in lowercase

# comext_raw <- subset(comext_raw, flow == 1 &
#                            reporter == "EU" & partner == "EU_EXTRA") # selecting imports

comext_raw <- comext_raw %>%
  mutate(year = as.numeric(stringr::str_sub(as.character(period), 1, 4)),
         period = NULL)

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

#### CREATING AN UNIQUE DATABASE OF REFERENCE PRICES ####

prices_all <- bind_rows(price_comtrade_def, price_comext_def,
                        price_imf_def, price_usgs_def) %>%
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

#comm_key_tidy$none_code <- NA_character_


comm_key_tidy <- tidyr::pivot_longer(comm_key_tidy, tidyselect::ends_with("code"),
                                     names_to = "source_code", values_to = "code",
                                     names_pattern = "(.*)_code")

comm_key_tidy <- comm_key_tidy %>%
  mutate(source_code = ifelse(source == "none",
                              "none",
                              source_code),
         code = ifelse(source == "none",
                       NA_character_,
                       code)) %>%
  unique()

comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))

comm_key_tidy$source_code <- NULL

# Joining price_ref and comm_key tibbles to get the final dataset to be used in the analysis

clcc_prices_ref <- comm_key_tidy %>%
  left_join(ref_prices)

clcc_prices_ref <- clcc_prices_ref %>%
  mutate(across(mean:max, ~ if_else(is.na(.x), 0, .x)),
         ref_yr = ref_yr)

# Loading old prices for 2023-2024 comparison

prices_23 <- read_excel(
  path = here("data-raw", "prices_23.xlsx")
)

# checking if the call retrieved data for all years (comtrade data only)

check_comtrade_l <- clcc_prices_ref %>%
  filter(source == "comtrade") %>%
  group_by(no_comm) %>%
  group_split()

length_check <- sapply(check_comtrade_l, function (x) x[["n_obs"]] != h)

if (sum(length_check != 0)) stop ("The comtrade query did not return data for all years selected for at least one commodity")

#loading data with simapro commodity codes

simapro_codes <- read_excel(here("data-raw/db_comm_master_2024.xlsx"),
                            sheet = "simapro_codes") %>%
  mutate(formula = noquote(formula))


#Loading metadata files to prepare output csv to be loaded in SimaPro

template_names <- c("top", "mid1", "mid2", "bottom", "gruppi_top", "gruppi_bottom",
                    "gruppi_critical_top", "gruppi_critical_bottom")

# simapro_template <- lapply(template_names,
#                            function(x) readLines(here::here("data-raw",
#                                                 paste0("simapro_template_", x))
#                                      )
#
# )

simapro_template <- lapply(template_names,
                           function(x) read_delim(here(paste0("data-raw/simapro_template_", x, ".csv")),
                                                  delim = ";",
                                                  col_names = FALSE))

names(simapro_template) <- template_names

# Adding commodity macro-categories and periodic table symbols



usethis::use_data(clcc_prices_ref, prices_23, overwrite = TRUE)

usethis::use_data(simapro_template, simapro_codes, overwrite = TRUE, internal = TRUE)

