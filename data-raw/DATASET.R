## code to prepare `DATASET` dataset goes here

# loading libraries

pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate)

# loading measurement units of IMF prices and performing measurement unit conversion

um_imf <- read_xlsx(here("data-raw/um.xlsx"), sheet = "imf")[, c("imf_code", "um")]

um_p <- read_xlsx(here("data-raw/um.xlsx"), sheet = "p") # oading measurement unit conversion factors for price data

um_q <- read_xlsx(here("data-raw/um.xlsx"), sheet = "q") # loading measurement unit conversion factors for inventory data

# defining reference year

ref_yr <- 2021

# downloading GDP deflators (Euro area for Eurostat and Comext data and US for Comtrade and USITC data)

gdp_defl_raw <- rdb(c("OECD/MEI/EA19.NAGIGP01.IXOBSA.A",
      "OECD/MEI/USA.NAGIGP01.IXOBSA.A"))

gdp_defl <- gdp_defl_raw[, c("original_period", "LOCATION", "value")]

names(gdp_defl) <- c("year", "country", "value")

gdp_defl$year <- as.numeric(gdp_defl$year)


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

comtrade_codes_l <- split(comtrade_codes, ceiling(seq_along(comtrade_codes)/5))

# creating a nested tibble to download codes later

codes_tibble <- tibble::tibble(start_date = rep(c(ref_yr - 9, ref_yr - 4), length(comtrade_codes_l)),
                               end_date = rep(c(ref_yr - 5, ref_yr), length(comtrade_codes_l)),
                               codes = rep(comtrade_codes_l, each = 2))

# Downloading raw trade data

if (ct_get_remaining_hourly_queries() < length(codes_tibble)) stop("Superaro limite massimo di query orarie")

trade_data_raw <- codes_tibble %>%
  dplyr::rowwise() %>%
  dplyr::mutate(flows = list(tibble::as_tibble(
    comtradr::ct_search(
      reporters = "All", partners = "World",
      trade_direction = "exports", start_date = start_date,
      end_date = end_date, commod_codes = codes
    )))) %>%
  dplyr::ungroup() # downloads raw trade data

trade_data_raw <- trade_data_raw %>%
  mutate(record_num = sapply(flows, function(x) nrow(x))) # number of records extracted by every single query

if (sum(trade_data_raw$record_num > 50000) != 0) stop("C'è almeno una query incompleta (più di 50 mila record)")

# tidying trade data and removing flows of less than 100 kg

trade_data_tidy <- do.call(rbind, trade_data_raw$flows)

trade_data_tidy <- subset(trade_data_tidy, netweight_kg >= 100,
                          select = c(year, reporter_iso, partner_iso, commodity_code,
                                     commodity, netweight_kg, trade_value_usd))

# removing rows with missing values (either quantity or value)

trade_data_tidy <- trade_data_tidy[!with(trade_data_tidy,
                      is.na(netweight_kg) | is.na(trade_value_usd)), ]

# Calculating unit values

trade_data_tidy <- aggregate(cbind(netweight_kg, trade_value_usd)
                             ~ year + commodity_code, trade_data_tidy, sum)

unit_values <- within(trade_data_tidy, {
  uv_usd_kg <- trade_value_usd / netweight_kg
  trade_value_usd <- NULL
  netweight_kg <- NULL
})

# Adding missing rows (some commodities do not have values for every year)

uv_grid <- tidyr::expand_grid(year = seq(ref_yr - 9, ref_yr, 1),
                              commodity_code = unlist(comtrade_codes_l))

unit_values <- merge(uv_grid, unit_values, all.x = TRUE)

#### Downloading data from the IMF commodity price database using the rdbnomics database####

codes_imf <- subset(master_data, source == "imf")$imf_code %>%
  unique()

codes_imf <- paste0("IMF/PCPS/A.W00.", codes_imf, ".USD") # creating codes compatible with the rdbnomics package

price_imf_raw <- rdb(codes_imf) # prices in raw data form

str(price_imf_raw)

price_imf_raw <- price_imf_raw[, c("original_period", "COMMODITY", "value")]

names(price_imf_raw) <- c("year", "comm", "price_usd")

# preparing the final tibble (mu conversion)

price_imf_all <- price_imf_raw %>%
  left_join(um_imf,
            by = c("comm" = "imf_code")) %>%
  rename(um_from = um) %>%
  left_join(um_p) # adding original measurement units to price series in USD

price_imf_all <- price_imf_all %>%
  mutate(price_usd = price_usd / fct,
         year = as.numeric(year),
         source = "imf",
         cur = "usd") %>%
  select(!c(um_from, um_to, fct))

#### Downloading electric energy prices from Eurostat ####

# Downloading prices for non-household electricty consumption at various annual consumption levels

eurostat_indicators <- rdb_dimensions(provider_code = "Eurostat", dataset_code = "nrg_pc_205")

cons_band_codes <- eurostat_indicators$Eurostat$nrg_pc_205$consom$consom # codes to retrieve data for different consumption bands

cons_band_eurostat <- eurostat_indicators$Eurostat$nrg_pc_205$consom # data table with consumption bands codes and labels

cons_band_eurostat <- add_column(cons_band_eurostat, min_band = c(0, 20, 500, 2000, 20000, 70000, 150000),
           max_band = c(19, 499, 1999, 19999, 69999, 149999, Inf)) # conumption bands legend

# building codes for extraction
cons_band_codes <- paste0("Eurostat/nrg_pc_205/S.6000.KWH.", cons_band_codes, ".X_TAX.EUR.EU27_2020")

# retrieving data

price_eurostat_raw <- rdb(cons_band_codes)

price_eurostat <- price_eurostat_raw[, c("period", "consom", "Consumption", "value")]

price_eurostat$Consumption <- str_sub(price_eurostat$Consumption, 1, 7)

price_eurostat$period <- lubridate::year(price_eurostat$period)

names(price_eurostat) <- c("year", "cons_code", "cons_label", "price")

price_eurostat <- price_eurostat %>%
  group_by(year, cons_code, cons_label) %>%
  summarise(price = mean(price)) %>%
  ungroup() # tibble with all average yearly prices by consumption band

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
         usitc_code = `HTS Number`,
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



usethis::use_data(DATASET, overwrite = TRUE)
