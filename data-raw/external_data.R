# Loading external data

library(tidyverse)
library(comtradr)
library(devtools)
library(lubridate)

# Defining the reference year. Usually new data about the previous year are available
# from June of the current year onwards

if(month(Sys.Date(), label = FALSE) >= 6){

  ref_year <- year(Sys.Date()) - 1

}else{

  ref_year <- year(Sys.Date()) - 2

}

# Master file ####

db_comm_master <- vroom::vroom("data-raw/db_comm_master.csv")

# Comext ####

comext <- vroom::vroom("data-raw/comext.csv")

# selecting imports and flows regarding the EU as a whole

rare_earths_eu <- dplyr::rename_with(comext, tolower) # variable names in lowercase

rare_earths_eu <- subset(rare_earths_eu, flow == 1 &
                           reporter == "EU" & partner == "EU_EXTRA")

rare_earths_eu <- within(rare_earths_eu, {
  year <- as.numeric(stringr::str_sub(as.character(period), 1, 4))
  period <- NULL
})

# Comext prices expressed in euro/kg (current)

rare_earths_eu <- rare_earths_eu[, c("product", "indicators",
                                     "indicator_value", "year")]

rare_earths_eu <- subset(rare_earths_eu, year > ref_year - 10)

rare_earths_eu <- tidyr::pivot_wider(rare_earths_eu,
                                     names_from = indicators,
                                     values_from = indicator_value)

price_comext_def <- within(rare_earths_eu, {
  price <- VALUE_IN_EUROS / QUANTITY_IN_100KG / 100
  code <- as.character(product)
  source <- "comext"
  cur <- "eur"
  VALUE_IN_EUROS <- NULL
  QUANTITY_IN_100KG <- NULL
  product <- NULL
})

# Exchange rate ####

exc_rate_usd <- subset(eurostat::get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG",
                                                                              currency = "USD"),
                                              time_format = "num"), select = c(time, values))

names(exc_rate_usd) <- c("year", "exc_rate")

# GDP deflator ####

gdp_dfl <- subset(eurostat::get_eurostat("nama_10_gdp", time_format = "num",
                                         filters = list(geo = "EA", unit = "PD15_EUR",
                                                        na_item = "B1GQ")), select = c(time, values))

names(gdp_dfl) <- c("year", "gdp_dfl")

# Unit values - Comtrade ####

# Defining a function to calculate unit values for those series for which no price has been identified ####

# HS codes to download from comtrade

comtrade_codes <- subset(db_comm_master, source == "comtrade")$comtrade_code

comtrade_codes <- unique(as.character(comtrade_codes))

codes_list <- split(comtrade_codes, ceiling(seq_along(comtrade_codes)/5))

# creating a nested tibble to download codes later

codes_tibble <- tibble::tibble(start_date = rep(c(ref_year - 9, ref_year - 4), length(codes_list)),
                               end_date = rep(c(ref_year - 5, ref_year), length(codes_list)),
                               codes = rep(codes_list, each = 2))

# Downloading raw trade data

trade_data_raw <- codes_tibble %>%
  dplyr::rowwise() %>%
  dplyr::mutate(flows = list(tibble::as_tibble(
    comtradr::ct_search(
      reporters = "All", partners = "World",
      trade_direction = "exports", start_date = start_date,
      end_date = end_date, commod_codes = codes
    )))) %>%
  dplyr::ungroup()

# tyding trade data and removing flows of less than 100 kg

trade_data_tidy <- tidyr::unnest(dplyr::select(trade_data_raw, flows), flows)

trade_data_tidy <- subset(trade_data_tidy, netweight_kg >= 100,
                          select = c(year, reporter_iso, partner_iso, commodity_code,
                                     commodity, netweight_kg, trade_value_usd))

# removing rows with missing values (either quantity or value)

trade_data_tidy[!with(trade_data_tidy,
                      is.na(netweight_kg) | is.na(trade_value_usd)),]

# Calculating unit values

trade_data_tidy <- aggregate(cbind(netweight_kg, trade_value_usd)
                             ~ year + commodity_code, trade_data_tidy, sum)

unit_values <- within(trade_data_tidy, {
  uv_usd_kg <- trade_value_usd / netweight_kg
  trade_value_usd <- NULL
  netweight_kg <- NULL
})

# Adding missing rows (some commodities do not have values for every year)

uv_grid <- tidyr::expand_grid(year = seq(ref_year - 9, ref_year, 1),
                              commodity_code = unlist(codes_list))

unit_values <- merge(uv_grid, unit_values, all.x = TRUE)

# Comtrade prices expressed in usd/kg (current values)

unit_values <- dplyr::rename(unit_values, code = commodity_code,
                             price = uv_usd_kg)

price_comtrade_def <- within(unit_values, {
  source <- "comtrade"
  cur <- "usd"
})

# Electricity prices from Eurostat ####

# electric energy prices, expressed in euro/MJ (current values)

el_energy_eu_code <- unique(subset(db_comm_master, !is.na(eurostat_code))$eurostat_code)


price_eurostat <- eurostat::get_eurostat(el_energy_eu_code,
                                         filters = list(consom = "4162906", currency = "EUR",
                                                        geo = "EU27_2020", tax = "X_TAX"))

price_eurostat$time <- lubridate::year(price_eurostat$time)

price_eurostat <- setNames(aggregate(values ~ time, price_eurostat, mean),
                           c("year", "price"))

price_eurostat$um_from <- "kwh"

price_eurostat <- merge(price_eurostat, um_p, all.x = TRUE)

# dataset to be joined with the other prices (current prices)

price_eurostat_def <- within(price_eurostat, {
  price <- price / fct
  source <- "eurostat"
  cur <- "eur"
  code <- el_energy_eu_code
  um_from <- NULL
  um_to <- NULL
  fct <- NULL
})

# IMF prices (dowloaded via Quandl) ####

# Data are downloaded using the Nasdaq Data Link API, since IMF's is not working properly
# Quandl codes are therefore used

nasdaq_api = "vyzaUsZKLVqVCr2eMw5Z"

# commodities of which IMF prices is the reference price
codes_imf <- subset(db_comm_master, source == "imf")$quandl_code

Quandl::Quandl.api_key(nasdaq_api)

price_imf_raw <- Quandl::Quandl(codes_imf, type = "raw", collapse = "annual")

price_imf_raw <- within(price_imf_raw, {
  year <- lubridate::year(Date)
  Date <- NULL
})

price_imf_raw <- tidyr::pivot_longer(price_imf_raw, !year, names_to = "comm",
                                     values_to = "price_usd")

price_imf_raw <- within(price_imf_raw, {
  comm <- stringr::str_remove_all(comm, " - Value|ODA.|_USD")
})

# adding original measurement units to price series in USD

price_imf_all <- dplyr::mutate(price_imf_raw,
                               um = dplyr::case_when(
                                 comm == "PNGASEU" ~ "btu",
                                 comm == "PGOLD" ~ "oz",
                                 comm == "POILAPSP" ~ "bb",
                                 comm == "PURAN" ~ "lb",
                                 TRUE ~ "t"
                               ))

# converting all prices in usd/kg or usd/m3
price_imf_all <- merge(price_imf_all, um_p, by.x = "um", by.y = "um_from", all.x = TRUE)

price_imf_all <- within(price_imf_all, {
  price_usd_kg <- price_usd / fct
})

price_imf_all[, c('price_usd', 'fct', "um", "um_to")] <- list(NULL)

# preparing tibble to be joined later with the other prices. Values are in usd/kg or usd/m3 (current)

price_imf_def <- within(price_imf_all, {
  source <- "imf"
  cur <- "usd"
})

# dataset to be joined with the other prices (current prices)

price_imf_def <- dplyr::rename(price_imf_def, code = comm, price = price_usd_kg)

# USITC unit values data ####

usitc <- vroom::vroom("data-raw/usitc.csv")

# Usitc

# USITC already provides unit values (usd/kg). Values already expressed in usd/kg

usitc <- tidyr::pivot_longer(usitc,
                             !usitc_code, names_to = "year", values_to = "vu_usd")

price_usitc_def <- within(usitc, {
  year <- as.numeric(year)
  price <- as.numeric(vu_usd)
  code  <- as.character(usitc_code)
  source <- "usitc"
  cur <- "usd"
  vu_usd <- NULL
  usitc_code <- NULL
})

# Measurement units ####

# loading um conversion coefficients data

um_p <- vroom::vroom("data-raw/um_p.csv")

# looking for new commodity codes

all_materials <- readxl::read_xlsx(here::here("data-raw/all_materials.xlsx"))$comm

ct_commodity_lookup("acids")

# creating a sysdata.rds file to be used by package functions ####

use_data(price_comext_def, db_comm_master, exc_rate_usd,
         gdp_dfl, price_comtrade_def, price_eurostat_def,
         price_imf_def, um_p, price_usitc_def,
         overwrite = TRUE, internal = TRUE)

