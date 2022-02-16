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

# loading master file

db_comm_master <- vroom::vroom("data-raw/db_comm_master.csv")

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

use_data(price_comtrade_def, overwrite = TRUE)
