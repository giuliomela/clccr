# downloading IMF prices ####

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

usethis::use_data(price_imf_def,
                  overwrite = TRUE) # makes the file available in /data folder as .rda
