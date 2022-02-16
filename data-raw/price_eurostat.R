# Downloading Eurostat Data ####

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

usethis::use_data(price_eurostat_def,
                  overwrite = TRUE) # makes the file available in /data folder as .rda
