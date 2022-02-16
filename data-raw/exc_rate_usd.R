# downloading exchange rates

exc_rate_usd <- subset(eurostat::get_eurostat("ert_bil_eur_a", filters = list(statinfo = "AVG",
                                                                              currency = "USD"),
                                              time_format = "num"), select = c(time, values))

names(exc_rate_usd) <- c("year", "exc_rate")

usethis::use_data(exc_rate_usd,
                  overwrite = TRUE) # makes the file available in /data folder as .rda
