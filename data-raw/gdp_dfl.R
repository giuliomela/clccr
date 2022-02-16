# downloading GDP deflator

gdp_dfl <- subset(eurostat::get_eurostat("nama_10_gdp", time_format = "num",
                                         filters = list(geo = "EA", unit = "PD15_EUR",
                                                        na_item = "B1GQ")), select = c(time, values))

names(gdp_dfl) <- c("year", "gdp_dfl")

usethis::use_data(gdp_dfl,
                  overwrite = TRUE) # makes the file available in /data folder as .rda
