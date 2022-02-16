# loading comext data

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

# selecting imports and flows regarding the EU as a whole

usethis::use_data(price_comext_def, overwrite = TRUE) # makes the file available in /data folder as .rda
