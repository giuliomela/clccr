# loading usitc data (neodymium)

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

usethis::use_data(price_usitc_def, overwrite = TRUE) # makes the file available in /data folder as .rda
