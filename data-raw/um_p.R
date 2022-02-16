# loading um conversion coefficients data

um_p <- vroom::vroom("data-raw/um_p.csv")

usethis::use_data(um_p, overwrite = TRUE) # makes the file available in /data folder as .rda

