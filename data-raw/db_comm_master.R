# loading master data

db_comm_master <- vroom::vroom("data-raw/db_comm_master.csv")

usethis::use_data(db_comm_master, overwrite = TRUE) # makes the file available in /data folder as .rda
