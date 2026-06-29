## code to prepare `DATASET` dataset goes here

# loading libraries

pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate, httr, censusapi, jsonlite, eurostat, fredr, openxlsx)

price_version_comparison <-
  FALSE # if TRUE a price comparison with the previous year is carried out

# Defining parameters

dataset_version <- 2026 # version year
ref_yr <- 2025 # price reference year
h <- 10 # time horizon along which compute historical price averages

# loading master file

master_file_path <- here("data-raw", paste0("db_comm_master_", dataset_version, ".xlsx"))

master_data <- read_excel(master_file_path)



source("data-raw/retrieve_usgs_data.R")
source("data-raw/retrieve_comtrade_data.R")
source("data-raw/retrieve_imf_data.R")
source("data-raw/retrieve_comext_data.R")

source("data-raw/prepare_prices_dataset.R")

# join con master
source("data-raw/prepare_final_dataset.R")










usethis::use_data(clcc_prices_ref, prices_23, overwrite = TRUE)

usethis::use_data(simapro_template, simapro_codes, overwrite = TRUE, internal = TRUE)
