library(dplyr)
library(readxl)
library(rdbnomics)

get_imf_prices <- function(master_data_path = here::here("data-raw", "db_comm_master_2026.xlsx"),
                           output_dir = here::here("data-raw", "imf"),
                           usgs_um_legend = "usgs_um.xlsx") {

  message("Fetching IMF prices via DBnomics...")

  master_data <-
    readxl::read_xlsx(
      master_data_path
    )

  # Filter IMF codes from master data
  codes_imf <- master_data |>
    dplyr::filter(source == "imf") |>
    dplyr::pull(imf_code) |>
    unique()

  if (length(codes_imf) == 0) {
    warning("No IMF codes found in master_data.")
    return(dplyr::tibble())
  }

  # Format codes for DBnomics IMF provider
  dbnomics_codes <- paste0("IMF/PCPS/A.W00.", codes_imf, ".USD")

  price_imf_raw <- rdbnomics::rdb(dbnomics_codes) |>
    dplyr::select(year = original_period, code = COMMODITY, price_usd = value) |>
    dplyr::mutate(year = as.numeric(year))


  # Load unit conversions (assuming structure matches the standard usgs_um helper)
  um_legend <- readxl::read_xlsx(here::here("data-raw", "um.xlsx"), sheet = "p")

  # Mapping file specific to IMF codes to target units
  # Expects file data-raw/um.xlsx with sheet 'imf'
  um_imf <- readxl::read_xlsx(here::here("data-raw", "um.xlsx"), sheet = "imf") |>
    dplyr::select(imf_code, um)

  price_imf_def <- price_imf_raw |>
    dplyr::left_join(um_imf, by = c("code" = "imf_code")) |>
    dplyr::left_join(um_legend, by = c("um" = "um_from")) |>
    dplyr::mutate(
      price = price_usd / fct,
      source = "imf",
      cur = "usd"
    ) |>
    dplyr::select(year, code, price, source, cur) |>
    dplyr::filter(!is.na(price))

  return(price_imf_def)
}

get_imf_prices()
