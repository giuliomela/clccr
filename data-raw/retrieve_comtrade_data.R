library(dplyr)
library(comtradr)
library(tidyr)
library(purrr)

get_comtrade_prices <- function(master_data_path = here::here("data-raw", "db_comm_master_2026.xlsx"),
                                ref_yr = 2025,
                                horizon = 10,
                                api_key = Sys.getenv("COMTRADE_PRIMARY"),
                                chunk_size = 20) {

  if (is.null(api_key) || api_key == "") {
    stop("Comtrade API key is missing. Please set COMTRADE_PRIMARY in .Renviron or pass it to api_key.")
  }

  comtradr::set_primary_comtrade_key(api_key)

  master_data <- readxl::read_xlsx(
    master_data_path
  )

  # Extract codes (excluding Spodumene as it requires specific reporter parameters)
  comtrade_codes <- master_data |>
    dplyr::filter(source == "comtrade" & comm != "Spodumene") |>
    dplyr::pull(comtrade_code) |>
    unique()

  spodumene_code <- master_data |>
    dplyr::filter(comm == "Spodumene") |>
    dplyr::pull(comtrade_code) |>
    unique()

  start_year <- ref_yr - (horizon - 1)

  # --- Chunking Logic to prevent HTTP 414 URI Too Long ---
  message("Splitting ", length(comtrade_codes), " commodity codes into chunks of ", chunk_size, "...")

  # Split the vector into a list of smaller vectors
  code_chunks <- split(comtrade_codes, ceiling(seq_along(comtrade_codes) / chunk_size))

  message("Fetching standard Comtrade data via multi-batch API calls...")

  # Iterate over chunks, fetch data, and bind rows safely
  comtrade_raw <- purrr::map_df(code_chunks, function(chunk) {
    message("Querying batch containing codes: ", paste(head(chunk, 3), collapse = ", "), "...")

    # Optional: pause briefly to respect API rate limits if needed
    Sys.sleep(0.5)

    comtradr::ct_get_data(
      flow_direction = "export",
      start_date = start_year,
      end_date = ref_yr,
      commodity_code = chunk
    )
  })

  # Target query Australia for Spodumene (kept separate as per legacy logic)
  message("Fetching Spodumene data for AUS reporter...")
  spodumene_raw <- comtradr::ct_get_data(
    flow_direction = "export",
    start_date = start_year,
    end_date = ref_yr,
    reporter = "AUS",
    commodity_code = spodumene_code
  )

  # Combine results and fallback to alternative quantity metrics if primary weight is missing
  comtrade_all <- dplyr::bind_rows(comtrade_raw, spodumene_raw) |>
    dplyr::mutate(qty = dplyr::if_else(is.na(qty) | qty == 0, alt_qty, qty))

  # Tidy and aggregate weights and values globally per code/year
  price_comtrade_def <- comtrade_all |>
    dplyr::select(year = ref_year, code = cmd_code, netweight_kg = qty, trade_value_usd = fobvalue) |>
    dplyr::filter(!is.na(netweight_kg), netweight_kg > 0) |>
    dplyr::group_by(year, code) |>
    dplyr::summarise(
      netweight_kg = sum(netweight_kg, na.rm = TRUE),
      trade_value_usd = sum(trade_value_usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(price = trade_value_usd / netweight_kg) |>
    dplyr::select(year, code, price)

  # Ensure complete time series grids exist for all monitored matrix fields
  all_codes <- unique(c(comtrade_codes, spodumene_code))
  uv_grid <- tidyr::expand_grid(year = seq(start_year, ref_yr, 1), code = as.character(all_codes))

  final_comtrade <- uv_grid |>
    dplyr::left_join(price_comtrade_def, by = c("year", "code")) |>
    dplyr::mutate(source = "comtrade", cur = "usd")

  return(final_comtrade)
}

get_comtrade_prices()
