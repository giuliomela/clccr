get_comext_prices <- function(ref_yr = 2026,
                              horizon = 10,
                              comext_path = here::here("data-raw", "comext", "comext_2026.csv")) {


  message("Processing local Comext database file: ", comext_path)

  # Safe check: verify file existence before breaking the pipeline
  if (!file.exists(comext_path)) {
    warning("Comext bulk extraction file missing at location: ", comext_path, ". Returning empty dataset.")
    return(dplyr::tibble())
  }

  # Efficient read of Comext raw text extract
  comext_raw <- readr::read_delim(
    comext_path,
    delim = NULL, # Automatically detects comma, semicolon or tab
    show_col_types = FALSE
  ) |>
    dplyr::rename_with(tolower)


  # Target timeframe boundary calculation
  start_year <- ref_yr - (horizon - 1)

  # --- Data Cleaning and Defensive Filtering ---
  processed_comext <- comext_raw |>
    # Extract 4-digit year from period column (e.g., '202552' or '202500' -> 2025)
    dplyr::mutate(year = as.numeric(stringr::str_sub(as.character(period), 1, 4))) |>
    dplyr::filter(year >= start_year & year <= ref_yr)

  # Defensive check: enforce standard Eurostat Comext filtering if columns are present
  # flow == 1 represents Imports; reporter == "EU" or similar filters out specific member states
  if ("flow" %in% names(processed_comext)) {
    processed_comext <- processed_comext |> dplyr::filter(flow == 1)
  }
  if ("reporter" %in% names(processed_comext)) {
    processed_comext <- processed_comext |> dplyr::filter(reporter %in% c("EU", "EU_EXTRA", "TOTAL"))
  }

  # --- Safe conversion of indicator_value from character/factor to numeric ---
  processed_comext <- processed_comext |>
    dplyr::mutate(
      indicator_value = readr::parse_number(
        as.character(indicator_value),
        na = c(":", "-", "c", "NA", "")
      )
    )


  # --- Reshaping and Unit Value Calculation ---
  final_comext <- processed_comext |>
    dplyr::select(product, indicators, indicator_value, year) |>
    # Handle implicit aggregation if duplicates exist across secondary keys
    dplyr::group_by(product, indicators, year) |>
    dplyr::summarise(indicator_value = sum(indicator_value, na.rm = TRUE), .groups = "drop") |>
    # Pivot metrics columns (VALUE_IN_EUR and QUANTITY_IN_KG) into separate variables
    tidyr::pivot_wider(
      names_from = indicators,
      values_from = indicator_value
    ) |>
    # Enforce safe naming to prevent case mismatch issues
    dplyr::rename_with(toupper, .cols = dplyr::any_of(c("value_in_eur", "quantity_in_kg"))) |>
    # Filter out records with zero or missing quantities to prevent Division-by-Zero errors
    dplyr::filter(!is.na(QUANTITY_IN_KG) & QUANTITY_IN_KG > 0) |>
    dplyr::mutate(
      price = VALUE_IN_EUR / QUANTITY_IN_KG,
      code = as.character(product),
      source = "comext",
      cur = "eur"
    ) |>
    dplyr::select(year, code, price, source, cur) |>
    dplyr::filter(!is.na(price))

  message("Successfully extracted ", nrow(final_comext), " records from Comext file.")

  return(final_comext)
}

