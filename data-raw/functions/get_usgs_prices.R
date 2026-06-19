get_usgs_prices <- function(ref_yr = 2026,
                            output_dir = here::here("data-raw","usgs"),
                            historical_file = "usgs_historical.xlsx", # historical prices
                            usgs_corr_table = "usgs_corr_table.xlsx", # # correspondence table simapro - usgs
                            latest_wollastonite = NA_real_, # insert manually latest wollastonite price
                            usgs_um_legend = "usgs_um.xlsx" # measurement unit key
                            ) {

  usgs_key <- readxl::read_xlsx(
    here::here("data-raw", "usgs", usgs_corr_table)
  )

  usgs_um <- readxl::read_xlsx(
    here::here("data-raw", "usgs", usgs_um_legend)
  )

  query_res <- # finding dataset of interest
    sbtools::query_sb_text(
    paste0("Mineral Commodity Summaries ",
           ref_yr,
           " Salient"))

  titles <- sapply(query_res, \(x) x$title)

  idx <- grep("Commodity Salient U\\.S\\. and World Statistics", titles)

  commodity_item <- query_res[[idx[1]]]

  sb_id <- commodity_item$id

  # donwloading data

  year_dir <- file.path(output_dir, ref_yr)

  if (!dir.exists(year_dir)) {
  dir.create(year_dir, recursive = TRUE, showWarnings = FALSE) # creating destination folder
  }

  comm_data_raw <-
    sbtools::item_file_download(
      sb_id,
      dest_dir = year_dir,
      overwrite_file = TRUE
    )


  # loading just downloaded data from disk

  downloaded_files <- list.files(year_dir)

  csv_file_name <- downloaded_files[
    grepl("Commodities_Data\\.csv$", downloaded_files)
  ]

  if(length(csv_file_name) > 1) stop(paste0("Check downloaded data in data-raw/usgs/", ref_yr, ". Multiple .csv donwloaded"))

  csv_file_path <- file.path(year_dir, csv_file_name)

  df <- readr::read_csv(csv_file_path, show_col_types = FALSE)

  df_tidy <-
    df |>
    dplyr::filter(Statistics == "Price",
                  stringr::str_detect(Value, "\\d")) |>
    dplyr::mutate(Value = readr::parse_number(Value),
                  year = readr::parse_integer(Year)) |>
    dplyr::select(
      usgs_label = Commodity, year, um = Unit, price_type = Statistics_detail, value = Value
    )

  # --- Diagnostics: detect unmapped USGS labels and price types ---
  # These checks help identify breaking changes in the USGS MCS data structure
  # before filtering price rows using the historical correspondence table.

  usgs_labels_available <- df_tidy |>
    dplyr::distinct(usgs_label) |>
    dplyr::arrange(usgs_label)

  usgs_labels_expected <- usgs_key |>
    dplyr::distinct(usgs_label) |>
    dplyr::arrange(usgs_label)

  # USGS labels found in the new MCS data but not used in the CLCC mapping table.
  new_usgs_labels <- usgs_labels_available |>
    dplyr::anti_join(
      usgs_labels_expected,
      by = "usgs_label"
    )

  # USGS labels expected by the CLCC mapping table but not found in the new MCS data.
  missing_usgs_labels <- usgs_labels_expected |>
    dplyr::anti_join(
      usgs_labels_available,
      by = "usgs_label"
    )

  # Price types found in the new MCS data for known USGS labels,
  # but not selected in the CLCC correspondence table.
  new_price_types_for_known_labels <- df_tidy |>
    dplyr::semi_join(
      usgs_key |> dplyr::distinct(usgs_label),
      by = "usgs_label"
    ) |>
    dplyr::distinct(usgs_label, price_type, um) |>
    dplyr::anti_join(
      usgs_key |> dplyr::distinct(usgs_label, price_type),
      by = c("usgs_label", "price_type")
    ) |>
    dplyr::arrange(usgs_label, price_type)

  # Price types expected by the CLCC correspondence table
  # but not found in the new MCS data.
  missing_price_types <- usgs_key |>
    dplyr::distinct(usgs_label, price_type) |>
    dplyr::anti_join(
      df_tidy |> dplyr::distinct(usgs_label, price_type),
      by = c("usgs_label", "price_type")
    ) |>
    dplyr::arrange(usgs_label, price_type)

  # --- Diagnostics: detect unit changes for mapped USGS price types ---
  # This check compares the physical unit declared in the mapping table
  # with the unit found in the latest USGS dataset.

  unit_check <- df_tidy |>
    dplyr::semi_join(
      usgs_key,
      by = c("usgs_label", "price_type")
    ) |>
    dplyr::distinct(usgs_label, price_type, um) |>
    dplyr::left_join(
      usgs_key |>
        dplyr::distinct(usgs_label, price_type, usgs_phy_um),
      by = c("usgs_label", "price_type")
    ) |>
    dplyr::mutate(
      usgs_phy_um_from_new_data = dplyr::case_when(
        stringr::str_detect(um, "pound") ~ "pound",
        stringr::str_detect(um, "metric ton") ~ "metric ton",
        stringr::str_detect(um, "kilogram") ~ "kilogram",
        stringr::str_detect(um, "troy ounce") ~ "troy ounce",
        stringr::str_detect(um, "carat") ~ "carat",
        TRUE ~ NA_character_
      )
    )

  unit_changes <- unit_check |>
    dplyr::filter(
      !is.na(usgs_phy_um_from_new_data),
      !is.na(usgs_phy_um),
      usgs_phy_um_from_new_data != usgs_phy_um
    ) |>
    dplyr::arrange(usgs_label, price_type)

  # --- Emit warnings for diagnostics ---

  if (nrow(new_usgs_labels) > 0) {
    warning("New USGS labels found but not mapped in usgs_key. Inspect `new_usgs_labels`.")
  }

  if (nrow(missing_usgs_labels) > 0) {
    warning("Some USGS labels expected by usgs_key were not found in the latest MCS data. Inspect `missing_usgs_labels`.")
  }

  if (nrow(new_price_types_for_known_labels) > 0) {
    warning("New price types found for known USGS labels. Inspect `new_price_types_for_known_labels`.")
  }

  if (nrow(missing_price_types) > 0) {
    warning("Some price types expected by usgs_key were not found in the latest MCS data. Inspect `missing_price_types`.")
  }

  if (nrow(unit_changes) > 0) {
    warning("Some mapped USGS price types appear to have changed unit. Inspect `unit_changes`.")
  }


  # Keep only USGS price rows explicitly selected in the historical mapping table
  usgs_price_rows <- df_tidy |>
    dplyr::semi_join(
      usgs_key,
      by = c("usgs_label", "price_type")
    ) |>
    dplyr::mutate(
      # Convert cents to dollars where needed.
      value = dplyr::if_else(
        stringr::str_detect(um, "dollars"),
        value,
        value / 100 #converting prices expressed in USD cents into USD
      )
    )

  usgs_prices_mapped <- usgs_price_rows |>
    dplyr::left_join(
      usgs_key,
      by = c("usgs_label", "price_type"),
      relationship = "many-to-many"
    )

  # Validate final uniqueness: one price per CLCC flow and year.
  check_final_duplicates <- usgs_prices_mapped |>
    dplyr::count(comm, code, year, sort = TRUE) |>
    dplyr::filter(n > 1)

  if (nrow(check_final_duplicates) != 0)
    stop("Duplicates: please check the data")


  # --- Select columns of interest in original USGS physical units ---

  usgs_prices <- usgs_prices_mapped |>
    dplyr::select(
      comm,
      code,
      usgs_phy_um,
      year,
      price = value
    )

  # --- Add manually updated Wollastonite price, if available ---
  # MCS publication year is one year after the reference price year.
  # Example: MCS 2026 contains 2025 price data.

  latest_price_year <- ref_yr - 1

  usgs_prices <- usgs_prices |>
    dplyr::add_row(
      comm = "Wollastonite",
      code = "7345_USGS",
      usgs_phy_um = "t",
      year = latest_price_year,
      price = latest_wollastonite
    )

  # --- Load historical USGS prices in original USGS physical units ---

  historical <- readxl::read_excel(
    here::here("data-raw", "usgs", historical_file),
    sheet = "historical"
  ) |>
    dplyr::select(
      comm,
      code,
      usgs_phy_um = um,
      year,
      price
    )

  # --- Bind historical and latest prices ---
  # If the same CLCC commodity/year exists in both datasets,
  # the newly downloaded USGS value must take priority.

  usgs_prices_new <- usgs_prices |>
    dplyr::mutate(source_priority = 2L,
                  source_version = paste0("MCS", ref_yr)
                  )

  historical_new <- historical |>
    dplyr::mutate(source_priority = 1L,
                  source_version = NA_character_
                  )

  usgs_prices_all_raw <- dplyr::bind_rows(
    historical_new,
    usgs_prices_new
  )

  # --- Inspect duplicated years before resolving them ---

  duplicated_years_before <- usgs_prices_all_raw |>
    dplyr::count(comm, code, year, sort = TRUE) |>
    dplyr::filter(n > 1)

  # --- Resolve duplicates by keeping the most recent source ---
  # source_priority = 2 means newly downloaded USGS data.
  # source_priority = 1 means historical data.

  usgs_prices_all_raw <- usgs_prices_all_raw |>
    dplyr::arrange(comm, code, year, dplyr::desc(source_priority)) |>
    dplyr::distinct(comm, code, year, .keep_all = TRUE) |>
    dplyr::select(-source_priority)

  # --- Validate that no duplicate years remain in the raw historical dataset ---

  duplicated_years_after <- usgs_prices_all_raw |>
    dplyr::count(comm, code, year, sort = TRUE) |>
    dplyr::filter(n > 1)

  if (nrow(duplicated_years_after) > 0) {
    stop("Duplicate years remain after resolving historical/latest overlap.")
  }

  # --- Check missing years within each commodity time series ---

  year_gaps <- usgs_prices_all_raw |>
    dplyr::group_by(comm, code) |>
    dplyr::summarise(
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      observed_years = list(sort(unique(year))),
      expected_years = list(seq(min_year, max_year)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      missing_years = purrr::map2(expected_years, observed_years, setdiff),
      n_missing_years = purrr::map_int(missing_years, length)
    ) |>
    dplyr::filter(n_missing_years > 0)

  if (nrow(year_gaps) > 0) {
    warning("Some USGS price series contain missing years. Inspect `year_gaps`.")
  }

  # --- Convert prices to USD/kg for CLCC calculations ---

  missing_um <- usgs_prices_all_raw |>
    dplyr::distinct(usgs_phy_um) |>
    dplyr::anti_join(
      usgs_um,
      by = c("usgs_phy_um" = "usgs_phy_um")
    )

  if (nrow(missing_um) > 0) {
    print(missing_um)
    stop(
      "Missing USGS unit conversion factors for: ",
      paste(missing_um$usgs_phy_um, collapse = ", ")
    )
  }

  usgs_prices_all <- usgs_prices_all_raw |>
    dplyr::left_join(
      usgs_um,
      by = c("usgs_phy_um" = "usgs_phy_um")
    ) |>
    dplyr::mutate(
      price = price / fct,
      um = um_to,
      source = "usgs",
      cur = "usd"
    ) |>
    dplyr::select(
      comm,
      code,
      um,
      year,
      price,
      source,
      cur
    )

  usgs_prices_all_raw <- usgs_prices_all_raw |>
    arrange(comm, code, year)

  usgs_prices_all <- usgs_prices_all |>
    arrange(comm, code, year)

  # --- Return final dataset and diagnostics ---
  # prices_raw should be saved as the new historical dataset for next year.
  # prices is the converted dataset used in CLCC calculations.

  return(
    list(
      prices = usgs_prices_all,
      prices_raw = usgs_prices_all_raw,
      duplicated_years_before = duplicated_years_before,
      year_gaps = year_gaps,
      diagnostics = list(
        new_usgs_labels = new_usgs_labels,
        missing_usgs_labels = missing_usgs_labels,
        new_price_types_for_known_labels = new_price_types_for_known_labels,
        missing_price_types = missing_price_types,
        unit_changes = unit_changes
      )
    )
  )


}

price_usgs <- get_usgs_prices()

# Saving new historical file

openxlsx::write.xlsx(
  list(
    historical = price_usgs$prices_raw
  ),
  file = here::here("data-raw", "usgs", paste0("usgs_historical", "_MCS2026", ".xlsx")),
  overwrite = TRUE
)

price_usgs_def <- price_usgs$prices





