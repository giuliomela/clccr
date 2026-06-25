get_usgs_prices <- function(ref_yr = 2026,
                            output_dir = here::here("data-raw", "usgs"),
                            historical_file = "usgs_historical.xlsx",
                            usgs_corr_table = "usgs_corr_table.xlsx",
                            latest_wollastonite = NA_real_,
                            usgs_um_legend = "usgs_um.xlsx",
                            verbose = TRUE,
                            save_logs = TRUE) {

  log_msg <- function(...) {
    if (isTRUE(verbose)) message(...)
  }

  clean_utf8_df <- function(x) {
    x |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.character), ~ iconv(.x, from = "", to = "UTF-8", sub = "")),
        dplyr::across(dplyr::where(is.list), ~ vapply(.x, function(z) paste(z, collapse = ", "), character(1)))
      )
  }

  normalize_usgs_um <- function(x) {
    dplyr::case_when(
      x == "lb" ~ "pound",
      x == "t" ~ "metric ton",
      x == "kg" ~ "kilogram",
      x == "g" ~ "gram",
      x == "troy_ounce" ~ "troy ounce",
      x == "short t" ~ "short ton",
      TRUE ~ x
    )
  }

  infer_usgs_um_from_unit <- function(x) {
    dplyr::case_when(
      stringr::str_detect(x, stringr::regex("dry\\s+metric\\s+ton\\s+unit", ignore_case = TRUE)) ~ "dry metric ton unit",
      stringr::str_detect(x, stringr::regex("short\\s+ton", ignore_case = TRUE)) ~ "short ton",
      stringr::str_detect(x, stringr::regex("troy\\s+ounce", ignore_case = TRUE)) ~ "troy ounce",
      stringr::str_detect(x, stringr::regex("metric\\s+ton", ignore_case = TRUE)) ~ "metric ton",
      stringr::str_detect(x, stringr::regex("kilogram", ignore_case = TRUE)) ~ "kilogram",
      stringr::str_detect(x, stringr::regex("gram", ignore_case = TRUE)) ~ "gram",
      stringr::str_detect(x, stringr::regex("carat", ignore_case = TRUE)) ~ "carat",
      stringr::str_detect(x, stringr::regex("pound", ignore_case = TRUE)) ~ "pound",
      stringr::str_detect(x, stringr::regex("\\bton\\b", ignore_case = TRUE)) ~ "ton",
      TRUE ~ NA_character_
    )
  }

  normalize_for_match <- function(x) {
    x |>
      stringi::stri_enc_toutf8() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      stringr::str_to_lower() |>
      stringr::str_replace_all("[^a-z0-9]+", " ") |>
      stringr::str_squish()
  }

  # --- Load Tables ---
  usgs_key <- readxl::read_xlsx(file.path(output_dir, usgs_corr_table)) |>
    dplyr::filter(!is.na(usgs_label), !is.na(price_type)) |>
    dplyr::mutate(
      usgs_label_key = normalize_for_match(usgs_label),
      price_type_key = normalize_for_match(price_type)
    )

  usgs_um <- readxl::read_xlsx(file.path(output_dir, usgs_um_legend))

  log_msg("Loaded USGS correspondence table: ", nrow(usgs_key), " rows")

  # --- ScienceBase Query & Download (With callback) ---
  search_string <- paste0("Mineral Commodity Summaries ", ref_yr)
  log_msg("Querying ScienceBase for: ", search_string)

  # Control variables
  download_success <- FALSE
  query_res <- NULL

  # First attempt: specific year
  tryCatch({
    query_res <- sbtools::query_sb_text(search_string)
  }, error = function(e) {
    log_msg("Warning: First ScienceBase query failed due to server connection issues.")
  })

  titles <- if (!is.null(query_res)) vapply(query_res, function(x) x$title, character(1)) else character(0)
  idx <- grep("Salient|Statistics|Commodities.*Data", titles, ignore.case = TRUE)

  # Second attempt: callback on historical server
  if (length(idx) == 0) {
    log_msg("Specific year item not found or server down. Attempting fallback to USGS Historical Salient Hub...")
    tryCatch({
      query_res <- sbtools::query_sb_text("Mineral Commodity Summaries Salient U.S. and World Statistics")
      titles <- vapply(query_res, function(x) x$title, character(1))
      idx <- grep("Salient|Statistics", titles, ignore.case = TRUE)
    }, error = function(e) {
      log_msg("Warning: Historical Hub query failed as well.")
    })
  }

  year_dir <- file.path(output_dir, ref_yr)
  if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)

  # Defining a local file name standard path
  target_csv_name <- "Commodities_Data.csv"
  csv_file_path <- file.path(year_dir, target_csv_name)

  # If one of the queries worked, attempting to download the file
  if (length(idx) > 0) {
    tryCatch({
      commodity_item <- query_res[[idx[1]]]
      sb_id <- commodity_item$id
      sb_files <- sbtools::item_list_files(sb_id, recursive = FALSE)

      csv_to_download <- sb_files |>
        dplyr::filter(stringr::str_detect(fname, stringr::regex("Commodities_Data\\.csv$|salient.*\\.csv$", ignore_case = TRUE)))

      if (nrow(csv_to_download) > 0) {
        target_csv_name <- csv_to_download$fname[[1]]
        csv_file_path <- file.path(year_dir, target_csv_name)
        log_msg("Downloading refreshed file from ScienceBase: ", target_csv_name)
        sbtools::item_file_download(sb_id, files = target_csv_name, dest_dir = year_dir, overwrite_file = TRUE)
        download_success <- TRUE
      }
    }, error = function(e) {
      log_msg("Warning: Error occurred during file extraction from ScienceBase. Switching to offline mode.")
    })
  }

  # --- Third attempt: if no files are downloaded, loading local cache copy
  if (!isTRUE(download_success)) {
    log_msg("🔴 ScienceBase is completely unreachable or items are missing.")

    # Looking up for old files inside year directory
    possible_local_files <- list.files(year_dir, pattern = "\\.csv$", full.names = TRUE)

    if (length(possible_local_files) > 0) {
      csv_file_path <- possible_local_files[1]
      log_msg("🟢 Safe local fallback activated! Using cached file found at: ", csv_file_path)
    } else {
      # Look into main usgs folder as secondary backup plan
      global_backup <- file.path(output_dir, "Commodities_Data.csv")
      if (file.exists(global_backup)) {
        csv_file_path <- global_backup
        log_msg("🟢 Safe local fallback activated! Using global backup file: ", csv_file_path)
      } else {
        stop("Critical: ScienceBase servers are down AND no cached 'Commodities_Data.csv' was found locally.")
      }
    }
  }

  # --- Process Downloaded Data ---
  df <- readr::read_csv(csv_file_path, show_col_types = FALSE)

  df_tidy <- df |>
    dplyr::mutate(Value_raw = as.character(Value)) |>
    dplyr::filter(Statistics == "Price", stringr::str_detect(Value_raw, "\\d")) |>
    dplyr::mutate(
      Value = readr::parse_number(Value_raw),
      year = readr::parse_integer(Year),
      usgs_phy_um_from_unit = infer_usgs_um_from_unit(Unit),
      usgs_label_key = normalize_for_match(Commodity),
      price_type_key = normalize_for_match(Statistics_detail)
    ) |>
    dplyr::select(usgs_label = Commodity, year, um = Unit, usgs_phy_um_from_unit,
                  price_type = Statistics_detail, value = Value, usgs_label_key, price_type_key)

  # --- Change Detection Diagnostics ---
  new_usgs_labels <- df_tidy |>
    dplyr::distinct(usgs_label) |>
    dplyr::anti_join(usgs_key |> dplyr::distinct(usgs_label), by = "usgs_label")

  missing_usgs_labels <- usgs_key |>
    dplyr::distinct(usgs_label_key, usgs_label) |>
    dplyr::anti_join(df_tidy |> dplyr::distinct(usgs_label_key), by = "usgs_label_key")

  new_price_types_for_known_labels <- df_tidy |>
    dplyr::semi_join(usgs_key |> dplyr::distinct(usgs_label_key), by = "usgs_label_key") |>
    dplyr::distinct(usgs_label, price_type, um, usgs_label_key, price_type_key) |>
    dplyr::anti_join(usgs_key |> dplyr::distinct(usgs_label_key, price_type_key), by = c("usgs_label_key", "price_type_key")) |>
    dplyr::select(-usgs_label_key, -price_type_key)

  missing_price_types <- usgs_key |>
    dplyr::distinct(usgs_label_key, price_type_key, usgs_label, price_type) |>
    dplyr::anti_join(df_tidy |> dplyr::distinct(usgs_label_key, price_type_key), by = c("usgs_label_key", "price_type_key")) |>
    dplyr::select(-usgs_label_key, -price_type_key)

  unit_changes <- df_tidy |>
    dplyr::inner_join(usgs_key |> dplyr::distinct(usgs_label_key, price_type_key, usgs_phy_um), by = c("usgs_label_key", "price_type_key")) |>
    dplyr::distinct(usgs_label, price_type, usgs_phy_um_from_unit, usgs_phy_um) |>
    dplyr::mutate(usgs_phy_um_normalized = normalize_usgs_um(usgs_phy_um)) |>
    dplyr::filter(!is.na(usgs_phy_um_from_unit), !is.na(usgs_phy_um_normalized), usgs_phy_um_from_unit != usgs_phy_um_normalized) |>
    dplyr::select(usgs_label, price_type, current_unit = usgs_phy_um_from_unit, expected_unit = usgs_phy_um)

  diagnostics_summary <- tibble::tibble(
    check = c("new_usgs_labels", "missing_usgs_labels", "new_price_types_for_known_labels", "missing_price_types", "unit_changes"),
    n = c(nrow(new_usgs_labels), nrow(missing_usgs_labels), nrow(new_price_types_for_known_labels), nrow(missing_price_types), nrow(unit_changes))
  )

  if (isTRUE(verbose)) print(diagnostics_summary)

  # --- Mapping and Safe Cents Conversion ---
  usgs_price_rows <- df_tidy |>
    dplyr::semi_join(usgs_key, by = c("usgs_label_key", "price_type_key")) |>
    dplyr::mutate(
      value = dplyr::if_else(stringr::str_detect(um, stringr::regex("cents|¢", ignore_case = TRUE)), value / 100, value)
    )

  usgs_prices_mapped <- usgs_price_rows |>
    dplyr::left_join(usgs_key, by = c("usgs_label_key", "price_type_key"), relationship = "many-to-many")

  if (nrow(usgs_prices_mapped |> dplyr::count(comm, code, year) |> dplyr::filter(n > 1)) > 0) {
    stop("Critical: Duplicate keys found within the downloaded batch.")
  }

  usgs_prices <- usgs_prices_mapped |> dplyr::select(comm, code, usgs_phy_um, year, price = value)

  # --- Wollastonite Handling ---
  latest_price_year <- ref_yr - 1
  if (!is.na(latest_wollastonite)) {
    usgs_prices <- usgs_prices |>
      dplyr::add_row(comm = "Wollastonite", code = "7345_USGS", usgs_phy_um = "t", year = latest_price_year, price = latest_wollastonite)
  }

  # --- Database Merging (Priority Resolution) ---
  historical <- readxl::read_excel(file.path(output_dir, historical_file)) |>
    dplyr::select(comm, code, usgs_phy_um, year, price)

  usgs_prices_all_raw <- dplyr::bind_rows(
    historical |> dplyr::mutate(source_priority = 1L),
    usgs_prices |> dplyr::mutate(source_priority = 2L)
  )

  duplicated_years_before <- usgs_prices_all_raw |> dplyr::count(comm, code, year) |> dplyr::filter(n > 1)

  # Overwrite old history if newer matching data was downloaded
  usgs_prices_all_raw <- usgs_prices_all_raw |>
    dplyr::arrange(comm, code, year, dplyr::desc(source_priority)) |>
    dplyr::distinct(comm, code, year, .keep_all = TRUE) |>
    dplyr::select(-source_priority) |>
    dplyr::arrange(comm, code, year)

  # --- Gap Check ---
  year_gaps <- usgs_prices_all_raw |>
    dplyr::group_by(comm, code) |>
    dplyr::summarise(
      min_year = min(year, na.rm = TRUE), max_year = max(year, na.rm = TRUE),
      observed_years = list(sort(unique(year))), expected_years = list(seq(min_year, max_year)), .groups = "drop"
    ) |>
    dplyr::mutate(
      missing_years = purrr::map2(expected_years, observed_years, setdiff),
      n_missing_years = purrr::map_int(missing_years, length)
    ) |>
    dplyr::filter(n_missing_years > 0)

  # --- Metric and Currency Standardization ---
  usgs_prices_all_raw <- usgs_prices_all_raw |> dplyr::mutate(usgs_phy_um = normalize_usgs_um(usgs_phy_um))

  if (nrow(usgs_prices_all_raw |> dplyr::distinct(usgs_phy_um) |> dplyr::anti_join(usgs_um, by = "usgs_phy_um")) > 0) {
    stop("Critical: Missing conversion factors in legend file.")
  }

  usgs_prices_all <- usgs_prices_all_raw |>
    dplyr::left_join(usgs_um, by = "usgs_phy_um") |>
    dplyr::mutate(price = price / fct, um = um_to, source = "usgs", cur = "usd") |>
    dplyr::select(comm, code, um, year, price, source, cur) |>
    dplyr::arrange(comm, code, year)

  # --- Export Diagnostic Logs ---
  if (isTRUE(save_logs)) {
    log_file <- file.path(output_dir, paste0("usgs_diagnostics_MCS", ref_yr, ".xlsx"))
    diagnostics_to_export <- list(
      diagnostics_summary = diagnostics_summary, new_usgs_labels = new_usgs_labels,
      missing_usgs_labels = missing_usgs_labels, new_price_types = new_price_types_for_known_labels,
      missing_price_types = missing_price_types, unit_changes = unit_changes,
      duplicated_years = duplicated_years_before, year_gaps = year_gaps
    ) |> purrr::map(clean_utf8_df)

    openxlsx::write.xlsx(diagnostics_to_export, file = log_file, overwrite = TRUE)
  }

  return(list(prices = usgs_prices_all, prices_raw = usgs_prices_all_raw))
}
