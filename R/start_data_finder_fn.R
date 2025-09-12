#' Find the number of rows to skipe while importing an xlsx inventory from SimaPro
#'
#' This function finds the number of rows to skip while importing inventory data
#' from an excel file generated with SimaPro, a software for life-cycle assessment.
#'
#' @param path_xlsx Path to SimaPro data in .xlsx format
#' @param pattern A string describing the pattern to be searched in the first column of the excel sheet to identify rows to skip
#'     when importing the file
#' @return The number of rows to skip while importing the file to R (integer)
start_data_finder_fn <- function(path_xlsx, pattern = c("No", "N.")) {

  # Reads excel file with no column names
  data_excel <- readxl::read_excel(path_xlsx, col_names = FALSE, .name_repair = "unique_quiet")

  first_column <- stringr::str_trim(data_excel[[1]])

  # Filters first column
  start_row <- data_excel |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::filter(stringr::str_detect(first_column,
                                      paste(pattern, collapse = "|"))) |>
    dplyr::pull(.data[["row_id"]])


  # Returns the number of rows to skip
  if (length(start_row) > 0) {
    return(start_row[1] - 1)
  } else {
    # Se non viene trovata, restituisce NA e un avviso
    warning("Pattern 'No' o 'N.' non trovato nella prima colonna. Controlla il file.")
    return(NA)
  }
}
