#' Find the number of rows to skipe while importing an xlsx inventory from SimaPro
#'
#' This function finds the number of rows to skip while importing inventory data
#' from an excel file generated with SimaPro, a software for life-cycle assessment.
#'
#' @param path_xlsx Path to SimaPro data in .xlsx format
#' @return The number of rows to skip while importing the file to R (integer)
#' @export
start_data_finder_fn <- function(path_xlsx){

  xslx_str <- tidyxl::xlsx_cells(path_xlsx)

  # identifies the row number of the cell, in the A column, with the "No" string (start of the table)
  xslx_str <- dplyr::filter(xslx_str, stringr::str_detect(address, "A") &
                              character %in% c("No", "N."))

  xslx_str$row - 1 # returns the number of rows to skip while importing the excel file

}
