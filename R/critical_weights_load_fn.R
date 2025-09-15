#' Load critical weights from an Excel file
#'
#' This function loads critical weights from an Excel file. Such weights are project-specific. Some materials are considered
#' critical by the European Commission of the International Energy Agency (IEA) but are not considered as individual flows by
#' SimaPro. For example the SimaPro flow "Coal, hard" contains "coking coal" which is critical. The weights are therefore needed to
#' compute the Critical-CLCC indicator.
#' @param weights_path A character vector. Path to the file containing the critical weights for each commodity and phase.
#' @examples
#' \dontrun{
#' critical_path <- path_to_critical_weights_file
#'
#' critical_weights_load_fn(path_weights = critical_path)
#'
#' }
critical_weights_load_fn <-
  function(
    weights_path
  ){

    sheets_labels <-
      readxl::excel_sheets(weights_path)


    tryCatch(
      {
        coke_sheet <- sheets_labels[stringr::str_detect(sheets_labels, "coal")]

        critical_weights_coke <- readxl::read_excel(weights_path, sheet = coke_sheet)
      }, error = function(e) {
        warning(paste("Something went wrong with the loading of the coking coal critical weights file. Error message:",
                      e$message))
      }
    )

    tryCatch(
      {
        silicon_sheet <- sheets_labels[stringr::str_detect(sheets_labels, "silicon")]

        critical_weights_silicon <- readxl::read_excel(weights_path, sheet = silicon_sheet)
      }, error = function(e) {
        warning(paste("Something went wrong with the loading of the coking coal critical weights file. Error:",
                      e$message))
      }
    )

    critical_weights <-
      list(
        coke = critical_weights_coke,
        silicon = critical_weights_silicon
      )

    purrr::walk(
      critical_weights,
      \(x){
        if(ncol(x) !=4) stop("Critical weights table must contain 4 columns, please check table format.")
      }
    )

    colnames(critical_weights$coke) <- c("object", "comm", "phase", "value")

    colnames(critical_weights$silicon) <- c("object", "comm", "phase", "value")

    critical_weights <-
      suppressWarnings(
        purrr::map(
        critical_weights,
        \(x) dplyr::mutate(x, phase = tolower(.data[["phase"]]))
      )
      )

    purrr::walk(
      critical_weights,
      \(x){
        if (isFALSE(is.numeric(x[["value"]])))
          stop("Values are not in numeric format, please check your weight table")
      }
    )




    critical_weights <- # all variables in lower case
      purrr::map(
        critical_weights,
        \(x) dplyr::mutate(
          x,
          dplyr::across(dplyr::where(is.character),
                        tolower)
        )
      )

    return(critical_weights)


  }
