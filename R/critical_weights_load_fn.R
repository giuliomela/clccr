#' Load critical weights from an Excel file
#'
#' This function loads critical weights from an Excel file. Such weights are project-specific. Some materials are considered
#' critical by the European Commission of the International Energy Agency (IEA) but are not considered as individual flows by
#' SimaPro. For example the SimaPro flow "Coal, hard" contains "coking coal" which is critical. The weights are therefore needed to
#' compute the Critical-CLCC indicator.
#' @param path_weights A character vector. Path to the file containing the critical weights for each commodity and phase.
#' @examples
#' \dontrun{
#' critical_path <- path_to_critical_weights_file
#'
#' critical_weights_load_fn(path_weights = critical_path)
#'
#' }
critical_weights_load_fn <-
  function(
    path_weights
  ){

    critical_weights <- readxl::read_excel(path_weights) |>
      dplyr::select(1, 2, 3, 6)

    colnames(critical_weights) <- c("object", "comm", "phase", "weight")

    critical_weights$phase <- tolower(critical_weights$phase)

    critical_weights$weight <- suppressWarnings(readr::parse_number(critical_weights$weight)) # converte tutto in numero

    critical_weights <- # all variables in lower case
      critical_weights |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character),
          tolower
        )
      )

    return(critical_weights)


  }
