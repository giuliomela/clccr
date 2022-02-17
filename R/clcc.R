#' Calculate the CLCC indicator
#'
#' This function calculates the CLCC indicator, a measure of resource scarcity,
#' for a product/process during its life cycle. The function automatically loads
#' the latest prices available and SimaPro inventories using other functions of
#' the clccr package.
#' The function can also return the CLCC indicator calculated taking into account
#' critical materials only.
#'
#' @param path A character vector. Path to the folder in which raw xlsx files are stored.
#' File names must be of the following format: project_name-object_name.xlsx
#' @param critical An optional argument. If set to TRUE, the function returns a tibble
#' containing both the clcc and the critical clcc indicators. Default set to FALSE.
#' @return A tibble containing the CLCC indicator calculated for each object and phase.
#' @importFrom magrittr '%>%'
#' @export
clcc <- function(path, critical = FALSE){

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  prices <- ref_prices()

  inv_prices <- merge(inventories, prices, all.x = TRUE)

  inv_prices <- inv_prices[, -which(names(inv_prices) %in% c("um", "source", "code"))]

  project <- object <- phase <- mean <- quantity <- NULL # avoids notes (dplyr and NSE)

  # Comouting the CLCC indicator

  clcc <- inv_prices %>%
    dplyr::group_by(project, object, phase) %>%
    dplyr::summarize(clcc = sum(mean * quantity)) %>%
    dplyr::ungroup()

  if(critical == TRUE){

    inv_prices <- inv_prices[inv_prices$critical == "yes", ]

    clcc_critical <- inv_prices %>%
      dplyr::group_by(project, object, phase) %>%
      dplyr::summarize(clcc_critical = sum(mean * quantity)) %>%
      dplyr::ungroup()

    clcc <- merge(clcc, clcc_critical, all.x = TRUE)

    clcc <- within(clcc, {
      share_critical <- ifelse(clcc != 0,
        clcc_critical / clcc * 100,
        0)
    })

  }

  tidyr::as_tibble(clcc)

}
