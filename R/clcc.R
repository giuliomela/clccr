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
#' @param critical An optional argument. If set to TRUE, the function returns a tibble
#' containing both the clcc and the critical clcc indicators. Default set to FALSE.
#' @return A tibble containing the CLCC indicator calculated for each object and phase.
#' @importFrom magrittr '%>%'
#' @export
clcc <- function(path, critical = FALSE){

  clcc_critical <- object <- phase <- NULL

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  prices <- ref_prices()

  inv_prices <- merge(inventories, prices, all.x = TRUE)

  inv_prices <- inv_prices[, -which(names(inv_prices) %in% c("um", "source", "code"))]

  # Comouting the CLCC indicator

  clcc_res <- by(inv_prices, list(inv_prices$object, inv_prices$phase), function(df) {
    with(df, data.frame(object = object[[1]], phase = phase[[1]],
                        clcc = sum(mean * quantity)))
  })
  clcc_res <- do.call(rbind, clcc_res)

  if(critical == TRUE){

    inv_prices_crit <- inv_prices[inv_prices$critical == "yes", ]

    clcc_crit_res <- by(inv_prices_crit, list(inv_prices_crit$object,
                                              inv_prices_crit$phase), function(df) {
      with(df, data.frame(object = object[[1]], phase = phase[[1]],
                          clcc_critical = sum(mean * quantity)))
    })
    clcc_crit_res <- do.call(rbind, clcc_crit_res)

    clcc_res <- merge(clcc_res, clcc_crit_res, all.x = TRUE)

    clcc_res <- within(clcc_res, {
      share_critical <- ifelse(clcc != 0,
        clcc_critical / clcc * 100,
        0)
    })

  }

  tidyr::as_tibble(clcc_res) %>%
    dplyr::arrange(object, phase)

}
