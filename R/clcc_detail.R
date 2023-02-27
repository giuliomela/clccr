#' Compute the realtive share of individual material/energy flows on total CLCC
#'
#' This function computes the relative shares of each commodity considered for the
#' computation of the CLCC indicator on the total value of the indicator for both the
#' `baseline` and `critical` versions of the indicator itself.
#'
#' @param path A character vector. Path to the folder in which raw xlsx files are stored.
#' @param critical A logical value. If set to `TRUE`, shares referred to the critical-clcc indicator
#'     are returned. Default is set to `FALSE`.
#' @param phase_of_int A character string. The life-cycle phase for which the relative shares
#'     must be computed. Default is `total`. The user can however select any phase included in the
#'     inventory file.
#' @param collapse_share A numeric value. A parameter that the user can use to limit the number of
#'     commodities considered. For example, if `collapse_share = 0.9` (the default) only commodities which
#'     shares sum up to 90% of the total are returned as output. All the others are summed up in the
#'     `others` category. The parameter must be between `0` and `1`.
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
  #' data_path <- path_to_inventory_folder
  #'
  #' clcc_detail(path = data_path)
  #'
  #' clcc_detail(path = data_path, critical = TRUE, collapse_share = 0.5)
  #'
  #' }
clcc_detail <- function (path, critical = FALSE, phase_of_int = "total", collapse_share = 0.9) {

  if(collapse_share > 1 | collapse_share < 0)
    stop("Please provide a valid value for 'collapse_share'; between 0 and 1.")

  quantity <- phase <- comm <- object <- clcc_type <- desc <- share <- cum_share <- NULL

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  prices <- clccr::clcc_prices_ref

  test_commodity <- unique(inventories$comm) %in% prices$comm

  if(isTRUE(any(test_commodity == F))) stop("At least one commodity in the inventory is not present in the master file") # if true, at least one commodity is not in the master file

  inv_prices <- merge(inventories, prices, all.x = TRUE)

  if (isTRUE(critical)) {

    inv_prices <- inv_prices %>%
      dplyr::filter(critical == "yes") %>%
      dplyr::mutate(clcc_type = "critical-clcc")

  } else {

    inv_prices$clcc_type <- "baseline"

  }

  #inv_prices <- inv_prices[, -which(names(inv_prices) %in% c("um", "source", "code"))]

  # Computing the CLCC indicator

  clcc_raw <- inv_prices %>%
    dplyr::mutate(clcc = mean * quantity) %>%
    dplyr::filter(phase == phase_of_int) %>%
    dplyr::select(comm, object, phase, clcc_type, clcc) %>%
    tibble::as_tibble()

  clcc_tot <- clcc_raw %>%
    dplyr::group_by(object, phase, clcc_type) %>%
    dplyr::summarise(clcc_tot = sum(clcc)) %>%
    dplyr::ungroup()

  clcc_raw <- clcc_raw %>%
    dplyr::left_join(clcc_tot) %>%
    dplyr::mutate(share = clcc / clcc_tot) %>%
    dplyr::group_by(object) %>%
    dplyr::arrange(desc(share)) %>%
    dplyr::mutate(cum_share = cumsum(share),
                  comm = ifelse(
                    cum_share <= collapse_share,
                    comm,
                    "Other"
                  )) %>%
    dplyr::group_by(object, comm, phase, clcc_type, clcc_tot) %>%
    dplyr::summarise(dplyr::across(c(clcc, share), sum)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(object, desc(share))


}