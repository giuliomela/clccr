#' Dataset with commodity names, codes and reference prices - 2023 edition
#'
#' A datataset containing all raw materials considered for the computation of the
#' CLCC indicator and the various sources from which it is possible to gather price
#' data for each commodity. This dataset is for comparison with 2024 prices only.
#'
#' @format A tibble with variables:
#' \describe{
#' \item{comm}{commodity names, as in SimaPro output}
#' \item{price23}{Average reference price, expressed in constant euro/kg (m3) at the price levels of the reference year}
#' }
"prices_23"
