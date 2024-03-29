#' Dataset with commodity names, codes and reference prices
#'
#' A datataset containing all raw materials considered for the computation of the
#' CLCC indicator and the various sources from which it is possible to gather price
#' data for each commodity. The dataset also provides information on the preferred
#' data source for each commodity and on average prices (last 10 years) to be used in
#' computations. The data set provides also minimum and maximum prices of the reference
#' period to be used in Monte Carlo analysis
#'
#' @format A tibble with variables:
#' \describe{
#' \item{comm}{commodity names, as in SimaPro output}
#' \item{no_comm}{commodity ID}
#' \item{um}{measurement unit, as in SimaPro output}
#' \item{source}{Reference price source}
#' \item{critical}{Whether or not a given commodity belogns to the critical materials group defined by the EU}
#' \item{energy}{Whether or not a given commodity belong to the energy group}
#' \item{simapro_export}{Whether or not a commodity must be included in the .csv file to be exported to SimaProa}
#' \item{code}{Commodity code (depends on the source)}
#' \item{mean}{Average reference price, expressed in constant euro/kg (m3) at the price levels of the reference year}
#' \item{min}{Minimum price suring the reference period}
#' \item{max}{Maximum price suring the reference period}
#' \item{n_obs}{Number of years over which reference prices are computed}
#' \item{ref_yr}{Reference year of the analysis}
#' }
"clcc_prices_ref"
