#' Master file with commodity keys
#'
#' A datataset containing all raw materials considered for the computation of the
#' CLCC indicator and the various sources from which it is possible to gather price
#' data for each commodity. The dataset also provides information on the preferred
#' data source for each commodity.
#'
#' @format A tibble with variables:
#' \describe{
#' \item{comm}{commodity names, as in SimaPro output}
#' \item{no_comm}{commodity ID}
#' \item{um}{measurement unit, as in SimaPro output}
#' \item{comtrade_code}{Comtrade HS 6-digit code}
#' \item{usitc_code}{USITC 8-digit code}
#' \item{comext_code}{COMEXT 8-digit code}
#' \item{ds_code}{Refinitiv Datastream code}
#' \item{imf_code}{IMF code}
#' \item{eurostat_code}{Eurostat code}
#' \item{source}{Reference price source}
#' \item{critical}{A critical material for the EU, yes/no}
#' }
#'
"db_comm_master"


#' Unit values for some rare earths materials (Comext)
#'
#' A dataset containing unit values (a proxy for market prices) for some rare
#' earths materials included in the CLCC indicator. Data are from the Comext database,
#' which provides detailed trade data for EU countries. The raw data cannot be
#' downloaded through an API, therefore are provided through a spreadsheet
#' file.
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year, 10 year period before reference year}
#' \item{cur}{currency (EUR)}
#' \item{source}{data source}
#' \item{code}{code, 8-digit HS}
#' \item{price}{unit value}
#' }
#'
"price_comext_def"


#' Unit values for some raw materials (Comtrade)
#'
#' A dataset containing unit values (a proxy for market prices) for some raw materials
#' included in the CLCC indicator. Data are from the UN's Comtrade database, which
#' provides trade data (6-digit level). Data care downloaded through the UN comtrade API
#' (see data-raw folder for the script used for downloading the series).
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year, 10 year period before reference year}
#' \item{cur}{currency (USD)}
#' \item{source}{data source}
#' \item{code}{code, 6-digit HS}
#' \item{price}{unit value}
#' }
#'
"price_comtrade_def"


#' Electricity prices - European Union average
#'
#' A dataset containing electricity prices, to be used in the CLCC indicator calculation
#' as a proxy for the price of electric energy from renewable sources. Data are
#' from the Eurostat database and refer to the EU and to the price paid by a large
#' idustrial plant. Data are downloaded through the Eurostat API (see data-raw
#' folder for the script used for downloading the series).
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year, 10 year period before reference year}
#' \item{cur}{currency (EUR)}
#' \item{source}{data source}
#' \item{code}{code, eurostat database}
#' \item{price}{price, excluding taxes and levies}
#' }
#'
"price_eurostat_def"

#' Primary commodity prices from the IMF
#'
#' A dataset containing primary commodity prices from the IMF database. Such prices
#' are used as market prices for some raw materials included in the CLCC indicator.
#' Prices are downloaded through the Nasdaq Data Link platform (API) since the IMF
#' is under maintennance (see data-raw folder for the script used for downloading the series).
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year, 10 year period before reference year}
#' \item{cur}{currency (USD)}
#' \item{source}{data source}
#' \item{code}{code, IMF database}
#' \item{price}{price}
#' }
#'
"price_imf_def"


#' Unit values for some rare earths materials (USITC)
#'
#' A dataset containing unit values (a proxy for market prices) for some rare
#' earths materials included in the CLCC indicator. Data are from the US International
#' Trade database,which provides detailed trade data for the US. The raw data cannot be
#' downloaded through an API, therefore are provided through a spreadsheet
#' file.
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year, 10 year period before reference year}
#' \item{cur}{currency (USD)}
#' \item{source}{data source}
#' \item{code}{code, 10-digit HS}
#' \item{price}{unit value}
#' }
#'
"price_usitc_def"


#' Measurement units of original IMF commodity prices
#'
#' Primary commoidty prices come in some specific measurement units, which
#' need to be converted in either kg, m3 or MJ, the measurement units
#' used for the calculation of the CLCC indicator. The conversion is done using
#' the *units* package but this dataset provides information on the original
#' units and those to which original data must be converted.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{um_from}{original measurement unit}
#' \item{um_to}{measurement units to which original data mist be converted)}
#' \item{fct}{conversion factor, not used}
#' }

"um_p"
