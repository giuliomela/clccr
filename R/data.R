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
"master_data"


#' Unit values from Comext (EU imports at 8-digit level)
#'
#' A dataset containing unit values (a proxy for market prices) for some
#' materials included in the CLCC indicator. Data are from the Comext database (imports),
#' which provides detailed trade data for EU countries. The raw data cannot be
#' downloaded through an API, therefore are provided through a spreadsheet
#' file.
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year}
#' \item{cur}{currency (EUR)}
#' \item{source}{data source}
#' \item{code}{code, 8-digit HS}
#' \item{price}{euro/kg, current}
#' }
#' @source \url{http://epp.eurostat.ec.europa.eu/newxtweb/}
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
#' \item{year}{year}
#' \item{cur}{currency (USD)}
#' \item{source}{data source}
#' \item{code}{code, 6-digit HS}
#' \item{price}{unit value, usd/kg, current}
#' }
#' @source \url{https://comtradeplus.un.org/}
"price_comtrade_def"


#' Electricity prices - European Union average
#'
#' A dataset containing electricity prices, to be used in the CLCC indicator calculation
#' as a proxy for the price of electric energy from renewable sources. Data are
#' from the Eurostat database (provided by DB.Nomics) and refer to the EU and to the price paid non -domestic users
#' according to different consumption levels. Data are downloaded through the DB.Nomics API (see data-raw
#' folder for the script used for downloading the series).
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year}
#' \item{cur}{currency (EUR)}
#' \item{cons_label}{label identifying annual consumption level}
#' \item{cons_code}{code, code identifying annual consumption level}
#' \item{price}{price, excluding taxes and levies, euro/MJ, current}
#' }
#' @source \url{https://db.nomics.world/Eurostat/nrg_pc_205}
"price_eurostat_def"

#' Primary commodity prices from the IMF
#'
#' A dataset containing primary commodity prices from the IMF database. Such prices
#' are used as market prices for some raw materials included in the CLCC indicator.
#' Prices are downloaded through through the DB.Nomics API.
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year}
#' \item{cur}{currency, (USD)}
#' \item{source}{data source}
#' \item{code}{code, IMF database}
#' \item{price}{price, current usd/kg - usd/m3 according to the commodity considered}
#' }
#' @source \url{https://db.nomics.world/IMF/PCPS}
"price_imf_def"

#' Unit values for materials (USITC)
#'
#' A dataset containing unit values (a proxy for market prices) for some
#' materials included in the CLCC indicator. Data are from the US International
#' Trade database,which provides detailed trade data for the US. The raw data cannot be
#' downloaded through an API, therefore are provided through a spreadsheet
#' file.
#'
#' @format A tibble with five variables:
#' \describe{
#' \item{year}{year}
#' \item{cur}{currency (USD)}
#' \item{source}{data source}
#' \item{code}{code, 10-digit HS}
#' \item{price}{unit value, usd/kg, current}
#' }
#' @source \url{https://dataweb.usitc.gov/}
"price_usitc_def"

#' USD-EUR Nominal exchange rate
#'
#' Mean annual nominal exchange rate: USD per EUR. Data are retrived from the
#' Eurostat database. See script in data-raw for details.
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{year}{year}
#' \item{exc_rate}{USD per 1 euro}
#' }
#' @source \url{https://db.nomics.world/OECD/MEI}
"exc_rate"

#' Euro Area GDP deflator
#'
#' Annual GDP deflator for the Euro Area. Base year 2015.
#'
#' @format A tibble with two variables
#' \describe{
#' \item{year}{year}
#' \item{cur}{Country to which the GDP deflator refers to}
#' \item{gdp_defl}{GDP deflator}
#' }
#' @source \url{https://db.nomics.world/OECD/MEI}
"gdp_defl"

