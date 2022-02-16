#' Compute the reference prices to be used for CLCC calculation
#'
#' This function computes the reference prices to be used for CLCC calculation
#' and Monte Carlo analysis. Prices are expressed in constant euro. The reference
#' price is the mean of a ten-year period starting from the reference year, which
#' is the last year for which data are avilable (this is automatically calculated).
#' Such prices are computed for all raw materials present in the *db_comm_prices*
#' master file.
#'
#' @return A tibble containing raw material names, and mean, minimum and maximum
#' prices of the last ten-year period
#' @export
ref_prices <- function(){

  if(lubridate::month(Sys.Date(), label = FALSE) >= 6){

    ref_year <- lubridate::year(Sys.Date()) - 1

  }else{

    ref_year <- lubridate::year(Sys.Date()) - 2

  }

  # Preparing the final tibble for CLCC calculation ####

  # binding all prices togheter

  prices_all_def <- rbind(price_imf_def, price_comtrade_def,
                          price_comext_def, price_eurostat_def, price_usitc_def)

  prices_all_def <- Reduce(merge, list(prices_all_def, exc_rate_usd, gdp_dfl))

  # converting all prices in euro (constant values)

  prices_all_def <- within(prices_all_def, {
    price_eur <- ifelse(cur == "usd", price / exc_rate, price)
  })

  prices_all_def <- prices_all_def %>%
    dplyr::group_by(code) %>%
    dplyr::mutate(price_eur_k = price_eur / gdp_dfl * gdp_dfl[year == ref_year]) %>%
    dplyr::ungroup()

  prices_all_def <- prices_all_def[, c("code", "year", "source", "price_eur_k")]

  # Calculating baseline and min&max values, to be used for the analysis

  mean_min_max_fn <- function(x){ # a function that calculates min, max and mean values
    out <- aggregate(price_eur_k ~ code + source, prices_all_def, x)
    out[, x] <- out$price_eur_k
    out$price_eur_k <- NULL
    out
  }

  price_ref <- Reduce(merge,
                      lapply(c("mean", "min", "max"), mean_min_max_fn))

  # Preparing final tibble for C-LCC calculation and Monte Carlo analysis ####

  # manipulating the comm_key tibble to make it tidy and ready to be joined with prices

  comm_key_tidy <- dplyr::mutate(db_comm_master,
                                 dplyr::across(c(comtrade_code,
                                                 comext_code, usitc_code), as.character))

  comm_key_tidy$quandl_code <- NULL


  comm_key_tidy <- tidyr::pivot_longer(comm_key_tidy, ends_with("code"),
                                       names_to = "source_code", values_to = "code",
                                       names_pattern = "(.*)_code")

  comm_key_tidy <- within(comm_key_tidy, {
    source_code <- ifelse(source == "none", "none", source_code)
  })

  comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))

  comm_key_tidy$source_code <- NULL

  # Joining price_ref and comm_key tibbles to get the final dataset to be used in the analysis

  clcc_prices_ref <- merge(comm_key_tidy, price_ref, all.x = TRUE)

  clcc_prices_ref <- within(clcc_prices_ref, {
    mean <- ifelse(is.na(mean), 0, mean)
    min <- ifelse(is.na(min), 0, min)
    max <- ifelse(is.na(max), 0, max)
  })

  tidyr::as_tibble(clcc_prices_ref)

}
