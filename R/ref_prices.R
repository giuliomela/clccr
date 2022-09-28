#' Compute the reference prices to be used for CLCC calculation
#'
#' This function computes the reference prices to be used for CLCC calculation
#' and Monte Carlo analysis. Prices are expressed in constant euro. The reference
#' price is the mean of a ten-year period starting from the reference year, which
#' is the last year for which data are available (this is automatically calculated).
#' Such prices are computed for all raw materials present in the *db_comm_prices*
#' master file.
#'
#' @param h A numeric value. Time span (in years) over which calculating average prices.
#'     Default is `10`. Maximum and minimum values admitted are `20` and `5` respectively.
#' @return A tibble containing raw material names, and mean, minimum and maximum
#'     prices of the last ten-year period
#' @export
ref_prices <- function(h = 10){

  if(lubridate::month(Sys.Date(), label = FALSE) >= 6){

    ref_year <- lubridate::year(Sys.Date()) - 1

  }else{

    ref_year <- lubridate::year(Sys.Date()) - 2

  }

  cur <- price <- exc_rate <- code <- year <- comtrade_code <- comext_code <-
    usitc_code <- price_eur <- NULL # to avoid R RMD check creating notes

  # Preparing the final tibble for CLCC calculation ####

  # binding all prices together

  prices_all <- rbind(clccr::price_imf_def, clccr::price_comtrade_def,
                      clccr::price_comext_def, clccr::price_usitc_def)

  if (h < 5 | h > 20) stop ("The time frame over which the average values is calculated must be between 5 and 20 years")

  # attaching gdp_deflator to each price data frame and adjusting price series for inflation

  # ricomincia da qui

  prices_all_l <- split(prices_all, list(prices_all$code, prices_all$cur))

  prices_all_l <- purrr::map(prices_all_l, function (x) {

    data <- merge(x, gdp_defl)
    ref_defl <- subset(data, year == ref_year)$defl
    data$price <- data$price / data$defl * ref_defl
    data$defl <- NULL
    data

  }) # all prices are now at the levels of the reference year

  # converting all prices in euro (constant values)

  exc_rate_ref <- subset(exc_rate, year == ref_year)$exc_rate

  prices_all <- dplyr::bind_rows(prices_all_l)

  prices_all$price_eur <- ifelse(prices_all$cur == "usd",
                                 prices_all$price / exc_rate_ref,
                                 prices_all$price)

  prices_all <- prices_all[, c("code", "year", "source", "price_eur")]

  # sub-setting data (according to time limit)

  prices_all <- subset(prices_all, year > ref_year - h)

  # Calculating baseline and min&max values, to be used for the analysis

  prices_ref <- prices_all %>%
    dplyr::group_by(code, source) %>%
    dplyr::summarize(mean = mean(price_eur, na.rm = T),
           min = min(price_eur, na.rm = T),
           max = max(price_eur, na.rm = T)) %>%
    dplyr::ungroup()


  # mean_min_max_fn <- function(x){ # a function that calculates min, max and mean values
  #   out <- stats::aggregate(price_eur ~ code + source, prices_all, x)
  #   out[, x] <- out$price_eur
  #   out$price_eur <- NULL
  #   out
  # }
  #
  # price_ref <- Reduce(merge,
  #                     lapply(c("mean", "min", "max"), mean_min_max_fn))

  # Preparing final tibble for C-LCC calculation and Monte Carlo analysis ####

  # manipulating the comm_key tibble to make it tidy and ready to be joined with prices

  comm_key_tidy <- dplyr::mutate(master_data,
                                 dplyr::across(c(comtrade_code,
                                                 comext_code, usitc_code), as.character))

  comm_key_tidy$quandl_code <- NULL


  comm_key_tidy <- tidyr::pivot_longer(comm_key_tidy, tidyselect::ends_with("code"),
                                       names_to = "source_code", values_to = "code",
                                       names_pattern = "(.*)_code")

  comm_key_tidy <- within(comm_key_tidy, {
    source_code <- ifelse(source == "none", "none", source_code)
  })

  comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))

  comm_key_tidy$source_code <- NULL

  # Joining price_ref and comm_key tibbles to get the final dataset to be used in the analysis

  clcc_prices_ref <- merge(comm_key_tidy, prices_ref, all.x = TRUE)

  clcc_prices_ref <- within(clcc_prices_ref, {
    mean <- ifelse(is.na(mean), 0, mean)
    min <- ifelse(is.na(min), 0, min)
    max <- ifelse(is.na(max), 0, max)
  })

  tidyr::as_tibble(clcc_prices_ref)

}
