## code to prepare `DATASET` dataset goes here

# loading libraries

pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate, httr, censusapi, jsonlite)


# loading master file

master_data <- read_excel(here("data-raw/db_comm_master_2023_OLD.xlsx"))

# provisional - untill contradr will be restored, the untrader package is used to retrieve trade data

comtradr::set_primary_comtrade_key("2b9783161f3342eda6ec85e4af0487db")

# loading measurement units of IMF prices and performing measurement unit conversion

um_imf <- read_xlsx(here("data-raw/um.xlsx"), sheet = "imf")[, c("imf_code", "um")]

um_p <- read_xlsx(here("data-raw/um.xlsx"), sheet = "p") # oading measurement unit conversion factors for price data

# defining reference year and horizon length

ref_yr <- 2022

h <- 10

# downloading GDP deflators (Euro area for Eurostat and Comext data and US for Comtrade and USITC data)

gdp_defl_raw <- rdb(c("OECD/MEI/EA20.NAGIGP01.IXOBSA.A",
      "OECD/MEI/USA.NAGIGP01.IXOBSA.A"))

gdp_defl <- gdp_defl_raw[, c("original_period", "LOCATION", "value")]

names(gdp_defl) <- c("year", "cur", "defl")

gdp_defl$year <- as.numeric(gdp_defl$year)

gdp_defl$cur <- ifelse(gdp_defl$cur == "USA", "usd", "eur")


# downloading USD-EUR exchange rate from the European Central Bank database

exc_rate_raw <- rdb("ECB/EXR/A.USD.EUR.SP00.A")

exc_rate <- exc_rate_raw[, c("original_period", "value")]

names(exc_rate) <- c("year", "exc_rate")

exc_rate$year <- as.numeric(exc_rate$year)


#### downloading comtrade data ####

comtrade_codes <- subset(master_data, !is.na(comtrade_code))$comtrade_code

comtrade_codes <- unique(as.character(comtrade_codes)) # list of all comtrade codes

# comtrade_codes_l <- split(comtrade_codes, ceiling(seq_along(comtrade_codes)/4))
#
# years_l <- list(c(start_yr = ref_yr - h + 1, end_yr = ref_yr - h/2),
#                 c(start_yr = ref_yr - h/2 + 1, end_yr = ref_yr))
#
# #if (ct_get_remaining_hourly_queries() < length(comtrade_codes_l) * length(years_l)) stop("Superato limite massimo di query orarie")
#
# # defining a function to download comtrade data without infringing API limits
#
# comtrade_raw <- NULL
#
# for (i in 1:length(comtrade_codes_l)) {
#
#   for (j in 1:length(years_l)) {
#
#     comtrade_raw[[paste0(i, "_", j)]] <- comtradr::ct_get_data(
#       reporter = "all",
#       partner = "World",
#       flow_direction = "export",
#       start_date = years_l[[j]][["start_yr"]],
#       end_date = years_l[[j]][["end_yr"]],
#       commodity_code = comtrade_codes_l[[i]]
#     )
#
#     Sys.sleep(5)
#
#   }
#
#   Sys.sleep(5)
#
# }

comtrade_raw <- ct_get_data(
  flow_direction = "export",
  start_date = ref_yr - 9,
  end_date = ref_yr,
  commodity_code = comtrade_codes
)

# tidying data

trade_data_tidy <- subset(comtrade_raw,  # removes flows of less than 100 kg
                          select = c(refYear, reporterISO, partnerISO, cmdCode,
                                     cmdDesc, qty, fobvalue)) %>%
  as_tibble()

names(trade_data_tidy) <- c("year", "reporter_iso", "partner_iso", "commodity_code",
                            "commodity", "netweight_kg", "trade_value_usd")

if (!setequal(unique(trade_data_tidy$commodity_code), comtrade_codes)) stop ("The Comtrade query did not return all selected codes")

# Calculating unit values

price_comtrade_def <- trade_data_tidy %>%
  group_by(year, commodity_code) %>%
  summarize(across(c(netweight_kg, trade_value_usd), \(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(price = trade_value_usd / netweight_kg) %>%
  select(year, code = commodity_code, price)

# Adding missing rows (some commodities might not have values for every year)

uv_grid <- expand_grid(year = seq((ref_yr - h + 1), ref_yr, 1),
                              code = comtrade_codes)


price_comtrade_def <- uv_grid %>%
  left_join(price_comtrade_def) %>%
  mutate(source = "comtrade",
         cur = "usd")

#### Downloading data from the IMF commodity price database using the rdbnomics database####

codes_imf <- subset(master_data, source == "imf")$imf_code %>%
  unique()

codes_imf <- paste0("IMF/PCPS/A.W00.", codes_imf, ".USD") # creating codes compatible with the rdbnomics package

price_imf_raw <- rdb(codes_imf) # prices in raw data form

price_imf_raw <- price_imf_raw[, c("original_period", "COMMODITY", "value")]

names(price_imf_raw) <- c("year", "code", "price_usd")

# preparing the final tibble (mu conversion)

price_imf_def <- price_imf_raw %>%
  left_join(um_imf,
            by = c("code" = "imf_code")) %>%
  rename(um_from = um) %>%
  left_join(um_p) # adding original measurement units to price series in USD

price_imf_def <- price_imf_def %>%
  mutate(price = price_usd / fct,
         year = as.numeric(year),
         source = "imf",
         cur = "usd") %>%
  select(!c(um_from, um_to, fct, price_usd))

#### Loading Comext data ####

comext <- read_delim(here("data-raw/comext.csv"))

# selecting imports and flows regarding the EU as a whole

comext_raw <- dplyr::rename_with(comext, tolower) # variable names in lowercase

# comext_raw <- subset(comext_raw, flow == 1 &
#                            reporter == "EU" & partner == "EU_EXTRA") # selecting imports

comext_raw <- comext_raw %>%
  mutate(year = as.numeric(stringr::str_sub(as.character(period), 1, 4)),
         period = NULL)

# Comext prices expressed in euro/kg (current)

comext_raw <- comext_raw[, c("product", "indicators",
                                     "indicator_value", "year")]

comext_raw <- subset(comext_raw, year > ref_yr - 10)

comext_raw <- tidyr::pivot_wider(comext_raw,
                                     names_from = indicators,
                                     values_from = indicator_value)

price_comext_def <- comext_raw %>%
  mutate(price = VALUE_IN_EUROS / QUANTITY_IN_100KG / 100,
         code = as.character(product),
         source = "comext",
         cur = "eur",
         VALUE_IN_EUROS = NULL,
         QUANTITY_IN_100KG = NULL,
         product = NULL
  )

#### Loading USITC data ####

census_key <- "116b4815204719a5aeb6a6e4ab6a69fc56ffee54"

# direct API Interrogation

usitc_codes <- master_data %>%
  filter(source == "usitc") %>%
  pull(usitc_code) %>%
  unique()

base_url <- "https://api.census.gov/data/timeseries/intltrade/imports/hs"

vars <- c("CTY_CODE", # countru code
         "CTY_NAME", # country name
         "GEN_VAL_MO", # total value, general imports
         "DUT_VAL_MO", # Duitable value
         "GEN_CIF_MO", # CIF value
         "GEN_QY1_MO", # General imports, quantity 1
         "UNIT_QY1", # general quantity unit
         "I_COMMODITY", # commodity code
         "I_COMMODITY_SDESC" # short commodity description
)

vars <- paste0(vars, collapse = ",")

years <- seq(to = ref_yr, from = ref_yr - 9, by = 1)

commodities <- map_chr(usitc_codes,
                   ~ paste0("&I_COMMODITY=",
                            .x)) %>%
  paste0(collapse = "")

# codice_prova <- paste0(base_url,
#        "?get=",
#        vars,
#        #"&key=",
#        #census_key,
#        "&YEAR=",
#        2021,
#        commodities
# )



#GET(codice_prova)


prices_usitc_raw <- map_dfr(years,
         function(x) {

           query_url <- paste0(base_url,
                               "?get=",
                               vars,
                               "&key=",
                               census_key,
                               "&YEAR=",
                               x,
                              commodities
           )

           usitcs_raw <- GET(query_url)

           usitcs_df <- fromJSON(rawToChar(usitcs_raw$content), flatten = TRUE) %>%
             as.data.frame()


           colnames(usitcs_df) <- usitcs_df[1,]

           usitcs_df <- usitcs_df[-1, ]

           as_tibble(usitcs_df)


         })

# library(rvest)
# library(tidyverse)
# endpoint_url <- codice_prova # Replace with your JSON url
# html_nodes(read_html(endpoint_url), "body") %>% as.character %>% writeLines("file.txt")
# readLines("file.txt")


price_usitc_def <- prices_usitc_raw %>%
  as_tibble() %>%
  filter(CTY_CODE == "-") %>%
  group_by(YEAR, I_COMMODITY, UNIT_QY1) %>%
  summarise(value = sum(as.numeric(GEN_VAL_MO), na.rm = T),
         quantity = sum(as.numeric(GEN_QY1_MO), na.rm = T)) %>%
  ungroup() %>%
  rename(year = YEAR, code = I_COMMODITY) %>%
  mutate(price = case_when(
    UNIT_QY1 == "T" ~ value / (quantity * 1000),
    UNIT_QY1 == "CGM" ~ value / (quantity / 1000),
    UNIT_QY1 == "KG" ~ value / quantity
  ),
  year = as.numeric(year),
  source = "usitc",
  cur = "usd")

#### CREATING AN UNIQUE DATABASE OF REFERENCE PRICES ####

prices_all <- bind_rows(price_comtrade_def, price_comext_def,
          price_imf_def, price_usitc_def) %>%
  as_tibble()

# adjusting for inflation

prices_all <- prices_all %>%
  left_join(gdp_defl) %>%
  group_by(code, source) %>%
  mutate(price_k = price / defl * defl[year == ref_yr]) %>%
  ungroup()

# converting price into euros

exc_rate_ref <- subset(exc_rate, year == ref_yr)$exc_rate

prices_all <- prices_all %>%
  mutate(price_eur = if_else(cur == "usd",
                             price_k / exc_rate_ref,
                             price_k)) %>%
  select(!c(price, price_k, cur, defl))

# Computing average and minimum and maximum prices of the last 10 years

prices_all <- prices_all %>%
  filter(year > ref_yr - h)

ref_prices <-  prices_all %>%
  group_by(code, source) %>%
  summarise(mean = mean(price_eur, na.rm = T),
            min = min(price_eur, na.rm = T),
            max = max(price_eur, na.rm = T),
            n_obs = sum(!is.na(price_eur)) # number of years over which the average is actually computed
          ) %>%
  ungroup()

#### PREPRARING FINAL TIBBLE WITH COMMODITY NAMES AND REFERENCE PRICES

# manipulating the comm_key tibble to make it tidy and ready to be joined with prices

comm_key_tidy <- mutate(master_data,
                               across(c(comtrade_code,
                                               comext_code, usitc_code), as.character))

comm_key_tidy$quandl_code <- NULL

#comm_key_tidy$none_code <- NA_character_


comm_key_tidy <- tidyr::pivot_longer(comm_key_tidy, tidyselect::ends_with("code"),
                                     names_to = "source_code", values_to = "code",
                                     names_pattern = "(.*)_code")

comm_key_tidy <- comm_key_tidy %>%
  mutate(source_code = ifelse(source == "none",
                              "none",
                              source_code),
         code = ifelse(source == "none",
                       NA_character_,
                       code)) %>%
  unique()

comm_key_tidy <- unique(subset(comm_key_tidy, source == source_code))

comm_key_tidy$source_code <- NULL

# Joining price_ref and comm_key tibbles to get the final dataset to be used in the analysis

clcc_prices_ref_old <- comm_key_tidy %>%
  left_join(ref_prices)

clcc_prices_ref_old <- clcc_prices_ref_old %>%
  mutate(across(mean:max, ~ if_else(is.na(.x), 0, .x)),
         ref_yr = ref_yr)

# checking if the call retrieved data for all years (comtrade data only)

check_comtrade_l_old <- clcc_prices_ref_old %>%
  filter(source == "comtrade") %>%
  group_by(no_comm) %>%
  group_split()

length_check <- sapply(check_comtrade_l, function (x) x[["n_obs"]] != h)

if (sum(length_check != 0)) stop ("The comtrade query did not return data for all years selected for at least one commodity")


usethis::use_data(clcc_prices_ref_old, overwrite = TRUE)

#usethis::use_data(simapro_template, simapro_codes, overwrite = TRUE, internal = TRUE)
