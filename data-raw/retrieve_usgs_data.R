# Caricamento dei pacchetti
library(httr)
library(jsonlite)
library(dplyr)
library(utils)
library(here)
library(tidyverse)

# Prezzi da inserire manualmente

cesium_latest <- 124

wollastonite_latest <- NA_real_ # prezzo 2024 non disponibile

zeolite_latest <- 175

ref_yr <- 2024

# ID dell'elemento ScienceBase per Mineral Commodity Summaries latest
sb_id <- "677eaf95d34e760b392c4970" # questo id si trova nell'url della pagina https://www.sciencebase.gov/catalog/item/677eaf95d34e760b392c4970

# "65a6e45fd34e5af967a46749"


# URL API di ScienceBase per ottenere i metadati dei file
sb_url <- paste0("https://www.sciencebase.gov/catalog/item/", sb_id, "?format=json")

# Effettua la richiesta GET per ottenere i dettagli dell'elemento
response <- GET(sb_url)

# Controlla se la richiesta è andata a buon fine
if (status_code(response) == 200) {
  data <- content(response, as = "parsed", type = "application/json")
  files <- data$files
} else {
  print(paste0("Impossibile scaricare i file, errore: ", status_code(response)))
}

  # Creare un dataframe con i file disponibili
  file_list <- data.frame(
    Name = sapply(files, function(x) x$name),
    URL = sapply(files, function(x) x$url)
  )

  # Filtra solo i file ZIP
  zip_files <- file_list |> filter(grepl("\\.zip$", Name))

    # Creare una cartella per i file estratti se non esiste
    output_dir <- here::here(
      "data-raw",
      "usgs"
      )

    if (!dir.exists(output_dir)) dir.create(output_dir)

    salient_url <- zip_files[str_detect(zip_files$Name, regex('salient', ignore_case = T)), ]$URL

    salient_name <- zip_files[str_detect(zip_files$Name, regex('salient', ignore_case = T)), ]$Name

    salient_path <- file.path(
      output_dir,
      salient_name
    )

    # Scarico il file zip con tutti i report in .csv

    download.file(salient_url,
                  salient_path,
                  mode = "wb")

    print(paste("Scaricato:", salient_name))

    # Decomprimo i file

    unzip(salient_path, exdir = here(output_dir, as.character(ref_yr)))

    print(paste("Decompresso:", salient_name))

    #cartella con i file compressi

    folder_name <-
      list.files(here(output_dir, as.character(ref_yr)))

    # Lista di tutti i file CSV estratti
    csv_files <- list.files(
      file.path(here(output_dir, as.character(ref_yr), folder_name)),
      pattern = "\\.csv$", full.names = TRUE)

# Creiamo una lista di dataframe e poi li uniamo in un unico dataframe
df_list <- lapply(csv_files, read_csv)

lista_nomi <- map(
  df_list,
  \(x) colnames(x)
)

# seleziono solamente i data.frame contenenti i prezzi delle commodity

# correggo i nomi dei prezzi del silicio (65) e Titanium Minerals (76)

colnames(df_list[[65]]) <-
  c(
  colnames(df_list[[65]])[1:9],
  paste0("Price_", c("ferrosilicon_50", "ferrosilicon_75", "silicon_metal"), "_ctslb"),
  colnames(df_list[[65]])[13:16]
  )

colnames(df_list[[76]]) <-
  c(
    colnames(df_list[[76]])[1:7],
    paste0("Price_", c("rutile", "ilmenite_leucoxene_aus", "ilmenite_unit_value_import", "slag"), "_dt"),
    colnames(df_list[[76]])[12:13]
  )



df_list_light <- map(
  df_list,
  \(x){

    if ("Commodity" %in% colnames(x)) {

      out <- x |>
        select(DataSource = 1, Commodity, Year, contains("Price", ignore.case = TRUE),
               contains("error", ignore.case = T))

      return(out)

    } else {

      return(NULL)

    }
  }
)

# uniformo i nomi delle colonne per poter fare il bind_rows

all_variable_names <- # tutti i nomi delle variabili che compaiono nei vari database
  map(
    df_list_light,
  \(x) colnames(x)
) |>
  unlist() |>
  unique()


all_vars_df <- # dataframe con tutte le variabili
  map(
    df_list_light,
  \(x){

    out <- x

    mancanti <- setdiff(all_variable_names, colnames(out))

    #return(mancanti)

    for (col in mancanti){

      out[[col]] <- NA
    }

    return(out)

  }
  ) |> bind_rows()

all_prices_tidy <- all_vars_df |>
  mutate(across(!c(DataSource, Commodity, Year), \(x) as.numeric(x))) |>
  pivot_longer(
    !c(DataSource, Commodity, Year),
    names_to = "price_var",
    values_to = "value"
  ) |>
  drop_na() |>
  separate(price_var, into = c("price_type", "um"), sep = "_(?=[^_]+$)") |>
  mutate(price_type = str_remove(price_type, "Price_"))

# Manipolaziona archivio vecchi prezzi

historical_usgs_sheets <-
  readxl::excel_sheets(
    here("data-raw", "usgs", "prices_usgs_historical.xlsx")
  )

historical_usgs <-
  openxlsx::read.xlsx(
    here("data-raw", "usgs", "prices_usgs_historical.xlsx"),
    sheet = "historical"
  ) |>
  as_tibble()

usgs_simapro_corr_table <-
  openxlsx::read.xlsx(
    here("data-raw", "usgs", "prices_usgs_historical.xlsx"),
    sheet = "corr_table"
  ) |>
  as_tibble()

historical_usgs <- historical_usgs |>
  left_join(usgs_simapro_corr_table)

filter_tibble <-
  historical_usgs |>
  select(Commodity, price_type) |>
  unique()

last_yr_corr_table <-
  historical_usgs |>
  select(comm, code, Commodity, price_type) |>
  unique()

last_yr_prices <-
  all_prices_tidy |>
  semi_join(filter_tibble) |>
  filter(Year == ref_yr) |>
  mutate(um = str_remove(um, "d")) |>
  select(!DataSource) |>
  filter(!(Commodity == "Soda Ash" & um == "st"))

last_yr_prices <- last_yr_corr_table |>
  left_join(last_yr_prices) |>
  rename(price = value)

colnames(last_yr_prices) <- str_to_lower(colnames(last_yr_prices))

colnames(historical_usgs) <- str_to_lower(colnames(historical_usgs))


usgs_prices <-
  historical_usgs |>
  bind_rows(last_yr_prices)

# Inserimento manuale prezzi cesio, wollastonite e zeolite

usgs_prices <-
  usgs_prices |>
  mutate(
    year = if_else(is.na(year), ref_yr, year),
    price = case_when(
      is.na(price) & comm == "Cesium" ~ cesium_latest,
      is.na(price) & comm == "Wollastonite" ~ wollastonite_latest,
      is.na(price) & comm == "Zeolite" ~ zeolite_latest,
      TRUE ~ price
    )
  )

usgs_prices <- usgs_prices  |>
  left_join(
    usgs_prices |>
      filter(!is.na(um))  |>
      mutate(year = year + 1)  |>   # Shift year forward to match missing rows
      select(comm, year, um_prev = um),
    by = c("comm", "year")
  )  |>
  mutate(
    um = if_else(is.na(um), um_prev, um)
  )  |>
  select(-um_prev)

# Conversione in kg

um_conv_table <-
  read.delim(
    here(
      "data-raw",
      "um_p.csv"
    ),
    sep = ";"
  )

usgs_prices <- usgs_prices |>
  mutate(
    um = case_when(
      um == "mct" ~ "carat",
      um == "st" ~ "short t",
      um == "to" | um == "troy_ounce" ~ "oz",
      TRUE ~ um
    )
  ) |>
  left_join(um_conv_table, by = c("um" = "um_from")) |>
  mutate(
    price = price / fct,
    um = um_to,
    um_to = NULL,
    fct = NULL
  ) |>
  select(comm, code, um, year, price)


