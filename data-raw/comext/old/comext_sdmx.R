
pacman::p_load(tidyverse, here, readxl, comtradr, rdbnomics,
               lubridate, httr, censusapi, jsonlite, rsdmx)

# Adding Comext to Service providers in the rsdmx package

comext_builder <- SDMXRequestBuilder(
  regUrl = "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1",
  repoUrl = "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1",
  formatter = list(
    dataflow = function(obj){
      #format each dataflow id with some prefix
      obj@resourceId <- paste0("df_",obj@resourceId)
      return(obj)
    },
    datastructure = function(obj){
      #do nothing
      return(obj)
    },
    data = function(obj){
      #format each dataset id with some prefix
      obj@flowRef <- paste0("data_",obj@flowRef)
      return(obj)
    }
  ),
  handler = list(
    dataflow = function(obj){
      req <- sprintf("%s/dataflow",obj@regUrl)
      return(req)
    },
    datastructure = function(obj){
      req <- sprintf("%s/datastructure",obj@regUrl)
      return(req)
    },
    data = function(obj){
      req <- sprintf("%s/data",obj@regUrl)
      return(req)
    }
  ),
  compliant = TRUE
)

#create the provider
comext_provider <- SDMXServiceProvider(
  agencyId = "COMEXT",
  name = "International trade of the EU - COMEXT",
  builder = comext_builder
)

#add it to the list
addSDMXServiceProvider(comext_provider)

dataset_comext <- rsdmx::readSDMX(providerId = "COMEXT",
                                 resource = "dataflow") |>
  as_tibble()

# Verifico la struttura dei dati

dsd_comext <- rsdmx::readSDMX(
  providerId = "COMEXT",
  resource = "datastructure",
  resourceId = "DS-045409")

data_str_comext <- slot(dsd_comext, "datastructures")

slotNames(slot(data_str_comext[[1]], "Components"), "Attributes") |>
  as.data.frame()

dimensions_comext <- slot()


components_comext <- slot(slot(datastructures_comext[[1]], "datastructures")[[1]], "Components")

dimensioni <- as.data.frame(components)

dimensioni <- dimensioni |>
  filter(component == "Dimension")  |>
  select(component, conceptRef, codelist)

codici <- map(
  dimensioni$codelist,
  \(x) as.data.frame(slot(data_str, "codelists"), codelistId = x) |>
    pull(id, label.it)
) |> set_names(dimensioni$codelist)



comext_raw <- rsdmx::readSDMX(
  providerId = "ISTAT",
  resource = "data",
  flowRef = "DS-045409",
  start = 2022,
  end = 2022,
  dsd = F
)


