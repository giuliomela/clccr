library(rvest)
library(tidyverse)


endpoint_url <- paste0(base_url,
                       "?get=",
                       vars,
                       "&key=",
                       census_key,
                       "&YEAR=",
                       c(2013:2022),
                       commodities
)


html_nodes(read_html(endpoint_url[10]), "body") %>% as.character %>% writeLines("file.txt")
readLines("file.txt")

# critical_key <- master_data[, c("comm", "critical")]
#
# clcc_prices_ref <- clccr::clcc_prices_ref %>%
#   select(!critical) %>%
#   left_join(critical_key)
