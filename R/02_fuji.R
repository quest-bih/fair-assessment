#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the local FUJI server ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data 
source("R/01_rdm_ids.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Test guid´s status codes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Test guids
url_tester <- function(url_list){
  url_list %>%
    # Create a possibly() version of GET() that would otherwise return NULL 
    map( possibly(GET, NULL) ) %>%
    # Set the names of the result
    set_names( url_list ) %>%
    # Remove the NULL
    #  compact() %>%
    # Extract all the "status_code" elements
    map("status_code")
}

# Try this function on the urls object
url_test <- url_tester(charite_rd_2020_guid )

save(url_test, file = "output/Rdata/url_test.Rdata")
load("output/Rdata/url_test.Rdata")

url_test_df <- url_test %>% tibble() %>% mutate(name = names(url_test)) %>% relocate(2, value = 1) %>% filter(value != "200")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

system2(command = "pwd")

system2(command = "docker",
        args    = c("ps -a"))

# Function to query local FUJI server with guids
fuji_local_server <- function(rd_id){
  
  headers = c(
    `accept` = 'application/json',
   # `Authorization` = 'Basic dGVzdDp0ZXN0',
    `Content-Type` = 'application/json')
  
  data <- list(metadata_service_endpoint = "", 
               # metadata_service_endpoint http://ws.pangaea.de/oai/provider gives less scores?!
               metadata_service_type = "oai_pmh",
               object_identifier = rd_id,
               test_debug = TRUE,
               use_datacite = TRUE)
  
  res <- httr::POST(url = 'http://localhost:1071/fuji/api/v1/evaluate', httr::add_headers(.headers=headers), body = data, encode = "json")  
  
  fuji_local_parsed <- content(res) # , as = "parsed" removed because it causes problems with some guids
  return(fuji_local_parsed)
}

sample <- sample(charite_rd_2020_guid, 2)
fuji_local_list <- map(sample, fuji_local_server)

rate <- rate_delay(5)
slow_fuji <-
  slowly(fuji_local_server, rate = rate, quiet = FALSE)

fuji_local_list <-
  map(charite_rd_2020_guid, slow_fuji)


# Query larger set of ids with map()
fuji_local_list <- map(charite_rd_2020_guid, fuji_local_server)

# Save data locally
save(fuji_local_list, file = "output/Rdata/fuji_local_list_2022_03_04.Rdata")
load("output/Rdata/fuji_local_list_2022_03_04.Rdata")

# Filter vector with dplyr 
# https://stackoverflow.com/questions/44169164/dplyr-filter-on-a-vector-rather-than-a-dataframe-in-r
# charite_rd_2020_guid %>% .[matches("qak8e", vars=.)]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract only GUID to query FAIR Enough and Fair Evaluation Service ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_guid <- map_dfr(seq_along(fuji_local_list), 
                                ~ bind_rows(fuji_local_list[[.x]][["results"]][[1]][["output"]]) %>%
                                  mutate(rd_id = fuji_local_list[[.x]][["request"]][["object_identifier"]])) %>%
                                  mutate(guid = str_trim(guid)) %>%
                                  select(!guid_scheme)

# test <- fuji_guid %>% filter(guid != rd_id)
                                
save(fuji_guid, file = "output/Rdata/fuji_guid.Rdata")

# Help
# https://curlconverter.com/#r curl translater
# https://developer.ibm.com/articles/what-is-curl-command/ how does curl work
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/