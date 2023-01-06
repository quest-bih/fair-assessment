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
source("R/01_rdm_ids_2021.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

# Create vector with research data ids
charite_rd_2021_guids <- charite_rd_2021 |> pull(best_identifier)

# Sample for testing purposes
sample <- sample(charite_rd_2021_guids, 2)
fuji_local_list <- map(sample, fuji_local_server)

# Create rate delay (not necessary for local server)
rate <- rate_delay(1)
slow_fuji <-
  slowly(fuji_local_server, rate = rate, quiet = FALSE)

fuji_local_list <-
  map(charite_rd_2021_guids, slow_fuji)

# Time: 2023-01-05 12:15-

# Query larger set of ids with map()
fuji_local_list <- map(charite_rd_2021_guids, fuji_local_server)

# Save data locally
save(fuji_local_list, file = "output/Rdata/fuji_local_list_2023_01_05.Rdata")
load("output/Rdata/fuji_local_list_2023_01_05.Rdata")


# Help
# https://curlconverter.com/#r curl translater
# https://developer.ibm.com/articles/what-is-curl-command/ how does curl work
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/