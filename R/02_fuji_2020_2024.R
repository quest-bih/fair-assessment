#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the local FUJI server ----
# Contact: anastasiia.iarkaeva@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)
library(progressr)
library(purrr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data 
source("R/01_rdm_ids_2020_2024.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Function to query local FUJI server with guids
fuji_local_server <- function(rd_id){
  data <- list(object_identifier = rd_id,
               metadata_service_endpoint = "http://ws.pangaea.de/oai/provider",
               metadata_service_type = "oai_pmh",
               use_datacite = TRUE,
               use_github = FALSE,
               use_headless = FALSE,
               metric_version = "metrics_v0.8",
               test_debug = TRUE)
  
  res <- httr::POST(url = 'http://localhost:1071/fuji/api/v1/evaluate', 
                    httr::accept_json(), 
                    httr::content_type_json(),
                    body = data, 
                    encode = "json")  
  
  status <- httr::status_code(res)
  raw_text <- httr::content(res, as = "text", encoding = "UTF-8")
  
  if (status != 200) {
    message("rd_id = ", rd_id, " | status = ", status, " | body = ", raw_text)
    return(NULL)
  }
  
  fuji_local_parsed <- content(res) #, encoding = "UTF-8") # , as = "parsed" removed because it causes problems with some guids
  return(fuji_local_parsed)
}


# Create vector with research data ids
charite_rd_od_guids <- charite_rd_od_cleaned_repos |> pull(data_identifier)

# Test the query function directly 
fuji_local_server('https://doi.org/10.2210/pdb8c99/pdb')

# Safe handling of problematic ids - because there was an entry in resulted list that was NA
set.seed(123)
handlers(global = TRUE) # see progress for the larg set quering
safe_fuji <- possibly(fuji_local_server, otherwise = NA, quiet = FALSE)

# Test the query through safely function
sample_1 <- sample(charite_rd_od_cleaned_repos, 5)
fuji_local_list_test <- map(sample_1$data_identifier[1], safe_fuji, .progress = TRUE)

# Query larger set of ids with map()
fuji_local_list <- map(charite_rd_od_guids, safe_fuji, .progress = TRUE)
#fuji_local_list <- map(charite_rd_od_guids, fuji_local_server)

# Time needed for v0.8 2334 ids: 2026-10-08 17:49 - 18:45

# Save data locally
save(fuji_local_list, file = "output/Rdata/fuji_local_list_2020_2024.Rdata")
load("output/Rdata/fuji_local_list_2020_2024.Rdata")

# Help
# https://curlconverter.com/#r curl translator
# https://developer.ibm.com/articles/what-is-curl-command/ how does curl work
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/