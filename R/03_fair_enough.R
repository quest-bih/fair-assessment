#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the FAIR Enough server ----
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

# Load GUIS from FUJI Assessment
load("output-Rdata/fuji_guid.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FAIR enough data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# API function to query FAIR Enough server with ids
fair_enough_server <- function(rd_id) {
  headers = c(`accept` = 'application/json',
              `Content-Type` = 'application/json')
  
  data <- list(resource_uri = rd_id,
               collection = "fair-metrics")
  
  res <-
    httr::POST(
      url = 'https://api.fair-enough.semanticscience.org/rest/evaluations',
      httr::add_headers(.headers = headers),
      body = data,
      encode = "json"
    )
  
  fair_enough_parsed <- content(res, as = "parsed")
}

# Query larger set of ids with map()
# Use slowly() to set rate limit
rate <- rate_delay(10)
slow_fair_enough <-
  slowly(fair_enough_server, rate = rate, quiet = FALSE)

test_fair_enough_slowly <-
  map(fuji_guid$guid, slow_fair_enough)

# Save data locally
save(test_fair_enough_slowly, file = "output-Rdata/fair_enough_slowly_list.Rdata")
load("output-Rdata/fair_enough_slowly_list.Rdata")

# NCmisc::list.functions.in.file("fair_assessments/03_fair_enough.R")

