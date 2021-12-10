#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the FAIR Evaluation Service server ----
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
# FAIR Evaluation data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fair_evaluation_server <- function(guid) {
  headers = c(`Accept` = 'application/json',
              `Content-Type` = 'application/json')

  data = list(resource = guid,
              executor = "0000-0002-4968-8622",
              title = "charite_assessment")
  
  res <-
    httr::POST(
      url = 'https://fair-evaluator.semanticscience.org/FAIR_Evaluator/collections/22/evaluate',
      httr::add_headers(.headers = headers),
      body = data,
      encode = "json"
    )
  
  fair_evaluation_parsed <-
    content(res, as = "parsed")
  
} 

# Query larger set of ids with map()
# Use slowly() to set rate limit
rate <- rate_delay(10)
fair_evaluation_server_slowly <- slowly(fair_evaluation_server, rate = rate, quiet = FALSE)
fair_evaluation_data <- map(fuji_guid$guid, fair_evaluation_server_slowly)

save(fair_evaluation_data, file = "output-Rdata/fair_evaluation_data.Rdata")
load("output-Rdata/fair_evaluation_data.Rdata")


