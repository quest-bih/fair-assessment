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

# Login to server with ssh jtaubitz@s-quest.bihealth.org
# Password
# Run scripts with: Rscript file.R
# Exit session with Ctrl+D

library(ssh)
# Docu https://cran.r-project.org/web/packages/ssh/vignettes/intro.html

# Start ssh session
session <- ssh_connect("jtaubitz@s-quest.bihealth.org", passwd = "bhu8765")
print(session)

# Commands
ssh_exec_wait(session, command = "ls")

# Upload files and scripts
path = "/Users/jan/Documents/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair-assessment/R/04_fair_evaluation_service_test.R"
scp_upload(session, path)

path_guid = "/Users/jan/Documents/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair-assessment/output-Rdata/fuji_guid_2.csv"
scp_upload(session, path_guid)

# ssh_exec_wait(session, command = "-a /System/Applications/TextEdit.app 04_fair_evaluation_service_test.R")


# Run script on server
ssh_exec_wait(session, command = "Rscript 04_fair_evaluation_service_test.R")
# ssh_exec_wait(session, command = "Rscript testscript.R")

# Download file
path_out = "/Users/jan/Documents/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair-assessment/output-Rdata"
scp_download(session, "fair_evaluation_list_test.Rdata", to = path_out)
load("output-Rdata/fair_evaluation_list_test.Rdata")

# scp_download(session, "hello.txt", to = tempdir())

ssh_disconnect(session)
session

# Run R script with screen tool to detach terminal
# 1. run $ screen
# 2. run $ Rscript file.R
# 3. detach from ssh server with ctrl+a, d

