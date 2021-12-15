#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script prepares the FAIR Evaluation Service server ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load GUIS from FUJI Assessment
load("output-Rdata/fuji_guid.Rdata")
write_csv(fuji_guid,"output-Rdata/fuji_guid.csv")

# Login to server with ssh jtaubitz@s-quest.bihealth.org
# Password
# Screen
# Run scripts with: Rscript file.R
# Detach with Ctrl+a,d
# Exit session with Ctrl+d


library(ssh)
# Docu https://cran.r-project.org/web/packages/ssh/vignettes/intro.html

# Start ssh session
session <- ssh_connect("jtaubitz@s-quest.bihealth.org", passwd = "...")
print(session)

# Commands
ssh_exec_wait(session, command = "ls")

# Upload files and scripts
path = "/Users/jan/Documents/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair-assessment/R/04_fair_evaluation_service_test.R"
scp_upload(session, path)

path_guid = "/Users/jan/Documents/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair-assessment/output-Rdata/fuji_guid.csv"
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
# 1. start $ ssh
# 2. run $ screen
# 3. run $ Rscript file.R
# 4. detach from ssh server with ctrl+a, d
# 5. close server

# 6. or run $ tmux https://tmuxcheatsheet.com
