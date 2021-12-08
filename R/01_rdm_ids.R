#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script imports the Open Data records, after ODDPub, Numbat and manual verification ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(readxl)
library(tidyverse)
library(openxlsx)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data on research data sets from xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path <- "~/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/Open Data datasets for FAIR.xlsx"
path <- "input/open_data_datasets_for_fair_assessment.xlsx"  
charite_rd_2020 <- read_excel(path = path, 
                              sheet = "OD 2d round", na = "NA")

# Create new column with best guid for querying assessment tools
charite_rd_2020_clean <- charite_rd_2020 %>%
  filter(own_or_reuse_data == "own_open_data") %>%
  mutate(best_identifier_merge = if_else(is.na(best_identifier), identifier, best_identifier), .after = best_identifier) %>%
  mutate(best_identifier_merge = str_trim(best_identifier_merge)) %>%
  filter(str_detect(best_identifier_merge, "^http|^10|^doi")) %>%
  mutate(repository = str_trim(repository)) %>%
  mutate(repository = str_to_lower(repository)) %>%
  mutate(
    repository_type = case_when(
      str_detect(repository, "ncbi") ~ "field-specific repository",
      str_detect(repository, "code?ocean") ~ "field-specific repository",
      str_detect(repository, "openneuro") ~ "field-specific repository",
      str_detect(repository, "gin") ~ "field-specific repository",
      str_detect(repository, "^dcc") ~ "field-specific repository",
      str_detect(repository, "^synapse") ~ "field-specific repository",
      str_detect(repository, "gigadb") ~ "field-specific repository",
      str_detect(repository, "ghdx") ~ "field-specific repository",
      str_detect(repository, "decipher") ~ "field-specific repository",
      str_detect(repository, "st. jude cloud") ~ "field-specific repository",
      str_detect(repository, "fastgenomics") ~ "field-specific repository",
      str_detect(repository, "physionet") ~ "field-specific repository",
      str_detect(repository, "depmat portal") ~ "field-specific repository",
      str_detect(repository, "23andme") ~ "field-specific repository",
      str_detect(repository, "encode") ~ "field-specific repository",
      str_detect(repository, "speed2") ~ "field-specific repository",
      str_detect(repository, "cptac data portal") ~ "field-specific repository",
      str_detect(repository, "massive ucsd") ~ "field-specific repository",
      str_detect(repository, "ulb center for diabetes research") ~ "field-specific repository",
      str_detect(repository, "broad institute cancer") ~ "field-specific repository",
      str_detect(repository, "gap") ~ "field-specific repository",
      str_detect(repository, "uniprot") ~ "field-specific repository",
      str_detect(repository, "raeslab") ~ "field-specific repository",
      str_detect(repository, "10xgenomics") ~ "field-specific repository",
      str_detect(repository, "ccms") ~ "field-specific repository",
      str_detect(repository, "gpcrmd") ~ "field-specific repository",
      str_detect(repository, "tcga") ~ "field-specific repository",
      str_detect(repository, "addgene") ~ "field-specific repository",
      str_detect(repository, "adhd-200") ~ "field-specific repository",
      str_detect(repository, "clinicaltrials.gov") ~ "field-specific repository",
      str_detect(repository, "nda") ~ "field-specific repository",
      str_detect(repository, "imagen") ~ "field-specific repository",
      str_detect(repository, "proteindiffraction") ~ "field-specific repository",
      str_detect(repository, "midas") ~ "field-specific repository",
      str_detect(repository, "the mnist database") ~ "field-specific repository",
      str_detect(repository, "flowrepository") ~ "field-specific repository",
      str_detect(repository, "gesis") ~ "field-specific repository",
      str_detect(repository, "github") ~ "general-purpose repository",
      str_detect(repository, "cyverse") ~ "general-purpose repository",
      str_detect(repository, "tu datalib") ~ "general-purpose repository",
    #  is.na(open_data_category) ~ "field-specific repository",
      TRUE ~ open_data_category
    ), .after = open_data_category
  ) %>%
  select(-comment_JT,
         -comment,
         -data_statement_ODDPub,
         -best_identifier,
         -assessment,
         -manual_check,
         -open_data_category,
         -own_or_reuse_data) %>%
  relocate(best_identifier = best_identifier_merge, .after = identifier) %>%
  select(-identifier)

# Create vector with best identifiers for querying assessment tools
charite_rd_2020_guid <- charite_rd_2020_clean %>%
  pull(best_identifier) %>%
  unique()

# count_repos <- charite_rd_2020 %>%
#   group_by(repository, repository_type) %>%
#   summarise(count = n()) %>%
#   filter(count >= 5) %>%
#   arrange(-count)
# 
# save_data <- function(path) {
#   wb <- createWorkbook()
#   addWorksheet(wb, "repositories")
#   writeData(wb, "repositories", count_repos, keepNA = TRUE)
#   saveWorkbook(wb, path, overwrite = TRUE)
# }
# 
# path = "output/frequent_repositories.xlsx"
# save_data(path)

# test <- charite_rd_2020 %>%
#   group_by(identifier) %>%
#   filter(n() > 1)
# 
# charite_rd_2020 %>%
#   distinct(own_or_reuse_data)

