#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script imports the Open Data records, after ODDPub, Numbat and manual verification ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(glue)
library(openxlsx)
library(readxl)
library(tidyverse)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data on research data sets from xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path <- "input/open_data_datasets_for_fair_assessment.xlsx"  
charite_rd_2020 <- read_excel(path = path, 
                              sheet = "OD 2d round", na = "NA")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make some cleanups and create a new column ----
# with the best IDs for querying assessment 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Sort input and create a comma separated vector
# dput(as.character(is_field_specific))
# cat(paste(shQuote(is_field_specific, type="cmd"), collapse=", "))

# Vector with field-specific repositories
is_field_specific <-
  c(
    "^10xgenomics",
    "^23andme",
    "^addgene",
    "^adhd?200",
    "^broad institute cancer",
    "^ccms",
    "^clinicaltrials.gov",
    "^code?ocean",
    "^cptac data portal",
    "^dcc",
    "^decipher",
    "^depmat portal",
    "^encode",
    "^fastgenomics",
    "^flowrepository",
    "^gap",
    "^gesis",
    "^ghdx",
    "^gigadb",
    "^gin$",
    "^gpcrmd",
    "^imagen",
    "^massive",
    "^midas",
    "^ncbi",
    "^nda",
    "^openneuro",
    "^physionet",
    "^proteindiffraction",
    "^raeslab",
    "^speed2",
    "^st. jude cloud",
    "^synapse",
    "^tcga",
    "^the mnist database",
    "^ulb center for diabetes research",
    "^uniprot"
  ) %>%
  glue_collapse(sep = "|")

# Vector with general-purpose repositories
is_general_purpose <-
  c("^cyverse", "^github", "^tu datalib") %>%
  glue_collapse(sep = "|")

# TODO 10.1016/s0140-6736%2820%2930750-9


charite_rd_2020_clean <- charite_rd_2020 %>%
  filter(own_or_reuse_data == "own_open_data") %>%
  mutate(article = str_to_lower(str_trim(article))) %>%
  mutate(article = str_replace_all(article, "%28", "("), 
         article = str_replace_all(article, "%29", ")")) %>%
  mutate(
    best_identifier = if_else(is.na(best_identifier), identifier, best_identifier)) %>%
  mutate(best_identifier = str_to_lower(str_trim(best_identifier))) %>%
  relocate(best_identifier, .after = identifier) %>%
  filter(str_detect(best_identifier, "^http|^10|^doi")) %>%
  mutate(repository = str_to_lower(str_trim(repository))) %>%
  mutate(
    repository_type = case_when(
      str_detect(repository, is_field_specific) ~ "field-specific repository",
      str_detect(repository, is_general_purpose) ~ "general-purpose repository",
      TRUE ~ open_data_category
    )) %>%
  select(article, best_identifier, repository, repository_type)


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



