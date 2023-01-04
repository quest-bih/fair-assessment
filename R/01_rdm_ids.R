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

path <- "input/open_data_datasets_for_fair_assessment_2020.xlsx"  
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

charite_rd_2020_clean <- charite_rd_2020 %>%
  # Analyze only own open data
  filter(own_or_reuse_data == "own_open_data") %>%
  select(article,
         identifier,
         best_identifier,
         repository,
         repository_type = open_data_category) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(c(article, starts_with("repository")), str_to_lower)) %>%
  # Clean DOIs
  mutate(article = str_replace_all(article, "%28", "("), 
         article = str_replace_all(article, "%29", ")")) %>%
  # If applicable, use best_identifier
  mutate(best_identifier = case_when(is.na(best_identifier) ~ identifier,
                                     TRUE ~ best_identifier)) %>%
  filter(str_detect(best_identifier, "^http|^10|^doi")) %>%
  # Classify repository type
  mutate(
    repository_type = case_when(
      str_detect(repository, is_field_specific) ~ "field-specific repository",
      str_detect(repository, is_general_purpose) ~ "general-purpose repository",
      TRUE ~ repository_type
    )) %>%
  select(-identifier)

# Create vector with best identifiers for querying assessment tools
charite_rd_2020_guid <- charite_rd_2020_clean %>%
  pull(best_identifier) %>%
  unique()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Function to save df output as xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

save_data_xlsx <- function(df, name) {
  if(class(df) != "list" ) stop("Df must be a list")
  wb <- createWorkbook()
  
  for (i in seq_along(df)) {
  addWorksheet(wb, sheetName = name[i])
  writeData(wb, sheet = name[i], x = df[[i]], keepNA = TRUE)
  }
  
  saveWorkbook(wb, paste0("output/", name[1], ".xlsx"), overwrite = TRUE)
}

# save_data(df = list(charite_rd_2020_clean), name = c("input_clean"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove unused objects ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(path, charite_rd_2020, is_field_specific, is_general_purpose)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# library(rcrossref)
# library(roadoi)
# 
# # Create test data
# test_data <- head(charite_rd_2020_clean$article, 10)
# 
# # Query unpaywall
# test_unpaywall <- roadoi::oadoi_fetch(dois = test_data,
#                                       email = "jan.taubitz@charite.de",
#                                       .progress = "text")
# 
# # Query crossref
# test_crossref <- id_converter(test_data, type = "doi")
# # OR
# test_crossref <- id_converter(test_unpaywall$doi, type = "doi")
# 
# # Flatten crossref data
# test_crossref <- test_crossref[["records"]] %>% bind_cols() %>% select(doi, pmid)
# 
# # Join unpaywall and crossref data
# test_result <- test_unpaywall %>%
#   select(doi, journal_name) %>%
#   full_join(test_crossref %>% mutate(doi = tolower(doi)), by = "doi") %>%
#   relocate(pmid, .after = doi)

