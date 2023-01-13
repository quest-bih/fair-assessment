#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script imports the Open Data records, after ODDPub, Numbat and manual verification ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(fuzzyjoin)
library(openxlsx)
library(readxl)
library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data on research data sets from xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path <- "input/joint_data_cleaned_updated_2021.csv"
  
charite_rd_2021 <- read_delim(path, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

charite_rd_2021 <- charite_rd_2021 |>
  filter(open_data_assessment == "open_data") |>
  select(article = doi,
         best_identifier = data_identifier,
         repository,
         license_name) |>
  mutate(repository = case_when(best_identifier == "http://www.addgene.org/129027/" ~ "Addgene",
                                best_identifier == "http://doi.org/10.5281/zenodo.5524539" ~ "Zenodo",
                                repository == "zenodo" ~ "Zenodo",
                                repository == "osf" ~ "Open Science Framework",
                                str_detect(repository, "^GWAS.*") ~ "GWAS",
                                TRUE ~ repository))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load dataframe with repository names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

repository_names <- read_delim("input/re3data_categories.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

repository_names <- repository_names |>
  select(repository_type, repository) |>
  add_row(repository_type = "general-purpose repository", repository = "Fairdata.fi")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join dataframes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2021 <- charite_rd_2021 |>
  regex_left_join(repository_names, by = c(repository = "repository"), ignore_case = TRUE) |>
  rename(repository = repository.x) |>
  select(-repository.y) |>
  distinct(best_identifier, .keep_all = TRUE)

charite_rd_2021 <- charite_rd_2021 |>
  mutate(repository = case_when(repository == "ClinVAR;NCBI Nucleotide" ~ "NCBI Nucleotide",
                                TRUE ~ repository)) |>
  mutate(repository_re3data = repository) |>
  mutate(repository = case_when(repository_re3data == "European Nucleotide Archive" ~ "ENA",
                                repository_re3data == "Gene Expression Omnibus" ~ "GEO",
                                repository_re3data == "Mass Spectrometry Interactive Virtual Environment" ~ "MassIVE",
                                repository_re3data == "Open Science Framework" ~ "OSF",
                                repository_re3data == "Proteomics Identifications Database" ~ "PRIDE",
                                repository_re3data == "Sequence Read Archive" ~ "SRA",
                                repository_re3data == "The Electron Microscopy Data Bank" ~ "EMDB",
                                repository_re3data == "Worldwide Protein Data Bank" ~ "wwPDB",
                                TRUE ~ repository))


test <- charite_rd_2021 |>
  count(repository, repository_re3data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make some cleanups and create a new column ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2021 <- charite_rd_2021 |>
  # Remove white spaces
  mutate(across(where(is.character), str_trim)) |>
  # Clean DOIs
  mutate(article = str_replace_all(article, "%28", "("), 
         article = str_replace_all(article, "%29", ")")) |>
  mutate(best_identifier = str_replace(best_identifier, "^https:\\/d", "https:\\/\\/d")) 

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

rm(path, repository_names, test)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
