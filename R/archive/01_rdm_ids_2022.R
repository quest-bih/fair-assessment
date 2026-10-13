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
library(stringr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data on research data sets ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

path <- "input/OD_LOM_2022_data_ids_OD.csv"
  
charite_rd_2022 <- read_delim(path, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

charite_rd_2022 <- charite_rd_2022 |>
  #filter(open_data_assessment == "open_data") |>
  select(article = doi,
         best_identifier = data_identifier,
         repository) |>
  distinct(best_identifier, .keep_all = T) |>
  #mutate(repository_low = str_trim(tolower(repository)))
  mutate(repository_low = tolower(trimws(repository)))
  # mutate(repository = case_when(best_identifier == "http://www.addgene.org/129027/" ~ "Addgene",
  #                               best_identifier == "http://doi.org/10.5281/zenodo.5524539" ~ "Zenodo",
  #                               TRUE ~ repository))
# repeating ids
#n_occur <- data.frame(table(charite_rd_2022$best_identifier))

charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "doi 10.17605/OSF.IO/85DHZ"] <- "10.17605/OSF.IO/85DHZ"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://zenodo.org/record/4807754"] <- "https://doi.org/10.5281/zenodo.4807754"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?task=02333066ad284481b3d4f5aba3a4d150"] <- "https://doi.org/doi:10.25345/C5CV76"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?task=8cba098f6ba34b8cbb5ab9e3caa4b314"] <- "https://doi.org/doi:10.25345/C57940Z39"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://ega-archive.org/datasets/EGAD00001004459/files"] <- "https://ega-archive.org/datasets/EGAD00001004459"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://ega-archive.org/datasets/EGAD00001007806/files"] <- "https://ega-archive.org/datasets/EGAD00001007806"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD027956-1&test=no"] <- "https://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD027956-1"
charite_rd_2022$best_identifier[charite_rd_2022$best_identifier == "https://zenodo.org/badge/DOI/10.5281/zenodo.4454538.svg)](https://doi.org/10.5281/zenodo.4454538"] <- "https://doi.org/10.5281/zenodo.4454538"

charite_rd_2022 <-charite_rd_2022 |>
  mutate(best_identifier = sub("\\?query.*", "", best_identifier),
         best_identifier = sub("\\?show=reads.*", "", best_identifier),
         best_identifier = sub("&utm_source.*", "", best_identifier)) 

write.csv2(charite_rd_2022,"ids_2022.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load dataframe with repository names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

repository_names <- read_delim("input/re3data_categories.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)

repository_names <- repository_names |>
  select(repository_type, repository) |>
  mutate(repository = ifelse(repository == "open science framework", 
                      "open science framework (osf)", 
                      repository),
         # repository = ifelse(repository == "proteomics identifications database", 
         #                     "Pride Archive ProteomeXchange Consortium", 
         #                     repository),
         repository = ifelse(repository == "gene expression omnibus", 
                             "ncbi gene expression omnibus (geo)", 
                             repository),
         repository = ifelse(repository == "ncbi nucleotide", 
                             "ncbi nucleotide (nuccore)", 
                             repository), 	
         repository = ifelse(repository == "biostudies", 
                             "embl-ebi biostudies", 
                             repository),
         repository = ifelse(repository == "european nucleotide archive", 
                             "embl-ebi european nucleotide archive", 
                             repository) ,
         repository = ifelse(repository == "ncbi sequence read archive",
                             "ncbi sequence read archive (sra)",
                             repository)
         
         ) |>
  filter(repository != "european genome-phenom archive",
         repository != "the european genome-phenome archive",
         repository != "proteomeexchange",
         repository != "gwas") |>
  add_row(repository_type = "general-purpose repository", repository = "Fairdata.fi") |>
  add_row(repository_type = "disciplinary repository", repository = "NCBI Genome") |>
  add_row(repository_type = "disciplinary repository", repository = "edoc-Server") |>
  add_row(repository_type = "disciplinary repository", repository = "jPOSTrepo (Japan ProteOme STandard Repository)") |>
  add_row(repository_type = "disciplinary repository", repository = "Refubium") |>
  add_row(repository_type = "disciplinary repository", repository = "uniprot") |>
  add_row(repository_type = "disciplinary repository", repository = "Leipzig Health Atlas") |>
  add_row(repository_type = "disciplinary repository", repository = "BonaRes Repository") |>
  add_row(repository_type = "disciplinary repository", repository = "UNIMI Dataverse") |>
  add_row(repository_type = "disciplinary repository", repository = "Software Heritage Archive") |>
  add_row(repository_type = "disciplinary repository", repository = "GigaDB") |>
  add_row(repository_type = "disciplinary repository", repository = "Publisso") |>
  add_row(repository_type = "disciplinary repository", repository = "Stanford Digital Repository") |>
  add_row(repository_type = "disciplinary repository", repository = "GIN") |>
  add_row(repository_type = "disciplinary repository", repository = "University of Bristol data repository") |>
  #mutate(repository = tolower(repository))
  mutate(repository = tolower(trimws(repository)))

repos <- repository_names[repository_names$repository %in% charite_rd_2022$repository_low,]

# repository_names_2022 <- repository_names[repository_names$repository %in% repos$repository,] 
# repository_names_2022 <- repository_names_2022 |>
#   mutate(repository = str_trim(tolower(repository)))


#OD_LOM_2022_ids_v3 <- OD_LOM_2022_ids_v2 |> filter(repository %in% repos)

write.csv2(repository_names,"re3data_categories_2022.csv")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join dataframes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
charite_rd_2022 <- charite_rd_2022 |>
  regex_left_join(repository_names, by = c("repository_low" = "repository"), ignore_case = TRUE) |>
  mutate(repository.y = ifelse(is.na(repository.y) & grepl("geo", repository_low), 
                               "ncbi gene expression omnibus (geo)", 
                               repository.y),
         repository.y = ifelse(is.na(repository.y) & grepl("nuccore", repository_low), 
                               "ncbi nucleotide (nuccore)", 
                               repository.y),
         repository_type = ifelse(repository.y == "ncbi gene expression omnibus (geo)",
                                  "disciplinary repository",
                                  repository_type),
         repository_type = ifelse(repository.y == "ncbi nucleotide (nuccore)",
                                  "disciplinary repository",
                                  repository_type)) |>
  rename(repository = repository.x) |>
  select(-repository.y)

charite_rd_2022 <- charite_rd_2022 |>
  mutate(repository = case_when(repository == "ClinVAR;NCBI Nucleotide" ~ "NCBI Nucleotide",
                                repository == "zenodo" ~ "Zenodo",
                                repository == "GWAS cata" ~ "GWAS Catalog",
                                repository == "GWAS" ~ "GWAS Catalog",
                                repository == "SRA" ~ "NCBI Sequence Read Archive",
                                repository == "osf" ~ "OSF",
                                TRUE ~ repository)) |>
  mutate(repository_re3data = repository) #|>
  # mutate(repository = case_when(repository_re3data == "European Nucleotide Archive" ~ "ENA",
  #                               repository_re3data == "Gene Expression Omnibus" ~ "GEO",
  #                               repository_re3data == "Mass Spectrometry Interactive Virtual Environment" ~ "MassIVE",
  #                               repository_re3data == "Open Science Framework" ~ "OSF",
  #                               repository_re3data == "Proteomics Identifications Database" ~ "PRIDE",
  #                               repository_re3data == "Sequence Read Archive" ~ "SRA",
  #                               repository_re3data == "The Electron Microscopy Data Bank" ~ "EMDB",
  #                               repository_re3data == "Worldwide Protein Data Bank" ~ "wwPDB",
  #                               TRUE ~ repository))


test <- charite_rd_2022 |>
  count(repository, repository_re3data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Make some cleanups and create a new column ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2022 <- charite_rd_2022 |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(article = str_replace_all(article, "%28", "("), 
         article = str_replace_all(article, "%29", ")")) |>
  mutate(best_identifier = str_replace(best_identifier, "^https:\\/d", "https:\\/\\/d")) 

count_repos <- charite_rd_2022 |>
  count(repository, sort = T)

write.csv2(count_repos,"count_repos.csv", sep = ";")


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

save_data_xlsx(df = list(charite_rd_2022), name = c("input_clean"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove unused objects ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(path, repository_names)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
