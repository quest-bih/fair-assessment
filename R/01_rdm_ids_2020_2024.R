#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script imports the Open Data records, after ODDPub, Numbat and manual verification ----
# Contact: anastasiia.iarkaeva@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(fuzzyjoin)
library(openxlsx)
library(readxl)
library(tidyverse)
library(stringr)
#library(here)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data on research data sets ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("input/master_od_screening_manual_check_2020_2024_v1.rda")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare functions to clean data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Decode master dataset table to UTF 8
to_utf8_latin1 <- function(x) {
  if (is.character(x)) {
    iconv(x, from = "latin1", to = "UTF-8", sub = "\uFFFD")
  } else {
    x
  }
}

# Normalize repository names from master file to match - remove white spaces and 
# translate to lower case
norm_repos <- function(x) str_squish(str_to_lower(x))

# Matching the unnormalized repository names to the alias (original/predefined) repo names list
original_repo <- function(x) {
  key <- norm_repos(x)
  alias_key <- match(key, names(repo_alias))
  
  out <- str_squish(x)
  matched <- !is.na(alias_key)
  out[matched] <- unname(repo_alias[alias_key[matched]])
  
  zivahub <- coalesce(str_detect(out, regex("zivahub", ignore_case = TRUE)), FALSE)
  
  out[zivahub] <- "ZivaHub"
  out }


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Decode master file to UTF8 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

master_2020_2024_repos[] <- lapply(master_2020_2024_repos, to_utf8_latin1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Correct some data identifiers in master file ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Exclude IDs that are not (open) data
not_include_datasets <- c(paste0("https://doi.org/10.1371/journal.pone.", 
                                 c("0231299.s003", "0242197.s001", "0239210.s001", "0229260.s003",
                                   "0229661.s006", "0237944.s001", "0241480.s001")),
                          "https://www.ncbi.nlm.nih.gov/clinvar/variation/933149/")

# Make corrections to the articles' DOIs
article_doi_correct <- c("10.1093/bioinformatics/btae029" = "https://portal.gdc.cancer.gov/projects/TCGA-LGG",
                        "10.1172/jci157780" = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE192720",
                        "10.1074/mcp.ra119.001446" = "https://www.ebi.ac.uk/pride/archive/projects/PXD017710")

# Make corrections to the data IDs
data_id_clean <- c("doi 10.17605/OSF.IO/85DHZ" = "10.17605/OSF.IO/85DHZ",
                  "https://zenodo.org/record/4807754" = "https://doi.org/10.5281/zenodo.4807754",
                  "https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?task=02333066ad284481b3d4f5aba3a4d150" = "https://doi.org/doi:10.25345/C5CV76",
                  "https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?task=8cba098f6ba34b8cbb5ab9e3caa4b314" = "https://doi.org/doi:10.25345/C57940Z39",
                  "https://ega-archive.org/datasets/EGAD00001004459/files" = "https://ega-archive.org/datasets/EGAD00001004459",
                  "https://ega-archive.org/datasets/EGAD00001007806/files" = "https://ega-archive.org/datasets/EGAD00001007806",
                  "https://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD027956-1&test=no" = "https://proteomecentral.proteomexchange.org/cgi/GetDataset?ID=PXD027956-1",
                  "https://zenodo.org/badge/DOI/10.5281/zenodo.4454538.svg)](https://doi.org/10.5281/zenodo.4454538" = "https://doi.org/10.5281/zenodo.4454538",
                  "https://www.ncbi.nlm.nih.gov/prjna833766" = "https://www.ncbi.nlm.nih.gov/bioproject/?term=prjna833766",
                  "BC & EB: prjeb27665, AI & NR: GSE136264" = "https://www.ebi.ac.uk/ena/browser/view/PRJEB27665",
                  "https://www.ncbi.nlm.nih.gov/gds/?term=GSE160301[Accession]" = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE160301",
                  "mk598003" = "https://www.ncbi.nlm.nih.gov/nuccore/MK598003",
                  "AI & BC: https://osf.io/u9fw8/, EB: https://osf.io/3d5um/" = "https://osf.io/u9fw8/",
                  "EB: https://osf.io/q745r/, NR: https://osf.io/wyzgu/" = "https://osf.io/q745r/")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Correct repository names from master file ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add missing repository name
fill_missing_repo <- c("https://doi.org/10.24433/CO.1724316.v1" = "Code Ocean",
                      "https://proteindiffraction.org/project/3czf/" = "Proteindiffraction.org",
                      "https://submission.gpcrmd.org/home/" = "GPCRmd",
                      "https://cptac-data-portal.georgetown.edu/study-summary/S060" = "NCI Proteomic Data Commons",
                      "https://tudatalib.ulb.tu-darmstadt.de/handle/tudatalib/2279" = "TUdatalib",
                      "10.21228/M8914J" = "Metabolomics Workbench",
                      "10.21430/M35PWV2M56" = "ImmPort",
                      "10.25345/C5542JM4T" = "Mass Spectrometry Interactive Virtual Environment",
                      "https://alphafold.ebi.ac.uk/entry/AF-Q6ZT89-F1" = "EBI AlphaFold Protein Structure Database")

# Repo names pattern detection
repo_names_pattern <- tribble(
  ~col, ~pattern, ~repository,
  "id",  "PCAWG",                                                    "ICGC Data Portal",
  "id",  "10\\.17632/|mendeley",                                     "Mendeley Data",
  "repository","Humboldt",                                                 "edoc-Server",
  "repository","RCSB PDB|Worldwide Protein Data Bank",                     "Worldwide Protein Data Bank",
  "repository","UC Irvine",                                                "UC Irvine Machine Learning Repository",
  "id",  "zenodo",                                                   "Zenodo",
  "id",  "emdb",                                                     "EBI The Electron Microscopy Data Bank",
  "repository","^Allen",                                                   "Allen Institute for Brain Science",
  "id",  "/pxd",                                                     "PRoteomics IDEntifications Database",
  "repository","proteomics",                                               "PRoteomics IDEntifications Database",
  "id",  "/delcode",                                                 "DZNE DELCODE",
  "id",  "^https://www\\.ebi\\.ac\\.uk/biostudies/",                 "EMBL-EBI BioStudies",
  "id",  "^https://ega-archive",                                     "The European Genome-phenome Archive",
  "id",  "^https?://www\\.ebi\\.ac\\.uk/ena/",                       "EMBL-EBI European Nucleotide Archive",
  "id",  "^https://www\\.ebi\\.ac\\.uk/gxa/",                        "EBI Expression Atlas",
  "id",  "^https://www\\.gsea-msigdb\\.org/",                        "GSEA Molecular Signatures Database (MSigDB)",
  "id",  "^https://gwas\\.mrcieu\\.ac\\.uk/",                        "OpenGWAS",
  "id",  "^http://mousebrain\\.org/",                                "Mousebrain",
  "id",  "/geo/query/",                                              "NCBI Gene Expression Omnibus (GEO)",
  "id",  "^https://www\\.ncbi\\.nlm\\.nih\\.gov/bioproject",         "NCBI BioProject",
  "id",  "^https://www\\.ncbi\\.nlm\\.nih\\.gov/nuccore/",           "NCBI Nucleotide (Nuccore)",
  "id",  "^https://trace\\.ncbi\\.nlm\\.nih\\.gov/(sra|Traces)/|^https://www\\.ncbi\\.nlm\\.nih\\.gov/sra/", "NCBI Sequence Read Archive (SRA)",
  "id",  "uniprot\\.org",                                            "UniProt",
  "id",  "osf\\.io",                                                 "Open Science Framework (OSF)",
  "id",  "vivli\\.org",                                              "Vivli",
  "id",  "23andme",                                                  "23andMe",
  "id",  "www\\.synapse\\.org",                                      "Synapse",
  "id",  "10xgenomic",                                               "10x Genomics",
  "id",  "www\\.ukbiobank",                                          "UK Biobank",
  "id",  "^https://www\\.ncbi\\.nlm\\.nih\\.gov/protein/",           "NCBI Protein",
  "id",  "gdc\\.cancer\\.gov/",                                      "NIH GDC Data Portal",
  "id",  "^https://www\\.cbioportal\\.org/",                         "https://www.cbioportal.org/",
  "id",  "^https://www\\.cancerimagingarchive\\.net/",               "TCGA Cancer Imaging Archive",
  "id",  "^https://repository\\.jpostdb\\.org/|^https?://proteomecentral", "PRoteomics IDEntifications Database",
  "id",  "^https://www\\.dzne\\.de/forschung/studien/klinische-studien/dancer/", "DZNE",
  "id",  "^https://www\\.ncbi\\.nlm\\.nih\\.gov/(projects/)?gap/|^https://dbgap\\.ncbi\\.nlm\\.nih\\.gov/", "database of Genotypes and Phenotypes (dbGaP)",
  "id",  "^https://www\\.rcsb\\.org/",                               "Worldwide Protein Data Bank",
  "id",  "^https://neuromorpho\\.org/",                              "NeuroMorpho.org",
  "id",  "rodare",                                                   "Rodare",
  "id",  "empiar",                                                   "EMBL-EBI EMPIAR",
  "id",  "gwas",                                                     "GWAS Catalog",
  "id",  "g-node",                                                   "GIN",
  "id",  "^https://www\\.pgscatalog\\.org/",                         "PGS Catalog",
  "id",  "pdb",                                                      "Worldwide Protein Data Bank",
  "id",  "massive",                                                  "Mass Spectrometry Interactive Virtual Environment",
  "id",  "metabolomicswork",                                         "Metabolomics Workbench",
  "id",  "openneuro",                                                "OpenNeuro",
  "id",  "addgene",                                                  "Addgene",
  "id",  "dryad",                                                    "Dryad",
  "id",  "flowrepository",                                           "FlowRepository",
  "id",  "metabolights",                                             "MetaboLights",
  "id",  "codeocean",                                                "Code Ocean",
  "id",  "PeptideAtlas",                                             "PeptideAtlas",
  "id",  "10\\.7910/DVN",                                            "Harvard Dataverse",
  "id",  "ghdx\\.healthdata\\.org",                                  "Global Health Data Exchange (GHDx)",
  "id",  "deciphergenomics",                                         "Decipher Genomics",
  "id",  "physionet\\.org",                                          "PhysioNet",
  "id",  "^10\\.7937/",                                              "The Cancer Imaging Archive (TCIA)",
  "id", "^10\\.25375/uct.*",                                         "ZivaHub"
)

repo_alias <- c(
  "clinvar;ncbi nucleotide" = "NCBI Nucleotide (Nuccore)",
  "ncbi nucleotide"         = "NCBI Nucleotide (Nuccore)",
  "gwas cata"               = "GWAS Catalog",
  "gwas"                    = "GWAS Catalog",
  "sra"                     = "NCBI Sequence Read Archive (SRA)",
  "ncbi sequence read archive" = "NCBI Sequence Read Archive (SRA)",
  "sequence read archive"   = "NCBI Sequence Read Archive (SRA)",
  "open science framework"  = "Open Science Framework (OSF)",
  "osf"                     = "Open Science Framework (OSF)",
  "osf (open science framework)" = "Open Science Framework (OSF)",
  "tu darmstadt data library" = "TUdatalib",
  "boris portal bern open repository and information system" = "BORIS Portal",
  "data radboud university" = "Radboud Data University",
  "biostudies"              = "EMBL-EBI BioStudies",
  "ebi biostudies"          = "EMBL-EBI BioStudies",
  "neuromorpho"             = "NeuroMorpho.org",
  "ebi european nucleotide archive" = "EMBL-EBI European Nucleotide Archive",
  "european nucleotide archive"     = "EMBL-EBI European Nucleotide Archive",
  "university of bristol data repository" = "data.bris Research Data Repository",
  "universitt hamburg"      = "ZFDM repository",
  "gene expression omnibus" = "NCBI Gene Expression Omnibus (GEO)",
  "bioproject"              = "NCBI BioProject",
  "arrayexpress"            = "NCBI ArrayExpress",
  "decipher"                = "Decipher Genomics",
  "the electron microscopy data bank" = "EBI The Electron Microscopy Data Bank",
  "empiar"                  = "EMBL-EBI EMPIAR",
  "dbgap"                   = "database of Genotypes and Phenotypes (dbGaP)"
)

# Matching the repository names for further categorisation, and recoding them to original
which_repository <- function(id, repository) {
  result <- unname(fill_missing_repo[id])
  resolved <- !is.na(result)
  
  for (i in seq_len(nrow(repo_names_pattern))) {
    
    x <- if (repo_names_pattern$col[i] == "id") {
      id
    } else {
      repository
    }
    
    hit <- !resolved &
      coalesce(str_detect(x, regex(repo_names_pattern$pattern[i], ignore_case = TRUE)), FALSE)
    result[hit] <- repo_names_pattern$repository[i]
    resolved[hit] <- TRUE
  }
  
  result[!resolved] <- repository[!resolved]
  original_repo(result)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load dataframe with repository names ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
repository_names <- read_delim(
  "input/re3data_categories.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE) |>
  select(repository_type, repository) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add new repositories from master file not yet in the re3data list ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

extra_repos <- bind_rows(
  tibble(repository_type = "general-purpose repository",
         repository = c("Fairdata.fi", "Recherche Data Gouv", "Apollo - University of Cambridge Repository",
                        "BORIS Portal", "Edinburgh DataShare", "edoc-Server", "ePub Universitt Regensburg",
                        "Radboud Data University", "Refubium", "Rodare", "Stanford Digital Repository",
                        "TUdatalib", "UNIMI Dataverse", "ZFDM repository",
                        "data.bris Research Data Repository", "ZivaHub")),
  tibble(repository_type = "disciplinary repository",
         repository = c("NCBI Genome", "jPOSTrepo (Japan ProteOme STandard Repository)", "UniProt",
                        "Leipzig Health Atlas", "BonaRes Repository", "Software Heritage Archive", "GigaDB",
                        "Publisso", "GIN", "NCI Proteomic Data Commons",
                        "Biological Magnetic Resonance Data Bank (BMRB)", "CIViC",
                        "Global Health Data Exchange (GHDx)", "GPCRmd", "ICGC Data Portal", "ModelArchive",
                        "NeuroMorpho.org", "iProX", "NIH GDC Data Portal", "NIST Data Public", "OmicsPred",
                        "OMIX, China National Center for Bioinformation", "PanoramaWeb", "PeptideAtlas",
                        "PhysioNet", "PLUMED-NEST", "Proteindiffraction.org",
                        "Protein Circular Dichroism Data Bank PCDDB", "SCOPE", "Study of Health in Pomerania",
                        "UC Irvine Machine Learning Repository", "The Cancer Imaging Archive (TCIA)"))
)

repository_names <- repository_names |>
  filter(!norm_repos(repository) %in% c("european genome-phenom archive","proteomeexchange","gwas")) |>
  mutate(repository = original_repo(repository),
         repo_key = norm_repos(repository)) |>
  bind_rows(extra_repos) |>
  mutate(repository = original_repo(repository),
         repo_key = norm_repos(repository)) |>
  distinct(repo_key, .keep_all = TRUE)

write.csv2(select(repository_names, -repo_key), "output/repository_categories_2020_2024.csv", row.names = FALSE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join corrected repo names with the ids ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
charite_rd_od <- master_2020_2024_repos |>
  filter(open_data_assessment == "open_data",
         !data_identifier %in% not_include_datasets) |>
  select(doi, data_identifier, year, repository, license_name) |>
  distinct(data_identifier, .keep_all = TRUE)

charite_rd_od_cleaned_repos <- charite_rd_od |>
  mutate(across(where(is.character), str_trim),
         repository = iconv(as.character(repository), to = "UTF-8", sub = ""),
         data_identifier = case_when(
           data_identifier == ""    ~ "NULL",
           doi %in% names(article_doi_correct) ~ unname(article_doi_correct[doi]),
           .default = coalesce(unname(data_id_clean[data_identifier]), data_identifier))) |>
  mutate(repository_original = repository,
         repository = which_repository(data_identifier, repository),
         data_identifier = str_remove(data_identifier,
                                      "\\?query.*|\\?show=reads.*|&utm_source.*|\\?report.*|\\(accession number.*"),
         data_identifier = str_replace(data_identifier, "^https:/d", "https://d"),
         doi = str_replace_all(doi, c("%28" = "(", "%29" = ")")))

charite_rd_od_cleaned_repos <- charite_rd_od_cleaned_repos |>
  mutate(
    repository = original_repo(repository),
    repo_key = norm_repos(repository)
  ) |>
  left_join(
    repository_names |>
      select(repo_key, repository_type),
    by = "repo_key"
  ) |>
  select(-c(repo_key, repository_original)) |>
  rename(repository_re3data = repository) |>
  relocate(repository_re3data, .after = data_identifier)

# Test not categorised repositories
charite_rd_od_cleaned_repos |>
  filter(is.na(repository_type)) |>
  count(repository_re3data, sort = TRUE)

write.csv2(charite_rd_od_cleaned_repos, "output/charite_ids_repos_2020_2024.csv", row.names = FALSE)









#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Summary of repository accurance ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

count_repos <- charite_rd_od_cleaned_repos |>
  count(repository_re3data, sort = T)

write.csv2(count_repos,"output/count_repos_2020_2024.csv")


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

save_data_xlsx(df = list(charite_rd_od_cleaned_repos), name = c("input_clean_2020_2024"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save necessary objects for next step ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
keep <- c("charite_rd_od_cleaned_repos")
rm(list = setdiff(ls(), keep))
save(list = ls(), file = "output/charite_data_cleaned_repos_2020_2024.Rdata")

#save(charite_rd_od_cleaned_repos, file = "output/Rdata/charite_data_cleaned_repos_2020_2024.Rdata")
#rm(repository_names, master_2020_2024_repos)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
