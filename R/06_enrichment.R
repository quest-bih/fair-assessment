#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Enrich data with classifications----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

#library(fuzzyjoin)
library(httr)
library(janitor)
library(tidyverse)
library(readxl)
library(roadoi)
#library(stringdist)
library(tm)
library(xml2)


load("output/Rdata/charite_rd_2020_join.Rdata")
source("R/01_rdm_ids.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get unpaywall data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

article_dois <- charite_rd_2020_clean %>%
  pull(article) %>%
  unique

# output_unpaywall <- roadoi::oadoi_fetch(dois = article_dois,
#                                         email = "jan.taubitz@charite.de",
#                                         .progress = "text")
# 
# save(output_unpaywall, file = "output/Rdata/output_unpaywall.Rdata")
load(file = "output/Rdata/output_unpaywall.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get unpaywall data for all charite publications 2020----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

publications_charite <- read_csv("input/publications_charite.csv")

publ_cha_2020 <- publications_charite %>%
  select(doi, year) %>%
  mutate(doi = str_to_lower(doi), doi = str_trim(doi)) %>%
  filter(year == 2020) %>%
  filter(!str_detect(doi, "^keine doi.*"))

rd_cha_2020 <- charite_rd_2020_clean %>%
  select(article) %>%
  distinct() %>%
  full_join(publ_cha_2020, by = c("article" = "doi")) %>%
  distinct(article, .keep_all = TRUE)

# output_unpaywall_publ_cha_2020 <- roadoi::oadoi_fetch(dois = rd_cha_2020$article,
#                                         email = "jan.taubitz@charite.de",
#                                         .progress = "text")
# 
# save(output_unpaywall_publ_cha_2020, file = "output/Rdata/output_unpaywall_publ_cha_2020.Rdata")
load(file = "output/Rdata/output_unpaywall_publ_cha_2020.Rdata")

output_unpaywall_publ_cha_2020_summary <- output_unpaywall_publ_cha_2020 %>%
  count(publisher) %>%
  mutate(publisher_prop_unpaywall = n / sum(n)) %>%
  rename(publisher_n_unpaywall = n)

output_unpaywall_jour_cha_2020_summary <- output_unpaywall_publ_cha_2020 %>%
  count(journal_name) %>%
  mutate(journal_prop_unpaywall = n / sum(n)) %>%
  rename(journal_n_unpaywall = n)

# Problem: ca. 20 of the 260 analysed articles or not included in the publication list of Charité Medical Library
# Some are correctly not included, some are falsely not included
# Not charite doi?
# 10.1021/acs.analchem.0c01273
# 
# Is Charite doi
# 10.1016/j.jclinepi.2020.09.022



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load FoR table ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

field_of_research_journals <- read_excel("input/era_2018_journal_list.xlsx", 
                                         sheet = "ERA 2018 Journal List", col_types = c("numeric", 
                                                                                        "text", "text", "text", "text", "text", 
                                                                                        "text", "text", "text", "text", "text", 
                                                                                        "text", "text", "text", "text", "text")) %>% 
  clean_names()


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join with pivot longer ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

output_unpaywall_longer <- output_unpaywall %>% 
  select(doi, journal_name, journal_issns, issn_1 = journal_issn_l) %>%
  separate(col = journal_issns, into = c("issn_2", "issn_3"), sep = ",", remove = FALSE) %>%
  select(-journal_issns) %>%
  pivot_longer(cols = starts_with("issn_"), values_to = "issn", values_drop_na = TRUE) %>%
  distinct(doi, issn, .keep_all = TRUE)
  
# test <- output_unpaywall_longer %>%
#   filter(issbn_1 != journal_issn_l)
  
field_of_research_journals_longer <- field_of_research_journals %>%
  select(title, starts_with("issn_"), ends_with("_name")) %>%
  pivot_longer(cols = starts_with("issn_"), values_to = "issn", values_drop_na = TRUE)

unpaywall_fo_r_join <- output_unpaywall_longer %>% 
  left_join(field_of_research_journals_longer, by = "issn") %>%
  distinct(doi, journal_name, .keep_all = TRUE) %>%
  select(doi, journal_name, starts_with("fo_r_")) %>%
  pivot_longer(cols = starts_with("fo_r_"), values_to = "fo_r", values_drop_na = TRUE) %>%
  select(-name) %>%
  group_by(doi) %>%
  sample_n(1)

charite_rd_2020_join_2 <- charite_rd_2020_join %>%
  left_join(output_unpaywall %>% select(doi, journal_name, publisher), by = c("article" = "doi")) %>%
  left_join(unpaywall_fo_r_join %>% select(doi, fo_r), by = c("article" = "doi")) %>%
  relocate(journal_name_unpaywall = journal_name, .after = article) %>%
  relocate(publisher_unpaywall = publisher, .after = journal_name_unpaywall) %>%
  relocate(fields_of_research = fo_r, .after = publisher_unpaywall) %>%
  relocate(guid_scheme_fuji = guid_scheme, .after = repository_type) %>%
  relocate(license_fuji = license, .after = guid_scheme_fuji) %>%
  rename(guid_fuji = guid)

# Join with long dataframe
# https://stackoverflow.com/questions/59987662/merge-two-data-frames-based-on-multiple-columns-in-r

unpaywall_fo_r_join %>%
  group_by(fo_r) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)) %>%
  filter(count > 2) %>%
  ggplot(aes(reorder(fo_r, perc), perc)) +
  geom_col() +
  coord_flip()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# re3data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Use case for re3date analysis 
# https://github.com/re3data/using_the_re3data_API/blob/main/re3data_API_certification_by_type.ipynb

# Obtain re3data IDs of all repositories indexed in re3data
re3data_request <- GET("http://re3data.org/api/v1/repositories")
re3data_IDs <- xml_text(xml_find_all(read_xml(re3data_request), xpath = "//id"))
URLs <- paste("https://www.re3data.org/api/v1/repository/", re3data_IDs, sep = "")
#URLs <- URLs[1:100]
#URLs <- c("https://www.re3data.org/api/v1/repository/r3d100011137", "https://www.re3data.org/api/v1/repository/r3d100011137")

# Define what information about the repositories should be requested
# re3data Metadata Schema: https://gfzpublic.gfz-potsdam.de/rest/items/item_758898_6/component/file_775891/content
extract_data <-
  c(
    "re3data.orgIdentifier",
    "repositoryName",
    "additionalName",
    "repositoryURL",
    "repositoryIdentifier", 
    "type",
    "subject", 
    "keyword",
    "policyName",
    "policyURL",
    "dataLicenseName",
    "pidSystem",
    "dataAccessType", # added to filter re3data for Numbat workflow
    "size", # added to filter re3data for Numbat workflow
    "metadataStandardName" # added to filter re3data for Numbat workflow
  )

# Create functions to request information about repositories
paste_unique_xml <- compose(
  partial(paste, collapse = "_AND_"),
  unique,
  xml_text,
  partial(xml_find_all, x = repository_metadata_XML)
)

extract_repository_info <- function(url) {
  l <- list()
  for (i in seq_along(extract_data)) {
  l[i] <- list(
    paste_unique_xml(paste0("//r3d:", extract_data[i])) # must be object
    )
  names(l)[i] <- extract_data[i]
  
  }
l 
}

# Gather detailed information about repositories

# repository_info <- data.frame()
# 
# for (url in URLs) {
#   repository_metadata_request <- GET(url)
#   repository_metadata_XML <-read_xml(repository_metadata_request)
#   results_list <- extract_repository_info(repository_metadata_XML)
#   repository_info <- rbind(repository_info, results_list)
# }

# Function xml_structure can be very useful for inspecting the structure of XML objects
# xml_structure(repository_metadata_XML)

save(repository_info, file = "output/Rdata/repository_info.Rdata")
load("output/Rdata/repository_info.Rdata")

repository_info <- repository_info %>%
  mutate_all(na_if, "")

#repository_info <- repository_info %>% mutate(certificate = ifelse(is.na(certificate), FALSE, TRUE))
#repository_info <- repository_info %>% separate_rows(type, sep = "_AND_")

repository_info2 <- repository_info %>% separate_rows(policyName, policyURL, sep = "_AND_") %>% 
  distinct()

# Classification
# https://www.dfg.de/en/dfg_profile/statutory_bodies/review_boards/subject_areas/index.jsp

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create semicolon-delimited string of repository names for numbat workflow ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Test with random characters

# library(stringi)
# string <- stri_rand_strings(1500, 10, pattern = "[a-z0-9]") %>%
#   glue::glue_collapse(sep = ";")

# Filter nach Life Sciences, und other repositories

string <-
  repository_info %>% 
  filter(str_detect(subject, "21 Biology|22 Medicine")|str_detect(type, "other")) %>% 
  filter(!is.na(repositoryIdentifier)) %>%
  pull(repositoryName) %>% 
  str_replace_all("[\r\n]" , "")  %>% 
  str_squish() %>% 
  #str_trunc(40) %>%  #, ellipsis = ""
  #str_replace_all(pattern = " ", replacement = "-") %>% 
  sort() %>%
 # sample(size = 1000) %>% 
  glue_collapse(sep = ";")

test <- charite_rd_2020_final %>%
  filter(!repository_re3data %in% string) %>%
  pull(repository_re3data) %>%
  unique()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join re3data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rdata <- charite_rd_2020_clean %>%
  select(article, best_identifier, repository) %>%
  mutate(repository = removePunctuation(repository)) %>%
  mutate(repository = removeWords(repository, stopwords("english"))) %>%
  mutate(repository = str_replace_all(repository, pattern = " ", repl = ""))

rep <- repository_info %>%
  mutate(repository_name_long = repositoryName) %>%
#  select(1:5) %>%
  separate_rows(additionalName, sep = "_AND_") %>%
  pivot_longer(cols = c("repositoryName", "additionalName"), values_to = "repository", values_drop_na = TRUE) %>%
  select(-name) %>%
  mutate_all(na_if, "") %>%
  mutate(repository = str_to_lower(repository),
         repository = removePunctuation(repository),
         repository = removeWords(repository, stopwords("english")),
         repository = str_replace_all(repository, pattern = " ", repl = "")) %>%
  clean_names()

r3data_join <- rdata %>% left_join(rep, by = "repository", keep = TRUE) %>%
  select(-article, -repository_url, -repository_identifier) %>%
 # distinct(repository.x, .keep_all = TRUE) %>%
  distinct(best_identifier, .keep_all = TRUE) %>%
  relocate(repository.y, .after = repository.x)

charite_rd_2020_join_3 <- charite_rd_2020_join_2 %>% 
  left_join(r3data_join, by = c("best_identifier")) %>%
  select(-repository.x, - repository.y) %>%
  relocate(repository_re3data = repository_name_long, .after = repository) %>%
  mutate(repository_re3data = case_when(is.na(repository_re3data) ~ repository,
                                        TRUE ~ repository_re3data)) %>%
  relocate(repository_type_re3data = type, .after = repository_type) %>%
  relocate(subject_re3data = subject, .after = repository_type_re3data) %>%
  relocate(keyword_re3data = keyword, .after = subject_re3data) %>%
  relocate(re3data_org_identifier, .after = repository_type_re3data) %>%
  relocate(license_fair_enough, .after = license_fuji)

charite_rd_2020_final <- charite_rd_2020_join_3

# Join final data with summarized publisher data and journal data from unpaywall
charite_rd_2020_final <- charite_rd_2020_final %>%
  left_join(output_unpaywall_publ_cha_2020_summary, by = c("publisher_unpaywall" = "publisher")) %>%
  relocate(c(publisher_n_unpaywall, publisher_prop_unpaywall), .after = publisher_unpaywall) %>%
  left_join(output_unpaywall_jour_cha_2020_summary, by = c("journal_name_unpaywall" = "journal_name")) %>%
  relocate(c(journal_n_unpaywall, journal_prop_unpaywall), .after = journal_name_unpaywall)

save(charite_rd_2020_final, file = "output/Rdata/charite_rd_2020_final.Rdata")
load("output/Rdata/charite_rd_2020_final.Rdata")

# Save final data to xlsx
save_data_xlsx(df = list(charite_rd_2020_final), name = "final_output_2020")

data_policy_2020 <- charite_rd_2020_join_3 %>%
  group_by(repository_re3data, repository_type, policy_name, policy_url, data_license_name) %>%
  summarise(count = n ()) %>%
  arrange(desc(count))

save_data_xlsx(df = list(data_policy_2020), name = "data_policy_2020")

classification_2020 <- charite_rd_2020_join_3 %>%
  group_by(repository_re3data, repository_type, repository_type_re3data, subject_re3data, keyword_re3data) %>%
  summarise(count = n ()) %>%
  arrange(desc(count))

save_data_xlsx(df = list(classification_2020), name = "classification_2020")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Save license data to xlsx ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2020_license <- charite_rd_2020_final %>%
  select(article, best_identifier, guid_fuji, repository_re3data, repository_type, data_license_name, license_fuji, license_fair_enough) %>%
  arrange(desc(repository_type), repository_re3data)

save_data_xlsx(df = list(charite_rd_2020_license), name = "licenses_2020")
licenses_2020 <- read_excel("output/licenses_2020.xlsx")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load data from AI and enrich is ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Read processed data
path <- "output/licenses_2020_processed_AI.xlsx"  
licenses_AI <- read_excel(path = path) 
licenses_AI <- licenses_AI %>%
  select(article, best_identifier, agreement, comment)

# Read license data
licenses_2020 <- read_excel("output/licenses_2020.xlsx") 

# Join license data with processed data
licenses_2020 <- licenses_2020 %>%
  left_join(licenses_AI, by = c("article", "best_identifier"))

# Save data to output folder
save_data_xlsx(df = list(licenses_2020), name = "licenses_2020_processed_AI")
