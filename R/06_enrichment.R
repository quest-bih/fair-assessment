#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Enrich data with classifications----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(fuzzyjoin)
library(httr)
library(janitor)
library(tidyverse)
library(readxl)
library(roadoi)
library(stringdist)
library(tm)
library(xml2)


load("output-Rdata/charite_rd_2020_join.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Get unpaywall data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

article_dois <- charite_rd_2020_join %>%
  pull(article) %>%
  unique

# output_unpaywall <- roadoi::oadoi_fetch(dois = article_dois,
#                                         email = "jan.taubitz@charite.de",
#                                         .progress = "text")

# save(output_unpaywall, file = "output-Rdata/output_unpaywall.Rdata")
load(file = "output-Rdata/output_unpaywall.Rdata")

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

unpaywall_fo_r_join %>%
  group_by(fo_r) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)) %>%
  filter(count > 2) %>%
  ggplot(aes(reorder(fo_r, perc), perc)) +
  geom_col() +
  coord_flip()

# Join with long dataframe
# https://stackoverflow.com/questions/59987662/merge-two-data-frames-based-on-multiple-columns-in-r


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# re3data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Use case for re3date analysis 
# https://github.com/re3data/using_the_re3data_API/blob/main/re3data_API_certification_by_type.ipynb


# Obtain re3data IDs of all repositories indexed in re3data
re3data_request <- GET("http://re3data.org/api/v1/repositories")
re3data_IDs <- xml_text(xml_find_all(read_xml(re3data_request), xpath = "//id"))
URLs <- paste("https://www.re3data.org/api/v1/repository/", re3data_IDs, sep = "")
URLs2 <- URLs[1:100]

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
    "dataLicenseName",
    "pidSystem"
  )

# Create functions to request information about repositories
paste_unique_xml <- compose(
  partial(paste, collapse = "_AND_"),
  unique,
  xml_text,
  partial(xml_find_all, x = repository_metadata_XML)
)

extract_repository_info <- function(url) {
  
  for (i in seq_along(extract_data)) {
  l[i] <- list(
    paste_unique_xml(paste0("//r3d:", extract_data[i])) # must be object
    )
  names(l)[i] <- extract_data[i]
  
  }
  
l 

}

# Gather detailed information about repositories
# repository_info <- data.frame(matrix(ncol = 1, nrow = 0))
# 
# for (url in URLs) {
#   repository_metadata_request <- GET(url)
#   repository_metadata_XML <-read_xml(repository_metadata_request) 
#   results_list <- extract_repository_info(repository_metadata_XML)
#   repository_info <- rbind(repository_info, results_list)
# }

# Function xml_structure can be very useful for inspecting the structure of XML objects
# xml_structure(repository_metadata_XML)

save(repository_info, file = "output-Rdata/repository_info.Rdata")
load("output-Rdata/repository_info.Rdata")

repository_info <- repository_info %>%
  mutate_all(na_if, "")

repository_info <- repository_info %>% mutate_all(na_if, "")
repository_info <- repository_info %>% mutate(certificate = ifelse(is.na(certificate), FALSE, TRUE))
repository_info <- repository_info %>% separate_rows(type, sep = "_AND_")

# Classification
# https://www.dfg.de/en/dfg_profile/statutory_bodies/review_boards/subject_areas/index.jsp

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join re3data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rdata <- charite_rd_2020_join %>%
  select(1:4) %>%
  mutate_all(str_to_lower) %>%
  mutate(repository = removePunctuation(repository)) %>%
  mutate(repository = removeWords(repository, stopwords("english"))) %>%
  mutate(repository = str_replace_all(repository, pattern = " ", repl = ""))

rep <- repository_info %>%
#  select(1:5) %>%
  separate_rows(additionalName, sep = "_AND_") %>%
  pivot_longer(cols = c("repositoryName", "additionalName"), values_to = "repository", values_drop_na = TRUE) %>%
  mutate_all(str_to_lower) %>%
  select(-name) %>%
  mutate_all(na_if, "") %>%
  mutate(repository = removePunctuation(repository)) %>%
  mutate(repository = removeWords(repository, stopwords("english"))) %>%
  mutate(repository = str_replace_all(repository, pattern = " ", repl = "")) %>%
  clean_names()


r3data_join <- rdata %>% left_join(rep, by = "repository", keep = TRUE) %>%
 # select(-repositoryURL, -repositoryIdentifier) %>%
  distinct(repository.x, .keep_all = TRUE) %>%
  relocate(repository.y, .after = repository.x) %>%
  mutate(is_re3data = case_when(is.na(re3data_org_identifier) ~ FALSE,
                              TRUE ~ TRUE))



