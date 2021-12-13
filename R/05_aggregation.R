#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from FUJI and Fair Enough Assessment ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)

# Load data
source("R/01_rdm_ids.R")

load("output-Rdata/fuji_local_list.Rdata")
load("output-Rdata/fair_enough_list.Rdata")
#load("output-Rdata/fair_evaluation_data.Rdata")
load("output-Rdata/fuji_guid.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check depth of nested fuji_local_list
# vec_depth(fuji_local_list)

# Extract data from API results
fuji_summary <-
  map_dfr(
    seq_along(fuji_local_list),
    ~ bind_rows(fuji_local_list[[.x]][["summary"]], .id = "id") %>%
      mutate(rd_id = fuji_local_list[[.x]][["request"]][["object_identifier"]], .before = id)
  )


# Extract single values
# fuji_local_list %>% map("request") %>% map_chr("object_identifier")

# Extract only score_percent and FAIR
fuji_summary_results <- fuji_summary %>%
  filter(id == "score_percent") %>%
  select(1, 2, 10) %>%
  rename(fuji_percent = FAIR) %>%
  distinct()

# Extract only score_percent and FAIR
fuji_summary_results_all <- fuji_summary %>%
  filter(id == "score_percent") %>%
  select(rd_id, id, 
         fuji_percent = FAIR, 
         fuji_percent_f = `F`, 
         fuji_percent_a = `A`, 
         fuji_percent_i = `I`, 
         fuji_percent_r = `R`) %>%
  distinct()

fuji_mean_median_max <- fuji_summary_results_all %>%
  #  group_by(id) %>%
  summarise_if(is.numeric, list(mean, median, max), na.rm = TRUE) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(assessment_tool = "FUJI", .before = 1) %>%
  rename_all(~ (str_replace_all(., c(fn1 = "mean", fn2 = "median", fn3 = "max"))))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract license data from FUJI results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_license <-
  map_dfr(
    seq_along(fuji_local_list),
    ~ bind_rows(fuji_local_list[[.x]][["results"]][[11]][["output"]]) %>%
      mutate(rd_id = fuji_local_list[[.x]][["request"]][["object_identifier"]])
  ) %>%
  mutate_all(~ str_trim(.)) %>%
  mutate_all(~ na_if(., "")) %>%
  mutate(license = str_to_lower(license)) %>%
  mutate(
    license_2 = case_when(
      str_detect(license, "by\\/4\\.0|cc-by") ~ "CC-BY",
      str_detect(license, "cc0|public domain") ~ "CC0",
      str_detect(license, "by-nc-sa\\/") ~ "CC-BY-NC-SA",
      str_detect(license, "by-nc\\/") ~ "CC-BY-NC",
      is.na(license) ~ "no license",
      TRUE ~ "other license"
    )
  ) %>%
  mutate(license_2 = factor(
    license_2,
    levels = c(
      "CC0",
      "CC-BY",
      "CC-BY-NC",
      "CC-BY-NC-SA",
      "other license",
      "no license"
    )
  ))

fuji_license <- fuji_license %>%
  select(rd_id, license = license_2) %>%
  mutate_all(str_to_lower) %>%
  distinct()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract guid_scheme ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_guid_scheme <-
  map_dfr(
    seq_along(fuji_local_list),
    ~ bind_rows(fuji_local_list[[.x]][["request"]]) %>%
      mutate(guid_scheme = fuji_local_list[[.x]][["results"]][[1]][["output"]][["guid_scheme"]])) %>%
  select(rd_id = object_identifier, guid_scheme) %>%
  mutate_all(~ str_trim(.)) %>%
  mutate_all(str_to_lower) %>%
  distinct()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FAIR Enough data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Extract results from data
fair_enough_summary <- map_dfr(
  seq_along(fair_enough_list),
  ~ bind_rows(fair_enough_list[[.x]][["score"]]) %>%
    mutate(guid = fair_enough_list[[.x]][["resource_uri"]], .before = total_score)
) %>% distinct() %>%
  select(guid, fair_enough_percent = percent) %>%
  left_join(fuji_guid, by = "guid")

fuji_fair_enough_summary_results <-
  fuji_summary_results_all %>% full_join(fair_enough_summary, by = "rd_id") %>%
  select(-id) %>%
  mutate_all(str_to_lower) %>%
  distinct() 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2020_join <- charite_rd_2020_clean %>%
  left_join(fuji_fair_enough_summary_results,
            by = c("best_identifier" = "rd_id")) %>%
  relocate(guid, .after = best_identifier) %>%
  drop_na(guid) %>%
  left_join(fuji_license, by = c("best_identifier" = "rd_id")) %>%
  mutate(license = replace_na(license, "no license")) %>%
  left_join(fuji_guid_scheme, by = c("best_identifier" = "rd_id"))
  
save(charite_rd_2020_join, file = "output-Rdata/charite_rd_2020_join.Rdata")

load("output-Rdata/charite_rd_2020_join.Rdata")

charite_rd_2020_license <- charite_rd_2020_join %>%
  select(article, best_identifier, repository, repository_type, license) %>%
  arrange(desc(repository_type), repository)

path = "output/licenses_2020.xlsx"

save_data <- function(path) {
  wb <- createWorkbook()
  addWorksheet(wb, "licenses_2020")
  writeData(wb, "licenses_2020", charite_rd_2020_license, keepNA = TRUE)
  saveWorkbook(wb, path, overwrite = TRUE)
}

save_data(path)


results_long <-
  charite_rd_2020_join %>% pivot_longer(cols = c("fuji_percent", "fair_enough_percent"),
                                         names_to = "assessment_tool")

summary_grouped_repository <- results_long %>%
  drop_na(value) %>%
  group_by(assessment_tool, open_data_category) %>%
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n())

summary <- results_long %>%
  drop_na(value) %>%
  group_by(assessment_tool) %>%
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n())

path = "~/OneDrive - Charité - Universitätsmedizin Berlin/_BIH/BUA-Dashboards/fair_assessment.xlsx"

save_data <- function(path) {
  wb <- createWorkbook()
  addWorksheet(wb, "all_ids")
  writeData(wb, "all_ids", charite_rd_2020_join, keepNA = TRUE)
  addWorksheet(wb, "summary")
  writeData(wb, "summary", summary, keepNA = TRUE)
  addWorksheet(wb, "summary_grouped_repository")
  writeData(wb,
            "summary_grouped_repository",
            summary_grouped_repository,
            keepNA = TRUE)
  saveWorkbook(wb, path, overwrite = TRUE)
}

save_data(path)


