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

load("output/Rdata/fuji_local_list_2022_03_04.Rdata")
load("output/Rdata/fair_enough_list.Rdata")
#load("output-Rdata/fair_evaluation_data.Rdata")
load("output/Rdata/fuji_guid.Rdata")

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
  rename(fuji_percent = FAIR)

# Extract only score_percent and FAIR
fuji_summary_results_all <- fuji_summary %>%
  filter(id == "score_percent") %>%
  select(rd_id, id, 
         fuji_percent = FAIR, 
         fuji_percent_f = `F`, 
         fuji_percent_a = `A`, 
         fuji_percent_i = `I`, 
         fuji_percent_r = `R`,
         fuji_percent_f1 = `F1`,
         fuji_percent_f2 = `F2`,
         fuji_percent_f3 = `F3`,
         fuji_percent_f4 = `F4`,
         fuji_percent_a1 = `A1`,
         fuji_percent_i1 = `I1`,
         fuji_percent_i3 = `I3`,
         fuji_percent_r1 = `R1`,
         fuji_percent_r1_1 = `R1.1`,
         fuji_percent_r1_2 = `R1.2`,
         fuji_percent_r1_3 = `R1.3`)

fuji_mean_median_max <- fuji_summary_results_all %>%
  #  group_by(id) %>%
  summarise_if(is.numeric, list(mean, median, max), na.rm = TRUE) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(assessment_tool = "FUJI", .before = 1) %>%
  rename_all(~ (str_replace_all(., c(fn1 = "mean", fn2 = "median", fn3 = "max"))))


# Get all FsF metrics scores

fuji_metrics_list <- list()

for (i in seq_along(fuji_local_list)) {
  
  fuji_metrics_df <-
    map_dfr(
      seq_along(1:16), 
      ~ bind_cols(metric_identifier = fuji_local_list[[i]][["results"]][[.x]][["metric_identifier"]]) %>%
        mutate(metric_name = fuji_local_list[[i]][["results"]][[.x]][["metric_name"]]) %>%
        mutate(score_total = fuji_local_list[[i]][["results"]][[.x]][["score"]][["total"]]) %>%
        mutate(score_earned = fuji_local_list[[i]][["results"]][[.x]][["score"]][["earned"]]) %>%
        mutate(guid = fuji_local_list[[i]][["request"]][["object_identifier"]], .before = metric_identifier))
  
  fuji_metrics_list[[i]] <- fuji_metrics_df
  
}

fuji_metrics <- bind_rows(fuji_metrics_list) 

save(fuji_metrics, file = "output/Rdata/fuji_metrics.Rdata")
load("output/Rdata/fuji_metrics.Rdata")


# Make FsF metrics scores wider for join with F-UJI summary df

fuji_metrics_long <- fuji_metrics %>%
  mutate(score_earned = score_earned / score_total * 100) %>%
  mutate(metric_identifier = factor(metric_identifier, levels = c(
    "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
    "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D", 
    "FsF-I1-01M", "FsF-I1-02M", "FsF-I3-01M",
    "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"
  ))) %>%
  arrange(metric_identifier) %>%
  select(guid, metric_identifier, score_earned) %>%
  pivot_wider(names_from = metric_identifier, values_from = score_earned)


fuji_summary_results_all <- fuji_summary_results_all %>% left_join(fuji_metrics_long, by = c("rd_id" = "guid"))

df_sum <- fuji_metrics %>%
  group_by(metric_identifier, metric_name, score_total) %>%
  summarise(mean_score_earned = mean(score_earned)) %>%
  mutate(mean_score_earned_perc = mean_score_earned / score_total * 100) %>%
  mutate(parent = str_extract(metric_identifier, "(?<=-)(.*)(?=-)")) %>%
  rename(name = metric_identifier,
         value = mean_score_earned_perc,
         parent = parent,
         score = score_total,
         principle = metric_name) %>%
  select(-mean_score_earned) %>%
  mutate(name = factor(name, levels = 
                         c("FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
                           "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D", "FsF-I1-01M", "FsF-I1-02M", "FsF-I3-01M",
                           "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"))) %>%
  arrange(name)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract license data from FUJI results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_license <-
  map_dfr(
    seq_along(fuji_local_list),
    ~ bind_rows(fuji_local_list[[.x]][["results"]][[11]][["output"]]) %>%
      mutate(rd_id = fuji_local_list[[.x]][["request"]][["object_identifier"]])
  ) %>%
  mutate_all(str_trim) %>%
  mutate_all(na_if, "") %>%
  mutate(license = str_to_lower(license)) %>%
  mutate(
    license_2 = case_when(
      str_detect(license, "public domain") ~ "Public Domain",
      str_detect(license, "cc0") ~ "CC0",
      str_detect(license, "by\\/4\\.0|cc-by") ~ "CC BY",
      str_detect(license, "by-sa\\/") ~ "CC BY-SA",
      str_detect(license, "by-nc\\/") ~ "CC BY-NC",
      str_detect(license, "by-nc-sa\\/") ~ "CC BY-NC-SA",
      str_detect(license, "by-nd\\/") ~ "CC BY-ND",
      str_detect(license, "by-nc-nd\\/") ~ "CC BY-NC-ND",
      is.na(license) ~ "no license",
      TRUE ~ "other license"
    )
  ) %>%
  mutate(license_2 = factor(
    license_2,
    levels = c(
      "Public Domain",
      "CC0",
      "CC BY",
      "CC BY-SA",
      "CC BY-NC",
      "CC-BY-NC-SA",
      "CC BY-ND",
      "CC BY-NC-ND",
      "other license",
      "no license"
    )
  ))

fuji_license <- fuji_license %>%
  select(rd_id, license = license_2)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract guid_scheme ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_guid_scheme <-
  map_dfr(
    seq_along(fuji_local_list),
    ~ bind_rows(fuji_local_list[[.x]][["request"]]) %>%
      mutate(guid_scheme = fuji_local_list[[.x]][["results"]][[1]][["output"]][["guid_scheme"]])) %>%
  select(rd_id = object_identifier, guid_scheme) %>%
  mutate_all(str_trim)

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


fair_enough_license <- map_dfr(
  seq_along(fair_enough_list),
  ~ bind_cols(fair_enough_list[[.x]][["results"]][[8]][["logs"]]) %>%
    mutate(guid = fair_enough_list[[.x]][["resource_uri"]], .before = `...1`) %>%
    mutate(score = fair_enough_list[[.x]][["results"]][[8]][["score"]], .after = guid)
  ) %>% filter(score == 1)

fair_enough_license <- fair_enough_license %>%
  unite(license, matches("\\d+$"), sep = "_", remove = TRUE, na.rm = TRUE) %>%
  mutate(license = case_when(
      str_detect(license, "public?domain") ~ "Public Domain",
      str_detect(license, "cc0") ~ "CC0",
      str_detect(license, "by\\/4\\.0|cc-by") ~ "CC BY",
      str_detect(license, "by-sa\\/") ~ "CC BY-SA",
      str_detect(license, "by-nc\\/") ~ "CC BY-NC",
      str_detect(license, "by-nc-sa\\/") ~ "CC BY-NC-SA",
      str_detect(license, "by-nd\\/") ~ "CC BY-ND",
      str_detect(license, "by-nc-nd\\/") ~ "CC BY-NC-ND",
      TRUE ~ "other license"
    )) %>%
  rename(license_fair_enough = license) %>%
  select(-score)
    
fair_enough_summary <- fair_enough_summary %>% left_join(fair_enough_license, by = "guid") %>%
  mutate(license_fair_enough = replace_na(license_fair_enough, "no license"))
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Join FUJI and FAIR Enough data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_fair_enough_summary_results <-
  fuji_summary_results_all %>% full_join(fair_enough_summary, by = "rd_id") %>%
  select(-id)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

charite_rd_2020_join <- charite_rd_2020_clean %>%
  left_join(fuji_fair_enough_summary_results,
            by = c("best_identifier" = "rd_id")) %>%
  relocate(guid, .after = best_identifier) %>%
  #drop_na(guid) %>%
  left_join(fuji_license, by = c("best_identifier" = "rd_id")) %>%
  mutate(license = replace_na(license, "no license")) %>%
  left_join(fuji_guid_scheme, by = c("best_identifier" = "rd_id"))
  
save(charite_rd_2020_join, file = "output/Rdata/charite_rd_2020_join.Rdata")
load("output/Rdata/charite_rd_2020_join.Rdata")


results_long <-
  charite_rd_2020_join %>% pivot_longer(cols = c("fuji_percent", "fair_enough_percent"),
                                         names_to = "assessment_tool")

summary_grouped_repository <- results_long %>%
  drop_na(value) %>%
  group_by(assessment_tool, repository_type) %>%
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n())

summary <- results_long %>%
  drop_na(value) %>%
  group_by(assessment_tool) %>%
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n())

save_data_xlsx(list(summary, summary_grouped_repository), name= c("summary", "summary_repository"))


