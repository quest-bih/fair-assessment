#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from FUJI and Fair Enough Assessment ----
# Contact: anastasiia.iarkaeva@charite.de (previous ownner: jan.taubitz@charite.de, Email not accessible)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)
library(purrr)

# Load data
source("R/01_rdm_ids_2020_2024.R")

load("output/Rdata/fuji_local_list_2020_2024.Rdata")
# load("output/Rdata/fair_enough_list.Rdata")
# load("output-Rdata/fair_evaluation_data.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check depth of nested fuji_local_list
pluck_depth(fuji_local_list) # for v0.8 it is 12

# Catching problematic ids - because there was an entry in resulted list that was NA
no_error <- vapply(fuji_local_list, 
                   function(x) 
                     is.list(x) && !is.null(x[["summary"]]), 
                   logical(1))
fuji_summary <-
  map_dfr(which(no_error), function(x) {
    y <- fuji_local_list[[x]]
    bind_rows(y[["summary"]], .id = "id") |>
      mutate(rd_id = y[["request"]][["object_identifier"]] %||% NA_character_, .before = id)
  })

# Checking single object identifiers
# fuji_local_list |> map("request") |> map_chr("object_identifier")

# Extract only score_percent and FAIR
fuji_summary_results <- fuji_summary |>
  filter(id == "score_percent") |>
  select(1, 2, 21) |>
  rename(fuji_percent = FAIR) |>
  distinct(rd_id, .keep_all = T)

# Extract only score_percent and FAIR
fuji_summary_results_all <- fuji_summary |>
  filter(id == "score_percent") |>
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
         fuji_percent_a1_1 = `A1.1`,
         fuji_percent_a1_2 = `A1.2`,
         fuji_percent_i1 = `I1`,
         fuji_percent_i2 = `I2`,
         fuji_percent_i3 = `I3`,
         fuji_percent_r1 = `R1`,
         fuji_percent_r1_1 = `R1.1`,
         fuji_percent_r1_2 = `R1.2`,
         fuji_percent_r1_3 = `R1.3`) |>
  distinct(rd_id, .keep_all = T)

save(fuji_summary_results_all, file = "output/Rdata/fuji_summary_results_2020_2024.Rdata")
#load("output/Rdata/fuji_metrics_all_years_20261008.Rdata")
load("output/Rdata/fuji_summary_results_2020_2024.Rdata")


fuji_mean_median_max <- fuji_summary_results_all |>
  #  group_by(id) |>
  summarise_if(is.numeric, list(mean, median, max), na.rm = TRUE) |>
  mutate_if(is.numeric, round, digits = 2) |>
  mutate(assessment_tool = "FUJI", .before = 1) |>
  rename_all(~ (str_replace_all(., c(fn1 = "mean", fn2 = "median", fn3 = "max"))))

# Get all FsF metrics scores

fuji_metrics_list <- list()

for (i in which(no_error)) {
  
  fuji_metrics_df <-
    map_dfr(
      seq_along(1:16),
      ~ tibble(metric_identifier = fuji_local_list[[i]][["results"]][[.x]][["metric_identifier"]]) |>
        mutate(metric_name  = fuji_local_list[[i]][["results"]][[.x]][["metric_name"]]) |>
        mutate(score_total  = fuji_local_list[[i]][["results"]][[.x]][["score"]][["total"]]) |>
        mutate(score_earned = fuji_local_list[[i]][["results"]][[.x]][["score"]][["earned"]]) |>
        mutate(guid = fuji_local_list[[i]][["request"]][["object_identifier"]], .before = metric_identifier)
    )
  
  fuji_metrics_list[[i]] <- fuji_metrics_df
}

fuji_metrics <- bind_rows(fuji_metrics_list) 

save(fuji_metrics, file = "output/Rdata/fuji_metrics_2020_2024.Rdata")
load("output/Rdata/fuji_metrics_2020_2024.Rdata")


# Make FsF metrics scores wider for join with F-UJI summary df

fuji_metrics_long <- fuji_metrics |>
  mutate(score_earned = score_earned / score_total * 100) |>
  mutate(metric_identifier = factor(metric_identifier, levels = c(
    "FsF-F1-01MD", "FsF-F1-02MD", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
    "FsF-A1-01M", "FsF-A1-02MD", "FsF-A1.1-01MD", "FsF-A1.2-01MD", 
    "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
    "FsF-R1-01M", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M" # "FsF-A1-03D", "FsF-I1-02M", "FsF-R1.3-02D"
  ))) |>
  arrange(metric_identifier) |>
  select(guid, metric_identifier, score_earned) |>
  pivot_wider(names_from = metric_identifier, values_from = score_earned, values_fn = list)


fuji_summary_results_all <- fuji_summary_results_all |> left_join(fuji_metrics_long, by = c("rd_id" = "guid"))

df_sum <- fuji_metrics |>
  group_by(metric_identifier, metric_name, score_total) |>
  summarise(mean_score_earned = mean(score_earned, na.rm = T), .groups = "drop") |>
  mutate(value = mean_score_earned / score_total * 100) |>
  mutate(parent = str_extract(metric_identifier, "(?<=-)(.*)(?=-)")) |>
  select(name = metric_identifier,
         value,
         parent,
         score = score_total,
         principle = metric_name) |>
  mutate(name = factor(name, levels = 
                         c("FsF-F1-01MD", "FsF-F1-02MD", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
                           "FsF-A1-01M", "FsF-A1-02MD", "FsF-A1.1-01MD", "FsF-A1.2-01MD", 
                           "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M",
                           "FsF-R1-01M", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M" # "FsF-A1-03D", "FsF-I1-02M", "FsF-R1.3-02D"
                           ))) |>
  arrange(name)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract license data from FUJI results ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_license <-
    map_dfr(which(no_error), function(i) {
      x <- fuji_local_list[[i]]
      
      res <- keep(x[["results"]], ~ identical(.x[["metric_identifier"]], "FsF-R1.1-01M"))
      if (length(res) == 0) return(NULL)
      
      bind_rows(res[[1]][["output"]]) |>
        mutate(rd_id = x[["request"]][["object_identifier"]])
    }) |>
  mutate(across(everything(), as.character)) |>
  mutate_all(str_trim) |>
  mutate_all(na_if, "")

fuji_license <- fuji_license |>
  mutate(
    license = license |>
      str_to_lower() |>
      stringi::stri_trans_general("Latin-ASCII") |>
      str_replace_all("[\\p{Pd}\\u00a0\\s_/]+", " ") |>   
      str_squish(),
    license_2 = case_when(
      str_detect(license, "public ?domain|gemeinfrei") ~ "Public Domain",
      str_detect(license, "\\bcc0|cc zero|publicdomain zero") ~ "CC0",
      str_detect(license, "\\bby nc nd\\b|attribution noncommercial no ?deriv|namensnennung nicht kommerziell keine bearbeitung") ~ "CC BY-NC-ND",
      str_detect(license, "\\bby nc sa\\b|attribution non ?commercial share ?alike|namensnennung nicht kommerziell weitergabe") ~ "CC BY-NC-SA",
      str_detect(license, "\\bby nc\\b|attribution non ?commercial|namensnennung nicht kommerziell") ~ "CC BY-NC",
      str_detect(license, "\\bby nd\\b|attribution no ?deriv|namensnennung keine bearbeitung") ~ "CC BY-ND",
      str_detect(license, "\\bby sa\\b|attribution share ?alike|namensnennung weitergabe") ~ "CC BY-SA",
      str_detect(license, "\\bby \\d|\\bcc by\\b|creative commons attribution|namensnennung") ~ "CC BY",
      is.na(license) ~ "no license",
      TRUE ~ "other license"
    )
  ) |>
  mutate(license_2 = factor(
    license_2,
    levels = c(
      "Public Domain",
      "CC0",
      "CC BY",
      "CC BY-SA",
      "CC BY-NC",
      "CC BY-NC-SA",
      "CC BY-ND",
      "CC BY-NC-ND",
      "other license",
      "no license"
    )
  ))

table(fuji_license$license_2, useNA = "ifany")
# CC BY      CC BY-NC   CC BY-NC-ND   CC BY-NC-SA      CC BY-SA           CC0 other license Public Domain 
# 927            15            19            17             4             4           322           110 

fuji_license <- fuji_license |>
  select(rd_id, license = license_2) |>
  distinct(rd_id, .keep_all = TRUE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract guid_scheme ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_guid_scheme <-
  map_dfr(
    which(no_error), function(x) {
      y = fuji_local_list[[x]]
      bind_rows(y[["request"]]) |>
      mutate(guid_scheme = y[["results"]][[1]][["output"]][["unique_identifiers"]][[1]][["guid_scheme"]])}) |>
  select(rd_id = object_identifier,
         guid_scheme,
         normalized_object_identifier
         ) |>
  mutate_all(str_trim) |>
  distinct(rd_id, .keep_all = TRUE)
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # Aggregate and manipulate FAIR Enough data ----
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# # Extract results from data
# fair_enough_summary <- map_dfr(
#   seq_along(fair_enough_list),
#   ~ bind_rows(fair_enough_list[[.x]][["score"]]) |>
#     mutate(guid = fair_enough_list[[.x]][["resource_uri"]], .before = total_score)
# ) |> distinct() |>
#   select(guid, fair_enough_percent = percent) |>
#   left_join(fuji_guid, by = "guid")
# 
# 
# fair_enough_license <- map_dfr(
#   seq_along(fair_enough_list),
#   ~ bind_cols(fair_enough_list[[.x]][["results"]][[8]][["logs"]]) |>
#     mutate(guid = fair_enough_list[[.x]][["resource_uri"]], .before = `...1`) |>
#     mutate(score = fair_enough_list[[.x]][["results"]][[8]][["score"]], .after = guid)
#   ) |> filter(score == 1)
# 
# fair_enough_license <- fair_enough_license |>
#   unite(license, matches("\\d+$"), sep = "_", remove = TRUE, na.rm = TRUE) |>
#   mutate(license = case_when(
#       str_detect(license, "public?domain") ~ "Public Domain",
#       str_detect(license, "cc0") ~ "CC0",
#       str_detect(license, "by\\/4\\.0|cc-by") ~ "CC BY",
#       str_detect(license, "by-sa\\/") ~ "CC BY-SA",
#       str_detect(license, "by-nc\\/") ~ "CC BY-NC",
#       str_detect(license, "by-nc-sa\\/") ~ "CC BY-NC-SA",
#       str_detect(license, "by-nd\\/") ~ "CC BY-ND",
#       str_detect(license, "by-nc-nd\\/") ~ "CC BY-NC-ND",
#       TRUE ~ "other license"
#     )) |>
#   rename(license_fair_enough = license) |>
#   select(-score)
#     
# fair_enough_summary <- fair_enough_summary |> left_join(fair_enough_license, by = "guid") |>
#   mutate(license_fair_enough = replace_na(license_fair_enough, "no license"))
#   
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # Join FUJI and FAIR Enough data ----
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# fuji_fair_enough_summary_results <-
#   fuji_summary_results_all |> full_join(fair_enough_summary, by = "rd_id")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
charite_rd_od_2020_2024_join <- charite_rd_od_cleaned_repos |>
  left_join(fuji_summary_results_all,
            by = c("data_identifier" = "rd_id")) |>
  select(-id) |>
  left_join(fuji_license, by = c("data_identifier" = "rd_id")) |>
  mutate(license_fuji = replace_na(license, "no license")) |>
  left_join(fuji_guid_scheme, by = c("data_identifier" = "rd_id")) |>
  rename(license_numbat = license_name)

save(charite_rd_od_2020_2024_join, file = "output/Rdata/charite_rd_od_join_2020_2024.Rdata")
load("output/Rdata/charite_rd_od_join_2020_2024.Rdata")


results_long <-
  charite_rd_od_2020_2024_join |> pivot_longer(cols = c("fuji_percent"), #, "fair_enough_percent"),
                                         names_to = "assessment_tool")

summary_grouped_repository <- results_long |>
  drop_na(value) |>
  group_by(assessment_tool, repository_type) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n(), .groups = "drop")

summary <- results_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            count = n())

save_data_xlsx(list(summary, summary_grouped_repository), name= c("summary", "summary_repository_2020_2024"))


