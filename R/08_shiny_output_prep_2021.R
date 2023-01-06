#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data for dashboard shiny app ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load final data and prepare and export it for shiny app ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("output/Rdata/charite_rd_2021_join.Rdata")

fair_assessement_2021 <- charite_rd_2021_join |>
  select(
    article,
    best_identifier,
    repository,
    repository_re3data,
    repository_type,
    license_fuji,
    license_numbat,
    guid_scheme_fuji = guid_scheme,
    starts_with("fuji_percent"),
    starts_with("FsF")) |>
  mutate(repository_type = case_when(repository_type == "disciplinary repository" ~ "field-specific repository",
                                     TRUE ~ repository_type))

write_csv(fair_assessement_2021, file = "output/fair_assessment_2021.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

