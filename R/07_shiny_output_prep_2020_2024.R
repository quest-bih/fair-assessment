#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data for dashboard shiny app ----
# Contact: anastasiia.iarkaeva@charite.de (previous ownner: jan.taubitz@charite.de, Email not accessible)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(dplyr)
library(readr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load final data and prepare and export it for shiny app ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("output/Rdata/charite_rd_od_join_2020_2024.Rdata")

fair_assessement_2020_2024 <- charite_rd_od_2020_2024_join |>
  select(
    doi,
    year,
    data_identifier,
    repository_re3data,
    repository_type,
    license_fuji,
    license_numbat,
    guid_scheme_fuji = guid_scheme,
    starts_with("fuji_percent"),
    starts_with("FsF")) |>
  mutate(repository_type = case_when(repository_type == "disciplinary repository" ~ "field-specific repository",
                                     TRUE ~ repository_type))


to_num <- function(x) {
  if (is.list(x)) {
    x <- vapply(x, function(v) {
      if (is.null(v) || length(v) == 0) NA_character_ else as.character(v[[1]])
    }, character(1))
  }
  x <- as.character(x)
  x[x %in% c("NULL", "", "NA", "N/A")] <- NA_character_
  as.numeric(x)
}

fair_assessement_2020_2024 <- fair_assessement_2020_2024 %>%
  mutate(across(starts_with("FsF"), to_num))

write_csv(fair_assessement_2020_2024, file = "output/fair_assessment_2020_2024.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

