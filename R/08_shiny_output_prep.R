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
library(readxl)
library(plotly)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load final data and prepare and export it for shiny app ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("output/Rdata/charite_rd_2020_final.Rdata")

fair_assessement <- charite_rd_2020_final %>%
  select(
    article,
    best_identifier,
    repository,
    repository_re3data,
    repository_type,
    license_fuji,
    guid_scheme_fuji,
    starts_with("fuji_percent"),
    starts_with("FsF"))

write_csv(fair_assessement, file = "output/fair_assessment.csv")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# END ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

