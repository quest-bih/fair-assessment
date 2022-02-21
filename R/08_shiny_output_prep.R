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
# Load final data and prepare it for shiny app ----
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
    fuji_percent,
    fuji_percent_f,
    fuji_percent_a,
    fuji_percent_i,
    fuji_percent_r)

write_csv(fair_assessement, file = "output/fair_assessment.csv")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Test code for shiny app ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot_data <- fair_assessement

plot_fair_license <- function(plot_data)
{
  
  #pal_lic <- colorRampPalette(c("#007265", "#F1BA50", "#AA493A"))
  #  pal_license <- c(rev(pal_lic(6)), "#879C9D")
  pal_license <- c("#F1BA50", "#F1BA50","#F1BA50","#F1BA50","#F1BA50","#F1BA50","#879C9D")
  
  plot_data %>%
    group_by(license_fuji, repository_type) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(license_fuji, repository_type, fill = list(n = 0)) %>%
    group_by(repository_type) %>%
    mutate(perc = round(n/sum(n),3)) %>%
    ungroup() %>%
    mutate(has_license = case_when(license_fuji != "no license" ~ TRUE,
                                   TRUE ~ FALSE)) %>%
    mutate(rep_type_2 = case_when(repository_type == "field-specific repository" ~ paste0("field-specific repository\nn = ", sum(n[repository_type == "field-specific repository"])),
                                  repository_type == "general-purpose repository" ~ paste0("general-purpose repository\nn = ", sum(n[repository_type == "general-purpose repository"]))
    )) %>%
    plot_ly(
      x = ~ perc,
      y = ~ rep_type_2,
      color = ~ license_fuji,
      colors = pal_license,
      marker = list(line = list(color = "#000000",
                                width = 1))
    ) %>%
    add_bars(
      text = ~ license_fuji,
      textposition = 'inside',
      insidetextanchor = "middle",
      textangle = 0,
      textfont = list(color = "#ffffff", size = 14)
    ) %>%
    layout(
      #margin = marg,
      barmode = "stack",
      xaxis = list(
        title = FALSE,
        side = "top",
        tickformat = ",.0%"
      ),
      # , zerolinecolor = "#F7F7F7FF", zerolinewidth = 1
      yaxis = list(title = FALSE, side = "right"),
      uniformtext = list(minsize = 8, mode = "hide"),
      legend = list(orientation = 'h', traceorder = "normal", x = 1.2, xanchor = "right")
    )
}

plot_fair_license(fair_assessement)
