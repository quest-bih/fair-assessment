#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize data ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(crosstalk)
library(plotly)
library(tidyverse)


load("output-Rdata/charite_rd_2020_final.Rdata")

data <- charite_rd_2020_final

data_ncbi <- data %>%
  mutate(repository_ncbi = case_when(str_detect(best_identifier, "ncbi") ~ "ncbi repository",
                                     repository_type == "field-specific repository" ~ "other field-specific repository",
                                     TRUE ~ repository_type), .after = repository_type) 


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly FAIR repo type ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_ncbi_long <- data_ncbi %>%
  select(best_identifier, repository_ncbi, guid_scheme_fuji, fuji_percent_f, fuji_percent_a, fuji_percent_i, fuji_percent_r) %>%
  pivot_longer(cols = starts_with("fuji_percent")) %>%
  mutate(name = case_when(name == "fuji_percent_f" ~ "F",
                          name == "fuji_percent_a" ~ "A",
                          name == "fuji_percent_i" ~ "I",
                          name == "fuji_percent_r" ~ "R"
  )) %>%
  mutate(name = factor(name, levels = c("F", "A", "I", "R"))) %>%
  mutate(value = value/100)

plot_1 <- data_ncbi_long %>%
  filter(repository_ncbi == "ncbi repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "NCBI rep.", boxmean = TRUE,
              color = list(color = "rgb(193,32,117)"),
              marker = list(color = "rgb(193,32,117)"),
              line = list(color = "rgb(193,32,117)"))

plot_2 <- data_ncbi_long %>%
  filter(repository_ncbi == "other field-specific repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "field-specific rep.", boxmean = TRUE,
              color = list(color = "rgb(182,213,224)"),
              marker = list(color = "rgb(182,213,224)"),
              line = list(color = "rgb(182,213,224)"))

plot_3 <- data_ncbi_long %>%
  filter(repository_ncbi == "general-purpose repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "general-purpose rep.", boxmean = TRUE, 
              color = list(color = "rgb(182,213,224)"),
              marker = list(color = 'rgb(182,213,224)'),
              line = list(color = 'rgb(182,213,224)'))

box_ncbi <- subplot(plot_1, plot_2, plot_3, shareY = TRUE) %>% hide_legend() %>%
  layout(yaxis = list(tickformat = ",.0%", title = FALSE),
         hovermode = "x",
         annotations = list(
           list(x = 0.05 , y = 1, text = "<b>NCBI repository</b>", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.5 , y = 1, text = "<b>other field-specific rep.</b>", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.95 , y = 1, text = "<b>general-purpose rep.</b>", showarrow = F, xref='paper', yref='paper')))

