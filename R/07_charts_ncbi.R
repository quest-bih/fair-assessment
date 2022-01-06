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

data %>% filter(str_detect(best_identifier, "ncbi")) %>% distinct(article) %>% nrow()

data_ncbi <- data %>%
  mutate(repository_ncbi = case_when(str_detect(best_identifier, "ncbi") ~ "ncbi repository",
                                     repository_type == "field-specific repository" ~ "other field-specific repository",
                                     TRUE ~ repository_type), .after = repository_type) 


data_ncbi_rep <- data_ncbi %>%
  filter(repository_re3data != "Open Science Framework" | repository_ncbi != "ncbi repository") %>%
  group_by(repository_re3data, repository_ncbi) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(repository_re3data = fct_reorder(repository_re3data,
                               n, max, .desc = TRUE))


data_ncbi_rep <- data_ncbi %>%
  filter(repository_re3data != "Open Science Framework" | repository_ncbi != "ncbi repository") %>%
  group_by(repository_re3data, repository_ncbi) %>%
  mutate(repository_re3data = case_when(n() >= 3 ~ repository_re3data,
                                        n() <= 2 & repository_ncbi == "other field-specific repository" ~ "other field-specific repository",
                                        n() <= 2 & repository_ncbi == "general-purpose repository" ~ "other general-purpose repository",
                                    TRUE ~ "other ncbi repository")) %>%
  group_by(repository_re3data, repository_ncbi) %>%
  summarise(n = n(), mean_fuji = mean(fuji_percent/100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(repository_re3data = fct_reorder(repository_re3data,
                                          n, max, .desc = TRUE)) %>%
  mutate(repository_ncbi = factor(repository_ncbi, levels = c("ncbi repository", "other field-specific repository", "general-purpose repository")))


pal <- c("#c12075", "#b6d5e0", "#70acc0") %>% setNames(c("ncbi repository", "other field-specific repository", "general-purpose repository"))

bar_ncbi <- data_ncbi_rep %>%
  plot_ly(x = ~n, y = ~repository_re3data, color = ~repository_ncbi,
          colors = pal, text = ~n, textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(xaxis = list(autorange = "reversed", side = "top", title = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.5)) %>%
  config(displayModeBar = FALSE)

bar_ncbi_perc <- data_ncbi_rep %>%
  plot_ly(x = ~mean_fuji, y = ~repository_re3data, color = ~repository_ncbi,
          colors = pal, text = ~paste0(round(mean_fuji*100, 1), "%"), textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(xaxis = list(autorange = "reversed", side = "top", title = FALSE, tickformat = ",.0%"),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.7)) %>%
  config(displayModeBar = FALSE)

bar_ncbi_perc
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
              color = list(color = "#b6d5e0"),
              marker = list(color = "#b6d5e0"),
              line = list(color = "#b6d5e0"))

plot_3 <- data_ncbi_long %>%
  filter(repository_ncbi == "general-purpose repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "general-purpose rep.", boxmean = TRUE, 
              color = list(color = "#70acc0"),
              marker = list(color = '#70acc0'),
              line = list(color = '#70acc0'))

box_ncbi <- subplot(plot_1, plot_2, plot_3, shareY = TRUE) %>% hide_legend() %>%
  layout(yaxis = list(tickformat = ",.0%", title = FALSE),
         hovermode = "x",
         annotations = list(
           list(x = 0.05 , y = 1.05, text = "<b>NCBI repository</b>", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.5 , y = 1.05, text = "<b>other field-specific rep.</b>", showarrow = F, xref='paper', yref='paper'),
           list(x = 0.95 , y = 1.05, text = "<b>general-purpose rep.</b>", showarrow = F, xref='paper', yref='paper')))

data_ncbi_long_sum <- data_ncbi_long %>%
  group_by(repository_ncbi, name) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  mutate(repository_ncbi = factor(repository_ncbi, levels = c("ncbi repository", "other field-specific repository", "general-purpose repository")))

bar_ncbi_fair <- data_ncbi_long_sum %>%
  group_by(repository_ncbi) %>%
  do(p=plot_ly(., x = ~name, y = ~value, color = ~repository_ncbi, colors = pal, type = "bar",
               text = ~paste0(round(value*100, 0), "%"), textposition = 'outside', textangle = 0, textfont = list(color = "#000000")) %>%
       layout(yaxis = list(title = "Average FAIR Score according to F-UJI", tickformat = ",.0%"))
     ) %>%
  subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>% 
  layout(legend = list(orientation = "h")) %>%
  config(displayModeBar = FALSE)
