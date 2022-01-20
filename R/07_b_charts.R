#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualizations of FAIR Assessments ----
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

load("output/Rdata/charite_rd_2020_final.Rdata")
data <- charite_rd_2020_final

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create color palettes ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Colors for repository type
pal <- c("#879C9D", "#F1BA50") %>% setNames(c("field-specific repository", "general-purpose repository"))
pal_single <- "#B6B6B6"
pal_bg <- "#DCE3E5"

pal_bar <- list(color = pal_single, line = list(color = "#000000", width = 1))

# Charité Dashboard color palette
color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                   "#303A3E", "#007265", "#634587", "#000000",
                   "#DCE3E5")

# Show colors from vector with colors
# scales::show_col(color_palette)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly FAIR repo type ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Long data to analyse FAIR main principles
data_2 <- data %>%
  select(
    best_identifier,
    repository_re3data, 
    repository_type,
    guid_scheme_fuji,
    fuji_percent_f,
    fuji_percent_a,
    fuji_percent_i,
    fuji_percent_r
  ) %>%
  pivot_longer(cols = starts_with("fuji_percent")) %>%
  mutate(
    name = case_when(
      name == "fuji_percent_f" ~ "F",
      name == "fuji_percent_a" ~ "A",
      name == "fuji_percent_i" ~ "I",
      name == "fuji_percent_r" ~ "R"
    )
  ) %>%
  mutate(name = factor(name, levels = c("F", "A", "I", "R"))) %>%
  mutate(value = value / 100) %>%
  drop_na(value)

# Faceted box plot (not included)
plot_1 <- data_2 %>%
  filter(repository_type == "field-specific repository") %>%
  plot_ly(x = ~ name, y = ~ value) %>%
  add_boxplot(
    name = "field-specific rep.",
    boxmean = TRUE,
    color = list(color = "#879C9D"),
    marker = list(color = "#879C9D"),
    line = list(color = "#879C9D")
  ) %>%
  layout(xaxis = list(tickprefix="<b>",ticksuffix ="</b>"))

plot_2 <- data_2 %>%
  filter(repository_type == "general-purpose repository") %>%
  plot_ly(x = ~ name,
          y = ~ value) %>%
  add_boxplot(
    name = "general-purpose rep.",
    boxmean = TRUE,
    color = list(color = "#F1BA50"),
    marker = list(color = "#F1BA50"),
    line = list(color = "#F1BA50")
  ) %>%
  layout(title = "Faceted Box Plot FAIRness by Repository Type",
         xaxis = list(tickprefix="<b>",ticksuffix ="</b>"))

box_faceted <-
  subplot(plot_1, plot_2, shareY = TRUE) %>% hide_legend() %>%
  layout(
    yaxis = list(tickformat = ",.0%", title = "FAIR Score according to F-UJI"),
    annotations = list(
      list(
        x = 0.1 ,
        y = 1,
        text = "field-specific repository",
        showarrow = F,
        xref = 'paper',
        yref = 'paper'
      ),
      list(
        x = 0.9 ,
        y = 1,
        text = "general-purpose repository",
        showarrow = F,
        xref = 'paper',
        yref = 'paper'
      )
    )
  ) %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Faceted box plot in dashboard style ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

box_faceted_margin <- box_faceted %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = -0.08 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = -0.08 ,
        y = 1.30,
        text = "<b>18 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = -0.08 ,
        y = 1.15,
        text = "is the average FAIR score of open data from Charité authors in 2020",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Grouped Box and Violin Plot ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

box_grouped <- data_2 %>%
  plot_ly(x = ~name, y = ~value, color = ~repository_type, colors = pal) %>%
  add_boxplot(boxmean = TRUE) %>%
  layout(boxmode = "group",
         title = "Grouped Box Plot FAIRness by Repository Type",
         legend = list(orientation = "h"),
         yaxis = list(title = "FAIR Score according to F-UJI", tickformat = ",.0%"),
         xaxis = list(title = FALSE)) %>%
  config(displayModeBar = FALSE)

violin_grouped <- data_2 %>%
  plot_ly(x = ~name, y = ~value, color = ~repository_type, colors = pal) %>%
  add_trace(type = "violin") %>%
  layout(violinmode = "group",
         title = "Grouped Violin Plot FAIRness by Repository Type",
         legend = list(orientation = "h"),
         yaxis = list(title = "FAIR Score according to F-UJI", tickformat = ",.0%"),
         xaxis = list(title = FALSE)) %>%
  config(displayModeBar = FALSE)

data_2_sum <- data_2 %>%
  group_by(repository_type, name) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup()

bar_faceted <- data_2_sum %>%
  group_by(repository_type) %>%
  group_map(~ plot_ly(., x = ~name, y = ~value, color = ~repository_type, colors = pal, type = "bar",
                      text = ~paste0(round(value*100, 0), "%"), textposition = 'outside', textangle = 0, textfont = list(color = "#000000")) %>%
              layout(yaxis = list(title = "Average FAIR Score according to F-UJI", tickformat = ",.0%")),
            .keep = TRUE) %>%
  subplot(nrows = 1, shareX = FALSE, shareY = TRUE) %>% 
  layout(title = "Faceted Bar Plot FAIRness by Repository Type",
         legend = list(orientation = "h")) %>%
  config(displayModeBar = FALSE)

bar_grouped <- data_2_sum %>%
  plot_ly(x = ~name, y = ~value, color = ~repository_type, colors = pal) %>%
  add_bars(text = ~paste0(round(value*100, 0), "%"), textposition = 'outside', textangle = 0, textfont = list(color = "#000000")) %>%
  layout(barmode = "group",
         title = "Grouped Bar Plot FAIRness by Repository Type",
         legend = list(orientation = "h"),
         yaxis = list(title = "Average FAIR Score according to F-UJI", tickformat = ",.0%"),
         xaxis = list(title = FALSE)) %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Faceted Violin Plot ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

chart_violin_repository <- data_2 %>%
  ggplot(aes(
    x = name,
    y = value,
    fill = repository_type,
    group = name,
    text = paste0(
      "FAIR Principle: ",
      name,
      "<br>FAIR Score: ",
      round(value, 1) * 100,
      "%",
      "<br>Repository: ",
      repository_re3data
    )
  )) +
  #geom_boxplot() +
  geom_violin(trim = TRUE,
              scale = "width",
              na.rm = TRUE) +
  geom_jitter(
    height = 0.015,
    width = 0.45,
    size = 0.5,
    shape = 21,
    alpha = 0.2,
    color = "#000000",
    na.rm = TRUE
  ) +
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.3,
    size = 0.25,
    color = "#000000",
    na.rm = TRUE
  ) +
  facet_wrap( ~ repository_type) +
  theme_minimal() +
  labs(title = "FAIRness according to FUJI assessment") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
   # axis.text.x = element_text(face="bold"),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank()
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal)


chart_violin_repository_plotly <- ggplotly(chart_violin_repository,
                                           tooltip = "text") %>% #list("AB" = "name", "XY" = "value")
  layout(title = "Faceted Violin Plot FAIRness by Repository Type",
         yaxis = list(
           title = list(text = "FAIR score according to F-UJI", font = list(size = 12)),
           tickformat = ",.0%"
         ))


chart_violin_repository_plotly <- chart_violin_repository_plotly %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = -0.08 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = -0.08 ,
        y = 1.35,
        text = "<b>18 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = -0.08 ,
        y = 1.20,
        text = "is the average FAIR score of open data from Charité authors in 2020",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )
  
# Documentation Tooltip: 
# https://stackoverflow.com/questions/38733403/edit-labels-in-tooltip-for-plotly-maps-using-ggplot2-in-r
# https://github.com/tidyverse/ggplot2/issues/3749
# https://stackoverflow.com/questions/40598011/how-to-customize-hover-information-in-ggplotly-object/40598524

# Ungrouped / unfaceted box and violin plot (not included)
box <- data_2 %>%
  plot_ly(x = ~ name, y = ~ value, type = "box",
          color = list(color = "#B6B6B6"),
          marker = list(color = "#B6B6B6"),
          line = list(color = "#B6B6B6")) %>%
            #  add_boxplot(boxmean = TRUE) %>%
  layout(
    title = "Box Plot FAIR Principles",
    legend = list(orientation = "h"),
    yaxis = list(title = "FAIR Score according to F-UJI", tickformat = ",.0%"),
    xaxis = list(title = FALSE)
  ) %>%
  config(displayModeBar = FALSE)

violin <- data_2 %>%
  plot_ly(
    x = ~ name,
    y = ~ value,
    split = ~ name,
    type = "violin",
    meanline = list(visible = TRUE),
    color = list(color = "#B6B6B6"),
    marker = list(color = "#B6B6B6"),
    line = list(color = "#B6B6B6")
  ) %>%
  layout(
    title = "Violin Plot FAIR Principles",
    yaxis = list(title = "FAIR Score according to F-UJI", tickformat = ",.0%"),
    xaxis = list(title = FALSE)
  ) %>%
  hide_legend() %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly licenses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_license <- data %>%
  group_by(license_fuji, repository_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(license_fuji, repository_type, fill = list(n = 0)) %>%
  mutate(perc = round(n/sum(n),3))


licenses_bar_grouped <- data_license %>%
  plot_ly(x = ~license_fuji, y = ~perc, color = ~repository_type, colors = pal) %>%
  add_bars(text = ~n, textposition = 'outside', textangle = 0, textfont = list(color = "#000000")) %>%
  layout(barmode = "group",
         title = "Grouped Bar Plot Licenses by Repository Type",
         legend = list(orientation = "h"),
         yaxis = list(title = FALSE, tickformat = ",.0%"),
         xaxis = list(title = FALSE)) %>%
  config(displayModeBar = FALSE)

#~paste0(round(perc*100, 1), "%")

data_license <- data %>%
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
                                ))

# library(colortools)
# sequential("#F1BA50", percentage = 100/12)

pal_lic <- colorRampPalette(c("#F1C164", "#F19F00"))
#scales::show_col(test(6))
#pal_license <- c("#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#879C9D") %>% setNames(levels(data_license$license_fuji))
pal_license <- c(rev(pal_lic(length(levels(data_license$license_fuji))-1)), "#879C9D") %>% setNames(levels(data_license$license_fuji))



licenses_bar <- data_license %>%
  plot_ly(x = ~perc, y = ~rep_type_2, color = ~license_fuji, colors = pal_license,
          marker = list(line = list(color = "#000000",
                                    width = 1))) %>%
  add_bars(text = ~license_fuji, textposition = 'inside', insidetextanchor = "middle", textangle = 0, textfont = list(color = "#ffffff")) %>%
  layout(barmode = "stack",
         xaxis = list(title = FALSE, autorange = "reversed", side = "top", tickformat = ",.0%", zeroline = FALSE),
         yaxis = list(title = FALSE, side = "right"),
         uniformtext = list(minsize = 8, mode = "hide"))  %>%
  hide_legend()

marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

licenses_bar_marg <- licenses_bar %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = 0 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = 0 ,
        y = 1.30,
        text = "<b>51 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = 0 ,
        y = 1.15,
        text = "of research data sets published in general-purpose repositories in 2020 have an open license",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# License Chart Talk EB ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


test <- data %>%
  group_by(repository_re3data, license_fuji) %>%
  count()

# library(colortools)
# sequential("#F1BA50", percentage = 100/20)

pal_lic <- colorRampPalette(c("#F1DCB5", "#F19F00"))

pal_lic <- colorRampPalette(c("#007265", "#F1C36C", "#F1BA50"))

#scales::show_col(pal_lic(6))
#pal_license <- c("#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#F1BA50", "#879C9D") %>% setNames(levels(data_license$license_fuji))
pal_license <- c(rev(pal_lic(length(levels(data_license$license_fuji))-1)), "#879C9D") %>% setNames(levels(data_license$license_fuji))

marg <- list(
  pad = 10
)


licenses_bar <- data_license %>%
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
    yaxis = list(title = FALSE),
    uniformtext = list(minsize = 8, mode = "hide"),
    legend = list(orientation = 'h', traceorder = "normal", x = -0.25)
  )


marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 10
)

licenses_bar_marg_2 <- licenses_bar %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = -0.35 ,
        y = 1.6,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = -0.35 ,
        y = 1.45,
        text = "<b>51 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = -0.35 ,
        y = 1.25,
        text = "of research data sets published in general-purpose repositories in 2020 have an open license",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )













#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Resource Identifier ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_id <- data %>%
  group_by(guid_scheme_fuji, repository_type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(guid_scheme_fuji, repository_type, fill = list(n = 0)) %>%
  mutate(perc = round(n/sum(n),3)) %>%
  drop_na()

id_bar_grouped <- data_id %>%
  plot_ly(x = ~guid_scheme_fuji, y = ~perc, color = ~repository_type, colors = pal) %>%
  add_bars(text = ~n, textposition = 'outside', textangle = 0, textfont = list(color = "#000000")) %>%
  layout(barmode = "group",
         title = "Grouped Bar Plot Ressource Identifiers by Repository Type",
         legend = list(orientation = "h"),
         yaxis = list(title = FALSE, tickformat = ",.0%"),
         xaxis = list(title = FALSE)) %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Repositories ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_rep <- data %>%
  group_by(repository_re3data, repository_type) %>%
  mutate(repository_re3data = case_when(n() >= 3 ~ repository_re3data,
                                        n() <= 2 & repository_type == "field-specific repository" ~ "Other field-specific Repositories",
                                        n() <= 2 & repository_type == "general-purpose repository" ~ "Other general-purpose Repositories",
                                        TRUE ~ "Other field-specific Repository")) %>%
  group_by(repository_re3data, repository_type) %>%
  summarise(n = n(), mean_fuji = mean(fuji_percent/100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(repository_re3data = fct_reorder(repository_re3data,
                                          n, max, .desc = TRUE),
         repository_re3data = fct_relevel(repository_re3data, "Other field-specific Repositories", after = Inf),
         repository_re3data = fct_relevel(repository_re3data, "Other general-purpose Repositories", after = Inf))


rep_bar_freq <- data_rep %>%
  plot_ly(x = ~n, y = ~repository_re3data, color = ~repository_type,
          colors = pal, text = ~n, textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(title = "Frequency of Repositories",
         xaxis = list(autorange = "reversed", side = "top", title = FALSE, showticklabels = FALSE, zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.5)) %>%
  config(displayModeBar = FALSE)

rep_bar_fair <- data_rep %>%
  plot_ly(x = ~mean_fuji, y = ~repository_re3data, color = ~repository_type,
          colors = pal, text = ~paste0(round(mean_fuji*100, 1), "%"), textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(title = "FAIR Score of Repositories (ordered by freq.)",
         xaxis = list(autorange = "reversed", side = "top", title = FALSE, tickformat = ",.0%", showticklabels = FALSE, zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.7)) %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Publisher ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_pub <- data %>%
  group_by(publisher_unpaywall, repository_type) %>%
  mutate(publisher_unpaywall = case_when(n() >= 2 ~ publisher_unpaywall,
                                        n() <= 1 ~ "Other Publishers",
                                        TRUE ~ publisher_unpaywall)) %>%
  group_by(publisher_unpaywall, repository_type) %>%
  summarise(n = n(), mean_fuji = mean(fuji_percent/100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(publisher_unpaywall = fct_reorder(publisher_unpaywall,
                                          n, max, .desc = TRUE),
         publisher_unpaywall = fct_relevel(publisher_unpaywall, "Other Publishers", after = Inf)) %>%
  mutate(repository_type = factor(repository_type, levels = c("field-specific repository", "general-purpose repository")))

pub_bar_freq <- data_pub %>%
  plot_ly(x = ~n, y = ~publisher_unpaywall, color = ~repository_type,
          colors = pal, text = ~n, textposition = 'auto', textangle = 0, textfont = list(color = "#000000"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(barmode = "stack",
         title = "Frequency of Publishers",
         xaxis = list(autorange = "reversed", side = "top", title = FALSE, showticklabels = FALSE, zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.5)) %>%
  config(displayModeBar = FALSE)

pal_pub <- c("#879C9D", "#F1BA50")

pub_od <- data %>%
  select(starts_with("publisher")) %>%
  group_by(publisher_unpaywall) %>%
  summarise(n = n(), 
            publisher_n_unpaywall = max(publisher_n_unpaywall),
            publisher_prop_unpaywall = round(max(publisher_prop_unpaywall), 4)) %>%
  filter(n >= 5) %>%
 # mutate(publisher_unpaywall = replace(publisher_unpaywall, publisher_unpaywall == "Rockefeller University Press", "Rockefeller University Press\nN = 5")) %>%
  mutate(publisher_unpaywall = paste0(publisher_unpaywall, "<br>n = ", publisher_n_unpaywall, ", p = ", publisher_prop_unpaywall)) %>%
  mutate(prop_od = round(n / publisher_n_unpaywall, 2)) %>%
  mutate(prop_no_od = 1 - prop_od) %>%
 # ungroup() %>%
  mutate(publisher_unpaywall = fct_reorder(publisher_unpaywall, prop_od, .desc = TRUE)) %>%
  pivot_longer(cols = starts_with("prop"))


pub_od_bar <- pub_od %>%
  plot_ly(x = ~value, y = ~publisher_unpaywall, color = ~name, colors = pal_pub,
          text = ~paste(value*100, "%"), textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff")) %>%
  add_bars() %>%
  add_annotations(
    x=0.2,
    y=1,
    xref = "x",
    yref = "y",
    text = "without open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  add_annotations(
    x=0.75,
    y=1,
    xref = "x",
    yref = "y",
    text = "with open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  layout(barmode = "stack",
         title = FALSE,
         xaxis = list( side = "top", title = FALSE, tickformat = ",.0%", zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE, tickfont = list(size = 7)),
         uniformtext = list(minsize = 8, mode = "hide")) %>% # legend = list(orientation = "h")) 
  hide_legend() %>%
  config(displayModeBar = FALSE)


marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

pub_od_bar_margin <- pub_od_bar %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = 0 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = 0 ,
        y = 1.30,
        text = "<b>100 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = 0 ,
        y = 1.15,
        text = "of 2020 Rockefeller UP articles from Charité authors provide open data",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )

# "Proportion of OD by Publisher (only Publisher with OD >= 5)"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Journals----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_jour <- data %>%
  group_by(journal_name_unpaywall) %>%
  mutate(journal_name_unpaywall = case_when(n() >= 3 ~ journal_name_unpaywall,
                                         n() <= 2 ~ "Other Journals",
                                         TRUE ~ journal_name_unpaywall)) %>%
  group_by(journal_name_unpaywall) %>%
  summarise(n = n(), mean_fuji = mean(fuji_percent/100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(journal_name_unpaywall = fct_reorder(journal_name_unpaywall,
                                           n, max, .desc = TRUE),
         journal_name_unpaywall = fct_relevel(journal_name_unpaywall, "Other Journals", after = Inf))

jour_bar_freq <- data_jour %>%
  plot_ly(x = ~n, y = ~journal_name_unpaywall,
          marker = pal_bar,
          text = ~n, textposition = 'auto', textangle = 0, textfont = list(color = "#000000"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(barmode = "stack",
         title = "Frequency of Journals",
         xaxis = list(autorange = "reversed", side = "top", title = FALSE, showticklabels = FALSE, zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.5)) %>%
  config(displayModeBar = FALSE)

jour_od <- data %>%
  distinct(article, .keep_all = TRUE) %>%
  select(starts_with("journal")) %>%
  group_by(journal_name_unpaywall) %>%
  summarise(n = n(), 
            journal_n_unpaywall = max(journal_n_unpaywall),
            journal_prop_unpaywall = round(max(journal_prop_unpaywall), 4)) %>%
  filter(n >= 3) %>%
  # mutate(publisher_unpaywall = replace(publisher_unpaywall, publisher_unpaywall == "Rockefeller University Press", "Rockefeller University Press\nN = 5")) %>%
  mutate(journal_name_unpaywall = paste0(journal_name_unpaywall, "<br>n = ", journal_n_unpaywall, ", p = ", journal_prop_unpaywall)) %>%
  mutate(prop_od = round(n / journal_n_unpaywall, 2)) %>%
  mutate(prop_no_od = 1 - prop_od) %>%
  # ungroup() %>%
  mutate(journal_name_unpaywall = fct_reorder(journal_name_unpaywall, prop_od, .desc = TRUE)) %>%
  pivot_longer(cols = starts_with("prop"))


jour_od_bar <- jour_od %>%
  plot_ly(x = ~value, y = ~journal_name_unpaywall, color = ~name, colors = pal_pub,
          text = ~paste(value*100, "%"), textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff")) %>%
  add_bars() %>%
  add_annotations(
    x=0.25,
    y=5,
    xref = "x",
    yref = "y",
    text = "without open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  add_annotations(
    x=0.75,
    y=5,
    xref = "x",
    yref = "y",
    text = "with open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  layout(barmode = "stack",
         title = FALSE,
         xaxis = list( side = "top", title = FALSE, tickformat = ",.0%", zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE, tickfont = list(size = 7)),
         uniformtext = list(minsize = 6, mode = "hide")) %>% # legend = list(orientation = "h")) 
  hide_legend() %>%
  config(displayModeBar = FALSE)


marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

jour_od_bar_margin <- jour_od_bar %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = 0 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = 0 ,
        y = 1.30,
        text = "<b>100 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = 0 ,
        y = 1.15,
        text = "of 2020 Journal of Experimental Medicine articles from Charité authors provide open data",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )








jour_od <- data %>%
  distinct(article, .keep_all = TRUE) %>%
  select(starts_with("journal")) %>%
  group_by(journal_name_unpaywall) %>%
#  mutate(journal_name_unpaywall = case_when(journal_n_unpaywall >= 16 ~ journal_name_unpaywall,
 #                                           journal_n_unpaywall <= 15 ~ "Other Journals",
  #                                          TRUE ~ journal_name_unpaywall)) %>%
  mutate(journal_n_unpaywall = replace(journal_n_unpaywall, journal_name_unpaywall == "Other Journals", sum(journal_n_unpaywall))) %>%
  summarise(n = n(), 
            journal_n_unpaywall = max(journal_n_unpaywall),
            journal_prop_unpaywall = round(max(journal_prop_unpaywall), 4)) %>%
  filter(journal_n_unpaywall >= 16) %>%
  # mutate(publisher_unpaywall = replace(publisher_unpaywall, publisher_unpaywall == "Rockefeller University Press", "Rockefeller University Press\nN = 5")) %>%
  mutate(journal_name_unpaywall = paste0(journal_name_unpaywall, "<br>n = ", journal_n_unpaywall, ", p = ", journal_prop_unpaywall)) %>%
  mutate(prop_od = round(n / journal_n_unpaywall, 2)) %>%
  mutate(prop_no_od = 1 - prop_od) %>%
  # ungroup() %>%
  mutate(journal_name_unpaywall = fct_reorder(journal_name_unpaywall, prop_od, .desc = TRUE)) %>%
  pivot_longer(cols = starts_with("prop"))


jour_od_bar <- jour_od %>%
  plot_ly(x = ~value, y = ~journal_name_unpaywall, color = ~name, colors = pal_pub,
          text = ~paste(value*100, "%"), textposition = 'inside', textangle = 0, textfont = list(color = "#ffffff")) %>%
  add_bars() %>%
  add_annotations(
    x=0.25,
    y=0,
    xref = "x",
    yref = "y",
    text = "without open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  add_annotations(
    x=0.75,
    y=0,
    xref = "x",
    yref = "y",
    text = "with open data",
    xanchor = 'center',
    showarrow = FALSE,
    font = list(color = "#ffffff")
  ) %>%
  layout(barmode = "stack",
         title = FALSE,
         xaxis = list( side = "top", title = FALSE, tickformat = ",.0%", zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE, tickfont = list(size = 7)),
         uniformtext = list(minsize = 6, mode = "hide")) %>% # legend = list(orientation = "h")) 
  hide_legend() %>%
  config(displayModeBar = FALSE)


marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

jour_od_bar_margin_top <- jour_od_bar %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
    title = FALSE,
    annotations = list(
      list(
        x = 0 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = 0 ,
        y = 1.30,
        text = "<b>15</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = 0 ,
        y = 1.15,
        text = "articles or more were published in this journals by Charité authors in 2020",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )








#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Journal Classification FoR----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_class <- data %>%
  group_by(fields_of_research) %>%
  mutate(fields_of_research = case_when(n() >= 2 ~ fields_of_research,
                                            n() <= 1 ~ "Other Fields of Research",
                                            TRUE ~ fields_of_research)) %>%
  group_by(fields_of_research) %>%
  summarise(n = n(), mean_fuji = mean(fuji_percent/100, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(fields_of_research = fct_reorder(fields_of_research,
                                              n, max, .desc = TRUE),
         fields_of_research = fct_relevel(fields_of_research, "Other Fields of Research", after = Inf))

class_bar_freq <- data_class %>%
  plot_ly(x = ~n, y = ~fields_of_research,
          marker = pal_bar,
          text = ~n, textposition = 'auto', textangle = 0, textfont = list(color = "#000000"),
          width = "100%", height = 650) %>% #, color = ~repository_ncbi
  add_bars() %>%
  layout(barmode = "stack",
         title = "Frequency of Fields of Research",
         xaxis = list(autorange = "reversed", side = "top", title = FALSE, showticklabels = FALSE, zeroline = FALSE),
         yaxis = list(autorange = "reversed", side = "right", title = FALSE),
         legend = list(x = 0.1, y = 0.5)) %>%
  config(displayModeBar = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly licenses 2 ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sub1 <- data %>%
  group_by(repository_type, license_fuji) %>%
  summarise(count = n()) %>%
  mutate(has_license = case_when(license_fuji == "no license" ~ "no license",
                                 TRUE ~ "has license")) %>%
  filter(repository_type == "general-purpose repository") %>%
  plot_ly(x = ~has_license, y = ~count, color = ~license_fuji, type = 'bar',
          showlegend = FALSE, legendgroup = ~license_fuji) %>%
  layout(barmode = 'stack') %>% 
  layout(xaxis = list(title = "general_purpose"))

sub2 <- data %>%
  group_by(repository_type, license_fuji) %>%
  summarise(count = n()) %>%
  mutate(has_license = case_when(license_fuji == "no license" ~ "no license",
                                 TRUE ~ "has license")) %>%
  filter(repository_type == "field-specific repository") %>%
  plot_ly(x = ~has_license, y = ~count, color = ~license_fuji, type = 'bar',
          showlegend = TRUE, legendgroup = ~license_fuji) %>%
  layout(barmode = 'stack') %>% 
  layout(xaxis = list(title = "field-specific"))

p <- subplot(sub1, sub2, titleX = TRUE, shareY = TRUE) %>% layout(showlegend = TRUE) %>% plotly_build()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly linked licenses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shared_repo <- data %>% SharedData$new(key = ~repository_type)

bc <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_type) %>%
  summarise(n = n()) %>%
  add_bars(x = ~repository_type, y = ~n, color = ~repository_type, colors = pal,
           text = ~repository_type, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = FALSE),
         barmode = "overlay")

bubble <- shared_repo %>%
  plot_ly() %>%
  group_by(license_fuji, repository_type) %>%
  summarise(n = n()) %>%
  add_bars(x = ~license_fuji, y = ~n, color = ~repository_type, colors = pal,
           hoverinfo = "text", text = ~license_fuji, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = FALSE),
         barmode = "overlay")

licenses_linked <- subplot(bc, bubble, widths = c(0.2, 0.8)) %>% highlight(on = "plotly_click", off = "plotly_doubleclick") %>% hide_legend()



data_license <- data %>%
  group_by(repository_type, license_fuji) %>%
  count() %>%
  ungroup()

bc <- data_license %>%
  plot_ly() %>%
  add_bars(x = ~repository_type, y = ~n, color = ~repository_type, colors = pal,
           text = ~repository_type, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = FALSE),
         barmode = "stack")

bubble <- data_license %>%
  plot_ly() %>%
  add_bars(x = ~license_fuji, y = ~n, color = ~repository_type, colors = pal,
           hoverinfo = "text", text = ~license_fuji, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = TRUE),
         barmode = "group",
         legend = list(orientation = "h"))

licenses_linked <- subplot(bc, bubble, widths = c(0.2, 0.8)) %>% highlight(on = "plotly_click", off = "plotly_doubleclick") %>% hide_legend()


data_license <- data %>%
  group_by(repository_type, license_fuji) %>%
  count() %>%
  ungroup()

shared_repo <- data_license %>% SharedData$new(key = ~repository_type)

bc <- shared_repo %>%
  plot_ly() %>%
  # group_by(repository_type) %>%
  # summarise(n = n()) %>%
  add_bars(x = ~repository_type, y = ~n, color = ~repository_type, colors = pal,
           text = ~repository_type, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = FALSE),
         barmode = "group")

bubble <- shared_repo %>%
  plot_ly() %>%
  # group_by(license_fuji, repository_type) %>%
  # summarise(n = n()) %>%
  add_bars(x = ~license_fuji, y = ~n, color = ~repository_type, colors = pal,
           hoverinfo = "text", text = ~license_fuji, textposition = 'auto', textfont = list(color = "#000000")) %>%
  layout(xaxis= list(showticklabels = FALSE),
         barmode = "group")

licenses_linked <- subplot(bc, bubble, widths = c(0.2, 0.8)) %>% highlight(on = "plotly_click", off = "plotly_doubleclick") %>% hide_legend()

licenses_linked

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly FUJI v FAIR Enough ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shared_repo <- data %>% SharedData$new(key = ~ repository_type)

bc <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_type) %>%
  summarise(n = n()) %>%
  add_bars(
    x = ~ repository_type,
    y = ~ n,
    text = ~ repository_type,
    textposition = 'inside',
    insidetextfont = list(color = "#ffffff"),
    insidetextanchor = "start", 
    textangle = 270,
    size = 3,
    marker = list(color = pal)
  ) %>%
  layout(barmode = "overlay",
    xaxis = list(title = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(title = "Number datasets", zeroline = FALSE, side = "right")
  )

bubble <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_re3data, repository_type) %>%
  summarise(
    n = n(),
    fair_enough = mean(fair_enough_percent) / 100,
    fuji = mean(fuji_percent) / 100
  ) %>%
  add_markers(
    x = ~ fuji,
    y = ~ fair_enough,
    size = ~ n,
    color = ~ repository_type,
    colors = pal,
    hoverinfo = "text",
    text = ~ repository_re3data,
    hovertemplate = paste(
      "<b>%{text}</b><br>",
      "FUJI: %{x}<br>",
      "FAIR Enough: %{y}",
      "<extra></extra>"
    ),
    marker = list(sizemode = "diameter")
  ) %>%
  layout(
    xaxis = list(
      title = "FUJI Score",
      tickformat = ",.0%",
      zeroline = FALSE
    ),
    yaxis = list(title = "FAIR Enough Score", tickformat = ",.0%")
  )

scatter_fuji_fair <-
  subplot(
    bubble,
    bc,
    nrows = 1,
    widths = c(0.8, 0.2),
    titleX = TRUE,
    titleY = TRUE
  ) %>% hide_legend() %>%
  layout(title = "FAIR Score: FUJI vs. FAIR Enough") %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly Number of Datasets per Repository and FUJI Score Percent ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bc <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_type) %>%
  summarise(n = n()) %>%
  add_bars(y = ~repository_type, x = ~n,
           text = ~repository_type, textposition = 'inside', size = 3,
           marker = list(color = c("blue", "gray"))) %>%
  layout(barmode = "overlay", yaxis = list(showticklabels = FALSE)) 

bubble <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_re3data, repository_type) %>%
  summarise(n = n(), mean = mean(fuji_percent)) %>%
  add_markers(y = ~mean, x = ~n, color = ~repository_type, colors = c("blue", "gray"),
              hoverinfo = "text", text = ~repository_re3data,
              hovertemplate = paste(
                "<b>%{text}</b><br><br>",
                "Number: %{y}<br>",
                "Mean FAIR in %: %{x}<br>"),
              size = ~n, marker = list(sizemode = "diameter"))

subplot(bc, bubble, nrows = 2, heights = c(0.2, 0.8)) %>% hide_legend()

scatter_fuji_type <- subplot(bubble, bc, nrows = 2, heights = c(0.8, 0.2)) %>% hide_legend()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly Publisher v FUJI Percent ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# data %>% count(publisher_unpaywall, journal_name_unpaywall)
# 
# test <- data %>%
#   group_by(repository_type, publisher_unpaywall, journal_name_unpaywall) %>%
#   summarise(n = n(), fuji = mean(fuji_percent)) %>%
#   ungroup() %>%
#   mutate(publisher_unpaywall = fct_reorder(publisher_unpaywall, n, .fun = sum))
# 
# test %>%
#   plot_ly() %>%
#   add_bars(y = ~publisher_unpaywall, x = ~n, color = ~journal_name_unpaywall) %>%
#   layout(barmode = "stack") %>% layout(showlegend = FALSE)
# 
# 
# test <- data %>%
#   group_by(fields_of_research, repository_re3data) %>%
#   summarise(n = n()) %>%
#   ungroup()
# 
# test %>%
#   plot_ly() %>%
#   add_bars(y = ~fields_of_research, x = ~n, color = ~repository_re3data) %>%
#   layout(barmode = "stack") %>% layout(showlegend = FALSE)
# 
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # Static vis with ggplot ----
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# chart_bar_license <- data %>%
#   # filter(!is.na(repository_type)) %>%
#   ggplot(aes(license_fuji, fill = repository_type)) +
#   geom_bar(aes(y = (..count..)/sum(..count..))) +
#   #  facet_wrap( ~ repository_type) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         #  legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) +
#   labs(title = "Licenses according to FUJI assessment",
#        fill = "Repository") +
#   scale_y_continuous(labels = scales::percent)
# 
# chart_bar_count <- data %>%
#   filter(!is.na(repository_type)) %>%
#   group_by(repository) %>%
#   mutate(repository_big = case_when(n() >= 10 ~ repository,
#                                     n() <= 9 & repository_type == "field-specific repository" ~ "other field-specific repository",
#                                     n() <= 9 & repository_type == "general-purpose repository" ~ "other general-purpose repository",
#                                     TRUE ~ "other repository")) %>%
#   ungroup() %>%
#   ggplot(aes(x = repository_type, group = fct_rev(fct_infreq(repository_big)))) +
#   geom_bar(colour="black", fill = "lightgrey") + 
#   geom_text(aes(label = repository_big), stat = "count", colour = "black", size = 2.5, position = position_stack(0.5)) + # , stat = "count"
#   theme_minimal() +
#   theme(legend.position = "none") +
#   labs(title = "Count", subtitle = "(other repository is n < 10)")
# 
# library(treemapify)
# library(ggplot2)
# data_3 <- data %>%
#   group_by(repository) %>%
#   mutate(repository_big = case_when(n() >= 10 ~ repository,
#                                     TRUE ~ "other repository")) %>%
#   ungroup() %>%
#   filter(!is.na(repository_type)) %>%
#   group_by(repository_big, repository_type) %>%
#   summarise(sum = n())
# 
# chart_treemap_repository <- ggplot(data_3, aes(area = sum, fill = repository_type, label = repository_big, subgroup = repository_type)) +
#   geom_treemap() +
#   geom_treemap_subgroup_text(place = "bottom", grow = F, alpha = 0.5, colour =
#                                "black", fontface = "italic", min.size = 0) +
#   geom_treemap_text(colour = "white", place = "centre",
#                     grow = FALSE, min.size = 0) +
#   theme(legend.position = "none") +
#   labs(title = "Treemap of Repositories", subtitle = "(other repository is n < 10)")
# 
# 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly Treemap ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_treemap <- data %>% 
  group_by(repository, repository_re3data, repository_type) %>%
  summarise(n = n(), 
            fair_score = round(mean(fuji_percent, na.rm = TRUE),1),
            f_score = round(mean(fuji_percent_f, na.rm = TRUE),1),
            a_score = round(mean(fuji_percent_a, na.rm = TRUE),1),
            i_score = round(mean(fuji_percent_i, na.rm = TRUE),1),
            r_score = round(mean(fuji_percent_r, na.rm = TRUE),1)
            ) %>%
  ungroup()

data_treemap_head <-
  data.frame(
    repository = unique(data_treemap$repository_type),
    repository_re3data = NA,
    repository_type = NA,
    fair_score = NA,
    f_score = NA,
    a_score = NA,
    i_score = NA,
    r_score = NA,
    n = c(221, 82)
  )

data_treemap <- rbind(data_treemap_head, data_treemap)

# runif to add column with random numbers between x and y
# data_treemap <- data_treemap %>%
#   mutate(x = round(runif(40, 0.1:0.5), 1))

treemap_chart <- data_treemap %>% plot_ly(
  labels = ~repository,
  parents = ~repository_type,
  values = ~n,
  type ="treemap",
  branchvalues = "total",
  textinfo = "label+text",
  text = ~paste0(
                 "<span style='font-family:courier'>F: ", f_score, "%",
                 "<br>A: ", a_score, "%",
                 "<br>I: ", i_score, "%",
                 "<br>R: ", r_score, "%</span>"),
  hoverinfo = "text",
  hovertext = ~paste0("<b>", repository_re3data, 
                      "</b><br><span style='font-family:courier'>", "n: ", n, 
                      "<br>FAIR: ", fair_score, "%",
                      "<br>F: ", f_score, "%",
                      "<br>A: ", a_score, "%",
                      "<br>I: ", i_score, "%",
                      "<br>R: ", r_score, "%</span>"),
  textfont = list(color = "white"),
  marker = list(colors = pal)) 

treemap_chart

#   color = test$x
#   marker = list(colorscale = 'Reds')
#   marker = list(colors = pal, colorscale = test$x)

#fig %>% layout(treemapcolorway= pal)

# fig %>% layout(title = "Treemap Repositories")

marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

treemap_chart_marg <- treemap_chart %>%
  layout(
    margin = marg,
    paper_bgcolor = pal_bg,
    plot_bgcolor = pal_bg,
  #  title = FALSE,
    annotations = list(
      list(
        x = 0 ,
        y = 1.45,
        text = "<b>Open Data</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(size = 15, color = "#2F3E4E", family = "Arial")
      ),
      list(
        x = 0 ,
        y = 1.30,
        text = "<b>73 %</b>",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 35,
          color = "#9C2E7A",
          family = "Arial"
        )
      ),
      list(
        x = 0 ,
        y = 1.15,
        text = "of open data by Charité authors were published in field-specific repositories in 2020",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        font = list(
          size = 15,
          color = "#9C2E7A",
          family = "Arial"
        )
      )
    )
  )
