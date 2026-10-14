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


load("output/Rdata/charite_rd_2020_final.Rdata")
data <- charite_rd_2020_final

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly licenses ----
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

# Save and load plot
saveRDS(p, "output/charts/license_bar.rds")
d <- readRDS("output/charts/license_bar.rds")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly linked licenses ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shared_repo <- data %>% SharedData$new(key = ~repository_type)

bc <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_type) %>%
  summarise(n = n()) %>%
  add_bars(x = ~repository_type, y = ~n) %>%
  layout(barmode = "overlay")

bubble <- shared_repo %>%
  plot_ly() %>%
  group_by(license_fuji) %>%
  summarise(n = n()) %>%
  add_bars(x = ~license_fuji, y = ~n, hoverinfo = "text", text = ~license_fuji) %>%
  layout(barmode = "overlay")

licenses_linked <- subplot(bc, bubble) %>% highlight(on = "plotly_click", off = "plotly_doubleclick") %>% hide_legend()

saveRDS(licenses_linked, "output/charts/licenses_linked.rds")

library(htmlwidgets)
saveWidget(licenses_linked, "output/charts/licenses_linked.html", selfcontained = FALSE, libdir = "lib")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly FUJI v FAIR Enough ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shared_repo <- data %>% SharedData$new(key = ~repository_type)

bc <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_type) %>%
  summarise(n = n()) %>%
  add_bars(y = ~repository_type, x = ~n,
           text = ~repository_type, textposition = 'inside', size = 3,
           marker = list(color = c("blue", "gray"))) %>%
  layout(barmode = "overlay", yaxis = list(showticklabels = FALSE)) %>%
  layout(xaxis = list(title = "Number datasets"),
         yaxis = list(title = ""))

bubble <- shared_repo %>%
  plot_ly() %>%
  group_by(repository_re3data, repository_type) %>%
  summarise(n = n(), fair_enough = mean(fair_enough_percent), fuji = mean(fuji_percent)) %>%
  add_markers(x = ~fuji, y = ~fair_enough, color = ~repository_type, colors = c("blue", "gray"),
              hoverinfo = "text", text = ~repository_re3data,
              hovertemplate = paste(
                "<b>%{text}</b><br><br>",
                "FUJI: %{x}<br>",
                "FAIR Enough: %{y}<br>"),
              size = ~n, marker = list(sizemode = "diameter")) %>%
  layout(xaxis = list(title = "FUJI Score (%)"),
         yaxis = list(title = "FAIR Enough Score (%)"))

subplot(bubble, bc, nrows = 2, heights = c(0.8, 0.2), titleX = TRUE, titleY = TRUE) %>% hide_legend() %>%
  layout(title = "FUJI vs FAIR Enough")

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

subplot(bubble, bc, nrows = 2, heights = c(0.8, 0.2)) %>% hide_legend()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly Publisher v FUJI Percent ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data %>% count(publisher_unpaywall, journal_name_unpaywall)

test <- data %>%
  group_by(repository_type, publisher_unpaywall, journal_name_unpaywall) %>%
  summarise(n = n(), fuji = mean(fuji_percent)) %>%
  ungroup() %>%
  mutate(publisher_unpaywall = fct_reorder(publisher_unpaywall, n, .fun = sum))

test %>%
  plot_ly() %>%
  add_bars(y = ~publisher_unpaywall, x = ~n, color = ~journal_name_unpaywall) %>%
  layout(barmode = "stack") %>% layout(showlegend = FALSE)


test <- data %>%
  group_by(fields_of_research, repository_re3data) %>%
  summarise(n = n()) %>%
  ungroup()

test %>%
  plot_ly() %>%
  add_bars(y = ~fields_of_research, x = ~n, color = ~repository_re3data) %>%
  layout(barmode = "stack") %>% layout(showlegend = FALSE)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Plotly Treemap ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

test <- data %>%
  count(repository_re3data, repository_type)

test2 <- data.frame(repository_re3data = unique(test$repository_type),
                    repository_type = NA,
                    n = c(221, 82))

test <- rbind(test2, test)

test <- test %>%
  mutate(color = "#879C9D") %>%
  mutate(x = round(runif(40, 0.1:0.5), 1))


test %>% plot_ly(
  labels = ~repository_re3data,
  parents = ~repository_type,
  values = ~n,
  type ="treemap",
  branchvalues = 'total',
  text = ~paste0(x, " % FAIR Score"),
  textinfo = "label+value+text",
  textfont = list(color = "white"),
  marker = list(colors = pal))

# https://plotly.com/r/reference/treemap/


fig <- plot_ly(
  labels = test$repository_re3data,
  parents = test$repository_type,
  values = test$n,
  type ="treemap",
  branchvalues = 'total',
  textinfo = "label+value",
  marker = list(colors = pal))

#   color = test$x
#   marker = list(colorscale = 'Reds')
#   marker = list(colors = pal, colorscale = test$x)

fig %>% layout(treemapcolorway= pal)

fig %>% layout(title = "Treemap Repositories")

fig %>% layout(font = list(color = "#ffffff"))

marg <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 150,
  pad = 4
)

fig %>%
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
        text = "of open data by Charité authors in 2020 was published in field-specific repositories",
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

partial_bundle(fig) %>% saveWidget( "output/charts/p1.html", selfcontained = TRUE)
saveWidget(fig, "output/charts/p1.html", selfcontained = FALSE, libdir = "lib")
library(htmlwidgets)
fig


chart_bar_count <- data %>%
  filter(!is.na(repository_type)) %>%
  group_by(repository) %>%
  mutate(repository_big = case_when(n() >= 10 ~ repository,
                                      TRUE ~ "other repository")) %>%
  ungroup() %>%
  group_by(repository_type, repository_big) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = repository_type, y = count, group = repository_big )) + #reorder(repository_big, count)
  geom_col(colour="black", fill = "lightgrey")+ # position = position_stack(reverse = TRUE)
#  geom_text(aes(label = repository_big), colour = "black", size = 2.5, position = position_stack(0.5)) + # , stat = "count"
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Count", subtitle = "(other repository is n < 10)")

chart_bar_count


chart_bar_count <- data %>%
  filter(!is.na(repository_type)) %>%
  group_by(repository) %>%
  mutate(repository_big = case_when(n() >= 10 ~ repository,
                                    n() <= 9 & repository_type == "field-specific repository" ~ "other field-specific repository",
                                    n() <= 9 & repository_type == "general-purpose repository" ~ "other general-purpose repository",
                                    TRUE ~ "other repository")) %>%
  ungroup() %>%
  ggplot(aes(x = repository_type, group = fct_rev(fct_infreq(repository_big)))) +
  geom_bar(colour="black", fill = "lightgrey") + 
  geom_text(aes(label = repository_big), stat = "count", colour = "black", size = 2.5, position = position_stack(0.5)) + # , stat = "count"
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Count", subtitle = "(other repository is n < 10)")

chart_bar_count

library(treemapify)
library(ggplot2)
data_3 <- data %>%
  group_by(repository) %>%
  mutate(repository_big = case_when(n() >= 10 ~ repository,
                                    TRUE ~ "other repository")) %>%
  ungroup() %>%
  filter(!is.na(repository_type)) %>%
  group_by(repository_big, repository_type) %>%
  summarise(sum = n())

chart_treemap_repository <- ggplot(data_3, aes(area = sum, fill = repository_type, label = repository_big, subgroup = repository_type)) +
  geom_treemap() +
  geom_treemap_subgroup_text(place = "bottom", grow = F, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "centre",
                    grow = FALSE, min.size = 0) +
  theme(legend.position = "none") +
  labs(title = "Treemap of Repositories", subtitle = "(other repository is n < 10)")

chart_treemap_repository 

data %>%
  group_by(repository) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(x = repository, y = fuji_percent, color = repository_type)) +
  geom_boxplot() +
  coord_flip()

data %>%
  group_by(repository) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(x = repository, y = fuji_percent, fill = repository_type)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_jitter(height = 0.4, width = 0.4, size = 0.5) +
  coord_flip()

data %>%
  filter(repository_type == "general-purpose repository") %>%
  ggplot(aes(x = repository, y = fuji_percent)) +
  geom_violin(trim = TRUE, scale = "width") +
  geom_jitter(height = 0.4, width = 0.3, size = 0.5) +
  coord_flip()

data %>%
  ggplot(aes(x = repository_type)) +
  geom_bar() +
  coord_flip()

data %>%
  group_by(repository) %>%
  filter(n() >= 10) %>%
  ggplot(aes(x = repository, fill = repository_type)) +
  geom_bar() +
  coord_flip()


data_2 <- data %>%
  select(best_identifier, repository_type, guid_scheme_fuji, fuji_percent_f, fuji_percent_a, fuji_percent_i, fuji_percent_r) %>%
  pivot_longer(cols = starts_with("fuji_percent")) %>%
  mutate(name = case_when(name == "fuji_percent_f" ~ "F",
                          name == "fuji_percent_a" ~ "A",
                          name == "fuji_percent_i" ~ "I",
                          name == "fuji_percent_r" ~ "R"
                          )) %>%
  mutate(name = factor(name, levels = c("F", "A", "I", "R"))) 

chart_violin_repository <- data_2 %>%
  #filter(repository_type == "general-purpose repository") %>%
  #filter(repository_type == "field-specific repository") %>%
  filter(!is.na(repository_type)) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
 # geom_boxplot() +
  geom_violin(trim = TRUE, scale = "width") +
  
  geom_jitter(aes(color = guid_scheme_fuji), height = 1, width = 0.3, size = 0.5, shape = 1) +
  stat_summary(fun = mean, geom = "point", size = 2,color = "red") +
  facet_wrap(~ repository_type) +
  theme_minimal() +
  labs(title = "FAIRness according to FUJI assessment") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = ""))

chart_violin_repository

ggplotly(chart_violin_repository)

data_2 %>% 
  group_by(name, repository_type) %>%
  summarize(mean = mean(value)) %>%
  filter(!is.na(repository_type)) %>%
  ggplot(aes(x = name, y = mean, fill = name)) +
  geom_col() +
  facet_wrap(~ repository_type) +
  labs(title = "FAIRness according to FUJI")


data %>%
  ggplot(aes(x = guid_scheme, fill = repository_type)) +
  geom_bar()

data_2 %>%
  #filter(repository_type == "general-purpose repository") %>%
  #filter(repository_type == "field-specific repository") %>%
  #filter(guid_scheme != "handle") %>%
  filter(!is.na(repository_type)) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  # geom_boxplot() +
  geom_violin(trim = TRUE, scale = "width") +
  
  geom_jitter(height = 1, width = 0.3, size = 0.5, shape = 1) +
  stat_summary(fun = mean, geom = "point", size = 2,color = "red") +
  facet_grid(repository_type ~ guid_scheme) +
 # facet_grid(guid_scheme ~ repository_type ) + # , margins=TRUE 
 # facet_wrap(~ repository_type + guid_scheme) +
  theme_minimal() +
  labs(title = "FAIRness according to FUJI assessment") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = ""))

data_2 %>%
  mutate(ncbi = case_when(str_detect(best_identifier, "ncbi") ~ "ncbi repository",
                          TRUE ~ "other repository")) %>%
  #filter(repository_type == "general-purpose repository") %>%
  #filter(repository_type == "field-specific repository") %>%
  filter(!is.na(repository_type)) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  # geom_boxplot() +
  geom_violin(trim = TRUE, scale = "width") +
  
  geom_jitter(aes(color = repository_type), height = 1, width = 0.3, size = 0.5, shape = 1) +
  stat_summary(fun = mean, geom = "point", size = 2,color = "red") +
  # facet_wrap(~ ncbi) +
  facet_grid(repository_type ~ ncbi) +
  theme_minimal() +
  labs(title = "FAIRness according to FUJI assessment") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = ""))

fig <- data_2 %>%
  filter(repository_type == "general-purpose repository") %>%
  plot_ly(
    x = ~name,
    y = ~value,
    split = ~name,
 #   color = ~repository_type,
    type = 'box',
   # boxpoints = "all", 
    #jitter = 1,
    #pointpos = 0,
   # bandwidth = 0,
    box = list(
      visible = FALSE
    ),
    meanline = list(
      visible = T
    )
  ) 

fig

fig <- data_2 %>%
  plot_ly(type = 'violin') 
fig <- fig %>%
  add_trace(
    x = ~name[data_2$repository_type == 'field-specific repository'],
    y = ~value[data_2$repository_type == 'field-specific repository'],
    legendgroup = 'Yes',
    scalegroup = 'Yes',
    name = 'field',
    side = 'negative',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("blue")
  ) 
fig <- fig %>%
  add_trace(
    x = ~name[data_2$repository_type == 'general-purpose repository'],
    y = ~value[data_2$repository_type == 'general-purpose repository'],
    legendgroup = 'No',
    scalegroup = 'No',
    name = 'general',
    side = 'positive',
    box = list(
      visible = T
    ),
    meanline = list(
      visible = T
    ),
    color = I("green")
  ) 

fig <- fig %>%
  layout(
    xaxis = list(
      title = ""  
    ),
    yaxis = list(
      title = "",
      zeroline = F
    ),
    violingap = 0,
    violingroupgap = 0,
    violinmode = 'overlay'
  )

fig

data_2 <- data_2 %>%
  mutate(value = value/100)

plot_1 <- data_2 %>%
  filter(repository_type == "field-specific repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "field-specific rep.", boxmean = TRUE)

plot_2 <- data_2 %>%
  filter(repository_type == "general-purpose repository") %>%
  plot_ly(x = ~name, y = ~value) %>%
  add_boxplot(name = "general-purpose rep.", boxmean = TRUE) %>%
  layout(title = "FAIRness")

box_1 <- subplot(plot_1, plot_2, shareY = TRUE) %>% hide_legend() %>%
  layout(yaxis = list(tickformat = ",.0%", title = FALSE),
         annotations = list(
    list(x = 0.1 , y = 1, text = "field-specific repository", showarrow = F, xref='paper', yref='paper'),
    list(x = 0.9 , y = 1, text = "general-purpose repository", showarrow = F, xref='paper', yref='paper')))

saveRDS(box_1, "output/charts/box_1.rds")

save(box_1, file = "output/charts/box_1.Rdata")

data %>%
  mutate(publisher_unpaywall = as.factor(publisher_unpaywall)) %>%
  group_by(publisher_unpaywall) %>%
  filter(n() >= 5) %>%
ggplot(aes(publisher_unpaywall, fill = journal_name_unpaywall)) +
  geom_bar() +
  coord_flip()

data %>%
  group_by(journal_name_unpaywall) %>%
  filter(n() >= 5) %>%
  ggplot(aes(journal_name_unpaywall, fill = publisher_unpaywall)) +
  geom_bar() +
  coord_flip()

