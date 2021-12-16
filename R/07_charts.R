#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Visualize data ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(plotly)
library(tidyverse)


load("output-Rdata/charite_rd_2020_final.Rdata")

data <- charite_rd_2020_final

data %>%
  filter(repository_type == "general-purpose repository") %>%
  ggplot(aes(license_fuji)) +
  geom_bar()

data %>%
  filter(repository_type == "field-specific repository") %>%
  ggplot(aes(license_fuji)) +
  geom_bar()

chart_bar_license <- data %>%
 # filter(!is.na(repository_type)) %>%
  ggplot(aes(license_fuji, fill = repository_type)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
#  facet_wrap( ~ repository_type) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
      #  legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Licenses according to FUJI assessment",
       fill = "Repository") +
  scale_y_continuous(labels = scales::percent)

chart_bar_license

data %>%
  filter(!is.na(repository_type)) %>%
  ggplot(aes(y = fuji_percent, x = repository_type)) +
  geom_boxplot()

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
  plot_ly(
    x = ~name,
    y = ~value,
    split = ~name,
    type = 'violin',
    bandwidth = 0,
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
