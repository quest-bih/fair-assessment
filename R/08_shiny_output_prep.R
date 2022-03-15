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

library(tidyverse)
library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

# basic example of ohlc charts
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

maxi_dif <-  as.integer(which (df$diff == max(df$diff)))
maxi_x <- df[maxi_dif,'Date']
maxi_y <- df[maxi_dif,'diff']

fig <- df %>% plot_ly(
  x = ~ Date,
  type = "candlestick",
  open = ~ AAPL.Open,
  close = ~ AAPL.Close,
  high = ~ AAPL.High,
  low = ~ AAPL.Low
) %>%
  add_trace(
    data = df %>% filter(Date == "2022-01-28"),
    x = ~ Date,
    type = "candlestick",
    open = ~ AAPL.Open,
    close = ~ AAPL.Close,
    high = ~ AAPL.High,
    low = ~ AAPL.Low,
    increasing = list(fillcolor = "Blue", line = list(color = "Blue")),
    decreasing = list(fillcolors = "Blue", line = list(color = "Blue")),
    showlegend = FALSE
  )
fig
reprex::reprex()
df %>% filter(Date == "2022-01-28")

reprex::reprex(mean(rnorm(10)))

