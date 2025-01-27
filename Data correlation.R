# No Outliers Data 
install.packages("tidyverse")
install.packages("plotly")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("modeldata")


library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(modeldata)
library(readr)
library(readxl)

data <- read_excel("Phenoloxidase Activity data/Data_for_ R.xlsx")
view(data)
str(data)
summary(data)
glimpse(data)

ggplot(data, aes(x = Site,
                  y = `micromol*g soil*hour`,
                  fill = Site)) + geom_boxplot()
plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~Site,
  type = "box",
  color = ~Site,
  showlegend = FALSE
)

plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~Average_pH,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~Organic_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~Total_C,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)



plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~Total_N,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~LUI_index_2022,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~`LUI_index_(2018-2022)`,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)

plot_ly(
  data = data,
  y = ~`micromol*g soil*hour`,
  x = ~`Soil Type`,
  type = "scatter",
  color = ~Site,
  showlegend = TRUE
)


