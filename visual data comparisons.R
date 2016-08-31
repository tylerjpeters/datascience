# Tyler Peters
# DS 700 Final Project

library(dplyr)
library(ggplot2)
library(readxl)

## Data visualizations prior to cleaning (note that non-numeric values were removed prior to import)
  # import the dirty data into the environment for initial analysis
  dirty_data <- read_excel("Dataset_original.xlsx", sheet = "preclean")

  # remove rows with no data
  dirty_data <- dirty_data %>%
    filter(!is.na(dirty_data$`Incoming Examinations`))
  
  dirty_data.2 <- dirty_data %>%
    filter(dirty_data$`Incoming Examinations` < 6774)
  
  # create a boxplot
  ggplot(dirty_data, aes(x = factor(0), y = dirty_data$`Incoming Examinations`)) +
    geom_boxplot()
  
  # create a histogram
  ggplot(dirty_data, aes(dirty_data$`Incoming Examinations`)) +
    geom_histogram()
  
  summary(dirty_data)
  
  
3184 + 1.5*(3184-791)

## Data visualizations once outliers were removed, and data was imputed