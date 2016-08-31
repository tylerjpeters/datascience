#######################
# Tyler J Peters
# 2016-08-15
# DS 700 Final Project
# Data Cleaning Script
#######################

## import and prepare the data for cleansing

# Check for missing packages; install if needed.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, ggplot2, dplyr)

# define the location of the data file(s)
data_path <- "dataset.xlsx"

# Import each of the sheets from the workbook
import_sheets <- function(filename) {
  sheets <- excel_sheets(filename)
  x <- lapply(sheets, function(X) read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

dataset <- import_sheets(data_path)

# remove the first sheet from the dataset
dataset$`Babson Catalogue Information` <- NULL

# convert each of the lists to a data frame
dataset <- lapply(dataset, data.frame)

## begin cleaning the data

for(i in length(dataset$`Abbeville, LA`$Incoming.Examinations)) {
  if(as.numeric(dataset$`Abbeville, LA`$Incoming.Examinations[i] & !is.na(dataset$`Abbeville, LA`$Incoming.Examinations)),
     dataset$`Abbeville, LA`$Incoming.Examinations[i],
     "NA")
}
