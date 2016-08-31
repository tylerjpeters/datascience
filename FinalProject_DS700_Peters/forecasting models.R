#####################################################
#
#   Tyler J Peters
#   DS 700 Final Project R Script
#   Aug. 19, 2016
#
#####################################################


# import all needed libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(zoo)
library(mice) # load the mice() package
library(VIM)
library(forecast)
library(lattice)

#####################################################

## Data visualizations prior to cleaning (note that non-numeric values were removed prior to import)
  # import the dirty data into the environment for initial analysis
    dirty_data <- read_excel("Dataset_original.xlsx", sheet = "preclean")
  # remove rows with no data
    dirty_data <- dirty_data %>%
      filter(!is.na(dirty_data$`Incoming Examinations`))
  # remove any outliers
    dirty_data <- dirty_data %>%
      filter(dirty_data$`Incoming Examinations` < 6774)
  # create a boxplot
    ggplot(dirty_data, aes(x = factor(0), y = dirty_data$`Incoming Examinations`)) +
      geom_boxplot(fill="#3399FF", colour="black") +
      ggtitle("Incoming examinations for Abbeville HC") +
      xlab("") +
      ylab("Incoming Examinations") +
      scale_x_discrete(breaks = NULL) +
      coord_flip() 

  # add a year-month column to the dataframe
    dirty_data <- dirty_data %>%
      mutate(date = as.Date(paste0(as.character(dirty_data$Month),"/01/",as.character(dirty_data$Year)), format = "%m/%d/%Y"))
  
  # sort the data by year then month
    dirty_data <- dirty_data %>%
      arrange(Year, Month)
  
  # plot the data as a time series 
    ggplot(dirty_data, aes(date, `Incoming Examinations`)) + 
      geom_line() +
      scale_x_date(date_labels = "%Y") +
      ggtitle("Time versus appointments for Abbeville HC") +
      xlab("Year") + 
      ylab("Monthly Appointments")

#####################################################    
    
## Impute the data and visualize again
  # import the actual clean data from its respective spreadsheet
    cleandata <- read_excel("Dataset_cleaned.xlsx", sheet = "Abbeville, LA")
  
  # impute the missing values
    data.impute <- mice(cleandata, m = 20, seed = 150, method = "norm", printFlag = FALSE)
  
  # insert the imputed values into the dataset
    finalData <- complete(data.impute, 7)
  
  # add a year-month column to the dataframe
    finalData <- finalData %>%
      mutate(date = as.Date(paste0(as.character(finalData$Month),"/01/",as.character(finalData$Year)), format = "%m/%d/%Y"))
  
  # generate the new time series plot of the cleaned data
    ggplot(finalData, aes(date, `Incoming Examinations`)) + 
      geom_line() +
      scale_x_date(date_labels = "%Y") +
      ggtitle("Time versus appointments for Abbeville HC") +
      xlab("Year") + 
      ylab("Monthly Appointments")

#####################################################

## Forcasting Models
  # define a ts object for use in the models
  ts.data <- ts(finalData$`Incoming Examinations`, start = c(2006,1), end = c(2013,12), frequency = 12)
  
  ## ARIMA model
    # employ the auto.arima function to find the arima model with the lowest AIC
      arima.model <- auto.arima(ts.data)
    
    # plot the ARIMA model with 12 month forecast
      plot(forecast(arima.model, level = 0.95, h = 12))
    
    # plot the residuals for the ARIMA method
      opar <- par() # capture current graphics settings
      par(mfrow=c(1,2))
      hist(residuals(arima.model), xlab = "Residuals", main = "ARIMA Resids")
      plot(residuals(arima.model), xlab = "Time", ylab = "")
      par(opar) # reset graphics settings
  
  ## Holt Winters model
    # employ the HoltWinters() function
      holt.model <- HoltWinters(ts.data)
    
    # plot the Holt Winters model with 12 month forecast
      plot(forecast.HoltWinters(holt.model, level = 0.95, h = 12)) 
    
    # plot the residuals for the Holt Winters method
      opar <- par() # capture current graphics settings
      par(mfrow=c(1,2))
      hist(residuals(holt.model), xlab = "Residuals", main = "Holt-Winters Resids")
      plot(residuals(holt.model), xlab = "Time", ylab = "")
      par(opar) # reset graphics settings

#####################################################
      
## Model Comparison
  # Error Measures for ARIMA Model
    accuracy(arima.model)

  # Error Measures for the Holt-Winters Model
    accuracy(forecast.HoltWinters(holt.model))



