library(readxl)
library(mice)
library(forecast)

# import the cleaned data into the environment
cleandata <- read_excel("Dataset_cleaned.xlsx", sheet = "Abbeville, LA")

# visualize the data prior to imputation
plot.ts(cleandata$`Incoming Examinations`)

# impute the missing values
data.impute <- mice(cleandata, m = 20, seed = 150, method = "norm")
summary(data.impute)
data.impute$imp

#for (i in 1:20) {
finalData <- complete(data.impute, 7)
plot.ts(finalData$`Incoming Examinations`)
#}
# export the imputed dataset
write.csv(finalData, "finalData.csv")


ts.lm <- tslm(`Incoming Examinations` ~ date, data = cleandata)
