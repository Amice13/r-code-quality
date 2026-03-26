library(tidyverse)
library(dplyr)
library(glmnet)
library(Metrics)
library(missForest)




########################################
#####RUN1 works very well 

data <- read.csv("CO2_2%.csv")


# Convert the date variable to a time-based numeric variable
data$Date <- as.Date(data$Date)
data$time_var <- as.numeric(data$Date - min(data$Date))

# Divide the time-based numeric variable into multiple predictors to capture seasonality
data$day_of_week <- as.factor(weekdays(data$Date))
data$month <- as.factor(format(data$Date, "%b"))
data$quarter <- as.factor(format(data$Date, "%q"))

# Identify the missing values
missing_mask <- is.na(data)


# Impute the missing values using missForest
set.seed(1234)
imputed_data <- missForest(data[, -1], maxiter = 400, mtry = 15, ntree = 1200)

# Extract the imputed data and exclude the predictors used for imputation
imputed_data <- imputed_data$ximp
imputed_data <- imputed_data %>% select(-time_var, -day_of_week, -month, -quarter)

# Join the imputed data with the original date variable
imputed_data <- cbind(data[,c("Date")], imputed_data)

write.csv(imputed_data, "CO2_MissForest-Principe-2%.csv")
