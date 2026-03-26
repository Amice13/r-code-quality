library(data.table)
library(reshape)
#######################################################################
##### Import Data from classification task:  ######
#######################################################################
load("classified_data-xgboost.RData") # classified data from supervised learning

## we're using the xgboost algorithm to label killings either targeted or untargeted
data$code <- data$xgboost.code
table(data$code, useNA="always")

## monthly and bi-weekly breaks, but the bi-weekly breaks have to start with the first of each month:
data <- data.table(data)

## data[, month := cut(as.Date(date_of_death), breaks="month")]
data[, date_of_death := as.character(date_of_death)]

load("survey-date2w.Rdata")
data <- merge(data, survey.times, by.x="date_of_death", by.y="day")

## make sure there are no missing dates
stopifnot(nrow(data[is.na(data$date2w),])==0)

#######################################################################
##### Save output:  ######
#######################################################################
save(data, file="classified_data.RData")












