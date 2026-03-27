library(prophet)
library(lubridate)
library(tidyverse)
library(dplyr)



### Optional to run parallel for faster processing 

cores <- detectCores()
cl <- makeCluster(cores - 5)
registerDoParallel(cl)


stopCluster(cl)

## Stage 2 running - Prophet 


data <- read.csv("Flux_data_long_gaps.csv")

## It is very important to define the data and time at this stage 
data$DateTime <- ymd_hm (data$DateTime)


ds <- data$DateTime
y <- data$X2._3D_CO2_smooth

df <- data.frame(ds, y)


m <- prophet(  yearly.seasonality = TRUE, weekly.seasonality = TRUE ,daily.seasonality = TRUE,
               growth = "linear", changepoint.range = 0.9, changepoint.prior.scale = 10,
               seasonality.prior.scale = 100, seasonality.mode = "additive",
               uncertainty.samples = 10000,  
               holidays = NULL, fit = TRUE, mcmc.samples = 1200)


m <- fit.prophet(m, df)

future <- make_future_dataframe(m, periods = 25)

tail (future)

forecast <- predict (m, future)


write.csv(forecast, "Pro-CO2_filled.csv")
