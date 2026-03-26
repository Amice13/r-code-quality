# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://data.humdata.org/event/opt-israel-hostilities')
#browseURL('https://data.humdata.org/group/pse')

rm(list = ls())

library(forecast)
library(tidyverse)

wd_main <- "..."
currencyi = 'NIS'
method = 'price'

setwd(wd_main)

price_data <- readRDS('price_data.RDS')

#--------------------- 0. Define Data --------------------------- 

war_date <- as.Date('2023-10-01')

seed = 4

l = 1 # lags to choose

if(method == 'perc'){
  price_data <- price_data %>%
    dplyr::select(-price) %>%
    rename(price = perc_change) %>%
    drop_na(price)
}

gaza_data <- price_data %>%
  # remove duplicates
  filter(lag == l & currency == currencyi) %>%
  filter(!commodity %in% c('energy','water')) %>%
  mutate(
    origin = as.factor(str_to_title(origin))) %>%
  # remove empty levels
  mutate(across(where(is.factor), droplevels)) %>%
  filter(area == 'gaza') %>%
  dplyr::select(-area, -lag, -currency) %>%
  arrange(date)

# Store commodity names
commodity_names <- unique(gaza_data$commodity)

# Function to convert to time series for each period
convert_to_ts <- function(data) {
  data %>%
    arrange(date) %>%
    group_by(commodity) %>%
    summarise(price_ts = list(ts(price, 
                                 start = c(year(min(date)), month(min(date))), 
                                 frequency = 12))) %>%
    pull(price_ts)# Extract list of time series objects
}

# Convert data into a list of time series objects
ts_data <- convert_to_ts(gaza_data)
ts_war  <- gaza_data %>%
  filter(date >= war_date) %>%
  convert_to_ts()

# Assign names to lists
names(ts_data) <- commodity_names
names(ts_war) <- commodity_names

war_length <- length(ts_war[[1]])

#--------------------- 1. Define Forecasting Function for Multiple Horizons --------------------------- 

# auto.arima() selects the best ARIMA model automatically.
# tbats() is useful for handling complex seasonality.
# Forecast horizons (h = 1:t) match the "after-war" period length.
# Define forecast as actual price + error: Y = p+ϵ

# Function to forecast using ARIMA and TBATS for multiple horizons
forecast_models_multi_horizon <- function(ts_series, max_h = war_length) {
  
  # Ensure the time series has enough data points for forecasting
  if (length(ts_series) < 24) {  # At least 2 years of data needed
    return(list(
      message = "Not enough historical data for reliable forecasting"
    ))
  }
  
  # fit univariate TS models (H = 12):
  # Each model carries out a H-step recursive forecast, each consisting of H-1 inner iterations. 
  # every inner-iteration is a one-step ahead prediction, Yi = p+ϵh
  # for every h<H, the predicted price joins the input data for the following forecast, 
  # Y = p+ϵ1+ϵ2+ϵ3+ϵ4+...ϵH
  # along with the new ϵ to create a new input set of observations and h-1 predicted values. 
  # As the forecasting errors accumulate, performance decreases with any additional iteration. 
  
  # Fit ARIMA model
  arima_model <- auto.arima(ts_series)
  arima_forecast <- forecast(arima_model, h = max_h)
  
  # Fit TBATS model
  tbats_model <- tbats(ts_series)
  tbats_forecast <- forecast(tbats_model, h = max_h)
  
  return(list(
    arima_forecast = as.numeric(arima_forecast$mean), 
    arima_lower = as.numeric(arima_forecast$lower[,2]),  # Extract lower bound (95% CI)
    arima_upper = as.numeric(arima_forecast$upper[,2]),  # Extract upper bound (95% CI)
    
    tbats_forecast = as.numeric(tbats_forecast$mean),
    tbats_lower = as.numeric(tbats_forecast$lower[,2]),  # Extract lower bound (95% CI)
    tbats_upper = as.numeric(tbats_forecast$upper[,2])   # Extract upper bound (95% CI)
  ))
}

#--------------------- 2. Generate Forecasts for Each Commodity --------------------------- 

forecast_results <- list()

add_below <- matrix(numeric(3*99), ncol = 3)
colnames(add_below) = c('year', 'month' ,'price')

# forecast war prices for 1 to 17 months ahead.
# iterate with rolling cv: add 1 month in each iteration.
# last iteration to forecast last date of data.

for (com in commodity_names)  {
  print(paste('calculating',com))
  
  forecast_output <- list()
  test_com <- gaza_data %>%
    filter(commodity == com) %>%
    dplyr::select(date,price) %>%
    mutate(year = year(date), month = month(date), 
           .keep = 'unused', .after = 1) %>%
    # add 11 unobserved months
    rbind(add_below) %>%
    as.matrix()
  
  # Extract time series for the commodity
  ts_series <- ts_data[[com]]
  start_train <- length(window(ts_series, 
                               # allow war_length months forecast
                               end = c(2022, 5)))
  end_train <- length(ts_series)-1
  
  # 1st forecast = Oct.2023
  for (i in start_train:end_train) {
    test_i <-  test_com[(i+1):(i+war_length),]
    # Apply forecasting for multiple horizons
    forecast_i <- forecast_models_multi_horizon(ts_series[1:i], 
                                                     max_h = war_length)
    
    forecast_i <- do.call(cbind, forecast_i)
    forecast_i <- forecast_i %>%
      as.data.frame() %>%
      mutate(forecast_date = as.Date(
        paste(test_i[,'year'],test_i[,'month'],1,sep = '-'),'%Y-%m-%d'),
             horizon = 1:war_length,
             obs. = test_i[,'price'],
             .before = 1)
    
    forecast_output[[(i-start_train)+1]] <- forecast_i
  }
  forecast_output <- do.call(rbind,forecast_output)
  
  # remove forecasts with no observation to compare with
  forecast_output <- forecast_output %>%
    drop_na(forecast_date)
  
  # Reshape the forecast_output into long format
  forecast_output <- forecast_output %>%
    pivot_longer(
      cols = ends_with(c("forecast", "lower", "upper")),
      names_to = c("model", ".value"),
      names_sep = "_"
    ) %>%
    rename(
      forecast = forecast,
      lower_ci = lower,
      upper_ci = upper
    ) %>%
    mutate(commodity = com, .before = 1)
  
  # Store results
  forecast_results[[com]] <- forecast_output
}

forecast_results <- do.call(rbind, forecast_results)

#--------------------- 4. Compare Forecasts with Actual "After-War" Prices --------------------------- 

# Root Mean Squared Errors (RMSE)
rmse_fun <- function(obs, pred){ 
  sqrt(mean(pred-obs)^2)
}

# Mean Absolute Percentage Errors (MAPE)
mape_fun <- function(obs, pred){ 
  mean(abs((obs-pred) / obs))
}

# tbats_forecast & arima_forecast → Predicted prices for war period.
# actual_values → Observed prices during war.
# tbats_mape & arima_mape → Mean Absolute Percentage Error (MAPE).
# Lower MAPE = better forecast accuracy
# If MAPE is high, it suggests war-time prices deviated significantly from pre-war patterns.

forecast_results <- forecast_results %>%
  mutate(# absolute percentage errors (APE)
    ape = abs((obs. - forecast)/obs.)) %>%
  group_by(commodity, horizon, model) %>%
  mutate(mape = mape_fun(obs., forecast),
            rmse = rmse_fun(obs., forecast)) %>%
  ungroup() %>%
  mutate(type = method,
         currency = currencyi)

#--------------------- 5. Interpretation --------------------------- 

# For each commodity and forecast horizon, we analyze:
# MAPE → If MAPE increases over time, forecasts become worse.
# RMSE → If RMSE increases, the actual price deviates significantly from forecasts.
# ARIMA vs. TBATS → If TBATS performs better, it indicates strong seasonal patterns in pre-war prices.

# Lower MAPE and RMSE in TBATS → Better at capturing seasonality.
# MAPE increases over time → Forecasts diverge further from actual values.
# RMSE trend → Increasing RMSE indicates that war introduced structural price shocks.
# 
# 7. Next Steps
# If MAPE & RMSE remain low → War had no major impact, and pre-war price trends were stable.
# If MAPE & RMSE increase sharply → War introduced significant market shocks.
# If TBATS outperforms ARIMA → Seasonality played a major role in pre-war pricing.

#--------------------- 6. Save data --------------------------- 

setwd(paste0(wd_main,'/graphs'))

save(forecast_results,
     file = paste0('forecast_results_',method,currencyi,'.RData'))
