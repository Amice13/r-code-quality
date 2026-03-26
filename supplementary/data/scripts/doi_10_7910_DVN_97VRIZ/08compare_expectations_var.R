# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://data.humdata.org/event/opt-israel-hostilities')
#browseURL('https://data.humdata.org/group/pse')

rm(list = ls())

library(forecast)
library(tidyverse)
library(tseries)
library(vars)

wd_main <- "..."

setwd(wd_main)
currencyi = 'NIS'
method = 'price'

price_data <- readRDS('price_data.RDS')

# Related-Commodity Groups
commodity_groups <- list(
  bread         = c("bread", "wheat_flour", "semolina"),
  #chicken_meat  = c("chicken_meat","olive_oil"),
  #milk          = c("milk"),
  #olive_oil     = c("olive_oil",   "chicken_meat"),
  rice_long     = c("rice_long",   "rice_small", "semolina"),
  rice_small    = c("rice_small",  "rice_long", "semolina"),
  semolina      = c("semolina",    "wheat_flour","rice_long",   "rice_small"),
  sugar         = c("sugar",       "rice_long", "rice_small", "semolina"),
  wheat_flour   = c("wheat_flour", "semolina","rice_long",   "rice_small")
) 

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
  dplyr::select(date,commodity,price,origin) %>%
  arrange(date)

# Store commodity names
commodity_names <- names(commodity_groups)

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
ts_data <- gaza_data %>%
  dplyr::select(date, commodity, price)

ts_nowar  <- gaza_data %>%
  filter(date < war_date) %>%
  dplyr::select(date, commodity, price)

ts_war  <- gaza_data %>%
  filter(date >= war_date) %>%
  dplyr::select(date, commodity, price)

war_length <- length(unique(ts_war$date))

#--------------------- 1. Check for Trend and Seasonality ---------------------------

seasonal_threshold <- 0.5  # Define threshold for strong seasonality
stationarity_results <- list()
seasonality_results <- list()
model_types <- list()  # Store model selection

for (com in commodity_names) {
  print(paste('calculating',com))

  # Augmented Dickey-Fuller Test for Stationarity
  adf_p <- adf.test(ts_nowar[ts_nowar$commodity==com,]$price)$p.value
  
  # Check for seasonality using ACF
  acf_values <- Acf(ts_nowar[ts_nowar$commodity==com,]$price, lag.max = war_length, plot = FALSE)$acf[-1]
  max_acf <- max(acf_values)  # Maximum autocorrelation
  
  # Determine model type
  if (adf_p < 0.05 && max_acf < seasonal_threshold) {
    model_types[[com]] <- "const"
  } else if (adf_p < 0.05 && max_acf >= seasonal_threshold) {
    model_types[[com]] <- "const + seasonal dummies"
  } else if (adf_p >= 0.05 && max_acf < seasonal_threshold) {
    model_types[[com]] <- "both"
  } else {
    model_types[[com]] <- "seasonal differencing"
  }
  
  # Store results
  stationarity_results[[com]] <- adf_p
  seasonality_results[[com]] <- max_acf
}

# Convert to data frame for better readability
stationarity_df <- data.frame(
  commodity = names(stationarity_results),
  adf_p_value = unlist(stationarity_results),
  max_acf = unlist(seasonality_results),
  model_type = unlist(model_types)
)

print(stationarity_df)

# Interpretation:
# ADF p-value < 0.05 (Stationary) & max_acf < 0.5 (No strong seasonality) → const (regular VAR)
# ADF p-value < 0.05 (Stationary) & max_acf > 0.5 (Strong seasonality) → const + seasonal dummies
# ADF p-value > 0.05 & max_acf < 0.5 → Trend present, use both.
# ADF p > 0.05 & max_acf > 0.5 → Apply seasonal difference


#--------------------- 2. Define the VAR Model --------------------------- 

# fit a VAR(p) model on wide_before, 
# p is selected using Akaike Information Criterion (AIC).

fit_and_forecast_VAR <- function(ts_series, horizon, var_type, p_optimal) {
  
  if (nrow(ts_series) < 24) {
    return(list(
      message = "Not enough historical data for reliable forecasting"
    ))
  }
  
  var_model <- NULL
  seasonal_dummies <- NULL
  
  if (var_type == "const") {
    
    var_model <- VAR(ts_series[,-1], p = p_optimal, type = "const")
    
  } else if (var_type == "const + seasonal dummies") {
    
    if (!"date" %in% colnames(ts_series)) {
      stop("ts_series must include a 'date' column.")
    }
    
    ts_series$month <- factor(month(ts_series$date))
    seasonal_dummies <- model.matrix(~ month - 1, data = ts_series)
    ts_series <- ts_series %>% select(-month, -date)
    
    var_model <- VAR(ts_series, p = p_optimal, type = "const", exogen = seasonal_dummies)
    
  } else if (var_type == "both") {
    
    var_model <- VAR(ts_series[,-1], p = p_optimal, type = "both")
    
  } else if (var_type == "seasonal differencing") {
    
    ts_series <- ts_series[,-1]
    ts_series <- diff(ts_series, lag = 12)
    ts_series <- na.omit(ts_series)
    
    var_model <- VAR(ts_series, p = p_optimal, type = "const")
    
  } else {
    stop("Invalid var_type.")
  }
  
  # Forecast
  var_pred <- predict(var_model, n.ahead = horizon, ci = 0.95)
  
  var_forecast <- sapply(var_pred$fcst, function(x) x[, "fcst"])
  var_lower    <- sapply(var_pred$fcst, function(x) x[, "lower"])
  var_upper    <- sapply(var_pred$fcst, function(x) x[, "upper"])
  
  return(list(
    model = var_model,
    var_forecast = var_forecast,
    var_lower = var_lower,
    var_upper = var_upper
  ))
}

#--------------------- 3. Train the VAR Model --------------------------- 

forecast_results <- list()

# forecast war prices for 1 to war_length months ahead.
# iterate with rolling cv: add 1 month in each iteration.
# last iteration to forecast last date of data.

for (com in commodity_names){
  print(paste('calculating',com))
  
  # Get the group of commodities relevant for 'com'
  com_group <- commodity_groups[[com]]
  
  forecast_output <- list()
  ts_com <- ts_data %>%
    # only relevant commodities
    filter(commodity %in% com_group) %>%
    pivot_wider(
      names_from = commodity,
      values_from = price
    ) %>%
    arrange(date) %>%
    # com becomes the 2nd column (after date)
    dplyr::select(date, all_of(com_group))
  
  ts_com <- bind_rows(
    ts_com,
    tibble(date = seq(max(ts_com$date) %m+% months(1), 
                      by = "month", length.out = 11)) %>%
      mutate(across(
        .cols = -date,
        .fns = ~ 0
      ))
  ) %>%
    # replace NA with 0
    mutate(across(everything(), ~ replace_na(.x, 0)))
  
  # Extract time series for the commodity
  start_train <- nrow(ts_data %>%
                        # allow war_length months forecast
                        filter(commodity == com, 
                               date <= '2022-5-01'))
                               
  end_train <- nrow(ts_data %>%
                      # allow war_length months forecast
                      filter(commodity == com))-1
  
  # VAR specifities
  # Determine correct type
  var_type <- ifelse(stationarity_df[stationarity_df$commodity==com,]$model_type < 0.05, "const", "both")
  
  # Optimal lag selection
  lag_selection <- VARselect(ts_com[,-1], lag.max = 12, type = var_type)
  
  p_optimal <- lag_selection$selection["AIC(n)"]
  
  # 1st forecast = Oct.2023
  for (i in start_train:end_train) {
    test_i <-  ts_com[(i+1):(i+war_length),1:2]
    colnames(test_i) <- c('date','obs.')
    
    # Apply forecasting for multiple horizons
    forecast_i <- fit_and_forecast_VAR(
      ts_series = ts_com[1:i,-1],
      horizon = war_length,
      var_type = var_type,
      p_optimal = p_optimal
    )
    
    forecast_i <- cbind(forecast_i$var_forecast[,1],
                        cbind(forecast_i$var_lower[,1],
                              forecast_i$var_upper[,1]))
    
    colnames(forecast_i) = c("forecast", "lower_ci", "upper_ci")
    
    forecast_i <- forecast_i %>%
      as.data.frame() %>%
      mutate(forecast_date = as.Date(test_i$date,'%Y-%m-%d'),
             horizon = 1:war_length,
             obs. = test_i$obs.,
             model = 'var',
        .before = 1)
    
    forecast_output[[(i-start_train)+1]] <- forecast_i
  }
  forecast_output <- do.call(rbind,forecast_output)
  
  # remove forecasts with no observation to compare with
  forecast_output <- forecast_output %>%
    filter(forecast_date >= war_date & forecast_date <= ts_com$date[end_train+1]) 
  
  # Reshape the forecast_output into long format
  forecast_output <- forecast_output %>%
    mutate(commodity = com,
           .before = 1)
  
  # Store results
  forecast_results[[com]] <- forecast_output
}

forecast_results <- do.call(rbind, forecast_results)

#--------------------- 4. Compare Forecasts with Actual "during-War" Prices --------------------------- 

# Root Mean Squared Errors (RMSE)
rmse_fun <- function(obs, pred){ 
  sqrt(mean(pred-obs)^2)
}

# Mean Absolute Percentage Errors (MAPE)
mape_fun <- function(obs, pred){ 
  mean(abs((obs-pred) / obs))
}

# var_forecast → Predicted prices for war period.
# actual_values → Observed prices during war.
# mape → Mean Absolute Percentage Error (MAPE).
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
# Understanding MAPE (Mean Absolute Percentage Error)
# MAPE measures the accuracy of the forecasts by calculating:
#  average percentage difference between the actual and predicted values.
# MAPE < 10% → Highly accurate forecasts (small errors).
# 10% ≤ MAPE ≤ 20% → Moderate accuracy (acceptable error range).
# MAPE > 20% → Low accuracy, large forecasting errors.
# If VAR predictions stay within confidence intervals but MAPE is high:
#  external shocks (e.g., war) likely caused unexpected volatility.

# Summary of Approach
# Step	Method Used
# 1. Data Preparation	Reshape to wide format (spread())
# 2. VAR Model Selection	VARselect() to find optimal lag (p)
# 3. Fit VAR Model	VAR() using p_optimal
# 4. Forecast Next 14 Months	predict(VAR_model, n.ahead=14, ci=0.95)
# 5. Compare Forecast with Actual Data	Compute MAPE for accuracy

#--------------------- 6. Save data --------------------------- 

setwd(paste0(wd_main,'/graphs'))

save(forecast_results,
     file = paste0('forecast_results_var_',method,currencyi,'.RData'))
