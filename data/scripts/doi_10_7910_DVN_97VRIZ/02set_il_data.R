#browseURL('https://www.cbs.gov.il/en/Statistics/Pages/Generators/Database-of-Prices-and-Price-Indices.aspx')
#browseURL('https://data.humdata.org/dataset/faostat-food-prices-for-israel')

# Local Food Prices in Israel - Products exported to Gaza

library(forecast)
library(httr)
library(imputeTS)
library(readxl)
library(stringdist)
library(tidyverse)

rm(list=ls())

wd_main <- "..."
wd_data <- paste0(wd_main, "/data/raw_data")
setwd(wd_data)
seed = 4

#--------------------- 1. define parameters to work with --------------------------- 

# map of items
food_map <- read_csv('ps_meta.csv')

food_map <- food_map %>%
  dplyr::select(il,commodity, origin) %>%
  drop_na() %>% rename(commodity_raw = il)

# USD~NIS ratio
usd_nis_url <- 'https://edge.boi.gov.il/FusionEdgeServer/sdmx/v2/data/dataflow/BOI.STATISTICS/EXR/1.0/RER_USD_ILS.D.USD.ILS.ILS.OF00?c%5BTIME_PERIOD%5D=ge:2007-01-01+le:2025-01-08&locale=en&format=csvfile'

GET(usd_nis_url, write_disk(tf <- tempfile(fileext = '.csv')))

usd_nis <- read_csv(tf)

usd_nis <- usd_nis %>%
  mutate(date = floor_date(TIME_PERIOD, unit = "month")) %>%
  group_by(date) %>%
  summarise(usd_nis = mean(OBS_VALUE))


#--------------------- 2. Arrange and IL Data (from WD) --------------------------- 

# Get cbs files in wd, with no updated data
israel_prices_cbs <- read_excel("israel_prices_cbs.xlsx")

# Food prices (dependent variable)
# Reshape the data from wide to long format
il_p <- israel_prices_cbs %>%
  pivot_longer(
    cols = January:December,  # Select columns from January to December
    names_to = "month_name",  # Create a new column for month names
    values_to = "price"       # Create a new column for prices
  ) %>%
  mutate(
    month = match(month_name, month.name), # Convert month names to numeric
    year = as.numeric(Year),              # Convert year to numeric
    commodity_raw = Item                    # Rename 'Item' to 'commodity_raw'
  ) %>%
  mutate(date = as.Date(paste(year,month,'01',sep = '-'))) %>%
  dplyr::select(date, commodity_raw, price) # Keep only relevant columns

# get prices in USD
il_p = il_p  %>%
  left_join(y = usd_nis, by = 'date',
            relationship = "many-to-many") %>%
  # keeps all observations in x
  left_join(y = food_map, by = 'commodity_raw',
            relationship = "many-to-many")


il_p = il_p %>%
  mutate(usdprice = price/usd_nis) %>%
  pivot_longer(cols = c('price','usdprice'),
               names_to = 'currency', values_to = 'price') %>%
  group_by(commodity, currency) %>%
  arrange(commodity, currency, date) %>%
  mutate(gap = case_when(
    is.na(lag(date)) ~ NA_integer_,  # First row has no previous row
    TRUE ~ (as.numeric(format(date, "%Y")) - as.numeric(format(lag(date), "%Y"))) * 12 +
      as.numeric(format(date, "%m")) - as.numeric(format(lag(date), "%m"))  # Month difference
  ))

#--------------------- 6. Handle Missing Data --------------------------- 

# The imputation logic in the function works by:
#  1. create a complete sequence of dates for each group. 
#  2. Imputation
# # 2.1 Short-term gaps (1 month) filled using linear interpolation, assumes gradual change. 
# # 2.2 For long-term gaps, use ARIMA models to predict missing values based on trends and seasonality detected in the available data. 
# #### Seasonality is dynamically determined using statistical tests (nsdiffs), 
# #### predictions are applied only to the positions of missing values. 
# #### If ARIMA fails (e.g., due to insufficient data), the missing values remain as NA, and are dropped.

# Function to add missing rows
fill_missing_data <- function(data) {
  # Initialize an empty tibble to store new rows
  additional_rows <- tibble(date = as.Date(character()),
                            area = character(),
                            commodity = character(),
                            currency = character(),
                            price = numeric(),
                            origin = character(),
                            gap = numeric(),
                            source = character())
  
  # Iterate over rows to handle missing data
  for (i in 1:nrow(data)) {
    if (!is.na(data$gap[i])) {
      if (data$gap[i] == 2) {
        # Add one row with gap == 2
        new_row <- tibble(
          date = data$date[i] - months(1),
          area = data$area[i],
          commodity = data$commodity[i],
          currency = data$currency[i],
          price = NA,
          origin = data$origin[i],
          gap = NA,
          source = "linear"
        )
        additional_rows <- bind_rows(additional_rows, new_row)
      } else if (data$gap[i] > 2) {
        # Add (gap-1) rows with gap > 2
        num_new_rows <- data$gap[i] - 1  # Calculate the number of rows to add
        new_rows <- tibble(
          date = seq(data$date[i] - months(num_new_rows), by = "1 month", length.out = num_new_rows),
          area = rep(data$area[i], num_new_rows),
          commodity = rep(data$commodity[i], num_new_rows),
          currency = rep(data$currency[i], num_new_rows),
          price = rep(NA, num_new_rows),
          origin = rep(data$origin[i], num_new_rows),
          gap = rep(NA, num_new_rows),
          source = rep("arima", num_new_rows)
        )
        additional_rows <- bind_rows(additional_rows, new_rows)
      }
    }
  }
  
  # Combine original data with additional rows and sort by date
  result <- bind_rows(data, additional_rows) %>%
    arrange(date) %>%
    ungroup()
  
  return(result)
}


# Function to fill missing prices in a single tibble
fill_prices <- fill_na_prices <- function(tibble_data) {
  tibble_data <- tibble_data %>% arrange(date) # Ensure data is ordered by date
  
  # Handle linear imputations
  if ("linear" %in% tibble_data$source) {
    linear_rows <- which(tibble_data$source == "linear" & is.na(tibble_data$price))
    if (length(linear_rows) > 0) {
      message("Performing linear interpolation for rows: ", paste(linear_rows, collapse = ", "))
      tibble_data$price[linear_rows] <- approx(
        x = tibble_data$date[!is.na(tibble_data$price)],
        y = tibble_data$price[!is.na(tibble_data$price)],
        xout = tibble_data$date[linear_rows]
      )$y
    }
  }
  
  # Handle ARIMA imputations
  if ("arima" %in% tibble_data$source) {
    arima_rows <- which(tibble_data$source == "arima" & is.na(tibble_data$price))
    if (length(arima_rows) > 0) {
      for (row in arima_rows) {
        # Filter data up to the missing point
        training_data <- tibble_data$price[1:(row - 1)]
        training_data <- training_data[!is.na(training_data)]
        
        if (length(training_data) > 2) { # Ensure sufficient data points
          arima_model <- auto.arima(training_data)
          prediction <- forecast(arima_model, h = 1)$mean
          message("Performing ARIMA imputation for row: ", row, 
                  " (date: ", tibble_data$date[row], ")")
          tibble_data$price[row] <- prediction
        } else {
          message("Not enough data to perform ARIMA for row: ", row, 
                  " (date: ", tibble_data$date[row], ")")
        }
      }
    }
  }
  
  return(tibble_data)
}

max_lag = il_p %>% drop_na(gap) %>%
  ungroup() %>%
  summarise(max(gap)) %>% as.numeric()

# perform only missing data exists
if(max_lag > 1){
  local_p <- fill_missing_data(il_p)
  
  p_list <- il_p %>%
    drop_na(date) %>%
    group_by(commodity, currency) %>%
    group_split()
  
  # Apply the function to each tibble in the list
  p_list <- lapply(p_list, fill_prices)
  
  il_p <- do.call(rbind, p_list)} else {
    # do nothing
    il_p <- il_p
}

il_p <- il_p %>%
  dplyr::select(date, commodity,price, currency, origin) %>%
  mutate(area = 'il', .after = date) %>%
  drop_na(price)

#--------------------- Save Data
setwd(wd_main)

save(il_p, 
     file = 'price_il.RData')
