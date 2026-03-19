# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://data.humdata.org/event/opt-israel-hostilities')
#browseURL('https://data.humdata.org/group/pse')

# Local Food 
Prices (WFP)

library(forecast)
library(httr)
library(imputeTS)
library(stringdist)
library(tidyverse)

rm(list=ls())

wd_main <- "..."
wd_data <- paste0(wd_main,'/data/raw_data')

setwd(wd_data)
seed = 4

#--------------------- 1. define parameters to work with --------------------------- 

food_map <- read_csv('ps_meta.csv')

food_map <- food_map %>%
  dplyr::select(wfp,commodity, origin) %>%
  pivot_longer(cols = 'wfp',
               names_to = 'source',
               values_to = 'commodity_raw') %>%
  drop_na()

#--------------------- 2. Download WFP data (energy, water) --------------------------- 

# base URL
wfp_url = "https://data.humdata.org/dataset/7d06b059-5831-4101-aa68-6d9123ad65b7/resource/b82509ec-d48e-41d7-b376-af51c7f66737/download/wfp_food_prices_pse.csv"

GET(wfp_url, write_disk(tf <- tempfile(fileext = '.csv')))

wfp_data <- read_csv(tf)

# Define the processing function
process_data <- function(df) {
  df %>%
    # 1. Remove rows where all columns start with "#" or are empty/NA
    filter(!if_all(everything(), ~ str_starts(as.character(.), "#") | . == "" | is.na(.))) %>%
    
    # 2. Mutate numeric-like columns into numeric
    mutate(across(where(~ all(suppressWarnings(!is.na(as.numeric(as.character(.)))))), ~ as.numeric(as.character(.)))) %>%
    
    # 3. Mutate date-like columns into Date
    mutate(across(where(~ all(!is.na(as.Date(as.character(.), format = "%Y-%m-%d")))), ~ as.Date(as.character(.))))
}

wfp_data <- process_data(wfp_data)

# Filter rows where category matches and match distance is below a threshold
wfp_p <- wfp_data %>%
  mutate(commodity = case_when(str_detect(commodity, "(?i)fuel") ~'energy',
                               str_detect(commodity, "(?i)water") ~'water',
                               .default = NA)) %>%
  drop_na()

# 4.1 Handle complexities in units
# Normalize units and adjust prices
wfp_p <- wfp_p %>%
  mutate(
    # Retain the original unit for verification
    original_unit = unit,
    
    # Extract numeric multiplier from the unit
    multiplier = str_extract(unit, "\\d+") %>% as.numeric(),
    
    # Normalize price and usdprice using the multiplier (default to 1 if no multiplier)
    multiplier = if_else(is.na(multiplier), 1, multiplier),
    price = price / multiplier,
    usdprice = usdprice / multiplier,
    
    # Simplify the unit by removing numeric parts
    unit = str_remove(unit, "\\d+") %>% str_trim()
  ) %>% dplyr::select(-multiplier)


# 4.2 Handle complexities in markets
## if several Gaza, take only Gaza
## if several West Bank, take mean
wfp_p <- wfp_p %>%
  group_by(date, admin1, commodity) %>%
  mutate(
    # Logic for Gaza Strip
    admin2 = case_when(
      admin1 == "Gaza Strip" & admin2 == "Gaza" ~ admin2,
      admin1 == "Gaza Strip" & !any(admin2 == "Gaza") ~ "mean",
      admin1 == "West Bank" ~ "mean",
      TRUE ~ admin2
    ),
    # Aggregate price and usdprice
    price = case_when(
      admin1 == "Gaza Strip" & admin2 == "mean" ~ mean(price, na.rm = TRUE),
      admin1 == "West Bank" ~ mean(price, na.rm = TRUE),
      TRUE ~ price
    ),
    usdprice = case_when(
      admin1 == "Gaza Strip" & admin2 == "mean" ~ mean(usdprice, na.rm = TRUE),
      admin1 == "West Bank" ~ mean(usdprice, na.rm = TRUE),
      TRUE ~ usdprice
    )
  ) %>%
  ungroup() %>%
  distinct(date, admin1, commodity, .keep_all = T)


#--------------------- 3. Arrange and Tidy Observed Prices ---------------------------

setwd(wd_main)
load('price_food.RData')

colnames(local_p)

nonfood_p <- wfp_p %>%  
  mutate(
    date = date,
    area = if_else(str_detect(admin1, "(?i)gaza"), "gaza",
                 if_else(str_detect(admin1, "(?i)west"), "wb", NA)),
    commodity = as.factor(commodity),
    price = price, usdprice = usdprice,
    .keep = 'none'
    ) %>%
  pivot_longer(cols = c('price','usdprice'),
               names_to = 'currency', values_to = 'price') %>%
  group_by(area, commodity, currency) %>%
  arrange(area, commodity, currency, date) %>%
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
                            origin = 'imported',
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

max_lag = nonfood_p %>% drop_na(gap) %>%
  ungroup() %>%
  summarise(max(gap)) %>% as.numeric()

# perform only missing data exists
if(max_lag > 1){
  nonfood_p <- fill_missing_data(nonfood_p)
  
  p_list <- nonfood_p %>%
    drop_na(date) %>%
    group_by(area, commodity, currency) %>%
    group_split()
  
  # Apply the function to each tibble in the list
  p_list <- lapply(p_list, fill_prices)
  
  nonfood_p <- do.call(rbind, p_list)} else {
    # do nothing
    nonfood_p <- nonfood_p
  }

nonfood_p <- nonfood_p %>%
  mutate(origin = 'non-food') %>%
  dplyr::select(date, area, commodity,price, currency, origin)

#--------------------- Save Data
save(nonfood_p, 
     file = 'price_nonfood.RData')
