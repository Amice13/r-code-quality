#browseURL('https://www.worldbank.org/en/research/commodity-markets')

# Global World Bank Prices (USD/Tonne)


library(httr)
library(readxl)
library(rvest)
library(stringdist)
library(tidyverse)

wd_main <- "..."
wd_data <- paste0(wd_main, "/data/raw_data")

setwd(wd_data)
seed = 4

#--------------------- 1. Define Relevant Indicators --------------------------- 

#--- define parameters to work with
commodity_map <- read_csv('meta_data/food_meta.csv')

# Replace commodity_raw in fao_data using food_map
commodity_map <- commodity_map %>%
  dplyr::select(category, commodity, global_commodity) %>%
  distinct_all()

# List of relevant columns
relevant_prices <- commodity_map %>%
  drop_na() %>% distinct(global_commodity)

relevant_prices <- c(relevant_prices$global_commodity)

# Replace character values with NA and convert to numeric
clean_data <- function(data) {
  char_cols <- sapply(data, is.character)
  data[, char_cols] <- lapply(data[, char_cols], function(x) as.numeric(as.character(x)))
  return(data)
}

# Define commodity names normalisation
normalize_commodity <- function(commodity) {
  commodity %>%
    tolower() %>%                           # Convert to lowercase
    str_remove_all("\\(.*\\)") %>%          # Remove content inside parentheses
    str_trim() %>%                          # Trim whitespace
    str_replace_all(",", "_") %>%           # Replace commas with underscores
    str_replace_all("\\s+", "")             # Remove spaces
}

# dependent variable
load('price_local_data.RData')
food_p <- price_local_data$food_p
date0 = min(food_p$date) # min date + max lag
date0 = as.Date(paste(year(date0)-1,month(date0),day(date0),sep='-'))

#--------------------- 2. World Bank: Download and filter global prices --------------------------- 
url_prices <- 'https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx'
GET(url_prices, write_disk(tf <- tempfile(fileext = '.xlsx')))

# nominal prices
raw_price <- read_xlsx(tf, sheet = 'Monthly Prices', col_names = F)
# Find the index of the first row without NA values
first_data <- which(raw_price[,1] == "1960M01")-1

# define titles
titles <- which(raw_price[,2] == 'Crude oil, average')
titles <- c('date', as.character(raw_price[titles,-1]))

raw_price <- read_xlsx(tf, sheet = 'Monthly Prices', skip = first_data, col_names = F)
colnames(raw_price) <- titles

# Comment on
# Wheat SRW Missing data points (late 2022 and early 2023): 
##  may correlate with temporary changes in wheat trading mechanisms in response to geopolitical or economic shocks
## (Ukraine-Russia war significantly disrupted global wheat markets, especially for Soft Red Winter wheat).

global_p <- raw_price %>%
  as.data.frame() %>%
  dplyr::select(
    date,
    # Match relevant_prices patterns
    matches(paste(relevant_prices, collapse = "|"), ignore.case = T)
  ) %>%
  # transforn date col into date format
  mutate(date = sub(date, pattern = 'M', replacement = '-01-')) %>%
  mutate(date = as.Date(date, '%Y-%d-%m')) %>%
  # replace character values with NA and convert to numeric
  clean_data() %>% drop_na()


global_p <- global_p %>%
  pivot_longer(cols = -1,
               names_to = 'commodity',
               values_to = 'price') %>%
  arrange(commodity,date) %>%
  group_by(commodity) %>%
  mutate(gap = case_when(
    is.na(lag(date)) ~ NA_integer_,  # First row has no previous row
    TRUE ~ (as.numeric(format(date, "%Y")) - as.numeric(format(lag(date), "%Y"))) * 12 +
      as.numeric(format(date, "%m")) - as.numeric(format(lag(date), "%m"))  # Month difference
  ),
  source = "world bank")

#--------------------- 2. Handle Missing Data --------------------------- 

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
                            commodity = character(),
                            price = numeric(),
                            gap = numeric(),
                            source = character())
  
  # Iterate over rows to handle missing data
  for (i in 1:nrow(data)) {
    if (!is.na(data$gap[i])) {
      if (data$gap[i] == 2) {
        # Add one row with gap == 2
        new_row <- tibble(
          date = data$date[i] - months(1),
          commodity = data$commodity[i],
          price = NA,
          gap = NA,
          source = "linear"
        )
        additional_rows <- bind_rows(additional_rows, new_row)
      } else if (data$gap[i] > 2) {
        # Add (gap-1) rows with gap > 2
        new_rows <- tibble(
          date = seq(data$date[i] - months(data$gap[i] - 1), by = "1 month", length.out = data$gap[i] - 1),
          commodity = rep(data$commodity[i], data$gap[i] - 1),
          price = rep(NA, data$gap[i] - 1),
          gap = rep(NA, data$gap[i] - 1),
          source = rep("arima", data$gap[i] - 1)
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

max_lag = global_p %>% drop_na(gap) %>%
  ungroup() %>%
  summarise(max(gap)) %>% as.numeric()


# perform only missing data exists
if(max_lag > 1){
  global_p <- fill_missing_data(global_p)
  
  global_list <- global_p %>%
    drop_na(date) %>%
    group_by(commodity) %>%
    group_split()
  
  # Apply the function to each tibble in the list
  global_list <- lapply(global_list, fill_prices)
  
  global_p <- do.call(rbind, global_list)
} else {
  # do nothing
  global_p <- global_p
}


#--------------------- 3. Adapt Global to Local ----------------- 

global_p <- global_p %>%
  mutate(commodity = normalize_commodity(commodity))


#--------------------- 4. Save Data ---------------------------

save(global_p, file = 'global_data.RData')

wd_main <- "..."
wd_data <- paste0(wd_main,'/data/raw_data')

#--------------------- 1. define parameters to work with --------------------------- 
global_p <- global_p %>%
  mutate(price = price/1000) %>%
  mutate(currency = 'usd', .after = price) %>%
  mutate(area = 'global', .after = date) %>%
  mutate(commodity = case_when(commodity=="rice_thai5%" ~ 'rice',
                               commodity=="sugar_world" ~ 'sugar',
                               commodity=="wheat_ushrw" ~ 'wheat',
                               .default = NA)) %>%
  mutate(origin = 'imported', .after = currency) %>%
  dplyr::select(date, area, commodity,price,currency,origin) %>%
  drop_na(commodity)

#--------------------- Save Data
setwd(wd_main)

save(global_p, 
     file = 'global_p.RData')
