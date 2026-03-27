# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic')
# food:
# browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic?permalink=ab720aa5-9aba-4ff9-9bc8-28aed972e853,192f7ffd-837d-436e-b6aa-f9ded4c44b15,7bc4cdb5-b906-48cd-b3f0-4e069e0d7fa8,cefd6767-5b40-47ad-984a-baa52718b346,15d9d8d3-26bd-4dc8-aec3-aa40d5cf28fe,98fc49ef-57e5-47c3-9584-437511c58f22,a18cd529-3b31-4746-9905-b727948fa416,32679c2f-1ccc-44bd-b571-587db0029216,3fe35410-fea3-48dc-afa8-ed4a859f2b1b,dfdaab5e-719e-4a1d-ae3d-34ccb195933c,2bfd008d-badb-4867-8868-995a3faf8a9a,2956ae5f-02c2-436e-85ad-54b8968945c6,65d5080a-2cea-4c14-9ce6-6c79cd8c2a90,3cdd2142-5a56-495f-89cf-b805140be0f5,c8f9bdff-5797-44ad-865f-74f866b9324d,40c0966f-f1f8-48b8-a5cf-8c4d6965956c')
# milk:
# browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic?permalink=dc2a49b3-5e8b-4c74-aaec-7d78acdebba0,912fffc0-83d7-4e80-8d63-7f3c1b0e5062')

# Local Food Prices (WFP)

library(forecast)
library(imputeTS)
library(stringdist)
library(tidyverse)

wd_main <- "..."
wd_data <- paste0(wd_main,"/data")

wd_main <- "..."
wd_data <- paste0(wd_main, "/data/raw_data")
setwd(wd_data)
seed = 4

# Output directory
output_dir <- paste0(wd_data,"/raw_data")
setwd(output_dir)
dir.create(output_dir, showWarnings = FALSE)

#--------------------- 1. Arrange and FAO Food Data (from WD) --------------------------- 

# Get fao files in wd, with no updated data
csv_fao <- list.files(pattern = "^fao.*\\.csv$")
fao_list <- lapply(csv_fao, read_csv)
fao_data <- fao_list[[1]] %>%
  full_join(y = fao_list[[2]], by = 'Date') %>%
  pivot_longer(cols = -Date, values_to = 'price') %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y", # original format
                        tryFormat = "%Y-%m-%d"), .keep = 'unused', .before = 1) %>%
  mutate(
    area = str_extract(name, "(?<=Occupied Palestinian Territory, RETAIL, ).*?(?=,)"),
    commodity_raw = str_extract(name, 
                            "(?<=Occupied Palestinian Territory, RETAIL, (Gaza Strip|West Bank), ).*?(?=, (ILS-real|ILS|USD)/(Kg|Liter))"),
    currency = case_when(str_detect(name, "(?i)ils") & str_detect(name, "(?i)real") ~ "local_real",
                         str_detect(name, "(?i)ils") ~ "local",
                         str_detect(name, "(?i)usd") ~ "usd",
                         TRUE ~ NA_character_),
    source = 'fpma',
    .keep = 'unused'
  ) %>% relocate(price, .before = source)

#--------------------- 2. define parameters to work with --------------------------- 

setwd(wd_main)

food_map <- read_csv('data/meta_data/ps_meta.csv')

food_map <- food_map %>%
  dplyr::select(fpma,commodity, origin) %>%
  pivot_longer(cols = 'fpma',
               names_to = 'source',
               values_to = 'commodity_raw') %>%
  drop_na()

#--------------------- 3. Arrange and Tidy Observed Prices ---------------------------

# Food prices (dependent variable)
local_p = fao_data  %>%
  # keeps all observationsin x
  left_join(y = food_map, by = c('source','commodity_raw'),
            relationship = "many-to-many") %>%
  distinct(date, area, commodity, currency, price, .keep_all = T) %>%
  mutate(
    area = if_else(str_detect(area, "(?i)gaza"), "gaza",
                   if_else(str_detect(area, "(?i)west"), "wb", NA)),
    .keep = 'unused'
  ) %>%
  drop_na(price) %>%
  group_by(area, commodity, currency) %>%
  arrange(area, commodity, currency, date) %>%
  mutate(gap = case_when(
    is.na(lag(date)) ~ NA_integer_,  # First row has no previous row
    TRUE ~ (as.numeric(format(date, "%Y")) - as.numeric(format(lag(date), "%Y"))) * 12 +
      as.numeric(format(date, "%m")) - as.numeric(format(lag(date), "%m"))  # Month difference
  ))

#--------------------- 4. Handle Missing Data --------------------------- 

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

max_lag = local_p %>% drop_na(gap) %>%
  ungroup() %>%
  summarise(max(gap)) %>% as.numeric()

# perform only missing data exists
if(max_lag > 1){
  local_p <- fill_missing_data(local_p)
  
  p_list <- local_p %>%
    drop_na(date) %>%
    group_by(area, commodity, currency) %>%
    group_split()
  
  # Apply the function to each tibble in the list
  p_list <- lapply(p_list, fill_prices)
  
  local_p <- do.call(rbind, p_list)} else {
    # do nothing
    local_p <- local_p
}

local_p <- local_p %>%
  dplyr::select(date, area, commodity, price, currency, origin)

#--------------------- Save Data
save(local_p, 
     file = 'price_food.RData')
