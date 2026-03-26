# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://data.humdata.org/event/opt-israel-hostilities')
#browseURL('https://data.humdata.org/group/pse')
#browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic')
# food:
# browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic?permalink=ab720aa5-9aba-4ff9-9bc8-28aed972e853,192f7ffd-837d-436e-b6aa-f9ded4c44b15,7bc4cdb5-b906-48cd-b3f0-4e069e0d7fa8,cefd6767-5b40-47ad-984a-baa52718b346,15d9d8d3-26bd-4dc8-aec3-aa40d5cf28fe,98fc49ef-57e5-47c3-9584-437511c58f22,a18cd529-3b31-4746-9905-b727948fa416,32679c2f-1ccc-44bd-b571-587db0029216,3fe35410-fea3-48dc-afa8-ed4a859f2b1b,dfdaab5e-719e-4a1d-ae3d-34ccb195933c,2bfd008d-badb-4867-8868-995a3faf8a9a,2956ae5f-02c2-436e-85ad-54b8968945c6,65d5080a-2cea-4c14-9ce6-6c79cd8c2a90,3cdd2142-5a56-495f-89cf-b805140be0f5,c8f9bdff-5797-44ad-865f-74f866b9324d,40c0966f-f1f8-48b8-a5cf-8c4d6965956c')
# milk:
# browseURL('https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/domestic?permalink=dc2a49b3-5e8b-4c74-aaec-7d78acdebba0,912fffc0-83d7-4e80-8d63-7f3c1b0e5062')

# Local Food Prices (WFP)

rm(list = ls())

library(ggthemes)
library(tidyverse)

wd_main <- "..."

setwd(wd_main)

#--------------------- 1. Download Raw Local data --------------------------- 

# Create an empty list to store tibbles
price_data <- list()

# List all .RData files in the current working directory
data_files <- list.files(pattern = "\\.RData$", full.names = TRUE)

# Load each .RData file and extract the tables
for (file in data_files) {
  temp_env <- new.env()       # Create a temporary environment
  load(file, envir = temp_env) # Load the RData file into the environment
  
  # Assuming there's only one table per file, extract it
  table <- as.list(temp_env)[[1]]
  
  # Append the table to the list
  price_data[[file]] <- table
}

price_data <- bind_rows(price_data)

price_data <- price_data %>%
  mutate(
    area = as.factor(area),
    commodity = as.factor(commodity),
    currency = case_when(currency == 'local_real' ~'NIS_real',
                         .default = currency)) %>%
  mutate(currency = case_when(currency %in% c('local','price') ~'NIS',
                             currency %in% c('usd','usdprice') ~ 'USD',
                             .default = currency)
         )


seed = 4

#--------------------- 2. Make Real Precent Change of Prices --------------------------- 

# Function to calculate percent change in monthly lags
calculate_percent_change <- function(data, col_name, lag_range = 1:3) {
  # Ensure the column name exists in the data
  if (!(col_name %in% colnames(data))) {
    stop(paste("Column", col_name, "does not exist in the data."))
  }
  
  # Initialize an empty list to store results for each lag
  results <- list()
  
  # Loop over the range of lags
  for (lag in lag_range) {
    # Calculate percent change for the current lag
    lagged_data <- data %>%
      mutate(
        perc_change = (get(col_name) - lag(get(col_name), lag)) / lag(get(col_name), lag),
        lag = lag # Add the lag value
      )
    
    # Append the lagged data to the results list
    results[[as.character(lag)]] <- lagged_data
  }
  
  # Combine all lagged data into a single data frame
  combined_data <- bind_rows(results)
  
  return(combined_data)
}
  
price_data <- price_data %>%
  group_by(area, currency, commodity) %>%
  calculate_percent_change(col_name = "price",
                           lag_range = 1:3) %>%
  ungroup() #%>%
  #filter(date <= '2024-11-01')

#--------------------- Save Data
write_rds(price_data, 
     file = 'price_data.RDS')
