# Compare between all forecasting options
rm(list = ls())

library(modelsummary)
library(tidyverse)

wd_main <- "..."

setwd(wd_main)

l = 1

price_data <- readRDS('price_data.RDS')

setwd(paste0(wd_main,'/graphs'))

#--------------------- 1. Set Functions --------------------------- 

# Absolute Percentage Errors (APE)
ape_fun <- function(obs, pred) {
  obs_safe <- ifelse(obs == 0, 1e-8, obs)
  ape <- ifelse(obs == pred, 0, abs((obs - pred) / obs_safe))
  return(ape)
}

# Root Mean Squared Errors (RMSE)
rmse_fun <- function(obs, pred) {
  error <- ifelse(obs == pred, 0, pred - obs)
  sqrt(mean(error^2))
}

# Mean Absolute Percentage Errors (MAPE)
mape_fun <- function(obs, pred) {
  obs_safe <- ifelse(obs == 0, 1e-8, obs)
  ape <- ifelse(obs == pred, 0, abs((obs - pred) / obs_safe))
  mean(ape)
}

# Define function to load and bind RData files
load_and_bind_rdata <- function(pattern) {
  # List all .RData files and filter those matching the pattern
  rdata_files <- list.files(pattern = "\\.RData$", full.names = TRUE)
  rdata_files <- rdata_files[grepl(pattern, rdata_files)]
  
  if (length(rdata_files) == 0) {
    stop("No matching RData files found.")
  }
  
  # Initialize an empty list to store loaded data frames
  data_list <- list()
  
  # Loop through each file and load its contents into a temporary environment
  for (file in rdata_files) {
    temp_env <- new.env()  # Create a temporary environment to load the data
    load(file, envir = temp_env)  # Load data into temp_env
    
    # Get the names of objects in the temporary environment
    loaded_objects <- ls(envir = temp_env)
    
    # Identify the correct object (assuming the name contains "forecast")
    data_object <- loaded_objects[grepl(pattern, loaded_objects)]
    
    if (length(data_object) == 1) {
      # Retrieve the data frame and check if it's valid
      loaded_df <- get(data_object, envir = temp_env)
      if (is.data.frame(loaded_df)) {
        data_list[[file]] <- loaded_df
      } else {
        warning(paste("Skipping file:", file, "- Object is not a data frame."))
      }
    } else {
      warning(paste("Skipping file:", file, "- Could not determine the correct object."))
    }
  }
  
  # Check if any data was loaded
  if (length(data_list) == 0) {
    stop("No valid data frames loaded.")
  }
  
  # Combine all loaded data frames into a single data frame
  combined_data <- bind_rows(data_list)  # Add source_file column
  
  return(combined_data)
}

war_date <- as.Date('2023-10-01')

#--------------------- 2. Get Data ----------------------

forecast_tab <- load_and_bind_rdata(pattern = "forecast_results")  # Loads all forecast RData files

gaza_data <- price_data %>%
  # remove duplicates
  filter(lag == l) %>%
  filter(!commodity %in% c('energy','water')) %>%
  mutate(
    across(where(is.character), as.factor)) %>%
  # remove empty levels
  mutate(across(where(is.factor), droplevels)) %>%
  filter(area == 'gaza') %>%
  dplyr::select(-area, -lag) %>%
  arrange(date)

# Store group names
commodity_names <- unique(gaza_data$commodity)
models <- unique(forecast_tab$model)
currencies <- unique(forecast_tab$currency)
p_type = unique(forecast_tab$type)

last_obs = max(gaza_data$date)

#--------------------- 3. Create Summarising Table ----------------------

forecast_tab <- forecast_tab %>%
  mutate(forecast = case_when(forecast_date < war_date ~ 99,
                              .default = forecast)) %>%
  filter(forecast_date <= last_obs) %>%
  dplyr::select(-'mape',-'rmse') %>%
  mutate( # h horizons before forecast_date
  train_date = forecast_date %m-% months(as.integer(horizon)),
  model = case_when(forecast==99 ~ "obs",
                    TRUE ~ str_to_upper(model)))

results_tab <- forecast_tab %>%
  filter(forecast_date >= war_date, forecast_date <= last_obs) %>%
  mutate(# absolute percentage errors (APE)
    ape = ape_fun(obs = obs., pred = forecast))

errors_tab <- results_tab %>%
  group_by(commodity, horizon, model,currency,type) %>%
  summarise(total = n(),
            mape = mape_fun(obs., forecast),
            rmse = rmse_fun(obs., forecast),
            .groups = 'drop') %>%
  # set error metric as classifier
  pivot_longer(cols = matches(c('mape','rmse')),
               names_to = 'error_metric')

errors_list <- errors_tab %>%
  split(.$commodity) %>%
  lapply(function(df) {
    split(df, df$error_metric)
  })

forecast_list <- results_tab %>%
  filter(forecast != 99) %>%
  split(.$commodity) %>%
  lapply(function(df) {
    split(df, df$model)
  })


# -------------------------------- 4. Save to Excel --------------------------
# Load required libraries
library(writexl)

# Define the function
save_nested_list_to_excel <- function(nested_list, list_name = deparse(substitute(nested_list))) {
  
  # Extract the first word before "_list" in the list name
  output_dir <- paste0('tables/',gsub("_list.*", "", list_name), "_output")
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Loop through each main item in the list
  for (main_key in names(nested_list)) {
    
    # Get the sublist (each containing multiple data frames)
    sublist <- nested_list[[main_key]]
    
    # Define the output file path
    file_path <- file.path(output_dir, paste0(main_key, ".xlsx"))
    
    # Check if sublist contains data frames
    if (all(sapply(sublist, inherits, "data.frame"))) {
      # Write each data frame as a separate sheet in the Excel file
      write_xlsx(sublist, path = file_path)
    } else {
      warning(paste("Skipping", main_key, "- does not contain valid data frames"))
    }
  }
  
  # Completion message
  cat("All Excel files have been saved in the '", output_dir, "' directory.\n")
}

# Save
save_nested_list_to_excel(errors_list)  # Saves to "errors_output/"
save_nested_list_to_excel(forecast_list)

write_xlsx(results_tab, 
           path = 'tables/forecast_output/forecast_table.xlsx')

write_xlsx(errors_tab, 
           path = 'tables/errors_output/errors_table.xlsx')


# ------------ 5. Interpretation

# forecast_tab shows the forecasts (forecast) and the actual price (obs.) of food in Gaza since October 2023.
# errors_tab summarises the forecast errors (value) of food price forecasts in Gaza since October 2023.
# The forecasts were performed relative to the price (type="price") or % monthly price change (type="perc)
# The forecasts were performed for 1 to 17 months ahead (horizon) for different commodities (commodity)
# The forecasts were performed relative to 3 prices (currency: NIS, NIS_real, USD)

# In forecast_tab, the error values are given relative to Absolute Percentage Errors (ape)
# Analyse the forecast accuracy of the models: How did they perform during the months of the war? (forecast_date) 

# In errors_tab, the error values are given relative to 2 metrics (error_metric: MAPE, RMSE)
# Analyse the forecast accuracy of the models: How did they perform relative to forecast horizon and commodities?

# In both tables, analyse relative to currency and commodity (separated for each currency and commodity+currency)

saveRDS(results_tab, 'forecast_tab.RDS')
saveRDS(errors_tab, 'errors_tab.RDS')

NIS_price <- results_tab %>%
  filter(currency == 'NIS')

NIS_real_price <- results_tab %>%
  filter(currency == 'NIS_real')

trainSep23<- NIS_price %>%
  #filter(train_date == as.Date("2023-09-01")) %>%
  dplyr::select(-train_date, -currency)

NIS_error <- errors_tab %>%
  filter(currency == 'NIS') %>%
  dplyr::select(-total, -currency)


before_war <- gaza_data %>%
  filter(date < war_date) %>%
  mutate(period = 'before')

compare_diff <- gaza_data %>%
  mutate(period = 'all') %>%
  rbind(before_war)

compare_sum <- compare_diff %>%
  filter(currency =='NIS') %>%
  pivot_longer(cols = matches(c('price','perc_change')),
               names_to = 'type', 
               values_to = 'price') %>%
  drop_na(price) %>%
  group_by(commodity,period,type) %>%
  summarise(sd_p = sd(price))
  


# ------------ 6. Paper plot

# Definition for visualisation
theme_default <- 
  theme(rect = element_rect(fill = 'white'), 
        plot.background = element_rect(fill = alpha('#ffece1', 0.5)), 
        panel.background = element_rect(fill = 'white'),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(), 
        legend.position = 'none',
        legend.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        text = element_text(size = 16),
        panel.grid = element_blank())

# possible combinations
situation <- expand.grid(model = str_to_upper(models), currency = currencies, type = p_type)
situation <- cbind(run = 1:nrow(situation), situation)
situation <- as.matrix(situation)

# m = 'tbats'
# c = 'NIS'
# t = 'perc'

for (i in 1:nrow(situation)) {
  
  m = situation[i,'model']
  c = situation[i,'currency']
  t = situation[i,'type']
  
  if(t == 'perc'){
    y_axis = paste0('Price (',c,', % monthly change)')
  } else {
    y_axis = paste0('Price (',c,')')
  }
  
  if(m == 'ARIMA'){
    plot_colours = c('#008b8b','black')
  } else {
    plot_colours = c('black','#008b8b')
  }
  
  viz_tab <- forecast_tab %>%
    filter(model %in% c(m,'obs'), currency == c, type == t) %>%
    mutate(model = str_to_upper(model),
           train_date = forecast_date %m-% months(as.integer(horizon))) %>%
    pivot_longer(cols = c('forecast', 'obs.'), 
                 names_to = "model_", 
                 values_to = "price") %>%
    filter(price != 99) %>%
    # filter to keep forecasts only from train date
    filter(model == 'OBS' | train_date == as.Date("2023-09-01")) %>%
    mutate(model = case_when(model_ == 'forecast' ~ m,
                             TRUE ~ model_), .keep = 'unused') %>%
    dplyr::select(-ape)
  
  viz <- viz_tab %>%
    ggplot(aes(x = forecast_date, y = price, color = model)) +
    geom_vline(xintercept = war_date, alpha = 0.5, linetype = 'dashed') +
    geom_line(size = 1) +  # Plot actual and forecasted lines
    geom_ribbon(data = viz_tab %>% filter(model == m), 
                # Add confidence intervals
                aes(ymin = lower_ci, ymax = upper_ci), 
                colour = NA, fill = "#008b8b", alpha = 0.2) +
    labs(title = paste("Food Prices with", m, "Forecasts"),
         y = y_axis) +
    scale_x_date(
      date_labels = "%m/%y",    # Format as "10/23"
      date_breaks = "4 months" # Adjust interval to include Oct 2023
    ) +
    scale_colour_manual(values = plot_colours) +
    facet_wrap(~ commodity, scales = "free_y") +
    theme_default
  
  print(viz)
  
  ggsave(viz,
         filename = paste(m,c,t,'viz.pdf',sep = '_'),
         width = 297, height = 210, units = 'mm')
}

