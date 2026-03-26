# browseURL('https://www.analyticssteps.com/blogs/data-scraping-r-programming-part-1')
#browseURL('https://data.humdata.org/event/opt-israel-hostilities')
#browseURL('https://data.humdata.org/group/pse')

rm(list = ls())

library(forecast)
library(tidyverse)
library(tseries)

wd_main <- "..."

setwd(wd_main)
currencyi = 'NIS'
method = 'perc'

price_data <- readRDS('price_data.RDS')

#--------------------- 0. Define Data --------------------------- 

war_date <- as.Date('2023-10-01')

seed = 4

l = 1 # lags to choose

gaza_data <- price_data %>%
  # remove duplicates
  filter(lag == l & currency == currencyi) %>%
  filter(!commodity %in% c('energy','water')) %>%
  dplyr::select(-price) %>%
  mutate(
    origin = as.factor(str_to_title(origin))) %>%
  # remove empty levels
  mutate(across(where(is.factor), droplevels)) %>%
  filter(area == 'gaza') %>%
  dplyr::select(-area, -lag, -currency) %>%
  arrange(date) %>%
  drop_na(perc_change)

before_war <- gaza_data %>% filter(date < war_date)
war_price <- gaza_data %>% filter(date >= war_date)
war_length <- length(unique(war_price$date))

# Convert data into a list of time series objects
ts_data <- gaza_data %>%
  arrange(date) %>%
  group_by(commodity) %>%
  summarise(price_ts = list(ts(perc_change, start = c(2007, 1), frequency = 12))) %>%
  pull(price_ts)  # Extract list of time series objects

# Function to convert to time series for each period
convert_to_ts <- function(data) {
  data %>%
    arrange(date) %>%
    group_by(commodity) %>%
    summarise(price_ts = list(ts(perc_change, start = c(year(min(date)), month(min(date))), frequency = 12))) %>%
    pull(price_ts)
}

# Convert price to time series for both periods
ts_before <- convert_to_ts(before_war)
ts_after  <- convert_to_ts(war_price)

# Store commodity names
commodity_names <- unique(gaza_data$commodity)

# Assign names to lists
names(ts_before) <- commodity_names
names(ts_after)  <- commodity_names


#--------------------- 1. Analyse Trends --------------------------- 

# 1. Check for Trends
# Trends can be analyzed using:
## Linear trend models (OLS regression with time)
## Augmented Dickey-Fuller (ADF) test (for stationarity)
## Moving averages (to detect long-term movements)

# trend_price: Slope of price over time (positive = increasing, negative = decreasing).
# trend_perc_change: Slope of percentage change over time.
# p-values from ADF test (p < 0.05 -> series is stationary; no trend).

# Function to compute trend for each period
compute_trend <- function(ts_data) {
  lapply(ts_data, function(ts_series) {
    time_index <- seq_along(ts_series)
    lm_model <- lm(ts_series ~ time_index)
    trend_slope <- coef(lm_model)[2]
    adf_p_value <- adf.test(ts_series, alternative = "stationary")$p.value
    return(list(trend_slope = trend_slope, adf_p_value = adf_p_value))
  })
}

# Compute trends for both periods
trend_before <- compute_trend(ts_before)
trend_after  <- compute_trend(ts_after)

# Convert to data frame
trend_df <- data.frame(
  commodity = names(trend_before),
  trend_slope_before = sapply(trend_before, function(x) x$trend_slope),
  trend_slope_after = sapply(trend_after, function(x) x$trend_slope),
  adf_p_before = sapply(trend_before, function(x) x$adf_p_value),
  adf_p_after = sapply(trend_after, function(x) x$adf_p_value)
)

# Compare trend_slope_before vs. trend_slope_after → Did the trend change?
# Compare adf_p_before vs. adf_p_after → Did stationarity change?
print(trend_df)

#--------------------- 2. Analyse Seasonality --------------------------- 
# Autocorrelation Function (ACF): values close to 1 at lags l-> strong seasonality
# Seasonal decomposition using STL: Helps in detecting periodicity.

# Function to compute seasonality indicators
compute_seasonality <- function(ts_data) {
  lapply(ts_data, function(ts_series) {
    acf_values <- acf(ts_series, lag.max = war_length, plot = FALSE)$acf[-1] 
    stl_decomp <- stl(ts_series, s.window = "periodic")
    return(list(acf_max = max(acf_values), 
                seasonality_component = stl_decomp$time.series[, "seasonal"]))
  })
}

# Compute seasonality for both periods
seasonality_before <- compute_seasonality(ts_before)
#seasonality_after  <- compute_seasonality(ts_after)

# Convert to data frame
seasonality_df <- data.frame(
  commodity = names(seasonality_before),
  acf_before = sapply(seasonality_before, function(x) x$acf_max)#,
  #acf_after = sapply(seasonality_after, function(x) x$acf_max)
)

# Compare acf_before vs. acf_after → Did seasonality strength change?
print(seasonality_df)


#--------------------- 3. Correlation Between Commodities --------------------------- 

# Convert data to wide format for correlation analysis
wide_before <- before_war %>%
  dplyr::select(date, commodity, perc_change) %>%
  drop_na(perc_change) %>%
  spread(commodity, perc_change)

wide_after <- war_price %>%
  dplyr::select(date, commodity, perc_change) %>%
  drop_na(perc_change) %>%
  spread(commodity, perc_change)

# Compute correlation matrices
cor_matrix_before <- cor(wide_before[,-1], use = "pairwise.complete.obs")
cor_matrix_after  <- cor(wide_after[,-1], use = "pairwise.complete.obs")

# Compare correlations before and after → Did relationships between commodities change?
print("Correlation Before:")
print(cor_matrix_before)

print("Correlation After:")
print(cor_matrix_after)


#--------------------- 4. Differences Between Commodities --------------------------- 
# ANOVA (if normality assumptions hold)
# Kruskal-Wallis test (if data is non-normal)

test_results <- list()

for (com in commodity_names) {
  
  subset_before <- before_war %>%
    dplyr::select(date, commodity, perc_change) %>%
    drop_na(perc_change) %>% filter(commodity == com)
  
  subset_after <- war_price %>%
    dplyr::select(date, commodity, perc_change) %>%
    drop_na(perc_change) %>% filter(commodity == com)
  
  # Ensure enough groups exist
  if (length(unique(subset_before$period)) < 2 | length(unique(subset_after$period)) < 2) {
    test_results[[com]] <- list(
      normality_p_before = NA,
      normality_p_after = NA,
      test_used = "Not enough groups",
      test_result = "Skipped due to single group"
    )
    next
  }
  
  # Normality check
  shapiro_p_before <- if (nrow(subset_before) >= 3) shapiro.test(subset_before$perc_change)$p.value else NA
  shapiro_p_after  <- if (nrow(subset_after) >= 3)  shapiro.test(subset_after$perc_change)$p.value else NA
  
  # Choose ANOVA or Kruskal-Wallis
  if (!is.na(shapiro_p_before) && shapiro_p_before > 0.05) {
    test_result_before <- summary(aov(perc_change ~ period, data = subset_before))
    test_type_before <- "ANOVA"
  } else {
    test_result_before <- kruskal.test(perc_change ~ period, data = subset_before)
    test_type_before <- "Kruskal-Wallis"
  }
  
  if (!is.na(shapiro_p_after) && shapiro_p_after > 0.05) {
    test_result_after <- summary(aov(perc_change ~ period, data = subset_after))
    test_type_after <- "ANOVA"
  } else {
    test_result_after <- kruskal.test(perc_change ~ period, data = subset_after)
    test_type_after <- "Kruskal-Wallis"
  }
  
  test_results[[com]] <- list(
    normality_p_before = shapiro_p_before,
    normality_p_after = shapiro_p_after,
    test_used_before = test_type_before,
    test_used_after = test_type_after,
    test_result_before = test_result_before,
    test_result_after = test_result_after
  )
}

# Compare statistical differences before and after → Were price changes significantly different?
print(test_results)

#--------------------- 5. Report all results --------------------------- 

library(xtable)
library(stargazer)
library(Hmisc)  # For correlation matrix formatting

setwd(paste0(wd_main,'/graphs/tables'))

# 5.1 Trend anlysis
sink(paste0("trend_analysis_",currencyi,method,".tex"))
print(xtable(trend_df, 
             caption = "Trend Analysis Before and After War", 
             label = "tab:trend_analysis"), 
      include.rownames = FALSE, 
      type = "latex")
sink()


# 5.2 Seasonality analysis table
sink(paste0("seasonality_analysis_",currencyi,method,".tex"))
print(xtable(seasonality_df, 
             caption = "Seasonality Strength Before and After War", 
             label = "tab:seasonality_analysis"), 
      include.rownames = FALSE, 
      type = "latex")
sink()


# 5.3 Correlation matrix before war
sink(paste0("correlation_before_",currencyi,method,".tex"))
print(xtable(cor_matrix_before, 
             caption = "Correlation Matrix Before War", 
             label = "tab:cor_before"), 
      type = "latex")
sink()

# Save correlation matrix after war
sink(paste0("correlation_after_",currencyi,method,".tex"))
print(xtable(cor_matrix_after, 
             caption = "Correlation Matrix After War", 
             label = "tab:cor_after"), 
      type = "latex")
sink()


# 5.4 # Save summary statistics table
sink(paste0("summary_analysis_",currencyi,method,".tex"))
stargazer(trend_df, seasonality_df, type = "latex",
          title = "Summary of Trend and Seasonality Analysis",
          summary = FALSE, label = "tab:summary_analysis")
sink()
