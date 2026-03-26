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
method = 'price'

price_data <- readRDS('price_data.RDS')

# Definition for visualisation
theme_default <- 
  theme(rect = element_rect(fill = 'white'), 
        plot.background = element_rect(fill = alpha('#ffece1', 0.5)), 
        panel.background = element_rect(fill = 'white'),
        strip.background = element_rect(fill = 'white'),
        axis.title.x = element_blank(), 
        #legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.height = unit(1.5, "cm"),
        legend.background = element_blank(),
        text = element_text(size = 16),
        panel.grid = element_blank())

#--------------------- 0. Define Data --------------------------- 

war_date <- as.Date('2023-10-01')

seed = 4

l = 1 # lags to choose

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

before_war <- gaza_data %>% filter(date < war_date)
war_price <- gaza_data %>% filter(date >= war_date)
war_length <- length(unique(war_price$date))

# Convert data into a list of time series objects
ts_all <- gaza_data %>%
  arrange(date) %>%
  group_by(commodity) %>%
  summarise(price_ts = list(ts(price, start = c(2007, 1), frequency = 12))) %>%
  pull(price_ts)  # Extract list of time series objects

# Function to convert to time series for each period
convert_to_ts <- function(data) {
  data %>%
    arrange(date) %>%
    group_by(commodity) %>%
    summarise(price_ts = list(ts(price, start = c(year(min(date)), month(min(date))), frequency = 12))) %>%
    pull(price_ts)
}

# Convert price to time series for both periods
ts_before <- convert_to_ts(before_war)

# Store commodity names
commodity_names <- unique(gaza_data$commodity)

# Assign names to lists
names(ts_before) <- commodity_names
names(ts_all)  <- commodity_names

periods = c(before = "Before Oct.2023",
            all = "Jan.2007–Feb.2025")

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
trend_all  <- compute_trend(ts_all)

# Convert to data frame
trend_df <- data.frame(
  commodity = names(trend_before),
  # trend slope
  before = sapply(trend_before, function(x) x$trend_slope),
  all = sapply(trend_all, function(x) x$trend_slope)#,
  #adf_p_before = sapply(trend_before, function(x) x$adf_p_value),
  #adf_p_all = sapply(trend_all, function(x) x$adf_p_value)
)

# Compare trend_slope_before vs. trend_slope_all → Did the trend change?
# Compare adf_p_before vs. adf_p_all → Did stationarity change?
# These trends are linear — they assume constant rate of change.
# They are not adjusted for inflation, unless your price series is real (not nominal).
# Values near zero (e.g. ±0.001) suggest a flat or stable trend.
# Sign and magnitude:
# Positive trend → price is rising
# Negative trend → price is falling
# Larger magnitude → faster change
print(trend_df)

# Trend slope tidy format:
trend_long <- trend_df %>%
  pivot_longer(cols = c(all, before),
               names_to = "period",
               values_to = "trend") %>%
  mutate(across(where(is.character), as.factor))

#--------------------- 2. Analyse Seasonality --------------------------- 
# Autocorrelation Function (ACF): values close to 1 at lags l-> strong seasonality
# Seasonal decomposition using STL: Helps in detecting periodicity.

# Function to compute seasonality indicators
compute_seasonality <- function(ts_data, when, start_date = as.Date("2007-01-01")) {
  
  # Loop over each time series in the list
  results <- lapply(names(ts_data), function(com) {
    ts_series <- ts_data[[com]]
    
    # ACF (lags 1 to 12)
    acf_vals <- acf(ts_series, lag.max = 12, plot = FALSE)$acf[-1]
    
    # STL decomposition
    stl_decomp <- stl(ts_series, s.window = "periodic")
    seasonal_vec <- stl_decomp$time.series[, "seasonal"]
    
    # Dates for seasonal component
    dates <- seq(start_date, by = "month", length.out = length(seasonal_vec))
    
    # Return list of 2 data frames
    list(
      acf = tibble(
        lag = 1:12,
        commodity = com,
        !!paste0(when) := as.numeric(acf_vals)
      ),
      seasonal = tibble(
        year = year(dates),
        month = month(dates),
        commodity = com,
        !!paste0(when) := as.numeric(seasonal_vec)
      )
    )
  })
  
  # Combine all results
  acf_values <- bind_rows(map(results, "acf"))
  seasonality_component <- bind_rows(map(results, "seasonal"))
  
  return(list(
    acf_values = acf_values,
    seasonality_component = seasonality_component
  ))
}

# Compute seasonality for both periods
seasonality_before <- compute_seasonality(ts_before, when = "before")
seasonality_all  <- compute_seasonality(ts_all, when = "all")

acf_before <- seasonality_before$acf_values
acf_all  <- seasonality_all$acf_values

acf_combined <- full_join(acf_before, acf_all, by = c("commodity", "lag"))

acf_long <- acf_combined %>%
  pivot_longer(cols = c(before, all),
               names_to = "period",
               values_to = "acf") %>%
  mutate(across(where(is.character), as.factor))

#--------------------- 3. Correlation Between Commodities --------------------------- 

# Convert data to wide format for correlation analysis
wide_before <- before_war %>%
  dplyr::select(date, commodity, perc_change) %>%
  drop_na(perc_change) %>%
  spread(commodity, perc_change)

wide_all <- gaza_data %>%
  dplyr::select(date, commodity, perc_change) %>%
  drop_na(perc_change) %>%
  spread(commodity, perc_change)

# Compute correlation matrices
cor_matrix_before <- cor(wide_before[,-1], use = "pairwise.complete.obs")
cor_matrix_all  <- cor(wide_all[,-1], use = "pairwise.complete.obs")

# Compare correlations before and after → Did relationships between commodities change?
print("Correlation Before:")
print(cor_matrix_before)

print("Correlation All:")
print(cor_matrix_all)

cor_combined <- rbind(as.data.frame(cor_matrix_before) %>%
                        mutate(period = 'before') %>%
                        rownames_to_column(), 
                      as.data.frame(cor_matrix_all) %>%
                        mutate(period = 'all') %>%
                        rownames_to_column()
                      )

cor_long <- cor_combined %>%
  pivot_longer(cols = !c(rowname, period),
               names_to = "commodity",
               values_to = "cor") %>%
  mutate(across(where(is.character), as.factor)) %>%
  rename(commodity_cor = rowname)


#--------------------- 5. Plot all results ---------------------------

analyse_df <- acf_long %>%
  full_join(cor_long, by = c("commodity", "period"),
            relationship = "many-to-many") %>%
  full_join(trend_long, by = c("commodity", "period"),
            relationship = "many-to-many") %>%
  mutate(period = case_when(period == 'before' ~ periods['before'],
                            .default = periods['all']))

library(ggside)

# Plot seasonal trends by commodity
# Autocorrelation heatmaps
# See which commodities had strong seasonal/persistent structure and if that changed

seasonality_viz <- analyse_df %>% 
  ggplot(aes(y = factor(lag), x = commodity, fill = acf)) +
  geom_tile(color = "white") +
  facet_wrap(~ period) +
  viridis::scale_fill_viridis(
    option = "F",
    direction = -1,
    breaks = c(0, 0.5, 1)   # only show these values
  ) +
  labs(y = "Lag (months)") +
  theme_default

seasonality_viz

cor_viz <- analyse_df %>% 
  ggplot(aes(y = commodity_cor, x = commodity, fill = cor)) +
  geom_tile(color = "white") +
  facet_wrap(~ period) +
  viridis::scale_fill_viridis(
    option = "F",
    direction = -1
  ) +
  #labs(x = "Lag (months)") +
  theme_default

cor_viz

combined_viz <- analyse_df %>%
  ggplot(aes(x = commodity, y = commodity_cor, fill = cor)) +
  geom_tile(color = "white") +  # ACF heatmap in main panel
  # Add annual trend labels on the diagonal
  geom_text(
    data = analyse_df %>% filter(commodity==commodity_cor & lag == 1),
    aes(label = round(trend*12,digits=2)),
    color = "white",
    size = 5
  ) +
  # Correlation heatmap on the x-side (bottom panel)
  geom_xsidetile(
    aes(y = factor(lag), fill = acf)
  ) +
  facet_wrap(~ period) +
  viridis::scale_fill_viridis(option = "F", direction = -1) +
  theme_default +
  labs(
    y = "Commodity (bottom) / Lag (top)",
    fill = "Value"#,
    #title = "Lag Autocorrelations and Inter-Commodity Correlations"
  ) +
  theme(
    ggside.panel.scale = 0.7,          # size of bottom panel
    ggside.axis.text.y.left = element_text(angle = 0),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold")
  )

combined_viz

setwd(paste0(wd_main,'/graphs'))

ggsave(combined_viz,
       filename = paste(method,currencyi,'analyse_viz.pdf',sep = '_'),
       width = 297, height = 210, units = 'mm')
