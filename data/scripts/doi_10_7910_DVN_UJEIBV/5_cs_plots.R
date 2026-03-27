#############################################################################################
# Code to create and save all plots associated with the case study of the ultrafiltration 
# water treatment data.
# Taylor Grimm
# August 16, 2024
#############################################################################################

# load in necessary packages
library(MASS)
library(tidyverse)
library(lubridate)
library(magrittr)
library(glmnet)
library(xts)

# load in case study data
load('cleaned_uf_ops_data.RData')

uf1_ops_15min <- uf1_with_status_15min

source('7_cs_helper_functions.R')

# additional helper function to obtain daily observations from the original 15min dataset
long_term_data <- function(min_date = '2021-11-01', max_date = '2021-12-21', UF = 1, detrend = 'adaptive_lasso') {
  get_detrend_args <- function(UF = 1, min_date = '2021-04-02', max_date = '2021-04-13') {
    
    if(UF == 1) {
      uf_with_status_15min <- uf1_with_status_15min |> filter(Status == 4)
      explanatory_vars <- c("LasVirgenes/UF/AI_31090/Val", # pH
                            "LasVirgenes/UF/AI_31093/Val", # ORP
                            "LasVirgenes/UF/AI_31094/Val", # total chlorine
                            "LasVirgenes/UF/UF_01_Flux/Val", # flux
                            "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                            "LasVirgenes/UF/FI_36074/Val", # backwash flow
                            "LasVirgenes/UF/TI_14078/Val", # feed temperature
                            "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity // UF Filtrate conductivity
                            "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC // UF filtrate TOC
      response_vars <- c("LasVirgenes/UF/UF_01_TCPermeability/Val", # temperature-corrected permeability
                         "LasVirgenes/UF/AI_31009_1/Val", # filtrate turbidity
                         "LasVirgenes/UF/AI_36210/Val") # filtrate ammonia
    } else {
      print("Invalid 'UF' argument.")
    }
    tstamp_indices <- which(uf_with_status_15min$t_stamp >= as_date(min_date) &
                              uf_with_status_15min$t_stamp <= as_date(max_date))
    
    uf_explanatory <- uf_with_status_15min |>
      select(all_of(explanatory_vars)) |>
      xts(order.by = uf_with_status_15min$t_stamp)
    uf_response <- uf_with_status_15min |>
      select(all_of(response_vars)) |> 
      xts(order.by = uf_with_status_15min$t_stamp)
    
    list(explanatory = uf_explanatory,
         response = uf_response,
         indices = tstamp_indices)
  }
  
  detrended_lvmwd <- function(UF = 1,
                              min_date = '2022-04-27',
                              max_date = '2022-05-11',
                              detrend = 'adaptive_lasso',
                              tuning = 'manual',
                              detrend_args = list) {
    
    uf_detrend_args <- get_detrend_args(UF = UF,
                                        min_date = min_date,
                                        max_date = max_date)
    
    if (UF == 1) {
      uf_ops_15min <- uf1_with_status_15min |> filter(Status == 4)
    } else {
      print("Invalid 'UF' argument.")
    }
    
    detrend_data_lvmwd(data_response = uf_detrend_args$response,
                       data_predictor = uf_detrend_args$explanatory,
                       train_index = uf_detrend_args$indices,
                       t_stamps = uf_ops_15min$t_stamp,
                       tuning = tuning,
                       detrend = detrend,
                       detrend_args = detrend_args,
                       UF = UF)
  }
  
  uf_detrended <- detrended_lvmwd(UF = UF, min_date = min_date, max_date = max_date, detrend = detrend)
  min_date <- as_datetime(min_date, tz = 'America/Los_Angeles')
  max_date <- as_datetime(max_date, tz = 'America/Los_Angeles')
  uf_resids <- uf_detrended$detrended_data
  t_stamps <- uf_detrended$data_response[,1] |>
    names() |>
    as_datetime(tz = 'America/Los_Angeles')
  
  train_t_stamps <- t_stamps[t_stamps >= min_date & t_stamps <= max_date]
  test_t_stamps <- t_stamps[t_stamps > max_date]
  
  train_detrended <- uf_resids[train_t_stamps]
  test_detrended <- uf_resids[test_t_stamps]
  
  train_original <- xts(uf_detrended$data_response, order.by = t_stamps)[train_t_stamps]
  test_original <- xts(uf_detrended$data_response, order.by = t_stamps)[test_t_stamps]
  
  list(train_detrended = train_detrended, test_detrended = test_detrended,
       train_original = train_original, test_original = test_original)
  
}

long_term <- long_term_data(min_date = '2021-11-01', max_date = '2021-12-21')

# Set up data with time stamps for plotting. Also specify plotting ylims.
use_train <- long_term$train_original
use_test <- long_term$test_original
ylim1 <- c(2.25, 7)
ylim2 <- c(10, 70)
ylim3 <- c(0, 3)

train_tstamps <- use_train |> time()
daily_train <- use_train |>
  as_tibble() |> 
  set_colnames(c('temp_perm', 'turb', 'ammonia')) |> 
  mutate(datetime = train_tstamps) |> 
  mutate(date = floor_date(datetime, unit = 'day')) |> 
  group_by(date) |> 
  slice(1)

test_tstamps <- use_test |> time()
daily_test <- use_test |>
  as_tibble() |> 
  set_colnames(c('temp_perm', 'turb', 'ammonia')) |> 
  mutate(datetime = test_tstamps) |> 
  mutate(date = floor_date(datetime, unit = 'day')) |> 
  group_by(date) |> 
  slice(1)

daily_train <- daily_train[, 1:3] |> xts(order.by = daily_train$date)
daily_test <- daily_test[, 1:3] |> xts(order.by = daily_test$date)

draw_rectangles <- function() {
  
  rect(xleft = as.POSIXct('2022-06-13', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-07-08', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-08-27 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-08-28 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  
  # turbidity
  rect(xleft = as.POSIXct('2022-02-03', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-02-20', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-04-02 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-04-03 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-04-12 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-04-13 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-05-15', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-06-10', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-07-23 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-07-25 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-09-02 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-09-04 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  
  # ammonia
  rect(xleft = as.POSIXct('2022-01-13', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-01-24', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-03-02', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-03-05', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-09-14', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-09-30', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
}
# Create and save plots showing the observed values of temperature-corrected permeability,
# filtrate turbidity, and filtrate ammonia, respectively, during the 
# phase I (gray) and Phase II (black) periods

# pdf(file = 'original_varplots.pdf', width = 6.5, height = 4.5)
add_temp_perm_rect <- function() {
  # rect(xleft = as.POSIXct('2022-01-01 12:00:00', tz = 'America/Los_Angeles'),
  #      xright = as.POSIXct('2022-01-9', tz = 'America/Los_Angeles'),
  #      ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  # rect(xleft = as.POSIXct('2022-03-04', tz = 'America/Los_Angeles'),
  #      xright = as.POSIXct('2022-03-06 12:00:00', tz = 'America/Los_Angeles'),
  #      ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  # rect(xleft = as.POSIXct('2022-04-02', tz = 'America/Los_Angeles'),
  #      xright = as.POSIXct('2022-04-04', tz = 'America/Los_Angeles'),
  #      ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-06-13', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-07-08', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  # rect(xleft = as.POSIXct('2022-07-23', tz = 'America/Los_Angeles'),
  #      xright = as.POSIXct('2022-07-25', tz = 'America/Los_Angeles'),
  #      ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-08-27 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-08-28 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  # rect(xleft = as.POSIXct('2022-09-02', tz = 'America/Los_Angeles'),
  #      xright = as.POSIXct('2022-09-04', tz = 'America/Los_Angeles'),
  #      ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
}
pdf(file = 'temp_perm_original.pdf', width = 8, height = 2.2)
# par(mfrow = c(3, 1), mar = c(2, 4, 2, 8) + 0.1)
par(mfrow = c(1, 1), mar = c(2, 4, 3, 8) + 0.1)
plot(time(daily_train$temp_perm), as.numeric(daily_train$temp_perm), col = 'gray', type = 'l', ylim = ylim1,
     xlim = c(min(time(daily_train$temp_perm)), max(time(daily_test$temp_perm))),
     xaxt = 'n', ylab = 'gfd/psi at 20 C')
title(expression('Temperature-corrected Permeability'), cex.main = 0.9, line = 0.5, adj = 0.025)
axis(1,
     at = seq(min(time(daily_train$temp_perm)), max(time(daily_test$temp_perm)), length = 7),
     rep('', 7))
add_temp_perm_rect()
lines(time(daily_test), as.numeric(daily_test$temp_perm))
title('Original Phase I Data', line = 1.75)
dev.off()

add_turb_rect <- function() {
  rect(xleft = as.POSIXct('2022-02-03', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-02-20', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-04-02 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-04-03 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-04-12 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-04-13 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-05-15', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-05-27', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-05-30', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-06-10', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-06-29 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-06-30 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-07-23 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-07-25 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-09-02 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-09-04 12:00:00', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
}
pdf(file = 'turb_original.pdf', width = 8, height = 2)
# par(mfrow = c(1, 1), mar = c(2, 4, 2.5, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(2, 4, 2, 8) + 0.1)
plot(time(daily_train$turb), as.numeric(daily_train$turb), col = 'gray', type = 'l', ylim = ylim2,
     xlim = c(min(time(daily_train$turb)), max(time(daily_test$turb))),
     xaxt = 'n', ylab = 'mNTU')
title(expression('Filtrate Turbidity'), cex.main = 0.9, line = 0.5, adj = 0.025)
axis(1,
     at = seq(min(time(daily_train$temp_perm)), max(time(daily_test$temp_perm)), length = 7),
     rep('', 7))
add_turb_rect()
lines(time(daily_test), as.numeric(daily_test$turb))
legend('right', inset = -.2,
       legend = c("Phase I", "Phase II", 'Fault'),
       col = c("gray","black", rgb(70/255, 130/255, 180/255, .4)), lty = c(1, 1, 1), 
       lwd = c(1, 1, 5), xpd = TRUE, cex = 1, seg.len=1, bty = 'n')
dev.off()

add_ammonia_rect <- function() {
  rect(xleft = as.POSIXct('2022-01-13', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-01-24', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-03-02', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-03-05', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-05-17 12:00:00', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-06-09', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
  rect(xleft = as.POSIXct('2022-09-14', tz = 'America/Los_Angeles'),
       xright = as.POSIXct('2022-09-30', tz = 'America/Los_Angeles'),
       ybottom = 0, ytop = 1e5, col = rgb(70/255, 130/255, 180/255, .4), border = F)
}
pdf(file = 'ammonia_original.pdf', width = 8, height = 2)
# par(mfrow = c(1, 1), mar = c(2, 4, 2.5, 1) + 0.1)
par(mfrow = c(1, 1), mar = c(2, 4, 2, 8) + 0.1)
plot(time(daily_train$ammonia), as.numeric(daily_train$ammonia), col = 'gray', type = 'l', ylim = ylim3,
     xlim = c(min(time(daily_train$ammonia)), max(time(daily_test$ammonia))),
     xaxt = 'n', ylab = 'mg/L N')
title(expression('Filtrate Ammonia'), cex.main = 0.9, line = 0.5, adj = 0.025)
axis(1,
     at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7),
     format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7), "%d %b"))
add_ammonia_rect()
lines(time(daily_test), as.numeric(daily_test$ammonia))
dev.off()

#########################################################
################### Contaminated training data

# Introduce contamination into daily_train
set.seed(24)
num_contaminated <- 10 # number of contaminated points
contamination <- rep(c(1, 35, 1), each = num_contaminated) # size of mean shift for each variable
cont_indices <- sample(1:nrow(daily_train), num_contaminated) # which indices to contaminate
daily_train[cont_indices, 1:3] <- daily_train[cont_indices, 1:3] + rnorm(3*num_contaminated, mean = contamination,
                                                                         sd = rep(c(.25, 20, .25), each = 10))

# Create and save plots showing the contaminated Phase I data (gray with blue points for outliers)
# and original Phase II data (black)

# Temperature-Corrected Permeability
pdf(file = 'temp_perm_contaminated.pdf', width = 8, height = 2.2)
par(mfrow = c(1, 1), mar = c(2, 4, 3, 8) + 0.1)
plot(time(daily_train$temp_perm), as.numeric(daily_train$temp_perm), col = 'gray', type = 'l', ylim = ylim1,
     xlim = c(min(time(daily_train$temp_perm)), max(time(daily_test$temp_perm))),
     xaxt = 'n', ylab = 'gfd/psi at 20 C')
title(expression('Temperature-corrected Permeability'), cex.main = 0.9, line = 0.5, adj = 0.025)
points(time(daily_train$temp_perm[cont_indices]), daily_train$temp_perm[cont_indices], pch = 19, cex = .5,
       col = 'steelblue')
axis(1,
     at = seq(min(time(daily_train$temp_perm)), max(time(daily_test$temp_perm)), length = 7),
     rep('', 7))
add_temp_perm_rect()
lines(time(daily_test), as.numeric(daily_test$temp_perm))
title('Contaminated Phase I Data', line = 1.75)
dev.off()


# Turbidity
pdf(file = 'turb_contaminated.pdf', width = 8, height = 2)
par(mfrow = c(1, 1), mar = c(2, 4, 2, 8) + 0.1)
plot(time(daily_train$turb), as.numeric(daily_train$turb), col = 'gray', type = 'l', ylim = ylim2,
     xlim = c(min(time(daily_train$turb)), max(time(daily_test$turb))),
     xaxt = 'n', ylab = 'mNTU')
title(expression('Filtrate Turbidity'), cex.main = 0.9, line = 0.5, adj = 0.025)
points(time(daily_train$turb[cont_indices]), daily_train$turb[cont_indices], pch = 19, cex = .5,
       col = 'steelblue')
axis(1,
     at = seq(min(time(daily_train$turb)), max(time(daily_test$turb)), length = 7),
     rep('', 7))
add_turb_rect()
lines(time(daily_test), as.numeric(daily_test$turb))
legend('right', inset = -.3,
       legend = c("Phase I", "Phase II", 'Fault', 'Contamination'),
       col = c("gray","black", rgb(70/255, 130/255, 180/255, .4), 'steelblue'), lty = c(1, 1, 1, NA), pch = c(NA, NA, NA, 19), 
       lwd = c(1, 1, 5), xpd = TRUE, cex = 1, seg.len=1, bty = 'n')
dev.off()

# Ammonia
pdf(file = 'ammonia_contaminated.pdf', width = 8, height = 2)
par(mfrow = c(1, 1), mar = c(2, 4, 2, 8) + 0.1)
plot(time(daily_train$ammonia), as.numeric(daily_train$ammonia), col = 'gray', type = 'l', ylim = ylim3,
     xlim = c(min(time(daily_train$ammonia)), max(time(daily_test$ammonia))),
     xaxt = 'n', ylab = 'mg/L N')
title(expression('Filtrate Ammonia'), cex.main = 0.9, line = 0.5, adj = 0.025)
points(time(daily_train$ammonia[cont_indices]), daily_train$ammonia[cont_indices], pch = 19, cex = .5,
       col = 'steelblue')
axis(1,
     at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7),
     format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7), "%d %b"))
add_ammonia_rect()
lines(time(daily_test), as.numeric(daily_test$ammonia))
dev.off()


plot_all_methods <- function(train_data, test_data) {
  train_size <- nrow(train_data)
  test_size <- nrow(test_data)
  
  # T2 CHARTS
  pdf('t2_plots_faults.pdf', width = 7/2, height = 5)
  par(mfrow = c(4, 1), mar = c(2, 4, 2.5, 1) + 0.1)
  t2_classical <- hot_t2(train_data, test_data,
                         method = 'classical', threshold = 'simulation', far = 0.005)
  plot(time(daily_test), t2_classical$mon_stats, ylim = c(0, .4*max(t2_classical$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression(italic(T)^2),
       xaxt = 'n')
  abline(h = t2_classical$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), t2_classical$mon_stats, ylim = c(0, .4*max(t2_classical$mon_stats)))
  
  t2_robust <- hot_t2(train_data, test_data,
                      method = 'rmcd', threshold = 'simulation', far = 0.005)
  
  plot(time(daily_test), t2_robust$mon_stats, ylim = c(0, .35*max(t2_robust$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('R-'*italic(T)^2),
       xaxt = 'n')
  abline(h = t2_robust$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), t2_robust$mon_stats, ylim = c(0, .35*max(t2_robust$mon_stats)))
  
  sst2_fit <- sst2(as.matrix(rbind(train_data, test_data)), threshold = 'simulation')
  plot(time(daily_test), sst2_fit$mon_stats[(train_size + 1):(train_size + test_size)], ylim = c(0, 60), xaxt = 'n',
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('SS-'*italic(T)^2))
  abline(h = sst2_fit$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), sst2_fit$mon_stats[(train_size + 1):(train_size + test_size)])
  
  rssb_t2 <- robust_bayes(train_data, test_data,
                          chart = 't2', far = 0.005)
  
  plot(time(daily_test), rssb_t2$mon_stats, ylim = c(0, .25*max(rssb_t2$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('RSSB-'*italic(T)^2),
       xaxt = 'n')
  abline(h = rssb_t2$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), rssb_t2$mon_stats, ylim = c(0, .25*max(rssb_t2$mon_stats)))
  dev.off()
  
  # MEWMA CHARTS
  pdf('mewma_plots_faults.pdf', width = 7/2, height = 5)
  par(mfrow = c(4, 1), mar = c(2, 4, 2.5, 1) + 0.1)
  mewma_classical <- mewma(train_data, test_data,
                           method = 'classical', lambda = 0.1, far = 0.005)
  plot(time(daily_test), mewma_classical$mon_stats, ylim = c(0, .5*max(mewma_classical$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('MEWMA'),
       xaxt = 'n')
  abline(h = mewma_classical$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), mewma_classical$mon_stats, ylim = c(0, .5*max(mewma_classical$mon_stats)))
  
  mewma_robust <- mewma(train_data, test_data,
                        method = 'rmcd', lambda = 0.1, far = 0.005)
  
  plot(time(daily_test), mewma_robust$mon_stats, ylim = c(0, .2*max(mewma_robust$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('R-MEWMA'),
       xaxt = 'n')
  abline(h = mewma_robust$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), mewma_robust$mon_stats, ylim = c(0, .2*max(mewma_robust$mon_stats)))
  
  ssmewma_fit <- ssmewma(as.matrix(rbind(train_data, test_data)), threshold = 'simulation')
  plot(time(daily_test), ssmewma_fit$mon_stats[(train_size + 1):(train_size + test_size)], ylim = c(0, 60), xaxt = 'n',
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('SS-MEWMA'))
  abline(h = ssmewma_fit$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), ssmewma_fit$mon_stats[(train_size + 1):(train_size + test_size)])
  
  rssb_mewma <- robust_bayes(train_data, test_data,
                             chart = 'mewma', far = 0.005)
  
  plot(time(daily_test), rssb_mewma$mon_stats, ylim = c(0, .15*max(rssb_mewma$mon_stats)),
       type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('RSSB-MEWMA'),
       xaxt = 'n')
  abline(h = rssb_mewma$threshold, lty = 2, col = 'red4')
  axis(1,
       at = seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1],
       format(seq(min(time(daily_train$ammonia)), max(time(daily_test$ammonia)), length = 7)[-1], "%d %b"))
  draw_rectangles()
  lines(time(daily_test), rssb_mewma$mon_stats, ylim = c(0, .15*max(rssb_mewma$mon_stats)))
  dev.off()
}

plot_all_methods(daily_train, daily_test)

train_data <- daily_train
test_data <- daily_test

t2_classical <- hot_t2(train_data, test_data,
                       method = 'classical', threshold = 'simulation', far = 0.005)
t2_robust <- hot_t2(train_data, test_data,
                    method = 'rmcd', threshold = 'simulation', far = 0.005)
rssb_t2 <- robust_bayes(train_data, test_data,
                        chart = 't2', far = 0.005)
sst2_fit <- sst2(as.matrix(rbind(train_data, test_data)), threshold = 'simulation')
mewma_classical <- mewma(train_data, test_data,
                         method = 'classical', lambda = 0.1, far = 0.005)
mewma_robust <- mewma(train_data, test_data,
                      method = 'rmcd', lambda = 0.1, far = 0.005)
rssb_mewma <- robust_bayes(train_data, test_data,
                           chart = 'mewma', far = 0.005)
ssmewma_fit <- ssmewma(as.matrix(rbind(train_data, test_data)), threshold = 'simulation')

get_performance <- function(chart_fit) {
  
  rect_dates <- tibble(c(c(as.POSIXct('2022-06-13', tz = 'America/Los_Angeles'), as.POSIXct('2022-07-08', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-08-27 12:00:00', tz = 'America/Los_Angeles'), as.POSIXct('2022-08-28 12:00:00', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-02-03', tz = 'America/Los_Angeles'), as.POSIXct('2022-02-20', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-04-02', tz = 'America/Los_Angeles'), as.POSIXct('2022-04-03', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-04-12 12:00:00', tz = 'America/Los_Angeles'), as.POSIXct('2022-04-13 12:00:00', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-05-15', tz = 'America/Los_Angeles'), as.POSIXct('2022-06-10', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-07-23 12:00:00', tz = 'America/Los_Angeles'), as.POSIXct('2022-07-25 12:00:00', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-09-02 12:00:00', tz = 'America/Los_Angeles'), as.POSIXct('2022-09-04 12:00:00', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-01-13', tz = 'America/Los_Angeles'), as.POSIXct('2022-01-24', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-03-02', tz = 'America/Los_Angeles'), as.POSIXct('2022-03-05', tz = 'America/Los_Angeles'),
                           as.POSIXct('2022-09-14', tz = 'America/Los_Angeles'), as.POSIXct('2022-09-30', tz = 'America/Los_Angeles'))))
  
  rect_dates <- tibble(begin = rect_dates[seq(1, 21, 2),]$`c(...)`, end = rect_dates[seq(2, 22, 2),]$`c(...)`)
  
  
  test_times <- time(daily_test)
  
  in_any_range <- function(test_date) {
    in_range <- logical()
    for(i in 1:nrow(rect_dates)) {
      in_range[i] <- between(test_date, rect_dates$begin[i], rect_dates$end[i])
    }
    any(in_range)
  }
  all_fault_dates_indices <- test_times |> map_lgl(in_any_range) |> which()
  all_fault_dates <- test_times[all_fault_dates_indices]  
  
  mon_stats <- chart_fit$mon_stats
  cl <- chart_fit$threshold
  
  no_fault_indices <- (!(test_times %in% all_fault_dates)) |> which()
  no_fault_dates <- test_times[no_fault_indices]
  
  
  list(fault_dates = all_fault_dates,
       fault_detected = mon_stats[all_fault_dates_indices] > cl,
       no_fault_dates = no_fault_dates,
       false_alarm = mon_stats[no_fault_indices] > cl)
}

t2_classical_performance <- get_performance(t2_classical)
t2_robust_performance <- get_performance(t2_robust)
rssb_t2_performance <- get_performance(rssb_t2)

mewma_classical_performance <- get_performance(mewma_classical)
mewma_robust_performance <- get_performance(mewma_robust)
rssb_mewma_performance <- get_performance(rssb_mewma)

ssmewma_performance <- get_performance(ssmewma_fit)
sst2_performance <- get_performance(sst2_fit)

tibble(method = c('t2', 't2_rmcd', 'sst2', 'rssb_t2', 'mewma', 'mewma_rmcd', 'ssmewma', 'rssb_mewma'),
       detected = c(t2_classical = t2_classical_performance$fault_detected |> mean(), t2_robust = t2_robust_performance$fault_detected |> mean(),
                    sst2 = sst2_performance$fault_detected |> mean(), rssb_t2 = rssb_t2_performance$fault_detected |> mean(),
                    mewma_classical = mewma_classical_performance$fault_detected |> mean(),
                    mewma_robust = mewma_robust_performance$fault_detected |> mean(), ssmewma = ssmewma_performance$fault_detected |> mean(),
                    rssb_mewma = rssb_mewma_performance$fault_detected |> mean()),
       false_alarm = c(t2_classical = t2_classical_performance$false_alarm |> mean(), t2_robust = t2_robust_performance$false_alarm |> mean(),
                       sst2 = sst2_performance$false_alarm |> mean(), rssb_t2 = rssb_t2_performance$false_alarm |> mean(),
                       mewma_classical = mewma_classical_performance$false_alarm |> mean(),
                       mewma_robust = mewma_robust_performance$false_alarm |> mean(), ssmewma = ssmewma_performance$false_alarm |> mean(na.rm = T),
                       rssb_mewma = rssb_mewma_performance$false_alarm |> mean()))

# c(sum(rssb_t2_performance$fault_detected), length(rssb_t2_performance$fault_detected))
# c(sum(rssb_t2_performance$false_alarm), length(rssb_t2_performance$false_alarm))
