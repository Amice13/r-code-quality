##########################################
# Code to perform univariate SPM with Shewhart
# control charts on each monitoring variable
# of LVMWD UF data (long-term case study)
# Taylor Grimm
# October 18th, 2023
##########################################

# Load in packages
library(qcc)
library(tidyverse)
library(zoo)
library(lubridate)

# 2 day window = 4*24*2 = 192 observations
# 12 day window = 4*24*12 = 1152 observations

temp_perm_xbar1 <- qcc(data_response[1:1152, 'temp_perm'], type = 'xbar.one',
                       newdata = data_response[1153:1250, 'temp_perm'], confidence.level = 0.995)
# default uses nsigmas 3 standard deviations
# which is a confidence level of 0.9973002 (+/- 3sd of N(0,1))

train_stats <- temp_perm_xbar1$statistics # training period
test_stats <- temp_perm_xbar1$newstats # testing period
low_lim <- temp_perm_xbar1$limits[1] # lower control limit
up_lim <- temp_perm_xbar1$limits[2] # upper control limit
# qnorm(c(.0025, .9975), mean = 5.334889, sd = .1926961) # both control limits directly, where
# the mean and sd are the center and sd from qcc()

temp_perm_exceedances <- ((test_stats > up_lim) |
                      (test_stats < low_lim)) |> as.numeric()

plot(c(train_stats, test_stats), ylim = c(low_lim - 1, up_lim + 1), type = 'l')
abline(h = low_lim, col = 'red', lty = 2)
abline(h = up_lim, col = 'red', lty = 2)

# Write a function that does adaptive shewhart xbar charts for each monitoring variable
# allow for varying number of consecutive exceedances before an alarm
# retrain for every 1 day of IC observations, throwing out OC observations (same as AD-PCA approach)

# for turbidity and ammonia, only do an upper-tailed control chart
# for temp corrected permeability, do upper and lower tailed


get_flags <- function(data,
                      variable = 'temp_perm',
                      train_rows,
                      test_rows,
                      alpha = 0.005,
                      update = 4*24*1,
                      alarm_num = 1) {
  
  if(variable == 'temp_perm') {
    temp_perm_xbar1 <- qcc(data[train_rows, 'temp_perm'], type = 'xbar.one',
                           newdata = data[test_rows, 'temp_perm'],
                           confidence.level = 1 - alpha, plot = FALSE)
    train_stats_temp_perm <- temp_perm_xbar1$statistics # training period
    test_stats_temp_perm <- temp_perm_xbar1$newstats # testing period
    low_lim_temp_perm <- temp_perm_xbar1$limits[1] # lower control limit
    up_lim_temp_perm <- temp_perm_xbar1$limits[2] # upper control limit
    
    temp_perm_exceedances <- ((test_stats_temp_perm > up_lim_temp_perm) |
                                (test_stats_temp_perm < low_lim_temp_perm)) |> as.numeric()
    tibble(temp_perm_flag = temp_perm_exceedances,
           temp_perm_low_lim = low_lim_temp_perm,
           temp_perm_up_lim = up_lim_temp_perm) |>
      xts::xts(order.by = index(data[test_rows, ]))
  } else if(variable == 'turbidity') {
    # for turbidity and ammonia, only do an upper-tailed control chart
    # to do this, we'll adjust the confidence level by multiplying alpha by 2
    turbidity_xbar1 <- qcc(data[train_rows, 'turbidity'], type = 'xbar.one',
                           newdata = data[test_rows, 'turbidity'],
                           confidence.level = 1 - alpha * 2, plot = FALSE)
    train_stats_turbidity <- turbidity_xbar1$statistics
    test_stats_turbidity <- turbidity_xbar1$newstats
    up_lim_turbidity <- turbidity_xbar1$limits[2]
    
    turbidity_exceedances <- (test_stats_turbidity > up_lim_turbidity)|> as.numeric()
    
    tibble(turbidity_flag = turbidity_exceedances,
           turbidity_up_lim = up_lim_turbidity) |>
      xts::xts(order.by = index(data[test_rows, ]))
  } else if(variable == 'ammonia'){
    ammonia_xbar1 <- qcc(data[train_rows, 'ammonia'], type = 'xbar.one',
                         newdata = data[test_rows, 'ammonia'],
                         confidence.level = 1 - alpha * 2, plot = FALSE)
    train_stats_ammonia <- ammonia_xbar1$statistics
    test_stats_ammonia <- ammonia_xbar1$newstats
    up_lim_ammonia <- ammonia_xbar1$limits[2]
    
    ammonia_exceedances <- (test_stats_ammonia > up_lim_ammonia) |> as.numeric()
    tibble(ammonia_flag = ammonia_exceedances,
           ammonia_up_lim = up_lim_ammonia) |>
      xts::xts(order.by = index(data[test_rows, ]))
  }
}

shewhart_lvmwd <- function(data,
                           train_size = 4*24*2, # 2 days, 4*24*12 = 1152 for 12 days
                           update = 4*24, # update after 1 day of IC observations
                           alpha = .005,
                           alarm_num = 1) {
  
  train_rows <- 1:train_size
  test_rows <- (train_size + 1):nrow(data)
  
  train_data <- data[train_rows, ]  
  test_data <- data[test_rows, ]
  
  shewhart_results_temp_perm <- get_flags(data = data,
                                          variable = 'temp_perm',
                                          train_rows = train_rows,
                                          test_rows = test_rows,
                                          alpha = alpha,
                                          update = update,
                                          alarm_num = alarm_num)
  
  shewhart_results_turbidity <- get_flags(data = data,
                                          variable = 'turbidity',
                                          train_rows = train_rows,
                                          test_rows = test_rows,
                                          alpha = alpha,
                                          update = update,
                                          alarm_num = alarm_num)
  
  shewhart_results_ammonia <- get_flags(data = data,
                                        variable = 'ammonia',
                                        train_rows = train_rows,
                                        test_rows = test_rows,
                                        alpha = alpha,
                                        update = update,
                                        alarm_num = alarm_num)
  
  shewhart_results_temp_perm$Alarm <- shewhart_results_temp_perm[, "temp_perm_flag"] |>
    rollmean(k = alarm_num, align = 'right', fill = 0)
  shewhart_results_temp_perm$Alarm <- as.numeric(shewhart_results_temp_perm$Alarm == 1)
  
  shewhart_results_turbidity$Alarm <- shewhart_results_turbidity[, "turbidity_flag"] |>
    rollmean(k = alarm_num, align = 'right', fill = 0)
  shewhart_results_turbidity$Alarm <- as.numeric(shewhart_results_turbidity$Alarm == 1)
  
  shewhart_results_ammonia$Alarm <- shewhart_results_ammonia[, "ammonia_flag"] |>
    rollmean(k = alarm_num, align = 'right', fill = 0)
  shewhart_results_ammonia$Alarm <- as.numeric(shewhart_results_ammonia$Alarm == 1)
  
  non_alarmed_obs_temp_perm <- shewhart_results_temp_perm[shewhart_results_temp_perm[, "Alarm"] == 0, ]
  non_alarmed_obs_turbidity <- shewhart_results_turbidity[shewhart_results_turbidity[, "Alarm"] == 0, ]
  non_alarmed_obs_ammonia <- shewhart_results_ammonia[shewhart_results_ammonia[, "Alarm"] == 0, ]
  
  kept_obs_index_temp_perm <- head(index(non_alarmed_obs_temp_perm), n = update)
  kept_obs_index_turbidity <- head(index(non_alarmed_obs_turbidity), n = update)
  kept_obs_index_ammonia <- head(index(non_alarmed_obs_ammonia), n = update)
  
  kept_obs_temp_perm <- test_data[kept_obs_index_temp_perm]
  kept_obs_turbidity <- test_data[kept_obs_index_turbidity]
  kept_obs_ammonia <- test_data[kept_obs_index_ammonia]
  
  ## keep track of when we retrain and which observations are used
  obs_to_tr_temp_perm <- which(index(data) %in% index(kept_obs_temp_perm))
  obs_to_tr_new_temp_perm <- obs_to_tr_temp_perm
  obs_to_tr_turbidity <- which(index(data) %in% index(kept_obs_turbidity))
  obs_to_tr_new_turbidity <- obs_to_tr_turbidity
  obs_to_tr_ammonia <- which(index(data) %in% index(kept_obs_ammonia))
  obs_to_tr_new_ammonia <- obs_to_tr_ammonia
  
  retrain_obs <- min(test_rows)
  
  retrain_loop_func <- function(variable = 'temp_perm') {
    if(variable == 'temp_perm'){
      obs_to_tr_new <- obs_to_tr_new_temp_perm
      shewhart_results <- shewhart_results_temp_perm
    } else if(variable == 'turbidity'){
      obs_to_tr_new <- obs_to_tr_new_turbidity
      shewhart_results <- shewhart_results_turbidity
    } else if(variable == 'ammonia'){
      obs_to_tr_new <- obs_to_tr_new_ammonia
      shewhart_results <- shewhart_results_ammonia
    }
    
    while (length(obs_to_tr_new) == update) {
      nn <- length(obs_to_tr_new)
      ## update training and test window
      if (nn < train_size) {
        train_rows <- c(tail(train_rows, n = (train_size - nn)),
                        obs_to_tr_new)
      } else {
        train_rows <- head(obs_to_tr[(nn - train_size + 1):nn], 
                           n = train_size)
      }
      test_rows <- (max(train_rows)+1):nrow(data)
      retrain_obs <- c(retrain_obs, min(test_rows))
      if (max(train_rows) == nrow(data)) 
        break
      
      train_data <- data[train_rows, ]  
      test_data <- data[test_rows, ]
      
      shewhart_results_new <- get_flags(data = data,
                                    variable = variable,
                                    train_rows = train_rows,
                                    test_rows = test_rows,
                                    alpha = alpha,
                                    update = update,
                                    alarm_num = alarm_num)
      
      shewhart_results_new$Alarm <- shewhart_results_new[, 1] |>
        rollmean(k = alarm_num, align = 'right', fill = 0)
      shewhart_results_new$Alarm <- as.numeric(shewhart_results_new$Alarm == 1)
      
      non_alarmed_obs_new <- shewhart_results_new[shewhart_results_new[, "Alarm"] == 0, ]
      
      kept_obs_index_new <- head(index(non_alarmed_obs_new), n = update)
      kept_obs_new <- test_data[kept_obs_index_new]
      
      shewhart_results[index(shewhart_results_new), ] <- shewhart_results_new
      obs_to_tr_new <- which(index(data) %in% index(kept_obs_new))
      if (length(obs_to_tr_new) != 0) {
        obs_to_tr <- c(obs_to_tr, obs_to_tr_new)
      }
    } 
    list(shewhart_results = shewhart_results,
         retrain_obs = retrain_obs)
  }
 
  results_temp_perm <- retrain_loop_func(variable = 'temp_perm')
  temp_perm_shewhart <- results_temp_perm$shewhart_results
  colnames(temp_perm_shewhart)[4] <- "Alarm_temp_perm"
  temp_perm_shewhart$temp_perm_val <- data[-c(1:train_size), 'temp_perm']
  
  results_turbidity <- retrain_loop_func(variable = 'turbidity')
  turbidity_shewhart <- results_turbidity$shewhart_results
  colnames(turbidity_shewhart)[3] <- "Alarm_turbidity"
  turbidity_shewhart$turbidity_val <- data[-c(1:train_size), 'turbidity']
  
  results_ammonia <- retrain_loop_func(variable = 'ammonia')
  ammonia_shewhart <- results_ammonia$shewhart_results
  colnames(ammonia_shewhart)[3] <- "Alarm_ammonia"
  ammonia_shewhart$ammonia_val <- data[-c(1:train_size), 'ammonia']
  
  all_shewhart <- cbind(temp_perm_shewhart, turbidity_shewhart, ammonia_shewhart)
  list(shewhart_results = all_shewhart,
       retrain_temp_perm = results_temp_perm$retrain_obs,
       retrain_turbidity = results_turbidity$retrain_obs,
       retrain_ammonia = results_ammonia$retrain_obs)
}

univariate_fault_detect <- shewhart_lvmwd(data = data_response,
                                          train_size = 4*24*12,
                                          alarm_num = 5)  

shewhart_results <- univariate_fault_detect$shewhart_results

time(shewhart_results$Alarm_temp_perm[shewhart_results$Alarm_temp_perm == 1])
time(shewhart_results$Alarm_turbidity[shewhart_results$Alarm_turbidity == 1])
time(shewhart_results$Alarm_ammonia[shewhart_results$Alarm_ammonia == 1])

univariate_fault_detect$retrain_temp_perm
univariate_fault_detect$retrain_turbidity
univariate_fault_detect$retrain_ammonia



second_training_times <- time(shewhart_results$temp_perm_val)[(time(shewhart_results$temp_perm_val) >= as_date('2022-04-27')) &
                                                            (time(shewhart_results$temp_perm_val) <= as_date('2022-05-11'))]
known_fault_times <- time(shewhart_results$temp_perm_val)[(time(shewhart_results$temp_perm_val) >= as_date('2022-05-12')) &
                                                        (time(shewhart_results$temp_perm_val) <= as_date('2022-05-25'))]

par(mar = c(3.5, 4, 2, 1.75))

plot(time(shewhart_results$temp_perm_val), shewhart_results$temp_perm_val, type = 'l', ylim = c(-2, 10),
     ylab = 'gfd/psi at 20 C', xlab = '', xaxt = 'n',
     main = 'Shewhart Control Chart for Temperature-Corrected Permeability')
lines(second_training_times, shewhart_results$temp_perm_val[second_training_times],
      col = 'steelblue')
lines(known_fault_times, shewhart_results$temp_perm_val[known_fault_times],
      col = 'goldenrod')
lines(time(shewhart_results$temp_perm_low_lim), shewhart_results$temp_perm_low_lim, lty = 1, col = 'red')
lines(time(shewhart_results$temp_perm_up_lim), shewhart_results$temp_perm_up_lim, lty = 1, col = 'red')
axis(1,
     seq(min(time(shewhart_results$temp_perm_val)),
         max(time(shewhart_results$temp_perm_val)), length = 5),
     format(seq(min(time(shewhart_results$temp_perm_val)),
                max(time(shewhart_results$temp_perm_val)), length = 5), "%b %Y"))
legend('bottomleft', bty = 'n', col = c('black', 'steelblue', 'goldenrod', 'red'), lty = c(1,1,1,1),
       legend = c("Testing Observations", "Known In-Control", "Known Fault", "Threshold"),
       lwd = c(3, 3, 3))

plot(time(shewhart_results$turbidity_val), shewhart_results$turbidity_val, type = 'l', ylim = c(-40, 100),
     ylab = 'mNTU', xlab = '', xaxt = 'n',
     main = 'Shewhart Control Chart for Filtrate Turbidity')
lines(second_training_times, shewhart_results$turbidity_val[second_training_times],
      col = 'steelblue')
lines(known_fault_times, shewhart_results$turbidity_val[known_fault_times],
      col = 'goldenrod')
lines(time(shewhart_results$turbidity_up_lim), shewhart_results$turbidity_up_lim, lty = 1, col = 'red')
axis(1,
     seq(min(time(shewhart_results$turbidity_val)),
         max(time(shewhart_results$turbidity_val)), length = 5),
     format(seq(min(time(shewhart_results$turbidity_val)),
                max(time(shewhart_results$turbidity_val)), length = 5), "%b %Y"))
legend('bottomleft', bty = 'n', col = c('black', 'steelblue', 'goldenrod', 'red'), lty = c(1,1,1,1),
       legend = c("Testing Observations", "Known In-Control", "Known Fault", "Threshold"),
       lwd = c(3, 3, 3))


plot(time(shewhart_results$ammonia_val), shewhart_results$ammonia_val, type = 'l', ylim = c(-2, 5),
     ylab = 'mg/L N', xlab = '', xaxt = 'n',
     main = 'Shewhart Control Chart for Filtrate Ammonia')
lines(second_training_times, shewhart_results$ammonia_val[second_training_times],
      col = 'steelblue')
lines(known_fault_times, shewhart_results$ammonia_val[known_fault_times],
      col = 'goldenrod')
lines(time(shewhart_results$ammonia_up_lim), shewhart_results$ammonia_up_lim, lty = 1, col = 'red')
axis(1,
     seq(min(time(shewhart_results$ammonia_val)),
         max(time(shewhart_results$ammonia_val)), length = 5),
     format(seq(min(time(shewhart_results$ammonia_val)),
                max(time(shewhart_results$ammonia_val)), length = 5), "%b %Y"))
legend('bottomleft', bty = 'n', col = c('black', 'steelblue', 'goldenrod', 'red'), lty = c(1,1,1,1),
       legend = c("Testing Observations", "Known In-Control", "Known Fault", "Threshold"),
       lwd = c(3, 3, 3))

par(mar = c(5, 4, 4, 2) + 0.1)


#################################################################################
# Make tables for # and % of OC obs and retrainings
#################################################################################

#################################################################################
# Number and proportion of OC obs per period for each shewhart chart
second_training_indices_temp_perm <- which((time(shewhart_results$temp_perm_val) >= as_date('2022-04-27')) &
                                           (time(shewhart_results$temp_perm_val) < as_date('2022-05-12')))
known_fault_indices_temp_perm <- which((time(shewhart_results$temp_perm_val) >= as_date('2022-05-12')) &
                                       (time(shewhart_results$temp_perm_val) <= as_date('2022-05-25')))
num_black_temp_perm <- sum((univariate_fault_detect$retrain_temp_perm - 1152) < min(second_training_indices)) # number of retrainings during black
num_gray_temp_perm <- sum((univariate_fault_detect$retrain_temp_perm - 1152) >= min(second_training_indices) &
                          (univariate_fault_detect$retrain_temp_perm - 1152) < min(known_fault_indices)) # number of retrainings during gray
num_known_temp_perm <- sum((univariate_fault_detect$retrain_temp_perm - 1152) >= min(known_fault_indices))

second_training_indices_turbidity <- which((time(shewhart_results$turbidity_val) >= as_date('2022-04-27')) &
                                           (time(shewhart_results$turbidity_val) < as_date('2022-05-12')))
known_fault_indices_turbidity <- which((time(shewhart_results$turbidity_val) >= as_date('2022-05-12')) &
                                       (time(shewhart_results$turbidity_val) <= as_date('2022-05-25')))
num_black_turbidity <- sum((univariate_fault_detect$retrain_turbidity - 1152) < min(second_training_indices)) # number of retrainings during black
num_gray_turbidity <- sum((univariate_fault_detect$retrain_turbidity - 1152) >= min(second_training_indices) &
                          (univariate_fault_detect$retrain_turbidity - 1152) < min(known_fault_indices)) # number of retrainings during gray
num_known_turbidity <- sum((univariate_fault_detect$retrain_turbidity - 1152) >= min(known_fault_indices))


second_training_indices_ammonia <- which((time(shewhart_results$ammonia_val) >= as_date('2022-04-27')) &
                                   (time(shewhart_results$ammonia_val) < as_date('2022-05-12')))
known_fault_indices_ammonia <- which((time(shewhart_results$ammonia_val) >= as_date('2022-05-12')) &
                               (time(shewhart_results$ammonia_val) <= as_date('2022-05-25')))
num_black_ammonia <- sum((univariate_fault_detect$retrain_ammonia - 1152) < min(second_training_indices)) # number of retrainings during black
num_gray_ammonia <- sum((univariate_fault_detect$retrain_ammonia - 1152) >= min(second_training_indices) &
                  (univariate_fault_detect$retrain_ammonia - 1152) < min(known_fault_indices)) # number of retrainings during gray
num_known_ammonia <- sum((univariate_fault_detect$retrain_ammonia - 1152) >= min(known_fault_indices))

# Temp Perm
tibble(`Statistic` = c("Testing", "Known IC", "Known Fault"),
       `Proportion OC` = c(shewhart_results$temp_perm_flag[-c(second_training_indices_temp_perm, known_fault_indices_temp_perm)] |> mean(),
                           shewhart_results$temp_perm_flag[second_training_indices_temp_perm] |> mean(),
                           shewhart_results$temp_perm_flag[known_fault_indices_temp_perm] |> mean()),
       `Number OC` = c(shewhart_results$temp_perm_flag[-c(second_training_indices_temp_perm, known_fault_indices_temp_perm)] |> sum(),
                       shewhart_results$temp_perm_flag[second_training_indices_temp_perm] |> sum(),
                       shewhart_results$temp_perm_flag[known_fault_indices_temp_perm] |> sum()))

# Turbidity
tibble(`Statistic` = c("Testing", "Known IC", "Known Fault"),
       `Proportion OC` = c(shewhart_results$turbidity_flag[-c(second_training_indices_turbidity, known_fault_indices_turbidity)] |> mean(),
                           shewhart_results$turbidity_flag[second_training_indices_turbidity] |> mean(),
                           shewhart_results$turbidity_flag[known_fault_indices_turbidity] |> mean()),
       `Number OC` = c(shewhart_results$turbidity_flag[-c(second_training_indices_turbidity, known_fault_indices_turbidity)] |> sum(),
                       shewhart_results$turbidity_flag[second_training_indices_turbidity] |> sum(),
                       shewhart_results$turbidity_flag[known_fault_indices_turbidity] |> sum()))

# Ammonia
tibble(`Statistic` = c("Testing", "Known IC", "Known Fault"),
       `Proportion OC` = c(shewhart_results$ammonia_flag[-c(second_training_indices_ammonia, known_fault_indices_ammonia)] |> mean(),
                           shewhart_results$ammonia_flag[second_training_indices_ammonia] |> mean(),
                           shewhart_results$ammonia_flag[known_fault_indices_ammonia] |> mean()),
       `Number OC` = c(shewhart_results$ammonia_flag[-c(second_training_indices_ammonia, known_fault_indices_ammonia)] |> sum(),
                       shewhart_results$ammonia_flag[second_training_indices_ammonia] |> sum(),
                       shewhart_results$ammonia_flag[known_fault_indices_ammonia] |> sum()))


#################################################################################
# Retrainings (proportion and count) for each shewhart chart
black_times <- time(shewhart_results$temp_perm_val)[-c(second_training_indices, known_fault_indices)]
gray_times <- time(shewhart_results$temp_perm_val)[second_training_indices]
known_fault_times <- time(shewhart_results$temp_perm_val)[known_fault_indices]

num_pos_black <- floor(length(black_times) / (4*24*1))
num_pos_gray <- floor(length(gray_times) / (4*24*1))
num_pos_known <- floor(length(known_fault_times) / (4*24*1))

# Temp Perm
tibble(period = c('testing', 'known IC', 'known fault'),
       prop_retrain = c(num_black_temp_perm / num_pos_black,
                        num_gray_temp_perm / num_pos_gray,
                        num_known_temp_perm / num_pos_known),
       days_possible = c(num_pos_black, num_pos_gray, num_pos_known),
       num_retrained = c(num_black_temp_perm, num_gray_temp_perm, num_known_temp_perm))

# Turbidity
tibble(period = c('testing', 'known IC', 'known fault'),
       prop_retrain = c(num_black_turbidity / num_pos_black,
                        num_gray_turbidity / num_pos_gray,
                        num_known_turbidity / num_pos_known),
       days_possible = c(num_pos_black, num_pos_gray, num_pos_known),
       num_retrained = c(num_black_turbidity, num_gray_turbidity, num_known_turbidity))

# Ammonia
tibble(period = c('testing', 'known IC', 'known fault'),
       prop_retrain = c(num_black_ammonia / num_pos_black,
                        num_gray_ammonia / num_pos_gray,
                        num_known_ammonia / num_pos_known),
       days_possible = c(num_pos_black, num_pos_gray, num_pos_known),
       num_retrained = c(num_black_ammonia, num_gray_ammonia, num_known_ammonia))
