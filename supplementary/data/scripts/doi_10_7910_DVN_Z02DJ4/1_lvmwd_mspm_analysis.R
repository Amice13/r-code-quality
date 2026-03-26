##########################################
# Code to load in data and perform MSPM
# on UF data (short-term case studies)
# Taylor Grimm
# October 18th, 2023
##########################################

# Load in packages and data
library(tidyverse)
library(lubridate)
library(magrittr)
library(glmnet)
library(xts)
library(caret)
load('uf_data_clean.Rda')

# Additional variable names used in some functions
uf1_with_status_15min <- uf1_ops_15min
uf2_with_status_15min <- uf2_ops_15min
uf3_with_status_15min <- uf3_ops_15min

# Specify variables to use for each UF (1, 2 and 3):
# Explanatory: pH, ORP, total chlorine, TMP, flux, temp corrected TMP, permeability, feed turbidity,
# backwash flow, feed temperature, RO feed conductivity, RO feed TOC

explanatory_vars1 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                       "LasVirgenes/UF/AI_31093/Val", # ORP
                       "LasVirgenes/UF/AI_31094/Val", # total chlorine
                       "LasVirgenes/UF/UF_01_DrivingPressure/Val", # TMP
                       "LasVirgenes/UF/UF_01_Flux/Val", # flux
                       "LasVirgenes/UF/UF_01_TCDrivingPressure/Val", # temp corrected TMP
                       "LasVirgenes/UF/UF_01_Permeability/Val", # permeability
                       "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                       "LasVirgenes/UF/FI_36074/Val", # backwash flow
                       "LasVirgenes/UF/TI_14078/Val", # feed temperature
                       "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity
                       "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC

explanatory_vars2 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                       "LasVirgenes/UF/AI_31093/Val", # ORP
                       "LasVirgenes/UF/AI_31094/Val", # total chlorine
                       "LasVirgenes/UF/UF_02_DrivingPressure/Val", # TMP
                       "LasVirgenes/UF/UF_02_Flux/Val", # flux
                       "LasVirgenes/UF/UF_02_TCDrivingPressure/Val", # temp corrected TMP
                       "LasVirgenes/UF/UF_02_Permeability/Val", # permeability
                       "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                       "LasVirgenes/UF/FI_36074/Val", # backwash flow
                       "LasVirgenes/UF/TI_14078/Val", # feed temperature
                       "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity
                       "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC

explanatory_vars3 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                       "LasVirgenes/UF/AI_31093/Val", # ORP
                       "LasVirgenes/UF/AI_31094/Val", # total chlorine
                       "LasVirgenes/UF/UF_03_DrivingPressure/Val", # TMP
                       "LasVirgenes/UF/UF_03_Flux/Val", # flux
                       "LasVirgenes/UF/UF_03_TCDrivingPressure/Val", # temp corrected TMP
                       "LasVirgenes/UF/UF_03_Permeability/Val", # permeability
                       "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                       "LasVirgenes/UF/FI_36074/Val", # backwash flow
                       "LasVirgenes/UF/TI_14078/Val", # feed temperature
                       "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity
                       "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC

# Specify variables to monitor: temp corrected permeability, filtrate turbidity, filtrate ammonia
# For example, we have:
# LasVirgenes/UF/UF_01_TCPermeability/Val = "UF 1 Temperature Corrected Permeability"
# LasVirgenes/UF/AI_31009_1/Val = "UF 1 Filtrate Turbidity"
# LasVirgenes/UF/AI_36210/Val = "UF Filtrate Ammonia"

response_vars1 <- c("LasVirgenes/UF/UF_01_TCPermeability/Val",
                    "LasVirgenes/UF/AI_31009_1/Val",
                    "LasVirgenes/UF/AI_36210/Val")
response_vars2 <- c("LasVirgenes/UF/UF_02_TCPermeability/Val",
                    "LasVirgenes/UF/AI_31009_2/Val",
                    "LasVirgenes/UF/AI_36210/Val")
response_vars3 <- c("LasVirgenes/UF/UF_03_TCPermeability/Val",
                    "LasVirgenes/UF/AI_31009_3/Val",
                    "LasVirgenes/UF/AI_36210/Val")


# Set up functions to create plots
source('2_detrend_data_lvmwd.R')
source('3_helper_functions.R')


# For the specified min_date and max_date, create monitoring statistic plots
# on the log scale for each detrending method for each UF (1, 2, or 3)
detrend_methods <- c("knn", "xgboost", "rf", "adaptive_lasso", "none")

# Uncomment and run the code below to create and save pdf's of the plots (log-scale)
# of the T2 and SPE results for each UF and detrending method during the specified time frame.

# for(uf in 1:3) {
#   for(detrend in 1:5) {
#     #  par(mfrow=c(1,1), mar = c(2, 3, 1.75, 1))
#     pdf(paste0('uf', uf, "_", detrend_methods[detrend], '%03d.pdf'),
#         width = 6.5, height = 7.5, onefile = F) # if the plot_detrended_pca_log() function code is modified
#     # to produce separate plots for T2 and SPE, then this will
#     # allow those to be stored in separate pdf's with similar names.
#     if(detrend == 1){
#       plot_detrended_pca_log(UF = uf, detrend = 'knn',
#                              tuning = 'auto',# detrend_args = list(k = 20),
#                              min_date = '2022-04-27', max_date = '2022-05-11')
#     } else if(detrend ==2) {
#       plot_detrended_pca_log(UF = uf, detrend = 'xgboost',
#                              tuning = 'auto',#detrend_args = list(nrounds = 15),
#                              min_date = '2022-04-27', max_date = '2022-05-11')
#     } else if(detrend == 3) {
#       plot_detrended_pca_log(UF = uf, detrend = 'rf',
#                              tuning = 'auto',#detrend_args = list(ntree = 50),
#                              min_date = '2022-04-27', max_date = '2022-05-11')
#     } else if(detrend == 4) {
#       plot_detrended_pca_log(UF = uf, detrend = 'adaptive_lasso',
#                              min_date = '2022-04-27', max_date = '2022-05-11')
#     } else if(detrend == 5) {
#       plot_detrended_pca_log(UF = uf, detrend = 'none',
#                              min_date = '2022-04-27', max_date = '2022-05-11')
#     }
#     dev.off()
#   }
# }

# Example plot: UF 1 with adaptive lasso detrending, 2022 training period
plot_detrended_pca_log(UF = 1, detrend = 'adaptive_lasso',
                       min_date = '2022-04-27', max_date = '2022-05-11')


# For the specified min_date and max_date, create time series plots of original and residual
# (values each detrending method) for each UF (1, 2, or 3) and for each monitoring variable

# detrend_methods <- c("knn", "xgboost", "rf", "adaptive_lasso")
# var_list <- c("temp_perm", "turbidity", "ammonia")
# for(uf in 1:3) {
#   for(var in 1:3) {
#     if(var == 1) {
#       #ylims <- c(-1, 10) # use these ylims for the 2021 training period
#       ylims <- c(-5, 10) # use these ylims for the 2022 training period
#     } else if(var == 2) {
#       #ylims <- c(-20, 60) # use these ylims for the 2021 training period
#       ylims <- c(-10, 100) # use these ylims for the 2022 training period
#     } else if(var == 3) {
#       #ylims <- c(-1, 4) # use these ylims for the 2021 training period
#       ylims <- c(-1, 2) # use these ylims for the 2022 training period
#     }
#     for (detrend in 1:4) {
#       pdf(paste0('uf', uf, '_', var_list[var], '_', detrend_methods[detrend], '.pdf'),
#           width = 6, height = 4)
#       if(detrend == 1){
#         plot_original_and_resids(UF = uf, plot_var = var, detrend = 'knn',
#                                  tuning = 'auto',
#                                  min_date = '2022-04-27', max_date = '2022-05-11',
#                                  ylims = ylims)
#       } else if(detrend ==2) {
#         plot_original_and_resids(UF = uf, plot_var = var, detrend = 'xgboost',
#                                  tuning = 'auto',
#                                  min_date = '2022-04-27', max_date = '2022-05-11',
#                                  ylims = ylims)
#       } else if(detrend == 3){
#         plot_original_and_resids(UF = uf, plot_var = var, detrend = 'rf',
#                                  tuning = 'auto',
#                                  min_date = '2022-04-27', max_date = '2022-05-11',
#                                  ylims = ylims)
#       } else if(detrend == 4){
#         plot_original_and_resids(UF = uf, plot_var = var, detrend = 'adaptive_lasso',
#                                  min_date = '2022-04-27', max_date = '2022-05-11',
#                                  ylims = ylims)
#       }
#       dev.off()
#     }
#   }
# }

par(mfrow=c(1,1))
# Example plot: UF 1 with adaptive lasso detrending, 2022 training period
plot_original_and_resids(UF = 1, plot_var = 3, detrend = 'adaptive_lasso',
                         tuning = 'auto',
                         min_date = '2022-04-27', max_date = '2022-05-11',
                         ylims = c(-1, 2))

# Example plot of the original value of a monitoring variable ("plot_var") for UF 1
plot_original_responses(UF = 1, min_date = '2021-04-02', max_date = '2021-04-13', plot_var = 3)


##################################################################################################
####### Long-term MSPM ###########################################################################
##################################################################################################

# Run the MSPM model from the first training period through second testing period (ends may 25th 2022)

# Filter the UF 1 15min avg. data down to the desired time period
uf1_ops_use <- uf1_ops_15min |>
  filter((t_stamp >= as.Date('2021-04-02', tz = 'America/Los_Angeles')) &
           (t_stamp <= as.Date('2022-05-25', tz = 'America/Los_Angeles')))

# Extract monitoring variables
data_response <- xts::xts(uf1_ops_use |> select(all_of(response_vars1)),
                          order.by = uf1_ops_use$t_stamp)

colnames(data_response) <- c('temp_perm', 'turbidity', 'ammonia')

# Extract explanatory variables for detrending
data_predictor <- xts::xts(uf1_ops_use |> select(all_of(explanatory_vars1)),
                           order.by = uf1_ops_use$t_stamp)
colnames(data_predictor) <- c('pH', 'ORP', 'total_chlorine', 'TMP', 'flux',
                              'temp_corrected_TMP', 'permeability', 'feed_turbidity',
                              'backwash_flow', 'feed_temperature', 'RO_feed_conductivity',
                              'RO_feed_TOC')

response_vars <- colnames(data_response)
scale_predictors <- colnames(data_predictor) -> lag_predictors

source("4_ads_pca_functions.R")

# Code to obtain long-term monitoring results for UF1 for the
# specified training window length (tau),
# IC false alarm rate (alpha),
# and # of consecutive threshold exceedances before issuing an alarm (alarm_num)
system.time(uf1_results <- ads_pca(data_response = data_response,
                                   data_predictor = data_predictor,
                                   tau = 4*24*12, # 12 day training window. A 2 day training window is 4*24*2
                                   dynamic = TRUE,
                                   kappa = 4*24*1, # update after every 1 day of IC observations
                                   method = 'silverman',
                                   detrend = 'adaptive_lasso',
                                   var_amnt = 0.8,
                                   alpha = 0.05, # IC false alarm rate
                                   alarm_num = 1 # require 1 threshold exceedance before issuing an alarm
))

# To obtain results for other training window lengths, IC false alarm rates, and numbers of exceedances
# for alarms, modify "tau", "alpha", and "alarm_num" above.


# Create and save a plot of results associated with the model fit
# The code below needs to be modified to change plot titles and file names for different fits.

# pdf(file = '12day_train_05_t2_spe_1exceed.pdf', width = 6.5, height = 7.5)
par(mfrow=c(2,1), mar = c(1, 4, 3, 2))
plot(time(uf1_results$fdi_results$T2), log(as.numeric(uf1_results$fdi_results$T2)), type = 'l', 
     ylab = expression('log('*T^2*')'), xlab = '', main = expression("12 Day Window; "*alpha*" = 0.05; 1 Exceedance"),
     ylim = c(-6, 25), xaxt = 'n', col = 'black', cex.main = 1.5)
second_training_times <- time(uf1_results$fdi_results$T2)[(time(uf1_results$fdi_results$T2) >= as_date('2022-04-27')) &
                                                            (time(uf1_results$fdi_results$T2) <= as_date('2022-05-11'))]
known_fault_times <- time(uf1_results$fdi_results$T2)[(time(uf1_results$fdi_results$T2) >= as_date('2022-05-12')) &
                                                        (time(uf1_results$fdi_results$T2) <= as_date('2022-05-25'))]
lines(second_training_times, log(as.numeric(uf1_results$fdi_results$T2[second_training_times])),
      col = 'steelblue')
lines(known_fault_times, log(as.numeric(uf1_results$fdi_results$T2[known_fault_times])),
      col = 'goldenrod')
lines(time(uf1_results$fdi_results$T2), log(uf1_results$fdi_results$T2_threshold) |> as.numeric(), col = 'red', lty = 1)
axis(1,
     seq(min(time(uf1_results$fdi_results$T2)), max(time(uf1_results$fdi_results$T2)), length = 5),
     format(seq(min(time(uf1_results$fdi_results$T2)), max(time(uf1_results$fdi_results$T2)), length = 5), "%b %Y"))
legend('topleft', bty = 'n', col = c('black', 'steelblue', 'goldenrod', 'red'), lty = c(1,1,1,1),
       legend = c("Testing Observations", "Known In-Control", "Known Fault", "Threshold"),
       lwd = c(3, 3, 3))
par(mar = c(2,4,2,2))

plot(time(uf1_results$fdi_results$SPE), log(as.numeric(uf1_results$fdi_results$SPE)), type = 'l', 
     ylab = 'log(SPE)', xlab = '', main = '',
     ylim = c(-15,15), xaxt = 'n')
axis(1,
     seq(min(time(uf1_results$fdi_results$T2)), max(time(uf1_results$fdi_results$T2)), length = 5),
     format(seq(min(time(uf1_results$fdi_results$T2)), max(time(uf1_results$fdi_results$T2)), length = 5), "%b %Y"))
lines(second_training_times, log(as.numeric(uf1_results$fdi_results$SPE[second_training_times])),
      col = 'steelblue')
lines(known_fault_times, log(as.numeric(uf1_results$fdi_results$SPE[known_fault_times])),
      col = 'goldenrod')
lines(time(uf1_results$fdi_results$SPE), log(uf1_results$fdi_results$SPE_threshold) |> as.numeric(), col = 'red', lty = 1)
par(mar = c(5,4,4,2) + .1)
# dev.off()


# Table showing the number of days retrained (split between "black", "gray", and "known")
# "black" is the "Testing" period,
# "gray" is the "Known IC" period (it was plotted in gray in a previous version of the code), and
# "known" is the "Known Fault" period
second_training_indices <- which((time(uf1_results$fdi_results$T2) >= as_date('2022-04-27')) &
                                   (time(uf1_results$fdi_results$T2) < as_date('2022-05-12')))
known_fault_indices <- which((time(uf1_results$fdi_results$T2) >= as_date('2022-05-12')) &
                               (time(uf1_results$fdi_results$T2) <= as_date('2022-05-25')))

num_black <- sum((uf1_results$retrain_obs - 1212)< min(second_training_indices)) # number of retrainings during black
num_gray <- sum((uf1_results$retrain_obs - 1212) >= min(second_training_indices) &
                  (uf1_results$retrain_obs - 1212) < min(known_fault_indices)) # number of retrainings during gray
num_known <- sum((uf1_results$retrain_obs - 1212) >= min(known_fault_indices))

black_times <- time(uf1_results$fdi_results$T2)[-c(second_training_indices, known_fault_indices)]
gray_times <- time(uf1_results$fdi_results$T2)[second_training_indices]
known_fault_times <- time(uf1_results$fdi_results$T2)[known_fault_indices]


# Table containing proportion of observations that are OC (split between testing, known IC, and known fault)
tibble(`Statistic` = c('Testing T2', 'Testing SPE', 'IC T2', 'IC SPE', "Known Fault T2", "Known Fault SPE"),
       `Proportion OC` = c(uf1_results$fdi_results$T2_flag[-c(second_training_indices, known_fault_indices)] |> mean(),
                           uf1_results$fdi_results$SPE_flag[-c(second_training_indices, known_fault_indices)] |> mean(),
                           uf1_results$fdi_results$T2_flag[second_training_indices] |> mean(),
                           uf1_results$fdi_results$SPE_flag[second_training_indices] |> mean(),
                           uf1_results$fdi_results$T2_flag[known_fault_indices] |> mean(),
                           uf1_results$fdi_results$SPE_flag[known_fault_indices] |> mean()),
       `Number OC` = c(uf1_results$fdi_results$T2_flag[-c(second_training_indices, known_fault_indices)] |> sum(),
                       uf1_results$fdi_results$SPE_flag[-c(second_training_indices, known_fault_indices)] |> sum(),
                       uf1_results$fdi_results$T2_flag[second_training_indices] |> sum(),
                       uf1_results$fdi_results$SPE_flag[second_training_indices] |> sum(),
                       uf1_results$fdi_results$T2_flag[known_fault_indices] |> sum(),
                       uf1_results$fdi_results$SPE_flag[known_fault_indices] |> sum()),
       `Number Possible` = c(uf1_results$fdi_results$T2_flag[-c(second_training_indices, known_fault_indices)] |> length(),
                             uf1_results$fdi_results$SPE_flag[-c(second_training_indices, known_fault_indices)] |> length(),
                             uf1_results$fdi_results$T2_flag[second_training_indices] |> length(),
                             uf1_results$fdi_results$SPE_flag[second_training_indices] |> length(),
                             uf1_results$fdi_results$T2_flag[known_fault_indices] |> length(),
                             uf1_results$fdi_results$SPE_flag[known_fault_indices] |> length()))

# For each period, # of possible retrainings
num_possible <- floor(nrow(uf1_results$fdi_results) / (4*24*1))
num_pos_black <- floor(length(black_times) / (4*24*1))
num_pos_gray <- floor(length(gray_times) / (4*24*1))
num_pos_known <- floor(length(known_fault_times) / (4*24*1))
# of retrainings that occurred
num_retrain <- uf1_results$retrain_obs |> length()

# Table of proportion of observations retrained for each period (testing, known IC, known fault)
tibble(period = c('all', 'testing', 'known IC', 'known fault'),
       prop_retrain = c(num_retrain / num_possible, num_black / num_pos_black,
                        num_gray / num_pos_gray, num_known / num_pos_known),
       days_possible = c(num_possible, num_pos_black, num_pos_gray, num_pos_known),
       num_retrained = c(num_retrain, num_black, num_gray, num_known))




#################################################################################
#################################################################################
# List of all variables #########################################################
#################################################################################
#################################################################################

# LasVirgenes/UF/AI_10209/Val = "UF Feed Turbidity"
# LasVirgenes/UF/FI_11232/Val = "Raw Water Flow"
# LasVirgenes/UF/LI_14060/Val = "UF Feed Tank Level"
# LasVirgenes/UF/AIT_40006/Val = "RO Feed Conductivity"
# LasVirgenes/UF/AIT_40010/Val = "RO Feed TOC"
# LasVirgenes/UF/AI_31090/Val = "UF Filtrate pH"
# LasVirgenes/UF/AI_31093/Val = "UF Filtrate ORP"
# LasVirgenes/UF/AI_31094/Val = "UF Filtrate Total Chlorine"
# LasVirgenes/UF/AI_36210/Val = "UF Filtrate Ammonia"
# LasVirgenes/UF/FI_36074/Val = "UF Backwash Flow"
# LasVirgenes/UF/AI_31009_1/Val = "UF 1 Filtrate Turbidity"
# LasVirgenes/UF/FI_31032_1/Val = "UF 1 Feed Flow"
# LasVirgenes/UF/FV_31099_1/Val_Sts = "UF 1 Filtrate Valve"
# LasVirgenes/UF/FV_31201_1/Val_Sts = "UF 1 Backwash Valve"
# LasVirgenes/UF/FV_31401_1/Val_Sts = "UF 1 Feed Drain Valve"
# LasVirgenes/UF/FV_31452_1/Val_Sts = "UF 1 CIP Filtrate Valve"
# LasVirgenes/UF/FV_31499_1/Val_Sts = "UF 1 CIP Feed Valve"
# LasVirgenes/UF/PI_31045_1/Val = "UF 1 Feed Pressure"
# LasVirgenes/UF/PI_31059_1/Val = "UF 1 Filtrate Pressure"
# LasVirgenes/UF/LI_36260/Val = "UF Filtrate Tank Level"
# LasVirgenes/UF/TI_14078/Val = "UF Feed Temperature"
# LasVirgenes/UF/UF_01_DrivingPressure/Val = "UF 1 TMP"
# LasVirgenes/UF/UF_01_Flux/Val = "UF 1 Flux"
# LasVirgenes/UF/UF_01_Permeability/Val = "UF 1 Permeability"
# LasVirgenes/UF/UF_01_TCDrivingPressure/Val = "UF 1 Temperature Corrected TMP"
# LasVirgenes/UF/UF_01_TCFlux/Val = "UF 1 Temperature Corrected Flux"
# LasVirgenes/UF/UF_01_TCPermeability/Val = "UF 1 Temperature Corrected Permeability"
# LasVirgenes/UF/FI_37032/Val = "UF CIP Recirc Flow"
# LasVirgenes/UF/LI_37260/Val = "UF CIP Tank Level"
# LasVirgenes/UF/TI_37278/Val = "UF CIP Tank Temperature"

#LasVirgenes/UF/PI_10259/Val = "UF Feed Pressure" (listed in the form of UF (1, 2, 3) Feed Pressure)
