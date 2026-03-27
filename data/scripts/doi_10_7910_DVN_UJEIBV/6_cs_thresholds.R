#############################################################################################
# Code to find Phase II monitoring thresholds (control limits) for PWDF case study data
# to ensure a FAR of 0.005
# Taylor Grimm
# June 22nd, 2024
#############################################################################################

# Load/set up data, estimate time series parameters
library(MASS)
library(tidyverse)
library(lubridate)
library(magrittr)
library(glmnet)
library(xts)

load('cleaned_uf_ops_data.RData')

uf1_ops_15min <- uf1_with_status_15min

# Helper function from another paper that's used to preprocess data
# (can detrend responses for a given set of predictors using various supervised
# regression models, such as XGBoost and Adaptive Lasso)
# Here, it's not used for detrending; it's simply used in conjunction
# with other previously written functions to filter to data from the desired dates and variables
detrend_data_lvmwd <- function(data_response,
                               data_predictor,
                               train_index,
                               detrend = 'adaptive_lasso',
                               tuning = 'manual',
                               detrend_args = list,
                               t_stamps = NULL,
                               UF = 1)
{
  data_response <- as.matrix(data_response)
  data_predictor <- as.matrix(data_predictor)
  detrended_data <- NULL
  fitted <- NULL
  best_coefs <- NULL
  best_tune <- NULL
  
  explanatory_vars1 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                         "LasVirgenes/UF/AI_31093/Val", # ORP
                         "LasVirgenes/UF/AI_31094/Val", # total chlorine
                         "LasVirgenes/UF/UF_01_Flux/Val", # flux
                         "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                         "LasVirgenes/UF/FI_36074/Val", # backwash flow
                         "LasVirgenes/UF/TI_14078/Val", # feed temperature
                         "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity // UF Filtrate conductivity
                         "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC // UF filtrate TOC
  
  explanatory_vars2 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                         "LasVirgenes/UF/AI_31093/Val", # ORP
                         "LasVirgenes/UF/AI_31094/Val", # total chlorine
                         "LasVirgenes/UF/UF_02_Flux/Val", # flux
                         "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                         "LasVirgenes/UF/FI_36074/Val", # backwash flow
                         "LasVirgenes/UF/TI_14078/Val", # feed temperature
                         "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity // UF Filtrate conductivity
                         "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC // UF filtrate TOC
  
  explanatory_vars3 <- c("LasVirgenes/UF/AI_31090/Val", # pH
                         "LasVirgenes/UF/AI_31093/Val", # ORP
                         "LasVirgenes/UF/AI_31094/Val", # total chlorine
                         "LasVirgenes/UF/UF_03_Flux/Val", # flux
                         "LasVirgenes/UF/AI_10209/Val", # feed turbidity
                         "LasVirgenes/UF/FI_36074/Val", # backwash flow
                         "LasVirgenes/UF/TI_14078/Val", # feed temperature
                         "LasVirgenes/UF/AIT_40006/Val", # RO feed conductivity // UF Filtrate conductivity
                         "LasVirgenes/UF/AIT_40010/Val") # RO feed TOC // UF filtrate TOC
  
  # Monitor: temp corrected permeability, filtrate turbidity, filtrate ammonia
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
  
  if (UF == 1) {
    response_vars <- response_vars1
    scale_predictors <- explanatory_vars1[!(explanatory_vars1 |> str_detect("Sts"))]
    index_use <- uf1_ops_15min$t_stamp[train_index]
  } else if (UF == 2) {
    response_vars <- response_vars2
    scale_predictors <- explanatory_vars2[!(explanatory_vars1 |> str_detect("Sts"))]
    index_use <- uf2_ops_15min$t_stamp[train_index]
  } else if (UF == 3) {
    response_vars <- response_vars3
    scale_predictors <- explanatory_vars3[!(explanatory_vars1 |> str_detect("Sts"))]
    index_use <- uf3_ops_15min$t_stamp[train_index]
  }
  
  for(resp_var in response_vars){
    data_pred_tmp <- data_predictor
    
    ## scale data
    mu_train_resp <- mean(data_response[train_index, resp_var])
    sd_train_resp <- sd(data_response[train_index, resp_var])
    data_resp_scaled <- scale(data_response[ , resp_var], 
                              center = mu_train_resp, 
                              scale = sd_train_resp)
    
    mu_train_pred <- apply(data_pred_tmp[train_index, ], 2, mean)
    sd_train_pred <- apply(data_pred_tmp[train_index, ], 2, sd)
    mu_train_pred[!names(mu_train_pred)%in%scale_predictors] <- 0
    sd_train_pred[!names(sd_train_pred)%in%scale_predictors] <- 1
    sd_train_pred <- ifelse(sd_train_pred <= 1e-3, 1e-2, sd_train_pred)
    
    data_pred_scaled <- scale(data_pred_tmp, 
                              center = mu_train_pred, 
                              scale = sd_train_pred) 
    
    data_pred_scaled %<>% as.matrix()
    data_resp_scaled %<>% as.vector()
    
    
    if(detrend == 'none') {
      # No detrending -----------------------------------------------------------
      detrended_data <- cbind(detrended_data, data_resp_scaled)
      fitted <- cbind(fitted, data_response[, resp_var])
    } else if(detrend == 'knn') {
      set.seed(1234)
      if(tuning == 'auto') {
        knn_mod <- caret::train(data_pred_scaled[train_index, ],
                                data_resp_scaled[train_index],
                                method = 'knn',
                                # tuneLength = 10,
                                trControl = trainControl(method = 'cv'))
        best_tune <- knn_mod$bestTune
      } else {
        knn_mod <- do.call(caret::knnreg, c(list(x = data_pred_scaled[train_index, ],
                                                 y = data_resp_scaled[train_index]),
                                            detrend_args))
      }
      ## get predicted values and calculate residuals
      missing_rows <- apply(data_pred_scaled, 1, function(x) any(is.na(x))) |> which()
      if(length(missing_rows) == 0) {
        fitted_vals <- predict(knn_mod, newdata = data_pred_scaled)
      } else {
        fitted_vals <- numeric(nrow(data_pred_scaled))
        fitted_vals[missing_rows] <- NA
        fitted_vals[-missing_rows] <- predict(knn_mod, newdata = na.omit(data_pred_scaled))
      }
      fitted_vals %<>% as.matrix(ncol = 1)
      rownames(fitted_vals) <- rownames(data_pred_scaled)
      fitted_orig_scale <- sd_train_resp*fitted_vals+mu_train_resp
      
      ## calculate residuals
      residuals <- data_resp_scaled - fitted_vals  
      
      detrended_data <- cbind(detrended_data, residuals)
      fitted <- cbind(fitted, fitted_orig_scale)
    } else if(detrend == 'xgboost') {
      # XGBoost detrending ------------------------------------------------------
      set.seed(1234)
      if(tuning == 'auto') {
        xgb_mod <- caret::train(data_pred_scaled[train_index, ],
                                data_resp_scaled[train_index],
                                method = 'xgbTree',
                                #  tuneLength = 2,
                                trControl = trainControl(method = 'cv'))
        best_tune <- xgb_mod$bestTune
        fitted_vals <- predict(xgb_mod, newdata = data_pred_scaled)
      } else {
        all_train_data <- cbind(data_pred_scaled[train_index, ], data_resp_scaled[train_index])
        # Add label name to response var column
        colnames(all_train_data)[ncol(all_train_data)] <- resp_var
        
        dtrain <- xgboost::xgb.DMatrix(data = all_train_data, label = all_train_data[, resp_var]) 
        
        xg_mod <- do.call(xgboost::xgb.cv, c(list(data = dtrain,
                                                  eta = 0.2,
                                                  verbose = 0,
                                                  nfold = 10,
                                                  early_stopping_rounds = 3),
                                             detrend_args))
        # xg_mod <- xgboost::xgb.cv(data = dtrain,
        #                           nrounds = 30,# max number of boosting iterations
        #                           nfold = 10,  # k-fold cross-validation
        #                           verbose = 0, # don't print out info at each interation
        #                           eta = 0.2) 
        best_nrounds <- xg_mod$best_iteration
        
        xg_mod <- xgboost::xgboost(dtrain,
                                   nrounds = best_nrounds,
                                   max_depth = 4,
                                   eta = 0.2,
                                   verbose = 0)
        
        all_data <- cbind(data_pred_scaled, data_resp_scaled)
        colnames(all_data)[ncol(all_data)] <- resp_var
        
        all_data <- xgboost::xgb.DMatrix(data = all_data, label = all_data[, resp_var])
        fitted_vals <- predict(xg_mod, newdata = all_data)
      }
      fitted_orig_scale <- sd_train_resp*fitted_vals+mu_train_resp
      
      ## calculate residuals
      residuals <- data_resp_scaled - fitted_vals  
      
      detrended_data <- cbind(detrended_data, residuals)
      fitted <- cbind(fitted, fitted_orig_scale)
      
    } else if(detrend == 'rf') {
      # Random Forest detrending ------------------------------------------------
      set.seed(1234)
      
      if(tuning == 'auto') {
        rf_mod <- caret::train(data_pred_scaled[train_index, ],
                               data_resp_scaled[train_index],
                               method = 'rf',
                               tuneLength = 5,
                               trControl = trainControl(method = 'cv'))#,
        #  ntree = 50)
        best_tune <- rf_mod$bestTune
        fitted_vals <- predict(rf_mod, newdata = data_pred_scaled)
      } else {
        rf_mod <- do.call(randomForest::randomForest, c(list(x = data_pred_scaled[train_index, ],
                                                             y = data_resp_scaled[train_index]),
                                                        detrend_args))
        
        
        # get predicted values and calculate residuals
        missing_rows <- apply(data_pred_scaled, 1, function(x) any(is.na(x))) |> which()
        if(length(missing_rows) == 0) {
          fitted_vals <- predict(rf_mod, newdata = data_pred_scaled)
        } else {
          fitted_vals <- numeric(nrow(data_pred_scaled))
          fitted_vals[missing_rows] <- NA
          fitted_vals[-missing_rows] <- predict(rf_mod, newdata = na.omit(data_pred_scaled))
        }
      }
      fitted_vals %<>% as.matrix(ncol = 1)
      rownames(fitted_vals) <- rownames(data_pred_scaled)
      fitted_orig_scale <- sd_train_resp*fitted_vals+mu_train_resp
      ## calculate residuals
      residuals <- data_resp_scaled - fitted_vals  
      
      detrended_data <- cbind(detrended_data, residuals)
      fitted <- cbind(fitted, fitted_orig_scale)
      
    } else if(detrend == 'adaptive_lasso') {
      # Adaptive Lasso detrending -----------------------------------------------
      set.seed(1234)
      ## get initial estimates using ridge regression
      ridge_cv <- cv.glmnet(x = data_pred_scaled[train_index, ], 
                            y = data_resp_scaled[train_index], 
                            alpha = 0, 
                            standardize = FALSE)
      best_ridge_coef <- as.numeric(coef(ridge_cv, s = ridge_cv$lambda.min))[-1]
      
      ## adaptive lasso using initial estimates above
      ## use k-fold cross validation to choose regularization parameter, lambda
      alasso_cv <- cv.glmnet(x = data_pred_scaled[train_index, ], 
                             y = data_resp_scaled[train_index],   
                             alpha = 1,
                             standardize = FALSE,
                             penalty.factor = 1 / abs(best_ridge_coef))
      best_alasso_coef <- as.numeric(coef(alasso_cv, s = alasso_cv$lambda.min))[-1]
      
      ## get predicted values and calculate residuals
      fitted_vals <- predict(alasso_cv, 
                             newx = data_pred_scaled, 
                             s = alasso_cv$lambda.min)
      fitted_orig_scale <- sd_train_resp*fitted_vals+mu_train_resp
      
      ## calculate residuals
      residuals <- data_resp_scaled - fitted_vals  
      
      # at each iteration of the loop (looping through response variables),
      # append the new residuals, fitted vals, and lasso coefficients to
      # their respective data frames
      detrended_data <- cbind(detrended_data, residuals)
      fitted <- cbind(fitted, fitted_orig_scale)
      best_coefs <- rbind(best_coefs, best_alasso_coef)
    } else {
      stop('Invalid detrending method option. Choose "none", "knn", "xgboost", "rf", or "adaptive_lasso".')
    }
  }
  
  colnames(detrended_data) <- response_vars
  colnames(fitted) <- response_vars
  fitted %<>% 
    xts(order.by = t_stamps)
  detrended_data %<>% 
    xts(order.by = t_stamps)
  list(detrended_data = detrended_data,
       best_coefs = best_coefs,
       fitted = fitted,
       data_response = data_response,
       best_tune = best_tune)
}   
source('3_helper_functions_independent.R')

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

par(mfrow = c(3, 1), mar = c(2, 4, 2.5, 1) + 0.1)
use_train <- long_term$train_original
use_test <- long_term$test_original
ylim1 <- c(3, 6.5); ylim2 <- c(10, 70); ylim3 <- c(0, 2)

# Create daily train/test (phase I and phase II) datasets
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

# Introduce contamination into daily_train
set.seed(24)
num_contaminated <- 10 # number of contaminated points
contamination <- rep(c(1, 35, 1), each = num_contaminated) # size of mean shift for each variable
cont_indices <- sample(1:nrow(daily_train), num_contaminated) # which indices to contaminate
daily_train[cont_indices, 1:3] <- daily_train[cont_indices, 1:3] + rnorm(3*num_contaminated, mean = contamination,
                                                                         sd = rep(c(.25, 20, .25), each = 10))

varfit <- MTS::VAR(daily_train[, 1:3], p = 1)

far_sim_cs <- function(samp_size = 50, # number of obs in the initial daily_train dataset
                       chart = 'mewma', method = 'classical', threshold = NULL) {
  # function to find the FAR of a single sample
  far1 <- function(samp_size = 50, chart = 'mewma', method = 'classical', threshold = NULL) {
    gen_dat <- my_VARMAsim(nobs = samp_size + 1000, arlags = 1,
                           cnst = varfit$Ph0, phi = varfit$Phi,
                           sigma = varfit$Sigma, errors = 'mvn')
    samp <- gen_dat$series[1:samp_size, ]
    samp2 <- gen_dat$series[-c(1:samp_size), ]
    
    if(chart == 'mewma') {
      mean(mewma(train_data = samp, test_data = samp2,
                 method = method, far = NULL, ic_arl = 200)$mon_stats >= threshold)
    } else if(chart == 't2') {
      mean(hot_t2(train_data = samp, test_data = samp2,
                  method = method, threshold = 'parametric')$mon_stats >= threshold)
    } else if(chart == 'ssmewma') {
      mean(ssmewma(data = rbind(samp, samp2),
                   lambda = 0.1, manual_cl = threshold)$mon_stats[-c(1:samp_size)] >= threshold)
    } else if(chart == 'sst2') {
      mean(sst2(data = rbind(samp, samp2),
                manual_cl = threshold)$mon_stats[-c(1:samp_size)] >= threshold)
    } else if(chart == 'robust_bayes_t2') {
      mean(robust_bayes(initial_train = samp, test = samp2, chart = 't2', far = 0.005,
                        manual_cl = threshold)$mon_stats >= threshold)
    } else if(chart == 'robust_bayes_mewma') {
      mean(robust_bayes(initial_train = samp, test = samp2, chart = 'mewma', far = 0.005,
                        manual_cl = threshold)$mon_stats >= threshold)
    } else {
      stop("Invalid control chart argument.")
    }
  }
  # get lots of FAR's from lots of samples
  all_far <- replicate(5000, far1(samp_size = samp_size, chart = chart,
                                  method = method, threshold = threshold))
  
  mean(all_far)
}

# use the bisection algorithm to find the threshold that results in a 0.005 FAR
far_0_cs <- function(threshold, samp_size = 50, method = 'classical',
                      chart = 'mewma', far = 0.005) {
  far_sim_cs(samp_size = samp_size, method = method,
              chart = chart, threshold = threshold) - far
}


system.time(classical_mewma_cs_threshold <- uniroot(far_0_cs, interval = c(10, 600), chart = 'mewma', method = 'classical')$root)
system.time(robust_mewma_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 'mewma', method = 'rmcd')$root)
system.time(robust_bayes_mewma_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 'robust_bayes_mewma')$root)

system.time(classical_t2_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 't2', method = 'classical')$root)
system.time(robust_t2_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 't2', method = 'rmcd')$root)
system.time(robust_bayes_t2_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 'robust_bayes_t2')$root)

system.time(ssmewma_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 'ssmewma')$root)

system.time(sst2_cs_threshold <- uniroot(far_0_cs, interval = c(10, 1000), chart = 'sst2')$root)

cs_thresholds <- tibble(classical_mewma = classical_mewma_cs_threshold,
                        robust_mewma = robust_mewma_cs_threshold,
                        rssb_mewma = robust_bayes_mewma_cs_threshold,
                        classical_t2 = classical_t2_cs_threshold,
                        robust_t2 = robust_t2_cs_threshold,
                        rssb_t2 = robust_bayes_t2_cs_threshold,
                        ssmewma = ssmewma_cs_threshold,
                        sst2 = sst2_cs_threshold)

# save(cs_thresholds, file = 'cs_thresholds.rds')
