# Getting cs_thresholds and creating plots of results for the original (cleanish) case study data

# Also, computation times for all methods (if interested)

#############################################################################################
# Code to find Phase II monitoring thresholds (control limits) for original PWDF case study data
# to ensure a FAR of 0.005
# Taylor Grimm
# January 18th, 2025
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

cs_thresholds_original <- tibble(classical_mewma = classical_mewma_cs_threshold,
                                 robust_mewma = robust_mewma_cs_threshold,
                                 rssb_mewma = robust_bayes_mewma_cs_threshold,
                                 classical_t2 = classical_t2_cs_threshold,
                                 robust_t2 = robust_t2_cs_threshold,
                                 rssb_t2 = robust_bayes_t2_cs_threshold,
                                 ssmewma = ssmewma_cs_threshold,
                                 sst2 = sst2_cs_threshold)

# save(cs_thresholds_original, file = 'cs_thresholds_original.rds')


source('3_helper_functions_independent.R')
# load('cs_thresholds_original.rds')

# The control charts defined in "3_helper_functions_independent.R" use thresholds
# that assume independence. Here, we'll adjust those functions to use thresholds
# that account for an estimate of the dependence based on the initial 50 observations in the case study.
mewma <- function(train_data, test_data, method = 'classical',
                  lambda = 0.1, far = 0.005, ic_arl = NULL) {
  if(!is.null(ic_arl)) {
    far <- NULL
  }
  if(method == 'classical') {
    mean_vec <- colMeans(train_data)
    cov_mat <- cov(train_data)
  } else if(method == 'rmcd') {
    ests <- robustbase::covMcd(train_data) # reweighted MCD by default
    mean_vec <- ests$center 
    cov_mat <- ests$cov
  } else {
    stop(paste0("Invalid estimation method '", method, "'."))
  }
  
  # center the testing (phase 2) data using the parameter estimates from the training (phase 1) data
  x <- scale(test_data, center = mean_vec, scale = F)
  
  p <- ncol(x)
  q <- matrix(nrow = nrow(x), ncol = p)
  
  for(i in 1:nrow(x)) {
    if(i == 1) {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
    } else {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
    }
    # sigma_q <- ((lambda / (2 - lambda)) * (1 - (1 - lambda)^(2*i))) * cov_mat # exact covariance
  }
  sigma_q <- (lambda / (2 - lambda)) * cov_mat # asymptotic variance, used in Lowry et al. (1992)
  
  t2_val <- numeric()
  for(i in 1:nrow(x)) {
    t2_val[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
  }
  
  if(!is.null(ic_arl)) {
    # use a control limit (h) that ensures IC ARL of ic_arl (such as 200)
    h <- spc::mewma.crit(l = lambda, L0 = ic_arl, p = p) # this assumes multivariate normality
    far <- NULL
  }
  num_train <- nrow(train_data)
  if(method == 'classical') {
    h <- cs_thresholds$classical_mewma
  } else {
    h <- cs_thresholds$robust_mewma
  }
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

hot_t2 <- function(train_data, test_data,
                   method = 'classical', threshold = 'simulation', far = 0.005, ic_arl = NULL) {
  if(method == 'classical') {
    mean_vec <- colMeans(train_data)
    cov_mat <- cov(train_data)
  } else if(method == 'rmcd') {
    ests <- robustbase::covMcd(train_data)
    mean_vec <- ests$center # reweighted MCD by default
    cov_mat <- ests$cov
  } else {
    stop(paste0("Invalid estimation method '", method, "'."))
  }
  p <- ncol(train_data)
  num_train <- nrow(train_data)
  
  if(!is.null(ic_arl)) {
    prob <- 1 - (1 / ic_arl)
    far <- NULL
  } else {
    prob <- 1 - far
  }
  
  t2_train <- mahalanobis(train_data, center = mean_vec, cov = cov_mat)
  
  if(threshold == 'nonparametric') {
    # use the t2_train values to compute a nonparametric threshold, if desired
    # nonparametric threshold using the Silverman bandwidth
    h <- BMS:::quantile.density(density(t2_train, bw = 'nrd0'), probs = prob) 
  } else if(threshold == 'parametric') {
    # parametric sampling distribution under independence and multivariate normality (for classical mean/cov estimators)
    h <- ((p*(num_train^2 - 1)) / (num_train*(num_train - p))) * qf(prob, p, num_train - p)
  } else if(threshold == 'simulation') {
    if(method == 'classical') {
      h <- cs_thresholds$classical_t2
    } else {
      h <- cs_thresholds$robust_t2
    }
  }
  # t2 values during the testing period
  t2_val <- mahalanobis(test_data, center = mean_vec, cov = cov_mat) 
  
  list(train_t2 = t2_train, mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

robust_bayes <- function(initial_train, test, chart = 't2', far = 0.005, manual_cl = NULL) {
  
  p <- ncol(initial_train)
  m0 <- nrow(initial_train)
  
  ests <- list()
  mean_est <- matrix(nrow = nrow(test) + 1, ncol = p)
  cov_est <- list()
  ests[[1]] <- robustbase::covMcd(initial_train)
  mean_est[1, ] <- ests[[1]]$center
  cov_est[[1]] <- ests[[1]]$cov
  
  if(chart == 't2') {
    train_t2 <- mahalanobis(initial_train, center = mean_est[1, ], cov = cov_est[[1]])
    
    if(is.null(manual_cl)) {
      # cl <- (p * (m0^2 - 1) / (m0*(m0 - p))) * qf(1 - far, p, m0 - p)
      cl <- cs_thresholds$rssb_t2
    } else {
      cl <- manual_cl
    }
  } else if(chart == 'mewma') {
    x <- scale(initial_train, center = mean_est[1, ], scale = F)
    q <- matrix(nrow = nrow(x), ncol = p)
    lambda <- 0.1
    for(i in 1:nrow(x)) {
      if(i == 1) {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
      } else {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
      }
      # sigma_q <- ((lambda / (2 - lambda)) * (1 - (1 - lambda)^(2*i))) * cov_mat # exact covariance
    }
    sigma_q <- (lambda / (2 - lambda)) * cov_est[[1]] # asymptotic variance, used in Lowry et al. (1992)
    train_t2 <- numeric()
    for(i in 1:nrow(x)) {
      train_t2[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
    }
    # Set up q for testing data later on
    q <- matrix(nrow = nrow(test), ncol = p)
    if(is.null(manual_cl)) {
      # cl <- spc::mewma.crit(0.1, 200, p)
      cl <- cs_thresholds$rssb_mewma
    } else {
      cl <- manual_cl
    }
  } else {
    stop(paste0('Invalid chart option: "', chart, '".'))
  }
  
  # Compute monitoring statistics during the testing data, evaluate exceedances, update accordingly
  test_t2 <- numeric(nrow(test))
  train_use <- initial_train
  # initialize hyperparameters for first round of posterior updates
  kn <- m0
  vn <- m0 - 1
  for(i in 1:nrow(test)) {
    
    # use previous posterior estimates of mean & covariance matrix to compute monitoring statistics
    # (for i = 1, initial priors of RMCD estimates are used)
    if(chart == 't2') {
      test_t2[i] <- mahalanobis(test[i, ], center = mean_est[i, ], cov = cov_est[[i]])
    } else if(chart == 'mewma') {
      
      x <- scale(test, center = mean_est[i, ], scale = F)
      sigma_q <- (lambda / (2 - lambda)) * cov_est[[i]]
      
      if(i == 1) {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*0 #q_0 is the true IC mean, usually the 0 vector (without loss of generality for simulation study)
      } else {
        q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
      }
      test_t2[i] <- t(q[i, ]) %*% solve(sigma_q) %*% q[i, ]
    }
    
    # check if the i'th monitoring statistic exceeds the control limit
    if(test_t2[i] > cl) {
      # don't update estimates
      ests[[i + 1]] <- ests[[i]]
      mean_est[i + 1, ] <- mean_est[i, ]
      cov_est[[i + 1]] <- cov_est[[i]]
    } else {
      # update mean & covariance estimates
      prior_mean <- mean_est[i, ]
      prior_cov <- cov_est[[i]]
      # obtain posterior mean + covariance updates to use as mean/cov estimates for next observation
      ests[[i + 1]] <- nniw_ests(k0 = kn, # use previous posterior parameters as the new prior parameters
                                 v0 = vn,
                                 prior_mean = prior_mean,
                                 prior_cov = prior_cov,
                                 test_data = test[i, ])
      mean_est[i + 1, ] <- ests[[i + 1]]$posterior_mean_mu
      cov_est[[i + 1]] <- ests[[i + 1]]$posterior_mean_sig
      kn <- ests[[i + 1]]$kn
      vn <- ests[[i + 1]]$vn
    }
  }
  list(train_t2 = train_t2, mon_stats = test_t2, mean_est = mean_est, cov_est = cov_est, 
       exceedances = test_t2 > cl, threshold = cl)
}

ssmewma_stats <- function(data, lambda = 0.1, threshold = 'parametric', far = 0.005, ic_arl = 200,
                          manual_cl = NULL) {
  
  x <- data
  p <- ncol(x)
  if(is.null(manual_cl)) {
    if(threshold == 'parametric') {
      h <- spc::mewma.crit(lambda, ic_arl, p)
    } else if(threshold == 'simulation') {
      h <- cs_thresholds$ssmewma
    } else {
      stop(paste0('Invalid threshold argument "', threshold, '".'))
    }
  } else {
    h <- manual_cl
  }
  
  q <- matrix(nrow = nrow(x), ncol = p)
  t2_val <- rep(NA, nrow(x))
  threshold <- numeric(nrow(x))
  # only start monitoring with the p + 2nd observation
  # since the first nontrivial U_n vector occurs at observation p + 2
  for(i in (p + 2):nrow(x)) {
    if(i == (p + 2)) {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*0
    } else {
      q[i, ] <- lambda * x[i, ] + (1 - lambda)*q[i-1, ]
    }
    sigma_q_inv <- (2 - lambda) / (lambda * (1 - (1 - lambda)^(2*(i - p - 1)))) * diag(p)
    t2_val[i] <- t(q[i, ]) %*% sigma_q_inv %*% q[i, ]
  }
  
  
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

sst2_stats <- function(data, threshold = 'parametric', far = 0.005, ic_arl = 200,
                       manual_cl = NULL) {
  
  x <- data
  p <- ncol(x)
  if(is.null(manual_cl)) {
    if(threshold == 'parametric') {
      if(!is.null(ic_arl)) {
        prob <- 1 - (1 / ic_arl)
        far <- NULL
      } else {
        prob <- 1 - far
      }
      h <- qchisq(prob, df = p)
    } else if(threshold == 'simulation') {
      num_train <- nrow(data) - 1000
      h <- cs_thresholds$sst2
    } else {
      stop(paste0('Invalid threshold argument "', threshold, '".'))
    }
  } else {
    h <- manual_cl
  }
  
  t2_val <- mahalanobis(data, center = rep(0, p), cov = diag(p))
  
  list(mon_stats = t2_val, exceedances = t2_val > h, threshold = h)
}

# Helper function from another paper that's used to preprocess data
# (can detrend responses for a given set of predictors using various supervised
# regression models, such as XGBoost and Adaptive Lasso)
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

train_data <- daily_train
test_data <- daily_test


train_size <- nrow(train_data)
test_size <- nrow(test_data)

cs_thresholds <- cs_thresholds_original
# T2 CHARTS
pdf('t2_plots_faults_original.pdf', width = 7/2, height = 5)
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
     type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('R-'*italic(T)^2),#main = expression(italic(T)['RMCD']^2),
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
pdf('mewma_plots_faults_original.pdf', width = 7/2, height = 5)
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
     type = 'n', xlab = '', ylab = 'Monitoring Statistic', main = expression('R-MEWMA'),#main = expression('MEWMA'['RMCD']),
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







# Computation times for all methods

### Fit each desired control chart, determine signal probability
library(MASS)
library(tidyverse)
library(mvtnorm)

# load in all helper functions
source('3_helper_functions_independent.R')


m0 <- c(30, 50, 100, 250, 500, 1000)
dimension <- c(3, 6)
contamination <- c(0, .1, .2, .3, .4)
errors <- 'mvn' # only considered MVN errors here, but could add additional options such as skewed errors
shift <- c(0, 0.75, 1.5, 2.25, 3)
ph2_scenario <- c(1, 2, 3, 4)

one_sim_time <- function(m0 = 250, dimension = 3, contamination = 0, errors = 'mvn', shift = 0,
                         ph2_scenario = 1, method = 't2') {
  
  gen_dat <- mvrnorm(n = m0, mu = rep(0, dimension), Sigma = diag(dimension))
  
  # define the initial "training" set as the first m0 observations
  # with contaminated errors (or without if contamination = 0)
  if(contamination > 0) {
    # determine how many and which observations should have their error terms replaced
    # (use ceiling() in case m0 * contamination is not a whole number)
    num_contaminated <- ceiling(m0 * contamination)
    contamination_indices <- sample(1:m0, size = num_contaminated)
    dat_train <- gen_dat
    dat_train[contamination_indices, ] <- mvrnorm(n = num_contaminated, mu = rep(shift, dimension), Sigma = diag(dimension))
  } else {
    dat_train <- gen_dat
  }
  if(ph2_scenario == 1) {
    dat <- mvrnorm(1000, mu = rep(shift, dimension), Sigma = diag(dimension))
    oc_indices <- 1:1000
    ic_indices <- NULL
  } else if(ph2_scenario == 2) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(800, mu = rep(shift, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:1000)
    ic_indices <- c(1:200)
  } else if(ph2_scenario == 3) {
    dat <- rbind(mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(200, mu = rep(0, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(shift, dimension), Sigma = diag(dimension)),
                 mvrnorm(100, mu = rep(0, dimension), Sigma = diag(dimension)))
    oc_indices <- c(201:300, 501:600, 801:900)
    ic_indices <- c(1:200, 301:500, 601:800, 901:1000)
  } else if(ph2_scenario == 4) {
    dat <- mvrnorm(1000, mu = rep(0, dimension), Sigma = diag(dimension))
    shifted_obs <- sample(1:1000, 500)
    dat[shifted_obs, ] <- dat[shifted_obs, ] + shift
    oc_indices <- shifted_obs
    ic_indices <- (1:1000)[-shifted_obs]
  }
  
  if(method == 't2') {
    hot_t2(dat_train, dat, method = 'classical', threshold = 'simulation')
  } else if(method == 't2_rmcd') {
    hot_t2(dat_train, dat, method = 'rmcd', threshold = 'simulation')
  } else if(method == 'mewma') {
    mewma(dat_train, dat, method = 'classical', lambda = 0.1)
  } else if(method == 'mewma_rmcd') {
    mewma(dat_train, dat, method = 'rmcd', lambda = 0.1) 
  } else if(method == 'ssmewma') {
    ssmewma(rbind(dat_train, dat), lambda = 0.1, threshold = 'simulation')
  } else if(method == 'sst2') {
    sst2(rbind(dat_train, dat), threshold = 'simulation')
  } else if(method == 'rssb_t2') {
    robust_bayes(initial_train = dat_train, test = dat, chart = 't2')
  } else if(method == 'rssb_mewma') {
    robust_bayes(initial_train = dat_train, test = dat, chart = 'mewma')
  }
}


all_times <- tibble('t2' = bench::mark(one_sim_time(method = 't2'))$median,
                    't2_rmcd' = bench::mark(one_sim_time(method = 't2_rmcd'))$median,
                    'mewma' = bench::mark(one_sim_time(method = 'mewma'))$median,
                    'mewma_rmcd' = bench::mark(one_sim_time(method = 'mewma_rmcd'))$median,
                    'ssmewma' = bench::mark(one_sim_time(method = 'ssmewma'))$median,
                    'sst2' = bench::mark(one_sim_time(method = 'sst2'))$median,
                    'rssb_t2' = bench::mark(one_sim_time(method = 'rssb_t2'))$median,
                    'rssb_mewma' = bench::mark(one_sim_time(method = 'rssb_mewma'))$median)



