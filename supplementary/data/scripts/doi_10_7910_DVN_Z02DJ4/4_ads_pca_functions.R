##########################################
# All functions needed to perform ADS-PCA
# as described by Klanderman et al., 2020
#
# Taylor Grimm
# October 18th, 2023
##########################################


###########################################
###########################################
## Function implements ADS-PCA
## -data_response (xts) xts of response vars
## -data_predictor (xts) xts of predictor vars
## -tau (int) number of observations in training window
## -dynamic (bool) TRUE if lagged values should be used (dynamic PCA)
## -kappa (int) reset training window after how many observations (adaptive PCA)
## -method (str) how to estimate threshold ("sj", "silverman", "scott", "boot", or "raw")
## -detrend (str) detrending method to apply ("none", "knn", "xgboost", "rf", or "adaptive_lasso")
## -var_amnt (dbl) amnt of variability captured by PCs (btwn 0 & 1)
## -alpha (dbl) false alarm rate to determine threshold (btwn 0 & 1) 
## - alarm_num (int) how many threshold exceedances in a row before triggering an alarm?
## -detrend_args (list) list of additional arguments to pass to the specified detrending method
## -tuning (str) 'auto' to automatically select optimal tuning parameters for detrending (via cross-validation)
## -pca_method (str) how to perform pca ("classical", "mcd", "M", "proj_pursuit")
#############################################
#############################################
ads_pca <- function (data_response,
                     data_predictor,
                     tau = 5*60*24, 
                     dynamic = TRUE, 
                     kappa = 1*60*24, 
                     method = "sj",
                     detrend = "adaptive_lasso",
                     var_amnt = 0.8,
                     alpha = 0.005,
                     alarm_num = 5,
                     detrend_args = list(), # list(k = 50) (knn), list(nrounds = 30) (xgboost), list(ntree = 50, mtry = 10, nodesize = 10) (rf)
                     tuning = 'manual',
                     pca_method = 'classical')
{
  
  if(dynamic){
    train_index <- 60+1:tau
    test_index <- (tau + 61):nrow(data_response) 
  }else {
    train_index <- 1:tau
    test_index <- (tau + 1):nrow(data_response)  
  }
  
  ## train & monitor using initial training window
  monitor_obj <- monitor_process(data_response = data_response,
                                 data_predictor = data_predictor,
                                 train_index = train_index, 
                                 test_index = test_index,
                                 kappa = kappa, 
                                 dynamic = dynamic,
                                 method = method,
                                 detrend = detrend,
                                 var_amnt = var_amnt,
                                 alpha = alpha,
                                 alarm_num = alarm_num,
                                 detrend_args = detrend_args,
                                 tuning = tuning,
                                 pca_method = pca_method)
  
  ## store values from first training
  detrended_data <- monitor_obj$detrended_data
  fitted <- monitor_obj$fitted
  model_coefs <- monitor_obj$model_coefs
  fdi_results <- monitor_obj$fdi_results
  train_T2s <- monitor_obj$train_T2s
  train_SPEs <- monitor_obj$train_SPEs
  best_tune <- list(monitor_obj$best_tune)
  
  ## keep track of when we retrain and which observations are used
  obs_to_tr <- which(index(data_response) %in% index(monitor_obj$kept_obs))
  obs_to_tr_new <- obs_to_tr
  retrain_obs <- min(test_index)
  
  while (length(obs_to_tr_new) == kappa) {
    nn <- length(obs_to_tr_new)
    ## update training and test window
    if (nn < tau) {
      train_index <- c(tail(train_index, n = (tau - nn)),
                       obs_to_tr_new)
    } else {
      train_index <- head(obs_to_tr[(nn - tau + 1):nn], 
                          n = tau)
    }
    test_index <- (max(train_index)+1):nrow(data_response)
    retrain_obs <- c(retrain_obs, min(test_index))
    if (max(train_index) == nrow(data_response)) 
      break
    
    ## retrain model using new training window
    monitor_obj <- monitor_process(data_response = data_response,
                                   data_predictor = data_predictor,
                                   train_index = train_index, 
                                   test_index = test_index,
                                   kappa = kappa, 
                                   dynamic = dynamic,
                                   method = method,
                                   detrend = detrend,
                                   var_amnt = var_amnt,
                                   alpha = alpha,
                                   alarm_num = alarm_num,
                                   best_lags = monitor_obj$best_lags,
                                   detrend_args = detrend_args,
                                   tuning = tuning,
                                   pca_method = pca_method)
    
    ## update various stored values
    model_coefs <- append(model_coefs, monitor_obj$model_coefs)
    best_tune <- append(best_tune, monitor_obj$best_tune)
    
    #train_T2s[index(data_response[train_index,])] <- monitor_obj$train_T2s
    #train_SPEs[index(data_response[train_index,])] <- monitor_obj$train_SPEs
    
    fdi_results[index(monitor_obj$fdi_results), ] <- monitor_obj$fdi_results
    detrended_data[index(monitor_obj$fdi_results), ] <- monitor_obj$detrended_data[index(monitor_obj$fdi_results), ]
    fitted[index(monitor_obj$fdi_results), ] <- monitor_obj$fitted[index(monitor_obj$fdi_results), ]
    obs_to_tr_new <- which(index(data_response) %in% index(monitor_obj$kept_obs))
    if (length(obs_to_tr_new) != 0) {
      obs_to_tr <- c(obs_to_tr, obs_to_tr_new)
    }
  }
  
  list(fdi_results = fdi_results, 
       retrain_obs = retrain_obs, 
       detrended_data = detrended_data,
       model_coefs = model_coefs,
       fitted = fitted,
       initial_train_T2s = train_T2s,
       initial_train_SPEs = train_SPEs,
       best_tune = best_tune,
       settings = list(tau = tau, dynamic = dynamic, kappa = kappa, method = method,
                       detrend = detrend, var_amnt = var_amnt, alpha = alpha, alarm_num = alarm_num,
                       detrend_args = detrend_args, tuning = tuning, pca_method = pca_method))
}


###########################################
###########################################
## Function monitors process based on a
## given training window and returns alarm for OC 
## obs. and shifted vars. 
## -data_response (xts) xts of response vars
## -data_predictor (xts) xts of predictor vars
## -train_index (vec of length tau) indices of training window
## -test_index (vec) indices of monitoring period
## -dynamic (bool) TRUE if lagged values should be used
## -method (str) how to estimate threshold ("sj", "silverman", "scott","boot", or "raw")
## -detrend (str) detrending method to apply ("none", "knn", "xgboost", "rf", or "adaptive_lasso")
## -kappa (int) reset training window after how many obs.
## -var_amnt (dbl) amnt of variability captured by PCs (btwn 0 & 1)
## -alpha (dbl) false alarm rate to determine threshold (btwn 0 & 1) 
## - alarm_num (int) how many threshold exceedances in a row before triggering an alarm?
## -best_lags (list) list of best lags 
## -detrend_args (list) list of additional arguments to pass to the specified detrending method
## -pca_method (str) how to perform pca ("classical", "mcd", "M", "proj_pursuit")
#############################################
#############################################
monitor_process <- function (data_response,
                             data_predictor,
                             train_index, 
                             test_index, 
                             dynamic = TRUE,
                             method = "sj",
                             detrend = 'adaptive_lasso',
                             kappa = 1*60*24, 
                             var_amnt = 0.8, 
                             alpha = 0.005,
                             alarm_num = 5,
                             best_lags = NULL,
                             detrend_args = detrend_args,
                             tuning = 'manual',
                             pca_method = 'classical')
{
  ## detrend data
  detrended_obj <- detrend_data(data_response = data_response,
                                data_predictor = data_predictor,
                                train_index = train_index,
                                test_index = test_index,
                                dynamic = dynamic,
                                detrend = detrend,
                                best_lags = best_lags,
                                detrend_args = detrend_args,
                                tuning = tuning)
  
  ## save values from detrended data object
  model_coefs <- list(detrended_obj$best_coefs)
  detrended_data <- detrended_obj$detrended_data
  fitted <- detrended_obj$fitted
  best_lags <- detrended_obj$best_lags
  best_tune <- detrended_obj$best_tune
  
  ## find best lag for (residuals of) response vars 
  ## if we haven't already
  if(dynamic & is.null(best_lags$response)){
    get_lag_resp <- function(resp_var, detrended_data, train_index) {
      ts(detrended_data[train_index, resp_var]) %>%
        pacf(max.lag = 60, plot = FALSE) %>%
        .$acf %>% 
        which.max()
    }
    
    best_lags$response <- map(response_vars, 
                              ~get_lag_resp(., detrended_data, train_index)) %>% 
      setNames(response_vars)  
  }
  
  ## append lagged residuals of response variables
  if(dynamic){
    for(resp_var in response_vars) {
      lag <- best_lags %>% 
        pluck("response", resp_var)
      detrended_data <- cbind(detrended_data, 
                              stats::lag(detrended_data[, resp_var], lag))
    }  
  }
  
  ## split into training and test data, apply PCA, get monitoring statistics & find thresholds
  train_data <- detrended_data[train_index, ]  
  test_data <- detrended_data[test_index, ]  
  mon_stat_obj <- get_mon_stats(train_data = train_data, 
                                var_amnt = var_amnt,
                                pca_method = pca_method)
  threshold_obj <- threshold(mon_stat_obj,
                             method = method,
                             alpha = alpha)
  
  train_T2s <- mon_stat_obj$T2s |> xts(order.by = index(train_data))
  train_SPEs <- mon_stat_obj$SPEs |> xts(order.by = index(train_data))
  
  ## add columns with monitoring stats & alarms (0 or 1)
  fdi_results <- lapply(1:nrow(test_data), function(index) {
    fault_detect(threshold_obj = threshold_obj, 
                 test_obs = test_data[index, ])
  })
  fdi_results <- do.call(rbind, fdi_results) %>% 
    xts(order.by = index(test_data))
  fdi_results <- cbind(fdi_results, rep(0, nrow(fdi_results)))
  colnames(fdi_results)[ncol(fdi_results)] <- "any_flag"
  
  for (i in 1:nrow(fdi_results)) {
    SPE_flag <- fdi_results[i, "SPE_flag"] 
    T2_flag <- fdi_results[i, "T2_flag"]
    flags <- as.numeric(SPE_flag + T2_flag > 0)
    if(flags == 1) fdi_results[i, "any_flag"] <- 1 
  }
  fdi_results$Alarm <- fdi_results[, "any_flag"] |>
    rollmean(k = alarm_num, align = 'right', fill = 0)
  fdi_results$Alarm <- as.numeric(fdi_results$Alarm == 1)
  
  fault_isolation_obj <- fault_isolation(train_data = train_data, 
                                         test_data = test_data, 
                                         fdi_results = fdi_results)
  
  fdi_results <- cbind(fdi_results, fault_isolation_obj)
  
  
  
  non_alarmed_obs <- fdi_results[fdi_results[, "Alarm"] == 0, ]
  kept_obs_index <- head(index(non_alarmed_obs), n = kappa)
  kept_obs <- test_data[kept_obs_index]
  
  list(fdi_results = fdi_results, 
       kept_obs = kept_obs,
       best_lags = best_lags, 
       detrended_data = detrended_data,
       model_coefs = model_coefs, 
       fitted = fitted,
       train_T2s = train_T2s,
       train_SPEs = train_SPEs,
       best_tune = best_tune)
}


###########################################
###########################################
## Function detrends the data based on a given
## training window using the specified "detrend" method (below)
## -data_response (xts) xts of response vars
## -data_predictor (xts) xts of predictor vars
## -train_index (vec of length tau) indices of training window
## -test_index (vec) indices of monitoring period
## -dynamic (bool) TRUE if lagged values should be used
## -detrend (str) detrending method to apply ("none", "knn", "xgboost", "rf", or "adaptive_lasso")
## -best_lags (list) list of best lags 
## -detrend_args (list) list of additional arguments to pass to the specified detrending method
## -tuning (str) 'auto' to automatically select optimal tuning parameters for detrending (via cross-validation)
#############################################
#############################################
detrend_data <- function(data_response,
                         data_predictor,
                         train_index, 
                         test_index, 
                         dynamic = TRUE,
                         detrend = 'adaptive_lasso',
                         best_lags = NULL,
                         detrend_args = list,
                         tuning = 'manual')
{
  
  detrended_data <- NULL
  fitted <- NULL
  best_tune <- NULL
  
  ## Select best # of lags to include for each variable
  if(dynamic & is.null(best_lags)){
    get_lag_pred <- function(resp_var, pred_var, train_index) {
      mv_ts <- ts(cbind(data_response[train_index, resp_var], 
                        data_predictor[train_index, pred_var]))
      mv.pacf <- pacf(mv_ts, 
                      max.lag = 60, 
                      plot = FALSE)
      which.max(abs(mv.pacf$acf[,1,2])) 
    }
    
    best_lags$predictors <- map(response_vars, function(x){
      map(lag_predictors, function(y) {
        get_lag_pred(x, y, train_index)
      }) %>% 
        setNames(lag_predictors)  
    }) %>% setNames(response_vars) 
  }
  
  ## add lagged predictors to the list of predictor vars to scale
  if(dynamic) scale_predictors <- c(scale_predictors, paste0(scale_predictors, ".1"))
  
  best_coefs <- NULL
  
  for(resp_var in response_vars){
    data_pred_tmp <- data_predictor
    
    if(dynamic){
      ## add lagged version of predictors
      for(pred_var in lag_predictors) {
        lag <- best_lags %>% 
          pluck("predictors", resp_var, pred_var)
        data_pred_tmp <- cbind(data_pred_tmp, 
                               stats::lag(data_pred_tmp[,pred_var], k = lag))
      }  
    } 
    
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
  if(!is.null(best_coefs)) {
    rownames(best_coefs) <- colnames(data_response)
    if(dynamic) {
      colnames(best_coefs) <- c(colnames(data_predictor), paste0(lag_predictors, ".l"))
    } else {
      colnames(best_coefs) <- colnames(data_predictor)
    }
  }
  
  colnames(detrended_data) <- response_vars
  colnames(fitted) <- response_vars
  fitted %<>% 
    xts(order.by = index(data_response))
  detrended_data %<>% 
    xts(order.by = index(data_response))
  list(detrended_data = detrended_data,
       best_lags = best_lags,
       best_coefs = best_coefs,
       fitted = fitted,
       best_tune = best_tune)
}   


###########################################
###########################################
## Function performs PCA and calculates 
## monitoring statistics
## -train_data (xts) training data used to do PCA
## -var_amnt (dbl) amnt of variability captured by PCs (btwn 0 & 1)
## -pca_method (str) how to perform pca ("classical", "mcd", "M", "proj_pursuit")
#############################################
#############################################
get_mon_stats <- function (train_data, 
                           var_amnt = 0.8,
                           pca_method = 'classical') 
{
  if(pca_method == 'classical') {
    RR <- cor(train_data)
  } else if(pca_method == 'mcd') {
    set.seed(24)
    RR <- robust::covRob(train_data, estim = 'mcd', corr = T)$cov
  } else if(pca_method == 'M') {
    set.seed(24)
    RR <- robustbase::covMcd(train_data, cor = T)$cor
  } else if(pca_method == 'proj_pursuit') {
    set.seed(24)
    rob_cov <- pcaPP::covPCAproj(train_data)$cov
    D_inv <- solve(diag(sqrt(diag(rob_cov))))
    RR <- D_inv %*% rob_cov %*% D_inv
  }
  
  eigen_RR <- eigen(RR)
  lambda <- eigen_RR$values
  PP <- eigen_RR$vectors
  prop_var <- as.matrix(cumsum(lambda)/sum(lambda) * 100)
  if (var_amnt != 1) {
    comps <- which(prop_var - (var_amnt * 100) > 0)[1]
    PP_k <- as.matrix(PP[, 1:comps])
    lambda_k <- diag(lambda[1:comps], ncol = length(1:comps))
  } else {
    PP_k <- as.matrix(PP)
    lambda_k <- diag(lambda, ncol = length(lambda))
  }
  
  yy <- train_data %*% PP_k
  yy_hat <- yy %*% t(PP_k)
  E <- train_data - yy_hat
  SPEs <- diag(E %*% t(E))
  lambda_inv <- solve(lambda_k)
  T2s <- diag(yy %*% lambda_inv %*% t(yy))
  
  list(PP_k = PP_k, 
       lambda_inv = lambda_inv, 
       SPEs = SPEs, 
       T2s = T2s)
}


###########################################
###########################################
## Function finds thresholds based on 
## monitoring statistics from training period
## -mon_stat_obj (list) object returned from get_mon_stats object
## -method (str) how to estimate threshold ("sj", "silverman", "scott", "boot", or "raw")
## -alpha (dbl) false alarm rate to determine threshold (btwn 0 & 1) 
#############################################
#############################################
threshold <- function (mon_stat_obj, 
                       method = "sj", 
                       alpha = 0.005) 
{
  
  get_threshold <- function (mon_stat, 
                             method = "sj", 
                             alpha = 0.005) 
  {
    
    if(method == "sj") {
      mon_stat_dens <- density(mon_stat, 
                               bw = "SJ", 
                               kernel = "gaussian", 
                               from = 0)
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, 1 - alpha)
      
    } else if(method == 'silverman') {
      mon_stat_dens <- density(mon_stat, 
                               bw = "nrd0", 
                               kernel = "gaussian", 
                               from = 0)
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, 1 - alpha)
      
    } else if(method == 'scott') {
      mon_stat_dens <- density(mon_stat, 
                               bw = "nrd", 
                               kernel = "gaussian", 
                               from = 0)
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, 1 - alpha)
      
    } else if(method == "boot") {
      set.seed(24)
      bs_quant <- function(data, indices, alpha) quantile(data[indices], probs = c(1 - alpha))
      mon_stat_threshold <- boot::boot(data = mon_stat, 
                                       statistic = bs_quant, 
                                       R = 1000, 
                                       alpha = alpha)$t %>% mean()
    } else if (method == 'raw') {
      mon_stat_threshold <- quantile(mon_stat, probs = c(1 - alpha))
    } else {
      stop('That is not a valid method option. Choose "sj", "silverman", "scott", "boot", or "raw".')
    }
    
    mon_stat_threshold
  }
  
  SPEs <- mon_stat_obj$SPEs
  T2s <- mon_stat_obj$T2s
  SPE_threshold <- get_threshold(SPEs, 
                                 method = method,
                                 alpha = alpha)
  T2_threshold <- get_threshold(T2s, 
                                method = method,
                                alpha = alpha)
  
  list(SPE_threshold = SPE_threshold, 
       T2_threshold = T2_threshold, 
       PP_k = mon_stat_obj$PP_k, 
       lambda_inv = mon_stat_obj$lambda_inv)
  
}


###########################################
###########################################
## Function calculates monitoring stats for 
## a given test obs. and determines whether 
## it exceeds the threshold
## -threshold_obj (list) object returned from threshold function
## -test_obs (row from xts) observation from test data set
#############################################
#############################################
fault_detect <- function (threshold_obj, 
                          test_obs) 
{
  ## get value of threshold for monitoring statistics
  SPE_threshold <- threshold_obj$SPE_threshold
  T2_threshold <- threshold_obj$T2_threshold
  
  ## calculate monitoring statistics and determine if they exceeded thresholds
  PP_k <- threshold_obj$PP_k
  lambda_inv <- threshold_obj$lambda_inv
  yy <- test_obs %*% PP_k
  yy_hat <- yy %*% t(PP_k)
  EE <- test_obs - yy_hat
  SPE <- diag(EE %*% t(EE))
  SPE_flag <- as.numeric(SPE > SPE_threshold)
  T2 <- diag(yy %*% lambda_inv %*% t(yy))
  T2_flag <- as.numeric(T2 > T2_threshold)
  
  
  
  object <- matrix(c(SPE, rep(SPE_threshold, length(SPE_flag)), SPE_flag,
                     T2, rep(T2_threshold, length(T2_threshold)), T2_flag),
                   nrow = 1)
  colnames(object) <- c("SPE", "SPE_threshold", "SPE_flag", "T2", "T2_threshold", "T2_flag")
  object
}


fault_isolation <- function(train_data, 
                            test_data,
                            fdi_results) {
  
  shifted_vars <- matrix(rep(0, nrow(test_data)*length(response_vars)), 
                         nrow = nrow(test_data))
  colnames(shifted_vars) <- response_vars
  
  ## PCA
  RR <- cor(train_data[, response_vars])
  eigen_RR <- eigen(RR)
  PP <- eigen_RR$vectors
  sqrt_lambda <- eigen_RR$values %>%
    sqrt() %>%
    diag()  
  
  for(obs in 1:nrow(fdi_results)){
    
    if(fdi_results[obs, "Alarm"] != 0){
      test_obs <- test_data[obs, response_vars] %>% 
        as.vector()
      yy <- PP %*% test_obs
      yy_star <- solve(sqrt_lambda) %*% yy
      PP_star <- solve(sqrt_lambda) %*% PP
      
      ## OLS
      mu_hat_ols <- solve(t(PP_star)%*%PP_star)%*%t(PP_star)%*%yy_star
      
      ## Fit adaptive lasso
      set.seed(51918)
      fit_cv <- cv.glmnet(x = PP_star,
                          y = yy_star,
                          penalty.factor = 1/abs(mu_hat_ols),
                          standardize = FALSE,
                          intercept = FALSE,
                          nfolds = length(test_obs), # leave-one-out cross-validation
                          grouped = FALSE) # grouped = FALSE is necessary when # obs per fold
      # is less than 3
      
      mu_hat <- as.numeric(coef(fit_cv, s = "lambda.min"))[-1]
      shifted_vars[obs, ] <- sign(mu_hat)
    }
  }
  
  shifted_vars
}
