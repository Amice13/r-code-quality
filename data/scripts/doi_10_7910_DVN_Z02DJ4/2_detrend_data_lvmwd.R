###########################################
###########################################
## Function detrends the data based on a given
## training window using the specified "detrend" method (below)
## -data_response (xts) xts of response vars
## -data_predictor (xts) xts of predictor vars
## -train_index (vec of length tau) indices of training window
## -detrend (str) detrending method to apply ("none", "knn", "xgboost", "rf", or "adaptive_lasso")
## - tuning (str) hyperparameter tuning can be done with 10-fold CV if tuning = "auto".
## Otherwise, detrend_args need to be specified for KNN, RF, and XGBoost to specify things
## like k (KNN), ntree (RF), and nrounds (XGBoost).
## -detrend_args (list) list of additional arguments to pass to the specified detrending method
##
## Taylor Grimm
## October 18th, 2023
#############################################
#############################################
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
