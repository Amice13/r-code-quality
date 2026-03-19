#############################################################################################
# Code to create helper functions for the case study
# Taylor Grimm
# August 16, 2024
#############################################################################################


source('3_helper_functions_independent.R')
load('cs_thresholds.rds')
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
