##########################################
# Code to create helper functions to perform
# MSPM and visualize results
# Taylor Grimm
# October 9th, 2023
##########################################


filter_uf <- function(UF = 1, min_date = '2021-04-02', max_date = '2021-04-13') {
  if(UF == 1) {
    uf_use <- uf1_ops_15min
  } else if(UF == 2) {
    uf_use <- uf2_ops_15min
  } else if(UF == 3) {
    uf_use <- uf3_ops_15min
  }
  uf_use |>
    filter(Status == 4) |> 
    filter(t_stamp >= as.POSIXct(min_date, tz = 'America/Los_Angeles') &
             t_stamp <= as.POSIXct(max_date, tz = 'America/Los_Angeles')) |> 
    select(!Status)
}

get_resid_mat <- function(explanatory, response) {
  resid_mat <- matrix(nrow = nrow(explanatory), ncol = ncol(response))
  colnames(resid_mat) <- colnames(response)
  for(i in 1:ncol(response)) {
    df <- cbind(response[,i], explanatory)
    colnames(df)[1] <- 'response'
    mod <- lm(response ~ ., data = df)
    resid_mat[, i] <- resid(mod)
  }
  resid_mat
}

# Force the model to only keep 2 PC's (since there are only 3 total with 3 monitoring variables)
get_mon_stats <- function (train_data, var_amnt = 0.8) {
  RR <- cor(train_data)
  eigen_RR <- eigen(RR)
  lambda <- eigen_RR$values
  PP <- eigen_RR$vectors
  prop_var <- as.matrix(cumsum(lambda)/sum(lambda) * 100)
  if (var_amnt != 1) {
    #comps <- which(prop_var - (var_amnt * 100) > 0)[1]
    comps <- 2 #############################
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
       T2s = T2s,
       PP = PP)
}


# Function for initial PCA for a given UF and date range
initial_pca <- function(UF = 1, min_date = '2021-04-02', max_date = '2021-04-13') {
  uf_initial <- filter_uf(UF, min_date = min_date, max_date = max_date)
  t_stamps <- uf_initial |> pull(t_stamp)
  uf_no_valves <- uf_initial |> select(!c(t_stamp, contains("Sts")))
  
  uf_means <- apply(uf_no_valves, 2, mean)
  uf_sds <- apply(uf_no_valves, 2, sd)
  
  uf_initial_scaled <- scale(uf_no_valves, center = uf_means, scale = uf_sds)
  uf_initial_scaled <- cbind(uf_initial_scaled, uf_initial |> select(contains("Sts")))
  
  if(UF == 1) {
    explanatory_vars <- explanatory_vars1
    response_vars <- response_vars1
  } else if(UF == 2) {
    explanatory_vars <- explanatory_vars2
    response_vars <- response_vars2
  } else if(UF == 3) {
    explanatory_vars <- explanatory_vars3
    response_vars <- response_vars3
  }
  # Include all 3 valves as explanatory variables
  uf_raw_explanatory <- uf_initial |> select(all_of(explanatory_vars))
  uf_raw_response <- uf_initial |> select(all_of(response_vars))
  uf_scaled_explanatory <- uf_initial_scaled |> select(all_of(explanatory_vars))
  uf_scaled_response <- uf_initial_scaled |> select(all_of(response_vars))
  
  uf_resids <- get_resid_mat(uf_scaled_explanatory, uf_scaled_response)
  
  uf_mon <- get_mon_stats(uf_resids)
  
  list(mon_obj = uf_mon,
       resids = uf_resids,
       t_stamps = t_stamps,
       raw_explanatory = uf_raw_explanatory,
       raw_response = uf_raw_response)
}


# Time periods: april 2nd 2021 - april 13th 2021
# or april 27th 2022 - may 11th 2022
get_detrend_args <- function(UF = 1, min_date = '2021-04-02', max_date = '2021-04-13') {
  if(UF == 1) {
    uf_with_status_15min <- uf1_with_status_15min |> filter(Status == 4)
    explanatory_vars <- explanatory_vars1
    response_vars <- response_vars1
  } else if(UF == 2) {
    uf_with_status_15min <- uf2_with_status_15min |> filter(Status == 4)
    explanatory_vars <- explanatory_vars2
    response_vars <- response_vars2
  } else if(UF == 3) {
    uf_with_status_15min <- uf3_with_status_15min |> filter(Status == 4)
    explanatory_vars <- explanatory_vars3
    response_vars <- response_vars3
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
  } else if (UF == 2) {
    uf_ops_15min <- uf2_with_status_15min |> filter(Status == 4)
  } else if (UF == 3) {
    uf_ops_15min <- uf3_with_status_15min |> filter(Status == 4)
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

detrended_pca <- function(UF = 1,
                          min_date = '2021-04-02',
                          max_date = '2021-04-13',
                          detrend = 'adaptive_lasso',
                          tuning = 'manual',
                          detrend_args = list) {
  
  uf_detrended <- detrended_lvmwd(UF = UF, min_date = min_date, max_date = max_date,
                                  tuning = tuning, detrend = detrend, detrend_args = detrend_args)
  
  uf_resids <- uf_detrended$detrended_data
  t_stamps <- uf_detrended$data_response[,1] |>
    names() |>
    as_datetime(tz = 'America/Los_Angeles')
  
  train_t_stamps <- t_stamps[t_stamps >= min_date & t_stamps <= max_date]
  test_t_stamps <- t_stamps[t_stamps > max_date]
  
  train_resids <- uf_resids[t_stamps >= min_date & t_stamps <= max_date, ]
  test_obs <- uf_resids[t_stamps > max_date, ]
  
  uf_mon <- get_mon_stats(train_resids, var_amnt = 0.75)
  
  
  PP_k <- uf_mon$PP_k
  lambda_inv <- uf_mon$lambda_inv
  yy <- test_obs %*% PP_k
  yy_hat <- yy %*% t(PP_k)
  EE <- test_obs - yy_hat
  SPE <- diag(EE %*% t(EE))
  T2 <- diag(yy %*% lambda_inv %*% t(yy))
  
  silverman_threshold_t2 <- BMS:::quantile.density(density(uf_mon$T2s, bw = 'nrd0'),
                                                   probs = 0.995)
  silverman_threshold_spe <- BMS:::quantile.density(density(uf_mon$SPEs, bw = 'nrd0'),
                                                    probs = 0.995)
  
  list(mon_obj = uf_mon,
       t2_threshold = silverman_threshold_t2,
       spe_threshold = silverman_threshold_spe,
       train_T2 = uf_mon$T2s,
       train_SPE = uf_mon$SPEs,
       test_T2 = T2,
       test_SPE = SPE,
       uf_resids = uf_resids,
       train_t_stamps = train_t_stamps,
       test_t_stamps = test_t_stamps,
       all_t_stamps = t_stamps,
       best_tune = uf_detrended$best_tune)
}

# Function to create plots of the T2 and SPE monitoring statistics for a selected
# UF, time period, and detrending method.
plot_detrended_pca <- function(UF = 1,
                               min_date = '2021-04-02',
                               max_date = '2021-04-13',
                               tuning = 'manual',
                               detrend = 'adaptive_lasso',
                               detrend_args = list) {
  uf_pca <- detrended_pca(UF = UF, min_date = min_date, max_date = max_date,
                          tuning = tuning, detrend = detrend, detrend_args = detrend_args)
  
  train_length <- length(uf_pca$train_T2)
  test_length <- length(uf_pca$test_T2)
  
  train_t_stamps <- uf_pca$train_t_stamps
  test_t_stamps <- uf_pca$test_t_stamps
  
  par(mfrow=c(2,1), mar = c(4,4,2,2))
  # Plot T2
  plot(c(train_t_stamps, test_t_stamps[1:1000]), c(uf_pca$train_T2, uf_pca$test_T2[1:1000]),
       type = 'l', col = 'gray',
       xlab = 'Time',
       ylab = expression(T^2),
       main = paste0('UF', UF, ' Monitoring Statistics, Detrend: ', detrend))
  lines(test_t_stamps[1:1000], uf_pca$test_T2[1:1000], type = 'l')
  abline(h = uf_pca$t2_threshold, col = 'red', lwd = .5, lty = 2)
  
  # Plot SPE
  plot(c(train_t_stamps, test_t_stamps[1:1000]), c(uf_pca$train_SPE, uf_pca$test_SPE[1:1000]),
       type = 'l', col = 'gray',
       xlab = 'Time',
       ylab = 'SPE')
  lines(test_t_stamps[1:1000], uf_pca$test_SPE[1:1000], type = 'l')
  abline(h = uf_pca$spe_threshold, col = 'red', lwd = .5, lty = 2)
  
  astsa::acf2(uf_pca$train_T2, main = paste0('UF', UF, ' T2, Detrend: ', detrend))
  astsa::acf2(uf_pca$train_SPE, main = paste0('UF', UF, ' SPE, Detrend: ', detrend))
  par(mfrow=c(1,1), mar = c(5,4,4,2) + 0.01)
}

# Function to create plots (log) of the T2 and SPE monitoring statistics for a selected
# UF, time period, and detrending method.
plot_detrended_pca_log <- function(UF = 1,
                                   min_date = '2021-04-02',
                                   max_date = '2021-04-13',
                                   tuning = 'manual',
                                   detrend = 'adaptive_lasso',
                                   detrend_args = list) {
  uf_pca <- detrended_pca(UF = UF, min_date = min_date, max_date = max_date,
                          tuning = tuning, detrend = detrend, detrend_args = detrend_args)
  
  train_length <- length(uf_pca$train_T2)
  test_length <- length(uf_pca$test_T2)
  
  train_t_stamps <- uf_pca$train_t_stamps
  test_t_stamps <- uf_pca$test_t_stamps
  
  detrend_title <- ''
  if(detrend == 'none') {
    detrend_title <- "No Detrending"
  } else if(detrend == 'adaptive_lasso') {
    detrend_title <- "Adaptive Lasso"
  } else if(detrend == 'knn') {
    detrend_title <- "K-Nearest Neighbors"
  } else if(detrend == 'rf') {
    detrend_title <- "Random Forest"
  } else if(detrend == 'xgboost') {
    detrend_title <- "XGBoost"
  }
  
  par(mfrow=c(2,1), mar = c(1,4,3,2))
  # Plot T2
  plot(c(train_t_stamps, test_t_stamps[1:1000]), c(log(uf_pca$train_T2), log(uf_pca$test_T2[1:1000])),
       type = 'l', col = 'gray',
       xlab = '',
       ylab = expression(log(T^2)),
       main = paste0('UF ', UF, ' Monitoring Statistics: ', detrend_title),
       cex.main = 1.5,
       ylim = c(-15, 20))
  lines(test_t_stamps[1:1000], log(uf_pca$test_T2[1:1000]), type = 'l')
  abline(h = log(uf_pca$t2_threshold), col = 'purple', lwd = 2, lty = 2)
  abline(v = test_t_stamps[1], col = 'red', lwd = 2, lty = 2)
  legend('topleft', bty = 'n',
         legend = c("Training Data", "Testing Data", "Threshold"), lty = c(1, 1, 2),
         col = c('gray', 'black', 'purple'))
  par(mar = c(2,4,2,2))
  # Plot SPE
  plot(c(train_t_stamps, test_t_stamps[1:1000]), c(log(uf_pca$train_SPE), log(uf_pca$test_SPE[1:1000])),
       type = 'l', col = 'gray',
       xlab = '',
       ylab = 'log(SPE)',
       cex.main = 1.5,
       ylim = c(-30, 20))
  lines(test_t_stamps[1:1000], log(uf_pca$test_SPE[1:1000]), type = 'l')
  abline(h = log(uf_pca$spe_threshold), col = 'purple', lwd = 2, lty = 2)
  abline(v = test_t_stamps[1], col = 'red', lwd = 2, lty = 2)
  
  # astsa::acf2(uf_pca$train_T2, main = paste0('UF', UF, ' T2: ', detrend_title))
  # astsa::acf2(uf_pca$train_SPE, main = paste0('UF', UF, ' SPE: ', detrend_title))
  
  # par(mfrow=c(1,1), mar = c(5,4,4,2) + 0.01)
}

plot_detrended_pca_log(UF = 1, min_date = '2022-04-27', max_date = '2022-05-11', detrend = 'none')
plot_detrended_pca_log(UF = 1, min_date = '2022-04-27', max_date = '2022-05-11', detrend = 'adaptive_lasso')

# Function to plot the time series of the original monitoring variable (plot_var) values
# along with the residuals from the detrending model.
plot_original_and_resids <- function(UF = 1,
                                     plot_var = 1,
                                     min_date = '2021-04-02',
                                     max_date = '2021-04-13',
                                     tuning = 'manual',
                                     detrend = 'adaptive_lasso',
                                     detrend_args = list,
                                     ylims = NULL) {
  uf_detrended <- detrended_lvmwd(UF = UF,
                                  min_date = min_date,
                                  max_date = max_date,
                                  tuning = tuning,
                                  detrend = detrend,
                                  detrend_args = detrend_args)
  
  t_stamps <- uf_detrended$data_response[,plot_var] |>
    names() |>
    as_datetime(tz = 'America/Los_Angeles')
  
  resp_vars <- c("Temp-Corrected Permeability", "Filtrate Turbidity", "Filtrate Ammonia")
  
  detrend_title <- ''
  if(detrend == 'none') {
    detrend_title <- "No Detrending"
  } else if(detrend == 'adaptive_lasso') {
    detrend_title <- "Adaptive Lasso"
  } else if(detrend == 'knn') {
    detrend_title <- "K-Nearest Neighbors"
  } else if(detrend == 'rf') {
    detrend_title <- "Random Forest"
  } else if(detrend == 'xgboost') {
    detrend_title <- "XGBoost"
  }
  
  var_units <- c("gfd/psi", "mNTU", "mg/L N")
  
  min_datetime <- as_datetime(min_date, tz = 'America/Los_Angeles')
  max_datetime <- as_datetime(max_date, tz = 'America/Los_Angeles')
  plot_indices <- which(t_stamps >= min_date & t_stamps <= (max_datetime + 60*60*24*10))
  plot_response <- uf_detrended$data_response[plot_indices, plot_var]
  plot_resids <- plot_response - uf_detrended$fitted[plot_indices, plot_var]
  
  if (is.null(ylims)) {
    ylims = c(min(c(plot_response, plot_resids)), max(c(plot_response, plot_resids)))
  }
  
  par(mfrow=c(1,1), mar = c(3, 4, 2.5, 1.5))
  plot(t_stamps[plot_indices], plot_response, type = 'l',
       xlab = "",
       ylab = var_units[plot_var],
       main = paste0("UF ", UF, " ", resp_vars[plot_var], ": ", detrend_title),
       xlim = c(min_datetime, max_datetime + 60*60*24*10), # from min date to 10 days ahead
       ylim = ylims,
       cex.main = 1.25)
  lines(t_stamps[plot_indices], plot_resids, col = 'blue')
  abline(v = max_datetime, col = 'red', lty = 2, lwd = 2)
  legend('topleft', col = c("black", "blue"), lty = 1, legend = c("Original Data", "Model Residuals"), bty = 'n')
}

# Function to plot the time series of a specified monitoring variable
# during a specific period of time.
# ylimits need to be adjusted manually within the funciton in order to zoom in
plot_original_responses <- function(UF = 1,
                                    min_date = '2021-04-02',
                                    max_date = '2021-04-13',
                                    plot_var = NULL) {
  uf_detrended <- detrended_lvmwd(UF = UF,
                                  min_date = min_date,
                                  max_date = max_date,
                                  detrend = 'adaptive_lasso',
                                  detrend_args = list)
  # par(mfrow=c(3,1), mar = c(3,4,3,2))
  par(mar = c(3, 4, 2.5, 1.5))
  #  par(mfrow=c(1,1), mar = c(2, 3, 1.75, 1))
  #  for(plot_var in 1:3) {
  t_stamps <- uf_detrended$data_response[,plot_var] |>
    names() |>
    as_datetime(tz = 'America/Los_Angeles')
  
  resp_vars <- c("Temperature Corrected Permeability", "Filtrate Turbidity", "Filtrate Ammonia")
  var_units <- c("gfd/psi", "mNTU", "mg/L N")
  
  min_datetime <- as_datetime(min_date, tz = 'America/Los_Angeles')
  max_datetime <- as_datetime(max_date, tz = 'America/Los_Angeles')
  plot_indices <- which(t_stamps >= min_date & t_stamps <= (max_datetime + 60*60*24*10))
  plot_response <- uf_detrended$data_response[plot_indices, plot_var]
  
  plot(t_stamps[plot_indices], plot_response, type = 'l',
       #xlab = paste0("Date (", year(max_datetime), ")"),
       #   ylim=c(10,60),
       xlab = '',
       ylab = var_units[plot_var], # resp_vars[plot_var],
       main = paste0("UF ", UF, " ", resp_vars[plot_var]),
       xlim = c(min_datetime, max_datetime + 60*60*24*10)) # from min date to 10 days ahead 
  abline(v = max_datetime, col = 'red', lty = 2)
  # }
  # par(mfrow=c(1,1), mar = c(5,4,4,2) + .1)
}

