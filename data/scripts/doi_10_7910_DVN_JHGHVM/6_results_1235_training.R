##########################################
# Code to get threshold estimates for
# the water treatment data for the original
# training period.
# Taylor Grimm
# March 20th, 2024
##########################################

# NOTE: The majority of the code below comes
# from "Fault isolation for a complex decentralized
# waste water treatment facility" by Klanderman et al.

library(tidyverse)
library(glmnet) ## adaptive lasso
library(lubridate) ## working with POSIXct objects


## read in data
phfault.NAs <- read.csv("case_study_clean.csv", 
                        header = T, stringsAsFactors = F)
## convert date column to POSIXct object
phfault.NAs$date <- as.POSIXct(phfault.NAs$date, 
                               format = "%Y-%m-%d %H:%M:%S")

## Indicator of where NAs are located in the dataset
start_largeNA <- which(phfault.NAs$date==as.POSIXct("2010-04-19T08:10:00", 
                                                    format = "%Y-%m-%dT%H:%M:%S"))
end_largeNA <- which(phfault.NAs$date==as.POSIXct("2010-04-20T01:00:00", 
                                                  format = "%Y-%m-%dT%H:%M:%S"))

## remove observations with large spike
start_outliers <- which(phfault.NAs$date==as.POSIXct("2010-04-16T12:50:00", 
                                                     format = "%Y-%m-%dT%H:%M:%S"))
end_outliers <- which(phfault.NAs$date==as.POSIXct("2010-04-16T16:10:00", 
                                                   format = "%Y-%m-%dT%H:%M:%S"))

##########################################
## Cyclical trends
##########################################
## extract hour, minute, and 2 hour from data
phfault.NAs$hour <- sapply(phfault.NAs$date, hour)
phfault.NAs$minute <- sapply(phfault.NAs$date, minute)
phfault.NAs$hour2cycle <- sapply(1:nrow(phfault.NAs), function(row){
  min <- ifelse(phfault.NAs$hour[row]%%2==1, 
                phfault.NAs$minute[row]+60, phfault.NAs$minute[row])
  return(min)
})

## daily trend
phfault.NAs$cos_daily <- sapply(phfault.NAs$hour, function(hour){
  return(cos(2*pi*hour/24))
})
phfault.NAs$sin_daily <- sapply(phfault.NAs$hour, function(hour){
  return(sin(2*pi*hour/24))
})
## hourly trend
phfault.NAs$cos_hourly <- sapply(phfault.NAs$minute, function(minute){
  return(cos(2*pi*minute/60))
})
phfault.NAs$sin_hourly <- sapply(phfault.NAs$minute, function(minute){
  return(sin(2*pi*minute/60))
})
## 2 hour trend
phfault.NAs$cos_2hour <- sapply(phfault.NAs$hour2cycle, function(hour2cycle){
  return(cos(2*pi*hour2cycle/120))
})
phfault.NAs$sin_2hour <- sapply(phfault.NAs$hour2cycle, function(hour2cycle){
  return(sin(2*pi*hour2cycle/120))
})

###########################################
## decompose variables into 3 categories
## as described in Table A6 in the Appendix
###########################################
## cyclic variables
cyclical_trends <- c("cos_daily", "sin_daily", "cos_2hour", "sin_2hour", 
                     "cos_hourly", "sin_hourly")
##control variables
control_vars <- c("bio_1_blow_flow", "bio_2_blow_flow", "mbr_1_tmp", "mbr_2_tmp",
                  "ambient_temp", "bio_1_phase1", "bio_1_phase2", "bio_2_phase1",
                  "bio_2_phase2", "mbr_1_mode1", "mbr_1_mode2", "mbr_1_mode4",
                  "mbr_2_mode1", "mbr_2_mode2", "mbr_2_mode4")
## response variables
response_vars <- c("mbr_1_perm_flow", "mbr_2_perm_flow", "ras_temp",
                   "bio_1_do", "bio_2_do", "mbr_1_level", "mbr_2_level", 
                   "perm_turb", "sewage_flow", "bio_1_level", "bio_2_level",
                   "bio_1_temp", "bio_2_temp", "bio_1_tss", "bio_2_tss", 
                   "perm_tank_level", "ras_do", "ras_ph", "perm_cond", "ras_tss")

####################################
## Remove cyclical trends from data
## before we select best lag
####################################
## create data set with no NAs
phfault <- phfault.NAs[complete.cases(phfault.NAs), ]

## index of last observation before NAs begin on 4/16
index_noNAs <- which(phfault$date==as.POSIXct("2010-04-16T12:40:00",
                                              format = "%Y-%m-%dT%H:%M:%S"))
residuals <- matrix(rep(NA, index_noNAs*length(response_vars)), 
                    nrow = index_noNAs)
for(i in 1:length(response_vars)){
  cyclictrend <- paste(response_vars[i],  
                       "~ sin(2*pi*hour/24) + cos(2*pi*hour/24)",
                       "+ sin(2*pi*minute/60) + cos(2*pi*minute/60)",
                       "+ sin(2*pi*hour2cycle/120) + cos(2*pi*hour2cycle/120)")
  cyclictrend <- lm(as.formula(cyclictrend), data = phfault[1:index_noNAs,])
  residuals[,i] <- residuals(cyclictrend)
}

residuals <- as.data.frame(residuals)
names(residuals) <- response_vars

###################################
## choose best lag for control vars
## based on response vars with 
## cyclical trends removed
###################################
resp.control.combos <- expand.grid(response_vars, control_vars, 
                                   stringsAsFactors = F)
names(resp.control.combos) <- c("response", "control")
## identify lag at which the PACF is maximized for each combination 
## of response and control variable
resp.control.combos$best.lag <- sapply(1:nrow(resp.control.combos), function(var){
  resp <- resp.control.combos[var, 1]
  control <- resp.control.combos[var, 2]
  mv.pacf <- pacf(ts(cbind(residuals[, resp], phfault[1:index_noNAs, control])), 
                  plot = FALSE, na.action = na.pass)  
  return(which.max(mv.pacf$acf[,1,2]))  
})
## maximum lag used (used to remove NAs)
max.lag <- max(resp.control.combos$best.lag)

##########################################
## A few necessary functions
###########################################

##############################
## function calculated R^2
##############################
## yy (numeric vector) is the observed data
## yhat (numeric vector) is the fitted data
r_squared <- function(yy, yhat) {
  ybar <- mean(yy)
  ## Total SS
  ss_tot <- sum((yy - ybar)^2)
  ## Residual SS
  ss_res <- sum((yy - yhat)^2)
  ## R^2 = 1 - ss_res/ ss_tot
  1 - (ss_res / ss_tot)
}

###############################################################
## function returns coefficients for a glmnet object
###############################################################
## coef_obj is a list of coefficients from a glmnet object
get.coefs <- function(coef_obj){
  coefs <- rep(0, coef_obj@Dim[1]-1)
  for(i in 2:length(coef_obj@i)){
    index <- coef_obj@i[i]
    coefs[index] <- coef_obj@x[i]
  }
  return(coefs)
}

###################################################
## function detrends data using adaptive lasso
## and returns a list of information about the fit
###################################################
## resp_all (numeric vector) is the response variable during IC and monitoring period
## pred_all (numeric matrix) is the matrix of predictor variables during IC and monitoring period
## IC_indices (numeric vector) is the indices corresponding to the time points 
## at which the data are IC for resp_all and pred_all
alasso_fit <- function(resp_all, pred_all, IC_indices){
  ## data from IC period
  pred_IC <- pred_all[IC_indices, ]
  resp_IC <- resp_all[IC_indices]
  ## standardize Y
  resp_IC_scaled <- scale(resp_IC)
  mean_resp <- attr(resp_IC_scaled, "scaled:center")
  sd_resp <- attr(resp_IC_scaled, "scaled:scale")
  resp_all_scaled <- scale(resp_all, scale = sd_resp, center = mean_resp)
  ## standardize X
  pred_IC_scaled <- scale(pred_IC)
  mean_pred <- attr(pred_IC_scaled, "scaled:center")
  sd_pred <- attr(pred_IC_scaled, "scaled:scale")
  pred_all_scaled <- scale(pred_all, scale = sd_pred, center = mean_pred)
  ## get initial estimates using ridge regression
  ridge_cv <- cv.glmnet(x = pred_IC_scaled, y = resp_IC_scaled, 
                        alpha = 0, standardize = FALSE)
  best_ridge_coef <- as.numeric(coef(ridge_cv, s = ridge_cv$lambda.min))[-1]
  ## adaptive lasso using initial estimates above
  alasso <- glmnet(x = pred_IC_scaled, y = resp_IC_scaled, alpha = 1,
                   penalty.factor = 1 / abs(best_ridge_coef))
  ## use k-fold cross validation to choose regularization parameter, lambda
  ## if you do not set the seed, your choice of lambda will vary each time
  set.seed(1586)
  alasso_cv <- cv.glmnet(x = pred_IC_scaled, y = resp_IC_scaled, alpha = 1,
                         penalty.factor = 1 / abs(best_ridge_coef))
  ## extract coefficients using lambda with minimum cross-validation error
  best_alasso_coef <- get.coefs(coef(alasso_cv, s = alasso_cv$lambda.min))
  names(best_alasso_coef) <- names(pred_all)
  ## get results on original scale
  fitted_all <- sd_resp*predict(alasso, 
                                newx = pred_all_scaled, 
                                s = alasso_cv$lambda.min) + mean_resp
  ## calculate residuals
  residuals_all <- resp_all - fitted_all
  ## R^2
  r_squared_alasso <- r_squared(resp_IC, fitted_all[IC_indices])
  return(list("alasso_coef" = best_alasso_coef, 
              "fitted_all" = fitted_all, 
              "residuals_all" = residuals_all, 
              "r2" = r_squared_alasso, 
              "resp_all" = resp_all))
}

################################################################
## function to get lagged variable based on time series and lag
################################################################
## ts (numeric vector) is a time series
## lag (int > 0) is the lag of interest
get.lagged.var <- function(ts, lag){
  lagged.var <- c(rep(NA, lag), ts[1:(length(ts)-lag)])  
  return(lagged.var)
}

####################################################
## Detrend variables using
## 1.) control variables and lagged control variables
## 2.) cyclical trends--daily, hourly, and 2 hour
####################################################
alasso.residuals <- NULL
alasso.coef <- NULL
alasso.all <- NULL

for(i in 1:length(response_vars)){
  var <- response_vars[i]
  resp.var <- phfault.NAs[,var]
  
  ## create data frame to hold lagged control variables
  lagged.control <- as.data.frame(matrix(rep(NA, length(control_vars)*nrow(phfault.NAs)), 
                                         ncol = length(control_vars)))
  names(lagged.control) <- paste(control_vars, "l", sep = ".")
  ## get lagged control variables based on lag chosen using PACF and add to data frame
  for(j in 1:length(control_vars)){
    control.lag <- resp.control.combos$best.lag[resp.control.combos$response==var&resp.control.combos$control==control_vars[j]]
    control.var <- phfault.NAs[,control_vars[j]]
    lagged.control[,j] <- get.lagged.var(control.var, control.lag)
  }
  ## combine response variable and all control variables (cyclic, control, and lagged control)
  all.vars <- cbind.data.frame(resp.var, phfault.NAs[,c(cyclical_trends, control_vars, "date")],
                               lagged.control)
  
  ## remove all variables with NAs
  index_allNAs <- c(1:max.lag, start_outliers:(end_outliers+max.lag), start_largeNA:(end_largeNA+max.lag))
  all.vars.complete <- all.vars[-index_allNAs, ]
  
  ## index that separates monitoring period from IC period
  endIC <- which(all.vars.complete$date==as.POSIXct("2010-04-19T08:00:00",
                                                    format = "%Y-%m-%dT%H:%M:%S"))
  
  ## all predictors
  pred_all <- as.matrix(all.vars.complete[, !names(all.vars.complete)%in%c("resp.var", "date")])
  ## response
  resp_all <- all.vars.complete$resp.var
  ## detrend data using adaptive lasso
  alasso <- alasso_fit(resp_all, pred_all, 1:endIC)
  
  # Save alasso results
  alasso.all[[i]] <- alasso
  alasso.residuals[[i]] <- alasso$residuals_all
  alasso.coef[[i]] <- alasso$alasso_coef
}  

alasso.residuals.df <- as.data.frame(alasso.residuals)
names(alasso.residuals.df) <- response_vars
alasso.residuals.df$date <- all.vars.complete$date

## get index where monitoring period begins
begin_monitoring <- which(alasso.residuals.df$date>=as.POSIXct("2010-04-20T01:10:00", 
                                                               format = "%Y-%m-%dT%H:%M:%S"))[1]

## get residuals for response variables for IC period only
residual_IC <- alasso.residuals.df[1:(begin_monitoring-1), ]
residual_IC_mat <- as.matrix(residual_IC[,response_vars])

## get residuals for response variables for monitoring period only
residual_mon <- alasso.residuals.df[begin_monitoring:nrow(alasso.residuals.df), ]
residual_mon_mat <- as.matrix(residual_mon[,response_vars])

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
threshold <- function (mon_stat_obj, 
                       method = "sj", 
                       alpha = 0.005) 
{
  
  get_threshold <- function (mon_stat, 
                             method = "sj", 
                             alpha = 0.005) 
  {
    phi_hat <- arima(mon_stat, order = c(1,0,0), method = 'CSS-ML')$coef[1]
    n_eff <- function(phi_hat, n = NULL) {
      # Compute effective sample size for adjusted KDE bandwidths (adjusting for dependence)
      n_eff <- effective_size(n = n, phi = phi_hat)
      l <- ceiling(log(.05) / log(abs(phi_hat)))
      l_use <- min(l, n/2)
      list(n_eff = n_eff, l = l_use)
    }
    neff_and_l <- n_eff(phi_hat, n = length(mon_stat))
    neff_hat <- neff_and_l$n_eff
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
    } else if (method == 'scott_adj') {
      scott_bw <- function(x, n_eff = NULL) {
        if(is.null(n_eff)) n_eff <- length(x)
        1.06*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
        
      }
      bw <- scott_bw(mon_stat, n_eff = neff_hat)
      mon_stat_dens <- density(mon_stat, bw = bw, kernel = 'gaussian')
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, c(1 - alpha))
      
    } else if (method == 'silverman_adj') {
      silverman_bw <- function(x, n_eff = NULL) {
        if(is.null(n_eff)) n_eff <- length(x)
        0.9*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
      }
      bw <- silverman_bw(mon_stat, n_eff = neff_hat)
      mon_stat_dens <- density(mon_stat, bw = bw, kernel = 'gaussian')
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, c(1 - alpha))
      
    } else if (method == 'mb_boot') {
      boot_mbb_stat <- function(series) series |> quantile(probs = 1 - alpha)
      mon_stat_threshold <- boot::tsboot(tseries = mon_stat, statistic = boot_mbb_stat,
                                         R = 1000, sim = 'fixed', l = neff_and_l$l)$t |> mean()
    } else if (method == 'rb_boot') {
      boot_rb_stat <- function(series) series |> quantile(probs = 1 - alpha)
      mon_stat_threshold <- boot::tsboot(tseries = mon_stat, statistic = boot_rb_stat,
                                         R = 1000, sim = 'geom', l = neff_and_l$l)$t |> mean()
    } else {
      stop('That is not a valid method option. Choose "sj", "silverman", "scott", or "boot".')
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
fault_detect_new <- function (threshold_obj, 
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
  
  
  
  object <- cbind(SPE, rep(SPE_threshold, length(SPE_flag)), SPE_flag,
                  T2, rep(T2_threshold, length(T2_threshold)), T2_flag)
  colnames(object) <- c("SPE", "SPE_threshold", "SPE_flag", "T2", "T2_threshold", "T2_flag")
  object |> as_tibble()
}
effective_size <- function(n, phi) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix(phi^abs(xy[, 1] - xy[, 2]) / (1-phi^2), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}

##############################################################################################
### Center and scale the residual matrix from madel fit, then compute monitoring statistics
##############################################################################################

IC_center <- colMeans(residual_IC_mat)
IC_sd <- apply(residual_IC_mat, 2, sd)
mon_stats <- get_mon_stats(residual_IC_mat |> scale(center = IC_center, scale = IC_sd))
threshold_obj <- threshold(mon_stats, method = 'silverman', alpha = .005)

mon_mat_scaled <- residual_mon_mat |> scale(center = IC_center, scale = IC_sd)
fault_detect_results <- fault_detect_new(threshold_obj = threshold_obj, test_obs = mon_mat_scaled)

##############################################################################################
####### Get thresholds for different estimators ##############################################
##############################################################################################

# Obtain estimated phi and effective size (assuming AR(1)) for T2 and SPE
effective_size_arma <- function(n, phi, theta) {
  x <- 1:n
  y <- 1:n
  xy <- expand.grid(x=x,y=y)
  COV <- matrix((((1 + theta*phi)*(phi + theta)) / (1 - phi^2))*phi^abs(xy[, 1] - xy[, 2]), nrow = n, ncol = n)
  in.brackets <- 1 + (sum(COV) - sum(diag(COV))) / n
  floor(n/(in.brackets))
}
phihat_t2 <- ar.yw(mon_stats$T2s, order.max = 1)$ar
phihat_spe <- ar.yw(mon_stats$SPEs, order.max = 1)$ar
neff_t2 <- effective_size(length(mon_stats$T2s), phihat_t2)
neff_spe <- effective_size(length(mon_stats$T2s), phihat_spe)

# Obtain nonparametric threshold estimates for each estimator
thresh_silverman <- threshold(mon_stats, method = 'silverman', alpha = .005)
thresh_scott <- threshold(mon_stats, method = 'scott', alpha = .005)
thresh_boot <- threshold(mon_stats, method = 'boot', alpha = .005)
thresh_raw <- threshold(mon_stats, method = 'raw', alpha = .005)
thresh_silverman_adj <- threshold(mon_stats, method = 'silverman_adj', alpha = .005)
thresh_scott_adj <- threshold(mon_stats, method = 'scott_adj', alpha = .005)
thresh_mbb <- threshold(mon_stats, method = 'mb_boot', alpha = .005)
thresh_rb <- threshold(mon_stats, method = 'rb_boot', alpha = .005)

# Get parametric thresholds for comparison
k <- threshold_obj$PP_k |> ncol()
parametric_t2 <- (k * (length(mon_stats$T2s)^2 - 1)) /
  (length(mon_stats$T2s)*(length(mon_stats$T2s) - k)) *
  qf(.995, k, length(mon_stats$T2s) - k)

spe_mean <- mean(mon_stats$SPEs)# = av
spe_var <- var(mon_stats$SPEs)# = 2a^2v
# a = mean / v
#var = 2*mean^2/v
# v = 2* (mean^2) / var

v <- 2*spe_mean^2 / spe_var
a <- spe_mean / v
parametric_spe <- a*qchisq(.995, v)


##########################################################################################
# ARMA thresholds ########################################################################
##########################################################################################

# Obtain estimated phi and effective size (assuming ARMA(1, 1)) for T2 and SPE
phihat_t2_arma <- arima(mon_stats$T2s, order = c(1, 0, 1), method = 'ML')$coef[1]
thetahat_t2 <- arima(mon_stats$T2s, order = c(1, 0, 1), method = 'ML')$coef[2]
neff_t2 <- effective_size_arma(length(mon_stats$T2s), phi = phihat_t2_arma, theta = thetahat_t2)

phihat_spe_arma <- arima(mon_stats$SPEs, order = c(1, 0, 1), method = 'ML')$coef[1]
thetahat_spe <- arima(mon_stats$SPEs, order = c(1, 0, 1), method = 'ML')$coef[2]
neff_spe <- effective_size_arma(length(mon_stats$SPEs), phi = phihat_spe_arma, theta = thetahat_spe)

threshold_arma <- function (mon_stat_obj, 
                            method = "sj", 
                            alpha = 0.005) 
{
  
  get_threshold <- function (mon_stat, 
                             method = "sj", 
                             alpha = 0.005) 
  {
    arma11_fit <- mon_stat |> as_tibble() |> map(~ arima(.x, order = c(1, 0, 1), method = 'ML')$coef[1:2])
    est_params <- arma11_fit |> bind_rows()
    
    phi_hat <- est_params$ar1
    theta_hat <- est_params$ma1
    
    n_eff <- function(phi_hat, theta_hat, n = NULL) {
      n_eff <- effective_size_arma(n = n, phi = phi_hat, theta = theta_hat)
      acf_vals <- ARMAacf(ar = phi_hat, ma = theta_hat, lag.max = (n/2) + 1)[-1]
      if(any(abs(acf_vals) <= 0.05)) {
        l <- which(abs(acf_vals) <= 0.05)[1]
        l_use <- min(l, n/2)
      } else {
        l_use <- n/2
      }
      n_eff <- max(1, n_eff)
      list(n_eff = n_eff, l = l_use)
    }
    neff_and_l <- n_eff(phi_hat, theta_hat, n = length(mon_stat))
    neff_hat <- neff_and_l$n_eff
      

    #neff_hat <- effective_size_arma(n = length(mon_stat), phi = phi_hat, theta = theta_hat)
    
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
    } else if (method == 'scott_adj') {
      scott_bw <- function(x, n_eff = NULL) {
        if(is.null(n_eff)) n_eff <- length(x)
        1.06*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
        
      }
      bw <- scott_bw(mon_stat, n_eff = neff_hat)
      mon_stat_dens <- density(mon_stat, bw = bw, kernel = 'gaussian')
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, c(1 - alpha))
      
    } else if (method == 'silverman_adj') {
      silverman_bw <- function(x, n_eff = NULL) {
        if(is.null(n_eff)) n_eff <- length(x)
        0.9*(n_eff^(-1/5))*min(sd(x), (quantile(x, .75) - quantile(x, .25))/1.34)
      }
      bw <- silverman_bw(mon_stat, n_eff = neff_hat)
      mon_stat_dens <- density(mon_stat, bw = bw, kernel = 'gaussian')
      mon_stat_threshold <- BMS:::quantile.density(mon_stat_dens, c(1 - alpha))
      
    } else if (method == 'mb_boot') {
      boot_mbb_stat <- function(series) series |> quantile(probs = 1 - alpha)
      mon_stat_threshold <- boot::tsboot(tseries = mon_stat, statistic = boot_mbb_stat,
                   R = 1000, sim = 'fixed', l = neff_and_l$l)$t |> mean()
    } else if (method == 'rb_boot') {
      boot_rb_stat <- function(series) series |> quantile(probs = 1 - alpha)
      mon_stat_threshold <- boot::tsboot(tseries = mon_stat, statistic = boot_rb_stat,
                                         R = 1000, sim = 'geom', l = neff_and_l$l)$t |> mean()
    } else {
      stop('That is not a valid method option. Choose "sj", "silverman", "scott", or "boot".')
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

# Obtain nonparametric threshold estimates for each estimator
thresh_silverman_arma <- threshold_arma(mon_stats, method = 'silverman', alpha = .005)
thresh_scott_arma <- threshold_arma(mon_stats, method = 'scott', alpha = .005)
thresh_boot_arma <- threshold_arma(mon_stats, method = 'boot', alpha = .005)
thresh_raw_arma <- threshold_arma(mon_stats, method = 'raw', alpha = .005)
thresh_silverman_adj_arma <- threshold_arma(mon_stats, method = 'silverman_adj', alpha = .005)
thresh_scott_adj_arma <- threshold_arma(mon_stats, method = 'scott_adj', alpha = .005)
thresh_mbb_arma <- threshold_arma(mon_stats, method = 'mb_boot', alpha = .005)
thresh_rb_arma <- threshold_arma(mon_stats, method = 'rb_boot', alpha = .005)


# Create a tibble to display all thresholds for each monitoring statistic
# and each correlation structure
tibble(`Monitoring Statistic` = c('T2', 'T2', 'SPE', 'SPE'),
       `Correlation Structure` = c("AR(1)", "ARMA(1,1)", "AR(1)", "ARMA(1,1)"),
       `Sample Quantile` = c(thresh_raw$T2_threshold, thresh_raw_arma$T2_threshold,
                thresh_raw$SPE_threshold, thresh_raw_arma$SPE_threshold),
       SLVM = c(thresh_silverman$T2_threshold, thresh_silverman_arma$T2_threshold,
                thresh_silverman$SPE_threshold, thresh_silverman_arma$SPE_threshold),
       SCOTT = c(thresh_scott$T2_threshold, thresh_scott_arma$T2_threshold,
                 thresh_scott$SPE_threshold, thresh_scott_arma$SPE_threshold),
       BOOT = c(thresh_boot$T2_threshold, thresh_boot_arma$T2_threshold,
                thresh_boot$SPE_threshold, thresh_boot_arma$SPE_threshold),
       `ADJ-SLVM` = c(thresh_silverman_adj$T2_threshold, thresh_silverman_adj_arma$T2_threshold,
                       thresh_silverman_adj$SPE_threshold, thresh_silverman_adj_arma$SPE_threshold),
       `ADJ-SCOTT` = c(thresh_scott_adj$T2_threshold, thresh_scott_adj_arma$T2_threshold,
                       thresh_scott_adj$SPE_threshold, thresh_scott_adj_arma$SPE_threshold),
       `MB-BOOT` = c(thresh_mbb$T2_threshold, thresh_mbb_arma$T2_threshold,
                     thresh_mbb$SPE_threshold, thresh_mbb_arma$SPE_threshold),
       `RB-BOOT` = c(thresh_rb$T2_threshold, thresh_rb_arma$T2_threshold,
                     thresh_rb$SPE_threshold, thresh_rb_arma$SPE_threshold))




