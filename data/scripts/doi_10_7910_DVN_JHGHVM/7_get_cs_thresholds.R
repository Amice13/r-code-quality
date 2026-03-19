##########################################
# Function to get threshold estimates for
# the water treatment data for various
# training period sizes. (assumes AR(1) correlation)
# Taylor Grimm
# September 21st, 2023
##########################################

get_thresholds <- function(train_size = 100) {
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
  endIC <- train_size
  
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
begin_monitoring <- train_size + 1

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
  COV <- matrix(phi^abs(xy[, 1] - xy[, 2]), nrow = n, ncol = n)
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

phihat_t2 <- max(0, ar.yw(mon_stats$T2s, order.max = 1)$ar)
phihat_spe <- ar.yw(mon_stats$SPEs, order.max = 1)$ar
neff_t2 <- effective_size(length(mon_stats$T2s), phihat_t2)
neff_spe <- effective_size(length(mon_stats$T2s), phihat_spe)


thresh_silverman <- threshold(mon_stats, method = 'silverman', alpha = .005)
thresh_scott <- threshold(mon_stats, method = 'scott', alpha = .005)
thresh_boot <- threshold(mon_stats, method = 'boot', alpha = .005)
thresh_raw <- threshold(mon_stats, method = 'raw', alpha = .005)
thresh_silverman_adj <- threshold(mon_stats, method = 'silverman_adj', alpha = .005)
thresh_scott_adj <- threshold(mon_stats, method = 'scott_adj', alpha = .005)
thresh_mbb <- threshold(mon_stats, method = 'mb_boot', alpha = .005)
thresh_rb <- threshold(mon_stats, method = 'rb_boot', alpha = .005)

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


# Return estimates thresholds
list(train_size = train_size,
     phihat_t2 = phihat_t2, 
     phihat_spe = phihat_spe,
     neff_t2 = neff_t2,
     neff_spe = neff_spe,
     parametric_t2 = parametric_t2,
     parametric_spe = parametric_spe,
     sample_quantile_t2 = thresh_raw$T2_threshold,
     sample_quantile_spe = thresh_raw$SPE_threshold,
     SLVM_t2 = thresh_silverman$T2_threshold,
     SLVM_spe = thresh_silverman$SPE_threshold,
     BOOT_t2 = thresh_boot$T2_threshold,
     BOOT_spe = thresh_boot$SPE_threshold,
     `ADJ-SLVM_t2` = thresh_silverman_adj$T2_threshold,
     `ADJ-SLVM_spe` = thresh_silverman_adj$SPE_threshold,
     SCOTT_t2 = thresh_scott$T2_threshold,
     SCOTT_spe = thresh_scott$SPE_threshold,
     `ADJ-SCOTT_t2` = thresh_scott_adj$T2_threshold,
     `ADJ-SCOTT_spe` = thresh_scott_adj$SPE_threshold,
     `MB-BOOT_t2` = thresh_mbb$T2_threshold,
     `MB-BOOT_spe` = thresh_mbb$SPE_threshold,
     `RB-BOOT_t2` = thresh_rb$T2_threshold,
     `RB-BOOT_spe` = thresh_rb$SPE_threshold)
}