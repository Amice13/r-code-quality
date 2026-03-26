sps_simulation <- function(s, total_sim_size, design, HPC = TRUE){

  set.seed((1  + s))

  design_num <- s %% nrow(design)
  if(design_num == 0 ) design_num <- nrow(design)

  num_var <- design[design_num, 1]
  select_var <- seq(from = 1, to = num_var)
  N_s <- design[design_num, 2]
  coef_mul <- design[design_num, 3]
  se_mul <- design[design_num, 4]
  dgp_coef_use <- dgp_coef*coef_mul
  bet_se_use <- bet_se*se_mul
  N <- design[design_num, 5]

  ## DGP
  dgp_data <- dgp_sps_Naumann(for_use = for_use,
                              N = N, dgp_coef = dgp_coef_use,
                              dgp_base = list("X_cov" = X_cov, "bet_se" = bet_se_use, "within_se" = within_se))
  tau_mean <- mean(dgp_data$tau)

  ## #######
  ## SPS
  ## #######
  X_diversify <- dgp_data$X[, select_var]

  sps_out <- sps(X_diversify, N_s = N_s)

  if(any(is.na(sps_out$selected_sites))){
    est_sps <- c(NA, NA)
    sps_p_value_use <- NA
    cat("\n NA in sps. ")
  }else{
    estimate_exp <- cbind(dgp_data$tau[sps_out$internal$ss == 1],
                          dgp_data$se[sps_out$internal$ss == 1])
    rownames(estimate_exp) <- sps_out$selected_sites

    sps_est <- sps_estimator(out = sps_out, estimates_selected = estimate_exp)

    # sps_p_value <- sps_cv(out = sps_out, estimates_selected = estimate_exp)

    est_sps <- sps_est$average_site_ATE
    # sps_p_value_use <- sps_p_value$p_value
  }

  # ##################
  # Random Sampling
  # ##################
  site_rs <- sample(seq(1:N), size = N_s)
  est_tau_rs <- dgp_data$tau[site_rs]
  ## Random effect
  est_rs_out <- metafor::rma(yi = est_tau_rs, vi = rep(within_se^2, N_s), method = "REML")
  est_rs <- c(est_rs_out$beta, est_rs_out$se)

  out_all <- list(list("design_num" = design_num,
                       "true" = tau_mean,
                       "true_between_sd" = sd(dgp_data$tau),
                       "est_rs" = est_rs,
                       "est_sps" = est_sps))
                       # "sps_p" = sps_p_value_use))


  # Save to file
  if(HPC == TRUE){
    sim_num_str   <- s
    saveRDS(
      out_all,
      paste0(
        OUT_DIR,
        "/sps_",
        data_type,
        "_",
        run_ts,
        "_sim_",  
        sim_num_str,
        ".rds"
      )
    )
  }
  # End timing
  loop_end_time <- Sys.time()
  return(out_all)
}

dgp_sps_Naumann <- function(for_use, N, dgp_coef, dgp_base){
  
  X_cov <- dgp_base$X_cov
  bet_se <- dgp_base$bet_se
  within_se <- dgp_base$within_se
  
  X_use <-  MASS::mvrnorm(n = N, mu = rep(0, ncol(X_cov)), Sigma = X_cov)
  
  # Relevant Variables
  X_main <- model.matrix(for_use, data = as.data.frame(X_use))
  
  ### DGP based on Naumann
  tau <- X_main %*% dgp_coef
  # across-site variation
  epsilon <- rnorm(n = N, sd = bet_se)  
  # within-site variation
  epsilon_est <- rnorm(n = N, sd = within_se) 
  tau_obs <- tau + epsilon + epsilon_est
  true_overall <- mean(tau)
  
  ## within-site SE
  se_tau <- rep(within_se, nrow(X_use))
  
  rownames(X_use) <- seq(1:nrow(X_use))
  
  dgp_out <- list("tau" = tau, "se" = se_tau, "X" = X_use)
  return(dgp_out)
}


