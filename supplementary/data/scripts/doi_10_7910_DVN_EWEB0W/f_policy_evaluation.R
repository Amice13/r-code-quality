# Inverse probability scores (function) -- it needs Y, W, the propensities, and the policy to evaluate
v_ips_function <- function(Y,W,ehat,my_policy) {
  ips = (W==my_policy)*(W==0)/(1-ehat) + (W==my_policy)*(W==1)/ehat
    temp = mean(Y*ips)
   return(temp)
}


# Function that evaluates (1) Reward (IPW), (2) % users targeted, (3) imbalance, (4) empirical ATE for treated and for non-treated, for each policy and dataset
off_policy_evalution = function(X, W, Y, Z,
                                policy_targets,
                                policy_name,
                                data_type,
                                W.hat){
  
  
  # Reward (IPW estimator)
  ipw  = v_ips_function(Y, W, W.hat, policy_targets)
  
  # Proportion of users targeted
  target_ratio = mean(policy_targets)
  
  # Amount of imbalance
  scaled.Z = scale(Z)
  scaled.Z[is.nan(scaled.Z)] <- 0 # we get NAs in rare categories with no variation
  scaled.Z = data.table(scaled.Z)

  aux_avg = colMeans(scaled.Z)
  aux_policy = colMeans(scaled.Z[policy_targets==1])
  target_weight_imbalance  = sum((aux_policy-aux_avg)^2)
  target_weight_imbalance_per_dim = (aux_policy-aux_avg)^2
  
  return(list('ipw' = ipw,
              'target_ratio' = target_ratio,
              'target_weight_imbalance' = target_weight_imbalance,
              'target_weight_imbalance_per_dim' = target_weight_imbalance_per_dim,
              'policy_name'=policy_name,
              'data_type'=data_type))
}


# Function that collects all the policy evaluations, by target_rate
collect_benchmark_results = function(target_rate){
  
  evaluate_policy_results = function(policy_list, policy_name){
    policy = policy_list[[policy_name]]
    out_train = off_policy_evalution(X_train, W_train, Y_train, Z_train,policy$train, policy_name, 'train', W.hat_train)
    out_test = off_policy_evalution(X_test, W_test, Y_test, Z_test, policy$test, policy_name, 'test', W.hat_test)
    return(data.table(rbind(unlist(out_train),unlist(out_test) )))
  }
  
  my_targeting_policies = list(
    'grf_demog' = list(
      'train' = as.integer(tau.pred$grf_demog$train > quantile(tau.pred$grf_demog$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_demog$test > quantile(tau.pred$grf_demog$test, c(1-target_rate)))
    ),
    'grf' = list(
      'train' = as.integer(tau.pred$grf$train > quantile(tau.pred$grf$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf$test > quantile(tau.pred$grf$test, c(1-target_rate)))
    ),
    'grf_beat' = list(
      'train' = as.integer(tau.pred$grf_beat$train > quantile(tau.pred$grf_beat$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_beat$test > quantile(tau.pred$grf_beat$test, c(1-target_rate)))
    ),    
    'random' = list(
      'train' = as.integer(tau.pred$random$train > quantile(tau.pred$random$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$random$test > quantile(tau.pred$random$test, c(1-target_rate)))
    ),
    'uniform' = list(
      'train' = (sum(tau.pred$grf_demog$train)>0)*rep(1,length(tau.pred$grf_demog$train)),
      'test' = (sum(tau.pred$grf_demog$test)>0)*rep(1,length(tau.pred$grf_demog$test))
    ),
    'grf_residual_rf_x' = list(
      'train' = as.integer(tau.pred$grf_residual_rf_x$train > quantile(tau.pred$grf_residual_rf_x$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_residual_rf_x$test > quantile(tau.pred$grf_residual_rf_x$test, c(1-target_rate)))
    ),
    'grf_residual_rf_xy' = list(
      'train' = as.integer(tau.pred$grf_residual_rf_xy$train > quantile(tau.pred$grf_residual_rf_xy$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_residual_rf_xy$test > quantile(tau.pred$grf_residual_rf_xy$test, c(1-target_rate)))
    ),
    'grf_residual_tau_demog' = list(
      'train' = as.integer(tau.pred$grf_residual_tau_demog$train > quantile(tau.pred$grf_residual_tau_demog$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_residual_tau_demog$test > quantile(tau.pred$grf_residual_tau_demog$test, c(1-target_rate)))
    ),
    'plugin_rf' = list(
      'train' = as.integer(tau.pred$plugin_rf$train > quantile(tau.pred$plugin_rf$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$plugin_rf$test > quantile(tau.pred$plugin_rf$test, c(1-target_rate)))
    ),
    'plugin_xg' = list(
      'train' = as.integer(tau.pred$plugin_xg$train > quantile(tau.pred$plugin_xg$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$plugin_xg$test > quantile(tau.pred$plugin_xg$test, c(1-target_rate)))
    )
  )
  
  results = rbindlist(lapply(names(my_targeting_policies),
                             evaluate_policy_results,
                             policy_list= my_targeting_policies))
  
}


# Function that collects the policy evaluations for GRF models, by target_rate
collect_grf_results = function(target_rate){
  
  evaluate_policy_results = function(policy_list, policy_name){
    policy = policy_list[[policy_name]]
    out_train = off_policy_evalution(X_train, W_train, Y_train, Z_train,policy$train, policy_name, 'train', W.hat_train)
    out_test = off_policy_evalution(X_test, W_test, Y_test, Z_test, policy$test, policy_name, 'test', W.hat_test)
    return(data.table(rbind(unlist(out_train),unlist(out_test) )))
  }
  
  my_targeting_policies = list(
    'grf_demog' = list(
      'train' = as.integer(tau.pred$grf_demog$train > quantile(tau.pred$grf_demog$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf_demog$test > quantile(tau.pred$grf_demog$test, c(1-target_rate)))
    ),
    'grf' = list(
      'train' = as.integer(tau.pred$grf$train > quantile(tau.pred$grf$train, c(1-target_rate))),
      'test' = as.integer(tau.pred$grf$test > quantile(tau.pred$grf$test, c(1-target_rate)))
    )
  )
  
  results = rbindlist(lapply(names(my_targeting_policies),
                             evaluate_policy_results,
                             policy_list= my_targeting_policies))
  
}

