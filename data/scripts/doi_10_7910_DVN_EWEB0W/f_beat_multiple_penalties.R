# Function that selects targets based on tau_pred
target_tau_proportion  = function(tau_pred, target_rate){
  return(as.integer(tau_pred > quantile(tau_pred, c(1-target_rate))))
}

# Function that runs BEAT for different levels of penalty -- returns TAUs
estimate_taus_per_penalty = function(penalty_value){
  print(penalty_value)
  fit_beat <- balanced_causal_forest(X_train, Y_train, W_train,
                                     target.weights = as.matrix(Z_train),
                                     target.weight.penalty = penalty_value,
                                     target.weight.standardize = TRUE,
                                     target.weight.bins.breaks = 256,
                                     honesty = TRUE,
                                     num.trees = num_trees,
                                     tune.parameters=my_tunable_params,
                                     seed=my_seed)
  
  tau_train = predict(fit_beat)$predictions
  tau_test = predict(fit_beat, X_test)$predictions
  
  out_train = data.table(row_id = 1: dim(X_train)[1],
                         tau_predict = tau_train,
                         penalty_rate =penalty_value,
                         data_type = "train")
  out_test = data.table(row_id = (dim(X_train)[1]+1): (dim(X_train)[1]+dim(X_test)[1]),
                        tau_predict = tau_test,
                        penalty_rate =penalty_value,
                        data_type = "test")
  out = rbind(out_train, out_test)
  
  return(out)
}

# Function that collects policy results per target vector using BEAT by penalty
collect_penalty_results = function(target_vector, data_type="train"){
  if(data_type=="train"){
    stopifnot(length(target_vector)==dim(X_train)[1])
    out = off_policy_evalution(X = X_train,
                                  W = W_train,
                                  Y = Y_train,
                                  Z = Z_train,
                                  policy_targets = target_vector,
                                  policy_name = "balanced_grf" ,
                                  data_type = 'train',
                                  W.hat = W.hat_train)
    out = rbind(unlist(out))
    out = as.data.table(out)
  }else if(data_type=="test"){
    stopifnot(length(target_vector)==dim(X_test)[1])
    out = off_policy_evalution(X = X_test,
                                  W = W_test,
                                  Y = Y_test,
                                  Z = Z_test,
                                  policy_targets = target_vector,
                                  policy_name = "balanced_grf" ,
                                  data_type = 'test',
                                  W.hat = W.hat_test)
    out = rbind(unlist(out))
    out = as.data.table(out)
  }else{
    stop("data_type is either train or test")
  }
  return(out)
}