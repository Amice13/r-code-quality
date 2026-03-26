collect_counterfactual_results <- function(target_rate){
  
  counterfactual_results <- data.table()
  
  for(index in 1:length(names(tau.pred_new))) {
    
    name=names(tau.pred_new)[index]
    print(paste("running ",name,sep=""))

    my_tau = tau.pred[[name]]$test #was using index
    my_tau_new = tau.pred_new[[name]]$test #was using index
    
    my_ranking = rank(my_tau)
    my_ranking_new = my_ranking
    
    my_policy = as.integer(my_tau > quantile(my_tau, c(1 - target_rate)))
    my_policy_new = my_policy
    
    for (i in c(1:length(my_tau))) {
      my_tau_new_i = my_tau
      my_tau_new_i[i] = my_tau_new[i]
      
      my_ranking_new_i = rank(my_tau_new_i)
      my_ranking_new[i] = my_ranking_new_i[i]
      
      my_policy_i = as.integer(my_tau_new_i > quantile(my_tau_new_i, c(1 - target_rate)))
      my_policy_new[i] = my_policy_i[i]
    }
    
    delta_ranking = mean(abs(my_ranking - my_ranking_new))
    delta_policy = mean(abs(my_policy - my_policy_new))
    
    counterfactual_results = rbind(
      counterfactual_results,
      data.table(
        policy = names(tau.pred_new)[index],
        target_rate = target_rate,
        delta_ranking = delta_ranking,
        delta_policy = delta_policy
      )
    )
  }
  return(counterfactual_results)
}

