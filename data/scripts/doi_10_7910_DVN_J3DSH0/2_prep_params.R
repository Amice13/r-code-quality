# This script prepares the parameters for the allocation model

library(cyra4cm) # the R package provided as supplementary material
source("./jri_code_and_data/constants/TVA_CONST.R")
source("./jri_code_and_data/constants/cyra_fun_updated.R")

prepCompanyParams = function(sample, th_vals = NULL, budget_pct = DEFAULT_BUDGET_PCT, ded_pct = DEFAULT_DEDUCTIBLE_PCT){
  company_sample = sample[sample$company, ]
  
  # budget
  all_budget = company_sample$rev[1]*1000000*budget_pct/100
  
  # deductible
  ded = company_sample$rev[1]*1000000*ded_pct/100
  
  # loss parameters
  params = prepareCyraParameters(sample)
  
  if(is.null(params)){
    return(NULL)
  }
  
  # theta
  if(!is.null(th_vals)){
    init_th = th_vals
  } else {
    init_th = rep(DEFAULT_TH, length(unique(params$m)))
    names(init_th) = unique(params$m)
  }
  
  # discrete investment options
  inv_opt_mat = INV_OPT_DEFAULT[INV_OPT_DEFAULT[,"idx"] %in% params$m, ]
  inv_opt_list = apply(inv_opt_mat, 1, function(x) do.call(cyraInvestOption, as.list(x)), simplify = FALSE)
  inv_opt = do.call(cyraInvestOptions, inv_opt_list)
  
  # weights
  unique_ik = unique(params$path_c[, c("T", "A")])
  weights <- createWeightsTables(
    cyra.params = params,
    nu.ik = rep(DEFAULT_NU, nrow(unique_ik)),
    om.ik = rep(DEFAULT_OM, nrow(unique_ik)),
    eta.j = rep(DEFAULT_ETA, length(params$m)),
    nu = DEFAULT_NU,
    om = DEFAULT_OM,
    eta = DEFAULT_ETA,
    path_c = params$path_c
  )
  
  # loss distribution
  # loss_dists <- generateLossDists(init_th, params, scale.fun = scaleLogNormal)
  
  
  
  return(
    list(
      init_th = init_th,
      inv_opt = inv_opt,
      all_budget = all_budget,
      ded = ded,
      weights = weights,
      params = params,
      # loss_dists = loss_dists,
      ik_pairs = unique(params$lambda_ik[, c("i", "k")])
    )
  )
}