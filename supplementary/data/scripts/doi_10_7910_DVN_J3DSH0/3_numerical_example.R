library(cyra4cm) # the R package provided as supplementary material
library(foreach)
library(doParallel)
library(plyr)

source("./jri_code_and_data/constants/TVA_CONST.R")
source("./jri_code_and_data/constants/cyra_fun_updated.R")
source("./jri_code_and_data/1_build_sample.R")
source("./jri_code_and_data/2_prep_params.R")

ind_records_df = readRDS("./jri_code_and_data/saved_data/ind_records_df.rds")

optimal_allo_list = list()
for(comp_id in unique(ind_records_df$company)){
  sample = buildSample(comp_id, ind_records_df)
  params_c = prepCompanyParams(sample)
  
  if(is.null(params_c)){
    next
  }
  
  inci_num = sum(sample$company)
  list2env(params_c, envir = globalenv())
  list2env(params, envir = globalenv())
  list2env(weights, envir = globalenv())
  nu_om_ik_base <- nu_om_ik
  nu_om_base <- nu_om
  normalizer = DEFAULT_NORMALIZER
  penalty.thres = DEFAULT_PENALTY_THRES
  invest_table <- createInvestTable(inv_opt)
  
  th_table <- extractFromInvestTable(invest_table, "theta")
  M_table <- extractFromInvestTable(invest_table, "invest")
  insur_table <- generateInsurTable(ik_pairs)
  allo_list <- list()
  params_df <- prepXParams(l, m, n, loss_params, dist_params_names)
  
  # temporary solution
  ga_ik <- ik_pairs
  ga_ik['ga'] <- rep(DEFAULT_GA,nrow(ga_ik))
  ga <- DEFAULT_GA
  profit_loading <- DEFAULT_LOADING
  prem_vec <- gsub("c", "p", names(insur_table))
  
  cores=detectCores()
  cl <- makeCluster(cores[1]-2)
  registerDoParallel(cl)

  # comp_table = data.frame()
  # for(r in 1:nrow(th_table)){
  comp_table = foreach(r=1:nrow(th_table), .combine = rbind) %dopar% {
  library(cyra4cm)
    Kik_holistic_table <- NULL
    
    th <- unlist(th_table[r, ])
    names(th) = gsub("theta", "", names(th_table))
    M <- unlist(M_table[r, ])
    names(M) = names(M_table)
    nu_om <- nu_om_base
    nu_om_ik <- nu_om_ik_base
    
    x_mass <- generateXMassTable(th, params_df, params, scale.fun=scaleLogNormal)
    zik_mass <- generateZikMassTable(q, th, x_mass)
    sik_mass_raw <- generateSikMassTable(q, lambda_ik, zik_mass)
    
    # plan_table <- merge(insur_table, t(th))
    plan_table = insur_table
    plan_table[prem_vec] <- NA
    plan_table[c("obj_c", "obj_i", "obj_r", "obj_tot", "feasible")] <- NA
    
    for (insur_idx in 1:nrow(insur_table)){
      cat(r, ",", insur_idx, "\n\r")
      insur <- insur_table[insur_idx, ]
      cik_mass <- generateCikMassTable(insur, zik_mass, q, ded = ded)
      ci_mass <- generateZiMassTable(q, cik_mass)
      l_mass <- generateLMassTable(p_i, ci_mass)
      sik_mass <- generateSikMassTable(q, lambda_ik, cik_mass)
      s_mass <- generateSMassTable(q, lambda, l_mass)
      
      subclaim <- generateSubClaimMassTable(insur, zik_mass, q, ded = ded)
      subclaim_mass <- subclaim$mass
      subclaim_q <- subclaim$q
      threat_mass <- generateZiMassTable(subclaim_q, subclaim_mass)
      tot_subclaim_mass <- generateSikMassTable(subclaim_q, lambda_ik, subclaim_mass)
      
      premium_table <- calcPremiumTable(insur, tot_subclaim_mass, subclaim_q, profit_loading)
      
      total_budget <- max(0, all_budget - sum(M) - sum(premium_table$prem))
      
      for(rik in 1:nrow(sik_mass)) {
        p <- unlist(sik_mass[rik, paste0("p", c(0, q))])
        VaR <- calcVaR(alpha = penalty.thres, p = p, q = c(0, q))
        if(normalizer == "local") {
          norm_fac <- calcNormalizer.local(h.func = calcPenaltyValue, p = p, q = c(0, q), list(x = VaR))
          if(is.nan(norm_fac)) norm_fac <- 1
        } else if (normalizer == "none") {
          norm_fac <- 1
        } else if (normalizer == "l1") {
          norm_fac <- calcNormalizer.l1norm(term.pt = "TVaR", h.func = calcPenaltyValue, p = p, q = c(0, q), list(x = VaR))
          if(is.nan(norm_fac)) norm_fac <- 1
        } else if (normalizer == "cte"){
          norm_fac <- calcNormalizer.cte(p = p, q = c(0,q), list(x = VaR))
          if(norm_fac==0) norm_fac <- 1
        } else if (class(normalizer == "function")) {
          norm_fac <- normalizer(...)
          if(is.nan(norm_fac)) norm_fac <- 1
        }
        nu_om_ik[rik, "om"] <- nu_om_ik_base[rik, "om"]/norm_fac
      }
      
      p <- unlist(s_mass)
      VaR <- calcVaR(alpha = penalty.thres, p = p, q = c(0, q))
      if(normalizer == "local") {
        norm_fac <- calcNormalizer.local(h.func = calcPenaltyValue, p = p, q = c(0, q), list(x = VaR))
      } else if (normalizer == "none") {
        norm_fac <- 1
      } else if (normalizer == "l1") {
        norm_fac <- calcNormalizer.l1norm(term.pt = "TVaR", h.func = calcPenaltyValue, p = p, q = c(0,q), list(x = VaR))
      } else if (normalizer == "cte"){
        norm_fac <- calcNormalizer.cte(p = p, q = c(0, q), list(x = VaR))
      } else if (class(normalizer == "function")){
        norm_fac <- normalizer(...)
      }
      if(norm_fac==0) norm_fac <- 1
      nu_om$om <- nu_om_base$om/norm_fac
      
      Kik_bar <- calcStandaloneKik(penalty.thres, q, sik_mass, nu_om_ik, lambda_ik)
      K_bar <- calcAggregateK(penalty.thres, q, s_mass, nu_om)
      
      I_mat <- generateIMat(ik_pairs)
      Kik_holistic_wo_budget_constraint <- calcConstrainedHolisticKik(I_mat, nu_om, nu_om_ik, Kik_bar, K_bar)
      names(Kik_holistic_wo_budget_constraint)[names(Kik_holistic_wo_budget_constraint) =='Kik'] = "holistic"
      if(sum(Kik_holistic_wo_budget_constraint$holistic)<=total_budget){
        Kik_bar <- merge(Kik_bar, Kik_holistic_wo_budget_constraint, by = c('i', 'k'))
      } else {
        Kik_bar['crit'] <- Kik_bar$om*Kik_bar$Kik
        Kik_bar <- Kik_bar[order(Kik_bar$crit, decreasing = FALSE),]
        Kik_bar['holistic'] <- 0
        for(rI in 1:nrow(Kik_bar)){
          I_try <- rI:nrow(Kik_bar)
          cond <- total_budget + sum(Kik_bar$crit[rI]/Kik_bar$om[I_try] - Kik_bar$Kik[I_try]) >= 0
          if(cond == 1) break
        }
        Kik_bar$holistic[I_try] <- Kik_bar$Kik[I_try] - (1/Kik_bar$om[I_try])/(sum(1/Kik_bar$om[I_try]))* (sum(Kik_bar$Kik[I_try]) - total_budget)
      }
      
      Kik_holistic <- Kik_bar[c('i', 'k')]
      Kik_holistic['Kik'] <- Kik_bar$holistic
      Kik_row_order <- match(paste0(nu_om_ik$i, nu_om_ik$k), paste0(Kik_holistic$i, Kik_holistic$k))
      Kik_holistic <- Kik_holistic[Kik_row_order, ]
      
      obj <- calcObjValue(penalty.thres, q, Kik_holistic, M, nu_om, nu_om_ik, etas, ga, ga_ik, sik_mass, s_mass, premium_table)
      plan_table[insur_idx, c("obj_c", "obj_i", "obj_r", "obj_tot")] <- obj
      plan_table[insur_idx, "feasible"] <- (all_budget - sum(M) - sum(premium_table$prem) >= 0)
      plan_table[insur_idx, prem_vec] <- premium_table$prem
      Kik_holistic_table <- rbind(Kik_holistic_table, Kik_holistic$Kik)
    }
    colnames(Kik_holistic_table) <- paste0("K", Kik_holistic$i, Kik_holistic$k)
    
    th_col = t(th)
    colnames(th_col) = paste0("theta", colnames(th_col))
    M_col = t(M)
    # colnames(M_col) = paste0("invest", colnames(M_col))
    allo <- cbind(th_col, M_col, plan_table[, c(names(insur_table), prem_vec)], Kik_holistic_table, plan_table[, c("obj_c", "obj_i", "obj_r", "obj_tot", "feasible")])
    # comp_table = rbind(comp_table, allo)
  }
  stopCluster(cl)
  feasible_allo = comp_table[comp_table$feasible, ]
  optimal_allo = feasible_allo[order(feasible_allo$obj_tot, decreasing = FALSE), ][1,]
  optimal_allo["company"] = comp_id
  optimal_allo["rev"] = sample[sample$company, "rev"][1]
  optimal_allo["inci_num"] = inci_num
  optimal_allo["budget"] = all_budget
  optimal_allo["path_num"] = nrow(path_c)
  optimal_allo["ent"] = ent
  optimal_allo_list[[comp_id]] = optimal_allo
}

optimal_allo_df = do.call(rbind.fill, optimal_allo_list)
saveRDS(optimal_allo_df, "./jri_code_and_data/saved_data/optimal_allo_df.rds")