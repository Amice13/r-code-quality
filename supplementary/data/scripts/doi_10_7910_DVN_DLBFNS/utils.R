
theme_new <- function(base_size = 16, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}

weighted.se.mean <- function(x, w, na.rm = T){
  ## Remove NAs 
  if (na.rm) {
    i <- !is.na(x)
    w <- w[i]
    x <- x[i]
  }
  
  ## Calculate effective N and correction factor
  n_eff <- (sum(w))^2/(sum(w^2))
  correction = n_eff/(n_eff-1)
  
  ## Get weighted variance 
  numerator = sum(w*(x-weighted.mean(x,w))^2)
  denominator = sum(w)
  
  ## get weighted standard error of the mean 
  se_x = sqrt((correction * (numerator/denominator))/n_eff)
  return(se_x)
}


nonpar_summary_function <- function(tx_group, control_group, outcome_string){
  
  ## construct summary table
  sum_table = data.frame(outcome = outcome_string,
                         tx_value = weighted.mean(tx_group[[outcome_string]],
                                                  w = tx_group$ra_weight),
                         tx_sem = weighted.se.mean(tx_group[[outcome_string]],
                                         w = tx_group$ra_weight),
                         control_value = weighted.mean(control_group[[outcome_string]],
                                                       w = control_group$ra_weight),
                         control_sem = weighted.se.mean(control_group[[outcome_string]],
                                                        w = control_group$ra_weight)) %>%
    mutate(diff_values = tx_value - control_value,
           total_N = nrow(tx_group) + nrow(control_group)) 
  return(sum_table)
}



## function to construct regression formulas
construct_reg <- function(outcome_var, 
                          control_var,data, model = "main",
                          moderator = ""){
  
  if(model == "main"){
    
    return(lm(formula(sprintf("%s ~ treatment + %s",
                              outcome_var,
                              control_var)),
              data = data,
              weights = ra_weight))
  } else if (model == "het"){
    
    return(lm(formula(sprintf("%s ~ treatment*%s + %s",
                              outcome_var,
                              moderator,
                              control_var)),
              data = data,
              weights = ra_weight))
    
  }
}

## function to estimate different models 
run_regs <- function(outcome_var_name, data, moderator = "", model = "main"){
  
  tier = construct_reg(outcome_var = outcome_var_name,
                       control_var = "strata_forRA",
                       data = data, moderator = moderator, model = model)
  
  block = construct_reg(outcome_var = outcome_var_name,
                        control_var = "block_id",
                        data = data, moderator = moderator, model = model)
  
  tier_ologit = construct_reg(outcome_var = outcome_var_name,
                              control_var = paste(c("strata_forRA", "ologit_linearpred"),
                                                  collapse = "+"),
                              data = data, moderator= moderator, model = model)
  
  block_ologit = construct_reg(outcome_var = outcome_var_name,
                               control_var = paste(c("block_id", "ologit_linearpred"),
                                                   collapse = "+"),
                               data = data, moderator = moderator,
                               model = model)
  
  all_outcome = list(tier = tier, block = block,
                     tier_ologit = tier_ologit,
                     block_ologit = block_ologit)
  return(all_outcome)
  
}

## function just running the preferred specification
run_regs_preferred <- function(outcome_var_name, data, moderator = "", model = "main"){
  
  block = construct_reg(outcome_var = outcome_var_name,
                        control_var = "block_id",
                        data = data, moderator = moderator, model = model)
  
  return(block)
  
}

## manual way (just used as quality check to make sure setup of auto is correct)
compliance_reg_manual <- function(data, outcome_var, 
                                  control_var){
  
  fit_stage1 = lm(formula(sprintf("comply_3level ~ treatment + %s",
                                  paste(control_var, collapse = "+"))),
                  data = data, weight = ra_weight)
  #print(summary(fit_stage1))
  data$predict_attend = fit_stage1$fitted.values
  fit_stage2 = lm(formula(sprintf("%s ~ predict_attend + %s",
                                  outcome_var, 
                                  paste(control_var, collapse = "+"))),
                  data = data, weight = ra_weight)
  return(fit_stage2)
}

compliance_reg_auto <- function(data, outcome_var, control_var, comply_var){
  
  auto_iv = ivreg::ivreg(formula(sprintf("%s ~ %s + %s |
                      treatment + %s ", 
                                         outcome_var,
                                         comply_var,
                                         paste(control_var, collapse = "+"),
                                         paste(control_var, collapse = "+"))), data = data, weight = ra_weight)
  return(auto_iv)
  
}

## create dataframe with coefficients and SEs from IV
get_iv_summary <- function(one_reg, outcome_forgraph){
  
  beta = summary(one_reg)$coefficients["comply_3level", 1]
  se = summary(one_reg)$coefficients["comply_3level", 2]
  p = summary(one_reg)$coefficients["comply_3level", 4]
  df_summary = data.frame(beta = beta, se = se, p = p) %>%
    mutate(outcome = outcome_forgraph,
           lower = beta-1.96*se,
           upper = beta+1.96*se,
           n = one_reg$n)
  return(df_summary)
}

get_itt_summary <- function(one_reg, outcome_forgraph){
  
  beta = summary(one_reg)$coefficients["treatment", 1]
  se = summary(one_reg)$coefficients["treatment", 2]
  p = summary(one_reg)$coefficients["treatment", 4]
  df_summary = data.frame(beta = beta, se = se, p = p) %>%
    mutate(outcome = outcome_forgraph,
           lower = beta-1.96*se,
           upper = beta+1.96*se,
           n = nrow(one_reg$model))
  return(df_summary)
}

## specify function: reg table format
gen_reg_table <- function(list_models, vec_col_labels, vec_cols_omit, caption_str, label_str, table_name_str){
  stargazer(list_models,
            type = "latex",
            column.labels  = vec_col_labels,
            covariate.labels = c("Invited", "Constant"),
            model.numbers = FALSE,
            omit = vec_cols_omit, report = "vcsp*",
            column.sep.width = "10pt", 
            dep.var.labels.include = FALSE,
            label = sprintf("tab:%s", label_str),
            title = caption_str,
            star.cutoffs = c(0.05, 0.01, 0.001),
            out = sprintf("%s%s.tex", 
                          here(TABLE_OUTPUT_DIR), 
                          table_name_str))
  
}

