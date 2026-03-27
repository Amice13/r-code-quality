# Useful functions for the regressions

# Returns labels for tables
get_labels<- function(var_list){
  labels = c()
  for (var in var_list) {
    if (var == "know_gap_count"){
      labels = c(labels,"Knowledge gap")
    }
    if (var == "know_gap_count_followup"){
      labels = c(labels,"Knowledge gap follow up")
    }
    if (var == "intended_behavior"){
      labels = c(labels,"Number of links")
    }
    if (var == "safety_count_variable"){
      labels = c(labels,"Safety count variable")
      
    }
    if (var == "wtp_masks"){
      labels = c(labels,"WTP Masks")
      
    }
    if (var == "fed_balance_agree"){
      labels = c(labels,"Trust in Federal response")
    }
    if (var == "state_balance_agree"){
      labels = c(labels,"Trust in Local response")
    }
    if (var == "charity_covid"){
      labels = c(labels,"Donation to COVID")
    }
    if (var == "charity_black"){
      labels = c(labels,"Donation to Black")
    }

    if (var == "post_belief_lower_4"){
      labels = c(labels,"Posterior belief < 4")
    }
    if (var == "post_belief_higher_4"){
      labels = c(labels,"Posterior belief >= 4")
    }
    

  } 
  return(labels)
}

# Returns the lasso support (Lasso selection of the covariates)
get_lasso_support <- function(target_list, x_list, data_sel) {
  selected_support <- c()
  
  for (target in target_list) {
    
    lasso_rhs <- paste0(x_list, collapse = "+")
    
    lasso_t_spec <- as.formula(paste0(target, "~", lasso_rhs))

    model_lasso_t <- rlasso(formula = lasso_t_spec, data = data_sel, post = FALSE)
    support_t <- names(model_lasso_t$coefficients[model_lasso_t$coefficients != 0])
    
    toRemove <- grep(pattern = "ntercept",x = support_t) #don't want intercept to be lasso selected!
    support_t <- support_t[-toRemove]

    
    selected_support <- union(selected_support,support_t)
  }
  
  return(selected_support)
}


# Filters data depending on the panel we want
get_filtered_data <- function(data_orig, dem_select = "All",outcome) {
  if (dem_select == "African-American") {
    data_orig <- data_orig %>% filter(resp_black == 1)
  }
  else if (dem_select == "White") {
    data_orig <- data_orig %>% filter(resp_white == 1)
  }
  
  if (dem_select == "Male") {
    data_orig <- data_orig %>% filter(sex_male == 1)
  }
  
  if (dem_select == "Female") {
    data_orig <- data_orig %>% filter(sex_female == 1)
  }
  if (dem_select == "White rep") {
    data_orig <- data_orig %>% filter(race_white__party_rep == 1) 
  }
  if (dem_select == "White dem") {
    data_orig <- data_orig %>% filter(race_white__party_dem == 1)
  }

  if (dem_select == "White independent") {
    data_orig <- data_orig %>% filter(resp_white == 1) %>%  filter(race_white__party_dem == 0) %>%  filter(race_white__party_rep == 0)
  }


  if (dem_select == "not HS graduate") {
    data_orig <- data_orig %>% filter(not_hs_grad == 1)
  }
  if (dem_select == "HS graduate") {
    data_orig <- data_orig %>% filter(hs_graduate == 1)
  }
  
  # For every outcome except follow up outcomes and knowledge outcomes, we select respondents who answered the links questions
  if (!(outcome %in% c("know_gap_count","safety_count_variable","know_gap_count_followup")))
  {
    data_orig = data_orig[!is.na(data_orig$timing_link_choice_page_submit),]
    }
  
  
  return(data_orig)
  
}

get_strat = function(dem_option){

  if (dem_option=="White"){
    return("+ strat1 + strat2 + strat4 + strat5  + strat7 + strat8  + strat10 + strat11")
  }else if (dem_option=="African-American"){
    return("+ strat3  + strat6 + strat9 + strat12 ")
  }else{
    return("+ strat1 + strat2 + strat4 + strat5  + strat7 + strat8  + strat10 + strat11 + strat3  + strat6 + strat9 + strat12 ")
  }

}

# OLS regression
run_reg_outcome <- function(outcome, data_reg, primary_rhs,balance_model=FALSE,DPL=FALSE) {
  final_rhs <- primary_rhs
  if (DPL==TRUE){
  support_y <- get_lasso_support(outcome, lasso_controls, data_reg)
  
  dpl_controls <- union(support_y, support_t_net)
  
  

  }else{
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet = get_strat(dem_select)
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, extra_controls_snippet, dpl_snippet)) #newprint(paste0(outcome, "~", final_rhs, "+ stratum ", extra_controls_snippet))
  print(paste0(outcome, "~", final_rhs, extra_controls_snippet))
  model <- lm(model_spec, data_reg)
  if (balance_model){ # Observations with weight = NA are omitted from the regression
  
    if (outcome %in% follow_outcomes){
      model <- lm(model_spec, data_reg,weights = data_reg$weights_follow)
      print("weights follow")
    }
    }

  
  
  return(model)
  
}

# Logit regression
run_reg_binary_outcome <- function(outcome, data_reg, primary_rhs,balance_model=FALSE,DPL=FALSE) {
  final_rhs <- primary_rhs
  if (DPL == TRUE){
  support_y <- get_lasso_support(outcome, lasso_controls, data_reg)

  dpl_controls <- union(support_y, support_t_net)

  

 
  }else{
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet = get_strat(dem_select)
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, extra_controls_snippet, dpl_snippet)) #newprint(paste0(outcome, "~", final_rhs, "+ stratum ", extra_controls_snippet))
  print(paste0(outcome, "~", final_rhs, extra_controls_snippet))

  model <- glm(formula=model_spec,family=binomial,data= data_reg,na.action="na.omit")
  if (balance_model){# Observations with weight = NA are omitted from the regression
  }


  
  return(model)
  
}

# Negative binomial regression
run_reg_discrete_outcome <- function(outcome, data_reg, primary_rhs, type="NB",balance_model=FALSE,DPL=FALSE) {
  final_rhs <- primary_rhs
  if (DPL==TRUE){
  support_y <- get_lasso_support(outcome, lasso_controls, data_reg)

  dpl_controls <- union(support_y, support_t_net)
  
  
  
  }else{
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet = get_strat(dem_select)
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, extra_controls_snippet, dpl_snippet)) #newprint(paste0(outcome, "~", final_rhs, "+ stratum ", extra_controls_snippet))
  print(paste0(outcome, "~", final_rhs, extra_controls_snippet))
  
    
  model = glm.nb(formula = model_spec,data = data_reg,na.action="na.omit",link=log)
  if (balance_model){# Observations with weight = NA are omitted from the regression


    if (outcome %in% follow_outcomes){
      model <- glm.nb(formula = model_spec, data = data_reg,na.action="na.omit",weights = data_reg$weights_follow,link=log)
      print("weights follow")
    }


    }
  return(model)
  
}

# Function which gives the IRR of a NB model
# This function is adapted from the function negbinirr in mfx package.
# The negbinirr function does not allow weights in the regression. Therefore, we had to recode it.

# We build the IRR confidence intervals by taking the exponential of the coefficient confidence interval
To_irr = function(fit){
  
  x1 = model.matrix(fit)
  if (any(alias <- is.na(coef(fit)))) {
    x1 <- x1[, !alias, drop = FALSE]
  }
  
  # get variances
  vcv = vcov(fit) 
  
  
  
  irr = data.frame(irr=exp(na.omit(coef(fit))), se=NA)
  # get standard errors
  gr = diag(exp(na.omit(coef(fit))))
  
  irr$se = sqrt(diag(gr %*% vcv %*% t(gr)))
  
  
   irr$CI_lower_bound = exp(na.omit(coef(fit)) - 1.96*sqrt(diag(vcv)))  # CI for IRR = exp(CI for coefficient)
   irr$CI_upper_bound = exp(na.omit(coef(fit)) + 1.96*sqrt(diag(vcv)))  # CI for IRR = exp(CI for coefficient)
  
  # pick out constant and remove from mfx table
  temp1 = apply(x1,2,function(x)length(table(x))==1)
  const = names(temp1[temp1==TRUE])
  irr = irr[row.names(irr)!=const,]
  
  res = list(fit=fit, irr=irr)
  
  est = NULL
  zstat = log(res$irr$irr)*res$irr$irr/res$irr$se
  
  est$irr = cbind(IRR = res$irr$irr,#
                  StdErr = res$irr$se,#
                  CI_lower_bound = irr$CI_lower_bound, 
                  CI_upper_bound = irr$CI_upper_bound,
                  z.value = zstat,
                  p.value = 2*pt(-abs(zstat), df = Inf)) # p-value for the test "coefficient == 0"
  colnames(est$irr) = c("IRR","Std. Err.","CI_lower_bound","CI_upper_bound","z","P>|z|")
  rownames(est$irr) =  rownames(res$irr)
  
  est$fit = res$fit
  est$call = match.call() 
  est
}