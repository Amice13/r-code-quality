# Useful functions for the regressions

# Returns labels for tables
get_labels<- function(var_list){
  labels = c()
  for (var in var_list) {
    if (var == "posterior_protect"){
      labels = c(labels,"Posterior Protect Community")
    }
    if (var == "posterior_no_good"){
      labels = c(labels,"Posterior Up to No Good")
    }
    if (var == "posterior_was_sick"){
      labels = c(labels,"Posterior Was Sick")
    }
    if (var == "masks_for_the_people"){
      labels = c(labels,"Donation to Mask For the People")
    }
    if (var == "want_mask_vid"){
      labels = c(labels,"Want DIY Mask Tutorial Video")
    }
    if (var == "knowledge_count_variable"){
      labels = c(labels,"Knowledge gap count")
    }
    if (var == "intended_behavior"){
      labels = c(labels,"Number of links")
    }
    if (var == "any_knowledge_gap"){
      labels = c(labels,"Any knowledge gap")
    }
    if (var == "want_any_link"){
      labels = c(labels,"Any link")
    }
    

    if (var == "vid1_net_rating"){
      labels = c(labels,"Introductory Video Rating")
    }
    if (var == "vid2_net_rating"){
      labels = c(labels,"Social Distancing Video Rating")
    }
    if (var == "vid3_net_rating"){
      labels = c(labels,"Mask Info Video Rating")
    }
    if (var == "vid1_time"){
      labels = c(labels,"Time spent watching Introductory Video")
    }
    if (var == "vid2_time"){
      labels = c(labels,"Time spent watching Social Distancing Video")
    }
    if (var == "vid3_time"){
      labels = c(labels,"Time spent watching Mask Info Video")
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
get_filtered_data <- function(data_orig, dem_select = "All") {
  if (dem_select == "African-American") {
    data_orig <- data_orig %>% filter(resp_black == 1)
  }
  else if (dem_select == "Latinx") {
    data_orig <- data_orig %>% filter(resp_latino == 1)
  }
  
  if (dem_select == "Male") {
    data_orig <- data_orig %>% filter(resp_gender == "Male")
  }
  
  if (dem_select == "Female") {
    data_orig <- data_orig %>% filter(resp_gender == "Female")
  }
  
  if (dem_select == "African-American Male") {
    data_orig <- data_orig %>% filter(resp_black == 1) %>% filter(resp_gender == "Male")
  }
  
  if (dem_select == "African-American Female") {
    data_orig <- data_orig %>% filter(resp_black == 1) %>% filter(resp_gender == "Female")
  }
  
  if (dem_select == "Latinx Male") {
    data_orig <- data_orig %>% filter(resp_latino == 1) %>% filter(resp_gender == "Male")
  }
  
  if (dem_select == "Latinx Female") {
    data_orig <- data_orig %>% filter(resp_latino == 1) %>% filter(resp_gender == "Female")
  }
  
  
  return(data_orig)
  
}

# OLS regression
run_reg_outcome <- function(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=FALSE,DPL=FALSE) {

  
  support_y <- get_lasso_support(outcome, baseline_vars_dummies, data_reg)
  
  dpl_controls <- union(support_y, support_t_net)
  
  final_rhs <- primary_rhs
  
  if (DPL==FALSE) {
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet <- ifelse(dem_select == "All" | dem_select == "Male" | dem_select == "Female", "+ resp_race", "")
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet, dpl_snippet)) #new
  print(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet))
  
  model <- lm(model_spec, data_reg)
  if (balance_model){ # Observations with weight = NA are omitted from the regression
  if (outcome %in% links_outcomes){
    model <- lm(model_spec, data_reg,weights = data_reg$weights_links)
  }
  if (outcome %in% knowledge_outcomes){
    model <- lm(model_spec, data_reg,weights = data_reg$weights_knowledge)
  }
    if (outcome %in% videos_outcomes){
      model <- lm(model_spec, data_reg,weights = data_reg$weights_videos)
    }}

  
  
  return(model)
  
}

# Logit regression
run_reg_binary_outcome <- function(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=FALSE,DPL=FALSE) {

  support_y <- get_lasso_support(outcome, baseline_vars_dummies, data_reg)

  dpl_controls <- union(support_y, support_t_net)

  final_rhs <- primary_rhs

  if (DPL==FALSE) {
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet <- ifelse(dem_select == "All" | dem_select == "Male" | dem_select == "Female", "+ resp_race", "")
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet, dpl_snippet)) #new
  print(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet))

  model <- glm(formula=model_spec,family=binomial,data= data_reg,na.action="na.omit")
  if (balance_model){# Observations with weight = NA are omitted from the regression
  if (outcome %in% links_outcomes){
    model <- glm(formula=model_spec,family=binomial,data= data_reg,na.action="na.omit",weights = data_reg$weights_links)
  }
  if (outcome %in% knowledge_outcomes){
    model <- glm(formula=model_spec,family=binomial,data= data_reg,na.action="na.omit",weights = data_reg$weights_knowledge)
  }}


  
  return(model)
  
}

# Negative binomial (or Poisson) regression
run_reg_discrete_outcome <- function(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=FALSE,DPL=FALSE) {

  support_y <- get_lasso_support(outcome, baseline_vars_dummies, data_reg)

  dpl_controls <- union(support_y, support_t_net)
  
  final_rhs <- primary_rhs
  

  if (DPL==FALSE) {
    dpl_controls = c() # without DPL
  }
  dpl_snippet <- ifelse(length(dpl_controls) != 0, paste0("+",paste0(dpl_controls, collapse = "+")), "")
  
  extra_controls_snippet <- ifelse(dem_select == "All" | dem_select == "Male" | dem_select == "Female", "+ resp_race", "")
  
  model_spec <- as.formula(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet, dpl_snippet)) #new
  print(paste0(outcome, "~", final_rhs, "+ stratum + resp_lang", extra_controls_snippet))



    
  model = glm.nb(formula = model_spec,data = data_reg,na.action="na.omit",link=log)
  if (balance_model){# Observations with weight = NA are omitted from the regression

  if (outcome %in% links_outcomes){

    model <- glm.nb(formula = model_spec,data = data_reg,na.action="na.omit",weights = data_reg$weights_links,link=log)

  }
  if (outcome %in% knowledge_outcomes){

    model <- glm.nb(formula = model_spec,data = data_reg,na.action="na.omit",weights = data_reg$weights_knowledge,link=log)

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