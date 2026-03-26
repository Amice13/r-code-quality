#This script runs the mobility regressions for Table 2 and Table S4 (panel = Both campaigns)


DPL_regression<- function(var,treatments,covariates,data_reg,formula_str,DPL,baseline_var,print=FALSE){
  
  data_lasso = data_reg 
  if (DPL==TRUE){
    control_vars_formula_snippet <- paste0(covariates, collapse = "+")
    support_treatments = c()
    for (treat in treatments){
      lasso_formula_treatment <- as.formula(paste0(treat, "~ ", control_vars_formula_snippet,"+factor(state)+ day_1 + day_2 + day_3 + day_4 + day_5 + day_6 + day_7 "))
      model_lasso_treatment <- rlasso(formula = lasso_formula_treatment,
                                      penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                     lambda.start = NULL),
                                      data = data_lasso) #you might adjust the penalty argument here
      support_treatment <- names(model_lasso_treatment$coefficients[model_lasso_treatment$coefficients != 0])
      toRemove <- grep(pattern = "ntercept",x = support_treatment) #don't want intercept to be lasso selected!
      support_treatment <- support_treatment[-toRemove]
      toRemove <- grepl(pattern = "day",x = support_treatment,fixed=TRUE) #don't want dates to be lasso selected
      support_treatment <- support_treatment[!toRemove]
      toRemove <- grepl(pattern = "state",x = support_treatment,fixed=TRUE) #don't want states to be lasso selected
      support_treatment <- support_treatment[!toRemove]
      support_treatments = union(support_treatments,support_treatment)
      print(support_treatment)
    }
    lasso_formula_outcome <- as.formula(paste0(var, "~ ", control_vars_formula_snippet,"+factor(state)+day_1 + day_2 + day_3 + day_4 + day_5 + day_6 + day_7 + (day_1 + day_2 + day_3 + day_4 + day_5 + day_6 + day_7):",baseline_var))
    model_lasso_outcome <- rlasso(formula = lasso_formula_outcome,
                                  penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                 lambda.start = NULL),
                                  data = data_lasso) #you might adjust the penalty argument here
    support_outcome <- names(model_lasso_outcome$coefficients[model_lasso_outcome$coefficients != 0])
    toRemove <- grep(pattern = "ntercept",x = support_outcome) #don't want intercept to be lasso selected!
    support_outcome <- support_outcome[-toRemove]
    toRemove <- grepl(pattern = "day",x = support_outcome,fixed=TRUE) #don't want dates to be lasso selected
    support_outcome <- support_outcome[!toRemove]
    toRemove <- grepl(pattern = "state",x = support_outcome,fixed=TRUE) #don't want states to be lasso selected
    support_outcome <- support_outcome[!toRemove]
    print(support_outcome)
    outcome_controls <- union(support_outcome, support_treatments)
  }else{outcome_controls=c()}
  outcome_controls_formula_snippet <- ifelse(length(outcome_controls) != 0, paste0("+", paste0(outcome_controls, collapse = "+")),"")
  
  formula = as.formula(paste0(formula_str,outcome_controls_formula_snippet))
  reg = lm.cluster(formula = formula, data = data_reg, cluster = "user_loc")
  
  if (print==TRUE){
    print(formula)
  }
  return(reg)
  
}

for (DPL in c(FALSE,TRUE)){# whether we want to add DPL controls in the regressions or not
  
  
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  
  control_means = list()  
  control_sd = list()  
  
  treatment_means = list()  
  treatment_sd = list()  
  i = 1
  
  data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  var="movement_ch"
    

    data_for_regression$period_1 = 0
    data_for_regression$day_1=0
    data_for_regression$day_2=0
    data_for_regression$day_3=0
    data_for_regression$day_4=0
    data_for_regression$day_5=0
    data_for_regression$day_6=0
    data_for_regression$day_7=0
    data_for_regression$treated =  0
    
    
    data_for_regression$period_1[as.logical(indices_T1*(data_for_regression$date >= "2020-11-23")*(data_for_regression$date <= "2020-11-25"))]=1
    
    data_for_regression$day_1[as.logical(indices_T1*(data_for_regression$date == "2020-11-23"))]=1
    
    data_for_regression$day_2[as.logical(indices_T1*(data_for_regression$date == "2020-11-24"))]=1
    
    data_for_regression$day_3[as.logical(indices_T1*(data_for_regression$date == "2020-11-25"))]=1
    
    
    
    
    data_for_regression$treated[indices_T1] =  data_T1$high_county_T1
    
    
    
    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date >= "2020-12-21")*(data_for_regression$date <= "2020-12-23"))]=1
    
    
    data_for_regression$day_1[as.logical(indices_X*(data_for_regression$date == "2020-12-21"))]=1
    
    
    data_for_regression$day_2[as.logical(indices_X*(data_for_regression$date == "2020-12-22"))]=1
    
    
    data_for_regression$day_3[as.logical(indices_X*(data_for_regression$date == "2020-12-23"))]=1
    
    
    
    
    data_for_regression$treated[indices_X] =  data_X$high_county_X
    

    formula_str = paste0(var," ~  day_1 + day_2 + day_3  + treated +  (day_1 + day_2 + day_3):baseline_th_",var)
                       
    treatments_for_reg = c("treated")
    
    
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
    
    
    baseline_var=paste0("baseline_th_",var)
    

      reg = DPL_regression(var,treatments_for_reg,covariates_list,data_reg,formula_str,DPL,baseline_var,print=TRUE)

    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary["treated","Estimate"]
    pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
    se = reg_summary["treated", 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    formula = as.formula(paste(var,"  ~  ","1"))
    
    data_control = data_reg %>% filter(treated==0)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_means[[i]] = summary(reg)[,"Estimate"]
    control_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_means[[i]] =summary(reg)[,"Estimate"]
    treatment_sd[[i]] =summary(reg)[,"Std. Error"]
    
    i=i+1
    
    
    
  
  
  
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(nb_obs)){ 
    
    lines = c(lines,paste0(format(round(treatment_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_means[[i]] - 1.96*treatment_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_means[[i]] + 1.96*treatment_sd[[i]], digits=3), nsmall = 3),")")) 
    
    lines = c(lines,paste0(format(round(control_means[[i]], digits=3), nsmall = 3)," (",format(round(control_means[[i]] - 1.96*control_sd[[i]], digits=3), nsmall = 3),",",format(round(control_means[[i]] + 1.96*control_sd[[i]], digits=3), nsmall = 3),")")) 
    
    temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }
  tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
  
  cols = c("High","Low","High Intensity (coef)","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Distance Traveled",1))
  
  write.csv(tab, paste0("../Output/Mobility/Pooled_Distance_Traveled_DPL_",DPL,".csv"))
  
}




for (DPL in c(FALSE,TRUE)){# whether we want to add DPL controls in the regressions or not
  
  
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  
  control_means = list()  
  control_sd = list()  
  
  treatment_means = list()  
  treatment_sd = list()  
  i = 1
  
  data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  var="leave_home"

    
    #Thanksgiving/Xmas days
    
    
    data_for_regression$period_1 = 0
    data_for_regression$day_1=0
    data_for_regression$day_2=0
    data_for_regression$day_3=0
    data_for_regression$day_4=0
    data_for_regression$day_5=0
    data_for_regression$day_6=0
    data_for_regression$day_7=0
    data_for_regression$treated =  0
    
    
    data_for_regression$period_1[as.logical(indices_T1*(data_for_regression$date == "2020-11-26"))]=1
    
    data_for_regression$day_1[as.logical(indices_T1*(data_for_regression$date == "2020-11-26"))]=1

    
    data_for_regression$treated[indices_T1] =  data_T1$high_county_T1

    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date >= "2020-12-24")*(data_for_regression$date <= "2020-12-25"))]=1
    
    
    data_for_regression$day_1[as.logical(indices_X*(data_for_regression$date == "2020-12-24"))]=1
    
    
    data_for_regression$day_2[as.logical(indices_X*(data_for_regression$date == "2020-12-25"))]=1
    

    
    data_for_regression$treated[indices_X] =  data_X$high_county_X
    
    
    formula_str = paste0(var," ~  day_1 + day_2  + treated +  (day_1 + day_2):baseline_th_",var)
    treatments_for_reg = c("treated")
    
    
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
    
    
    baseline_var=paste0("baseline_th_",var)
    

      reg = DPL_regression(var,treatments_for_reg,covariates_list,data_reg,formula_str,DPL,baseline_var,print=TRUE)

    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary["treated","Estimate"]
    pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
    se = reg_summary["treated", 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    formula = as.formula(paste(var,"  ~  ","1"))
    
    data_control = data_reg %>% filter(treated==0)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_means[[i]] = summary(reg)[,"Estimate"]
    control_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_means[[i]] =summary(reg)[,"Estimate"]
    treatment_sd[[i]] =summary(reg)[,"Std. Error"]
    
    i=i+1
    
  
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(nb_obs)){ 
    
    lines = c(lines,paste0(format(round(treatment_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_means[[i]] - 1.96*treatment_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_means[[i]] + 1.96*treatment_sd[[i]], digits=3), nsmall = 3),")")) 
    
    lines = c(lines,paste0(format(round(control_means[[i]], digits=3), nsmall = 3)," (",format(round(control_means[[i]] - 1.96*control_sd[[i]], digits=3), nsmall = 3),",",format(round(control_means[[i]] + 1.96*control_sd[[i]], digits=3), nsmall = 3),")")) 
    
    temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }
  tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
  
  cols = c("High","Low","High Intensity (coef)","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Share Ever Left Home",1))
  
  write.csv(tab, paste0("../Output/Mobility/Pooled_Leave_Home_DPL_",DPL,".csv"))
  
}