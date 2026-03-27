# This script runs the mobility regressions for Table 2 and Table S4 (Thanksgiving or Christmas campaigns)

set.seed(787567)


###########################
### Regressions for Mobility
###########################

## Definition of double post lasso procedures for the following regressions


# In every regression, we interact the baseline with date
# At thanksgiving, we only have one day (nov 26) in the "Share Ever Left Home" regression, so we adapt the regressions by removing the date interaction

DPL_regression_movement<- function(var,treatments,covariates,data_reg,formula_str,DPL,baseline_var,print=FALSE){
  
  data_lasso = data_reg 
  if (DPL==TRUE){
    control_vars_formula_snippet <- paste0(covariates, collapse = "+")
    support_treatments = c()
    for (treat in treatments){
      lasso_formula_treatment <- as.formula(paste0(treat, "~ ", control_vars_formula_snippet,"+ factor(date)"))
      model_lasso_treatment <- rlasso(formula = lasso_formula_treatment,
                                      penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                     lambda.start = NULL),
                                      data = data_lasso) #you might adjust the penalty argument here
      support_treatment <- names(model_lasso_treatment$coefficients[model_lasso_treatment$coefficients != 0])
      toRemove <- grep(pattern = "ntercept",x = support_treatment) #don't want intercept to be lasso selected!
      support_treatment <- support_treatment[-toRemove]
      toRemove <- grepl(pattern = "factor(date)",x = support_treatment,fixed=TRUE) #don't want dates to be lasso selected
      support_treatment <- support_treatment[!toRemove]
      support_treatments = union(support_treatments,support_treatment)
      print(support_treatment)
    }
    lasso_formula_outcome <- as.formula(paste0(var, "~ ", control_vars_formula_snippet," + factor(date)+  factor(date):",baseline_var))
    model_lasso_outcome <- rlasso(formula = lasso_formula_outcome,
                                  penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                 lambda.start = NULL),
                                  data = data_lasso) #you might adjust the penalty argument here
    support_outcome <- names(model_lasso_outcome$coefficients[model_lasso_outcome$coefficients != 0])
    toRemove <- grep(pattern = "ntercept",x = support_outcome) #don't want intercept to be lasso selected!
    support_outcome <- support_outcome[-toRemove]
    toRemove <- grepl(pattern = "factor(date)",x = support_outcome,fixed=TRUE) #don't want dates to be lasso selected
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


DPL_regression_leave_home_thanksgiving<- function(var,treatments,covariates,data_reg,formula_str,DPL,baseline_var,print=FALSE){
  
  data_lasso = data_reg 
  if (DPL==TRUE){
    control_vars_formula_snippet <- paste0(covariates, collapse = "+")
    support_treatments = c()
    for (treat in treatments){
      lasso_formula_treatment <- as.formula(paste0(treat, "~ ", control_vars_formula_snippet))
      model_lasso_treatment <- rlasso(formula = lasso_formula_treatment,
                                      penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                     lambda.start = NULL),
                                      data = data_lasso) #you might adjust the penalty argument here
      support_treatment <- names(model_lasso_treatment$coefficients[model_lasso_treatment$coefficients != 0])
      toRemove <- grep(pattern = "ntercept",x = support_treatment) #don't want intercept to be lasso selected!
      support_treatment <- support_treatment[-toRemove]
      support_treatments = union(support_treatments,support_treatment)
      print(support_treatment)
    }
    lasso_formula_outcome <- as.formula(paste0(var, "~ ", control_vars_formula_snippet," + ",baseline_var))
    model_lasso_outcome <- rlasso(formula = lasso_formula_outcome,
                                  penalty = list(homoscedastic = FALSE, X.dependent.lambda = FALSE, 
                                                 lambda.start = NULL),
                                  data = data_lasso) #you might adjust the penalty argument here
    support_outcome <- names(model_lasso_outcome$coefficients[model_lasso_outcome$coefficients != 0])
    toRemove <- grep(pattern = "ntercept",x = support_outcome) #don't want intercept to be lasso selected!
    support_outcome <- support_outcome[-toRemove]
    
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

for (treatment in c("T1","X")){ # for each treatment (T1 = thanksgiving treatment, X = christmas treatment)
  
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

## Start regressions

for (var in c("movement_ch","leave_home")){ # Our two outcomes
  
  formula_str = switch(var,"movement_ch"=paste0(var," ~ factor(date) + treated  + factor(date):baseline_th_",var),
                       "leave_home"=paste0(var," ~ treated  + baseline_th_",var))
  
  if((var!="leave_home")||(treatment=="X")){
    formula_str=paste0(var," ~ factor(date) + treated  + factor(date):baseline_th_",var)
  }else{
    formula_str=paste0(var," ~ treated  + baseline_th_",var) ## adapt formula for Thanksgiving Share Ever Left Home
  }
  
  baseline_var=paste0("baseline_th_",var)
  treatments_for_reg = c("treated")
  
  spec = paste0(treatment,"_",var)

  start_date = switch(spec,"T1_movement_ch"="2020-11-23","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-21","X_leave_home"="2020-12-24")
  end_date = switch(spec,"T1_movement_ch"="2020-11-25","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-23","X_leave_home"="2020-12-25")

    data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)
  
  data_reg$treated = data_reg[[paste0("high_county_",treatment)]]
  
  if((var!="leave_home")||(treatment=="X")){
    reg = DPL_regression_movement(var,treatments_for_reg,covariates_list,data_reg,formula_str,DPL,baseline_var,print=TRUE)
  }else{
    reg = DPL_regression_leave_home_thanksgiving(var,treatments_for_reg,covariates_list,data_reg,formula_str,DPL,baseline_var,print=TRUE)
    ## adapt formula for Thanksgiving Share Ever Left Home
  }
  
  reg_summary <- summary(reg)
  
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
  
  
}



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
tab = t(matrix(lines, ncol= 2)) # matrix containing the table to export




cols = c("High","Low","High Intensity (coef)","p-value")



colnames(tab)=c(cols,"Observations")
rownames(tab)=c("Distance Traveled","Share Ever Left Home")

write.csv(tab, paste0("../Output/Mobility/",treatment,"_Movement_outcomes_DPL_",DPL,".csv"))



}}


