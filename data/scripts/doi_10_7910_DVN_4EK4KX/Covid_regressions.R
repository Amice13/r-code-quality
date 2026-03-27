#This script runs the Covid-19 cases regressions for Table 3 and robustness tables: Table S6a, Table S6b and Table S6c

for (var in c("asinh_two_weeks_cases","log_two_weeks_cases_half_min","log_two_weeks_cases_plus_1","log_two_weeks_cases_zeros_omitted")){
  for (treatment in c("T1","X")){ # for each treatment (T1 = thanksgiving treatment, X = christmas treatment)
    
    regression_dates = switch(treatment,"T1"=c("2020-12-14"),"X"=c("2021-01-14"))
    
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
    for (regression_date in regression_dates){
      
      
      formula_str = switch(treatment,"X"= paste0(var," ~  treated +  baseline_ch_log_cases + factor(user_loc)"),
                           "T1"=paste0(var," ~  treated + baseline_th_log_cases + factor(user_loc)"))
      
      # for all counties (both treated and control)
      data_reg = data%>% filter(date == regression_date)
      treatments = c("treated")
      data_reg$treated = data_reg[[paste0("treated_",treatment)]]
      data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
      data_reg = data_reg %>% filter(!is.na(high_county))
      reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary["treated","Estimate"]
      pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
      se = reg_summary["treated", 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      
      
      formula = as.formula(paste(var,"  ~  ","1"))
      
      data_control = data_reg %>% filter(treated==0)
      reg =  lm.cluster(formula = formula, data = data_control,cluster="zip")
      
      control_means[[i]] = summary(reg)[,"Estimate"]
      control_sd[[i]] = summary(reg)[,"Std. Error"]
      
      data_treated = data_reg %>% filter(treated==1)
      reg =  lm.cluster(formula = formula, data = data_treated,cluster="zip")
      
      treatment_means[[i]] =summary(reg)[,"Estimate"]
      treatment_sd[[i]] =summary(reg)[,"Std. Error"]
      
      i=i+1
      
      
      for (j in 0:1){
        data_reg = data %>% filter(date == regression_date)
        treatments = c("treated")
        data_reg$treated = data_reg[[paste0("treated_",treatment)]]
        data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
        data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
        reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
        
        reg_summary <- summary(reg)
        coefficients[[i]] = reg_summary["treated","Estimate"]
        pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
        se = reg_summary["treated", 'Std. Error']
        CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
        CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
        nb_obs[[i]] = nobs(lm(reg$lm_res))
        
        
        
        formula = as.formula(paste(var,"  ~  ","1"))
        
        data_control = data_reg %>% filter(treated==0)
        reg =  lm.cluster(formula = formula, data = data_control,cluster="zip")
        
        control_means[[i]] = summary(reg)[,"Estimate"]
        control_sd[[i]] = summary(reg)[,"Std. Error"]
        
        data_treated = data_reg %>% filter(treated==1)
        reg =  lm.cluster(formula = formula, data = data_treated,cluster="zip")
        
        treatment_means[[i]] =summary(reg)[,"Estimate"]
        treatment_sd[[i]] =summary(reg)[,"Std. Error"]
        
        i=i+1
      }
 
      
      
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
    tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
    
    cols = c("Treated","Control","Treated (coef)","p-value")
    
    
    colnames(tab)=c(cols,"Observations")
    rownames(tab)=c(rep(var,3))
    
    write.csv(tab, paste0("../Output/Covid/",treatment,"_Covid_cases_",var,".csv"))
  }
}

## Pooled regression
for (var in c("asinh_two_weeks_cases","log_two_weeks_cases_half_min","log_two_weeks_cases_plus_1","log_two_weeks_cases_zeros_omitted")){

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
  
  
  dates_T1 = c("2020-12-14")
  dates_X = c("2021-01-14")
  
  for (d in c(1:1)){
    
    
    
    formula_str = paste0(var," ~  treated +high_county+ baseline_log_cases+factor(user_loc) ")
    
    
    data_for_regression$period_1 = 0
    data_for_regression$treated =  0
    data_for_regression$high_county =  0
    data_for_regression$baseline_log_cases=0
    
    data_for_regression$period_1[as.logical(indices_T1*(data_for_regression$date == dates_T1[d]))]=1
    
    
    data_for_regression$treated[indices_T1] =  data_T1$treated_T1
    data_for_regression$high_county[indices_T1] =  data_T1$high_county_T1
    data_for_regression$baseline_log_cases[indices_T1] =data_T1$baseline_th_log_cases
    
    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date == dates_X[d]))]=1
    
    data_for_regression$treated[indices_X] =  data_X$treated_X
    data_for_regression$high_county[indices_X] =  data_X$high_county_X
    data_for_regression$baseline_log_cases[indices_X] =data_X$baseline_ch_log_cases
    
    
    data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
    treatments = c("treated")
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    
    reg_summary <- summary(reg)
    
    coefficients[[i]] = reg_summary["treated","Estimate"]
    pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
    se = reg_summary["treated", 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    formula = as.formula(paste(var,"  ~  ","1"))
    
    data_control = data_reg %>% filter(treated==0)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="zip")
    
    control_means[[i]] = summary(reg)[,"Estimate"]
    control_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="zip")
    
    treatment_means[[i]] =summary(reg)[,"Estimate"]
    treatment_sd[[i]] =summary(reg)[,"Std. Error"]
    
    i=i+1
    
    for (j in 0:1){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      treatments = c("treated")
      
      data_reg = data_reg  %>% filter(high_county==j)
      reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary["treated","Estimate"]
      pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
      se = reg_summary["treated", 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      
      
      formula = as.formula(paste(var,"  ~  ","1"))
      
      data_control = data_reg %>% filter(treated==0)
      reg =  lm.cluster(formula = formula, data = data_control,cluster="zip")
      
      control_means[[i]] = summary(reg)[,"Estimate"]
      control_sd[[i]] = summary(reg)[,"Std. Error"]
      
      data_treated = data_reg %>% filter(treated==1)
      reg =  lm.cluster(formula = formula, data = data_treated,cluster="zip")
      
      treatment_means[[i]] =summary(reg)[,"Estimate"]
      treatment_sd[[i]] =summary(reg)[,"Std. Error"]
      
      i=i+1
    }
    
    
    
    
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
  tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
  
  cols = c("Treated","Control","Treated (coef)","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep(var,3))
  
  write.csv(tab, paste0("../Output/Covid/Pooled_Covid_cases_",var,".csv"))
}

