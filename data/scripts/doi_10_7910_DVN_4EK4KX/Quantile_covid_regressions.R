#This script runs the Covid-19 cases quantile regressions for Table S5

# NB: Unfortunately a different seed was used for the Table in the paper, which explains the standard errors difference
set.seed(89173)

#Quantile regressions with boostrapped standard errors

for (tau in c(0.5)){
  for (var in c("log_two_weeks_cases_plus_1")){
    
    for (treatment in c("T1","X")){
      
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
        
        
        baseline_var= switch(treatment,"X"= "baseline_ch_log_cases",
                             "T1"="baseline_th_log_cases")
        # for all counties (both treated and control)
        data_reg = data%>% filter(date == regression_date)
        treatments = c("treated")
        data_reg$treated = data_reg[[paste0("treated_",treatment)]]
        data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
        data_reg = data_reg %>% filter(!is.na(high_county))
        
        
        data_reg = data_reg %>% filter(!is.na(user_loc))%>% filter(!is.na(log_two_weeks_cases_plus_1))
        data_reg=data_reg[!is.na(data_reg[[baseline_var]]),]
        data_reg$Y_hat = residuals(lm(as.formula(paste0(var,"~",baseline_var,"+factor(user_loc)")),data=data_reg))
        data_reg$treated_hat = residuals(lm(as.formula(paste0("treated ~",baseline_var,"+ factor(user_loc)")),data=data_reg))
        ## Variables are de-meaned to accelerate the computation of the boostrapped standard errors
        
        
        reg = rq(as.formula("Y_hat ~  treated_hat"), tau =tau, data = data_reg,na.action=na.omit)
        
        reg_summary = coef(summary.rq(reg,se="boot"))
        coefficients[[i]] = reg_summary["treated_hat","Value"]
        pvalues[[i]] <- reg_summary["treated_hat", 'Pr(>|t|)']
        se = reg_summary["treated_hat", 'Std. Error']
        CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
        CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
        
        
        
        
        i=i+1
        for (j in 0:1){
          data_reg = data %>% filter(date == regression_date)
          treatments = c("treated")
          data_reg$treated = data_reg[[paste0("treated_",treatment)]]
          data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
          data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
          
          data_reg = data_reg %>% filter(!is.na(user_loc))%>% filter(!is.na(log_two_weeks_cases_plus_1))
          data_reg=data_reg[!is.na(data_reg[[baseline_var]]),]
          data_reg$Y_hat = residuals(lm(as.formula(paste0(var,"~",baseline_var,"+factor(user_loc)")),data=data_reg))
          data_reg$treated_hat = residuals(lm(as.formula(paste0("treated ~",baseline_var,"+ factor(user_loc)")),data=data_reg))
          ## Variables are de-meaned to accelerate the computation of the boostrapped standard errors
          
          reg = rq(as.formula("Y_hat ~  treated_hat"), tau =tau, data = data_reg,na.action=na.omit)
          
          
          reg_summary = coef(summary.rq(reg,se="boot"))
          
          coefficients[[i]] = reg_summary["treated_hat","Value"]
          pvalues[[i]] <- reg_summary["treated_hat", 'Pr(>|t|)']
          se = reg_summary["treated_hat", 'Std. Error']
          CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
          CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
          
          
          i=i+1
        }
     
        
        
        
      }
      
      
      ##
      ## We build the table and then export it to .csv:
      ##
      
      lines = c()
      for (i in 1:length(coefficients)){ 
        
        
        temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
        lines = c(lines,temp)
        lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
        
        
      }
      tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
      
      cols = c("Treated (coef)","p-value")
      
      
      colnames(tab)=c(cols)
      rownames(tab)=c(rep("Log(Fortnightly Cases)",3))
      
      write.csv(tab, paste0("../Output/Covid//tau_",tau,"_",treatment,"_",var,".csv"))
    }
  }
}

# Pooled quantile reg
for (tau in c(0.5)){
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
  
  var="log_two_weeks_cases_plus_1"
  
  dates_T1 = c("2020-12-14","2020-12-28","2021-01-11")
  dates_X = c("2021-01-14","2021-01-28","2021-02-11")
  
  for (d in c(1:1)){
    
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
    data_reg = data_reg %>% filter(!is.na(user_loc))%>% filter(!is.na(log_two_weeks_cases_plus_1))
    data_reg=data_reg[!is.na(data_reg[[baseline_var]]),]
    data_reg$Y_hat = residuals(lm(as.formula(paste0(var,"~",baseline_var,"+high_county+factor(user_loc)")),data=data_reg))
    data_reg$treated_hat = residuals(lm(as.formula(paste0("treated ~",baseline_var,"+high_county+ factor(user_loc)")),data=data_reg))
    
    
    reg = rq(as.formula("Y_hat ~  treated_hat"), tau =tau, data = data_reg,na.action=na.omit)
    
    reg_summary = coef(summary.rq(reg,se="boot"))
    coefficients[[i]] = reg_summary["treated_hat","Value"]
    pvalues[[i]] <- reg_summary["treated_hat", 'Pr(>|t|)']
    se = reg_summary["treated_hat", 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    
    
    
    i=i+1
    
    for (j in 0:1){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      treatments = c("treated")
      
      data_reg = data_reg  %>% filter(high_county==j)
      data_reg = data_reg %>% filter(!is.na(user_loc))%>% filter(!is.na(log_two_weeks_cases_plus_1))
      data_reg=data_reg[!is.na(data_reg[[baseline_var]]),]
      data_reg$Y_hat = residuals(lm(as.formula(paste0(var,"~",baseline_var,"+high_county+factor(user_loc)")),data=data_reg))
      data_reg$treated_hat = residuals(lm(as.formula(paste0("treated ~",baseline_var,"+ high_county+factor(user_loc)")),data=data_reg))
      
      
      reg = rq(as.formula("Y_hat ~  treated_hat"), tau =tau, data = data_reg,na.action=na.omit)
      
      reg_summary = coef(summary.rq(reg,se="boot"))
      
      coefficients[[i]] = reg_summary["treated_hat","Value"]
      pvalues[[i]] <- reg_summary["treated_hat", 'Pr(>|t|)']
      se = reg_summary["treated_hat", 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      
      
      
      
      i=i+1
    }
    
    
  }
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(coefficients)){ 
    
    temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
    
    
  }
  tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
  
  cols = c("Treated (coef)","p-value")
  
  
  colnames(tab)=cols
  rownames(tab)=c(rep("Log(Fortnightly Cases)",3))
  
  write.csv(tab, paste0("../Output/Covid//pooled_tau_",tau,"_",var,".csv"))
}