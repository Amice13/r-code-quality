#This script computes the Randomization Inference p-values for Table 3


# N.B: in this script, the results are outputted in this order: Low Intensity counties, High Intensity counties, All counties
# i.e they are not in the same order as in the Tables in the paper

var="asinh_two_weeks_cases" #the outcome

nb_randomization=1000


  for (treatment in c("T1","X")){
    randomized_treatments = read.csv("../Data/randomized_zip.csv")
    to_remove=switch(treatment,"T1"="X","X"="T1")
    randomized_treatments = randomized_treatments[,colnames(randomized_treatments)[!grepl(to_remove,colnames(randomized_treatments),fixed=TRUE)]]
    randomized_treatments$X=NULL                                          
    p_treated_RI = c()
    
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
    k = 1
    for (regression_date in regression_dates){
      
      
      formula_str = switch(treatment,"X"= paste0(var," ~  treated +  baseline_ch_log_cases + factor(user_loc)"),
                           "T1"=paste0(var," ~  treated + baseline_th_log_cases + factor(user_loc)"))
      
      for (j in 0:1){
        data_reg = data %>% filter(date == regression_date)
        treatments = c("treated")
        data_reg$treated = data_reg[[paste0("treated_",treatment)]]
        data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
        data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
        reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
        
        reg_summary <- summary(reg)
        
        coefficients[[k]] = reg_summary["treated","Estimate"]
        pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
        se = reg_summary["treated", 'Std. Error']
        CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
        CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
        nb_obs[[k]] = nobs(lm(reg$lm_res))
        
        pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)
        coef=c()
        for (i in c(1:nb_randomization)){
          data_reg = data %>% filter(date == regression_date)
          
          data_reg =  merge(data_reg,randomized_treatments[,c("user_loc","zip",paste0("treated_zip_",treatment,"_",i),paste0("high_county_",treatment,"_",i))])
          data_reg$treated = data_reg[[paste0("treated_zip_",treatment,"_",i)]]
          data_reg$high_county= data_reg[[paste0("high_county_",treatment,"_",i)]]
          data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
          reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
          
          reg_summary <- summary(reg)
          
        coef = c(coef, reg_summary["treated","Estimate"])
          
          setTxtProgressBar(pb, i)
        }
        close(pb)
        p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)
        k=k+1
      }
      # for all counties (both treated and control)
      data_reg = data%>% filter(date == regression_date)
      treatments = c("treated")
      data_reg$treated = data_reg[[paste0("treated_",treatment)]]
      data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
      data_reg = data_reg %>% filter(!is.na(high_county))
      reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[k]] = reg_summary["treated","Estimate"]
      pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
      se = reg_summary["treated", 'Std. Error']
      CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
      CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
      nb_obs[[k]] = nobs(lm(reg$lm_res))
      
      pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)
      coef=c()
      for (i in c(1:nb_randomization)){
        data_reg = data %>% filter(date == regression_date)
        
        data_reg =  merge(data_reg,randomized_treatments[,c("user_loc","zip",paste0("treated_zip_",treatment,"_",i),paste0("high_county_",treatment,"_",i))])
        data_reg$treated = data_reg[[paste0("treated_zip_",treatment,"_",i)]]
        data_reg$high_county= data_reg[[paste0("high_county_",treatment,"_",i)]]
        data_reg = data_reg %>% filter(!is.na(high_county)) 
        reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
        
        reg_summary <- summary(reg)
        
        coef = c(coef, reg_summary["treated","Estimate"])
        
        setTxtProgressBar(pb, i)
      }
      close(pb)
      p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)
      
      k=k+1
      
      
      
    }
    
    
    ##
    ## We build the table and then export it to .csv:
    ##
    
    lines = c()
    for (i in 1:length(nb_obs)){ 
      
      temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
      lines = c(lines,temp)
      lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
      lines = c(lines,format(round(p_treated_RI[[i]], digits=3), nsmall = 3))
      lines = c(lines,nb_obs[[i]]) # Number of observations
      
    }
    tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
    
    cols = c("Treated (coef)","p-value","RI p-value")
    
    
    colnames(tab)=c(cols,"Observations")
    rownames(tab)=c(rep("Log(Fortnightly Cases)",3))
    
    write.csv(tab, paste0("../Output/Covid/RI_",treatment,"_",var,".csv"))
  }


## Pooled regression

randomized_treatments = read.csv("../Data/randomized_zip.csv")
randomized_treatments$X=NULL

  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  p_treated_RI=c()
  k = 1
  
  data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  data_for_regression$campaign=0 # 0: thanksgiving, 1: christmas
  data_for_regression$campaign[indices_X]=1
  
  dates_T1 = c("2020-12-14","2020-12-28","2021-01-11")
  dates_X = c("2021-01-14","2021-01-28","2021-02-11")
  
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
    
    for (j in 0:1){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      treatments = c("treated")
      
      data_reg = data_reg  %>% filter(high_county==j)
      reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[k]] = reg_summary["treated","Estimate"]
      pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
      se = reg_summary["treated", 'Std. Error']
      CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
      CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
      nb_obs[[k]] = nobs(lm(reg$lm_res))
      
      coef=c()
      pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)
      for (i in c(1:nb_randomization)){
        data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
        
        data_reg =  merge(data_reg,randomized_treatments[,c("user_loc","zip",paste0("treated_zip_T1_",i),paste0("high_county_T1_",i),paste0("treated_zip_X_",i),paste0("high_county_X_",i))])
        data_reg$treated[data_reg$campaign==0] = data_reg[[paste0("treated_zip_T1_",i)]][data_reg$campaign==0]
        data_reg$high_county[data_reg$campaign==0]= data_reg[[paste0("high_county_T1_",i)]][data_reg$campaign==0]
        data_reg$treated[data_reg$campaign==1] = data_reg[[paste0("treated_zip_X_",i)]][data_reg$campaign==1]
        data_reg$high_county[data_reg$campaign==1]= data_reg[[paste0("high_county_X_",i)]][data_reg$campaign==1]
        data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
        reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
        
        reg_summary <- summary(reg)
        
        coef = c(coef, reg_summary["treated","Estimate"])
        
        setTxtProgressBar(pb, i)
      }
      close(pb)
      p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)
      

      k=k+1
    }
    
    
    
    
    formula_str = paste0(var," ~  treated +high_county+ baseline_log_cases+factor(user_loc) ")
    data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
    treatments = c("treated")
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    
    reg_summary <- summary(reg)
    
    coefficients[[k]] = reg_summary["treated","Estimate"]
    pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
    se = reg_summary["treated", 'Std. Error']
    CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
    CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
    nb_obs[[k]] = nobs(lm(reg$lm_res))
    
    coef=c()
    pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)
    for (i in c(1:nb_randomization)){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      
      data_reg =  merge(data_reg,randomized_treatments[,c("user_loc","zip",paste0("treated_zip_T1_",i),paste0("high_county_T1_",i),paste0("treated_zip_X_",i),paste0("high_county_X_",i))])
      data_reg$treated[data_reg$campaign==0] = data_reg[[paste0("treated_zip_T1_",i)]][data_reg$campaign==0]
      data_reg$high_county[data_reg$campaign==0]= data_reg[[paste0("high_county_T1_",i)]][data_reg$campaign==0]
      data_reg$treated[data_reg$campaign==1] = data_reg[[paste0("treated_zip_X_",i)]][data_reg$campaign==1]
      data_reg$high_county[data_reg$campaign==1]= data_reg[[paste0("high_county_X_",i)]][data_reg$campaign==1]
      data_reg = data_reg %>% filter(!is.na(high_county)) 
      reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coef = c(coef, reg_summary["treated","Estimate"])
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
    p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)
    
    

    k=k+1
    
  }
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(nb_obs)){ 
    

    temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
    lines = c(lines,format(round(p_treated_RI[[i]], digits=3), nsmall = 3))
    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }
  tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
  
  cols = c("Treated (coef)","p-value","p-value RI")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Log(Fortnightly Cases)",3))
  
  write.csv(tab, paste0("../Output/Covid/RI_Pooled_zip_cases_",var,".csv"))
