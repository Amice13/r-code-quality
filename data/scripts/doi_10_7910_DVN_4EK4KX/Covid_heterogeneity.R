#This script runs the Covid-19 cases regressions with interactions

# Create some heterogeneity variables

temp = distinct(data[,c("zip","user_loc",
                        "baseline_ch_log_cases",
                        "baseline_th_log_cases")])

temp$high_log_cumulative_cases_th = as.numeric(temp$baseline_th_log_cases >median(temp$baseline_th_log_cases,na.rm=TRUE))
temp$high_log_cumulative_cases_ch = as.numeric(temp$baseline_ch_log_cases >median(temp$baseline_ch_log_cases,na.rm=TRUE))

data_het= merge(data,temp[,c("user_loc","zip","high_log_cumulative_cases_th","high_log_cumulative_cases_ch")],by=c("user_loc","zip"),all.x=TRUE)

temp = distinct(data_het[,c("user_loc","propurban")])
temp$high_urban = as.numeric(temp$propurban > median(temp$propurban,na.rm=TRUE))
temp=na.omit(temp)
data_het = merge(data_het,temp[,c("user_loc","high_urban")],by=c("user_loc"),all.x=TRUE)

data_het$majority_urban = as.numeric(data_het$share_urban>0.5)

data_het$urban_rep = data_het$majority_gop * data_het$majority_urban
data_het$not_in_X_sample=as.numeric(!(data_het$user_loc %in% christmas_data$user_loc))

# Regressions 

for (treatment in (c("T1","X"))){
  
  heterogeneity_variables = switch(treatment,"T1"=c("majority_urban","majority_gop","high_education","high_log_cumulative_cases","not_in_X_sample"),
                                   "X"=c("majority_urban","majority_gop","high_education","high_log_cumulative_cases"))
for (het_var in heterogeneity_variables){

    
    if (het_var=="high_log_cumulative_cases"){
      het_var = switch(treatment,"T1"="high_log_cumulative_cases_th","X"="high_log_cumulative_cases_ch")
    }
    
    var="asinh_two_weeks_cases"
    regression_dates = switch(treatment,"T1"=c("2020-12-14"),"X"=c("2021-01-14"))
    
    
    
    coefficients = list()
    standarderrors = list()
    CI_lower_bound = list()
    CI_upper_bound = list()
    pvalues = list()
    models <- list()
    cmeans <- c()
    nb_obs = list()  
    
    i = 1
    for (regression_date in regression_dates){
      
      
      formula_str = switch(treatment,"X"= paste0(var," ~  treated_het_var + treated + het_var +  baseline_ch_log_cases + factor(user_loc)"),
                           "T1"=paste0(var," ~  treated_het_var + treated + het_var + baseline_th_log_cases + factor(user_loc)"))
      # for all counties (both treated and control)
      data_reg = data_het %>% filter(date == regression_date)
      treatments = c("treated_het_var","treated","het_var")
      data_reg$het_var = data_reg[[het_var]]
      data_reg$treated = data_reg[[paste0("treated_",treatment)]]
      data_reg$treated_het_var = data_reg$het_var * data_reg$treated 
      data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
      data_reg = data_reg %>% filter(!is.na(high_county))
      reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary[,"Estimate"]
      pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
      se = reg_summary[, 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      i=i+1
      
      for (j in 0:1){
        data_reg = data_het %>% filter(date == regression_date)
        data_reg$het_var = data_reg[[het_var]]
        data_reg$treated = data_reg[[paste0("treated_",treatment)]]
        data_reg$treated_het_var = data_reg$het_var * data_reg$treated 
        data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
        data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
        reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
        
        reg_summary <- summary(reg)
        
        coefficients[[i]] = reg_summary[,"Estimate"]
        pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
        se = reg_summary[, 'Std. Error']
        CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
        CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
        nb_obs[[i]] = nobs(lm(reg$lm_res))
        
        
        
        
        i=i+1
        
      }
     
      
      
    }
    
    
    ##
    ## We build the table and then export it to .csv:
    ##
    
    
    lines = c()
    for (i in 1:length(nb_obs)){ 
      
     
      temp = paste0(format(round(coefficients[[i]][["treated_het_var"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_het_var"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_het_var"]], digits=3), nsmall = 3),")") # coeff and CI
      lines = c(lines,temp)
      lines = c(lines,format(round(pvalues[[i]][["treated_het_var"]], digits=3), nsmall = 3))
      
      temp = paste0(format(round(coefficients[[i]][["treated"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated"]], digits=3), nsmall = 3),")") # coeff and CI
      lines = c(lines,temp)
      lines = c(lines,format(round(pvalues[[i]][["treated"]], digits=3), nsmall = 3))
      
      temp = paste0(format(round(coefficients[[i]][["het_var"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["het_var"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["het_var"]], digits=3), nsmall = 3),")") # coeff and CI
      lines = c(lines,temp)
      lines = c(lines,format(round(pvalues[[i]][["het_var"]], digits=3), nsmall = 3))
      
      
      lines = c(lines,nb_obs[[i]]) # Number of observations
      
    }
    tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
    
    cols = c("Treated","Control","Treated (coef)","p-value")
    
    
    
    cols = c("Treated x group1","p-value","Treated","p-value", "group1","p-value")
    
    
    colnames(tab)=c(cols,"Observations")
    rownames(tab)=c(rep("Asinh(Fortnightly Cases)",3))
    
    write.csv(tab, paste0("../Output/Covid_Heterogeneity/",treatment,"_heterogeneity_",het_var,"_zip_cases.csv"))
  }}


### Additional heterogeneity table urban x party


for (treatment in (c("T1","X"))){
  
  
  var="asinh_two_weeks_cases"
  regression_dates = switch(treatment,"T1"=c("2020-12-14"),"X"=c("2021-01-14"))
  
  
  
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()
  
  i = 1
  for (regression_date in regression_dates){
    
    
    formula_str = switch(treatment,"X"= paste0(var," ~  treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban +  baseline_ch_log_cases + factor(user_loc)"),
                         "T1"=paste0(var," ~ treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban + baseline_th_log_cases  + factor(user_loc)"))
    # for all counties (both treated and control)
    data_reg = data_het %>% filter(date == regression_date)
    
    data_reg$treated = data_reg[[paste0("treated_",treatment)]]
    data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
    data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
    data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
    data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
    data_reg = data_reg %>% filter(!is.na(high_county))
    reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    
    reg_summary <- summary(reg)
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    i=i+1
    for (j in 0:1){
      data_reg = data_het %>% filter(date == regression_date)
      treatments = c("treated_urban_rep", "treated_gop", "treated","treated_urban")
      data_reg$treated = data_reg[[paste0("treated_",treatment)]]
      data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
      data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
      data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
      data_reg$high_county= data_reg[[paste0("high_county_",treatment)]]
      data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
      reg =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary[,"Estimate"]
      pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
      se = reg_summary[, 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      
      
      
      
      i=i+1
      
    }
    
    
    
  }
  
  
  ##
  ## We build the table and then export it to .csv:
  ##
  
  
  lines = c()
  for (i in 1:length(nb_obs)){
    
    temp = paste0(format(round(coefficients[[i]][["treated_urban_rep"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_urban_rep"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_urban_rep"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_urban_rep"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated_gop"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_gop"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_gop"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_gop"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated_urban"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_urban"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_urban"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_urban"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated"]], digits=3), nsmall = 3))
    
    
    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }
  tab = t(matrix(lines, ncol= 3)) # matrix containing the table to export
  
  
  
  cols = c("Treated x Majority Urban x Majority Rep","p-value","Treated x Majority Rep","p-value","Treated x Majority Urban","p-value","Treated","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Asinh(Fortnightly Cases)",3))
  
  write.csv(tab, paste0("../Output/Covid_Heterogeneity/",treatment,"_heterogeneity_urban_rep_zip_cases.csv"))
}

# Pooled regressions

heterogeneity_variables = c("majority_urban","majority_gop","high_education","high_log_cumulative_cases")

for (het_var in heterogeneity_variables){
  var="asinh_two_weeks_cases"
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  

  i = 1
  
  data_X=data_het %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data_het %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  
  dates_T1 = c("2020-12-14","2020-12-28","2021-01-11")
  dates_X = c("2021-01-14","2021-01-28","2021-02-11")
  
  
  for (d in c(1:1)){
    
    
    
    
    formula_str = paste0(var," ~ treated_het_var + treated + het_var+ baseline_log_cases + high_county + factor(user_loc)")
    data_for_regression$period_1 = 0
    data_for_regression$treated =  0
    data_for_regression$high_county =  0
    data_for_regression$baseline_log_cases=0
    
    data_for_regression$period_1[as.logical(indices_T1*(data_for_regression$date == dates_T1[d]))]=1
    
    
    data_for_regression$treated[indices_T1] =  data_T1$treated_T1
    data_for_regression$high_county[indices_T1] =  data_T1$high_county_T1
    data_for_regression$baseline_log_cases[indices_T1] =data_T1$baseline_th_log_cases
    data_for_regression$high_log_cumulative_cases[indices_T1]=data_T1$high_log_cumulative_cases_th
    
    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date == dates_X[d]))]=1
    
    data_for_regression$treated[indices_X] =  data_X$treated_X
    data_for_regression$high_county[indices_X] =  data_X$high_county_X
    data_for_regression$baseline_log_cases[indices_X] =data_X$baseline_ch_log_cases
    data_for_regression$high_log_cumulative_cases[indices_X]=data_T1$high_log_cumulative_cases_ch
    
    data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
    
    data_reg$het_var = data_reg[[het_var]]
    
    data_reg$treated_het_var = data_reg$het_var * data_reg$treated 
    
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    
    reg_summary <- summary(reg)
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    i=i+1
    for (j in 0:1){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      
      data_reg$het_var = data_reg[[het_var]]

      data_reg$treated_het_var = data_reg$het_var * data_reg$treated 

      data_reg = data_reg %>%  filter(high_county==j)
      reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary[,"Estimate"]
      pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
      se = reg_summary[, 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      

      i=i+1
    }
    
  
    
  }
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(nb_obs)){ 
    

    temp = paste0(format(round(coefficients[[i]][["treated_het_var"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_het_var"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_het_var"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_het_var"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["het_var"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["het_var"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["het_var"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["het_var"]], digits=3), nsmall = 3))


    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }

  tab = t(matrix(lines, ncol=3)) # matrix containing the table to export
  
  
  cols = c("Treated x group1","p-value","Treated","p-value","group1","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Asinh(Fortnightly Cases)",3))
  
  write.csv(tab, paste0("../Output/Covid_Heterogeneity/pooled_heterogeneity_",het_var,"_zip_cases.csv"))
}

## Urban * Republican pooled regression

  var="asinh_two_weeks_cases"
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  
  i = 1
  
  data_X=data_het %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data_het %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  
  dates_T1 = c("2020-12-14","2020-12-28","2021-01-11")
  dates_X = c("2021-01-14","2021-01-28","2021-02-11")
  
  for (d in c(1:1)){

    formula_str = paste0(var," ~ treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban + baseline_log_cases +high_county+ factor(user_loc)")
    
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
    
    data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
    data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
    data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
    
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    
    reg_summary <- summary(reg)
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    i=i+1
    for (j in 0:1){
      data_reg = data_for_regression %>% filter(period_1==1) %>% filter(!is.na(high_county))
      
      data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
      data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
      data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
      
      data_reg = data_reg %>% filter(!is.na(high_county)) %>% filter(high_county==j)
      reg = lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
      
      reg_summary <- summary(reg)
      
      coefficients[[i]] = reg_summary[,"Estimate"]
      pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
      se = reg_summary[, 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      nb_obs[[i]] = nobs(lm(reg$lm_res))
      
      
      
      i=i+1
    }
  
    
  }
  ##
  ## We build the table and then export it to .csv:
  ##
  
  
  lines = c()
  for (i in 1:length(nb_obs)){
    
    temp = paste0(format(round(coefficients[[i]][["treated_urban_rep"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_urban_rep"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_urban_rep"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_urban_rep"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated_gop"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_gop"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_gop"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_gop"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated_urban"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated_urban"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated_urban"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated_urban"]], digits=3), nsmall = 3))
    
    temp = paste0(format(round(coefficients[[i]][["treated"]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][["treated"]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][["treated"]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][["treated"]], digits=3), nsmall = 3))
    
    
    lines = c(lines,nb_obs[[i]]) # Number of observations
    
  }
  tab = t(matrix(lines, ncol=3)) # matrix containing the table to export
  
  
  
  cols = c("Treated x Majority Urban x Majority Rep","p-value","Treated x Majority Rep","p-value","Treated x Majority Urban","p-value","Treated","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Asinh(Fortnightly Cases)",3))
  
  write.csv(tab, paste0("../Output/Covid_Heterogeneity/pooled_heterogeneity_urban_rep_zip_cases.csv"))
