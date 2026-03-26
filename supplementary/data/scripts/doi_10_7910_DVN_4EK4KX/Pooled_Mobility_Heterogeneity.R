#This script runs the mobility regressions with interactions (panel = both campaigns)

set.seed(787567)
DPL=FALSE

data$majority_gop = as.numeric(data$per_gop>data$per_dem)
data$majority_urban = as.numeric(data$share_urban>0.5)
data$urban_rep = data$majority_gop * data$majority_urban
heterogeneity_variables = c("majority_gop","majority_urban","high_education","high_infection_rate")


for (het_var in heterogeneity_variables){

  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  
  control_1_means = list()  
  control_1_sd = list()  
  control_2_means = list()  
  control_2_sd = list()  
  treatment_1_means = list()  
  treatment_1_sd = list()  
  treatment_2_means = list()  
  treatment_2_sd = list()  
  i = 1
  
  data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  
  var = "movement_ch"
  data_for_regression$high_infection_rate[indices_T1] = data_T1$high_infection_rate_th
  data_for_regression$high_infection_rate[indices_X] = data_X$high_infection_rate_ch

    
    
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
  
      data_for_regression$treated_het_var = data_for_regression$treated * data_for_regression[[het_var]]
      data_for_regression$het_var=data_for_regression[[het_var]]
 

    
    
    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date >= "2020-12-21")*(data_for_regression$date <= "2020-12-23"))]=1
    
    
    data_for_regression$day_1[as.logical(indices_X*(data_for_regression$date == "2020-12-21"))]=1
    
    
    data_for_regression$day_2[as.logical(indices_X*(data_for_regression$date == "2020-12-22"))]=1
    
    
    data_for_regression$day_3[as.logical(indices_X*(data_for_regression$date == "2020-12-23"))]=1
    
    
    
    
    data_for_regression$treated[indices_X] =  data_X$high_county_X

      data_for_regression$treated_het_var = data_for_regression$treated * data_for_regression[[het_var]]
      data_for_regression$het_var=data_for_regression[[het_var]]
      

    
    
    formula_str = paste0(var," ~ day_1 + day_2 + day_3 + treated_het_var + treated + het_var+  (day_1 + day_2 + day_3):baseline_th_",var)
    treatments_for_reg = c("treated","treated_het_var","het_var")
    
    
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
    

    baseline_var=paste0("baseline_th_",var)

    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
      
 
    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    
    formula = as.formula(paste(var,"  ~  ","1"))
    
    data_control= data_reg %>% filter(treated==0) %>% filter(het_var==1)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_1_means[[i]] = summary(reg)[,"Estimate"]
    control_1_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)%>% filter(het_var==1)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_1_means[[i]] =summary(reg)[,"Estimate"]
    treatment_1_sd[[i]] =summary(reg)[,"Std. Error"]
    
    
    data_control = data_reg %>% filter(treated==0)%>% filter(het_var==0)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_2_means[[i]] = summary(reg)[,"Estimate"]
    control_2_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)%>% filter(het_var==0)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_2_means[[i]] =summary(reg)[,"Estimate"]
    treatment_2_sd[[i]] =summary(reg)[,"Std. Error"]
    
    i=i+1
    
    
  
  
  
  
  ##
  ## We build the table and then export it to .csv:
  ##

  lines = c()
  for (i in 1:length(nb_obs)){ 
    
    lines = c(lines,paste0(format(round(treatment_1_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_1_means[[i]] - 1.96*treatment_1_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_1_means[[i]] + 1.96*treatment_1_sd[[i]], digits=3), nsmall = 3),")")) 
    lines = c(lines,paste0(format(round(treatment_2_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_2_means[[i]] - 1.96*treatment_2_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_2_means[[i]] + 1.96*treatment_2_sd[[i]], digits=3), nsmall = 3),")")) 
    
    lines = c(lines,paste0(format(round(control_1_means[[i]], digits=3), nsmall = 3)," (",format(round(control_1_means[[i]] - 1.96*control_1_sd[[i]], digits=3), nsmall = 3),",",format(round(control_1_means[[i]] + 1.96*control_1_sd[[i]], digits=3), nsmall = 3),")")) 
    lines = c(lines,paste0(format(round(control_2_means[[i]], digits=3), nsmall = 3)," (",format(round(control_2_means[[i]] - 1.96*control_2_sd[[i]], digits=3), nsmall = 3),",",format(round(control_2_means[[i]] + 1.96*control_2_sd[[i]], digits=3), nsmall = 3),")")) 
    
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
  tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
  
  cols = c("High x group1","High x group2","Low x group1","Low x group2","High Intensity x group1","p-value","High Intensity","p-value", "group1","p-value")
  
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Distance Traveled",1))
  
  write.csv(tab, paste0("../Output/Mobility_Heterogeneity/Pooled_heterogeneity_",het_var,"_distance.csv"))
  


#for leave home

  
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()
  models <- list()
  cmeans <- c()
  nb_obs = list()  
  
  control_1_means = list()  
  control_1_sd = list()  
  control_2_means = list()  
  control_2_sd = list()  
  treatment_1_means = list()  
  treatment_1_sd = list()  
  treatment_2_means = list()  
  treatment_2_sd = list()  
  i = 1
  
  data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
  data_for_regression = bind_rows(data_X,data_T1)
  indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
  indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
  data_for_regression$high_infection_rate[indices_T1] = data_T1$high_infection_rate_th
  data_for_regression$high_infection_rate[indices_X] = data_X$high_infection_rate_ch
  var = "leave_home"
    
    
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

      data_for_regression$treated_het_var = data_for_regression$treated * data_for_regression[[het_var]]
      data_for_regression$het_var=data_for_regression[[het_var]]
      
    
    
    
    data_for_regression$period_1[as.logical(indices_X*(data_for_regression$date >= "2020-12-24")*(data_for_regression$date <= "2020-12-25"))]=1
    
    
    data_for_regression$day_1[as.logical(indices_X*(data_for_regression$date == "2020-12-24"))]=1
    
    
    data_for_regression$day_2[as.logical(indices_X*(data_for_regression$date == "2020-12-25"))]=1

    
    data_for_regression$treated[indices_X] =  data_X$high_county_X

      data_for_regression$treated_het_var = data_for_regression$treated * data_for_regression[[het_var]]
      data_for_regression$het_var=data_for_regression[[het_var]]
      
    
    
    
    formula_str = paste0(var," ~ day_1 + day_2  + treated_het_var + treated + het_var +  (day_1 + day_2):baseline_th_",var)
    treatments_for_reg = c("treated","treated_het_var","het_var")
    
    
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
    

    baseline_var=paste0("baseline_th_",var)

    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
    

    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    formula = as.formula(paste(var,"  ~  ","1"))
    
    data_control = data_reg %>% filter(treated==0) %>% filter(het_var==1)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_1_means[[i]] = summary(reg)[,"Estimate"]
    control_1_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)%>% filter(het_var==1)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_1_means[[i]] =summary(reg)[,"Estimate"]
    treatment_1_sd[[i]] =summary(reg)[,"Std. Error"]
    
    
    data_control = data_reg %>% filter(treated==0)%>% filter(het_var==0)
    reg =  lm.cluster(formula = formula, data = data_control,cluster="user_loc")
    
    control_2_means[[i]] = summary(reg)[,"Estimate"]
    control_2_sd[[i]] = summary(reg)[,"Std. Error"]
    
    data_treated = data_reg %>% filter(treated==1)%>% filter(het_var==0)
    reg =  lm.cluster(formula = formula, data = data_treated,cluster="user_loc")
    
    treatment_2_means[[i]] =summary(reg)[,"Estimate"]
    treatment_2_sd[[i]] =summary(reg)[,"Std. Error"]
    
    i=i+1
    
    
    
    

  
  
  
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(nb_obs)){ 
    
    lines = c(lines,paste0(format(round(treatment_1_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_1_means[[i]] - 1.96*treatment_1_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_1_means[[i]] + 1.96*treatment_1_sd[[i]], digits=3), nsmall = 3),")")) 
    lines = c(lines,paste0(format(round(treatment_2_means[[i]], digits=3), nsmall = 3)," (",format(round(treatment_2_means[[i]] - 1.96*treatment_2_sd[[i]], digits=3), nsmall = 3),",",format(round(treatment_2_means[[i]] + 1.96*treatment_2_sd[[i]], digits=3), nsmall = 3),")")) 
    
    lines = c(lines,paste0(format(round(control_1_means[[i]], digits=3), nsmall = 3)," (",format(round(control_1_means[[i]] - 1.96*control_1_sd[[i]], digits=3), nsmall = 3),",",format(round(control_1_means[[i]] + 1.96*control_1_sd[[i]], digits=3), nsmall = 3),")")) 
    lines = c(lines,paste0(format(round(control_2_means[[i]], digits=3), nsmall = 3)," (",format(round(control_2_means[[i]] - 1.96*control_2_sd[[i]], digits=3), nsmall = 3),",",format(round(control_2_means[[i]] + 1.96*control_2_sd[[i]], digits=3), nsmall = 3),")")) 
    
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
  tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
  
  cols = c("High x group1","High x group2","Low x group1","Low x group2","High Intensity x group1","p-value","High Intensity","p-value", "group1","p-value")
  
  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c(rep("Share Ever Left Home",1))
  
  write.csv(tab, paste0("../Output/Mobility_Heterogeneity/Pooled_heterogeneity_",het_var,"_leave.csv"))
  
}


### Additional regressions for urban * rep

    coefficients = list()
    standarderrors = list()
    CI_lower_bound = list()
    CI_upper_bound = list()
    pvalues = list()
    models <- list()
    cmeans <- c()
    nb_obs = list()  
    

    i = 1
    
    data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
    data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
    data_for_regression = bind_rows(data_X,data_T1)
    indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
    indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))

    var = "movement_ch"
    

    
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

    
    
    treatments_for_reg = c("treated","treated_het_var","het_var")
    
         formula_str = paste0(var,"~day_1 + day_2 + day_3  +treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban + (day_1 + day_2 + day_3):baseline_th_",var)
                              
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
        data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
        data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
        data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
    
    baseline_var=paste0("baseline_th_",var)
    
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
    
    
    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    
    
    i=i+1
    
    
    
    
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
    tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
    
    
    
    cols = c("High x Majority Urban x Majority Rep","p-value","High x Majority Rep","p-value","High x Majority Urban","p-value","High","p-value")
    
    
    
    colnames(tab)=c(cols,"Observations")
    rownames(tab)=c(rep("Distance Traveled",1))
    
    write.csv(tab, paste0("../Output/Mobility_Heterogeneity/Pooled_heterogeneity_urban_rep_distance.csv"))
    
  
  
  #for leave home

    
    coefficients = list()
    standarderrors = list()
    CI_lower_bound = list()
    CI_upper_bound = list()
    pvalues = list()
    models <- list()
    cmeans <- c()
    nb_obs = list()  
    
    i = 1
    
    data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
    data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
    data_for_regression = bind_rows(data_X,data_T1)
    indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
    indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))
    
    var = "leave_home"
    
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

    
    
    formula_str = paste0(var," ~ day_1 + day_2 +treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban +  (day_1 + day_2):baseline_th_",var)

    treatments_for_reg = c("treated","treated_het_var","het_var")
    
    
    data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
    data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
    data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
    data_reg$treated_urban = data_reg$majority_urban* data_reg$treated
    
    baseline_var=paste0("baseline_th_",var)
    
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
    
    
    reg_summary <- summary(reg)
    coef = reg_summary[,"Estimate"]
    
    
    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))
    
    
    
    i=i+1
    
    
    
    
    
    
    
    
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
    tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export
    
    
    
    cols = c("High x Majority Urban x Majority Rep","p-value","High x Majority Rep","p-value","High x Majority Urban","p-value","High","p-value")
    
    
    
    colnames(tab)=c(cols,"Observations")
    rownames(tab)=c(rep("Share Ever Left Home",1))
    
    write.csv(tab, paste0("../Output/Mobility_Heterogeneity/Pooled_heterogeneity_urban_rep_leave.csv"))
    




