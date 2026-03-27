#This script runs the mobility regressions with interactions (panel = Thanksgiving or Christmas)

set.seed(787567)


data$majority_gop = as.numeric(data$per_gop>data$per_dem)
data$majority_urban = as.numeric(data$share_urban>0.5)
data$urban_rep = data$majority_gop * data$majority_urban
heterogeneity_variables = c("high_education","majority_gop","majority_urban","high_infection_rate")


###########################
### Regressions
###########################
for (treatment in c("T1","X")){ # for each treatment (T1 = thanksgiving treatment, X = christmas treatment)

for (het_var in heterogeneity_variables){


coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()  

i = 1

if (het_var=="high_infection_rate"){
  het_var =  switch(treatment,"T1"="high_infection_rate_th","X"="high_infection_rate_ch")
}

for (var in c("movement_ch","leave_home")){
  

  if((var!="leave_home")||(treatment=="X")){
    formula_str=paste0(var,"~ factor(date)+treated_het_var + treated + het_var +  factor(date):baseline_th_",var)
  }else{
    formula_str=paste0(var,"~ treated_het_var + treated + het_var +  baseline_th_",var)
  }
  baseline_var=paste0("baseline_th_",var)
  treatments_for_reg = c("treated","treated_het_var","het_var")
  

  spec = paste0(treatment,"_",var)
  
  start_date = switch(spec,"T1_movement_ch"="2020-11-23","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-21","X_leave_home"="2020-12-24")
  end_date = switch(spec,"T1_movement_ch"="2020-11-25","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-23","X_leave_home"="2020-12-25")
  

  data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)

  
  data_reg$treated = data_reg[[paste0("high_county_",treatment)]]
  

  data_reg$treated_het_var = data_reg$treated * data_reg[[het_var]]
  data_reg$het_var=data_reg[[het_var]]

    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")

  reg_summary <- summary(reg)
  
  coefficients[[i]] = reg_summary[,"Estimate"]
  pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
  se = reg_summary[, 'Std. Error']
  CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
  CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
  nb_obs[[i]] = nobs(lm(reg$lm_res))
  
 
  i=i+1
  
  
  
  
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
tab = t(matrix(lines, ncol= 2)) # matrix containing the table to export



cols = c("High Intensity x group1","p-value","High Intensity","p-value", "group1","p-value")


colnames(tab)=c(cols,"Observations")
rownames(tab)=c("Distance Traveled","Share Ever Left Home")

write.csv(tab, paste0("../Output/Mobility_Heterogeneity/",treatment,"_mobility_heterogeneity_",het_var,".csv"))

}

# Additional regression for urban x rep


coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
  i = 1

  for (var in c("movement_ch","leave_home")){

      if((var!="leave_home")||(treatment=="X")){
formula_str=paste0(var,"~ factor(date)+treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban  +  factor(date):baseline_th_",var)
}else{
 formula_str=paste0(var,"~ treated_urban_rep + treated_gop + treated_urban + treated + urban_rep + majority_gop + majority_urban  +  baseline_th_",var)
}
baseline_var=paste0("baseline_th_",var)
    treatments = c("treated_urban_rep", "treated_gop", "treated","treated_urban")


    spec = paste0(treatment,"_",var)

    start_date = switch(spec,"T1_movement_ch"="2020-11-23","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-21","X_leave_home"="2020-12-24")
    end_date = switch(spec,"T1_movement_ch"="2020-11-25","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-23","X_leave_home"="2020-12-25")


      data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)

    data_reg$treated = data_reg[[paste0("high_county_",treatment)]]

    data_reg$treated_urban_rep = data_reg$urban_rep * data_reg$treated
    data_reg$treated_gop = data_reg$majority_gop* data_reg$treated
    data_reg$treated_urban = data_reg$majority_urban* data_reg$treated


    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")


    reg_summary <- summary(reg)

    coefficients[[i]] = reg_summary[,"Estimate"]
    pvalues[[i]] <- reg_summary[, 'Pr(>|t|)']
    se = reg_summary[, 'Std. Error']
    CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
    CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
    nb_obs[[i]] = nobs(lm(reg$lm_res))


    i=i+1



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
  tab = t(matrix(lines, ncol= 2)) # matrix containing the table to export



  cols = c("Treated x Majority Urban x Majority Rep","p-value","Treated x Majority Rep","p-value","Treated x Majority Urban","p-value","Treated","p-value")



  colnames(tab)=c(cols,"Observations")
  rownames(tab)=c("Distance Traveled","Share Ever Left Home")

  write.csv(tab, paste0("../Output/Mobility_Heterogeneity/",treatment,"_mobility_heterogeneity_urban_rep.csv"))

}
