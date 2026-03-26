#This script runs the mobility quantile regressions for Table S5a

set.seed(89173)

#Quantile regressions with bootstrapped standard errors
#The 95% CI and p-values might slightly differ from the ones in the paper (because of the random bootstrap method)

quantiles = c(0.10,0.25,0.5,0.75,0.9)

for (tau in quantiles){
  for (treatment in c("T1","X")){ # for each treatment (T1 = thanksgiving treatment, X = christmas treatment)
    
    coefficients = list()
    standarderrors = list()
    CI_lower_bound = list()
    CI_upper_bound = list()
    pvalues = list()
      
    i=1
    
    
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
      
      reg = rq(as.formula(formula_str), tau =tau, data = data_reg,na.action=na.omit)

      reg_summary = coef(summary.rq(reg,se="boot"))
      
      coefficients[[i]] = reg_summary["treated","Value"]
      pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
      se = reg_summary["treated", 'Std. Error']
      CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
      CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef
      i=i+1
      
      
    }
    
  
    lines = c()
    for (i in 1:length(coefficients)){ 
 
      temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
      lines = c(lines,temp)
      lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))
      
    }
    tab = t(matrix(lines, ncol= 2)) # matrix containing the table to export
 
    colnames(tab)=c("High Intensity (coef)","p-value")
    rownames(tab)=c("Distance Traveled","Share Ever Left Home")
    
    write.csv(tab, paste0("../Output/Mobility/mobility_quantile_reg_tau_",tau,"_",treatment,".csv"))
    
  }
    
}

## Pooled regression for Distance Traveled

for (tau in quantiles){

coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()

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


formula_str = paste0(var," ~  day_1 + day_2  + treated +  (day_1 + day_2 + day_3):baseline_th_",var)

data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))

baseline_var=paste0("baseline_th_",var)

reg = rq(as.formula(formula_str), tau =tau, data = data_reg,na.action=na.omit)

reg_summary = coef(summary.rq(reg,se="boot"))


coefficients[[i]] = reg_summary["treated","Value"]
pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
se = reg_summary["treated", 'Std. Error']
CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef


i=i+1



##
## We build the table and then export it to .csv:
##

lines = c()
for (i in 1:length(coefficients)){ 
  
  temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
  lines = c(lines,temp)
  lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))

  
}
tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export


colnames(tab)=c("High Intensity (coef)","p-value")
rownames(tab)=c(rep("Distance Traveled",1))

write.csv(tab, paste0("../Output/Mobility/Pooled_quantile_reg_tau_",tau,"_",var,".csv"))

}


## Pooled regression for Share Ever Left Home
for (tau in quantiles){
  coefficients = list()
  standarderrors = list()
  CI_lower_bound = list()
  CI_upper_bound = list()
  pvalues = list()

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
  
  
  formula_str = paste0(var," ~  day_1  + treated +  (day_1 + day_2):baseline_th_",var)

  data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
  
  
  baseline_var=paste0("baseline_th_",var)
  
  
  reg = rq(as.formula(formula_str), tau =tau, data = data_reg,na.action=na.omit)
  
  reg_summary = coef(summary.rq(reg,se="boot"))
  
  
  coefficients[[i]] = reg_summary["treated","Value"]
  pvalues[[i]] <- reg_summary["treated", 'Pr(>|t|)']
  se = reg_summary["treated", 'Std. Error']
  CI_lower_bound[[i]]=coefficients[[i]] - 1.96*se # CI for the coef
  CI_upper_bound[[i]]=coefficients[[i]] + 1.96*se# CI for the coef

  
  i=i+1
  
  
  ##
  ## We build the table and then export it to .csv:
  ##
  
  lines = c()
  for (i in 1:length(coefficients)){ 
    
    temp = paste0(format(round(coefficients[[i]], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]], digits=3), nsmall = 3))

    
  }
  tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export

  colnames(tab)=c("High Intensity (coef)","p-value")
  rownames(tab)=c(rep("Share Ever Left Home",1))
  
  write.csv(tab, paste0("../Output/Mobility/Pooled_quantile_reg_tau_",tau,"_",var,".csv"))
}