#This script computes the Randomization Inference p-values for Table 2

# Number of randomized treatments in the randomization inference
nb_randomization=1000

for (treatment in c("T1","X")){# for each treatment (T1 = thanksgiving treatment, X = christmas treatment)
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

randomized_treatments = read.csv("../Data/randomized_zip.csv") #import the randomized treatments generated with zip_randomization.R
to_remove=switch(treatment,"T1"="X","X"="T1")
randomized_treatments = randomized_treatments[,colnames(randomized_treatments)[!grepl(to_remove,colnames(randomized_treatments),fixed=TRUE)]]
randomized_treatments = randomized_treatments[,colnames(randomized_treatments)[!grepl("zip",colnames(randomized_treatments),fixed=TRUE)]]
randomized_treatments$X=NULL
randomized_treatments = na.omit(distinct(randomized_treatments))
for (var in c("movement_ch","leave_home")){
  
  formula_str = switch(var,"movement_ch"=paste0(var," ~ factor(date) + treated  + factor(date):baseline_th_",var),
                       "leave_home"=paste0(var," ~ treated  + baseline_th_",var))
  
  if((var!="leave_home")||(treatment=="X")){
    formula_str=paste0(var," ~ factor(date) + treated  + factor(date):baseline_th_",var)
  }else{
    formula_str=paste0(var," ~ treated  + baseline_th_",var)
  }
  
  

  spec = paste0(treatment,"_",var)
  
  start_date = switch(spec,"T1_movement_ch"="2020-11-23","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-21","X_leave_home"="2020-12-24")
  end_date = switch(spec,"T1_movement_ch"="2020-11-25","T1_leave_home"="2020-11-26","X_movement_ch"="2020-12-23","X_leave_home"="2020-12-25")
  if (treatment=="X"){
    data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)%>%  filter(!is.na(high_county_X))
  }else{
    data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)%>%  filter(!is.na(high_county_T1))}
  
  data_reg$treated = data_reg[[paste0("high_county_",treatment)]]
  
 reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
 reg_summary <- summary(reg)
  
  coefficients[[k]] = reg_summary["treated","Estimate"]
  pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
  se = reg_summary["treated", 'Std. Error']
  CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
  CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
  nb_obs[[k]] = nobs(lm(reg$lm_res))
  
  coef=c()
  pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)
  
  for (i in c(1:nb_randomization)){# for each randomized treatment
    if (treatment=="X"){
      data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)%>%  filter(!is.na(high_county_X))
    }else{
      data_reg = data %>% filter(date>=start_date) %>% filter(date<=end_date)%>%  filter(!is.na(high_county_T1))}
    
    data_reg = merge(data_reg,randomized_treatments[,c("user_loc",paste0("high_county_",treatment,"_",i))],by="user_loc")
    data_reg$treated = data_reg[[paste0("high_county_",treatment,"_",i)]]
    reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
    
    reg_summary <- summary(reg)
    
    coef = c(coef,reg_summary["treated","Estimate"])
 
    setTxtProgressBar(pb, i)
  }
  close(pb)
  p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization) # RI p-value
  
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
tab = t(matrix(lines, ncol= 2)) # matrix containing the table to export




cols = c("High Intensity (coef)","p-value","RI p-value")



colnames(tab)=c(cols,"Observations")
rownames(tab)=c("Distance Traveled","Share Ever Left Home")

write.csv(tab, paste0("../Output/Mobility/RI_mobility_",treatment,".csv"))
}
## Pooled regression

# Movement

coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
p_treated_RI=c()
nb_obs = list()  
k = 1

data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
data_for_regression = bind_rows(data_X,data_T1)
indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))

data_for_regression$campaign=0 # 0: thanksgiving, 1: christmas
data_for_regression$campaign[indices_X]=1
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



data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))


reg = lm.cluster(formula=as.formula(formula_str),data=data_reg,cluster="user_loc")

reg_summary <- summary(reg)

coefficients[[k]] = reg_summary["treated","Estimate"]
pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
se = reg_summary["treated", 'Std. Error']
CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
nb_obs[[k]] = nobs(lm(reg$lm_res))

randomized_treatments = read.csv("../Data/randomized_zip.csv")
randomized_treatments = randomized_treatments[,colnames(randomized_treatments)[!grepl("zip",colnames(randomized_treatments),fixed=TRUE)]]
randomized_treatments = na.omit(distinct(randomized_treatments))

coef=c()
pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)

for (i in c(1:nb_randomization)){
  data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
  data_reg = merge(data_reg,randomized_treatments[c("user_loc",paste0("high_county_X_",i),paste0("high_county_T1_",i))],by="user_loc")
  data_reg$treated = 0
  data_reg$treated[data_reg$campaign==0]=data_reg[[paste0("high_county_T1_",i)]][data_reg$campaign==0]
  data_reg$treated[data_reg$campaign==1]=data_reg[[paste0("high_county_T1_",i)]][data_reg$campaign==1]
  
  reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
  
  reg_summary <- summary(reg)
  
  coef = c(coef,reg_summary["treated","Estimate"])
  
  setTxtProgressBar(pb, i)
}
close(pb)
p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)


k=k+1







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
tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export

cols = c("High Intensity (coef)","p-value","RI p-value")


colnames(tab)=c(cols,"Observations")
rownames(tab)=c("Distance Traveled")

write.csv(tab, paste0("../Output/Mobility/Pooled_RI_Distance_Traveled.csv"))

# Leave home
var="leave_home"


coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
p_treated_RI=c()
models <- list()
cmeans <- c()
nb_obs = list()  
k = 1

data_X=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
data_T1=data %>% filter(!(is.na(high_county_X)*is.na(high_county_T1)))
data_for_regression = bind_rows(data_X,data_T1)
indices_X = c(rep(TRUE,nrow(data_X)),rep(FALSE,nrow(data_X)))
indices_T1 = c(rep(FALSE,nrow(data_X)),rep(TRUE,nrow(data_X)))

data_for_regression$campaign=0 # 0: thanksgiving, 1: christmas
data_for_regression$campaign[indices_X]=1

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


reg = lm.cluster(formula=as.formula(formula_str),data=data_reg,cluster="user_loc")

reg_summary <- summary(reg)

coefficients[[k]] = reg_summary["treated","Estimate"]
pvalues[[k]] <- reg_summary["treated", 'Pr(>|t|)']
se = reg_summary["treated", 'Std. Error']
CI_lower_bound[[k]]=coefficients[[k]] - 1.96*se # CI for the coef
CI_upper_bound[[k]]=coefficients[[k]] + 1.96*se# CI for the coef
nb_obs[[k]] = nobs(lm(reg$lm_res))

randomized_treatments = read.csv("../Data/randomized_zip.csv")
randomized_treatments = randomized_treatments[,colnames(randomized_treatments)[!grepl("zip",colnames(randomized_treatments),fixed=TRUE)]]
randomized_treatments = na.omit(distinct(randomized_treatments))

coef=c()
pb <- txtProgressBar(min = 0, max = nb_randomization, style = 3)

for (i in c(1:nb_randomization)){
  data_reg =  data_for_regression %>% filter(period_1==1) %>% filter(!is.na(treated))
  data_reg = merge(data_reg,randomized_treatments[c("user_loc",paste0("high_county_X_",i),paste0("high_county_T1_",i))],by="user_loc")
  data_reg$treated = 0
  data_reg$treated[data_reg$campaign==0]=data_reg[[paste0("high_county_T1_",i)]][data_reg$campaign==0]
  data_reg$treated[data_reg$campaign==1]=data_reg[[paste0("high_county_X_",i)]][data_reg$campaign==1]
  
  reg = lm.cluster(formula = as.formula(formula_str), data = data_reg, cluster = "user_loc")
  
  reg_summary <- summary(reg)
  
  coef = c(coef,reg_summary["treated","Estimate"])
  
  setTxtProgressBar(pb, i)
}
close(pb)
p_treated_RI=c(p_treated_RI,sum(abs(coef-mean(coef))>abs(coefficients[[k]]-mean(coef)))/nb_randomization)


k=k+1







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
tab = t(matrix(lines, ncol= 1)) # matrix containing the table to export

cols = c("High Intensity (coef)","p-value","RI p-value")


colnames(tab)=c(cols,"Observations")
rownames(tab)=c("Share Ever Left Home")

write.csv(tab, paste0("../Output/Mobility/Pooled_RI_Leave_Home.csv"))

