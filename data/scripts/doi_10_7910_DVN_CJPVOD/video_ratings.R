# Regressions for the three video ratings outcomes (appendix)

# Lines 65-73: choice of main specifications for the regressions

# Please set working directory to be source file location

rm(list = ls())

library('dplyr')
library('fastDummies')
library('readr')
library('anytime')
library('stats')
library('hdm')
library('lubridate') 
library("MASS")
library("ebal")
library('car')

source('shortlist_functions.R')

simulation = 0
set.seed(302)


mainlaunch <- "./Data/COVID USA T1_May 27, 2020_07.20.csv"


df=read_csv(mainlaunch)[-c(1,2),]

df$StartTime <- anytime(df$StartDate) #parse
df$duration <- as.numeric(df$`Duration (in seconds)`)

#raw dataset of accepts 

raw <- df %>% filter((StartTime > anytime("2020-05-13 15:00:00 EDT")) & (DistributionChannel == "anonymous") & (Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & is.na(test))

raw <- as.data.frame(raw)


source('basic_cleaning.R')

source('hainmueller_weights.R')



dem_options <- c("All", "African-American", "Latinx")


binary_outcomes= c("dont_know_3_practices","dont_know_asymptomatic", "dont_know_symptoms","want_mask_vid")

discrete_outcomes = c("intended_behavior","knowledge_count_variable")

knowledge_outcomes = c("knowledge_count_variable","posterior_protect","posterior_no_good", "posterior_was_sick")

links_outcomes = c("intended_behavior","want_mask_vid","masks_for_the_people")

videos_outcomes = c("vid1_net_rating", "vid2_net_rating", "vid3_net_rating","vid1_time","vid2_time","vid3_time")




raw_anytreat <- raw %>% filter(anytreat == 1)
## Regression with every treatment as controls 


### PARAMETERS 

DPL = FALSE # whether we want to add DPL controls in regressions or not
weights = TRUE# whether we want to weight the regressions with hainmueller weights or not


### END of PARAMETERS

Completed_key_baseline = TRUE # TRUE if we want to compute the means on the same observations that are used for the weighted regressions


# Selection of the relevant data set for the regressions
data_model = raw_anytreat


treatment_desc <- ""
treatment_labels = c()

outcomes = c("vid1_net_rating", "vid2_net_rating", "vid3_net_rating","vid1_time","vid2_time","vid3_time")




rel_treatments = c("birx","elephant1","elephant2")



primary_rhs = paste0(rel_treatments, collapse = "+")
outcome_grp_title <- "Title"
coefficients = list()
standarderrors = list()
pvalues = list()
models <- list()
nb_obs = list()
CI_lower_bound=list()
CI_upper_bound=list()
i = 1
for (dem_select in dem_options) {
  
  
  #filter to the right thing 
  data_reg <- get_filtered_data(data_model, dem_select)
  #double lasso for t
  support_t_net <- get_lasso_support(rel_treatments, baseline_vars_dummies, data_reg)
  
  
  
  
  models_missing <- list()
  
  
  for (outcome in outcomes) {
    
    if (outcome %in% binary_outcomes){
      model <- run_reg_binary_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=weights,DPL=DPL)
      coef_model = coef(summary(model))
      coef_model = coef_model[2:nrow(coef_model),] # we don't need the intercept
      se = coef_model[,"Std. Error"]
      coef = coef_model[,"Estimate"]
      
      
      CI_lower_bound[[i]]=exp(coef - 1.96*se) # CI for the Odds ratio
      CI_upper_bound[[i]]=exp(coef + 1.96*se)# CI for the Odds ratio
      coefficients[[i]]=exp(coef) # Odds ratio
      pvalues[[i]]=coef_model[,"Pr(>|z|)"] # p values for test coef == 0
    }else{
      if (outcome %in% discrete_outcomes){
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL)
        model_irr=To_irr(model)
        model_irr=model_irr$irr
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0
      }else{
        model <- run_reg_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL)
        sum = summary(model)
        coef_model = sum$coefficients
        coef_model = coef_model[2:nrow(coef_model),] # we don't need the intercept
        se = coef_model[,"Std. Error"]
        coef = coef_model[,"Estimate"]
        
        
        CI_lower_bound[[i]]=coef - 1.96*se # CI for the coef
        CI_upper_bound[[i]]=coef + 1.96*se# CI for the coef
        coefficients[[i]]=coef 
        pvalues[[i]]=coef_model[,"Pr(>|t|)"] # p values for test coef == 0
      }}
    
    
    models[[i]] = model
    nb_obs[[i]] = nobs(model)
    
    
    
    
    i = i + 1
  }
  
  

}


#### Code for tables 




lines = c()
for (i in 1:length(models)){
  for (j in 1:3){
    temp = paste0(round(coefficients[[i]][j],3)," (",round(CI_lower_bound[[i]][j],3),",",round(CI_upper_bound[[i]][j],3),")")
    lines = c(lines,temp)
    lines = c(lines,round(pvalues[[i]][j],3))
  }
  lines = c(lines,nb_obs[[i]])
  if (i%%length(outcomes)==0){
    lines = c(lines,rep("",7))
  }
}
tab = t(matrix(lines, ncol=21))
outcomes_labels = get_labels(outcomes)
colnames(tab)= c("Birx",	"p-value",	"Acknowledgement 1",	"p-value",	"Acknowledgement 2",	"p-value",	"Observations")
rownames(tab) = c(outcomes_labels, "",outcomes_labels, "",outcomes_labels, "")

write.csv(tab, "./Appendix_output/video_ratings_regressions.csv")


lines = c()
i=1
for (treatment in c("birx","elephant1","elephant2")){
  for (j in 0:1){
    for (dem_select in dem_options) {
      df <- get_filtered_data(raw_anytreat, dem_select)
      df = df[df[[treatment]]==j,]
      if (Completed_key_baseline==TRUE){
      df= df[!is.na(df$weights_videos),]}
      for (var in outcomes){
        temp = df[!is.na(df[[var]]),]
        
        formula = as.formula(paste(var,"  ~  ","1"))
        reg =  lm(formula = formula, data = temp)
        coef_model = summary(reg)$coefficients
        se = coef_model[1,"Std. Error"] 
        coef = coef_model[1,"Estimate"] 
        
        lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")"),nrow(temp)) 
        
      }
      lines=c(lines,"")
    }
  }

  
}

outcomes_labels = get_labels(outcomes)
tab = matrix(lines, ncol=6)
colnames(tab) = c("Birx = 0","Birx = 1", "Elephant1 = 0", "Elephant1 = 1", "Elephant2 = 0", "Elephant2 = 1")
rownames= c()
for (i in 1:length(outcomes)){
  rownames = c(rownames,outcomes_labels[i],"Observations")
  
}
rownames(tab)= rep(c(rownames,""),3)
write.csv(tab, paste0("./Appendix_output/video_ratings_means_completed_",Completed_key_baseline,".csv"))

