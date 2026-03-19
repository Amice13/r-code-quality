# This script runs the regressions with "all black treatments" (Supplement Table 12)


set.seed(302)

raw = resp

library('dplyr')
library('fastDummies')
library('readr')
library('anytime')
library('stats')
library('hdm')
library('lubridate') 
library("MASS")
library("ebal")

# Import of useful functions
source('helpers.R')


source("entropy_weighting.R")



raw$resp_lang = raw$user_language
raw$resp_race = raw$race_strat
raw$resp_black = 1-raw$race_white
raw$resp_white = raw$race_white
#rename variables
raw$intended_behavior = raw$links_total
raw$knowledge_count_variable = raw$know_gap_count
raw$safety_count_variable = 4-raw$safety_total_always_followup
raw$anytreat = raw$covid_any


# Outcomes type (it enables to select the right regression model/weights)

discrete_outcomes = c("intended_behavior","know_gap_count","safety_count_variable","know_gap_count_followup")
binary_outcomes = c("fed_balance_agree", "state_balance_agree")
follow_outcomes = c("safety_count_variable","know_gap_count_followup")

# Outcomes that we want to put in the regressions

OLS_outcomes = c("wtp_masks","charity_black","charity_covid")
logit_outcomes = c("fed_balance_agree", "state_balance_agree")
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")


#### PARAMETERS

# The three panels for the main regressions
dem_options <- c("All", "African-American", "White")
panel_type = ""

DPL = FALSE# TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE # TRUE if we want to include the hainmueller weights in the regression for follow up outcomes, else FALSE

# Build covariates for DPL regressions
source("dummy_variables.R")


### Computation of regressions for all types of outcomes
for (outcome_type in c("NB","OLS","logit")){
  
  outcomes = switch(outcome_type,"OLS"=OLS_outcomes,"NB"=NB_outcomes,"logit"=logit_outcomes)
  


raw$covid_any__any_black_treatment = raw$covid_any * raw$any_black_treatment
raw$covid_any__all_black_treatments = raw$covid_any * raw$all_black_treatments

###########################
### Regression for all black treatments
###########################

raw$racism_ama__black_doc = raw$racism_ama*raw$black_doc
rel_treatments <-c("covid_any__all_black_treatments","covid_any","racism_ama__black_doc")


primary_rhs = paste0(rel_treatments, collapse = "+")



outcome_grp_title <- "Title"
#black_equals_white =  list()
coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
for (dem_select in dem_options) {
  
  
  
  for (outcome in outcomes) {
    
    #filter to the right thing 
    data_reg <- get_filtered_data(raw, dem_select,outcome)
    
    #double lasso for t
    if (DPL==TRUE){
      support_t_net <- get_lasso_support(rel_treatments,lasso_controls, data_reg)
    }
    
    if (outcome %in% binary_outcomes){
      model <- run_reg_binary_outcome(outcome, data_reg, primary_rhs,balance_model=weights,DPL=DPL)
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
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs,balance_model=weights,DPL=DPL)
        model_irr=To_irr(model)
        model_irr=model_irr$irr
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0
        
      }else{
        model <- run_reg_outcome(outcome, data_reg, primary_rhs,balance_model=weights,DPL=DPL)
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



###########################
### We build the table to export:
###########################

lines = c()
for (i in 1:length(models)){
  for (j in 1:3){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }
  
  lines = c(lines,nb_obs[[i]])
  
}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)

colnames(tab) = colnames(tab)= c("covid_any__all_black_treatments","p-value",

                                 "covid_any","p-value",
                                 "racism_ama__black_doc","p-value",
                                 "Observations")
rownames(tab) =  rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,panel_type,"_regression_anytreat_allblack_weights_",weights,"_DPL_",DPL,".csv"))
}