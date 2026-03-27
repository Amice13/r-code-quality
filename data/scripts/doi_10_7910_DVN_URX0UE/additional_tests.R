# This file runs the main regressions with q-values (Tables 2 and 3) and some F-stats for Table 3


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
library("qvalue")
library("systemfit")
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
raw$race_white__party_dem = raw$race_white * raw$party_dem
raw$race_white__party_rep = raw$race_white * raw$party_rep
raw$not_hs_grad = 1 - raw$hs_graduate

# Outcomes type (it enables to select the right regression model/weights)

discrete_outcomes = c("intended_behavior","know_gap_count","safety_count_variable","know_gap_count_followup")
binary_outcomes = c("fed_balance_agree", "state_balance_agree")
follow_outcomes = c("safety_count_variable","know_gap_count_followup")

# Outcomes that we want to put in the regressions
OLS_outcomes = c("wtp_masks")
logit_outcomes = c()
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")

outcomes = c(NB_outcomes,logit_outcomes,OLS_outcomes)

#### PARAMETERS

# The three panels for the main regressions
dem_options <- c("All", "African-American", "White")

DPL = FALSE# TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE # TRUE if we want to include the hainmueller weights in the regression for follow up outcomes, else FALSE

# Build covariates for DPL regressions
source("dummy_variables.R")


### Computation of regressions for all types of outcomes


####### Regression with every treatment 

rel_treatments <-c("covid_any__black_doc","covid_any__racism_ama","covid_any__racism_doc","black_doc",
                     "racism_ama",
                     "covid_any")


#define primary rhs
primary_rhs = paste0(rel_treatments, collapse = "+")



raw_anytreat <- raw %>% filter(covid_any==1)

###########################
#### Regressions:
###########################


coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
qvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
j=0
for (dem_select in dem_options) { # This loop computes all the regressions

  pvalues_to_adjust = c()
  
  for (outcome in outcomes){ # We run the appropriate regression for each outcome
    
    #filter to the right data set
    data_reg <- get_filtered_data(raw, dem_select,outcome)

    
    #double lasso for treatment (selection of the covariates)
    if (DPL==TRUE){
      support_t_net <- get_lasso_support(rel_treatments,lasso_controls, data_reg)
    }
    
    if (outcome %in% binary_outcomes){ # Logit model
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
      if (outcome %in% discrete_outcomes){ # Negative Binomial model
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs,balance_model=weights,DPL=DPL)
        model_irr=To_irr(model)
        model_irr=model_irr$irr
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0
        
      }else{ # OLS model
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
    
    pvalues_to_adjust = c(pvalues_to_adjust,pvalues[[i]][1:6])
    
    
    i = i + 1
  }
  
  temp = qvalue(pvalues_to_adjust,pi0=1)$qvalues
  n=length(c(NB_outcomes,logit_outcomes,OLS_outcomes))
  
  for (k in 1:n){
    
    qvalues[[(j*n)+k]]=temp[6*(k-1)+c(1:6)]
  }
  j=j+1
}



###########################
## We build the table and then export it to .csv:
###########################
nb_coef = 6
lines = c()
for (i in 1:length(models)){ # Each model is a column of a panel in the table
  p_values_multiple_testing = c()
  for (j in 1:nb_coef){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(qvalues[[i]][j], digits=3), nsmall = 3))
  }

  
  lines = c(lines,nb_obs[[i]]) # Number of observations
}
tab = t(matrix(lines, ncol= length(models))) # matrix containing the table to export

outcomes_labels = get_labels(outcomes)


cols = c("covid_any__black_doc","q-value",
    "covid_any__racism_ama","q-value",
    "covid_any__racism_doc","q-value",
    "black_doc","q-value",
    "racism_ama","q-value",
    "covid_any","q-value")	

colnames(tab)=c(cols,"Observations")
rownames(tab)=rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/qvalues_regression_all_treatments_weights_",weights,"_DPL_",DPL,".csv"))


###########################
## Regressions for any covid treatment :
###########################

rel_treatments <-c("anytreat")

primary_rhs = paste0(rel_treatments, collapse = "+")


outcome_grp_title <- "Title"
coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
qvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
j=0
for (dem_select in dem_options) {
  

  pvalues_to_adjust=c()
  
  for (outcome in outcomes) {
    
    #filter to the right sample
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
    pvalues_to_adjust=c(pvalues_to_adjust,pvalues[[i]][1])
    
    
    
    i = i + 1
  }
  temp = qvalue(pvalues_to_adjust,pi0=1)$qvalues
  n=length(c(NB_outcomes,logit_outcomes,OLS_outcomes))
  
  for (k in 1:n){
    
    qvalues[[(j*n)+k]]=temp[k]
  }
  j=j+1
  

}

###########################
### We build the table to export:
###########################
lines = c()
for (i in 1:length(models)){
  for (j in 1:1){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(qvalues[[i]][j], digits=3), nsmall = 3))
  }

  lines = c(lines,nb_obs[[i]])
  
}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)

colnames(tab) = colnames(tab)= c("Coefficient","q-value", "Observations")
rownames(tab) =  rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/qvalues_regression_anytreat_weights_",weights,"_DPL_",DPL,".csv"))




###########################
### Additional tests: F-stat across several equations with systemfit
###########################


raw$covid_any__black_doc_b = raw$covid_any__black_doc*raw$resp_black
raw$covid_any__racism_ama_b =  raw$covid_any__racism_ama *raw$resp_black
raw$covid_any__racism_doc_b = raw$covid_any__racism_doc*raw$resp_black
raw$black_doc_b = raw$black_doc*raw$resp_black
raw$racism_ama_b = raw$racism_ama*raw$resp_black
raw$covid_any_b = raw$covid_any*raw$resp_black

raw$covid_any__black_doc_w = raw$covid_any__black_doc*raw$resp_white
raw$covid_any__racism_ama_w =  raw$covid_any__racism_ama *raw$resp_white
raw$covid_any__racism_doc_w = raw$covid_any__racism_doc*raw$resp_white
raw$black_doc_w = raw$black_doc*raw$resp_white
raw$racism_ama_w = raw$racism_ama*raw$resp_white
raw$covid_any_w = raw$covid_any*raw$resp_white


rel_treatments <-c("covid_any__black_doc_b","covid_any__racism_ama_b","covid_any__racism_doc_b","black_doc_b",
                   "racism_ama_b",
                   "covid_any_b",
                   "covid_any__black_doc_w","covid_any__racism_ama_w","covid_any__racism_doc_w","black_doc_w",
                   "racism_ama_w",
                   "covid_any_w")


primary_rhs = paste0(rel_treatments, collapse = "+")

equations = list()
data_reg <- raw
i=1
for (outcome in outcomes){ # We run the appropriate regression for each outcome
  
  equations[[i]]=as.formula(paste0(outcome," ~ ",primary_rhs,"+strat1 + strat2 + strat4 + strat5  + strat7 + strat8  + strat10 + strat11 + strat3  + strat6 + strat9 + strat12"))
  
  i = i + 1
}


for (outcome in outcomes){ # We run the appropriate regression for each outcome
  
  equations[[i]]=as.formula(paste0(outcome," ~ covid_any__black_doc+covid_any__racism_ama+covid_any__racism_doc+black_doc+racism_ama+covid_any+strat1 + strat2 + strat4 + strat5  + strat7 + strat8  + strat10 + strat11 + strat3  + strat6 + strat9 + strat12"))
  
  i = i + 1
}


fitsur <- systemfit(equations, method = "SUR" ,data=data_reg)


# Test: all variants have no effect (All/Black/White)

linearHypothesis(fitsur,c(
  "eq1_covid_any__black_doc_b=0", "eq1_covid_any__black_doc_w=0","eq1_covid_any__racism_ama_b=0","eq1_covid_any__racism_ama_w=0","eq1_covid_any__racism_doc_b=0","eq1_covid_any__racism_doc_w=0",
 "eq2_covid_any__black_doc_b=0","eq2_covid_any__black_doc_w=0","eq2_covid_any__racism_ama_b=0","eq2_covid_any__racism_ama_w=0","eq2_covid_any__racism_doc_b=0","eq2_covid_any__racism_doc_w=0",
  "eq3_covid_any__black_doc_b=0","eq3_covid_any__black_doc_w=0","eq3_covid_any__racism_ama_b=0","eq3_covid_any__racism_ama_w=0","eq3_covid_any__racism_doc_b=0","eq3_covid_any__racism_doc_w=0",
 "eq4_covid_any__black_doc_b=0","eq4_covid_any__black_doc_w=0","eq4_covid_any__racism_ama_b=0","eq4_covid_any__racism_ama_w=0","eq4_covid_any__racism_doc_b=0","eq4_covid_any__racism_doc_w=0",
"eq5_covid_any__black_doc_b=0","eq5_covid_any__black_doc_w=0","eq5_covid_any__racism_ama_b=0","eq5_covid_any__racism_ama_w=0","eq5_covid_any__racism_doc_b=0","eq5_covid_any__racism_doc_w=0",

"eq6_covid_any__black_doc=0","eq6_covid_any__racism_ama=0","eq6_covid_any__racism_doc=0",
"eq7_covid_any__black_doc=0","eq7_covid_any__racism_ama=0","eq7_covid_any__racism_doc=0",
"eq8_covid_any__black_doc=0","eq8_covid_any__racism_ama=0","eq8_covid_any__racism_doc=0",
"eq9_covid_any__black_doc=0","eq9_covid_any__racism_ama=0","eq9_covid_any__racism_doc=0",
"eq10_covid_any__black_doc=0","eq10_covid_any__racism_ama=0","eq10_covid_any__racism_doc=0"))

# Test: effect is the same on Blacks and Whites

linearHypothesis(fitsur,c("eq1_covid_any__black_doc_b=eq1_covid_any__black_doc_w","eq1_covid_any__racism_ama_b=eq1_covid_any__racism_ama_w","eq1_covid_any__racism_doc_b=eq1_covid_any__racism_doc_w","eq1_covid_any_b=eq1_covid_any_w",
                          "eq2_covid_any__black_doc_b=eq2_covid_any__black_doc_w","eq2_covid_any__racism_ama_b=eq2_covid_any__racism_ama_w","eq2_covid_any__racism_doc_b=eq2_covid_any__racism_doc_w","eq2_covid_any_b=eq2_covid_any_w",
                          "eq3_covid_any__black_doc_b=eq3_covid_any__black_doc_w","eq3_covid_any__racism_ama_b=eq3_covid_any__racism_ama_w","eq3_covid_any__racism_doc_b=eq3_covid_any__racism_doc_w","eq3_covid_any_b=eq3_covid_any_w",
                          "eq4_covid_any__black_doc_b=eq4_covid_any__black_doc_w","eq4_covid_any__racism_ama_b=eq4_covid_any__racism_ama_w","eq4_covid_any__racism_doc_b=eq4_covid_any__racism_doc_w","eq4_covid_any_b=eq4_covid_any_w",
                          "eq5_covid_any__black_doc_b=eq5_covid_any__black_doc_w","eq5_covid_any__racism_ama_b=eq5_covid_any__racism_ama_w","eq5_covid_any__racism_doc_b=eq5_covid_any__racism_doc_w","eq5_covid_any_b=eq5_covid_any_w"))



