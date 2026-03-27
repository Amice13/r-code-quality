# This file runs regressions for the Supplement Tables 2a to 2g

# Depending on the panel (All, African-American or White) that you want for the age/prior belief interaction tables (Supp Table 2f/2d), 
# uncomment either line 69 (All), line 70 (African-American) or line 71 (White)

# Regressions for party interaction (Table 2e) are at the end of this script

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
raw$race_white__party_dem = raw$race_white * raw$party_dem
raw$race_white__party_rep = raw$race_white * raw$party_rep
raw$not_hs_grad = 1 - raw$hs_graduate
raw$independent = (1-raw$party_dem) * (1-raw$party_rep)
raw$prior_belief_lower_4= as.numeric(raw$b_death_mult_prior < 4)
raw$prior_belief_higher_4= as.numeric(raw$b_death_mult_prior >= 4)
raw$post_belief_lower_4= as.numeric(raw$b_death_mult_post < 4)
raw$post_belief_higher_4= as.numeric(raw$b_death_mult_post >= 4)

raw$hhi_under_60k = 1-raw$hhi_above_60k

raw$old=as.numeric(raw$age_strat=="old")
raw$young = as.numeric(raw$age_strat=="young")

# Outcomes type (it enables to select the right regression model/weights)

discrete_outcomes = c("intended_behavior","know_gap_count","safety_count_variable","know_gap_count_followup")
binary_outcomes = c("fed_balance_agree", "state_balance_agree","post_belief_lower_4","post_belief_higher_4")
follow_outcomes = c("safety_count_variable","know_gap_count_followup")


# Outcomes are treated separetely according to the regression type
OLS_outcomes = c("wtp_masks","charity_covid", "charity_black")
logit_outcomes = c("fed_balance_agree", "state_balance_agree","post_belief_lower_4","post_belief_higher_4") #belief variables are for prior belief interaction
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")


#### PARAMETERS


# The panels for additional regressions to compare groups


dem_options = c("All","All","All","All","All","All")
#dem_options = c("All","All","All","African-American","African-American","All") #to have panel = Black for age and prior belief interactions
#dem_options = c("All","All","All","White","White","All") #to have panel = White for age and prior belief interactions
interaction_var_1 = c("sex_male","not_hs_grad","resp_white","prior_belief_lower_4","young","hhi_under_60k")
interaction_var_2 = c("sex_female","hs_graduate","resp_black","prior_belief_higher_4","old","hhi_above_60k")


DPL = FALSE # TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE # TRUE if we want to include the hainmueller weights in the regression for follow up outcomes, else FALSE

# Build covariates for DPL regressions
source("dummy_variables.R")


### Computation of regressions for all types of outcomes
for (outcome_type in c("NB","OLS","logit")){
  
outcomes = switch(outcome_type,"OLS"=OLS_outcomes,"NB"=NB_outcomes,"logit"=logit_outcomes)


pvalues_diff = list()
coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
j=1
for (dem_select in dem_options) {
  

  
  for (outcome in outcomes) {
    
    
    
    #filter to the right thing 
    data_reg <- get_filtered_data(raw, dem_select,outcome)
    
    data_reg[[paste0(interaction_var_1[j],"__anytreat")]]= data_reg[[interaction_var_1[j]]]*data_reg$anytreat
    data_reg[[paste0(interaction_var_2[j],"__anytreat")]]= data_reg[[interaction_var_2[j]]]*data_reg$anytreat
    rel_treatments = c(paste0(interaction_var_1[j],"__anytreat"),paste0(interaction_var_2[j],"__anytreat"),
                         interaction_var_1[j])

    
    
    
    
    primary_rhs = paste0(rel_treatments, collapse = "+")
    
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

    pvalues_diff[[i]] =  linearHypothesis(model, paste0(interaction_var_1[j],"__anytreat = ",interaction_var_2[j],"__anytreat"),singular.ok = TRUE,test = "F")[2, "Pr(>F)"]

    
    models[[i]] = model
    nb_obs[[i]] = nobs(model)
    
    
    
    
    i = i + 1
  }
  j=j+1
  
  
}


# We build the table:

nb_coef = 2

lines = c()
for (i in 1:length(models)){
  for (j in 1:nb_coef){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }

  lines = c(lines,format(round(pvalues_diff[[i]], digits=3), nsmall = 3),nb_obs[[i]])
  
}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)
rownames(tab) =  rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"interacted_regression_anytreat_weights_all_except_party.csv"))

# Control and treatment means:


lines = c()
#pvalues = c()
i=1
for (dem_select in dem_options) {
  var1 = interaction_var_1[i]
  var2 = interaction_var_2[i]
    
      for (outcome in outcomes){ 
        for (j in 0:1){
        df <- get_filtered_data(raw, dem_select,outcome) #select the right data set
        df = df[df$covid_any==j,] # control or treated respondents depending on j
        temp = df[!is.na(df[[outcome]]),]
        
        if (outcome%in% c("safety_count_variable","know_gap_count_followup")){ # we take the observations used in the weighted regression
          
          temp=temp[!is.na(temp$weights_follow),]
        }
        
        
        if (outcome %in% NB_outcomes){ # count variables must be normalized to get the incidence rate
          
          formula = as.formula(paste(outcome,"  ~  ","1"))
          reg =  lm(formula = formula, data = temp[temp[[var1]]==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
          coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
          
          reg =  lm(formula = formula, data = temp[temp[[var2]]==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
          coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
          

        }else{
          formula = as.formula(paste(outcome,"  ~  ","1"))
          reg =  lm(formula = formula, data = temp[temp[[var1]]==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]
          coef = coef_model[1,"Estimate"]
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
          reg =  lm(formula = formula, data = temp[temp[[var2]]==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]
          coef = coef_model[1,"Estimate"]
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        
          

          }
        
        
      }
    }
  i=i+1
  }

nb_coef = 2

tab = t(matrix(lines, nrow=2*nb_coef)) # matrix containing the data we want to export
outcomes_labels = get_labels(outcomes)

rownames= outcomes_labels
rownames(tab)= rep(rownames,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"interacted_means_anytreat_weights_all_except_party.csv"))
}

#######
## Interaction table for Party dem/rep and Indep
#######

OLS_outcomes = c("wtp_masks","charity_covid", "charity_black")
logit_outcomes = c("fed_balance_agree", "state_balance_agree")
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")

interaction = "party"
dem_options = c("White")
interaction_var_1 = c("party_rep")
interaction_var_2 = c("party_dem")

### Computation of regressions for all types of outcomes
for (outcome_type in c("NB","OLS","logit")){
  
outcomes = switch(outcome_type,"OLS"=OLS_outcomes,"NB"=NB_outcomes,"logit"=logit_outcomes)


raw_anytreat <- raw %>% filter(covid_any==1)


pvalues_diff = list()
coefficients = list()
standarderrors = list()
CI_lower_bound = list()
CI_upper_bound = list()
pvalues = list()
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
j=1
for (dem_select in dem_options) {
  
  
  
  for (outcome in outcomes) {
    
    
    
    #filter to the right thing 
    data_reg <- get_filtered_data(raw, dem_select,outcome)
    
    data_reg[[paste0(interaction_var_1[j],"__anytreat")]]= data_reg[[interaction_var_1[j]]]*data_reg$anytreat
    data_reg[[paste0(interaction_var_2[j],"__anytreat")]]= data_reg[[interaction_var_2[j]]]*data_reg$anytreat

      data_reg$independent__anytreat= data_reg$independent*data_reg$anytreat
      
      rel_treatments = c(paste0(interaction_var_1[j],"__anytreat"),paste0(interaction_var_2[j],"__anytreat"),
                         "independent__anytreat",interaction_var_1[j],interaction_var_2[j])

    
    
    
    
    primary_rhs = paste0(rel_treatments, collapse = "+")
    
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

      pvalues_diff[[i]] =  c(linearHypothesis(model, paste0("party_rep__anytreat = party_dem__anytreat"),singular.ok = TRUE,test = "F")[2, "Pr(>F)"],
                             linearHypothesis(model, paste0("party_rep__anytreat = independent__anytreat"),singular.ok = TRUE,test = "F")[2, "Pr(>F)"],
                             linearHypothesis(model, paste0("independent__anytreat = party_dem__anytreat"),singular.ok = TRUE,test = "F")[2, "Pr(>F)"])

    
    models[[i]] = model
    nb_obs[[i]] = nobs(model)
    
    
    
    
    i = i + 1
  }
  j=j+1
  
  
}


# We build the table:

nb_coef =3

lines = c()
for (i in 1:length(models)){
  for (j in 1:nb_coef){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }
  
  lines = c(lines,format(round(pvalues_diff[[i]], digits=3), nsmall = 3),nb_obs[[i]])
  
}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)

rownames(tab) =  rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"interacted_regression_anytreat_weights_party.csv"))

# Control and treatment means:


lines = c()
#pvalues = c()
i=1
for (dem_select in dem_options) {
  var1 = interaction_var_1[i]
  var2 = interaction_var_2[i]
  
  for (outcome in outcomes){ 
    for (j in 0:1){
      df <- get_filtered_data(raw, dem_select,outcome) #select the right data set
      df = df[df$covid_any==j,] # control or treated respondents depending on j
      temp = df[!is.na(df[[outcome]]),]
      
      if (outcome%in% c("safety_count_variable","know_gap_count_followup")){ # we take the observations used in the weighted regression
        
        temp=temp[!is.na(temp$weights_follow),]
      }
      
      
      if (outcome %in% NB_outcomes){ # count variables must be normalized to get the incidence rate
        
        formula = as.formula(paste(outcome,"  ~  ","1"))
        reg =  lm(formula = formula, data = temp[temp[[var1]]==1,])
        coef_model = summary(reg)$coefficients
        se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
        coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
        
        lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        
        reg =  lm(formula = formula, data = temp[temp[[var2]]==1,])
        coef_model = summary(reg)$coefficients
        se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
        coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
        
        lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        

          reg =  lm(formula = formula, data = temp[temp$independent==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
          coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
          

      }else{
        formula = as.formula(paste(outcome,"  ~  ","1"))
        reg =  lm(formula = formula, data = temp[temp[[var1]]==1,])
        coef_model = summary(reg)$coefficients
        se = coef_model[1,"Std. Error"]
        coef = coef_model[1,"Estimate"]
        
        lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        reg =  lm(formula = formula, data = temp[temp[[var2]]==1,])
        coef_model = summary(reg)$coefficients
        se = coef_model[1,"Std. Error"]
        coef = coef_model[1,"Estimate"]
        
        lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        
        

          reg =  lm(formula = formula, data = temp[temp$independent==1,])
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]
          coef = coef_model[1,"Estimate"]
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 

      }
      
      
    }
  }
  i=i+1
}

nb_coef=3

tab = t(matrix(lines, nrow=2*nb_coef)) # matrix containing the data we want to export
outcomes_labels = get_labels(outcomes)

rownames= outcomes_labels
rownames(tab)= rep(rownames,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"interacted_means_anytreat_weights_party.csv"))

}