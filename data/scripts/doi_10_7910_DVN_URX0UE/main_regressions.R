# This file runs the main regressions for the paper 
# (Tables 2 and 3, Supplement Tables 3, 4, 5, 6, 7, 8a, 8b, 11)

# Parameters can be changed at lines 68 (DPL) and 69 (weights)

# The generated regressions depend on the values of these two parameters

# DPL = FALSE, weights = TRUE:
# this code will run the regressions for Tables 2 and 3, Supplement Tables 7, 8a, 8b, 11

# DPL = TRUE, weights = TRUE:
# this code will run the regressions for Supplement Table 3 and 4

# DPL = FALSE, weights = FALSE:
# this code will run the regressions for Supplement Table 5 and 6

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
library("ggplot2")

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
OLS_outcomes = c("wtp_masks","charity_black","charity_covid")
logit_outcomes = c("fed_balance_agree", "state_balance_agree")
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")


#### PARAMETERS

# The three panels for the main regressions
dem_options <- c("All", "African-American", "White")

DPL = FALSE# TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE # TRUE if we want to include the hainmueller weights in the regression for follow up outcomes, else FALSE

# Build covariates for DPL regressions
source("dummy_variables.R")


### Computation of regressions for all types of outcomes
for (outcome_type in c("NB","OLS","logit")){

outcomes = switch(outcome_type,"OLS"=OLS_outcomes,"NB"=NB_outcomes,"logit"=logit_outcomes)



####### Regression with every treatment 

rel_treatments <-c("covid_any__black_doc","covid_any__racism_ama","covid_any__racism_doc","black_doc",
                     "racism_ama",
                     "racism_doc","covid_any")


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
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
for (dem_select in dem_options) { # This loop computes all the regressions

  
  
  for (outcome in outcomes) { # We run the appropriate regression for each outcome
    
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
        
        if (dem_select=="All"){
        model_residuals = residuals(model)
        df_hist = as.data.frame(model_residuals)
        p<-ggplot(df_hist, aes(x=model_residuals)) + 
          geom_histogram(color="black", fill="grey",binwidth=0.5)+theme_bw()+
          xlab("Residuals")+ylab("Observations")
        last_plot()
        ggsave(paste0("./Output/histogram_residuals_",outcome,"_weights_",weights,"_DPL_",DPL,"_.png"), width = 6, height =5)
        }
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
    
    
    
    
    i = i + 1
  }
  
  
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
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }

  
  lines = c(lines,nb_obs[[i]]) # Number of observations
}
tab = t(matrix(lines, ncol= length(models))) # matrix containing the table to export

outcomes_labels = get_labels(outcomes)


cols = c("covid_any__black_doc","p-value",
    "covid_any__racism_ama","p-value",
    "covid_any__racism_doc","p-value",
    "black_doc","p-value",
    "racism_ama","p-value",
    "covid_any","p-value")	

colnames(tab)=c(cols,"Observations")
rownames(tab)=rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"_regression_all_treatments_weights_",weights,"_DPL_",DPL,".csv"))


###########################
### Control/treatment means:
###########################
# Sample = any covid treatment

rel_treatments <-c(
  "concord_doc",
  "racism_ama",
  "racism_doc")

lines = c()
i=1
for (treatment in rel_treatments){ # we compute the control/treatment means for each treatment
  for (j in 0:1){
    

    for (dem_select in dem_options) {

      for (outcome in outcomes){ 
        df <- get_filtered_data(raw_anytreat, dem_select,outcome) #select the right data set
        df = df[df[[treatment]]==j,] # control or treated respondents depending on j
        temp = df[!is.na(df[[outcome]]),]
        
        if (outcome%in% follow_outcomes){ # we take the observations used in the weighted regression
          
          temp=temp[!is.na(temp$weights_follow),]
        }
        
        
        if (outcome %in% NB_outcomes){ # count variables must be normalized to get the incidence rate
          
          formula = as.formula(paste(outcome,"  ~  ","1"))
          reg =  lm(formula = formula, data = temp)
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE) # We have to divide by max value to get the incidence rate
          coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE) 
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")"),nrow(temp)) 
          
          
        }else{
          formula = as.formula(paste(outcome,"  ~  ","1"))
          reg =  lm(formula = formula, data = temp)
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"]
          coef = coef_model[1,"Estimate"]
          
          lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")"),nrow(temp)) 
        }
        
        
      }
    }
  }
}

tab = matrix(lines, ncol=2*length(rel_treatments)) # matrix containing the data we want to export
outcomes_labels = get_labels(outcomes)
colnames(tab)= c(
                   "black_doc=0","black_doc=1",
                   "racism_ama=0", "racism_ama=1",
                   "racism_doc=0","racism_doc=1")
rownames= c()
for (i in 1:length(outcomes)){
  rownames = c(rownames,outcomes_labels[i],"Observations")
  
}
rownames(tab)= rep(rownames,3)


write.csv(tab, paste0("./Output/",outcome_type,"_means_all_treatments_weights_",weights,".csv"))

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
models <- list()
cmeans <- c()
nb_obs = list()
i = 1
for (dem_select in dem_options) {
  
  
  
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
    
    
    
    
    i = i + 1
  }
  
  

}

###########################
### We build the table to export:
###########################
lines = c()
for (i in 1:length(models)){
  for (j in 1:1){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }

  lines = c(lines,nb_obs[[i]])
  
}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)

colnames(tab) = colnames(tab)= c("Coefficient","p-value", "Observations")
rownames(tab) =  rep(outcomes_labels,length(dem_options))

write.csv(tab, paste0("./Output/",outcome_type,"_regression_anytreat_weights_",weights,"_DPL_",DPL,".csv"))



### Control/treatment means

lines = c()
pvalues = c()
i=1
for (treatment in c("anytreat")){
  for (j in 0:1){
    obs = c()

    for (dem_select in dem_options) {

        for (outcome in outcomes){ # for other outcomes
          df <- get_filtered_data(raw, dem_select,outcome)
          df = df[df[[treatment]]==j,]
          temp = df[!is.na(df[[outcome]]),]
          
          
          if (outcome %in% follow_outcomes){#select sample = participants who answered the knowledge questions in the follow up survey
            
            temp=temp[!is.na(temp$weights_follow),]
          }
          obs = c(obs,nrow(temp))
          
          if (outcome %in% NB_outcomes){

            formula = as.formula(paste(outcome,"  ~  ","1"))
            reg =  lm(formula = formula, data = temp)
            coef_model = summary(reg)$coefficients
            se = coef_model[1,"Std. Error"]/max(raw[[outcome]],na.rm=TRUE)
            coef = coef_model[1,"Estimate"]/max(raw[[outcome]],na.rm=TRUE)
            
            lines = c(lines,paste0(format(round(coef , digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
          
          }else{
            formula = as.formula(paste(outcome,"  ~  ","1"))
            reg =  lm(formula = formula, data = temp)
            coef_model = summary(reg)$coefficients
            se = coef_model[1,"Std. Error"]
            coef = coef_model[1,"Estimate"]
            
            lines = c(lines,paste0(format(round(coef, digits=3), nsmall = 3)," (",format(round(coef - 1.96*se, digits=3), nsmall = 3),",",format(round(coef + 1.96*se, digits=3), nsmall = 3),")")) 
        }
        
        
      }
      
    }
    lines = c(lines,obs)
  }
}


tab = matrix(lines, ncol=4)
colnames(tab) = colnames(tab)= c("Control", "Observations", "Treatment", "Observations")
rownames(tab) =  c(outcomes_labels,outcomes_labels,outcomes_labels)



write.csv(tab, paste0("./Output/",outcome_type,"_means_anytreat_weights_",weights,".csv"))

}
