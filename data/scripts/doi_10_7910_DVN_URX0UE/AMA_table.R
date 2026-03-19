# This file computes the AMA specific table (Supplement Table 9)


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
raw$resp_black = 1-raw$race_white
raw$resp_white = raw$race_white
#rename variables
raw$intended_behavior = raw$links_total
raw$knowledge_count_variable = raw$know_gap_count
raw$safety_count_variable = 4-raw$safety_total_followup
raw$anytreat = raw$covid_any

raw$ama_black = as.numeric(raw$ama_race=="black")
raw$ama_white = as.numeric(raw$ama_race=="white")

raw$covid_any__racism_ama__ama_black = raw$covid_any*raw$racism_ama*raw$ama_black
raw$covid_any__racism_ama__ama_white = raw$covid_any*raw$racism_ama*raw$ama_white
raw$covid_any__nonracism_ama__ama_black = raw$covid_any*(1-raw$racism_ama)*raw$ama_black
raw$covid_any__nonracism_ama__ama_white = raw$covid_any*(1-raw$racism_ama)*raw$ama_white
raw$noncovid__racism_ama__ama_black = (1-raw$covid_any)*raw$racism_ama*raw$ama_black
raw$noncovid__racism_ama__ama_white = (1-raw$covid_any)*raw$racism_ama*raw$ama_white
raw$noncovid__nonracism_ama__ama_black = (1-raw$covid_any)*(1-raw$racism_ama)*raw$ama_black

# The three panels
dem_options <- c("All", "African-American", "White")

# Outcomes type (it enables to select the right regression model/weights)

discrete_outcomes = c("intended_behavior","know_gap_count","safety_count_variable","know_gap_count_followup")
binary_outcomes = c("fed_balance_agree", "state_balance_agree","any_knowledge_gap","links_any")
follow_outcomes = c("safety_count_variable","know_gap_count_followup")


# Outcomes are treated separetely according to the regression type
OLS_outcomes = c("wtp_masks","charity_black","charity_covid")
logit_outcomes = c("fed_balance_agree", "state_balance_agree")
NB_outcomes = c("know_gap_count", "intended_behavior","safety_count_variable","know_gap_count_followup")

#### PARAMETERS

DPL = FALSE # TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE # TRUE if we want to include the hainmueller weights in the regression for follow up outcomes, else FALSE

# Build covariates for DPL regressions
source("dummy_variables.R")

### Computation of regressions for all types of outcomes
for (outcome_type in c("NB","OLS","logit")){
  
outcomes = switch(outcome_type,"OLS"=OLS_outcomes,"NB"=NB_outcomes,"logit"=logit_outcomes)
  
  


####### Regression with every treatment 
rel_treatments <-c("covid_any__racism_ama__ama_black",
                   "covid_any__racism_ama__ama_white",
                   "covid_any__nonracism_ama__ama_black ",
                   "covid_any__nonracism_ama__ama_white",
                   "noncovid__racism_ama__ama_black",
                   "noncovid__racism_ama__ama_white",
                   "noncovid__nonracism_ama__ama_black")



primary_rhs = paste0(rel_treatments, collapse = "+")


#define primary rhs

raw_anytreat <- raw %>% filter(covid_any==1)

white_ama = list()
black_coeff_test = list()
covid_coeff_joint_test = list()
ama_coeff_joint_test = list()
black_equals_white =  list()
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
      
      if (i<(length(outcomes)+1)){
        print(i)
        black_equals_white[[i]]= linearHypothesis(model, 
                                                  "strat1 + strat2 + strat4 + strat5 + strat7 + strat8 + strat10 + strat11 = strat3 + strat6 + strat9 + strat12")[2,'Pr(>Chisq)' ]
      }
      
      covid_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                              "covid_any__racism_ama__ama_white=0",
                                                              "covid_any__nonracism_ama__ama_black=0",
                                                              "covid_any__nonracism_ama__ama_white=0"))[2,'Pr(>Chisq)' ]
      
      ama_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                            "covid_any__racism_ama__ama_white=0"))[2,'Pr(>Chisq)' ]
      
      black_coeff_test[[i]]= linearHypothesis(model, c("covid_any__racism_ama__ama_black = covid_any__racism_ama__ama_white ",
                                                       "covid_any__nonracism_ama__ama_black = covid_any__nonracism_ama__ama_white"))[2,'Pr(>Chisq)' ]
      
      white_ama[[i]]= linearHypothesis(model, "covid_any__racism_ama__ama_black = covid_any__nonracism_ama__ama_white")[2,'Pr(>Chisq)' ]
      
    }else{
      if (outcome %in% discrete_outcomes){ # Negative Binomial model
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs,balance_model=weights,DPL=DPL)
 
        model_irr=To_irr(model)
        model_irr=model_irr$irr
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0
        
        if (i<(length(outcomes)+1)){
          print(i)
          black_equals_white[[i]]= linearHypothesis(model, 
                                                    "strat1 + strat2 + strat4 + strat5 + strat7 + strat8 + strat10 + strat11 = strat3 + strat6 + strat9 + strat12")[2,'Pr(>Chisq)' ]
        }
        covid_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                                "covid_any__racism_ama__ama_white=0",
                                                                "covid_any__nonracism_ama__ama_black=0",
                                                                "covid_any__nonracism_ama__ama_white=0"))[2,'Pr(>Chisq)' ]
        
        ama_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                              "covid_any__racism_ama__ama_white=0"))[2,'Pr(>Chisq)' ]
        black_coeff_test[[i]]= linearHypothesis(model, c("covid_any__racism_ama__ama_black = covid_any__racism_ama__ama_white ",
                                                         "covid_any__nonracism_ama__ama_black = covid_any__nonracism_ama__ama_white"))[2,'Pr(>Chisq)' ]
        white_ama[[i]]= linearHypothesis(model, "covid_any__racism_ama__ama_black = covid_any__nonracism_ama__ama_white")[2,'Pr(>Chisq)' ]
        
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
        
        if (i<(length(outcomes)+1)){
          print(i)
          black_equals_white[[i]]= linearHypothesis(model, 
                                                    "strat1 + strat2 + strat4 + strat5 + strat7 + strat8 + strat10 + strat11 = strat3 + strat6 + strat9 + strat12")[2,'Pr(>F)' ]
        }
        covid_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                                "covid_any__racism_ama__ama_white=0",
                                                                "covid_any__nonracism_ama__ama_black=0",
                                                                "covid_any__nonracism_ama__ama_white=0"))[2,'Pr(>F)' ]
        
        ama_coeff_joint_test[[i]] = linearHypothesis(model, c("covid_any__racism_ama__ama_black=0",
                                                              "covid_any__racism_ama__ama_white=0"))[2,'Pr(>F)' ]
        black_coeff_test[[i]]= linearHypothesis(model, c("covid_any__racism_ama__ama_black = covid_any__racism_ama__ama_white ",
                                                         "covid_any__nonracism_ama__ama_black = covid_any__nonracism_ama__ama_white"))[2,'Pr(>F)' ]
        white_ama[[i]]= linearHypothesis(model, "covid_any__racism_ama__ama_black = covid_any__nonracism_ama__ama_white")[2,'Pr(>F)' ]
        
      }}
    
    
    models[[i]] = model
    nb_obs[[i]] = nobs(model)
    
    
    
    
    i = i + 1
  }
  
  
}


## Table :

lines = c()
for (i in 1:length(models)){ # Each model is a column of a panel in the table
  for (j in 1:length(rel_treatments)){
    temp = paste0(format(round(coefficients[[i]][j], digits=3), nsmall = 3)," (",format(round(CI_lower_bound[[i]][j], digits=3), nsmall = 3),",",format(round(CI_upper_bound[[i]][j], digits=3), nsmall = 3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,format(round(pvalues[[i]][j], digits=3), nsmall = 3))
  }
  if (i<length(outcomes)+1){
    lines = c(lines,format(round(black_equals_white[[i]], digits=3), nsmall = 3))
  }else{lines = c(lines,"")}
  
  lines = c(lines,format(round(covid_coeff_joint_test[[i]], digits=3), nsmall = 3))
  lines = c(lines,format(round(ama_coeff_joint_test[[i]], digits=3), nsmall = 3))

  lines = c(lines,format(round(black_coeff_test[[i]], digits=3), nsmall = 3))
  lines = c(lines,format(round(white_ama[[i]], digits=3), nsmall = 3))
  lines = c(lines,nb_obs[[i]]) # Number of observations
}
tab = t(matrix(lines, ncol= length(models))) # matrix containing the table to export

outcomes_labels = get_labels(outcomes)
cols = c("covid_any__racism_ama__ama_black","p-value",
         "covid_any__racism_ama__ama_white","p-value",
         "covid_any__nonracism_ama__ama_black ","p-value",
         "covid_any__nonracism_ama__ama_white","p-value",
         "noncovid__racism_ama__ama_black","p-value",
         "noncovid__racism_ama__ama_white","p-value",
         "noncovid__nonracism_ama__ama_black","p-value")


colnames(tab)=c(cols,"p-value white=black","p-value joint test COVID = 0","p-value joint test COVID*AMA racism = 0","p-value joint test COVID*AMA White = COVID*AMA Black",
                "p-value COVID*AMA racism = COVID * AMA White","Observations")
rownames(tab)=c(outcomes_labels,outcomes_labels,outcomes_labels)

write.csv(tab, paste0("./Output/",outcome_type,"_regression_AMA_weights_",weights,"_DPL_",DPL,".csv"))
}
