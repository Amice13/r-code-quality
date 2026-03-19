# This file computes the main regressions of the paper

# The first one is the regression of the outcomes on all treatments (race concordance, birx video,...)
# The second one is a regression of the outcome on a dummy variable "Any treatment"

# Lines 74-90: all of the main parameters (outcomes, DPL controls, weights,...) can be changed here

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

# Import of useful functions
source('shortlist_functions.R')

simulation = 0
set.seed(302)

# Import of the data set
mainlaunch <- "./Data/COVID USA T1_May 27, 2020_07.20.csv"


df=read_csv(mainlaunch)[-c(1,2),]

df$StartTime <- anytime(df$StartDate) #parse
df$duration <- as.numeric(df$`Duration (in seconds)`)

raw <- df %>% filter((StartTime > anytime("2020-05-13 15:00:00 EDT")) & (DistributionChannel == "anonymous") & (Q2 == "Yes, I would like to take part in this study, and confirm that I LIVE IN THE U.S., and I am 18 or older") & is.na(test))

raw <- as.data.frame(raw)


# Pre-processing
source('basic_cleaning.R')

# Computation of weights
source('hainmueller_weights.R')

raw$resp_lang[grep("Spanish",raw$resp_lang)]="Spanish" # We replace "Spanish/Espa\xf1ol" with "Spanish" to avoid further errors due to unknown characters


# The three panels
dem_options <- c("All", "African-American", "Latinx")

# Outcomes type (it enables to select the right regression model)
binary_outcomes= c("dont_know_3_practices","dont_know_asymptomatic", "dont_know_symptoms","want_mask_vid","want_any_link","any_knowledge_gap")

discrete_outcomes = c("intended_behavior","knowledge_count_variable")


# categories for hainmueller weighting:
knowledge_outcomes = c("knowledge_count_variable","posterior_protect","posterior_no_good", "posterior_was_sick","any_knowledge_gap")

links_outcomes = c("intended_behavior","want_mask_vid","masks_for_the_people","want_any_link")

videos_outcomes = c("vid1_net_rating", "vid2_net_rating", "vid3_net_rating")

# Outcomes are treated separetely according to the regression type
OLS_outcomes = c("posterior_protect","posterior_no_good", "posterior_was_sick","masks_for_the_people")
logit_outcomes = c("want_mask_vid","any_knowledge_gap","want_any_link")
NB_outcomes = c("knowledge_count_variable", "intended_behavior")


#### PARAMETERS

DPL = FALSE # TRUE if we want to include the double post lasso controls in the regression, else FALSE
weights = TRUE# TRUE if we want to include the hainmueller weights in the regression, else FALSE

# Outcomes choice for the regressions: uncomment the ones you want

#outcomes = OLS_outcomes
#outcome_type = "OLS"

#outcomes=logit_outcomes
#outcome_type = "logit"

outcomes = NB_outcomes
outcome_type = "NB"

#### END OF PARAMETERS

Completed_key_baseline = TRUE# TRUE if we want to compute the means on the same observations that are used for the weighted regressions


# Output file
if (outcome_type == "NB"){
  output_file = "Output"
}else{
  output_file = "Appendix_output"
}


####### Regression with every treatment 
rel_treatments <-c("race_concord","birx","elephant1","elephant2","debiasing")
#define primary rhs
primary_rhs = paste0(rel_treatments, collapse = "+")

raw_anytreat <- raw %>% filter(anytreat == 1)


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
  
 
  #filter to the right data set
  data_reg <- get_filtered_data(raw_anytreat, dem_select)
  
  #double lasso for treatment (selection of the covariates)
  support_t_net <- get_lasso_support(rel_treatments, baseline_vars_dummies, data_reg)
  

  
  for (outcome in outcomes) { # We run the appropriate regression for each outcome
    
    if (outcome %in% binary_outcomes){ # Logit model
      model <- run_reg_binary_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL)
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
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL )
        model_irr=To_irr(model)
        model_irr=model_irr$irr

        
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0

        
          
        
        
        
      }else{ # OLS model
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

## Table with the results of the regressions:

lines = c()
for (i in 1:length(models)){ # Each model is a column of a panel in the table
  
  for (j in 1:5){

    temp = paste0(round(coefficients[[i]][j],3)," (",round(CI_lower_bound[[i]][j],3),",",round(CI_upper_bound[[i]][j],3),")") # coeff and CI
    lines = c(lines,temp)
    lines = c(lines,round(pvalues[[i]][j],3))
  }
  lines = c(lines,nb_obs[[i]]) # Number of observations
}
tab = t(matrix(lines, ncol= length(models))) # matrix containing the table to export

outcomes_labels = get_labels(outcomes)

colnames(tab) = c("Race concordance", "p-value","Birx","p-value",		"Acknowledgement 1","p-value",		"Acknowledgement 2","p-value",		"Debiasing","p-value","Observations"	) 
rownames(tab)=c(outcomes_labels,outcomes_labels,outcomes_labels)

write.csv(tab, paste0("./",output_file,"/",outcome_type,"_regression_all_treatments_weights_",weights,"_DPL_",DPL,".csv"))

### Table with control/treatment means:


lines = c()
i=1
for (treatment in c("race_concord","birx","elephant1","elephant2","debiasing")){ # we compute the control/treatment means for each treatment
  for (j in 0:1){
  for (dem_select in dem_options) {
    df <- get_filtered_data(raw_anytreat, dem_select) #select the right data set
    df = df[df[[treatment]]==j,] # control or treated respondents depending on j
    
    
    
    if ("knowledge_count_variable" %in% outcomes){ # We treat count variables separately because their mean must be divided
      temp = df[!is.na(df$knowledge_count_variable),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_knowledge),] }
      formula = as.formula(paste("knowledge_count_variable","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/7  # We have to divide by max value (7) to get the incidence rate
      coef = coef_model[1,"Estimate"]/7  # We have to divide by max value (7) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")"),nrow(temp)) 
      
      
      temp = df[!is.na(df$intended_behavior),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_links),] }
      formula = as.formula(paste("intended_behavior","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/10  # We have to divide by max value (10) to get the incidence rate
      coef = coef_model[1,"Estimate"]/10  # We have to divide by max value (10) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")"),nrow(temp)) 
      

    }else{
    
    for (outcome in outcomes){ # For the other outcomes
      temp = df[!is.na(df[[outcome]]),]
      if (Completed_key_baseline==TRUE){
      if (outcome %in% links_outcomes){
        temp = temp[!is.na(temp$weights_links),]
      }
        if (outcome %in% knowledge_outcomes){
          temp = temp[!is.na(temp$weights_knowledge),]
        }
        
      }
      
      formula = as.formula(paste(outcome,"  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]  
      coef = coef_model[1,"Estimate"] 
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")"),nrow(temp)) 

      
    }}
  }
  }
}

tab = matrix(lines, ncol=10) # matrix containing the data we want to export
outcomes_labels = get_labels(outcomes)
colnames(tab)= c("Race concordance = 0", "Race concordance = 1", "Birx = 0",	"Birx = 1",	"Acknowledgement 1 = 0",	"Acknowledgement 1 = 1",	"Acknowledgement 2 = 0","Acknowledgement 2 = 1", "Debiasing = 0", "Debiasing = 1")
rownames= c()
for (i in 1:length(outcomes)){
  rownames = c(rownames,outcomes_labels[i],"Observations")
  
}
rownames(tab)= rep(rownames,3)

write.csv(tab, paste0("./",output_file,"/",outcome_type,"_means_all_treatments_weights_",weights,"_completed_key_baseline_",Completed_key_baseline,".csv"))

## Regressions for anytreat :
## Very similar to the previous loop, but for "Any treatment" instead of "race concordance + birx + ..."

rel_treatments <-c("anytreat")

primary_rhs = paste0(rel_treatments, collapse = "+")


#raw_anytreat <- raw %>% filter(anytreat == 1)

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
  
  
  #filter to the right thing 
  data_reg <- get_filtered_data(raw, dem_select)
  
  #double lasso for t
  support_t_net <- get_lasso_support(rel_treatments, baseline_vars_dummies, data_reg)
  
  
  
  for (outcome in outcomes) {
    
    if (outcome %in% binary_outcomes){
      model <- run_reg_binary_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL)
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
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,  balance_model=weights,DPL=DPL )
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


# We build the table:

lines = c()
for (i in 1:length(models)){
  for (j in 1:1){
    temp = paste0(round(coefficients[[i]][j],3)," (",round(CI_lower_bound[[i]][j],3),",",round(CI_upper_bound[[i]][j],3),")") # coef and CI
    lines = c(lines,temp)
    lines = c(lines,round(pvalues[[i]][j],3))
  }
  lines = c(lines,nb_obs[[i]])

}
tab = t(matrix(lines, ncol=length(models))) # matrix containing the table with the regressions
outcomes_labels = get_labels(outcomes)

colnames(tab) = colnames(tab)= c("Coefficient","p-value", "Observations")
rownames(tab) =  c(outcomes_labels,outcomes_labels,outcomes_labels)

write.csv(tab, paste0("./",output_file,"/",outcome_type,"_regression_anytreat_weights_",weights,"_DPL_",DPL,".csv"))



### Control/treatment means

lines = c()

i=1
for (treatment in c("anytreat")){
  for (j in 0:1){
    obs = c()
    for (dem_select in dem_options) {
      df <- get_filtered_data(raw, dem_select)
      df = df[df[[treatment]]==j,]
      
      if ("knowledge_count_variable" %in% outcomes){ # We treat count variables separately because their mean must be divided
      temp = df[!is.na(df$knowledge_count_variable),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_knowledge),] }#if we want to omit the uncomplete data
      
      formula = as.formula(paste("knowledge_count_variable","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/7  # We have to divide by max value (7) to get the incidence rate
      coef = coef_model[1,"Estimate"]/7 # We have to divide by max value (7) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")")) 
      
      obs = c(obs,nrow(temp))
      
      
      
      
      temp = df[!is.na(df$intended_behavior),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_links),] }#if we want to omit the uncomplete data
      
      
      
      formula = as.formula(paste("intended_behavior","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/10  # We have to divide by max value (10) to get the incidence rate
      coef = coef_model[1,"Estimate"]/10 # We have to divide by max value (10) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")")) 
      
      obs = c(obs,nrow(temp))
      

      }else{
        
        for (outcome in outcomes){ # for other outcomes
          temp = df[!is.na(df[[outcome]]),]
          if (Completed_key_baseline==TRUE){
          if (outcome %in% links_outcomes){
            temp = temp[!is.na(temp$weights_links),]
          }
          if (outcome %in% knowledge_outcomes){
              temp = temp[!is.na(temp$weights_knowledge),]
          }}
          
          formula = as.formula(paste(outcome,"  ~  ","1"))
          reg =  lm(formula = formula, data = temp)
          coef_model = summary(reg)$coefficients
          se = coef_model[1,"Std. Error"] 
          coef = coef_model[1,"Estimate"] 
          
          lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")")) 
          obs = c(obs,nrow(temp))
          
        }
        
        
      }
      
    }
    lines = c(lines,obs)
  }
}

tab = matrix(lines, ncol=4)
colnames(tab) = colnames(tab)= c("Control", "Observations", "Treatment", "Observations")
rownames(tab) =  c(outcomes_labels,outcomes_labels,outcomes_labels)


write.csv(tab, paste0("./",output_file,"/",outcome_type,"_means_anytreat_weights_",weights,"_completed_",Completed_key_baseline,".csv"))





