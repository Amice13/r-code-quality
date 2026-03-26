# Interaction tables (appendix): sex, education and prior estimate

# Lines 94-122: all of the main parameters (interaction, DPL controls, weights,...) can be changed here

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

raw$resp_lang[grep("Spanish",raw$resp_lang)]="Spanish" # We replace "Spanish/Espa\xf1ol" with "Spanish" to avoid further errors due to unknown characters


dem_options <- c("All", "African-American", "Latinx")



binary_outcomes= c("dont_know_3_practices","dont_know_asymptomatic", "dont_know_symptoms","want_mask_vid")

discrete_outcomes = c("intended_behavior","knowledge_count_variable")

knowledge_outcomes = c("knowledge_count_variable","posterior_protect","posterior_no_good", "posterior_was_sick")

links_outcomes = c("intended_behavior","want_mask_vid","masks_for_the_people")

videos_outcomes = c("vid1_net_rating", "vid2_net_rating", "vid3_net_rating")

# Creation of variables

raw$femaletreated = raw$female*raw$anytreat
raw$maletreated = (1-raw$female)*raw$anytreat
raw$femalecontrol = raw$female*(1-raw$anytreat)
raw$malecontrol = (1-raw$female)*(1-raw$anytreat)

raw$hs_or_below = as.numeric(raw$education_numeric <= 4)

raw$hs_or_below_treated = raw$hs_or_below* raw$anytreat
raw$hs_or_below_control = raw$hs_or_below*(1- raw$anytreat)
raw$higher_educ_treated = (1-raw$hs_or_below)* raw$anytreat
raw$higher_educ_control = (1-raw$hs_or_below)*(1- raw$anytreat)

raw$under_treated = raw$prior_underestimate* raw$debiasing
raw$under_control = raw$prior_underestimate* (1-raw$debiasing)
raw$over_treated = raw$prior_overestimate* raw$debiasing
raw$over_control = raw$prior_overestimate* (1-raw$debiasing)

raw_anytreat <- raw %>% filter(anytreat == 1)


OLS_outcomes = c("posterior_protect","posterior_no_good", "posterior_was_sick","masks_for_the_people")
logit_outcomes = c("want_mask_vid")
NB_outcomes = c("knowledge_count_variable", "intended_behavior")


female_interaction = c("femaletreated","maletreated","female","Female","Male")
education_interaction = c("hs_or_below_treated","higher_educ_treated","hs_or_below","Low education","High education")
prior_estimate_interaction = c("under_treated","over_treated","prior_overestimate","Underestimate","Overestimate")

#### PARAMETERS

DPL = FALSE # whether we want to add DPL controls in regressions or not
weights = TRUE# whether we want to weight the regressions with hainmueller weights or not

#Outcomes choice:

#outcomes = OLS_outcomes
#outcome_type = "OLS"

#outcomes = logit_outcomes
#outcome_type = "logit"

outcomes = NB_outcomes
outcome_type = "NB"


#Interaction:

interaction <-female_interaction
interaction_type = "sex"

#interaction  <-education_interaction
#interaction_type = "education"

#interaction = prior_estimate_interaction
#interaction_type = "prior_estimate"

#### END OF PARAMETERS

Completed_key_baseline = TRUE # TRUE if we want to compute the means on the same observations that are used for the weighted regressions

## Regression with every treatment as controls 

rel_treatments = interaction[1:3]
labels = interaction[4:5]

# Selection of the relevant data set for the regressions

if (interaction_type == "prior_estimate"){
data_model = raw_anytreat
}else{
  data_model = raw
}


primary_rhs = paste0(rel_treatments, collapse = "+")
outcome_grp_title <- "Title"
coefficients = list()
standarderrors = list()
pvalues = list()
pvalues_diff = list()
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
        model <- run_reg_discrete_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=weights,DPL=DPL)
        model_irr=To_irr(model)
        model_irr=model_irr$irr
        
        CI_lower_bound[[i]]=model_irr[,"CI_lower_bound"] # CI for the IRR
        CI_upper_bound[[i]]=model_irr[,"CI_upper_bound"] # CI for the IRR
        coefficients[[i]]=model_irr[,"IRR"] # IRR 
        pvalues[[i]]=model_irr[,"P>|z|"] # p values for test coef == 0
      }else{
        model <- run_reg_outcome(outcome, data_reg, primary_rhs, baseline_vars_dummies,balance_model=weights,DPL=DPL)
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
    
      if (interaction_type == "sex"){
        test = "femaletreated = maletreated"
      }
      if (interaction_type == "education"){
        test = "hs_or_below_treated = higher_educ_treated"
      }
      if (interaction_type == "prior_estimate"){
        test = "under_treated = over_treated"
      }
      pvalues_diff =  c(pvalues_diff,linearHypothesis(model, test,singular.ok = TRUE,test = "F")[2, "Pr(>F)"])
      ## test of the difference between control and treated respondents
    
  
    models[[i]] = model
    nb_obs[[i]] = nobs(model)

    
    
    
    i = i + 1
  }
  
  

}


#### Code for tables 

## Coefficients/IRR/Odds:

lines = c()
for (i in 1:(length(models))){
  
  temp = paste0(round(coefficients[[i]][1],3)," (",round(CI_lower_bound[[i]][1],3),",",round(CI_upper_bound[[i]][1],3),")")
  lines = c(lines,temp)
  p1 = round(pvalues[[i]][1],3)

  
   temp = paste0(round(coefficients[[i]][2],3)," (",round(CI_lower_bound[[i]][2],3),",",round(CI_upper_bound[[i]][2],3),")")
   lines = c(lines,temp)
   p2 = round(pvalues[[i]][2],3)
   lines = c(lines,p1)
   lines = c(lines,p2)

  
  
  lines = c(lines,round(pvalues_diff[[i]],3))
  if (i%%(length(outcomes))==0){
    lines  = c(lines,"","","","","")

  }
}
tab = t(matrix(lines, ncol=3+length(models)))

outcomes_labels = get_labels(outcomes)
rownames(tab) = rep(c(outcomes_labels,""),3)

colnames(tab)=c(labels,paste0(labels[1], " p-value"),paste0(labels[2], " p-value"),"Difference")


write.csv(tab, paste0("./Appendix_output/",outcome_type,"_interact_",interaction_type,"_regression_weights",weights,"_DPL_",DPL,".csv"))


### Control/treatment means:

lines = c()
i=1


if (interaction_type == "sex"){
  groups = c("femalecontrol","femaletreated","malecontrol","maletreated")
}
if (interaction_type == "education"){
  groups = c("hs_or_below_control","hs_or_below_treated","higher_educ_control","higher_educ_treated")
}
if (interaction_type == "prior_estimate"){
  groups = c("under_control","under_treated","over_control","over_treated")
}

for (var in groups){
  

   obs = c()
   for (dem_select in dem_options) {
     df <- get_filtered_data(data_model, dem_select)

     
    df = df[df[[var]]==1,]
    
    if ("knowledge_count_variable" %in% outcomes){
      temp = df[!is.na(df$knowledge_count_variable),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_knowledge),]}#if we want to omit the uncomplete data
      obs = c(obs,nrow(temp))
      
      formula = as.formula(paste("knowledge_count_variable","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/7  # We have to divide by max value (7) to get the incidence rate
      coef = coef_model[1,"Estimate"]/7  # We have to divide by max value (7) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")")) 
      

      temp = df[!is.na(df$intended_behavior),]
      if (Completed_key_baseline==TRUE){
      temp = temp[!is.na(temp$weights_links),]}#if we want to omit the uncomplete data
      obs = c(obs,nrow(temp))
      
      formula = as.formula(paste("intended_behavior","  ~  ","1"))
      reg =  lm(formula = formula, data = temp)
      coef_model = summary(reg)$coefficients
      se = coef_model[1,"Std. Error"]/10  # We have to divide by max value (10) to get the incidence rate
      coef = coef_model[1,"Estimate"]/10  # We have to divide by max value (10) to get the incidence rate
      
      lines = c(lines,paste0(round(coef ,3)," (",round(coef - 1.96*se,3),",",round(coef + 1.96*se,3),")")) 
      
}
    else{
    for (outcome in outcomes){
      temp = df[!is.na(df[[outcome]]),]
      if (Completed_key_baseline==TRUE){
      if (outcome %in% links_outcomes){# for DIY mask video outcome
        temp = temp[!is.na(temp$weights_links),]#if we want to omit the uncomplete data
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
    
    obs = c(obs,"")
    lines = c(lines,"")
  }
  lines = c(lines,obs)
}

outcomes_labels = get_labels(outcomes)
tab = matrix(lines, ncol=8)
rownames(tab) = rep(c(outcomes_labels,""),3)
colnames(tab) = c(paste0(labels[1]," control"),"Observations",paste0(labels[1]," treated"),"Observations",
                  paste0(labels[2]," control"),"Observations",paste0(labels[2]," treated"),"Observations"
)
write.csv(tab, paste0("./Appendix_output/",outcome_type,"_interact_",interaction_type,"_means_weights",weights,".csv"))

