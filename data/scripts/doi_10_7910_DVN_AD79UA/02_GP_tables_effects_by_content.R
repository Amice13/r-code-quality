# ReadMe ----------------------------------------------------------------------------
# Project - Covid19 WB info campaign
# Author  - Meghna Sinha Ray & Louis-Mael Jean
# Purpose - Outcome Survey Regressions over Time and by Content
# Notes   - This file implements Tables S17-S19 and S22-S24. Note that other GP
#           appendix files tables are implemented in Stata do-files.

# libraries -------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(estimatr)
library(fixest)
library(broom)
library(modelsummary)
library(DescTools)
library(ggdist)
library(multcomp)
library(haven)
library(scales)
library(patchwork)
library(texreg)
library(modelsummary)
library(kableExtra)
library(webshot)
library(pastecs)

# Loading and Preparing Data  -------------------------------------------------------------

path_LM = "/Users/louis-maeljean/Dropbox (MIT)/West Bengal Information Campaign/AER_I/for_submission"

path = path_LM    #other users should change this
setwd(path)

df_main = read_dta("data/outcomes_reg_input.dta") ## 1989 obs

df_main = df_main %>% 
  mutate(across(.cols = all_of( c("district", "id_date", "jio_yn", "resp_gender", "smartphone")),
                as.factor)) %>% 
  rename(Neut = Type2, NO = Type1)


df_control = filter(df_main, Treatment == 0) #just the control group
outcomes = c('travel_own', 'W2total_interactions_vill', 'typical_handwash', 'resp_mask_wear', 'W2ever_talk', 'net_knowledge')
sum_stats <- stat.desc(df_control[outcomes])
control_stdevs = c(sum_stats["std.dev",outcomes])

df_main = df_main %>% mutate(Ztravel_own = travel_own/control_stdevs[[1]], 
                             ZW2total_interactions_vill = W2total_interactions_vill/control_stdevs[[2]], 
                             Ztypical_handwash = typical_handwash/control_stdevs[[3]], 
                             Zresp_mask_wear = resp_mask_wear/control_stdevs[[4]], 
                             ZW2ever_talk = W2ever_talk/control_stdevs[[5]], 
                             Znet_knowledge = net_knowledge/control_stdevs[[6]])

# Helper Functions  -------------------------------------------------------------

fit_model_by_content = function(outcome, treatments, sample, controls, data, ci) {
  
  model_data = switch(sample,
                      "Jio"      = filter(data, jio_yn == 1),
                      "Non_Jio"  = filter(data, jio_yn == 0),
                      "pooled"   = data)
  
  model_controls = switch(sample,
                          pooled = c(controls, "jio_yn"),
                          controls)
  
  tmt_form  = reformulate(response = outcome,
                          termlabels = c(treatments, model_controls))
  
  reg = lm_robust(formula  = tmt_form,
                  data     = model_data,
                  clusters = pincode,
                  se_type  = "stata",
                  alpha = (1 - ci)/2)

  return(reg)
}


# -------------------------------------------------------------------------------------------------------

output_regression_by_content = function(outcomes, controls, treatments, sample,type) { 

  for (t in 1:length(treatments)){
    pval_list <- c()
    for (i in 1:length(outcomes)){
      reg <-fit_model_by_content(outcomes[i],treatments[[t]],sample,controls,df_main,0.95)
      assign(paste0("m", i,treatments[[t]][1]),reg)
      p_test = summary(glht(reg, linfct = c(paste0(treatments[[t]][1]," - ", treatments[[t]][2]," = 0"))))$test$pvalues[1]
      pval_list <- c(pval_list, p_test)
    }
    assign(paste0("pval_",treatments[[t]][1]),pval_list)
  }
    
  for (treat in c("SD", "Ext", "NO")){
    m <- mget(ls(pattern = treat))        #Get all models with the same independent variables
    to_omit = tidy(m[[1]])$term[-c(2,3)]  #omit all variables but treatment
    to_omit = paste0(to_omit, collapse ="|") #provide correct format for model_summary
    
    gm = gof_map
    gm$clean[gm$raw == "nobs"] = "N"
    
    pval_list <- switch(treat, "SD" = pval_SD, "Ext" = pval_Ext, "NO" = pval_NO)
    for (i in 1:length(pval_list)){
      assign(paste0("p",i),toString(round(pval_list[i],3)))
    }
    pval_term <- switch(treat, "SD" = "p(SD = Hyg)", "Ext" = "p(Ext = Int)", "NO" = "p(NO = Neut)")
    rows_to_add <- switch(sample,
                            pooled = tribble(~term, ~m1,~m2,~m3,~m4,~m5,~m6,
                                             #pval_term, p1, p2, p3, p4, p5, p6,
                                             'District FE' , "Yes",  "Yes","Yes","Yes","Yes","Yes",
                                             'Survey Day FE',"Yes",  "Yes","Yes","Yes","Yes","Yes",
                                             'Jio FE' , "Yes",  "Yes","Yes","Yes","Yes","Yes")
                                             ,
                            tribble(~term, ~m1,~m2,~m3,~m4,~m5,~m6,
                                    #pval_term, p1, p2, p3, p4, p5, p6,
                                    'District FE' , "Yes",  "Yes","Yes","Yes","Yes","Yes",
                                    'Survey Day FE',"Yes",  "Yes","Yes","Yes","Yes","Yes"))
                                   
    
    attr(rows_to_add, 'position') <-switch(sample,
                                           pooled = c(7,8,9),
                                           c(7,8,9))
    
    models <- list("Did you travel outside\nyour village?" = m[[1]],
                   "No. of interactions\nwith people within \n2 arms length" = m[[2]],
                   "% time handwash upon\nreturning home" = m[[3]],
                   "Did you use\na mask?" = m[[4]],
                   "No. of COVID-19\nrelated conversations" = m[[5]],
                   "COVID-19 Knowledge\nIndex" = m[[6]])
    
    
    reg_output <- modelsummary(models,
                               statistic = c('({std.error})', '[{p.value}]'),
                               coef_omit = to_omit,
                               gof_omit="[^Num.Obs.]",
                               gof_map = gm,
                               add_rows = rows_to_add,
                               output = paste0("output/Tables/",sample,treat,"_by_content_",type,".tex"))
  }
    
}





# Analaysis Set up  -------------------------------------------------------------

outcomes = c('travel_own', 'W2total_interactions_vill', 'typical_handwash','resp_mask_wear', 'W2ever_talk', 'net_knowledge')
Zoutcomes = c('Ztravel_own', 'ZW2total_interactions_vill', 'Ztypical_handwash', 'Zresp_mask_wear', 'ZW2ever_talk', 'Znet_knowledge')

controls = c("district", "id_date", "resp_age", "resp_gender", "smartphone")
date_dummies = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9")
samples = c("pooled", "Jio", "Non_Jio")
treatments = list(c("SD", "Hyg"), c("Ext", "Int"),c("NO", "Neut"))





# Run regressions  -------------------------------------------------------------
for (sample in samples){
  if (sample == "pooled"){
    output_regression_by_content(Zoutcomes, controls, treatments,sample,"scaled") #Table A10
  } else {
    output_regression_by_content(outcomes, controls, treatments,sample,"unscaled") #Tables A8-A9
  }
}



################################################################################################
#
# END 
#
################################################################################################




################################################################################################
#
# MISCALLENIOUS 
#
################################################################################################


# texreg(list(m1_Jio, m2_Jio,m3_Jio,m4_Jio,m5_Jio,m6_Jio), include.ci = FALSE, omit.coef = to_omit, stars = numeric(0),
#        include.rsquared = F, include.adjrs = F, include.nobs = T, include.rmse = F, digits = 3,
#        custom.coef.names=c("Treatment day 3", "Treatment day 4",
#                       "Treatment day 5", "Treatment day 6",
#                       "Treatment day 7", "Treatment day 8",
#                       "Treatment day 10", "Treatment day 13",
#                       "Treatment day 14"), 
#        custom.model.names = c("Did you travel outside\nyour village?",
#                               "No. of interactions\nwith people within \n2 arms length",
#                               "% time handwash upon\nreturning home",
#                               "Did you use\na mask?",
#                               "No. of COVID-19\nrelated conversations",
#                               "COVID-19 Knowledge\nIndex"),
#        file = "Tables/Jio_over_time.tex")
# 


#notes = list(paste("District fixed effects are included",
#                    "as well as controls for age, gender and smartphone access",
#                    "Standard errors are clustered at the PIN code level and reported in parentheses.",
#                    "p-values are reported in brackets.")),
#
#
# #Standardizing variables
#df_main <- df_main %>% mutate(travel_own = scale(travel_own),
#                              W2total_interactions_vill = scale(W2total_interactions_vill),
#                              typical_handwash = scale(typical_handwash),
#                              resp_mask_wear = scale(resp_mask_wear),
#                              W2ever_talk = scale(W2ever_talk),
#                              net_knowledge = scale(net_knowledge))
#
#stat.desc(df_main[c("travel_own", "W2total_interactions_vill", "typical_handwash", "resp_mask_wear", "W2ever_talk", "net_knowledge")])  #check standardization

#