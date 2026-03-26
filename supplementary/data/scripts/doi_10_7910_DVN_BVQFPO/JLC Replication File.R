################################################################################
# Replication Data                                                             #
# Domestic Constitutional Oversight and International Courts:Islamic Law States#    
# Emilia Justyna Powell and Ilana Rothkopf                                     #
################################################################################
library(tidyverse)
library(stargazer)

#Load repdataset file (available as .RData, .csv, and .dta formats)

D <- repdataset

#####Table 2: Descriptive statistics for main variables#####
mainvars<- D %>% 
  dplyr::select(CCIndex, v2juhcind, IslamicIndex, SecularIndex2)
stargazer(mainvars, omit.summary.stat = c("n" , "p25", "p75"), type = "html", out="Table 2.htm") #html table for main vars descriptives


#####Main paper models#####

robustse.f <- function(model, cluster, df_correction) {
  ## Huber-White heteroskedasticity-robust standard error calculation and table generation code for lm and glm models in R. Citation: written by Joshua Gubler ~  http://joshuagubler.com.  
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  if(missing(cluster)) {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    model$se <- coeftest(model, vcov=vcovHC(model,"HC1"))[,2]
    model$vcovHC <- vcovHC(model,"HC1")
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcov=vcovHC(model,"HC1"))
  } else {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    vcovCL <- cluster.vcov(model, cluster, df_correction = df_correction)
    model$vcovCL <- vcovCL
    modelname <- paste(name,"clustrob",sep=".")
    model$se <- coeftest(model, vcovCL)[,2]
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcovCL)
  }
}

 
#Compulsory Jurisdiction#

#Model 1 (with interaction term)
m1<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*CCIndex, family= binomial(link = logit), data=D)
robustse.f(m1, ~COWcode, df_correction = F) #robust clustered standard errors

#Model 2 (basic)
m2 <- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time, family= binomial(link = logit), data=D) 
robustse.f(m2, ~COWcode, df_correction = F) #robust clustered standard errors

#Table 3
stargazer(m1, m2, se= list(m1.clustrob$se, m2.clustrob$se), column.labels = c("Model 1 (with interaction term)", "Model 2 (basic)"), type = "html", out = "Table 3.htm") 


#Compromissory Jurisdiction#

#Model 1 (with interaction term)
m3<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*CCIndex, data=D, maxit=100)
robustse.f(m3, ~COWcode, df_correction = F) #robust clustered standard errors

#Model 2 (basic)
m4 <- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time, data=D, maxit = 100) 
robustse.f(m4, ~COWcode, df_correction = F) #robust clustered standard errors

#Table 4
stargazer(m3, m4, se= list(m3.clustrob$se, m4.clustrob$se), column.labels = c("Model 1 (with interaction term)", "Model 2 (basic)"), type = "html", out = "Table 4.htm") 


#####Robustness Checks#####

#Models as described in Tables A1-A6 of the appendix. Models match the model numbers in these tables (e.g. Model 5  is "m5")

#Table A1
m5<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + democracy + pacsett_mod + time + v2juhcind*CCIndex, family= binomial(link = logit), data=D)
robustse.f(m5, ~COWcode, df_correction = F) #robust clustered standard errors

m6<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + region2 + v2juhcind*CCIndex, family= binomial(link = logit), data=D)
robustse.f(m6, ~COWcode, df_correction = F) #robust clustered standard errors

m7<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + custlaw.d + v2juhcind*CCIndex, family= binomial(link = logit), data=D)
robustse.f(m7, ~COWcode, df_correction = F) #robust clustered standard errors

m8<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + custlaw.d, family= binomial(link = logit), data=D)
robustse.f(m8, ~COWcode, df_correction = F) #robust clustered standard errors

m9<- glm(dichacceptance ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + chalstag.post + v2juhcind*CCIndex, family= binomial(link = logit), data=D) 
robustse.f(m9, ~COWcode, df_correction = F) #robust clustered standard errors

m10<- glm(dichacceptance ~ v2jureview + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*v2jureview, family= binomial(link = logit), data=D)
robustse.f(m10, ~COWcode, df_correction = F) #robust clustered standard errors

m11<- glm(dichacceptance ~ interpret + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*interpret, family= binomial(link = logit), data=D)
robustse.f(m11, ~COWcode, df_correction = F) #robust clustered standard errors

m12<- glm(dichacceptance ~ interpret + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time, family= binomial(link = logit), data=D)
robustse.f(m12, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m1, m2, m5, m6, m7, m8, m9, m10, m11, m12, se= list(m1.clustrob$se, m2.clustrob$se, m5.clustrob$se, m6.clustrob$se, m7.clustrob$se, m8.clustrob$se, m9.clustrob$se, m10.clustrob$se, m11.clustrob$se, m12.clustrob$se), type = "html", out = "Table A1.htm") 


#Table A2
m13<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + democracy + pacsett_mod + time + v2juhcind*CCIndex, data=D)
robustse.f(m13, ~COWcode, df_correction = F) #robust clustered standard errors

m14<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + region2 + v2juhcind*CCIndex, data=D)
robustse.f(m14, ~COWcode, df_correction = F) #robust clustered standard errors

m15<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + custlaw.d + v2juhcind*CCIndex, data=D)
robustse.f(m15, ~COWcode, df_correction = F) #robust clustered standard errors

m16<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + custlaw.d, data=D)
robustse.f(m16, ~COWcode, df_correction = F) #robust clustered standard errors

m17<- glm.nb(numcompt ~ CCIndex + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + chalstag.post + v2juhcind*CCIndex, data=D) 
robustse.f(m17, ~COWcode, df_correction = F) #robust clustered standard errors

m18<- glm.nb(numcompt ~ v2jureview + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*v2jureview, data=D)
robustse.f(m18, ~COWcode, df_correction = F) #robust clustered standard errors

m19<- glm.nb(numcompt ~ interpret + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*interpret,  data=D)
robustse.f(m19, ~COWcode, df_correction = F) #robust clustered standard errors

m20<- glm.nb(numcompt ~ interpret + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time,  data=D)
summary(m20a)
robustse.f(m20, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m3, m4, m13, m14, m15, m16, m17, m18, m19, m20, se= list(m3.clustrob$se, m4.clustrob$se, m13.clustrob$se, m14.clustrob$se, m15.clustrob$se, m16.clustrob$se, m17.clustrob$se, m18.clustrob$se, m19.clustrob$se, m20.clustrob$se), type = "html", out = "Table A2.htm") 


#Table A3
m21<- glm(dichacceptance ~ CCIndex + v2juhcind +  constitutionsi + constitutionho + headofstate + supremacyofsharia + shariaedu + constitutioncustomarylaw + ruleoflaw + prd + secularcourts + womenincts + educationinconst + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*CCIndex, family= binomial(link = logit), data=D)
robustse.f(m21, ~COWcode, df_correction = F) #robust clustered standard errors

m22 <- glm(dichacceptance ~ CCIndex + v2juhcind + constitutionsi + constitutionho + headofstate + supremacyofsharia + shariaedu + constitutioncustomarylaw + ruleoflaw + prd + secularcourts + womenincts + educationinconst + cinc + v2x_polyarchy + pacsett_mod + time, family= binomial(link = logit), data=D) 
robustse.f(m22, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m1, m2, m21, m22, se= list(m1.clustrob$se, m2.clustrob$se, m21.clustrob$se, m22.clustrob$se), type = "html", out = "Table A3.htm") 


#Table A4
m23<- glm.nb(numcompt ~ CCIndex + v2juhcind +  constitutionsi + constitutionho + headofstate + supremacyofsharia + shariaedu + constitutioncustomarylaw + ruleoflaw + prd + secularcourts + womenincts + educationinconst + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*CCIndex,  data=D)
robustse.f(m23, ~COWcode, df_correction = F) #robust clustered standard errors

m24 <- glm.nb(numcompt ~ CCIndex + v2juhcind + constitutionsi + constitutionho + headofstate + supremacyofsharia + shariaedu + constitutioncustomarylaw + ruleoflaw + prd + secularcourts + womenincts + educationinconst + cinc + v2x_polyarchy + pacsett_mod + time, data=D) 
robustse.f(m24, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m3, m4, m23, m24, se= list(m3.clustrob$se, m4.clustrob$se, m23.clustrob$se, m24.clustrob$se), type = "html", out = "Table A4.htm") #main compulsory models
  

#Table A5
m25<- glm(dichacceptance ~ con_court_mentioned + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*con_court_mentioned, family= binomial(link = logit), data=D)
robustse.f(m25, ~COWcode, df_correction = F) #robust clustered standard errors

m26<- glm(dichacceptance ~ times_con_court + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*times_con_court, family= binomial(link = logit), data=D)
robustse.f(m26, ~COWcode, df_correction = F) #robust clustered standard errors

m27<- glm(dichacceptance ~ word_count_con_court + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*word_count_con_court, family= binomial(link = logit), data=D)
robustse.f(m27, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m1, m25, m26, m27, se= list(m1.clustrob$se, m25.clustrob$se, m26.clustrob$se, m27.clustrob$se), type = "html", out = "Table A5.htm") 


#Table A6
m28<- glm.nb(numcompt ~ con_court_mentioned + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*con_court_mentioned, data=D, maxit=100)
robustse.f(m28, ~COWcode, df_correction = F) #robust clustered standard errors

m29<- glm.nb(numcompt ~ times_con_court + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*times_con_court, data=D, maxit=100)
robustse.f(m29, ~COWcode, df_correction = F) #robust clustered standard errors

m30<- glm.nb(numcompt ~ word_count_con_court + v2juhcind + IslamicIndex + SecularIndex2 + cinc + v2x_polyarchy + pacsett_mod + time + v2juhcind*word_count_con_court, data=D, maxit=100)
robustse.f(m30, ~COWcode, df_correction = F) #robust clustered standard errors

stargazer(m3, m28, m29, m30, se= list(m3.clustrob$se, m28.clustrob$se, m29.clustrob$se, m30.clustrob$se), type = "html", out = "Table A6.htm") 






