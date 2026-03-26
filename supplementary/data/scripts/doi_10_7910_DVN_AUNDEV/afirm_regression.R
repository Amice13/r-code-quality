#Load packages
library(readr)
library (ggplot2)
library (ggpubr)
library(dplyr)
library(ggsignif)

#Load dataset
#afirm_data <-read.csv("C:/Users/Roy/Google Drive/IDeAL MSc/Data Analysis/Gam lysate ELISA/AFIRM/afirm_data.csv")
afirm_data <-read.csv("afirm_data.csv")

#to generate binary variables
afirm_data$asexualmcl_pos<-ifelse(afirm_data$asexual_mcl > 0, 1, 0) # asexualmcl is asexual parasite density by microscopy
afirm_data$gammcl_pos<-ifelse(afirm_data$gam_mcl > 0, 1, 0)# gammcl is gametocyte density by microscopy
afirm_data$aspos<-ifelse(afirm_data$nasba_18s > 0, 1, 0) #nasba_18s is asexual denisty by molecular detection (NASBA)
afirm_data$qpcrpos<-ifelse(afirm_data$qpcr_18s > 0, 1, 0) #qpcr_18s is asexual density by molecular detection (qPCR)....not used in analysis
afirm_data$gampos<-ifelse(afirm_data$nasba_pfs25 > 0, 1, 0) #nasba_pfs25 is gametocyte density by molecular detection (NASBA)


#To generate agegroups
afirm_data$age_groups <- afirm_data$age
afirm_data$age_groups <- ifelse((afirm_data$age >=0 &afirm_data$age<=4),"<5 Yrs", afirm_data$age_groups)
afirm_data$age_groups <- ifelse((afirm_data$age >=5 &afirm_data$age<=15),"5-15 Yrs", afirm_data$age_groups)
afirm_data$age_groups <- ifelse((afirm_data$age >=16 &afirm_data$age<=70),">15 Yrs", afirm_data$age_groups)


#----Predicting gametocyte positivity----
#Robust sandwich estimator
#'function to generate robust standard errors for cluster analysis 
#'with confidence intervals, also gives F-statistic via Wald test
super.cluster.fun <- function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL <- cluster.vcov(model, cluster)
  
  coef <- coeftest(model, vcovCL)
  w <- waldtest(model, vcov = vcovCL, test = "F")
  ci <- get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}

get_confint <- function(model, vcovCL) {
  t <- qt(.975, model$df.residual)
  ct <- coeftest(model, vcovCL)
  est <- cbind(ct[, 1], ct[, 1] - t * ct[, 2], ct[, 1] + t * ct[, 2])
  colnames(est) <- c("Estimate", "LowerCI", "UpperCI")
  return(est)
}

#Univariable - run for each variable tested
#Univariable analysis gam positivity (gampos) and asexual positivity (nasba_18s)
model_nasbaasex_afirm <- glm(gampos ~ aspos,
               data = afirm_data,
               family = binomial(link = "logit"))

super.cluster.fun(model_nasbaasex_afirm, afirm_data$sampleid)

OR_nasbaasex_afirm<-as.data.frame(exp(coefs.confint <-
                               super.cluster.fun(model_nasbaasex_afirm, afirm_data$sampleid)[[3]]))


#Univariable analysis gam positivity and gam response
model_gamresponse_afirm <- glm(gampos ~ log10(gam_response),
              data = afirm_data,
              family = binomial(link = "logit"))

super.cluster.fun(model_gamresponse_afirm, afirm_data$sampleid)

OR_gamresponse_afirm <-as.data.frame(exp(coefs.confint <-
                               super.cluster.fun(model_gamresponse_afirm, afirm_data$sampleid)[[3]]))

#Univariable analysis gam positivity and ama1 response
model_ama1_afirm <- glm(gampos ~ log10(ama1_response),
                         data = afirm_data,
                         family = binomial(link = "logit"))

super.cluster.fun(model_ama1_afirm, afirm_data$sampleid)

OR_ama1_afirm <-as.data.frame(exp(coefs.confint <-
                                     super.cluster.fun(model_ama1_afirm, afirm_data$sampleid)[[3]]))


#Univariable analysis gam positivity and age_groups
model_agegroups_afirm <- glm(gampos ~ age_groups2,
                 data = afirm_data,
                 family = binomial(link = "logit"))

super.cluster.fun(model_agegroups_afirm, afirm_data$sampleid)

OR_agegroups_afirm <-as.data.frame(exp(coefs.confint <-
                             super.cluster.fun(model_agegroups_afirm, afirm_data$sampleid)[[3]]))


#Univariable analysis gam positivity and season
model_season_afirm <- glm(gampos ~ season,
                 data = afirm_data,
                 family = binomial(link = "logit"))

super.cluster.fun(model_season_afirm, afirm_data$sampleid)

OR_season_afirm <-as.data.frame(exp(coefs.confint <-
                             super.cluster.fun(model_season_afirm, afirm_data$sampleid)[[3]]))

#Multivariable Analysis

#Multivariable Analysis - without ama1 (with gam extract)
model_allminusama1_afirm <- glm(gampos ~  log10(gam_response) + aspos + season + age_groups2,
                          data = afirm_data,family = binomial(link = "logit"))

super.cluster.fun(model_allminusama1_afirm, afirm_data$sampleid)

OR_allminusama1_afirm <-as.data.frame(exp(coefs.confint <-
                                     super.cluster.fun(model_allminusama1_afirm, afirm_data$sampleid)[[3]]))

#Multivariable Analysis - with ama1 and gam extract 
model_allwithama1_afirm <- glm(gampos ~  log10(gam_response) + aspos + log10(ama1_response)+ season + age_groups2,
                          data = afirm_data,family = binomial(link = "logit"))

super.cluster.fun(model_allwithama1_afirm, afirm_data$sampleid)

OR_allwithama1_afirm <-as.data.frame(exp(coefs.confint <-
                                      super.cluster.fun(model_allwithama1_afirm, afirm_data$sampleid)[[3]]))

#Multivariable Analysis - with ama1 (no gam extract) 
model_allminusge_afirm <- glm(gampos ~  aspos + log10(ama1_response)+ season + age_groups,
                               data = afirm_data,family = binomial(link = "logit"))

super.cluster.fun(model_allminusge_afirm, afirm_data$sampleid)

OR_allminusge_afirm <-as.data.frame(exp(coefs.confint <-
                                           super.cluster.fun(model_allminusge_afirm, afirm_data$sampleid)[[3]]))


# ------------------------------------------------#
# Check predictive ability of the model: ROC curve#
# ------------------------------------------------#
require(pROC)
#Predictive ability with gam exract (minus ama1)
probsnoama1_afirm= predict(model_allminusama1_afirm, newdata = afirm_data, type = "response")
test_rocnoama1_afirm = roc(afirm_data$gampos ~ probsnoama1_afirm, plot = TRUE, legacy.axes=TRUE,ylab= "True positive rate",
                           xlab= "False positive rate",
                     ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocnoama1_afirm # save plot as pdf 5.5 x 5.5 inches


#Predictive ability with ama1 and ge
probswithama1_afirm= predict(model_allwithama1_afirm, newdata = afirm_data, type = "response")
test_rocwithama1_afirm = roc(afirm_data$gampos ~ probswithama1_afirm, plot = TRUE, legacy.axes=TRUE,ylab= "True positive rate",
                             xlab= "False positive rate", 
                     ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocwithama1_afirm # save plot as pdf 5.5 x 5.5 inches

#Predictive ability with ama1 only
probsminusge_afirm= predict(model_allminusge_afirm, newdata = afirm_data, type = "response")
test_rocminusge_afirm = roc(afirm_data$gampos ~ probsminusge_afirm, plot = TRUE, legacy.axes=TRUE,ylab= "True positive rate",
                             xlab= "False positive rate", 
                             ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocminusge_afirm # save plot as pdf 5.5 x 5.5 inches



#############################

#Analysis of microscopic gam positivity
# Univariable analysis

#Univariable analysis gam positivity and asexual parasitemia (microscopy)
model_microscopyasex <- glm(gammcl_pos ~ asexualmcl_pos,
                  data = afirm_data,
                  family = binomial(link = "logit"))

super.cluster.fun(model_microscopyasex, afirm_data$sampleid)

OR_microscopyasex <-as.data.frame(exp(coefs.confint <-
                              super.cluster.fun(model_microscopyasex, afirm_data$sampleid)[[3]]))


#Univariable analysis gam positivity and gam response
model_microscopygamresponse <- glm(gammcl_pos ~ log10(gam_response),
                         data = afirm_data,
                         family = binomial(link = "logit"))

super.cluster.fun(model_microscopygamresponse, afirm_data$sampleid)

OR_microscopygamresponse <-as.data.frame(exp(coefs.confint <-
                                     super.cluster.fun(model_microscopygamresponse, afirm_data$sampleid)[[3]]))

#Univariable analysis gam positivity and age_groups
model_microscopyagegroups <- glm(gammcl_pos ~ age_groups,
                       data = afirm_data,
                       family = binomial(link = "logit"))

super.cluster.fun(model_microscopyagegroups, afirm_data$sampleid)

OR_microscopyagegroups <-as.data.frame(exp(coefs.confint <-
                                   super.cluster.fun(model_microscopyagegroups, afirm_data$sampleid)[[3]]))


#Univariable analysis gam positivity and season
model_microscopyseason <- glm(gammcl_pos ~ season,
                    data = afirm_data,
                    family = binomial(link = "logit"))

super.cluster.fun(model_microscopyseason, afirm_data$sampleid)

OR_microscopyseason <-as.data.frame(exp(coefs.confint <-
                                super.cluster.fun(model_microscopyseason, afirm_data$sampleid)[[3]]))

#Multivariable Analysis

model_microscopyall <- glm(gammcl_pos ~  log10(gam_response) + asexualmcl_pos + season + age_groups,
                 data = afirm_data,
                 family = binomial(link = "logit"))

super.cluster.fun(model_microscopyall, afirm_data$sampleid)

OR_microscopyall<-as.data.frame(exp(coefs.confint <-
                            super.cluster.fun(model_microscopyall, afirm_data$sampleid)[[3]]))

