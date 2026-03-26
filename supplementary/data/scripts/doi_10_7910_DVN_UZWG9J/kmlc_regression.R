#Load data
kmlc_data <- read.csv("kmlc_data.csv")

#Definitions
#pf_mcl is asexual parasite denisty by microscopy (aspara is the binary variable from this)
#gam_mcl is gametocyte density by microscopy (gampos is the binary variable from this)

#Predicting gametocyte positivity 

#----Predicting gametocyte positivity----
#robust sandwich estimator
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

#Univariable analysis gam positivity and asexual parasitemia 
model_asexp_kmlc <- glm(gampos ~ aspara,
                  data = kmlc_data,
                  family = binomial(link = "logit"))

super.cluster.fun(model_asexp_kmlc, kmlc_data$studyno)

OR_asexp_kmlc <-as.data.frame(exp(coefs.confint <-
                              super.cluster.fun(model_asexp_kmlc, kmlc_data$studyno)[[3]]))

#Univariable analysis gam positivity and asexual parasitemia (continous)
model_asexp_cont <- glm(gampos ~ log10(pf_mcl+1),
                   data = kmlc_data,
                   family = binomial(link = "logit"))

super.cluster.fun(model_asexp_cont, kmlc_data$studyno)

OR_asexp_cont <-as.data.frame(exp(coefs.confint <-
                               super.cluster.fun(model_asexp_cont, kmlc_data$studyno)[[3]]))


#Univariable analysis gam positivity and gam response
model_gamresponse_kmlc <- glm(gampos ~ log10(ge.conc),
                   data = kmlc_data,
                   family = binomial(link = "logit"))

super.cluster.fun(model_gamresponse_kmlc, kmlc_data$studyno)

OR_gamresponse_kmlc <-as.data.frame(exp(coefs.confint <-
                               super.cluster.fun(model_gamresponse_kmlc, kmlc_data$studyno)[[3]]))

#Univariable analysis gam positivity and ama1 response
model_ama1_kmlc <- glm(gampos ~ log10(ama1.conc),
                      data = kmlc_data,
                      family = binomial(link = "logit"))

super.cluster.fun(model_ama1_kmlc, kmlc_data$studyno)

OR_ama1_kmlc <-as.data.frame(exp(coefs.confint <-
                                  super.cluster.fun(model_ama1_kmlc, kmlc_data$studyno)[[3]]))

#Univariable analysis gam positivity and age_group
model_agegroup_kmlc <- glm(gampos ~ age_grcut2,
                   data = kmlc_data,
                   family = binomial(link = "logit"))

super.cluster.fun(model_agegroup_kmlc, kmlc_data$studyno)

OR_agegroup_kmlc <-as.data.frame(exp(coefs.confint <-
                               super.cluster.fun(model_agegroup_kmlc, kmlc_data$studyno)[[3]]))

#Univariable analysis gam positivity and cohort sub groups
model_cohort_kmlc <- glm(gampos ~ cohort2,
                      data = kmlc_data,
                      family = binomial(link = "logit"))

super.cluster.fun(model_cohort_kmlc, kmlc_data$studyno)

OR_cohort_kmlc <-as.data.frame(exp(coefs.confint <-
                                  super.cluster.fun(model_cohort_kmlc, kmlc_data$studyno)[[3]]))


#Multivariable Analysis......./minus AMA1
model_allminusama1_kmlc <- glm(gampos ~  aspara + log10(ge.conc) + age_grcut2 + cohort2,
                 data = kmlc_data,
                 family = binomial(link = "logit"))

super.cluster.fun(model_allminusama1_kmlc, kmlc_data$studyno)

OR_allminusama1_kmlc <-as.data.frame(exp(coefs.confint <-
                             super.cluster.fun(model_allminusama1_kmlc, kmlc_data$studyno)[[3]]))

#Multivariable Analysis..../with AMA1 and gam extract

model_allwithama1_kmlc <- glm(gampos ~  aspara + log10(ge.conc) + log10(ama1.conc) + age_grcut2 + cohort2,
                      data = kmlc_data,
                      family = binomial(link = "logit"))

super.cluster.fun(model_allwithama1_kmlc, kmlc_data$studyno)

OR_allwithama1_kmlc <-as.data.frame(exp(coefs.confint <-
                                  super.cluster.fun(model_allwithama1_kmlc, kmlc_data$studyno)[[3]]))

#Multivariable Analysis..../with AMA1 minus gam extract

model_allminusge_kmlc <- glm(gampos ~  aspara + log10(ama1.conc) + age_grcut2 + cohort2,
                              data = kmlc_data,
                              family = binomial(link = "logit"))

super.cluster.fun(model_allminusge_kmlc, kmlc_data$studyno)

OR_allminusge_kmlc <-as.data.frame(exp(coefs.confint <-
                                          super.cluster.fun(model_allminusge_kmlc, kmlc_data$studyno)[[3]]))

#Multivariable Analysis..../without age.. age did not predict in univariate

model_allwithoutage_kmlc <- glm(gampos ~  aspara + log10(ge.conc) + log10(ama1.conc) + cohort2,
                              data = kmlc_data,
                              family = binomial(link = "logit"))

super.cluster.fun(model_allwithoutage_kmlc, kmlc_data$studyno)

OR_allwithoutage_kmlc <-as.data.frame(exp(coefs.confint <-
                                          super.cluster.fun(model_allwithoutage_kmlc, kmlc_data$studyno)[[3]]))


# ------------------------------------------------#
# Check predictive ability of the model: ROC curve#
# ------------------------------------------------#
require(pROC)

#Predictive ability minus ama1 (with gam extract)
probsnoama1_kmlc= predict(model_allminusama1_kmlc, newdata = kmlc_data, type = "response")
test_rocnoama1_kmlc = roc(kmlc_data$gampos ~ probsnoama1_kmlc, plot = TRUE,legacy.axes=TRUE,ylab= "True positive rate",
                     xlab= "False positive rate",
                     ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocnoama1_kmlc # save plot as pdf 5.5 x 5.5 inches

#test with ama1 and gam extract
probswithama1_kmlc= predict(model_allwithama1_kmlc, newdata = kmlc_data, type = "response")
test_rocwithama1_kmlc = roc(kmlc_data$gampos ~ probswithama1_kmlc, plot = TRUE, legacy.axes=TRUE,ylab= "True positive rate",
                            xlab= "False positive rate",
                     ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocwithama1_kmlc # save plot as pdf 5.5 x 5.5 inches

#test minus gam extract (with ama1)
probsminusge_kmlc= predict(model_allminusge_kmlc, newdata = kmlc_data, type = "response")
test_rocminusge_kmlc = roc(kmlc_data$gampos ~ probsminusge_kmlc, plot = TRUE, legacy.axes=TRUE,ylab= "True positive rate",
                            xlab= "False positive rate",
                            ci=TRUE, ci.alpha=0.9, print.auc = TRUE)
test_rocminusge_kmlc # save plot as pdf 5.5 x 5.5 inches


#Combine ROC plots
#Plots without ama1 (gam extract only) # Change colours
par(pty="s") #maintains square roc plot
plot.roc(test_rocnoama1_kmlc, add = FALSE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",  
         xlab= "False positive rate", col = "#000000", lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE,
         print.auc.x = 0.5, print.auc.y = 0.2) 

plot.roc(test_rocnoama1_afirm, add = TRUE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",
         xlab= "False positive rate",col = "#0000FF", lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE, 
         print.auc.x = 0.5,
         print.auc.y = 0.3) 

#Plots with ama1 (minus gam extract).....# save as pdf 20 x 7 ....magnify x75 screenshot
plot.roc(test_rocminusge_kmlc, add = TRUE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",
         xlab= "False positive rate", col ="#999999", lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE,
         print.auc.x = 0.5, print.auc.y = 0.1) 

plot.roc(test_rocminusge_afirm, add = TRUE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",
         xlab= "False positive rate",col = "#FF0000", lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE, 
         print.auc.x = 0.5,
         print.auc.y = 0.4) + legend(0, 0.7, title = "Multivariate prediction model", c("Molecular (AMA1)", "Molecular (Gam extract)",
                                                                                        "Microscopic (Gam extract)","Microscopic (AMA1)"), 
                                     col = c("#FF0000","#0000FF","#000000","#999999"), lty = c(1,1,1,1),
                                     lwd = c(2,2,2,2), 
                                     bty = "n", text.width = 0.45, xpd = TRUE, inset = c(0, 0),
                                     x.intersp = 0.5, y.intersp = 0.6) 

#Plots with ama1 and gam extract.....# save as pdf 14 x 5.5
par(pty="s") #maintains square roc plot
plot.roc(test_rocwithama1_kmlc, add = FALSE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",
         xlab= "False positive rate",col = 1, lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE,
         print.auc.x = 0.5, print.auc.y = 0.3) 
plot.roc(test_rocwithama1_afirm, add = TRUE, reuse.auc = TRUE, axes = TRUE, legacy.axes = TRUE,
         ylab= "True positive rate",
         xlab= "False positive rate",col = 2, lty = 1, ci=TRUE, ci.alpha=0.9, print.auc = TRUE, 
         print.auc.x = 0.5,
         print.auc.y = 0.4) + legend(0, 0.7, title = "Multivariate prediction model",
                                     c("Molecular (AMA1 and Gam extract)", "Microscopic (AMA1 and Gam extract)"), 
                                     col = c(2,1), lty = c(1,1),
                                     lwd = c(2,2), 
                                     bty = "n", text.width = 0.45, xpd = TRUE, inset = c(0, 0),
                                     x.intersp = 0.5, y.intersp = 0.6)
  