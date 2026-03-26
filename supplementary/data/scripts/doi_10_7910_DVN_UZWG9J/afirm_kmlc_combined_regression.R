#Predicting gametocyte positivity in the combined cohort analysis 

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

#Combined regression model (AFIRM and KMLC)
#Univariable - run for each variable tested
#Univariable analysis gam positivity (gammcl_pos) and asexual positivity (asexualmcl_pos)
model_microscopyasex_combined <- glm(gammcl_pos ~ asexualmcl_pos,
                                     data = afirm_kmlc,
                                     family = binomial(link = "logit"))

super.cluster.fun(model_microscopyasex_combined, afirm_kmlc$sampleID)

OR_microscopyasex_combined <-as.data.frame(exp(coefs.confint <-
                                                 super.cluster.fun(model_microscopyasex_combined, afirm_kmlc$sampleID)[[3]]))


#Univariable analysis gam positivity and gam response
model_microscopygamresponse_combined <- glm(gammcl_pos ~ log10(gam_response),
                                            data = afirm_kmlc,
                                            family = binomial(link = "logit"))

super.cluster.fun(model_microscopygamresponse_combined, afirm_kmlc$sampleID)

OR_microscopygamresponse_combined <-as.data.frame(exp(coefs.confint <-
                                                        super.cluster.fun(model_microscopygamresponse_combined, afirm_kmlc$sampleID)[[3]]))

#Univariable analysis gam positivity and ama1 response
model_microscopyama1_combined <- glm(gammcl_pos ~ log10(ama1_response),
                                     data = afirm_kmlc,
                                     family = binomial(link = "logit"))

super.cluster.fun(model_microscopyama1_combined, afirm_kmlc$sampleID)

OR_microscopyama1_combined <-as.data.frame(exp(coefs.confint <-
                                                 super.cluster.fun(model_microscopyama1_combined, afirm_kmlc$sampleID)[[3]]))


#Univariable analysis gam positivity and age_groups
model_microscopyagegroups_combined <- glm(gammcl_pos ~ age_groups,
                                          data = afirm_kmlc,
                                          family = binomial(link = "logit"))

super.cluster.fun(model_microscopyagegroups_combined, afirm_kmlc$sampleID)

OR_microscopyagegroups_combined <-as.data.frame(exp(coefs.confint <-
                                                      super.cluster.fun(model_microscopyagegroups_combined, afirm_kmlc$sampleID)[[3]]))


#Multivariable Analysis
#Multivariable Analysis - with gam extract and ama1
model_allmicroscopy_combined <- glm(gammcl_pos ~  log10(gam_response) +  log10(ama1_response) + asexualmcl_pos + age_groups,
                                    data = afirm_kmlc,family = binomial(link = "logit"))

super.cluster.fun(model_allmicroscopy_combined, afirm_kmlc$sampleID)

OR_allmicroscopy_combined <-as.data.frame(exp(coefs.confint <-
                                                super.cluster.fun(model_allmicroscopy_combined, afirm_kmlc$sampleID)[[3]]))

#save data
write.csv(afirm_kmlc,
          file = "afirm_kmlc2.csv",
          row.names = FALSE)