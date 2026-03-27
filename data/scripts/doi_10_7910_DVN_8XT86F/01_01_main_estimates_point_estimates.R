##############################################################################
#
#                             Replication scripts
#                       Main Estimates - Point Estimates
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note to speed up replication time:
# This script may be run concurrently with
# 01_02_heterogeneity_point_estimates
# 01_03_robustness_checks_point_estimates
# 01_04_heterogeneity_appendix_point_estimates
# 01_05_different_stages_point_estimates
# by using four additional instances of R

# load data
load("replication_data.RData")

# load libraries
library(econet); library(minpack.lm)

# preallocate list to store results
main_estimates <- list()

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

#-----------------------------------------------------------------------------
# Model with peer effects (not controlling for endogeneity)
#-----------------------------------------------------------------------------

main_estimates[[2]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                               model = "model_B", estimation = "NLLS", hypothesis = "lim", 
                               endogeneity = FALSE, to_weight = db$weights, 
                               time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model with peer effects (controlling for endogeneity)
#-----------------------------------------------------------------------------

main_estimates[[3]] <- net_dep(formula = ff, data = db,
                               G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                               hypothesis = "lim", endogeneity = TRUE, 
                               correction = "heckman", first_step = "standard", 
                               exclusion_restriction = alumni_network, 
                               to_weight = db$weights, time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model without peer effects
#-----------------------------------------------------------------------------

# Run model using OLS
ols <- lm(formula = ff, data = db, weights = db$weights)

# Store ols results to create starting values for NLLS estimation
starting <- econet:::toList(coef(ols))
names(starting) <- c("alpha", "gender1", "nowhite1", "party1", "seniority", "margin", "dw", "deleg_size", "nchair1", "maj_leader1", "min_leader1", "speaker1", "time2", "time3", "time4", "time5")
names(starting)[2:length(starting)] <- paste0("beta_", names(starting)[2:length(starting)])

# Run NLLS estimation
main_estimates[[1]] <- nlsLM(formula("les ~ alpha * Ones + beta_gender1 * gender1 + beta_nowhite1 * nowhite1 + beta_party1 * party1 + beta_seniority * seniority + beta_margin * margin + beta_dw * dw + beta_deleg_size * deleg_size + beta_nchair1 * nchair1 + beta_maj_leader1 * maj_leader1 + beta_min_leader1 * min_leader1 + beta_speaker1 * speaker1 + beta_time2 * time2 + beta_time3 * time3 + beta_time4 * time4 + beta_time5 * time5"), 
                             start = starting, data = main_estimates[[2]]$second_step$data, trace = T, weights = main_estimates[[2]]$second_step$data$to_weight,
                             control = nls.lm.control(ftol = sqrt(.Machine$double.eps)/1000, ptol = sqrt(.Machine$double.eps)/1000, gtol = 0, diag = list(), epsfcn = 0, factor = 100, maxfev = integer(), maxiter = 300, nprint = 0))

#-----------------------------------------------------------------------------
# Print results table 1
#-----------------------------------------------------------------------------

# print results column 1 table 1 
# Note: coefficients are multiplied by 1000 in the table of the paper
summary(main_estimates[[3]], print = "first.step")

#-----------------------------------------------------------------------------
# Print results table 2
#-----------------------------------------------------------------------------

# print results column 1 table 2
summary(main_estimates[[1]])

# print results column 2 table 2
summary(main_estimates[[2]])

# print point estimates column 3 table 2
summary(main_estimates[[3]])

#-----------------------------------------------------------------------------
# Tests
#-----------------------------------------------------------------------------

# Partial F-test
anova(main_estimates[[1]], main_estimates[[2]]$second_step)
anova(main_estimates[[2]]$second_step, main_estimates[[3]]$second_step)

# F-Test Weak Instrument 
# Run NLLS estimation
df <- main_estimates[[3]]$first_step$model
baseline <- lm(formula(y ~ 1), data = df)

rss2 <- sum(resid(main_estimates[[3]]$first_step)^2)
rss1 <- sum(resid(baseline)^2)
p2 <- length(coef(main_estimates[[3]]$first_step))
p1 <- length(coef(baseline))
n <- nobs(main_estimates[[3]]$first_step)

num <- (rss1 - rss2) / (p2 - p1)
den <- (rss2) / (n - p2)
num/den

anova(baseline, main_estimates[[3]]$first_step)

#-----------------------------------------------------------------------------
# Table A.1
#-----------------------------------------------------------------------------

qtf <- quantify(main_estimates[[3]])
rownames(qtf) <- rownames(summary(main_estimates[[3]]$second_step)$coefficient)[- c(1, 18)]
qtf

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(main_estimates, file = "main_estimates.rda")

#-----------------------------------------------------------------------------
# Notes
#-----------------------------------------------------------------------------

# Results from column 4 table 2 can be obtained using the file
# "02_01_main_estimates_school_fixed_effects_point_estimates"
