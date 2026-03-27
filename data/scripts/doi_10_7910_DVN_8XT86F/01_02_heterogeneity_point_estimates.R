##############################################################################
#
#                             Replication scripts
#                               Heterogeneity
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note to speed up replication time:
# This script may be run concurrently with
# 01_01_main_estimates_point_estimates
# 01_03_robustness_checks_point_estimates
# 01_04_heterogeneity_appendix_point_estimates
# 01_05_different_stages_point_estimates
# by using four additional instances of R

# load data
load("replication_data.RData")

# load libraries
library(econet)

# preallocate list to store results
het_estimates <- list()

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

#-----------------------------------------------------------------------------
# Heterogeneity by race
#-----------------------------------------------------------------------------

het_estimates[[1]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$nowhite)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[2]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$nowhite)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by seniority
#-----------------------------------------------------------------------------

het_estimates[[3]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$seniority)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[4]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$seniority)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by committee chair
#-----------------------------------------------------------------------------

het_estimates[[5]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$nchair)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[6]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$nchair)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(het_estimates, file = "heterogeneity_estimates.rda")
