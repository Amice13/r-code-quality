##############################################################################
#
#                             Replication scripts
#                           Heterogeneity - Appendix
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note to speed up replication time:
# This script may be run concurrently with
# 01_01_main_estimates_point_estimates
# 01_02_heterogeneity_point_estimates
# 01_03_robustness_checks_point_estimates
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
# Heterogeneity by gender
#-----------------------------------------------------------------------------

het_estimates[[1]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$gender)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[2]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$gender)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by party
#-----------------------------------------------------------------------------

het_estimates[[3]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$party)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[4]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = as.numeric(as.character(db$party)), 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by vote share
#-----------------------------------------------------------------------------

het_estimates[[5]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = db$margin, 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[6]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = db$margin, 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by distance to the median
#-----------------------------------------------------------------------------

het_estimates[[7]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = db$dw, 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[8]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_r", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = db$dw, 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Heterogeneity by size of congressional delegation
#-----------------------------------------------------------------------------

het_estimates[[9]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                              model = "model_B", estimation = "NLLS", 
                              hypothesis = "het_l", endogeneity = TRUE, 
                              correction = "heckman", 
                              z = db$deleg_size, 
                              exclusion_restriction = alumni_network, 
                              start.val = NULL, to_weight = db$weights, 
                              time_fixed_effect = "time")

het_estimates[[10]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                               model = "model_B", estimation = "NLLS", 
                               hypothesis = "het_r", endogeneity = TRUE, 
                               correction = "heckman", 
                               z = db$deleg_size, 
                               exclusion_restriction = alumni_network, 
                               start.val = NULL, to_weight = db$weights, 
                               time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Print point estimates table A.5
#-----------------------------------------------------------------------------

# print results column 1 table A.5
summary(het_estimates[[1]])

# print results column 2 table A.5
summary(het_estimates[[2]])

# print results column 3 table A.5
summary(het_estimates[[3]])

# print results column 4 table A.5
summary(het_estimates[[4]])

# print results column 5 table A.5
summary(het_estimates[[5]])

# print results column 6 table A.5
summary(het_estimates[[6]])

# print results column 7 table A.5
summary(het_estimates[[7]])

# print results column 8 table A.5
summary(het_estimates[[8]])

# print results column 9 table A.5
summary(het_estimates[[9]])

# print results column 10 table A.5
summary(het_estimates[[10]])

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(het_estimates, file = "het_estimates_appendix.rda")
