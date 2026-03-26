##############################################################################
#
#                             Replication scripts
#                           Table A7 - Appendix
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
# 01_04_heterogeneity_appendix_point_estimates
# by using four additional instances of R

# load data
load("replication_data.RData")

# load libraries
library(econet)

# preallocate list of results
table_a7 <- list()

#-----------------------------------------------------------------------------
# LES: considering only BILL
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les_bill ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

table_a7[[1]] <- net_dep(formula = ff, data = db,
                         G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                         hypothesis = "lim", endogeneity = TRUE, 
                         correction = "heckman", first_step = "standard", 
                         exclusion_restriction = alumni_network, 
                         to_weight = db$weights,
                         time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# LES: considering BILL + AIC
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les_bill_aic ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

table_a7[[2]] <- net_dep(formula = ff, data = db,
                         G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                         hypothesis = "lim", endogeneity = TRUE, 
                         correction = "heckman", first_step = "standard", 
                         exclusion_restriction = alumni_network, 
                         to_weight = db$weights,
                         time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# LES: considering ABC + PASS + LAW
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les_3 ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

table_a7[[3]] <- net_dep(formula = ff, data = db,
                         G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                         hypothesis = "lim", endogeneity = TRUE, 
                         correction = "heckman", first_step = "standard", 
                         exclusion_restriction = alumni_network, 
                         to_weight = db$weights,
                         time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Print point estimates table A.7
#-----------------------------------------------------------------------------

# print results column 1 table A.7
summary(table_a7[[1]])

# print results column 2 table A.7
summary(table_a7[[2]])

# print results column 3 table A.7
summary(table_a7[[3]])

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(table_a7, file = "table_a7.rda")
