##############################################################################
#
#                             Replication scripts
#                              Robustness checks
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note to speed up replication time:
# This script may be run concurrently with
# 01_01_main_estimates_point_estimates
# 01_02_heterogeneity_point_estimates
# 01_04_heterogeneity_appendix_point_estimates
# 01_05_different_stages_point_estimates
# by using four additional instances of R

# load data
load("replication_data.RData")

# load libraries
library(econet)

# preallocate list to store results
robustness <- list()

# subset data for which state_leg_prof is available
keep <- which(!is.na(db$state_leg_prof))
sub_db <- db[keep, ]
sub_cosponsorship_network <- cosponsorship_network[keep, keep] 
sub_alumni_network <- alumni_network[keep, keep]

#-----------------------------------------------------------------------------
# Model without dw control
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

robustness[[1]] <- net_dep(formula = ff, data = db,
                           G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                           hypothesis = "lim", endogeneity = TRUE, 
                           correction = "heckman", first_step = "standard", 
                           exclusion_restriction = alumni_network, 
                           to_weight = db$weights,
                           time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model with committe fixed effects
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + 
              H.102 + H.104 + H.106 + H.113 + H.115 + H.124 + H.128 + 
              H.134 + H.138 +  H.142 + H.156 + H.164 + H.173 + H.176 + 
              H.182 + H.184 + H.186 + H.192 +  H.196 + H.242 + H.251 + time")

robustness[[2]] <- net_dep(formula = ff, data = db,
                           G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                           hypothesis = "lim", endogeneity = TRUE, 
                           correction = "heckman", first_step = "standard", 
                           exclusion_restriction = alumni_network, 
                           to_weight = db$weights,
                           time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model with legislative experience
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + 
              state_leg + state_leg_prof + time")

robustness[[3]] <- net_dep(formula = ff, data = sub_db,
                           G = sub_cosponsorship_network, model = "model_B", estimation = "NLLS",
                           hypothesis = "lim", endogeneity = TRUE, 
                           correction = "heckman", first_step = "standard", 
                           exclusion_restriction = sub_alumni_network, 
                           to_weight = sub_db$weights,
                           time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model with legislative experience and committee fixed effects
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + 
              state_leg + state_leg_prof + H.102 + H.104 + H.106 + 
              H.113 + H.115 + H.124 + H.128 + H.134 + H.138 +  H.142 + 
              H.156 + H.164 + H.173 + H.176 +  H.182 + H.184 + H.186 + 
              H.192 +  H.196 + H.242 + H.251 + time")

robustness[[4]] <- net_dep(formula = ff, data = sub_db,
                           G = sub_cosponsorship_network, model = "model_B", estimation = "NLLS",
                           hypothesis = "lim", endogeneity = TRUE, 
                           correction = "heckman", first_step = "standard", 
                           exclusion_restriction = sub_alumni_network, 
                           to_weight = sub_db$weights,
                           time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Model without weights
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

robustness[[5]] <- net_dep(formula = ff, data = db,
                           G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                           hypothesis = "lim", endogeneity = TRUE, 
                           correction = "heckman", first_step = "standard", 
                           exclusion_restriction = alumni_network, 
                           to_weight = NULL,
                           time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Print results table A.2
#-----------------------------------------------------------------------------

# print point estimates column 1 table A.2
summary(robustness[[1]])

# print point estimates column 2 table A.2
summary(robustness[[2]])

# print point estimates column 3 table A.2
summary(robustness[[3]])

# print point estimates column 4 table A.2
summary(robustness[[4]])

# print point estimates column 5 table A.2
summary(robustness[[5]])

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(robustness, file = "robustness.rda")
