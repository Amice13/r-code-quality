##############################################################################
#
#                             Replication scripts
#       Main Estimates including school fixed effects - Point Estimates
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note 1:
# This script should be run after completing
# 01_01_main_estimates_point_estimates

# Note 2
# To speed up replication time, this script may be run concurrently with
# 02_02_horse_race_point_estimates
# 02_03_party_effects_ties_effects_timing_point_estimates
# by using two additional instances of R

##############################################################################
# Warning: this part of the script must be run using RevoscaleR 
##############################################################################

# load packages
library(RevoScaleR)
library(dplyr)

# load dataset
load("replication_data_first_step_school_fe.rda")

# remove individual fixed effects
db_first_step_to_fit <- db_first_step[, - which(colnames(db_first_step) %in% c("cidx"))]

#-----------------------------------------------------------------------------
# Table 1 column 2
#-----------------------------------------------------------------------------

# set formula
formula_first_step <- formula(paste0(colnames(db_first_step_to_fit)[1], " ~ ",
                                     paste(colnames(db_first_step_to_fit)[-1], collapse = " + ")))

# run model
first_step <- rxLinMod(formula = formula_first_step, data = db_first_step, covCoef = TRUE)

# run baseline model
baseline <- rxLinMod(formula = formula("y ~ 1"), data = db_first_step, covCoef = TRUE)

# print results column 2 table 1
# Note: coefficients are multiplied by 1000 in the table of the paper
summary(first_step)

#-----------------------------------------------------------------------------
# F-Test Weak Instrument
#-----------------------------------------------------------------------------

# extract residuals (first step)
res_form <- rxPredict(first_step, data = db_first_step, computeResiduals = T)

# extract residuals (baseline)
res_form_baseline <- rxPredict(baseline, data = db_first_step, computeResiduals = T)

rss2 <- sum(res_form$y_Resid^2)
rss1 <- sum(res_form_baseline$y_Resid^2)
p2 <- length(coef(first_step))
p1 <- length(coef(baseline))
n <- nrow(db_first_step_to_fit)

# F-Test Weak Instrument (Table 2 column 4)
num <- (rss1 - rss2) / (p2 - p1)
den <- (rss2) / (n - p2)
num/den

#-----------------------------------------------------------------------------
# Create endogeneity correction
#-----------------------------------------------------------------------------

# cbind db_first_step and res_form
unobs <- data.frame(cidx = db_first_step$cidx,
                    unobservables = res_form$y_Resid)

# aggregate residuals by Congress member id
unobs <- unobs %>% group_by(db_first_step$cidx) %>%
  summarize(unobservables = sum(unobservables))

# order unobs by id
unobs <- data.frame(unobs)
colnames(unobs)[1] <- "id_1"

# save results and close RevoscaleR
save(unobs, file = "unobservables_school_fe.rda")

###########################################################################################################
# Switch now to R
###########################################################################################################

# load data
load("replication_data.RData")
load("unobservables_school_fe.rda")

# load packages
library(plyr)
library(econet)

#-----------------------------------------------------------------------------
# Model with peer effects (controlling endogeneity)
#-----------------------------------------------------------------------------

db <- plyr::join(db, unobs)

# load dataset
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + unobservables + time")

fit <- net_dep(formula = ff, data = db,
               G = cosponsorship_network, model = "model_B", estimation = "NLLS",
               hypothesis = "lim", endogeneity = FALSE, 
               to_weight = db$weights,
               time_fixed_effect = "time")

# print results: table 2 column 4
summary(fit)

# Partial F-test
load("main_estimates.rda")
anova(main_estimates[[3]]$second_step, fit[[1]])

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(fit, file = "main_estimates_school_fe.rda")
