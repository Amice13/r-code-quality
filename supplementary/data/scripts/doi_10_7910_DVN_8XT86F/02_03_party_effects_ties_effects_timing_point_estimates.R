##############################################################################
#
#                             Replication scripts
#                             Table A6 - Appendix
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
# 02_01_main_estimates_school_fixed_effects_point_estimates
# 02_02_horse_race_point_estimates
# by using two additional instances of R

# load data
load("replication_data.RData")
load("main_estimates.rda")
load("network_strong_weak_ties.rda")
load("replication_data_first_step_united.rda")

# load libraries
library(econet); library(minpack.lm); library(aod); library(plyr); library(dplyr)

# preallocate list of results
table_a6 <- list()

#-----------------------------------------------------------------------------
# Party effects
#-----------------------------------------------------------------------------

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + time")

table_a6[[1]] <- net_dep(formula = ff, data = db, G = cosponsorship_network, 
                         model = "model_B", estimation = "NLLS", 
                         hypothesis = "par", endogeneity = TRUE, 
                         correction = "heckman", 
                         z = as.numeric(as.character(db$party)), 
                         exclusion_restriction = alumni_network, 
                         start.val = NULL, to_weight = db$weights, 
                         time_fixed_effect = "time")

#-----------------------------------------------------------------------------
# Weak vs Strong Ties (1)
#-----------------------------------------------------------------------------

# prepare data
variables <- c("les", "gender", "nowhite", "party", "seniority", "margin", "dw", "deleg_size", "nchair", "maj_leader", "min_leader", "speaker", "time")
db_reg <- db[, variables]
data_list <- econet:::prepare_data(ff, db_reg, "time")
e <- data_list[["data"]]
e$les <- data_list[["y"]]
e <- econet:::toList(e)
n <- nrow(cosponsorship_network)
I <- diag(n)
e[["I"]] <- I
e[["Ones"]] <- rep(1, n)
e[["G_within"]] <- as.matrix(G_strong_sym)
e[["G_between"]] <- as.matrix(G_weak_sym)
e[["unobservables"]] <- main_estimates[[3]]$second_step$data$unobservables

# load econet:::solve_block to invert the matrix during estimation
solve_block <- econet:::solve_block

ws_f <- formula("les ~ solve_block(I - phi_within * G_within - phi_between * G_between) %*% (alpha * Ones + beta_gender * gender1 + beta_nowhite * nowhite1 + beta_party * party1 + beta_seniority * seniority + beta_margin * margin + beta_dw * dw + beta_deleg_size * deleg_size + beta_nchair * nchair1 + beta_maj_leader * maj_leader1 + beta_min_leader * min_leader1 + beta_speaker * speaker1 + beta_unobservables * unobservables + beta_time_2 * time2 + beta_time_3 * time3 + beta_time_4 * time4 + beta_time_5 * time5)")
ws_s <- list(phi_within = 0.831122388116603, phi_between = 0.834616813013307, 
             alpha = 0.253481154172082, beta_gender = -0.0303340505596922, 
             beta_nowhite = -0.108814881855557, beta_party = -0.117284350982968, 
             beta_seniority = 0.0228003472045303, beta_margin = -0.00130964183952466, 
             beta_dw = -0.320417270262815, beta_deleg_size = -0.00340246138882683, 
             beta_nchair = 1.61116767845225, beta_maj_leader = -0.0482239161691091, 
             beta_min_leader = -0.293675209903377, beta_speaker = -1.25933940469175, 
             beta_time_2 = 0.00824685670493507, beta_time_3 = -0.000848975038485562, 
             beta_time_4 = 0.00495982633573841, beta_time_5 = 0.0204644462012931, 
             beta_unobservables = -0.412530418901113)
set.control <- nls.lm.control(ftol = sqrt(.Machine$double.eps)/1000, ptol = sqrt(.Machine$double.eps)/1000, gtol = 0,
                              diag = list(), epsfcn = 0, factor = 100, maxfev = integer(),
                              maxiter = 300, nprint = 0)

# run model
table_a6[[2]] <- nlsLM(ws_f, start = ws_s, data = e, trace = T, weights = db$weights, control = set.control)

# test statistical difference between weak and strong ties effects
wald.test(b = coef(table_a6[[2]]), Sigma = vcov(table_a6[[2]]), Terms = 1:2)

# transform table_a6[[2]] into an econet object
table_a6[[2]] <- list(second_step = table_a6[[2]], centrality = list(), 
             first_step = list(), hypothesis = "par", 
             starting.values = econet:::toList(coef(table_a6[[2]])))
class(table_a6[[2]]) <- "econet"
table_a6[[2]]$second_step$data <- e
table_a6[[2]]$second_step$to_weight <- db$weights
colnames(table_a6[[2]]$second_step$data$G_within) <- 
    rownames(table_a6[[2]]$second_step$data$G_within) <- 
    rownames(cosponsorship_network)
colnames(table_a6[[2]]$second_step$data$G_between) <- 
    rownames(table_a6[[2]]$second_step$data$G_between) <- 
    rownames(cosponsorship_network)
names(table_a6[[2]]$second_step$data$les) <- 
    rownames(cosponsorship_network)
attributes(table_a6[[2]])$hypothesis <- "par"
attributes(table_a6[[2]])$model <- "model_B"

#-----------------------------------------------------------------------------
# Weak vs Strong Ties (2)
#-----------------------------------------------------------------------------

# prepare data
e[["G_within"]] <- as.matrix(G_strong_shared)
e[["G_between"]] <- as.matrix(G_weak_shared)

ws_f <- formula("les ~ solve_block(I - phi_within * G_within - phi_between * G_between) %*% (alpha * Ones + beta_gender * gender1 + beta_nowhite * nowhite1 + beta_party * party1 + beta_seniority * seniority + beta_margin * margin + beta_dw * dw + beta_deleg_size * deleg_size + beta_nchair * nchair1 + beta_maj_leader * maj_leader1 + beta_min_leader * min_leader1 + beta_speaker * speaker1 + beta_unobservables * unobservables + beta_time_2 * time2 + beta_time_3 * time3 + beta_time_4 * time4 + beta_time_5 * time5)")
ws_s <- list(phi_within = 0.743396261064656, phi_between = 0.919612821022007, 
             alpha = 0.20321650778652, beta_gender = -0.0286595115760386, 
             beta_nowhite = -0.109825312140994, beta_party = -0.0988753774019842, 
             beta_seniority = 0.0229943193214714, beta_margin = 0.00379736229094391, 
             beta_dw = -0.31971742660807, beta_deleg_size = -0.00345524151511289, 
             beta_nchair = 1.60783594578883, beta_maj_leader = -0.0424677783930278, 
             beta_min_leader = -0.295464046661812, beta_speaker = -1.23687144564153, 
             beta_time_2 = 0.0147374130280857, beta_time_3 = 0.00558811369392844, 
             beta_time_4 = 0.0149831967243928, beta_time_5 = 0.0300351847618499, 
             beta_unobservables = -0.400470711889827)
set.control <- nls.lm.control(ftol = sqrt(.Machine$double.eps)/1000, ptol = sqrt(.Machine$double.eps)/1000, gtol = 0,
                              diag = list(), epsfcn = 0, factor = 100, maxfev = integer(),
                              maxiter = 300, nprint = 0)

# run model
table_a6[[3]] <- nlsLM(ws_f, start = ws_s, data = e, trace = T, weights = db$weights, control = set.control)

# test statistical difference between weak and strong ties effects
wald.test(b = coef(table_a6[[3]]), Sigma = vcov(table_a6[[3]]), Terms = 1:2)

# transform table_a6[[3]] into an econet object
table_a6[[3]] <- list(second_step = table_a6[[3]], centrality = list(), 
                      first_step = list(), hypothesis = "par", 
                      starting.values = econet:::toList(coef(table_a6[[3]])))
class(table_a6[[3]]) <- "econet"
table_a6[[3]]$second_step$data <- e
table_a6[[3]]$second_step$to_weight <- db$weights
colnames(table_a6[[3]]$second_step$data$G_within) <- 
    rownames(table_a6[[3]]$second_step$data$G_within) <- 
    rownames(cosponsorship_network)
colnames(table_a6[[3]]$second_step$data$G_between) <- 
    rownames(table_a6[[3]]$second_step$data$G_between) <- 
    rownames(cosponsorship_network)
names(table_a6[[3]]$second_step$data$les) <- 
    rownames(cosponsorship_network)
attributes(table_a6[[3]])$hypothesis <- "par"
attributes(table_a6[[3]])$model <- "model_B"

#-----------------------------------------------------------------------------
# Timing: interaction
#-----------------------------------------------------------------------------

# First step
# create formula
formula_first_step <- "y ~ exclusion_restriction + gender1 + nowhite1 + party1 + seniority + 
margin + dw + deleg_size + nchair1 + united1"

# run first step
first_step <- lm(formula = formula_first_step, data = db_first_step)

# cbind db_first_step and res_form
unobs <- data.frame(cidx = db_first_step$cidx,
                    unobservables = residuals(first_step))

# aggregate residuals by Congress member id
unobs <- unobs %>% group_by(as.character(cidx)) %>%
    summarize(unobservables = sum(unobservables))

# order unobs by id
unobs <- data.frame(unobs)
colnames(unobs)[1] <- "id_1"
unobs$id_1 <- as.numeric(unobs$id_1)
unobs <- plyr::join(db[, "id_1", drop = F], unobs)

# Second step
db$unobservables <- unobs$unobservables
united <- rep(1, nrow(db))
united[which(db$time %in% c(1, 3))] <- 0
db$united <- as.factor(united)

# create formula
ff <- formula("les ~ gender + nowhite + party + seniority + margin + dw + 
              deleg_size + nchair + maj_leader + min_leader + speaker + 
              united + unobservables")

# run model  
table_a6[[4]] <- net_dep(formula = ff, data = db,
                         G = cosponsorship_network, model = "model_B", estimation = "NLLS",
                         hypothesis = "het_l", endogeneity = FALSE,
                         z = as.numeric(as.character(db$united)),
                         to_weight = db$weights)

#-----------------------------------------------------------------------------
# Print point estimates table A.6
#-----------------------------------------------------------------------------

# print results column 1 table A.6
summary(table_a6[[1]])

# print results column 2 table A.6
summary(table_a6[[2]])

# print results column 3 table A.6
summary(table_a6[[3]])

# print results column 4 table A.6
summary(table_a6[[4]])

#-----------------------------------------------------------------------------
# Store results
#-----------------------------------------------------------------------------

save(table_a6, file = "table_a6.rda")
