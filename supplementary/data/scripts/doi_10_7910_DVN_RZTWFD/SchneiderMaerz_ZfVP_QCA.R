########################################################
### Script to Schneider and Maerz 2017:
### Legitimation, Cooptation, and Repression 
### and the Survival of Electoral Autocracies,
### Zeitschrift fuer Vergleichende Politikwissenschaft, 2017 (forthoming)       
########################################################


## The R code in this file starts with the calibrated data (anchors see Table 2 in our article)
## and conducts fuzzy-set QCA for the outcome electoral defeat and no electoral defeat,
## including ESA (Schneider and Wagemenn 2012, Chapter 8.2), robustness tests and theory evaluation
## (for robustness tests concerning the calibration, see the other R script)

## For replication purposes please note that the analysis uses the raw data of:
## Lueders, Hans and Aurel Croissant. 2014. 
## Wahlen, Strategien autokratischer Herrschaftssicherung 
## und das ?berleben autokratischer Regierungen. 
## Zeitschrift f?r vergleichende Politikwissenschaft, 8 (3-4): 329-355

## The R code was written by using the packages QCA 2.5 and SetMethods 2.0


rm(list = ls())

# Set working directory: 
setwd()


# load packages
library(QCA); library(SetMethods); library(lattice); library(foreign);
library(arm); library(plyr); library(car); library(stringr); library(xtable); 
library(stargazer); library(venn)

# Load calibrated data:
load("calibrated_data_LuedersCroissant.Rda")


#################################
### Outcome: electoral defeat ###
#################################


### Analysis of necessity ###
#############################

QCAfit(data5[, 3:8], data5$defeat, necessity = TRUE, names(data5[, 3:8]))
QCAfit(1-data5[, 3:8], data5$defeat, necessity = TRUE, paste("not", names(data5[, 3:8])))
# no necessary condition

superSubset(data5, outcome = "defeat",
            conditions = c("leg_spec","leg_diff","coop_form", 
                           "coop_inf","rep_h","rep_s"),
            incl.cut = 0.91, cov.cut = 0.7,
            necessity = TRUE)
# no meaningful logical OR connection necessary for Y



### Analysis of sufficiency ###
###############################

# truth Table
TT_y <- truthTable(data5, outcome = "defeat",
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.8,
                   n.cut = 2,
                   show.cases = TRUE,
                   sort.by = c("OUT", "incl", "n"))

TT_y
# there is one row with consistency higher than .8
# yet, only one row (3 cases) are covered, no matter if ncut = 2 or ncut = 1
# this shows that the WZB model cannot explain why autocrats lose elections
# it is only good for explaining why they do not lose elections (see negation)

# plot truth table row which passes consist. threshold against the outcome
# in order to see if the cases covered are just-so typical or strong members
# first add column to data that contains each case's membership in the tt wo
data5$tt_row <- pmin(data5$leg_spec, 1-data5$leg_diff, data5$coop_form, 1-data5$coop_inf, 1-data5$rep_h, 1-data5$rep_s)

# then plot that row against the outcome
xy.plot(data5$tt_row, data5$defeat,
        pch = 19,
        main = "XY Plot of Truth Table Row, Defeat of Autocrats",
        ylab = "Membership in Defeat of Autocrats", 
        xlab = "Membership in Truth Table Row", 
        case.lab = TRUE, labs = rownames(data5), srt=40)

# finding: HU_90 has high (>.9) and MDG_93 almost .7 membership in the tt row

data5$tt_row <- NULL



# For the sake of it: logical minimization
# conservative solution
sol_y_c <- eqmcc(data5, outcome = "defeat",
                 conditions = c("leg_spec","leg_diff","coop_form", 
                                "coop_inf","rep_h","rep_s"),
                 incl.cut1 = 0.8,
                 n.cut = 2,
                 show.cases = TRUE,
                 details = TRUE)
sol_y_c

# only 3 cases are covered 
# this further supports that the model 
# can explain only why autocrats do NOT lose elections


# most parsimonious solution
sol_y_p <- eqmcc(data5, outcome = "defeat",
                 conditions = c("leg_spec","leg_diff","coop_form", 
                                "coop_inf","rep_h","rep_s"),
                 incl.cut1 = 0.8,
                 n.cut = 2,
                 show.cases = TRUE,
                 details = TRUE,
                 include = "?", 
                 min.dis = FALSE, row.dom = FALSE)
sol_y_p

# simplifying assumptions
sol_y_p$SA

# intermediate solution
sol_y_i <- eqmcc(data5, outcome = "defeat",
                 conditions = c("leg_spec","leg_diff","coop_form", 
                                "coop_inf","rep_h","rep_s"),
                 incl.cut1 = 0.8,
                 n.cut = 2,
                 show.cases = TRUE,
                 details = TRUE,
                 include = "?",
                 min.dis = FALSE,
                 row.dom = FALSE,
                 dir.exp = c("-","-","-","-",1,"-"))
sol_y_i
# dir. ex.: high repression can backfire (Gerschewski 2013)

# easy counterfactuals
sol_y_i$i.sol$C1P1$EC
# no easy counterfactuals, intermediate = conservative solution


### preliminary finding: WZB model does not explain why autocrats lose elections 
### let's now see if WZB model explains why autocrats do not loose elections:




################################################
### Negation of outcome: NO electoral defeat ###
################################################

### Analysis of necessity ###
#############################

QCAfit(data5[, 3:8], data5$defeat, necessity = TRUE, names(data5[, 3:8]), negation = TRUE)
# leg_diff has consist. 0.96, yet low RoN, thus not a nec.cond.
# coop_form looks potentially good (RoN 0.417)

# plot coop_form with NO defeat (for this, flip outcome)
q <- data5$defeat
data5$nodefeat <- (q==0)*1
xy.plot(data5$coop_form, data5$nodefeat, necessity = TRUE, 
        case.lab = TRUE, labs = rownames(data5), srt=40)

# observation: 
# 7 deviant cases consistency


QCAfit(1-data5[, 3:8], data5$defeat, necessity = TRUE, paste("not", names(data5[, 3:8])), negation = TRUE)
# no necessary condition

nec_ny <- superSubset(data5, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny
# several expressions pass the consistency and coverage test.
# however, except for COOP_FORM, all show too low RoN values

# based on the analysis, we could declare coop_form as nec. cond.
# YET, caution/more evidence needed due to operationalization, as noted in the text!



### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data5, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2, the formulas with n.cut = 1 have one more term,
# but are not more meaningful

# NOTE: robustness tests below check cons=0.9 and cons=0.74 and n.cut= 1



# logical minimization
# conservative
sol_ny_c <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c


# parsimonious
sol_ny_p <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE, include = "?", 
                  min.dis = FALSE, row.dom = FALSE)
sol_ny_p
# 2 models


# simplifying assumptions
sol_ny_p$SA

# intermediate solution
sol_ny_i <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE, include = "?", 
                  min.dis = FALSE, row.dom = FALSE, dir.exp = c(1,"-",1,"-","-","-"))
sol_ny_i
# directional expectation COOP_FORM and LEG_SPEC -> no electoral defeat

# easy counterfactuals
sol_ny_i$i.sol$C1P1$EC
# no easy counterfactuals, intermediate = conservative



##################################
### Enhanced Standard Analysis ###
##################################

# CHECK: any contradictory assumptions?
# idea: none of the SAs should simultaniously 
# assume sufficiency for y and -y 

CSA_p <- intersect(rownames(sol_y_p$SA$M1), rownames(sol_ny_p$SA$M1))
CSA_p

CSA_i <- intersect(rownames(sol_y_i$i.sol$C1P1$EC), rownames(sol_ny_i$i.sol$C1P1$EC))
CSA_i

# for the parsimonious, remainder rows 5, 13, 37 and 45 are included in both Y and ~Y
# lets exclude these rows for the parsim.
TT_nynew <- TT_ny
TT_nynew$tt[CSA_p, 'OUT'] <- 0 

TT_nynew

# show parsim.
sol_ny_pe <- eqmcc(TT_nynew, outcome = "defeat", neg.out = TRUE, details = TRUE, include = "?", 
                  min.dis = FALSE, row.dom = FALSE, show.cases = FALSE)
sol_ny_pe
# the enhanced parsim. yields model ambiguity

# simplifying assumptions
sol_ny_pe$SA

# intermediate solution
sol_ny_i <- eqmcc(TT_nynew, outcome = "defeat", neg.out = TRUE, details = TRUE, include = "?", 
                  min.dis = FALSE, row.dom = FALSE, dir.exp = c(1,"-",1,"-","-","-"), 
                  show.cases = FALSE)
sol_ny_i

# easy counterfactuals
sol_ny_i$i.sol$C1P1$EC
# no easy counterfactuals
# intermediate is still the same as conservative

# we take sol_ny_i for interpretation




########################
### Robustness tests ###
########################


### 1.) sensitivity tests: change n.cut, consistency thresholds ###
### 2.) Double check and test alternative calibrations (in separate script) ###


# 1a.) how would the results change when a 
# slightly different, but equally plausible consistency threshold is applied?
# (gaps also after 0.9 and 0.74)

# alternative truth table with cons. threshold = 0.9
TT_nya <- truthTable(data5, outcome = "defeat", neg.out = TRUE,
                     conditions = c("leg_spec","leg_diff","coop_form", 
                                    "coop_inf","rep_h","rep_s"),
                     incl.cut1 = 0.9,
                     n.cut = 2,
                     show.cases = TRUE,
                     sort.by = c("OUT", "incl", "n"))

TT_nya

# alternative truth table with cons. threshold = 0.74
TT_nyb <- truthTable(data5, outcome = "defeat", neg.out = TRUE,
                     conditions = c("leg_spec","leg_diff","coop_form", 
                                    "coop_inf","rep_h","rep_s"),
                     incl.cut1 = 0.74,
                     n.cut = 2,
                     show.cases = TRUE,
                     sort.by = c("OUT", "incl", "n"))

TT_nyb


# now logical minimization with TT_nya 

# conservative
sol_nya_c <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.9,
                   n.cut = 2,
                   details = TRUE, show.cases = FALSE)
sol_nya_c
# low coverage

# parsimonious
sol_nya_p <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.9,
                   n.cut = 2,
                   details = TRUE, include = "?", 
                   min.dis = FALSE, row.dom = FALSE, 
                   show.cases = FALSE)
sol_nya_p
# no model ambiguity, but also low coverage

# simplifying assumptions
sol_nya_p$SA

# intermediate solution
sol_nya_i <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.9, 
                   n.cut = 2,
                   details = TRUE, include = "?", 
                   min.dis = FALSE, row.dom = FALSE, 
                   dir.exp = c(1,"-",1,"-", "-","-"), show.cases = FALSE)
sol_nya_i

# easy counterfactuals
sol_nya_i$i.sol$C1P1$EC
# intermediate = conservative

# higher consistency, yet, much lower coverage
# only three terms, one with higher unique coverage than in other models
# BUT, solution coverage is too low



# also, logical minimization with
# TT_nyb 

# conservative
sol_nyb_c <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.74,
                   n.cut = 2,
                   show.cases = FALSE,
                   details = TRUE)
sol_nyb_c
# 3 models, all yield highly complex solution formulas

# parsimonious
sol_nyb_p <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.74,
                   n.cut = 2,
                   show.cases = FALSE, 
                   details = TRUE, include = "?", 
                   min.dis = FALSE, row.dom = FALSE)
sol_nyb_p
# model ambiguity, 4 models

# simplifying assumptions
sol_nyb_p$SA

# intermediate solution
sol_nyb_i <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.74,
                   n.cut = 2,
                   show.cases = FALSE, 
                   details = TRUE, include = "?", 
                   min.dis = FALSE, row.dom = FALSE, 
                   dir.exp = c(1,"-",1,"-", "-","-"))
sol_nyb_i
# model ambiguity, 3 models

# easy counterfactuals
sol_nyb_i$i.sol$C1P1$EC
# conservative is intermediate


### Conclusions regarding alternative consist. thresholds: ###
# threshold at 0.9 yields low coverage solution terms
# threshold at 0.74 yields model ambiguity in all 3 solution formulas
# we therefore keep threshold at 0.8



# 1b.) how would the results change when if n.cut = 1?

# alternative truth table with ncut = 1
TT_nyn <- truthTable(data5, outcome = "defeat", neg.out = TRUE,
                     conditions = c("leg_spec","leg_diff","coop_form", 
                                    "coop_inf","rep_h","rep_s"),
                     incl.cut1 = 0.8,
                     ncut=1,
                     show.cases = TRUE,
                     complete= FALSE,
                     sort.by = c("OUT", "incl", "n"))

TT_nyn

# now logical minimization with TT_nya 

# conservative
sol_nyn_c <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.8,
                   ncut=1,
                   show.cases = FALSE,
                   details = TRUE)
sol_nyn_c
# low coverage!

# parsimonious
sol_nyn_p <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.8,
                   ncut=1,
                   show.cases = TRUE,
                   details = TRUE,
                   include = "?", 
                   min.dis = FALSE, row.dom = FALSE)
sol_nyn_p
# model ambiguity, 3 models

# simplifying assumptions
sol_nyn_p$SA

# intermediate solution
sol_nyn_i <- eqmcc(data5, outcome = "defeat", neg.out = TRUE,
                   conditions = c("leg_spec","leg_diff","coop_form", 
                                  "coop_inf","rep_h","rep_s"),
                   incl.cut1 = 0.8,
                   ncut=1,
                   show.cases = TRUE,
                   details = TRUE,
                   include = "?", 
                   min.dis = FALSE, row.dom = FALSE, 
                   dir.exp = c(1,"-",1,"-", "-","-"))
sol_nyn_i

# easy counterfactuals
sol_nyn_i$i.sol$C1P1$EC

### summary of results with ncut=1: ###
# no significant change because only one row (row 48) is excluded if ncut = 2
# yet, the solution formulas are less complex 
# hence, the formulas with ncut = 1 have one more term, but are not more meaningful.




#########################
### theory evaluation ###
#########################


#	the logic of the argument is this
# 	a) 	where do the theory (T) and empirical QCA findings (E) (not) overlap
# 		(T*E; ~T*E; T*~E; ~T*~E)
# 	b) 	which cases are in these four intersections?
# 	c) 	what are the parameters of fit of these four intersections


# theory is here the "two worlds of autocracies" by Gerschewski (2013)
# formulate it in boolean expression 
my_theory <- "LEG_DIFF*COOP_FORM*REP_S*REP_H+LEG_SPEC*COOP_INF*REP_S"


### outcome: NO electoral defeat (-Y) ###
#########################################

#	a) create boolean expressions for intersections and display each case's membership in the four intersections
TH <- theory.evaluation(theory = my_theory, empirics = sol_ny_i, outcome = "DEFEAT", intermed = T)
TH

# b) now list, for each intersection, the cases and separate those that are members of Y from those with ~Y
TH_cases <- cases.theory.evaluation(TH)
TH_cases

# c) display the parameters of fit for the four intersections
TH_fit <- theory.fit(TH)
TH_fit


# for the sake of it, also for outcome "electoral defeat" (Y):

### outcome: electoral defeat (Y) ###
#####################################


#	a) create boolean expressions for intersections and display each case's membership in the four intersections
TH_y <- theory.evaluation(theory = my_theory, empirics = sol_y_i, outcome = "DEFEAT", intermed = T)
TH_y

# b) now list, for each intersection, the cases and separate those that are members of Y from those with ~Y
cases.theory.evaluation(TH_y)

# c) display the parameters of fit for the four intersections
theory.fit(TH_y)

# end of script.
