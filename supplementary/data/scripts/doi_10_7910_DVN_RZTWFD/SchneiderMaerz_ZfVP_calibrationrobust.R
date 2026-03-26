########################################################
### Script to Schneider and Maerz 2017:
### Legitimation, Cooptation, and Repression 
### and the Survival of Electoral Autocracies,
### Zeitschrift fuer Vergleichende Politikwissenschaft, 2017 (forthoming)       
########################################################


## The R code in this file tests alternative anchors for calibration

## For replication purposes please note that the analysis uses the raw data of:
## Lueders, Hans and Aurel Croissant. 2014. 
## Wahlen, Strategien autokratischer Herrschaftssicherung 
## und das ?berleben autokratischer Regierungen. 
## Zeitschrift f?r vergleichende Politikwissenschaft, 8 (3-4): 329-355

## The R code was written by using the packages QCA 2.5 and SetMethods 2.0


######################################################
### Double check and test alternative calibrations ###
######################################################


rm(list = ls())

# Set working directory:
setwd()



# load packages
library(QCA); library(SetMethods); library(lattice); library(foreign);
library(arm); library(plyr); library(car); library(stringr); library(xtable); library(stargazer)

# Load data:
load("raw_data_LuedersCroissant.Rda")


# we test only fuzzy sets, that is
# 1. repression_soft_lag1
# 2. repression_hard_lag1
# 3. unrest_mass_lag1
# 4. gdpgrowth_lag
# 5. mort_change
# 6. rents_lag1
# 7. sector

# our anchors so far are (fully out/cross over/fully in):
# 6, 4.5, 3,   # repression_soft_lag1
# 7, 5.5,4,    # repression_hard_lag1
# 2, 1.5, 0,      # unrest_mass_lag1
# 3, 5, 10,     # gdpgrowth_lag
# -2, -5, -15,  # mort_change
# 10, 20, 100,  # rents
# 15, 25, 40),  # sector



#	1. let us first look at the calibration of 
# repression_soft_lag1, so far anchors at 6, 4.5, 3 (th_1)

#	the raw scores look like this:
summary(data5$repression_soft_lag1)
# note: max is 7 here for this selection of autocracies, 
# yet, the overall max of this indicator is 8, meaning no soft repression

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(5.5, 4, 3)
th_3 <- quantile(data5$repression_soft_lag1, c(.95, .85, .75))
th_3
# th_3 suggests thresholds 6,5,4


#	make new fuzzy variables
data5$hirep_s_al1 <- calibrate(data5$repression_soft_lag1, 'fuzzy', thresholds=th_2, logistic=T)
data5$hirep_s_al2 <- calibrate(data5$repression_soft_lag1, 'fuzzy', thresholds=th_3, logistic=T)

#	compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$repression_soft_lag1, data5$hirepression_soft_lag1, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$repression_soft_lag1, data5$hirep_s_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$repression_soft_lag1, data5$hirep_s_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$hirepression_soft_lag1, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='High Soft Repression', ylab='Electoral Defeat')
xy.plot(data5$hirep_s_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='High Soft Repression', ylab='Electoral Defeat')
xy.plot(data5$hirep_s_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='High Soft Repression', ylab='Electoral Defeat')
plot.new()	

#	now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$hirepression_soft_lag1,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$hirep_s_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$hirep_s_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirep_s_al1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data6 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data6[, 3:8], data6$defeat, necessity = TRUE, names(data6[, 3:8]), negation = TRUE)
QCAfit(1-data6[, 3:8], data6$defeat, necessity = TRUE, paste("not", names(data6[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data6, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data6, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data6, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: only very minor effect,
# basically the same solution formula as with old anchor
# (difference: ~leg_spec in third term)


# now the same with second alternative:

### Applying the WZB model ###
##############################


# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirep_s_al2)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data7 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data7[, 3:8], data7$defeat, necessity = TRUE, names(data7[, 3:8]), negation = TRUE)
QCAfit(1-data7[, 3:8], data7$defeat, necessity = TRUE, paste("not", names(data7[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data7, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data7, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data7, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: same as with old anchor

##################################################################
##################################################################

# 2. lets now look at the calibration of 
# repression_hard_lag1, so far anchors at 7, 5.5, 4 (th_1)


#	The raw scores look like this:
summary(data5$repression_hard_lag1)


#	now, let's try alternative calibrations 
th_2 <- c(5.5, 4, 3)
th_3 <- quantile(data5$repression_hard_lag1, c(.95, .85, .75))
th_3
# th_3 suggests thresholds 6,6,5 which will lead to NaN
# therefore:
th_3 <- c(7,6,5)


#	Make new fuzzy variables
data5$hirep_h_al1 <- calibrate(data5$repression_hard_lag1, 'fuzzy', thresholds=th_2, logistic=T)
data5$hirep_h_al2 <- calibrate(data5$repression_hard_lag1, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$repression_hard_lag1, data5$hirepression_hard_lag1, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$repression_hard_lag1, data5$hirep_h_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$repression_hard_lag1, data5$hirep_h_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$hirepression_hard_lag1, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='High Hard Repression', ylab='Electoral Defeat')
xy.plot(data5$hirep_h_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='High Hard Repression', ylab='Electoral Defeat')
xy.plot(data5$hirep_h_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='High Hard Repression', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$hirepression_hard_lag1,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$hirep_h_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$hirep_h_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirep_h_al1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data8 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data8[, 3:8], data8$defeat, necessity = TRUE, names(data8[, 3:8]), negation = TRUE)
QCAfit(1-data8[, 3:8], data8$defeat, necessity = TRUE, paste("not", names(data8[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data8, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data8, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data8, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# consistency is higher, yet, coverage is much lower
# furthermore, the old anchor reflects the meaning of the set much better


# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirep_h_al2)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data9 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data9[, 3:8], data9$defeat, necessity = TRUE, names(data9[, 3:8]), negation = TRUE)
QCAfit(1-data9[, 3:8], data9$defeat, necessity = TRUE, paste("not", names(data9[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data9, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data9, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data9, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: model ambiguity


#####################################################################
#####################################################################


# make overview of plots for Appendix, soft and hard repression as example:
par(mfrow=c(2, 3))
plot(data5$repression_soft_lag1, data5$hirepression_soft_lag1, pch=1,
     main= "Old Calibration Soft Repression",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=4.5)
plot(data5$repression_soft_lag1, data5$hirep_s_al1, pch=1, 
     main='First alternative calibration Soft Repression',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=4)
plot(data5$repression_soft_lag1, data5$hirep_s_al2, pch=1, 
     main='Second alternative calibration Soft Repression',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=5)
plot(data5$repression_hard_lag1, data5$hirepression_hard_lag1, pch=1, 
     main= "Old Calibration Hard Repression",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=5.5)
plot(data5$repression_hard_lag1, data5$hirep_h_al1, pch=1,
     main='First alternative calibration Hard Repression',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=4)
plot(data5$repression_hard_lag1, data5$hirep_h_al2, pch=1, 
     main='Second alternative calibration Hard Repression',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col="black")
abline(v=6)


#################################################################
#################################################################

#	3. let us now look at the calibration of 
# unrest_mass_lag1, so far anchors at 2, 1.5, 0 (th_1)


#	The raw scores look like this:
summary(data5$unrest_mass_lag1)

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(4, 2, 0.5)
th_3 <- quantile(data5$unrest_mass_lag1, c(.95, .85, .75))
th_3
# th_3 suggests thresholds 5,2,1


#	Make new fuzzy variables
data5$lounrest_mass_al1 <- calibrate(data5$unrest_mass_lag1, 'fuzzy', thresholds=th_2, logistic=T)
data5$lounrest_mass_al2 <- calibrate(data5$unrest_mass_lag1, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$unrest_mass_lag1, data5$lounrest_mass, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$unrest_mass_lag1, data5$lounrest_mass_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$unrest_mass_lag1, data5$lounrest_mass_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$lounrest_mass, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$lounrest_mass_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$lounrest_mass_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$lounrest_mass,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$lounrest_mass_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$lounrest_mass_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass_al1, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data10 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data10[, 3:8], data10$defeat, necessity = TRUE, names(data10[, 3:8]), negation = TRUE)
QCAfit(1-data10[, 3:8], data10$defeat, necessity = TRUE, paste("not", names(data10[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data10, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data10, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data10, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# same as with old anchor


# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass_al2, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data11 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                 "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data11[, 3:8], data11$defeat, necessity = TRUE, names(data11[, 3:8]), negation = TRUE)
QCAfit(1-data11[, 3:8], data11$defeat, necessity = TRUE, paste("not", names(data11[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data11, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data11, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data11, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: same as with old anchor



#################################################################
#################################################################

#	4. let us now look at the calibration of 
# 3, 5, 10,     # gdpgrowth_lag

#	The raw scores look like this:
summary(data5$gdpgrowth_lag)

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(4, 7, 15)
th_3 <- quantile(data5$gdpgrowth_lag, c(.75, .85, .95))
th_3



#	Make new fuzzy variables
data5$higdp_al1 <- calibrate(data5$gdpgrowth_lag, 'fuzzy', thresholds=th_2, logistic=T)
data5$higdp_al2 <- calibrate(data5$gdpgrowth_lag, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$gdpgrowth_lag, data5$higdpgrowth_lag, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$gdpgrowth_lag, data5$higdp_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$gdpgrowth_lag, data5$higdp_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$higdpgrowth_lag, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$higdp_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$higdp_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$higdpgrowth_lag,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$higdp_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$higdp_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdp_al1, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data12 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data12[, 3:8], data12$defeat, necessity = TRUE, names(data12[, 3:8]), negation = TRUE)
QCAfit(1-data12[, 3:8], data12$defeat, necessity = TRUE, paste("not", names(data12[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data12, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data12, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data12, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: much more complex solution formula, 5 terms



# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdp_al2, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data13 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data13[, 3:8], data11$defeat, necessity = TRUE, names(data13[, 3:8]), negation = TRUE)
QCAfit(1-data13[, 3:8], data11$defeat, necessity = TRUE, paste("not", names(data13[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data13, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data13, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data13, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: also here, 5 terms




#################################################################
#################################################################

#	5. let us now look at the calibration of 
# -2, -5, -15,  # mort_change

#	The raw scores look like this:
summary(data5$mort_change)

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(-4, -7, -20)
th_3 <- quantile(data5$mort_change, c(.95, .85, .75))
th_3



#	Make new fuzzy variables
data5$lomort_al1 <- calibrate(data5$mort_change, 'fuzzy', thresholds=th_2, logistic=T)
data5$lomort_al2 <- calibrate(data5$mort_change, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$mort_change, data5$lomort, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$mort_change, data5$lomort_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$mort_change, data5$lomort_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$lomort, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$lomort_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$lomort_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$lomort,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$lomort_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$lomort_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort_al1)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data14 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data14[, 3:8], data12$defeat, necessity = TRUE, names(data14[, 3:8]), negation = TRUE)
QCAfit(1-data14[, 3:8], data12$defeat, necessity = TRUE, paste("not", names(data14[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data14, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data14, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data14, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: one more term which seems not very meaningful (including ~rep_s, ~rep_h)



# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdp_al2, data5$lomort_al2)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data15 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data15[, 3:8], data11$defeat, necessity = TRUE, names(data15[, 3:8]), negation = TRUE)
QCAfit(1-data15[, 3:8], data11$defeat, necessity = TRUE, paste("not", names(data15[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data15, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data15, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data15, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: same as al1 (both have also low coverage)



#################################################################
#################################################################

#	6. let us now look at the calibration of 
# 10, 20, 100,  # rents

#	The raw scores look like this:
summary(data5$rents_lag1)

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(20,50,150)
th_3 <- quantile(data5$rents_lag1, c(.75, .85, .95))
th_3



#	Make new fuzzy variables
data5$hirents_al1 <- calibrate(data5$rents_lag1, 'fuzzy', thresholds=th_2, logistic=T)
data5$hirents_al2 <- calibrate(data5$rents_lag1, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$rents_lag1, data5$hirents_lag1, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$rents_lag1, data5$hirents_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$rents_lag1, data5$hirents_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$rents_lag1, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$hirents_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$hirents_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$hirents_lag1,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$hirents_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$hirents_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_al1)


# check data
head(data5)

# sort data, only conditions and outcome
data16 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data16[, 3:8], data16$defeat, necessity = TRUE, names(data16[, 3:8]), negation = TRUE)
QCAfit(1-data16[, 3:8], data16$defeat, necessity = TRUE, paste("not", names(data16[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data16, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data16, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data16, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: one more term which seems not very meaningful (very low coverage)



# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdp_al2, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_al2)


# check data
head(data5)

# sort data, only conditions and outcome
data17 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data17[, 3:8], data17$defeat, necessity = TRUE, names(data17[, 3:8]), negation = TRUE)
QCAfit(1-data17[, 3:8], data17$defeat, necessity = TRUE, paste("not", names(data17[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data17, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data17, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data17, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: same as al1 


#################################################################
#################################################################



#	7. let us now look at the calibration of 
# 15, 25, 40,  # sector

#	The raw scores look like this:
summary(data5$sector)

# visualization of calibration see script on data preparation

#	now, let's try alternative calibrations 
th_2 <- c(20,30,50)
th_3 <- quantile(data5$sector, c(.75, .85, .95))
th_3



#	Make new fuzzy variables
data5$hisector_al1 <- calibrate(data5$sector, 'fuzzy', thresholds=th_2, logistic=T)
data5$hisector_al2 <- calibrate(data5$sector, 'fuzzy', thresholds=th_3, logistic=T)

#	Compare the three calibrations:

par(mfrow=c(3, 1))
plot(data5$sector, data5$hisector, pch=19, col=rgb(0,0,1,0.5),
     main= "Old Calibration",
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))
plot(data5$sector, data5$hisector_al1, pch=19, col=rgb(0,0,1,0.5),
     main='First alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))	
plot(data5$sector, data5$hisector_al2, pch=19, col=rgb(0,0,1,0.5),
     main='Second alternative calibration',
     xlab='Raw score',
     ylab='Fuzzy score')
abline(h=0.5, col=rgb(.5,.5,.5,.5))


#	now plot all alternatives with outcome 

par(mfrow=c(2, 2))
xy.plot(data5$hisector, data5$defeat, necessity=F, 
        main='Old calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$hisector_al1, data5$defeat, necessity=F,
        main='First alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
xy.plot(data5$hisector_al2, data5$defeat, necessity=F,
        main='Second alternative calibration',
        xlab='Fuzzy Score', ylab='Electoral Defeat')
plot.new()	

#	Now let's consider parameters of fit for sufficiency of soft repression
#	for outcome = electoral defeat:

nf1 <- QCAfit(data5$defeat, data5$hisector,   necessity=F)
nf2 <- QCAfit(data5$defeat, data5$hisector_al1, necessity=F)
nf3 <- QCAfit(data5$defeat, data5$hisector_al2, necessity=F)

rbind(nf1, nf2, nf3)

# use first alternative anchor for alternative QCA
# / outcome "no electoral defeat":


### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector_al1)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data18 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data18[, 3:8], data18$defeat, necessity = TRUE, names(data18[, 3:8]), negation = TRUE)
QCAfit(1-data18[, 3:8], data18$defeat, necessity = TRUE, paste("not", names(data18[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data16, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data18, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data18, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: one more term which has very low coverage



# now the same with second alternative:

### Applying the WZB model ###
##############################

# Combine conditions by logical OR according to WZB model

# rename conditions for repression (just for better visibility):
# hard repression: rep_h
data5$rep_h <- c(data5$hirepression_hard_lag1)
# soft repression: rep_s
data5$rep_s <- c(data5$hirepression_soft_lag1)


# Legitimation, combinations by using logical OR
# leg_diff = combination of unrest mass and elite
data5$leg_diff <- pmax(data5$lounrest_mass, data5$lounrest_elite) 
# leg_spec = combination of gdp and child moratality
data5$leg_spec <- pmax(data5$higdpgrowth_lag, data5$lomort)


# Cooptation, partly combinations
# coop_form = combination of party and sector
data5$coop_form <- pmax(data5$hiparty, data5$hisector_al2)
# coop_inf = rents
data5$coop_inf <- c(data5$hirents_lag1)


# check data
head(data5)

# sort data, only conditions and outcome
data19 <- data5[c("country", "year", "leg_spec","leg_diff","coop_form", 
                  "coop_inf","rep_h","rep_s", "defeat")]

### Analysis of necessity ###
#############################

QCAfit(data19[, 3:8], data19$defeat, necessity = TRUE, names(data19[, 3:8]), negation = TRUE)
QCAfit(1-data19[, 3:8], data19$defeat, necessity = TRUE, paste("not", names(data19[, 3:8])), negation = TRUE)


nec_ny <- superSubset(data19, outcome = "defeat", neg.out = TRUE,
                      conditions = c("leg_spec","leg_diff","coop_form", 
                                     "coop_inf","rep_h","rep_s"),
                      incl.cut = 0.91, cov.cut = 0.7)
nec_ny


### Analysis of sufficiency ###
###############################

# truth table
TT_ny <- truthTable(data19, outcome = "defeat", neg.out = TRUE,
                    conditions = c("leg_spec","leg_diff","coop_form", 
                                   "coop_inf","rep_h","rep_s"),
                    incl.cut1 = 0.8, 
                    n.cut = 2,
                    show.cases = TRUE,
                    complete = FALSE,
                    sort.by = c("OUT", "incl", "n"))

TT_ny

# consistency threshold 0.8
# n.cut = 2

# logical minimization
# conservative
sol_ny_c <- eqmcc(data19, outcome = "defeat", neg.out = TRUE,
                  conditions = c("leg_spec","leg_diff","coop_form", 
                                 "coop_inf","rep_h","rep_s"),
                  incl.cut1 = 0.8, 
                  n.cut = 2,
                  show.cases = TRUE,
                  details = TRUE)
sol_ny_c
# observation: two more terms (5 altogether), both have very very low coverage
# (0.016 / 0.023)
# therefore, not very meaningful 

##############################################################################
##############################################################################

# Conclusion of the alternative calibrations:
# no alternative anchor changes the solution formula in a meaningful way
# (new solution terms have very low coverage)
# the "old" anchors seem to represent the meaning of the sets better than the alternatives
# hence, the "old" anchors are robust!

