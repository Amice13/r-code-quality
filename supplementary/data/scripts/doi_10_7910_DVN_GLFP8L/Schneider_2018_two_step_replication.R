# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REPLICATION MATERIAL FOR:
# "TWO-STEP QCA REVISITED: THE NECESSITY OF REMOTE
# CONDITIONS"
# Carsten Q. Schneider
#
# August 2018
# R version 3.4.4
# Packages QCA 3.3, SetMethods 2.4
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Clear working space
rm(list = ls())

# Load packages
library(QCA)
library(SetMethods)


#set working directory
# PC
setwd("D:/My Documents/Dropbox/QCA/two-step_update/data")
# Mac
setwd("~/Dropbox/QCA/two-step_update/data")


# Load data Haesebrouck 2015 #### 

HAS <- read.csv("Haesebrouck_15.csv", row.names = 1, sep = ";",dec = ",")

head(HAS)


# Outcome
# LC = Large personnel contributions

# Remote conditions (international level)
# MC = Military capabilities
# GP = High geographic proximity
# PI = Prior peacekeaping involvement
# MS = Military stretch

 
# Proximate conditions (domestic level) 
# LE = Left executive
# LP = Left parliament
# PV = Parliamentary Veto
# ED = Large Electoral Distance



#### Change data for presentational purposes ####

# change outcome score of SK from 0.05 to 0.8
# that adds one more consistent tt row to minimization
HAS[22,1] <- 0.8

# change outcome score of CZ from 0.05 to 0.8
# that adds one more consistent tt row to minimization
HAS[3,1] <- 0.8

# change outcome score of GB from 0.05 to 0.4
# that adds one more consistent tt row to minimization
HAS[9,1] <- 0.4

# change PV score of LU from 0.8 to 0.4
# this turns its tt row into a remainder
HAS[15,9] <- 0.4



#### STEP 1: Necessity of remote conditions ####

SUIN_y <- superSubset(data = HAS, conditions = c("MC", "GP", "PI", "MS"),
                      outcome = "LC",
                      relation = "nec",
                      incl.cut = 0.9,
                      cov.cut = 0.6,
                      ron.cut = 0.5)
SUIN_y
# MC+PI

# creat XY plot of context
pimplot.lab(data = HAS, results = SUIN_y, outcome = "LC", 
        necessity = TRUE)
# no deviant cases consistency in kind



#### STEP 2: Sufficiency or Remote-Proximate Conjunctions ####

TT_y <- truthTable(data = HAS, outcome = "LC",
                   conditions = c("MC", "PI", "LE", "LP", "PV", "ED"),
                   incl.cut = 0.8,
                   show.cases = TRUE,
                   complete = TRUE,
                   sort.by = c("OUT", "incl"))

TT_y

# Interesting note: without remote factors, no tt row is above 
# consistency threshold - further argument for two-step


# 
sol_yc <- minimize(TT_y, details = TRUE, show.cases = TRUE)
sol_yc
# SUIN PI has disappeared
# One term contains ~PI. Since that term also contains disjunct MC
# it is not a fundamental problem, though. At minimium, it is unrelated to 
# the treatment of logical remainders

sol_yp <- minimize(TT_y, details = TRUE, include = "?", all.sol = TRUE)
sol_yp
# model ambiguity and
# SUIN disappear from some terms


# check simplifying assumptions
sol_yp$SA
# several are untenable (contain negation of necessary context)



### Apply ESA ####

# exclude remainders contradicting necessity

# first negate the necessary condition expression
negate_nec <- negate("MC+PI", snames = "MC, PI")
negate_nec

# then identify rows in the truth table
nec <- findRows(obj = TT_y, expression = negate_nec, remainders = TRUE)
nec

#then exclude those rows from the logical minization
sol_yp2 <- minimize(TT_y, details = TRUE, include = "?", 
                    all.sol = TRUE,
                    exclude = nec)
sol_yp2
# model ambiguity
# But all sufficient term contain at least one of the two
# SUIN contexts

# check simplifying assumptions
sol_yp2$SA
# No untenable assumptions are made


# produce enhanced intermediate solution
sol_yi2 <- minimize(TT_y, details = TRUE, include = "?", 
                    all.sol = TRUE,
                    dir.exp = "1,1,1,1,1,1",
                    exclude = nec)
sol_yi2
# all terms contain at least one SUIN
# no model ambiguity

# check easy counterfactuals
EC1 <- sol_yi2$i.sol$C1P1$EC
EC2 <- sol_yi2$i.sol$C1P2$EC
EC1
EC2
# no untenable easy counterfactuals.

# XY plot for intermediate solution
pimplot.lab(data = HAS, outcome = "LC", results = sol_yi2,
        sol = "c1p2i1")


