####################################################################
### Script to Julia Leib 2022:
### How Justice Becomes Part of the Deal: Pre-Conditions for the 
### Inclusion of Transitional Justice Provisions in Peace Agreements
### International Journal of Transitional Justice, 2022 (forthoming)    
####################################################################


## The R code in this file conducts crisp-set QCA for the outcome Transitional Justice provisions 
## and No Transitional Justice provisions, including ESA (Schneider and Wagemenn 2012, Chapter 8.2).

## The R code was written by using the packages QCA 3.10 and SetMethods 2.6


rm(list = ls())

# Set working directory: 
setwd()

# Installing and loading relevant packages
install.packages("QCA", dependencies = TRUE)
install.packages("SetMethods", dependencies = TRUE)
library(QCA); library(SetMethods)


# Load calibrated data:
TJ <- read.table("Leib2022_Data_TJ_Provisions.csv", header = TRUE, row.names = "ID", sep = ';')
head(TJ,10)


#################################
# ---------------------------------------------------------------
# (1) Analysis of necessity
# ---------------------------------------------------------------
#################################

condsTJ <- subset(TJ, select = c("ATR","REB","GOV","THIRD","CIV")) 

### Analyzing necessary conditions for the outcome (TJ)
pof(condsTJ, "TJ", TJ, relation = "nec")
pof(1-condsTJ, "TJ", TJ, relation = "nec")

### Analyzing necessary conditions for the negated outcome (~TJ)
pof(condsTJ, "~TJ", TJ, relation = "nec")
pof(1-condsTJ, "~TJ", TJ, relation = "nec")

# no necessary condition


### Exploring further possible superset relations
NecPeace_TJ <- superSubset(TJ[, 4:8], outcome = "TJ", incl.cut = 0.9, cov.cut = 0.6)
NecPeace_TJ

NecPeaceN_TJ <- superSubset(TJ[, 4:8], outcome = "~TJ", incl.cut = 0.9, cov.cut = 0.6)

# no meaningful logical OR connection necessary for Y or ~Y


#################################
# ---------------------------------------------------------------
# (2) Analysis of sufficiency
# ---------------------------------------------------------------
#################################


### Outcome: Transitional Justice provisions (TJ)

### truth Table
TT_TJ <- truthTable(TJ, outcome = "TJ", 
                     conditions = c("ATR","REB","GOV","THIRD","CIV"),
                     incl.cut = 0.9,
                     show.cases = TRUE,
                     complete = TRUE,
                     sort.by = c("incl", "n"))
TT_TJ

## check for deviant cases consistency in kind
truthTable(TJ[, 4:8], "TJ", incl.cut = 0.9, show.cases = TRUE, dcc = TRUE,
           sort.by = "OUT, n")


### logical minimization
## Conservative solution
cons_TJ <- minimize(TT_TJ, show.cases = TRUE, details = TRUE, 
                       PRI = TRUE, use.tilde = TRUE, row.dom = FALSE)
cons_TJ


## Parsimonious solution
pars_TJ <- minimize(TT_TJ, details = TRUE, show.cases = TRUE, include = "?",
                         PRI = TRUE, use.tilde = TRUE, row.dom = TRUE)
pars_TJ

# Simplifying assumptions  
pars_TJ$SA


## Intermediate solution
ints_TJ <- minimize(TT_TJ, details = TRUE, include = "?", 
                       row.dom = TRUE, PRI = TRUE, use.tilde = TRUE, show.cases = TRUE,
                       dir.exp = c(1,"-",1,1,1))
ints_TJ

# directional expectation ATR, GOV, THIRD, and CIV -> TJ provisions; ambiguous for REB

# No test for untenable assumptions or contradictory TT rows needed, as no necessary conditions have been identified. 

ints_TJ$PIchart

factorize(ints_TJ)


# Easy counterfactuals
ECints_TJ <- ints_TJ$i.sol$C1P1$EC
ECints_TJ

# truth table rows 2,6,17,18,22 represent easy counterfactuals and are included in the minimization for the intermediate solution.

# Difficult counterfactuals
DCints_TJ <- ints_TJ$i.sol$C1P1$DC
DCints_TJ


### Sufficiency plot
pimplot(TJ,
        cons_TJ,
        "TJ",
        incl.tt=NULL,
        ttrows= c(),
        necessity=FALSE,
        sol=1,
        all_labels=FALSE,
        markers = TRUE,
        labcol="black",
        jitter = FALSE,
        font = "sans",
        fontface = "italic",
        fontsize = 3,
        crisp = TRUE,
        consH = FALSE
        )


### Single case types

# Typical cases:
smmr(ints_TJ, outcome = "TJ", match = FALSE, 
    cases = 1, sol=1)

# Deviant consistency:
smmr(ints_TJ, outcome = "TJ", match = FALSE, 
    cases = 3, sol=1)

# Deviant coverage:
smmr(ints_TJ, outcome = "TJ", match = FALSE, 
    cases = 4, sol=1)

# IIR:
smmr(ints_TJ, outcome = "TJ", match = FALSE, 
    cases = 5, sol=1)



#### ----------------

### Outcome: No Transitional Justice provisions (~TJ)

### truth table 
TT_TJn <- truthTable(TJ, outcome = "~TJ", 
                    conditions = c("ATR","REB","GOV","THIRD","CIV"),
                    incl.cut = 0.8,
                    show.cases = TRUE,
                    complete = TRUE, 
                    sort.by = c("incl", "n"))
TT_TJn


# A minimization of the truth table is not  possible, since all outcome values have been coded to zero.
# There are no truth table rows with consistency higher than .5; we cannot deduce a solution formula.
# This shows that the contextual conditions cannot explain why TJ provisions are not included.
# The model is only good for explaining why TJ provisions are included in agreements.


# end of script.

