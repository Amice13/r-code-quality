#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 
#                          

#~~~~~~~~~~~~~~   Independent Exercise on a full QCA - Solution   ~~~~~~~~~~~~~~~~~~



# Load the packages:

library(SetMethods)
library(QCA)

# The data comes from Paykani, Toktam, Rafiey, Hassan, Sajjadi, Homeira (2018)
# "A fuzzy set qualitative comparative analysis of 131 countries: Which
# configuration of the structural conditions can explain health better?"
# [https://pubmed.ncbi.nlm.nih.gov/29357889/]

# Outcome: 
# HL = High life expectancy

# Conditions:

# HE = High quality education
# GG = Good governance
# AH = Affluent health system
# HI = High income inequality
# HW = High wealth

# Load the data

data("PAYF")
head(PAYF)

# Store condition names in one object in order to save time and space 
conds <- c("HE", "GG", "AH", "HI","HW")


#### 1. BEFORE THE ANALYTIC MOMENT ####

# The data is already calibrated, therefore we can perform some diagnostics tests

# Check the skewness of the data for the 5 conditions and the outcome HL:

skew.check(PAYF[, 3:8])

# Check whether there are any ambiguous cases with set-membership scores of 0.5
# in any of the 5 conditions and the outcome HL:

ambig.cases(PAYF[, 3:8])


#### 2. NECESSITY ANALYSIS FOR THE PRESENCE OF THE OUTCOME ####


# Check whether we have any single necessary conditions for outcome HL:

QCAfit(PAYF[, 3:7],
       PAYF$HL,
       necessity = TRUE)


# Display SUIN conditions, setting the inclusion cutoff to 0.9,
# coverage cutoff to 0.6, and RoN to 0.5. Display only the combinations
# of a maximum of two conditions.

SUIN_y <- superSubset(data = PAYF,
                      outcome  = "HL",
                      conditions = conds,
                      incl.cut = 0.9,
                      cov.cut = 0.6,
                      ron.cut = 0.5,
                      depth = 2)
SUIN_y

# Plot the results:

pimplot(data = PAYF,
            outcome = "HL",
            results = SUIN_y,
            necessity = TRUE)


#### 3. SUFFICIENCY ANALYSIS FOR THE PRESENCE OF THE OUTCOME ####

# Truth Table
# Create the truth table, setting the inclusion cutoff to 0.84 and the
# number of cases needed to include a row in the minimization to 1.

TT_y <- truthTable(data = PAYF,
                   outcome  = "HL",
                   conditions = conds,
                   incl.cut = 0.84,
                   n.cut = 1,
                   sort.by = c("OUT", "incl"),
                   complete = TRUE,
                   show.cases = FALSE)
TT_y


# Logical Minimization 
# Produce the conservative solution:

sol_yc <- minimize(TT_y, 
                   details = TRUE)
sol_yc

# Produce the most parsimonious solution:

sol_yp <- minimize(TT_y, 
                   details = TRUE, 
                   include = "?")
sol_yp

# Check the simplifying assumptions:

sol_yp$SA


# Produce the intermediate solution with the following 
# directional expectations 1, 1, 1, 0, 1

sol_yi <- minimize(TT_y, 
                   details = TRUE, 
                   include = "?",
                   dir.exp = c(1, 1, 1, 0, 1))
sol_yi

# Check the easy counterfactuals

sol_yi$i.sol$C1P1$EC

# There are no easy counterfactuals

# Plot the intermediate solution

pimplot(data = PAYF,
        results = sol_yi,
        outcome = "HL")


#### 4. NECESSITY ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Analyze necessary conditions for outcome ~Y using the superSubset
# function. Set inclusion cutoff to 0.908, depth to 2, coverage
# cutoff to 0.8 and RoN to 0.8.

SUIN_ny <- superSubset(data = PAYF,
                       outcome  = "~HL",
                       conditions = conds,
                       incl.cut = 0.908,
                       cov.cut = 0.8,
                       ron.cut = 0.8,
                       depth = 2)
SUIN_ny


# Plot the results:

pimplot(data = PAYF,
        outcome = "HL",
        results = SUIN_ny,
        necessity = TRUE)


#### 5. SUFFICIENCY ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Truth Table
# Create a truth table setting the inclusion cutoff to 0.9 and the
# number of cases needed to include a row in the analysis to 2.

TT_ny <- truthTable(data = PAYF,
                        outcome  = "~HL",
                        conditions = conds,
                        incl.cut = 0.9,
                        n.cut = 2,
                        sort.by = c("OUT", "incl"),
                        complete = TRUE)
TT_ny

# LLogical Minimization 
# Produce the most parsimonious solution:

sol_nyp <- minimize(TT_ny, 
                    details = TRUE, 
                    include = "?")
sol_nyp

# Check the simplifying assumptions:

sol_nyp$SA

# Produce the intermediate solution with the following 
# direcional expectations 0, 0, 0, 1, 0:

sol_nyi <- minimize(TT_ny, 
                    details = TRUE, 
                    include = "?", 
                    dir.exp = c(0, 0, 0, 1, 0))
sol_nyi

# Check the easy counterfactuals 

sol_nyi$i.sol$C1P1$EC
sol_nyi$i.sol$C1P2$EC


# Look at the PI chart
sol_nyi$PIchart


#### 6. ENHANCED STANDARD ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Assumptions contradicting necessity
# Let's use the intermediate solution 

# From SUIN_ny, we know that the expression ~HE+~GG contradicts
# the statement of necessity.

# Does any of the simplifying assumptions for the most parsimonious
# solution contradict the statement of necessity?  

sol_nyp$SA

# Create a new truth table which will set such rows to OUT=0,
# using the esa function

TT_ny_esa <- esa(TT_ny, 
                 nec_cond = c("~HE+~GG"))
TT_ny_esa

# Rerun the analysis of sufficiency producing the ENHANCED
# intermediate solution for ~Y with the following directional
# expectations: 0,0,0,1,0

sol_nyi_esa <- minimize(TT_ny_esa, 
                        include = "?", 
                        details = TRUE, 
                        dir.exp = c(0,0,0,1,0))

sol_nyi_esa

# Contradictory easy counterfactuals 
# Check if we are making contradictory easy counterfactuals 
# Put them in an object named CEC 

CEC <- intersect(rownames(sol_yi$i.sol$C1P1$EC), 
                 rownames(sol_nyi$i.sol$C1P1$EC))
CEC

# We have none 

# Simultaneous subset relations 
# Now check if one or more rows were included into the minimization for both
# outcome Y and ~Y 
# Use the intersect command, create a new object SSR

SSR <- intersect(rownames(TT_y$tt)[TT_y$tt$OUT==1],
               rownames(TT_ny$tt)[TT_ny$tt$OUT==1])
SSR

# Row 20 is included in both logical minimization processes,
# however, use the following commands to check the
# PRI value: 
TT_y$tt[20,] # notice the low PRI value
TT_ny$tt[20,] # PRI is much higher and much above 0.5
# We see that while for outcome HL, PRI is very low, for outcome
# ~HL, PRI is much better (0.69), so we keep it in the logical minimization.


# Impossible remainder 
# The combination of four conditions never empirically occurs:
# ~H_EDU*G_GOV*~AFF_HEAL*~H_INCEQ, so let's assume that it is
# impossible. Avoid untenable assumptions on any logical remainder
# that contains the combination of these four conditions.
# Produce new ENHANCED intermediate solution for ~Y:

TT_ny_esa <- esa(TT_ny,
                nec_cond = "~HE+~GG",
                untenable_LR = "~HE*GG*~AH*~HI")
TT_ny_esa

# Produce the enhanced intermediate solution:

sol_nyi_esa <- minimize(TT_ny_esa, 
                       details = TRUE, 
                       include = "?", 
                       dir.exp = c(0,0,0,1,0))
sol_nyi_esa


#### 7. THEORY EVALUATION ####


# Let us assume that according to our theory,  
# good governance (GG) and affluent health system (AH) or
# low income inequality (~HI) lead to high life expectancy (HL)

# Intersect theory with the most parsimonious solution:

TH <- theory.evaluation(theory = "GG*AH+~HI", 
                        empirics =  sol_yp, 
                        outcome = "HL", 
                        sol = 1)
TH


#### 8. ROBUSTNESS ####

# Sensitivity ranges:

# Find the raw consistency and the frequency cut sensitivity ranges
# (for Y):

rob.inclrange(
  data = PAYF,
  step = 0.01,
  max.runs = 20,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.84,
  n.cut = 1,
  include = "?"
)

rob.ncutrange(
  data = PAYF,
  step = 1,
  max.runs = 20,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.84,
  n.cut = 1,
  include = "?"
)

# Load the raw data:
data("PAYR")
head(PAYR)

# Creating the test solutions:

# Alter consistency to 0.8 and inclusion cutoff to 2. Create a new
# object TS1.
TS1 <- minimize(data = PAYF,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.8,
                n.cut = 2,
                include = "?",
                details = TRUE, show.cases = TRUE)
TS1

# Alter calibration for the condition HW, using the condition WEAL
# in the raw dataset and putting thresholds at 4000, 25000, and 45000
PAYF2 <- PAYF
PAYF2$HW <- calibrate(PAYR$WEAL, 
                    type="fuzzy", 
                    thresholds = c(4000,25000,45000), 
                    logistic = TRUE, 
                    idm = 0.95)

# Create object called TS2. This should be the most parsimonious solution
# using parameters from the very first truth table (sufficiency analysis
# for the presence of the outcome)
TS2 <- minimize(data = PAYF2,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.84,
                n.cut = 1,
                include = "?",
                details = TRUE, show.cases = TRUE)

TS2

# Save the most parsimonious solution for the presence of the 
# outcome as a new object IS:
IS <- sol_yp

# Create the test set in a list:
TS <- list(TS1, TS2)

# Calculate parameters for the robust core:
rob.corefit(test_sol = TS, initial_sol = IS, outcome = "HL") 

# Calculate robustness parameters:
rob.fit(test_sol = TS,
        initial_sol = IS,  
        outcome = "HL")



#### 9. SMMR Y ####

# Print the typical cases for each focal conjunct in the
# second sufficient term of the intermediate solution

typ_foc <- smmr(sol_yi, 
            outcome = "HL",
            match = FALSE,
            cases = 2, 
            term = 2)
typ_foc

# Show the typical - IIR cases for each term, using the intermediate
# solution:

typ_iir_term <- smmr(results = sol_yi,
                     outcome = "HL",
                     match = TRUE,
                     cases = 6)
typ_iir_term

#### 10. SMMR ~Y ####

# Display the deviant cases coverage, using the intermediate
# solution:

dcov <- smmr(results = sol_nyi,
             outcome = "HL",
             match = FALSE,
             cases = 4)
dcov

# Using the intermediate solution, show the best pairs of
# Deviant Coverage - IIR cases in each TT row:

dcov_iir <- smmr(results = sol_nyi,
                 outcome = "HL",
                 match = TRUE,
                 cases = 4)
dcov_iir 



#### 11. CLUSTER ~Y ####

# We use the enhanced intermediate solution for the negated outcome

# Perform cluster diagnostics for the enhanced intermediate
# solution with "REGION" as clusters and "COUNTRY" as units:

C_PAYF <- cluster(data = PAYF, 
        result = sol_nyi_esa, 
        outcome = "HL", 
        unit_id = "COUNTRY",
        cluster_id = 'REGION',
        necessity = FALSE,
        wicons = FALSE)

C_PAYF

# Plot between consistencies: 

cluster.plot(C_PAYF,
             labs = TRUE,
             size = 7,
             angle = 15,
             wicons = FALSE)
