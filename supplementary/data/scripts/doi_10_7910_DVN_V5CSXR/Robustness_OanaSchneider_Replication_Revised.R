###
# "A Robustness Test Protocol for Applied QCA: Theory and R Software Application"
# Sociological Methods & Research
# DOI: https://doi.org/10.1177/00491241211036158
# I.-E. Oana and C.Q. Schneider
#
# Replication Script
###

library(SetMethods)

# Load data:

data("PAYR")
head(PAYR)

data("PAYF")
PF  <- PAYF[,3:8]
head(PF)

# Create the initial solution:

conds = names(PF[,1:5])

IS <- minimize(data = PF,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.87,
                n.cut = 2,
                include = "?",
                details = TRUE)
IS


# Sensitivity ranges:

rob.calibrange(
  raw.data = PAYR,
  calib.data = PF,
  test.cond.raw = "WEAL",
  test.cond.calib = "HW",
  test.thresholds = c(3000,10500,28500),
  type = "fuzzy",
  step = 500,
  max.runs = 40,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.87,
  n.cut = 2,
  include = "?"
)

rob.inclrange(
  data = PF,
  step = 0.01,
  max.runs = 20,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.87,
  n.cut = 2,
  include = "?"
)

rob.ncutrange(
  data = PF,
  step = 1,
  max.runs = 20,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.87,
  n.cut = 2,
  include = "?"
)

# Creating the test solutions:

# altering consistency
TS1 <- minimize(data = PF,
                 outcome  = "HL",
                 conditions = conds,
                 incl.cut = 0.75,
                 n.cut = 2,
                 include = "?",
                 details = TRUE, show.cases = TRUE)
TS1

# altering calibration for one condition :
PF2 <- PF
PF2$HW <- calibrate(PAYR$WEAL, 
                      type="fuzzy", 
                      thresholds = c(1000,9000,37000), 
                      logistic = TRUE, 
                      idm = 0.95)


TS2 <- minimize(data = PF2,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.87,
                n.cut = 2,
                include = "?",
                details = TRUE, show.cases = TRUE)

TS2

# altering ncut and calibation for one condition :

TS3 <- minimize(data = PF2,
                outcome  = "HL",
                conditions = conds,
                incl.cut = 0.87,
                n.cut = 1,
                include = "?",
                details = TRUE, show.cases = TRUE)

TS3

# Obtain Boolean expression for minTS:
intersection(TS1, TS2, TS3)

# Obtain Boolean expression for maxTS:
simplify("(GG*AH + HE*~GG*HI*HW)+(HE + ~GG*HW)+(GG*AH + AH*HI)+(HE*GG*AH + HE*AH*HI)", snames = conds)

# Create the test set in a list:
TS <- list(TS1, TS2, TS3)

# Obtain Boolean expression for RC:
intersection(IS, TS1, TS2, TS3)

# Calculate parameters for the robust core:
rob.corefit(test_sol = TS, initial_sol = IS, outcome = "HL") 

# Calculate robustness parameters:
RF <- rob.fit(test_sol = TS, 
           initial_sol = IS,  
           outcome = "HL")
RF

# Plotting the initial solution against the test set:

rob.xyplot(test_sol = TS, 
                  initial_sol = IS, 
                  outcome = "HL", 
                  fontsize = 5, 
                  all_labels = FALSE,
                  jitter=TRUE,
                  area_lab = TRUE)

# Looking at cases against the test set:

rob.cases(test_sol = TS, 
                 initial_sol = IS, 
                 outcome = "HL")


# Worse test solution:

rob.singletest(test_sol = TS, initial_sol = IS, outcome = "HL")

