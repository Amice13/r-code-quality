#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 


#~~~~~ Chapter 7 - Rounding Up - A Solid QCA ~~~~~


#### 1. Robustness #### 


library(SetMethods)

# Load data:

data("PAYR")
head(PAYR)

data("PAYF")
PF  <- PAYF[,3:8]
head(PF)

# Create an object storing the condition names:

conds <- c("HE", "GG", "AH", "HI", "HW")

# Create the initial solution:

IS <- minimize(data = PF,
               outcome  = "HL",
               conditions = conds,
               incl.cut = 0.87,
               n.cut = 2,
               include = "?",
               details = TRUE)
IS

# Export solution to Latex

stargazerSol(results = IS,
             outcome = "HL",
             digits = 3,
             title = "Initial Solution")

####  1.1. Sentivity ranges ####

# Determine sensitivity ranges for qualitative calibration anchors of condition HW:

rob.calibrange(
  raw.data = PAYR,
  calib.data = PF,
  test.cond.raw = "WEAL",
  test.cond.calib = "HW",
  test.thresholds = c(3000,10500,28500),
  step = 500,
  max.runs = 40,
  outcome  = "HL",
  conditions =  conds,
  incl.cut = 0.87,
  n.cut = 2,
  include = "?"
)

# Determine sensitivity range for raw consistency threshold:

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

# Determine sensitivity range for frequency cut-off:

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

#### 1.2. Fit-oriented and case-oriented robustness ####

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

# Obtain Boolean expression for TS:
TS_exp <- intersection("HE + ~GG*HW","GG*AH + AH*HI","HE*GG*AH + HE*AH*HI")
TS_exp

# Obtain parameters of fit for TS:
pof(TS_exp, 
    outcome = "HL", 
    data = PF, 
    relation = "sufficiency")

# Create the test set in a list:
TS <- list(TS1, TS2, TS3)


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
           fontsize = 3.5, 
           jitter=TRUE,
           all_labels = FALSE,
           area_lab=TRUE)

# Obtaining names of case types and robustness case parameters:

rob.cases(test_sol = TS, 
          initial_sol = IS, 
          outcome = "HL")


# Worse test solution:

rob.singletest(test_sol = TS, initial_sol = IS, outcome = "HL")

#### 2. Cluster Diagnostics #### 


data("PAYF")
conds <-  c("HE", "GG", "AH", "HI","HW")


# Create parsimonious solution:

PS <- minimize(data = PAYF,
               outcome  = "HL",
               conditions = conds,
               incl.cut = 0.87,
               n.cut = 2,
               include = "?",
               details = TRUE,
               use.tilde = TRUE)
PS

# Determine robustness of results across clusters:

CS <- cluster(results = PS, 
                    data = PAYF, 
                    outcome  = "HL", 
                    unit_id = "COUNTRY", 
                    cluster_id = "REGION", 
                    wicons = FALSE)
CS

# Plot results of cluster diagnostics:

cluster.plot(CS,
             labs = TRUE,
             size = 11,
             angle = 32,
             wicons = FALSE)


#### 3. Causal Chains #### 

data("PAYF")

PAYF <- PAYF[,3:8]

head(PAYF)

# Detect temporal dependencies:

PAK_chain <- causalChain(data = PAYF, 
                         ordering = "GG < HW < HL",
                         sol.cons = 0.8,
                         sol.cov = 0.7, 
                         pi.cons = 0.8,
                         include = "?",
                         n.cut = 2,
                         row.dom = TRUE,
                         strict = TRUE)
PAK_chain

conds <- c("HE", "GG", "AH", "HI", "HW")

test_tt <- truthTable(PAYF, 
                      outcome = "~HL",
                      conditions = conds,
                      incl.cut = 0.9,
                      pri.cut = 0.5,
                      n.cut = 2,
                      sort.by = c('incl', 'n'),
                      complete = TRUE)
test_tt


test <- minimize(test_tt,
                 include = "?",
                 details = TRUE)
test
