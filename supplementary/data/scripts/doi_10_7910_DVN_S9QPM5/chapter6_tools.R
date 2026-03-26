#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 
   

#~~~~~ Chapter 6 - Post-QCA Tools  ~~~~~



library(SetMethods)


#### 1. Theory Evaluation  #### 

data("THOF")
head(THOF)



# Obtain the intermediate solution:

TTC <- truthTable(data=THOF, 
                  outcome = "CUSTOM", 	
                  conditions = "RESP, SAL, RES, VPO, VPL, COERC", 
                  incl.cut=0.764, 
                  sort.by="incl, n", 
                  complete=TRUE, 
                  show.cases=FALSE)
TTC

C_int <- minimize(TTC, 
                  include = "?", 
                  details=TRUE, 
                  show.cases=TRUE, 
                  dir.exp = "1, 1, 1, 1, 1,-",  
                  use.tilde=TRUE)
C_int

# Summarize the theory in Boolean terms:
TCUSTOM <- "RESP*SAL*RES*VPO + RESP*SAL*RES*VPL + 
	RESP*COERC"

# Perform theory evaluation:
TEV <- theory.evaluation(theory = TCUSTOM, 
                         empirics = C_int,
                         outcome = "CUSTOM",
                         print.fit = FALSE,
                         print.data = FALSE)

TEV

# Perform theory evaluation with fit:
TEV <- theory.evaluation(theory = TCUSTOM, 
                         empirics = C_int,
                         outcome = "CUSTOM",
                         print.fit = TRUE,
                         print.data = FALSE)

TEV



#### 2. SMMR for Y  #### 


# Load  data:

data("PAYF")


# Analysis of outcome Y

cond_sets <-  c("HE", "GG", "AH", "HI","HW")

TT_y <- truthTable(data = PAYF,
                   outcome  = "HL",
                   conditions = cond_sets,
                   incl.cut = 0.84,
                   n.cut = 2,
                   pri.cut = 0.4,
                   sort.by = c("OUT", "incl"),
                   complete = TRUE,
                   show.cases = FALSE)
TT_y


# Produce the conservative solution

sol_yc <- minimize(TT_y, 
                   details = TRUE)
sol_yc

# Produce the parsimonious solution

sol_yp <- minimize(TT_y, 
                   details = TRUE, 
                   include = "?")
sol_yp

# Check the simplifying assumptions

sol_yp$SA

# Plot the parsimonious solution

pimplot(data = PAYF,
        results = sol_yp,
        outcome = "HL")

# Produce the intermediate solution
# directional expectations are: 1, 1, 1, 0, 1 

sol_yi <- minimize(TT_y, 
                   details = TRUE, 
                   include = "?",
                   dir.exp = c(HE, GG, AH, ~HI, HE))
sol_yi

# Check easy counterfactuals 

sol_yi$i.sol$C1P1$EC

# Print solution in latex format:

stargazerSol(results = sol_yi,
             outcome = "HL")

# Plot the intermediate solution 

par(mfrow=c(2, 2))
pimplot(data = PAYF,
        results = sol_yi,
        outcome = "HL",
        sol = 1,
        jitter = TRUE)


#### 2.1. Single case SMMR for Y  #### 

# Identify typical cases for single within-case analysis:

typ_y <- smmr(results = sol_yi,
             outcome = "HL",
             sol = 1,
             match = FALSE,
             cases = 2,
             term = 1, 
             max_pairs = 7)
typ_y

# Identify deviant consistency cases for single within-case analysis:

dcon_y <- smmr(results = sol_yi,
              outcome = "HL",
              sol = 1,
              match = FALSE,
              cases = 3)
dcon_y

# Identify deviant coverage cases for single within-case analysis:

dcov_y <- smmr(results = sol_yi,
              outcome = "HL",
              sol = 1,
              match = FALSE,
              cases = 4)
dcov_y


# Display IIR cases

iir_y <- smmr(results = sol_yi,
             outcome = "HL",
             sol = 1,
             match = FALSE,
             cases = 5)
iir_y


#### 2.2. Comparative SMMR for Y  #### 


# Identify best-matching pair of typical and IIR cases for comparative within-case analysis:

typiir_y <- smmr(results = sol_yi,
                outcome = "HL",
                sol = 1,
                match = TRUE,
                cases = 2,
                term = 1)

typiir_y

# Identify best-matching pair of two typical cases for comparative within-case analysis:

tytyp_y <- smmr(results = sol_yi,
               outcome = "HL",
               match = TRUE,
               cases = 1,
               term = 1, 
               max_pairs = 3)
tytyp_y


# Identify best-matching pair of typical and 
# deviant consistency cases for comparative within-case analysis:	

typdcon_y <- smmr(results = sol_yi,
                 outcome = "HL",
                 sol = 1,
                 match = TRUE,
                 cases = 3)
typdcon_y

# Identify best-matching pair of deviant coverage 
# and IIR cases for comparative within-case analysis:

dcoviir_y <- smmr(results = sol_yi,
                 outcome = "HL",
                 sol = 1,
                 match = TRUE,
                 cases = 4)
dcoviir_y

