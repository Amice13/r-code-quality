#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 
#                          

#~~~~~~~~~~~~~~   Independent Exercise on a full QCA    ~~~~~~~~~~~~~~~~~~


# Load the packages:

library(SetMethods)
library(QCA)

# The data comes from Paykani, Toktam, Rafiey, Hassan, Sajjadi, Homeira (2018)
# "A fuzzy set qualitative comparative analysis of 131 countries: Which
# configuration of the structural conditions can explain health better?"
# [https://pubmed.ncbi.nlm.nih.gov/29357889/]

# Outcome: 
# HL = High life expectancy
# LL = Low life expectancy

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

# Check the skewness of the data for the 5 conditions and the outcome:

# Check whether there are any ambiguous cases with set-membership scores of 0.5
# in any of the 5 conditions and the outcome:


#### 2. NECESSITY ANALYSIS FOR THE PRESENCE OF THE OUTCOME ####

# Check whether we have any single necessary conditions for outcome HL:


# Display SUIN conditions, setting the inclusion cutoff to 0.9,
# coverage cutoff to 0.6, and RoN to 0.5. Display only the combinations
# of a maximum of two conditions.


# Plot the results:


#### 3. SUFFICIENCY ANALYSIS FOR THE PRESENCE OF THE OUTCOME ####

# Truth Table
# Create the truth table, setting the inclusion cutoff to 0.84 and the
# number of cases needed to include a row in the minimization to 1.


# Logical Minimization 
# Produce the conservative solution:


# Produce the most parsimonious solution:


# Check the simplifying assumptions:



# Produce the intermediate solution with the following 
# directional expactations 1, 1, 1, 0, 1


# Check the easy counterfactuals


# Plot the intermediate solution



#### 4. NECESSITY ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Analyze necessary conditions for outcome ~Y using the superSubset
# function. Set inclusion cutoff to 0.908, depth to 2, coverage
# cutoff to 0.8 and RoN to 0.8.

# Plot the results

#### 5. SUFFICIENCY ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Truth Table
# Create a truth table setting the inclusion cutoff to 0.9 and the
# number of cases needed to include a row in the analysis to 2.


# Logica Minimization 
# Produce the most parsimonious solution:


# Check the simplifying assumptions:


# Produce the intermediate solution with the following 
# direcional expectations 0, 0, 0, 1, 0:


# Check the easy counterfactuals 

# Look at the PI chart



#### 6. ENHANCED STANDARD ANALYSIS FOR THE ABSENCE OF THE OUTCOME ####

# Assumptions contradicting necessity

# Let's use the intermediate solution 

# From SUIN_ny, we know that the expression ~HE+~GG contradicts
# the statement of necessity.

# Does any of the simplifying assumptions for the most parsimonious
# solution contradict the statement of necessity?  

# Create a new truth table which will set such rows to OUT = 0,
# using the esa function


# Rerun the analysis of sufficiency producing the ENHANCED
# intermediate solution for ~Y with the following directional
# expectations: 0,0,0,1,0


# Contradictory easy counterfactuals 
# Check if we are making contradictory easy counterfactuals 
# Put them in an object named CEC 


# Simultaneous subset relations 
# Now check if one or more rows were included into the minimization for both
# outcome Y and ~Y 
# Use the intersect command, create a new object SSR


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


# Produce the enhanced intermediate solution:



#### 7. THEORY EVALUATION ####


# Let us assume that according to our theory,  
# good governance (GG) and affluent health system (AH) or
# low income inequality (~HI) lead to high life expectancy (HL)

# Intersect theory with the most parsimonious solution:



#### 8. ROBUSTNESS ####

# Sensitivity ranges:

# Find the raw consistency and the frequency cut sensitivity ranges
# (for Y):


# Load the raw data:
data("PAYR")
head(PAYR)

# Creating the test solutions:

# Alter consistency to 0.8 and inclusion cutoff to 2. Create a new
# object TS1.


# Alter calibration for the condition HW, using the condition WEAL
# in the raw dataset and putting thresholds at 4000, 25000, and 45000


# Create object called TS2. This should be the most parsimonious solution
# using parameters from the very first truth table (sufficiency analysis
# for the presence of the outcome)


# Save the most parsimonious solution for the presence of the 
# outcome as a new object IS:


# Create the test set in a list:


# Calculate robustness fit parameters:

# Identify types of robustness relevant cases and robustness case parameters:

#### 9. SMMR Y ####

# Print the typical cases for each focal conjunct in the
# second sufficient term of the intermediate solution


# Show the typical - IIR cases for each term, using the intermediate
# solution:


#### 10. SMMR ~Y ####

# Display the deviant cases coverage, using the intermediate
# solution:


# Using the intermediate solution, show the best pairs of
# Deviant Coverage - IIR cases in each TT row:



####  11. CLUSTER ~Y ####

# We use the enhanced intermediate solution for the negated outcome

# Perform cluster diagnostics for the enhanced intermediate
# solution with "REGION" as clusters and "COUNTRY" as units:


# Plot between consistencies: 

