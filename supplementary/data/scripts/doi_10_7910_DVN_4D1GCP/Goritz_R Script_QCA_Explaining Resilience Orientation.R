########################################################################################################################
############
############ R-Script for Study Explaining Strong and Weak Resilience Orientation in Bioeconomy Policies
############
########################################################################################################################

#### 1.0 SETTING UP ANALYSIS ####
## 1.1 Installing / Loading Packages and Defining a Working Directory ####
## Before we can start running any analysis in R, we need to go through several preparational steps

## EXPEP 1: INEXPALLING PACKAGES ##
# We use 'QCA' and 'SetMethods' to perform QCA in RStudio. Therefore we install these two packages using the command 'install.packages'. If we set dependencies = TRUE, additional packages that are required for the selected packages are also installed in one step, making sure that all functions run properly.
install.packages(c("Rtools", "sp", "raster", "rgdal", "dplyr"))
install.packages(c("QCA"), dependencies = TRUE)
#install.packages(c("SetMethods"), dependencies = TRUE)
# You need to change your file path here.
install.packages("YOUR DIRECTORY/SetMethods_3.0.tar.gz", repos = NULL, type="source")

## EXPEP 2: LOADING PACKAGES ##
# Once we have downloaded all packages, you need to load them before they can be used. This has to be done everytime you reopen RStudio since RStudio works like a library: Similar to the fact that you need to take the books from your shelf onto your desk in order to work with them, you need to load the packages so that you can work with them.
# To load packages, use the command 'library'
library (sp)
library (rgdal)
library (raster)
library(dplyr)
library(QCA); library(SetMethods)

## EXPEP 3: DEFINING WORKING DIRECTORY (WD) ##
setwd("YOUR DIRECTORY")
getwd()
list.files()

#Import Input Data from Stata
library(haven)
ROD <- read_dta("ResilienceOrientationData.dta")
View(ROD)

write.csv2(ROD, file = "ROD.csv")
ROD <- read.csv2("ROD.csv")
View(ROD)


# Detecting missings
is.na(ROD)


## 2. DIRECT CALIBRATION ##
ROD$RES  <- calibrate(ROD$GEN_ResilOrient_comb, type = "fuzzy", thresholds = "e=27.6 , c=47.59, i=64", logistic = TRUE)

ROD$COR <- calibrate(ROD$ti_cpi, type = "fuzzy", thresholds = "e=14, c=52, i=70", logistic = TRUE)

ROD$GDP <- calibrate(ROD$wdi_gdpcapcon2010, type = "fuzzy", thresholds = "e=1046, c=12696, i=43324", logistic = TRUE)

ROD$ARA <- calibrate(ROD$fao_luagrara, type = "fuzzy", thresholds = "e=9.96, c=51, i=87", logistic = TRUE)

ROD$OIL <- calibrate(ROD$wdi_oilrent, type = "fuzzy", thresholds = "e=0  , c=1.34, i=5", logistic = TRUE)

ROD$EXP <- calibrate(ROD$NDGAIN_exposure, type = "fuzzy", thresholds = "e= .38  , c=.45, i=.52", logistic = TRUE)

#For Robustness Checks#
ROD$COR_Rob <- calibrate(ROD$ti_cpi, type = "fuzzy", thresholds = "e=10, c=40, i=60", logistic = TRUE)

ROD$GDP_Rob <- calibrate(ROD$wdi_gdpcapcon2010, type = "fuzzy", thresholds = "e=500, c=10000, i=40000", logistic = TRUE)

ROD$ARA_Rob <- calibrate(ROD$fao_luagrara, type = "fuzzy", thresholds = "e=5, c=48, i=90", logistic = TRUE)

ROD$OIL_Rob <- calibrate(ROD$wdi_oilrent, type = "fuzzy", thresholds = "e=0  , c=3, i=9", logistic = TRUE)

ROD$EXP_Rob <- calibrate(ROD$NDGAIN_exposure, type = "fuzzy", thresholds = "e= .25  , c=.4, i=.6", logistic = TRUE)


# This object can then be used to calculate the all possible superset relations both presence and absence for the presence of the outcome.
RES <- ROD$RES
COR <- ROD$COR
GDP <- ROD$GDP
ARA <- ROD$ARA
OIL <- ROD$OIL
EXP <- ROD$EXP
COR_Rob <- ROD$COR_Rob
GDP_Rob <- ROD$GDP_Rob
ARA_Rob <- ROD$ARA_Rob
OIL_Rob <- ROD$OIL_Rob
EXP_Rob <- ROD$EXP_Rob


## 6.0 ANALYSIS OF SUBSETS / SUFFICIENCY ####
# In the following section, we demonstrate how to perform an analysis of subset relations in QCA. To this end, we start with the construction and interpretation of the truth table. 
#From there,we go on explaining different minimization strategies. 
#Section 7 then goes beyond the standard template by exploring additional problems that might occur throughout your analysis, and which are important to check for if you do not want to risk drawing flawed or even incorrect inferences. 
#These pitfalls include, inter alia, i) potential contradictions with the analysis of necessary conditions, ii) simultaneous subset relations, and iii) exploring model ambiguities.


### OUTCOME: RES ; CONDITIONS: COR, ARA, GDP, EXP, OIL = CAGEO2 (with 0.79 incl.cut)
#Create a base Truth Table
ttRES_CAGEO2 <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = FALSE)
# Look at it with DCC
ttRES_CAGEO2dcc <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = TRUE)
ttRES_CAGEO2dcc
#--> No strong DCC
#Further SSROC check
findRows(obj = ttRES_CAGEO2, type = 3)
#Nothing found
# I looked at all OUT = 1 & n >= 2 and then decided to code 0 those cases that had dcc/n >= 50%: -->13 --> I took these out manually, since not possible with QCA package
#ttRES_CAGEO2$tt['13', 'OUT'] <- 0


#Identify SSRLR
findRows(obj = ttRES_CAGEO2, type = 2)
SSRLR <- findRows(obj = ttRES_CAGEO2, type = 2)

#based on the above analysis, exclue SSRLR from truth table
ttRES_CAGEO2_SSRLR <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", show.cases=TRUE, dcc = FALSE, exclude = SSRLR)
ttRES_CAGEO2_SSRLR
#also exclude 13 as above
#ttRES_CAGEO2_SSRLR$tt['13', 'OUT'] <- 0


write.csv(ttRES_CAGEO2_SSRLR$tt, "ttRES_CAGEO2_SSRLR.csv")



## 6.2 Minimizing a Truth Table

## CONSERVATIVE STRATEGY ## (non-enhanced, per definition, as per Adrian Dusa email)
csRES_CAGEO2 <- minimize(ttRES_CAGEO2_SSRLR, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=TRUE, use.tilde=TRUE)
csRES_CAGEO2

stargazerSol(results = csRES_CAGEO2, outcome = "RES",
             type = "html", show.cases=TRUE, out = "csRES_CAGEO2.html")


## ENHANCED MOST PARSIMONIOUS SOLUTION
epsRES_CAGEO2 <- minimize(ttRES_CAGEO2_SSRLR, include="?", details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=TRUE, use.tilde=TRUE)
epsRES_CAGEO2

#Solution (here: "csOUTCOME")
#stargazerSol(results = csOUTCOME, outcome = "INFL",
 #            type = "html", show.cases=TRUE, out = "csOUTCOME.html")



#### ROBUSTNESS PROTOCOL ####
###Step 2: Determine the Sensitivity Ranges (using different steps and max.runs for the same condition at some points) ####
rob.calibrange(raw.data = ROD,
               calib.data = ROD,
               test.cond.raw = "ti_cpi",
               test.cond.calib = "COR",
               test.thresholds =c(14,52,70),
               type = "fuzzy",
               step = 1,
               max.runs = 40,
               outcome = "RES",
               conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
               incl.cut = 0.79,
               n.cut = 2,
               pri.cut = 0.6)

#Exclusion:  Lower bound  14 Threshold  14 Upper bound  30 
#Crossover:  Lower bound  52 Threshold  52 Upper bound  58 
#Inclusion:  Lower bound  70 Threshold  70 Upper bound  NA 

rob.calibrange(raw.data = ROD,
               calib.data = ROD,
               test.cond.raw = "wdi_gdpcapcon2010",
               test.cond.calib = "GDP",
               test.thresholds =c(1046,12696,43324),
               type = "fuzzy",
               step = 50,
               max.runs = 3000,
               outcome = "RES",
               conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
               incl.cut = 0.79,
               n.cut = 2,
               pri.cut = 0.6)

#Exclusion:  Lower bound  1041 Threshold  1046 Upper bound  2266 
#Crossover:  Lower bound  12695 Threshold  12696 Upper bound  26416
#Inclusion:  Lower bound  43324 Threshold  43324 Upper bound  142224 

rob.calibrange(raw.data = ROD,
               calib.data = ROD,
               test.cond.raw = "fao_luagrara",
               test.cond.calib = "ARA",
               test.thresholds =c(9.96,51,87),
               type = "fuzzy",
               step = 0.1,
               max.runs = 1000,
               outcome = "RES",
               conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
               incl.cut = 0.79,
               n.cut = 2,
               pri.cut = 0.6)

#Exclusion:  Lower bound  9.46 Threshold  9.96 Upper bound  9.96 
#Crossover:  Lower bound  51 Threshold  51 Upper bound  66.5 
#Inclusion:  Lower bound  87 Threshold  87 Upper bound  138.099999999997 

rob.calibrange(raw.data = ROD,
               calib.data = ROD,
               test.cond.raw = "wdi_oilrent",
               test.cond.calib = "OIL",
               test.thresholds =c(0,1.34,5),
               type = "fuzzy",
               step = 0.1,
               max.runs = 300,
               outcome = "RES",
               conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
               incl.cut = 0.79,
               n.cut = 2,
               pri.cut = 0.6)

#Exclusion:  Lower bound  -1 Threshold  0 Upper bound  1.3 
#Crossover:  Lower bound  1.34 Threshold  1.34 Upper bound  2.54 
#Inclusion:  Lower bound  5 Threshold  5 Upper bound  7.89999999999999 


#PROBLEMATIC CASE
rob.calibrange(raw.data = ROD,
               calib.data = ROD,
               test.cond.raw = "NDGAIN_exposure",
               test.cond.calib = "EXP",
               test.thresholds =c(.38,.45,.52),
               type = "fuzzy",
               step = .1,
               max.runs = 20,
               outcome = "RES",
               conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
               incl.cut = 0.79,
               n.cut = 2,
               pri.cut = 0.6)

#Exclusion:  Lower bound  0.28 Threshold  0.38 Upper bound  0.38 
#Crossover:  Lower bound  0.45 Threshold  0.45 Upper bound  0.45 
#Inclusion:  Lower bound  0.52 Threshold  0.52 Upper bound  NA 




#Inclusion Consistency
rob.inclrange(data = ROD,
              step = .01,
              max.runs = 100,
              outcome = "RES",
              conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
              incl.cut = 0.79,
              n.cut = 2,
              pri.cut = 0.6)
#Raw Consistency T.:  Lower bound  0.01 Threshold  0.79 Upper bound  0.79 


rob.ncutrange(data = ROD,
              step = 1,
              max.runs = 10,
              outcome = "RES",
              conditions = c("COR", "ARA", "GDP", "EXP", "OIL"),
              incl.cut = 0.79,
              n.cut = 2,
              pri.cut = 0.6)
#N.Cut:  Lower bound  1 Threshold  2 Upper bound  2 



#Step 3: Produce Alternative Solutions, Taking Into Consideration the Sensitivity Range Analysis and Conceptually Plausible Changes in the Hard Test Range  ####

#### TS1 (n.cut = 1) ####
#Create a base Truth Table
ttRES_TS1 <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=1, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = FALSE)
# Look at it with DCC
ttRES_TS1dcc <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=1, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = TRUE)
ttRES_TS1dcc
#--> No strong DCC
#Further SSROC check
findRows(obj = ttRES_TS1, type = 3)
#Nothing found
# I looked at all OUT = 1 & n >= 2 and then decided to code 0 those cases that had dcc/n >= 50%: -->13 --> I took these out manually, since not possible with QCA package
#ttRES_TS1$tt['13', 'OUT'] <- 0


#Identify SSRLR
findRows(obj = ttRES_TS1, type = 2)
SSRLR <- findRows(obj = ttRES_TS1, type = 2)

#based on the above analysis, exclue SSRLR from truth table
ttRES_TS1_SSRLR <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=1, pri.cut=0.5, sort.by="OUT, incl, n", show.cases=TRUE, dcc = FALSE, exclude = SSRLR)
ttRES_TS1_SSRLR
#also exclude 13 as above
#ttRES_TS1_SSRLR$tt['13', 'OUT'] <- 0


## 6.2 Minimizing a Truth Table

## CONSERVATIVE STRATEGY ## (non-enhanced, per definition, as per Adrian Dusa email)
csRES_TS1 <- minimize(ttRES_TS1_SSRLR, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=TRUE, use.tilde=TRUE)
csRES_TS1






#### TS2 (incl.cut=0.9) ####
#Create a base Truth Table
ttRES_TS2 <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.9, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = FALSE)
# Look at it with DCC
ttRES_TS2dcc <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.9, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = TRUE)
ttRES_TS2dcc
#--> No strong DCC
#Further SSROC check
findRows(obj = ttRES_TS2, type = 3)
#Nothing found
# I looked at all OUT = 1 & n >= 2 and then decided to code 0 those cases that had dcc/n >= 50%: -->13 --> I took these out manually, since not possible with QCA package
#ttRES_TS2$tt['13', 'OUT'] <- 0


#Identify SSRLR
findRows(obj = ttRES_TS2, type = 2)
SSRLR <- findRows(obj = ttRES_TS2, type = 2)

#based on the above analysis, exclue SSRLR from truth table
ttRES_TS2_SSRLR <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.9, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", show.cases=TRUE, dcc = FALSE, exclude = SSRLR)
ttRES_TS2_SSRLR
#also exclude 13 as above
#ttRES_TS2_SSRLR$tt['13', 'OUT'] <- 0


## 6.2 Minimizing a Truth Table

## CONSERVATIVE STRATEGY ## (non-enhanced, per definition, as per Adrian Dusa email)
csRES_TS2 <- minimize(ttRES_TS2_SSRLR, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=TRUE, use.tilde=TRUE)
csRES_TS2




#### TS2 (incl.cut=0.79) ####
#Create a base Truth Table
ttRES_TS2 <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = FALSE)
# Look at it with DCC
ttRES_TS2dcc <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", complete=TRUE, show.cases=TRUE, dcc = TRUE)
ttRES_TS2dcc
#--> No strong DCC
#Further SSROC check
findRows(obj = ttRES_TS2, type = 3)
#Nothing found
# I looked at all OUT = 1 & n >= 2 and then decided to code 0 those cases that had dcc/n >= 50%: -->13 --> I took these out manually, since not possible with QCA package
#ttRES_TS2$tt['13', 'OUT'] <- 0


#Identify SSRLR
findRows(obj = ttRES_TS2, type = 2)
SSRLR <- findRows(obj = ttRES_TS2, type = 2)

#based on the above analysis, exclue SSRLR from truth table
ttRES_TS2_SSRLR <- truthTable(data=ROD, outcome = "RES", conditions = "COR, ARA, GDP, EXP, OIL", incl.cut=0.79, n.cut=2, pri.cut=0.5, sort.by="OUT, incl, n", show.cases=TRUE, dcc = FALSE, exclude = SSRLR)
ttRES_TS2_SSRLR
#also exclude 13 as above
#ttRES_TS2_SSRLR$tt['13', 'OUT'] <- 0


## 6.2 Minimizing a Truth Table

## CONSERVATIVE STRATEGY ## (non-enhanced, per definition, as per Adrian Dusa email)
csRES_TS2 <- minimize(ttRES_TS2_SSRLR, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=TRUE, use.tilde=TRUE)
csRES_TS2
