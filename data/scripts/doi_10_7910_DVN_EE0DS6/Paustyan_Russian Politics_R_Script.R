### Replication script to Paustyan (2021):
### A Treaty for the Rich and Politically Loyal?
### Explaining the Bilateral Center-Region Treaties in Post-Soviet Russia,
### Russian Politics (forthoming)       

# Remove everything from the working environment:
rm(list=ls())

# Set your working directory: 
setwd()
getwd()

# Load the packages:
library(QCA)
library(SetMethods)

# Load raw data:
DT <- read.csv("raw_data.csv", row.names = 1, sep=",")
head(DT)

# Calibration ==== 

#SOV#

# Check the distribution of the raw data
hist(DT$SOV_raw,
     xlab = "SOV_raw",
     main = paste("Histogram of the raw SOV scores"))

# To calibrate the set of the regions demanding sovereignity (SOV)
# I use the following threshold values:
# 0 - 0; 
# 1,2,3 - 0.33;
# 4 - 0.67; 
# 5 - 1 

SOV <- NA 
SOV[DT$SOV_raw<=0]<-0
SOV[DT$SOV_raw>0 & DT$SOV_raw<=3]<-0.33
SOV[DT$SOV_raw>3 & DT$SOV_raw<=4]<-0.67
SOV[DT$SOV_raw>4 & DT$SOV_raw<=5]<-1
SOV

# To add the new calibrated set to the data frame:
DT$SOV<-SOV
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$SOV,
     xlab = "SOV",
     main = paste("Histogram of the fuzzy set SOV scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$SOV_raw, DT$SOV)


# ELECT #

# Check the distribution of the raw data
hist(DT$ELECT_raw,
     xlab = "ELECT_raw",
     main = paste("Histogram of the raw ELECT scores"))

# # To calibrate the set of the regions having elected executive (ELECT)
# I use the following threshold values:
# 1 - 0; 
# 2 - 0.33;
# 3 - 0.67; 
# 4 - 1  

ELECT <- NA 
ELECT[DT$ELECT_raw<=1]<-0
ELECT[DT$ELECT_raw>1 & DT$ELECT_raw<=2]<-0.33
ELECT[DT$ELECT_raw>2 & DT$ELECT_raw<=3]<-0.67
ELECT[DT$ELECT_raw>3 & DT$ELECT_raw<=4]<-1
ELECT

# To add the new calibrated set to the data frame:
DT$ELECT<-ELECT
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$ELECT,
     xlab = "ELECT",
     main = paste("Histogram of the fuzzy set ELECT scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$ELECT_raw, DT$ELECT)


# VOT #

# Check the distribution of the raw data
hist(DT$VOT_raw,
     xlab = "VOT_raw",
     main = paste("Histogram of the raw VOT scores"))

# To calibrate the set of the regions voting for Yelstin 
# in the 1996 presidential election (VOT)
# I use the following threshold values:
# 1 - 0; 
# 2 - 0.33;
# 3 - 0.67; 
# 4 - 1

VOT <- NA 
VOT[DT$VOT_raw<=1]<-0
VOT[DT$VOT_raw>1 & DT$VOT_raw<=2]<-0.33
VOT[DT$VOT_raw>2 & DT$VOT_raw<=3]<-0.67
VOT[DT$VOT_raw>3 & DT$VOT_raw<=4]<-1
VOT

# To add the new calibrated set to the data frame:
DT$VOT<-VOT
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$VOT,
     xlab = "VOT",
     main = paste("Histogram of the fuzzy set VOT scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$VOT_raw, DT$VOT)


# Observation: none of the cases has a score of 0.33


# DON # 

# Check the distribution of the raw data
hist(DT$DON_raw,
     xlab = "DON_raw",
     main = paste("Histogram of the raw DON scores"))

# To calibrate the set of donor regions (DON)
# I use the following threshold values:
# 0 - 0; 
# 1,2 - 0.33;
# 3,4 - 0.67; 
# 5 - 1 

DON <- NA 
DON[DT$DON_raw<=0]<-0
DON[DT$DON_raw>0 & DT$DON_raw<=2]<-0.33
DON[DT$DON_raw>2 & DT$DON_raw<=4]<-0.67
DON[DT$DON_raw>4 & DT$DON_raw<=5]<-1
DON

# To add the new calibrated set to the data frame:
DT$DON<-DON
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$DON,
     xlab = "DON",
     main = paste("Histogram of the fuzzy set DON scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$DON_raw, DT$DON)

# OUT # 

# Check the distribution of the raw data
hist(DT$OUT_raw,
     xlab = "OUT_raw",
     main = paste("Histogram of the raw OUT scores"))

# To calibrate the outcome set (OUT)
# I use the following threshold values:
# 1 - 0;
# 2 - 0.33;
# 3 - 0.67;
# 4 - 1

OUT <- NA 
OUT[DT$OUT_raw<=1]<-0
OUT[DT$OUT_raw>1 & DT$OUT_raw<=2]<-0.33
OUT[DT$OUT_raw>2 & DT$OUT_raw<=3]<-0.67
OUT[DT$OUT_raw>3 & DT$OUT_raw<=4]<-1
OUT

# To add the new calibrated set to the data frame:
DT$OUT<-OUT
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$OUT,
     xlab = "OUT",
     main = paste("Histogram of the fuzzy set OUT scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$OUT_raw, DT$OUT)

head(DT)

# To remove columns with raw data
DT <- DT[,-c(1:5)]
head(DT)

# Examine skewness of the data:
skew.check(DT)

# Save calibrated data set as a csv file
write.csv(DT, "calibrated_data.csv")

# Outcome: a signed treaty ----

rm(list=ls())

DT <- read.csv("calibrated_data.csv", row.names = 1, sep=",")
head(DT)

# Analysis of necessity ----

QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE)

# No necessary conditions 

SUIN_y <- superSubset(data = DT, 
                      outcome = "OUT",
                      conditions = c("SOV", "ELECT", "VOT", "DON"),
                      relation = "necessity",
                      incl.cut = 0.90,
                      cov.cut = 0.6,
                      ron.cut = 0.5,
                      depth = 2)
SUIN_y

# sov+DON 
# as this combination does not stand for any higher-order concept,
# it is not interpreted substantially 

# Analysis of sufficiency ----

# To create a truth table

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = .91,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# 0.91 is a meaningful cut-off for consistency 


# Conservative solution 

sol_c <- minimize(TT, details = TRUE, 
                  show.cases = TRUE, 
                  use.tilde=FALSE)
sol_c

# XY-plot 

pimplot(data = DT,
        results = sol_c,
        outcome = "OUT",
        all_labels = TRUE,
        jitter = TRUE)

# Most parsimonious solution 
sol_p <- minimize(TT, details = TRUE, include = "?", show.cases = TRUE)
sol_p

sol_p$SA

# It is identical to conservative, 
# as there are no simplifying assumptions

# Intermediate solution
sol_i <- minimize(TT, details = TRUE, include = "?",  
                  dir.exp = c(1,1,1,1),
                  show.cases = TRUE)

sol_i

sol_i$i.sol$C1P1$EC

# It is identical to conservative, 
# as there are no easy counterfactuals

# Typical cases

typ_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 1,
              term = 1)

typ_y

# Deviant cases
dev_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 3,
              term = 1)

dev_y

# Outcome: no signed treaty ----

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE, neg.out = TRUE)

# No necessary conditions

SUIN_ny <- superSubset(data = DT, 
                       outcome = "~OUT",
                       conditions = c("SOV", "ELECT", "VOT", "DON"),
                       relation = "necessity",
                       incl.cut = 0.90,
                       cov.cut = 0.6,
                       ron.cut = 0.5,
                       depth = 2)

SUIN_ny

# No necessary disdjunctions

# Analysis of sufficiency ----

# To create a truth table

TT_n <- truthTable(DT, outcome = "OUT", neg.out = TRUE,
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.90,
                   pri.cut = 0.50,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))


# 0.90 is a consistency threshold, 0.50 is a PRI threshold                 
TT_n


sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE)



sol_c_n

# XY-plot 

pimplot(data = DT,
        results = sol_c_n,
        outcome = "~OUT",
        all_labels = TRUE,
        jitter = TRUE)

# Typical cases
typ_y_n <- smmr(results = sol_c_n,
                outcome = "~OUT" ,
                sol = 1 ,
                match = FALSE,
                cases = 1,
                term = 1)

typ_y_n

# Deviant cases
dev_y_n <- smmr(results = sol_c_n,
                outcome = "~OUT" ,
                sol = 1 ,
                match = FALSE,
                cases = 3,
                term = 1)

dev_y_n

# Parsimonious solution
sol_p_n <- minimize(TT_n, 
                    details = TRUE, 
                    include = "?", 
                    row.dom = TRUE, 
                    show.cases = TRUE,
                    exclude = c("14"))

# Row 14 (1101) is excluded because the assumption is that
# at least two conditions should be absent to produce the absence of the outcome, no signed treaty

sol_p_n 
sol_p_n$SA

# Row 2, 0001, is a simplifying assumption

# Typical cases
typ_y <- smmr(results = sol_p_n,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 1,
              term = 1)

typ_y

# Deviant cases
dev_y <- smmr(results = sol_p_n,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 3,
              term = 1)
dev_y

# Intermediate solution 
sol_i_n <- minimize(TT_n, details = TRUE, include = "?", 
                    show.cases = TRUE, 
                    dir.exp = c(0,0, 0, 0),
                    exclude = c("14"))

sol_i_n
sol_i_n$i.sol$C1P1$EC

# Raw 2 represents a simplifying assumption as well as an easy counterfactual,
# therefore, parsimonious and intermediate solutions look identical 

# Check for simultaneous subset relations
SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR
# none

# Check for any contradictory simplifying assumptions
CSA <- intersect(rownames(sol_p$i.sol$sol_p$C1P1$SA), rownames(sol_p_n$i.sol$C1P1$sol_p_n$SA))
CSA 
# none

# Check for any contradictory easy counterfactuals
CEC <- intersect(rownames(sol_i$i.sol$C1P1$EC), rownames(sol_i_n$i.sol$C1P1$EC))
CEC

# none

# Robustness Test 1: Direct calibration ----

rm(list=ls())

# Load raw data:
DT <- read.csv("raw_rob1.csv", row.names = 1, sep=",")
head(DT)

# The anchors are the following (fully out/cross over/fully in):
# 0, 2.5, 5,   # SOV
# 1, 2.5, 4,   # ELECT
# 1, 2.5, 4,   # VOT
# 0, 2.5, 5,   # DON
# 0, 0.4, 1,   # OUT

# SOV #
SOV <- calibrate(DT$SOV_raw, type = "fuzzy", thresholds = c(0, 2.5, 5))
SOV

# To add the new calibrated set to the data frame:
DT$SOV<-SOV
head(DT)

# To visualize the fuzzy set scores using a histogram:
hist(DT$SOV)

# To plot the raw scores against the fuzzy set scores:
plot(DT$SOV_raw, DT$SOV)

# ELECT #
ELECT <- calibrate(DT$ELECT_raw, type = "fuzzy", thresholds = c(1, 2.5, 4))
ELECT

# To add the new calibrated set to the data frame:
DT$ELECT<-ELECT
head(DT)

# To visualize the fuzzy set scores using a histogram:
hist(DT$ELECT)

# To plot the raw scores against the fuzzy set scores:
plot(DT$ELECT_raw, DT$ELECT)

# VOT #
VOT <- calibrate(DT$VOT_raw, type = "fuzzy", thresholds = c(1, 2.5, 4))
VOT

# To add the new calibrated set to the data frame:
DT$VOT<-VOT
head(DT)

# To visualize the fuzzy set using a histogram:
hist(DT$VOT)

# To plot the raw scores against the fuzzy set scores:
plot(DT$VOT_raw, DT$VOT)

# DON #
DON <- calibrate(DT$DON_raw, type = "fuzzy", thresholds = c(0, 2.5, 5))
DON

# To add the new calibrated set to the data frame:
DT$DON<-DON
head(DT)

# To visualize the fuzzy set scores using a histogram:
hist(DT$DON)

# To plot the raw scores against the fuzzy set scores:
plot(DT$DON_raw, DT$DON)

# OUT # 
OUT <- calibrate(DT$OUT_raw, type = "fuzzy", thresholds = c(0, 0.4, 1))
OUT

# To add the new calibrated set to the data frame:
DT$OUT<-OUT
head(DT)

# and to visualize the fuzzy set using a histogram:
hist(DT$OUT)

# To plot the raw scores against the fuzzy set scores:
plot(DT$OUT_raw, DT$OUT)

# To round
DT <- round(DT, digits=2)
head(DT)

# To remove columns with raw data
dt <- DT[,-c(1:5)]
head(dt)

skew.check(dt)

# To save the calibrated dataset
write.csv(dt, "calibrated_rob1.csv")

### Outcome: a signed treaty  ----

rm(list=ls())

# Load calibrated data:
DT <- read.csv("calibrated_rob1.csv", row.names = 1, sep=",")
head(DT)

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE)

# No necessary conditions 

SUIN_y <- superSubset(data = DT, 
                      outcome = "OUT",
                      conditions = c("SOV", "ELECT", "VOT", "DON"),
                      relation = "necessity",
                      incl.cut = 0.90,
                      cov.cut = 0.6,
                      ron.cut = 0.5,
                      depth = 2)
SUIN_y

# no 

# Analysis of sufficiency ----

# To create a truth table

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = .80,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# 0.80 is a meaningful cut-off for consistency

# Conservative solution

sol_c <- minimize(TT, details = TRUE, 
                  show.cases = TRUE, 
                  use.tilde=FALSE)


sol_c

# XY-plot 

pimplot(data = DT,
        results = sol_c,
        outcome = "OUT",
        all_labels = TRUE,
        jitter = TRUE)

# Parsimonious solution 
sol_p <- minimize(TT, details = TRUE, include = "?", show.cases = TRUE)
sol_p

# To check simplifying assumptions
sol_p$SA

# It is identical to conservative, 
# as there are no simplifying assumptions

# Intermediate solution
sol_i <- minimize(TT, details = TRUE, include = "?", 
                  show.cases = TRUE, 
                  dir.exp = c(1,1, 1, 1))
sol_i

# To check easy counterfactuals
sol_i$i.sol$C1P1$EC

# It is identical to conservative, 
# as there are no either simplifying assumptions or easy counterfactuals


# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 1,
              term = 1)

typ_y

# Deviant cases
dev_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 3,
              term = 1)
dev_y

# Outcome: no signed treaty ----

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE, neg.out = TRUE)

# No necessary conditions
# ~DON comes closer to 0.9, but has low relevance of only 0.405

SUIN_ny <- superSubset(data = DT, 
                       outcome = "~OUT",
                       conditions = c("SOV", "ELECT", "VOT", "DON"),
                       relation = "necessity",
                       incl.cut = 0.90,
                       cov.cut = 0.6,
                       ron.cut = 0.5,
                       depth = 2)

SUIN_ny

# No necessary disdjunctions

# Analysis of sufficiency ----

# To create a truth table

TT_n <- truthTable(DT, outcome = "OUT", neg.out = TRUE,
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.80,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))
TT_n

# Conservative solution 
sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE)


sol_c_n

# Parsimonious solution 
sol_p_n <- minimize(TT_n, 
                    details = TRUE, 
                    include = "?", 
                    row.dom = TRUE, 
                    show.cases = TRUE,
                    exclude = "14")


sol_p_n 

sol_p_n$SA
# Row 2

# Intermediate solution 
sol_i_n <- minimize(TT_n, details = TRUE, include = "?", 
                    show.cases = TRUE, 
                    dir.exp = c(0,0, 0, 0),
                    exclude = "14")

# Row 14 is excluded

sol_i_n
sol_i_n$i.sol$C1P1$EC
# Row 2

# Check for simultaneous subset relations
SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR
# none

# Check for any contradictory simplifying assumptions
CSA <- intersect(rownames(sol_p$i.sol$sol_p$C1P1$SA), rownames(sol_p_n$i.sol$C1P1$sol_p_n$SA))
CSA 
# none

# Check for any contradictory easy counterfactuals
CEC <- intersect(rownames(sol_i$i.sol$C1P1$EC), rownames(sol_i_n$i.sol$C1P1$EC))
CEC
# none

# Robustness Test 2: 57 cases ----

rm(list=ls())

# Load calibrated data:
DT <- read.csv("calibrated_rob2.csv", row.names = 1, sep=",")
head(DT)

# Outcome: a signed treaty ----

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE)

# No necessary conditions 

SUIN_y <- superSubset(data = DT, 
                      outcome = "OUT",
                      conditions = c("SOV", "ELECT", "VOT", "DON"),
                      relation = "necessity",
                      incl.cut = 0.90,
                      cov.cut = 0.6,
                      ron.cut = 0.5,
                      depth = 2)
SUIN_y

# no 

# Analysis of sufficiency ----

# To create a truth table

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = .80,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# 0.80 is a meaningful cut-off for consistency

# Conservative solution 

sol_c <- minimize(TT, details = TRUE, 
                  show.cases = TRUE, 
                  use.tilde=FALSE)


sol_c

# XY-plot 

pimplot(data = DT,
        results = sol_c,
        outcome = "OUT",
        all_labels = TRUE,
        jitter = TRUE)

# Parsimonious solution 
sol_p <- minimize(TT, details = TRUE, include = "?", show.cases = TRUE)
sol_p

# To check simplifying assumptions
sol_p$SA

# It is identical to conservative, 
# as there are no simplifying assumptions

# Intermediate solution
sol_i <- minimize(TT, details = TRUE, include = "?", 
                  show.cases = TRUE, 
                  dir.exp = c(1,1, 1, 1))
sol_i

# To check easy counterfactuals
sol_i$i.sol$C1P1$EC

# It is identical to conservative, 
# as there are no easy counterfactuals

# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 1,
              term = 1)

typ_y

# Deviant cases
dev_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 3,
              term = 1)
dev_y

# Outcome: no signed treaty ----

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE, neg.out = TRUE)

# No necessary conditions
# ~DON comes closer to 0.9, but has low relevance of only 0.396

SUIN_ny <- superSubset(data = DT, 
                       outcome = "~OUT",
                       conditions = c("SOV", "ELECT", "VOT", "DON"),
                       relation = "necessity",
                       incl.cut = 0.90,
                       cov.cut = 0.6,
                       ron.cut = 0.5,
                       depth = 2)

SUIN_ny

# No necessary disdjunctions

# Analysis of sufficiency ----

# To create a truth table

TT_n <- truthTable(DT, outcome = "OUT", neg.out = TRUE,
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.75,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))
TT_n

# Conservative solution 
sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE)


sol_c_n

# Parsimonius solution
sol_p_n <- minimize(TT_n, 
                    details = TRUE, 
                    include = "?", 
                    row.dom = TRUE, 
                    show.cases = TRUE,
                    exclude = "14")

# Row 14 is excluded

sol_p_n 
sol_p_n$SA
# Row 2

# Intermediate solution
sol_i_n <- minimize(TT_n, details = TRUE, include = "?", 
                    show.cases = TRUE, 
                    dir.exp = c(0,0, 0, 0),
                    exclude = "14")

sol_i_n
# same as parsimonious
sol_i_n$i.sol$C1P1$EC

# Check for simultaneous subset relations
SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR
# none

# Check for any contradictory simplifying assumptions
CSA <- intersect(rownames(sol_p$i.sol$sol_p$C1P1$SA), rownames(sol_p_n$i.sol$C1P1$sol_p_n$SA))
CSA 
# none

# Check for any contradictory easy counterfactuals
CEC <- intersect(rownames(sol_i$i.sol$C1P1$EC), rownames(sol_i_n$i.sol$C1P1$EC))
CEC
# none

# Robustness Test 3: Changing consistency cut-off ---- 

rm(list=ls())

# Load calibrated data:
DT <- read.csv("calibrated_data.csv", row.names = 1, sep=",")
head(DT)

# Outcome: a signed treaty ----

# Analysis of sufficiency ----

# To create a truth table

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = .78,
                 pri.cut = 0.50,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# 0.78 is a meaningful cut-off for consistency 
# row 7 is excluded with a pri.cut set to equal to or higher than 0.50


# Conservative solution 
sol_c <- minimize(TT, 
                  details = TRUE, 
                  show.cases = TRUE)


sol_c


typ_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 1,
              term = 1)

typ_y

# Deviant cases
dev_y <- smmr(results = sol_c,
              outcome = "OUT" ,
              sol = 1 ,
              match = FALSE,
              cases = 3,
              term = 1)

dev_y

# XY-plot 

pimplot(data = DT,
        results = sol_c,
        outcome = "OUT",
        all_labels = TRUE,
        jitter = TRUE)

# Parsimonious solution 
sol_p <- minimize(TT, details = TRUE, include = "?", show.cases = TRUE, use.tilde=FALSE)
sol_p

sol_p$SA
# Row 13

# Intermediate solution
sol_i <- minimize(TT, details = TRUE, include = "?", 
                  show.cases = TRUE, 
                  dir.exp = c(1,1, 1, 1))
sol_i
sol_i$i.sol$C1P1$EC

# It is identical to parsimonious 

# Outcome: no signed treaty ----

# Analysis of sufficiency ----

# To create a truth table

TT_n <- truthTable(DT, outcome = "OUT", neg.out = TRUE,
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.75,
                   pri.cut = 0.50,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))
TT_n

sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE,
                    use.tilde = FALSE)


sol_c_n

# Parsimonious solution 
sol_p_n <- minimize(TT_n, 
                    details = TRUE, 
                    include = "?", 
                    row.dom = TRUE, 
                    show.cases = TRUE,
                    use.tilde = FALSE)

sol_p_n 

sol_p_n$SA

# Intermediate solution 
sol_i_n <- minimize(TT_n, details = TRUE, include = "?", 
                    show.cases = TRUE, 
                    dir.exp = c(0,0, 0, 0),
                    use.tilde = FALSE)

sol_i_n

sol_i_n$i.sol$C1P1$EC

# Check for simultaneous subset relations
SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR
# none

# Check for any contradictory simplifying assumptions
CSA <- intersect(rownames(sol_p$i.sol$sol_p$C1P1$SA), rownames(sol_p_n$i.sol$C1P1$sol_p_n$SA))
CSA 
# none

# Check for any contradictory easy counterfactuals
CEC <- intersect(rownames(sol_i$i.sol$C1P1$EC), rownames(sol_i_n$i.sol$C1P1$EC))
CEC
# none 

# Cluster diagnostics ---- 
rm(list=ls())

# Load calibrated data:
DT <- read.csv("calibrated_cluster.csv", row.names = 1, sep=",")
head(DT)

# Outcome: a signed treaty ----

# Analysis of sufficiency ----

# To create a truth table

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,3:6]),
                 incl.cut1 = .91,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# 0.91 is a meaningful cut-off for consistency 


# Conservative solution 

sol_c <- minimize(TT, details = TRUE, 
                  show.cases = TRUE, 
                  use.tilde=FALSE)
sol_c

# This solution is produced by the main analysis 
# Let us cluster by 'REG_TYPE'

# Get pooled, within, and between consistencies 
# for the conservative solution

cluster(DT, 
        sol_c, 
        outcome = 'OUT', 
        unit_id = "REGION",
        cluster_id = 'REG_TYPE') 

# parameters of fit look good
# no need to addd 'REG_TYPE' as a separate condition 
