### Replication script to Paustyan (2022):
### Intergovernmental bargaining in multilevel autocracies: 
### The Case of the 2018 FIFA World Cup Russia
### Territory, Politics, Governance (forthcoming)       

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

# CAP #

# Check the distribution of the raw data
hist(DT$CAP_raw,
     xlab = "CAP_raw",
     main = paste("Histogram of the raw CAP scores"))

# I use the following threshold values:
# 10 - 0; 
# 20 - 0.33;
# 35 - 0.67; 
# 40 - 1

CAP <- NA 
CAP[DT$CAP_raw<=10]<-0
CAP[DT$CAP_raw>10 & DT$CAP_raw<=20]<-0.33
CAP[DT$CAP_raw>20 & DT$CAP_raw<=35]<-0.67
CAP[DT$CAP_raw>35 & DT$CAP_raw<=70]<-1
CAP

# To add the new calibrated set to the data frame:
DT$CAP<-CAP
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$CAP,
     xlab = "CAP",
     main = paste("Histogram of the fuzzy set CAP scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$CAP_raw, DT$CAP)

head(DT)


# LOB # 

# Check the distribution of the raw data
hist(DT$LOB_raw,
     xlab = "LOB_raw",
     main = paste("Histogram of the raw LOB scores"))

# To calibrate the set 
# I use the following threshold values:
# less than or equal to 3 - 0; 
# 4,5 - 0.33;
# 6,7 - 0.67; 
# more than 8 - 1 

LOB <- NA 
LOB[DT$LOB_raw<=3]<-0
LOB[DT$LOB_raw>3 & DT$LOB_raw<=5]<-0.33
LOB[DT$LOB_raw>5 & DT$LOB_raw<=8]<-0.67
LOB[DT$LOB_raw>8 & DT$LOB_raw<=15]<-1
LOB



# To add the new calibrated set to the data frame:
DT$LOB<-LOB
head(DT)



# Visualize the the fuzzy set scores using a histogram:
hist(DT$LOB,
     xlab = "LOB",
     main = paste("Histogram of the fuzzy set LOB scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$LOB_raw, DT$LOB)


# VOT # 

# Check the distribution of the raw data
hist(DT$VOT_raw,
     xlab = "VOT_raw",
     main = paste("Histogram of the raw VOT scores"))

# To calibrate the set 
# I use the following threshold values:
# 1,2 - 0; 
# 3,4 - 0.33;
# 5,6 - 0.67; 
# 7,8 - 1 

VOT <- NA 
VOT[DT$VOT_raw<=2]<-0
VOT[DT$VOT_raw>2 & DT$VOT_raw<=4]<-0.33
VOT[DT$VOT_raw>4 & DT$VOT_raw<=6]<-0.67
VOT[DT$VOT_raw>6 & DT$VOT_raw<=8]<-1
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


# STAB #

# Check the distribution of the raw data
hist(DT$STAB_raw,
     xlab = "STAB_raw",
     main = paste("Histogram of the raw STAB scores"))

# # To calibrate the set 
# I use the following threshold values:
# more than 10 per cent - 0; 
# more than 5 per cent but less than 10 per cent - 0.33;
# more than 3 per cent but less than 5 per cent - 0.67; 
# less than or equal to 3 per cent - 1  

STAB <- NA 
STAB[DT$STAB_raw<=3]<-1
STAB[DT$STAB_raw>3 & DT$STAB_raw<=5]<-0.67
STAB[DT$STAB_raw>5 & DT$STAB_raw<=10]<-0.33
STAB[DT$STAB_raw>10 & DT$STAB_raw<=40]<-0
STAB

# To add the new calibrated set to the data frame:
DT$STAB<-STAB
head(DT)

# Visualize the the fuzzy set scores using a histogram:
hist(DT$STAB,
     xlab = "STAB",
     main = paste("Histogram of the fuzzy set STAB scores"))

# Plot the raw data against the fuzzy set scores:
plot(DT$STAB_raw, DT$STAB)


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
write.csv(DT, "calibrated.csv")

# Main analysis ----

# Remove everything from the working environment:

rm(list=ls())

# Load calibrated data:

DT <- read.csv("calibrated.csv", row.names = 1)

# Outcome: selection as a World cup venue ----
# Analysis of necessity ----

QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE)


# LOB has a consistency score of 0.965

# let us creat XY plot 
xy.plot("LOB", 
        "OUT", 
        data = DT,
        xlab="LOB", 
        ylab="OUT", 
        necessity=TRUE,
        jitter = TRUE)



SUIN_y <- superSubset(data = DT, 
                      outcome = "OUT",
                      conditions = c("VOT", "STAB", "CAP", "LOB"),
                      relation = "necessity",
                      incl.cut = 0.9,
                      cov.cut = 0.60,
                      ron.cut = 0.60,
                      depth = 2)
                      
SUIN_y

# LOB

# Analysis of sufficieny ----

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = 0.80,
                 pri.cut = 0.65,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# Provide the conservative solution:
sol_c <- minimize(TT, details = TRUE, show.cases = TRUE)
sol_c


# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y

# Deviant cases
# consistency in kind

dcon_y <- smmr(results = sol_c,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y

# KDA I

pimplot(DT, sol_c, outcome = "OUT", all_labels = TRUE, jitter=TRUE)

# Provide the parsimonious solution
sol_p <- minimize(TT, details = TRUE, 
                  include = "?", show.cases = TRUE)
                  
sol_p

# Show the simplifying assumptions used for the parsimonious solution
sol_p$SA

sol_i <- minimize(TT, details = TRUE, include = "?", 
                  row.dom = TRUE, show.cases = TRUE, 
                  dir.exp = c(1,1, 1, 1))
                  
sol_i

# Get the easy counterfactuals:
sol_i$i.sol$C1P1$EC

# same as simplifying assumptions

# Typical cases
typ_y_p <- smmr(results = sol_p,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y_p

# Deviant cases
# consistency in kind

dcon_y_p <- smmr(results = sol_p,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y_p

# KDA I

# Outcome: non-selections as the World Cup venue ----

# Analysis of necessity ----
QCAfit(DT[, 1:4], DT$OUT, names(DT[, 1:4]), necessity = TRUE, neg.out = TRUE)

# no


# Let us also check SUIN conditions
SUIN_ny <- superSubset(data = DT, 
                       outcome = "~OUT",
                       conditions = c("VOT", "STAB", "CAP", 
                                      "LOB"),
                       relation = "necessity",
                       incl.cut = 0.90,
                       ron.cut =  0.6,
                       cov.cut = 0.6,
                       depth = 2)

SUIN_ny

# no 

# Analysis of sufficiency ----


TT_n <- truthTable(DT, outcome = "~OUT", 
                       conditions = colnames(DT[,1:4]),
                       incl.cut1 = 0.95,
                       complete = TRUE,
                       show.cases = TRUE,
                       sort.by = c("OUT","incl", "n"))
TT_n

# conservative solution
sol_c_n <- minimize(TT_n, 
            details = TRUE, 
            show.cases = TRUE)
            
sol_c_n


# Typical cases
typ_y_n <- smmr(results = sol_c_n,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y_n

# Deviant cases
# consistency in kind

dcon_y_n <- smmr(results = sol_c_n,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y_n

# no


# parsimonious solution
sol_p_n <- minimize(TT_n, 
                    details = TRUE, 
                    include = "?", 
                    row.dom = TRUE, 
                    show.cases = TRUE)


sol_p_n
sol_p_n$SA


# intermediate solution:

sol_i_n <- minimize(TT_n, 
             details = TRUE, 
             include = "?", 
             row.dom = TRUE, 
             show.cases = TRUE, 
             dir.exp = c(0, 0, 0, 0))
                       
sol_i_n

# same as conservative

# easy counterfactuals:

sol_i_n$i.sol$C1P1$EC

# no

### Check for simultaneous subset relations:

SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR

# none

# Let's also check for contradictory simplifying assumptions

CSA <- intersect(rownames(sol_p$SA$M1), rownames(sol_p_n$SA$M1))
CSA

#none

# check if we are making contradictory easy counterfactuals 

CEC <- intersect(rownames(sol_i$i.sol$C1P1$EC), rownames(sol_i_n$i.sol$C1P1$EC))
CEC

# none


# Robustness ----

# Test 1 # ----
# 
rm(list=ls())

# Load the data set 
DT <- read.csv("rob1.csv", row.names = 1)

# Analysis of sufficiency for Y

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = 0.75,
                 pri.cut = 0.60,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# Provide the conservative solution:
sol_c <- minimize(TT, details = TRUE, show.cases = TRUE)
sol_c


# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y

# Deviant cases
# consistency in kind

dcon_y <- smmr(results = sol_c,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y

# KDA I

# Analysis of sufficiency fpr ~Y

TT_n <- truthTable(DT, outcome = "~OUT", 
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.90,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))
TT_n

# conservative solution
sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE)

sol_c_n


# Typical cases
typ_y_n <- smmr(results = sol_c_n,
                outcome = "OUT",
                sol = 1,
                match = FALSE,
                cases = 1)
typ_y_n

# Deviant cases
# consistency in kind

dcon_y_n <- smmr(results = sol_c_n,
                 outcome = "OUT",
                 sol = 1,
                 match = FALSE,
                 cases = 3)
dcon_y_n

# no

### Check for simultaneous subset relations:

SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR

# no

# Test 2 # ----

rm(list=ls())

DT <- read.csv("calibrated.csv", row.names = 1)


# Let us remove the rows with KDA II, MOW, SPE, and TA :


DT <- DT[-c(8, 11:14),]
DT

# Analysis of sufficiency for Y

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = 0.75,
                 pri.cut = 0.50,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# Provide the conservative solution:
sol_c <- minimize(TT, details = TRUE, show.cases = TRUE)
sol_c


# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y

# Deviant cases
# consistency in kind

dcon_y <- smmr(results = sol_c,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y

# no


# Analysis of sufficiency for ~Y

TT_n <- truthTable(DT, outcome = "~OUT", 
                   conditions = colnames(DT[,1:4]),
                   incl.cut1 = 0.85,
                   pri.cut = 0.60,
                   complete = TRUE,
                   show.cases = TRUE,
                   sort.by = c("OUT","incl", "n"))
TT_n

# conservative solution
sol_c_n <- minimize(TT_n, 
                    details = TRUE, 
                    show.cases = TRUE)

sol_c_n


# Typical cases
typ_y_n <- smmr(results = sol_c_n,
                outcome = "OUT",
                sol = 1,
                match = FALSE,
                cases = 1)
typ_y_n

# Deviant cases
# consistency in kind

dcon_y_n <- smmr(results = sol_c_n,
                 outcome = "OUT",
                 sol = 1,
                 match = FALSE,
                 cases = 3)
dcon_y_n

# MO


### Check for simultaneous subset relations:

SSR<-intersect(rownames(TT$tt)[TT$tt$OUT==1],rownames(TT_n$tt)[TT_n$tt$OUT==1])
SSR

# no

# Test 3 # ----

rm(list=ls())

DT <- read.csv("calibrated.csv", row.names = 1)

# Let us set a higher threshold  of 0.85 

# Analysis of sufficiency for Y

TT <- truthTable(DT, outcome = "OUT",
                 conditions = colnames(DT[,1:4]),
                 incl.cut1 = 0.85,
                 pri.cut = 0.60,
                 complete = TRUE,
                 show.cases = TRUE,
                 PRI = TRUE,
                 sort.by = c("OUT","incl", "n"))

TT

# Provide the conservative solution:
sol_c <- minimize(TT, details = TRUE, show.cases = TRUE)
sol_c


# Typical cases
typ_y <- smmr(results = sol_c,
              outcome = "OUT",
              sol = 1,
              match = FALSE,
              cases = 1)
typ_y

# Deviant cases
# consistency in kind

dcon_y <- smmr(results = sol_c,
               outcome = "OUT",
               sol = 1,
               match = FALSE,
               cases = 3)
dcon_y

# no


# Analysis of sufficiency for ~Y has not been performed as 
# there is no possibility to either increase or decrease 
# the consistency threshold

