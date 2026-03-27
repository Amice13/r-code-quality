################################################################
##### Calibrate data for QCA on Large scale Contentious Politics #####
################################################################

## The R code in this file tests alternative anchors for calibration

## For replication purposes of PHYSREP and CIVREP, please note that the analysis uses the raw data of:
# (David L. Cingranelli and David L. Richards. 1999. "Measuring the Level, Pattern, and Sequence of 
#  Government Respect for Physical Integrity Rights." International
# Studies Quarterly, Vol 43.2: 407-18).
# Cingranelli, David L., David L. Richards, and K. Chad Clay. 2014. "The CIRI Human Rights Dataset."
# Available online at http://www.humanrightsdata.com. Version 2014.04.14).

## The R code was written by using the packages QCA 3.1, 3.3 and SetMethods 2.3, 2.4


rm(list = ls())

# set working directory:
setwd("D:/UCLouvain/PhD Thesis/PhD/2018/Dataset/Dataset 2019")
Don <-read.csv("Don_rawdata.csv", row.names=1)

## NOTE: For correct replications, please load the packages QCA 2.2 and SetMethods 2.0

# load packages
library(QCA); library(QCAGUI); library(SetMethods); library(lattice);
library(arm); library(plyr); library(car); library(stringr); library(xtable)
library(betareg); library (gmodels); library (Hmisc); library (MASS);
library (memisc); library (polycor); library (psych); library (reshape); 
library (VIM); library (XML); library (foreign); library (directlabels);
library (SetMethods); dependencies = TRUE

head(Don)

# calibrate conditions -----------------------------------------------------------#
# --------------------------------------------------------------------------------#

# Set calibration (using loop function): 
# STEP 1:
# store all of your conditions in an object 

conds <- c("PHYSREP", "CIVREP")
conds <- c("YOUTH")


# STEP 2:
# create matrix that stores the qualitative anchors for each condition
# the order of conditions must be the one in which they appear in object conds
# full non-membership; cross-over point, full membership

# initial cal 4.6
conds_anchors <- matrix(c(1, 4.6, 6,
                          2, 5.4, 9),
                        ncol=3, byrow = TRUE)


# For Youth 10, 16, 35
conds_anchors <- matrix(c(10,16,35),
                        ncol=3, byrow = TRUE)

# STEP 3:
# now use those qualitative anchors stored 
# in object conds_anchors in the calibration function
# note that we store the newly calibrated sets in our data set, which happens to have 16
# columns, so we start putting the new sets at 16+i, 

for (i in 1:length(conds)) {
  Don[ ,16+i] <- calibrate(Don[ ,conds[i]], type = "fuzzy", logistic = TRUE, idm = 0.95,
                           thresholds = c("e"= conds_anchors[i,1],"c"= conds_anchors[i,2],"i"= conds_anchors[i,3]))
}

head(Don)

# STEP 4
# name the new columns just added to the data, using their original name with "low" 

names(Don)[c(17:18)] <- paste0("LOW", conds)
names(Don)[c(19)] <- paste0("HIGH", conds)

head(Don)


# STEP 5
# Visualizing your calibration against base variables
# PHYSREP / LOWPHYSREP
# In PHYSREP x IS THE FUZZY SCORE; IN y IS THE RAW SCORE
# In main "Myset"

plot(Don$CIVREP, Don$LOWCIVREP, pch=18, col="black",
     main='Calibration LOWCIVREP',
     xlab=' CIVREP ',
     ylab=' LOWCIVREP ')
abline(h=0.5, col="black")
abline(v= 4.2, col="black")

plot(Don$PHYSREP, Don$LOWPHYSREP, pch=18, col="black",
     main='Calibration LOWPHYSREP',
     xlab=' PHYSREP ',
     ylab=' LOWPHYSREP ')
abline(h=0.5, col="black")
abline(v= 4.2, col="black")

plot(Don$YOUTH, Don$HIGHYOUTH, pch=18, col="black",
     main='Calibration HIGHYOUTH',
     xlab=' YOUTH ',
     ylab=' HIGHYOUTH ')
abline(h=0.5, col="red",lty="dotted")
abline(v= 4.2, col="black")

# Create histograms and plots using a loop function

# create object with all sets for histograms
setsclo <- c("LOWPHYSREP", "LOWCIVREP")
setsclo

setsclo <- c("HIGHYOUTH")
setsclo


# Then create histograms for all calibrated sets
par(mfrow=c(1,3))
for (i in setsclo){
  hist(Don[, i],
       xlab=i, main = paste(""))
}

# STEP 6:
#Evaluating skewness. To check the skewness of our set, we can identify the number 
# of cases that have set membership above 0.5 
# and check whether there is a disproportionate amount of them in your data.:

skewLOWPHYSREP <- as.numeric(Don$LOWPHYSREP > 0.5)
sum(skewLOWPHYSREP)

skewLOWCIVREP <- as.numeric(Don$LOWCIVREP > 0.5)
sum(skewLOWCIVREP)

skewHIGHYOUTH <- as.numeric(Don$HIGHYOUTH > 0.5)
sum(skewHIGHYOUTH)


# Obtain the percentage of cases with set membership above 0.5 (right-hand side value):

prop.table(table(skewLOWPHYSREP))
prop.table(table(skewLOWCIVREP))
prop.table(table(skewHIGHYOUTH))

# Identify the names of the cases with set membership above 0.5:

rownames(subset(Don, LOWPHYSREP > 0.5))
rownames(subset(Don, LOWCIVREP > 0.5))
rownames(subset(Don, HIGHYOUTH > 0.5))

# STEP 7:
#Evaluating cases on crossover point
checkLOWPHYSREP <- as.numeric(Don$LOWPHYSREP == 0.5)
sum(checkLOWPHYSREP)

# And their percentage:
  
prop.table(table(checkLOWPHYSREP))


#DELETING UNCALIBRATED COLUMNS:
# to prevent confusion, erase uncalibrated columns:
#data[ ,(6:15)] <- NULL


Don[ ,(15:16)] <- NULL
Don[ ,(4)] <- NULL


# Union of variables pol, eco, soc as LOWSTATCAP, defined by logical OR
Don$POL <-pmax(Don$POLEFF, Don$POLEG)
Don$ECO <-pmax(Don$ECOEFF, Don$ECOLEG)
Don$SOC <-pmax (Don$SOCEFF, Don$SOLEG)

# Effect-leg state, pol, eco, soc, defined by OR

Don$LOWSTATCAP <- pmax(Don$POL, Don$ECO, Don$SOC)


# specific support of despotic power, lowphysrep, lowcivrepc, combined by OR
Don$LIMDESPOT <- pmax(Don$LOWPHYSREP, Don$LOWCIVREP)


# exclude not merged leg data
Don[ ,c(4:9)] <- NULL
Don[ ,c(7:9)] <- NULL


# rename other colums:

Don <- rename(Don, c("LOWPHYSREP" = "PHYSREP", 
                       "LOWCIVREP" = "CIVREP",
                       "HIGHYOUTH" = "YOUTH",
                       "LOWEFFLEGSTAT" = "STATCAP"))

Don <- rename(Don, c("HIGHYOUTH" = "YOUTH"))



# for alternative calibrations see scripts on robustness tests

# safe data
save(Don, file = "calibratedDon.rda")

# to be continued in the QCA script! :-)

