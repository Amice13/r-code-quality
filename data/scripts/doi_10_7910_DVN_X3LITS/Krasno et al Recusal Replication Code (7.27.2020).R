rm(list=ls())       # clear objects in memory


#####################################
######## NECESSARY PACKAGES #########
#####################################

#install.packages("ri")
library(ri)         # load the RI package
#install.packages("foreign")
library(foreign)    # package allows R to read datasets
#install.packages("xtable")
library(xtable)     # package for table generation
#install.packages("weights")
library(weights)    # package for determining weighted means and running weighted t.tests


#####################################
######## PRELIMINARY SETUP ##########
#####################################

set.seed(283848)   # Sets random number seed so that results are reproducible. 283848 is the seed that Dane Thorley uses in his research.

setwd("C:/Users/dthorley/Dropbox/OSI Research Working Group/Recusal/Data - Wisconsin Arm/Analysis")  #Sets the working directory. Should stay the same throughout the script.

data <- read.csv("Krasno et al Recusal Replication Data (Experimental Cases).csv")
#BEFORE COMPUTING, THE DATA FILE SHOULD BE SORTED BY BLOCK (variable name "block") AND THEN BY SUBJECT NUMBER ("subject")--EACH IN ASCENDING ORDER.
#IF THE DATA FILE IS NOT ORDERED CORRECTLY, THE CODE BELOW WILL NOT WORK PROPERLY.

#Function for simple random assignment
simple.ra <- function(N, prob){
  assign <- rbinom(n=N,size=1,prob=prob)
  return(assign)
}

#Function for complete random assingment 
complete.ra <- function(N,m){
  assign <- ifelse(1:N %in% sample(1:N,m),1,0) #run each argument in here if it is confusing
  return(assign)
}


#####################################
########## BALANCE CHECK ############ 
#####################################

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)
weighted.mean(judge.gender1,data1$probability)
weighted.mean(judge.gender0,data0$probability)
B1 <- wtd.t.test(x=judge.gender1, y=judge.gender0,weight=data1$probability,weighty=data0$probability)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)
weighted.mean(attorney.gender1,data1$probability)
weighted.mean(attorney.gender0,data0$probability)
B2 <- wtd.t.test(x=attorney.gender1, y=attorney.gender0,weight=data1$probability,weighty=data0$probability)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)
weighted.mean(divorce1,data1$probability)
weighted.mean(divorce0,data0$probability)
B3 <- wtd.t.test(x=divorce1, y=divorce0,weight=data1$probability,weighty=data0$probability)

#Attorney Side
weighted.mean(data1$attSideBi,data1$probability, na.rm = TRUE)
weighted.mean(data0$attSideBi,data0$probability, na.rm = TRUE)
B4 <- wtd.t.test(x=data1$attSideBi,y=data0$attSideBi,weight=data1$probability,weighty=data0$probability)

#Donation Amount
weighted.mean(data1$attDonation,data1$probability)
weighted.mean(data0$attDonation,data0$probability)
B5 <- wtd.t.test(x=data1$attDonation,y=data0$attDonation,weight=data1$probability,weighty=data0$probability)

#Donation Proportion
weighted.mean(data1$proportion,data1$probability)
weighted.mean(data0$proportion,data0$probability)
B6 <- wtd.t.test(x=data1$proportion,y=data0$proportion,weight=data1$probability,weighty=data0$probability)

#Rows for Table 1
T1.R1 <- c("Pre-treatment Covariate", "Treatment Group Weighted Mean", "Control Group Weighted Mean", "p-value (2-tailed weighted t-test)")
T1.R2 <- c("Judge Gender (Male)", weighted.mean(judge.gender1,data1$probability), weighted.mean(judge.gender0,data0$probability), unname(B1$coefficients[3]))
T1.R3 <- c("Attorney Gender (Male)", weighted.mean(attorney.gender1,data1$probability), weighted.mean(attorney.gender0,data0$probability), unname(B2$coefficients[3]))
T1.R4 <- c("Case Type (Divorce)", weighted.mean(divorce1,data1$probability), weighted.mean(divorce0,data0$probability), unname(B3$coefficients[3]))
T1.R5 <- c("Attorney Side (Plaintiff)", weighted.mean(data1$attSideBi,data1$probability, na.rm = TRUE), weighted.mean(data0$attSideBi,data0$probability, na.rm = TRUE), unname(B4$coefficients[3]))
T1.R6 <- c("Donation Amount (Dollars)", weighted.mean(data1$attDonation,data1$probability), weighted.mean(data0$attDonation,data0$probability), unname(B5$coefficients[3]))
T1.R7 <- c("The Attorney's Contributions as a Proportion of Total Contributions that a Judge Received", weighted.mean(data1$proportion,data1$probability), weighted.mean(data0$proportion,data0$probability), unname(B6$coefficients[3]))

#Table 1 Matrix
T1 <- matrix(c(T1.R1,T1.R2,T1.R3,T1.R4,T1.R5,T1.R6,T1.R7), nrow = 7, ncol = 4, byrow = TRUE)
T1

#Table 1 Output
Table.Balance <- xtable(T1)
print.xtable(Table.Balance, type="html", file="Table1Balance (7.27.2020).html")


#####################################
####### VARIABLE GENERATION #########
#####################################

###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset


###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.request2 <- data$request2     # whether a recusal was requested, with a more liberal coding
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.transfer2 <- data$transfer2     # whether the donee judge was transferred (doesn't include recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


#####################################
########## ESTIMATED ATE ############
#####################################

##This section creates the estimated ATEs for the data for each of the outcomes of interest for the two models (bivariate and multivariate).

###Regressions w/o covariates

#Runs Regression
reg.request <- summary(lm(data$request~Z, weights = weights))
reg.granted <- summary(lm(data$granted~Z, weights = weights))
reg.transfer <- summary(lm(data$transfer~Z, weights = weights)) 
reg.attorney <- summary(lm(data$attorney~Z, weights = weights))
reg.mention <- summary(lm(data$mention~Z, weights = weights)) 
reg.action <- summary(lm(data$action~Z, weights = weights)) 

#Pulls Estimated ATE
ate.request <- reg.request$coefficients[2]
ate.granted <- reg.granted$coefficients[2]
ate.transfer <- reg.transfer$coefficients[2]
ate.attorney <- reg.attorney$coefficients[2]
ate.mention <- reg.mention$coefficients[2]
ate.action <- reg.action$coefficients[2]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))

#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2]
ate.cv.granted <- reg.cv.granted$coefficients[2]
ate.cv.transfer <- reg.cv.transfer$coefficients[2]
ate.cv.attorney <- reg.cv.attorney$coefficients[2]
ate.cv.mention <- reg.cv.mention$coefficients[2]
ate.cv.action <- reg.cv.action$coefficients[2]



#####################################
######## Sidak Correction ###########
#####################################

Sidak <- 1 - (1 - .05)^(1/5) #Based on 5 outcomes of interest.



#####################################
##### RANDOMIZATION INFERENCE #######
#####################################

sims <- 10000  # Run hypothetical experiment 10,000 times
perms <- matrix(NA,nrow=N,ncol=sims) #Creates an empty object to put the simulated assignments into


###Reconstructing the Assignment Process used in "Recusal Assignment Code.R"
for (i in 1:sims) {
  #i <- 1
  block1 <- as.numeric(complete.ra(3,1)) #for judge 6,
  block2 <-complete.ra(2,1) #for judge 5
  block3 <-complete.ra(2,1) #for judge 7
  block4 <-complete.ra(2,1) #for judge 8
  block5 <-complete.ra(2,1) #for judge 9
  block6 <-complete.ra(5,2) #for judges 1,2,3,4,10
  block7 <-complete.ra(2,1) #for judges 11, 12
  block8 <-complete.ra(2,1) #for judges 13, 10
  block9 <-simple.ra(1,.5) #for judge 14
  block10 <- complete.ra(2,1) #for judge 1 (cases 51 and 53)
  block11 <- simple.ra(1,.5) #for judge 13 (case 57)
  block12 <- simple.ra(1,.5) #for judge 17 (case 66)
  block13 <- simple.ra(1,.5) #for judge 18 (case 73)
  block14 <- complete.ra(3,2) #for judge 16 (case 65), judge 15 (case 68), and judge 19 (case 78)
  block15 <- complete.ra(2,1) #for judge 20 (case 80) and judge 21 (case 82)
  block16 <- simple.ra(1,.5) #for judge 22 (case 87)
  block17 <- simple.ra(1,.5) #for judge 14 (case 101)
  block18 <- simple.ra(1,.5) #for judge 23 (case 108)
  block19 <- simple.ra(1,.5) #for judge 2 (case 117)
  block20 <- simple.ra(1,.5) #for judge 24 (case 123)
  block21 <- simple.ra(1,.5) #for judge 21 (case 130)
  block22 <- simple.ra(1,.5) #for judge 25 (case 135)
  block23 <- complete.ra(3,2) #for judges 26 (case 137), judge 17 (case 138), and judge 14 (case 139)
  block24 <- simple.ra(1,.5) #for judge 15 (case 141)
  block25 <- simple.ra(1,.5) #for judge 27 (case 147)
  block26 <- simple.ra(1,.5) #for judge 28 (case 150)
  block27 <- simple.ra(1,.5) #for judge 29 (case 152)
  block28 <- simple.ra(1,.5) #for judge 30 (case 156)
  block29 <- simple.ra(1,.5) #for judge 2 (case 158)
  block30 <- simple.ra(1,.5) #for judge 13 (case 171)
  block31 <- simple.ra(1,.5) #for judge 23 (case 184)
  block32 <- simple.ra(1,(2/3)) #for judge 26 (case 188) (At this point, the probability of treatment was moved from .5 to 2/3 to increase number of cases in treatement.)
  block33 <- simple.ra(1,(2/3)) #for judge 31 (case 196)
  block34 <- complete.ra(3,1) #for judge 32 (cases 218, 219, 220) (we are only treated one case because they are all with the same judge)
  block35 <- complete.ra(3,2) #for judges 23 (case 232), 28 (case 235), and 33 (case 238)
  block36 <- simple.ra(1,(2/3)) #for judge 34 (case 242)
  block37 <- simple.ra(1,(2/3)) #for judge 15 (case 258)
  block38 <- simple.ra(1,(2/3)) #for judge 35 (case 267)

  perms[1,i] <- block1[1]
  perms[2,i] <- block1[2]
  perms[3,i] <- block1[3]
  perms[4,i] <- block2[1]
  perms[5,i] <- block2[2]
  perms[6,i] <- block3[1]
  perms[7,i] <- block3[2]
  perms[8,i] <- block4[1]
  perms[9,i] <- block4[2]
  perms[10,i] <- block5[1]
  perms[11,i] <- block5[2]
  perms[12,i] <- block6[1]
  perms[13,i] <- block6[2]
  perms[14,i] <- block6[3]
  perms[15,i] <- block6[4]
  perms[16,i] <- block6[5]
  perms[17,i] <- block7[1]
  perms[18,i] <- block7[2]
  perms[19,i] <- block8[1]
  perms[20,i] <- block8[2]
  perms[21,i] <- block9[1]
  perms[22,i] <- block10[1]
  perms[23,i] <- block10[2]
  perms[24,i] <- block11[1]
  perms[25,i] <- block12[1]
  perms[26,i] <- block13[1]
  perms[27,i] <- block14[1]
  perms[28,i] <- block14[2]
  perms[29,i] <- block14[3]
  perms[30,i] <- block15[1]
  perms[31,i] <- block15[2]
  perms[32,i] <- block16[1]
  perms[33,i] <- block17[1]
  perms[34,i] <- block18[1]
  perms[35,i] <- block19[1]
  perms[36,i] <- block20[1]
  perms[37,i] <- block21[1]
  perms[38,i] <- block22[1]
  perms[39,i] <- block23[1]
  perms[40,i] <- block23[2]
  perms[41,i] <- block23[3]
  perms[42,i] <- block24[1]
  perms[43,i] <- block25[1]
  perms[44,i] <- block26[1]
  perms[45,i] <- block27[1]
  perms[46,i] <- block28[1]
  perms[47,i] <- block29[1]
  perms[48,i] <- block30[1]
  perms[49,i] <- block31[1]
  perms[50,i] <- block32[1]
  perms[51,i] <- block33[1]
  perms[52,i] <- block34[1]
  perms[53,i] <- block34[2]
  perms[54,i] <- block34[3]
  perms[55,i] <- block35[1]
  perms[56,i] <- block35[2]
  perms[57,i] <- block35[3]
  perms[58,i] <- block36[1]
  perms[59,i] <- block37[1]
  perms[60,i] <- block38[1]
}


###Calculating Potential Outcomes Under Sharp Null of 0 for All Units
Ys.request <- genouts(Y.request,Z,ate=0)
Y1.request <- Ys.request$Y1
Y0.request <- Ys.request$Y0

Ys.granted <- genouts(Y.granted,Z,ate=0)
Y1.granted <- Ys.granted$Y1
Y0.granted <- Ys.granted$Y0

Ys.transfer <- genouts(Y.transfer,Z,ate=0)
Y1.transfer <- Ys.transfer$Y1
Y0.transfer <- Ys.transfer$Y0

Ys.mention <- genouts(Y.mention,Z,ate=0)
Y1.mention <- Ys.mention$Y1
Y0.mention <- Ys.mention$Y0

Ys.attorney <- genouts(Y.attorney,Z,ate=0)
Y1.attorney <- Ys.attorney$Y1
Y0.attorney <- Ys.attorney$Y0

Ys.action <- genouts(Y.action,Z,ate=0)
Y1.action <- Ys.action$Y1
Y0.action <- Ys.action$Y0


###Creating Empty Objects For RI
Coeff.request <- rep(NA, sims) #To Capture Simulation Coefficients
Coeff.cv.request <- rep(NA, sims)
Coeff.granted <- rep(NA, sims)
Coeff.cv.granted <- rep(NA, sims)
Coeff.transfer <- rep(NA, sims)
Coeff.cv.transfer <- rep(NA, sims)
Coeff.mention <- rep(NA, sims)
Coeff.cv.mention <- rep(NA, sims)
Coeff.attorney <- rep(NA, sims)
Coeff.cv.attorney <- rep(NA, sims)
Coeff.action <- rep(NA, sims)
Coeff.cv.action <- rep(NA, sims)

RI.request <- rep(NA, sims) #To Count Simulations That Come Up Significant
RI.cv.request <- rep(NA, sims)
RI.granted <- rep(NA, sims)
RI.cv.granted <- rep(NA, sims)
RI.transfer <- rep(NA, sims)
RI.cv.transfer <- rep(NA, sims)
RI.mention <- rep(NA, sims)
RI.cv.mention <- rep(NA, sims)
RI.attorney <- rep(NA, sims)
RI.cv.attorney <- rep(NA, sims)
RI.action <- rep(NA, sims)
RI.cv.action <- rep(NA, sims)


###Calculating Simulated ATE Estimates
for (i in 1:sims){
  
  # generate the weights for each randomized assignment:
  weights <- rep(NA, N)
  weights[perms[,i] == 1] <- 1/probs[perms[,i] == 1]
  weights[perms[,i] == 0] <- 1/(1-probs)[perms[,i] == 0]
  
  ##Recusal Requests
  Ys.request <- Y1.request*perms[,i] + Y0.request*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.request ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.request[i] <- ATE.sim$coefficients[2]
  RI.request[i] <- ifelse(ATE.sim$coefficients[2] >= ate.request, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.request ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.request[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.request[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.request, 1, 0) # Determine significance according to p <= 0.05 

  ##Recusal Requests Granted
  Ys.granted <- Y1.granted*perms[,i] + Y0.granted*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.granted ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.granted[i] <- ATE.sim$coefficients[2]
  RI.granted[i] <- ifelse(ATE.sim$coefficients[2] >= ate.granted, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.granted ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.granted[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.granted[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.granted, 1, 0) # Determine significance according to p <= 0.05

  ##Judicial Transfers (with recusals)
  Ys.transfer <- Y1.transfer*perms[,i] + Y0.transfer*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.transfer ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.transfer[i] <- ATE.sim$coefficients[2]
  RI.transfer[i] <- ifelse(ATE.sim$coefficients[2] >= ate.transfer, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.transfer ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.transfer[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.transfer[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.transfer, 1, 0) # Determine significance according to p <= 0.05

  ##Donation Relationships Disclosed
  Ys.mention <- Y1.mention*perms[,i] + Y0.mention*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.mention ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.mention[i] <- ATE.sim$coefficients[2]
  RI.mention[i] <- ifelse(ATE.sim$coefficients[2] >= ate.mention, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.mention ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.mention[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.mention[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.mention, 1, 0) # Determine significance according to p <= 0.05
  
  ##Attorneys Withdrawn or Transferred
  Ys.attorney <- Y1.attorney*perms[,i] + Y0.attorney*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.attorney ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.attorney[i] <- ATE.sim$coefficients[2]
  RI.attorney[i] <- ifelse(ATE.sim$coefficients[2] >= ate.attorney, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.attorney ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.attorney[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.attorney[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.attorney, 1, 0) # Determine significance according to p <= 0.05
  
  ##Any Action At All (Combination)
  Ys.action <- Y1.action*perms[,i] + Y0.action*(1-perms[,i]) # Determine which outcome is revealed by the random assignment
  ATE.sim <- lm(Ys.action ~ perms[,i], weights = weights) # Do analysis (Regression w/o covariates)
  Coeff.action[i] <- ATE.sim$coefficients[2]
  RI.action[i] <- ifelse(ATE.sim$coefficients[2] >= ate.action, 1, 0) # Determine significance according to p <= 0.05
  ATE.sim.cv <- lm(Ys.action ~ perms[,i] + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights) # Do analysis (Regression w/ covariates)
  Coeff.cv.action[i] <- ATE.sim.cv$coefficients[2]
  RI.cv.action[i] <- ifelse(ATE.sim.cv$coefficients[2] >= ate.cv.action, 1, 0) # Determine significance according to p <= 0.05
  }


#####################################
####### RI-DERIVED P-VALUES #########
#####################################

###This section calculates p-values (onesided) and assigns standard errors to items (Se)

onesided.request <- mean(RI.request)
se.request <- reg.request$coefficients[2,2]
onesided.cv.request <- mean(RI.cv.request)
se.cv.request <- reg.cv.request$coefficients[2,2]

onesided.granted <- mean(RI.granted) 
se.granted <- reg.granted$coefficients[2,2]
onesided.cv.granted <- mean(RI.cv.granted)
se.cv.granted <- reg.cv.granted$coefficients[2,2]

onesided.transfer <- mean(RI.transfer)
se.transfer <- reg.transfer$coefficients[2,2]
onesided.cv.transfer <- mean(RI.cv.transfer)
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

onesided.mention <- mean(RI.mention)
se.mention <- reg.mention$coefficients[2,2]
onesided.cv.mention <- mean(RI.cv.mention)
se.cv.mention <- reg.cv.mention$coefficients[2,2]

onesided.attorney <- mean(RI.attorney)
se.attorney <- reg.attorney$coefficients[2,2]
onesided.cv.attorney <- mean(RI.cv.attorney)
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

onesided.action <- mean(RI.action)
se.action <- reg.action$coefficients[2,2]
onesided.cv.action <- mean(RI.cv.action)
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 2 (Estimated ATE from Multivariate Regression)
T2.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE**", "P-value* (1-tailed)", "Standard Errors")
T2.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T2.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T2.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T2.R5 <- c("Judicial Disclosure of Conflict", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T2.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T2.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)

#Table 2 Matrix
T2 <- matrix(c(T2.R1,T2.R2,T2.R3,T2.R4,T2.R5,T2.R6,T2.R7), nrow = 7, ncol = 6, byrow = TRUE)
#Note that in the final table, we combine rows 2 and 3.
T2

#Table 2 Output
Table.ATE.Multivariate <- xtable(T2)
print.xtable(Table.ATE.Multivariate, type="html", file="Table2ATEMulti (7.27.2020).html")



#################################################################################################################################
################################################ SUPPLEMENTAL ANALYSES ##########################################################
#################################################################################################################################

#These are tests or observations that we specified we would conduct in our PAP but did not include in the body of the paper.



########################################################
####### TABLE E1: WEIGHTED COMPARISON OF MEANS #########
########################################################

#In our PAP, we specified that we would conduct weighted difference-in-means tests for each outcome (standard one-sided t-tests)

com.request <- wtd.t.test(x = data$request[which(data$treated == 1)], y = data$request[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.request <- com.request$additional[1]
onesided.com.request <- com.request$coefficients[3]

com.granted <- wtd.t.test(x = data$granted[which(data$treated == 1)], y = data$granted[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.granted <- com.granted$additional[1]
onesided.com.granted <- com.granted$coefficients[3]

com.transfer <- wtd.t.test(x = data$transfer[which(data$treated == 1)], y = data$transfer[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.transfer <- com.transfer$additional[1]
onesided.com.transfer <- com.transfer$coefficients[3]

com.mention <- wtd.t.test(x = data$mention[which(data$treated == 1)], y = data$mention[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.mention <- com.mention$additional[1]
onesided.com.mention <- com.mention$coefficients[3]

com.attorney <- wtd.t.test(x = data$attorney[which(data$treated == 1)], y = data$attorney[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.attorney <- com.attorney$additional[1]
onesided.com.attorney <- com.attorney$coefficients[3]

com.action <- wtd.t.test(x = data$action[which(data$treated == 1)], y = data$action[which(data$treated == 0)], weight = weights[data$treated==1], weighty = weights[data$treated==0], alternative = "greater")
ate.com.action <- com.action$additional[1]
onesided.com.action <- com.action$coefficients[3]


#Rows for Table E1 (Estimated ATE from Comparison of Means)
T4.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE**", "P-value* (1-tailed)")
T4.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.com.request, onesided.com.request)
T4.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.com.granted, onesided.com.granted)
T4.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.com.transfer, onesided.com.transfer)
T4.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.com.mention, onesided.com.mention)
T4.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.com.attorney, onesided.com.attorney)
T4.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.com.action, onesided.com.action)


#Table E1 Matrix
T4 <- matrix(c(T4.R1,T4.R2,T4.R3,T4.R4,T4.R5,T4.R6,T4.R7), nrow = 7, ncol = 5, byrow = TRUE)
#Note that in the final table, we combine rows 1 and 2.
T4

#Table E1 Output
Table.ATE.CompMeans <- xtable(T4)
print.xtable(Table.ATE.CompMeans, type="html", file="TableE1ATECoM (7.27.2020).html")


###############################################################
####### TABLE E2: TREATMENT-BY-COVARIATE INTERACTIONS #########
###############################################################

datasave <- data

#####MALE JUDGES######
data <- datasave
data <- data[which(data$judGender == "Male"),]


#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)

#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5a (Estimated ATE from Multivariate Regression for Male Judges)
T5a.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5a.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5a.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5a.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5a.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5a.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5a.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5a Matrix
T5a <- matrix(c(T5a.R1,T5a.R2,T5a.R3,T5a.R4,T5a.R5,T5a.R6,T5a.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####FEMALE JUDGES######
data <- datasave
data <- data[which(data$judGender == "Female"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)

#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + attorney.gender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5b (Estimated ATE from Multivariate Regression for Female Judges)
T5b.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5b.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5b.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5b.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5b.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5b.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5b.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5b Matrix
T5b <- matrix(c(T5b.R1,T5b.R2,T5b.R3,T5b.R4,T5b.R5,T5b.R6,T5b.R7), nrow = 7, ncol = 6, byrow = TRUE)

#####MALE ATTORNEYS######
data <- datasave
data <- data[which(data$attGender == "Male"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5c (Estimated ATE from Multivariate Regression for Male Attorneys)
T5c.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5c.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5c.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5c.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5c.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5c.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5c.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5c Matrix
T5c <- matrix(c(T5c.R1,T5c.R2,T5c.R3,T5c.R4,T5c.R5,T5c.R6,T5c.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####FEMALE ATTORNEYS######
data <- datasave
data <- data[which(data$attGender == "Female"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + divorce + data$attSideBi + data$attDonation + data$proportion, weights = weights))

#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5d (Estimated ATE from Multivariate Regression for Female Attorneys)
T5d.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5d.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5d.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5d.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5d.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5d.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5d.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5d Matrix
T5d <- matrix(c(T5d.R1,T5d.R2,T5d.R3,T5d.R4,T5d.R5,T5d.R6,T5d.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####DIVORCE CASES######
data <- datasave
data <- data[which(data$caseType == "Divorce"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5e (Estimated ATE from Multivariate Regression for Divorce Cases)
T5e.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5e.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5e.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5e.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5e.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5e.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5e.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5e Matrix
T5e <- matrix(c(T5e.R1,T5e.R2,T5e.R3,T5e.R4,T5e.R5,T5e.R6,T5e.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####NON-DIVORCE CASES######
data <- datasave
data <- data[which(data$caseType != "Divorce"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + data$attSideBi + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5f (Estimated ATE from Multivariate Regression for Non-Divorce Cases)
T5f.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5f.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5f.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5f.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5f.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5f.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5f.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5f Matrix
T5f <- matrix(c(T5f.R1,T5f.R2,T5f.R3,T5f.R4,T5f.R5,T5f.R6,T5f.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####PLAINTIFF DONOR ATTORNEY######
data <- datasave
data <- data[which(data$attSide == "Plaintiff"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5g (Estimated ATE from Multivariate Regression for Plaintiff Donor Attorney Cases)
T5g.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5g.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5g.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5g.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5g.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5g.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5g.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5g Matrix
T5g <- matrix(c(T5g.R1,T5g.R2,T5g.R3,T5g.R4,T5g.R5,T5g.R6,T5g.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####NON-PLAINTIFF DONOR ATTORNEY######
data <- datasave
data <- data[which(data$attSide != "Plaintiff"),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attDonation + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5h (Estimated ATE from Multivariate Regression for Non-Plaintiff Donor Attorney Cases)
T5h.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5h.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5h.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5h.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5h.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5h.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5h.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5h Matrix
T5h <- matrix(c(T5h.R1,T5h.R2,T5h.R3,T5h.R4,T5h.R5,T5h.R6,T5h.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####$300 - Donation######
data <- datasave
data <- data[which(data$attDonation < 300),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5i (Estimated ATE from Multivariate Regression for less than $300 donations)
T5i.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5i.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5i.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5i.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5i.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5i.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5i.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5i Matrix
T5i <- matrix(c(T5i.R1,T5i.R2,T5i.R3,T5i.R4,T5i.R5,T5i.R6,T5i.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####$300 + Donation######
data <- datasave
data <- data[which(data$attDonation >= 300),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     

###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$proportion, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5j (Estimated ATE from Multivariate Regression for $300 or more more donations)
T5j.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5j.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5j.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5j.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5j.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5j.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5j.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)



#Table 5j Matrix
T5j <- matrix(c(T5j.R1,T5j.R2,T5j.R3,T5j.R4,T5j.R5,T5j.R6,T5j.R7), nrow = 7, ncol = 6, byrow = TRUE)

#####2% - Donation######
data <- datasave
data <- data[which(data$proportion <= 0.02),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5k (Estimated ATE from Multivariate Regression for 2% or less donations)
T5k.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5k.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5k.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5k.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5k.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5k.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5k.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)



#Table 5k Matrix
T5k <- matrix(c(T5k.R1,T5k.R2,T5k.R3,T5k.R4,T5k.R5,T5k.R6,T5k.R7), nrow = 7, ncol = 6, byrow = TRUE)


#####2% + Donation######
data <- datasave
data <- data[which(data$proportion > 0.02),]

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]
nrow(data0)
nrow(data1)


#Judge Gender
judge.gender1 <- ifelse(data1$judGender == "Male",1,0)
judge.gender0 <- ifelse(data0$judGender == "Male",1,0)

#Attorney Gender
attorney.gender1 <- ifelse(data1$attGender == "Male",1,0)
attorney.gender0 <- ifelse(data0$attGender == "Male",1,0)

#Type of Case
#This balance test uses divorce as a proxy for case type and varies from the Harris sample, which balances on motor vehicle accident cases
divorce1 <- ifelse(data1$caseType == "Divorce",1,0)
divorce0 <- ifelse(data0$caseType == "Divorce",1,0)


###Assignment
Z  <- data$treated     


###Pre-treatment covariates
#Judge gender is already in the dataset
attorney.gender <- ifelse(data$attGender == "Male",1,0)
divorce <- ifelse(data$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the dataset
#Donation amount is already in the dataset
#Proportion that the attorney donation made up is already in the dataset

###Outcomes
Y.request <- data$request      # whether a recusal was requested
Y.granted <- data$granted      # whether a recusal was granted
Y.transfer <- data$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- data$attorney     # whether the donor attorney was removed or left
Y.mention <- data$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- data$action       # whether any of the above outcomes occurred


###Dataset Length
N <- length(Z)


###Weights
probs <- data$probability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(1-probs)[Z == 0]


###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(data$request~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights))
reg.cv.granted <- summary(lm(data$granted~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.transfer <- summary(lm(data$transfer~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.attorney <- summary(lm(data$attorney~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.mention <- summary(lm(data$mention~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights)) 
reg.cv.action <- summary(lm(data$action~Z + data$judGender + attorney.gender + divorce + data$attSideBi + data$attDonation, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


###Ratios for ATE Tables

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
data1 <- data[ which(data$treated==1),  ]
data0 <- data[ which(data$treated==0),  ]

#Rows for Table 5l (Estimated ATE from Multivariate Regression for more than 2% donations)
T5l.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 32)", "Treatment Group Weighted Mean (n = 28)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T5l.R2 <- c("Applications for Recusal", weighted.mean(data$request[data$treated==0],weights[data$treated==0]), weighted.mean(data$request[data$treated==1],weights[data$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T5l.R3 <- c("Recusals Granted", weighted.mean(data$granted[data$treated==0],weights[data$treated==0]), weighted.mean(data$granted[data$treated==1],weights[data$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T5l.R4 <- c("Judicial Transfers", weighted.mean(data$transfer[data$treated==0],weights[data$treated==0]), weighted.mean(data$transfer[data$treated==1],weights[data$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T5l.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(data$mention[data$treated==0],weights[data$treated==0]), weighted.mean(data$mention[data$treated==1],weights[data$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T5l.R6 <- c("Attorney Withdrawal", weighted.mean(data$attorney[data$treated==0],weights[data$treated==0]), weighted.mean(data$attorney[data$treated==1],weights[data$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T5l.R7 <- c("Any Action***", weighted.mean(data$action[data$treated==0],weights[data$treated==0]), weighted.mean(data$action[data$treated==1],weights[data$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table 5l Matrix
T5l <- matrix(c(T5l.R1,T5l.R2,T5l.R3,T5l.R4,T5l.R5,T5l.R6,T5l.R7), nrow = 7, ncol = 6, byrow = TRUE)

T5a
T5b
T5c
T5d
T5e
T5f
T5g
T5h
T5i
T5j

#Table E2 (combination of outcomes for treatment categories)
TBC_Table <- rbind(T5a.R1,
                   c("Male Judge",NA,NA,NA,NA,NA),
                   T5a.R2, T5a.R4, T5a.R5, T5a.R6, T5a.R7,
                   c("Female Judge",NA,NA,NA,NA,NA),
                   T5b.R2, T5b.R4, T5b.R5, T5b.R6, T5b.R7,
                   c("Male Attorney",NA,NA,NA,NA,NA),
                   T5c.R2, T5c.R4, T5c.R5, T5c.R6, T5c.R7,
                   c("Female Attorney",NA,NA,NA,NA,NA),
                   T5d.R2, T5d.R4, T5d.R5, T5d.R6, T5d.R7,
                   c("Divorce Case",NA,NA,NA,NA,NA),
                   T5e.R2, T5e.R4, T5e.R5, T5e.R6, T5e.R7,
                   c("Non-Divorce Case",NA,NA,NA,NA,NA),
                   T5f.R2, T5f.R4, T5f.R5, T5f.R6, T5f.R7,
                   c("Plaintiff Donor Attorney",NA,NA,NA,NA,NA),
                   T5g.R2, T5g.R4, T5g.R5, T5g.R6, T5g.R7,
                   c("Non-Plaintiff Donor Attorney",NA,NA,NA,NA,NA),
                   T5h.R2, T5h.R4, T5h.R5, T5h.R6, T5h.R7,
                   c("Attorney Donation Less Than $300",NA,NA,NA,NA,NA),
                   T5i.R2, T5i.R4, T5i.R5, T5i.R6, T5i.R7,
                   c("Attorney Donation $300 or More",NA,NA,NA,NA,NA),
                   T5j.R2, T5j.R4, T5j.R5, T5j.R6, T5j.R7,
                   c("Attorney Donation 2% or Less of Total Donations to Judge",NA,NA,NA,NA,NA),
                   T5k.R2, T5k.R4, T5k.R5, T5k.R6, T5k.R7,
                   c("Attorney Donation 2% or Less of Total Donations to Judge",NA,NA,NA,NA,NA),
                   T5l.R2, T5l.R4, T5l.R5, T5l.R6, T5l.R7)

TBC_Table

#Table E2 Output
Table.TBC <- xtable(TBC_Table)
print.xtable(Table.TBC, type="html", file="TableE2TBC (7.27.2020).html", digits = 3)


###########################################
####### TABLE E3: SPILLOVER TESTS #########
###########################################

#Notes: (1) Our spillover tests only include cases in which the presiding judge has previously been over a case assigned to treatment or control (but not both)
#       (2) The treatment probabilities for these cases are calculated as (a) the probability of the prior case being assigned to treatment (p) if the prior case was assigned to treatment
#           (b) (1-p) if the prior case was assigned to control, and (c) (1-p)(1-p) if the prior case was assigned to control and the case before that was also assigned to control.

rm(list=ls())       # clear objects in memory
#install.packages("ri")
library(ri)         # load the RI package
#install.packages("foreign")
library(foreign)    # package allows R to read datasets
#install.packages("xtable")
library(xtable)     # package for table generation
#install.packages("weights")
library(weights)    # package for determining weighted means and running weighted t.tests

set.seed(283848)   # Sets random number seed so that results are reproducible. 283848 is the seed that Dane Thorley uses in his research.


setwd("C:/Users/dthorley/Dropbox/OSI Research Working Group/Recusal/Data - Wisconsin Arm/Analysis")  #Sets the working directory. Should stay the same throughout the script.

spilloverdata <- read.csv("Krasno et al Recusal Replication Data (NonExperimental Cases).csv")
#210 non-experimental cases

spilloverdata <- spilloverdata[which(spilloverdata$spilloverEligible ==1),]
#55 spillover-eligible cases

describe(spilloverdata$spilloverNotes)
#14 cases in control (12 in normal control 2 in double control) and 41 cases in treatment

describe(spilloverdata$treated)
spilloverdata$treated <- ifelse(spilloverdata$spilloverNotes == "Treatment", 1, 0)
describe(spilloverdata$treated)
#Converts treatment and control group into binary indicators

#Creating Two Datasets for Each Treatment Group to Deal With Wonkyness in the "Weights" Package
spilloverdata1 <- spilloverdata[ which(spilloverdata$treated==1),  ]
spilloverdata0 <- spilloverdata[ which(spilloverdata$treated==0),  ]


###Assignment
Z  <- spilloverdata$treated     


###Pre-treatment covariates
#Judge gender is already in the spilloverdataset
attorney.gender <- ifelse(spilloverdata$attGender == "Male",1,0)
divorce <- ifelse(spilloverdata$caseType == "Divorce",1,0) #we use divorce as a proxy for case type
#Attorney side is already in the spilloverdataset
#Donation amount is already in the spilloverdataset
#Proportion that the attorney donation made up is already in the spilloverdataset


###Outcomes
Y.request <- spilloverdata$request      # whether a recusal was requested
Y.granted <- spilloverdata$granted      # whether a recusal was granted
Y.transfer <- spilloverdata$transfer     # whether the donee judge was transferred (includes recusal)
Y.attorney <- spilloverdata$attorney     # whether the donor attorney was removed or left
Y.mention <- spilloverdata$mention      # whether the conflict or donation was mentioned on the court record
Y.action <- spilloverdata$action       # whether any of the above outcomes occurred


###spilloverdataset Length
N <- length(Z)


###Weights
probs <- spilloverdata$spilloverProbability
weights <- rep(NA, N)
weights[Z == 1] <- 1/probs[Z == 1]
weights[Z == 0] <- 1/(probs)[Z == 0] #normally it would be (1-probs), but we already computed this value in the intial spilloverProbability


##This section creates the estimated ATEs for the spilloverdata for each of the outcomes of interest for the two models (bivariate and multivariate).

###Regressions w/ covariates

#Runs Regression
reg.cv.request <- summary(lm(spilloverdata$request~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights))
reg.cv.granted <- summary(lm(spilloverdata$granted~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights)) 
reg.cv.transfer <- summary(lm(spilloverdata$transfer~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights)) 
reg.cv.attorney <- summary(lm(spilloverdata$attorney~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights)) 
reg.cv.mention <- summary(lm(spilloverdata$mention~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights)) 
reg.cv.action <- summary(lm(spilloverdata$action~Z + spilloverdata$judGender + attorney.gender + divorce + spilloverdata$attSideBi + spilloverdata$attDonation, weights = weights))


#Pulls Estimated ATE
ate.cv.request <- reg.cv.request$coefficients[2,1]
onesided.cv.request <- reg.cv.request$coefficients[2,4]
se.cv.request <- reg.cv.request$coefficients[2,2]

ate.cv.granted <- reg.cv.granted$coefficients[2]
onesided.cv.granted <- reg.cv.granted$coefficients[2,4]
se.cv.granted <- reg.cv.granted$coefficients[2,2]

ate.cv.transfer <- reg.cv.transfer$coefficients[2]
onesided.cv.transfer <- reg.cv.transfer$coefficients[2,4]
se.cv.transfer <- reg.cv.transfer$coefficients[2,2]

ate.cv.attorney <- reg.cv.attorney$coefficients[2]
onesided.cv.attorney <- reg.cv.attorney$coefficients[2,4]
se.cv.attorney <- reg.cv.attorney$coefficients[2,2]

ate.cv.mention <- reg.cv.mention$coefficients[2]
onesided.cv.mention <- reg.cv.mention$coefficients[2,4]
se.cv.mention <- reg.cv.mention$coefficients[2,2]

ate.cv.action <- reg.cv.action$coefficients[2]
onesided.cv.action <- reg.cv.action$coefficients[2,4]
se.cv.action <- reg.cv.action$coefficients[2,2]


Sidak <- 1 - (1 - .05)^(1/5) #Based on 5 outcomes of interest.

#Rows for Table E3 (Estimated Spillover ATE from Multivariate Regression)
T6.R1 <- c("Outcomes", "Control Group Weighted Mean (n = 14)", "Treatment Group Weighted Mean (n = 41)", "Estimated ATE", "P-value* (1-tailed)", "Standard Errors")
T6.R2 <- c("Applications for Recusal", weighted.mean(spilloverdata$request[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$request[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.request, onesided.cv.request, se.cv.request)
T6.R3 <- c("Recusals Granted", weighted.mean(spilloverdata$granted[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$granted[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.granted, onesided.cv.granted, se.cv.granted)
T6.R4 <- c("Judicial Transfers", weighted.mean(spilloverdata$transfer[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$transfer[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.transfer, onesided.cv.transfer, se.cv.transfer)
T6.R5 <- c("Judicial Disclosure of Donations**", weighted.mean(spilloverdata$mention[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$mention[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.mention, onesided.cv.mention, se.cv.mention)
T6.R6 <- c("Attorney Withdrawal", weighted.mean(spilloverdata$attorney[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$attorney[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.attorney, onesided.cv.attorney, se.cv.attorney)
T6.R7 <- c("Any Action***", weighted.mean(spilloverdata$action[spilloverdata$treated==0],weights[spilloverdata$treated==0]), weighted.mean(spilloverdata$action[spilloverdata$treated==1],weights[spilloverdata$treated==1]), ate.cv.action, onesided.cv.action, se.cv.action)


#Table E3 Matrix
T6 <- matrix(c(T6.R1,T6.R2,T6.R3,T6.R4,T6.R5,T6.R6,T6.R7), nrow = 7, ncol = 6, byrow = TRUE)
#Note that in the final table, we combine rows 1 and 2.
T6

#Table E3 Output
Table.SpillReg <- xtable(T6)
print.xtable(Table.SpillReg, type="html", file="TableE3SpillReg (7.27.2020).html", digits = 3)


##################################################
####### TABLE D1: PAP POWER CALCULATIONS #########
##################################################

#Note: The power calculations for the PAP were conducted in STATA, so we have copied the stata code (and output) below.

#. power twoproportions .01 .3, test(fisher) n1(15) n2(5) onesided

#Estimated power for a two-sample proportions test
#Fisher's exact test
#Ho: p2 = p1  versus  Ha: p2 > p1

#Study parameters:

 #       alpha =    0.0500
  #          N =        20
   #        N1 =        15
    #       N2 =         5
     #   N2/N1 =    0.3333
      #  delta =    0.2900  (difference)
       #    p1 =    0.0100
        #   p2 =    0.3000

#Estimated power and alpha:

 #       power =    0.1618
 #actual alpha =    0.0000

#. power twoproportions .01 .3, test(fisher) n1(30) n2(10) oneside
#> d

#Estimated power for a two-sample proportions test
#Fisher's exact test
#Ho: p2 = p1  versus  Ha: p2 > p1

#Study parameters:
  
# alpha =    0.0500
#N =        40
#N1 =        30
#N2 =        10
#N2/N1 =    0.3333
#delta =    0.2900  (difference)
#p1 =    0.0100
#p2 =    0.3000

#Estimated power and alpha:
  
# power =    0.6069
#actual alpha =    0.0001

#. power twoproportions .01 .3, test(fisher) n1(60) n2(20) oneside
#> d

#Estimated power for a two-sample proportions test
#Fisher's exact test
#Ho: p2 = p1  versus  Ha: p2 > p1

#Study parameters:

#        alpha =    0.0500
#            N =        80
#          N1 =        60
#          N2 =        20
#       N2/N1 =    0.3333
#       delta =    0.2900  (difference)
#          p1 =    0.0100
#          p2 =    0.3000

#Estimated power and alpha:

#       power =    0.9528
#actual alpha =    0.0009


#############END