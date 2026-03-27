# R Project
#
# Janine Campbell
# Contact: janineinchile@gmail.com
#
# Date last modified: 10 April 2019
#
###############################################################################
# Before running the is code, follow these instructions
#     1. Save all files (R code files and .csv data files) in the same folder
#        on your computer
#     2. Change the working directory (line 31) to the folder where you have
#        saved the code and data
#     3. Code should then run
###############################################################################
#
###############################################################################
# Crisp Set Analysis with COMBINEDPISA data set
###############################################################################
#
#------------------------------------------------------------------------------
# Remove all objects surrently stored in active memory
#------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
# rm means remove, and it can also be used for an individual condition/variable
#------------------------------------------------------------------------------
# Check / set working directory
#------------------------------------------------------------------------------
getwd()
# Check that working directory is correct.  To change use following code:
setwd("/Users/janine/OneDrive/Study OTAGO/DATA") 
options(prompt="R> ")
#------------------------------------------------------------------------------
# Save Script with an original name 
#------------------------------------------------------------------------------
#
#    *****Manually save script*****
#
#------------------------------------------------------------------------------
# Activate packages for session
#------------------------------------------------------------------------------
library(readr)
library(QCA)
library(SetMethods)
library(ggplot2)
library(ggthemes)
#
#------------------------------------------------------------------------------
# Import csv file
#------------------------------------------------------------------------------
COMBINEDPISA <- read.csv("~/OneDrive/Study OTAGO/DATA/COMBINEDPISA.csv", row.names = 1)
head(COMBINEDPISA)
#
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
# Check cases in each of the outcome conditions (already coded in csv file)
#
rownames(COMBINEDPISA)[COMBINEDPISA$HIPISA==1]
#
rownames(COMBINEDPISA)[COMBINEDPISA$LOPISA==1]
#
rownames(COMBINEDPISA)[COMBINEDPISA$MIDPISA==1]
#
# Calibrate data into crisp sets
#
# Simplify GDPCAP into thousands (to reduce large labels on graphs etc)
COMBINEDPISA$GDPCAP<-COMBINEDPISA$GDPCAP/1000
#
# Calibrate sets with OECD average as cutoff
COMBINEDPISA$LOPOP <- ifelse(COMBINEDPISA$POP<34848,1,0)
COMBINEDPISA$HIGDPCAP <- ifelse(COMBINEDPISA$GDPCAP>38.88,1,0)
COMBINEDPISA$HIINCEQ <- ifelse(COMBINEDPISA$ININEQ<5.89,1,0)
COMBINEDPISA$HIDEM <- ifelse(COMBINEDPISA$DEM>8.13,1,0)
COMBINEDPISA$HIHDI <- ifelse(COMBINEDPISA$HDI>0.888,1,0)
COMBINEDPISA$LOGENINEQ <- ifelse(COMBINEDPISA$GENINEQ<0.123,1,0)
COMBINEDPISA$LOGENGAP <- ifelse(COMBINEDPISA$GENGAP>0.744,1,0)
COMBINEDPISA$HIECOFREE <- ifelse(COMBINEDPISA$ECOFREE>71.3,1,0)
COMBINEDPISA$LOIMM <- ifelse(COMBINEDPISA$IMM<7.3,1,0)
COMBINEDPISA$LOETHNICFRAC <- ifelse(COMBINEDPISA$ETHNICFRAC<0.25,1,0)
COMBINEDPISA$LOLANGFRAC <- ifelse(COMBINEDPISA$LANGFRAC<0.2345,1,0)
COMBINEDPISA$LORELFRAC <- ifelse(COMBINEDPISA$RELFRAC<0.4187,1,0)
#
COMBINEDPISA$HIPOWER <- ifelse(COMBINEDPISA$POWER>50,1,0)
COMBINEDPISA$HIINDIV <- ifelse(COMBINEDPISA$INDIV>50,1,0)
COMBINEDPISA$HICOMPET <- ifelse(COMBINEDPISA$COMPET>50,1,0)
COMBINEDPISA$HIUNCERT <- ifelse(COMBINEDPISA$UNCERT>50,1,0)
COMBINEDPISA$HILONG <- ifelse(COMBINEDPISA$LONG>50,1,0)
COMBINEDPISA$HIINDULG <- ifelse(COMBINEDPISA$INDULG>50,1,0)
#
# Remove raw data from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(2:19)]
#
#------------------------------------------------------------------------------
# Create condition HIGENEQ with "or" as gender conditions are functional equivalents
#------------------------------------------------------------------------------
#
COMBINEDPISA$HIGENEQ <- ifelse(COMBINEDPISA$LOGENINEQ==1 | COMBINEDPISA$LOGENGAP==1,1,0)
# 
# Remove original conditions from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(11:12)]
#
#------------------------------------------------------------------------------
# Save .csv data file with calibrated crisp sets
#------------------------------------------------------------------------------
#
write.csv(COMBINEDPISA, "CRISPCOMBINED.csv")
#
#******************************************************************************
# LINEAR ANALYSIS OF MACRO CONDITIONS 
#******************************************************************************
#
#------------------------------------------------------------------------------
# Analysis of correlations
#------------------------------------------------------------------------------
#
# Reload original data to explore correlations
COMBINEDPISA <- read.csv("~/OneDrive/Study OTAGO/DATA/COMBINEDPISA.csv", row.names = 1)
head(COMBINEDPISA)
#
# Drop cultural conditions from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(2:7)]
#
# Drop outcome sets from data frame (not required for correlational analysis)
COMBINEDPISA <- COMBINEDPISA[,-c(15:17)]
#
# Check correlation matrix (Pearson)
#
library("Hmisc")
macro_corr <- rcorr(as.matrix(COMBINEDPISA[,2:14]))
macro_corr
#
#########################################################################
# RESULTS                                                               #
#                                                                       #
# Variables with R >0.50:    GDPCAP   R=0.57,  p<0.0001                 #
#                            ININEQ   R=-0.59, p<0.0001                 #
#                            HDI      R=0.80,  p<0.0001                 #
#                            GENINEQ  R=-0.86, p<0.0001                 #
#                                                                       #
#########################################################################
# NOTE:                                                                 #
# All variables ex. LANGFRAC have stat sig correlation with PISASCORE   #
#                                                                       #
# Also of interest (due to hypotheses): IMM         R=0.47,  p=0.0007   #
#                                       ETHNICFRAC  R=-0.43, p=0.0019   #
#                                                                       #
#########################################################################
#                                                                       #
# Cross correlations (variables correlated with each other at R>0.5):   #
#                                                                       #
# GDPCAP <--> DEM, HID, GENINEQ, GENGAP, ECOFREE, IMM                   #
# ININEQ <--> HDI, GENINEQ                                              #
# DEM    <--> GDPCAP, HDI, GENINEQ, GENGAP, ECOFREE                     #
# HDI    <--> GDPCAP, DEM, GENINEQ, GENGAP, ECOFREE, IMM                #
# GENINEQ <-> GDPCAP, ININEQ, DEM, HDI, GENGAP, IMM, ETHNICFRAC         #
# IMM    <--> GDPCAP, HDI, GENINEQ, ECOFREE                             #
#                                                                       #
#########################################################################
#
# Produce scatterplots for each of the most interesting variables
#
# GDPCAP
ggplot(COMBINEDPISA,aes(GDPCAP,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(GDPCAP,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by GDP per capita") +
  xlab("GDP per capita") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# ININEQ
ggplot(COMBINEDPISA,aes(ININEQ,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(ININEQ,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by 80/20 Index of Income Inequality") +
  xlab("80/20 Income Inequality (higher equals more inequal)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# HDI
ggplot(COMBINEDPISA,aes(HDI,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(HDI,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Human Development Index") +
  xlab("Human Development Index (higher equals more developed)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# GENINEQ
ggplot(COMBINEDPISA,aes(GENINEQ,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(GENINEQ,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Gender Inequality Index") +
  xlab("Gender Inequality Index (higher equals more inequal)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# GENGAP
ggplot(COMBINEDPISA,aes(GENGAP,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(GENGAP,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Gender Gap Index") +
  xlab("Gender Gap Index (lower equals more inequal)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# IMM
ggplot(COMBINEDPISA,aes(IMM,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(IMM,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Immigrant stock (10-14 years)") +
  xlab("Immigrant stock (10-14 years)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# ETHNICFRAC
ggplot(COMBINEDPISA,aes(ETHNICFRAC,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(ETHNICFRAC,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Ethnic Fractionalization Index") +
  xlab("Ethnic Fractionalization Index (higher equals more fractionalization)") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
#------------------------------------------------------------------------------
# Analysis of necessity for HIPISA
#------------------------------------------------------------------------------
#
# Load csv file for crisp set conditions
CRISPCOMBINED <- read.csv("~/OneDrive/Study OTAGO/DATA/CRISPCOMBINED.csv", row.names = 1)
head(CRISPCOMBINED)
#
# Remove culture conditions
CRISPCOMBINED <- CRISPCOMBINED[,-c(16:21)]
head(CRISPCOMBINED)
#
# Testing all conditions in one command against HIPISA 
QCAfit(CRISPCOMBINED[,6:16], CRISPCOMBINED$HIPISA, necessity = TRUE, names(CRISPCOMBINED[,6:16]))
#
# Conduct a SUIN analysis
SUINHIPISA<-superSubset(CRISPCOMBINED[,3:16], outcome = "HIPISA", 
                  conditions = "LOPOP, HIINCEQ, HIDEM, HIHDI, HIECOFREE, LOIMM, LOETHNICFRAC, 
                  LOLANGFRAC, LORELFRAC, HIGENEQ",
                  neg.out = FALSE, relation = "nec",
                  incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINHIPISA
#
#------------------------------------------------------------------------------
# Analysis of Necessity for LOPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against LOPISA 
QCAfit(CRISPCOMBINED[,6:16], CRISPCOMBINED$LOPISA, necessity = TRUE, names(CRISPCOMBINED[,6:16]))
#
# Conduct a SUIN analysis
SUINLOPISA<-superSubset(CRISPCOMBINED[,3:16], outcome = "LOPISA", 
                        conditions = "LOPOP, HIINCEQ, HIDEM, HIHDI, HIECOFREE, LOIMM, LOETHNICFRAC, 
                        LOLANGFRAC, LORELFRAC, HIGENEQ",
                        neg.out = FALSE, relation = "nec",
                        incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINLOPISA
#
# Compare results for HIPISA and LOPISA
SUINHIPISA
SUINLOPISA
#
#########################################################################
# RESULTS                                                               #
#                                                                       #
# Conditions that repeat:    HIGENEQ-necessary alone in both directions #
#                            LOETHNICFRAC-SUIN CONDITION HIPISA         #
#                            HIINCEQ-SUIN CONDITION HIPISA              #
#                            HIHDI-SUIN CONDITION both directions       #
#                            LOIMM-SUIN CONDITION both directions       #
#                                                                       #
#########################################################################
#
#******************************************************************************
# ANALYSIS OF CULTURAL CONDITIONS 
#******************************************************************************
#
#------------------------------------------------------------------------------
# Analysis of correlations
#------------------------------------------------------------------------------
#
# Reload original data to explore correlations
COMBINEDPISA <- read.csv("~/OneDrive/Study OTAGO/DATA/COMBINEDPISA.csv", row.names = 1)
head(COMBINEDPISA)
#
# Drop macro conditions from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(8:19)]
#
# Drop outcome sets from data frame (not required for correlational analysis)
COMBINEDPISA <- COMBINEDPISA[,-c(9:11)]
#
# Check correlation matrix (Pearson)
#
library("Hmisc")
macro_corr <- rcorr(as.matrix(COMBINEDPISA[,2:8]))
macro_corr
#
#########################################################################
# RESULTS                                                               #
# Variables with statistically significant correlations                 #
#                            POWER    R=-0.46, p=0.0009                 #
#                            INDIV    R=0.54,  p<0.0001                 #
#                            LONG     R=0.36,  p=0.0104                 #
#                                                                       #
#########################################################################

#########################################################################
#                                                                       #
# Cross correlations (variables correlated with each other at R>0.5):   #
#                                                                       #
# POWER  <--> INDIV                                                     #
# LONG   <--> INDULG                                                    #
#                                                                       #
#########################################################################
#
# Produce scatterplots for each of the most interesting variables
#
# POWER
ggplot(COMBINEDPISA,aes(POWER,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(POWER,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Power Distance") +
  xlab("Power Distance") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# INDIV
ggplot(COMBINEDPISA,aes(INDIV,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(INDIV,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Individualism") +
  xlab("Individualism") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
# LONG
ggplot(COMBINEDPISA,aes(LONG,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(LONG,PISASCORE),method=lm,se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Long Term Planning") +
  xlab("Long Term Planning") +
  ylab("PISA score") +
  theme_calc()+ scale_colour_calc()
#
#------------------------------------------------------------------------------
# Analysis of necessity for HIPISA
#------------------------------------------------------------------------------
#
# Load csv file for crisp set conditions
CRISPCOMBINED <- read.csv("~/OneDrive/Study OTAGO/DATA/CRISPCOMBINED.csv", row.names = 1)
head(CRISPCOMBINED)
#
# Remove macro conditions
CRISPCOMBINED <- CRISPCOMBINED[,-c(6:15)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(12)]
head(CRISPCOMBINED)
#
# Testing all conditions in one command against HIPISA 
QCAfit(CRISPCOMBINED[,6:11], CRISPCOMBINED$HIPISA, necessity = TRUE, names(CRISPCOMBINED[,6:11]))
#
# Conduct a SUIN analysis
SUINHIPISA<-superSubset(CRISPCOMBINED[,3:11], outcome = "HIPISA", 
                        conditions = "HIPOWER, HIINDIV, HICOMPET, HIUNCERT, HILONG, HIINDULG",
                        neg.out = FALSE, relation = "nec",
                        incl.cut=0.95, cov.cut=0.5, ron.cut=0.6, use.tilde = TRUE)
# no combinations
#SUINHIPISA
#
#------------------------------------------------------------------------------
# Analysis of Necessity for LOPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against LOPISA 
QCAfit(CRISPCOMBINED[,6:11], CRISPCOMBINED$LOPISA, necessity = TRUE, names(CRISPCOMBINED[,6:11]))
#
# Conduct a SUIN analysis
SUINLOPISA<-superSubset(CRISPCOMBINED[,3:11], outcome = "LOPISA", 
                        conditions = "HIPOWER, HIINDIV, HICOMPET, HIUNCERT, HILONG, HIINDULG",
                        neg.out = FALSE, relation = "nec",
                        incl.cut=0.95, cov.cut=0.55, ron.cut=0.6, use.tilde = TRUE)
SUINLOPISA
#
#
#########################################################################
# RESULTS                                                               #
#                                                                       #
# Possible interesting conditions that repeat:                          #
#                            HIINDIV-SUIN LOPISA                        #
#                            HIPOWER-SUIN LOPISA                        #
#                                                                       #
#########################################################################
#
#
#******************************************************************************
# COMBINED ANALYSIS WITH MOST INTERESTING MACRO AND CULTURAL CONDITIONS
#******************************************************************************
#
# Load csv file for crisp set conditions
CRISPCOMBINED <- read.csv("~/OneDrive/Study OTAGO/DATA/CRISPCOMBINED.csv", row.names = 1)
head(CRISPCOMBINED)
#
# Remove unwanted conditions
CRISPCOMBINED <- CRISPCOMBINED[,-c(2)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(5)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(7)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(8)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(10:11)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(12:15)]
CRISPCOMBINED <- CRISPCOMBINED[,-c(5)]
head(CRISPCOMBINED)
#
# Testing all conditions in one command against HIPISA 
QCAfit(CRISPCOMBINED[,5:11], CRISPCOMBINED$HIPISA, necessity = TRUE, names(CRISPCOMBINED[,5:11]))
#
# Conduct a SUIN analysis
SUINHIPISA<-superSubset(CRISPCOMBINED[,2:11], outcome = "HIPISA", 
                        conditions = "HIPOWER, HIINDIV, HIINCEQ, HIHDI,
                        HIGENEQ, LOETHNICFRAC, LOIMM",
                        neg.out = FALSE, relation = "nec",
                        incl.cut=0.95, cov.cut=0.60, ron.cut=0.6, use.tilde = TRUE)
SUINHIPISA
#
#------------------------------------------------------------------------------
# Analysis of Necessity for LOPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against LOPISA 
QCAfit(CRISPCOMBINED[,5:11], CRISPCOMBINED$LOPISA, necessity = TRUE, names(CRISPCOMBINED[,5:11]))
#
# Conduct a SUIN analysis
SUINLOPISA<-superSubset(CRISPCOMBINED[,2:11], outcome = "LOPISA", 
                        conditions = "HIINDIV, HIINCEQ, HIHDI,
                        HIGENEQ, LOETHNICFRAC, LOIMM",
                        neg.out = FALSE, relation = "nec",
                        incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINLOPISA
#
SUINHIPISA
#
#------------------------------------------------------------------------------
# Test conditions for sufficiency with HIPISA (using SUIN solutions for HIPISA)
#------------------------------------------------------------------------------
#
TTSOLHIPISA<-truthTable(CRISPCOMBINED, outcome = "HIPISA", conditions = "HIGENEQ, HIINCEQ, HIHDI,
                   HIINDIV", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE, show.cases=T)
TTSOLHIPISA
#
SOLHIPISA_c <- minimize(TTSOLHIPISA, details=TRUE, show.cases=TRUE, row.dom = TRUE)
SOLHIPISA_c
#
#   
# CASES not covered by solution terms AUS, POL, PRT
# INCONSISTENT CASES AUT, ISL, CHE
#
#------------------------------------------------------------------------------
# Test conditions for sufficiency with LOPISA
#------------------------------------------------------------------------------
#
TTSOLLOPISA<-truthTable(CRISPCOMBINED, outcome = "LOPISA", conditions = "HIGENEQ,
                   HIINDIV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE, show.cases = T)
TTSOLLOPISA
#
SOLLOPISA_c <- minimize(TTSOLLOPISA, details=TRUE, show.cases=TRUE, row.dom = TRUE)
SOLLOPISA_c 
#
# CASES not covered by solution terms 0
# INCONSISTENT CASES HRV, RUS, VNM
#
# Compare solutions for HIPISA and LOPISA
SOLHIPISA_c
SOLLOPISA_c
#
#------------------------------------------------------------------------------
# Save CRSIPCOMBINED csv file with condition for membership in solutions
# 0 = not member in any solution
# 1 = member in HIGENEQ*HIHDI*~HIINDIV (HIPISA)
# 2 = member in HIGENEQ*HIINCEQ*HIINDIV (HIPISA)
# 3 = member in ~HIGENEQ*~HIINDIV*~HIHDI (LOPISA)
#------------------------------------------------------------------------------
#
# Load csv file for crisp set conditions
CRISPCOMBINED <- read.csv("~/OneDrive/Study OTAGO/DATA/CRISPCOMBINED.csv", row.names = 1)
head(CRISPCOMBINED)
#
CRISPCOMBINED$SOLUTION1 <- ifelse(CRISPCOMBINED$HIGENEQ==1 & CRISPCOMBINED$HIHDI==1 &
                                    CRISPCOMBINED$HIINDIV==0,1,0)
CRISPCOMBINED$SOLUTION2 <- ifelse(CRISPCOMBINED$HIGENEQ==1 & CRISPCOMBINED$HIINCEQ==1 &
                                    CRISPCOMBINED$HIINDIV==1,2,0)
CRISPCOMBINED$SOLUTION3 <- ifelse(CRISPCOMBINED$HIGENEQ==0 & CRISPCOMBINED$HIINDIV==0 &
                                    CRISPCOMBINED$HIHDI==0,3,0)
CRISPCOMBINED$SOLUTIONS <- CRISPCOMBINED$SOLUTION1+CRISPCOMBINED$SOLUTION2+CRISPCOMBINED$SOLUTION3
CRISPCOMBINED <- CRISPCOMBINED[,-c(12:14)]
head(CRISPCOMBINED)
#
# Save csv
write.csv(CRISPCOMBINED, "CRISPCOMBINED.csv")
#
###############################################################################
# Validation and Robustness Checks
###############################################################################
# Check Robustness of following solutions
# Necessity: HIGENEQ <= HIPISA
#            HIINDIV + HIHDI <= HIPISA
#            HIINCEQ + HIHDI <= HIPISA
#            higeneq*hiindiv <=> LOPISA
#
# Sufficiency: HIGENEQ*HIINCEQ*HIINDIV + HIGENEQ*HIHDI*hiindiv => HIPISA
#              higeneq*hiindiv <=> LOPISA
###############################################################################
# Also check how many non PISA countries meet the conditions of the solutions
###############################################################################
# Use MACRO-CULT-ALL data set
# includes 83 countries (34 more than COMBINEDPISA data set)
# includes 2 additional PISA countries - ISRAEL & COSTA RICA
###############################################################################
#
# Load csv file for all cases 
ALL <- read.csv("~/OneDrive/Study OTAGO/DATA/MACRO_CULT_ALL.csv", row.names = 1)
head(ALL)
#
#------------------------------------------------------------------------------
# Analysis of correlations
#------------------------------------------------------------------------------
#
# Drop cases without PISA results
ALLPISA <- ALL[ which(ALL$PISA==1), ]
head (ALLPISA)
#
# Drop PISA column
ALLPISA <- ALLPISA[,-c(2)]
#
# Check correlation matrix (Pearson)
#
macro_corr_valid <- rcorr(as.matrix(ALLPISA[,2:9]))
macro_corr_valid
#
#########################################################################
# RESULTS                                                               #
#                            ININEQ   R=-0.60, p<0.0001                 #
#                            HDI      R=0.80,  p<0.0001                 #
#                            GENINEQ  R=-0.86, p<0.0001                 #
#                            GENGAP   R=0.45,  p=0.0010                 #
#                            INDIV    R=0.56,  p<0.0001                 #
#                                                                       #
#########################################################################
#
# calibrate data into crisp sets
#
ALLPISA$HIINCEQ <- ifelse(ALLPISA$ININEQ<5.89,1,0)
ALLPISA$HIHDI <- ifelse(ALLPISA$HDI>0.888,1,0)
ALLPISA$LOGENINEQ <- ifelse(ALLPISA$GENINEQ<0.123,1,0)
ALLPISA$LOGENGAP <- ifelse(ALLPISA$GENGAP>0.744,1,0)
ALLPISA$HIINDIV <- ifelse(ALLPISA$INDIV>50,1,0)
#
# Remove original variables from data frame
ALLPISA <- ALLPISA[,-c(5:9)]
#
#------------------------------------------------------------------------------
# Create condition GENEQ as gender conditions are functional equivalents
#------------------------------------------------------------------------------
#
ALLPISA$HIGENEQ <- ifelse(ALLPISA$LOGENINEQ==1 | ALLPISA$LOGENGAP==1,1,0)
# 
# Remove original conditions from data frame
ALLPISA <- ALLPISA[,-c(7:8)]
#
#------------------------------------------------------------------------------
# Validation analysis of necessity for HIPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against HIPISA 
QCAfit(ALLPISA[,5:8], ALLPISA$HIPISA, necessity = TRUE, names(ALLPISA[,5:8]))
#
# Conduct a SUIN analysis
SUINHIPISAvalid<-superSubset(ALLPISA[,2:8], outcome = "HIPISA", 
                             conditions = "HIINCEQ, HIHDI, HIINDIV, HIGENEQ",
                             neg.out = FALSE, relation = "nec",
                             incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINHIPISAvalid
#
# Compare to results with 49 cases
SUINHIPISA
#
#------------------------------------------------------------------------------
# Validation analysis of Necessity for LOPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against LOPISA 
QCAfit(ALLPISA[,5:8], ALLPISA$LOPISA, necessity = TRUE, names(ALLPISA[,5:8]))
#
# Testing all negative conditions in one command against LOPISA
QCAfit(1-ALLPISA[,5:8], ALLPISA$LOPISA, necessity = TRUE, paste("not", names(ALLPISA[,5:8])))
#
# Conduct a SUIN analysis
SUINLOPISAvalid<-superSubset(ALLPISA[,2:8], outcome = "LOPISA", 
                             conditions = "HIINCEQ, HIHDI, HIINDIV, HIGENEQ",
                             neg.out = FALSE, relation = "nec",
                             incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINLOPISAvalid
#
# Compare to results with 49 cases
SUINLOPISA
#
#------------------------------------------------------------------------------
# Explore how many other countries in meet the conditions for the solutions
# DATA SET: ALL, number of cases 83 total
#------------------------------------------------------------------------------
#
ALL <- read.csv("~/OneDrive/Study OTAGO/DATA/MACRO_CULT_ALL.csv", row.names = 1)
head(ALL)
#
# Eliminate columns not needed for analysis
ALL <- ALL[,-c(2:5)]
head(ALL)
#
# Calibrate data
#
ALL$HIINCEQ <- ifelse(ALL$ININEQ<5.89,1,0)
ALL$HIHDI <- ifelse(ALL$HDI>0.888,1,0)
ALL$LOGENINEQ <- ifelse(ALL$GENINEQ<0.123,1,0)
ALL$LOGENGAP <- ifelse(ALL$GENGAP>0.744,1,0)
ALL$HIINDIV <- ifelse(ALL$INDIV>50,1,0)
#
# Remove raw data from data frame
ALL <- ALL[,-c(2:6)]
head(ALL)
#
# Create condition GENEQ as gender conditions are functional equivalents
ALL$HIGENEQ <- ifelse(ALL$LOGENINEQ==1 | ALL$LOGENGAP==1,1,0)
# 
# Remove original conditions from data frame
ALL <- ALL[,-c(4:5)]
head(ALL)
#
# Review number of countries for each solutions term
# 
# HIPISA
#
# HIGENEQ*HIINCEQ*HIINDIV
rownames(ALL)[ALL$HIGENEQ==1 & ALL$HIINCEQ==1 & ALL$HIINDIV==1]
# only 16 countries, the same countries as original analysis, no more countries with same configuration
#
# HIGENEQ*HIHDI*~HIINDIV
rownames(ALL)[ALL$HIGENEQ==1 & ALL$HIHDI==1 & ALL$HIINDIV==0]
# only 4 countries, the same countries as original analysis, no more countries with same configuration
#
# LOPISA
#
# ~HIGENEQ*~HIINDIV*~HIHDI
rownames(ALL)[ALL$HIGENEQ==0 & ALL$HIINDIV==0]
# 45 countries (54% of all countries with data), only 16 countries in original analysis, 
# 29 (of the 34 additional countries for which data is available) have the same configuration
#
#------------------------------------------------------------------------------
# Validate findings about GENEQ by repeating analysis with original data but 
# with slightly more stringent calibration cutoffs
# csv file "COMBINEDPISA"
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
# Import csv file
#------------------------------------------------------------------------------
COMBINEDPISA <- read.csv("~/OneDrive/Study OTAGO/DATA/COMBINEDPISA.csv", row.names = 1)
head(COMBINEDPISA)
#
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
# to calibrate data into crisp sets
#
# Simplify GDPCAP into thousands (to reduce labels on graphs etc)
COMBINEDPISA$GDPCAP<-COMBINEDPISA$GDPCAP/1000
#
# Calibrate sets with OECD average as cutoff
COMBINEDPISA$LOPOP <- ifelse(COMBINEDPISA$POP<34848,1,0)
COMBINEDPISA$HIGDPCAP <- ifelse(COMBINEDPISA$GDPCAP>38.88,1,0)
COMBINEDPISA$HIINCEQ <- ifelse(COMBINEDPISA$ININEQ<5.89,1,0)
COMBINEDPISA$HIDEM <- ifelse(COMBINEDPISA$DEM>8.13,1,0)
COMBINEDPISA$HIHDI <- ifelse(COMBINEDPISA$HDI>0.888,1,0)
COMBINEDPISA$HIECOFREE <- ifelse(COMBINEDPISA$ECOFREE>71.3,1,0)
COMBINEDPISA$LOIMM <- ifelse(COMBINEDPISA$IMM<7.3,1,0)
COMBINEDPISA$LOETHNICFRAC <- ifelse(COMBINEDPISA$ETHNICFRAC<0.25,1,0)
COMBINEDPISA$LOLANGFRAC <- ifelse(COMBINEDPISA$LANGFRAC<0.2345,1,0)
COMBINEDPISA$LORELFRAC <- ifelse(COMBINEDPISA$RELFRAC<0.4187,1,0)
#
# Change calibration of GENINEQ to 0.121 instead of 0.123
COMBINEDPISA$LOGENINEQ <- ifelse(COMBINEDPISA$GENINEQ<0.121,1,0)
# Change calibration of GENINEQ to 0.746 instead of 0.744
COMBINEDPISA$LOGENGAP <- ifelse(COMBINEDPISA$GENGAP>0.746,1,0)
#
# Calibrate culture data
COMBINEDPISA$HIPOWER <- ifelse(COMBINEDPISA$POWER>50,1,0)
COMBINEDPISA$HIINDIV <- ifelse(COMBINEDPISA$INDIV>50,1,0)
COMBINEDPISA$HICOMPET <- ifelse(COMBINEDPISA$COMPET>50,1,0)
COMBINEDPISA$HIUNCERT <- ifelse(COMBINEDPISA$UNCERT>50,1,0)
COMBINEDPISA$HILONG <- ifelse(COMBINEDPISA$LONG>50,1,0)
COMBINEDPISA$HIINDULG <- ifelse(COMBINEDPISA$INDULG>50,1,0)
#
# Remove raw data from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(2:19)]
#
# Remove unnecesary conditions
COMBINEDPISA <- COMBINEDPISA[,-c(5:7)]
COMBINEDPISA <- COMBINEDPISA[,-c(6)]
COMBINEDPISA <- COMBINEDPISA[,-c(7:11)]
COMBINEDPISA <- COMBINEDPISA[,-c(9)]
COMBINEDPISA <- COMBINEDPISA[,-c(10:13)]
#
#------------------------------------------------------------------------------
# Create condition GENEQ as gender conditions are functional equivalents
#------------------------------------------------------------------------------
#
COMBINEDPISA$HIGENEQ <- ifelse(COMBINEDPISA$LOGENINEQ==1 | COMBINEDPISA$LOGENGAP==1,1,0)
# 
# Remove original conditions from data frame
COMBINEDPISA <- COMBINEDPISA[,-c(7:8)]
#
#------------------------------------------------------------------------------
# Analysis of necessity for HIPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against HIPISA 
QCAfit(COMBINEDPISA[,5:8], COMBINEDPISA$HIPISA, necessity = TRUE, names(COMBINEDPISA[,5:8]))
#
# Conduct a SUIN analysis
SUINHIPISAgenvalid<-superSubset(COMBINEDPISA[,3:8], outcome = "HIPISA", 
                                conditions = "HIINCEQ, HIHDI, HIINDIV, HIGENEQ",
                                neg.out = FALSE, relation = "nec",
                                incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINHIPISAgenvalid
#
#------------------------------------------------------------------------------
# Analysis of Necessity for LOPISA
#------------------------------------------------------------------------------
#
# Testing all conditions in one command against LOPISA 
QCAfit(COMBINEDPISA[,5:8], COMBINEDPISA$LOPISA, necessity = TRUE, names(COMBINEDPISA[,5:8]))
#
# Conduct a SUIN analysis
SUINLOPISAgenvalid<-superSubset(COMBINEDPISA[,3:8], outcome = "LOPISA", 
                                conditions = "HIINCEQ, HIHDI, HIINDIV, HIGENEQ",
                                neg.out = FALSE, relation = "nec",
                                incl.cut=0.95, cov.cut=0.6, ron.cut=0.6, use.tilde = TRUE)
SUINLOPISAgenvalid
#
# Compare results for HIPISA and LOPISA, original and validated
SUINHIPISA
SUINHIPISAgenvalid
#
SUINLOPISA
SUINLOPISAgenvalid
#
#------------------------------------------------------------------------------
# Save Script
#------------------------------------------------------------------------------
#
#    *****Manually save script*****
#
#------------------------------------------------------------------------------
# Quit R
#------------------------------------------------------------------------------
q()


