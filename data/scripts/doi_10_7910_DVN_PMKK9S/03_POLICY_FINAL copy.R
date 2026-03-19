# R Coding Script
#
# Janine Campbell
# Date last modified: 10 April 2019
#
###############################################################################
# Before running the is code, follow these instructions
#     1. Save all files (R code files and .csv data files) in the same folder
#        on your computer
#     2. Change the working directory (line 28) to the folder where you have
#        saved the code and data
#     3. Code should then run
###############################################################################
#
###############################################################################
# Analysis with POLICY data set 
###############################################################################
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
# Input data from Excel csv file
#------------------------------------------------------------------------------
# to activate packages for session library(package name) - load all necessary
library(readr)
library(venn)
library(QCA)
library(SetMethods)
library(devtools)
library(plyr)
library(ggplot2)
library(ggthemes)
library(sjPlot)
library(sjmisc)
library(jtools)
library(interactions)
#
options("jtools-digits" = 4)
options(scipen = 999)
theme_set(theme_sjplot())
#
# Import csv file CUMEXP
CUMEXP <- read.csv("CUMEXP.csv", row.names = 1)
head(CUMEXP)
#
# Import csv file HOURS
HOURS <- read.csv("HOURS.csv", row.names = 1)
head(HOURS)
#
# Import csv file PUBENR
PUBENR <- read.csv("PUBENR.csv", row.names = 1)
head(PUBENR)
#
# Import csv file PRIEXP
PRIEXP <- read.csv("PRIEXP.csv", row.names = 1)
head(PRIEXP)
#
# Import csv file CLASS
CLASS <- read.csv("CLASS.csv", row.names = 1)
head(CLASS)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CUMULATIVE EXPENDITURE
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
CUMEXP$HICUMEXP <- ifelse(CUMEXP$CUMEXP>=85000,1,0)
#
rownames(CUMEXP)[CUMEXP$HICUMEXP==1]
rownames(CUMEXP)[CUMEXP$HICUMEXP==0]
#
rownames(CUMEXP)[CUMEXP$HIPISA==1]
rownames(CUMEXP)[CUMEXP$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of CUMEXP to PISASCORE
#------------------------------------------------------------------------------
#
CORR_CUMEXP <- cor.test(CUMEXP$CUMEXP, CUMEXP$PISASCORE, method = "pearson")
CORR_CUMEXP
#
#------------------------------------------------------------------------------
# Analysis of CUMEXP without outcome enabling conditions
#------------------------------------------------------------------------------
#
CUMEXP$FHICUMEXP <- factor(CUMEXP$HICUMEXP)
#
ggplot(CUMEXP,aes(CUMEXP,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(CUMEXP,PISASCORE),method=lm, colour = "grey", se=FALSE, linetype="solid", size = 1) +
  ggtitle("PISA scores by Cumulative Expenditure ") +
  xlab("Cumulative expenditure on education") +
  ylab("PISA score") + ylim(335,610) +
  theme_sjplot()+ scale_colour_grey()
#
# REGRESSION MODEL
lmCUMEXP <- lm(PISASCORE ~ CUMEXP, data=CUMEXP)
summary(lmCUMEXP)
summ(lmCUMEXP)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCUMEXP)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# CUMEXP IS SIGNIFICANT (p=0.00742) MODEL SIGNIFICANT (p=0.00742)
#
plot_model(lmCUMEXP, type = "pred", terms = c("CUMEXP"))
#
summ(lmCUMEXP, digits = 4)
effect_plot(lmCUMEXP, pred = CUMEXP, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of CUMEXP with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
CUMEXP$FHIINCEQ <- factor(CUMEXP$HIINCEQ)
ddply(CUMEXP,c("HICUMEXP","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CUMEXP,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHICUMEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HICUMEXP \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nCumulative \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CUMEXP,aes(CUMEXP,PISASCORE)) + 
              geom_point(aes(CUMEXP,PISASCORE,colour=factor(HIINCEQ)))+
              geom_smooth(data=subset(CUMEXP,HIINCEQ==0),
              aes(CUMEXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
              geom_smooth(data=subset(CUMEXP,HIINCEQ==1),
              aes(CUMEXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
              ggtitle("Observed relationship (grouped by HIINCEQ)") +
              xlab("Cumulative expenditure on education") +
              ylab("PISA score") + ylim(335,610) +
              labs(color = "HIINCEQ") +
              guides(size = "none") +
              scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCUMEXPHIINCEQ <- lm(PISASCORE ~ CUMEXP + HIINCEQ, data=CUMEXP)
summary(lmCUMEXPHIINCEQ)
#
summ(lmCUMEXPHIINCEQ)
#
# HIINCEQ IS NOT SIGNIFICANT (p=0.0646)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCUMEXPxHIINCEQ <- lm(PISASCORE ~ CUMEXP + HIINCEQ + CUMEXP:HIINCEQ, data=CUMEXP)
summary(lmCUMEXPxHIINCEQ)
#
summ(lmCUMEXPxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.297)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
CUMEXP$FHIGENEQ <- factor(CUMEXP$HIGENEQ)
ddply(CUMEXP,c("HICUMEXP","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))

#
ggplot(CUMEXP,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHICUMEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HICUMEXP \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nCumulative \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSIP
#
ggplot(CUMEXP,aes(CUMEXP,PISASCORE)) + 
  geom_point(aes(CUMEXP,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(CUMEXP,HIGENEQ==0),
              aes(CUMEXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(CUMEXP,HIGENEQ==1),
              aes(CUMEXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Cumulative expenditure on education") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCUMEXPHIGENEQ <- lm(PISASCORE ~ CUMEXP + HIGENEQ, data=CUMEXP)
summary(lmCUMEXPHIGENEQ)
#
# CUMEXP IS NOT SIGNIFICANT (p=0.313)
#
summ(lmCUMEXPHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCUMEXPxHIGENEQ <- lm(PISASCORE ~ CUMEXP + HIGENEQ + CUMEXP:HIGENEQ, data=CUMEXP)
summary(lmCUMEXPxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.056)
#
summ(lmCUMEXPxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
CUMEXP$FHIHDI <- factor(CUMEXP$HIHDI)
ddply(CUMEXP,c("HICUMEXP","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CUMEXP,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHICUMEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HICUMEXP \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nCumulative \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CUMEXP,aes(CUMEXP,PISASCORE)) + 
  geom_point(aes(CUMEXP,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(CUMEXP,HIHDI==0),
              aes(CUMEXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(CUMEXP,HIHDI==1),
              aes(CUMEXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("CUMEXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCUMEXPHIHDI <- lm(PISASCORE ~ CUMEXP + HIHDI, data=CUMEXP)
summary(lmCUMEXPHIHDI)
#
# CUMEXP IS NOT SIGNIFICANT (p=0.7268)
#
summ(lmCUMEXPHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCUMEXPxHIHDI <- lm(PISASCORE ~ CUMEXP + HIHDI + CUMEXP:HIHDI, data=CUMEXP)
summary(lmCUMEXPxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p<0.001), model significant (p<0.001)
#
plot_model(lmCUMEXPxHIHDI, type = "pred", terms = c("CUMEXP", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmCUMEXPxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCUMEXPxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
CUMEXP$FHIINDIV <- factor(CUMEXP$HIINDIV)
ddply(CUMEXP,c("HICUMEXP","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CUMEXP,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHICUMEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HICUMEXP \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nCumulative \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CUMEXP,aes(CUMEXP,PISASCORE)) + 
  geom_point(aes(CUMEXP,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(CUMEXP,HIINDIV==0),
              aes(CUMEXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(CUMEXP,HIINDIV==1),
              aes(CUMEXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("CUMEXP") +
  ylab("PISASCORE") + ylim(335, 610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  theme_sjplot()+ scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCUMEXPHIINDIV <- lm(PISASCORE ~ CUMEXP + HIINDIV, data=CUMEXP)
summary(lmCUMEXPHIINDIV)
#
# HIINDIV IS NOT SIGNIFICANT (p=0.647)
#
summ(lmCUMEXPHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCUMEXPxHIINDIV <- lm(PISASCORE ~ CUMEXP + HIINDIV + CUMEXP:HIINDIV, data=CUMEXP)
summary(lmCUMEXPxHIINDIV)
#
# THE INTERACTION TERM IS SIGNIFICANT (p<0.001), model significant (p<0.001)
#
plot_model(lmCUMEXPxHIINDIV, type = "pred", terms = c("CUMEXP", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmCUMEXPxHIINDIV)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCUMEXPxHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REMOVE FACTORS FROM DATA FRAME
CUMEXP <- CUMEXP[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
CUMEXP <- CUMEXP[,-c(2)]
#
# Reposition PISASCORE to end of data grid
CUMEXP$PISA <- CUMEXP$PISASCORE
CUMEXP <- CUMEXP[,-c(9)]
#
rownames(CUMEXP)[CUMEXP$HIPISA==1]
rownames(CUMEXP)[CUMEXP$HIPISA==0]
rownames(CUMEXP)[CUMEXP$LOPISA==1]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of CUMEXP for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(CUMEXP$HICUMEXP, CUMEXP$HIPISA, necessity = FALSE)
QCAfit(1-CUMEXP$HICUMEXP, CUMEXP$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of CUMEXP for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(CUMEXP$HICUMEXP, CUMEXP$LOPISA, necessity = FALSE)
QCAfit(1-CUMEXP$HICUMEXP, CUMEXP$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTCUMEXP1<-truthTable(CUMEXP, outcome = "HIPISA", conditions = "HIGENEQ, HIINCEQ,
                     HIHDI, HIINDIV, HICUMEXP", 
                     incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTCUMEXP1
#
sol_CUMEXP1_c <- minimize(TTCUMEXP1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_CUMEXP1_c
#
# With individual OECs
TTCUMEXPHDI<-truthTable(CUMEXP, outcome = "HIPISA", conditions = "HIHDI, HICUMEXP", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTCUMEXPHDI
#
TTCUMEXPGEN<-truthTable(CUMEXP, outcome = "HIPISA", conditions = "HIGENEQ, HICUMEXP", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTCUMEXPGEN
#
TTCUMEXPINC<-truthTable(CUMEXP, outcome = "HIPISA", conditions = "HIINCEQ, HICUMEXP", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTCUMEXPINC
#
TTCUMEXPINDIV<-truthTable(CUMEXP, outcome = "HIPISA", conditions = "HIINDIV, HICUMEXP", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTCUMEXPINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA not conducted as only three cases
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
# Check CUMEXP as subset of HDI
#------------------------------------------------------------------------------
QCAfit(CUMEXP$HIHDI, CUMEXP$HICUMEXP, necessity = TRUE)
#
rownames(CUMEXP)[CUMEXP$HICUMEXP==1]
rownames(CUMEXP)[CUMEXP$HICUMEXP==1 & CUMEXP$HIHDI==0]
#
# HICUMEXP is a subset of HIHDI (with the exception of Italy)
# CORRELATION HDI_CUMEXP 0.735
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# HOURS
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
HOURS$HIHOURS <- ifelse(HOURS$HOURS>=27,1,0)
#
rownames(HOURS)[HOURS$HIHOURS==1]
rownames(HOURS)[HOURS$HIHOURS==0]
#
rownames(HOURS)[HOURS$HIPISA==1]
rownames(HOURS)[HOURS$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of HOURS to PISASCORE
#------------------------------------------------------------------------------
#
CORR_HOURS <- cor.test(HOURS$HOURS, HOURS$PISASCORE, method = "pearson")
CORR_HOURS
#
#------------------------------------------------------------------------------
# Analysis of HOURS without outcome exabling conditions
#------------------------------------------------------------------------------
#
HOURS$FHIHOURS <- factor(HOURS$HIHOURS)
#
ggplot(HOURS,aes(HOURS,PISASCORE)) + 
  geom_point()+
 # geom_smooth(aes(HOURS,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by hours of instruction ") +
  xlab("Hours of instruction") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmHOURS <- lm(PISASCORE ~ HOURS, data=HOURS)
summary(lmHOURS)
summ(lmHOURS)
#
# HOURS IS NOT SIGNIFICANT (p=0.863) 
#
#------------------------------------------------------------------------------
# Analysis of HOURS with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
HOURS$FHIINCEQ <- factor(HOURS$HIINCEQ)
ddply(HOURS,c("HIHOURS","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(HOURS,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIHOURS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIHOURS \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "HIGH \nhours of \ninstruction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(HOURS,aes(HOURS,PISASCORE)) + 
  geom_point(aes(HOURS,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(HOURS,HIINCEQ==0),
              aes(HOURS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(HOURS,HIINCEQ==1),
              aes(HOURS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Hours of instruction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nIncome \nEquality") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmHOURSHIINCEQ <- lm(PISASCORE ~ HOURS + HIINCEQ, data=HOURS)
summary(lmHOURSHIINCEQ)
#
summ(lmHOURSHIINCEQ)
#
# HIINCEQ IS NOT SIGNIFICANT (p=0.333)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmHOURSxHIINCEQ <- lm(PISASCORE ~ HOURS + HIINCEQ + HOURS:HIINCEQ, data=HOURS)
summary(lmHOURSxHIINCEQ)
#
summ(lmHOURSxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.961)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
HOURS$FHIGENEQ <- factor(HOURS$HIGENEQ)
ddply(HOURS,c("HIHOURS","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(HOURS,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIHOURS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIHOURS \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nhours of \ninstruction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(HOURS,aes(HOURS,PISASCORE)) + 
  geom_point(aes(HOURS,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(HOURS,HIGENEQ==0),
              aes(HOURS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(HOURS,HIGENEQ==1),
              aes(HOURS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Hours of instruction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nGender \nEquity") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmHOURSHIGENEQ <- lm(PISASCORE ~ HOURS + HIGENEQ, data=HOURS)
summary(lmHOURSHIGENEQ)
#
# HOURS IS NOT SIGNIFICANT (p=0.91)
#
summ(lmHOURSHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmHOURSxHIGENEQ <- lm(PISASCORE ~ HOURS + HIGENEQ + HOURS:HIGENEQ, data=HOURS)
summary(lmHOURSxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.674)
#
summ(lmHOURSxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
HOURS$FHIHDI <- factor(HOURS$HIHDI)
ddply(HOURS,c("HIHOURS","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(HOURS,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIHOURS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIHOURS \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nhours of \ninstruction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(HOURS,aes(HOURS,PISASCORE)) + 
  geom_point(aes(HOURS,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(HOURS,HIHDI==0),
              aes(HOURS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(HOURS,HIHDI==1),
              aes(HOURS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("Hours of instruction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nHuman \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmHOURSHIHDI <- lm(PISASCORE ~ HOURS + HIHDI, data=HOURS)
summary(lmHOURSHIHDI)
#
# HOURS IS NOT SIGNIFICANT (p=0.949)
#
summ(lmHOURSHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmHOURSxHIHDI <- lm(PISASCORE ~ HOURS + HIHDI + HOURS:HIHDI, data=HOURS)
summary(lmHOURSxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.575)
#
summ(lmHOURSxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
HOURS$FHIINDIV <- factor(HOURS$HIINDIV)
ddply(HOURS,c("HIHOURS","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(HOURS,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIHOURS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIHOURS \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nhours of \ninstruction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(HOURS,aes(HOURS,PISASCORE)) + 
  geom_point(aes(HOURS,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(HOURS,HIINDIV==0),
              aes(HOURS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(HOURS,HIINDIV==1),
              aes(HOURS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Hours of instruction") +
  ylab("PISA score") + ylim(335, 610) +
  labs(color = "High \nIndividualism") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmHOURSHIINDIV <- lm(PISASCORE ~ HOURS + HIINDIV, data=HOURS)
summary(lmHOURSHIINDIV)
#
# HOURS IS NOT SIGNIFICANT (p=0.227)
#
summ(lmHOURSHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmHOURSxHIINDIV <- lm(PISASCORE ~ HOURS + HIINDIV + HOURS:HIINDIV, data=HOURS)
summary(lmHOURSxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.380)
#
summ(lmHOURSxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
HOURS <- HOURS[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
HOURS <- HOURS[,-c(2)]
#
# Reposition PISASCORE to end of data grid
HOURS$PISA <- HOURS$PISASCORE
HOURS <- HOURS[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of HOURS for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(HOURS$HIHOURS, HOURS$HIPISA, necessity = FALSE)
QCAfit(1-HOURS$HIHOURS, HOURS$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of HOURS for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(HOURS$HIHOURS, HOURS$LOPISA, necessity = FALSE)
QCAfit(1-HOURS$HIHOURS, HOURS$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTHOURS1<-truthTable(HOURS, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,
                     HIINDIV, HIINCEQ, HIHOURS", 
                     incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTHOURS1
#
sol_HOURS1_c <- minimize(TTHOURS1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_HOURS1_c
#
# With individual OECs
TTHOURSHDI<-truthTable(HOURS, outcome = "HIPISA", conditions = "HIHDI, HIHOURS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTHOURSHDI
#
TTHOURSGEN<-truthTable(HOURS, outcome = "HIPISA", conditions = "HIGENEQ, HIHOURS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTHOURSGEN
#
TTHOURSINC<-truthTable(HOURS, outcome = "HIPISA", conditions = "HIINCEQ, HIHOURS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTHOURSINC
#
TTHOURSINDIV<-truthTable(HOURS, outcome = "HIPISA", conditions = "HIINDIV, HIHOURS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTHOURSINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA not conducted as there is no evidence of relationship
#------------------------------------------------------------------------------
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CLASS
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
CLASS$BIGCLASS <- ifelse(CLASS$CLASS>=25,1,0)
#
rownames(CLASS)[CLASS$BIGCLASS==1]
rownames(CLASS)[CLASS$BIGCLASS==0]
#
rownames(CLASS)[CLASS$HIPISA==1]
rownames(CLASS)[CLASS$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of CLASS to PISASCORE
#------------------------------------------------------------------------------
#
CORR_CLASS <- cor.test(CLASS$CLASS, CLASS$PISASCORE, method = "pearson")
CORR_CLASS
#
#------------------------------------------------------------------------------
# Analysis of CLASS without outcome exabling conditions
#------------------------------------------------------------------------------
#
CLASS$FBIGCLASS <- factor(CLASS$BIGCLASS)
#
ggplot(CLASS,aes(CLASS,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(CLASS,PISASCORE),method=lm,se=FALSE, color = "grey", linetype="solid", size = 1) +
  ggtitle("PISA scores by class size") +
  xlab("Class size") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmCLASS <- lm(PISASCORE ~ CLASS, data=CLASS)
summary(lmCLASS)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCLASS)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# CLASS IS SIGNIFICANT (p=0.00106) MODEL SIGNIFICANT (p=0.00106)
#
plot_model(lmCLASS, type = "pred", terms = c("CLASS"))
#
summ(lmCLASS)
effect_plot(lmCLASS, pred = CLASS, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of CLASS with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
CLASS$FHIINCEQ <- factor(CLASS$HIINCEQ)
ddply(CLASS,c("BIGCLASS","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CLASS,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FBIGCLASS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by BIGCLASS \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "Big \nClass \nSize") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CLASS,aes(CLASS,PISASCORE)) + 
  geom_point(aes(CLASS,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(CLASS,HIINCEQ==0),
              aes(CLASS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(CLASS,HIINCEQ==1),
              aes(CLASS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Class size") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nIncome \nEquality") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCLASSHIINCEQ <- lm(PISASCORE ~ CLASS + HIINCEQ, data=CLASS)
summary(lmCLASSHIINCEQ)
#
# BOTH TERMS ARE SIGNIFICANT, MODEL IS SIGNIFICANT (p=0.0002)
#
summ(lmCLASSHIINCEQ)
plot_model(lmCLASSHIINCEQ, type = "pred", terms = c("CLASS", "HIINCEQ")) + scale_colour_grey() +
  ylim(335,610)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCLASSxHIINCEQ <- lm(PISASCORE ~ CLASS + HIINCEQ + CLASS:HIINCEQ, data=CLASS)
summary(lmCLASSxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.181)
#
summ(lmCLASSxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
CLASS$FHIGENEQ <- factor(CLASS$HIGENEQ)
ddply(CLASS,c("BIGCLASS","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CLASS,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FBIGCLASS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by BIGCLASS \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "Big \nClass \nSize") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CLASS,aes(CLASS,PISASCORE)) + 
  geom_point(aes(CLASS,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(CLASS,HIGENEQ==0),
              aes(CLASS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(CLASS,HIGENEQ==1),
              aes(CLASS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("CLASS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCLASSHIGENEQ <- lm(PISASCORE ~ CLASS + HIGENEQ, data=CLASS)
summary(lmCLASSHIGENEQ)
#
# CLASS IS NOT SIGNIFICANT (p=0.192)
#
summ(lmCLASSHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCLASSxHIGENEQ <- lm(PISASCORE ~ CLASS + HIGENEQ + CLASS:HIGENEQ, data=CLASS)
summary(lmCLASSxHIGENEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.003), model significant (p<0.001)
#
plot_model(lmCLASSxHIGENEQ, type = "pred", terms = c("CLASS", "HIGENEQ")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmCLASSxHIGENEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCLASSxHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIHDI
#
CLASS$FHIHDI <- factor(CLASS$HIHDI)
ddply(CLASS,c("BIGCLASS","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CLASS,aes(x=FHIHDI,y=PISASCORE, fill=factor(FBIGCLASS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by BIGCLASS \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "Big \nClass \nSize") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CLASS,aes(CLASS,PISASCORE)) + 
  geom_point(aes(CLASS,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(CLASS,HIHDI==0),
              aes(CLASS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(CLASS,HIHDI==1),
              aes(CLASS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("CLASS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCLASSHIHDI <- lm(PISASCORE ~ CLASS + HIHDI, data=CLASS)
summary(lmCLASSHIHDI)
#
# BOTH TERMS ARE SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
plot_model(lmCLASSHIHDI, type = "pred", terms = c("CLASS", "HIHDI")) + scale_colour_grey() + 
  ylim(335,610)
#
summ(lmCLASSHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCLASSxHIHDI <- lm(PISASCORE ~ CLASS + HIHDI + CLASS:HIHDI, data=CLASS)
summary(lmCLASSxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.002), model significant (p<0.001)
#
plot_model(lmCLASSxHIHDI, type = "pred", terms = c("CLASS", "HIHDI")) + scale_colour_grey() + 
  ylim(335,610)
#
summ(lmCLASSxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmCLASSxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
CLASS$FHIINDIV <- factor(CLASS$HIINDIV)
ddply(CLASS,c("BIGCLASS","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(CLASS,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FBIGCLASS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by BIGCLASS \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "Big \nClass \nSize") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(CLASS,aes(CLASS,PISASCORE)) + 
  geom_point(aes(CLASS,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(CLASS,HIINDIV==0),
              aes(CLASS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(CLASS,HIINDIV==1),
              aes(CLASS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Class size") +
  ylab("PISA score") + ylim(335, 610) +
  labs(color = "High \nIndividualism") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmCLASSHIINDIV <- lm(PISASCORE ~ CLASS + HIINDIV, data=CLASS)
summary(lmCLASSHIINDIV)
#
# CLASS IS NOT SIGNIFICANT (p=0.266)
#
summ(lmCLASSHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmCLASSxHIINDIV <- lm(PISASCORE ~ CLASS + HIINDIV + CLASS:HIINDIV, data=CLASS)
summary(lmCLASSxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.610)
#
summ(lmCLASSxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
CLASS <- CLASS[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
CLASS <- CLASS[,-c(2)]
#
# Reposition PISASCORE to end of data grid
CLASS$PISA <- CLASS$PISASCORE
CLASS <- CLASS[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of BIGCLASS for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(CLASS$BIGCLASS, CLASS$HIPISA, necessity = FALSE)
QCAfit(1-CLASS$BIGCLASS, CLASS$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of BIGCLASS for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(CLASS$BIGCLASS, CLASS$LOPISA, necessity = FALSE)
QCAfit(1-CLASS$BIGCLASS, CLASS$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTCLASS1<-truthTable(CLASS, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,
                     HIINDIV, HIINCEQ, BIGCLASS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTCLASS1
#
sol_CLASS1_c <- minimize(TTCLASS1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_CLASS1_c
#
# With individual OECs
TTCLASSHDI<-truthTable(CLASS, outcome = "HIPISA", conditions = "HIHDI, BIGCLASS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTCLASSHDI
#
TTCLASSGEN<-truthTable(CLASS, outcome = "HIPISA", conditions = "HIGENEQ, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSGEN
#
TTCLASSINC<-truthTable(CLASS, outcome = "HIPISA", conditions = "HIINCEQ, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSINC
#
TTCLASSINDIV<-truthTable(CLASS, outcome = "HIPISA", conditions = "HIINDIV, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTCLASSLO1<-truthTable(CLASS, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI,
                     HIINDIV, HIINCEQ, BIGCLASS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTCLASSLO1
#
sol_CLASSLO1_c <- minimize(TTCLASSLO1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_CLASSLO1_c
#
# With individual OECs
TTCLASSHDILO<-truthTable(CLASS, outcome = "LOPISA", conditions = "HIHDI, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSHDILO
#
TTCLASSGENLO<-truthTable(CLASS, outcome = "LOPISA", conditions = "HIGENEQ, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSGENLO
#
TTCLASSINCLO<-truthTable(CLASS, outcome = "LOPISA", conditions = "HIINCEQ, BIGCLASS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTCLASSINCLO
#
TTCLASSINDIVLO<-truthTable(CLASS, outcome = "LOPISA", conditions = "HIINDIV, BIGCLASS", 
                         incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                         complete=TRUE)
TTCLASSINDIVLO
#
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PUBENR
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
PUBENR$HIPUBENR <- ifelse(PUBENR$PUBENR>=90,1,0)
#
rownames(PUBENR)[PUBENR$HIPUBENR==1]
rownames(PUBENR)[PUBENR$HIPUBENR==0]
#
rownames(PUBENR)[PUBENR$HIPISA==1]
rownames(PUBENR)[PUBENR$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of PUBENR to PISASCORE
#------------------------------------------------------------------------------
#
CORR_PUBENR <- cor.test(PUBENR$PUBENR, PUBENR$PISASCORE, method = "pearson")
CORR_PUBENR
#
#------------------------------------------------------------------------------
# Analysis of PUBENR by outcome exabling conditions
#------------------------------------------------------------------------------
#
PUBENR$FHIPUBENR <- factor(PUBENR$HIPUBENR)
#
ggplot(PUBENR,aes(PUBENR,PISASCORE)) + 
  geom_point()+
#  geom_smooth(aes(PUBENR,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by public school enrolment") +
  xlab("Public school enrolment") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmPUBENR <- lm(PISASCORE ~ PUBENR, data=PUBENR)
summary(lmPUBENR)
#
summ(lmPUBENR)
#
#------------------------------------------------------------------------------
# Analysis of PUBENR with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
PUBENR$FHIINCEQ <- factor(PUBENR$HIINCEQ)
ddply(PUBENR,c("HIPUBENR","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PUBENR,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIPUBENR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPUBENR \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPublic \nEnrolment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PUBENR,aes(PUBENR,PISASCORE)) + 
  geom_point(aes(PUBENR,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(PUBENR,HIINCEQ==0),
              aes(PUBENR,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PUBENR,HIINCEQ==1),
              aes(PUBENR,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Public school enrolment") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nIncome \nEquality") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPUBENRHIINCEQ <- lm(PISASCORE ~ PUBENR + HIINCEQ, data=PUBENR)
summary(lmPUBENRHIINCEQ)
#
# PUBENR IS NOT SIGNIFICANT (p=0.549)
#
summ(lmPUBENRHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPUBENxRHIINCEQ <- lm(PISASCORE ~ PUBENR + HIINCEQ + PUBENR:HIINCEQ, data=PUBENR)
summary(lmPUBENRxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.080)
#
summ(lmPUBENxRHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
PUBENR$FHIGENEQ <- factor(PUBENR$HIGENEQ)
ddply(PUBENR,c("HIPUBENR","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PUBENR,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIPUBENR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPUBENR \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPublic \nEnrolment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PUBENR,aes(PUBENR,PISASCORE)) + 
  geom_point(aes(PUBENR,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(PUBENR,HIGENEQ==0),
              aes(PUBENR,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PUBENR,HIGENEQ==1),
              aes(PUBENR,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("PUBENR") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPUBENRHIGENEQ <- lm(PISASCORE ~ PUBENR + HIGENEQ, data=PUBENR)
summary(lmPUBENRHIGENEQ)
#
# PUBENR IS NOT SIGNIFICANT (p=0.245)
#
summ(lmPUBENRHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPUBENRxHIGENEQ <- lm(PISASCORE ~ PUBENR + HIGENEQ + PUBENR:HIGENEQ, data=PUBENR)
summary(lmPUBENRxHIGENEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.015), model significant (p<0.001)
#
plot_model(lmPUBENRxHIGENEQ, type = "pred", terms = c("PUBENR", "HIGENEQ")) + scale_colour_grey() +
  ylim(333,610)
#
summ(lmPUBENRxHIGENEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPUBENRxHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIHDI
#
PUBENR$FHIHDI <- factor(PUBENR$HIHDI)
ddply(PUBENR,c("HIPUBENR","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PUBENR,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIPUBENR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPUBENR \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPublic \nEnrolment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PUBENR,aes(PUBENR,PISASCORE)) + 
  geom_point(aes(PUBENR,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(PUBENR,HIHDI==0),
              aes(PUBENR,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PUBENR,HIHDI==1),
              aes(PUBENR,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship(grouped by HIHDI)") +
  xlab("PUBENR") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPUBENRHIHDI <- lm(PISASCORE ~ PUBENR + HIHDI, data=PUBENR)
summary(lmPUBENRHIHDI)
#
# PUBENR IS NOT SIGNIFICANT (p=0.082)
#
summ(lmPUBENRHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPUBENRxHIHDI <- lm(PISASCORE ~ PUBENR + HIHDI + PUBENR:HIHDI, data=PUBENR)
summary(lmPUBENRxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.026), model significant (p<0.001)
#
plot_model(lmPUBENRxHIHDI, type = "pred", terms = c("PUBENR", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmPUBENRxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPUBENRxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
PUBENR$FHIINDIV <- factor(PUBENR$HIINDIV)
ddply(PUBENR,c("HIPUBENR","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PUBENR,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIPUBENR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPUBENR \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPublic \nEnrolment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PUBENR,aes(PUBENR,PISASCORE)) + 
  geom_point(aes(PUBENR,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(PUBENR,HIINDIV==0),
              aes(PUBENR,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PUBENR,HIINDIV==1),
              aes(PUBENR,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Public school enrolment") +
  ylab("PISA score") + ylim(335, 610) +
  labs(color = "High \nIndividualism") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPUBENRHIINDIV <- lm(PISASCORE ~ PUBENR + HIINDIV, data=PUBENR)
summary(lmPUBENRHIINDIV)
#
# PUBENR IS NOT SIGNIFICANT (p=0.498)
#
summ(lmPUBENRHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPUBENRxHIINDIV <- lm(PISASCORE ~ PUBENR + HIINDIV + PUBENR:HIINDIV, data=PUBENR)
summary(lmPUBENRxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.238)
#
summ(lmPUBENRxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
PUBENR <- PUBENR[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
PUBENR <- PUBENR[,-c(2)]
#
# Reposition PISASCORE to end of data grid
PUBENR$PUBENR <- PUBENR$PISASCORE
PUBENR <- PUBENR[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of PUBENR for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PUBENR$HIPUBENR, PUBENR$HIPISA, necessity = FALSE)
QCAfit(1-PUBENR$HIPUBENR, PUBENR$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of PUBENR for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PUBENR$HIPUBENR, PUBENR$LOPISA, necessity = FALSE)
QCAfit(1-PUBENR$HIPUBENR, PUBENR$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTPUBENR1<-truthTable(PUBENR, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,  
                     HIINDIV, HIINCEQ, HIPUBENR", 
                     incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTPUBENR1
#
sol_PUBENR1_c <- minimize(TTPUBENR1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_PUBENR1_c
#
# With individual OECs
TTPUBENRHDI<-truthTable(PUBENR, outcome = "HIPISA", conditions = "HIHDI, HIPUBENR", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPUBENRHDI
#
TTPUBENRGEN<-truthTable(PUBENR, outcome = "HIPISA", conditions = "HIGENEQ, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRGEN
#
TTPUBENRINC<-truthTable(PUBENR, outcome = "HIPISA", conditions = "HIINCEQ, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRINC
#
TTPUBENRINDIV<-truthTable(PUBENR, outcome = "HIPISA", conditions = "HIINDIV, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTPUBENRLO1<-truthTable(PUBENR, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI,  
                      HIINDIV, HIINCEQ, HIPUBENR", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPUBENRLO1
#
sol_PUBENRLO1_c <- minimize(TTPUBENRLO1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_PUBENRLO1_c
#
# With individual OECs
TTPUBENRHDILO<-truthTable(PUBENR, outcome = "LOPISA", conditions = "HIHDI, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRHDILO
#
TTPUBENRGENLO<-truthTable(PUBENR, outcome = "LOPISA", conditions = "HIGENEQ, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRGENLO
#
TTPUBENRINCLO<-truthTable(PUBENR, outcome = "LOPISA", conditions = "HIINCEQ, HIPUBENR", 
                        incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPUBENRINCLO
#
TTPUBENRINDIVLO<-truthTable(PUBENR, outcome = "LOPISA", conditions = "HIINDIV, HIPUBENR", 
                          incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTPUBENRINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PRIEXP
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
PRIEXP$HIPRIEXP <- ifelse(PRIEXP$PRIEXP>=9,1,0)
#
rownames(PRIEXP)[PRIEXP$HIPRIEXP==1]
rownames(PRIEXP)[PRIEXP$HIPRIEXP==0]
#
rownames(PRIEXP)[PRIEXP$HIPISA==1]
rownames(PRIEXP)[PRIEXP$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of PRIEXP to PISASCORE
#------------------------------------------------------------------------------
#
CORR_PRIEXP <- cor.test(PRIEXP$PRIEXP, PRIEXP$PISASCORE, method = "pearson")
CORR_PRIEXP
#
#------------------------------------------------------------------------------
# Analysis of PRIEXP by outcome exabling conditions
#------------------------------------------------------------------------------
#
PRIEXP$FHIPRIEXP <- factor(PRIEXP$HIPRIEXP)
#
ggplot(PRIEXP,aes(PRIEXP,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(PRIEXP,PISASCORE),method=lm,se=FALSE, colour = "grey", linetype="solid", size = 1) +
  ggtitle("PISA scores by private expenditure on education") +
  xlab("Private expenditure on education") +
  ylab("PISA score") + ylim(335,610) +
  theme_sjplot()+ scale_colour_grey()
#
# REGRESSION MODEL
lmPRIEXP <- lm(PISASCORE ~ PRIEXP, data=PRIEXP)
summary(lmPRIEXP)
#
# PRIEXP IS SIGNIFICANT (p=0.00576) MODEL SIGNIFICANT (p=0.00576)
#
plot_model(lmPRIEXP, type = "pred", terms = c("PRIEXP"))
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPRIEXP)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
summ(lmPRIEXP)
effect_plot(lmPRIEXP, pred = PRIEXP, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of PRIEXP with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
PRIEXP$FHIINCEQ <- factor(PRIEXP$HIINCEQ)
#
ggplot(PRIEXP,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIPRIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPRIEXP \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPrivate \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PRIEXP,aes(PRIEXP,PISASCORE)) + 
  geom_point(aes(PRIEXP,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(PRIEXP,HIINCEQ==0),
              aes(PRIEXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PRIEXP,HIINCEQ==1),
              aes(PRIEXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("PRIEXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPRIEXPHIINCEQ <- lm(PISASCORE ~ PRIEXP + HIINCEQ, data=PRIEXP)
summary(lmPRIEXPHIINCEQ)
#
# ALL TERMS SIGNIFICANT, MODEL ALSO SIGNIFICANT (p<0.001)
#
plot_model(lmPRIEXPHIINCEQ, type = "pred", terms = c("PRIEXP", "HIINCEQ"))
#
summ(lmPRIEXPHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPRIEXPxHIINCEQ <- lm(PISASCORE ~ PRIEXP + HIINCEQ + PRIEXP:HIINCEQ, data=PRIEXP)
summary(lmPRIEXPxHIINCEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.047), MODEL ALSO SIGNIFICANT (p<0.001)
#
plot_model(lmPRIEXPxHIINCEQ, type = "pred", terms = c("PRIEXP", "HIINCEQ")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmPRIEXPxHIINCEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPRIEXPxHIINCEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
PRIEXP$FHIGENEQ <- factor(PRIEXP$HIGENEQ)
#
ggplot(PRIEXP,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIPRIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPRIEXP \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPrivate \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PRIEXP,aes(PRIEXP,PISASCORE)) + 
  geom_point(aes(PRIEXP,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(PRIEXP,HIGENEQ==0),
              aes(PRIEXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PRIEXP,HIGENEQ==1),
              aes(PRIEXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("PRIEXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPRIEXPHIGENEQ <- lm(PISASCORE ~ PRIEXP + HIGENEQ, data=PRIEXP)
summary(lmPRIEXPHIGENEQ)
#
# PRIEXP IS NOT SIGNIFICANT (p=0.058)
#
summ(lmPRIEXPHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPRIEXPxHIGENEQ <- lm(PISASCORE ~ PRIEXP + HIGENEQ + PRIEXP:HIGENEQ, data=PRIEXP)
summary(lmPRIEXPxHIGENEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p<0.001), model significant (p<0.001)
#
plot_model(lmPRIEXPxHIGENEQ, type = "pred", terms = c("PRIEXP", "HIGENEQ")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmPRIEXPxHIGENEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPRIEXPxHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIHDI
#
PRIEXP$FHIHDI <- factor(PRIEXP$HIHDI)
#
ggplot(PRIEXP,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIPRIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPRIEXP \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPrivate \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PRIEXP,aes(PRIEXP,PISASCORE)) + 
  geom_point(aes(PRIEXP,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(PRIEXP,HIHDI==0),
              aes(PRIEXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PRIEXP,HIHDI==1),
              aes(PRIEXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("PRIEXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPRIEXPHIHDI <- lm(PISASCORE ~ PRIEXP + HIHDI, data=PRIEXP)
summary(lmPRIEXPHIHDI)
#
# ALL TERMS ARE SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
plot_model(lmPRIEXPHIHDI, type = "pred", terms = c("PRIEXP", "HIHDI"))
#
summ(lmPRIEXPHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPRIEXPxHIHDI <- lm(PISASCORE ~ PRIEXP + HIHDI + PRIEXP:HIHDI, data=PRIEXP)
summary(lmPRIEXPxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.0002), model significant (p<0.001)
#
plot_model(lmPRIEXPxHIHDI, type = "pred", terms = c("PRIEXP", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmPRIEXPxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPRIEXPxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
PRIEXP$FHIINDIV <- factor(PRIEXP$HIINDIV)
#
ggplot(PRIEXP,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIPRIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPRIEXP \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPrivate \nExpenditure") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PRIEXP,aes(PRIEXP,PISASCORE)) + 
  geom_point(aes(PRIEXP,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(PRIEXP,HIINDIV==0),
              aes(PRIEXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(PRIEXP,HIINDIV==1),
              aes(PRIEXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("PRIEXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPRIEXPHIINDIV <- lm(PISASCORE ~ PRIEXP + HIINDIV, data=PRIEXP)
summary(lmPRIEXPHIINDIV)
#
# NEITHER TERM IS SIGNIFICANT
#
summ(lmPRIEXPHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPRIEXPxHIINDIV <- lm(PISASCORE ~ PRIEXP + HIINDIV + PRIEXP:HIINDIV, data=PRIEXP)
summary(lmPRIEXPxHIINDIV)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.002), model significant (p=0.0003)
#
plot_model(lmPRIEXPxHIINDIV, type = "pred", terms = c("PRIEXP", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmPRIEXPxHIINDIV)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmPRIEXPxHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REMOVE FACTORS FROM DATA FRAME
PRIEXP <- PRIEXP[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
PRIEXP <- PRIEXP[,-c(2)]
#
# Reposition PISASCORE to end of data grid
PRIEXP$PRIEXP <- PRIEXP$PISASCORE
PRIEXP <- PRIEXP[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of PRIEXP for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PRIEXP$HIPRIEXP, PRIEXP$HIPISA, necessity = FALSE)
QCAfit(1-PRIEXP$HIPRIEXP, PRIEXP$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of PRIEXP for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PRIEXP$HIPRIEXP, PRIEXP$LOPISA, necessity = FALSE)
QCAfit(1-PRIEXP$HIPRIEXP, PRIEXP$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTPRIEXP1<-truthTable(PRIEXP, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,
                      HIINDIV, HIINCEQ, HIPRIEXP", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPRIEXP1
#
sol_PRIEXP1_c <- minimize(TTPRIEXP1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_PRIEXP1_c
#
# With individual OECs
TTPRIEXPHDI<-truthTable(PRIEXP, outcome = "HIPISA", conditions = "HIHDI, HIPRIEXP", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPRIEXPHDI
#
TTPRIEXPGEN<-truthTable(PRIEXP, outcome = "HIPISA", conditions = "HIGENEQ, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPGEN
#
TTPRIEXPINC<-truthTable(PRIEXP, outcome = "HIPISA", conditions = "HIINCEQ, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPINC
#
TTPRIEXPINDIV<-truthTable(PRIEXP, outcome = "HIPISA", conditions = "HIINDIV, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with individual outcome enabling conditions (5 cases)
#------------------------------------------------------------------------------
#
TTPRIEXPLO1<-truthTable(PRIEXP, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI,
                      HIINDIV, HIINCEQ, HIPRIEXP", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPRIEXPLO1
#
# With individual OECs
TTPRIEXPHDILO<-truthTable(PRIEXP, outcome = "LOPISA", conditions = "HIHDI, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPHDILO
#
TTPRIEXPGENLO<-truthTable(PRIEXP, outcome = "LOPISA", conditions = "HIGENEQ, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPGENLO
#
TTPRIEXPINCLO<-truthTable(PRIEXP, outcome = "LOPISA", conditions = "HIINCEQ, HIPRIEXP", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPRIEXPINCLO
#
TTPRIEXPINDIVLO<-truthTable(PRIEXP, outcome = "LOPISA", conditions = "HIINDIV, HIPRIEXP", 
                          incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTPRIEXPINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# COMBINED POLICY excluding PUBENR & HOURS (no evidence of relationship found)
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
# Import csv file COMBINED
POLCOMBINED <- read.csv("POLCOMBINED.csv", row.names = 1)
head(POLCOMBINED)
#
POLCOMBINED$HICUMEXP <- ifelse(POLCOMBINED$CUMEXP>=85000,1,0)
rownames(POLCOMBINED)[POLCOMBINED$HICUMEXP==1]
rownames(POLCOMBINED)[POLCOMBINED$HICUMEXP==0]
#
POLCOMBINED$HIPRIEXP <- ifelse(POLCOMBINED$PRIEXP>=9,1,0)
rownames(POLCOMBINED)[POLCOMBINED$HIPRIEXP==1]
rownames(POLCOMBINED)[POLCOMBINED$HIPRIEXP==0]
#
POLCOMBINED$BIGCLASS <- ifelse(POLCOMBINED$CLASS>=25,1,0)
rownames(POLCOMBINED)[POLCOMBINED$BIGCLASS==1]
rownames(POLCOMBINED)[POLCOMBINED$BIGCLASS==0]
#
rownames(POLCOMBINED)[POLCOMBINED$HIPISA==1]
rownames(POLCOMBINED)[POLCOMBINED$LOPISA==1] # only 2 cases
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
POLCOMBINED <- POLCOMBINED[,-c(2:6)]
POLCOMBINED <- POLCOMBINED[,-c(5:7)]
POLCOMBINED <- POLCOMBINED[,-c(6)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
# With only outcome enabling conditions
#
TTCOMBIoec<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HIGENEQ, 
                       HIHDI, HIINCEQ, HIINDIV", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=T)
TTCOMBIoec
#
sol_COMBIoec_c <- minimize(TTCOMBIoec, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBIoec_c
#
sol_COMBIoec_p <- minimize(TTCOMBIoec, details=TRUE, show.cases=TRUE, include = '?', row.dom = TRUE)
sol_COMBIoec_p
#
sol_COMBIoec_i <- minimize(TTCOMBIoec, details=TRUE, show.cases=TRUE, include = '?', row.dom = TRUE,
                           dir.exp = "1,-,-,-")
sol_COMBIoec_i
#
# Without outcome enabling conditions
#
TTCOMBIx<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HICUMEXP, HIPRIEXP, BIGCLASS", 
                          incl.cut = .80, n.cut = 1, 
                          complete=TRUE, show.cases = T)
TTCOMBIx
#
sol_COMBIx_c <- minimize(TTCOMBIx, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBIx_c
#
# With outcome enabling conditions 1x1
#
TTCOMBI1<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HIGENEQ,
                       HICUMEXP, HIPRIEXP, BIGCLASS", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI1
#
sol_COMBI1_c <- minimize(TTCOMBI1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI1_c
#
TTCOMBI2<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HIHDI,
                       HICUMEXP, HIPRIEXP, BIGCLASS", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI2
#
sol_COMBI2_c <- minimize(TTCOMBI2, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI2_c
#
TTCOMBI3<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HIINCEQ,
                     HICUMEXP, HIPRIEXP, BIGCLASS", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI3
#
sol_COMBI3_c <- minimize(TTCOMBI3, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI3_c
#
TTCOMBI4<-truthTable(POLCOMBINED, outcome = "HIPISA", conditions = "HIINDIV,
                     HICUMEXP, HIPRIEXP, BIGCLASS", 
                     incl.cut = .8, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI4
#
sol_COMBI4_c <- minimize(TTCOMBI4, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI4_c
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

