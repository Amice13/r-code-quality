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
# Crisp Set Analysis with PROFESS data set
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
# Import csv file SALARY
SALARY <- read.csv("SALARY.csv", row.names = 1)
head(SALARY)
#
# Import csv file QUALS
QUALS <- read.csv("QUALS.csv", row.names = 1)
head(QUALS)
#
# Import csv file PDEV
PDEV <- read.csv("PDEV.csv", row.names = 1)
head(PDEV)
#
# Import csv file EXP
EXP <- read.csv("EXP.csv", row.names = 1)
head(EXP)
#
# Import csv file INDUCT
INDUCT <- read.csv("INDUCT.csv", row.names = 1)
head(INDUCT)
#
# Import csv file MENTOR
MENTOR <- read.csv("MENTOR.csv", row.names = 1)
head(MENTOR)
#
# Import csv file NONCON
NONCON <- read.csv("NONCON.csv", row.names = 1)
head(NONCON)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# TEACHERS SALARY
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
SALARY$HISALARY <- ifelse(SALARY$SALARY>=30000,1,0)
#
rownames(SALARY)[SALARY$HISALARY==1]
rownames(SALARY)[SALARY$HISALARY==0]
#
rownames(SALARY)[SALARY$HIPISA==1]
rownames(SALARY)[SALARY$LOPISA==0]
#
#------------------------------------------------------------------------------
# Check correlation of SALARY to PISASCORE
#------------------------------------------------------------------------------
#
CORR_SALARY <- cor.test(SALARY$SALARY, SALARY$PISASCORE, method = "pearson")
CORR_SALARY
#
#------------------------------------------------------------------------------
# Analysis of SALARY by outcome exabling conditions
#------------------------------------------------------------------------------
#
SALARY$FHISALARY <- factor(SALARY$HISALARY)
#
ggplot(SALARY,aes(SALARY,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(SALARY,PISASCORE),method=lm,se=FALSE, colour = "grey", linetype="solid", size = 1) +
  ggtitle("PISA scores by teachers salaries") +
  xlab("Teachers salaries") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmSALARY <- lm(PISASCORE ~ SALARY, data=SALARY)
summary(lmSALARY)
summ(lmSALARY)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmSALARY)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# SALARY IS SIGNIFICANT (p=0.0003) 
#
plot_model(lmSALARY, type = "pred", terms = c("SALARY"))
#
effect_plot(lmSALARY, pred = SALARY, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of SALARY with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
SALARY$FHIINCEQ <- factor(SALARY$HIINCEQ)
ddply(SALARY,c("HISALARY","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(SALARY,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHISALARY))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HISALARY \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") + 
  labs(fill = "High \nSalaries") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(SALARY,aes(SALARY,PISASCORE)) + 
              geom_point(aes(SALARY,PISASCORE,colour=factor(HIINCEQ)))+
              geom_smooth(data=subset(SALARY,HIINCEQ==0),
              aes(SALARY,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
              geom_smooth(data=subset(SALARY,HIINCEQ==1),
              aes(SALARY,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
              ggtitle("Observed relationship (grouped by HIINCEQ)") +
              xlab("SALARY") +
              ylab("PISASCORE") + ylim(335,610) +
              labs(color = "HIINCEQ") +
              guides(size = "none") +
              scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmSALARYHIINCEQ <- lm(PISASCORE ~ SALARY + HIINCEQ, data=SALARY)
summary(lmSALARYHIINCEQ)
#
# ALL TERMS ARE SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmSALARYHIINCEQ)
plot_model(lmSALARYHIINCEQ, type = "pred", terms = c("SALARY", "HIINCEQ")) +
  scale_colour_grey() + ylim(335,600)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmSALARYHIINCEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmSALARYxHIINCEQ <- lm(PISASCORE ~ SALARY + HIINCEQ + SALARY:HIINCEQ, data=SALARY)
summary(lmSALARYxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.278)
#
summ(lmSALARYxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
SALARY$FHIGENEQ <- factor(SALARY$HIGENEQ)
ddply(SALARY,c("HISALARY","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(SALARY,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHISALARY))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HISALARY \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nSalaries") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(SALARY,aes(SALARY,PISASCORE)) + 
  geom_point(aes(SALARY,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(SALARY,HIGENEQ==0),
              aes(SALARY,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(SALARY,HIGENEQ==1),
              aes(SALARY,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Teachers salaries") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmSALARYHIGENEQ <- lm(PISASCORE ~ SALARY + HIGENEQ, data=SALARY)
summary(lmSALARYHIGENEQ)
#
# SALARY IS NOT SIGNIFICANT (p=0.118)
#
summ(lmSALARYHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmSALARYxHIGENEQ <- lm(PISASCORE ~ SALARY + HIGENEQ + SALARY:HIGENEQ, data=SALARY)
summary(lmSALARYxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.551), model significant (p<0.001)
#
summ(lmSALARYxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
SALARY$FHIHDI <- factor(SALARY$HIHDI)
ddply(SALARY,c("HISALARY","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(SALARY,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHISALARY))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HISALARY \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nSalaries") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(SALARY,aes(SALARY,PISASCORE)) + 
  geom_point(aes(SALARY,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(SALARY,HIHDI==0),
              aes(SALARY,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(SALARY,HIHDI==1),
              aes(SALARY,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("Teachers salaries") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "High \nHuman \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmSALARYHIHDI <- lm(PISASCORE ~ SALARY + HIHDI, data=SALARY)
summary(lmSALARYHIHDI)
#
# SALARY IS NOT SIGNIFICANT (p=0.552)
#
summ(lmSALARYHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmSALARYxHIHDI <- lm(PISASCORE ~ SALARY + HIHDI + SALARY:HIHDI, data=SALARY)
summary(lmSALARYxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.142), model significant (p<0.001)
#
summ(lmSALARYxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
SALARY$FHIINDIV <- factor(SALARY$HIINDIV)
ddply(SALARY,c("HISALARY","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(SALARY,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHISALARY))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HISALARY \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nSalaries") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(SALARY,aes(SALARY,PISASCORE)) + 
  geom_point(aes(SALARY,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(SALARY,HIINDIV==0),
              aes(SALARY,PISASCORE,color=factor(HIINDIV)),method=lm,se=F, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(SALARY,HIINDIV==1),
              aes(SALARY,PISASCORE,color=factor(HIINDIV)),method=lm,se=F, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("SALARY") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  theme_sjplot()+ scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmSALARYHIINDIV <- lm(PISASCORE ~ SALARY + HIINDIV, data=SALARY)
summary(lmSALARYHIINDIV)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p=0.0002)
#
plot_model(lmSALARYHIINDIV, type = "pred", terms = c("SALARY", "HIINDIV"))
#
summ(lmSALARYHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmSALARYxHIINDIV <- lm(PISASCORE ~ SALARY + HIINDIV + SALARY:HIINDIV, data=SALARY)
summary(lmSALARYxHIINDIV)
#
# THE INTERACTION TERM IS SIGNIFICANT (p<0.001) MODEL IS SIGNIFICANT (p<0.001)
#
plot_model(lmSALARYxHIINDIV, type = "pred", terms = c("SALARY", "HIINDIV")) +
  scale_colour_grey() + ylim(335,610)
#
summ(lmSALARYxHIINDIV)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmSALARYxHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REMOVE FACTORS FROM DATA FRAME
SALARY <- SALARY[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
SALARY <- SALARY[,-c(2)]
#
# Reposition PISASCORE to end of data grid
SALARY$PISA <- SALARY$PISASCORE
SALARY <- SALARY[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of SALARY for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(SALARY$HISALARY, SALARY$HIPISA, necessity = FALSE)
QCAfit(1-SALARY$HISALARY, SALARY$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of SALARY for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(SALARY$HISALARY, SALARY$LOPISA, necessity = FALSE)
QCAfit(1-SALARY$HISALARY, SALARY$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTSALARY1<-truthTable(SALARY, outcome = "HIPISA", conditions = "HIGENEQ, HIINCEQ,
                     HIHDI, HIINDIV, HISALARY", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTSALARY1
#
sol_SALARY1_c <- minimize(TTSALARY1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_SALARY1_c
#
# By individual OECs
TTSALARYHDI<-truthTable(SALARY, outcome = "HIPISA", conditions = "HIHDI, HISALARY", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTSALARYHDI
#
TTSALARYGEN<-truthTable(SALARY, outcome = "HIPISA", conditions = "HIGENEQ, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYGEN
#
TTSALARYINC<-truthTable(SALARY, outcome = "HIPISA", conditions = "HIINCEQ, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYINC
#
TTSALARYINDIV<-truthTable(SALARY, outcome = "HIPISA", conditions = "HIINDIV, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTSALARY1LO<-truthTable(SALARY, outcome = "LOPISA", conditions = "HIGENEQ, HIINCEQ,
                      HIHDI, HIINDIV, HISALARY", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTSALARY1LO
#
sol_SALARY1_c <- minimize(TTSALARY1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_SALARY1_c
#
# By individual OECs
TTSALARYHDILO<-truthTable(SALARY, outcome = "LOPISA", conditions = "HIHDI, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYHDILO
#
TTSALARYGENLO<-truthTable(SALARY, outcome = "LOPISA", conditions = "HIGENEQ, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYGENLO
#
TTSALARYINCLO<-truthTable(SALARY, outcome = "LOPISA", conditions = "HIINCEQ, HISALARY", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTSALARYINCLO
#
TTSALARYINDIVLO<-truthTable(SALARY, outcome = "LOPISA", conditions = "HIINDIV, HISALARY", 
                          incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTSALARYINDIVLO
#
#
# Check SALARY as subset of HDI
QCAfit(SALARY$HIHDI, SALARY$HISALARY, necessity = TRUE)
#
rownames(SALARY)[SALARY$HISALARY==1]
rownames(SALARY)[SALARY$HISALARY==1 & SALARY$HIHDI==0]
#
# HISALARY is a subset of HIHDI (exceptions Italy and Spain)
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# QUALS
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
QUALS$HIQUALS <- ifelse(QUALS$QUALS>=92,1,0)
#
rownames(QUALS)[QUALS$HIQUALS==1]
rownames(QUALS)[QUALS$HIQUALS==0]
#
rownames(QUALS)[QUALS$HIPISA==1]
rownames(QUALS)[QUALS$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of QUALS to PISASCORE
#------------------------------------------------------------------------------
#
CORR_QUALS <- cor.test(QUALS$QUALS, QUALS$PISASCORE, method = "pearson")
CORR_QUALS
#
#------------------------------------------------------------------------------
# Analysis of QUALS by outcome exabling conditions
#------------------------------------------------------------------------------
#
QUALS$FHIQUALS <- factor(QUALS$HIQUALS)
#
ggplot(QUALS,aes(QUALS,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(QUALS,PISASCORE),method=lm,se=FALSE, colour = "grey", linetype="solid", size = 1) +
  ggtitle("PISA scores by percentage of qualified teachers") +
  xlab("Qualified teachers") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmQUALS <- lm(PISASCORE ~ QUALS, data=QUALS)
summary(lmQUALS)
summ(lmQUALS)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmQUALS)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# QUALS IS SIGNIFICANT (p=0.0003) 
#
plot_model(lmQUALS, type = "pred", terms = c("QUALS"))
#
effect_plot(lmQUALS, pred = QUALS, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of QUALS with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
QUALS$FHIINCEQ <- factor(QUALS$HIINCEQ)
ddply(QUALS,c("HIQUALS", "HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(QUALS,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIQUALS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIQUALS \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nQualified \nTeachers") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(QUALS,aes(QUALS,PISASCORE)) + 
  geom_point(aes(QUALS,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(QUALS,HIINCEQ==0),
              aes(QUALS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(QUALS,HIINCEQ==1),
              aes(QUALS,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("QUALS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmQUALSHIINCEQ <- lm(PISASCORE ~ QUALS + HIINCEQ, data=QUALS)
summary(lmQUALSHIINCEQ)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.0001)
#
summ(lmQUALSHIINCEQ)
plot_model(lmQUALSHIINCEQ, type = "pred", terms = c("QUALS", "HIINCEQ")) + scale_colour_grey() +
  ylim(335,610)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmQUALSHIINCEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmQUALSxHIINCEQ <- lm(PISASCORE ~ QUALS + HIINCEQ + QUALS:HIINCEQ, data=QUALS)
summary(lmQUALSxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.404)
#
summ(lmQUALSxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
QUALS$FHIGENEQ <- factor(QUALS$HIGENEQ)
ddply(QUALS,c("HIQUALS", "HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(QUALS,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIQUALS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIQUALS \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nQualified \nTeachers") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(QUALS,aes(QUALS,PISASCORE)) + 
  geom_point(aes(QUALS,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(QUALS,HIGENEQ==0),
              aes(QUALS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(QUALS,HIGENEQ==1),
              aes(QUALS,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("QUALS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmQUALSHIGENEQ <- lm(PISASCORE ~ QUALS + HIGENEQ, data=QUALS)
summary(lmQUALSHIGENEQ)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmQUALSHIGENEQ)
plot_model(lmQUALSHIGENEQ, type = "pred", terms = c("QUALS", "HIGENEQ")) + scale_colour_grey() +
  ylim(335, 610)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmQUALSHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmQUALSxHIGENEQ <- lm(PISASCORE ~ QUALS + HIGENEQ + QUALS:HIGENEQ, data=QUALS)
summary(lmQUALSxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.868), model significant (p<0.001)
#
summ(lmQUALSxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
QUALS$FHIHDI <- factor(QUALS$HIHDI)
ddply(QUALS,c("HIQUALS", "HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(QUALS,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIQUALS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIQUALS \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nQualified \nTeachers") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(QUALS,aes(QUALS,PISASCORE)) + 
  geom_point(aes(QUALS,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(QUALS,HIHDI==0),
              aes(QUALS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(QUALS,HIHDI==1),
              aes(QUALS,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("QUALS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmQUALSHIHDI <- lm(PISASCORE ~ QUALS + HIHDI, data=QUALS)
summary(lmQUALSHIHDI)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmQUALSHIHDI)
plot_model(lmQUALSHIHDI, type = "pred", terms = c("QUALS", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmQUALSHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmQUALSxHIHDI <- lm(PISASCORE ~ QUALS + HIHDI + QUALS:HIHDI, data=QUALS)
summary(lmQUALSxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.626), model significant (p<0.001)
#
summ(lmQUALSxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
QUALS$FHIINDIV <- factor(QUALS$HIINDIV)
ddply(QUALS,c("HIQUALS", "HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(QUALS,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIQUALS))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIQUALS \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nQualified \nTeachers") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(QUALS,aes(QUALS,PISASCORE)) + 
  geom_point(aes(QUALS,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(QUALS,HIINDIV==0),
              aes(QUALS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(QUALS,HIINDIV==1),
              aes(QUALS,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("QUALS") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmQUALSHIINDIV <- lm(PISASCORE ~ QUALS + HIINDIV, data=QUALS)
summary(lmQUALSHIINDIV)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmQUALSHIINDIV)
plot_model(lmQUALSHIINDIV, type = "pred", terms = c("QUALS", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610) 
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmQUALSHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmQUALSxHIINDIV <- lm(PISASCORE ~ QUALS + HIINDIV + QUALS:HIINDIV, data=QUALS)
summary(lmQUALSxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.155)
#
summ(lmQUALSxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
QUALS <- QUALS[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
QUALS <- QUALS[,-c(2)]
#
# Reposition PISASCORE to end of data grid
QUALS$PISA <- QUALS$PISASCORE
QUALS <- QUALS[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of QUALS for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(QUALS$HIQUALS, QUALS$HIPISA, necessity = FALSE)
QCAfit(1-QUALS$HIQUALS, QUALS$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of QUALS for LOIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(QUALS$HIQUALS, QUALS$LOPISA, necessity = FALSE)
QCAfit(1-QUALS$HIQUALS, QUALS$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with combinations of outcome enabling conditions
#------------------------------------------------------------------------------
#
TTQUALS1<-truthTable(QUALS, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI, 
                     HIINDIV, HIINCEQ, HIQUALS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTQUALS1
#
sol_QUALS1_c <- minimize(TTQUALS1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_QUALS1_c
#
# By individual OECs
TTQUALSHDI<-truthTable(QUALS, outcome = "HIPISA", conditions = "HIHDI, HIQUALS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTQUALSHDI
#
TTQUALSGEN<-truthTable(QUALS, outcome = "HIPISA", conditions = "HIGENEQ, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSGEN
#
TTQUALSINC<-truthTable(QUALS, outcome = "HIPISA", conditions = "HIINCEQ, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSINC
#
TTQUALSINDIV<-truthTable(QUALS, outcome = "HIPISA", conditions = "HIINDIV, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with combinations of outcome enabling conditions
#------------------------------------------------------------------------------
#
TTQUALS1LO<-truthTable(QUALS, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI, 
                     HIINDIV, HIINCEQ, HIQUALS", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTQUALS1LO
#
sol_QUALS1LO_c <- minimize(TTQUALS1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_QUALS1LO_c
#
# By individual OECs
TTQUALSHDILO<-truthTable(QUALS, outcome = "LOPISA", conditions = "HIHDI, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSHDILO
#
TTQUALSGENLO<-truthTable(QUALS, outcome = "LOPISA", conditions = "HIGENEQ, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSGENLO
#
TTQUALSINCLO<-truthTable(QUALS, outcome = "LOPISA", conditions = "HIINCEQ, HIQUALS", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTQUALSINCLO
#
TTQUALSINDIVLO<-truthTable(QUALS, outcome = "LOPISA", conditions = "HIINDIV, HIQUALS", 
                         incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                         complete=TRUE)
TTQUALSINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# PDEV
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
PDEV$HIPDEV <- ifelse(PDEV$PDEV>=50,1,0)
#
rownames(PDEV)[PDEV$HIPDEV==1]
rownames(PDEV)[PDEV$HIPDEV==0]
#
rownames(PDEV)[PDEV$HIPISA==1]
rownames(PDEV)[PDEV$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of PDEV to PISASCORE
#------------------------------------------------------------------------------
#
CORR_PDEV <- cor.test(PDEV$PDEV, PDEV$PISASCORE, method = "pearson")
CORR_PDEV
#
#------------------------------------------------------------------------------
# ggPLOT graphs of PDEV by outcome exabling conditions
#------------------------------------------------------------------------------
#
PDEV$FHIPDEV <- factor(PDEV$HIPDEV)
#
ggplot(PDEV,aes(PDEV,PISASCORE)) + 
  geom_point()+
#  geom_smooth(aes(PDEV,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by teachers reporting professional \ndevelopment") +
  xlab("Professional development") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmPDEV <- lm(PISASCORE ~ PDEV, data=PDEV)
summary(lmPDEV)
summ(lmPDEV)
#
# PDEV IS NOT SIGNIFICANT (p=0.121) 
#
#------------------------------------------------------------------------------
# Analysis of PDEV with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
PDEV$FHIINCEQ <- factor(PDEV$HIINCEQ)
ddply(PDEV,c("HIPDEV","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PDEV,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIPDEV))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPDEV \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nProfessional \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PDEV,aes(PDEV,PISASCORE)) + 
  geom_point(aes(PDEV,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(PDEV,HIINCEQ==0),
              aes(PDEV,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PDEV,HIINCEQ==1),
              aes(PDEV,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Professional development") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPDEVHIINCEQ <- lm(PISASCORE ~ PDEV + HIINCEQ, data=PDEV)
summary(lmPDEVHIINCEQ)
#
# PDEV NOT SIGNIFICANT (p=0.125)
#
summ(lmPDEVHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPDEVxHIINCEQ <- lm(PISASCORE ~ PDEV + HIINCEQ + PDEV:HIINCEQ, data=PDEV)
summary(lmPDEVxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.1366)
#
summ(lmPDEVxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
PDEV$FHIGENEQ <- factor(PDEV$HIGENEQ)
ddply(PDEV,c("HIPDEV","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PDEV,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIPDEV))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPDEV \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nProfessional \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PDEV,aes(PDEV,PISASCORE)) + 
  geom_point(aes(PDEV,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(PDEV,HIGENEQ==0),
              aes(PDEV,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PDEV,HIGENEQ==1),
              aes(PDEV,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Professional development") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPDEVHIGENEQ <- lm(PISASCORE ~ PDEV + HIGENEQ, data=PDEV)
summary(lmPDEVHIGENEQ)
#
# PDEV NOT SIGNIFICANT (p=0.517)
#
summ(lmPDEVHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPDEVxHIGENEQ <- lm(PISASCORE ~ PDEV + HIGENEQ + PDEV:HIGENEQ, data=PDEV)
summary(lmPDEVxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.914), model significant (p<0.001)
#
summ(lmPDEVxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
PDEV$FHIHDI <- factor(PDEV$HIHDI)
ddply(PDEV,c("HIPDEV","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PDEV,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIPDEV))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPDEV \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nProfessional \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PDEV,aes(PDEV,PISASCORE)) + 
  geom_point(aes(PDEV,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(PDEV,HIHDI==0),
              aes(PDEV,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PDEV,HIHDI==1),
              aes(PDEV,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("Professional development") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPDEVHIHDI <- lm(PISASCORE ~ PDEV + HIHDI, data=PDEV)
summary(lmPDEVHIHDI)
#
# PDEV IS NOT SIGNIFICANT (p=0.872)
#
summ(lmPDEVHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPDEVxHIHDI <- lm(PISASCORE ~ PDEV + HIHDI + PDEV:HIHDI, data=PDEV)
summary(lmPDEVxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.907), model significant (p<0.001)
#
summ(lmPDEVxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
PDEV$FHIINDIV <- factor(PDEV$HIINDIV)
ddply(PDEV,c("HIPDEV","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(PDEV,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIPDEV))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIPDEV \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nProfessional \nDevelopment") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(PDEV,aes(PDEV,PISASCORE)) + 
  geom_point(aes(PDEV,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(PDEV,HIINDIV==0),
              aes(PDEV,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(PDEV,HIINDIV==1),
              aes(PDEV,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Professional development") +
  ylab("PISA score") + ylim(335, 610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmPDEVHIINDIV <- lm(PISASCORE ~ PDEV + HIINDIV, data=PDEV)
summary(lmPDEVHIINDIV)
#
# PDEV IS NOT SIGNIFICANT (p=0.289)
#
summ(lmPDEVHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmPDEVxHIINDIV <- lm(PISASCORE ~ PDEV + HIINDIV + PDEV:HIINDIV, data=PDEV)
summary(lmPDExVHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.461)
#
summ(lmPDEVxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
PDEV <- PDEV[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
PDEV <- PDEV[,-c(2)]
#
# Reposition PISASCORE to end of data grid
PDEV$PISA <- PDEV$PISASCORE
PDEV <- PDEV[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of CLASS for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PDEV$HIPDEV, PDEV$HIPISA, necessity = FALSE)
QCAfit(1-PDEV$HIPDEV, PDEV$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of CLASS for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(PDEV$HIPDEV, PDEV$LOPISA, necessity = FALSE)
QCAfit(1-PDEV$HIPDEV, PDEV$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTPDEV1<-truthTable(PDEV, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI, 
                     HIINDIV, HIINCEQ, HIPDEV", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTPDEV1
#
sol_PDEV1_c <- minimize(TTPDEV1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_PDEV1_c
#
# With individual OECs
TTPDEVHDI<-truthTable(PDEV, outcome = "HIPISA", conditions = "HIHDI, HIPDEV", 
                    incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                    complete=TRUE)
TTPDEVHDI
#
TTPDEVGEN<-truthTable(PDEV, outcome = "HIPISA", conditions = "HIGENEQ, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVGEN
#
TTPDEVINC<-truthTable(PDEV, outcome = "HIPISA", conditions = "HIINCEQ, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVINC
#
TTPDEVINDIV<-truthTable(PDEV, outcome = "HIPISA", conditions = "HIINDIV, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
TTPDEV1LO<-truthTable(PDEV, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI, 
                    HIINDIV, HIINCEQ, HIPDEV", 
                    incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                    complete=TRUE)
TTPDEV1LO
#
sol_PDEV1LO_c <- minimize(TTPDEV1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_PDEV1LO_c
#
# With individual OECs
TTPDEVHDILO<-truthTable(PDEV, outcome = "LOPISA", conditions = "HIHDI, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVHDILO
#
TTPDEVGENLO<-truthTable(PDEV, outcome = "LOPISA", conditions = "HIGENEQ, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVGENLO
#
TTPDEVINCLO<-truthTable(PDEV, outcome = "LOPISA", conditions = "HIINCEQ, HIPDEV", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTPDEVINCLO
#
TTPDEVINDIVLO<-truthTable(PDEV, outcome = "LOPISA", conditions = "HIINDIV, HIPDEV", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTPDEVINDIVLO
#
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# EXP
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
EXP$HIEXP <- ifelse(EXP$EXP>=16,1,0)
#
rownames(EXP)[EXP$HIEXP==1]
rownames(EXP)[EXP$HIEXP==0]
#
rownames(EXP)[EXP$HIPISA==1]
rownames(EXP)[EXP$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of EXP to PISASCORE
#------------------------------------------------------------------------------
#
CORR_EXP <- cor.test(EXP$EXP, EXP$PISASCORE, method = "pearson")
CORR_EXP
#
#------------------------------------------------------------------------------
# Analysis of EXP by outcome exabling conditions
#------------------------------------------------------------------------------
#
EXP$FHIEXP <- factor(EXP$HIEXP)
#
ggplot(EXP,aes(EXP,PISASCORE)) + 
  geom_point()+
#  geom_smooth(aes(EXP,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by teacher experience") +
  xlab("Teacher experience") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmEXP <- lm(PISASCORE ~ EXP, data=EXP)
summary(lmEXP)
#
summ(lmEXP)
#
# EXP IS NOT SIGNIFICANT (p=0.561)
#
#------------------------------------------------------------------------------
# Analysis of EXP with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
EXP$FHIINCEQ <- factor(EXP$HIINCEQ)
ddply(EXP,c("HIEXP","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(EXP,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIEXP \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nTeacher \nExperience") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(EXP,aes(EXP,PISASCORE)) + 
  geom_point(aes(EXP,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(EXP,HIINCEQ==0),
              aes(EXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(EXP,HIINCEQ==1),
              aes(EXP,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Teacher experience") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmEXPHIINCEQ <- lm(PISASCORE ~ EXP + HIINCEQ, data=EXP)
summary(lmEXPHIINCEQ)
#
# EXP IS NOT SIGNIFICANT (p=0.797)
#
summ(lmEXPHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmEXPxHIINCEQ <- lm(PISASCORE ~ EXP + HIINCEQ + EXP:HIINCEQ, data=EXP)
summary(lmEXPxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.794)
#
summ(lmEXPxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
EXP$FHIGENEQ <- factor(EXP$HIGENEQ)
ddply(EXP,c("HIEXP","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(EXP,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIEXP \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nTeacher \nExperience") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(EXP,aes(EXP,PISASCORE)) + 
  geom_point(aes(EXP,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(EXP,HIGENEQ==0),
              aes(EXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(EXP,HIGENEQ==1),
              aes(EXP,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Teacher experience") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmEXPHIGENEQ <- lm(PISASCORE ~ EXP + HIGENEQ, data=EXP)
summary(lmEXPHIGENEQ)
#
# EXP IS NOT SIGNIFICANT (p=0.083)
#
summ(lmEXPHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmEXPxHIGENEQ <- lm(PISASCORE ~ EXP + HIGENEQ + EXP:HIGENEQ, data=EXP)
summary(lmEXPxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.083), model significant (p<0.001)
#
summ(lmEXPxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
EXP$FHIHDI <- factor(EXP$HIHDI)
ddply(EXP,c("HIEXP","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(EXP,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIEXP \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nTeacher \nExperience") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(EXP,aes(EXP,PISASCORE)) + 
  geom_point(aes(EXP,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(EXP,HIHDI==0),
              aes(EXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(EXP,HIHDI==1),
              aes(EXP,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("EXP") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmEXPHIHDI <- lm(PISASCORE ~ EXP + HIHDI, data=EXP)
summary(lmEXPHIHDI)
#
# EXP IS NOT SIGNIFICANT (p=0.104)
#
summ(lmEXPHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmEXPxHIHDI <- lm(PISASCORE ~ EXP + HIHDI + EXP:HIHDI, data=EXP)
summary(lmEXPxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.003), model significant (p<0.001)
#
plot_model(lmEXPxHIHDI, type = "pred", terms = c("EXP", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmEXPxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmEXPxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
EXP$FHIINDIV <- factor(EXP$HIINDIV)
ddply(EXP,c("HIEXP","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(EXP,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIEXP))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIEXP \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nTeacher \nExperience") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(EXP,aes(EXP,PISASCORE)) + 
  geom_point(aes(EXP,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(EXP,HIINDIV==0),
              aes(EXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(EXP,HIINDIV==1),
              aes(EXP,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Teacher experience") +
  ylab("PISA score") +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmEXPHIINDIV <- lm(PISASCORE ~ EXP + HIINDIV, data=EXP)
summary(lmEXPHIINDIV)
#
# EXP IS NOT SIGNIFICANT (p=0.491)
#
summ(lmEXPHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmEXPxHIINDIV <- lm(PISASCORE ~ EXP + HIINDIV + EXP:HIINDIV, data=EXP)
summary(lmEXPxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.820)
#
summ(lmEXPxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
EXP <- EXP[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
EXP <- EXP[,-c(2)]
#
# Reposition PISASCORE to end of data grid
EXP$EXP <- EXP$PISASCORE
EXP <- EXP[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of HIEXP for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(EXP$HIEXP, EXP$HIPISA, necessity = FALSE)
QCAfit(1-EXP$HIEXP, EXP$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of HIEXP for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(EXP$HIEXP, EXP$LOPISA, necessity = FALSE)
QCAfit(1-EXP$HIEXP, EXP$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with individual outcome enabling conditions 
#------------------------------------------------------------------------------
#
TTEXP1<-truthTable(EXP, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,
                    HIINDIV, HIINCEQ, HIEXP", 
                    incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                    complete=TRUE)
TTEXP1
#
sol_EXP1_c <- minimize(TTEXP1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_EXP1_c
#
# With individual OECs
TTEXPHDI<-truthTable(EXP, outcome = "HIPISA", conditions = "HIHDI, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPHDI
#
TTEXPGEN<-truthTable(EXP, outcome = "HIPISA", conditions = "HIGENEQ, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPGEN
#
TTEXPINC<-truthTable(EXP, outcome = "HIPISA", conditions = "HIINCEQ, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPINC
#
TTEXPINDIV<-truthTable(EXP, outcome = "HIPISA", conditions = "HIINDIV, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with individual outcome enabling conditions HIEXP
#------------------------------------------------------------------------------
#
TTEXP1LO<-truthTable(EXP, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI,
                   HIINDIV, HIINCEQ, HIEXP", 
                   incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                   complete=TRUE)
TTEXP1LO
#
sol_EXP1LO_c <- minimize(TTEXP1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_EXP1LO_c
#
# With individual OECs
TTEXPHDILO<-truthTable(EXP, outcome = "LOPISA", conditions = "HIHDI, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPHDILO
#
TTEXPGENLO<-truthTable(EXP, outcome = "LOPISA", conditions = "HIGENEQ, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPGENLO
#
TTEXPINCLO<-truthTable(EXP, outcome = "LOPISA", conditions = "HIINCEQ, HIEXP", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTEXPINCLO
#
TTEXPINDIVLO<-truthTable(EXP, outcome = "LOPISA", conditions = "HIINDIV, HIEXP", 
                       incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                       complete=TRUE)
TTEXPINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# INDUCT
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
INDUCT$HIINDUCT <- ifelse(INDUCT$INDUCT>=50,1,0)
#
rownames(INDUCT)[INDUCT$HIINDUCT==1]
rownames(INDUCT)[INDUCT$HIINDUCT==0]
#
rownames(INDUCT)[INDUCT$HIPISA==1]
rownames(INDUCT)[INDUCT$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of INDUCT to PISASCORE
#------------------------------------------------------------------------------
#
CORR_INDUCT <- cor.test(INDUCT$INDUCT, INDUCT$PISASCORE, method = "pearson")
CORR_INDUCT
#
#------------------------------------------------------------------------------
# Analysis of INDUCT by outcome exabling conditions
#------------------------------------------------------------------------------
#
INDUCT$FHIINDUCT <- factor(INDUCT$HIINDUCT)
#
ggplot(INDUCT,aes(INDUCT,PISASCORE)) + 
  geom_point()+
 # geom_smooth(aes(INDUCT,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by teacher induction") +
  xlab("Teacher induction") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmINDUCT <- lm(PISASCORE ~ INDUCT, data=INDUCT)
summary(lmINDUCT)
summ(lmINDUCT)
#
# INDUCT IS NOT SIGNIFICANT (p=0.846)
#
#------------------------------------------------------------------------------
# Analysis of INDUCT with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
INDUCT$FHIINCEQ <- factor(INDUCT$HIINCEQ)
ddply(INDUCT,c("HIINDUCT","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(INDUCT,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIINDUCT))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIINDUCT \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeacher \nInduction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(INDUCT,aes(INDUCT,PISASCORE)) + 
  geom_point(aes(INDUCT,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(INDUCT,HIINCEQ==0),
              aes(INDUCT,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(INDUCT,HIINCEQ==1),
              aes(INDUCT,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Teacher induction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmINDUCTHIINCEQ <- lm(PISASCORE ~ INDUCT + HIINCEQ, data=INDUCT)
summary(lmINDUCTHIINCEQ)
#
# INDUCT IS NOT SIGNIFICANT (p=0.648)
#
summ(lmINDUCTHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmINDUCTxHIINCEQ <- lm(PISASCORE ~ INDUCT + HIINCEQ + INDUCT:HIINCEQ, data=INDUCT)
summary(lmINDUCTxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.112)
#
summ(lmINDUCTxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
INDUCT$FHIGENEQ <- factor(INDUCT$HIGENEQ)
ddply(INDUCT,c("HIINDUCT","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(INDUCT,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIINDUCT))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIINDUCT \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeacher \nInduction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(INDUCT,aes(INDUCT,PISASCORE)) + 
  geom_point(aes(INDUCT,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(INDUCT,HIGENEQ==0),
              aes(INDUCT,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(INDUCT,HIGENEQ==1),
              aes(INDUCT,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("Teacher induction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmINDUCTHIGENEQ <- lm(PISASCORE ~ INDUCT + HIGENEQ, data=INDUCT)
summary(lmINDUCTHIGENEQ)
#
# INDUCT IS NOT SIGNIFICANT (p=0.255)
#
summ(lmINDUCTHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmINDUCTxHIGENEQ <- lm(PISASCORE ~ INDUCT + HIGENEQ + INDUCT:HIGENEQ, data=INDUCT)
summary(lmINDUCTxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.740), model significant (p<0.001)
#
summ(lmINDUCTxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
INDUCT$FHIHDI <- factor(INDUCT$HIHDI)
ddply(INDUCT,c("HIINDUCT","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(INDUCT,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIINDUCT))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIINDUCT \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeacher \nInduction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(INDUCT,aes(INDUCT,PISASCORE)) + 
  geom_point(aes(INDUCT,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(INDUCT,HIHDI==0),
              aes(INDUCT,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(INDUCT,HIHDI==1),
              aes(INDUCT,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("Teacher induction") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmINDUCTHIHDI <- lm(PISASCORE ~ INDUCT + HIHDI, data=INDUCT)
summary(lmINDUCTHIHDI)
#
# INDUCT IS NOT SIGNIFICANT (p=0.982)
#
summ(lmINDUCTHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmINDUCTxHIHDI <- lm(PISASCORE ~ INDUCT + HIHDI + INDUCT:HIHDI, data=INDUCT)
summary(lmINDUCTxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.080), model significant (p=0.004)
#
summ(lmINDUCTxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
INDUCT$FHIINDIV <- factor(INDUCT$HIINDIV)
ddply(INDUCT,c("HIINDUCT","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(INDUCT,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIINDUCT))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIINDUCT \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeacher \nInduction") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(INDUCT,aes(INDUCT,PISASCORE)) + 
  geom_point(aes(INDUCT,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(INDUCT,HIINDIV==0),
              aes(INDUCT,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(INDUCT,HIINDIV==1),
              aes(INDUCT,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("INDUCT") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmINDUCTHIINDIV <- lm(PISASCORE ~ INDUCT + HIINDIV, data=INDUCT)
summary(lmINDUCTHIINDIV)
#
# INDUCT IS NOT SIGNIFICANT (p=0.175)
#
summ(lmINDUCTHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmINDUCTxHIINDIV <- lm(PISASCORE ~ INDUCT + HIINDIV + INDUCT:HIINDIV, data=INDUCT)
summary(lmINDUCTxHIINDIV)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.0012), model significant (p<0.001)
#
plot_model(lmINDUCTxHIINDIV, type = "pred", terms = c("INDUCT", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmINDUCTxHIINDIV)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmINDUCTxHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REMOVE FACTORS FROM DATA FRAME
INDUCT <- INDUCT[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
INDUCT <- INDUCT[,-c(2)]
#
# Reposition PISASCORE to end of data grid
INDUCT$INDUCT <- INDUCT$PISASCORE
INDUCT <- INDUCT[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of INDUCT for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(INDUCT$HIINDUCT, INDUCT$HIPISA, necessity = FALSE)
QCAfit(1-INDUCT$HIINDUCT, INDUCT$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of INDUCT for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(INDUCT$HIINDUCT, INDUCT$LOPISA, necessity = FALSE)
QCAfit(1-INDUCT$HIINDUCT, INDUCT$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTINDUCT1<-truthTable(INDUCT, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI, 
                     HIINDIV, HIINCEQ, HIINDUCT", 
                     incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTINDUCT1
#
sol_INDUCT1_c <- minimize(TTINDUCT1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_INDUCT1_c
#
# With individual OECs
TTINDUCTHDI<-truthTable(INDUCT, outcome = "HIPISA", conditions = "HIHDI, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTHDI
#
TTINDUCTGEN<-truthTable(INDUCT, outcome = "HIPISA", conditions = "HIGENEQ, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTGEN
#
TTINDUCTINC<-truthTable(INDUCT, outcome = "HIPISA", conditions = "HIINCEQ, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTINC
#
TTINDUCTINDIV<-truthTable(INDUCT, outcome = "HIPISA", conditions = "HIINDIV, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTINDUCT1LO<-truthTable(INDUCT, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI, 
                      HIINDIV, HIINCEQ, HIINDUCT", 
                      incl.cut = .8, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTINDUCT1LO
#
sol_INDUCT1LO_c <- minimize(TTINDUCT1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_INDUCT1LO_c
#
# With individual OECs
TTINDUCTHDILO<-truthTable(INDUCT, outcome = "LOPISA", conditions = "HIHDI, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTHDILO
#
TTINDUCTGENLO<-truthTable(INDUCT, outcome = "LOPISA", conditions = "HIGENEQ, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTGENLO
#
TTINDUCTINCLO<-truthTable(INDUCT, outcome = "LOPISA", conditions = "HIINCEQ, HIINDUCT", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTINDUCTINCLO
#
TTINDUCTINDIVLO<-truthTable(INDUCT, outcome = "LOPISA", conditions = "HIINDIV, HIINDUCT", 
                          incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTINDUCTINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# MENTOR
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
MENTOR$HIMENTOR <- ifelse(MENTOR$MENTOR>=10,1,0)
#
rownames(MENTOR)[MENTOR$HIMENTOR==1]
rownames(MENTOR)[MENTOR$HIMENTOR==0]
#
rownames(MENTOR)[MENTOR$HIPISA==1]
rownames(MENTOR)[MENTOR$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of MENTOR to PISASCORE
#------------------------------------------------------------------------------
#
CORR_MENTOR <- cor.test(MENTOR$MENTOR, MENTOR$PISASCORE, method = "pearson")
CORR_MENTOR
#
#------------------------------------------------------------------------------
# Analysis of MENTOR by outcome exabling conditions
#------------------------------------------------------------------------------
#
MENTOR$FHIMENTOR <- factor(MENTOR$HIMENTOR)
#
ggplot(MENTOR,aes(MENTOR,PISASCORE)) + 
  geom_point()+
#  geom_smooth(aes(MENTOR,PISASCORE),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("PISA scores by mentor") +
  xlab("Teacher has mentor") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmMENTOR <- lm(PISASCORE ~ MENTOR, data=MENTOR)
summary(lmMENTOR)
summ(lmMENTOR)
#
# INDUCT IS NOT SIGNIFICANT (p=0.678)
#
#------------------------------------------------------------------------------
# Analysis of MENTOR with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
MENTOR$FHIINCEQ <- factor(MENTOR$HIINCEQ)
ddply(MENTOR,c("HIMENTOR","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(MENTOR,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHIMENTOR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIMENTOR \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeachers with \nMentor") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(MENTOR,aes(MENTOR,PISASCORE)) + 
  geom_point(aes(MENTOR,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(MENTOR,HIINCEQ==0),
              aes(MENTOR,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  geom_smooth(data=subset(MENTOR,HIINCEQ==1),
              aes(MENTOR,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="longdash", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("Teacher has mentor") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmMENTORHIINCEQ <- lm(PISASCORE ~ MENTOR + HIINCEQ, data=MENTOR)
summary(lmMENTORHIINCEQ)
#
# MENTOR IS NOT SIGNIFICANT (p=0.413)
#
summ(lmMENTORHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmMENTORxHIINCEQ <- lm(PISASCORE ~ MENTOR + HIINCEQ + MENTOR:HIINCEQ, data=MENTOR)
summary(lmMENTORxHIINCEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.456)
#
summ(lmMENTORxHIINCEQ)
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
MENTOR$FHIGENEQ <- factor(MENTOR$HIGENEQ)
ddply(MENTOR,c("HIMENTOR","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(MENTOR,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHIMENTOR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIMENTOR \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeachers with \nMentor") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(MENTOR,aes(MENTOR,PISASCORE)) + 
  geom_point(aes(MENTOR,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(MENTOR,HIGENEQ==0),
              aes(MENTOR,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(MENTOR,HIGENEQ==1),
              aes(MENTOR,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("MENTOR") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmMENTORHIGENEQ <- lm(PISASCORE ~ MENTOR + HIGENEQ, data=MENTOR)
summary(lmMENTORHIGENEQ)
#
# MENTOR IS NOT SIGNIFICANT (p=0.855)
#
summ(lmMENTORHIGENEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmMENTORxHIGENEQ <- lm(PISASCORE ~ MENTOR + HIGENEQ + MENTOR:HIGENEQ, data=MENTOR)
summary(lmMENTORxHIGENEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.001), model significant (p<0.001)
#
plot_model(lmMENTORxHIGENEQ, type = "pred", terms = c("MENTOR", "HIGENEQ")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmMENTORxHIGENEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmMENTORxHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIHDI
#
MENTOR$FHIHDI <- factor(MENTOR$HIHDI)
ddply(MENTOR,c("HIMENTOR","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(MENTOR,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHIMENTOR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIMENTOR \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeachers with \nMentor") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(MENTOR,aes(MENTOR,PISASCORE)) + 
  geom_point(aes(MENTOR,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(MENTOR,HIHDI==0),
              aes(MENTOR,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(MENTOR,HIHDI==1),
              aes(MENTOR,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("MENTOR") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmMENTORHIHDI <- lm(PISASCORE ~ MENTOR + HIHDI, data=MENTOR)
summary(lmMENTORHIHDI)
#
# MENTOR IS NOT SIGNIFICANT (p=0.479)
#
summ(lmMENTORHIHDI)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmMENTORxHIHDI <- lm(PISASCORE ~ MENTOR + HIHDI + MENTOR:HIHDI, data=MENTOR)
summary(lmMENTORxHIHDI)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.0002), model significant (p<0.001)
#
plot_model(lmMENTORxHIHDI, type = "pred", terms = c("MENTOR", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmMENTORxHIHDI)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmMENTORxHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIINDIV
#
MENTOR$FHIINDIV <- factor(MENTOR$HIINDIV)
ddply(MENTOR,c("HIMENTOR","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(MENTOR,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHIMENTOR))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HIMENTOR \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nPercentage \nTeachers with \nMentor") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(MENTOR,aes(MENTOR,PISASCORE)) + 
  geom_point(aes(MENTOR,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(MENTOR,HIINDIV==0),
              aes(MENTOR,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(MENTOR,HIINDIV==1),
              aes(MENTOR,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("Teacher has mentor") +
  ylab("PISA score") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmMENTORHIINDIV <- lm(PISASCORE ~ MENTOR + HIINDIV, data=MENTOR)
summary(lmMENTORHIINDIV)
#
# MENTOR IS NOT SIGNIFICANT (p=0.110)
#
summ(lmMENTORHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmMENTORxHIINDIV <- lm(PISASCORE ~ MENTOR + HIINDIV + MENTOR:HIINDIV, data=MENTOR)
summary(lmMENTORxHIINDIV)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.666)
#
summ(lmMENTORxHIINDIV)
#
# REMOVE FACTORS FROM DATA FRAME
MENTOR <- MENTOR[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
MENTOR <- MENTOR[,-c(2)]
#
# Reposition PISASCORE to end of data grid
MENTOR$MENTOR <- MENTOR$PISASCORE
MENTOR <- MENTOR[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of MENTOR for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(MENTOR$HIMENTOR, MENTOR$HIPISA, necessity = FALSE)
QCAfit(1-MENTOR$HIMENTOR, MENTOR$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of MENTOR for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(MENTOR$HIMENTOR, MENTOR$LOPISA, necessity = FALSE)
QCAfit(1-MENTOR$HIMENTOR, MENTOR$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTMENTOR1<-truthTable(MENTOR, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI,
                     HIINDIV, HIINCEQ, HIMENTOR", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTMENTOR1
#
sol_MENTOR1_c <- minimize(TTMENTOR1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_MENTOR1_c
#
# With inidividual OECs
TTMENTORHDI<-truthTable(MENTOR, outcome = "HIPISA", conditions = "HIHDI, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORHDI
#
TTMENTORGEN<-truthTable(MENTOR, outcome = "HIPISA", conditions = "HIGENEQ, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORGEN
#
TTMENTORINC<-truthTable(MENTOR, outcome = "HIPISA", conditions = "HIINCEQ, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORINC
#
TTMENTORINDIV<-truthTable(MENTOR, outcome = "HIPISA", conditions = "HIINDIV, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTMENTOR1LO<-truthTable(MENTOR, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI,
                      HIINDIV, HIINCEQ, HIMENTOR", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTMENTOR1LO
#
sol_MENTOR1LO_c <- minimize(TTMENTOR1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_MENTOR1LO_c
#
# With inidividual OECs
TTMENTORHDILO<-truthTable(MENTOR, outcome = "LOPISA", conditions = "HIHDI, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORHDILO
#
TTMENTORGENLO<-truthTable(MENTOR, outcome = "LOPISA", conditions = "HIGENEQ, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORGENLO
#
TTMENTORINCLO<-truthTable(MENTOR, outcome = "LOPISA", conditions = "HIINCEQ, HIMENTOR", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTMENTORINCLO
#
TTMENTORINDIVLO<-truthTable(MENTOR, outcome = "LOPISA", conditions = "HIINDIV, HIMENTOR", 
                          incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTMENTORINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# NONCON
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
#
NONCON$HINONCON <- ifelse(NONCON$NONCON>=0.50,1,0)
#
rownames(NONCON)[NONCON$HINONCON==1]
rownames(NONCON)[NONCON$HINONCON==0]
#
rownames(NONCON)[NONCON$HIPISA==1]
rownames(NONCON)[NONCON$LOPISA==1]
#
#------------------------------------------------------------------------------
# Check correlation of NONCON to PISASCORE
#------------------------------------------------------------------------------
#
CORR_NONCON <- cor.test(NONCON$NONCON, NONCON$PISASCORE, method = "pearson")
CORR_NONCON
#
#------------------------------------------------------------------------------
# Analysis of NONCON by outcome exabling conditions
#------------------------------------------------------------------------------
#
NONCON$FHINONCON <- factor(NONCON$HINONCON)
#
ggplot(NONCON,aes(NONCON,PISASCORE)) + 
  geom_point()+
  geom_smooth(aes(NONCON,PISASCORE),method=lm,se=FALSE, colour = "grey", linetype="solid", size = 1) +
  ggtitle("PISA scores by non contact hours") +
  xlab("Non contact hours") +
  ylab("PISA score") + ylim(335,610) +
  scale_colour_grey()
#
# REGRESSION MODEL
lmNONCON <- lm(PISASCORE ~ NONCON, data=NONCON)
summary(lmNONCON)
summ(lmNONCON)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmNONCON)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# NONCON IS SIGNIFICANT (p=0.0026) 
#
plot_model(lmNONCON, type = "pred", terms = c("NONCON"))
#
effect_plot(lmNONCON, pred = NONCON, interval = T, plot.points = T,
            main.title = "Predicted values of PISASCORE")
#
#------------------------------------------------------------------------------
# Analysis of NONCON with each outcome enabling condition
#------------------------------------------------------------------------------
#
# HIINCEQ
#
NONCON$FHIINCEQ <- factor(NONCON$HIINCEQ)
ddply(NONCON,c("HINONCON","HIINCEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(NONCON,aes(x=FHIINCEQ,y=PISASCORE, fill=factor(FHINONCON))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HINONCON \n(grouped by HIINCEQ)") +
  xlab("High income equality") +
  ylab("PISA score") +
  labs(fill = "High \nProportion \nNon contact \nHours") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(NONCON,aes(NONCON,PISASCORE)) + 
  geom_point(aes(NONCON,PISASCORE,colour=factor(HIINCEQ)))+
  geom_smooth(data=subset(NONCON,HIINCEQ==0),
              aes(NONCON,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(NONCON,HIINCEQ==1),
              aes(NONCON,PISASCORE,color=factor(HIINCEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINCEQ)") +
  xlab("NONCON") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINCEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmNONCONHIINCEQ <- lm(PISASCORE ~ NONCON + HIINCEQ, data=NONCON)
summary(lmNONCONHIINCEQ)
#
# HIINCEQ IS NOT SIGNIFICANT (p=0.199)
#
summ(lmNONCONHIINCEQ)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmNONCONxHIINCEQ <- lm(PISASCORE ~ NONCON + HIINCEQ + NONCON:HIINCEQ, data=NONCON)
summary(lmNONCONxHIINCEQ)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.044), model significant (p=0.002)
#
plot_model(lmNONCONxHIINCEQ, type = "pred", terms = c("NONCON", "HIINCEQ")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmNONCONxHIINCEQ)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmNONCONxHIINCEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
#------------------------------------------------------------------------------------
# HIGENEQ
#
NONCON$FHIGENEQ <- factor(NONCON$HIGENEQ)
ddply(NONCON,c("HINONCON","HIGENEQ"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(NONCON,aes(x=FHIGENEQ,y=PISASCORE, fill=factor(FHINONCON))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HINONCON \n(grouped by HIGENEQ)") +
  xlab("High gender equity") +
  ylab("PISA score") +
  labs(fill = "High \nProportion \nNon contact \nHours") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(NONCON,aes(NONCON,PISASCORE)) + 
  geom_point(aes(NONCON,PISASCORE,colour=factor(HIGENEQ)))+
  geom_smooth(data=subset(NONCON,HIGENEQ==0),
              aes(NONCON,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(NONCON,HIGENEQ==1),
              aes(NONCON,PISASCORE,color=factor(HIGENEQ)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIGENEQ)") +
  xlab("NONCON") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIGENEQ") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmNONCONHIGENEQ <- lm(PISASCORE ~ NONCON + HIGENEQ, data=NONCON)
summary(lmNONCONHIGENEQ)
#
# BOTH TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmNONCONHIGENEQ)
plot_model(lmNONCONHIGENEQ, type = "pred", terms = c("NONCON", "HIGENEQ")) + scale_colour_grey() +
  ylim(335,610)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmNONCONHIGENEQ)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmNONCONxHIGENEQ <- lm(PISASCORE ~ NONCON + HIGENEQ + NONCON:HIGENEQ, data=NONCON)
summary(lmNONCONxHIGENEQ)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.771), model significant (p<0.001)
#
summ(lmNONCONxHIGENEQ)
#
#------------------------------------------------------------------------------------
# HIHDI
#
NONCON$FHIHDI <- factor(NONCON$HIHDI)
ddply(NONCON,c("HINONCON","HIHDI"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(NONCON,aes(x=FHIHDI,y=PISASCORE, fill=factor(FHINONCON))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HINONCON \n(grouped by HIHDI)") +
  xlab("High human development") +
  ylab("PISA score") +
  labs(fill = "High \nProportion \nNon contact \nHours") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(NONCON,aes(NONCON,PISASCORE)) + 
  geom_point(aes(NONCON,PISASCORE,colour=factor(HIHDI)))+
  geom_smooth(data=subset(NONCON,HIHDI==0),
              aes(NONCON,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(NONCON,HIHDI==1),
              aes(NONCON,PISASCORE,color=factor(HIHDI)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIHDI)") +
  xlab("NONCON") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIHDI") +
  guides(size = "none") +
  scale_colour_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmNONCONHIHDI <- lm(PISASCORE ~ NONCON + HIHDI, data=NONCON)
summary(lmNONCONHIHDI)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p<0.001)
#
summ(lmNONCONHIHDI)
plot_model(lmNONCONHIHDI, type = "pred", terms = c("NONCON", "HIHDI")) + scale_colour_grey() +
  ylim(335,610)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmNONCONHIHDI)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REGRESSION MODEL WITH INTERACTION TERM
lmNONCONxHIHDI <- lm(PISASCORE ~ NONCON + HIHDI + NONCON:HIHDI, data=NONCON)
summary(lmNONCONxHIHDI)
#
# THE INTERACTION TERM IS NOT SIGNIFICANT (p=0.727), model significant (p=0.001)
#
summ(lmNONCONxHIHDI)
#
#------------------------------------------------------------------------------------
# HIINDIV
#
NONCON$FHIINDIV <- factor(NONCON$HIINDIV)
ddply(NONCON,c("HINONCON","HIINDIV"),summarise, mean=mean(PISASCORE), N=length(PISASCORE))
#
ggplot(NONCON,aes(x=FHIINDIV,y=PISASCORE, fill=factor(FHINONCON))) + 
  geom_boxplot() +
  ggtitle("PISA scores by HINONCON \n(grouped by HIINDIV)") +
  xlab("High individualism") +
  ylab("PISA score") +
  labs(fill = "High \nProportion \nNon contact \nHours") +
  guides(size = "none") +
  scale_colour_grey()
#
# OBSERVED RELATIONSHIP
#
ggplot(NONCON,aes(NONCON,PISASCORE)) + 
  geom_point(aes(NONCON,PISASCORE,colour=factor(HIINDIV)))+
  geom_smooth(data=subset(NONCON,HIINDIV==0),
              aes(NONCON,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  geom_smooth(data=subset(NONCON,HIINDIV==1),
              aes(NONCON,PISASCORE,color=factor(HIINDIV)),method=lm,se=FALSE, linetype="solid", size = 0.5) +
  ggtitle("Observed relationship (grouped by HIINDIV)") +
  xlab("NONCON") +
  ylab("PISASCORE") + ylim(335,610) +
  labs(color = "HIINDIV") +
  guides(size = "none") +
  scale_color_grey()
#
# REGRESSION MODEL WITHOUT INTERACTION TERM
lmNONCONHIINDIV <- lm(PISASCORE ~ NONCON + HIINDIV, data=NONCON)
summary(lmNONCONHIINDIV)
#
# ALL TERMS SIGNIFICANT, MODEL SIGNIFICANT (p=0.001)
#
plot_model(lmNONCONHIINDIV, type = "pred", terms = c("NONCON", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmNONCONHIINDIV)
#
# REGRESSION MODEL WITH INTERACTION TERM
lmNONCONxHIINDIV <- lm(PISASCORE ~ NONCON + HIINDIV + NONCON:HIINDIV, data=NONCON)
summary(lmNONCONxHIINDIV)
#
# THE INTERACTION TERM IS SIGNIFICANT (p=0.0431), model significant (p<0.001)
#
theme_set(theme_sjplot())
plot_model(lmNONCONxHIINDIV, type = "pred", terms = c("NONCON", "HIINDIV")) + scale_colour_grey() +
  ylim(335,610)
#
summ(lmNONCONxHIINDIV)
#
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lmNONCONHIINDIV)
plot(lmNONCONxHIINDIV)
par(mfrow = c(1, 1))  # Return plotting panel to 1 section
#
# REMOVE FACTORS FROM DATA FRAME
NONCON <- NONCON[,-c(15:19)]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
NONCON <- NONCON[,-c(2)]
#
# Reposition PISASCORE to end of data grid
NONCON$NONCON <- NONCON$PISASCORE
NONCON <- NONCON[,-c(9)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis of NONCON for HIPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(NONCON$HINONCON, NONCON$HIPISA, necessity = FALSE)
QCAfit(1-NONCON$HINONCON, NONCON$HIPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis of NONCON for LOPISA all cases
#------------------------------------------------------------------------------
#
QCAfit(NONCON$HINONCON, NONCON$LOPISA, necessity = FALSE)
QCAfit(1-NONCON$HINONCON, NONCON$LOPISA, necessity = FALSE)
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTNONCON1<-truthTable(NONCON, outcome = "HIPISA", conditions = "HIGENEQ, HIHDI, 
                     HIINDIV, HIINCEQ, HINONCON", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTNONCON1
#
sol_NONCON1_c <- minimize(TTNONCON1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_NONCON1_c
#
# With individual OECs
TTNONCONHDI<-truthTable(NONCON, outcome = "HIPISA", conditions = "HIHDI, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONHDI
#
TTNONCONGEN<-truthTable(NONCON, outcome = "HIPISA", conditions = "HIGENEQ, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONGEN
#
TTNONCONINC<-truthTable(NONCON, outcome = "HIPISA", conditions = "HIINCEQ, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONINC
#
TTNONCONINDIV<-truthTable(NONCON, outcome = "HIPISA", conditions = "HIINDIV, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONINDIV
#
#------------------------------------------------------------------------------
# Sufficiency analysis on LOPISA with individual outcome enabling conditions
#------------------------------------------------------------------------------
#
TTNONCON1LO<-truthTable(NONCON, outcome = "LOPISA", conditions = "HIGENEQ, HIHDI, 
                      HIINDIV, HIINCEQ, HINONCON", 
                      incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                      complete=TRUE)
TTNONCON1LO
#
sol_NONCON1LO_c <- minimize(TTNONCON1LO, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_NONCON1LO_c
#
# With individual OECs
TTNONCONHDILO<-truthTable(NONCON, outcome = "LOPISA", conditions = "HIHDI, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONHDILO
#
TTNONCONGENLO<-truthTable(NONCON, outcome = "LOPISA", conditions = "HIGENEQ, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONGENLO
#
TTNONCONINCLO<-truthTable(NONCON, outcome = "LOPISA", conditions = "HIINCEQ, HINONCON", 
                        incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                        complete=TRUE)
TTNONCONINCLO
#
TTNONCONINDIVLO<-truthTable(NONCON, outcome = "LOPISA", conditions = "HIINDIV, HINONCON", 
                          incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                          complete=TRUE)
TTNONCONINDIVLO
#
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# COMBINED PROFESS excluding EXP, PDEV, INDUCT, MENTOR (no relationship found),
#           
#------------------------------------------------------------------------------
# Calibrate Data
#------------------------------------------------------------------------------
#
# Import csv file COMBINED
PROFCOMBINED <- read.csv("PROFCOMBINED.csv", row.names = 1)
head(PROFCOMBINED)
#
rownames(PROFCOMBINED)[PROFCOMBINED$HIPISA==1]
rownames(PROFCOMBINED)[PROFCOMBINED$LOPISA==1]
#
PROFCOMBINED$HISALARY <- ifelse(PROFCOMBINED$SALARY>=30000,1,0)
rownames(PROFCOMBINED)[PROFCOMBINED$HISALARY==1]
rownames(PROFCOMBINED)[PROFCOMBINED$HISALARY==0]
#
PROFCOMBINED$HIQUALS <- ifelse(PROFCOMBINED$QUALS>=92,1,0)
rownames(PROFCOMBINED)[PROFCOMBINED$HIQUALS==1]
rownames(PROFCOMBINED)[PROFCOMBINED$HIQUALS==0]
#
PROFCOMBINED$HINONCON <- ifelse(PROFCOMBINED$NONCON>=0.50,1,0)
rownames(PROFCOMBINED)[PROFCOMBINED$HINONCON==1]
rownames(PROFCOMBINED)[PROFCOMBINED$HINONCON==0]
#
#------------------------------------------------------------------------------
# Eliminate variables not needed for QCA
#------------------------------------------------------------------------------
#
PROFCOMBINED <- PROFCOMBINED[,-c(2:8)]
#
# Reposition PISASCORE to end of data grid
PROFCOMBINED$PISA <- PROFCOMBINED$PISASCORE
PROFCOMBINED <- PROFCOMBINED[,-c(9)]
#
PROFCOMBINED <- PROFCOMBINED[,-c(5:7)]
#
#------------------------------------------------------------------------------
# Sufficiency analysis on HIPISA with outcome enabling conditions
#------------------------------------------------------------------------------
#
# With only outcome enabling conditions
#
TTCOMBIoec<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HIGENEQ, 
                     HIHDI, HIINDIV, HIINCEQ", 
                     incl.cut = .80, sort.by = c("out", "inc", "n"), n.cut = 1, 
                     complete=TRUE)
TTCOMBIoec
#
sol_COMBIoec_c <- minimize(TTCOMBIoec, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBIoec_c
#
# Without outcome enabling conditions
#
TTCOMBIpc<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HISALARY, HIQUALS, 
                              HINONCON", 
                              incl.cut = .80, show.cases = T, n.cut = 1, 
                              complete=TRUE)
TTCOMBIpc
#
sol_COMBIpc_c <- minimize(TTCOMBIpc, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBIpc_c
#
# With outcome enabling conditions 1x1
#
TTCOMBI1<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HIGENEQ, 
                      HISALARY, HIQUALS, HINONCON", 
                       incl.cut = .80, show.cases = T, n.cut = 1, 
                       complete=TRUE)
TTCOMBI1
#
sol_COMBI1_c <- minimize(TTCOMBI1, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI1_c
#
TTCOMBI2<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HIHDI, 
                      HISALARY, HIQUALS, HINONCON", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI2
#
sol_COMBI2_c <- minimize(TTCOMBI2, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI2_c
#
TTCOMBI3<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HIINCEQ, 
                     HISALARY, HIQUALS, HINONCON", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
                     complete=TRUE)
TTCOMBI3
#
sol_COMBI3_c <- minimize(TTCOMBI3, details=TRUE, show.cases=TRUE, row.dom = TRUE)
sol_COMBI3_c
#
TTCOMBI4<-truthTable(PROFCOMBINED, outcome = "HIPISA", conditions = "HIINDIV, 
                     HISALARY, HIQUALS, HINONCON", 
                     incl.cut = .80, show.cases = T, n.cut = 1, 
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



