rm(list=ls())
####################################################### Packages & Data #######################################################
library(foreign)
library(Hmisc)
library(arm)
library(survey)
library(miceadds)
library(foreign)
library(readstata13)
vote <- read.dta13("/Users/deanlacy/Dropbox/MLEshared/MLE/economicvoting/econ8008subsetforRv2.dta")
############################################################ Model ############################################################
# Line 2 in Stata file

# Alternatively, create a new dataset that excludes Year==2000 and rats==1
# vote <- subset(vote, Year != 2000 & rats3 == 0)
# logit.mod.2 <- glm.cluster(VoteMajor2 ~ info + win + party + age2srev + incomerev + BlackNMrev + Femalerev + Southernrev + RetroNatl + ProspNatl + RetroPocket + ProspPocket + infowin + inforetronatl + infopronatl + inforetropck + infopropck + infowinretronatl + infowinpropck + infowinretropck + infowinpronatl + winretronatl + winretropck + winpropck + winpronatl + as.factor(Year),cluster="Year",family=binomial(link="logit"),data=vote) 

logit.mod.2 <- glm.cluster(VoteMajor2 ~ info + win + party + age2srev + incomerev + BlackNMrev + Femalerev + Southernrev + RetroNatl + ProspNatl + RetroPocket + ProspPocket + infowin + inforetronatl + infopronatl + inforetropck + infopropck + infowinretronatl + infowinpropck + infowinretropck + infowinpronatl + winretronatl + winretropck + winpropck + winpronatl + as.factor(Year),cluster="Year",family=binomial(link="logit"),data=subset(vote, Year != 2000 & rats3 == 0)) 
summary(logit.mod.2)

############################################## Most Informed Voters, Incumbent wins ###########################################
# Line 11 in Stata file:
lincom.1 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=1,"winpronatl"=1,"infowinpronatl"=1)) 
lincom.1 # This is the "profile coefficient" for the individual described above.
lincom.1.data <- data.frame(lincom.1)
pred.lbound.1 <- lincom.1.data$contrast-(lincom.1.data$SE*1.96)
pred.pr.1 <- 1/(1+exp(-lincom.1.data$contrast))-(1/(1+exp(-0)))
pred.lbound.1 <- (1/(1+exp(-pred.lbound.1))-(1/(1+exp(-0))))
pred.se.1 <- (pred.pr.1-pred.lbound.1)/1.96
pred.pr.1
pred.se.1
# Stata Pred. Pr. change: coef=.208; se=.034

# Line 13 in Stata file:
lincom.2 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=1,"winretronatl"=1,"infowinretronatl"=1)) 
lincom.2 # This is the "profile coefficient" for the individual described above.
lincom.2.data <- data.frame(lincom.2)
pred.lbound.2 <- lincom.2.data$contrast-(lincom.2.data$SE*1.96)
pred.pr.2 <- 1/(1+exp(-lincom.2.data$contrast))-(1/(1+exp(-0)))
pred.lbound.2 <- (1/(1+exp(-pred.lbound.2))-(1/(1+exp(-0))))
pred.se.2 <- (pred.pr.2-pred.lbound.2)/1.96
pred.pr.2
pred.se.2
# Stata Pred. Pr. change: coef=.145; se=.019

# Line 15 in Stata file:
lincom.3 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=1,"winpropck"=1,"infowinpropck"=1)) 
lincom.3 # This is the "profile coefficient" for the individual described above.
lincom.3.data <- data.frame(lincom.3)
pred.lbound.3 <- lincom.3.data$contrast-(lincom.3.data$SE*1.96)
pred.pr.3 <- 1/(1+exp(-lincom.3.data$contrast))-(1/(1+exp(-0)))
pred.lbound.3 <- (1/(1+exp(-pred.lbound.3))-(1/(1+exp(-0))))
pred.se.3 <- (pred.pr.3-pred.lbound.3)/1.96
pred.pr.3
pred.se.3
# Stata Pred. Pr. change: coef=.106; se=.014

# Line 17 in Stata file:
lincom.4 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=1,"winretropck"=1,"infowinretropck"=1)) 
lincom.4 # This is the "profile coefficient" for the individual described above.
lincom.4.data <- data.frame(lincom.4)
pred.lbound.4 <- lincom.4.data$contrast-(lincom.4.data$SE*1.96)
pred.pr.4 <- 1/(1+exp(-lincom.4.data$contrast))-(1/(1+exp(-0)))
pred.lbound.4 <- (1/(1+exp(-pred.lbound.4))-(1/(1+exp(-0))))
pred.se.4 <- (pred.pr.4-pred.lbound.4)/1.96
pred.pr.4
pred.se.4
# Stata Pred. Pr. change: coef=.034; se=.026

# Create matrix of Most Informed Voters, Incumbent wins
cells <- c(1,pred.pr.1,pred.se.1,pred.pr.2,pred.se.2,pred.pr.3,pred.se.3,pred.pr.4,pred.se.4)
rnames <- c("1")
cnames <- c("Info","pronat.in","pronat.in.se","retnat.in","retnat.in.se","propck.in","propck.in.se","retpck.in","retpck.in.se")
most.inf.inc.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                           dimnames=list(rnames, cnames))
most.inf.inc.win

########################################## High Moderate Informed Voters, Incumbent wins ######################################
# Line 22 in Stata file:
lincom.5 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=.5,"winpronatl"=1,"infowinpronatl"=.5)) 
lincom.5 # This is the "profile coefficient" for the individual described above.
lincom.5.data <- data.frame(lincom.5)
pred.lbound.5 <- lincom.5.data$contrast-(lincom.5.data$SE*1.96)
pred.pr.5 <- 1/(1+exp(-lincom.5.data$contrast))-(1/(1+exp(-0)))
pred.lbound.5 <- (1/(1+exp(-pred.lbound.5))-(1/(1+exp(-0))))
pred.se.5 <- (pred.pr.5-pred.lbound.5)/1.96
pred.pr.5
pred.se.5
# Stata Pred. Pr. change: coef=.146; se=.021

# Line 24 in Stata file:
lincom.6 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=.5,"winretronatl"=1,"infowinretronatl"=.5)) 
lincom.6 # This is the "profile coefficient" for the individual described above.
lincom.6.data <- data.frame(lincom.6)
pred.lbound.6 <- lincom.6.data$contrast-(lincom.6.data$SE*1.96)
pred.pr.6 <- 1/(1+exp(-lincom.6.data$contrast))-(1/(1+exp(-0)))
pred.lbound.6 <- (1/(1+exp(-pred.lbound.6))-(1/(1+exp(-0))))
pred.se.6 <- (pred.pr.6-pred.lbound.6)/1.96
pred.pr.6
pred.se.6
# Stata Pred. Pr. change: coef=.139; se=.012

# Line 26 in Stata file:
lincom.7 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=.5,"winpropck"=1,"infowinpropck"=.5)) 
lincom.7 # This is the "profile coefficient" for the individual described above.
lincom.7.data <- data.frame(lincom.7)
pred.lbound.7 <- lincom.7.data$contrast-(lincom.7.data$SE*1.96)
pred.pr.7 <- 1/(1+exp(-lincom.7.data$contrast))-(1/(1+exp(-0)))
pred.lbound.7 <- (1/(1+exp(-pred.lbound.7))-(1/(1+exp(-0))))
pred.se.7 <- (pred.pr.7-pred.lbound.7)/1.96
pred.pr.7
pred.se.7 
# Stata Pred. Pr. change: coef=.087; se=.014

# Line 28 in Stata file:
lincom.8 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=.5,"winretropck"=1,"infowinretropck"=.5)) 
lincom.8 # This is the "profile coefficient" for the individual described above.
lincom.8.data <- data.frame(lincom.8)
pred.lbound.8 <- lincom.8.data$contrast-(lincom.8.data$SE*1.96)
pred.pr.8 <- 1/(1+exp(-lincom.8.data$contrast))-(1/(1+exp(-0)))
pred.lbound.8 <- (1/(1+exp(-pred.lbound.8))-(1/(1+exp(-0))))
pred.se.8 <- (pred.pr.8-pred.lbound.8)/1.96
pred.pr.8
pred.se.8 
# Stata Pred. Pr. change: coef=.038; se=.013

# Create matrix of Most Informed Voters, Incumbent wins
cells <- c(.5,pred.pr.5,pred.se.5,pred.pr.6,pred.se.6,pred.pr.7,pred.se.7,pred.pr.8,pred.se.8)
rnames <- c(".5")
cnames <- c("Info","pronat.in","pronat.in.se","retnat.in","retnat.in.se","propck.in","propck.in.se","retpck.in","retpck.in.se")
high.mod.inc.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                           dimnames=list(rnames, cnames))
high.mod.inc.win

############################################## Middle Info Voters, Incumbent wins ###########################################
# Line 33 in Stata file:
lincom.9 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=0,"winpronatl"=1,"infowinpronatl"=0)) 
lincom.9 # This is the "profile coefficient" for the individual described above.
lincom.9.data <- data.frame(lincom.9)
pred.lbound.9 <- lincom.9.data$contrast-(lincom.9.data$SE*1.96)
pred.pr.9 <- 1/(1+exp(-lincom.9.data$contrast))-(1/(1+exp(-0)))
pred.lbound.9 <- (1/(1+exp(-pred.lbound.9))-(1/(1+exp(-0))))
pred.se.9 <- (pred.pr.9-pred.lbound.9)/1.96
pred.pr.9
pred.se.9 
# Stata Pred. Pr. change: coef=.078; se=.012

# Line 35 in Stata file:
lincom.10 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=0,"winretronatl"=1,"infowinretronatl"=0)) 
lincom.10 # This is the "profile coefficient" for the individual described above.
lincom.10.data <- data.frame(lincom.10)
pred.lbound.10 <- lincom.10.data$contrast-(lincom.10.data$SE*1.96)
pred.pr.10 <- 1/(1+exp(-lincom.10.data$contrast))-(1/(1+exp(-0)))
pred.lbound.10 <- (1/(1+exp(-pred.lbound.10))-(1/(1+exp(-0))))
pred.se.10 <- (pred.pr.10-pred.lbound.10)/1.96
pred.pr.10
pred.se.10
# Stata Pred. Pr. change: coef=.133; se=.027

# Line 37 in Stata file:
lincom.11 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=0,"winpropck"=1,"infowinpropck"=0)) 
lincom.11 # This is the "profile coefficient" for the individual described above.
lincom.11.data <- data.frame(lincom.11)
pred.lbound.11 <- lincom.11.data$contrast-(lincom.11.data$SE*1.96)
pred.pr.11 <- 1/(1+exp(-lincom.11.data$contrast))-(1/(1+exp(-0)))
pred.lbound.11 <- (1/(1+exp(-pred.lbound.11))-(1/(1+exp(-0))))
pred.se.11 <- (pred.pr.11-pred.lbound.11)/1.96
pred.pr.11
pred.se.11
# Stata Pred. Pr. change: coef=.067; se=.017

# Line 39 in Stata file:
lincom.12 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=0,"winretropck"=1,"infowinretropck"=0)) 
lincom.12 # This is the "profile coefficient" for the individual described above.
lincom.12.data <- data.frame(lincom.12)
pred.lbound.12 <- lincom.12.data$contrast-(lincom.12.data$SE*1.96)
pred.pr.12 <- 1/(1+exp(-lincom.12.data$contrast))-(1/(1+exp(-0)))
pred.lbound.12 <- (1/(1+exp(-pred.lbound.12))-(1/(1+exp(-0))))
pred.se.12 <- (pred.pr.12-pred.lbound.12)/1.96
pred.pr.12
pred.se.12
# Stata Pred. Pr. change: coef=.042; se=.014

# Create matrix of Middle Informed Voters, Incumbent wins
cells <- c(0,pred.pr.9,pred.se.9,pred.pr.10,pred.se.10,pred.pr.11,pred.se.11,pred.pr.12,pred.se.12)
rnames <- c("0")
cnames <- c("Info","pronat.in","pronat.in.se","retnat.in","retnat.in.se","propck.in","propck.in.se","retpck.in","retpck.in.se")
mid.inf.inc.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
mid.inf.inc.win

########################################## Low-Moderately Informed Voters, Incumbent wins ######################################
# Line 45 in Stata file:
lincom.13 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-.5,"winpronatl"=1,"infowinpronatl"=-.5)) 
lincom.13 # This is the "profile coefficient" for the individual described above.
lincom.13.data <- data.frame(lincom.13)
pred.lbound.13 <- lincom.13.data$contrast-(lincom.13.data$SE*1.96)
pred.pr.13 <- 1/(1+exp(-lincom.13.data$contrast))-(1/(1+exp(-0)))
pred.lbound.13 <- (1/(1+exp(-pred.lbound.13))-(1/(1+exp(-0))))
pred.se.13 <- (pred.pr.13-pred.lbound.13)/1.96
pred.pr.13
pred.se.13
# Stata Pred. Pr. change: coef=.006; se=.019

# Line 47 in Stata file:
lincom.14 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-.5,"winretronatl"=1,"infowinretronatl"=-.5)) 
lincom.14 # This is the "profile coefficient" for the individual described above.
lincom.14.data <- data.frame(lincom.14)
pred.lbound.14 <- lincom.14.data$contrast-(lincom.14.data$SE*1.96)
pred.pr.14 <- 1/(1+exp(-lincom.14.data$contrast))-(1/(1+exp(-0)))
pred.lbound.14 <- (1/(1+exp(-pred.lbound.14))-(1/(1+exp(-0))))
pred.se.14 <- (pred.pr.14-pred.lbound.14)/1.96
pred.pr.14
pred.se.14
# Stata Pred. Pr. change: coef=.127; se=.046

# Line 49 in Stata file:
lincom.15 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-.5,"winpropck"=1,"infowinpropck"=-.5)) 
lincom.15 # This is the "profile coefficient" for the individual described above.
lincom.15.data <- data.frame(lincom.15)
pred.lbound.15 <- lincom.15.data$contrast-(lincom.15.data$SE*1.96)
pred.pr.15 <- 1/(1+exp(-lincom.15.data$contrast))-(1/(1+exp(-0)))
pred.lbound.15 <- (1/(1+exp(-pred.lbound.15))-(1/(1+exp(-0))))
pred.se.15 <- (pred.pr.15-pred.lbound.15)/1.96
pred.pr.15
pred.se.15
# Pred. Pr. change: coef=.046; se=.022

# Line 51 in Stata file:
lincom.16 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-.5,"winretropck"=1,"infowinretropck"=-.5)) 
lincom.16 # This is the "profile coefficient" for the individual described above.
lincom.16.data <- data.frame(lincom.16)
pred.lbound.16 <- lincom.16.data$contrast-(lincom.16.data$SE*1.96)
pred.pr.16 <- 1/(1+exp(-lincom.16.data$contrast))-(1/(1+exp(-0)))
pred.lbound.16 <- (1/(1+exp(-pred.lbound.16))-(1/(1+exp(-0))))
pred.se.16 <- (pred.pr.16-pred.lbound.16)/1.96
pred.pr.16
pred.se.16
# Pred. Pr. change: coef=.045; se=.028

# Create matrix of Low-Moderately Informed Voters, Incumbent wins
cells <- c(-.5,pred.pr.13,pred.se.13,pred.pr.14,pred.se.14,pred.pr.15,pred.se.15,pred.pr.16,pred.se.16)
rnames <- c("-.5")
cnames <- c("Info","pronat.in","pronat.in.se","retnat.in","retnat.in.se","propck.in","propck.in.se","retpck.in","retpck.in.se")
low.mod.inc.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
low.mod.inc.win

############################################## Least Informed Voters, Incumbent wins ###########################################
# Line 56 in Stata file:
lincom.17 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-1,"winpronatl"=1,"infowinpronatl"=-1)) 
lincom.17 # This is the "profile coefficient" for the individual described above.
lincom.17.data <- data.frame(lincom.17)
pred.lbound.17 <- lincom.17.data$contrast-(lincom.17.data$SE*1.96)
pred.pr.17 <- 1/(1+exp(-lincom.17.data$contrast))-(1/(1+exp(-0)))
pred.lbound.17 <- (1/(1+exp(-pred.lbound.17))-(1/(1+exp(-0))))
pred.se.17 <- (pred.pr.17-pred.lbound.17)/1.96
pred.pr.17
pred.se.17
# Pred. Pr. change: coef=-.065; se=.033

# Line 58 in Stata file:
lincom.18 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-1,"winretronatl"=1,"infowinretronatl"=-1)) 
lincom.18 # This is the "profile coefficient" for the individual described above.
lincom.18.data <- data.frame(lincom.18)
pred.lbound.18 <- lincom.18.data$contrast-(lincom.18.data$SE*1.96)
pred.pr.18 <- 1/(1+exp(-lincom.18.data$contrast))-(1/(1+exp(-0)))
pred.lbound.18 <- (1/(1+exp(-pred.lbound.18))-(1/(1+exp(-0))))
pred.se.18 <- (pred.pr.18-pred.lbound.18)/1.96
pred.pr.18
pred.se.18
# Pred. Pr. change: coef=.120; se=.067

# Line 60 in Stata file:
lincom.19 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-1,"winpropck"=1,"infowinpropck"=-1)) 
lincom.19 # This is the "profile coefficient" for the individual described above.
lincom.19.data <- data.frame(lincom.19)
pred.lbound.19 <- lincom.19.data$contrast-(lincom.19.data$SE*1.96)
pred.pr.19 <- 1/(1+exp(-lincom.19.data$contrast))-(1/(1+exp(-0)))
pred.lbound.19 <- (1/(1+exp(-pred.lbound.19))-(1/(1+exp(-0))))
pred.se.19 <- (pred.pr.19-pred.lbound.19)/1.96
pred.pr.19
pred.se.19
# Pred. Pr. change: coef=.026; se=.029

# Line 62 in Stata file:
lincom.20 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-1,"winretropck"=1,"infowinretropck"=-1)) 
lincom.20 # This is the "profile coefficient" for the individual described above.
lincom.20.data <- data.frame(lincom.20)
pred.lbound.20 <- lincom.20.data$contrast-(lincom.20.data$SE*1.96)
pred.pr.20 <- 1/(1+exp(-lincom.20.data$contrast))-(1/(1+exp(-0)))
pred.lbound.20 <- (1/(1+exp(-pred.lbound.20))-(1/(1+exp(-0))))
pred.se.20 <- (pred.pr.20-pred.lbound.20)/1.96
pred.pr.20
pred.se.20
# Pred. Pr. change: coef=.049; se=.043

# Create matrix of Least Informed Voters, Incumbent wins
cells <- c(-1,pred.pr.17,pred.se.17,pred.pr.18,pred.se.18,pred.pr.19,pred.se.19,pred.pr.20,pred.se.20)
rnames <- c("-1")
cnames <- c("Info","pronat.in","pronat.in.se","retnat.in","retnat.in.se","propck.in","propck.in.se","retpck.in","retpck.in.se")
least.inf.inc.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                            dimnames=list(rnames, cnames))
least.inf.inc.win

##################################### Merging Incumbent Wins Rows to Dataframe ###########################################
incumbent.wins <- rbind(most.inf.inc.win,high.mod.inc.win,mid.inf.inc.win,low.mod.inc.win,least.inf.inc.win)
incumbent.wins <- data.frame(incumbent.wins)

###################################### Most Informed Voters, Don't Know who wins ###########################################
# Line 68 in Stata file:
lincom.21 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=1,"winpronatl"=0,"infowinpronatl"=0)) 
lincom.21 # This is the "profile coefficient" for the individual described above.
lincom.21.data <- data.frame(lincom.21)
pred.lbound.21 <- lincom.21.data$contrast-(lincom.21.data$SE*1.96)
pred.pr.21 <- 1/(1+exp(-lincom.21.data$contrast))-(1/(1+exp(-0)))
pred.lbound.21 <- (1/(1+exp(-pred.lbound.21))-(1/(1+exp(-0))))
pred.se.21 <- (pred.pr.21-pred.lbound.21)/1.96
pred.pr.21
pred.se.21
# Stata Pred. Pr. change: coef=.161; se=.034

# Line 70 in Stata file:
lincom.22 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=1,"winretronatl"=0,"infowinretronatl"=0)) 
lincom.22 # This is the "profile coefficient" for the individual described above.
lincom.22.data <- data.frame(lincom.22)
pred.lbound.22 <- lincom.22.data$contrast-(lincom.22.data$SE*1.96)
pred.pr.22 <- 1/(1+exp(-lincom.22.data$contrast))-(1/(1+exp(-0)))
pred.lbound.22 <- (1/(1+exp(-pred.lbound.22))-(1/(1+exp(-0))))
pred.se.22 <- (pred.pr.22-pred.lbound.22)/1.96
pred.pr.22
pred.se.22
# Stata Pred. Pr. change: coef=.173; se=.033

# Line 72 in Stata file:
lincom.23 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=1,"winpropck"=0,"infowinpropck"=0)) 
lincom.23 # This is the "profile coefficient" for the individual described above.
lincom.23.data <- data.frame(lincom.23)
pred.lbound.23 <- lincom.23.data$contrast-(lincom.23.data$SE*1.96)
pred.pr.23 <- 1/(1+exp(-lincom.23.data$contrast))-(1/(1+exp(-0)))
pred.lbound.23 <- (1/(1+exp(-pred.lbound.23))-(1/(1+exp(-0))))
pred.se.23 <- (pred.pr.23-pred.lbound.23)/1.96
pred.pr.23
pred.se.23
# Stata Pred. Pr. change: coef=.097; se=.019

# Line 74 in Stata file:
lincom.24 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=1,"winretropck"=0,"infowinretropck"=0)) 
lincom.24 # This is the "profile coefficient" for the individual described above.
lincom.24.data <- data.frame(lincom.24)
pred.lbound.24 <- lincom.24.data$contrast-(lincom.24.data$SE*1.96)
pred.pr.24 <- 1/(1+exp(-lincom.24.data$contrast))-(1/(1+exp(-0)))
pred.lbound.24 <- (1/(1+exp(-pred.lbound.24))-(1/(1+exp(-0))))
pred.se.24 <- (pred.pr.24-pred.lbound.24)/1.96
pred.pr.24
pred.se.24
# Stata Pred. Pr. change: coef=.025; se=.031

# Create matrix of Most Informed Voters, Don't Know who wins
cells <- c(1,pred.pr.21,pred.se.21,pred.pr.22,pred.se.22,pred.pr.23,pred.se.23,pred.pr.24,pred.se.24)
rnames <- c("1")
cnames <- c("Info","pronat.dk","pronat.dk.se","retnat.dk","retnat.dk.se","propck.dk","propck.dk.se","retpck.dk","retpck.dk.se")
most.inf.dk.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
most.inf.dk.win

################################## High Moderate Informed Voters, Don't Know who wins ######################################
# Line 79 in Stata file:
lincom.25 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=.5,"winpronatl"=0,"infowinpronatl"=0)) 
lincom.25 # This is the "profile coefficient" for the individual described above.
lincom.25.data <- data.frame(lincom.25)
pred.lbound.25 <- lincom.25.data$contrast-(lincom.25.data$SE*1.96)
pred.pr.25 <- 1/(1+exp(-lincom.25.data$contrast))-(1/(1+exp(-0)))
pred.lbound.25 <- (1/(1+exp(-pred.lbound.25))-(1/(1+exp(-0))))
pred.se.25 <- (pred.pr.25-pred.lbound.25)/1.96
pred.pr.25
pred.se.25
# Stata Pred. Pr. change: coef=.108; se=.022

# Line 81 in Stata file:
lincom.26 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=.5,"winretronatl"=0,"infowinretronatl"=0)) 
lincom.26 # This is the "profile coefficient" for the individual described above.
lincom.26.data <- data.frame(lincom.26)
pred.lbound.26 <- lincom.26.data$contrast-(lincom.26.data$SE*1.96)
pred.pr.26 <- 1/(1+exp(-lincom.26.data$contrast))-(1/(1+exp(-0)))
pred.lbound.26 <- (1/(1+exp(-pred.lbound.26))-(1/(1+exp(-0))))
pred.se.26 <- (pred.pr.26-pred.lbound.26)/1.96
pred.pr.26
pred.se.26
# Stata Pred. Pr. change: coef=.153; se=.020

# Line 83 in Stata file:
lincom.27 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=.5,"winpropck"=0,"infowinpropck"=0)) 
lincom.27 # This is the "profile coefficient" for the individual described above.
lincom.27.data <- data.frame(lincom.27)
pred.lbound.27 <- lincom.27.data$contrast-(lincom.27.data$SE*1.96)
pred.pr.27 <- 1/(1+exp(-lincom.27.data$contrast))-(1/(1+exp(-0)))
pred.lbound.27 <- (1/(1+exp(-pred.lbound.27))-(1/(1+exp(-0))))
pred.se.27 <- (pred.pr.27-pred.lbound.27)/1.96
pred.pr.27
pred.se.27
# Stata Pred. Pr. change: coef=.057; se=.034

# Line 85 in Stata file:
lincom.28 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=.5,"winretropck"=0,"infowinretropck"=0)) 
lincom.28 # This is the "profile coefficient" for the individual described above.
lincom.28.data <- data.frame(lincom.28)
pred.lbound.28 <- lincom.28.data$contrast-(lincom.28.data$SE*1.96)
pred.pr.28 <- 1/(1+exp(-lincom.28.data$contrast))-(1/(1+exp(-0)))
pred.lbound.28 <- (1/(1+exp(-pred.lbound.28))-(1/(1+exp(-0))))
pred.se.28 <- (pred.pr.28-pred.lbound.28)/1.96
pred.pr.28
pred.se.28
# Stata Pred. Pr. change: coef=.046; se=.017

# Create matrix of Most Informed Voters, Incumbent wins
cells <- c(.5,pred.pr.25,pred.se.25,pred.pr.26,pred.se.26,pred.pr.27,pred.se.27,pred.pr.28,pred.se.28)
rnames <- c(".5")
cnames <- c("Info","pronat.dk","pronat.dk.se","retnat.dk","retnat.dk.se","propck.dk","propck.dk.se","retpck.dk","retpck.dk.se")
high.mod.dk.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
high.mod.dk.win

################################# Moderately Informed Voters, Don't Know who wins ###########################################
# Line 90 in Stata file:
lincom.29 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=0,"winpronatl"=0,"infowinpronatl"=0)) 
lincom.29 # This is the "profile coefficient" for the individual described above.
lincom.29.data <- data.frame(lincom.29)
pred.lbound.29 <- lincom.29.data$contrast-(lincom.29.data$SE*1.96)
pred.pr.29 <- 1/(1+exp(-lincom.29.data$contrast))-(1/(1+exp(-0)))
pred.lbound.29 <- (1/(1+exp(-pred.lbound.29))-(1/(1+exp(-0))))
pred.se.29 <- (pred.pr.29-pred.lbound.29)/1.96
pred.pr.29
pred.se.29
# Stata Pred. Pr. change: coef=.053; se=.016

# Line 92 in Stata file:
lincom.30 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=0,"winretronatl"=0,"infowinretronatl"=0)) 
lincom.30 # This is the "profile coefficient" for the individual described above.
lincom.30.data <- data.frame(lincom.30)
pred.lbound.30 <- lincom.30.data$contrast-(lincom.30.data$SE*1.96)
pred.pr.30 <- 1/(1+exp(-lincom.30.data$contrast))-(1/(1+exp(-0)))
pred.lbound.30 <- (1/(1+exp(-pred.lbound.30))-(1/(1+exp(-0))))
pred.se.30 <- (pred.pr.30-pred.lbound.30)/1.96
pred.pr.30
pred.se.30
# Stata Pred. Pr. change: coef=.134; se=.050

# Line 94 in Stata file:
lincom.31 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=0,"winpropck"=0,"infowinpropck"=0)) 
lincom.31 # This is the "profile coefficient" for the individual described above.
lincom.31.data <- data.frame(lincom.31)
pred.lbound.31 <- lincom.31.data$contrast-(lincom.31.data$SE*1.96)
pred.pr.31 <- 1/(1+exp(-lincom.31.data$contrast))-(1/(1+exp(-0)))
pred.lbound.31 <- (1/(1+exp(-pred.lbound.31))-(1/(1+exp(-0))))
pred.se.31 <- (pred.pr.31-pred.lbound.31)/1.96
pred.pr.31
pred.se.31
# Stata Pred. Pr. change: coef=.017; se=.054

# Line 96 in Stata file:
lincom.32 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=0,"winretropck"=0,"infowinretropck"=0)) 
lincom.32 # This is the "profile coefficient" for the individual described above.
lincom.32.data <- data.frame(lincom.32)
pred.lbound.32 <- lincom.32.data$contrast-(lincom.32.data$SE*1.96)
pred.pr.32 <- 1/(1+exp(-lincom.32.data$contrast))-(1/(1+exp(-0)))
pred.lbound.32 <- (1/(1+exp(-pred.lbound.32))-(1/(1+exp(-0))))
pred.se.32 <- (pred.pr.32-pred.lbound.32)/1.96
pred.pr.32
pred.se.32
# Stata Pred. Pr. change: coef=.066; se=.018

# Create matrix of Middle Informed Voters, Incumbent wins
cells <- c(0,pred.pr.29,pred.se.29,pred.pr.30,pred.se.30,pred.pr.31,pred.se.31,pred.pr.32,pred.se.32)
rnames <- c("0")
cnames <- c("Info","pronat.dk","pronat.dk.se","retnat.dk","retnat.dk.se","propck.dk","propck.dk.se","retpck.dk","retpck.dk.se")
mod.inf.dk.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                         dimnames=list(rnames, cnames))
mod.inf.dk.win

################################# Low-Moderately Informed Voters, Don't Know who wins ######################################
# Line 101 in Stata file:
lincom.33 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-.5,"winpronatl"=0,"infowinpronatl"=0)) 
lincom.33 # This is the "profile coefficient" for the individual described above.
lincom.33.data <- data.frame(lincom.33)
pred.lbound.33 <- lincom.33.data$contrast-(lincom.33.data$SE*1.96)
pred.pr.33 <- 1/(1+exp(-lincom.33.data$contrast))-(1/(1+exp(-0)))
pred.lbound.33 <- (1/(1+exp(-pred.lbound.33))-(1/(1+exp(-0))))
pred.se.33 <- (pred.pr.33-pred.lbound.33)/1.96
pred.pr.33
pred.se.33
# Stata Pred. Pr. change: coef=-.003; se=.022

# Line 103 in Stata file:
lincom.34 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-.5,"winretronatl"=0,"infowinretronatl"=0)) 
lincom.34 # This is the "profile coefficient" for the individual described above.
lincom.34.data <- data.frame(lincom.34)
pred.lbound.34 <- lincom.34.data$contrast-(lincom.34.data$SE*1.96)
pred.pr.34 <- 1/(1+exp(-lincom.34.data$contrast))-(1/(1+exp(-0)))
pred.lbound.34 <- (1/(1+exp(-pred.lbound.34))-(1/(1+exp(-0))))
pred.se.34 <- (pred.pr.34-pred.lbound.34)/1.96
pred.pr.34
pred.se.34
# Stata Pred. Pr. change: coef=.113; se=.086

# Line 105 in Stata file:
lincom.35 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-.5,"winpropck"=0,"infowinpropck"=0)) 
lincom.35 # This is the "profile coefficient" for the individual described above.
lincom.35.data <- data.frame(lincom.35)
pred.lbound.35 <- lincom.35.data$contrast-(lincom.35.data$SE*1.96)
pred.pr.35 <- 1/(1+exp(-lincom.35.data$contrast))-(1/(1+exp(-0)))
pred.lbound.35 <- (1/(1+exp(-pred.lbound.35))-(1/(1+exp(-0))))
pred.se.35 <- (pred.pr.35-pred.lbound.35)/1.96
pred.pr.35
pred.se.35
# Stata Pred. Pr. change: coef=-.024; se=.071

# Line 107 in Stata file:
lincom.36 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-.5,"winretropck"=0,"infowinretropck"=0)) 
lincom.36 # This is the "profile coefficient" for the individual described above.
lincom.36.data <- data.frame(lincom.36)
pred.lbound.36 <- lincom.36.data$contrast-(lincom.36.data$SE*1.96)
pred.pr.36 <- 1/(1+exp(-lincom.36.data$contrast))-(1/(1+exp(-0)))
pred.lbound.36 <- (1/(1+exp(-pred.lbound.36))-(1/(1+exp(-0))))
pred.se.36 <- (pred.pr.36-pred.lbound.36)/1.96
pred.pr.36
pred.se.36
# Stata Pred. Pr. change: coef=.087; se=.033

# Create matrix of Low-Moderately Informed Voters, Incumbent wins
cells <- c(-.5,pred.pr.33,pred.se.33,pred.pr.34,pred.se.34,pred.pr.35,pred.se.35,pred.pr.36,pred.se.36)
rnames <- c("-.5")
cnames <- c("Info","pronat.dk","pronat.dk.se","retnat.dk","retnat.dk.se","propck.dk","propck.dk.se","retpck.dk","retpck.dk.se")
low.mod.dk.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                         dimnames=list(rnames, cnames))
low.mod.dk.win

###################################### Least Informed Voters, Don't Know who wins
###########################################
# Line 112 in Stata file:
lincom.37 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-1,"winpronatl"=0,"infowinpronatl"=0)) 
lincom.37 # This is the "profile coefficient" for the individual described above.
lincom.37.data <- data.frame(lincom.37)
pred.lbound.37 <- lincom.37.data$contrast-(lincom.37.data$SE*1.96)
pred.pr.37 <- 1/(1+exp(-lincom.37.data$contrast))-(1/(1+exp(-0)))
pred.lbound.37 <- (1/(1+exp(-pred.lbound.37))-(1/(1+exp(-0))))
pred.se.37 <- (pred.pr.37-pred.lbound.37)/1.96
pred.pr.37
pred.se.37
# Stata Pred. Pr. change: coef=-.060; se=.034

# Line 114 in Stata file:
lincom.38 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-1,"winretronatl"=0,"infowinretronatl"=0)) 
lincom.38 # This is the "profile coefficient" for the individual described above.
lincom.38.data <- data.frame(lincom.38)
pred.lbound.38 <- lincom.38.data$contrast-(lincom.38.data$SE*1.96)
pred.pr.38 <- 1/(1+exp(-lincom.38.data$contrast))-(1/(1+exp(-0)))
pred.lbound.38 <- (1/(1+exp(-pred.lbound.38))-(1/(1+exp(-0))))
pred.se.38 <- (pred.pr.38-pred.lbound.38)/1.96
pred.pr.38
pred.se.38
# Stata Pred. Pr. change: coef=.093; se=.122

# Line 116 in Stata file:
lincom.39 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-1,"winpropck"=0,"infowinpropck"=0)) 
lincom.39 # This is the "profile coefficient" for the individual described above.
lincom.39.data <- data.frame(lincom.39)
pred.lbound.39 <- lincom.39.data$contrast-(lincom.39.data$SE*1.96)
pred.pr.39 <- 1/(1+exp(-lincom.39.data$contrast))-(1/(1+exp(-0)))
pred.lbound.39 <- (1/(1+exp(-pred.lbound.39))-(1/(1+exp(-0))))
pred.se.39 <- (pred.pr.39-pred.lbound.39)/1.96
pred.pr.39
pred.se.39
# Stata Pred. Pr. change: coef=-.064; se=.086

# Line 118 in Stata file:
lincom.40 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-1,"winretropck"=0,"infowinretropck"=0)) 
lincom.40 # This is the "profile coefficient" for the individual described above.
lincom.40.data <- data.frame(lincom.40)
pred.lbound.40 <- lincom.40.data$contrast-(lincom.40.data$SE*1.96)
pred.pr.40 <- 1/(1+exp(-lincom.40.data$contrast))-(1/(1+exp(-0)))
pred.lbound.40 <- (1/(1+exp(-pred.lbound.40))-(1/(1+exp(-0))))
pred.se.40 <- (pred.pr.40-pred.lbound.40)/1.96
pred.pr.40
pred.se.40
# Stata Pred. Pr. change: coef=.107; se=.051

# Create matrix of Least Informed Voters, Incumbent wins
cells <- c(-1,pred.pr.37,pred.se.37,pred.pr.38,pred.se.38,pred.pr.39,pred.se.39,pred.pr.40,pred.se.40)
rnames <- c("-1")
cnames <- c("Info","pronat.dk","pronat.dk.se","retnat.dk","retnat.dk.se","propck.dk","propck.dk.se","retpck.dk","retpck.dk.se")
least.inf.dk.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                           dimnames=list(rnames, cnames))
least.inf.dk.win

################################## Merging Don't Know Who Wins Rows to Dataframe ###########################################
dk.wins <- rbind(most.inf.dk.win,high.mod.dk.win,mod.inf.dk.win,low.mod.dk.win,least.inf.dk.win)
dk.wins <- data.frame(dk.wins)

########################################### Most Informed Voters, Challenger wins ###########################################
# Line 123 in Stata file:
lincom.41 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=1,"winpronatl"=-1,"infowinpronatl"=-1)) 
lincom.41 # This is the "profile coefficient" for the individual described above.
lincom.41.data <- data.frame(lincom.41)
pred.lbound.41 <- lincom.41.data$contrast-(lincom.41.data$SE*1.96)
pred.pr.41 <- 1/(1+exp(-lincom.41.data$contrast))-(1/(1+exp(-0)))
pred.lbound.41 <- (1/(1+exp(-pred.lbound.41))-(1/(1+exp(-0))))
pred.se.41 <- (pred.pr.41-pred.lbound.41)/1.96
pred.pr.41
pred.se.41
# Stata Pred. Pr. change: coef=.111; se=.051

# Line 125 in Stata file:
lincom.42 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=1,"winretronatl"=-1,"infowinretronatl"=-1)) 
lincom.42 # This is the "profile coefficient" for the individual described above.
lincom.42.data <- data.frame(lincom.42)
pred.lbound.42 <- lincom.42.data$contrast-(lincom.42.data$SE*1.96)
pred.pr.42 <- 1/(1+exp(-lincom.42.data$contrast))-(1/(1+exp(-0)))
pred.lbound.42 <- (1/(1+exp(-pred.lbound.42))-(1/(1+exp(-0))))
pred.se.42 <- (pred.pr.42-pred.lbound.42)/1.96
pred.pr.42
pred.se.42
# Stata Pred. Pr. change: coef=.199; se=.056

# Line 127 in Stata file:
lincom.43 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=1,"winpropck"=-1,"infowinpropck"=-1)) 
lincom.43 # This is the "profile coefficient" for the individual described above.
lincom.43.data <- data.frame(lincom.43)
pred.lbound.43 <- lincom.43.data$contrast-(lincom.43.data$SE*1.96)
pred.pr.43 <- 1/(1+exp(-lincom.43.data$contrast))-(1/(1+exp(-0)))
pred.lbound.43 <- (1/(1+exp(-pred.lbound.43))-(1/(1+exp(-0))))
pred.se.43 <- (pred.pr.43-pred.lbound.43)/1.96
pred.pr.43
pred.se.43   
# Stata Pred. Pr. change: coef=.087; se=.031

# Line 129 in Stata file:
lincom.44 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=1,"winretropck"=-1,"infowinretropck"=-1)) 
lincom.44 # This is the "profile coefficient" for the individual described above.
lincom.44.data <- data.frame(lincom.44)
pred.lbound.44 <- lincom.44.data$contrast-(lincom.44.data$SE*1.96)
pred.pr.44 <- 1/(1+exp(-lincom.44.data$contrast))-(1/(1+exp(-0)))
pred.lbound.44 <- (1/(1+exp(-pred.lbound.44))-(1/(1+exp(-0))))
pred.se.44 <- (pred.pr.44-pred.lbound.44)/1.96
pred.pr.44
pred.se.44
# Stata Pred. Pr. change: coef=.016; se=.043

# Create matrix of Most Informed Voters, Challenger wins
cells <- c(1,pred.pr.41,pred.se.41,pred.pr.42,pred.se.42,pred.pr.43,pred.se.43,pred.pr.44,pred.se.44)
rnames <- c("1")
cnames <- c("Info","pronat.ch","pronat.ch.se","retnat.ch","retnat.ch.se","propck.ch","propck.ch.se","retpck.ch","retpck.ch.se")
most.inf.cha.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                           dimnames=list(rnames, cnames))
most.inf.cha.win

###################################### High Moderate Informed Voters, Challenger wins ######################################
# Line 134 in Stata file:
lincom.45 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=.5,"winpronatl"=-1,"infowinpronatl"=-.5)) 
lincom.45 # This is the "profile coefficient" for the individual described above.
lincom.45.data <- data.frame(lincom.45)
pred.lbound.45 <- lincom.45.data$contrast-(lincom.45.data$SE*1.96)
pred.pr.45 <- 1/(1+exp(-lincom.45.data$contrast))-(1/(1+exp(-0)))
pred.lbound.45 <- (1/(1+exp(-pred.lbound.45))-(1/(1+exp(-0))))
pred.se.45 <- (pred.pr.45-pred.lbound.45)/1.96
pred.pr.45
pred.se.45
# Stata Pred. Pr. change: coef=.070; se=.033

# Line 136 in Stata file:
lincom.46 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=.5,"winretronatl"=-1,"infowinretronatl"=-.5)) 
lincom.46 # This is the "profile coefficient" for the individual described above.
lincom.46.data <- data.frame(lincom.46)
pred.lbound.46 <- lincom.46.data$contrast-(lincom.46.data$SE*1.96)
pred.pr.46 <- 1/(1+exp(-lincom.46.data$contrast))-(1/(1+exp(-0)))
pred.lbound.46 <- (1/(1+exp(-pred.lbound.46))-(1/(1+exp(-0))))
pred.se.46 <- (pred.pr.46-pred.lbound.46)/1.96
pred.pr.46
pred.se.46
# Stata Pred. Pr. change: coef=.167; se=.033

# Line 138 in Stata file:
lincom.47 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=.5,"winpropck"=-1,"infowinpropck"=-.5)) 
lincom.47 # This is the "profile coefficient" for the individual described above.
lincom.47.data <- data.frame(lincom.47)
pred.lbound.47 <- lincom.47.data$contrast-(lincom.47.data$SE*1.96)
pred.pr.47 <- 1/(1+exp(-lincom.47.data$contrast))-(1/(1+exp(-0)))
pred.lbound.47 <- (1/(1+exp(-pred.lbound.47))-(1/(1+exp(-0))))
pred.se.47 <- (pred.pr.47-pred.lbound.47)/1.96
pred.pr.47
pred.se.47
# Stata Pred. Pr. change: coef=.027; se=.064

# Line 140 in Stata file:
lincom.48 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=.5,"winretropck"=-1,"infowinretropck"=-.5)) 
lincom.48 # This is the "profile coefficient" for the individual described above.
lincom.48.data <- data.frame(lincom.48)
pred.lbound.48 <- lincom.48.data$contrast-(lincom.48.data$SE*1.96)
pred.pr.48 <- 1/(1+exp(-lincom.48.data$contrast))-(1/(1+exp(-0)))
pred.lbound.48 <- (1/(1+exp(-pred.lbound.48))-(1/(1+exp(-0))))
pred.se.48 <- (pred.pr.48-pred.lbound.48)/1.96
pred.pr.48
pred.se.48
# Stata Pred. Pr. change: coef=.054; se=.025

# Create matrix of Most Informed Voters, Incumbent wins
cells <- c(.5,pred.pr.45,pred.se.45,pred.pr.46,pred.se.46,pred.pr.47,pred.se.47,pred.pr.48,pred.se.48)
rnames <- c(".5")
cnames <- c("Info","pronat.ch","pronat.ch.se","retnat.in","retnat.ch.se","propck.ch","propck.ch.se","retpck.ch","retpck.ch.se")
high.mod.cha.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                           dimnames=list(rnames, cnames))
high.mod.cha.win

########################################### Middle Info Voters, Challenger wins ###########################################
# Line 145 in Stata file:
lincom.49 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=0,"winpronatl"=-1,"infowinpronatl"=0)) 
lincom.49 # This is the "profile coefficient" for the individual described above.
lincom.49.data <- data.frame(lincom.49)
pred.lbound.49 <- lincom.49.data$contrast-(lincom.49.data$SE*1.96)
pred.pr.49 <- 1/(1+exp(-lincom.49.data$contrast))-(1/(1+exp(-0)))
pred.lbound.49 <- (1/(1+exp(-pred.lbound.49))-(1/(1+exp(-0))))
pred.se.49 <- (pred.pr.49-pred.lbound.49)/1.96
pred.pr.49
pred.se.49
# Stata Pred. Pr. change: coef=.029; se=.028

# Line 147 in Stata file:
lincom.50 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=0,"winretronatl"=-1,"infowinretronatl"=0)) 
lincom.50 # This is the "profile coefficient" for the individual described above.
lincom.50.data <- data.frame(lincom.50)
pred.lbound.50 <- lincom.50.data$contrast-(lincom.50.data$SE*1.96)
pred.pr.50 <- 1/(1+exp(-lincom.50.data$contrast))-(1/(1+exp(-0)))
pred.lbound.50 <- (1/(1+exp(-pred.lbound.50))-(1/(1+exp(-0))))
pred.se.50 <- (pred.pr.50-pred.lbound.50)/1.96
pred.pr.50
pred.se.50
# Stata Pred. Pr. change: coef=.134; se=.081

# Line 149 in Stata file:
lincom.51 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=0,"winpropck"=-1,"infowinpropck"=0)) 
lincom.51 # This is the "profile coefficient" for the individual described above.
lincom.51.data <- data.frame(lincom.51)
pred.lbound.51 <- lincom.51.data$contrast-(lincom.51.data$SE*1.96)
pred.pr.51 <- 1/(1+exp(-lincom.51.data$contrast))-(1/(1+exp(-0)))
pred.lbound.51 <- (1/(1+exp(-pred.lbound.51))-(1/(1+exp(-0))))
pred.se.51 <- (pred.pr.51-pred.lbound.51)/1.96
pred.pr.51
pred.se.51
# Stata Pred. Pr. change: coef=-.034; se=.093

# Line 151 in Stata file:
lincom.52 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=0,"winretropck"=-1,"infowinretropck"=0)) 
lincom.52 # This is the "profile coefficient" for the individual described above.
lincom.52.data <- data.frame(lincom.52)
pred.lbound.52 <- lincom.52.data$contrast-(lincom.52.data$SE*1.96)
pred.pr.52 <- 1/(1+exp(-lincom.52.data$contrast))-(1/(1+exp(-0)))
pred.lbound.52 <- (1/(1+exp(-pred.lbound.52))-(1/(1+exp(-0))))
pred.se.52 <- (pred.pr.52-pred.lbound.52)/1.96
pred.pr.52
pred.se.52
# Stata Pred. Pr. change: coef=.091; se=.036

# Create matrix of Middle Informed Voters, Incumbent wins
cells <- c(0,pred.pr.49,pred.se.49,pred.pr.50,pred.se.50,pred.pr.51,pred.se.51,pred.pr.52,pred.se.52)
rnames <- c("0")
cnames <- c("Info","pronat.ch","pronat.ch.se","retnat.in","retnat.ch.se","propck.ch","propck.ch.se","retpck.ch","retpck.ch.se")
mid.inf.cha.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
mid.inf.cha.win

################################# Low-Moderately Informed Voters, Challenger wins ######################################
# Line 156 in Stata file:
lincom.53 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-.5,"winpronatl"=-1,"infowinpronatl"=.5)) 
lincom.53 # This is the "profile coefficient" for the individual described above.
lincom.53.data <- data.frame(lincom.53)
pred.lbound.53 <- lincom.53.data$contrast-(lincom.53.data$SE*1.96)
pred.pr.53 <- 1/(1+exp(-lincom.53.data$contrast))-(1/(1+exp(-0)))
pred.lbound.53 <- (1/(1+exp(-pred.lbound.53))-(1/(1+exp(-0))))
pred.se.53 <- (pred.pr.53-pred.lbound.53)/1.96
pred.pr.53
pred.se.53
# Stata Pred. Pr. change: coef=-.014; se=.041

# Line 158 in Stata file:
lincom.54 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-.5,"winretronatl"=-1,"infowinretronatl"=.5)) 
lincom.54 # This is the "profile coefficient" for the individual described above.
lincom.54.data <- data.frame(lincom.54)
pred.lbound.54 <- lincom.54.data$contrast-(lincom.54.data$SE*1.96)
pred.pr.54 <- 1/(1+exp(-lincom.54.data$contrast))-(1/(1+exp(-0)))
pred.lbound.54 <- (1/(1+exp(-pred.lbound.54))-(1/(1+exp(-0))))
pred.se.54 <- (pred.pr.54-pred.lbound.54)/1.96
pred.pr.54
pred.se.54
# Stata Pred. Pr. change: coef=.100; se=.139

# Line 160 in Stata file:
lincom.55 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-.5,"winpropck"=-1,"infowinpropck"=.5)) 
lincom.55 # This is the "profile coefficient" for the individual described above.
lincom.55.data <- data.frame(lincom.55)
pred.lbound.55 <- lincom.55.data$contrast-(lincom.55.data$SE*1.96)
pred.pr.55 <- 1/(1+exp(-lincom.55.data$contrast))-(1/(1+exp(-0)))
pred.lbound.55 <- (1/(1+exp(-pred.lbound.55))-(1/(1+exp(-0))))
pred.se.55 <- (pred.pr.55-pred.lbound.55)/1.96
pred.pr.55
pred.se.55
# Stata Pred. Pr. change: coef=--.093; se=.112

# Line 162 in Stata file:
lincom.56 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-.5,"winretropck"=-1,"infowinretropck"=.5)) 
lincom.56 # This is the "profile coefficient" for the individual described above.
lincom.56.data <- data.frame(lincom.56)
pred.lbound.56 <- lincom.56.data$contrast-(lincom.56.data$SE*1.96)
pred.pr.56 <- 1/(1+exp(-lincom.56.data$contrast))-(1/(1+exp(-0)))
pred.lbound.56 <- (1/(1+exp(-pred.lbound.56))-(1/(1+exp(-0))))
pred.se.56 <- (pred.pr.56-pred.lbound.56)/1.96
pred.pr.56
pred.se.56
# Stata Pred. Pr. change: coef=.127; se=.054

# Create matrix of Low-Moderately Informed Voters, Incumbent wins
cells <- c(-.5,pred.pr.53,pred.se.53,pred.pr.54,pred.se.54,pred.pr.55,pred.se.55,pred.pr.56,pred.se.56)
rnames <- c("-.5")
cnames <- c("Info","pronat.ch","pronat.ch.se","retnat.in","retnat.ch.se","propck.ch","propck.ch.se","retpck.ch","retpck.ch.se")
low.mod.cha.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                          dimnames=list(rnames, cnames))
low.mod.cha.win

###################################### Least Informed Voters, Don't Know who wins ###########################################
# Line 167 in Stata file:
lincom.57 <- svycontrast(logit.mod.2,c("ProspNatl"=1, "infopronatl"=-1,"winpronatl"=-1,"infowinpronatl"=1)) 
lincom.57 # This is the "profile coefficient" for the individual described above.
lincom.57.data <- data.frame(lincom.57)
pred.lbound.57 <- lincom.57.data$contrast-(lincom.57.data$SE*1.96)
pred.pr.57 <- 1/(1+exp(-lincom.57.data$contrast))-(1/(1+exp(-0)))
pred.lbound.57 <- (1/(1+exp(-pred.lbound.57))-(1/(1+exp(-0))))
pred.se.57 <- (pred.pr.57-pred.lbound.57)/1.96
pred.pr.57
pred.se.57
# Stata Pred. Pr. change: coef=-.056; se=.059

# Line 169 in Stata file:
lincom.58 <- svycontrast(logit.mod.2,c("RetroNatl"=1, "inforetronatl"=-1,"winretronatl"=-1,"infowinretronatl"=1)) 
lincom.58 # This is the "profile coefficient" for the individual described above.
lincom.58.data <- data.frame(lincom.58)
pred.lbound.58 <- lincom.58.data$contrast-(lincom.58.data$SE*1.96)
pred.pr.58 <- 1/(1+exp(-lincom.58.data$contrast))-(1/(1+exp(-0)))
pred.lbound.58 <- (1/(1+exp(-pred.lbound.58))-(1/(1+exp(-0))))
pred.se.58 <- (pred.pr.58-pred.lbound.58)/1.96
pred.pr.58
pred.se.58
# Stata Pred. Pr. change: coef=.064; se=.183

# Line 171 in Stata file:
lincom.59 <- svycontrast(logit.mod.2,c("ProspPocket"=1, "infopropck"=-1,"winpropck"=-1,"infowinpropck"=1)) 
lincom.59 # This is the "profile coefficient" for the individual described above.
lincom.59.data <- data.frame(lincom.59)
pred.lbound.59 <- lincom.59.data$contrast-(lincom.59.data$SE*1.96)
pred.pr.59 <- 1/(1+exp(-lincom.59.data$contrast))-(1/(1+exp(-0)))
pred.lbound.59 <- (1/(1+exp(-pred.lbound.59))-(1/(1+exp(-0))))
pred.se.59 <- (pred.pr.59-pred.lbound.59)/1.96
pred.pr.59
pred.se.59
# Stata Pred. Pr. change: coef=-.150; se=.118

# Line 173 in Stata file:
lincom.60 <- svycontrast(logit.mod.2,c("RetroPocket"=1, "inforetropck"=-1,"winretropck"=-1,"infowinretropck"=1)) 
lincom.60 # This is the "profile coefficient" for the individual described above.
lincom.60.data <- data.frame(lincom.60)
pred.lbound.60 <- lincom.60.data$contrast-(lincom.60.data$SE*1.96)
pred.pr.60 <- 1/(1+exp(-lincom.60.data$contrast))-(1/(1+exp(-0)))
pred.lbound.60 <- (1/(1+exp(-pred.lbound.60))-(1/(1+exp(-0))))
pred.se.60 <- (pred.pr.60-pred.lbound.60)/1.96
pred.pr.60
pred.se.60
# Stata Pred. Pr. change: coef=.162; se=.076

# Create matrix of Least Informed Voters, Incumbent wins
cells <- c(-1,pred.pr.57,pred.se.57,pred.pr.58,pred.se.58,pred.pr.59,pred.se.59,pred.pr.60,pred.se.60)
rnames <- c("-1")
cnames <- c("Info","pronat.ch","pronat.ch.se","retnat.in","retnat.ch.se","propck.ch","propck.ch.se","retpck.ch","retpck.ch.se")
least.inf.cha.win <- matrix(cells, nrow=1, ncol=9, byrow=TRUE,
                            dimnames=list(rnames, cnames))
least.inf.cha.win

##################################### Merging Challenger Wins Rows to Dataframe ###########################################
challenger.wins <- rbind(most.inf.cha.win,high.mod.cha.win,mid.inf.cha.win,low.mod.cha.win,least.inf.cha.win)
challenger.wins <- data.frame(challenger.wins)

############################ Merging Incumbent, DK, Challengers Columns to Dataframe #######################################
matrix <- cbind(incumbent.wins,dk.wins[-1],challenger.wins[-1])

################################################# Write to .csv ############################################################
write.csv(matrix, "/Users/deanlacy/Dropbox/MLEshared/MLE/economicvoting/matrix2.csv", row.names=FALSE, na="")


################################################# Create Figure 2 ############################################################
rm(list=ls())
pr3way08.norats<-read.csv("/Users/deanlacy/Dropbox/MLEshared/MLE/economicvoting/matrix2.csv")
attach(pr3way08.norats)
pdf("/Users/deanlacy/Dropbox/MLEshared/MLE/economicvoting/figure3new.pdf",width=9,height=8)

par(mfrow=c(3,4))
oma=c(2,0,2,0)
mtext="The Effects of Economic Evaluations on Vote Choice Vary By Voter's Information Level"
outer=TRUE

mfg=c(1,1,3,4)
plot(Info,retnat.in,main="Retrospective National",
     axes=FALSE,
     ylab="Incumbent will Win",
     xlab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retnat.in-1.96*retnat.in.se),lty=2)
lines(Info,(retnat.in+1.96*retnat.in.se),lty=2)
abline(h=0,col="grey")

mfg=c(1,2,3,4)
plot(Info,retpck.in,main="Retrospective Pocketbook",
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retpck.in-1.96*retpck.in.se), lty=2)
lines(Info,(retpck.in+1.96*retpck.in.se),lty=2)
abline(h=0,col="grey")

mfg=c(1,3,3,4)
plot(Info,pronat.in,main="Prospective National",
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(pronat.in-1.96*pronat.in.se), lty=2)
lines(Info,(pronat.in+1.96*pronat.in.se),lty=2)
abline(h=0,col="grey")

mfg=c(1,4,3,4)
plot(Info,propck.in,main="Prospective Pocketbook",
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(propck.in-1.96*propck.in.se), lty=2)
lines(Info,(propck.in+1.96*propck.in.se),lty=2)
abline(h=0,col="grey")

mfg=c(2,1,3,4)
plot(Info,retnat.dk,main=" ",
     axes=FALSE,
     ylab="Don't Know Who Will Win",
     xlab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retnat.dk-1.96*retnat.dk.se), lty=2)
lines(Info,(retnat.dk+1.96*retnat.dk.se),lty=2)
abline(h=0,col="grey")

mfg=c(2,2,3,4)
plot(Info,retpck.dk,main="",
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retpck.dk-1.96*retpck.dk.se), lty=2)
lines(Info,(retpck.dk+1.96*retpck.dk.se),lty=2)
abline(h=0,col="grey")

mfg=c(2,3,3,4)
plot(Info,pronat.dk,
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(pronat.dk-1.96*pronat.dk.se), lty=2)
lines(Info,(pronat.dk+1.96*pronat.dk.se),lty=2)
abline(h=0,col="grey")

mfg=c(2,4,3,4)
plot(Info,propck.dk,
     axes=FALSE,
     xlab=" ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(propck.dk-1.96*propck.dk.se), lty=2)
lines(Info,(propck.dk+1.96*propck.dk.se),lty=2)
abline(h=0,col="grey")

mfg=c(3,1,3,4)
plot(Info,retnat.ch,main=" ",
     axes=FALSE,
     ylab="Challenger Will Win",
     xlab="Information Level",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retnat.ch-1.96*retnat.ch.se),lty=2)
lines(Info,(retnat.ch+1.96*retnat.ch.se),lty=2)
abline(h=0,col="grey")

mfg=c(3,2,3,4)
plot(Info,retpck.ch,main=" ",
     axes=FALSE,
     xlab="Information Level ",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(retpck.ch-1.96*retpck.ch.se), lty=2)
lines(Info,(retpck.ch+1.96*retpck.ch.se),lty=2)
abline(h=0,col="grey")


mfg=c(3,3,3,4)
plot(Info,pronat.ch,main=" ",
     axes=FALSE,
     xlab="Information Level",
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(pronat.ch-1.96*pronat.ch.se), lty=2)
lines(Info,(pronat.ch+1.96*pronat.ch.se),lty=2)
abline(h=0,col="grey") 

mfg=c(3,4,3,4)
plot(Info,propck.ch, main=" ",
     axes=FALSE,
     xlab="Information Level", 
     ylab=" ",
     xlim=c(-1,1), ylim=c(-.4,.4), type="l")
axis(1,at=-1:1,lab=c("Low","Medium","High"))
axis(2, c(-.4,-.2,0,.2,.4))
lines(Info,(propck.ch-1.96*propck.ch.se), lty=2)
lines(Info,(propck.ch+1.96*propck.ch.se),lty=2)
abline(h=0,col="grey")

dev.off()