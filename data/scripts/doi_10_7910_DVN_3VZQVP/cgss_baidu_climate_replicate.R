## Wednesday, 21.August.2024
## this R code replicate the main results presented in 
## "Do Extreme Weather Events Increase Public Concern, Knowledge, and Attention to Climate Change in China?"


##############################
library(stargazer)
library(xtable)
library(lfe)
library(MASS)
dd <- c("D:/cao back/desktop/USB D backup/USB backup/new ideas/diaster_climate/data/") 
setwd(dd)


############## 1: Replicating the CGSS results ###############
###########################################################
cgss<-read.csv("cgss_replicate.csv", header=T)

##########################
## variable description:##

## DVs ##
# l14e.n: "you think the environmental damage caused by global warming as a function of climate change is:"
#         "extreme damaging", "very damaging", "somewhat damaging", "cannot choose", "not very damaging", and "not damaging at all"
#         -- these are coded as 6, 5, 4, 3, 2, and 1, with higher numbers representing a more severe perception of climate change. 
# co2f:   "CO2 increase will cause global warming"
#         We coded this as a binary variable with "true" being 1 and both "not true" and "cannot choose" as 0
# fuelf:  "we affect climate change every time we use coal, oil or natural gas". 
#         The answers and our coded numerical values are as follows: "very true" (5), "maybe true" (4), "cannot choose" (3), "maybe not true" (2), and "not true" (1). 

## Disaster variables ##
# tot0909c: the total number of extreme weather events that happened in the prefecture in 2009  
# tot0009c: the total number of extreme weather events during the 2000-2009 period 
# tot0008c: the total number of extreme weather events during the 2000-2008 period (we use this to calculate 
#           the difference between the number of 2009 events and the annual average number of events during 
#           the 2000-2008 period as I(tot0909c-tot0008c/9))
# stm0909c: the total number of storms that happened in the prefecture in 2009  
# stm0009c: the total number of storms during the 2000-2009 period 
# stm0008c: the total number of storms during the 2000-2008 period (we use this to calculate 
#           the difference between the number of 2009 events and the annual average number of events during 
#           the 2000-2008 period as I(stm0909c-stm0008c/9))
# fld0909c: the total number of floods that happened in the prefecture in 2009  
# fld0009c: the total number of floods during the 2000-2009 period 
# fld0008c: the total number of floods during the 2000-2008 period (we use this to calculate 
#           the difference between the number of 2009 events and the annual average number of events during 
#           the 2000-2008 period as I(fld0909c-fld0008c/9))

## Control variables ##
# gender: gender
# age: age
# married: married or not
# minority: ethnic minority or not
# hukou: Urban Hukou or not
# ccp: Chinese Communist Party member or not
# religion: religious or not
# income: in RMB
# edu: level of education (1-4 corresponding to primary school and below, middle school, high school, and college and above


## the following is to replicate the 6 regression tables (Table A2-A7; they are in online appendix because of space limit) and 
## Figure 1 that summarizes these 6 regression tables. 


############
## Table A2: Effects of extreme weather events on the perception of climate change severity
## total disasters:
lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0909c | s41 | 0 | s42, data = cgss); bla1<-lpm1  # column 1

lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0009c | s41 | 0 | s42, data = cgss); bla2<-lpm1  # column 2

lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(tot0909c-tot0008c/9) | s41 | 0 | s42, data = cgss); bla3<-lpm1  # column 3

## storms:
lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0909c | s41 | 0 | s42, data = cgss); bla4<-lpm1  # column 4

lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0009c | s41 | 0 | s42, data = cgss); bla5<-lpm1  # column 5
                  
lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(stm0909c-stm0008c/9) | s41 | 0 | s42, data = cgss); bla6<-lpm1  # column 6

## floods: 
lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0909c | s41 | 0 | s42, data = cgss); bla7<-lpm1  # column 7

lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0009c | s41 | 0 | s42, data = cgss); bla8<-lpm1  # column 8

lpm1<-felm(l14e.n ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(fld0909c-fld0008c/9) | s41 | 0 | s42, data = cgss); bla9<-lpm1  # column 9

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5,  bla6,  bla7, bla8,   bla9)


############
## Table A3: Effects of extreme weather events on climate change knowledge - whether CO2 increase will cause global warming
## total disasters:
lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0909c | s41 | 0 | s42, data = cgss); bla1<-lpm1  # column 1

lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0009c | s41 | 0 | s42, data = cgss); bla2<-lpm1  # column 2

lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(tot0909c-tot0008c/9) | s41 | 0 | s42, data = cgss); bla3<-lpm1  # column 3

## storms:
lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0909c | s41 | 0 | s42, data = cgss); bla4<-lpm1  # column 4

lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0009c | s41 | 0 | s42, data = cgss); bla5<-lpm1  # column 5
                  
lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(stm0909c-stm0008c/9) | s41 | 0 | s42, data = cgss); bla6<-lpm1  # column 6

## floods: 
lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0909c | s41 | 0 | s42, data = cgss); bla7<-lpm1  # column 7

lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0009c | s41 | 0 | s42, data = cgss); bla8<-lpm1  # column 8

lpm1<-felm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(fld0909c-fld0008c/9) | s41 | 0 | s42, data = cgss); bla9<-lpm1  # column 9

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5,  bla6,  bla7, bla8,   bla9)


############
## Table A4: Effects of extreme weather events on climate change knowledge - whether climate affected by fossil fuel use
## total disasters:
lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0909c | s41 | 0 | s42, data = cgss); bla1<-lpm1  # column 1

lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0009c | s41 | 0 | s42, data = cgss); bla2<-lpm1  # column 2

lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(tot0909c-tot0008c/9) | s41 | 0 | s42, data = cgss); bla3<-lpm1  # column 3

## storms:
lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0909c | s41 | 0 | s42, data = cgss); bla4<-lpm1  # column 4

lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0009c | s41 | 0 | s42, data = cgss); bla5<-lpm1  # column 5
                  
lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(stm0909c-stm0008c/9) | s41 | 0 | s42, data = cgss); bla6<-lpm1  # column 6

## floods: 
lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0909c | s41 | 0 | s42, data = cgss); bla7<-lpm1  # column 7

lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0009c | s41 | 0 | s42, data = cgss); bla8<-lpm1  # column 8

lpm1<-felm(fuelf ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(fld0909c-fld0008c/9) | s41 | 0 | s42, data = cgss); bla9<-lpm1  # column 9

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5,  bla6,  bla7, bla8,   bla9)


############
## Table A5: Effects of extreme weather events on the perception of climate change severity using ordered logit
## total disasters:
polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + tot0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla1<-polr.out  # column 1

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + tot0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla2<-polr.out  # column 2 

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(tot0909c-tot0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla3<-polr.out # column 3 

## storms:
polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + stm0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla4<-polr.out  # column 4

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + stm0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla5<-polr.out  # column 5 

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(stm0909c-stm0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla6<-polr.out # column 6 

## floods: 
polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + fld0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla7<-polr.out  # column 7

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + fld0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla8<-polr.out  # column 8 

polr.out <- polr(as.factor(l14e.n) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(fld0909c-fld0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla9<-polr.out # column 9 

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5, bla6, bla7, bla8, bla9)


############
## Table A6: Effects of extreme weather events on climate change knowledge - whether CO2 increase will cause global warming, using logit
## total disasters:
lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0909c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla1<-lpm1  # column 1

lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + tot0009c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla2<-lpm1  # column 2

lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(tot0909c-tot0008c/9) + as.factor(s41), data=cgss, family=binomial(link="logit")); bla3<-lpm1  # column 3
                
## storms:
lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0909c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla4<-lpm1  # column 4

lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + stm0009c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla5<-lpm1  # column 5
                  
lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(stm0909c-stm0008c/9) + as.factor(s41), data=cgss, family=binomial(link="logit")); bla6<-lpm1  # column 6

## floods: 
lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0909c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla7<-lpm1  # column 7

lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + fld0009c + as.factor(s41), data=cgss, family=binomial(link="logit")); bla8<-lpm1  # column 8

lpm1<-glm(co2f ~ gender + age + married + minority + hukou + ccp + religion + log(income+1) + as.factor(edu)
                  + I(fld0909c-fld0008c/9) + as.factor(s41), data=cgss, family=binomial(link="logit")); bla9<-lpm1  # column 9

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5,  bla6,  bla7, bla8,   bla9)


############
## Table A7: Effects of extreme weather events on climate change knowledge - whether climate affected by fossil fuel use, using ordered logit
## total disasters:
polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + tot0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla1<-polr.out  # column 1

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + tot0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla2<-polr.out  # column 2 

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(tot0909c-tot0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla3<-polr.out # column 3 

## storms:
polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + stm0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla4<-polr.out  # column 4

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + stm0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla5<-polr.out  # column 5 

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(stm0909c-stm0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla6<-polr.out # column 6 

## floods: 
polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + fld0909c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla7<-polr.out  # column 7

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + fld0009c + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla8<-polr.out  # column 8 

polr.out <- polr(as.factor(fuelf) ~ gender + age + married + minority + hukou + ccp + religion +log(income+1)+ as.factor(edu)
                 + I(fld0909c-fld0008c/9) + as.factor(s41), data=cgss,na.action=na.omit, method = "logistic" , Hess=TRUE); bla9<-polr.out # column 9 

## generate the regression table: 
stargazer(bla1, bla2, bla3, bla4, bla5, bla6, bla7, bla8, bla9)




#############################################################################
## Figure 1: this is to summarize all regressions above from CGSS analysis ##
## we've done this manually instead of simulation in the following: 
## figure 1a: 
m1<-c(0.142, -0.001,    0.124,    0.315,   0.004,  0.162,    0.118,    -0.006,    0.174)     ## mean coef estimates    
se1<-c(0.084,0.013,     0.063,    0.075,   0.014,  0.150,    0.122,    0.021,     0.124)     ## standard errors

m2<-c(-0.041, -0.007, 0.002, -0.034, -0.004, 0.043,   -0.027, -0.010,  0.011)
se2<-c(0.031, 0.005,  0.035,  0.068,  0.006, 0.104,   0.041,   0.011,  0.049)

m3<-c(0.108,    -0.003,    0.108,    -0.181,    -0.014,   0.134,    0.246,   0.023,      0.203) 
se3<-c(0.093,   0.013,     0.073,     0.111,     0.012,    0.207,   0.119,   0.022,0.110)

par(mfrow=c(1,4),mar= c(1.95, 0.95,1.95, 1.15),mgp=c(0,0,0))
ylim <- c(0.5,9.5)

xlim<-c(-3,3)
xlim2 <- c(-.8,.8)
xlim1 <- c(-.8,.8)
xlim3 <- c(-.8,.8)

plot(c(-5, 5) ,ylim,type="n",xlab="",ylab="",axes=FALSE, main="")
text(rep(1, 9), c(9:1), c("All disasters of 2009", 
                          "All disasters of 2000-09",
                          "Change in disaters: 2009 vs. 2000-08",
                          "Storms of 2009", 
                          "Storms of 2000-09",
                          "Change in storms: 2009 vs. 2000-08",
                          "Floods of 2009", 
                          "Floods of 2000-09",
                          "Change in floods: 2009 vs. 2000-08"), cex=1.5)

plot(xlim1,ylim,type="n",xlab="",ylab="",axes=FALSE, main="Climate Change Severity (OLS)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m1-1.96*se1, 9:1, m1+1.96*se1, 9:1, col="gray", lwd=3); points(m1, 9:1, pch=19, cex=1.5)

plot(xlim2,ylim,type="n",xlab="",ylab="",axes=FALSE, main="CO2 causes global warming (OLS)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m2-1.96*se2, 9:1, m2+1.96*se2, 9:1, col="gray", lwd=3); points(m2, 9:1, pch=19, cex=1.5)

plot(xlim3,ylim,type="n",xlab="",ylab="",axes=FALSE, main="Climate affected by fossil fuel (OLS)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m3-1.96*se3, 9:1, m3+1.96*se3, 9:1, col="gray", lwd=3); points(m3, 9:1, pch=19, cex=1.5)


### figure 1b: 
m01<-c(0.276, 0.005, 0.209,  0.637, 0.011, 0.253, 0.245, 0.00004, 0.311)
se01<-c(0.157,0.020,0.137,0.421,0.026,0.336,0.194,0.032,0.208)

m02<-c(-0.205,    -0.031,     -0.005,   -0.109,    -0.017,     0.220,  -0.142,        -0.046,    0.031)
se02<-c(0.197,0.025,0.168,0.572,0.034,0.414,0.240,0.040,0.253)

m03<-c(0.208,    -0.004,  0.203,    -0.251 ,   -0.026 ,  0.308 ,  0.460,   0.046, 0.368)      
se03<-c(0.157,0.020,0.139,0.409,0.026,0.347,0.196,0.033,0.208)

xlim<-c(-3,3)
xlim2 <- c(-1.8,1.8)
xlim1 <- c(-1.8,1.8)
xlim3 <- c(-1.8,1.8)

plot(c(-5, 5) ,ylim,type="n",xlab="",ylab="",axes=FALSE, main="")
text(rep(1, 9), c(9:1), c("All disasters of 2009", 
                          "All disasters of 2000-09",
                          "Change in disaters: 2009 vs. 2000-08",
                          "Storms of 2009", 
                          "Storms of 2000-09",
                          "Change in storms: 2009 vs. 2000-08",
                          "Floods of 2009", 
                          "Floods of 2000-09",
                          "Change in floods: 2009 vs. 2000-08"), cex=1.5)

plot(xlim1,ylim,type="n",xlab="",ylab="",axes=FALSE, main="Climate Change Severity (ordered logit)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m01-1.96*se01, 9:1, m01+1.96*se01, 9:1, col="gray", lwd=3); points(m01, 9:1, pch=19, cex=1.5, col="black")


plot(xlim2,ylim,type="n",xlab="",ylab="",axes=FALSE, main="CO2 causes global warming (logistic)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m02-1.96*se02, 9:1, m02+1.96*se02, 9:1, col="gray", lwd=3); points(m02, 9:1, pch=19, cex=1.5, col="black")

plot(xlim3,ylim,type="n",xlab="",ylab="",axes=FALSE, main="Climate affected by fossil fuel (ordered logit)",cex.main=1.5)
abline(v=0, col="black", lwd=1)
axis(1, cex.axis=1.15)
segments(m03-1.96*se03, 9:1, m03+1.96*se03, 9:1, col="gray", lwd=3); points(m03, 9:1, pch=19, cex=1.5, col="black")





############## 2: Replicating the Baidu SVI results ###############
###########################################################
Sys.setlocale(category = "LC_CTYPE", locale = "chs")
fram<-read.csv("baidu_replicate.csv", header=T)

#####################
## variable names: ##
# pref: prefecture name; this is written in Chinese
# day: in month/day/year format
# svi.cc: Baidu SVI for "climate change"
# svi.gcc: Baidu SVI for "global climate change" 
# svi.gwarm: Baidu SVI for "global warming" 
# disa.all: total number of extreme weather event within a prefecture and in the day
# disa.all.lg1: total number of extreme weather event within a prefecture in the previous day (i.e., temporally lagged by a day)
# disa.all.lg2: total number of extreme weather event within a prefecture in the day before yesterday (i.e., temporally lagged by two days)
# disa.all.lg3: total number of extreme weather event within a prefecture three days prior (i.e., temporally lagged by three days)
# disa.flood: total number of floods within a prefecture in the day 
# disa.rain: total number of heavy rains within a prefecture in the day 
# disa.storm: total number of storms within a prefecture in the day
# confirmed: cumulative confirmed Covid cases in a prefecture-day 
# dead: cumulative confirmed Covid death in a prefecture-day 
# temp.mean: average daily temperature 
# preci: total daily rainfall 


############
## Table 1: Testing the effects of natural disasters on Baidu SVI regarding climate change key words.   
lpm0<-felm(svi.cc  ~ disa.all + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla1<- lpm0
lpm0<-felm(svi.cc  ~ disa.all.lg1 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla2<- lpm0
lpm0<-felm(svi.cc  ~ disa.all.lg2 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla3<- lpm0
lpm0<-felm(svi.cc  ~ disa.all.lg3 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla4<- lpm0

lpm0<-felm(svi.gwarm  ~ disa.all + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla5<- lpm0
lpm0<-felm(svi.gwarm  ~ disa.all.lg1 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla6<- lpm0
lpm0<-felm(svi.gwarm  ~ disa.all.lg2 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla7<- lpm0
lpm0<-felm(svi.gwarm  ~ disa.all.lg3 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla8<- lpm0

lpm0<-felm(svi.gcc  ~ disa.all + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla9<- lpm0
lpm0<-felm(svi.gcc  ~ disa.all.lg1 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla10<- lpm0
lpm0<-felm(svi.gcc  ~ disa.all.lg2 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla11<- lpm0
lpm0<-felm(svi.gcc  ~ disa.all.lg3 + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla12<- lpm0

stargazer(bla1, bla2, bla3, bla4, bla5, bla6, bla7, bla8, bla9, bla10,bla11,bla12)


############
## Table 2: Effects of natural disaster types on Baidu SVI regarding climate change key words.    
lpm0<-felm(svi.cc  ~ disa.flood + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla1<- lpm0
lpm0<-felm(svi.cc  ~ disa.rain  + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla2<- lpm0
lpm0<-felm(svi.cc  ~ disa.storm + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla3<- lpm0

lpm0<-felm(svi.gwarm  ~ disa.flood + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla4<- lpm0
lpm0<-felm(svi.gwarm  ~ disa.rain  + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla5<- lpm0
lpm0<-felm(svi.gwarm  ~ disa.storm + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla6<- lpm0

lpm0<-felm(svi.gcc  ~ disa.flood + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla7<- lpm0
lpm0<-felm(svi.gcc  ~ disa.rain  + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla8<- lpm0
lpm0<-felm(svi.gcc  ~ disa.storm + confirmed + dead + temp.mean + preci |pref+day| 0 | pref, data = fram); bla9<- lpm0

stargazer(bla1, bla2, bla3, bla4, bla5, bla6, bla7, bla8, bla9)
