# This is the R code for replicating Thomann, Eva (2018). "The best of both worlds? Logics of action and customization ", in Thomann, Eva. Customized implementation of European Union food safety policy. United in diversity? Palgrave McMillan. Chapter 5.

#descriptive statistics for "The best of both worlds
#4 EU countries

#clear workspace
rm(list = ls())
library(lattice); library(arm); library(xtable); library(foreign); library(psych); library(directlabels); library(betareg); library(VIM); library(base); library(plyr); library(dplyr); library(QCA); library(QCAGUI); library(SetMethods)


setwd("C:/Users/ethof/Dropbox/Arbeit/Publikationen/Papers/Customization/Two logics/Data")

######classification########
#descriptives
logicsraw <- read.csv("logicsraw.csv", row.names=1, header = TRUE, sep = ";",  dec = ",")
describe(logicsraw)

# check if thresholds ok

colnames(logicsraw)
sum(logicsraw$vpo == 5)
sum(logicsraw$vpl == 2.4)
sum(logicsraw$coerc == 1.6)

#Customization: 0 = low, 1 0=intermediate, 2 = high
logicsraw$cust1 <- NA
logicsraw$cust1[logicsraw$custom <= 1] <- 0
logicsraw$cust1[logicsraw$custom == 2] <- 1
logicsraw$cust1[logicsraw$custom >= 3] <- 2
logicsraw$cust1

logicsraw$resp1 <- NA
logicsraw$resp1[logicsraw$resp == 0] <- 0
logicsraw$resp1[logicsraw$resp == 1] <- 2
logicsraw$resp1

logicsraw$sal1 <- NA
logicsraw$sal1[logicsraw$sal == 0] <- 0
logicsraw$sal1[logicsraw$sal == 1] <- 2
logicsraw$sal1

logicsraw$res1 <- NA
logicsraw$res1[logicsraw$res <= 2] <- 0
logicsraw$res1[logicsraw$res >= 3 & logicsraw$res <= 4] <- 1
logicsraw$res1[logicsraw$res >= 5] <- 2
logicsraw$res1

logicsraw$vpo1 <- NA
logicsraw$vpo1[logicsraw$vpo <=5] <- 0
logicsraw$vpo1[logicsraw$vpo >= 5] <- 2
logicsraw$vpo1

# check if not using mean as crossover makes a difference
vpodiff <-  as.numeric((logicsraw$vpo > 5) & (logicsraw$vpo <=  5.5))
sum(vpodiff)
#it doesn't make a difference

logicsraw$vpl1 <- NA
logicsraw$vpl1[logicsraw$vpl <=2.4] <- 0
logicsraw$vpl1[logicsraw$vpl >= 2.4] <- 2
logicsraw$vpl1

# build institutional constraints, check how skewed
logicsraw$inst1 <- NA
logicsraw$inst1[logicsraw$vpo1 == 0 & logicsraw$vpl1 == 0] <- 2
logicsraw$inst1[logicsraw$vpo1 == 2 | logicsraw$vpl1 == 2] <- 0
describe(logicsraw$inst1)
skewMYSET <- as.numeric(logicsraw$inst1 > 0)
sum(skewMYSET)


logicsraw$coerc1 <- NA
logicsraw$coerc1[logicsraw$coerc <=1.6] <- 0
logicsraw$coerc1[logicsraw$coerc >= 1.6] <- 2
logicsraw$coerc1

#save dataset
write.csv2(logicsraw, file = "logics.csv")

#####binomial tests#######
logi <- read.csv("logics.csv", row.names=1, header = TRUE, sep = ";",  dec = ",")
describe(logi)

#####interpretation 1: clear logic dominates unclear logic######

# type 1

logi$t1int1 <- as.numeric((logi$res1 == 2) & (logi$inst1 == 2) & (logi$resp1 == 2))
sum(logi$t1int1)
logi$t1loa <- as.numeric((logi$t1int1 == 1) & (logi$cust1 == 2))
sum(logi$t1loa)

#identify cases

casest1loatyp <- subset(logi, t1loa ==1 & cust1 ==2)
rownames(casest1loatyp)
casest1loadev <- subset(logi, t1int1 ==1 & cust1 < 2)
rownames(casest1loadev)

# type 2

logi$t2int1 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2) & (logi$resp1 == 2) & (logi$coerc1 == 2))
sum(logi$t2int1)
logi$t2loa <- as.numeric((logi$t2int1 == 1) & (logi$cust1 >= 1))
sum(logi$t2loa)

#identify cases

casest2loatyp <- subset(logi, t2loa ==1)
rownames(casest2loatyp)
casest2loadev <- subset(logi, t2int1 ==1 & cust1 < 1)
casest2loadev

# type 2 is a logical remainder

# type 3
logi$t3int1 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2) & (logi$resp1 == 2) & (logi$coerc1 == 0))
sum(logi$t3int1)
logi$t3loa <- as.numeric((logi$t3int1 == 1) & (logi$cust1 ==0))
sum(logi$t3loa)

#identify cases

casest3loatyp <- subset(logi, t3loa ==1)
rownames(casest3loatyp)
casest3loadev <- subset(logi, t3int1 ==1 & cust1 > 0)
rownames(casest3loadev)


# binomial test

pLoAint1 <- binom.test(9, 13,(2/6),alternative="greater")
pLoAint1

###interpretation 2: macro versus micro issues #######

# create sets of micro and macro issues
colnames(logi)

logi$micro1 <- as.numeric(logi$caseid == "d8" | logi$caseid == "d9" | logi$caseid == "d11" | logi$caseid == "d13" | logi$caseid == "a1" | logi$caseid == "a3")
logi$micro1

logi$micro <- as.numeric(logi$micro1== 1 | logi$sal1== 0)
sum(logi$micro)

# type 1

logi$t1int2 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 0))
sum(logi$t1int2)
logi$t1loc <- as.numeric((logi$t1int2 == 1) & (logi$cust1 == 0))
sum(logi$t1loc)
logi$t1loa <- as.numeric((logi$t1int2 == 1) & (logi$cust1 >= 1))
sum(logi$t1loa)

#identify cases
casest1loctyp <- subset(logi, t1loc ==1)
rownames(casest1loctyp)
casest1locdev <- subset(logi, t1int2 ==1 & cust1 > 0)
casest1locdev


casest1loatyp <- subset(logi, t1loa ==1)
casest1loatyp
casest1loadev <- subset(logi, t1int2 ==1 & cust1 < 1)
casest1loadev

# type 1 is a logical remainder

# type 2

logi$t2int2 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 1))
sum(logi$t2int2)
logi$t2loc <- as.numeric((logi$t2int2 == 1) & (logi$cust1 == 0))
sum(logi$t2loc)
logi$t2loa <- as.numeric((logi$t2int2 == 1) & (logi$cust1 >= 1))
sum(logi$t2loa)

#identify cases
casest2loctyp <- subset(logi, t2loc ==1)
rownames(casest2loctyp)
casest2locdev <- subset(logi, t2int2 ==1 & cust1 > 0)
rownames(casest2locdev)


casest2loatyp <- subset(logi, t2loa ==1)
rownames(casest2loatyp)
casest2loadev <- subset(logi, t2int2 ==1 & cust1 < 1)
rownames(casest2loadev)

# type 3

logi$t3int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0)  & (logi$micro == 0))
sum(logi$t3int2)
logi$t3loc <- as.numeric((logi$t3int2 == 1) & (logi$cust1 >= 1))
sum(logi$t3loc)
logi$t3loa <- as.numeric((logi$t3int2 == 1) & (logi$cust1 == 0))
sum(logi$t3loa)

#identify cases
casest3loctyp <- subset(logi, t3loc ==1)
casest3loctyp
casest3locdev <- subset(logi, t3int2 ==1 & cust1 == 0)
casest3locdev


casest3loatyp <- subset(logi, t3loa ==1)
casest3loatyp
casest3loadev <- subset(logi, t3int2 ==1 & cust1 > 0)
casest3loadev

# type 4

logi$t4int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0)  & (logi$micro == 1))
sum(logi$t4int2)
logi$t4loc <- as.numeric((logi$t4int2 == 1) & (logi$cust1 >= 1))
sum(logi$t4loc)
logi$t4loa <- as.numeric((logi$t4int2 == 1) & (logi$cust1 == 0))
sum(logi$t4loa)

#identify cases
casest4loctyp <- subset(logi, t4loc ==1)
rownames(casest4loctyp)
casest4locdev <- subset(logi, t4int2 ==1 & cust1 == 0)
rownames(casest4locdev)


casest4loatyp <- subset(logi, t4loa ==1)
rownames(casest4loatyp)
casest4loadev <- subset(logi, t4int2 ==1 & cust1 > 0)
rownames(casest4loadev)

# type 5

logi$t5int2 <- as.numeric((logi$res1 == 2) & (logi$resp1 == 0) & (logi$micro == 0))
sum(logi$t5int2)
logi$t5loc <- as.numeric((logi$t5int2 == 1) & (logi$cust1 == 0))
sum(logi$t5loc)
logi$t5loa <- as.numeric((logi$t5int2 == 1) & (logi$cust1 == 2))
sum(logi$t5loa)

#identify cases
casest5loctyp <- subset(logi, t5loc ==1)
casest5loctyp
casest5locdev <- subset(logi, t5int2 ==1 & cust1 > 0)
casest5locdev


casest5loatyp <- subset(logi, t5loa ==1)
casest5loatyp
casest5loadev <- subset(logi, t5int2 ==1 & cust1 < 2)
casest5loadev

# type 6

logi$t6int2 <- as.numeric((logi$res1 == 2) & (logi$resp1 == 0) & (logi$micro == 1))
sum(logi$t6int2)
logi$t6loc <- as.numeric((logi$t6int2 == 1) & (logi$cust1 == 0))
sum(logi$t6loc)
logi$t6loa <- as.numeric((logi$t6int2 == 1) & (logi$cust1 == 2))
sum(logi$t6loa)

#identify cases
casest6loctyp <- subset(logi, t6loc ==1)
casest6loctyp
casest6locdev <- subset(logi, t6int2 ==1 & cust1 > 0)
casest6locdev


casest6loatyp <- subset(logi, t6loa ==1)
rownames(casest6loatyp)
casest6loadev <- subset(logi, t6int2 ==1 & cust1 < 2)
rownames(casest6loadev)


# binomial test
# macro-issues

pLoCint2macro <- binom.test(6, 7,(3/6),alternative="greater")
pLoCint2macro


pLoAint2macro <- binom.test(1, 7,(2/6),alternative="greater")
pLoAint2macro

# micro-issues

pLoCint2micro <- binom.test(1, 11,(4/9),alternative="greater")
pLoCint2micro


pLoAint2micro <- binom.test(8, 11,(4/9),alternative="greater")
pLoAint2micro

##### remaining, nondiscernible types


# type 7

logi$t7int2 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0) & (logi$micro == 0))
sum(logi$t7int2)
logi$t7loa <- as.numeric((logi$t7int2 == 1) & (logi$cust1 == 0))
sum(logi$t7loa)

#identify cases


casest7loatyp <- subset(logi, t7loa ==1)
casest7loatyp
casest7loadev <- subset(logi, t7int2 ==1 & cust1 >0)
casest7loadev

# type 7 is a logical remainder

# type 8

logi$t8int2 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0) & (logi$micro == 1))
sum(logi$t8int2)
logi$t8loa <- as.numeric((logi$t8int2 == 1) & (logi$cust1 == 0))
sum(logi$t8loa)

#identify cases


casest8loatyp <- subset(logi, t8loa ==1)
casest8loatyp
casest8loadev <- subset(logi, t8int2 ==1 & cust1 >0)
casest8loadev

# type 8 is a logical remainder

# type 9

logi$t9int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0)  & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 0))
sum(logi$t9int2)
logi$t9loa <- as.numeric((logi$t9int2 == 1) & (logi$cust1 > 0))
sum(logi$t9loa)

#identify cases


casest9loatyp <- subset(logi, t9loa ==1)
casest9loatyp
casest9loadev <- subset(logi, t9int2 ==1 & cust1 == 0)
casest9loadev

# type 10

logi$t10int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0)  & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 1))
sum(logi$t10int2)
logi$t10loa <- as.numeric((logi$t10int2 == 1) & (logi$cust1 > 0))
sum(logi$t10loa)

#identify cases


casest10loatyp <- subset(logi, t10loa ==1)
casest10loatyp
casest10loadev <- subset(logi, t10int2 ==1 & cust1 == 0)
casest10loadev

# type 11

logi$t11int2 <- as.numeric((logi$res1 == 2) & (logi$inst1 == 0)  & (logi$resp1 == 2)& (logi$micro == 0))
sum(logi$t11int2)
logi$t11loa <- as.numeric((logi$t11int2 == 1) & (logi$cust1 > 0))
sum(logi$t11loa)

## contains cases with intermediate customization? strictly speaking, these would not conform to LoA
logi$t11notloa <- as.numeric((logi$t11loa == 1) & (logi$cust1 == 1))
sum(logi$t11notloa)

# this is adjusted in the table: 9 cases confirm for LoC, only 6 for Loa
#identify cases


casest11loatyp <- subset(logi, t11loa ==1)
casest11loatyp
casest11loadev <- subset(logi, t11int2 ==1 & cust1 == 0)
casest11loadev

# type 12

logi$t12int2 <- as.numeric((logi$res1 == 2) & (logi$inst1 == 0)  & (logi$resp1 == 2) & (logi$micro == 1))
sum(logi$t12int2)
logi$t12loa <- as.numeric((logi$t12int2 == 1) & (logi$cust1 > 0))
sum(logi$t12loa)

#identify cases


casest12loatyp <- subset(logi, t12loa ==1)
casest12loatyp
casest12loadev <- subset(logi, t12int2 ==1 & cust1 == 0)
casest12loadev

# type 12 is a logical remainder

# type 13

logi$t13int2 <- as.numeric((logi$res1 == 2) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$micro == 0))
sum(logi$t13int2)
logi$t13loa <- as.numeric((logi$t13int2 == 1) & (logi$cust1 == 2))
sum(logi$t13loa)

#identify cases

casest13loatyp <- subset(logi, t13loa ==1)
casest13loatyp
casest13loadev <- subset(logi, t13int2 ==1 & cust1 < 2)
casest13loadev

# type 14

logi$t14int2 <- as.numeric((logi$res1 == 2) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$micro == 1))
sum(logi$t14int2)
logi$t14loa <- as.numeric((logi$t14int2 == 1) & (logi$cust1 == 2))
sum(logi$t14loa)

#identify cases


casest14loatyp <- subset(logi, t14loa ==1)
casest14loatyp
casest14loadev <- subset(logi, t14int2 ==1 & cust1 < 2)
casest14loadev

# type 14 is a logical remainder

# type 15

logi$t15int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 0))
sum(logi$t15int2)
logi$t15loa <- as.numeric((logi$t15int2 == 1) & (logi$cust1 > 0))
sum(logi$t15loa)

#identify cases


casest15loatyp <- subset(logi, t15loa ==1)
casest15loatyp
casest15loadev <- subset(logi, t15int2 ==1 & cust1 == 0)
casest15loadev

# type 15 is a logical remainder

# type 16

logi$t16int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$micro == 1))
sum(logi$t16int2)
logi$t16loa <- as.numeric((logi$t16int2 == 1) & (logi$cust1 > 0))
sum(logi$t16loa)

#identify cases

casest16loatyp <- subset(logi, t16loa ==1)
casest16loatyp
casest16loadev <- subset(logi, t16int2 ==1 & cust1 == 0)
casest16loadev

# type 16 is a logical remainder

# type 17

logi$t17int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$coerc1 == 0) & (logi$micro == 0))
sum(logi$t17int2)
logi$t17loa <- as.numeric((logi$t17int2 == 1) & (logi$cust1 == 0))
sum(logi$t17loa)

#identify cases

casest17loatyp <- subset(logi, t17loa ==1)
casest17loatyp
casest17loadev <- subset(logi, t17int2 ==1 & cust1 > 0)
casest17loadev

# type 18

logi$t18int2 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 2)  & (logi$resp1 == 2) & (logi$coerc1 == 0) & (logi$micro == 1))
sum(logi$t18int2)
logi$t18loa <- as.numeric((logi$t18int2 == 1) & (logi$cust1 == 0))
sum(logi$t18loa)

#identify cases

casest18loatyp <- subset(logi, t18loa ==1)
casest18loatyp
casest18loadev <- subset(logi, t18int2 ==1 & cust1 > 0)
casest18loadev

# type 19

logi$t19int2 <- as.numeric((logi$res1 < 2) & (logi$resp1 == 0) & (logi$micro == 0))
sum(logi$t19int2)
logi$t19loa <- as.numeric((logi$t19int2 == 1) & (logi$cust1 == 0))
sum(logi$t19loa)

#identify cases

casest19loatyp <- subset(logi, t19loa ==1)
casest19loatyp
casest19loadev <- subset(logi, t19int2 ==1 & cust1 > 0)
casest19loadev

# type 20

logi$t20int2 <- as.numeric((logi$res1 < 2) & (logi$resp1 == 0) & (logi$micro == 1))
sum(logi$t20int2)
logi$t20loa <- as.numeric((logi$t20int2 == 1) & (logi$cust1 == 0))
sum(logi$t20loa)

#identify cases


casest20loatyp <- subset(logi, t20loa ==1)
casest20loatyp
casest20loadev <- subset(logi, t20int2 ==1 & cust1 > 0)
casest20loadev

# binomial test, all cases
# macro-issues

pLoCint2macroall <- binom.test(22, 24,(8/15),alternative="greater")
pLoCint2macroall

pLoAint2macroall <- binom.test(20,33,(8/21),alternative="greater")
pLoAint2macroall

# micro-issues

pLoCint2microall <- binom.test(16, 39,(7/15),alternative="greater")
pLoCint2microall


pLoAint2microall <- binom.test(26, 43,(8/18),alternative="greater")
pLoAint2microall



##### Interpretation 3:  old and new directives#######

# build the set of old and new directives

colnames(logi)

logi$old <- as.numeric(logi$directive == "90/167/EEC")
logi$old
sum(logi$old)

# type 1

logi$t1int3 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$old == 1))
sum(logi$t1int3)
logi$t1loc <- as.numeric((logi$t1int3 == 1) & (logi$cust1 == 0))
sum(logi$t1loc)
logi$t1loa <- as.numeric((logi$t1int3 == 1) & (logi$cust1 >= 1))
sum(logi$t1loa)

#identify cases
casest1loctyp <- subset(logi, t1loc ==1)
casest1loctyp
casest1locdev <- subset(logi, t1int3 ==1 & cust1 > 0)
casest1locdev


casest1loatyp <- subset(logi, t1loa ==1)
casest1loatyp
casest1loadev <- subset(logi, t1int3 ==1 & cust1 < 1)
casest1loadev

# type 1 is a logical remainder

# type 2

logi$t2int3 <- as.numeric((logi$res1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 2) & (logi$old == 0))
sum(logi$t2int3)
logi$t2loc <- as.numeric((logi$t2int3 == 1) & (logi$cust1 == 0))
sum(logi$t2loc)
logi$t2loa <- as.numeric((logi$t2int3 == 1) & (logi$cust1 >= 1))
sum(logi$t2loa)

#identify cases
casest2loctyp <- subset(logi, t2loc ==1)
casest2loctyp
casest2locdev <- subset(logi, t2int3 ==1 & cust1 > 0)
casest2locdev


casest2loatyp <- subset(logi, t2loa ==1)
casest2loatyp
casest2loadev <- subset(logi, t2int3 ==1 & cust1 < 1)
casest2loadev

# type 3

logi$t3int3 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0)  & (logi$old == 1))
sum(logi$t3int3)
logi$t3loc <- as.numeric((logi$t3int3 == 1) & (logi$cust1 >= 1))
sum(logi$t3loc)
logi$t3loa <- as.numeric((logi$t3int3 == 1) & (logi$cust1 == 0))
sum(logi$t3loa)

#identify cases
casest3loctyp <- subset(logi, t3loc ==1)
casest3loctyp
casest3locdev <- subset(logi, t3int3 ==1 & cust1 == 0)
casest3locdev


casest3loatyp <- subset(logi, t3loa ==1)
casest3loatyp
casest3loadev <- subset(logi, t3int3 ==1 & cust1 > 0)
casest3loadev

# type 4

logi$t4int3 <- as.numeric((logi$res1 == 1) & (logi$inst1 == 0) & (logi$resp1 == 2) & (logi$coerc1 == 0)  & (logi$old == 0))
sum(logi$t4int3)
logi$t4loc <- as.numeric((logi$t4int3 == 1) & (logi$cust1 >= 1))
sum(logi$t4loc)
logi$t4loa <- as.numeric((logi$t4int3 == 1) & (logi$cust1 == 0))
sum(logi$t4loa)

#identify cases
casest4loctyp <- subset(logi, t4loc ==1)
casest4loctyp
casest4locdev <- subset(logi, t4int3 ==1 & cust1 == 0)
casest4locdev


casest4loatyp <- subset(logi, t4loa ==1)
casest4loatyp
casest4loadev <- subset(logi, t4int3 ==1 & cust1 > 0)
casest4loadev


# type 5

logi$t5int3 <- as.numeric((logi$res1 == 2) & (logi$resp1 == 0) & (logi$old == 1))
sum(logi$t5int3)
logi$t5loc <- as.numeric((logi$t5int3 == 1) & (logi$cust1 == 0))
sum(logi$t5loc)
logi$t5loa <- as.numeric((logi$t5int3 == 1) & (logi$cust1 == 2))
sum(logi$t5loa)

#identify cases
casest5loctyp <- subset(logi, t5loc ==1)
casest5loctyp
casest5locdev <- subset(logi, t5int3 ==1 & cust1 > 0)
casest5locdev


casest5loatyp <- subset(logi, t5loa ==1)
casest5loatyp
casest5loadev <- subset(logi, t5int3 ==1 & cust1 < 2)
casest5loadev


# type 6

logi$t6int3 <- as.numeric((logi$res1 == 2) & (logi$resp1 == 0) & (logi$old == 0))
sum(logi$t6int3)
logi$t6loc <- as.numeric((logi$t6int3 == 1) & (logi$cust1 == 0))
sum(logi$t6loc)
logi$t6loa <- as.numeric((logi$t6int3 == 1) & (logi$cust1 == 2))
sum(logi$t6loa)

#identify cases
casest6loctyp <- subset(logi, t6loc ==1)
casest6loctyp
casest6locdev <- subset(logi, t6int3 ==1 & cust1 > 0)
casest6locdev


casest6loatyp <- subset(logi, t6loa ==1)
casest6loatyp
casest6loadev <- subset(logi, t6int3 ==1 & cust1 < 2)
casest6loadev

# binomial test
# old decisions

pLoCint3old <- binom.test(1, 3,(3/6),alternative="greater")
pLoCint3old


pLoAint3old <- binom.test(1, 7,(2/6),alternative="greater")
pLoAint3old

# new decisions

pLoCint3new <- binom.test(6, 15,(2/6),alternative="greater")
pLoCint3new


pLoAint3new <- binom.test(7, 15,(3/6),alternative="greater")
pLoAint3new

### interpretation 4: only one logic ######

# 3 scenarios

pLoCint4 <- binom.test(7, 18,(4/12),alternative="greater")
pLoCint4


pLoAint4 <- binom.test(9, 18,(4/12),alternative="greater")
pLoAint4

# binomial test, all cases and issues

pLoCint2all <- binom.test(38, 76,(9/18),alternative="greater")
pLoCint2all

pLoAint2all <- binom.test(46,76,(10/24),alternative="greater")
pLoAint2all

