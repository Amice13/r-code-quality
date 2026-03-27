# This is the R code for replicating Thomann, Eva (2018). "	Europeanized solutions to shared problems? How customization affects policy outcomes ", in Thomann, Eva. Customized implementation of European Union food safety policy. United in diversity? Palgrave McMillan. Chapter 6.

### r script for chapter 6 ####

# clean environment
rm(list=ls()) 


# load packages
library(lattice); library(arm); library(xtable); library(foreign); library(psych); library(directlabels); library(betareg); library(VIM); library(base); library(plyr); library(dplyr); library(QCA); library(SetMethods)

# set working directory

setwd("C:/Users/ethof/Dropbox/Arbeit/Publikationen/Papers/Customization/Customizing Europe book/Material & Data/Data/Chapter 6/Chapter 7")
# load dataset
mydata <- read.csv("outcomes_raw_171013.csv", row.names=1, header = TRUE, sep = ",",  dec = ".")
mydata

# descriptive statistics
describe(mydata)


########Calibration###########

#Thresholds see Table 3

##successful implementation SUC

mydata$SUC <- mydata$success
mydata$SUC

# check skewness
skewSUC <- as.numeric(mydata$SUC > 0.5)
prop.table(table(skewSUC))

##extensive customization CUST
mydata$CUST <- calibrate(mydata$customization, type = "fuzzy", thresholds = "e=0, c=1.5, i=4", logistic = TRUE)
mydata$CUST

# check skewness
skewCUST <- as.numeric(mydata$CUST > 0.5)
prop.table(table(skewCUST))

#check for Switzerland
custch <- subset(mydata, country == "CH")
skewCUSTch <- as.numeric(custch$CUST > 0.5)
prop.table(table(skewCUSTch))

# Switzerland skews the set toward extensive customization

###tractable problem TRACT
mydata$TRACT <- recode(mydata$tractability, "6=0; 5=0.33; 3:4=0.67;1:2=1; else=NA")
mydata$TRACT


# check skewness
prop.table(table(mydata$TRACT))
skewTRACT <- as.numeric(mydata$TRACT > 0.5)
prop.table(table(skewTRACT))

##Centralized implementation structure CENT

#strong regional self-rule REG
colnames(mydata)
mydata$REG <- calibrate(mydata$self.rule, type = "fuzzy", thresholds = "e=7.75, c=9.5, i=15", logistic = TRUE)
mydata$REG

# check skewness
prop.table(table(mydata$self.rule))
skewREG <- as.numeric(mydata$REG > 0.5)
prop.table(table(skewREG))

# integrated implementation structure INT
colnames(mydata)
mydata$INT <- recode(mydata$implementingunits, "11=0; 10=0.33; 9=0.67;8=1; else=NA")
mydata$INT

# check skewness
prop.table(table(mydata$implementingunits))
skewINT <- as.numeric(mydata$INT > 0.5)
prop.table(table(skewINT))

# build set CENT = reg + INT

mydata$CENT <- compute("reg + INT", data=mydata)
mydata$CENT

# check skewness
skewCENT <- as.numeric(mydata$CENT > 0.5)
prop.table(table(skewCENT))

##active enforcement system ENF
colnames(mydata)
mydata$ENF <- mydata$enforcement
mydata$ENF

# check skewness
prop.table(table(mydata$ENF))
skewENF <- as.numeric(mydata$ENF> 0.5)
prop.table(table(skewENF))

## coherent policy design COH
colnames(mydata)
mydata$COH <- calibrate(mydata$coherencenew, type = "fuzzy", thresholds = "e=1, c=2.5, i=4", logistic = TRUE)
mydata$COH

# check skewness
prop.table(table(mydata$COH))
skewCOH <- as.numeric(mydata$COH> 0.5)
prop.table(table(skewCOH))


## Domestic resistance RES
colnames(mydata)
mydata$RES <- recode(mydata$resistance, "1:2=0; 3=0.33; 4:5=0.67;6=1; else=NA")
mydata$RES

# check skewness
prop.table(table(mydata$resistance))
skewRES <- as.numeric(mydata$RES > 0.5)
prop.table(table(skewRES))

###XY plots for calibration

colnames(mydata)

par(mfrow=c(3, 3))
plot(mydata$success, mydata$SUC, pch=18,  col="black",
     main='SUC',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 0.5, col="black")

plot(mydata$customization, mydata$CUST, pch=18,  col="black", ylim=c(0, 1), 
    main='CUST',
    xlab=' Raw score ',
    ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 1.5, col="black")

plot(mydata$tractability, mydata$TRACT, pch=18,  col="black",ylim=c(0, 1), xlim=c(1, 6), 
     main='TRACT',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 4.5, col="black")

plot(mydata$self.rule, mydata$REG, pch=18,  col="black",ylim=c(0, 1), xlim=c(0, 15), 
     main='REG',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 9.5, col="black")

plot(mydata$implementingunits, mydata$INT, pch=18,  col="black",ylim=c(0, 1), xlim=c(8, 11), 
     main='INT',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 9.5, col="black")

plot(mydata$enforcement, mydata$ENF, pch=18,  col="black",
     main='ENF',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 0.5, col="black")

plot(mydata$coherence, mydata$COH, pch=18,  col="black", ylim=c(0, 1), xlim=c(0, 6),
     main='COH',
     xlab=' Raw score ',
     ylab=' Crisp score ')
abline(h=0.5, col="black")
abline(v= 3.5, col="black")

plot(mydata$coherencenew, mydata$COH, pch=18,  col="black", xlim=c(0, 5),
     main='COH',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 2.5, col="black")

plot(mydata$resistance, mydata$RES, pch=18,  col="black", xlim=c(1, 6),
     main='RES',
     xlab=' Raw score ',
     ylab=' Fuzzy score ')
abline(h=0.5, col="black")
abline(v= 3.5, col="black")

# save calibrated dataset

outcomes_fuzzy <- subset(mydata, select = c("SUC", "CUST", "TRACT", "CENT", "ENF", "COH", "RES"))
write.csv(outcomes_fuzzy, "outcomes_fuzzy.csv")

#############analysis of necessity##########

mydata <- read.csv("outcomes_fuzzy.csv", row.names=1, header = TRUE, sep = ",",  dec = ".")
mydata

# for SUC

?superSubset
necSUC <- superSubset(mydata, outcome = "SUC", 
            conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
            incl.cut = 0.9, cov.cut = 0.55)
necSUC

conds <- subset(mydata, select = c("CUST", "TRACT", "CENT", "ENF", "COH", "RES"))
pof(conds, SUC, mydata, relation = "nec")
pof(1-conds, SUC, mydata, relation = "nec")

# no condition has a RoN above 0.5; there are no singular necessary conditions

# for suc

necsuc <- superSubset(mydata, outcome = "~SUC", 
            conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
            incl.cut = 0.9, cov.cut = 0.6)
necsuc

necsuc2 <- superSubset(mydata, outcome = "~SUC", 
                      conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                      incl.cut = 0.9, cov.cut = 0.5)
necsuc2
                      

conds <- subset(mydata, select = c("CUST", "TRACT", "CENT", "ENF", "COH", "RES"))
pof(conds, ~SUC, mydata, relation = "nec")
pof(1-conds, ~SUC, mydata, relation = "nec")

# No condition passes inclusion and covrage thresholds; when lowring the latter to 0.5, no condition has a RoN above 0.5; there are no singular necessary conditions

###########analysis of sufficiency########

mydata <- read.csv("outcomes_fuzzy.csv", row.names=1, header = TRUE)
mydata

QCAfit(mydata$CUST, mydata$SUC,  necessity=FALSE, neg.out=FALSE)
QCAfit(1-mydata$CUST, mydata$SUC,  necessity=FALSE, neg.out=FALSE)
QCAfit(mydata$CUST, 1-mydata$SUC,  necessity=FALSE, neg.out=FALSE)
QCAfit(1-mydata$CUST, 1-mydata$SUC,  necessity=FALSE, neg.out=FALSE)

# customization is neither necessary nor sufficient for successful or unsuccesful implementation

# let's see how they correlate

cor.test(mydata$CUST, mydata$SUC, method = "pearson")

# Parson's R is low and not statisticially significant at the 0.1 level

## plot this

par(mfrow=c(1, 2))
xy.plot(mydata$CUST, mydata$SUC, necessity=FALSE, ylab = "SUC", xlab = "CUST" )
xy.plot(mydata$CUST, 1-mydata$SUC, necessity=FALSE, ylab = "~SUC", xlab = "CUST" )

#### successful policy implementation SUC########

### truth table analysis

ttSUC <- truthTable(data=mydata, outcome = "SUC", 	
                        conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                        incl.cut=0.71, sort.by="incl, n", complete=TRUE, show.cases=FALSE)
ttSUC

# Cosistencies and PRIs are generally high (the outcome is a bit skewed  toward positive values), making it hard to find a raw consistncy threshold. 
#plot the truth table rows

psOUTCOME <- eqmcc(ttSUC, include="?", details=TRUE, show.cases=TRUE,  row.dom=TRUE, all.sol=FALSE)
psOUTCOME

par(mfrow=c(1, 1))
pimplot(data=mydata, results=psOUTCOME, incl.tt=0.7, outcome= "SUC")

# I will base my decisions partly on the ratio of consistent cases/ deviant cases consistency in kind 

write.csv(ttSUC$tt, "mytt.csv")

##### Option 1: the raw consistency threshold where PRI decends markdly is including row 63 and excluding row 64 (50% contradictory cases) is 0.859####

ttSUC <- truthTable(data=mydata, outcome = "SUC", 	
                    conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                    incl.cut=0.859, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttSUC
#"-, 1, -, 1, 1, 0"



# conservative solution
csSUC <- eqmcc(ttSUC, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC


# intermediate solution
isSUC <- eqmcc(ttSUC, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC

# parsimonious solution

psSUC <- eqmcc(ttSUC, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC


# number of cases covered

solSUC1 <- compute("cust*CENT*enf*COH*res + CUST*cent*ENF*coh*RES + CUST*TRACT*cent*coh*RES + CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res", data=mydata)

NsolSUC1 <- as.numeric(solSUC1 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC1)

###### Option 2: alternative threshold 0.819 that includes three more rows with no DCCK , while excluding all rows with more than 25% DCCK .#####

ttSUC2 <- truthTable(data=mydata, outcome = "SUC", 	
                    conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                    incl.cut=0.819, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttSUC2

#exclude rows 64, 59, 46, 51, and 3,
ttSUC2$tt[c('64','59','46', '51', '3'), "OUT"] <- 0
ttSUC2


# conservative solution
csSUC2 <- eqmcc(ttSUC2, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC2


# intermediate solution
isSUC2 <- eqmcc(ttSUC2, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC2



# parsimonious solution

psSUC2 <- eqmcc(ttSUC2, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC2

# number of covered cases

solSUC2 <- compute("cust*CENT*enf*COH*res + CUST*cent*ENF*coh*RES + CUST*TRACT*cent*coh*RES +
    CUST*TRACT*cent*ENF*coh + CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES +
                   cust*TRACT*enf*COH*res + TRACT*cent*ENF*coh*RES + TRACT*CENT*ENF*COH*res +
                   cust*tract*CENT*ENF*COH*RES", data=mydata)

NsolSUC2 <- as.numeric(solSUC2 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC2)

##### Option 3: Raw consistency threshold 0.819, excluding rows '64','59','46', '51', '3' and 27####

ttSUC3 <- truthTable(data=mydata, outcome = "SUC", 	
                     conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                     incl.cut=0.819, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttSUC3

ttSUC3$tt[c('64','59','46', '51', '3', '27'), "OUT"] <- 0
ttSUC3


# conservative solution
csSUC3 <- eqmcc(ttSUC3, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC3


# intermediate solution
isSUC3 <- eqmcc(ttSUC3, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC3


# parsimonious solution

psSUC3 <- eqmcc(ttSUC3, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC3

# number of covered cases

solSUC3 <- compute("CUST*cent*ENF*coh*RES + CUST*TRACT*cent*coh*RES + CUST*TRACT*cent*ENF*coh +
    CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES + TRACT*cent*ENF*coh*RES +
                   TRACT*CENT*ENF*COH*res + cust*tract*CENT*enf*COH*res +
                   cust*tract*CENT*ENF*COH*RES + cust*TRACT*cent*enf*COH*res", data=mydata)

NsolSUC3 <- as.numeric(solSUC3 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC3)

#### option 4: only rows with 0 DCCK:  0.781, excluding rows 27, 63, 64, 59, 46, 51, 3, 32, 48, 62, 23, 61, 15, 49#####
ttSUC4 <- truthTable(data=mydata, outcome = "SUC", 	
                     conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                     incl.cut=0.781, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttSUC4

ttSUC4$tt[c('27','63', '64', '59', '46', '51', '3', '32', '48', '62', '23', '61','15', '49'), "OUT"] <- 0
ttSUC4


# conservative solution
csSUC4 <- eqmcc(ttSUC4, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC4


# intermediate solution
isSUC4 <- eqmcc(ttSUC4, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC4


# parsimonious solution

psSUC4 <- eqmcc(ttSUC4, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC4

# number of covered cases

solSUC4 <- compute("TRACT*cent*ENF*coh + CUST*cent*ENF*coh*RES + CUST*TRACT*cent*coh*RES +
    CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES + cust*tract*cent*enf*coh*RES +
                   cust*tract*CENT*enf*COH*res + cust*tract*CENT*ENF*COH*RES +
                   cust*TRACT*cent*enf*COH*res + cust*TRACT*CENT*ENF*COH*res ", data=mydata)

NsolSUC4 <- as.numeric(solSUC4 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC4)


#####option 5 all rows with less than 25% DCCK are included (raw consistency threshold 0.781, excluding rows 27, 64, 59, 46, 51, 3, 32, 48, 62, 23, 61, 15, 49).######

ttSUC5 <- truthTable(data=mydata, outcome = "SUC", 	
                     conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                     incl.cut=0.781, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttSUC5

ttSUC5$tt[c('27', '64', '59', '46', '51', '3', '32', '48', '62', '23', '61','15', '49'), "OUT"] <- 0
ttSUC5


# conservative solution
csSUC5 <- eqmcc(ttSUC5, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC5


# intermediate solution
isSUC5 <- eqmcc(ttSUC5, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC5


# parsimonious solution

psSUC5 <- eqmcc(ttSUC5, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC5


# parsimonious solution

psSUC4 <- eqmcc(ttSUC4, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC4

# number of covered cases

solSUC5 <- compute("TRACT*cent*ENF*coh + CUST*cent*ENF*coh*RES + CUST*TRACT*cent*coh*RES +
    CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES + TRACT*CENT*ENF*COH*res +
    cust*tract*cent*enf*coh*RES + cust*tract*CENT*enf*COH*res +
    cust*tract*CENT*ENF*COH*RES + cust*TRACT*cent*enf*COH*res ", data=mydata)

NsolSUC5 <- as.numeric(solSUC5 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC5)

###### option 6: all rows with 25% DCCK or less are included (raw consistency threshold 0.781, excluding rows 64, 59, 46, 51, 3, 32, 48, 62, 23, 61, 15, 49). #####

ttSUC6 <- truthTable(data=mydata, outcome = "SUC", 	
                     conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                     incl.cut=0.781, sort.by="incl, n", complete=FALSE, show.cases=TRUE)
ttSUC6

ttSUC6$tt[c('64', '59', '46', '51', '3', '32', '48', '62', '23', '61','15', '49'), "OUT"] <- 0
ttSUC6


# conservative solution
csSUC6 <- eqmcc(ttSUC6, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE)
csSUC6


# intermediate solution
isSUC6 <- eqmcc(ttSUC6, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 1, -, 1, 1, 0")
isSUC6


# parsimonious solution

psSUC6 <- eqmcc(ttSUC6, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
psSUC6


# number of covered cases

solSUC6 <- compute("TRACT*cent*ENF*coh + cust*CENT*enf*COH*res + CUST*cent*ENF*coh*RES +
    CUST*TRACT*cent*coh*RES + CUST*tract*CENT*ENF*res + CUST*TRACT*cent*ENF*RES +
                   cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + cust*tract*cent*enf*coh*RES +
                   cust*tract*CENT*ENF*COH*RES ", data=mydata)


NsolSUC6 <- as.numeric(solSUC6 > 0.5 & mydata$SUC > 0.5)
sum(NsolSUC6)

## option 6 ranks best.


##plot the intermediate solution
# CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res)

par(mfrow=c(1, 1))
pimplot(data=mydata, results=isSUC6, outcome = "SUC", intermed=TRUE, sol=1)

myx <- compute("CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res", data=mydata)
xy.plot(myx, mydata$SUC, ylab = "Successful implementation SUC", xlab = "Intermediate solution terms", case.lab = TRUE, labs = rownames(mydata))


# calculate simplifying assumptions and easy counterfactuals
SAOUTCOME <- psSUC6$SA
SAOUTCOME

ECOUTCOME <- isSUC6$i.sol$C1P1$EC
ECOUTCOME


###### unsuccessful policy implementation suc######

ttsuc <- truthTable(data=mydata, outcome = "~SUC", 	
                    conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                    incl.cut=0.7, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttsuc

# Consistencies and PRIs tend to be low. Plot the turth table rows



pssuc <- eqmcc(ttsuc, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
pssuc

write.csv2(ttsuc$tt, "myttcust_final.csv")

par(mfrow=c(1, 1))
pimplot(data=mydata, results=pssuc, incl.tt=0.330, outcome= "SUC")

##### option 1: threshold 0.774#####

ttsuc <- truthTable(data=mydata, outcome = "~SUC", 	
                    conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                    incl.cut=0.774, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttsuc

# exclude untenable assumptions


# CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res


rows <- findRows("CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res", ttsuc, remainders = FALSE)
rows
tt1suc <- ttsuc
tt1suc$tt[as.character(rows), "OUT"] <- 0
tt1suc


# conservative solution
cssuc <- eqmcc(ttsuc, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE, omit = rows)
cssuc


# intermediate solution
issuc <- eqmcc(ttsuc, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 0, -, 0, 0, 1", omit = rows)
issuc

# enhanced parsimonious solution

epssuc <- eqmcc(ttsuc, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, omit = rows)
epssuc


# number of covered cases

solsuc1 <- compute("cust*CENT*coh + CUST*CENT*enf + TRACT*CENT*coh*RES + cust*TRACT*enf*coh*RES
                   ", data=mydata)


Nsolsuc1 <- as.numeric(solsuc1 > 0.5 & mydata$SUC < 0.5)
sum(Nsolsuc1)

# number of DCCK
Nsolsuc1 <- as.numeric(solsuc1 > 0.5 & mydata$SUC > 0.5)
sum(Nsolsuc1)


###### option 2: threshold 0.75, omit rows 61, 49, 21#####

ttsuc2 <- truthTable(data=mydata, outcome = "~SUC", 	
                    conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                    incl.cut=0.75, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttsuc2

# exclude rows 21, 49, 61

ttsuc2$tt[c('61','49', '21'), "OUT"] <- 0
ttsuc2

# parsimonious solution


pssuc2 <- eqmcc(ttsuc2, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
pssuc2

# exclude untenable assumptions
isSUC6

# CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res

rows <- findRows("CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res", ttsuc, remainders = FALSE)
rows


# conservative solution
cssuc2 <- eqmcc(ttsuc2, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE, omit = rows)
cssuc2


# intermediate solution
issuc2 <- eqmcc(ttsuc2, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 0, -, 0, 0, 1", omit = rows)
issuc2

# enhanced parsimonious solution

epssuc2 <- eqmcc(ttsuc2, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, omit = rows)
epssuc2



# number of covered cases

solsuc2 <- compute(" cust*CENT*coh + CUST*enf*COH*res + TRACT*CENT*coh*RES + cust*TRACT*CENT*RES +
       cust*TRACT*enf*coh*RES", data=mydata)


Nsolsuc2 <- as.numeric(solsuc2 > 0.5 & mydata$SUC < 0.5)
sum(Nsolsuc2)

# number of DCCK
Nsolsuc2 <- as.numeric(solsuc2 > 0.5 & mydata$SUC > 0.5)
sum(Nsolsuc2)


###### option 3: threshold 0.7, omit rows 61, 49, 21, 53, 16, 37, 22, 46, 64, 45#####

ttsuc3 <- truthTable(data=mydata, outcome = "~SUC", 	
                     conditions = "CUST, TRACT, CENT, ENF, COH, RES", 
                     incl.cut=0.7, sort.by="incl, n", complete=FALSE, show.cases=FALSE)
ttsuc3

# exclude rows 1, 49, 21, 53, 16, 37, 22, 46, 64, 45

ttsuc3$tt[c('61','49', '21', '53', '16', '37', '22', '46', '64', '45'), "OUT"] <- 0
ttsuc3

# parsimonious solution


pssuc3 <- eqmcc(ttsuc3, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE)
pssuc3

# exclude untenable assumptions
isSUC6

# CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res

rows <- findRows("CUST*cent*coh*RES + tract*cent*coh*RES + TRACT*cent*ENF*coh + TRACT*cent*ENF*RES + cust*CENT*enf*COH*res + cust*tract*ENF*COH*RES + cust*TRACT*enf*COH*res + TRACT*CENT*ENF*COH*res + CUST*tract*CENT*ENF*res", ttsuc, remainders = FALSE)
rows


# conservative solution
cssuc3 <- eqmcc(ttsuc3, details=TRUE, show.cases=TRUE, row.dom=TRUE, all.sol=FALSE, use.tilde=FALSE, omit = rows)
cssuc3


# intermediate solution
issuc3 <- eqmcc(ttsuc3, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, dir.exp = "-, 0, -, 0, 0, 1", omit = rows)
issuc3

# enhanced parsimonious solution

epssuc3 <- eqmcc(ttsuc3, details=TRUE, show.cases=TRUE, row.dom=TRUE,  use.tilde=FALSE, include = "?", all.sol=FALSE, omit = rows)
epssuc3



# number of covered cases

solsuc3 <- compute("cust*CENT*coh + CUST*enf*COH + cust*TRACT*CENT*RES + TRACT*CENT*coh*RES +
       cust*TRACT*enf*RES + tract*cent*enf*res", data=mydata)


Nsolsuc3 <- as.numeric(solsuc3 > 0.5 & mydata$SUC < 0.5)
sum(Nsolsuc3)

# number of DCCK
Nsolsuc3 <- as.numeric(solsuc3 > 0.5 & mydata$SUC > 0.5)
sum(Nsolsuc3)


##plot the intermediate solution
# cust*CENT*coh + CUST*enf*COH*res + TRACT*CENT*coh*RES + cust*TRACT*CENT*RES + cust*TRACT*enf*coh*RES
par(mfrow=c(1, 1))
pimplot(data=mydata, results=issuc2, outcome = "~SUC", intermed=TRUE, sol=1)

myx <- compute("cust*CENT*coh + CUST*enf*COH*res + TRACT*CENT*coh*RES + cust*TRACT*CENT*RES + cust*TRACT*enf*coh*RES ", data=mydata)
xy.plot(myx, 1-mydata$SUC, ylab = "Unsuccessful implementation suc", xlab = "Intermediate solution terms", case.lab = TRUE, labs = rownames(mydata))


# calculate simplifying assumptions and easy counterfactuals
SAOUTCOME <- epssuc2$SA
SAOUTCOME

ECOUTCOME <- issuc2$i.sol$C1P1$EC
ECOUTCOME

###post-qca case selection####

# typical cases 

cases.suf.typ.most(results = isSUC6, outcome = "SUC", neg.out=FALSE, intermed=TRUE, sol=1)

cases.suf.typ.most(results = issuc2, outcome = "SUC", neg.out=TRUE, intermed=TRUE, sol=1)


# match for deviant cases consistency

matches.suf.typdcn(results = isSUC6, outcome = "SUC", neg.out=FALSE, intermed=TRUE, sol=1)
matches.suf.typdcn(results = issuc2, outcome = "~SUC", neg.out=TRUE, intermed=TRUE, sol=1)


# match deviant cases coverage

matches.suf.dcviir(results = isSUC6, outcome = "SUC", neg.out=FALSE, intermed=TRUE, sol=1)

matches.suf.dcviir(results = issuc2, outcome = "~SUC", neg.out=TRUE, intermed=TRUE, sol=1)
