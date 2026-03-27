#### ARF intermediaries study 2 #####

# Clear  workspace 

rm(list = ls())

### Set  Working directory 
# load packages

library(psych); 
library(base); 
library(QCA); 
library(admisc); 
library(SetMethods)
library(stargazer)

setwd("C:/Users/ethof/Dropbox/Arbeit/Publikationen/Papers/ARF intermediaries/Data and analyses/Analyses new 250422")

#####dataset 1#######

mydatabig <- read.csv("prevent_survey_final_dataset.csv", row.names=1, header = TRUE)
mydatabig 

#subset relevant variables
colnames(mydatabig)
mydata1 <- mydatabig[,c("gender_binary", "age", "training_selfguided", "training_seminar", "training_independent", "training_unsure", "training_notraining","prevent_familiarity", "rule_pressure","peer_pressure","societal_pressure","market_pressure", "political_pressure",  "professional_dilemma", "societal_dilemma","market_dilemma", "political_dilemma", "willingness","likelihood_to_report", "action_other", "action_no_further_new", "action_formal_referral_new", "action_informal_colleague_new", "action_private_chat_new", "action_other_new", "action_exposure_opinions", "action_check_procedures", "action_educate_oneself", "action_monitor")]

colnames(mydata1)

# build training variable binary
mydata1$TRAIN <- 0
mydata1$TRAIN[mydata1$training_selfguided ==1] <- 1
mydata1$TRAIN[mydata1$training_seminar==1] <- 1
mydata1$TRAIN


###calibration dataset 1####

# descriptive statistics dataset 1

describe(mydata1)
stargazer(mydata1, type="html", summary=TRUE, out="des.html")

# calibrate compliance binary
mydata1$comp <- 0
mydata1$comp[mydata1$likelihood_to_report > 4] <- 1
describe(mydata1$comp)

# calibrate rule pressure binary
mydata1$rule <- 0
mydata1$rule[mydata1$rule_pressure > 4] <- 1
describe(mydata1$rule)

# calibrate rule-professional dilemma binary
mydata1$dprof <- 0
mydata1$dprof[mydata1$professional_dilemma > 4] <- 1
describe(mydata1$dprof)

# calibrate rule-societal dilemma binary
mydata1$dsoc <- 0
mydata1$dsoc[mydata1$societal_dilemma > 4] <- 1
describe(mydata1$dsoc)

# calibrate rule-political dilemma binary
mydata1$dpol <- 0
mydata1$dpol[mydata1$political_dilemma > 4] <- 1
describe(mydata1$dpol)

#calibrate fuzzy sets
mydata1$COMP <- calibrate(mydata1$likelihood_to_report, type = "fuzzy", thresholds = "e=1, c=4.5, i=7")
mydata1$COMP
describe(mydata1$COMP)

mydata1$RULE <- calibrate(mydata1$rule_pressure, type = "fuzzy", thresholds = "e=1, c=4.5, i=7")
mydata1$RULE
describe(mydata1$RULE)

mydata1$DSOC <- calibrate(mydata1$societal_dilemma, type = "fuzzy", thresholds = "e=1, c=4.5, i=7")
mydata1$DSOC
describe(mydata1$DSOC)

mydata1$DPROF <- calibrate(mydata1$professional_dilemma, type = "fuzzy", thresholds = "e=1, c=4.5, i=7")
mydata1$DPROF
describe(mydata1$DPROF)


mydata1$DPOL <- calibrate(mydata1$political_dilemma, type = "fuzzy", thresholds = "e=1, c=4.5, i=7")
mydata1$DPOL
describe(mydata1$DPOL)

#save calibrated dataset excluding NAs

mydata2 = mydata1[, c("COMP", "RULE", "TRAIN", "DSOC", "DPROF", "DPOL")]
nomissings <- as.numeric(complete.cases(mydata2))
mydata3 <- mydata2[nomissings== 1,]
 
write.csv2(mydata3, "dataset1.csv")

#calculate tendency to the middle
checkcomp <- as.numeric(mydata1$likelihood_to_report == 4)
prop.table(table(checkcomp))

checkrule <- as.numeric(mydata1$rule_pressure == 4)
prop.table(table(checkrule))

checkdsoc <- as.numeric(mydata1$societal_dilemma == 4)
prop.table(table(checkdsoc))

checkdprof <- as.numeric(mydata1$professional_dilemma == 4)
prop.table(table(checkdprof))

checkdpol <- as.numeric(mydata1$political_dilemma == 4)
prop.table(table(checkdpol))

###replicate results######

mydata <- read.csv2("dataset1.csv", row.names=1, header = TRUE)
mydata

#necessary conditions 
nc1 <- compute(expression="TRAIN + ~DPROF", data=mydata)
nc1
QCAfit(nc1, mydata$COMP, necessity=TRUE)
nc2 <- compute(expression="TRAIN + ~DSOC", data=mydata)
nc2
QCAfit(nc2, mydata$COMP, necessity=TRUE)
nc3 <- compute(expression="TRAIN + ~DPOL", data=mydata)
nc3
QCAfit(nc3, mydata$COMP, necessity=TRUE)

nc4 <- compute(expression="DPROF + DPOL", data=mydata)
nc4
QCAfit(nc4, mydata$COMP, necessity=TRUE, neg.out=TRUE)

##sufficient conditions for COMP
sc1 <- compute(expression="TRAIN*~DPROF*~DPOL", data=mydata)
sc1
QCAfit(sc1, mydata$COMP, necessity=FALSE)

sc2 <- compute(expression="TRAIN*~DSOC", data=mydata)
sc2
QCAfit(sc2, mydata$COMP, necessity=FALSE)

sc3 <- compute(expression="~DPROF*~DSOC*~DPOL", data=mydata)
sc3
QCAfit(sc3, mydata$COMP, necessity=FALSE)


sc4 <- compute(expression="TRAIN*RULE*DPOL", data=mydata)
sc4
QCAfit(sc4, mydata$COMP, necessity=FALSE)

sc5 <- compute(expression="TRAIN*RULE*DPOL+~DPROF*~DSOC*~DPOL+TRAIN*~DPROF*~DPOL+TRAIN*~DSOC", data=mydata)
sc5
QCAfit(sc5, mydata$COMP, necessity=FALSE)

##sufficient conditions for ~COMP
sc6 <- compute(expression="~TRAIN*DPROF*DPOL", data=mydata)
sc6
QCAfit(sc6, mydata$COMP, necessity=FALSE, neg.out=TRUE)

sc7 <- compute(expression="DPROF*DSOC*~DPOL", data=mydata)
sc7
QCAfit(sc7, mydata$COMP, necessity=FALSE, neg.out=TRUE)

sc8 <- compute(expression="~RULE*~DPROF*DSOC*DPOL", data=mydata)
sc8
QCAfit(sc8, mydata$COMP, necessity=FALSE, neg.out=TRUE)

sc9 <- compute(expression="~RULE*~DPROF*DSOC*DPOL+DPROF*DSOC*~DPOL+~TRAIN*DPROF*DPOL", data=mydata)
sc9
QCAfit(sc9, mydata$COMP, necessity=FALSE, neg.out=TRUE)

###truth table analysis, dataset 1#####
#build truth table, raw consistency threshold 0.8

ttOUT <- truthTable(data=mydata, outcome = "COMP", 	
                    conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                    sort.by="incl, n", incl.cut=0.8,complete = FALSE)
ttOUT


# parsimonious solution
psOUT <- minimize(ttOUT, include="?", details=TRUE)
psOUT

# export results table
stargazerSol(results = psOUT, outcome = "COMP",
             type = "html", show.cases=TRUE, out = "psOUTCOME1.html")


####dataset 2#######

##########calibration diagnostics#############
#Load the crisp dataset

mydata <- read.csv2("intermediaries_crisp.csv", row.names=1, header = TRUE)
mydata
write.csv2(mydata, "dataset2.csv")

#skewness check

skew.check(mydata)

# reference to societal pressure is present in all cases. This condition will therefore not be included in the analyses of subset relations.

# descriptive statistics
describe(mydata)

stargazer(mydata, type="html", summary=TRUE, out="des.html")

###ANALYSIS OF NECESSITY#####

names(mydata)
# COMP
NC1 <- superSubset(mydata, outcome = "COMP", 
            conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
            incl.cut =1, cov.cut = 0.5, ron.cut=0.5)
NC1

#export results
write.csv2(NC1$incl.cov, "necOUT.csv")

#plot results
pimplot(data=mydata, results=NC1, outcome = "COMP", necessity=TRUE, all_labels = TRUE, crisp=TRUE)

# ~COMP
NC2 <- superSubset(mydata, outcome = "COMP", 
                   conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                   incl.cut = 1, cov.cut = 0.5, ron.cut=0.5, neg.out=TRUE)
NC2

# plot necessity results
pimplot(data=mydata, results=NC2, outcome = "COMP", necessity=TRUE, all_labels = TRUE, crisp=TRUE)

#export results
write.csv2(NC2$incl.cov, "necNOUT.csv")

####ANALYSIS OF SUFFICIENCY######

mydata <- read.csv2("intermediaries_crisp.csv", row.names=1, header = TRUE)
mydata

##### TRUTH TABLE ANALYSIS########

#####COMP#####
##construct the truth table for COMP (raw consistency threshold: 1)


ttOUT <- truthTable(data=mydata, outcome = "COMP", 	
                    conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                    sort.by="incl, n", incl.cut=1,show.cases=TRUE, complete = FALSE)
ttOUT
write.csv2(ttOUT$tt, "ttOUT.csv")

# conservative solution
csOUT <- minimize(ttOUT, details=TRUE, show.cases=TRUE)
csOUT

# parsimonious solution
psOUT <- minimize(ttOUT, include="?", details=TRUE, show.cases=TRUE)
psOUT # two parsimonious models that differ on 1 out of 3 path; model 1 is theoretically more plausible

# intermediate solution
# directional expectations: TRAIN -> COMP, RULE -> COMP, ~DPROF -> COMP, ~DSOC -> COMP, ~DPOL -> COMP

isOUT <- minimize(ttOUT, include = "?", details=TRUE, show.cases=TRUE,   dir.exp = "TRAIN, RULE, ~DPROF, ~DSOC, ~DPOL")
isOUT # 3 models, marginal difference, M1 theoretically most plausible and most parsimonious


# enhanced truth table

NC1
ettOUT <- esa(ttOUT, nec_cond =c("TRAIN + ~DPROF", "TRAIN + ~DSOC", "TRAIN + ~DPOL"))
ettOUT


# enhanced parsimonious solution
epsOUT <- minimize(ettOUT, include="?", details=TRUE, show.cases=TRUE)
epsOUT 
psOUT

# enhanced intermediate solution
# directional expectations: ~TRAIN -> ~COMP, ~RULE -> ~COMP,  DPROF -> ~COMP, DSOC -> ~COMP, DPOL -> ~COMP

eisOUT <- minimize(ettOUT, include = "?", details=TRUE, show.cases=TRUE,   dir.exp = "TRAIN, RULE, ~DPROF, ~DSOC, ~DPOL")
eisOUT 
isOUT
# ESA eliminates all model ambiguities


#export solution

#stargazerSol(results = eisOUT$i.sol$C1P1, outcome = "COMP",
             type = "html", show.cases=TRUE, out = "isOUT.html")


#####~COMP######
##construct the truth table for ~COMP (raw consistency threshold: 1)
##see DCCKs

ttNOUT <- truthTable(data=mydata, outcome = "~COMP", 	
                    conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                    sort.by="incl, n", incl.cut=0.8,show.cases=TRUE, complete = FALSE, dcc=TRUE)
ttNOUT 

# normal view
ttNOUT <- truthTable(data=mydata, outcome = "~COMP", 	
                     conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                     sort.by="incl, n", incl.cut=1,show.cases=TRUE, complete = TRUE)
ttNOUT

write.csv2(ttNOUT$tt, "ttNOUT.csv")


# conservative solution
csNOUT <- minimize(ttNOUT, details=TRUE, show.cases=TRUE)
csNOUT

# parsimonious solution
psNOUT <- minimize(ttNOUT, include="?", details=TRUE, show.cases=TRUE)
psNOUT # 2 almost identical models with PS

# intermediate solution
# directional expectations: ~TRAIN -> ~COMP, ~RULE -> ~COMP,  DPROF -> ~COMP, DSOC -> ~COMP, DPOL -> ~COMP

isNOUT <- minimize(ttNOUT, include = "?", details=TRUE, show.cases=TRUE,   dir.exp = "~TRAIN, ~RULE, DPROF, DSOC, DPOL")
isNOUT # model ambiguity disappears

# enhanced truth table
#identify contradictory simplifying assumptions


SAOUT <- psOUT$SA$M1
SAOUT

SAout <- psNOUT$SA$M1
SAout

CAs <- intersect(rownames(SAOUT), rownames(SAout))
CAs

eisOUT
ettNOUT <- esa(ttNOUT, nec_cond ="DPROF + DPOL", untenable_LR=c("TRAIN*~DSOC", "TRAIN*RULE*DPOL"," TRAIN*~DPROF*~DPOL", "~DPROF*~DSOC*~DPOL"))
ettNOUT 

# there is an issue with the untenable_LR option. cannot use + 
?esa

# enhanced parsimonious solution
epsNOUT <- minimize(ettNOUT, include="?", details=TRUE, show.cases=TRUE)
epsNOUT 
psNOUT

# enhanced intermediate solution
# directional expectations: ~TRAIN -> ~COMP, ~RULE -> ~COMP,  DPROF -> ~COMP, DSOC -> ~COMP, DPOL -> ~COMP

eisNOUT <- minimize(ettNOUT, include = "?", details=TRUE, show.cases=TRUE,   dir.exp = "~TRAIN, ~RULE, DPROF, DSOC, DPOL")
eisNOUT 
isNOUT

# SA and ESA yield the same IS

#export solution

#stargazerSol(results = eisNOUT, outcome = "~COMP",
             type = "html", show.cases=TRUE, out = "isNOUT.html")

######COUNTERFACTUAL ASSUMPTIONS#########

####identify and export simplifying and contradictory assumptions

SAOUT <- psOUT$SA$M1
SAOUT

ECOUT <- eisOUT$i.sol$C1P1$EC
ECOUT
write.csv(SAOUT$M1, "SAOUT.csv")
write.csv(ECOUT, "ECOUT.csv")

SANOUT <- psNOUT$SA$M1
SANOUT

ECNOUT <- eisNOUT$i.sol$C1P1$EC
ECNOUT
write.csv(SANOUT$M1, "SANOUT.csv")
write.csv(ECNOUT, "ECNOUT.csv")

###SMMR######

typOUT <- smmr(results = eisOUT,
               outcome = "COMP" ,
               match = FALSE ,
               cases = 1,
               max_pairs = 5)
typOUT

##for COMP, the uniquely covered most typical cases are:
# 3 ~DPROF*~DSOC*~DPOL
# 13 or 17 TRAIN*~DPROF*~DPOL
# 8        TRAIN*~DSOC
# 16 or 21   TRAIN*RULE*DPOL

typNOUT <- smmr(results = eisNOUT,
               outcome = "~COMP" ,
               match = FALSE ,
               cases = 1,
               max_pairs = 5)
typNOUT

##for ~COMP, the uniquely covered most typical cases are:
# 9 ~RULE*~DPROF*DSOC*DPOL
# 7,18, 29, 31 or 35      ~TRAIN*DPROF*DPOL
# 6 or 34      DPROF*DSOC*~DPOL

#pair deviant coverage--IIR case for comparison (identify omitted conjunction)

pairOUT <- smmr(results = eisOUT,
                outcome = "COMP" ,
                match = TRUE ,
                cases = 4,
                term = 1,
                max_pairs = 2)
pairOUT

# compare deviant case coverage for COMP 11 with IIR case 2

pairNOUT <- smmr(results = eisNOUT,
                outcome = "~COMP" ,
                match = TRUE ,
                cases = 4,
                term = 1,
                max_pairs = 2)
pairNOUT

# compare deviant case coverage for ~COMP 2 with IIR case 11


###VISUALIZATION#####
# plot solution terms
pimplot (data =  mydata,
         outcome = "COMP" ,
         results = eisOUT,
         all_labels = TRUE,
         jitter = TRUE,
         fontsize = 6, crisp=TRUE)


#####Theory evaluation####

#theoretical expectations in expressions 1 and 2

TCOMP <- "TRAIN + RULE + ~DPROF *~DSOC*~DPOL"
TnCOMP <- "~TRAIN + ~RULE + DPROF* DSOC  + DPOL* DSOC + DPROF*DPOL"

# Theory evaluation for COMP

TECOMP <- theory.evaluation(theory= TCOMP, empirics = eisOUT, outcome = "COMP", print.fit = FALSE, print.data=FALSE)
TECOMP

TENCOMP <- theory.evaluation(theory= TnCOMP, empirics = eisNOUT, outcome = "~COMP", print.fit = FALSE, print.data=FALSE)
TENCOMP

####robustness test#####

####sensitivity range
conds <- c("TRAIN", "RULE", "DPROF", "DSOC", "DPOL")
rob.inclrange(data=mydata, step= 0.01, max.runs=80, outcome ="COMP", conditions=conds, incl.cut=1, n.cut=1, include="?")
rob.inclrange(data=mydata, step= 0.01, max.runs=80, outcome ="~COMP", conditions=conds, incl.cut=1, n.cut=1, include="?")

#derive parsimonious solutions including row 24 for both outcomes
#COMP
ttOUT3 <- truthTable(data=mydata, outcome = "COMP", 	
                    conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                    sort.by="incl, n", incl.cut=0.5,show.cases=TRUE, complete = FALSE)
ttOUT3


# parsimonious solution COMP
psOUT3 <- minimize(ttOUT3, details=TRUE, show.cases=TRUE, include = "?")
psOUT3

#~COMP
ttOUT4 <- truthTable(data=mydata, outcome = "~COMP", 	
                     conditions = "TRAIN, RULE, DPROF, DSOC, DPOL", 
                     sort.by="incl, n", incl.cut=0.5,show.cases=TRUE, complete = FALSE)
ttOUT4

# parsimonious solution ~COMP
psOUT4 <- minimize(ttOUT4, details=TRUE, show.cases=TRUE, include = "?")
psOUT4

### fit-oriented robustness COMP
IS <- isOUT
TS <- psOUT3
?rob.fit

RF <-rob.fit(test_sol=TS, initial_sol=IS, outcome="COMP")
RF

#case-oriented robustness COMP
rob.xyplot(test_sol=TS, initial_sol=IS, outcome="COMP", all_labels=TRUE, fontsize=3.5, jitter=TRUE, area_lab=TRUE)
rob.cases(test_sol=TS, initial_sol=IS, outcome="COMP")


###fit-oriented robustness ~COMP
IS1 <- isNOUT
TS1 <- psOUT4

RF1 <-rob.fit(test_sol=TS1, initial_sol=IS1, outcome="~COMP")
RF1

#case-oriented robustness ~COMP
rob.xyplot(test_sol=TS1, initial_sol=IS1, outcome="~COMP", all_labels=TRUE, fontsize=3.5, jitter=TRUE, area_lab=TRUE)
rob.cases(test_sol=TS1, initial_sol=IS1, outcome="~COMP")
