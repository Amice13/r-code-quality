                                                 
################################################
### SURVEY RESULTS FROM ITALY - DEC 17, 2016 ###
################################################

rm(list=ls(all=TRUE))
detach(ITA.survey) 
detach(subset.poor)
detach(subset.rich)

### Load libraries
library(arm)
library(interplot)
library(stargazer)
library(Zelig)
library(ZeligChoice)
library(mediation)
library(lavaan)
library(plyr)


### Read and attach data

ITA.survey <- read.csv("C:/Users/gmagni/OneDrive/DISS-PAPERS/1-MainExper/00-Latest/z-BJPS/R&R/0-Resubmit/Copyediting/SurveyExp_ITA__FINAL.csv")


ls(ITA.survey)
attach(ITA.survey)
n = nrow(ITA.survey)


##############

# Subset for those who passed attention check
detach(subset.att)
table(ITA.survey$Referndum)
ITA.survey$Referndum <- as.numeric(as.character(ITA.survey$Referndum))
table(ITA.survey$Referndum)
subset.att <- subset(ITA.survey, Referndum == "0" | Referndum == "1"| Referndum == "2")
table(subset.att$Referndum)
attach(subset.att)
n = nrow(subset.att)




################

#Subset: eliminating 5% slowest/fastest 
table(ITA.survey$Length)
ITA.survey$Length <- as.numeric(as.character(ITA.survey$Length))
q<-quantile(ITA.survey$Length, 0.025, na.rm=TRUE)
q2<-quantile(ITA.survey$Length, 0.975, na.rm=TRUE)
#ITA.survey <- subset(ITA.survey, ITA.survey$Length >q)
ITA.survey <- subset(ITA.survey, ITA.survey$Length >q & ITA.survey$Length <q2)
attach(ITA.survey)
n = nrow(ITA.survey)


################

#Subset: poor and rich

#Poor (household income)
table(ITA.survey$IncomeHouse)
ITA.survey$IncomeHouse <- as.numeric(as.character(ITA.survey$IncomeHouse))
table(ITA.survey$IncomeHouse)
subset.poor <- subset(ITA.survey, IncomeHouse == "1" | IncomeHouse == "2" |
                        IncomeHouse == "3" | IncomeHouse == "4" | IncomeHouse == "5")
table(subset.poor$IncomeHouse)
attach(subset.poor)
n = nrow(subset.poor)

#Rich (household income)
table(ITA.survey$IncomeHouse)
ITA.survey$IncomeHouse <- as.numeric(as.character(ITA.survey$IncomeHouse))
table(ITA.survey$IncomeHouse)
subset.rich <- subset(ITA.survey, IncomeHouse == "9" | IncomeHouse == "10" | IncomeHouse == "11" |
                              IncomeHouse == "12" | IncomeHouse == "13" | IncomeHouse == "14" | IncomeHouse == "16")
table(subset.rich$IncomeHouse)
attach(subset.rich)
n = nrow(subset.rich)


#####

# Subset: Conservatives

table(ITA.survey$PolIdeol.Soc_1)
ITA.survey$PolIdeol.Soc_1 <- as.numeric(as.character(ITA.survey$PolIdeol.Soc_1))
table(ITA.survey$PolIdeol.Soc_1)
subset.cons <- subset(ITA.survey, PolIdeol.Soc_1 == "7" | PolIdeol.Soc_1 == "8" | 
                        PolIdeol.Soc_1 == "9" | PolIdeol.Soc_1 == "10")
table(subset.cons$PolIdeol.Soc_1)
attach(subset.cons)
n = nrow(subset.cons)



## Code variables

table(CONTR.Quest)
table(TR.Ineq.Intro)
table(TR.Pov.Intro)
treat = rep(NA,n)
treat[CONTR.Quest=="1"]="Control"
treat[TR.Ineq.Intro=="1"]="Inequality"
treat[TR.Pov.Intro=="1"]="Poverty"
table(treat)

treat.factor <- factor(treat, levels = c("Control", "Inequality", "Poverty"))
table(treat.factor)

treat.factor.subset <- factor(treat, levels = c("Control", "Inequality"))
table(treat.factor.subset)

treat.factor.2 <- factor(treat, levels = c("Poverty", "Inequality", "Control"))
table(treat.factor.2)


table(IncomePers)
IncomePers[IncomePers=="99"]="NA"
table(IncomePers)

income = rep(NA,n)
income[IncomePers=="1"]=1
income[IncomePers=="2"]=2
income[IncomePers=="3"]=3
income[IncomePers=="4"]=4
income[IncomePers=="5"]=5
income[IncomePers=="6"]=6
income[IncomePers=="7"]=7
income[IncomePers=="8"]=8
income[IncomePers=="9"]=9
income[IncomePers=="10"]=10
income[IncomePers=="14"]=11
income[IncomePers=="11"]=12
income[IncomePers=="12"]=13
table(income)
mean(income, na.rm=TRUE)


income.factor <- factor(income, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                                               "13", "14"))
table(income.factor)

table(IncomeHouse)
IncomeHouse[IncomeHouse=="99"]="NA"
table(IncomeHouse)

income.h = rep(NA,n)
income.h[IncomeHouse=="1"]=1
income.h[IncomeHouse=="2"]=2
income.h[IncomeHouse=="3"]=3
income.h[IncomeHouse=="4"]=4
income.h[IncomeHouse=="5"]=5
income.h[IncomeHouse=="6"]=6
income.h[IncomeHouse=="7"]=7
income.h[IncomeHouse=="8"]=8
income.h[IncomeHouse=="16"]=9
income.h[IncomeHouse=="9"]=10
income.h[IncomeHouse=="10"]=11
income.h[IncomeHouse=="11"]=12
income.h[IncomeHouse=="12"]=13
income.h[IncomeHouse=="13"]=14
income.h[IncomeHouse=="14"]=15
table(income.h)
mean(income.h, na.rm=TRUE)

income.h.poor <- as.numeric(income.h == "1" | income.h == "2" |
                              income.h == "3" | income.h == "4" | income.h == "5")
table(income.h.poor)

income.h.rich <- as.numeric(income.h == "9" | income.h == "10" |
                              income.h == "11" | income.h == "12" | income.h == "13"
                            | income.h == "14" | income.h == "15")
table(income.h.rich)



income.h.factor <- factor(income.h, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
                                               "13", "14", "15", "16"))
table(income.h.factor)


table(HouseComp)
household <- as.numeric(as.character(HouseComp))
table(household)

income.pc <- income.h/household
table(income.pc)

table(Educ)
educ <- as.numeric(as.character(Educ))
table(educ)
prop.table(table(educ))

educ.f <- factor(educ)
table(educ.f)


table(Female)
female = rep(NA,n)
female[Female=="1"]=1
female[Female=="0"]=0
table(female)
prop.table(table(female))

table(Age)
age <- as.numeric(as.character(Age))
table(age)
mean(age, na.rm=TRUE)

table(EconInsec2)
eco.sec <- as.numeric(as.character(EconInsec2))
table(eco.sec)
eco.insec <- 6-eco.sec
table(eco.insec)

table(PolIdeol.Eco_1)
eco.right <- as.numeric(as.character(PolIdeol.Eco_1))
table(eco.right)

table(PolIdeol.Soc_1)
soc.conserv <- as.numeric(as.character(PolIdeol.Soc_1))
table(soc.conserv)
soc.conserv.dummy.6 <- as.numeric(soc.conserv=="6" | soc.conserv=="7" | soc.conserv=="8" | soc.conserv=="9" | soc.conserv=="10")
table(soc.conserv.dummy.6)
soc.conserv.dummy.7 <- as.numeric(soc.conserv=="7" | soc.conserv=="8" | soc.conserv=="9" | soc.conserv=="10")
table(soc.conserv.dummy.7)
soc.conserv.dummy.8 <- as.numeric(soc.conserv=="8" | soc.conserv=="9" | soc.conserv=="10")
table(soc.conserv.dummy.8)
soc.conserv.dummy.9 <- as.numeric(soc.conserv=="9" | soc.conserv=="10")
table(soc.conserv.dummy.9)
soc.conserv.dummy.10 <- as.numeric(soc.conserv=="10")
table(soc.conserv.dummy.10)

table(PartyChoice)
party.choice = rep(NA,n)
party.choice[PartyChoice=="1"]="PD"
party.choice[PartyChoice=="2"]="M5S"
party.choice[PartyChoice=="3"]="FI"
party.choice[PartyChoice=="4"]="LN"
party.choice[PartyChoice=="5"]="FdI"
party.choice[PartyChoice=="6"]="NCD"
party.choice[PartyChoice=="7"]="SI"
party.choice[PartyChoice=="8"]="Other"
table(party.choice)
prop.table(table(party.choice))


table(Referndum)
referendum <- as.numeric(as.character(Referndum))
table(referendum)

refer.part <- as.numeric(referendum=="1" | referendum=="2")
table(refer.part)
prop.table(table(refer.part))


#DVs: Welfare support

table(Redistr)
redistr <- as.numeric(as.character(Redistr))
table(redistr)
prop.table(table(redistr))

redistr.dummy <- as.numeric(redistr=="5")
table(redistr.dummy)
prop.table(table(redistr.dummy))

table(WS.SocialCard)
poor.ita <- as.numeric((as.character(WS.SocialCard)))
table(poor.ita)
prop.table(table(poor.ita))

poor.ita.dummy <- as.numeric(poor.ita=="5")
table(poor.ita.dummy)
prop.table(table(poor.ita.dummy))

poor.ita.dummy.2 <- as.numeric(poor.ita=="4" | poor.ita=="5")
table(poor.ita.dummy.2)

table(WS.SocCardImmigr)
poor.imm <- as.numeric((as.character(WS.SocCardImmigr)))
table(poor.imm)
poor.imm.dummy <- as.numeric(poor.imm=="2" | poor.imm=="3" |
                                      poor.imm == "4" | poor.imm=="5")
table(poor.imm.dummy)
prop.table(table(poor.imm.dummy))

poor.imm.dummy.3 <- as.numeric(poor.imm == "4" | poor.imm=="5")
table(poor.imm.dummy.3)
prop.table(table(poor.imm.dummy.3))



#Issue importance & manipulation checks

table(InequalImport)
ineq.import <- as.numeric(as.character(InequalImport))
table(ineq.import)
prop.table(table(ineq.import))
ineq.import.dummy <- as.numeric(ineq.import=="5")
table(ineq.import.dummy)

table(ManCh.Ineq)
mancheck.ineq <- as.numeric(as.character(ManCh.Ineq))
table(mancheck.ineq)
mancheck.ineq.dummy <- as.numeric(mancheck.ineq=="0")
table(mancheck.ineq.dummy)

table(ManCh.Scarc)
mancheck.pov <- as.numeric(as.character(ManCh.Scarc))
table(mancheck.pov)
mancheck.pov.dummy <- as.numeric(mancheck.pov=="0")
table(mancheck.pov.dummy)

ls(ITA.survey)


# Causal Mechanism

table(ImmDeserv)
imm.undes <- as.numeric(as.character(ImmDeserv))
table(imm.undes)
imm.undes.dummy <- as.numeric(imm.undes == "5")
table(imm.undes.dummy)
imm.des.dummy <- as.numeric(imm.undes.dummy == "0")
table(imm.des.dummy)

table(GroupDeserv_1)
poor.undes <- as.numeric(as.character(GroupDeserv_1))
table(poor.undes)
poor.deserv.dummy <- as.numeric(poor.undes =="1")
table(poor.deserv.dummy)
prop.table(table(poor.deserv.dummy))
poor.undeserv.dummy <- as.numeric(poor.deserv.dummy =="0")
table(poor.undeserv.dummy)


table(ImmPriority)
priority.ita <- as.numeric(as.character(ImmPriority))
table(priority.ita)
priority.ita.dummy <- as.numeric(priority.ita=="5")
table(priority.ita.dummy)


table(Meritocr)
meritocr <- as.numeric(as.character(Meritocr))
table(meritocr)
meritocr.dummy <- as.numeric(meritocr=="2"|meritocr=="3"|meritocr=="4"|meritocr=="5")
table(meritocr.dummy)
meritocr.dummy.2 <- as.numeric(meritocr=="3"|meritocr=="4"|meritocr=="5") #0 is negative view
table(meritocr.dummy.2)
un.meritocr.dummy.2 <- as.numeric(meritocr=="1"|meritocr=="2") #1 is negative view
table(un.meritocr.dummy.2)

lack.merit <- 5 - meritocr
table(lack.merit)

table(Geo.Area.5)




#################
### Analysis ####
#################


## REDISTRIBUTION


#OLS, Logit, Ordered logit (without and with controls): Table 2

#1
redistr.m.ols.nocontrols <- lm(redistr.dummy ~ treat)
summary(redistr.m.ols.nocontrols)

## appendix - with meritocr (online appendix: model 2 page A33)
redistr.m.ols.nocontrols.merit <- lm(redistr.dummy ~ treat + lack.merit)
summary(redistr.m.ols.nocontrols.merit)
##

#2
redistr.m.ols.yescontrols <- lm(redistr.dummy ~ treat + educ + age + female + income.pc + 
                                  eco.right + soc.conserv + party.choice +
                                  Geo.Area.5)
summary(redistr.m.ols.yescontrols)


#3
redistr.m.logit.nocontrols <- glm(redistr.dummy ~ treat, 
                                  family=binomial(link="logit"))
summary(redistr.m.logit.nocontrols)

#4
redistr.m.0.logit <- glm(redistr.dummy ~ treat + educ + age + female + income.pc + 
                           eco.right + soc.conserv + party.choice +
                                      Geo.Area.5, 
                                    family=binomial(link="logit"))
summary(redistr.m.0.logit) 

#5
redistr.m.ologit.nocontrols <- zelig(formula = as.ordered(redistr) ~ treat,
                                     model = "ologit", data = ITA.survey, cite = FALSE)
summary(redistr.m.ologit.nocontrols)

#6
redistr.m.0.ologit <- zelig(formula = as.ordered(redistr) ~ treat + educ + age + female + income.pc + 
                              eco.right + soc.conserv + party.choice +  
                             Geo.Area.5,
                           model = "ologit", data = ITA.survey, cite = FALSE)
summary(redistr.m.0.ologit) 


stargazer(redistr.m.ols.nocontrols, redistr.m.ols.yescontrols, 
          redistr.m.logit.nocontrols, redistr.m.0.logit,
          redistr.m.ologit.nocontrols, redistr.m.0.ologit,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")



########


## LOW-INCOME ITALIANS (NATIVES) and IMMIGRANTS 


## OLS, logit, oredered logit: without and with controls: Table 3


## NATIVES

#1
poor.ita.m.ols.nocontrols <- lm(poor.ita.dummy ~ treat)
summary(poor.ita.m.ols.nocontrols)

## appendix - with meritocr (APPENDIX: model 4 page A33)
poor.ita.m.ols.nocontrols.merit <- lm(poor.ita.dummy ~ treat + meritocr)
summary(poor.ita.m.ols.nocontrols.merit)
##

#2
poor.ita.m.0.ols <- lm(poor.ita.dummy ~ treat + educ + age + female + income.pc + 
                         eco.right + soc.conserv + party.choice +
                         Geo.Area.5)
summary(poor.ita.m.0.ols)


#3
poor.ita.m.logit.nocontrols <- glm(poor.ita.dummy ~ treat, 
                                   family=binomial(link="logit"))
summary(poor.ita.m.logit.nocontrols)


#4
poor.ita.m.0.logit <- glm(poor.ita.dummy ~ treat + educ + age + female + income.pc + 
                            eco.right + soc.conserv + party.choice +
                           Geo.Area.5, 
                         family=binomial(link="logit"))
summary(poor.ita.m.0.logit) 

#5
poor.ita.m.ologit.nocontrols <- zelig(formula = as.ordered(poor.ita) ~ treat,
                                      model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.ita.m.ologit.nocontrols)

#6
poor.ita.m.0.ologit <- zelig(formula = as.ordered(poor.ita) ~ treat + educ + age + female + income.pc + 
                               eco.right + soc.conserv + party.choice +
                              Geo.Area.5,
                            model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.ita.m.0.ologit) 



## IMMIGRANTS

#1
poor.imm.m.ols.nocontrols <- lm(poor.imm.dummy ~ treat)
summary(poor.imm.m.ols.nocontrols)

## appendix - with meritocr (APPENDIX: model 6 page A33)
poor.imm.m.ols.nocontrols.merit <- lm(poor.imm.dummy ~ treat + meritocr)
summary(poor.imm.m.ols.nocontrols.merit)
##

#2
poor.imm.m.0.ols <- lm(poor.imm.dummy ~ treat + educ + age + female + income.pc + 
                         eco.right + soc.conserv + party.choice +
                         Geo.Area.5)
summary(poor.imm.m.0.ols)

#3
poor.imm.m.logit.nocontrols <- glm(poor.imm.dummy ~ treat, 
                                   family=binomial(link="logit"))
summary(poor.imm.m.logit.nocontrols)

#4
poor.imm.m.0.logit <- glm(poor.imm.dummy ~ treat + educ + age + female + income.pc + 
                            eco.right + soc.conserv + party.choice +
                            Geo.Area.5, 
                          family=binomial(link="logit"))
summary(poor.imm.m.0.logit) 

#5
poor.imm.m.ologit.nocontrols <- zelig(formula = as.ordered(poor.imm) ~ treat,
                                      model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.imm.m.ologit.nocontrols)

#6
poor.imm.m.0.ologit <- zelig(formula = as.ordered(poor.imm) ~ treat + educ + age + female + income.pc + 
                               eco.right + soc.conserv + party.choice +
                               Geo.Area.5,
                             model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.imm.m.0.ologit) 


stargazer(poor.ita.m.ols.nocontrols, poor.ita.m.0.ols,
          poor.ita.m.logit.nocontrols, poor.ita.m.0.logit,
          poor.ita.m.ologit.nocontrols, poor.ita.m.0.ologit,
          poor.imm.m.ols.nocontrols, poor.imm.m.0.ols,
          poor.imm.m.logit.nocontrols, poor.imm.m.0.logit,
          poor.imm.m.ologit.nocontrols, poor.imm.m.0.ologit,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")



## SELECTIVE SOLIDARITY: Table 4

sel.solid <- as.numeric(redistr.dummy == "1" & poor.ita.dummy.2 == "1" & poor.imm.dummy.3 =="0")

#ols
a<-lm(sel.solid~treat)
summary(a)

#logit, no controls
b <- glm(sel.solid ~ treat, 
         family=binomial(link="logit"))
summary(b)

#logit, with controls
c <- glm(sel.solid ~ treat + educ + age + female + income.pc + 
           eco.right + soc.conserv + party.choice +
           Geo.Area.5, 
         family=binomial(link="logit"))
summary(c)

library(stargazer)
stargazer(b, c,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")



##############

### Interactions with social CONSERVATIVES 


#Italians

#1 - paper: Table 6, model 1
poor.ita.m.1.ols <- lm(poor.ita.dummy ~ treat*soc.conserv.dummy.7 + educ + age + female + income.pc + 
                            eco.right + soc.conserv.dummy.7 + party.choice +
                            Geo.Area.5)
summary(poor.ita.m.1.ols)

#2 - appendix: Table B5, model 2 | Table B13
poor.ita.m.1.logit <- glm(poor.ita.dummy ~ treat*soc.conserv.dummy.7 + educ + age + female + income.pc + 
                            eco.right + soc.conserv.dummy.7 + party.choice +
                            Geo.Area.5, 
                          family=binomial(link="logit"))
summary(poor.ita.m.1.logit) 

#3 - appendix Table B13
poor.ita.m.1.ologit <- zelig(formula = as.ordered(poor.ita) ~ treat + educ + age + female + 
                               income.pc + party.choice + eco.right + Geo.Area.5 +
                               treat*soc.conserv.dummy.7,
                             model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.ita.m.1.ologit) 


#Immigrants

#1 - paper: Table 6, model 3
poor.imm.m.1.ols <- lm(poor.imm.dummy ~ treat*soc.conserv.dummy.7 + educ + age + female + income.pc + 
                         eco.right + soc.conserv.dummy.7 + party.choice +
                         Geo.Area.5)
summary(poor.imm.m.1.ols)

#2 - appendix: Table B5, model 2 | Table B13
poor.imm.m.1.logit <- glm(poor.imm.dummy ~ treat*soc.conserv.dummy.7 + educ + age + female + income.pc + 
                            eco.right + soc.conserv.dummy.7 + party.choice +
                            Geo.Area.5, 
                          family=binomial(link="logit"))
summary(poor.imm.m.1.logit) 


#3 - appendix Table B13
poor.imm.m.1.ologit <- zelig(formula = as.ordered(poor.imm) ~ treat + educ + age + female + 
                               income.pc + party.choice + eco.right + Geo.Area.5 +
                               treat*soc.conserv.dummy.7,
                             model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.imm.m.1.ologit) 

#for paper
stargazer(poor.ita.m.1.ols, poor.imm.m.1.ols,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")

#for appendix
stargazer(poor.ita.m.1.ols, poor.ita.m.1.logit, poor.ita.m.1.ologit,
          poor.imm.m.1.ols, poor.imm.m.1.logit, poor.imm.m.1.ologit,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")


## Interaction with income (APPENDIX Table B14)

# ITA
#1
poor.ita.m.1.ols.income <- lm(poor.ita.dummy ~ treat*income.pc + educ + age + female + income.pc + 
                         eco.right + soc.conserv + party.choice +
                         Geo.Area.5)
summary(poor.ita.m.1.ols.income)

#2
poor.ita.m.1.logit.inc <- glm(poor.ita.dummy ~ treat*income.pc + educ + age + female + income.pc + 
                            eco.right + soc.conserv + party.choice +
                            Geo.Area.5, 
                          family=binomial(link="logit"))
summary(poor.ita.m.1.logit.inc) 

#3
poor.ita.m.1.ologit.inc <- zelig(formula = as.ordered(poor.ita) ~ treat*income.pc + educ + age + female + income.pc + 
                               eco.right + soc.conserv + party.choice +
                               Geo.Area.5,
                             model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.ita.m.1.ologit.inc)

# IMM
#1
poor.imm.m.1.ols.income <- lm(poor.imm.dummy ~ treat*income.pc + educ + age + female + income.pc + 
                                eco.right + soc.conserv + party.choice +
                                Geo.Area.5)
summary(poor.imm.m.1.ols.income)

#2
poor.imm.m.1.logit.inc <- glm(poor.imm.dummy ~ treat*income.pc + educ + age + female + income.pc + 
                                eco.right + soc.conserv + party.choice +
                                Geo.Area.5, 
                              family=binomial(link="logit"))
summary(poor.imm.m.1.logit.inc) 

#3
poor.imm.m.1.ologit.inc <- zelig(formula = as.ordered(poor.imm) ~ treat*income.pc + educ + age + female + income.pc + 
                                   eco.right + soc.conserv + party.choice +
                                   Geo.Area.5,
                                 model = "ologit", data = ITA.survey, cite = FALSE)
summary(poor.imm.m.1.ologit.inc)

stargazer(poor.ita.m.1.ols.income, poor.ita.m.1.ols.income, poor.ita.m.1.ols.income,
          poor.imm.m.1.ols.income, poor.imm.m.1.ols.income, poor.imm.m.1.ols.income,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")


#####

## INCOME SUBSETS - richer and poorer for ALTERNATIVE EXPLANATIONS (paper Table 7)

#Inequality treatment - Redistribution, natives, immigrants

#ols

poor.ita.m.0.ols.inc <- lm(poor.ita.dummy ~ treat + educ + income.pc +
                             age + female +  
                             eco.right + soc.conserv + party.choice +
                             Geo.Area.5)
summary(poor.ita.m.0.ols.inc)

poor.imm.m.0.inc <- lm(poor.imm.dummy ~ treat + educ + income.pc +
                         age + female +  
                         eco.right + soc.conserv + party.choice +
                         Geo.Area.5)
summary(poor.imm.m.0.inc)

library(stargazer)
stargazer(poor.ita.m.0.ols.inc, 
          poor.imm.m.0.inc, 
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")




### APPENDIX (Table B16) - Passed attention check 

poor.ita.m.0.ologit.att <- zelig(formula = as.ordered(poor.ita) ~ treat + educ + age + female + income.pc + 
                               eco.right + soc.conserv.dummy.7 + party.choice +
                               Geo.Area.5,
                             model = "ologit", data = subset.att, cite = FALSE)
summary(poor.ita.m.0.ologit.att) 
poor.ita.m.1.ologit.att <- zelig(formula = as.ordered(poor.ita) ~ treat + educ + age + female + 
                               income.pc + party.choice + eco.right + Geo.Area.5 +
                               treat*soc.conserv.dummy.7,
                             model = "ologit", data = subset.att, cite = FALSE)
summary(poor.ita.m.1.ologit.att) 

poor.imm.m.0.ologit.att <- zelig(formula = as.ordered(poor.imm) ~ treat + educ + age + female + income.pc + 
                                   eco.right + soc.conserv.dummy.7 + party.choice +
                                   Geo.Area.5,
                                 model = "ologit", data = subset.att, cite = FALSE)
summary(poor.imm.m.0.ologit.att)
poor.imm.m.1.ologit.att <- zelig(formula = as.ordered(poor.imm) ~ treat + educ + age + female + 
                                   income.pc + party.choice + eco.right + Geo.Area.5 +
                                   treat*soc.conserv.dummy.7,
                                 model = "ologit", data = subset.att, cite = FALSE)
summary(poor.imm.m.1.ologit.att)

stargazer(poor.ita.m.0.ologit.att, poor.ita.m.1.ologit.att, poor.imm.m.0.ologit.att, poor.imm.m.1.ologit.att,
          type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")





###################

## CAUSAL MECHANISM ANALYSIS

# CAUSAL MECHANISM (Table 5 in PAPER)
#Separate models for impact: ineq --> meritocr; meritocr --> welfare

#Table 5, model A
med.fit <- glm(un.meritocr.dummy.2 ~ treat + educ + age + female + 
                 eco.right + soc.conserv + income.pc +
                 party.choice + Geo.Area.5,
               family=binomial(link="logit"))
summary(med.fit)

#Table 5, model B1
out.fit.red <- zelig(formula = as.ordered(redistr) ~ un.meritocr.dummy.2 + educ + age + female + 
                       eco.right + soc.conserv  + party.choice + 
                       income.pc + Geo.Area.5,
                     model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.red)

#Table 5, model B2
out.fit.poor <- zelig(formula = as.ordered(poor.ita) ~ un.meritocr.dummy.2 + educ + age + female + 
                        eco.right + soc.conserv  + party.choice +
                        income.pc + Geo.Area.5,
                      model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.poor)

#Table 5, model B3
out.fit.imm <- zelig(formula = as.ordered(poor.imm) ~ un.meritocr.dummy.2 + educ + age + female + 
                       eco.right+ soc.conserv + party.choice +
                       income.pc + Geo.Area.5,
                     model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.imm)


stargazer(med.fit, out.fit.red, out.fit.poor, out.fit.imm, type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")



## Table 8 in the paper

## SOC.MOBILITY*CONSERV and SOC.MOBILITY*INCOME

## Show that effect of meritocr on support for ita and immmigr is stronger among conservatives

out.fit.imm.cons <- zelig(formula = as.ordered(poor.imm) ~ un.meritocr.dummy.2*soc.conserv.dummy.7
                          + educ + age + female + 
                       eco.right+ soc.conserv.dummy.7 + party.choice +
                       income.pc + Geo.Area.5,
                     model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.imm.cons)

out.fit.priority.cons <- zelig(formula = as.ordered(priority.ita) ~ un.meritocr.dummy.2*soc.conserv.dummy.7
                          + educ + age + female + 
                            eco.right+ soc.conserv.dummy.7 + party.choice +
                            income.pc + Geo.Area.5,
                          model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.priority.cons)



## Show that effect of meritocr on support for ita and immmigr is NOT stronger among poor

out.fit.imm.poor <- zelig(formula = as.ordered(poor.imm) ~ un.meritocr.dummy.2*income.h.poor
                          + educ + age + female + 
                            eco.right+ soc.conserv.dummy.7 + party.choice +
                            Geo.Area.5,
                          model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.imm.poor)

out.fit.priority.poor <- zelig(formula = as.ordered(priority.ita) ~ un.meritocr.dummy.2*income.h.poor
                               + educ + age + female + 
                                 eco.right+ soc.conserv.dummy.7 + party.choice +
                                 Geo.Area.5,
                               model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.priority.poor)


library(stargazer)
stargazer(out.fit.priority.cons, out.fit.priority.poor, out.fit.imm.cons, out.fit.imm.poor, type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE, 
          out="mod.htm") 


## no effect with rich either

out.fit.imm.rich <- zelig(formula = as.ordered(poor.imm) ~ un.meritocr.dummy.2*income.h.rich
                            + educ + age + female + 
                              eco.right+ soc.conserv.dummy.7 + party.choice +
                              Geo.Area.5,
                            model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.imm.rich)

out.fit.priority.rich <- zelig(formula = as.ordered(priority.ita) ~ un.meritocr.dummy.2*income.h.rich
                                 + educ + age + female + 
                                   eco.right+ soc.conserv.dummy.7 + party.choice +
                                   Geo.Area.5,
                                 model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.priority.rich)





## MEDIATION

## Appendix: Tables B7 and B8

#Mediation meritocracy - poor, imm, redistr

med.fit <- glm(un.meritocr.dummy.2 ~ treat + educ + age + female + 
                 eco.right + soc.conserv + income.pc +
                 party.choice + Geo.Area.5,
               family=binomial(link="logit"))
summary(med.fit)

# Output models
out.fit <- glm(redistr.dummy ~ un.meritocr.dummy.2 + treat + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="logit")) 
summary(out.fit)

out.fit <- glm(poor.ita.dummy ~ un.meritocr.dummy.2 + treat + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="logit")) 
summary(out.fit)

out.fit <- glm(poor.imm.dummy ~ un.meritocr.dummy.2 + treat + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="logit")) 
summary(out.fit)

#Mediation Analysis 
med.out <- mediate(med.fit, out.fit, treat = "treat", 
                   mediator = "un.meritocr.dummy.2", 
                   sims = 1000,
                   dropobs = TRUE,
                   robustSE = TRUE)
summary(med.out)
plot(med.out)



# Sensitivity analysis

table(treat)
treat.sens = rep(NA,n)
treat.sens[treat=="Control"]=0
treat.sens[treat=="Inequality"]=1
treat.sens[treat=="Poverty"]=2
table(treat.sens)
treat.sens<-factor(treat.sens)

med.fit <- lm(un.meritocr.dummy.2 ~ treat.sens + educ + age + female + eco.right + soc.conserv + income.pc +
                party.choice + Geo.Area.5)
summary(med.fit)

out.fit <- glm(redistr.dummy ~ un.meritocr.dummy.2 + treat.sens + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="probit")) 
summary(out.fit)

out.fit <- glm(poor.ita.dummy ~ un.meritocr.dummy.2 + treat.sens + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="probit")) 
summary(out.fit)

out.fit <- glm(poor.imm.dummy ~ un.meritocr.dummy.2 + treat.sens + educ + age + female + 
                 eco.right + soc.conserv + party.choice +
                 income.pc + Geo.Area.5,
               family=binomial(link="probit")) 
summary(out.fit)

med.out <- mediate(med.fit, out.fit, treat = "treat.sens", 
                   mediator = "un.meritocr.dummy.2", 
                   sims = 1000,
                   dropobs = TRUE,
                   robustSE = TRUE)
summary(med.out)

sens.out <- medsens(med.out, rho.by = 0.1, effect.type = "indirect", sims = 1000)
summary(sens.out)
plot(sens.out, sens.par = "rho", main = "Meritocracy", ylim = c(-0.2, 0.2))




##

#Impact of inequality on perceived lack of opportunity  
  #and impact of lack of opportunity on perceptions of welfare deservingness

# Appendix Table B9, page A35

med.fit <- lm(un.meritocr.dummy.2 ~ treat.sens + educ + age + female + eco.right + soc.conserv + income.pc +
                party.choice + Geo.Area.5)
summary(med.fit)


out.fit.priority <- zelig(formula = as.ordered(priority.ita) ~ un.meritocr.dummy.2 + educ + age + female + 
                            eco.right + soc.conserv + party.choice +
                            income.pc + Geo.Area.5,
                          model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.priority)

out.fit.desimm <- zelig(formula = as.ordered(imm.undes) ~ un.meritocr.dummy.2 + educ + age + female + 
                          eco.right + soc.conserv + party.choice +
                          income.pc + Geo.Area.5,
                        model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.desimm)

out.fit.desita <- zelig(formula = as.ordered(poor.undes) ~ un.meritocr.dummy.2 + educ + age + female + 
                          eco.right + soc.conserv + party.choice +
                          income.pc + Geo.Area.5,
                        model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.desita)


stargazer(med.fit, out.fit.desita, out.fit.desimm, out.fit.priority, type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")


#Impact of inequality on perceptions of welfare deservingness 
  #and impact of perceptions of welfare deservingness on welfare support for natives and immigrants

#Appendix Table B11, page A37

out.fit.desimm.2 <- zelig(formula = as.ordered(imm.undes) ~ treat + educ + age + female + 
                            eco.right + soc.conserv + party.choice +
                            income.pc + Geo.Area.5,
                          model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.desimm.2)
out.fit.desita.2 <- zelig(formula = as.ordered(poor.undes) ~ treat + educ + age + female + 
                            eco.right + soc.conserv + party.choice +
                            income.pc + Geo.Area.5,
                          model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.desita.2)
out.fit.poor.2 <- zelig(formula = as.ordered(poor.ita) ~ poor.undes + educ + age + female + 
                          eco.right + soc.conserv  + party.choice +
                          income.pc + Geo.Area.5,
                        model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.poor.2)
out.fit.imm.2 <- zelig(formula = as.ordered(poor.imm) ~ imm.undes + educ + age + female + 
                         eco.right+ soc.conserv + party.choice +
                         income.pc + Geo.Area.5,
                       model = "ologit", data = ITA.survey, cite = FALSE) 
summary(out.fit.imm.2)


stargazer(out.fit.desita.2, out.fit.desimm.2, out.fit.poor.2, out.fit.imm.2, type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")




#mediation: poor undeserv, poor support
med.fit.desita.2 <- glm(poor.undeserv.dummy ~ treat + educ + age + female + 
                          eco.right + soc.conserv + party.choice +
                          income.pc + Geo.Area.5,
                        family=binomial(link="logit")) 
summary(med.fit.desita.2)
out.fit.poor.2.m <- glm(poor.ita.dummy ~ treat + poor.undeserv.dummy + educ + age + female + 
                          eco.right + soc.conserv  + party.choice +
                          income.pc + Geo.Area.5,
                        family=binomial(link="logit")) 
summary(out.fit.poor.2.m)

med.out <- mediate(med.fit.desita.2, out.fit.poor.2.m, treat = "treat", 
                   mediator = "poor.undeserv.dummy", 
                   sims = 1000,
                   dropobs = TRUE,
                   robustSE = TRUE)
summary(med.out)

#mediation: immigrant undeserv, immigr support
med.fit.desimm.2 <- glm(imm.undes.dummy ~ treat + educ + age + female + 
                          eco.right + party.choice +
                          income.pc + Geo.Area.5,
                        family=binomial(link="logit")) 
summary(med.fit.desimm.2)
out.fit.imm.2.m <- lm(poor.imm.dummy ~ treat + imm.undes.dummy + educ + age + female + 
                        eco.right+ party.choice +
                        income.pc + Geo.Area.5,
                      family=binomial(link="logit")) 
summary(out.fit.imm.2.m) 

med.out <- mediate(med.fit.desimm.2, out.fit.imm.2.m, treat = "treat", 
                   mediator = "imm.undes.dummy", 
                   sims = 1000,
                   dropobs = TRUE,
                   robustSE = TRUE)
summary(med.out)




## Manipulation checks (Appendix)

# Issue Importance: Inequality (Table B3)
#1
ineq.import.m.ologit <- zelig(formula = as.ordered(ineq.import) ~ treat,
                              model = "ologit", data = ITA.survey, cite = FALSE)
summary(ineq.import.m.ologit)
#2
ineq.import.m.ologit.attcheck <- zelig(formula = as.ordered(ineq.import) ~ treat + refer.part,
                              model = "ologit", data = ITA.survey, cite = FALSE)
summary(ineq.import.m.ologit.attcheck)



# Factual knowledge of inequality (Table B1)

mancheck.ineq.logit <- glm(mancheck.ineq.dummy ~ treat, 
                         family=binomial(link="logit"))
summary(mancheck.ineq.logit)

mancheck.ineq.logit.att <- glm(mancheck.ineq.dummy ~ treat + refer.part, 
                           family=binomial(link="logit"))
summary(mancheck.ineq.logit.att)

stargazer(mancheck.ineq.logit, mancheck.ineq.logit.att, type="html", digits = 2,
          star.char = c("+", "*", "**", "***"), 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
          notes        = "+p<0.1; *p<0.05; **p<0.01; ***p<0.001", 
          notes.append = FALSE, no.space = TRUE,
          out="mod.htm")


# Factual knowledge of inequality and poverty with controls (Table B2)

mancheck.ineq.m.0 <- glm(mancheck.ineq.dummy ~ treat + educ + age + female + income + 
                           eco.right + party.choice + eco.insec +
                           Geo.Area.5, 
                         family=binomial(link="logit"))
summary(mancheck.ineq.m.0)


mancheck.ineq.m.1 <- glm(mancheck.ineq.dummy ~ treat + educ + age + female + income + 
                           eco.right + party.choice + eco.insec +
                           Geo.Area.5 + refer.part, 
                         family=binomial(link="logit"))
summary(mancheck.ineq.m.1)



mancheck.pov.m.0 <- glm(mancheck.pov.dummy ~ treat + educ + age + female + income + 
                          eco.right + party.choice + eco.insec +
                          Geo.Area.5, 
                        family=binomial(link="logit"))
summary(mancheck.pov.m.0)

mancheck.pov.m.1 <- glm(mancheck.pov.dummy ~ treat + educ + age + female + income + 
                          eco.right + party.choice + eco.insec +
                          Geo.Area.5 + refer.part, 
                        family=binomial(link="logit"))
summary(mancheck.pov.m.1)

