#Author: Amanda Graham
#Date: 9/28/2024

#libraries----
library(haven)
library(car)
library(survey)
library(jtools)
library(ggplot2)
library(tidyr)
library(psych)

#GLOBAL SUPPORT----
##BRING IN DATASET - GLOBAL SUPPORT N=800 - USE WEIGHTS
SLG<-read_sav("[insert file path]/SLData - Public.sav")
#cut to national rep sample (N=800)
SLG2<-SLG[which(SLG$weight_main!="NA"),]

weight<-svydesign(id=~caseid, weights=~weight_main,data=SLG2)

#race - white and non-white
ftable(SLG2$race)#1=white
SLG2$WHITE<-car::recode(SLG2$race,"1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0")
ftable(SLG2$WHITE)
#sex - males and females
ftable(SLG2$gender4)#1=male,2=female,3=NB,4=other (n=12 NB; coded m. v non-males)
SLG2$MALE<-car::recode(SLG2$gender4,"1=1;2=0;3=0;4=0")
ftable(SLG2$MALE)

#party - R v. non-R
ftable(SLG2$pid7)#R=5,6,7, non-R=1,2,3,4,8
SLG2$REPUB<-car::recode(SLG2$pid7,"1=0;2=0;3=0;4=0;5=1;6=1;7=1;8=0")
ftable(SLG2$REPUB)

#Edu - high and low (split at median)
ftable(SLG2$educ)#1-6
SLG2$EDU2<-car::recode(SLG2$educ,"1=0;2=0;3=0;4=1;5=1;6=1")
ftable(SLG2$EDU2)

#income - high and low (split at median)
ftable(SLG2$faminc_new)#97 missing values (impute then reanalyze this)
SLG2$faminc_new[SLG2$faminc_new==97] <- NA

#region - south and non-south
ftable(SLG2$region)
SLG2$SOUTH<-car::recode(SLG2$region,"1=0;2=0;3=1;4=0")#3=south
ftable(SLG2$SOUTH)

#AGE
SLG2$AGE<-(2023-SLG2$birthyr)
psych::describe(SLG2$AGE)
#MARRIED
ftable(SLG2$marstat)
SLG2$MARRIED<-car::recode(SLG2$marstat,"1=1;else=0")
ftable(SLG2$MARRIED)
#CONSERV.
ftable(SLG2$ideo5)
SLG2$CONSERV<-car::recode(SLG2$ideo5,"1=1;2=2;3=3;4=4;5=5;6=3")#make "not sure's" moderate
ftable(SLG2$CONSERV)

#RELIG
ftable(SLG2$religpew)
SLG2$RELIG<-car::recode(SLG2$religpew,"1=1;2=1;3=1;4=1;5=1;6=1;7=1;8=1;9=0;10=0;11=0;12=1")
ftable(SLG2$RELIG)

#RR
#reverse coding of #2
ftable(SLG2$q19_2)
SLG2$q19_2R<-car::recode(SLG2$q19_2,"1=5;2=4;3=3;4=2;5=1")
ftable(SLG2$q19_2R)
#weighted EFA
library(ltm)
svyfactanal(~SLG2$q19_1+SLG2$q19_2R+SLG2$q19_3+SLG2$q19_4,design=weight,factors=1)#FL range between .65(#2) and .92
svycralpha(~SLG2$q19_1+SLG2$q19_2R+SLG2$q19_3+SLG2$q19_4, weight, na.rm = FALSE)
#build RR
SLG2$RR<-((SLG2$q19_1+SLG2$q19_2R+SLG2$q19_3+SLG2$q19_4)/4)
psych::describe(SLG2$RR)

####weighted descriptives###
svymean(SLG2$q2,design = weight)
prop.table(svytable(~SLG2$q2,design = weight))
prop.table(svytable(~SLG2$q2_manipulation,design = weight))
svytable(~SLG2$q2_manipulation+SLG2$q2,design = weight)
SLG2$q2<-as.vector(SLG2$q2)
SLG2$q2_manipulation<-as.vector(SLG2$q2_manipulation)
svychisq(~q2_manipulation+q2,design = weight)

#impute income
library(simputation)
head(SLG2$faminc_new,25)
ftable(SLG2$faminc_new)
psych::describe(SLG2$faminc_new)
SLG2IMP<-impute_lm(SLG2,faminc_new~WHITE+AGE+MALE+EDU2+MARRIED+REPUB+CONSERV+RELIG+RR)
psych::describe(SLG2IMP$faminc_new)
ftable(SLG2IMP$faminc_new)
SLG2IMP$INCOME2<-car::recode(SLG2IMP$faminc_new,"1=0;2=0;3=0;4=0;5=0;6=0;7=0;8=0;else=1")
ftable(SLG2IMP$INCOME2)

#reverse coding and rescale 0-100
ftable(SLG2IMP$q2)
SLG2IMP$q2R<-car::recode(SLG2IMP$q2,"1=100;2=75;3=50;4=25;5=0")
ftable(SLG2IMP$q2R)

weightIMP<-svydesign(id=~caseid, weights=~weight_main,data=SLG2IMP)
summary(weightIMP)


###weighted descriptives####
svymean(SLG2IMP$RR,design = weightIMP)
svysd(~SLG2IMP$RR,design = weightIMP)
svymean(SLG2IMP$AGE,design = weightIMP)
svysd(~SLG2IMP$AGE,design = weightIMP)
psych::describe(SLG2IMP$AGE)
svytable(~SLG2IMP$MARRIED,design = weightIMP)
ftable(SLG2IMP$MARRIED)
ftable(SLG2IMP$marstat)
svymean(SLG2IMP$educ,design = weightIMP)
svytable(~SLG2IMP$educ,design=weightIMP)
svysd(~as.numeric(SLG2IMP$educ),design = weightIMP)
svymean(SLG2IMP$faminc_new,design = weightIMP)
svysd(~as.numeric(SLG2IMP$faminc_new),design = weightIMP)
svymean(SLG2IMP$CONSERV,design = weightIMP)
svysd(~as.numeric(SLG2IMP$CONSERV),design = weightIMP)
svytable(~SLG2IMP$CONSERV,design=weightIMP)
svytable(~SLG2IMP$RELIG,design = weightIMP)

svytable(~SLG2IMP$q2R,design = weightIMP)
svytable(~SLG2IMP$q2_manipulation,design = weightIMP)
svytable(~SLG2IMP$q2_manipulation+SLG2IMP$q2R,design = weightIMP)
svychisq(~q2_manipulation+q2R,design = weightIMP,statistic="Chisq")

svytable(~SLG2IMP$WHITE,design = weightIMP)
ftable(SLG2IMP$WHITE,SLG2IMP$q2R)
svytable(~SLG2IMP$WHITE+SLG2IMP$q2R,design = weightIMP)
svychisq(~WHITE+q2R,design = weightIMP,statistic="Chisq")
svytable(~SLG2IMP$MALE,design = weightIMP)
svytable(~SLG2IMP$MALE+SLG2IMP$q2R,design = weightIMP)
svychisq(~MALE+q2R,design = weightIMP,statistic=c("Chisq"))

svytable(~SLG2IMP$REPUB,design = weightIMP)
svytable(~SLG2IMP$REPUB+SLG2IMP$q2R,design = weightIMP)
svychisq(~REPUB+q2R,design = weightIMP,statistic=c("Chisq"))

svytable(~SLG2IMP$EDU2,design = weightIMP)
svytable(~SLG2IMP$EDU2+SLG2IMP$q2R,design = weightIMP)
svychisq(~EDU2+q2R,design = weightIMP,statistic=c("Chisq"))

svytable(~SLG2IMP$INCOME2,design = weightIMP)
svytable(~SLG2IMP$INCOME2+SLG2IMP$q2R,design = weightIMP)
svychisq(~INCOME2+q2R,design = weightIMP,statistic=c("Chisq"))

svytable(~SLG2IMP$SOUTH,design = weightIMP)
svytable(~SLG2IMP$SOUTH+SLG2IMP$q2R,design = weightIMP)
svychisq(~SOUTH+q2R,design = weightIMP,statistic=c("Chisq"))

##weighted GLM----
library(poliscidata)
SLGlm<-svyglm(q2R~as.factor(q2_manipulation)+RR+AGE+MALE+WHITE+MARRIED+educ+faminc_new+
                REPUB+CONSERV+RELIG+SOUTH,data=SLG2IMP,design=weightIMP,family=gaussian)
summary(SLGlm)
fit.svyglm(SLGlm)
lm.beta::lm.beta(SLGlm)


#SPECIFIC SL####
#bring in dataset 
SL<-read_sav("[insert file path]/SLDataTransformed - Public.sav")

#clean data
SL2<-SL[which(SL$weight_main!="NA"),]

weightSL2<-svydesign(id=~caseid, weights=~weight_main,data=SL2)


ftable(SL2$WHITE)
ftable(SL2$MALE)
ftable(SL2$REPUB)
ftable(SL2$educ)

#income 
ftable(SL2$faminc_new)#97 missing values
SL2$faminc_new[SL2$faminc_new==97] <- NA

ftable(SL2$SOUTH)

#bUILD OTHER VARS.
#AGE
psych::describe(SL2$AGE)
#MARRIED
ftable(SL2$MARRIED)
#CONSERV.
ftable(SL2$POLVIEWPOINT)
#RELIG
ftable(SL2$RELIGIOUS)

#RR
psych::describe(SL2$RR)


#impute income
library(simputation)
head(SL2$faminc_new,25)
ftable(SL2$faminc_new)
psych::describe(SL2$faminc_new)
SL2IMP<-impute_lm(SL2,faminc_new~WHITE+AGE+MALE+educ+MARRIED+REPUB+POLVIEWPOINT+RELIGIOUS+RR)
psych::describe(SL2IMP$faminc_new)
ftable(SL2IMP$faminc_new)

#Response
ftable(SL2IMP$Response)
#need to reverse and rescale (0-100)
SL2IMP$ResponseR<-car::recode(SL2IMP$Response,"1=100;2=66;3=33;4=0")
ftable(SL2IMP$ResponseR)
prop.table(ftable(SL2IMP$ResponseR))
#ResponseR = higher values = more support for release

ftable(SL2IMP$ResponseR,SL2IMP$ManipulationWARD,SL2IMP$ManipulationREHAB)
ftable(SL2IMP$ResponseR,SL2IMP$ManipulationWARD,SL2IMP$ManipulationREHAB,SL2IMP$ManipulationVIC)

weightSL2IMP<-svydesign(id=~caseid, weights=~weight_main, data=SL2IMP)
summary(weightSL2IMP)

##weighted/cluster models ----
SL2Glm<-svyglm(ResponseR~as.factor(ManipulationAGE)+
                 (ManipulationGENDER)+
                 as.factor(ManipulationRACE)+
                 relevel(as.factor(ManipulationCRIME),ref="First degree murder, committed during a robbery.")+
                 as.factor(manipulationCHILDHOOD)+
                 relevel(as.factor(ManipulationREHAB),ref="NO rehabilitation program completed.")+
                 as.factor(ManipulationWARD)+
                 as.factor(ManipulationVIC)+
                 RR+AGE+MALE+WHITE+MARRIED+educ+faminc_new+
                 REPUB+POLVIEWPOINT+RELIGIOUS+SOUTH,
               data=SL2IMP,design=weightSL2IMP,family=gaussian)
summary(SL2Glm)
SL2Glm
lm.beta::lm.beta(SL2Glm)

#manual calculation of adj R-square b/c nesting of vignettes in ppl: https://github.com/jacob-long/jtools/issues/112
S <- summary(SL2Glm)
N <- summary(svyglm(ResponseR~1, design=weightSL2IMP))
adjRsq = 1 - (S$dispersion / (S$df.residual)) / (N$dispersion / (N$df.residual))
show(adjRsq[1])