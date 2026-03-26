#####################################################################################
########### R code for the replication study of
########### A procedural replication: task shifting of antiretroviral treatment from doctors to primary-care nurses in South Africa (STRETCH)
#####################################################################################



#####################################################################################
######################Loading necessary libraries for analysis ######################
#####################################################################################

install.packages("foreign")
library(foreign)

install.packages("readstata13")
library(readstata13)

install.packages("pastecs")
library(pastecs)

install.packages("survival")
library(survival)

install.packages("rms")
library(rms)

install.packages("binom")
library(binom)

install.packages("pander")
library(pander)
panderOptions('digits', 2)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)


#####################################################################################
################################### Reading Stata data into R  ######################
#####################################################################################

data1=read.dta13("cohort1_replication.dta")
data2=read.dta13("cohort2_replication.dta")


#####################################################################################
##################################### Pure Replicate Table 1  #######################
#####################################################################################

### Sex
tbl=table(data1$sex,data1$arm)
tbl
chisq.test(tbl)

### Age at enrollment
quantile(data1$ageatenrolment[data1$arm=="STRETCH"])
quantile(data1$ageatenrolment[data1$arm=="Control"])
w1=wilcox.test(ageatenrolment ~ arm, data=data1) 
pander(w1)

### Idcheck
tbl=table(data1$idcheck,data1$arm)
tbl
chisq.test(tbl)

### Cd4 group
data1$cd4grp=1
data1$cd4grp[data1$eligiblecd4value>=50 & data1$eligiblecd4value<=99]=2
data1$cd4grp[data1$eligiblecd4value>=100 & data1$eligiblecd4value<=199]=3
data1$cd4grp[data1$eligiblecd4value>=200 & data1$eligiblecd4value<=350]=4

tbl=table(data1$cd4grp,data1$arm)
tbl
ch1=chisq.test(tbl)
pander(ch1)

### Viral load
tbl=table(data2$supprvl,data2$arm)
tbl
ch1=chisq.test(tbl)
pander(ch1)

tb2=rbind(c(3029-2378,2378),c(3202-2507,2507))
chisq.test(tb2)

#####################################################################################
##################################### Pure Replicate Table 2  #######################
#####################################################################################

############################ Overall analysis

x1=sum(data1$died[data1$arm=="STRETCH"])
n1=sum(data1$survivaltime[data1$arm=="STRETCH"])/30.4375
binom.confint(x=x1,n=n1)


x2=sum(data1$died[data1$arm=="Control"])
n2=sum(data1$survivaltime[data1$arm=="Control"])/30.4375
binom.confint(x=x2,n=n2)


################## Subgroups analysis: cd4 201-350

nrow(data1[data1$eligiblecd4value>=201,]) 


x1=sum(data1$died[data1$arm=="STRETCH" & data1$eligiblecd4value>=201])
n1=sum(data1$survivaltime[data1$arm=="STRETCH" & data1$eligiblecd4value>=201])/30.4375
binom.confint(x=x1,n=n1)

x1=sum(data1$died[data1$arm=="Control" & data1$eligiblecd4value>=201])
n1=sum(data1$survivaltime[data1$arm=="Control" & data1$eligiblecd4value>=201])/30.4375
binom.confint(x=x1,n=n1)


################## Subgroups analysis: cd4 <=200

nrow(data1[data1$eligiblecd4value<=200,]) 

x1=sum(data1$died[data1$arm=="STRETCH" & data1$eligiblecd4value<=200])
n1=sum(data1$survivaltime[data1$arm=="STRETCH" & data1$eligiblecd4value<=200])/30.4375
binom.confint(x=x1,n=n1)


x1=sum(data1$died[data1$arm=="Control" & data1$eligiblecd4value<=200])
n1=sum(data1$survivaltime[data1$arm=="Control" & data1$eligiblecd4value<=200])/30.4375
binom.confint(x=x1,n=n1)



################ Statified, clustered analysis

fit1=coxph( Surv(survivaltime,died)~arm+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit1)

data1$cd4group=ifelse(data1$eligiblecd4value<=200,1,0)

fit11=coxph( Surv(survivaltime,died)~arm*cd4group+strata(strata)+cluster(siteid), data=data1,  method="breslow")
summary(fit11)

data1$cd4group=ifelse(data1$eligiblecd4value<=200,0,1)
fit12=coxph( Surv(survivaltime,died)~arm*cd4group+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit12)

################# Adjusted for age, idcheck, baseline cd4 and sex

fit1=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+eligiblecd4value+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit1)


data1$cd4group=ifelse(data1$eligiblecd4value<=200,1,0)
fit11=coxph( Surv(survivaltime,died)~arm*cd4group+ageatenrolment+sex+idcheck+eligiblecd4value+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit11)

data1$cd4group=ifelse(data1$eligiblecd4value<=200,0,1)
fit12=coxph( Surv(survivaltime,died)~arm*cd4group+ageatenrolment+sex+idcheck+eligiblecd4value+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit12)



#####################################################################################
##################################### Pure Replicate Table 4  #######################
#####################################################################################

x1=sum(data2$supprvl[data2$arm=="Stretch" & data2$supprvl==1])
n1=length(data2$supprvl[data2$arm=="Stretch"])
binom.confint(x=x1,n=n1)


x1=sum(data2$supprvl[data2$arm=="Control" & data2$supprvl==1])
n1=length(data2$supprvl[data2$arm=="Control"])
binom.confint(x=x1,n=n1)




#####################################################################################
##################################### MEA analysis  #################################
#####################################################################################

#########################################################
############# Check PH assumption for Table 2 
############# overall model without adjusted variables
#########################################################

fit1=coxph( Surv(survivaltime,died)~arm+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit1)

#########Conclusion: arm satisfy PH assumption


############# Check PH assumption for Table 2: overall model with adjusted variables 
############# Adjusted for age, idcheck, baseline cd4 and sex

fit12=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+eligiblecd4value+strata(strata)+cluster(siteid), data=data1, method="breslow")
summary(fit12)
fit12.zph <- cox.zph(fit12, transform = 'log')
fit12.zph 
plot(fit12.zph)

######## Stratify by cd4
data1$cd4group=ifelse(data1$eligiblecd4value<=200,1,0)

fit12=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(strata)
+strata(cd4group)+cluster(siteid), data=data1, method="breslow")
summary(fit12)
fit12.zph <- cox.zph(fit12, transform = 'log')
fit12.zph 

#########Conclusion: all meet PH assumption.



################################################# Subgroup analysis with cd4>=201: unadjusted model

data1$cd4group=ifelse(data1$eligiblecd4value>200,1,0)

fit21=coxph( Surv(survivaltime,died)~arm+strata(strata)+cluster(siteid), data=data1, subset=cd4group==1, method="breslow")
summary(fit21)
cox.zph(fit21, transform = 'log')

#########Conclusion: all meet PH assumption

################################################# Subgroup analysis with cd4<=200: unadjusted model

fit22=coxph( Surv(survivaltime,died)~arm+strata(strata)+cluster(siteid), data=data1,subset=cd4group==0, method="breslow")
summary(fit22)
cox.zph(fit22, transform = 'log')

#########Conclusion: all meet PH assumption



################################################# Subgroup analysis with cd4>=201: adjusted model

fit31=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(strata)+cluster(siteid), data=data1,subset=cd4group==1, method="breslow")
summary(fit31)
cox.zph(fit31, transform = 'log')

###############stratify by sex
fit311=coxph( Surv(survivaltime,died)~arm+ageatenrolment+strata(strata)+strata(sex)+strata(idcheck)+cluster(siteid), data=data1,subset=cd4group==1, method="breslow")
summary(fit311)
cox.zph(fit311, transform = 'log')
plot(cox.zph(fit311, transform = 'log'))

###############stratify by sex, age, idcheck

quantile(data1$ageatenrolment,c(0.25,0.5,0.75))

data1$ageQ=1
data1$ageQ[data1$ageatenrolment>30 & data1$ageatenrolment<=35]=2
data1$ageQ[data1$ageatenrolment>35 & data1$ageatenrolment<=43]=3
data1$ageQ[data1$ageatenrolment>43]=4

fit311=coxph( Surv(survivaltime,died)~arm+strata(strata)++strata(ageQ)+strata(sex)+strata(idcheck)+cluster(siteid), data=data1,subset=cd4group==1, method="breslow")
summary(fit311)
cox.zph(fit311, transform = 'log')
plot(cox.zph(fit311, transform = 'log'))
#########Conclusion: all meet PH assumption


################################################# Subgroup analysis with cd4<=200: adjusted model

fit32=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(strata)+cluster(siteid), data=data1,subset=cd4group==0, method="breslow")
summary(fit32)
cox.zph(fit32, transform = 'log')

##########stratify by id check
fit322=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+strata(idcheck)+strata(strata)+cluster(siteid), data=data1,subset=cd4group==0, method="breslow")
summary(fit322)
cox.zph(fit322, transform = 'log')

#########Conclusion: all meet PH assumption



#########################################################
############# Frailty model 
#########################################################

install.packages("frailtypack")
library(frailtypack)

##############unadjusted model
fit41=coxph( Surv(survivaltime,died)~arm+strata(strata)+frailty(siteid,df=4), data=data1, method="breslow")
summary(fit41)
cox.zph(fit41, transform = 'log')

##############adjusted model
fit42=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(cd4group)+strata(strata)+frailty(siteid,df=4), data=data1, method="breslow")
summary(fit42)
cox.zph(fit42, transform = 'log')


################################################# Subgroup analysis with cd4>=201: unadjusted model
fit43=coxph( Surv(survivaltime,died)~arm+strata(strata)+frailty(siteid,df=4), data=data1, subset=cd4group==1, method="breslow")
summary(fit43)
cox.zph(fit43, transform = 'log')

################################################# Subgroup analysis with cd4>=201: adjusted model
fit44=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(strata)+frailty(siteid,df=4), data=data1, subset=cd4group==1, method="breslow")
summary(fit44)
cox.zph(fit44, transform = 'log')

################################################# Subgroup analysis with cd4<=200: unadjusted model

fit45=coxph( Surv(survivaltime,died)~arm+strata(strata)+frailty(siteid,df=4), data=data1, subset=cd4group==0, method="breslow")
summary(fit45)
cox.zph(fit45, transform = 'log')

################################################# Subgroup analysis with cd4<=200: adjusted model

fit46=coxph( Surv(survivaltime,died)~arm+ageatenrolment+sex+idcheck+strata(strata)+frailty(siteid,df=4), data=data1, subset=cd4group==0, method="breslow")
summary(fit46)
cox.zph(fit46, transform = 'log')






#########################################################
############# GEE for the 2nd outcome
#########################################################

install.packages("geepack")
library(geepack)

fit6=geeglm(supprvl~arm+strata,family=binomial("logit"),id=siteid,corstr="exch",data=data2)
summary(fit6)

################95% CI
exp(c(0.1092,0.1092+c(-1,1)*1.96*0.0567))


#########################################################
############# GLMM for the 2nd outcome
#########################################################

install.packages("lme4")
library(lme4)

fit5=glmer(supprvl~arm+strata+(1|siteid),family=binomial("logit"),data=data2)
summary(fit5)

################95% CI
exp(c(0.0757,0.0757+c(-1,1)*1.96*0.1081))



