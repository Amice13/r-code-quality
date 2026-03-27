#front matter
rm(list=ls())
library(foreign)
library(lattice)
library(utils)
library(Hmisc)
library(xtable)
library(mice)
library(pscl)
library(car)
library(gmodels)

##set the following to YOUR working directory##
setwd('/Volumes/MONOGAN/psARE/')

#read data
data.0<-read.dta('palsStrictPartic.dta')
#data.0<-read.csv('palsStrictPartic.csv')

##quick look at presidential vote choice by strictness##
#evangelical indicator
CrossTable(x=data.0$evangelical,y=data.0$bush,prop.c=F,prop.chisq=F,prop.t=F,chisq=T)

#religious conservatism scale
CrossTable(x=data.0$ri_conslib,y=data.0$bush,prop.c=F,prop.chisq=F,prop.t=F,chisq=T)

#exogenous strictness, and with a median split
CrossTable(x=data.0$strictness,y=data.0$bush,prop.c=F,prop.chisq=F,prop.t=F,chisq=T)
data.0$strict.2<-as.numeric(data.0$strictness>median(data.0$strictness))
CrossTable(x=data.0$strict.2,y=data.0$bush,prop.c=F,prop.chisq=F,prop.t=F,chisq=T)

#individual cost of religion, crosstab with a median split and logit
data.0$cost.2<-as.numeric(data.0$cost>median(data.0$cost,na.rm=T))
CrossTable(x=data.0$cost.2,y=data.0$bush,prop.c=F,prop.chisq=F,prop.t=F,chisq=T)
mod.bush<-glm(bush~cost,family=binomial(link='logit'),data=data.0);summary(mod.bush);summary(predict(mod.bush,type="response"))

##quick measure of strictness by religious tradition##
mean(data.0$cost[data.0$protTrad==1],na.rm=T)
mean(data.0$cost[data.0$protTrad==3],na.rm=T)
mean(data.0$cost[data.0$protTrad==4],na.rm=T)
mean(data.0$cost[data.0$protTrad==5],na.rm=T)
mean(data.0$cost[data.0$protTrad==6],na.rm=T)
mean(data.0$cost[is.na(data.0$protTrad)],na.rm=T)

##drop separate indicators of participation, ID variable, and protTrad variable##
data<-subset(data.0,select=-c(vote, po_sign, po_work, po_contact, po_persuade, po_attend, po_gave, po_workrel, po_demonstrate,resp_zri,protTrad))

##multiple random imputation##
imp<-mice(data)

###MODELS RUN WITH EXOGENOUS STRICTNESS###
#political participation: zero-inflated negative binomial
mods.participation <- with(data=imp,exp=zeroinfl(participation~strictness+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))
#mods.participation <- with(data=imp,exp=zeroinfl(participation~strictness+civic+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))

#diagnostics
#zero-inflation? Wald test
#Wald test
rhs<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
hm<-rbind(
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
)
linearHypothesis(mods.participation$analyses[[1]],hm,rhs)
linearHypothesis(mods.participation$analyses[[2]],hm,rhs)
linearHypothesis(mods.participation$analyses[[3]],hm,rhs)
linearHypothesis(mods.participation$analyses[[4]],hm,rhs)
linearHypothesis(mods.participation$analyses[[5]],hm,rhs)
#All five tests show the zero model to be essential.

#overdispersion? Following Cameron & Trivedi 935
avg.theta<-(log(mods.participation$analyses[[1]]$theta)+log(mods.participation$analyses[[2]]$theta)+log(mods.participation$analyses[[3]]$theta)+log(mods.participation$analyses[[4]]$theta)+log(mods.participation$analyses[[5]]$theta))/5

avg.within<-(mods.participation$analyses[[1]]$SE.logtheta^2
+mods.participation$analyses[[2]]$SE.logtheta^2
+mods.participation$analyses[[3]]$SE.logtheta^2
+mods.participation$analyses[[4]]$SE.logtheta^2
+mods.participation$analyses[[5]]$SE.logtheta^2)/5

between<-((log(mods.participation$analyses[[1]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[2]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[3]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[4]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[5]]$theta)-avg.theta)^2)/4

total.var<-avg.within+(1.2)*between

z<-avg.theta/sqrt(total.var)
z; 1-pnorm(z)
#The dispersion parameter is essential.

#Equation for Social Participation
mods.civic<-lm.mids(civic~strictness+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Income
mods.income<-lm.mids(dm_income~strictness+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Church Participation
mods.attendance<-lm.mids(attendance~strictness+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Print results in LaTeX file
#sink('palsAdditional.tex')
print("EXOGNEOUS STRICTNESS")
print("participation")
xtable(summary(pool(mods.participation))[,-c(6:11)],digits=4)
print("civic")
xtable(summary(pool(mods.civic))[,-c(6:11)],digits=4)
print("income")
xtable(summary(pool(mods.income))[,-c(6:11)],digits=4)
print("church attendance")
xtable(summary(pool(mods.attendance))[,-c(6:11)],digits=4)
#sink()

###MODELS RUN WITH RELIGIOUS CONSERVATISM###
#political participation: zero-inflated negative binomial
mods.participation <- with(data=imp,exp=zeroinfl(participation~ri_conslib+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))

#diagnostics
#zero-inflation? Wald test
#Wald test
rhs<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
hm<-rbind(
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
)
linearHypothesis(mods.participation$analyses[[1]],hm,rhs)
linearHypothesis(mods.participation$analyses[[2]],hm,rhs)
linearHypothesis(mods.participation$analyses[[3]],hm,rhs)
linearHypothesis(mods.participation$analyses[[4]],hm,rhs)
linearHypothesis(mods.participation$analyses[[5]],hm,rhs)
#All five tests show the zero model to be essential.

#overdispersion? Following Cameron & Trivedi 935
avg.theta<-(log(mods.participation$analyses[[1]]$theta)+log(mods.participation$analyses[[2]]$theta)+log(mods.participation$analyses[[3]]$theta)+log(mods.participation$analyses[[4]]$theta)+log(mods.participation$analyses[[5]]$theta))/5

avg.within<-(mods.participation$analyses[[1]]$SE.logtheta^2
+mods.participation$analyses[[2]]$SE.logtheta^2
+mods.participation$analyses[[3]]$SE.logtheta^2
+mods.participation$analyses[[4]]$SE.logtheta^2
+mods.participation$analyses[[5]]$SE.logtheta^2)/5

between<-((log(mods.participation$analyses[[1]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[2]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[3]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[4]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[5]]$theta)-avg.theta)^2)/4

total.var<-avg.within+(1.2)*between

z<-avg.theta/sqrt(total.var)
z; 1-pnorm(z)
#The dispersion parameter is essential.

#Equation for Social Participation
mods.civic<-lm.mids(civic~ri_conslib+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Income
mods.income<-lm.mids(dm_income~ri_conslib+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Church Participation
mods.attendance<-lm.mids(attendance~ri_conslib+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Print results in LaTeX file
#sink('palsAdditional.tex',append=TRUE)
print("RELIGIOUS CONSERVATISM")
print("participation")
xtable(summary(pool(mods.participation))[,-c(6:11)],digits=4)
print("civic")
xtable(summary(pool(mods.civic))[,-c(6:11)],digits=4)
print("income")
xtable(summary(pool(mods.income))[,-c(6:11)],digits=4)
print("church attendance")
xtable(summary(pool(mods.attendance))[,-c(6:11)],digits=4)
#sink()


###MODELS RUN WITH INDIVIDUAL STRICTNESS###
###THESE ARE THE RESULTS PRESENTED IN THE PRINT EDITION###
#political participation: zero-inflated negative binomial
mods.participation <- with(data=imp,exp=zeroinfl(participation~cost+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))

#diagnostics
#zero-inflation? Wald test
#Wald test
rhs<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
hm<-rbind(
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
)
linearHypothesis(mods.participation$analyses[[1]],hm,rhs)
linearHypothesis(mods.participation$analyses[[2]],hm,rhs)
linearHypothesis(mods.participation$analyses[[3]],hm,rhs)
linearHypothesis(mods.participation$analyses[[4]],hm,rhs)
linearHypothesis(mods.participation$analyses[[5]],hm,rhs)
#All five tests show the zero model to be essential.

#overdispersion? Following Cameron & Trivedi 935
avg.theta<-(log(mods.participation$analyses[[1]]$theta)+log(mods.participation$analyses[[2]]$theta)+log(mods.participation$analyses[[3]]$theta)+log(mods.participation$analyses[[4]]$theta)+log(mods.participation$analyses[[5]]$theta))/5

avg.within<-(mods.participation$analyses[[1]]$SE.logtheta^2
+mods.participation$analyses[[2]]$SE.logtheta^2
+mods.participation$analyses[[3]]$SE.logtheta^2
+mods.participation$analyses[[4]]$SE.logtheta^2
+mods.participation$analyses[[5]]$SE.logtheta^2)/5

between<-((log(mods.participation$analyses[[1]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[2]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[3]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[4]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[5]]$theta)-avg.theta)^2)/4

total.var<-avg.within+(1.2)*between

z<-avg.theta/sqrt(total.var)
z; 1-pnorm(z)
#The dispersion parameter is essential.

#Equation for Social Participation
mods.civic<-lm.mids(civic~cost+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Income
mods.income<-lm.mids(dm_income~cost+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Church Participation
mods.attendance<-lm.mids(attendance~cost+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Print results in LaTeX file
#sink('palsAdditional.tex',append=TRUE)
print("INDIVIDUAL STRICTNESS")
print("participation")
xtable(summary(pool(mods.participation))[,-c(6:11)],digits=4)
print("civic")
xtable(summary(pool(mods.civic))[,-c(6:11)],digits=4)
print("income")
xtable(summary(pool(mods.income))[,-c(6:11)],digits=4)
print("church attendance")
xtable(summary(pool(mods.attendance))[,-c(6:11)],digits=4)
#sink()

###MODELS RUN WITH EVANGELICAL INDICATOR###
#political participation: zero-inflated negative binomial
mods.participation <- with(data=imp,exp=zeroinfl(participation~evangelical+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))

#diagnostics
#zero-inflation? Wald test
#Wald test
rhs<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
hm<-rbind(
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
)
linearHypothesis(mods.participation$analyses[[1]],hm,rhs)
linearHypothesis(mods.participation$analyses[[2]],hm,rhs)
linearHypothesis(mods.participation$analyses[[3]],hm,rhs)
linearHypothesis(mods.participation$analyses[[4]],hm,rhs)
linearHypothesis(mods.participation$analyses[[5]],hm,rhs)
#All five tests show the zero model to be essential.

#overdispersion? Following Cameron & Trivedi 935
avg.theta<-(log(mods.participation$analyses[[1]]$theta)+log(mods.participation$analyses[[2]]$theta)+log(mods.participation$analyses[[3]]$theta)+log(mods.participation$analyses[[4]]$theta)+log(mods.participation$analyses[[5]]$theta))/5

avg.within<-(mods.participation$analyses[[1]]$SE.logtheta^2
+mods.participation$analyses[[2]]$SE.logtheta^2
+mods.participation$analyses[[3]]$SE.logtheta^2
+mods.participation$analyses[[4]]$SE.logtheta^2
+mods.participation$analyses[[5]]$SE.logtheta^2)/5

between<-((log(mods.participation$analyses[[1]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[2]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[3]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[4]]$theta)-avg.theta)^2+(log(mods.participation$analyses[[5]]$theta)-avg.theta)^2)/4

total.var<-avg.within+(1.2)*between

z<-avg.theta/sqrt(total.var)
z; 1-pnorm(z)
#The dispersion parameter is essential.

#Equation for Social Participation
mods.civic<-lm.mids(civic~evangelical+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Income
mods.income<-lm.mids(dm_income~evangelical+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Church Participation
mods.attendance<-lm.mids(attendance~evangelical+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Print results in LaTeX file
#sink('palsAdditional.tex', append=TRUE)
print("EVANGELICAL INDICATOR")
print("participation")
xtable(summary(pool(mods.participation))[,-c(6:11)],digits=4)
print("civic")
xtable(summary(pool(mods.civic))[,-c(6:11)],digits=4)
print("income")
xtable(summary(pool(mods.income))[,-c(6:11)],digits=4)
print("church attendance")
xtable(summary(pool(mods.attendance))[,-c(6:11)],digits=4)
#sink()


###MODEL WITH PROTESTANTS ONLY, BY TRADITION###
#subset and clean data
data.2<-subset(data.0,!is.na(protTrad),select=-c(vote, po_sign, po_work, po_contact, po_persuade, po_attend, po_gave, po_workrel, po_demonstrate,resp_zri,evangelical,ri_conslib,cost,strictness))
data.2$protTrad<-recode(data.2$protTrad,'1="Pentecostal";3="Fundamentalist";4="Evangelical";5="Mainline Protestant";6="Liberal Protestant"')

#multiple random imputation on subset
imp<-mice(data.2)

#political participation: only a Poisson converges
#mods.participation <- with(data=imp,exp=zeroinfl(participation~protTrad+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, dist="negbin"))
#mods.participation <- with(data=imp,exp=glm.nb(participation~protTrad+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed))
mods.participation <- glm.mids(participation~protTrad+civic+dm_income+attendance+hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, family="poisson"(link='log'),data=imp)

#Equation for Social Participation
mods.civic<-lm.mids(civic~ protTrad +hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Income
mods.income<-lm.mids(dm_income~ protTrad +hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Equation for Church Participation
mods.attendance<-lm.mids(attendance~ protTrad +hr_age+I((hr_age)^2)+education+hr_gender+black+hispanic+other+strength+recruitment+south+homeowner+wj_unemployed, data=imp)

#Print results in LaTeX file
#sink('palsAdditional.tex', append=TRUE)
print("PROTESTANT TRADITIONS")
print("participation")
xtable(summary(pool(mods.participation))[,-c(6:11)],digits=4)
print("civic")
xtable(summary(pool(mods.civic))[,-c(6:11)],digits=4)
print("income")
xtable(summary(pool(mods.income))[,-c(6:11)],digits=4)
print("church attendance")
xtable(summary(pool(mods.attendance))[,-c(6:11)],digits=4)
#sink()


