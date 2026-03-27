### Yu, Whang, and Lee ###
### When Does Audience Matter? Challengers' Stability and Audience Costs ###
### Replication Comparison between Linear and Curvilinear Specifications ###

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
rm(list=ls())

### Load Packages
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(stargazer)

### Load Dataset
data = read.csv('Yu, Whang, and Lee_dataset.csv')

### Table OA1: Comparison between Linear and Curvilinear Specifications

## Linear Specification
m1 = glm(recip~stability.1+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m2 = glm(recip~sfirev.1+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m1.c = coeftest(m1,vcov=cluster.vcov(m1, ~ ccode1+ccode2,use_white = T))
m2.c = coeftest(m2,vcov=cluster.vcov(m2, ~ ccode1+ccode2,use_white = T))

## Curvilinear Specification (Main Result)
m3 = glm(recip~poly(stability.1,2)+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m4 = glm(recip~poly(sfirev.1,2)+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m3.c = coeftest(m3,vcov=cluster.vcov(m3, ~ ccode1+ccode2,use_white = T))
m4.c = coeftest(m4,vcov=cluster.vcov(m4, ~ ccode1+ccode2,use_white = T))

## Results
stargazer(m1,m3,m2,m4,no.space=T,
          se=list(m1.c[,2],m3.c[,2],m2.c[,2],m4.c[,2]),
          omit='styear',
          type='text')
