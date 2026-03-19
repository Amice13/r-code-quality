### Yu, Whang, and Lee ###
### When Does Audience Matter? Challengers' Stability and Audience Costs ###
### Replication Main Result ###

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
rm(list=ls())

### Load Packages
library(Hmisc)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(stargazer)
library(jtools)

### Load Dataset
data = read.csv('Yu, Whang, and Lee_dataset.csv')

### Table 1: Correlation between Regime and Stability
rcorr(cbind(data$polity2.1,data$stability.1,data$sfirev.1))

### Table 2: Effect of Stability on Reciprocation

## Without Control
mb1 = glm(recip~poly(stability.1,2)-1
          +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
          +styear_2000+styear_2001+styear_2002+styear_2003
          +styear_2004+styear_2005+styear_2006+styear_2007
          +styear_2008+styear_2009+styear_2010
          ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
mb2 = glm(recip~poly(sfirev.1,2)-1
          +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
          +styear_2000+styear_2001+styear_2002+styear_2003
          +styear_2004+styear_2005+styear_2006+styear_2007
          +styear_2008+styear_2009+styear_2010
          ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

# Robust SEs
mb1.c = coeftest(mb1,vcov=cluster.vcov(mb1, ~ ccode1+ccode2,use_white = T))
mb2.c = coeftest(mb2,vcov=cluster.vcov(mb2, ~ ccode1+ccode2,use_white = T))

## With Control
m1 = glm(recip~poly(stability.1,2)+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m2 = glm(recip~poly(sfirev.1,2)+polity2.1+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

# Robust SEs
m1.c = coeftest(m1,vcov=cluster.vcov(m1, ~ ccode1+ccode2,use_white = T))
m2.c = coeftest(m2,vcov=cluster.vcov(m2, ~ ccode1+ccode2,use_white = T))

## Full Results
stargazer(mb1,m1,mb2,m2,no.space = T
          ,se=list(mb1.c[,2],m1.c[,2],mb2.c[,2],m2.c[,2])
          ,type='text',
          omit='styear',
          order=c(1,2,14,15,3:13))

### Figure 1: Reciprocation ~ Stability
frame1 = data[rownames(model.frame(m1)),]
frame2 = data[rownames(model.frame(m2)),]

pre = make_predictions(m1,pred='stability.1',data=frame1,cluster=c('ccode1','ccode2'))

hist(frame1[frame1$democracy==1,]$stability.1,breaks=20,col='grey',border='white',xlim=range(frame1$stability.1),ylim=c(0,50),xlab='',ylab='',xaxt='n',yaxt='n',main='')
hist(frame1[frame1$democracy==0,]$stability.1,breaks=20,add=T,col=rgb(1,1,1,alpha=0))
par(new=T)
plot(recip~stability.1,data=pre,type='l',ylab='Probability of Target Reciprocation',xlab='Stability',range(frame1$stability.1),ylim=c(0.5,1),lwd=2)
lines(ymax~stability.1,data=pre,lty='dashed',lwd=2)
lines(ymin~stability.1,data=pre,lty='dashed',lwd=2)

legend('topright',legend=c('Democracy','Non-democracy'),fill=c('grey','white'))

### Figure 2: Reciprocation ~ SFI
pre2 = make_predictions(m2,pred='sfirev.1',data=frame2,cluster=c('ccode1','ccode2'))

hist(frame2[frame2$democracy==1,]$sfirev.1,breaks=20,border='white',col='grey',xlim=range(frame2$sfirev.1),ylim=c(0,100),xlab='',ylab='',xaxt='n',yaxt='n',main='')
hist(frame2[frame2$democracy==0,]$sfirev.1,breaks=20,add=T,col=rgb(1,1,1,alpha=0))
par(new=T)
plot(recip~sfirev.1,data=pre2,type='l',ylab='Probability of Target Reciprocation',xlab='State Fragility Index',ylim=c(0.5,1),xlim=range(frame2$sfirev.1),lwd=2)
lines(ymax~sfirev.1,data=pre2,lty='dashed',lwd=2)
lines(ymin~sfirev.1,data=pre2,lty='dashed',lwd=2)

legend('topright',legend=c('Democracy','Non-democracy'),fill=c('grey','white'))

### Descriptive Statistics (Full)
data$gdppc = log(data$gdppc)

m.s = glm(recip~sfirev.1+stability.1+polity2.1+cinc.ratio+affinity+dist.log+alliance+sqevaluation.1+sqevaluation.2+territory+gdpgrowth
          +gdppc+history,data=data,family=binomial(link='probit'))
stargazer(data[!is.na(data$sfirev.1)|!is.na(data$stability.1),c('recip',names(m.s$coefficients[-1]))],type='text')

### Table A1: Stability model data
stabilitydata = data[!is.na(data$stability.1),c('recip',names(m.s$coefficients[-1]))]
stargazer(stabilitydata,omit.summary.stat=c('p25','p75'),omit='sfi'
          ,type='text')

length(unique(data[!is.na(data$stability.1),]$ccode1))
length(unique(data[!is.na(data$stability.1),]$ccode2))
range(data[!is.na(data$stability.1),]$styear)

# Table A2: SFI model data
sfidata = data[!is.na(data$sfirev.1),c('recip',names(m.s$coefficients[-1]))]
stargazer(sfidata,omit.summary.stat=c('p25','p75'),omit='stability'
          ,type='text')

length(unique(data[!is.na(data$sfirev.1),]$ccode1))
length(unique(data[!is.na(data$sfirev.1),]$ccode2))
range(data[!is.na(data$sfirev.1),]$styear)
