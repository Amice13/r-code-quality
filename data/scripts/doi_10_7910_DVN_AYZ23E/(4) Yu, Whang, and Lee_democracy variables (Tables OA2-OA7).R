### Yu, Whang, and Lee ###
### When Does Audience Matter? Challengers' Stability and Audience Costs ###
### Replication Democracy Variables ###

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
rm(list=ls())

### Load Packages
library(Hmisc)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(stargazer)

### Load Dataset
data = read.csv('Yu, Whang, and Lee_dataset.csv')

### Polity IV source

## democracy (Polity2 >= 7)

m11 = glm(recip~poly(stability.1,2)+democracy+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m12 = glm(recip~poly(sfirev.1,2)+democracy+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m11.c = coeftest(m11,vcov=cluster.vcov(m11, ~ ccode1+ccode2,use_white = T))
m12.c = coeftest(m12,vcov=cluster.vcov(m12, ~ ccode1+ccode2,use_white = T))

## exrec (Executive Recruitment)

m13 = glm(recip~poly(stability.1,2)+exrec+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m14 = glm(recip~poly(sfirev.1,2)+exrec+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m13.c = coeftest(m13,vcov=cluster.vcov(m13, ~ ccode1+ccode2,use_white = T))
m14.c = coeftest(m14,vcov=cluster.vcov(m14, ~ ccode1+ccode2,use_white = T))

## exconst (Executive Constraints)

m15 = glm(recip~poly(stability.1,2)+exconst+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m16 = glm(recip~poly(sfirev.1,2)+exconst+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m15.c = coeftest(m15,vcov=cluster.vcov(m15, ~ ccode1+ccode2,use_white = T))
m16.c = coeftest(m16,vcov=cluster.vcov(m16, ~ ccode1+ccode2,use_white = T))

## polcomp (Political Competition and Opposition)

m17 = glm(recip~poly(stability.1,2)+polcomp+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m18 = glm(recip~poly(sfirev.1,2)+polcomp+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m17.c = coeftest(m17,vcov=cluster.vcov(m17, ~ ccode1+ccode2,use_white = T))
m18.c = coeftest(m18,vcov=cluster.vcov(m18, ~ ccode1+ccode2,use_white = T))

### V-Dem source

## v2x_polyarchy (Electoral)

m21 = glm(recip~poly(stability.1,2)+v2x_polyarchy+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m22 = glm(recip~poly(sfirev.1,2)+v2x_polyarchy+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m21.c = coeftest(m21,vcov=cluster.vcov(m21, ~ ccode1+ccode2,use_white = T))
m22.c = coeftest(m22,vcov=cluster.vcov(m22, ~ ccode1+ccode2,use_white = T))

## v2x_libdem (Liberal)

m23 = glm(recip~poly(stability.1,2)+v2x_libdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m24 = glm(recip~poly(sfirev.1,2)+v2x_libdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m23.c = coeftest(m23,vcov=cluster.vcov(m23, ~ ccode1+ccode2,use_white = T))
m24.c = coeftest(m24,vcov=cluster.vcov(m24, ~ ccode1+ccode2,use_white = T))

stargazer(m23,m24,type='text',se=list(m23.c[,2],m24.c[,2]),keep=c('stability','sfirev'))

## v2x_partipdem (Participatory)

m25 = glm(recip~poly(stability.1,2)+v2x_partipdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m26 = glm(recip~poly(sfirev.1,2)+v2x_partipdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m25.c = coeftest(m25,vcov=cluster.vcov(m25, ~ ccode1+ccode2,use_white = T))
m26.c = coeftest(m26,vcov=cluster.vcov(m26, ~ ccode1+ccode2,use_white = T))

## v2x_delibdem (Deliberate)

m27 = glm(recip~poly(stability.1,2)+v2x_delibdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m28 = glm(recip~poly(sfirev.1,2)+v2x_delibdem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m27.c = coeftest(m27,vcov=cluster.vcov(m27, ~ ccode1+ccode2,use_white = T))
m28.c = coeftest(m28,vcov=cluster.vcov(m28, ~ ccode1+ccode2,use_white = T))

## v2x_egaldem (Egalitarian)

m29 = glm(recip~poly(stability.1,2)+v2x_egaldem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m30 = glm(recip~poly(sfirev.1,2)+v2x_egaldem+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m29.c = coeftest(m29,vcov=cluster.vcov(m29, ~ ccode1+ccode2,use_white = T))
m30.c = coeftest(m30,vcov=cluster.vcov(m30, ~ ccode1+ccode2,use_white = T))

### Boix et al. (e_boix_regime)

m31 = glm(recip~poly(stability.1,2)+e_boix_regime+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$stability.1),],family=binomial(link='probit'))
m32 = glm(recip~poly(sfirev.1,2)+e_boix_regime+cinc.ratio+affinity+dist.log+alliance
         +sqevaluation.1+sqevaluation.2+territory+gdpgrowth
         +log(gdppc)+history-1
         +styear_1995+styear_1996+styear_1997+styear_1998+styear_1999
         +styear_2000+styear_2001+styear_2002+styear_2003
         +styear_2004+styear_2005+styear_2006+styear_2007
         +styear_2008+styear_2009+styear_2010
         ,data=data[!is.na(data$sfirev.1),],family=binomial(link='probit'))

m31.c = coeftest(m31,vcov=cluster.vcov(m31, ~ ccode1+ccode2,use_white = T))
m32.c = coeftest(m32,vcov=cluster.vcov(m32, ~ ccode1+ccode2,use_white = T))

### Table OA4
stargazer(m11,m13,m15,m17,m31,omit='styear',
          se=list(m11.c[,2],
                  m13.c[,2],
                  m15.c[,2],
                  m17.c[,2],
                  m31.c[,2]),
          type='text',
          no.space = T)

### Table OA5
stargazer(m12,m14,m16,m18,m32,omit='styear',
          se=list(m12.c[,2],
                  m14.c[,2],
                  m16.c[,2],
                  m18.c[,2],
                  m32.c[,2]),
          type='text',
          no.space = T)

### Table OA6
stargazer(m21,m23,m25,m27,m29,omit='styear',
          se=list(m21.c[,2],
                  m23.c[,2],
                  m25.c[,2],
                  m27.c[,2],
                  m29.c[,2]),
          type='text',
          no.space = T)

### Table OA7
stargazer(m22,m24,m26,m28,m30,omit='styear',
          se=list(m22.c[,2],
                  m24.c[,2],
                  m26.c[,2],
                  m28.c[,2],
                  m30.c[,2]),
          type='text',
          no.space = T)

## Descriptive Statistics (Full)
data$gdppc = log(data$gdppc)

m.s = glm(recip~sfirev.1+stability.1+
            democracy+exrec+exconst+polcomp+e_boix_regime+
            v2x_polyarchy+v2x_libdem+v2x_partipdem+v2x_delibdem+v2x_egaldem
            +cinc.ratio+affinity+dist.log+alliance+sqevaluation.1+sqevaluation.2+territory+gdpgrowth
          +gdppc+history,data=data,family=binomial(link='probit'))
stargazer(data[!is.na(data$sfirev.1)|!is.na(data$stability.1),c('recip',names(m.s$coefficients[-1]))],type='text')

# Table OA2: Stability model data
stabilitydata = data[!is.na(data$stability.1),c('recip',names(m.s$coefficients[-1]))]
stargazer(stabilitydata,omit.summary.stat=c('p25','p75'),omit='sfi'
          ,type='text',no.space = T)

length(unique(data[!is.na(data$stability.1),]$ccode1))
length(unique(data[!is.na(data$stability.1),]$ccode2))
range(data[!is.na(data$stability.1),]$styear)

# Table OA3: SFI model data
sfidata = data[!is.na(data$sfirev.1),c('recip',names(m.s$coefficients[-1]))]
stargazer(sfidata,omit.summary.stat=c('p25','p75'),omit='stability'
          ,type='text',no.space = T)

length(unique(data[!is.na(data$sfirev.1),]$ccode1))
length(unique(data[!is.na(data$sfirev.1),]$ccode2))
range(data[!is.na(data$sfirev.1),]$styear)
