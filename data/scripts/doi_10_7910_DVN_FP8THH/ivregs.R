#####################################################################################
# TESTING THE EXCLUSION RESTRICTION OF THE MORTALITY INSTRUMENT
#####################################################################################


library(stargazer)
library(texreg)
library(arm)
library(foreign)
library(car)
library(xtable)
library(Hmisc)
library(ggplot2)

#install.packages("sandwich")
#install.packages("lmtest")

data <- read.csv("complete.data.iv.csv")
attach(data)
names(data)

#SPLIT THE SAMPLE 
data.base = subset(data, baseco==1)
data.no.outliers = subset(data.base, shortnam!="MLI" & shortnam!="NGA" & shortnam!="GMB")
data.africa = subset(data, africa==1 & baseco==1)
data.asia = subset(data, asia==1 & baseco==1)
data.s.america= subset(data, s.america==1 & baseco==1)

#MORTALITY NOT RANDOMLY ASSIGNED TO CONTINENTS
ggplot(data, aes(x=continent, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)

# PLOTS -- TEST WHETHER MORTALITY PREDICTS INSTITUTIONS
# SHOULD SEE LOW MORTALITY AND HIGH PROTECTION AGAINST EXPROPRIATION RISK
ggplot(data.base, aes(x=avexpr, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)

#excluding outliers of MLI, NGA, GMB
ggplot(data.no.outliers, aes(x=avexpr, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)

# MORTALITY IN AFRICA
ggplot(data.africa, aes(x=avexpr, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)

# MORTALITY IN ASIA 
ggplot(data.asia, aes(x=avexpr, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)

# MORTALITY IN SOUTH AMERICA 
ggplot(data.s.america, aes(x=avexpr, y=logem4, shape=continent, colour=continent))+geom_point(position="jitter", size=3) +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)


# split by settlement dates
ggplot(data.base, aes(x=continent, y=yr.col, colour=continent))+geom_point(position="jitter") +
  geom_rug(position="jitter", size=.2) + geom_text(position="jitter", aes(label=shortnam), size=4, hjust=0)


log.inst.age.1817 = log(inst.age.1817)

#plots
plot(logem4~log.inst.age.1817)
plot(inst.age.1817)
plot(inst.age.1900)

# test whether institutional age predicts mortality rates
require("sandwich")
require("lmtest")
m1 = lm(mortality ~ inst.age.1817, data=data, which(baseco==1))
m1$newse<-vcovHC(m1)
coeftest(m1,m1$newse)

m2 = lm(logem4 ~ inst.age.1817, data=data, which(baseco==1))
m2$newse<-vcovHC(m2)
coeftest(m2,m2$newse)

m3 = lm(logem4 ~ inst.age.1900, data=data, which(baseco==1))
m3$newse<-vcovHC(m3)
coeftest(m3,m3$newse)

# Output table
stargazer(m1, m2, m3,
          title="OLS Regressions \\ 
          Dependent Variable: Mortality rate in 1817, \\ 
          Independent Variable: Institutional age in 1817",
          align=TRUE, dep.var.labels=c("Mortality rate",
                                       "Log Mortality rate",
                                       "Log Mortality rate"),
          covariate.labels=c("Institutional age in 1817", "Institutional age in 1900"),
          column.labels=c("Base Colony Sample"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))


#Now test whether institutional age predicts current GDP, using both
# the AJR GDP measures:
# ECONOMIC PERFORMANCE
#######################
# logpgp95 = log GDP per capita PPP in 1995, World Bank 
# loghjypl = log output per worker in 1988 (US normalized to 1), Hall & Jones
# logpgp12 = log GDP per capital PPP in 2012, World Bank (** our measure)

m4 = lm(logpgp95 ~ inst.age.1817, data=data, which(baseco==1))
m5 = lm(logpgp95 ~ inst.age.1900, data=data, which(baseco==1))

m6 = lm(loghjypl ~ inst.age.1817, data=data, which(baseco==1))
m7 = lm(loghjypl ~ inst.age.1900, data=data, which(baseco==1))

m8 = lm(logpgp12 ~ inst.age.1817, data=data, which(baseco==1))
m9 = lm(logpgp12 ~ inst.age.1900, data=data, which(baseco==1))


# Output table
stargazer(m4, m5, m6, m7, m8, m9, 
          title="OLS Regressions \\ 
          Dependent Variable: Economic Performance, \\ 
          Independent Variable: Institutional age in 1817 and 1900",
          align=TRUE, dep.var.labels=c("Log GDP 1995", "Log Output per Worker", "Log GDP 2012"),
          covariate.labels=c("Institutional age in 1817","Institutional Age in 1900"),
          column.labels=c("Base Colony Sample"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))


#Testing the exclusion restriction directly - mortality rate should not
# predict economic performance (there should be no direct effect of the 
# instrument on the dependent variable or any effect running 
# through omitted variables)

## WITH PAST INSTITUTIONS IN 1900 AS A CONTROL
# cons00a = constraint on executive in 1900

k1 = lm(logpgp95~logem4 + cons00a, data=data, which(baseco==1))
k2 = lm(loghjypl~logem4 + cons00a, data=data, which(baseco==1))
k3 = lm(logpgp12~logem4 + cons00a, data=data, which(baseco==1))
k4 = lm(logpgp12~logem4 + inst.age.1817 + cons00a, data=data, which(baseco==1))
k5 = lm(logpgp12~logem4 + inst.age.1900 + cons00a, data=data, which(baseco==1))

# Output table
stargazer(k1, k2, k3, k4, k5, 
          title="OLS Regressions \\ 
          Dependent Variable: Economic Performance, \\ 
          Independent Variable: Mortality Rate, Past Institutions, Institutional Age",
          align=TRUE, dep.var.labels=c("Log GDP 1995", "Log Output per Worker",
                                       "Log GDP 2012", "Log GDP 2012", "Log GDP 2012"),
          covariate.labels=c("Log Mortality rate","Institutional Age in 1817", 
                             "Institutional Age in 1900", "Constraint on the Executive in 1900"),
          column.labels=c("Base Colony Sample", "Base Colony Sample",
                          "Base Colony Sample", "Base Colony Sample", "Base Colony Sample"),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))

####################################################################
# SPLITTING THE SAMPLE INTO S. AMERICA & AFRICA - replication of table 4
####################################################################

# PLOTTING THE DENSITIES BY CONTINENT
ggplot(data.asia, aes(x=mortality))+ geom_density() 
ggplot(data.africa, aes(x=mortality)) + geom_density() 
ggplot(data.s.america, aes(x=mortality)) + geom_density() 

qplot(mortality, data=data, geom="density", alpha=I(.5),
      main="Distribution of Mortality", xlab="Mortality (deaths per 1,000 soldiers)",
      ylab="Density") 

qplot(mortality, data=data.asia, geom="density", alpha=I(.5),
      main="Distribution of Mortality", xlab="Mortality (deaths per 1,000 soldiers)",
      ylab="Density") 

#Mortality as a determinant of past institutions
l1 = lm(cons00a ~ logem4, data=data.base)
l2 = lm(cons00a ~ logem4, data=data.no.outliers)
l3 = lm(cons00a ~ logem4, data=data.s.america)
l4 = lm(cons00a ~ logem4, data=data.africa)
l5 = lm(cons00a ~ logem4, data=data.asia)

# Output table
stargazer(l1, l2, l3, l4, l5, 
          title="OLS Regressions \\ 
          Dependent Variable: Past Institutions, \\ 
          Independent Variable: Log Mortality Rate",
          align=TRUE, dep.var.labels=c("Constraint on Executive in 1900"),
          covariate.labels=c("Log Mortality rate"),
          column.labels=c("Base Colonies", "No Outliers", "S. America", "Africa",
                          "Asia"),
          column.separate=c(1,2,3,4,5,6),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))

#IV regressions (2 stage Least Squares)

# PANEL A - IV REGS -- Second Stage Least Squares
avexpr.p = predict(lm(avexpr ~ logem4, data.base))
m1 = lm(logpgp95 ~ avexpr.p, data.base) 

avexpr.p = predict(lm(avexpr ~ logem4, data.no.outliers))
m2 = lm(logpgp95 ~ avexpr.p, data.no.outliers)

avexpr.p = predict(lm(avexpr ~ logem4, data.s.america))
m3 = lm(logpgp95 ~ avexpr.p, data.s.america) 

avexpr.p = predict(lm(avexpr ~ logem4, data.africa))
m4 = lm(logpgp95 ~ avexpr.p, data.africa) 

avexpr.p = predict(lm(avexpr ~ logem4, data.asia))
m5 = lm(logpgp95 ~ avexpr.p, data.asia) 


stargazer(m1, m2, m3, m4, m5, 
          title="IV Regressions by Continent \\ 
          Dependent Variable: Economic Performance, \\ 
          Instrumental Variable: Log Mortality Rate",
          align=TRUE, dep.var.labels=c("Log GDP per capita 1995"),
          covariate.labels=c("Average protection against expropriation risk, 1985-1995"),
          column.labels=c("Base Colonies","No Outliers", "S. America", "Africa", "Asia"
                          ),
          column.separate=c(1,2,3,4,5,6,7,8),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("3pt"))



# PANEL B - FIRST STAGE Least Squares
m1 = lm(avexpr ~ logem4, data.base)
m2 = lm(avexpr ~ logem4, data.no.outliers)
m3 = lm(avexpr ~ logem4, data.s.america)
m4 = lm(avexpr ~ logem4, data.africa)
m5 = lm(avexpr ~ logem4, data.asia)


stargazer(m1, m2, m3, m4, m5, 
          title="First Stage IV Regressions of Average protection against expropriation risk in 1985-1995,
          Replication of AJR Table 4, Panel B, \\ 
          Dependent Variable: Average protection against expropriation risk in 1985-1995,
          \\ Instrument for Institutions: 
          Log European Settler Mortality",
          align=TRUE, dep.var.labels=c("Average protection against expropriation risk, 1985-1995"),
          covariate.labels=c("Log European Settler Mortality"),
          column.labels=c("Base Colonies","No Outliers", "S. America", "Africa", "Asia"
          ),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))

# PANEL C - OLS
##################################################################################
# DV = logpgp95

m1 = lm(logpgp95 ~ avexpr, data=data.base)
m2 = lm(logpgp95 ~ avexpr, data=data.no.outliers)
m3 = lm(logpgp95 ~ avexpr, data=data.s.america)
m4 = lm(logpgp95 ~ avexpr, data=data.africa)
m5 = lm(logpgp95 ~ avexpr, data=data.asia)

stargazer(m1, m2, m3, m4, m5, 
          title="OLS Regressions of Economic Performance,
          Replication of AJR Table 4, Panel C, \\ 
          Dependent Variable: Economic Performance (Log GDP per Capita 1995),
          \\ Institutions Measure: 
          Average protection against expropriation risk, 1985-1995",
          align=TRUE, dep.var.labels=c("Log GDP per Capita (PPP) 1995"),
          covariate.labels=c("Average protection against expropriation risk, 1985-1995"),
          column.labels=c("Base Colonies","No Outliers", "S. America", "Africa", "Asia"
          ),
          notes.align=c("l"),
          omit.stat=c("LL", "ser","f"), no.space=TRUE, 
          font.size=c("scriptsize"),
          digits=2,
          column.sep.width=c("1pt"))





