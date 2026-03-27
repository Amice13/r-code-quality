# following example listed here: 
#https://www.rdocumentation.org/packages/mediation/versions/4.5.0/topics/medsens
#install the package
# install.packages("mediation")
# install.packages("haven")
# install.packages("lme4")
library(haven)
library(mediation)
library(ggplot2)
source("C:/Users/dbrown16/Downloads/medsens.R")

setwd("G:/My Drive/Projects--Closed/Ford Program/Kenya Education Project/Replication")
#load the data
student <- read_dta("BJPS_blecketal2025_anon.dta")
d0<-as_factor(student)

#create cluster
my.cluster<-(d0$schoolname)
my.weight<-(d0$weight3)

#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGq59comf<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$q59_comf)

sum(MISSINGq59comf)

d0_q59comf <- subset(d0, 
                     subset = !MISSINGq59comf)


#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGtrustsame<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$trustsame)

sum(MISSINGtrustsame)

d0_trustsame <- subset(d0, 
                     subset = !MISSINGtrustsame)

#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGq51_ever<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$q51_ever)

sum(MISSINGq51_ever)

d0_q51_ever <- subset(d0, 
                       subset = !MISSINGq51_ever)

#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGq50_ever<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$q50_ever)

sum(MISSINGq50_ever)

d0_q50_ever <- subset(d0, 
                       subset = !MISSINGq50_ever)

#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGidentity3<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$identity3)

sum(MISSINGidentity3)

d0_identity3 <- subset(d0, 
                      subset = !MISSINGidentity3)

#create subsets of the data for each sensitivity analysis, which omit missing observations
MISSINGidentity4<- is.na(d0$fr_diffeth) |
  is.na(d0$ingroup2) |
  is.na(d0$q16) |
  is.na(d0$gender) | 
  is.na(d0$q4_rc_1) |
  is.na(d0$q56_comf) |
  is.na(d0$identity4)

sum(MISSINGidentity4)

d0_identity4 <- subset(d0, 
                       subset = !MISSINGidentity4)

#create cluster
my.cluster<-(d0$schoolname)
my.weightq59comf<-(d0_q59comf$weight3)
my.weighttrustsame<-(d0_trustsame$weight3)
my.weightq50_ever<-(d0_q50_ever$weight3)
my.weightq51_ever<-(d0_q51_ever$weight3)
my.weightidentity3<-(d0_identity3$weight3)
my.weightidentity4<-(d0_identity4$weight3)


# fit models : q59_comf
b<-lm(fr_diffeth ~ national + q56_comf + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weightq59comf, data=d0_q59comf)
c<-glm(q59_comf ~ national + fr_diffeth + q56_comf + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weightq59comf, 
          family=quasibinomial(link="probit"), data=d0_q59comf)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function (note I removed 
  #this subcommand from the example: effect.type="both", because 
  #I want the indirect effect which is the default )
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)
png("q59_comf_sensitivity_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Tolerance")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()


###OUTCOME 2: Trustsame
source("C:/Users/dbrown16/Downloads/medsens.R")

# fit models--note removed 
b<-lm(fr_diffeth ~ national + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weighttrustsame, data=d0_trustsame)
c<-glm(trustsame ~ national + fr_diffeth + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weighttrustsame, 
       family=quasibinomial(link="probit"), data=d0_trustsame)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function 
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)

setwd("G:/My Drive/Projects--Closed/Ford Program/Kenya Education Project/Replication")
png("trustsame_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Trust")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()


###OUTCOME 3: q51_ever
source("C:/Users/dbrown16/Downloads/medsens.R")

# fit models--note removed 
b<-lm(fr_diffeth ~ national + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weightq51_ever, data=d0_q51_ever)
c<-glm(q51_ever ~ national + fr_diffeth + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weightq51_ever, 
       family=quasibinomial(link="probit"), data=d0_q51_ever)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function 
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)

setwd("G:/My Drive/Projects--Closed/Ford Program/Kenya Education Project/Replication")
png("q51_ever_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Lent Possessions")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()

###OUTCOME 4: q50_ever
source("C:/Users/dbrown16/Downloads/medsens.R")


# fit models 
b<-lm(fr_diffeth ~ national + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weightq51_ever, data=d0_q51_ever)
c<-glm(q50_ever ~ national + fr_diffeth + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weightq51_ever, 
       family=quasibinomial(link="probit"), data=d0_q51_ever)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function 
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)

png("q50_ever_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Lent Money")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()


###OUTCOME 6: identity3
source("C:/Users/dbrown16/Downloads/medsens.R")

# fit models 
b<-lm(fr_diffeth ~ national + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weightidentity3, data=d0_identity3)
c<-glm(identity3 ~ national + fr_diffeth + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weightidentity3, 
       family=quasibinomial(link="probit"), data=d0_identity3)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function 
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)

png("identity3_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Nationality Ranked Above Local Identities")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()


###OUTCOME 8: identity4
source("C:/Users/dbrown16/Downloads/medsens.R")

# fit models 
b<-lm(fr_diffeth ~ national + ingroup2 + q16 + gender + q4_rc_1 + 
        q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + q4_rc_6+c_1 + c_2 +c_3 + 
        c_4, weights=my.weightidentity4, data=d0_identity4)
c<-glm(identity4 ~ national + fr_diffeth + ingroup2 + q16 + 
         gender + q4_rc_1 + q4_rc_2  + q4_rc_3 + q4_rc_4  + q4_rc_5 + 
         q4_rc_6+c_1 + c_2 +c_3 + c_4,  weights=my.weightidentity4, 
       family=quasibinomial(link="probit"), data=d0_identity4)

#pass model through mediation function
med.cont <- mediate(b, c, treat="national", mediator="fr_diffeth", cluster=my.cluster, sims=10000)
summary(med.cont)

# Pass mediate output through medsens function 
sens.cont <- medsens(med.cont, rho.by=.1, effect.type="indirect")

# Use summary function to display results
summary(sens.cont)

png("identity4_Rplot.png")
# Plot true ACMEs and ADEs as functions of rho
par.orig <- par(mfrow = c(2,2))
plot(sens.cont, sens.par = "rho", main="Ethnicity Ranked above Nation/Africa")

# Plot true ACMEs and ADEs as functions of "R square tildes"
plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = 1,  ylim=c(0,.6), xlim=c(0,.7))

par(par.orig)
dev.off()