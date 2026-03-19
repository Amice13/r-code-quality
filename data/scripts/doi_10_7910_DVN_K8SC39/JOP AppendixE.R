library(foreign)
library(stargazer)
library(car)
library(lme4)
library(effects)
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(sampleSelection)
library(multiwayvcov)
library(MASS)
setwd('/Users/jsievert/Dropbox/APSA 2014/')


setwd('/Users/jsievert/Dropbox/APSA 2014/JOP Submission/JOP Replication')

appE <- read.csv('jop_appendixE.csv')

#all races
m <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov*pres_elec + postnov*pres_elec + gov_elec, iv ~ ivp + ipres + qual  + pres_elec + gov_elec , appE); summary(m)


m2 <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov + postnov + gov_elec, iv ~ ivp + ipres + qual + gov_elec   , subset(appE, pres_elec == 1 )); summary(m2)


m3 <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov + postnov + gov_elec, iv ~ ivp + ipres + qual + gov_elec , subset(appE, pres_elec == 0)); summary(m3)


#marginal races
m4 <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov*pres_elec + postnov*pres_elec + gov_elec, iv ~ ivp + ipres + qual + gov_elec + pres_elec , subset(appE, marginal == 1)); summary(m4)

m5 <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov + postnov + gov_elec, iv ~ ivp + ipres + qual + gov_elec , subset(appE, marginal == 1 & pres_elec == 1)); summary(m5)


m6 <- selection(inc_run ~ ivp + ipres + qual_t1 + open_t1 + inclost_t1 + prenov + postnov + gov_elec, iv ~ ivp + ipres + qual +  gov_elec   , subset(appE, marginal == 1 & pres_elec == 0)); summary(m6)


########################
#MODELS w/out SELECTION#
########################

mod <- lm(iv ~ ivp + ipres + qual + pres_elec + gov_elec , appE); summary(mod)

mod2 <- lm(iv ~ ivp + ipres + qual + gov_elec  + prenov + postnov  , subset(appE, pres_elec == 1 )); summary(mod2)

mod3 <- lm(iv ~ ivp + ipres + qual + gov_elec + prenov + postnov    , subset(appE, pres_elec == 0)); summary(mod3)


mod4 <- lm(iv ~ ivp + ipres + qual + prenov*pres_elec + postnov*pres_elec + gov_elec , subset(appE, marg <5)); summary(mod4)

mod5 <- lm(iv ~ ivp + ipres + qual + gov_elec  + prenov + postnov  , subset(appE, pres_elec == 1 & marg <5)); summary(mod5)

mod6 <- lm(iv ~ ivp + ipres + qual + gov_elec + prenov + postnov    , subset(appE, pres_elec == 0 & marg <5)); summary(mod6 )


#extract coefficients
tab <- array(NA, dim = c(6, 2))

tab[1, ] <- c(coef(m)[16], coef(mod)[4])
tab[2, ] <- c(coef(m2)[13], coef(mod2)[4])
tab[3, ] <- c(coef(m3)[13], coef(mod3)[4])
tab[4, ] <- c(coef(m4)[16], coef(mod4)[4])
tab[5, ] <- c(coef(m5)[13], coef(mod5)[4])
tab[6, ] <- c(coef(m6)[13], coef(mod6)[4])




