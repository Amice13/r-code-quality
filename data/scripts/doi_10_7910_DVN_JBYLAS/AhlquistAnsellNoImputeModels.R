##
#This file fits models to complete cases, as reported in the Appendix
# to Ahlquist & Ansell, "Taking Credit"
library(lattice)
library(foreign)
library(MASS)
library(ggplot2)
library(Hmisc)
library(coda)
library(R2WinBUGS)
library(arm)
library(mcmcplots)

#settings
seeds<-345
set.seed(seeds)#for replication purposes
bugs.dir<-"c:/Program Files (x86)/WinBUGS14/"  #user to input BUGS location
work.dir<-"U:/AhlquistAnsell"  #user to set working directory
setwd(work.dir) 


#getting data

master.df<-read.csv("AhlquistAnsellWorldPoliticsReplicationData.csv", header=T) #remember to update file

mod.form <- d.BDK.pvt.credit.gdp ~ lag.BDK.pvt.credit.gdp + lag.top1pct.interp*gol.maj +
 d.top1pct.interp*gol.maj + lag.pwt.log.gdp + d.pwt.log.gdp + log(lag.pwt.POP) + d.pwt.log.POP + eurozone
#using maj b/c not a sensitive at early part of time series.

lm.1<-lm(mod.form, data = master.df)

X<-data.frame(lm.1$model)
n.obs<-nrow(X)
countries<-master.df$imf.code[match(row.names(X),rownames(master.df))]
countries.polity<-as.character(master.df$polity.num[match(row.names(X),rownames(master.df))])
wbt<-master.df$wb.code[match(row.names(X),rownames(master.df))]
year<-master.df$years[match(row.names(X),rownames(master.df))]
n.years<-length(unique(year))
n.countries<-length(unique(countries))
years.d<-year - (min(year)-1) #getting years in numerical order for JAGS
country<-rep(1,n.obs) #getting countries in numerical order for JAGS
for(i in 2:n.obs){
  if(countries[i] ==countries[i-1]){
    country[i]<-country[i-1]}
 if( countries[i] != countries[i-1]){
    country[i]<-country[i-1]+1}
}

X<-data.frame(scale(X))
X$eurozone<-lm.1$model$eurozone
names(X)<-c("d.credit", "lag.credit", "lag.ineq", "cum.left", "d.ineq",
			"lag.gdp", "d.gdp", "lag.pop", "d.pop", "eurozone")
attach(X)
offsets<-rep(NA, n.obs) #
for(i in 2:n.obs){
  if(country[i]!=country[i-1]) offsets[i]<-i}
offsets[1]<-1
offsets<-na.omit(offsets)
n.offsets<-length(offsets) #should be same as n.countries

simp.mod.data<-list("country","n.obs", "n.countries",
                       "n.years", "d.credit", "lag.credit",
                       "lag.ineq","d.ineq", "cum.left",
                       "lag.pop", "d.pop",
                       "d.gdp","lag.gdp",
                       "years.d", "eurozone")
simp.mod.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                        "b.cl", "b.l.top1cl", "b.d.top1cl",
                        "b.l.pop", "b.d.pop",
                        "b.l.gdp", "b.d.gdp",
                        "a.unit", "g.unit","g.year","g.euro",
                        "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                        "y.pred","res")
a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
simp.mod.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.cl"= rnorm(1, sd=2),
                                 "b.l.top1cl"= rnorm(1, sd=2),
                                 "b.d.top1cl"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "a.unit" = a.unit.init,
                                 "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  

simp.mod<-bugs(data = simp.mod.data, inits = simp.mod.inits,
              model.file = "AppendixNoImputeSimple.bug",
              parameters = simp.mod.params,
              n.chains = 3, n.iter = 10000,
              n.burnin = 2000, n.thin = 10,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
resids<-simp.mod$median$res
1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T)) #0.17

m.simp.outlist<-as.mcmc.bugs(simp.mod)

mcmcplot1(m.simp.outlist[,"b.l.top1cl", drop=FALSE], style="plain") 

reg.pars<-simp.mod.params[2:6] #leaving out GDP & pop b/c too big for plot 
parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
  "Majoritarian", "lag ineq. x Maj.", expression(paste(Delta, " inequality x Maj.")))

par(mar=c(2,7.5,4,.25))
caterplot(m.simp.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")


#full model
mod.form2 <- d.BDK.pvt.credit.gdp ~ lag.BDK.pvt.credit.gdp + lag.top1pct.interp*cum_left_pct +
 d.top1pct.interp*cum_left_pct + lag.pwt.log.gdp + d.pwt.log.gdp + 
 log(lag.pwt.POP) + d.pwt.log.POP + eurozone + lag.oecd.unemp + d.oecd.unemp +
 lag.gcf.gdp.oecd + d.gcf.gdp.oecd + lag.curr.acct.gdp + d.curr.acct.gdp +
 lag.weo_gbds + d.weo_gbds + lag.world.abs.cab.gdp + d.world.abs.cab.gdp +
 lag.broad.money.growth + d.broad.money.growth + lag.elderly.pop.oecd + d.elderly.pop.oecd

lm.2<-lm(mod.form2, data = master.df)

X<-data.frame(lm.2$model)
n.obs<-nrow(X)
countries<-master.df$imf.code[match(row.names(X),rownames(master.df))]
countries.polity<-as.character(master.df$polity.num[match(row.names(X),rownames(master.df))])
wbt<-master.df$wb.code[match(row.names(X),rownames(master.df))]
year<-master.df$years[match(row.names(X),rownames(master.df))]
n.years<-length(unique(year))
n.countries<-length(unique(countries))
years.d<-year - (min(year)-1) #getting years in numerical order for JAGS
country<-rep(1,n.obs) #getting countries in numerical order for JAGS
for(i in 2:n.obs){
  if(countries[i] ==countries[i-1]){
    country[i]<-country[i-1]}
 if( countries[i] != countries[i-1]){
    country[i]<-country[i-1]+1}
}

X<-data.frame(scale(X))
X$eurozone<-lm.2$model$eurozone
names(X)<-c("d.credit", "lag.credit", "lag.ineq", "cum.left", "d.ineq",
      "lag.gdp", "d.gdp", "lag.pop", "d.pop", "eurozone", "lag.unemp", 
      "d.unemp", "lag.k", "d.k", "lag.cab", "d.cab", "lag.budg", "d.budg",
      "lag.world.save", "d.world.save", "lag.bm.growth", "d.bm.growth",
      "lag.old", "d.old")

attach(X)
offsets<-rep(NA, n.obs) #
for(i in 2:n.obs){
  if(country[i]!=country[i-1]) offsets[i]<-i}
offsets[1]<-1
offsets<-na.omit(offsets)
n.offsets<-length(offsets) #should be same as n.countries

noimp.data<-list("country","n.obs", "n.countries",
                       "n.years",
                       "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
                       "lag.pop", "d.pop",
                       "lag.unemp","d.unemp",
                       "d.gdp","lag.gdp",
                       "lag.k","d.k",
                       "lag.cab", "d.cab",
                       "lag.budg","d.budg",
                       "lag.world.save", "d.world.save",
                       "lag.bm.growth", "d.bm.growth",
                       "lag.old", "d.old",
                       "years.d", "eurozone"
                 )

noimp.params<-c("b.l.credit","b.l.top1", "b.d.top1",
                        "b.cl", "b.l.top1cl", "b.d.top1cl",
                        "b.l.unemp", "b.d.unemp",
                        "b.l.pop", "b.d.pop",
                        "b.l.gdp", "b.d.gdp",
                        "b.l.k", "b.d.k",
                        "b.l.cab", "b.d.cab",
                        "b.l.budg", "b.d.budg",
                        "b.l.world.save", "b.d.world.save",
                        "b.l.bm", "b.d.bm",
                        "b.l.old", "b.d.old", 
                        "a.unit", "g.unit","g.year","g.euro",
                        "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
                        "y.pred","res")

a.unit.init<-rep(0, n.countries)
g.unit.init<-rep(0, n.countries)
g.year.init<-rep(0, n.years)
temp.inits<-rbeta(n.countries,1,1)
noimp.inits<-function(){list(
                                 "temp" = temp.inits,
                                 "b.l.top1" = rnorm(1, sd=2),
                                 "b.d.top1" = rnorm(1, sd=2),
                                 "b.cl"= rnorm(1, sd=2),
                                 "b.l.top1cl"= rnorm(1, sd=2),
                                 "b.d.top1cl"= rnorm(1, sd=2),
                                 "b.l.unemp"= rnorm(1, sd=2),
                                 "b.d.unemp"= rnorm(1, sd=2),
                                 "b.l.pop"= rnorm(1, sd=2),
                                 "b.d.pop"= rnorm(1, sd=2),
                                 "b.d.gdp"= rnorm(1, sd=2),
                                 "b.l.gdp"= rnorm(1, sd=2),
                                 "b.l.k"= rnorm(1, sd=2),
                                 "b.d.k"= rnorm(1, sd=2),
                                 "b.l.cab"= rnorm(1, sd=2),
                                 "b.d.cab"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.world.save"= rnorm(1, sd=2),
                                 "b.d.world.save"= rnorm(1, sd=2),
                                 "b.l.bm"= rnorm(1, sd=2),
                                 "b.d.bm"= rnorm(1, sd=2),
                                 "b.l.old" = rnorm(1, sd=2),
                                 "b.d.old" = rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                "a.unit" = a.unit.init,
                                "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  

noimp.mod<-bugs(data = noimp.data, inits = noimp.inits,
              model.file = "AppendixNoImpute.bug",
              parameters = noimp.params,
              n.chains = 3, n.iter = 10000,
              n.burnin = 2000, n.thin = 10,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)

resids<-noimp.mod$median$res
1-(t(resids)%*%resids/sum((d.credit-mean(d.credit, na.rm=T))^2, na.rm=T))

m.noimp.outlist<-as.mcmc.bugs(noimp.mod)

mcmcplot1(m.noimp.outlist[,"b.l.top1cl", drop=FALSE], style="plain") 

reg.pars<-c(noimp.params[2:8], noimp.params[13:24]) #leaving out GDP & pop b/c too big for plot 

parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
"Left gov", "lag ineq. x Left", expression(paste(Delta, " inequality x Left")),
            "lag unemployment", expression(paste(Delta, " unemployment")), 
            #"lag population", "pop growth", 
            #"lag GDP", "GDP growth",
             "lag GFCF", expression(paste(Delta, " GFCF")), 
             "lag current acct", expression(paste(Delta, " current account")),
            "lag budget balance", expression(paste(Delta, " budget balace")),
             "lag world savings", expression(paste(Delta, " world savings")),
             "lag M3 growth", expression(paste(Delta, " M3 growth")),
             "lag Pop>64", expression(paste(Delta, " Pop>64"))
             )

pdf("BayesECMnoimputeParamEst.pdf")
par(mar=c(2,7.5,4,.25))
caterplot(m.noimp.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")
dev.off()

#END
