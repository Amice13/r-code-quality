# Fits appendix models for household savings


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


master.df<-read.csv("AhlquistAnsellWorldPoliticsReplicationData.csv", header=T) 

#getting data

mod.form <- d.hh.net.save.oecd ~ lag.hh.net.save.oecd + lag.top1pct.interp*cum_left_pct + 
  d.top1pct.interp*cum_left_pct + pwt.pc.growth + log(lag.pwt.rgdpch) + d.ltir + lag.ltir +
  d.elderly.pop.oecd + lag.elderly.pop.oecd + eurozone + gol.maj_avg

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
names(X)<-c("d.save", "lag.save", "lag.ineq", "cum.left", "d.ineq",
			"d.gdppc", "lag.gdppc","d.ltir", "lag.ltir", "d.old","lag.old",
      "eurozone", "maj")
attach(X)
offsets<-rep(NA, n.obs) #
for(i in 2:n.obs){
  if(country[i]!=country[i-1]) offsets[i]<-i}
offsets[1]<-1
offsets<-na.omit(offsets)
n.offsets<-length(offsets) #should be same as n.countries

simp.mod.data<-list("country","n.obs", "n.countries",
                       "n.years", "d.save", "lag.save",
                       "lag.ineq","d.ineq", "maj",
                       "lag.old", "d.old",
                       "d.gdppc","lag.gdppc",
                       "d.ltir", "lag.ltir",
                       "years.d", "eurozone")
simp.mod.params<-c("b.l.save","b.l.top1", "b.d.top1",
                        "b.maj", "b.l.top1maj", "b.d.top1maj",
                        "b.l.old", "b.d.old",
                        "b.l.gdppc", "b.d.gdppc",
                        "b.d.ltir", "b.l.ltir",
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
                                 "b.maj"= rnorm(1, sd=2),
                                 "b.l.top1maj"= rnorm(1, sd=2),
                                 "b.d.top1maj"= rnorm(1, sd=2),
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "b.d.gdppc"= rnorm(1, sd=2),
                                 "b.l.gdppc"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "b.l.ltir"= rnorm(1, sd=2),
                                 "b.l.ltir"= rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "a.unit" = a.unit.init,
                                 "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  

save.mod<-bugs(data = simp.mod.data, inits = simp.mod.inits,
              model.file = "AppendixSavings.bug",
              parameters = simp.mod.params,
              n.chains = 3, n.iter = 10000,
              n.burnin = 2000, n.thin = 10,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
resids<-save.mod$median$res
1-(t(resids)%*%resids/sum((d.save-mean(d.save, na.rm=T))^2, na.rm=T))

m.simp.outlist<-as.mcmc.bugs(save.mod)

mcmcplot1(m.simp.outlist[,"b.l.top1maj", drop=FALSE], style="plain") 

reg.pars<-simp.mod.params[2:12] #leaving out GDP & pop b/c too big for plot 
parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
  "Majoritarian", "lag ineq. x Maj.", expression(paste(Delta, " ineq. x Maj.")),
  "lag pop>64", expression(paste(Delta, " pop>64")),
  "lag GDPpc", "GDPpc growth",
  expression(paste(Delta, " interest rate")), "lag interest rate"
  )

par(mar=c(2,7.5,4,.25))
caterplot(m.simp.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")

#using cl

simp.mod.data<-list("country","n.obs", "n.countries",
                       "n.years", "d.save", "lag.save",
                       "lag.ineq","d.ineq", "cum.left",
                       "lag.old", "d.old",
                       "d.gdppc","lag.gdppc",
                       "d.ltir", "lag.ltir",
                       "years.d", "eurozone")
simp.mod.params<-c("b.l.save","b.l.top1", "b.d.top1",
                        "b.cl", "b.l.top1cl", "b.d.top1cl",
                        "b.l.old", "b.d.old",
                        "b.l.gdppc", "b.d.gdppc",
                        "b.d.ltir", "b.l.ltir",
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
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "b.d.gdppc"= rnorm(1, sd=2),
                                 "b.l.gdppc"= rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "b.l.ltir"= rnorm(1, sd=2),
                                 "b.l.ltir"= rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                 "a.unit" = a.unit.init,
                                 "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  

save.mod<-bugs(data = simp.mod.data, inits = simp.mod.inits,
              model.file = "AppendixSavingsCumLeft.bug",
              parameters = simp.mod.params,
              n.chains = 3, n.iter = 10000,
              n.burnin = 2000, n.thin = 10,
              bugs.directory=bugs.dir, bugs.seed=seeds,
              working.directory= work.dir, DIC=T, debug=F)
resids<-save.mod$median$res
1-(t(resids)%*%resids/sum((d.save-mean(d.save, na.rm=T))^2, na.rm=T))

m.simp.outlist<-as.mcmc.bugs(save.mod)
reg.pars<-simp.mod.params[2:12] #leaving out GDP & pop b/c too big for plot 
parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
  "Left Goverment", "lag ineq. x Left", expression(paste(Delta, " ineq. x Left")),
  "lag pop>64", expression(paste(Delta, " pop>64")),
  "lag GDPpc", "GDPpc growth",
  expression(paste(Delta, " interest rate")), "lag interest rate"
  )

par(mar=c(2,7.5,4,.25))
caterplot(m.simp.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")


#full model
mod.form2 <- d.hh.net.save.oecd ~ lag.hh.net.save.oecd + lag.top1pct.interp*cumul_left_yrs_wght + 
  d.top1pct.interp*cumul_left_yrs_wght + pwt.pc.growth + log(lag.pwt.rgdpch) + d.ltir + lag.ltir +
  d.elderly.pop.oecd + lag.elderly.pop.oecd + eurozone + gol.maj + lag.oecd.unemp + d.oecd.unemp +lag.weo_gbds + d.weo_gbds

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
names(X)<-c("d.save", "lag.save", "lag.ineq", "cum.left", "d.ineq",
      "d.gdppc", "lag.gdppc", "d.ltir", "lag.ltir", "d.old","lag.old",
      "eurozone", "maj", "lag.unemp", "d.unemp", "lag.budg", "d.budg")

attach(X)
offsets<-rep(NA, n.obs) #
for(i in 2:n.obs){
  if(country[i]!=country[i-1]) offsets[i]<-i}
offsets[1]<-1
offsets<-na.omit(offsets)
n.offsets<-length(offsets) #should be same as n.countries

noimp.data<-list("country","n.obs", "n.countries",
                       "n.years",
                        "d.save", "lag.save", "lag.ineq",
                        "cum.left", "d.ineq",
                        "d.gdppc", "lag.gdppc", 
                        "d.ltir", "lag.ltir",
                        "d.old","lag.old",
                        "eurozone", "years.d",
                        "lag.unemp", "d.unemp",
                        "lag.budg", "d.budg"
                 )

noimp.params<-c("b.l.save","b.l.top1", "b.d.top1",
                "b.cl", "b.l.top1cl", "b.d.top1cl",
                "b.l.old", "b.d.old",
                "b.l.gdppc", "b.d.gdppc",
                "b.d.ltir", "b.l.ltir",
                "b.l.unemp", "b.d.unemp",
                "b.l.budg", "b.d.budg", 
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
                                 "b.d.gdppc"= rnorm(1, sd=2),
                                 "b.l.gdppc"= rnorm(1, sd=2),
                                 "b.l.budg"= rnorm(1, sd=2),
                                 "b.d.budg"= rnorm(1, sd=2),
                                 "b.l.old"= rnorm(1, sd=2),
                                 "b.d.old"= rnorm(1, sd=2),
                                 "b.l.ltir" = rnorm(1, sd=2),
                                 "b.d.ltir" = rnorm(1, sd=2),
                                 "g.euro" = rnorm(1, sd=2),
                                 "sigma.country"=runif(1,0,50),
                                 "sigma.vc" = runif(1,0,5),
                                 "sigma.vy" = runif(1,0,5),
                                 "mu.c"=rnorm(1, sd=2),
                                "a.unit" = a.unit.init,
                                "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
save.mod<-bugs(data = noimp.data, inits = noimp.inits,
              model.file = "AppendixSavingsFull.bug",
              parameters = noimp.params,
              n.chains = 3, n.iter = 10000,
              n.burnin = 2000, n.thin = 10, bugs.seed=seeds,
              bugs.directory=bugs.dir,
              working.directory= work.dir, DIC=T, debug=F)

resids<-save.mod$median$res
1-(t(resids)%*%resids/sum((d.save-mean(d.save, na.rm=T))^2, na.rm=T)) 
m.save.outlist<-as.mcmc.bugs(save.mod)

mcmcplot1(m.save.outlist[,"b.l.top1cl", drop=FALSE], style="plain") 

reg.pars<-c(noimp.params[2:11]) #leaving out GDP & pop b/c too big for plot 

parnames<-c("lag inequality", expression(paste(Delta, " inequality")),
"Left gov", "lag ineq. x Left", expression(paste(Delta, " inequality x Left")),
            "lag unemployment", expression(paste(Delta, " unemployment")), 
            #"lag population", "pop growth", 
            #"lag GDP", "GDP growth",
             "lag investment", expression(paste(Delta, " investment")), 
             "lag current acct", expression(paste(Delta, " current account")),
            "lag budget balance", expression(paste(Delta, " budget balace")),
             "lag world savings", expression(paste(Delta, " world savings")),
             "lag M3 growth", expression(paste(Delta, " M3 growth"))
             )

par(mar=c(2,7.5,4,.25))
caterplot(m.save.outlist, style="plain", parms=reg.pars,
          greek=F, labels = parnames, cex.labels=.90, lwd=c(1.5,3), cex=3)
abline(v=0,lty=2)
title(main = "Posterior median with 68% & 95% BCI")


#END