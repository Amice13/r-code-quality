## File gets and structures data
## generates figures,
## calls JAGS/WinBugs model objects,
## and, conducts post-fitting evaluation
## This model replicates the main model 
## as described in the main text of
## Ahlquist & Ansell "Taking Credit," World Politics
## but sequentially omits one country at a time

library(lattice)
library(foreign)
library(MASS)
library(Hmisc)
library(arm)
library(coda)
library(R2WinBUGS)
library(mcmcplots)
library(ggplot2)

#settings
seeds<-345
set.seed(seeds)#for replication purposes
bugs.dir<-"c:/Program Files (x86)/WinBUGS14/"  #user to input BUGS location
work.dir<-"U:/AhlquistAnsell"  #user to set working directory
setwd(work.dir) 

#data
master.df<-read.csv("AhlquistAnsellWorldPoliticsReplicationData.csv", header=T)

cases.in<-c("AUS", "CAN", "CHE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "IRL", "ITA", "JPN",
            "NLD", "NOR", "NZL", "PRT", "SWE", "USA") #cases
years.in<-1980:2010 #time period

ECM2.mod<-NULL

for(k in 1:length(cases.in)){
  temp.case<-cases.in[-k]
  oecd.df<-subset(master.df, wb.code%in%temp.case)
  oecd.df<-subset(oecd.df, years%in%years.in)
  oecd.use<-subset(oecd.df,select=c("polity.num", "wb.code","years","singmemd_avg",
     "effpar_leg_avg", "gov_left", "dis_gall_avg", 
     "BDK.pvt.credit.gdp","lag.BDK.pvt.credit.gdp", "d.BDK.pvt.credit.gdp",
     "lag.top1pct", "lag.top1pct.interp", "d.top1pct.interp", "gol.maj_avg",
     "lag.pwt.POP", "d.pwt.log.POP","pwt.POP",
     "lag.oecd.unemp", "d.oecd.unemp","oecd.unemp",
     "lag.pwt.log.gdp", "d.pwt.log.gdp", "pwt.log.gdp",
     "lag.pwt.pc.growth", "d.pwt.pc.growth","pwt.pc.growth",
     "lag.pwt.ki", "d.pwt.ki", "pwt.ki",
     "lag.curr.acct.gdp", "d.curr.acct.gdp", "curr.acct.gdp",
     "lag.weo_gbds", "d.weo_gbds","weo_gbds",
     "lag.world.abs.cab.gdp", "d.world.abs.cab.gdp","world.abs.cab.gdp", "eurozone",
     "lag.broad.money.growth", "d.broad.money.growth", 
     "cumul_left_yrs_wght", "lag.stir", "d.stir",
     "lag.FIRE.empcosts.pct" , "d.FIRE.empcosts.pct",
     "lag.hh.net.save.oecd", "d.hh.net.save.oecd", 
     "lag.elderly.pop.oecd", "d.elderly.pop.oecd",
     "lag.gcf.gdp.oecd","d.gcf.gdp.oecd", "cum_left", "cum_left_pct",
     "cum_govparty", "avg_govparty"
     )
  )              
  oecd.use$lag.pwt.POP<-log(oecd.use$lag.pwt.POP)
  oecd.use$pwt.POP<-log(oecd.use$pwt.POP)
  scaling.frame<-oecd.use[,5:ncol(oecd.use)]
  scaling.frame<-data.frame(scale(scaling.frame)) #centering at 0 & scaling to SD=1 by column.
  scaling.frame$gol.maj_avg<-oecd.use$gol.maj_avg
  scaling.frame$eurozone<-as.numeric(oecd.use$eurozone)
  colnames(scaling.frame)<-c("enpp", "left.gov","disprop", "credit",
     "lag.credit", "d.credit",
     "lag.ineq1", "lag.ineq", "d.ineq", "maj",
     "lag.pop", "d.pop","pop",
     "lag.unemp", "d.unemp", "unemp",
     "lag.gdp", "d.gdp", "gdp",
     "lag.growth", "d.growth", "growth",
     "lag.k", "d.k", "ki",
     "lag.cab", "d.cab","cab",
     "lag.budg", "d.budg","budg",
     "lag.world.save", "d.world.save","world.save","eurozone",
     "lag.bm.growth", "d.bm.growth", "cum.left.old", "lag.stir", "d.stir",
     "lag.FIRE", "d.FIRE", "lag.save", "d.save", "lag.old", "d.old",
     "lag.gfcf", "d.gfcf", "cum.left.raw", "cum.left", "cum.govparty.raw", "cum.govparty" 
     )
  scaling.frame$wb.code<-oecd.use$wb.code
  scaling.frame$years<-oecd.use$years
  oecd.use<-scaling.frame
  attach(oecd.use)
  
  #getting data ready
  n.obs<-dim(oecd.use)[1]
  n.years<-length(years.in)
  n.countries<-length(cases.in)
  years.d<-years - (min(years)-1) #getting years in numerical order for BUGS/JAGS
  country<-rep(1,n.obs) #getting countries in numerical order for BUGS/JAGS
  for(i in 2:n.obs){
    if(wb.code[i] ==wb.code[i-1]){country[i]<-country[i-1]}
    if(wb.code[i] != wb.code[i-1]){country[i]<-country[i-1]+1}
  }

  ### ECM
  ## w/ Beta priors on lagged dv
  ECM2.data<-list("country","n.obs", "n.countries",
    "n.years",
    "d.credit", "lag.credit", "lag.ineq","d.ineq", "cum.left",
    "lag.pop", "d.pop",
    "lag.unemp","d.unemp",
    "d.gdp","lag.gdp",
    "lag.gfcf","d.gfcf",
    "lag.cab", "d.cab",
    "lag.budg","d.budg",
    "lag.world.save", "d.world.save",
    "lag.bm.growth", "d.bm.growth",
    "years.d", "eurozone",
    "lag.old", "d.old"
    )

  ECM2.params<-c("b.l.credit","b.l.top1", "b.d.top1",
    "b.cl", "b.l.top1cl", "b.d.top1cl",
    "b.l.unemp", "b.d.unemp",
    "b.l.pop", "b.d.pop",
    "b.l.gdp", "b.d.gdp",
    "b.l.k", "b.d.k",
    "b.l.cab", "b.d.cab",
    "b.l.budg", "b.d.budg",
    "b.l.world.save", "b.d.world.save",
    "b.l.bm", "b.d.bm", "b.l.old", "b.d.old",
    "a.unit", "g.unit","g.year","g.euro",
    "logsigma2.y", "sigma.country", "sigma.vc", "sigma.vy",
    "y.pred","res"
    )
  y.init<-rep(NA,n.obs)
  y.init[is.na(d.credit)]<-0
  a.unit.init<-rep(0, n.countries)
  g.unit.init<-rep(0, n.countries)
  g.year.init<-rep(0, n.years)
  temp.inits<-rbeta(n.countries,1,1)
  ECM2.inits<-function(){list(
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
    "b.l.old"= rnorm(1, sd=2),
    "b.d.old"= rnorm(1, sd=2),
    "g.euro" = rnorm(1, sd=2),
    "sigma.country"=runif(1,0,50),
    "sigma.l.credit"=runif(1,0,50),
    "sigma.l.top1"=runif(1,0,50),
    "sigma.d.top1"=runif(1,0,50),
    "sigma.l.budg" = runif(1,0,50),
    "sigma.d.budg" = runif(1,0,50),
    "sigma.l.unemp" = runif(1,0,50),
    "sigma.d.unemp" = runif(1,0,50),
    "sigma.l.bm.growth"= runif(1,0,50),
    "sigma.d.bm.growth"= runif(1,0,50),
    "sigma.vc" = runif(1,0,5),
    "sigma.vy" = runif(1,0,5),
    "mu.c"=rnorm(1, sd=2),
    "mu.l.credit" =rnorm(1, sd=2),
    "mu.l.top1"=rnorm(1, sd=2),
    "mu.d.top1"=rnorm(1, sd=2),
    "mu.l.budg" = rnorm(1, sd=2),
    "mu.d.budg" = rnorm(1, sd=2),
    "mu.l.unemp" = rnorm(1, sd=2),
    "mu.d.unemp" = rnorm(1, sd=2),
    "mu.l.bm.growth"= rnorm(1, sd=2),
    "mu.d.bm.growth"= rnorm(1, sd=2),
    "d.credit" = y.init, "a.unit" = a.unit.init,
    "g.unit"= g.unit.init, "g.year" = g.year.init
                             )
                      }  
  ECM2.mod[[k]]<-bugs(data = ECM2.data, inits = ECM2.inits,
    model.file = "AhlquistAnsellMainModel.bug",
    parameters = ECM2.params,
    n.chains = 3, n.iter = 30000,
    n.burnin = 10000, n.thin = 30,
    bugs.directory= bugs.dir,  
    working.directory= work.dir, DIC=T, debug=F)
  detach(oecd.use)
  rm(oecd.use)
  length(cases.in)-k
}
  
m2.outlist<-NULL
for(j in 1:length(ECM2.mod)){
  m2.outlist[[j]]<-as.mcmc.bugs(ECM2.mod[[j]])
}

#convergence diagnotic plot
pdf("LeaveOneOutResults.pdf")
tempmat<-as.data.frame(rbind(m2.outlist[[1]][1][[1]],m2.outlist[[1]][2][[1]],m2.outlist[[1]][3][[1]]))
pblt1cl<-tempmat$b.l.top1cl
plot(density(tempmat$b.l.top1cl), bty="n", 
  xlim=c(-0.35, 0.05), ylim=c(0,12), xlab="lag top 1% x cum left", 
  main = "posterior density and 95% BCI")
abline(v=quantile(tempmat$b.l.top1cl,0.5), lwd=0.5)
abline(v=quantile(tempmat$b.l.top1cl,c(0.025,0.975)),lty=2,lwd=0.5)

for(i in 2:length(ECM2.mod)){
  tempmat<-as.data.frame(rbind(m2.outlist[[i]][1][[1]],m2.outlist[[i]][2][[1]],m2.outlist[[i]][3][[1]]))
  pblt1cl<-c(pblt1cl,tempmat$b.l.top1cl)
  lines(density(tempmat$b.l.top1cl), add=T, col=rainbow_hcl(18)[i])
  abline(v=quantile(tempmat$b.l.top1cl,0.5),col=rainbow_hcl(18)[i], lwd=0.5)
  abline(v=quantile(tempmat$b.l.top1cl,c(0.025,0.975)),col=rainbow_hcl(18)[i], lty=2, lwd=0.5)
}

lines(density(pblt1cl), lwd=4)
abline(v=quantile(pblt1cl, 0.5), lwd=3)
abline(v=quantile(pblt1cl, c(0.025, 0.975)), lty=2, lwd=3)
dev.off()

for(i in 1:length(ECM2.mod)){
denplot(m2.outlist[[i]], 
  parms = "b.l.top1cl", collapse = T,
  ci=0.95, main = paste("No", cases.in[1]),
  xlab = "lag top 1% x  cum left")  
}
#END