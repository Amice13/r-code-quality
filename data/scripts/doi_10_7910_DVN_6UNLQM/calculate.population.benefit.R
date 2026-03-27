rm(list=ls())

################
#load libraries#
################
library(sas7bdat)
library(survey)
library(foreign)
library(truncnorm)

################
#functions
################
rr.to.or <- function(RR,p)
RR*(1-p)/(1-RR*p)

pred.prob.fxn <- function(rr,p1) 
  rr*p1

pred.prob.fxn2 <- function(or, p2) 
  (or*p2/(1-p2))/(1+or*p2/(1-p2))

pooled.se <- function(p1,p2,n1,n2) {
  x1 <- p1*n1
  x2 <- p2*n2
  p <- (x1+x2)/(n1+n2)
  se <- sqrt(p*(1-p)*(1/n1+1/n2))
  return(se)
}

trunc.fxn <- function(n,m,s) 
  rtruncnorm(n,a=-Inf,b=Inf,mean=m,sd=s)

summary.fxn <- function(x)
  paste(formatC(round(mean(x)),big.mark=",",format="d"),
        " (95% CI: ",
        formatC(round(quantile(x,probs=0.025)),big.mark=",",format="d"),
        " to ",
        formatC(round(quantile(x,probs=0.975)),big.mark=",",format="d"),
        ")",sep="")

format.fxn <- function(x) {
  pt.est <- formatC(100*as.numeric(x[[2]]),digits=1,format="f") 
  lower <- formatC(100*(as.numeric(x[[2]])-qnorm(0.975)*as.numeric(x[[3]])),digits=1,format="f")
  upper <- formatC(100*(as.numeric(x[[2]])+qnorm(0.975)*as.numeric(x[[3]])),digits=1,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn2 <- function(p, s) {
  pt.est <- formatC(100*p,digits=1,format="f") 
  lower <- formatC(100*(p-qnorm(0.975)*s),digits=1,format="f")
  upper <- formatC(100*(p+qnorm(0.975)*s),digits=1,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn3 <- function(p, s) {
  pt.est <- formatC(p,digits=1,format="f") 
  lower <- formatC(p-qnorm(0.975)*s,digits=1,format="f")
  upper <- formatC(p+qnorm(0.975)*s,digits=1,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn4 <- function(x) {
  pt.est <- formatC(100*as.numeric(x[[2]]),digits=1,format="f") 
  lower <- formatC(max(0,100*(as.numeric(x[[2]])-qnorm(0.975)*as.numeric(x[[3]]))),digits=1,format="f")
  upper <- formatC(min(100,100*(as.numeric(x[[2]])+qnorm(0.975)*as.numeric(x[[3]]))),digits=1,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn5 <- function(x) {
  pt.est <- formatC(100*mean(x),digits=2,format="f") 
  lower <- formatC(100*quantile(x,probs=0.025),digits=2,format="f")
  upper <- formatC(100*quantile(x,probs=0.975),digits=2,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn6 <- function(x) {
  pt.est <- formatC(100*as.numeric(x[[1]]),digits=1,format="f") 
  lower <- formatC(max(0,100*(as.numeric(x[[1]])-qnorm(0.975)*as.numeric(x[[2]]))),digits=1,format="f")
  upper <- formatC(min(100,100*(as.numeric(x[[1]])+qnorm(0.975)*as.numeric(x[[2]]))),digits=1,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

format.fxn7 <- function(x) {
  pt.est <- formatC(as.numeric(x[[1]]),digits=2,format="f") 
  lower <- formatC(as.numeric(x[[1]])-qnorm(0.975)*as.numeric(x[[2]]),digits=2,format="f")
  upper <- formatC(as.numeric(x[[1]])+qnorm(0.975)*as.numeric(x[[2]]),digits=2,format="f")
  return(paste(pt.est," (",lower,", ",upper,")",sep=""))
}

adol.fxn <- function(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se) {
  never.tried.cig.draw <- apply(never.tried.cig.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adol.ages] 
  tried.ecig.never.tried.cig.draw <- apply(ever.tried.ecig.never.tried.cig.pt.est.se, 1, function(x) trunc.fxn(1,x[[2]],x[[3]]))
  baseline.init.adol.draw <- rnorm(1,0.066,(0.071-0.061)/(2*qnorm(0.975)))
  or.draw <- exp(rnorm(1, ecig.cig.or.pt.est.se[1], ecig.cig.or.pt.est.se[2])) #from meta-analysis in ma.R (~/Dropbox/smoking/ecig_meta_analysis/)
  trans.ecig.cig <- pred.prob.fxn2(or.draw, baseline.init.adol.draw) - baseline.init.adol.draw
  init.daily.draw <- apply(current.cig.cig.init.pt.est.se, 1, function(x) rnorm(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adol.ages] 
  cig.init <- popl[adol.ages] *  never.tried.cig.draw * tried.ecig.never.tried.cig.draw * trans.ecig.cig
  longterm.smokers <- cig.init * init.daily.draw
  life.years <- trunc.fxn(round(sum(abs(longterm.smokers))),life.years.pt.est.se["35",1],life.years.pt.est.se["35",2])
  life.years <- ifelse(sum(longterm.smokers)<0, sum(-1 * life.years), sum(life.years))
  tab.pt.est <- data.frame(cbind(popl=popl[adol.ages], never.tried.cig=never.tried.cig.pt.est.se[adol.ages,2], tried.ecig.never.tried.cig=ever.tried.ecig.never.tried.cig.pt.est.se[adol.ages,2], trans=pred.prob.fxn2(exp(ecig.cig.or.pt.est.se[1]),0.066)-0.066, current.cig.cig.init=current.cig.cig.init.pt.est.se[adol.ages,2], life.years=life.years.pt.est.se["35",1]))
  life.years.pt.est.value <- sum(apply(tab.pt.est,1,prod))
  return(list(tab.pt.est=tab.pt.est,life.years.pt.est.value=life.years.pt.est.value,cig.init=cig.init,longterm.smokers=longterm.smokers,life.years=life.years))
}

ya.fxn <- function(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se) {
  never.cig.draw <- apply(never.tried.cig.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[ya.ages]
  never.cig.draw <- c(never.cig.draw[as.character(18:26)],rep(never.cig.draw["26"],3))
  never.cig.ecig.draw <- apply(never.cig.ecig.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[3]]),as.numeric(x[[4]])))
  baseline.init.ya.draw <- rnorm(1,0.087,(0.095-0.079)/(2*qnorm(0.975)))
  or.draw <- exp(rnorm(1, ecig.cig.or.pt.est.se[1], ecig.cig.or.pt.est.se[2])) #from meta-analysis in ma.R (~/Dropbox/smoking/ecig_meta_analysis/)
  trans.ecig.cig <- pred.prob.fxn2(or.draw, baseline.init.ya.draw) - baseline.init.ya.draw
  init.daily.draw <- apply(current.cig.cig.init.pt.est.se, 1, function(x) rnorm(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[ya.ages]
  init.daily.draw <- apply(as.matrix(init.daily.draw),1,function(x) max(x,0))
  cig.init <- popl[ya.ages] * never.cig.draw * never.cig.ecig.draw * trans.ecig.cig
  longterm.smokers <- cig.init * init.daily.draw
  life.years <- trunc.fxn(round(sum(abs(longterm.smokers))),life.years.pt.est.se["35",1],life.years.pt.est.se["35",2])
  life.years <- ifelse(sum(longterm.smokers)<0, sum(-1 * life.years), sum(life.years))
  tab.pt.est <- data.frame(cbind(popl=popl[ya.ages], never.tried.cig=c(never.tried.cig.pt.est.se[as.character(18:25),2],rep(never.tried.cig.pt.est.se[as.character(26),2],4)), tried.ecig.never.tried.cig=never.cig.ecig.pt.est.se[ya.ages,3], trans=pred.prob.fxn2(exp(ecig.cig.or.pt.est.se[1]),0.087)-0.087, current.cig.cig.init=current.cig.cig.init.pt.est.se[ya.ages,2], life.years=life.years.pt.est.se["35",1]))
  life.years.pt.est.value <- sum(apply(tab.pt.est,1,prod))
  return(list(tab.pt.est=tab.pt.est,life.years.pt.est.value=life.years.pt.est.value,cig.init=cig.init,longterm.smokers=longterm.smokers,life.years=life.years))
}

adult.quit.fxn <- function(popl, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages, life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, harm.reduction) {
  current.cig.draw <- apply(current.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adult.ages]
  tried.quit.draw <- apply(tried.quit.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adult.ages]
  current.cig.ecig.draw <- apply(current.cig.ecig.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adult.ages]
  past.year.cessation.tool.draw <- apply(past.year.cessation.tool.age.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adult.ages]
  aid.draw <- apply(pharmaceutical.aids.quit.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[1]]),as.numeric(x[[2]])))[adult.ages]
  no.aid.draw <- apply(pharmaceutical.aids.quit.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[3]]),as.numeric(x[[4]])))[adult.ages]
  cig.quit.or.draw <- rnorm(1,cig.quit.or.pt.est.se[1],cig.quit.or.pt.est.se[2])
  ecig.quit.draw <- pred.prob.fxn2(cig.quit.or.draw, aid.draw)  
  trans.cig.quit <-  past.year.cessation.tool.draw*(ecig.quit.draw-aid.draw) + (1-past.year.cessation.tool.draw)*(ecig.quit.draw-no.aid.draw)
  shortterm.quit.draw <- rnorm(1,0.74,(0.78-0.70)/(2*qnorm(0.975)))
  longterm.quit.draw <- rnorm(1,longterm.quit.pt.est.se[1],longterm.quit.pt.est.se[2])
  quit.cig <- popl[adult.ages] * current.cig.draw * tried.quit.draw * current.cig.ecig.draw * trans.cig.quit
  longterm.quit.cig <- quit.cig * shortterm.quit.draw * longterm.quit.draw
  life.years <- rep(NA,length(adult.ages))
  for (i in 1:length(adult.ages)) {
    life.years[i] <- sum(rnorm(round(abs(longterm.quit.cig[i])),life.years.pt.est[adult.ages[i]],life.years.pt.est[adult.ages[i]]/10))
    life.years[i] <- ifelse(longterm.quit.cig[i]>0,life.years[i],-1*life.years[i])
    life.years[i] <- ifelse(longterm.quit.cig[i]>0, (1-harm.reduction)*life.years[i], (1+harm.reduction)*life.years[i])  
  }
  life.years <- sum(life.years)
  trans.cig.quit.pt.est <- past.year.cessation.tool.age.pt.est.se[,2]*(pred.prob.fxn2(cig.quit.or.pt.est.se[1],pharmaceutical.aids.quit.pt.est.se[,1])-pharmaceutical.aids.quit.pt.est.se[,1]) + (1-past.year.cessation.tool.age.pt.est.se[,2])*(pred.prob.fxn2(cig.quit.or.pt.est.se[1],pharmaceutical.aids.quit.pt.est.se[,1])-pharmaceutical.aids.quit.pt.est.se[,3])
  tab.pt.est <- data.frame(cbind(popl=popl[adult.ages], current.cig=current.pt.est.se[adult.ages,2], tried.quit=tried.quit.pt.est.se[adult.ages,2], current.ecig=current.cig.ecig.pt.est.se[adult.ages,2], trans=trans.cig.quit.pt.est, shortterm=0.74, longterm=0.629, life.years=life.years.pt.est.se[adult.ages,1]))
  life.years.pt.est.value <- sum(apply(tab.pt.est,1,prod))
  return(list(tab.pt.est=tab.pt.est,life.years.pt.est.value=life.years.pt.est.value,quit.cig=quit.cig,longterm.quit.cig=longterm.quit.cig,life.years=life.years))
}

#age ranges
adol.ages <- as.character(12:17)
ya.ages <- as.character(18:29)
adult.ages <- as.character(seq(25,65,5))
adult.ages2 <- as.character(seq(20,60,10))
adult.ages.lab <- c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69")

#nsduh (from read.nsduh.R) and nats (from read.nats.R)
load("data/never.tried.cig.pt.est.se.Rdata")
load("data/current.cig.cig.init.pt.est.se.Rdata")

#population
population <- read.table("Population.txt",skip=2,header=TRUE)
population$Age5 <- 5*floor(population$Age/5)
popl <- subset(population,Year==2014 & Age>= 12 & Age<=65)$Total
names(popl) <- 12:65
popl5 <- by(population$Total,list(population$Year,population$Age5),sum)["2014",]
names(popl5) <- seq(0,110,5)

#national youth tobacco survey (2014)
nyts <- read.sas7bdat("nyts2014_dataset.sas7bdat", debug=FALSE)
nyts$age <- nyts$qn1+8
nyts$sex <- as.factor(nyts$qn2)
levels(nyts$sex) <- c("male","female",NA)
nyts$ever.tried.cig <- as.factor(nyts$qn7)
levels(nyts$ever.tried.cig) <- c("yes","no",NA)
nyts$ever.tried.ecig <- ifelse(nyts$qn31==1,1,0)
nyts.survey <- svydesign(ids=~1, weights=~wt, data=nyts)
ever.tried.ecig.never.tried.cig.pt.est.se <- svyby(~ever.tried.ecig, ~age, subset(nyts.survey, age %in% as.numeric(adol.ages) & ever.tried.cig=="no"), svyciprop, na.rm=TRUE)
ever.tried.ecig.never.tried.cig.pt.est.se.sens7 <- ever.tried.ecig.never.tried.cig.pt.est.se
ever.tried.ecig.never.tried.cig.pt.est.se.sens7[,2] <- (1-0.1)*ever.tried.ecig.never.tried.cig.pt.est.se[,2]
ever.tried.ecig.never.tried.cig.pt.est.se.sens8 <- ever.tried.ecig.never.tried.cig.pt.est.se
ever.tried.ecig.never.tried.cig.pt.est.se.sens8[,2] <- (1-0.2)*ever.tried.ecig.never.tried.cig.pt.est.se[,2]

#ecig -> cig pooled odds ratio
load("data/res.Rdata")
ecig.cig.or.pt.est.se <- c(res$b, res$se)
ecig.cig.or.pt.est.se.sens3 <- c(res$b, res$se)
ecig.cig.or.pt.est.se.sens3[1] <- log(1-0.1)+ecig.cig.or.pt.est.se[1] #ecig.cig.or.pt.est.se is on the log scale
ecig.cig.or.pt.est.se.sens4 <- c(res$b, res$se)
ecig.cig.or.pt.est.se.sens4[1] <- log(1-0.2)+ecig.cig.or.pt.est.se[1]

#life-years pt est
life.years.pt.est <- rep(NA,length(unique(c(adol.ages, ya.ages, adult.ages))))
names(life.years.pt.est) <- unique(c(adol.ages, ya.ages, adult.ages))
life.years.pt.est[as.character(adol.ages)] <- 17
life.years.pt.est[as.character(ya.ages)] <- 17
life.years.pt.est["25"] <- 10
life.years.pt.est["30"] <- 10
life.years.pt.est["35"] <- 9
life.years.pt.est["40"] <- 9
life.years.pt.est["45"] <- 6
life.years.pt.est["50"] <- 6
life.years.pt.est["55"] <- 4
life.years.pt.est["60"] <- 4
life.years.pt.est["65"] <- 2
life.years.pt.est.se <- data.frame(cbind(pt.est=life.years.pt.est,se=life.years.pt.est/10))

########
#Adults#
########
nhis <- read.xport("data/samadult.xpt")
nhis$age.cat <- as.factor(ifelse(nhis$AGE_P<=29, nhis$AGE_P, 5*floor(nhis$AGE_P/5)))
nhis$age.cat2 <- as.factor(ifelse(nhis$AGE_P<=29, nhis$AGE_P, 10*floor(nhis$AGE_P/10)))
nhis$cig.status <- as.factor(nhis$SMKSTAT2)
levels(nhis$cig.status) <- c("current every day","current some day","former","never","former",NA)
nhis$never <- ifelse(nhis$cig.status=="never",1,0)
nhis$current <- ifelse(nhis$cig.status %in% c("current every day","current some day"),1,0)
nhis$ecig.recent <- as.factor(nhis$ECIGED)
levels(nhis$ecig.recent) <- c(1,1,0,0)
nhis$ecig.recent.value <- ifelse(nhis$ecig.recent=="1",1,0)
nhis$ecig.recent.value[nhis$ECIGEV==2] <- 0
nhis$tried.quit <- ifelse(nhis$CIGQTYR=="1",1,0)
nhis$ecig.ever <- ifelse(nhis$ECIGEV==1,1,0)

nhis.survey <- svydesign(ids=~1, weights=~WTFA_SA, data=nhis)
current.pt.est.se <- svyby(~current, ~age.cat, nhis.survey, svyciprop, na.rm=TRUE)
tried.quit.pt.est.se <- svyby(~tried.quit, ~age.cat, nhis.survey, svyciprop, na.rm=TRUE)
never.cig.ecig.pt.est.se <- svyby(~ecig.ever, ~age.cat+never, subset(nhis.survey, age.cat %in% ya.ages), svyciprop, na.rm=TRUE)[c("18.1","19.1","20.1","21.1","22.1","23.1","24.1","25.1","26.1","27.1","28.1","29.1"),]
never.cig.ecig.pt.est.se.sens7 <- never.cig.ecig.pt.est.se
never.cig.ecig.pt.est.se.sens7[,3] <- (1-0.1) * never.cig.ecig.pt.est.se[,3]
never.cig.ecig.pt.est.se.sens8 <- never.cig.ecig.pt.est.se
never.cig.ecig.pt.est.se.sens8[,3] <- (1-0.2) * never.cig.ecig.pt.est.se[,3]
current.cig.ecig.pt.est.se <- svyby(~ecig.recent.value, ~age.cat2, subset(nhis.survey, age.cat %in% adult.ages & current==1 & tried.quit==1), svyciprop, method="beta", na.rm=TRUE)
current.cig.ecig.pt.est.se <- rbind(current.cig.ecig.pt.est.se,current.cig.ecig.pt.est.se)
current.cig.ecig.pt.est.se$age.cat2 <- c(seq(20,60,10),seq(25,65,10))
rownames(current.cig.ecig.pt.est.se) <- as.character(c(seq(20,60,10),seq(25,65,10)))
names(current.cig.ecig.pt.est.se) <- c("age.cat","ecig.recent.value","se.as.numeric(ecig.recent.value)")
current.cig.ecig.pt.est.se.sens5 <- current.cig.ecig.pt.est.se
current.cig.ecig.pt.est.se.sens5[,2] <- (1+0.1)*current.cig.ecig.pt.est.se[,2]
current.cig.ecig.pt.est.se.sens6 <- current.cig.ecig.pt.est.se
current.cig.ecig.pt.est.se.sens6[,2] <- (1+0.2)*current.cig.ecig.pt.est.se[,2]

load("~/Dropbox/smoking/wfu/data/past.year.cessation.tool.pt.est.se.Rdata") #past.year.cessation.tool.age.pt.est.se
pharmaceutical.aids.quit.pt.est.se <- matrix(NA,nrow=length(adult.ages),ncol=4) #table 3, messer et al, 2008 (ajph)
rownames(pharmaceutical.aids.quit.pt.est.se) <- adult.ages
colnames(pharmaceutical.aids.quit.pt.est.se) <- c("pt.est.aid","se(pt.est.aid)","pt.est.no.aid","se(pt.est.no.aid)")
pharmaceutical.aids.quit.pt.est.se[,"pt.est.aid"] <- c(rep(0.081,2),rep(0.093,3),rep(0.083,4))
pharmaceutical.aids.quit.pt.est.se[,"pt.est.no.aid"] <- c(rep(0.079,2),rep(0.052,3),rep(0.064,4))
pharmaceutical.aids.quit.pt.est.se[,"se(pt.est.aid)"] <- c(rep((0.108-0.055)/(2*qnorm(0.975)),2),rep((0.106-0.079)/(2*qnorm(0.975)),3),rep((0.100-0.066)/(2*qnorm(0.975)),4))
pharmaceutical.aids.quit.pt.est.se[,"se(pt.est.no.aid)"] <- c(rep((0.089-0.069)/(2*qnorm(0.975)),2),rep((0.058-0.045)/(2*qnorm(0.975)),3),rep((0.073-0.055)/(2*qnorm(0.975)),4))
cig.quit.or.pt.est.se <- c(0.86,(1.23-0.60)/(2*qnorm(0.975)))
cig.quit.or.pt.est.se.sens1 <- c(1.26,(2.34-0.68)/(2*qnorm(0.975)))
longterm.quit.pt.est.se <- c(0.658,0.014)

########################
#life years calculation#
########################
N <- 10^5
results.mat <- matrix(NA,nrow=N,ncol=9)
adult.quit.list <- adol.init.list <- list()
for (i in 1:N) {
  tmp.adol <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.ya <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.adult.quit <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
  results.mat[i,1] <- sum(tmp.adol$cig.init)
  results.mat[i,2] <- sum(tmp.ya$cig.init)
  results.mat[i,3] <- sum(tmp.adult.quit$quit.cig)
  results.mat[i,4] <- -1 * tmp.adol$life.years #life-years lost
  results.mat[i,5] <- -1 * tmp.ya$life.years #life-years lost
  results.mat[i,6] <- tmp.adult.quit$life.years
  results.mat[i,7] <- sum(tmp.adol$longterm.smokers)
  results.mat[i,8] <- sum(tmp.ya$longterm.smokers)
  results.mat[i,9] <- sum(tmp.adult.quit$longterm.quit)
  adult.quit.list[[i]] <- tmp.adult.quit
  adol.init.list[[i]] <- tmp.adol
  if (i %% 1000==1) print(paste("base case",round(100*i/N),"% done",date()))
}
results.mat <- data.frame(results.mat)
names(results.mat) <- c("adol.cig.init","ya.cig.init","adult.cig.quit","adol.life.years","ya.life.years","adult.cig.quit.life.years","adol.longterm.smokers","ya.longterm.smokers","longterm.quit")
save(results.mat, file="data/results.mat.Rdata")

######################
#sensitivity analysis#
######################
sens.mat1 <- sens.mat2 <- sens.mat3 <- sens.mat4 <- sens.mat5 <- sens.mat6 <- sens.mat7 <- sens.mat8 <- sens.mat9 <- matrix(NA,nrow=N,ncol=9)
harm.reduction <- rep(NA, N)
for (i in 1:N) {
  tmp.adol <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.ya <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.adult.quit <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
  tmp.adult.quit1 <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se.sens1, longterm.quit.pt.est.se, 0)
  tmp.adol3 <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se.sens3, current.cig.cig.init.pt.est.se)
  tmp.ya3 <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se.sens3, current.cig.cig.init.pt.est.se)
  tmp.adol4 <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se.sens4, current.cig.cig.init.pt.est.se)
  tmp.ya4 <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se.sens4, current.cig.cig.init.pt.est.se)
  tmp.adult.quit5 <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se.sens5, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
  tmp.adult.quit6 <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se.sens6, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
  tmp.adol7 <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se.sens7, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.ya7 <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se.sens7, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.adol8 <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se.sens8, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  tmp.ya8 <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se.sens8, ya.ages,life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
  harm.reduction[i] <- runif(1, 0, 1) 
  tmp.adult.quit9 <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages,life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, harm.reduction[i])
  
  sens.mat1[i,1] <- sum(tmp.adol$cig.init)
  sens.mat1[i,2] <- sum(tmp.ya$cig.init)
  sens.mat1[i,3] <- sum(tmp.adult.quit1$quit.cig)
  sens.mat1[i,4] <- -1 * tmp.adol$life.years #life-years lost
  sens.mat1[i,5] <- -1 * tmp.ya$life.years #life-years lost
  sens.mat1[i,6] <- tmp.adult.quit1$life.years
  sens.mat1[i,7] <- sum(tmp.adol$longterm.smokers)
  sens.mat1[i,8] <- sum(tmp.ya$longterm.smokers)
  sens.mat1[i,9] <- sum(tmp.adult.quit1$longterm.quit)
  
  sens.mat3[i,1] <- sum(tmp.adol3$cig.init)
  sens.mat3[i,2] <- sum(tmp.ya3$cig.init)
  sens.mat3[i,3] <- sum(tmp.adult.quit$quit.cig)
  sens.mat3[i,4] <- -1 * tmp.adol3$life.years #life-years lost
  sens.mat3[i,5] <- -1 * tmp.ya3$life.years #life-years lost
  sens.mat3[i,6] <- tmp.adult.quit$life.years
  sens.mat3[i,7] <- sum(tmp.adol3$longterm.smokers)
  sens.mat3[i,8] <- sum(tmp.ya3$longterm.smokers)
  sens.mat3[i,9] <- sum(tmp.adult.quit$longterm.quit)
  
  sens.mat4[i,1] <- sum(tmp.adol4$cig.init)
  sens.mat4[i,2] <- sum(tmp.ya4$cig.init)
  sens.mat4[i,3] <- sum(tmp.adult.quit$quit.cig)
  sens.mat4[i,4] <- -1 * tmp.adol4$life.years #life-years lost
  sens.mat4[i,5] <- -1 * tmp.ya4$life.years #life-years lost
  sens.mat4[i,6] <- tmp.adult.quit$life.years
  sens.mat4[i,7] <- sum(tmp.adol4$longterm.smokers)
  sens.mat4[i,8] <- sum(tmp.ya4$longterm.smokers)
  sens.mat4[i,9] <- sum(tmp.adult.quit$longterm.quit)
  
  sens.mat5[i,1] <- sum(tmp.adol$cig.init)
  sens.mat5[i,2] <- sum(tmp.ya$cig.init)
  sens.mat5[i,3] <- sum(tmp.adult.quit5$quit.cig)
  sens.mat5[i,4] <- -1 * tmp.adol$life.years #life-years lost
  sens.mat5[i,5] <- -1 * tmp.ya$life.years #life-years lost
  sens.mat5[i,6] <- tmp.adult.quit5$life.years
  sens.mat5[i,7] <- sum(tmp.adol$longterm.smokers)
  sens.mat5[i,8] <- sum(tmp.ya$longterm.smokers)
  sens.mat5[i,9] <- sum(tmp.adult.quit5$longterm.quit)
  
  sens.mat6[i,1] <- sum(tmp.adol$cig.init)
  sens.mat6[i,2] <- sum(tmp.ya$cig.init)
  sens.mat6[i,3] <- sum(tmp.adult.quit6$quit.cig)
  sens.mat6[i,4] <- -1 * tmp.adol$life.years #life-years lost
  sens.mat6[i,5] <- -1 * tmp.ya$life.years #life-years lost
  sens.mat6[i,6] <- tmp.adult.quit6$life.years
  sens.mat6[i,7] <- sum(tmp.adol$longterm.smokers)
  sens.mat6[i,8] <- sum(tmp.ya$longterm.smokers)
  sens.mat6[i,9] <- sum(tmp.adult.quit6$longterm.quit)
  
  sens.mat7[i,1] <- sum(tmp.adol7$cig.init)
  sens.mat7[i,2] <- sum(tmp.ya7$cig.init)
  sens.mat7[i,3] <- sum(tmp.adult.quit$quit.cig)
  sens.mat7[i,4] <- -1 * tmp.adol7$life.years #life-years lost
  sens.mat7[i,5] <- -1 * tmp.ya7$life.years #life-years lost
  sens.mat7[i,6] <- tmp.adult.quit$life.years
  sens.mat7[i,7] <- sum(tmp.adol7$longterm.smokers)
  sens.mat7[i,8] <- sum(tmp.ya7$longterm.smokers)
  sens.mat7[i,9] <- sum(tmp.adult.quit$longterm.quit)
  
  sens.mat8[i,1] <- sum(tmp.adol8$cig.init)
  sens.mat8[i,2] <- sum(tmp.ya8$cig.init)
  sens.mat8[i,3] <- sum(tmp.adult.quit$quit.cig)
  sens.mat8[i,4] <- -1 * tmp.adol8$life.years #life-years lost
  sens.mat8[i,5] <- -1 * tmp.ya8$life.years #life-years lost
  sens.mat8[i,6] <- tmp.adult.quit$life.years
  sens.mat8[i,7] <- sum(tmp.adol8$longterm.smokers)
  sens.mat8[i,8] <- sum(tmp.ya8$longterm.smokers)
  sens.mat8[i,9] <- sum(tmp.adult.quit$longterm.quit)
  
  sens.mat9[i,1] <- sum(tmp.adol$cig.init)
  sens.mat9[i,2] <- sum(tmp.ya$cig.init)
  sens.mat9[i,3] <- sum(tmp.adult.quit9$quit.cig)
  sens.mat9[i,4] <- -1 * tmp.adol$life.years #life-years lost
  sens.mat9[i,5] <- -1 * tmp.ya$life.years #life-years lost
  sens.mat9[i,6] <- tmp.adult.quit9$life.years
  sens.mat9[i,7] <- sum(tmp.adol$longterm.smokers)
  sens.mat9[i,8] <- sum(tmp.ya$longterm.smokers)
  sens.mat9[i,9] <- sum(tmp.adult.quit9$longterm.quit)
  
  if (i %% 500==1) print(paste("sensitivity",round(100*i/N),"% done",date()))
}
sens.mat1 <- data.frame(sens.mat1)
colnames(sens.mat1) <- colnames(sens.mat3) <- colnames(sens.mat4) <- colnames(sens.mat5) <- colnames(sens.mat6) <- colnames(sens.mat7) <- colnames(sens.mat8) <- colnames(sens.mat9) <- 
  c("adol.cig.init","ya.cig.init","adult.cig.quit","adol.life.years","ya.life.years","adult.cig.quit.life.years","adol.longterm.smokers","ya.longterm.smokers","longterm.quit")
save(sens.mat1, sens.mat3, sens.mat4, sens.mat5, sens.mat6, sens.mat7, sens.mat8, sens.mat9, 
     cig.quit.or.pt.est.se, cig.quit.or.pt.est.se.sens1, 
     ecig.cig.or.pt.est.se, ecig.cig.or.pt.est.se.sens3, ecig.cig.or.pt.est.se.sens4, 
     current.cig.ecig.pt.est.se, current.cig.ecig.pt.est.se.sens5, current.cig.ecig.pt.est.se.sens6, 
     ever.tried.ecig.never.tried.cig.pt.est.se.sens7, ever.tried.ecig.never.tried.cig.pt.est.se.sens8, never.cig.ecig.pt.est.se.sens7, never.cig.ecig.pt.est.se.sens8, 
     harm.reduction, 
     file="data/sens.mat.Rdata")

########################################
#public health goals/threshold analysis#
########################################
N2 <- 1000
cig.quit.or.list <- seq(1.0,4.0,length=61)
sens.cig.quit.mat <- matrix(NA,nrow=N2,ncol=length(cig.quit.or.list))
for (i in 1:length(cig.quit.or.list)) {
  for (j in 1:N2) {
    tmp.cig.quit.or.pt.est.se <- c(cig.quit.or.list[i],0)
    tmp.adol <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.ya <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.adult.quit.sens <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages, life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, tmp.cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
    sens.cig.quit.mat[j,i] <- -1*tmp.adol$life.years + -1*tmp.ya$life.years + tmp.adult.quit.sens$life.years
  }
  print(paste("sensitivity cig quit rr", round(100*i/length(cig.quit.or.list)),"% done",date()))
}
colnames(sens.cig.quit.mat) <- cig.quit.or.list

cig.init.or.list <- log(seq(0.5,3.5,length=61))
sens.cig.init.mat <- matrix(NA,nrow=N2,ncol=length(cig.init.or.list))
for (i in 1:length(cig.init.or.list)) {
  for (j in 1:N2) {
    tmp.cig.init.or.pt.est.se <- c(cig.init.or.list[i],0)
    tmp.adol.sens <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, tmp.cig.init.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.ya.sens <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages, life.years.pt.est.se, tmp.cig.init.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.adult.quit <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages, life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
    sens.cig.init.mat[j,i] <- -1*tmp.adol.sens$life.years + -1*tmp.ya.sens$life.years + tmp.adult.quit$life.years
  }
  print(paste("sensitivity cig init or", round(100*i/length(cig.init.or.list)),"% done",date()))
}
colnames(sens.cig.init.mat) <- exp(cig.init.or.list)

N3 <- 500
current.cig.ecig.increase.list <- seq(1.0,5.5,length=46)
sens.current.cig.ecig.increase.mat <- matrix(NA,nrow=N3,ncol=length(current.cig.ecig.increase.list))
for (i in 1:length(current.cig.ecig.increase.list)) {
  for (j in 1:N3) {
    current.cig.ecig.pt.est.se.sens <- current.cig.ecig.pt.est.se
    current.cig.ecig.pt.est.se.sens[,2] <- current.cig.ecig.increase.list[i]*current.cig.ecig.pt.est.se.sens[,2]
    current.cig.ecig.pt.est.se.sens[,2] <- sapply(current.cig.ecig.pt.est.se.sens[,2], function(x) min(x,1))
    current.cig.ecig.pt.est.se.sens[,3] <- 0
    tmp.adol <- adol.fxn(popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.ya <- ya.fxn(popl, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se, ya.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.adult.quit.sens <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se.sens, adult.ages, life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
    sens.current.cig.ecig.increase.mat[j,i] <- -1*tmp.adol$life.years + -1*tmp.ya$life.years + tmp.adult.quit.sens$life.years
  }
  print(paste("sensitivity current ecig current cig increase", round(100*i/length(current.cig.ecig.increase.list)),"% done",date()))
}
colnames(sens.current.cig.ecig.increase.mat) <- current.cig.ecig.increase.list

ever.tried.ecig.never.tried.cig.reduction.list <- seq(0,1,length=21)
sens.ever.tried.cig.never.tried.cig.reduction.mat <- matrix(NA,nrow=N2,ncol=length(ever.tried.ecig.never.tried.cig.reduction.list))
for (i in 1:length(ever.tried.ecig.never.tried.cig.reduction.list)) {
  for (j in 1:N2) {
    tmp.ever.tried.ecig.never.tried.cig.pt.est.se <- ever.tried.ecig.never.tried.cig.pt.est.se
    tmp.ever.tried.ecig.never.tried.cig.pt.est.se[,2] <- (1-ever.tried.ecig.never.tried.cig.reduction.list[i])*ever.tried.ecig.never.tried.cig.pt.est.se[,2]
    tmp.ever.tried.ecig.never.tried.cig.pt.est.se[,3] <- 0
    tmp.never.cig.ecig.pt.est.se <- never.cig.ecig.pt.est.se
    tmp.never.cig.ecig.pt.est.se[,3] <- (1-ever.tried.ecig.never.tried.cig.reduction.list[i])*tmp.never.cig.ecig.pt.est.se[,3]
    tmp.never.cig.ecig.pt.est.se[,4] <- 0
    tmp.adol.sens <- adol.fxn(popl, never.tried.cig.pt.est.se, tmp.ever.tried.ecig.never.tried.cig.pt.est.se, adol.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.ya.sens <- ya.fxn(popl, never.tried.cig.pt.est.se, tmp.never.cig.ecig.pt.est.se, ya.ages, life.years.pt.est.se, ecig.cig.or.pt.est.se, current.cig.cig.init.pt.est.se)
    tmp.adult.quit <- adult.quit.fxn(popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se, adult.ages, life.years.pt.est.se, past.year.cessation.tool.age.pt.est.se, pharmaceutical.aids.quit.pt.est.se, cig.quit.or.pt.est.se, longterm.quit.pt.est.se, 0)
    sens.ever.tried.cig.never.tried.cig.reduction.mat[j,i] <- -1*tmp.adol.sens$life.years + -1*tmp.ya.sens$life.years + tmp.adult.quit$life.years
  }
  print(paste("sensitivity ever tried ecig never tried cig reduction", round(100*i/length(ever.tried.ecig.never.tried.cig.reduction.list)),"% done",date()))
}
colnames(sens.ever.tried.cig.never.tried.cig.reduction.mat) <- ever.tried.ecig.never.tried.cig.reduction.list
save(sens.cig.quit.mat, sens.cig.init.mat, sens.current.cig.ecig.increase.mat, sens.ever.tried.cig.never.tried.cig.reduction.mat,
     file="data/threshold.Rdata")

############################
#tables of parameter values#
############################
N4 <- 10^5
delta.prob <- matrix(NA, nrow=N4, ncol=length(adult.ages))
init.prob <- matrix(NA, nrow=N4, ncol=2)

for (i in 1:N4) {
  past.year.cessation.tool.draw <- apply(past.year.cessation.tool.age.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[2]]),as.numeric(x[[3]])))[adult.ages]
  aid.draw <- apply(pharmaceutical.aids.quit.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[1]]),as.numeric(x[[2]])))[adult.ages]
  no.aid.draw <- apply(pharmaceutical.aids.quit.pt.est.se, 1, function(x) trunc.fxn(1,as.numeric(x[[3]]),as.numeric(x[[4]])))[adult.ages]
  cig.quit.or.draw <- rnorm(1,cig.quit.or.pt.est.se[1],cig.quit.or.pt.est.se[2])
  ecig.quit.draw <- pred.prob.fxn2(cig.quit.or.draw,aid.draw)  
  trans.cig.quit <-  past.year.cessation.tool.draw*(ecig.quit.draw-aid.draw) + (1-past.year.cessation.tool.draw)*(ecig.quit.draw-no.aid.draw)
  delta.prob[i,] <- trans.cig.quit

  baseline.init.adol.draw <- rnorm(1,0.066,(0.071-0.061)/(2*qnorm(0.975)))
  baseline.init.ya.draw <- rnorm(1,0.087,(0.095-0.079)/(2*qnorm(0.975)))
  or.draw <- exp(rnorm(1, ecig.cig.or.pt.est.se[1], ecig.cig.or.pt.est.se[2]))
  init.adol.ever.ecig <- pred.prob.fxn2(or.draw, baseline.init.adol.draw)
  init.ya.ever.ecig <- pred.prob.fxn2(or.draw, baseline.init.ya.draw)
  init.prob[i,1] <- init.adol.ever.ecig - baseline.init.adol.draw
  init.prob[i,2] <- init.ya.ever.ecig - baseline.init.ya.draw
  if (i %% 10000==1) print(paste(i,date()))
  }

adult.quit.table <- cbind(age=adult.ages.lab,
                          popl=formatC(popl5[adult.ages],big.mark=",",format="d"),
                          current.cig=apply(current.pt.est.se,1,format.fxn)[adult.ages],
                          tried.quit=apply(tried.quit.pt.est.se,1,format.fxn)[adult.ages],
                          current.ecig=apply(current.cig.ecig.pt.est.se,1,format.fxn)[adult.ages],
                          trans.cig.quit=c(rep(format.fxn5(c(delta.prob[,1:2])),2),rep(format.fxn5(c(delta.prob[,3:5])),3),rep(format.fxn5(c(delta.prob[,6:9])),4)),
                          shortterm.quit=format.fxn2(0.74,(0.78-0.70)/(2*qnorm(0.975))),
                          longterm.quit=format.fxn2(longterm.quit.pt.est.se[1],longterm.quit.pt.est.se[2]),
                          life.years=apply(as.matrix(life.years.pt.est.se),1,function(x) format.fxn3(x[[1]],x[[2]]))[adult.ages])
trans.cig.quit.pt.est <- past.year.cessation.tool.age.pt.est.se[,2]*(pharmaceutical.aids.quit.pt.est.se[,1]*cig.quit.or.pt.est.se[1]-pharmaceutical.aids.quit.pt.est.se[,1]) + (1-past.year.cessation.tool.age.pt.est.se[,2])*(pharmaceutical.aids.quit.pt.est.se[,1]*cig.quit.or.pt.est.se[1]-pharmaceutical.aids.quit.pt.est.se[,3])
adult.quit.table2 <- cbind(popl=popl5[adult.ages],
                           current.cig=current.pt.est.se[adult.ages,2],
                           tried.quit=tried.quit.pt.est.se[adult.ages,2],
                           current.ecig=current.cig.ecig.pt.est.se[adult.ages,2],
                           trans.cig.quit <- past.year.cessation.tool.age.pt.est.se[,2]*(pred.prob.fxn2(cig.quit.or.pt.est.se[1],pharmaceutical.aids.quit.pt.est.se[,1])-pharmaceutical.aids.quit.pt.est.se[,1]) + (1-past.year.cessation.tool.age.pt.est.se[,2])*(pred.prob.fxn2(cig.quit.or.pt.est.se[1],pharmaceutical.aids.quit.pt.est.se[,1])-pharmaceutical.aids.quit.pt.est.se[,3]),
                           shortterm.quit=0.74,
                           longterm.quit=longterm.quit.pt.est.se[1],
                           life.years=life.years.pt.est.se[adult.ages,1])

adol.table <- cbind(age=adol.ages,
                    popl=formatC(popl[adol.ages],big.mark=",",format="d"),
                    never.tried.cig=apply(never.tried.cig.pt.est.se,1,format.fxn)[adol.ages],
                    tried.ecig.never.tried.cig=apply(ever.tried.ecig.never.tried.cig.pt.est.se,1,format.fxn)[adol.ages],
                    trans.ecig.cig=format.fxn5(init.prob[,1]),
                    init.daily=apply(current.cig.cig.init.pt.est.se,1,format.fxn)[adol.ages],
                    life.years=rep(format.fxn3(life.years.pt.est["35"],life.years.pt.est["35"]/10),6))
adol.table2 <- cbind(popl=popl[adol.ages],
                     never.tried.cig=never.tried.cig.pt.est.se[adol.ages,2],
                     tried.ecig.never.tried.cig=ever.tried.ecig.never.tried.cig.pt.est.se[adol.ages,2],
                     trans.ecig.cig=rep(pred.prob.fxn2(exp(ecig.cig.or.pt.est.se[1]),0.066)-0.066, length(adol.ages)),
                     init.daily=current.cig.cig.init.pt.est.se[adol.ages,2],
                     life.years=life.years.pt.est["35"])

ya.table <- cbind(age=ya.ages,
                  popl=formatC(popl[ya.ages],big.mark=",",format="d"),
                  never.tried.cig=c(apply(never.tried.cig.pt.est.se,1,format.fxn)[as.character(18:25)],rep(apply(never.tried.cig.pt.est.se,1,format.fxn)[as.character(26)],4)),
                  tried.ecig.never.tried.cig=apply(never.cig.ecig.pt.est.se[,c(1,3,4)],1,format.fxn),
                  trans.ecig.cig=format.fxn5(init.prob[,2]),
                  init.daily=apply(current.cig.cig.init.pt.est.se,1,format.fxn4)[ya.ages],
                  life.years=format.fxn3(life.years.pt.est["35"],life.years.pt.est["35"]/10))
ya.table2 <- cbind(popl=popl[ya.ages],
                   never.tried.cig=c(never.tried.cig.pt.est.se[as.character(18:25),2],rep(never.tried.cig.pt.est.se[as.character(26),2],4)),
                   tried.ecig.never.tried.cig=never.cig.ecig.pt.est.se[,3],
                   trans.ecig.cig=rep(pred.prob.fxn2(exp(ecig.cig.or.pt.est.se[1]),0.087)-0.087, length(ya.ages)),
                   init.daily=current.cig.cig.init.pt.est.se[ya.ages,2],
                   life.years=life.years.pt.est["35"])
ecig.cig.init.table <- cbind(age=c("12-17","18-29"),
                             prob.cig.init.never.ecig=c("6.6 (6.1, 7.1)", "8.7 (7.9, 9.5)"),
                             prob.cig.init.ever.ecig=c(paste(formatC(6.6+mean(100*init.prob[,1]),digits=1,format="f")," (",formatC(6.1+quantile(100*init.prob[,1],probs=0.025),digits=1,format="f"),", ",formatC(7.1+quantile(100*init.prob[,1],probs=0.975),digits=1,format="f"),")",sep=""),
                                                       paste(formatC(8.7+mean(100*init.prob[,2]),digits=1,format="f")," (",formatC(7.9+quantile(100*init.prob[,2],probs=0.025),digits=1,format="f"),", ",formatC(9.5+quantile(100*init.prob[,2],probs=0.975),digits=1,format="f"),")",sep="")),
                             delta.cig.init=c(format.fxn5(init.prob[,1]), format.fxn5(init.prob[,2])))

adult.ages3 <- c(25,35,50)
adult.ages3.lab <- c("25-34","35-49","50-69")
ecig.quit.table <- rbind(cbind(age=adult.ages3.lab,
                               cessation.aid=apply(past.year.cessation.tool.age.pt.est.se,1,format.fxn4)[as.character(adult.ages3)]),
                         cbind(age=adult.ages3.lab,
                               quit.aid=apply(pharmaceutical.aids.quit.pt.est.se[,1:2],1,format.fxn6)[as.character(adult.ages3)]),
                         cbind(age=adult.ages3.lab,
                               quit.no.aid=apply(pharmaceutical.aids.quit.pt.est.se[,3:4],1,format.fxn6)[as.character(adult.ages3)]),
                          cbind(age="25-69",format.fxn7(cig.quit.or.pt.est.se)))
ecig.cig.quit.table <- cbind(adult.ages3.lab,
                             c(format.fxn5(c(delta.prob[,1:2])),format.fxn5(c(delta.prob[,3:5])),format.fxn5(c(delta.prob[,6:9]))))

write.csv(adult.quit.table,file="tables/adult.quit.table.csv")
write.csv(adol.table,file="tables/adol.table.csv")
write.csv(ya.table,file="tables/ya.table.csv")
write.csv(ecig.cig.init.table,file="tables/ecig.cig.init.table.csv")
write.csv(ecig.quit.table,file="tables/ecig.quit.table.csv")
write.csv(ecig.cig.quit.table,file="tables/ecig.cig.quit.table.csv")

save(adol.table2, ya.table2, adult.quit.table2, popl5, current.pt.est.se, tried.quit.pt.est.se, current.cig.ecig.pt.est.se,
     popl, never.tried.cig.pt.est.se, ever.tried.ecig.never.tried.cig.pt.est.se, never.tried.cig.pt.est.se, never.cig.ecig.pt.est.se,
     file="data/confirmation.Rdata")