# present substantive effects
load("estimates.RData")
source("effectSim.R");source("effectSimPlot.R")
library(AER);library(arm)
lab <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1918,1922,1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
tlab <- 1:40
mpslib <- aggregate(allout$loyalty.x,by=list(allout$parliament),FUN=length)
voteslib <- aggregate(libs$Rice,by=list(libs$Parlement),FUN=length)

#x <- cbind(1,seq(from=min(allout$participation.norm), to=max(allout$participation.norm),length=1000),3,0,0,1,0,0,0,0,0,0,8,8,9,9)
#probs.out <- effectSim(model=modslib[[2]],x=x,sims=1000,covvar=vcovHAC(modslib[[2]]))
#x1 <- cbind(1,seq(from=min(allout$participation.norm), to=max(allout$participation.norm),length=1000),3,0,0,0,0,0,0,0,0,0,8,8,9,9)
#probs.outC <- effectSim(model=modscon[[2]],x=x1,sims=1000,covvar=vcovHAC(modscon[[2]]))

#x3 <- cbind(1,seq(from=min(allout$participation.norm), to=max(allout$participation.norm),length=1000),3,0,0,1,0,0,0,0,0,0,18,18,19,19)
#probs.out3 <- effectSim(model=modslib[[2]],x=x3,sims=1000,covvar=vcovHAC(modslib[[2]]))
#x4 <- cbind(1,seq(from=min(allout$participation.norm), to=max(allout$participation.norm),length=1000),3,0,0,0,0,0,0,0,0,0,18,18,19,19)
#probs.outC4 <- effectSim(model=modscon[[2]],x=x4,sims=1000,covvar=vcovHAC(modscon[[2]]))

#pdf("participation.pdf",height=5.5, width=7)
#par(mfrow=c(2,2))
#effectSimPlot(probs.out,low=min(allout$participation.norm),high=max(allout$participation.norm),x=x,i=2,covariate=allout$participation.norm,color="grey70",
#              main="Liberal Party 9th",sub="(Government)",xlab="Participation",ylab="Voting Loyalty",ylim=c(.8,1),bty="n")
#effectSimPlot(probs.outC,low=min(allout$participation.norm),high=max(allout$participation.norm),x=x1,i=2,covariate=allout$participation.norm,color="grey70",
#              main="Conservative Party 9th",sub="(Opposition)",xlab="Participation",ylab="Voting Loyalty",ylim=c(.8,1),bty="n")
#effectSimPlot(probs.out3,low=min(allout$participation.norm),high=max(allout$participation.norm),x=x3,i=2,covariate=allout$participation.norm,color="grey70",
#              main="Liberal Party 19th",sub="(Government)",xlab="Participation",ylab="Voting Loyalty",ylim=c(.965,1),bty="n")
#effectSimPlot(probs.outC4,low=min(allout$participation.norm),high=max(allout$participation.norm),x=x4,i=2,covariate=allout$participation.norm,color="grey70",
#              main="Conservative Party 19th",sub="(Opposition)",xlab="Participation",ylab="Voting Loyalty",ylim=c(.965,1),bty="n")
#dev.off()
#quantile(allout$participation.x[allout$parliament==19])
#quantile(alloutc$participation.x[alloutc$parliament==19])
# predicted level of voting loyalty, by term, holding all other variables constant
summary(modslib[[4]])

relparl <- rep(0,39)
probs.parl <- list()
meancohort <- rep(0,39)
input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
probs.parl[[1]] <- effectSim(model=modslib[[4]],x=input,sims=1000,covvar=vcovHAC(modslib[[4]]))
for (i in 1:39){
  relparl[i] <- 1
  meancohort[i] <- mean( coef(modslib[[4]])[11:49])
  input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
  relparl[i] <- 0
  meancohort[i] <- 0
  probs.parl[[i+1]] <- effectSim(model=modslib[[4]],x=input,sims=1000,covvar=vcovHAC(modslib[[4]]))
  cat("done with",i,"\n")
}

relcohort <- rep(0,39)
probs.cohort <- list()
meanparl <- rep(0,39)
input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
probs.cohort[[1]] <- effectSim(model=modslib[[4]],x=input,sims=1000,covvar=vcovHAC(modslib[[4]]))
for (i in 1:39){
  relcohort[i] <- 1
  meanparl[i] <- mean( coef(modslib[[4]])[50:88])
  input <- c(1,0,3,0,0,0,0,0,0,0,relcohort,meanparl)
  relcohort[i] <- 0
  meanparl[i] <- 0
  probs.cohort[[i+1]] <- effectSim(model=modslib[[4]],x=input,sims=1000,covvar=vcovHAC(modslib[[4]]))
  cat("done with",i,"\n")
}

parlprob <- do.call("rbind",probs.parl)
parlPP <-rowMeans(parlprob)
parlSD <- apply(parlprob,1,sd)
cohortprob <- do.call("rbind",probs.cohort)
cohortPP <- rowMeans(cohortprob)
cohortSD <- apply(cohortprob,1,sd)

lab <- c(1867,1872,1874,1878,1882,1887,1891,1896,1900,1904,1908,1911,1918,1922,1925,1926,1930,1935,1940,1945,1949,1953,1957,1958,1962,1963,1965,1968,1972,1974,1979,1980,1984,1988,1993,1997,2000,2004,2006,2008)
tlab <- 1:40

pdf("Figure-2a.pdf",width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
plot(1:40,parlPP,ylim=c(0.4,1),bty="n",ylab="Predicted loyalty (mean)"
     ,xaxt="n",pch=18,xlab="Cohort vs. Parliament Effects (Liberal party)")
legend("bottomleft",pch=c(18,16),legend=c("parliament","cohort"),cex=.75,bty="n",col=c("black","grey70"))
segments(x0=1:40,x1=1:40,y0=parlPP+1.96*parlSD,y1=parlPP-1.96*parlSD,col="black")
points(1.25:40.25,cohortPP,pch=16,col="grey70")
segments(x0=1.25:40.25,x1=1.25:40.25,y0=cohortPP+1.96*cohortSD,y1=cohortPP-1.96*cohortSD,col="grey70")
lines(lowess(1.25:40.25,cohortPP,f=.5),col="grey70",lwd=3)
lines(lowess(1:40,parlPP,f=.5),lwd=3)
axis(1,at=seq(1,40,1),labels=lab)
dev.off()
############# Conservatives
summary(modscon[[4]])

relparl <- rep(0,37)
probs.parl <- list()
meancohort <- rep(0,38)
input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
probs.parl[[1]] <- effectSim(model=modscon[[4]],beta=coef(modscon[[4]])[1:85],x=input,sims=1000,covvar=vcovHAC(modscon[[4]],na.omit=TRUE))
for (i in 1:37){
  relparl[i] <- 1
  meancohort[i] <- mean( coef(modscon[[4]])[11:48])
  input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
  relparl[i] <- 0
  meancohort[i] <- 0
  probs.parl[[i+1]] <- effectSim(model=modscon[[4]],beta=coef(modscon[[4]])[1:85],x=input,sims=1000,covvar=vcovHAC(modscon[[4]],na.omit=TRUE))
  cat("done with",i,"\n")
}

relcohort <- rep(0,38)
probs.cohort <- list()
meanparl <- rep(0,37)
input <- c(1,0,3,0,0,0,0,0,0,0,meancohort,relparl)
probs.cohort[[1]] <- effectSim(model=modscon[[4]],beta=coef(modscon[[4]])[1:85],x=input,sims=1000,covvar=vcovHAC(modscon[[4]],na.omit=TRUE))
for (i in 1:37){
  relcohort[i] <- 1
  meanparl[i] <- mean( coef(modscon[[4]])[49:85])
  input <- c(1,0,3,0,0,0,0,0,0,0,relcohort,meanparl)
  relcohort[i] <- 0
  meanparl[i] <- 0
  probs.cohort[[i+1]] <- effectSim(model=modscon[[4]],beta=coef(modscon[[4]])[1:85],x=input,sims=1000,covvar=vcovHAC(modscon[[4]],na.omit=TRUE))
  cat("done with",i,"\n")
}

parlprob <- do.call("rbind",probs.parl)
parlPP <-rowMeans(parlprob)
parlSD <- apply(parlprob,1,sd)
cohortprob <- do.call("rbind",probs.cohort)
cohortPP <- rowMeans(cohortprob)
cohortSD <- apply(cohortprob,1,sd)
parlPP <- parlPP[1:34]
parlSD <- parlSD[1:34]
cohortPP <- cohortPP[1:34]
cohortSD <- cohortSD[1:34]

pdf("Figure-2b.pdf",width=7,height=2.75)
par(mar=c(4.1,4.1,3.1,2.1))
plot(1:34,parlPP,ylim=c(0.4,1),bty="n",ylab="Predicted loyalty (mean)"
     ,xaxt="n",pch=18,xlab="Cohort vs. Parliament Effects (Conservative party)")
legend("bottomleft",pch=c(18,16),legend=c("parliament","cohort"),cex=.75,bty="n",col=c("black","grey70"))
segments(x0=1:34,x1=1:34,y0=parlPP+1.96*parlSD,y1=parlPP-1.96*parlSD,col="black")
points(1.25:34.25,cohortPP,pch=16,col="grey70")
segments(x0=1.25:34.25,x1=1.25:34.25,y0=cohortPP+1.96*cohortSD,y1=cohortPP-1.96*cohortSD,col="grey70")
lines(lowess(1.25:34.25,cohortPP,f=.5),col="grey70",lwd=3)
lines(lowess(1:34,parlPP,f=.5),lwd=3)
axis(1,at=seq(1,34,1),labels=lab[1:34])
dev.off()



