#
#  Barry Edwards
#  Replication file for Formulating Voting Rights Act Remedies to Address Current Conditions
#
#  State-byState Analysis 
#  Note: this script may not reproduce Figure 4 exactly as it appears in the paper because
#  I am using Monte Carlo simulation to estimate parity conditions.
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
#
setwd(choose.dir())    # choose directory/folder with replication materials

library(foreign)



# Figure 4: State-by-state comparison of district proportions providing minority 
#           voters equal opportunities in open-seat elections, 2002-2010

nDraws <- 1000
############################################# break even by state for Af Am elections, 2002-2010

afAmResults <- read.csv("AfAmRepSimulationData.csv")
stateLabels <- afAmResults$stateAbbr
section5States <- afAmResults$section5
# these coef, sd from state-varying model
latinoVAPcoef	<-    -6.68  #   
latinoVAPcoefSD	<-    4.18  #   
openSeatCoef	<-      -2.89   #  
openSeatCoefSD	<-      1.10   #  
whiteIncCoef	<-   -7.66   #  
whiteIncCoefSD	<-   1.53   #  
commonSlope <- 34.69
commonSlopeSD <- 5.06
commonIntercept <- -8.41
commonInterceptSD <- 1.56
meanAfAmBreakEven <- NULL
sdAfAmBreakEven   <- NULL
latinoPropOfOtherInState  <- NULL
meanAfAmParity <- NULL
sdAfAmParity <- NULL
thisState <- 1

while (thisState <= 51) 
{
  drawLatinoVAPcoef <- rnorm(nDraws, latinoVAPcoef, latinoVAPcoefSD)
  drawOpenSeatCoef  <- rnorm(nDraws, openSeatCoef, openSeatCoefSD)
  drawWhiteIncCoef  <- rnorm(nDraws, whiteIncCoef, whiteIncCoefSD)
  drawCommonSlopeCoef  <- rnorm(nDraws, commonSlope, commonSlopeSD)
  drawCommonInterceptCoef  <- rnorm(nDraws, commonIntercept, commonInterceptSD)
  drawStateIntercept <- rnorm(nDraws,afAmResults$interceptChangeAfAm[thisState],afAmResults$sdInterceptChangeAfAm[thisState]) # 
  drawStateSlope     <- rnorm(nDraws,afAmResults$slopeChangeAfAm[thisState],afAmResults$sdSlopeChangeAfAm[thisState]) # 
  drawStateIntercept <- drawStateIntercept + drawCommonInterceptCoef  
  drawStateSlope     <- drawStateSlope + drawCommonSlopeCoef  
  latinoPropOfOtherInState[thisState] <- afAmResults$hispOver18[thisState] / (afAmResults$popOver18[thisState] - afAmResults$blackOver18[thisState])
  breakEvenAfAmVAP <- (drawStateIntercept +  drawOpenSeatCoef*1 + drawWhiteIncCoef*0 + drawLatinoVAPcoef*latinoPropOfOtherInState[thisState]) / (drawLatinoVAPcoef*latinoPropOfOtherInState[thisState] - 1*drawStateSlope)
  breakEvenAfAmVAP <- breakEvenAfAmVAP[breakEvenAfAmVAP <=1 & breakEvenAfAmVAP >=0]
  meanAfAmBreakEven[thisState] <- mean(breakEvenAfAmVAP)
  sdAfAmBreakEven[thisState] <- sd(breakEvenAfAmVAP)
  goThruDraws <- 1
  thisDrawParity <- NULL
  while (goThruDraws <= nDraws)
  {
    findParity <- NULL
    bvapRange <- seq(0,1,by=.01)
    hvapRange <- (1-bvapRange)*latinoPropOfOtherInState[thisState] 
    XB  <- drawLatinoVAPcoef[goThruDraws]*hvapRange + drawStateSlope[goThruDraws]*bvapRange + drawOpenSeatCoef[goThruDraws]*1 + drawStateIntercept[goThruDraws] 
    probability <- exp(XB) / (1 + exp(XB))
    findParity <- abs(probability - bvapRange)
    thisDrawParity[goThruDraws] <- bvapRange[findParity==min(findParity[5:95])]
    goThruDraws <- goThruDraws + 1
  }
  meanAfAmParity[thisState] <- mean(thisDrawParity)
  sdAfAmParity[thisState] <- sd(thisDrawParity)
  thisState <- thisState + 1
}

afAmResultsOrdered <- cbind(seq(1,51,by=1),meanAfAmBreakEven,sdAfAmBreakEven,meanAfAmParity,sdAfAmParity)
afAmResultsOrdered <- afAmResultsOrdered[order(-meanAfAmParity),]
stateNumber <- seq(51,1,by=-1)
stateRankOrder <- afAmResultsOrdered[,1]




par(mfrow=c(1,2))
par(mar=c(4.6,3.1,2.6,1.1), mgp=c(2,.6,.5), family = "serif")

plot(x="",y="",ylim=c(1,51),xlim=c(0,1),axes=F,ylab="",xlab="Proportion of Voting Age Population",
     main="(a) African American Voters",cex.lab=1.0,cex.main=1.0)
axis(side=2,tick=T,at=seq(1,51,by=1),labels=F,line=0)
axis(side=1,at=seq(0,1,by=.25),line=0)
text(-.20, 51:1, adj = 0,labels = stateLabels[stateRankOrder], xpd = TRUE,cex=.8)

segments(afAmResultsOrdered[,4]-afAmResultsOrdered[,5], stateNumber, afAmResultsOrdered[,4]+afAmResultsOrdered[,5], stateNumber, lty=2)
points(x=afAmResultsOrdered[,4],y=stateNumber,pch=16,col="white",cex=1)
points(x=afAmResultsOrdered[,4],y=stateNumber,pch=1 + 15*section5States[stateRankOrder],cex=1)

# find usa in ranking
i <- 1
while (i <= 51)
{
  if (stateRankOrder[i] == 51) { usaRank <- i }
  i <- i + 1
}

segments(afAmResultsOrdered[usaRank,4]-afAmResultsOrdered[usaRank,5], 52 - usaRank, afAmResultsOrdered[usaRank,4]+afAmResultsOrdered[usaRank,5], 52 - usaRank, lty=1)
points(x=afAmResultsOrdered[usaRank,4],y=52-usaRank,pch=15,cex=1,col="black")

box()
legend(x=.5, y=5, lty=c(2,2,1), lwd=c(1,1,1), pch=c(16,1,15), 
       legend=c('Section 5 States','Other States','National Average'),cex=.7)


################################### latino election results 2002-2010

latinoResults <- read.csv("LatinoRepSimulationData.csv")
stateLabels <- latinoResults$stateAbbr
section5States <- latinoResults$section5
# these coef, sd from state-varying model
blackVAPcoef	<-     -13.40  #    
blackVAPcoefSD	<-  2.52
openSeatCoef	<-      -1.33  #    
openSeatCoefSD	<-  0.67
whiteIncCoef	<-   -4.13    # 
whiteIncCoefSD	<-  0.54
commonSlope <- 14.05
commonSlopeSD <- 1.41
commonIntercept <- -4.14
commonInterceptSD <- 0.54
meanLatinoBreakEven <- NULL
sdLatinoBreakEven   <- NULL
blackPropOfOtherInState  <- NULL
meanLatinoParity <- NULL
sdLatinoParity <- NULL

thisState <- 1

while (thisState <= 51) 
{
  drawOpenSeatCoef <- rnorm(nDraws,openSeatCoef,openSeatCoefSD)
  drawWhiteIncCoef <- rnorm(nDraws,whiteIncCoef,whiteIncCoefSD)
  drawblackVAPcoef <- rnorm(nDraws,blackVAPcoef,blackVAPcoefSD)
  drawCommonSlope <- rnorm(nDraws,commonSlope,commonSlopeSD)
  drawCommonIntercept <- rnorm(nDraws,commonIntercept,commonInterceptSD)
  drawStateIntercept <- rnorm(nDraws,latinoResults$interceptChangeLatino[thisState],latinoResults$sdInterceptChangeLatino[thisState])
  drawStateSlope <- rnorm(nDraws,latinoResults$slopeChangeLatino[thisState],latinoResults$sdSlopeChangeLatino[thisState])
  drawStateSlope <- drawStateSlope + drawCommonSlope
  drawStateIntercept <- drawStateIntercept + drawCommonIntercept
  blackPropOfOtherInState[thisState] <- latinoResults$blackOver18[thisState] / (latinoResults$popOver18[thisState] - latinoResults$hispOver18[thisState])
  breakEvenLatinoVAP <- (drawStateIntercept +  drawOpenSeatCoef*1 + drawWhiteIncCoef*0 + drawblackVAPcoef*blackPropOfOtherInState[thisState]) / (drawblackVAPcoef*blackPropOfOtherInState[thisState] - 1*drawStateSlope)
  meanLatinoBreakEven[thisState] <- mean(breakEvenLatinoVAP)
  sdLatinoBreakEven[thisState] <- sd(breakEvenLatinoVAP)
  
  goThruDraws <- 1
  thisDrawParity <- NULL
  while (goThruDraws <= nDraws)
  {
    findParity <- NULL
    hvapRange <- seq(0,1,by=.01)
    bvapRange <- (1-hvapRange)*blackPropOfOtherInState[thisState] 
    XB  <- drawblackVAPcoef[goThruDraws]*bvapRange + drawStateSlope[goThruDraws]*hvapRange + drawOpenSeatCoef[goThruDraws]*1 + drawStateIntercept[goThruDraws] 
    probability <- exp(XB) / (1 + exp(XB))
    findParity <- abs(probability - hvapRange)
    thisDrawParity[goThruDraws] <- hvapRange[findParity==min(findParity[5:95])]
    goThruDraws <- goThruDraws + 1
  }
  meanLatinoParity[thisState] <- mean(thisDrawParity)
  sdLatinoParity[thisState] <- sd(thisDrawParity)
  thisState <- thisState + 1
}

latinoResultsOrd <- cbind(seq(1,51,by=1),meanLatinoBreakEven,sdLatinoBreakEven,meanLatinoParity,sdLatinoParity)
latinoResultsOrdered <- latinoResultsOrd[order(-meanLatinoParity),]
stateNumber <- seq(51,1,by=-1)
stateRankOrder <- latinoResultsOrdered[,1]

plot(x="",y="",ylim=c(1,51),xlim=c(0,1),axes=F,ylab="",xlab="Proportion of Voting Age Population",
     main="(b) Latino Voters",cex.lab=1.0,cex.main=1.0)
axis(side=2,tick=T,at=seq(1,51,by=1),labels=F,line=0)
axis(side=1,at=seq(0,1,by=.25),line=0)
text(-.20, 51:1, adj = 0,labels = stateLabels[stateRankOrder], xpd = TRUE,cex=.8)

segments(latinoResultsOrdered[,4]-latinoResultsOrdered[,5], stateNumber, latinoResultsOrdered[,4]+latinoResultsOrdered[,5], stateNumber, lty=2)
points(x=latinoResultsOrdered[,4],y=stateNumber,pch=16,col="white",cex=1)
points(x=latinoResultsOrdered[,4],y=stateNumber,pch=1 + 15*section5States[stateRankOrder],cex=1)

# find usa in ranking
i <- 1
while (i <= 51)
{
  if (stateRankOrder[i] == 51) { usaRank <- i }
  i <- i + 1
}

segments(latinoResultsOrdered[usaRank,4]-latinoResultsOrdered[usaRank,5], 52 - usaRank, latinoResultsOrdered[usaRank,4]+latinoResultsOrdered[usaRank,5], 52 - usaRank, lty=1)
points(x=latinoResultsOrdered[usaRank,4],y=52-usaRank,pch=15,cex=1,col="black")

box()
# legend(x=.65, y=5, lty=c(2,2,1), lwd=c(1,1,1), pch=c(16,1,15), 
#       legend=c('Section 5 States','Other States','National Average'),cex=.8)



# R Script for Figure 5: Probabilities of African American and Latino voters 
#                        electing preferred candidates in open-seat elections 
#                        in minority coalition districts

####################### based on analysis of 2000-2010 data

# Af Am voter success

blackVAP1 <-  19.58
hispanicVAP1 <-   -0.31
whiteIncumbent1 <-   -5.47
openSeatcoeff1 <-  -2.81
constant1 <-  -4.32

# 2           
blackVAP2 <-  -13.40
hispanicVAP2 <-   14.05
whiteIncumbent2 <-  -4.13
openSeatcoeff2 <-  -1.33
constant2<-   -4.14

blackVAPmean <- .122577
hispanicVAPmean <-   .1320064 
whiteIncumbMean <-   .7554023
openSeatMean <- .0914943 
BVAPrange <- seq(0,1,by=.01)
HVAPrange <- seq(1,0,by=-.01)

AfAmXB <-     blackVAP1*BVAPrange + hispanicVAP1*HVAPrange + whiteIncumbent1*0 + openSeatcoeff1*1  + constant1*1    
probElectAfAm <- exp(AfAmXB) / (1 + exp(AfAmXB))
# given range of BVAP values, what HVAP needed for 50% prob electing AfAm
HVAPfor50chanceAfAm <- (blackVAP1*BVAPrange  +  openSeatcoeff1*1  + constant1*1 - log(5)) / (- hispanicVAP1)
HVAPfor90chanceAfAm <- (blackVAP1*BVAPrange  + openSeatcoeff1*1 + constant1*1 - log(9)) / (- hispanicVAP1)
HVAPfor12chanceAfAm <- (blackVAP1*BVAPrange  + openSeatcoeff1*1 + constant1*1 - log(1.22)) / (- hispanicVAP1)

LatinoAmXB <-     blackVAP2*BVAPrange + hispanicVAP2*HVAPrange + whiteIncumbent2*0 + openSeatcoeff2*1   + constant2*1    
probElectLatino <- exp(LatinoAmXB) / (1 + exp(LatinoAmXB))
# given range of BVAP values, what HVAP needed for 50% prob electing LATINO
HVAPfor50Latino = (blackVAP2*BVAPrange  + openSeatcoeff2*1 + constant2*1 - log(5)) / (- hispanicVAP2)
HVAPfor90Latino = (blackVAP2*BVAPrange  + openSeatcoeff2*1 + constant2*1 - log(9)) / (- hispanicVAP2)
HVAPfor14Latino = (blackVAP2*BVAPrange  + openSeatcoeff2*1 + constant2*1 - log(1.37)) / (- hispanicVAP2)


################# create plot

par(mar=c(4.1,4.1,2.1,1.1), mgp=c(2,.6,.5), family = "serif")
par(xpd=FALSE)
plot(x="",y="",xlim=c(0,1),ylim=c(0,1),xlab="Proportion African American",ylab="Proportion Latino",font=2,cex.lab=1.0,
     axes=F)
axis(side=1,at=seq(0,1,by=.1),cex.axis=.9, line=0)
axis(side=2,at=seq(0,1,by=.1),las=2, line=0)
box()

polygon(x=c(BVAPrange,2,1),y=c(HVAPfor50chanceAfAm,0,1),border=F,col="gray35")
polygon(x=c(-.5,BVAPrange,0),y=c(0,HVAPfor50Latino,2),border=F,col="gray65")

polygon(x=c(BVAPrange,2,1),y=c(HVAPfor90chanceAfAm,0,1),border=F,col="gray10")
polygon(x=c(-.5,BVAPrange,0),y=c(0,HVAPfor90Latino,2),border=F,col="gray50")

rect(.8,-1,2,1,border="gray10",col="gray10")
polygon(x=c(-1,2,2), y=c(2,2,-1),col="white")
par(xpd=TRUE)
polygon(x=c(-1,2,2), y=c(2,2,-1),col="white", border=F)
legend(x=.15, y=1.05, border=T, fill=c('gray50', 'gray65'), 
       legend=c('90% Prob. Latino Success','50% Prob. Latino Success'),cex=.7)
legend(x=.6, y=.6, border=T, fill=c('gray10', 'gray35'), 
       legend=c('90% Prob. Af. Am. Success', '50% Prob. Af. Am. Success'),cex=.7)




# Figure 6: Expected minority representation at varying VRA targets

### state-by-state look
stateData <- read.csv("AfAmRepSimulationData.csv")
attach(stateData)
####################### based on analysis of 2002-2010 data
# fixed effects common to all the states
#  bugs: node	 mean	 sd	 MC error	2.5%	median	97.5%	start	sample
commonIntercept <-	-8.414	# 1.564	0.1168	-11.95	-8.235	-5.861	10000	10002
commonSlope <- 	34.69	# 5.057	0.3881	25.76	34.21	46.05	10000	10002
openSeatCoef <-	-2.888 #	1.095	0.05005	-5.255	-2.822	-0.8924	10000	10002
whiteIncCoef <-		-7.662 #	1.528	0.0941	-10.94	-7.544	-5.038	10000	10002
hvapCoef <- 	-6.68 #	4.179	0.3125	-17.46	-6.103	-0.482	10000	10002

########################### declare variables used in simulation
i <- 1
totalExpectedAfAmReps <- NULL
totalExpectedLatinoReps <- NULL
targetProportion <- NULL
minAfAmProportion <- NULL
minPopNeededPerDistrict <- NULL
afAmDistricts <- NULL
afAmRemainder <- NULL
afAmOtherDist <- NULL
afAmTargetXB <- NULL
probTargetAfAmDist <- NULL
expectAfAmRepsTarget <- NULL
remainderAfAmXB <- NULL
probRemainderAfAmDist <- NULL
expectAfAmRepsRemainder <- NULL
totalAfAmReps <- NULL
totalExpectedAfAmRepsSection5only <- NULL
totalExpectedAfAmRepsNEonly <- NULL
afAmMeanConstant <- NULL
latinoVAPtargetDist <- NULL
latinoVAPotherDist <- NULL
blackVotersProportion <- NULL
otherVotersProportion <- NULL
blackVotersNeeded <- NULL
targetDistOtherPopulation <- NULL
targetDistOtherVoters <- NULL
targetDistLatinoVoters <- NULL
blackPopTargetDistrict <- NULL

simulatedAfAmRepsTarget <- NULL
simulatedAfAmRepsRemainder <- NULL
totalSimulated <- NULL
simulationIterations <- 5000  # change number of iterations
seatOpensRandomly <- .1
probAfAmDist <- NULL

simulatedMean <- NULL
simulatedSD <- NULL
lower95quantile <- NULL
upper95quantile <- NULL
avgDistVoters <- NULL

# i <- 400
# j <- 33 # north carolina
# i <- 394 # at issue in NC litigation

while(i <= 1000) {
  
  simulatedAfAmRepsTotal <- rep(0,simulationIterations)
  simulatedAfAmRepsSection5 <- rep(0,simulationIterations)
  
  j <- 1
  while (j <= 50) {
    
    
    # determine how many target districts can be made
    targetProportion[j] <- i/1000
    
    minAfAmProportion[j] <- blackOver18[j]/popOver18[j]
    if(targetProportion[j] < minAfAmProportion[j]) {	targetProportion[j] <- minAfAmProportion[j] }
    afAmProportion <- blackOver18[j]/popOver18[j]
    
    geoDensityParameter <- 1 - (1/(1 - afAmProportion))*(1/(1 - afAmProportion))*(targetProportion[j] - afAmProportion)*(targetProportion[j] - afAmProportion)
    # alternative specification: linear relationship
    # geoDensityParameter <- (-1/(1 - afAmProportion))*targetProportion[j] + (1/(1 - afAmProportion))
    if (targetProportion[j] < afAmProportion) { geoDensityParameter  <- 1 } 
    
    avgDistSize[j] <- totPop[j]/nReps2010[j]
    avgDistVoters[j] <- popOver18[j]/nReps2010[j]
    
    blackVotersProportion[j] <- blackOver18[j]/blackPop[j]
    otherVotersProportion[j] <- (popOver18[j] - blackOver18[j]) / popOver18[j]
    
    blackVotersNeeded[j] <- (avgDistSize[j]*otherVotersProportion[j]) / ( ((1 - targetProportion[j])/targetProportion[j]) + (1/blackVotersProportion[j])*otherVotersProportion[j])
    
    blackVotersNeeded[j] <- blackVotersNeeded[j]
    blackPopTargetDistrict[j] <- blackVotersNeeded[j]*(1/blackVotersProportion[j])
    
    # blackPopTargetDistrict[j] <- avgDistSize[j]*targetProportion[j] 
    
    afAmDistricts[j] <- floor(blackPop[j]*geoDensityParameter/blackPopTargetDistrict[j])
    
    if (afAmDistricts[j] >= nReps2010[j])  # can reach target in all districts
    {
      afAmDistricts[j] <- nReps2010[j]
      afAmRemainder[j] <- 0
      afAmOtherDist[j] <- 0
    }
    if (afAmDistricts[j] < nReps2010[j])   # can reach target in less than all districts
    {
      afAmRemainder[j] <- blackOver18[j] - (afAmDistricts[j] * blackVotersNeeded[j])
      afAmOtherDist[j] <- afAmRemainder[j] / ((nReps2010[j] - afAmDistricts[j])* avgDistVoters[j])
    }
    if (afAmDistricts[j] == 0)   # cannot reach target in any district
    {
      blackPopTargetDistrict[j] <- 0
      afAmOtherDist[j] <- blackOver18[j]/(avgDistVoters[j]*nReps2010[j])
    }
    
    targetDistOtherPopulation[j] <- avgDistSize[j] - (blackPopTargetDistrict[j]) 
    targetDistOtherVoters[j] <- targetDistOtherPopulation[j]* otherVotersProportion[j]
    targetDistLatinoVoters[j] <- targetDistOtherVoters[j]*(hispOver18[j]/(popOver18[j] - blackOver18[j]))
    latinoVAPtargetDist[j] <- targetDistLatinoVoters[j] / (blackVotersNeeded[j] + targetDistOtherVoters[j])
    
    latinoVAPotherDist[j] <- (1 - afAmOtherDist[j])*(hispOver18[j]/(popOver18[j] - blackOver18[j]))
    
    
    
    # here's where I am specifying an open seat, if the status varies, it would need to change here
    # I could determine the number of seats in each state, how many af am incumbents they have, allocate 
    # incumbents based on current representation, want to include some random open seat factor (roughly 10%)
    
    totalAfAmReps[1:simulationIterations] <- 0
    CD <- 1
    while(CD <= nReps2010[j]) 
    {
      if (afAmDistricts[j] >= CD)
      { 
        BVAP <- targetProportion[j]
        HVAP <- latinoVAPtargetDist[j] # need to fix this
      }
      if (afAmDistricts[j] < CD)
      { 
        BVAP <- afAmOtherDist[j]
        HVAP <- latinoVAPotherDist[j]
      }
      
      if (afAmReps2010[j] >= CD) 
      { AfAmInc <- 1
      whiteInc <- 0
      openSeat <- 0
      }
      if (afAmReps2010[j] < CD)  
      { AfAmInc <- 0
      whiteInc <- 1
      openSeat <- 0
      }
      
      openSeat <- rbinom(1, 1, seatOpensRandomly)
      if (openSeat == 1)
      { AfAmInc <- 0
      whiteInc <- 0
      openSeat <- 1
      }
      
      afAmMeanConstant <- commonIntercept + openSeatCoef*openSeat + whiteIncCoef*whiteInc + hvapCoef*HVAP
      
      afAmXB <-  (commonSlope + slopeChangeAfAm[j])*BVAP  + afAmMeanConstant + interceptChangeAfAm[j]
      probAfAmDist[CD] <- exp(afAmXB )/(1 + exp(afAmXB ))
      
      
      simulatedAfAmReps <- rbinom(simulationIterations, 1, probAfAmDist[CD])
      
      totalAfAmReps <- totalAfAmReps + simulatedAfAmReps 
      
      CD <- CD + 1
    }
    
    
    # 
    simulatedAfAmRepsTotal <- simulatedAfAmRepsTotal + totalAfAmReps 
    simulatedAfAmRepsSection5 <- simulatedAfAmRepsSection5  + (totalAfAmReps*section5[j])
    
    j <- j + 1
  }
  
  
  simulatedMean[i] <- mean(simulatedAfAmRepsTotal)
  simulatedSD[i] <- sd(simulatedAfAmRepsTotal)
  lower95quantile[i] <- quantile(simulatedAfAmRepsTotal,probs=0.025)
  upper95quantile[i] <- quantile(simulatedAfAmRepsTotal,probs=0.975)
  # totalExpectedAfAmReps[i] <- sum(totalAfAmReps)
  totalExpectedAfAmReps[i] <- mean(simulatedAfAmRepsTotal)
  totalExpectedAfAmRepsSection5only[i] <- mean(simulatedAfAmRepsSection5)
  # totalExpectedAfAmRepsNEonly[i] <- sum(totalAfAmReps*stateData$neRegion)
  
  i <- i + 1
  
}


#pnorm((45.021-48.79504)/ 2.698489)
#pnorm(1.96)

# comparing 46.8% and 50% standard
# at 47% mean = 73.7252, sd = 2.452772
# at 50% mean = 67.5198, sd = 1.765398
# simulatedMean[470]
# simulatedMean[500]
# simulatedSD[470]
# simulatedSD[500]
simAt47 <- rnorm(1000,simulatedMean[468],simulatedSD[468])
simAt50 <- rnorm(1000,simulatedMean[500],simulatedSD[500])
mean(simAt47 >= simAt50)
# how often 47% greater than nationwide proportionality
mean(simAt47 >= 0.1121725*435)
mean(simAt50 >= 0.1121725*435)

####################################################################### create the plot


par(mar=c(3.2,2.8,1.7,0.1), mgp=c(2,.6,.5), family = "serif")
par(xpd=FALSE)
plot(x="",y="",xlim=c(1,1000),ylim=c(0,70),xlab="Target African American VAP (%)",ylab="Expected Seats in Congress",
     axes=F,cex.lab=1.0,main="(a) African American Representation",cex.main=1.0)


abline(h= 0.1121725*435,lty=2,lwd=1)
abline(h=0.168954*95,lty=3,col="gray30")

# nationwide
polygon(x=c(seq(1,1000,by=1),seq(1000,1,by=-1)),y=c(totalExpectedAfAmReps-qnorm(.975)*simulatedSD,rev(totalExpectedAfAmReps+qnorm(.975)*simulatedSD)),col="gray90",border=F)
polygon(x=c(seq(1,1000,by=1),seq(1000,1,by=-1)),y=c(totalExpectedAfAmReps-qnorm(.95)*simulatedSD,rev(totalExpectedAfAmReps+qnorm(.95)*simulatedSD)),col="gray80",border=F)
lines(x=seq(1,1000,by=1),y=totalExpectedAfAmReps,lwd=1)



# section 5 states
lines(x=seq(1,1000,by=1),y=totalExpectedAfAmRepsSection5only,lwd=1,lty=1,col="gray30")



axis(side=1,seq(0,1000,by=100),labels=seq(0,100,by=10),cex.axis=.7,line=0)
axis(side=2,seq(0,80,by=10),las=2,cex.axis=.7,line=0)

box()

xAxis <- seq(1,1000,by=1)
xAxis[totalExpectedAfAmReps== max(totalExpectedAfAmReps)]

#### just the legend now

par(mar=c(3.2,0.1,0.1,0.1), mgp=c(2,.6,.5), family = "serif")

plot(x="",y="",xlim=c(1,1000),ylim=c(0,60),xlab="",ylab="",axes=F,main="")
par(xpd=TRUE)
legend(x=0, y=0, border=c(F,F,T,T,F,F), fill=c('white','white','gray80','gray90','white','white'),
       lty=c(1,2,0,0,1,3), lwd=c(1,1,0,0,1,1), ncol=3, cex=.7, col=c('black','black','white','white','gray30','gray30'),
       legend=c('Expected Seats Nationwide', 'National Proportionality','90% Confidence Interval','95% Confidence Interval','Expected Seats, Section 5 States','Section 5 State Proportionality'))




stateData <- read.csv("LatinoRepSimulationData.csv")
attach(stateData)

####################### based on analysis of 2002-2010 data
commonIntercept <-	-7.119 #	1.283	0.09538	-9.832	-7.046	-4.805	10001	10000
commonSlope <- 	21.12	# 2.964	0.2282	15.68	21.11	27.18	10001	10000
openSeatCoef <-	-1.338 #	0.7825	0.02959	-2.897	-1.357	0.2088	10001	10000
whiteIncCoef <-	-4.417 #	0.6563	0.02956	-5.67	-4.415	-3.148	10001	10000
bvapCoef <- 	-16.21 #	4.04	0.2175	-24.72	-15.95	-8.88	10001	10000

######### Latino rep calculations 
targetAfAm <- FALSE # setting at 50% rather than varying to match
seatOpensRandomly <- .1
probLatinoDist <- NULL

i <- 1
totalExpectedLatinoReps <- NULL
targetProportion <- NULL
minLatinoProportion <- NULL
minPopNeededPerDistrict <- NULL
latinoDistricts <- NULL
latinoRemainder <- NULL
latinoOtherDist <- NULL
latinoTargetXB <- NULL
probTargetLatinoDist <- NULL
expectLatinoRepsTarget <- NULL
remainderLatinoXB <- NULL
probRemainderLatinoDist <- NULL
expectLatinoRepsRemainder <- NULL
totalLatinoReps <- NULL
latinoMeanConstant <- NULL
totalExpectedLatinoRepsSection5only <- NULL
totalExpectedLatinoRepsNEonly <- NULL
blackVAPtargetDist <- NULL
blackVAPotherDist <- NULL
latinoVotersProportion <- NULL
otherVotersProportion <- NULL
latinoVotersNeeded <- NULL
latinoPopTargetDistrict <- NULL
targetDistOtherPopulation <- NULL
targetDistOtherVoters <- NULL
targetDistAfAmVoters <- NULL
minAfAmProportion <- NULL
blackVotersProportion <- NULL
blackVotersNeeded <- NULL
blackPopTargetDistrict <- NULL
afAmDistricts <- NULL
afAmRemainder <- NULL
afAmOtherDist <- NULL

simulatedLatinoRepsTarget <- NULL
simulatedLatinoRepsRemainder <- NULL
totalSimulated <- NULL
simulationIterations <- 5000  # set the number of iterations here

simulatedMean <- NULL
simulatedSD <- NULL

# i <- 600

while(i <= 1000) {
  
  simulatedLatinoRepsTotal <- rep(0,simulationIterations)
  simulatedLatinoRepsSection5 <- rep(0,simulationIterations)
  
  j <- 1
  while (j <= 50) {
    
    targetProportion[j] <- i/1000
    minLatinoProportion[j] <- hispOver18[j]/popOver18[j]
    if (targetProportion[j] < minLatinoProportion[j]) targetProportion[j] <- minLatinoProportion[j] 
    
    hispProportion <- hispOver18[j]/popOver18[j]
    
    geoDensityParameter <- 1 - (1/(1 - hispProportion))*(1/(1 - hispProportion))*(targetProportion[j] - hispProportion)*(targetProportion[j] - hispProportion)
    # 1 - targetProportion[j] + blackOver18[j]/popOver18[j]
    if (targetProportion[j] < hispProportion) { geoDensityParameter  <- 1 } 
    
    avgDistSize[j] <- totPop[j]/nReps2010[j]
    latinoVotersProportion[j] <- hispOver18[j]/hispanicPop[j]
    otherVotersProportion[j] <- (popOver18[j] - hispOver18[j]) / popOver18[j]
    
    latinoVotersNeeded[j] <- (avgDistSize[j]*otherVotersProportion[j]) / ( ((1 - targetProportion[j])/targetProportion[j]) + (1/latinoVotersProportion[j])*otherVotersProportion[j])
    latinoPopTargetDistrict[j] <- latinoVotersNeeded[j]*(1/latinoVotersProportion[j])
    
    latinoDistricts[j] <- floor(hispanicPop[j]*geoDensityParameter/latinoPopTargetDistrict[j])
    
    if (latinoDistricts[j] >= nReps2010[j])
    {
      latinoDistricts[j] <- nReps2010[j]
      latinoRemainder[j] <- 0
      latinoOtherDist[j] <- 0
      probRemainderLatinoDist[j] <- 0
    }
    
    if (latinoDistricts[j] < nReps2010[j])
    {
      latinoRemainder[j] <- hispOver18[j] - (latinoDistricts[j] * latinoVotersNeeded[j])
      latinoOtherDist[j] <- latinoRemainder[j] / ((nReps2010[j] - latinoDistricts[j])* avgDistSize[j])
    }
    
    targetDistOtherPopulation[j] <- avgDistSize[j] - latinoPopTargetDistrict[j] 
    targetDistOtherVoters[j] <- targetDistOtherPopulation[j]* otherVotersProportion[j]
    targetDistAfAmVoters[j] <- targetDistOtherVoters[j]*(blackOver18[j]/(popOver18[j] - hispOver18[j]))
    
    ###### next line assumes VRA for AfAm in place
    if (targetAfAm)
    {
      ###################
      
      minAfAmProportion[j] <- blackOver18[j]/popOver18[j]
      if (targetProportion[j] < minAfAmProportion[j]) targetProportion[j] <- minAfAmProportion[j] 
      
      avgDistSize[j] <- totPop[j]/nReps2010[j]
      blackVotersProportion[j] <- blackOver18[j]/blackPop[j]
      otherVotersProportion[j] <- (popOver18[j] - blackOver18[j]) / popOver18[j]
      
      blackVotersNeeded[j] <- (avgDistSize[j]*otherVotersProportion[j]) / ( ((1 - targetProportion[j])/targetProportion[j]) + (1/blackVotersProportion[j])*otherVotersProportion[j])
      blackPopTargetDistrict[j] <- blackVotersNeeded[j]*(1/blackVotersProportion[j])
      
      blackPopTargetDistrict[j] <- avgDistSize[j]*targetProportion[j] 
      
      afAmDistricts[j] <- floor(blackPop[j]/blackPopTargetDistrict[j])
      
      if (afAmDistricts[j] >= nReps2010[j])  # can reach target in all districts
      {
        afAmDistricts[j] <- nReps2010[j]
        afAmRemainder[j] <- 0
        afAmOtherDist[j] <- 0
      }
      if (afAmDistricts[j] < nReps2010[j])   # can reach target in less than all districts
      {
        afAmRemainder[j] <- blackOver18[j] - (afAmDistricts[j] * blackVotersNeeded[j])
        afAmOtherDist[j] <- afAmRemainder[j] / ((nReps2010[j] - afAmDistricts[j])* avgDistSize[j])
      }
      
      blackVAPtargetDist[j]  <- afAmOtherDist[j] # overriding this
      #  
      ####################
    }
    
    # I'm assuming 50% Af Am district standard
    targetDistAfAmVoters[j] <- targetDistOtherVoters[j]*bvapOther50VRA[j]
    
    blackVAPtargetDist[j] <- targetDistAfAmVoters[j] / (latinoVotersNeeded[j] + targetDistOtherVoters[j])
    blackVAPotherDist[j] <- (1 - latinoOtherDist[j])*(blackOver18[j]/(popOver18[j] - hispOver18[j]))
    
    # here's where I am specifying an open seat, if the status varies, it would need to change here
    # I could determine the number of seats in each state, how many af am incumbents they have, allocate 
    # incumbents based on current representation, want to include some random open seat factor (roughly 10%)
    
    totalLatinoReps[1:simulationIterations] <- 0
    CD <- 1
    while(CD <= nReps2010[j]) 
    {
      if (latinoDistricts[j] >= CD)
      { 
        HVAP <- targetProportion[j]
      }
      if (latinoDistricts[j] < CD)
      { 
        HVAP <- latinoOtherDist[j]
      }
      if (latinoDistricts[j] == 0)   # cannot reach target in any district
      {
        HVAP <- hispOver18[j] / popOver18[j]
      }
      
      if ((nReps2010[j] - afAm50dists[j]) >= CD)
      { 
        BVAP <- bvapOther50VRA[j]
      }
      
      if ((nReps2010[j] - afAm50dists[j]) < CD)
      { 
        BVAP <- .50
      }
      
      if (latinoReps2010[j] >= CD) 
      { latinoInc <- 1
      whiteInc <- 0
      openSeat <- 0
      }
      if (latinoReps2010[j] < CD)  
      { latinoInc <- 0
      whiteInc <- 1
      openSeat <- 0
      }
      
      openSeat <- rbinom(1, 1, seatOpensRandomly)
      if (openSeat == 1)
      { latinoInc <- 0
      whiteInc <- 0
      openSeat <- 1
      }
      
      latinoMeanConstant <- commonIntercept + openSeatCoef*openSeat + whiteIncCoef*whiteInc + bvapCoef*BVAP
      
      latinoXB <-  (commonSlope + slopeChangeLatino[j])*HVAP  + latinoMeanConstant + interceptChangeLatino[j]
      probLatinoDist[CD] <- exp(latinoXB )/(1 + exp(latinoXB ))
      
      
      simulatedLatinoReps <- rbinom(simulationIterations, 1, probLatinoDist[CD])
      
      totalLatinoReps <- totalLatinoReps + simulatedLatinoReps 
      
      CD <- CD + 1
    }
    
    
    # 
    simulatedLatinoRepsTotal <- simulatedLatinoRepsTotal + totalLatinoReps 
    simulatedLatinoRepsSection5 <- simulatedLatinoRepsSection5  + (totalLatinoReps*section5[j])
    
    j <- j + 1
  }
  
  simulatedMean[i] <- mean(simulatedLatinoRepsTotal)
  simulatedSD[i] <- sd(simulatedLatinoRepsTotal)
  
  totalExpectedLatinoReps[i] <- simulatedMean[i] # sum(totalLatinoReps)
  totalExpectedLatinoRepsSection5only[i] <- mean(simulatedLatinoRepsSection5) # sum(totalLatinoReps*stateData$section5)
  
  i <- i + 1
  
}

############################################## plot the results

par(mar=c(3.2,2.8,1.7,0.1), mgp=c(2,.6,.5), family = "serif")
par(xpd=FALSE)
xAxis <- seq(1,1000,by=1)

plot(x="",y="",xlim=c(1,1000),ylim=c(0,70),xlab="Target Latino VAP (%)",ylab="Expected Seats in Congress",
     axes=F,cex.lab=1.0,main="(b) Latino Representation",cex.main=1.0)

abline(h= 0.1630839*435,lty=2,lwd=1)
abline(h=0.2021793*95,lty=3,col="gray30")

polygon(x=c(seq(1,1000,by=1),seq(1000,1,by=-1)),y=c(totalExpectedLatinoReps-qnorm(.975)*simulatedSD,rev(totalExpectedLatinoReps+qnorm(.975)*simulatedSD)),col="gray90",border=F)
polygon(x=c(seq(1,1000,by=1),seq(1000,1,by=-1)),y=c(totalExpectedLatinoReps-qnorm(.95)*simulatedSD,rev(totalExpectedLatinoReps+qnorm(.95)*simulatedSD)),col="gray80",border=F)

lines(x=xAxis,y=totalExpectedLatinoReps,lwd=1)
lines(x=xAxis,y=totalExpectedLatinoRepsSection5only,lwd=1,lty=1,col="gray30")

axis(side=1,seq(0,1000,by=100),labels=seq(0,100,by=10),font=1,cex.axis=.7,line=0)
axis(side=2,seq(0,80,by=10),font=1,las=2,cex.axis=.7,line=0)

box()


#### some analysis of the results

xAxis[totalExpectedLatinoReps == max(totalExpectedLatinoReps)]
maxVRA <- xAxis[totalExpectedLatinoReps == max(totalExpectedLatinoReps)]

totalExpectedLatinoReps[maxVRA]
simulatedSD[maxVRA]

# no assumptiom about vra for afam
pnorm((72.431-70.9415)/2.096597)

# with 60% BVAP districts
pnorm((77.2034-70.9415)/3.091074)

################################ 



