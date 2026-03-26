#
#  Barry C. Edwards
#  Replication file for "Formulating Voting Rights Act Remedies to Address Current Conditions"
#
#  Set-up for replication
#
rm(list=ls(all=TRUE))
library(foreign)
setwd(choose.dir())    # choose directory/folder with replication materials
data.stata <- read.dta("edwardsDataVotingRights.dta")


################## Figure 1: Exemplar vote-to-seat curves

vote_to_seats <- function(V, p, D=435, B) {
  Vpart <- V/(1-V)
  solve <- exp(p*log(Vpart))
  S <- solve/(1+solve)
  s <- D*solve/(1+solve)
  S <- 1/(1 + exp(-log(B) - p*log(Vpart)))
  return(S)
  #### variables defined by King & Browning (1987, 1252)
  # v  = number of votes for Democratic party candidates
  # T  = total number of votes for both parties
  # vr = T - v = number of votes for Republican candidates
  # V  = v/T = proportion votes for Dem candidates
  # Vr = 1 - V = proportion votes for Repub candidates
  # s  = number of seats allocated to Dem 
  # sr = number of seats allocated to Repub
  # D  = total number of single member legis districts
  # S  = s/D = proportion seats allocated to Dem candidates  
  # Sr = 1 - S = proportion seats allocated to Repub candidates  
  # p  = responsiveness variables, to be estimated
  # B  = bias parameter
}

par(mfrow=c(1,3), mar=c(3.6,3.1,3.1,0.6), mgp=c(2,.6,.5), family="serif")
V <- seq(0, 1, by=.01) # x-axis value series for all three plots

plot(x="", y="", axes=F, xlim=c(0,1), ylim=c(0,1), xlab="Proportion of Votes", 
     ylab="Proportion of Seats", main="(a) Proportionate Representation",
     cex.lab=1.25, cex.main=1.25, font=2, asp=1)
S <- vote_to_seats(V=V, p=1, B=1)
lines(x=V, y=S)
points(x=c(.5, 0, 1), y=c(.5, 0, 1), pch=3) # reference points
axis(side=1, at=seq(0,1,by=.1), font=2, cex.axis=.8, line=0)
axis(side=2, at=seq(0,1,by=.1), font=2, las=2, cex.axis=.8)
box()

plot(x="", y="", axes=F, xlim=c(0,1), ylim=c(0,1), xlab="Proportion of Votes", ylab="",
     main="(b) Extremely Majoritarian", cex.lab=1.25, cex.main=1.25, font=2, asp=1)
S <- vote_to_seats(V=V, p=8, B=1)
lines(x=V, y=S)
points(x=c(.5, 0, 1), y=c(.5, 0, 1), pch=3) # reference points
axis(side=1, at=seq(0,1,by=.1), font=2, cex.axis=.8, line=0)
axis(side=2, at=seq(0,1,by=.1), las=2, font=2, cex.axis=.8, line=0)
box()
segments(0,0,1,1,lty=3,col="gray50")

plot(x="", y="", axes=F, xlim=c(0,1), ylim=c(0,1), xlab="Proportion of Votes", ylab="",
     main="(c) Majoritarian and Biased", cex.lab=1.25, cex.main=1.25, font=2, asp=1)
S <- vote_to_seats(V=V, p=2, B=.5)
lines(x=V, y=S)
points(x=c(.5, 0, 1), y=c(.5, 0, 1), pch=3) # reference points
axis(side=1, at=seq(0,1,by=.1), font=2, cex.axis=.8, line=0, family="serif")
axis(side=2, at=seq(0,1,by=.1), font=2, las=2, cex.axis=.8, line=0, family="serif")
box()
segments(0,0,1,1,lty=3,col="gray")






# R Code for Figure 2: Correlation of minority voter success and district composition, 
#                      2002-2010 congressional elections

recentElections <- subset(data.stata,electionYear>=2002)
recentElectionsB <- recentElections[order(+recentElections$blackVAP),]
resultAfAmObs <- recentElectionsB$afAmVoterSuccess
bvapObs <- recentElectionsB$blackVAP
recentElectionsH <- recentElections[order(+recentElections$hispanicVAP),]
resultLatinoObs <- recentElectionsH$latinoVoterSuccess
hvapObs <- recentElectionsH$hispanicVAP

interval <- 25
n_points <- round(length(resultAfAmObs) / interval)
afamploty <- rep(NA, n_points)
afamplotx <- rep(NA, n_points)
latinoploty <- rep(NA, n_points)
latinoplotx <- rep(NA, n_points)

for(i in 1:n_points)
{
  afamploty[i] <- mean(resultAfAmObs[((i-1)*interval+1):(i*interval)])
  afamplotx[i] <- mean(bvapObs[((i-1)*interval+1):(i*interval)])
  latinoploty[i] <- mean(resultLatinoObs[((i-1)*interval+1):(i*interval)])
  latinoplotx[i] <- mean(hvapObs[((i-1)*interval+1):(i*interval)])
  }

par(mfrow=c(1,2), mar=c(3.6,3.6,3.1,1.1), mgp=c(2,.6,.5), family = "serif")

plot(x="", y="", xlim=c(0,1), xlab="Proportion of Voting Age Population",
     ylab="Success Rate", ylim=c(0,1), main="(a) African American Voters",
     axes=F, cex.lab=1.0, cex.main=1.0, font=2, asp=1)
segments(0, 0, 1, 1, col="gray50", lty=3) # reference line
lines(x=afamplotx, y=afamploty, lwd=1)
points(x=afamplotx, y=afamploty, pch=16, cex=.8)
axis(side=1, at=seq(0,1,by=.1), font=2, cex.axis=.7, line=0)
axis(side=2, at=seq(0,1,by=.1), font=2, las=2, cex.axis=.7, line=0)
box()
legend(x=.50, y=.1, pch=c(16), lwd=c(1), cex=.65,  
       legend=c('Mean of 25 Districts'))

plot(x="", y="", xlim=c(0,1), xlab="Proportion of Voting Age Population",
     ylab="Success Rate", ylim=c(0,1), main="(b) Latino Voters", axes=F,
     cex.lab=1.0, cex.main=1.0, font=2, asp=1)
segments(0, 0, 1, 1, col="gray50", lty=3) # reference line
lines(x=latinoplotx, y=latinoploty, lwd=1)
points(x=latinoplotx, y=latinoploty, pch=16, cex=.8)
axis(side=1, at=seq(0,1,by=.1), font=2, cex.axis=.7, line=0)
axis(side=2, at=seq(0,1,by=.1), font=2, las=2, cex.axis=.7, line=0)
box()
legend(x=.50, y=.1, pch=c(16), lwd=c(1),  cex=.65,
       legend=c('Mean of 25 Districts'))






# Figure 3: Changing effect of minority voter populations on electoral success 
#           in elections without White incumbent running for reelection

################## curves over time for African American representation
# note hispanicVAP not avail for terms 87-92

thisYear <- minYear <- 93
maxYear <- 112

coefs_table_afam   <- data.frame(Intercept=NA, blackVAP=NA, hispanicVAP=NA, whiteIncumbent=NA, openSeat=NA)
coefs_table_latino <- data.frame(Intercept=NA, hispanicVAP=NA, blackVAP=NA, whiteIncumbent=NA, openSeat=NA)
variable_means_table <- data.frame(bvapMean=NA, hvapMean=NA, section5stateMean=NA, medFamIncomeMean=NA, demVoteMean=NA, 
                                   whiteIncumbentMean=NA, northeastRegionMean=NA, openSeatMean=NA)

# needs to run from thisYear 93 to 112
for(i in 1:20) {
  thisYear <- i + 92
  oneYearData <- subset(data.stata, data.stata$cong==thisYear)
  
  variable_means_table[i, ] <- c(mean(na.omit(oneYearData$blackVAP)), mean(na.omit(oneYearData$hispanicVAP)),
                                 mean(na.omit(oneYearData$section5state)), mean(na.omit(oneYearData$medianFamilyIncome)),
                                 mean(na.omit(oneYearData$demSharePresVote)), mean(na.omit(oneYearData$whiteIncumbent)),
                                 mean(na.omit(oneYearData$northeastRegion)), mean(na.omit(oneYearData$openSeat)))
  
  oneYearModel <- glm(repIsBlack ~ blackVAP + hispanicVAP + whiteIncumbent + openSeat,
                      data = oneYearData, family=binomial(link = "logit"))
  coefs_table_afam[i, ] <- oneYearModel$coefficients
  
  oneYearModel <- glm(repIsLatino ~ hispanicVAP + blackVAP + whiteIncumbent + openSeat, 
                      data = oneYearData, family=binomial(link = "logit"))
  coefs_table_latino[i, ] <- oneYearModel$coefficients
}


# ### kludge here, using open seat coef for 1970s for two cycles where no open seat Af Am wins
coefs_table_afam[2, "openSeat"] <- -1.87 # for term 94
coefs_table_afam[3, "openSeat"] <- -1.87 # for term 95
afamYearMeanConstant <- (coefs_table_afam$hispanicVAP*variable_means_table$hvapMean + coefs_table_afam$whiteIncumbent*0 
                        + coefs_table_afam$openSeat*1 + coefs_table_afam$Intercept)

# latino data not available for first three of the cycles
# also kludge to fill in open seat coef that can't be calculated for single cycle
coefs_table_latino[1:3, ] <- NA
coefs_table_latino[8, "openSeat"] <- -1.30 # for term 100
coefs_table_latino[10, "openSeat"] <- -1.30 # for term 102
coefs_table_latino[12, "openSeat"] <- -1.86 # for term 104
coefs_table_latino[15, "openSeat"] <- -1.86 # for term 107

latinoYearMeanConstant <- (coefs_table_latino$blackVAP*variable_means_table$bvapMean + coefs_table_latino$whiteIncumbent*0 
                          + coefs_table_latino$openSeat*1 + coefs_table_latino$Intercept)

# when does xB = 0, that's when there's a 50% chance of minorityRepresentative = 1


afamYearMeanConstant[93:112] <- afamYearMeanConstant
afamLogitCoefficient <- NULL
afamLogitCoefficient[93:112] <- coefs_table_afam$blackVAP

i <- 112
afAmProbabilities <- NULL

while (i >= 93) {
  
  genPercent <- seq(0,1,by=.05)
  xB <- genPercent*afamLogitCoefficient[i] + afamYearMeanConstant[i]
  checkXB <- 1
  while (checkXB <= length(xB))
  {
    if (xB[checkXB] > 500) xB[checkXB] <- 500
    checkXB <- checkXB + 1
  }
  probAfAmElected <- exp(xB) / (1 + exp(xB))
  
  afAmProbabilities <- c(afAmProbabilities,probAfAmElected)
  i <- i - 1
}




par(mfrow=c(1,2))
par(mar=c(3.6,3.6,1.1,1.1), mgp=c(2,.6,.5), family = "serif")


#create a surface plot
#phi +view above or -view from below 
#theta turn +left or -right
#r how close to plot
persp(y = seq(1972,2010, by=2), x = seq(0,1,by=.05), 
      z = array(rev(afAmProbabilities), dim=c(21,20)), ylim=c(1972,2010), xlim=c(0,1), zlim = c(0,1), expand = .45, 
      axes = T, ticktype="detailed", ylab = "Election Year      ", xlab = "District Proportion African American", zlab = "Probability", 
      theta = +65, phi = 30, r = 5, col = "white", ltheta = -90, lphi = 5, shade = .3, border = "gray20")


###################################### LATINO CALCULATIONS


latinoYearMeanConstant[93:112] <- latinoYearMeanConstant
latinoLogitCoefficient <- NULL
latinoLogitCoefficient[93:112] <- coefs_table_latino$hispanicVAP

i <- 112
latinoProbabilities <- NULL

while (i >= 96) {
  genPercent <- seq(0,1,by=.05)
  xB <- genPercent*latinoLogitCoefficient[i] + latinoYearMeanConstant[i]
  checkXB <- 1
  while (checkXB <= length(xB))
  {
    if (xB[checkXB] > 500) xB[checkXB] <- 500
    checkXB <- checkXB + 1
  }
  probLatinoElected <- exp(xB) / (1 + exp(xB))
  latinoProbabilities <- c(latinoProbabilities ,probLatinoElected )
  i <- i - 1
}

#create a surface plot
#phi +view above or -view from below 
#theta turn +left or -right
#r how close to plot
persp(y = seq(1978,2010, by=2),x = seq(0,1,by=.05), z = array(rev(latinoProbabilities),dim=c(21,17)), ylim=c(1978,2010), xlim=c(0,1), zlim = c(0,1), expand = .45, 
      axes = T, ticktype="detailed", ylab = "Election Year", xlab = "District Proportion Latino", zlab = "Probability", 
      theta = +65, phi = 30, r = 5, col = "white", ltheta = -90, lphi = 5, shade = .3, border = "gray20")




#  Code to Plot Parity Conditions Over Time
#  Figure 3 in paper is generated from two different scripts, this is the bottom half
spacing <- .06
par(mfrow=c(1,2), mar=c(3.1,3.1,1.1,1.1), mgp=c(2,.6,.5), family = "serif")

################# what is break even percent?
minYear <- 93
maxYear <- 112
magicNumber <- NULL
findParity <- NULL
bvapParity <- NULL
# 0 = afamLogitCoefficient *magicNumber + afamYearMeanConstant
magicNumber <- -1*afamYearMeanConstant/afamLogitCoefficient 

counter <- 93
while (counter <= 112)
{
  bvapRange <- seq(0,1,by=.001)
  XB  <- afamLogitCoefficient[counter]*bvapRange + afamYearMeanConstant[counter]
  probability <- exp(XB) / (1 + exp(XB))
  findParity <- abs(probability[50:950] - bvapRange[50:950])
  bvapParity[counter] <- bvapRange[findParity==min(findParity)] + .05
  counter <- counter + 1
}

year <- seq(1,maxYear,by=1)
plot(x="",y="",xlim=c(minYear,maxYear),xlab="",ylab="African American VAP (%)",ylim=c(0,80),
     main="",axes=F,cex.lab=1.0,cex.main=1.0 )
points(x=year,y=magicNumber*100,pch=1)
lines(x=year,y=magicNumber*100,lty=2)
points(x=year,y=bvapParity*100,pch=16)
lines(x=year,y=bvapParity*100,lty=1)
axis(2,las=2, line=0)
axis(1, at=seq(minYear,maxYear,by=1), labels=seq(1972,2010,by=2),las=2, line=0)
box()
rect(97.5-spacing,-20,97.5+spacing,200,col="white",border=T)
rect(102.5-spacing,-20,102.5+spacing,200,col="white",border=T)
rect(107.5-spacing,-20,107.5+spacing,200,col="white",border=T)
legend(x=107.5, y=10, lwd=c(1,1),  col=c("black","black"), lty=c(1,2), pch=c(16,1),bty="n",
       legend=c('Parity','50% Chance'), cex=.6)



spacing <- .05
minYear <- 96
maxYear <- 112
latinoMagicNumber <- NULL
latinoMagicNumber <- -1*latinoYearMeanConstant/latinoLogitCoefficient
findParity <- NULL
hvapParity <- NULL

counter <- 96
while (counter <= 112)
{
  hvapRange <- seq(0,1,by=.001)
  XB  <- latinoLogitCoefficient[counter]*hvapRange + latinoYearMeanConstant[counter]
  probability <- exp(XB) / (1 + exp(XB))
  
  findParity <- abs(probability[50:950] - hvapRange[50:950])
  hvapParity[counter] <- hvapRange[findParity==min(findParity)] + .05
  counter <- counter + 1
}

year <- seq(1,maxYear,by=1)
plot(x="",y="",xlim=c(minYear,maxYear),xlab="",ylab="Latino VAP (%)",ylim=c(0,80),main="",axes=F,cex.lab=1.0,cex.main=1.0 )
points(x=year,y=latinoMagicNumber*100,pch=1)
lines(x=year,y=latinoMagicNumber*100,lty=2)
points(x=year,y=hvapParity*100,pch=16)
lines(x=year,y=hvapParity*100)
# segments(year,seLower,year,seUpper,lty=2)
# abline(h=0,lty=3)
axis(2,las=2, line=0)
axis(1, at=seq(minYear,maxYear,by=1), labels=seq(1978,2010,by=2),las=2, line=0)
# abline(h=50,lty=2)
# indicate redistricting points
box()
# rect(92.5-spacing,-20,92.5+spacing,200,col="white",border=T)
rect(97.5-spacing,-20,97.5+spacing,200,col="white",border=T)
rect(102.5-spacing,-20,102.5+spacing,200,col="white",border=T)
rect(107.5-spacing,-20,107.5+spacing,200,col="white",border=T)
legend(x=108, y=10, lwd=c(1,1),  col=c("black","black"), lty=c(1,2), pch=c(16,1),bty="n",
       legend=c('Parity','50% Chance'), cex=.6)





