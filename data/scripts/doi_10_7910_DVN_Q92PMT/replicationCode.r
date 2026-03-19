##==================================
## replicationCode.r
## Code to replicate the results reported in Rice, Douglas (2016) "Issue Divisions in U.S. Supreme Court Decision Making" Journal of Politics.
##
## dr 5.3.2016
##==================================

## ----------------- ##
## Front-End Matter  ##
## ----------------- ##

library(tm)
library(foreign)
library(slam)
library(wordcloud)
library(topicmodels)
library(stringr)
library(betareg)

load("dissents-100topics.Rdata")
load("dissents-25topics.Rdata")
load("regConcs-100topics.Rdata")
load("regConcs-25topics.Rdata")
load("speConcs-100topics.Rdata")
load("speConcs-25topics.Rdata")
load("majority-100topics.Rdata")
load("majority-25topics.Rdata")



## -------- ##
## Table 1  ##
## -------- ##

# Column 2: 25 Topic Herfindahl Index 
majDiffBeta25 <- betareg(herfDiffScaled ~ numDissents + minVotes + mqScore.var + logWC + early.salience.est + factor(author) + factor(chief), data=majDataBriefs25, link="loglog")

# Column 3: 100 Topic Herfindahl Index 
majDiffBeta100 <- betareg(herfDiffScaled ~ numDissents + minVotes + mqScore.var + logWC + early.salience.est + factor(author) + factor(chief), data=majDataBriefs100, link="loglog")

## -------- ##
## Figure 2 ##
## -------- ##

# Left Plot (25 Topics)
# plot predictions across values of dissents, and majVotes
newDf <- data.frame(minVotes=mean(majDataBriefs25$minVotes, na.rm=T), 
				logWC=mean(majDataBriefs25$logWC, na.rm=T),
				mqScore.var=mean(majDataBriefs25$mqScore.var, na.rm=T),
				early.salience.est=mean(majDataBriefs25$early.salience.est, na.rm=T),
				numDissents <- c(0:4),
				author="GINSBURG",
				chief="REHNQUIST")
plot(c(0:4),predict(majDiffBeta25, newDf), ylim=c(summary(majDataBriefs25$herfDiffScaled, na.rm=T)[2], summary(majDataBriefs25$herfDiffScaled, na.rm=T)[5]), type="o", xlab="Number of Dissenting Opinions", ylab="Predicted Difference in \n Topic Concentration", pch=19, cex=1.5) 
abline(h=mean(majDataBriefs25$herfDiffScaled, na.rm=T), lty=2, col="grey41")

# Right Plot (100 Topics)
newDf <- data.frame(minVotes=mean(majDataBriefs100$minVotes, na.rm=T), 
				logWC=mean(majDataBriefs100$logWC, na.rm=T),
				mqScore.var=mean(majDataBriefs100$mqScore.var, na.rm=T),
				early.salience.est=mean(majDataBriefs100$early.salience.est, na.rm=T),
				numDissents <- c(0:4),
				author="GINSBURG",
				chief="REHNQUIST")
plot(c(0:4),predict(majDiffBeta100, newDf), ylim=c(summary(majDataBriefs100$herfDiffScaled, na.rm=T)[2], summary(majDataBriefs100$herfDiffScaled, na.rm=T)[5]), type="o", xlab="Number of Dissenting Opinions", ylab="Predicted Difference in \n Topic Concentration", pch=19, cex=1.5) 
abline(h=mean(majDataBriefs100$herfDiffScaled, na.rm=T), lty=2, col="grey41")


## -------- ##
## Table 2  ##
## -------- ##

## Top: 25 Topic Dissimilarity Models 
dissSimBeta25 <- betareg(simDissToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=dissDataBriefs25, link="loglog")
scSimBeta25 <- betareg(simSCToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=speConcDataBriefs25, link="loglog")
rcSimBeta25 <- betareg(simRCToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=regConcDataBriefs25, link="loglog")

## Bottom: 100 Topic Dissimilarity Models 
dissSimBeta100 <- betareg(simDissToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=dissDataBriefs100, link="loglog")
scSimBeta100 <- betareg(simSCToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=speConcDataBriefs100, link="loglog")
rcSimBeta100 <- betareg(simRCToMaj ~ distanceFromOpMed + briefsHerf_mean + majVariance + majVotes + logWC + early.salience.est + factor(author) + factor(chief), data=regConcDataBriefs100, link="loglog")

## -------- ##
## Figure 3 ##
## -------- ##

# Left Plot (25 Topics)
# gen predicted values across opinion majority distance 
low <- min(dissDataBriefs25$majVariance, na.rm=T)
high <- max(dissDataBriefs25$majVariance, na.rm=T)
newData <- data.frame(briefsHerf_mean=mean(dissDataBriefs25$briefsHerf_mean, na.rm=T), majVariance = seq(low,high,by=.05), majVotes=5, distanceFromOpMed=mean(dissDataBriefs25$distanceFromOpMed, na.rm=T), early.salience.est = mean(dissDataBriefs25$early.salience.est, na.rm=T), logWC = mean(dissDataBriefs25$logWC), author="BRENNAN", chief="REHNQUIST")
yhats <- predict(dissSimBeta25, newdata = newData, type="response")
hline <- median(dissDataBriefs25$simDissToMaj[which(dissDataBriefs25$simDissToMaj<1)], na.rm=T)

# plot predicted values across variance
dev.new(width=4,height=5)
par(mar = c(5,5,2,1))
ymin = summary(dissDataBriefs25$simDissToMaj[which(dissDataBriefs25$simDissToMaj<1)])[2]
ymax = summary(dissDataBriefs25$simDissToMaj[which(dissDataBriefs25$simDissToMaj<1)])[5]
plot(newData$majVariance, yhats, ylim=c(ymin,ymax), pch="", ylab="Predicted Value", xlab="Ideological Variance\nOf Majority Coalition", main="", type="o", lwd=2, cex=1.5)
abline(h=hline, lty=2, col="grey41")

# Right Plot (100 Topics)
# gen predicted values across opinion majority distance 
low <- min(dissDataBriefs100$majVariance, na.rm=T)
high <- max(dissDataBriefs100$majVariance, na.rm=T)
newData <- data.frame(briefsHerf_mean=mean(dissDataBriefs100$briefsHerf_mean, na.rm=T), majVariance = seq(low,high,by=.05), majVotes=5, distanceFromOpMed=mean(dissDataBriefs100$distanceFromOpMed, na.rm=T), early.salience.est = mean(dissDataBriefs100$early.salience.est, na.rm=T), logWC = mean(dissDataBriefs100$logWC), author="BRENNAN", chief="REHNQUIST")
yhats <- predict(dissSimBeta100, newdata = newData, type="response")
hline <- median(dissDataBriefs100$simDissToMaj[which(dissDataBriefs100$simDissToMaj<1)], na.rm=T)

# plot predicted values across variance
dev.new(width=4,height=5)
par(mar = c(5,5,2,1))
ymin = summary(dissDataBriefs100$simDissToMaj[which(dissDataBriefs100$simDissToMaj<1)])[2]
ymax = summary(dissDataBriefs100$simDissToMaj[which(dissDataBriefs100$simDissToMaj<1)])[5]
plot(newData$majVariance, yhats, ylim=c(ymin,ymax), pch="", ylab="Predicted Value", xlab="Ideological Variance\nOf Majority Coalition", main="", type="o", lwd=2, cex=1.5)
abline(h=hline, lty=2, col="grey41")


	