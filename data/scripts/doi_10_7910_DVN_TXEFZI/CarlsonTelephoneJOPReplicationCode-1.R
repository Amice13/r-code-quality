# The Promise of Playing Telephone with Political Information 
# Taylor N. Carlson
# Replication Code

# This file includes the code used to create the variables to be used in the analysis for 
#"The Promise of Playing Telephone with Political Information"

# --- Read in Datasets --- #
rm(list=ls())
setwd("/Users/Taylor/Documents/Seminar Paper/JOP Submission/JOP Data Replication Code/")

# Unique dataset
du <- read.csv("telephone_unique.csv")
u <- read.csv("irrtestdata.csv") # for checking intercoder reliability

#----- Inter-coder Reliability for Units of Information -----#

# Test for correlation in units of information between coders 
cor.test(u$units.2, u$units.1)
#.863, p<.001

cor.test(du$round1units.1, du$round1units.2) #.88
cor.test(du$round2units.1, du$round2units.2) #.81
cor.test(du$round3units.1, du$round3units.2) #.88

# Calculate Krippendorf's Alpha
# Install IRR package http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/
#install.packages("irr")
library(irr)

# Krippendorf's alpha
kripp.alpha(t(u), method="ratio")
#.654

# Calculate ICC (Intraclass correlation coefficient)
icc(u, model="twoway", type="agreement")
# .85 (95% CI: [.81,.882])

# --- 1. The Amount of Information Transmitted Declines --- #

## Word count
t.test(du$words1, du$words2) #ns
t.test(du$words2, du$words3) #ns
t.test(du$words1, du$words3) #p<.05, CI=[0.797, 18.77]
  # mean round 1=52.9
  # mean round 2=47.3
  # mean round 3=43.1

### Original article to position 1
  52.9/1659

## Units of Information
t.test(du$round1units.avg, du$round2units.avg) #ns
t.test(du$round2units.avg, du$round3units.avg) #ns
t.test(du$round1units.avg, du$round3units.avg) # (p<.05)

### Original article to position 1
  mean(du$round1units.avg, na.rm=T)/228

#----- Code for Figure 2a and 2b -----#

#Units of information
unitmeans<-c(mean(du$round1units.avg, na.rm=T), mean(du$round2units.avg, na.rm=T), mean(du$round3units.avg, na.rm=T))

#Calculate 2x standard error of the mean
se951<-1.96*(sd(du$round1units.avg, na.rm=T)/sqrt(length(du$round1units.avg)))
se952<-1.96*(sd(du$round2units.avg, na.rm=T)/sqrt(length(du$round2units.avg)))
se953<-1.96*(sd(du$round3units.avg, na.rm=T)/sqrt(length(du$round3units.avg)))
#Make a vector with mean, mean+SE, mean-SE for each round
r195<-c(mean(du$round1units.avg, na.rm=T), mean(du$round1units.avg, na.rm=T)+se951, mean(du$round1units.avg, na.rm=T)-se951)
r295<-c(mean(du$round2units.avg, na.rm=T), mean(du$round2units.avg, na.rm=T)+se952, mean(du$round2units.avg, na.rm=T)-se952)
r395<-c(mean(du$round3units.avg, na.rm=T), mean(du$round3units.avg, na.rm=T)+se953, mean(du$round3units.avg, na.rm=T)-se953)
#Make table with upper bound CI as second row, lower bound as third row
setab<-cbind(r195, r295, r395)
h<-setab[2,] #defining high bound
l<-setab[3,] #defining low bound
x<-seq(1:ncol(setab))
par(mai=c(1,1,.8,.8))
barplot(unitmeans, main="Units of Information Communicated\nThroughout the Chain", 
        xlab="Position in the Chain", ylab="Mean Number of Units of Information", 
        col=c("seashell4", "seashell4", "seashell4"), names.arg=c("1", "2", "3"), ylim=c(0,8))
for(i in 1:ncol(setab)){
  lines(c(i*1.2-.5, i*1.2-.5), c(setab[2,i], setab[3,i]))
}
lines(c(0,4), c(0,0))

#Add a panel that shows the whole distribution in each round
plot(density(du$round2units.avg, na.rm=T), main="Distribution of Units of\nInformation in each Position", xlab="Units of Information", ylab="Density", lty=2, lwd=2, ylim=c(0,.15))
lines(density(du$round1units.avg, na.rm=T), lty=1, lwd=2)
lines(density(du$round3units.avg, na.rm=T), lty=3, lwd=2)
legend(7.75,.1, legend=c("Position 1", "Position 2", "Position 3"), lty=c(1,2,3), lwd=2, bty="n")

# --- The Quality and Precision of the Information Declines --- #

## Distortions at each stage
mean(du$dist.r1, na.rm=T) # 65.2
mean(du$dist.r2, na.rm=T) # 9.1
mean(du$dist.r3, na.rm=T) # 8.2

### Distortions a hypothetical person 4 is exposed to:
mean(du$dist.r1, na.rm=T) + mean(du$dist.r2, na.rm=T) + mean(du$dist.r3, na.rm=T) #=82.6
((mean(du$dist.r1, na.rm=T) + mean(du$dist.r2, na.rm=T) + mean(du$dist.r3, na.rm=T))-mean(du$dist.r1, na.rm=T))/mean(du$dist.r1, na.rm=T)
  # =.266


#----- Code for Figure 3 -----#

#Create vectors of the mean proportions in each position
numval.p<-c(mean(du$numval.r1p, na.rm=T), mean(du$numval.r2p, na.rm=T), mean(du$numval.r3p, na.rm=T))
qual.p<-c(mean(du$qual.r1p, na.rm=T), mean(du$qual.r2p, na.rm=T), mean(du$qual.r3p, na.rm=T))
gen.p<-c(mean(du$gen.r1p, na.rm=T), mean(du$gen.r2p, na.rm=T), mean(du$gen.r3p, na.rm=T))
new.p<-c(mean(du$new.r1p, na.rm=T), mean(du$new.r2p, na.rm=T), mean(du$new.r3p, na.rm=T))
wrong.p<-c(mean(du$wrong.r1p, na.rm=T), mean(du$wrong.r2p, na.rm=T), mean(du$wrong.r3p, na.rm=T))

#Plot it out
xaxis<-c(1:3)
par(mar=c(5,5,4,4)+.1)
plot(xaxis, new.p, type="b", col="black", main="Proportion of Each Type of\nInformation Distortion", xlab="Position in Chain", ylab="Proportion of Distortions\n(Number of Distortions of Type i/Total Distortions)", 
    ylim=c(0,1), pch=19, lwd=2, lty=1, xaxt="n")
lines(xaxis, qual.p, type="b", lty=1, col="seashell4", pch=19, lwd=2)
lines(xaxis, gen.p, type="b", lty=2, col="black", pch=19, lwd=2)
lines(xaxis, numval.p, type="b", lty=3, col="black", pch=19, lwd=2)
lines(xaxis, wrong.p, type="b", lty=4, col="black", pch=19, lwd=2)
axis(1, at=c(1,2,3), labels=xaxis)
legend(1,1.05, legend=c("New Info Introduced", "Qual Description of Quant", "Specific to General", "Numeric Value", "Wrong Info"), 
       col=c("black", "seashell4", "black", "black", "black"), 
       lty=c(1,1,2,3,4), pch=c(19,19), lwd=c(2,2), bty="n", cex=.75)


### --- Appendix Materials --- ###

#----- Descriptive Statistics for Appendix -----#

d2 <- read.csv("telephone_individualdiffs.csv")

# Create the table
demogtab <- matrix(NA, nrow=5, ncol=3)
demogtab[1,]<-c(prop.table(table(d2$dem[d2$position==1]))[2], 
                prop.table(table(d2$dem[d2$position==2]))[2], 
                prop.table(table(d2$dem[d2$position==3]))[2])
demogtab[2,]<-c(prop.table(table(d2$vote12[d2$position==1]))[2],
                prop.table(table(d2$vote12[d2$position==2]))[2],
                prop.table(table(d2$vote12[d2$position==3]))[2])
demogtab[3,]<-c(prop.table(table(d2$female[d2$position==1]))[2],
                prop.table(table(d2$female[d2$position==2]))[2],
                prop.table(table(d2$female[d2$position==3]))[2])
demogtab[4,]<-c(mean(d2$ideology.r3[d2$position==1]),
                mean(d2$ideology.r3[d2$position==2]), 
                mean(d2$ideology.r3[d2$position==3]))
demogtab[5,]<-c(mean(d2$interest[d2$position==1]), 
                mean(d2$interest[d2$position==2]),
                mean(d2$interest[d2$position==3]))
rownames(demogtab) <- c("Proportion Democrats", "Proportion Voting in 2012", "Proportion Female", "Mean Ideology (7pt scale)", "Mean Interest (4pt scale)")
colnames(demogtab) <- c("Position 1", "Position 2", "Position 3")
library(xtable)
xtable(demogtab)


## -- Show paper results for each coder separately -- ##


### -- Coder 1 -- ###

## Units of Information
t.test(du$round1units.1, du$round2units.1) #ns
t.test(du$round2units.1, du$round3units.1) #ns
t.test(du$round1units.1, du$round3units.1) # (p<.10)

### Original article to position 1
mean(du$round1units.1, na.rm=T)/228

#----- Code for Figure 2a and 2b -----#

#Units of information
unitmeans<-c(mean(du$round1units.1, na.rm=T), mean(du$round2units.1, na.rm=T), mean(du$round3units.1, na.rm=T))

#Calculate 2x standard error of the mean
se951<-1.96*(sd(du$round1units.1, na.rm=T)/sqrt(length(du$round1units.1)))
se952<-1.96*(sd(du$round2units.1, na.rm=T)/sqrt(length(du$round2units.1)))
se953<-1.96*(sd(du$round3units.1, na.rm=T)/sqrt(length(du$round3units.1)))
#Make a vector with mean, mean+SE, mean-SE for each round
r195<-c(mean(du$round1units.1, na.rm=T), mean(du$round1units.1, na.rm=T)+se951, mean(du$round1units.1, na.rm=T)-se951)
r295<-c(mean(du$round2units.1, na.rm=T), mean(du$round2units.1, na.rm=T)+se952, mean(du$round2units.1, na.rm=T)-se952)
r395<-c(mean(du$round3units.1, na.rm=T), mean(du$round3units.1, na.rm=T)+se953, mean(du$round3units.1, na.rm=T)-se953)
#Make table with upper bound CI as second row, lower bound as third row
setab<-cbind(r195, r295, r395)
h<-setab[2,] #defining high bound
l<-setab[3,] #defining low bound
x<-seq(1:ncol(setab))
par(mai=c(1,1,.8,.8))
barplot(unitmeans, main="Units of Information Communicated\nThroughout the Chain\nCoder 1", 
        xlab="Position in the Chain", ylab="Mean Number of Units of Information", 
        col=c("seashell4", "seashell4", "seashell4"), names.arg=c("1", "2", "3"), ylim=c(0,8))
for(i in 1:ncol(setab)){
  lines(c(i*1.2-.5, i*1.2-.5), c(setab[2,i], setab[3,i]))
}
lines(c(0,4), c(0,0))

#Add a panel that shows the whole distribution in each round
plot(density(du$round2units.1, na.rm=T), main="Distribution of Units of\nInformation in each Position\nCoder 1", xlab="Units of Information", ylab="Density", lty=2, lwd=2, ylim=c(0,.15))
lines(density(du$round1units.1, na.rm=T), lty=1, lwd=2)
lines(density(du$round3units.1, na.rm=T), lty=3, lwd=2)
legend(7.75,.1, legend=c("Position 1", "Position 2", "Position 3"), lty=c(1,2,3), lwd=2, bty="n")

# --- The Quality and Precision of the Information Declines --- #

## Distortions at each stage
mean(du$dist.r1.1, na.rm=T) # 76.5
mean(du$dist.r2.1, na.rm=T) # 10.3
mean(du$dist.r3.1, na.rm=T) # 9.7

### Distortions a hypothetical person 4 is exposed to:
mean(du$dist.r1.1, na.rm=T) + mean(du$dist.r2.1, na.rm=T) + mean(du$dist.r3.1, na.rm=T) #=96.6
((mean(du$dist.r1.1, na.rm=T) + mean(du$dist.r2.1, na.rm=T) + mean(du$dist.r3.1, na.rm=T))-mean(du$dist.r1.1, na.rm=T))/mean(du$dist.r1.1, na.rm=T)
# =.262


#----- Code for Figure 3 -----#

#Create vectors of the mean proportions in each position
numval.p<-c(mean(du$numval.r1p1, na.rm=T), mean(du$numval.r2p1, na.rm=T), mean(du$numval.r3p1, na.rm=T))
qual.p<-c(mean(du$qual.r1p1, na.rm=T), mean(du$qual.r2p1, na.rm=T), mean(du$qual.r3p1, na.rm=T))
gen.p<-c(mean(du$gen.r1p1, na.rm=T), mean(du$gen.r2p1, na.rm=T), mean(du$gen.r3p1, na.rm=T))
new.p<-c(mean(du$new.r1p1, na.rm=T), mean(du$new.r2p1, na.rm=T), mean(du$new.r3p1, na.rm=T))
wrong.p<-c(mean(du$wrong.r1p1, na.rm=T), mean(du$wrong.r2p1, na.rm=T), mean(du$wrong.r3p1, na.rm=T))

#Plot it out
xaxis<-c(1:3)
par(mar=c(5,5,4,4)+.1)
plot(xaxis, new.p, type="b", col="black", main="Proportion of Each Type of\nInformation Distortion\nCoder 1", xlab="Position in Chain", ylab="Proportion of Distortions\n(Number of Distortions of Type i/Total Distortions)", 
     ylim=c(0,1), pch=19, lwd=2, lty=1, xaxt="n")
lines(xaxis, qual.p, type="b", lty=1, col="seashell4", pch=19, lwd=2)
lines(xaxis, gen.p, type="b", lty=2, col="black", pch=19, lwd=2)
lines(xaxis, numval.p, type="b", lty=3, col="black", pch=19, lwd=2)
lines(xaxis, wrong.p, type="b", lty=4, col="black", pch=19, lwd=2)
axis(1, at=c(1,2,3), labels=xaxis)
legend(1,1.05, legend=c("New Info Introduced", "Qual Description of Quant", "Specific to General", "Numeric Value", "Wrong Info"), 
       col=c("black", "seashell4", "black", "black", "black"), 
       lty=c(1,1,2,3,4), pch=c(19,19), lwd=c(2,2), bty="n", cex=.75)


### -- Coder 2 -- ###

## Units of Information
t.test(du$round1units.2, du$round2units.2) #ns
t.test(du$round2units.2, du$round3units.2) #ns
t.test(du$round1units.2, du$round3units.2) # (p<.05)

### Original article to position 1
mean(du$round1units.2, na.rm=T)/228

#----- Code for Figure 2a and 2b -----#

#Units of information
unitmeans<-c(mean(du$round1units.2, na.rm=T), mean(du$round2units.2, na.rm=T), mean(du$round3units.2, na.rm=T))

#Calculate 2x standard error of the mean
se951<-1.96*(sd(du$round1units.2, na.rm=T)/sqrt(length(du$round1units.2)))
se952<-1.96*(sd(du$round2units.2, na.rm=T)/sqrt(length(du$round2units.2)))
se953<-1.96*(sd(du$round3units.2, na.rm=T)/sqrt(length(du$round3units.2)))
#Make a vector with mean, mean+SE, mean-SE for each round
r195<-c(mean(du$round1units.2, na.rm=T), mean(du$round1units.2, na.rm=T)+se951, mean(du$round1units.2, na.rm=T)-se951)
r295<-c(mean(du$round2units.2, na.rm=T), mean(du$round2units.2, na.rm=T)+se952, mean(du$round2units.2, na.rm=T)-se952)
r395<-c(mean(du$round3units.2, na.rm=T), mean(du$round3units.2, na.rm=T)+se953, mean(du$round3units.2, na.rm=T)-se953)
#Make table with upper bound CI as second row, lower bound as third row
setab<-cbind(r195, r295, r395)
h<-setab[2,] #defining high bound
l<-setab[3,] #defining low bound
x<-seq(1:ncol(setab))
par(mai=c(1,1,.8,.8))
barplot(unitmeans, main="Units of Information Communicated\nThroughout the Chain\nCoder 2", 
        xlab="Position in the Chain", ylab="Mean Number of Units of Information", 
        col=c("seashell4", "seashell4", "seashell4"), names.arg=c("1", "2", "3"), ylim=c(0,8))
for(i in 1:ncol(setab)){
  lines(c(i*1.2-.5, i*1.2-.5), c(setab[2,i], setab[3,i]))
}
lines(c(0,4), c(0,0))

#Add a panel that shows the whole distribution in each round
plot(density(du$round2units.2, na.rm=T), main="Distribution of Units of\nInformation in each Position\nCoder 2", xlab="Units of Information", ylab="Density", lty=2, lwd=2, ylim=c(0,.15))
lines(density(du$round1units.2, na.rm=T), lty=1, lwd=2)
lines(density(du$round3units.2, na.rm=T), lty=3, lwd=2)
legend(7.75,.1, legend=c("Position 1", "Position 2", "Position 3"), lty=c(1,2,3), lwd=2, bty="n")

# --- The Quality and Precision of the Information Declines --- #

## Distortions at each stage
mean(du$dist.r1.2, na.rm=T) # 53.9
mean(du$dist.r2.2, na.rm=T) # 7.96
mean(du$dist.r3.2, na.rm=T) # 6.65

### Distortions a hypothetical person 4 is exposed to:
mean(du$dist.r1.2, na.rm=T) + mean(du$dist.r2.2, na.rm=T) + mean(du$dist.r3.2, na.rm=T) #=68.5
((mean(du$dist.r1.2, na.rm=T) + mean(du$dist.r2.2, na.rm=T) + mean(du$dist.r3.2, na.rm=T))-mean(du$dist.r1.2, na.rm=T))/mean(du$dist.r1.2, na.rm=T)
# =.271


#----- Code for Figure 3 -----#

#Create vectors of the mean proportions in each position
numval.p<-c(mean(du$numval.r1p2, na.rm=T), mean(du$numval.r2p2, na.rm=T), mean(du$numval.r3p2, na.rm=T))
qual.p<-c(mean(du$qual.r1p2, na.rm=T), mean(du$qual.r2p2, na.rm=T), mean(du$qual.r3p2, na.rm=T))
gen.p<-c(mean(du$gen.r1p2, na.rm=T), mean(du$gen.r2p2, na.rm=T), mean(du$gen.r3p2, na.rm=T))
new.p<-c(mean(du$new.r1p2, na.rm=T), mean(du$new.r2p2, na.rm=T), mean(du$new.r3p2, na.rm=T))
wrong.p<-c(mean(du$wrong.r1p2, na.rm=T), mean(du$wrong.r2p2, na.rm=T), mean(du$wrong.r3p2, na.rm=T))

#Plot it out
xaxis<-c(1:3)
par(mar=c(5,5,4,4)+.1)
plot(xaxis, new.p, type="b", col="black", main="Proportion of Each Type of\nInformation Distortion\nCoder 2", xlab="Position in Chain", ylab="Proportion of Distortions\n(Number of Distortions of Type i/Total Distortions)", 
     ylim=c(0,1), pch=19, lwd=2, lty=1, xaxt="n")
lines(xaxis, qual.p, type="b", lty=1, col="seashell4", pch=19, lwd=2)
lines(xaxis, gen.p, type="b", lty=2, col="black", pch=19, lwd=2)
lines(xaxis, numval.p, type="b", lty=3, col="black", pch=19, lwd=2)
lines(xaxis, wrong.p, type="b", lty=4, col="black", pch=19, lwd=2)
axis(1, at=c(1,2,3), labels=xaxis)
legend(1,1.05, legend=c("New Info Introduced", "Qual Description of Quant", "Specific to General", "Numeric Value", "Wrong Info"), 
       col=c("black", "seashell4", "black", "black", "black"), 
       lty=c(1,1,2,3,4), pch=c(19,19), lwd=c(2,2), bty="n", cex=.75)

### -- Including Independents -- ###

# -- Check comparisons of each stage separately, including independents -- #

r1 <- read.csv("/Users/Taylor/Documents/Seminar Paper/JOP Submission/JOP Data Replication Code/JOP Replication Code to Upload/round1_wordcounts_withindependents.csv")
r2 <- read.csv("/Users/Taylor/Documents/Seminar Paper/JOP Submission/JOP Data Replication Code/JOP Replication Code to Upload/round2_wordcounts_withindependents.csv")
r3 <- read.csv("/Users/Taylor/Documents/Seminar Paper/JOP Submission/JOP Data Replication Code/JOP Replication Code to Upload/round3_wordcounts_withindependents.csv")

# Create a figure similar to that for Figure 2a and 2b

#Units of information
unitmeans<-c(mean(r1$words1, na.rm=T), mean(r2$words2, na.rm=T), mean(r3$words3, na.rm=T))

#Calculate 2x standard error of the mean
se951<-1.96*(sd(r1$words1, na.rm=T)/sqrt(length(r1$words1)))
se952<-1.96*(sd(r2$words2, na.rm=T)/sqrt(length(r2$words2)))
se953<-1.96*(sd(r3$words3, na.rm=T)/sqrt(length(r3$words3)))
#Make a vector with mean, mean+SE, mean-SE for each round
r195<-c(mean(r1$words1, na.rm=T), mean(r1$words1, na.rm=T)+se951, mean(r1$words1, na.rm=T)-se951)
r295<-c(mean(r2$words2, na.rm=T), mean(r2$words2, na.rm=T)+se952, mean(r2$words2, na.rm=T)-se952)
r395<-c(mean(r3$words3, na.rm=T), mean(r3$words3, na.rm=T)+se953, mean(r3$words3, na.rm=T)-se953)
#Make table with upper bound CI as second row, lower bound as third row
setab<-cbind(r195, r295, r395)
h<-setab[2,] #defining high bound
l<-setab[3,] #defining low bound
x<-seq(1:ncol(setab))
par(mai=c(1,1,.8,.8))
barplot(unitmeans, main="Number of Words Communicated\nThroughout the Chain\nWith Independents", 
        xlab="Position in the Chain", 
        ylab="Mean Number of Words", 
        col=c("seashell4", "seashell4", "seashell4"), names.arg=c("1", "2", "3"), ylim=c(0,70))
for(i in 1:ncol(setab)){
  lines(c(i*1.2-.5, i*1.2-.5), c(setab[2,i], setab[3,i]))
}
lines(c(0,4), c(0,0))

#Add a panel that shows the whole distribution in each round
plot(density(r2$words2), main="Distribution of Number of\nWords in each Position\nWith Independents", xlab="Number of Words", ylab="Density", lty=2, lwd=2)
lines(density(r1$words1), lty=1, lwd=2)
lines(density(r3$words3, na.rm=T), lty=3, lwd=2)
legend(100,.01, legend=c("Position 1", "Position 2", "Position 3"), lty=c(1,2,3), lwd=2, bty="n", cex=.75)

# t-tests on word count between each position 
t.test(r1$words1, r2$words2) #ns
t.test(r2$words2, r3$words3) #p<.10
t.test(r1$words1, r3$words3) #p<.05



