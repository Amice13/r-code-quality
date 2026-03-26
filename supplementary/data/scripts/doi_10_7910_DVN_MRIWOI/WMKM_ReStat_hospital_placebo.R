##################################################
##################################################
#This code reproduces Appendix Figures 10-17   ###
#for Wherry, Miller, Kaestner and Meyer REStat ###
#Contact: Sarah Miller (mille@umich.edu).      ###
##################################################
##################################################


setwd(".")

library(rdrobust)

#######Inpatient 2009: Appendix Figures 10 and 11

allblack <- read.csv("totalblack2009.csv")
allblack <- allblack[order(allblack$byear, allblack$bmonth),]
allblack$lognp <- log(allblack$count-allblack$preg)
allblack$logchron <- log(allblack$HCUPChronic)
allblack$lognonchron <- log(allblack$count-allblack$preg-allblack$HCUPChronic)

count <- 97
count2 <- 73
count3 <- 49

global4yr <- data.frame(1,1,1,1,1,1,1)
global3yr <- data.frame(1,1,1,1,1,1,1)
global2yr <- data.frame(1,1,1,1,1,1,1)
LLRIK <- data.frame(1,1,1,1,1,1)
LLRCCT <- data.frame(1,1,1,1,1,1)

cglobal4yr <- data.frame(1,1,1,1,1,1,1)
cglobal3yr <- data.frame(1,1,1,1,1,1,1)
cglobal2yr <- data.frame(1,1,1,1,1,1,1)
cLLRIK <- data.frame(1,1,1,1,1,1)
cLLRCCT <- data.frame(1,1,1,1,1,1)

allblack <- subset(allblack, byear > 1964)
allblack$marker <- seq(1:length(allblack[,1]))

for(i in c(0:177)){
counter <- count+i
counter2 <- count2+i
counter3 <- count3+i

datasetb <- subset(allblack, marker < counter & marker > i)
datasetb$time <- seq(1:96)-48
datasetb$time2 <- datasetb$time*datasetb$time
datasetb$after <- ifelse(datasetb$time > -1,1,0)

dataset3yr <- subset(allblack, marker < counter2 & marker > i)
dataset3yr$time <- seq(1:72)-36
dataset3yr$time2 <- dataset3yr$time*dataset3yr$time
dataset3yr$after <- ifelse(dataset3yr$time > -1,1,0)

dataset2yr <- subset(allblack, marker < counter3 & marker > i)
dataset2yr$time <- seq(1:48)-24
dataset2yr$time2 <- dataset2yr$time*dataset2yr$time
dataset2yr$after <- ifelse(dataset2yr$time > -1,1,0)

lastmonth <- datasetb[length(datasetb[,1]),]$bmonth
lastmonth2 <- dataset3yr[length(dataset3yr[,1]),]$bmonth
lastmonth3 <- dataset2yr[length(dataset2yr[,1]),]$bmonth

lastyear <- datasetb[length(datasetb[,1]),]$byear
lastyear2 <- dataset3yr[length(dataset3yr[,1]),]$byear
lastyear3 <- dataset2yr[length(dataset2yr[,1]),]$byear


global1 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=datasetb)
global2 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
global3 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid1 <- lm(lognp~as.factor(bmonth), data=datasetb)
llr1b <- rdrobust(resid1$residuals, datasetb$time, bwselect="IK")
llr2b <- rdrobust(resid1$residuals, datasetb$time, bwselect="CCT")

cglobal1 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=datasetb)
cglobal2 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
cglobal3 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid2 <- lm(logchron~as.factor(bmonth), data=datasetb)
cllr1b <- rdrobust(resid2$residuals, datasetb$time, bwselect="IK")
cllr2b <- rdrobust(resid2$residuals, datasetb$time, bwselect="CCT")

global4yr <- rbind(global4yr, c(summary(global1)$coefficients[2,], counter, lastmonth, lastyear))
global3yr <- rbind(global3yr, c(summary(global2)$coefficients[2,], counter, lastmonth2, lastyear2))
global2yr <- rbind(global2yr, c(summary(global3)$coefficients[2,], counter, lastmonth3, lastyear3))
LLRIK <- rbind(LLRIK, c(llr1b$coef, counter, lastmonth, lastyear))
LLRCCT <- rbind(LLRCCT, c(llr2b$coef, counter, lastmonth, lastyear))

cglobal4yr <- rbind(cglobal4yr, c(summary(cglobal1)$coefficients[2,], counter, lastmonth, lastyear))
cglobal3yr <- rbind(cglobal3yr, c(summary(cglobal2)$coefficients[2,], counter, lastmonth2, lastyear2))
cglobal2yr <- rbind(cglobal2yr, c(summary(cglobal3)$coefficients[2,], counter, lastmonth3, lastyear3))
cLLRIK <- rbind(cLLRIK, c(cllr1b$coef, counter, lastmonth, lastyear))
cLLRCCT <- rbind(cLLRCCT, c(cllr1b$coef, counter, lastmonth, lastyear))

}


global4yr <- global4yr[2:length(global4yr[,1]),]
global4yr <- subset(global4yr, X1.6 < 1984)
global4yr$del <- ifelse(global4yr$X1.6==1983 & global4yr$X1.5 > 9,1,0)
global4yr <- subset(global4yr, del==0)

cglobal4yr <- cglobal4yr[2:length(cglobal4yr[,1]),]
cglobal4yr <- subset(cglobal4yr, X1.6 < 1984)
cglobal4yr$del <- ifelse(cglobal4yr$X1.6==1983 & cglobal4yr$X1.5 > 9,1,0)
cglobal4yr <- subset(cglobal4yr, del==0)

global3yr <- global3yr[2:length(global3yr[,1]),]
global3yr <- subset(global3yr, X1.6 < 1984)
global3yr$del <- ifelse(global3yr$X1.6==1983 & global3yr$X1.5 > 9,1,0)
global3yr <- subset(global3yr, del==0)

cglobal3yr <- cglobal3yr[2:length(cglobal3yr[,1]),]
cglobal3yr <- subset(cglobal3yr, X1.6 < 1984)
cglobal3yr$del <- ifelse(cglobal3yr$X1.6==1983 & cglobal3yr$X1.5 > 9,1,0)
cglobal3yr <- subset(cglobal3yr, del==0)

global2yr <- global2yr[2:length(global2yr[,1]),]
global2yr <- subset(global2yr, X1.6 < 1984)
global2yr$del <- ifelse(global2yr$X1.6==1983 & global2yr$X1.5 > 9,1,0)
global2yr <- subset(global2yr, del==0)

cglobal2yr <- cglobal2yr[2:length(cglobal2yr[,1]),]
cglobal2yr <- subset(cglobal2yr, X1.6 < 1984)
cglobal2yr$del <- ifelse(cglobal2yr$X1.6==1983 & cglobal2yr$X1.5 > 9,1,0)
cglobal2yr <- subset(cglobal2yr, del==0)

LLRIK <- LLRIK[2:length(LLRIK[,1]),]
LLRIK <- subset(LLRIK, X1.5 < 1984)
LLRIK$del <- ifelse(LLRIK$X1.5==1983 & LLRIK$X1.4 > 9,1,0)
LLRIK <- subset(LLRIK, del==0)

cLLRIK <- cLLRIK[2:length(cLLRIK[,1]),]
cLLRIK <- subset(cLLRIK, X1.5 < 1984)
cLLRIK$del <- ifelse(cLLRIK$X1.5==1983 & cLLRIK$X1.4 > 9,1,0)
cLLRIK <- subset(cLLRIK, del==0)

LLRCCT <- LLRCCT[2:length(LLRCCT[,1]),]
LLRCCT <- subset(LLRCCT, X1.5 < 1984)
LLRCCT$del <- ifelse(LLRCCT$X1.5==1983 & LLRCCT$X1.4 > 9,1,0)
LLRCCT <- subset(LLRCCT, del==0)

cLLRCCT <- cLLRCCT[2:length(cLLRCCT[,1]),]
cLLRCCT <- subset(cLLRCCT, X1.5 < 1984)
cLLRCCT$del <- ifelse(cLLRCCT$X1.5==1983 & cLLRCCT$X1.4 > 9,1,0)
cLLRCCT <- subset(cLLRCCT, del==0)

###Appendix Figure 10

pdf("output/placebonp4.pdf", width=6.6, height=4.68)
hist(global4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.071, lwd=3)
dev.off()

ch <- subset(global4yr, X1 > 0.071)
ch2 <- subset(global4yr, X1 < -0.071)
1-((length(ch[,1])+length(ch2[,1]))/length(global4yr[,1]))
((length(ch[,1])+length(ch2[,1]))/length(global4yr[,1]))

pdf("output/placebonp3.pdf", width=6.6, height=4.68)
hist(global3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.095, lwd=3)
dev.off()

ch <- subset(global3yr, X1 > 0.095)
ch2 <- subset(global3yr, X1 < -0.095)
1-((length(ch[,1])+length(ch2[,1]))/length(global3yr[,1]))

pdf("output/placebonp2.pdf", width=6.6, height=4.68)
hist(global2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.144, lwd=3)
dev.off()

ch <- subset(global2yr, X1 > 0.144)
ch2 <- subset(global2yr, X1 < -0.144)
1-((length(ch[,1])+length(ch2[,1]))/length(global2yr[,1]))

pdf("output/placebonpLLRIK.pdf", width=6.6, height=4.68)
hist(LLRIK$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.126, lwd=3)
dev.off()

ch <- subset(LLRIK, X1 > 0.126)
ch2 <- subset(LLRIK, X1 < -0.126)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRIK[,1]))

pdf("output/placebonpLLRCCT.pdf", width=6.6, height=4.68)
hist(LLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.154, lwd=3)
dev.off()

ch <- subset(LLRCCT, X1 > 0.154)
ch2 <- subset(LLRCCT, X1 < -0.154)
1-((length(ch[,1])+length(ch2[,1]))/(length(LLRCCT[,1])))

###Appendix Figure 11

pdf("output/cplacebonp4.pdf", width=6.6, height=4.68)
hist(cglobal4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.106, lwd=3)
dev.off()

ch <- subset(cglobal4yr, X1 > 0.106)
ch2 <- subset(cglobal4yr, X1 < -0.106)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal4yr[,1]))

pdf("output/cplacebonp3.pdf", width=6.6, height=4.68)
hist(cglobal3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.120, lwd=3)
dev.off()

ch <- subset(cglobal3yr, X1 > 0.120)
ch2 <- subset(cglobal3yr, X1 < -0.120)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal3yr[,1]))

pdf("output/cplacebonp2.pdf", width=6.6, height=4.68)
hist(cglobal2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.167, lwd=3)
dev.off()

ch <- subset(cglobal2yr, X1 > 0.1672)
ch2 <- subset(cglobal2yr, X1 < -0.1672)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal2yr[,1]))

pdf("output/cplacebonpLLRIK.pdf", width=6.6, height=4.68)
hist(cLLRIK$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.145, lwd=3)
dev.off()

ch <- subset(cLLRIK, X1 > 0.145)
ch2 <- subset(cLLRIK, X1 < -0.145)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRIK[,1]))

pdf("output/cplacebonpLLRCCT.pdf", width=6.6, height=4.68)
hist(cLLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.154, lwd=3)
dev.off()

ch <- subset(cLLRCCT, X1 > 0.154)
ch2 <- subset(cLLRCCT, X1 < -0.154)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRCCT[,1]))


#######Low Income Inpatient 2009: Appendix Figures 12 and 13

allblack <- read.csv("lowincomeblack2009.csv")
allblack <- allblack[order(allblack$byear, allblack$bmonth),]
allblack$lognp <- log(allblack$count-allblack$preg)
allblack$logchron <- log(allblack$HCUPChronic)
allblack$lognonchron <- log(allblack$count-allblack$preg-allblack$HCUPChronic)

count <- 97
count2 <- 73
count3 <- 49

global4yr <- data.frame(1,1,1,1,1,1,1)
global3yr <- data.frame(1,1,1,1,1,1,1)
global2yr <- data.frame(1,1,1,1,1,1,1)
LLRIK <- data.frame(1,1,1,1,1,1)
LLRCCT <- data.frame(1,1,1,1,1,1)

cglobal4yr <- data.frame(1,1,1,1,1,1,1)
cglobal3yr <- data.frame(1,1,1,1,1,1,1)
cglobal2yr <- data.frame(1,1,1,1,1,1,1)
cLLRIK <- data.frame(1,1,1,1,1,1)
cLLRCCT <- data.frame(1,1,1,1,1,1)

allblack <- subset(allblack, byear > 1964)
allblack$marker <- seq(1:length(allblack[,1]))

for(i in c(0:177)){
counter <- count+i
counter2 <- count2+i
counter3 <- count3+i

datasetb <- subset(allblack, marker < counter & marker > i)
datasetb$time <- seq(1:96)-48
datasetb$time2 <- datasetb$time*datasetb$time
datasetb$after <- ifelse(datasetb$time > -1,1,0)

dataset3yr <- subset(allblack, marker < counter2 & marker > i)
dataset3yr$time <- seq(1:72)-36
dataset3yr$time2 <- dataset3yr$time*dataset3yr$time
dataset3yr$after <- ifelse(dataset3yr$time > -1,1,0)

dataset2yr <- subset(allblack, marker < counter3 & marker > i)
dataset2yr$time <- seq(1:48)-24
dataset2yr$time2 <- dataset2yr$time*dataset2yr$time
dataset2yr$after <- ifelse(dataset2yr$time > -1,1,0)

lastmonth <- datasetb[length(datasetb[,1]),]$bmonth
lastmonth2 <- dataset3yr[length(dataset3yr[,1]),]$bmonth
lastmonth3 <- dataset2yr[length(dataset2yr[,1]),]$bmonth

lastyear <- datasetb[length(datasetb[,1]),]$byear
lastyear2 <- dataset3yr[length(dataset3yr[,1]),]$byear
lastyear3 <- dataset2yr[length(dataset2yr[,1]),]$byear


global1 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=datasetb)
global2 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
global3 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid1 <- lm(lognp~as.factor(bmonth), data=datasetb)
llr1b <- rdrobust(resid1$residuals, datasetb$time, bwselect="IK")
llr2b <- rdrobust(resid1$residuals, datasetb$time, bwselect="CCT")

cglobal1 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=datasetb)
cglobal2 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
cglobal3 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid2 <- lm(logchron~as.factor(bmonth), data=datasetb)
cllr1b <- rdrobust(resid2$residuals, datasetb$time, bwselect="IK")
cllr2b <- rdrobust(resid2$residuals, datasetb$time, bwselect="CCT")

global4yr <- rbind(global4yr, c(summary(global1)$coefficients[2,], counter, lastmonth, lastyear))
global3yr <- rbind(global3yr, c(summary(global2)$coefficients[2,], counter, lastmonth2, lastyear2))
global2yr <- rbind(global2yr, c(summary(global3)$coefficients[2,], counter, lastmonth3, lastyear3))
LLRIK <- rbind(LLRIK, c(llr1b$coef, counter, lastmonth, lastyear))
LLRCCT <- rbind(LLRCCT, c(llr2b$coef, counter, lastmonth, lastyear))

cglobal4yr <- rbind(cglobal4yr, c(summary(cglobal1)$coefficients[2,], counter, lastmonth, lastyear))
cglobal3yr <- rbind(cglobal3yr, c(summary(cglobal2)$coefficients[2,], counter, lastmonth2, lastyear2))
cglobal2yr <- rbind(cglobal2yr, c(summary(cglobal3)$coefficients[2,], counter, lastmonth3, lastyear3))
cLLRIK <- rbind(cLLRIK, c(cllr1b$coef, counter, lastmonth, lastyear))
cLLRCCT <- rbind(cLLRCCT, c(cllr1b$coef, counter, lastmonth, lastyear))

}


global4yr <- global4yr[2:length(global4yr[,1]),]
global4yr <- subset(global4yr, X1.6 < 1984)
global4yr$del <- ifelse(global4yr$X1.6==1983 & global4yr$X1.5 > 9,1,0)
global4yr <- subset(global4yr, del==0)

cglobal4yr <- cglobal4yr[2:length(cglobal4yr[,1]),]
cglobal4yr <- subset(cglobal4yr, X1.6 < 1984)
cglobal4yr$del <- ifelse(cglobal4yr$X1.6==1983 & cglobal4yr$X1.5 > 9,1,0)
cglobal4yr <- subset(cglobal4yr, del==0)

global3yr <- global3yr[2:length(global3yr[,1]),]
global3yr <- subset(global3yr, X1.6 < 1984)
global3yr$del <- ifelse(global3yr$X1.6==1983 & global3yr$X1.5 > 9,1,0)
global3yr <- subset(global3yr, del==0)

cglobal3yr <- cglobal3yr[2:length(cglobal3yr[,1]),]
cglobal3yr <- subset(cglobal3yr, X1.6 < 1984)
cglobal3yr$del <- ifelse(cglobal3yr$X1.6==1983 & cglobal3yr$X1.5 > 9,1,0)
cglobal3yr <- subset(cglobal3yr, del==0)

global2yr <- global2yr[2:length(global2yr[,1]),]
global2yr <- subset(global2yr, X1.6 < 1984)
global2yr$del <- ifelse(global2yr$X1.6==1983 & global2yr$X1.5 > 9,1,0)
global2yr <- subset(global2yr, del==0)

cglobal2yr <- cglobal2yr[2:length(cglobal2yr[,1]),]
cglobal2yr <- subset(cglobal2yr, X1.6 < 1984)
cglobal2yr$del <- ifelse(cglobal2yr$X1.6==1983 & cglobal2yr$X1.5 > 9,1,0)
cglobal2yr <- subset(cglobal2yr, del==0)

LLRIK <- LLRIK[2:length(LLRIK[,1]),]
LLRIK <- subset(LLRIK, X1.5 < 1984)
LLRIK$del <- ifelse(LLRIK$X1.5==1983 & LLRIK$X1.4 > 9,1,0)
LLRIK <- subset(LLRIK, del==0)

cLLRIK <- cLLRIK[2:length(cLLRIK[,1]),]
cLLRIK <- subset(cLLRIK, X1.5 < 1984)
cLLRIK$del <- ifelse(cLLRIK$X1.5==1983 & cLLRIK$X1.4 > 9,1,0)
cLLRIK <- subset(cLLRIK, del==0)

LLRCCT <- LLRCCT[2:length(LLRCCT[,1]),]
LLRCCT <- subset(LLRCCT, X1.5 < 1984)
LLRCCT$del <- ifelse(LLRCCT$X1.5==1983 & LLRCCT$X1.4 > 9,1,0)
LLRCCT <- subset(LLRCCT, del==0)

cLLRCCT <- cLLRCCT[2:length(cLLRCCT[,1]),]
cLLRCCT <- subset(cLLRCCT, X1.5 < 1984)
cLLRCCT$del <- ifelse(cLLRCCT$X1.5==1983 & cLLRCCT$X1.4 > 9,1,0)
cLLRCCT <- subset(cLLRCCT, del==0)

###Produce Appendix Figure 12

pdf("output/placebonp4li.pdf", width=6.6, height=4.68)
hist(global4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.100, lwd=3)

dev.off()

ch <- subset(global4yr, X1 > 0.100)
ch2 <- subset(global4yr, X1 < -0.100)
1-((length(ch[,1])+length(ch2[,1]))/length(global4yr[,1]))

pdf("output/placebonp3li.pdf", width=6.6, height=4.68)
hist(global3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.150, lwd=3)
dev.off()

ch <- subset(global3yr, X1 > 0.150)
ch2 <- subset(global3yr, X1 < -0.150)
1-((length(ch[,1])+length(ch2[,1]))/length(global3yr[,1]))

pdf("output/placebonp2li.pdf", width=6.6, height=4.68)
hist(global2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-.227, max(global2yr$X1)))
abline(v=-0.227, lwd=3)
dev.off()

ch <- subset(global2yr, X1 > 0.227)
ch2 <- subset(global2yr, X1 < -0.227)
1-((length(ch[,1])+length(ch2[,1]))/length(global2yr[,1]))

pdf("output/placebonpLLRIKli.pdf", width=6.6, height=4.68)
hist(LLRIK$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.169, lwd=3)
dev.off()

ch <- subset(LLRIK, X1 > 0.169)
ch2 <- subset(LLRIK, X1 < -0.169)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRIK[,1]))

pdf("output/placebonpLLRCCTli.pdf", width=6.6, height=4.68)

hist(LLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.170, lwd=3)

dev.off()

ch <- subset(LLRCCT, X1 > 0.170)
ch2 <- subset(LLRCCT, X1 < -0.170)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRCCT[,1]))

###Produce Appendix Figure 13

pdf("output/cplacebonp4li.pdf", width=6.6, height=4.68)
hist(cglobal4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.148, lwd=3)
dev.off()

ch <- subset(cglobal4yr, X1 > 0.148)
ch2 <- subset(cglobal4yr, X1 < -0.148)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal4yr[,1]))

pdf("output/cplacebonp3li.pdf", width=6.6, height=4.68)
hist(cglobal3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.188, lwd=3)
dev.off()

ch <- subset(cglobal3yr, X1 > 0.188)
ch2 <- subset(cglobal3yr, X1 < -0.188)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal3yr[,1]))

pdf("output/cplacebonp2li.pdf", width=6.6, height=4.68)
hist(cglobal2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.28, max(cglobal2yr$X1)))
abline(v=-0.280, lwd=3)
dev.off()

ch <- subset(cglobal2yr, X1 > 0.280)
ch2 <- subset(cglobal2yr, X1 < -0.280)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal2yr[,1]))

pdf("output/cplacebonpLLRIKli.pdf", width=6.6, height=4.68)
hist(c(cLLRIK$X1), main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.236, max(cLLRIK$X1)))
abline(v=-0.235, lwd=3)
dev.off()

ch <- subset(cLLRIK, X1 > 0.235)
ch2 <- subset(cLLRIK, X1 < -0.235)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRIK[,1]))

pdf("output/cplacebonpLLRCCTli.pdf", width=6.6, height=4.68)
hist(cLLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.257, max(LLRCCT$X1)))
abline(v=-0.257, lwd=3)
dev.off()

ch <- subset(cLLRCCT, X1 > 0.257)
ch2 <- subset(cLLRCCT, X1 < -0.257)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRCCT))

#######ED 2009: Appendix Figure 14 and 15

allblack <- read.csv("sedd/edblack2009.csv")
allblack <- allblack[order(allblack$byear, allblack$bmonth),]
allblack$lognp <- log(allblack$count)
allblack$logchron <- log(allblack$HCUPChronic)
allblack$lognonchron <- log(allblack$count-allblack$HCUPChronic)

count <- 97
count2 <- 73
count3 <- 49

global4yr <- data.frame(1,1,1,1,1,1,1)
global3yr <- data.frame(1,1,1,1,1,1,1)
global2yr <- data.frame(1,1,1,1,1,1,1)
LLRIK <- data.frame(1,1,1,1,1,1)
LLRCCT <- data.frame(1,1,1,1,1,1)

cglobal4yr <- data.frame(1,1,1,1,1,1,1)
cglobal3yr <- data.frame(1,1,1,1,1,1,1)
cglobal2yr <- data.frame(1,1,1,1,1,1,1)
cLLRIK <- data.frame(1,1,1,1,1,1)
cLLRCCT <- data.frame(1,1,1,1,1,1)

allblack <- subset(allblack, byear > 1964)
allblack$marker <- seq(1:length(allblack[,1]))

for(i in c(0:177)){
counter <- count+i
counter2 <- count2+i
counter3 <- count3+i

datasetb <- subset(allblack, marker < counter & marker > i)
datasetb$time <- seq(1:96)-48
datasetb$time2 <- datasetb$time*datasetb$time
datasetb$after <- ifelse(datasetb$time > -1,1,0)

dataset3yr <- subset(allblack, marker < counter2 & marker > i)
dataset3yr$time <- seq(1:72)-36
dataset3yr$time2 <- dataset3yr$time*dataset3yr$time
dataset3yr$after <- ifelse(dataset3yr$time > -1,1,0)

dataset2yr <- subset(allblack, marker < counter3 & marker > i)
dataset2yr$time <- seq(1:48)-24
dataset2yr$time2 <- dataset2yr$time*dataset2yr$time
dataset2yr$after <- ifelse(dataset2yr$time > -1,1,0)

lastmonth <- datasetb[length(datasetb[,1]),]$bmonth
lastmonth2 <- dataset3yr[length(dataset3yr[,1]),]$bmonth
lastmonth3 <- dataset2yr[length(dataset2yr[,1]),]$bmonth

lastyear <- datasetb[length(datasetb[,1]),]$byear
lastyear2 <- dataset3yr[length(dataset3yr[,1]),]$byear
lastyear3 <- dataset2yr[length(dataset2yr[,1]),]$byear


global1 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=datasetb)
global2 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
global3 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid1 <- lm(lognp~as.factor(bmonth), data=datasetb)
llr1b <- rdrobust(resid1$residuals, datasetb$time, bwselect="IK")
llr2b <- rdrobust(resid1$residuals, datasetb$time, bwselect="CCT")

cglobal1 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=datasetb)
cglobal2 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
cglobal3 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid2 <- lm(logchron~as.factor(bmonth), data=datasetb)
cllr1b <- rdrobust(resid2$residuals, datasetb$time, bwselect="IK")
cllr2b <- rdrobust(resid2$residuals, datasetb$time, bwselect="CCT")

global4yr <- rbind(global4yr, c(summary(global1)$coefficients[2,], counter, lastmonth, lastyear))
global3yr <- rbind(global3yr, c(summary(global2)$coefficients[2,], counter, lastmonth2, lastyear2))
global2yr <- rbind(global2yr, c(summary(global3)$coefficients[2,], counter, lastmonth3, lastyear3))
LLRIK <- rbind(LLRIK, c(llr1b$coef, counter, lastmonth, lastyear))
LLRCCT <- rbind(LLRCCT, c(llr2b$coef, counter, lastmonth, lastyear))

cglobal4yr <- rbind(cglobal4yr, c(summary(cglobal1)$coefficients[2,], counter, lastmonth, lastyear))
cglobal3yr <- rbind(cglobal3yr, c(summary(cglobal2)$coefficients[2,], counter, lastmonth2, lastyear2))
cglobal2yr <- rbind(cglobal2yr, c(summary(cglobal3)$coefficients[2,], counter, lastmonth3, lastyear3))
cLLRIK <- rbind(cLLRIK, c(cllr1b$coef, counter, lastmonth, lastyear))
cLLRCCT <- rbind(cLLRCCT, c(cllr1b$coef, counter, lastmonth, lastyear))

}


global4yr <- global4yr[2:length(global4yr[,1]),]
global4yr <- subset(global4yr, X1.6 < 1984)
global4yr$del <- ifelse(global4yr$X1.6==1983 & global4yr$X1.5 > 9,1,0)
global4yr <- subset(global4yr, del==0)

cglobal4yr <- cglobal4yr[2:length(cglobal4yr[,1]),]
cglobal4yr <- subset(cglobal4yr, X1.6 < 1984)
cglobal4yr$del <- ifelse(cglobal4yr$X1.6==1983 & cglobal4yr$X1.5 > 9,1,0)
cglobal4yr <- subset(cglobal4yr, del==0)

global3yr <- global3yr[2:length(global3yr[,1]),]
global3yr <- subset(global3yr, X1.6 < 1984)
global3yr$del <- ifelse(global3yr$X1.6==1983 & global3yr$X1.5 > 9,1,0)
global3yr <- subset(global3yr, del==0)

cglobal3yr <- cglobal3yr[2:length(cglobal3yr[,1]),]
cglobal3yr <- subset(cglobal3yr, X1.6 < 1984)
cglobal3yr$del <- ifelse(cglobal3yr$X1.6==1983 & cglobal3yr$X1.5 > 9,1,0)
cglobal3yr <- subset(cglobal3yr, del==0)

global2yr <- global2yr[2:length(global2yr[,1]),]
global2yr <- subset(global2yr, X1.6 < 1984)
global2yr$del <- ifelse(global2yr$X1.6==1983 & global2yr$X1.5 > 9,1,0)
global2yr <- subset(global2yr, del==0)

cglobal2yr <- cglobal2yr[2:length(cglobal2yr[,1]),]
cglobal2yr <- subset(cglobal2yr, X1.6 < 1984)
cglobal2yr$del <- ifelse(cglobal2yr$X1.6==1983 & cglobal2yr$X1.5 > 9,1,0)
cglobal2yr <- subset(cglobal2yr, del==0)

LLRIK <- LLRIK[2:length(LLRIK[,1]),]
LLRIK <- subset(LLRIK, X1.5 < 1984)
LLRIK$del <- ifelse(LLRIK$X1.5==1983 & LLRIK$X1.4 > 9,1,0)
LLRIK <- subset(LLRIK, del==0)

cLLRIK <- cLLRIK[2:length(cLLRIK[,1]),]
cLLRIK <- subset(cLLRIK, X1.5 < 1984)
cLLRIK$del <- ifelse(cLLRIK$X1.5==1983 & cLLRIK$X1.4 > 9,1,0)
cLLRIK <- subset(cLLRIK, del==0)

LLRCCT <- LLRCCT[2:length(LLRCCT[,1]),]
LLRCCT <- subset(LLRCCT, X1.5 < 1984)
LLRCCT$del <- ifelse(LLRCCT$X1.5==1983 & LLRCCT$X1.4 > 9,1,0)
LLRCCT <- subset(LLRCCT, del==0)

cLLRCCT <- cLLRCCT[2:length(cLLRCCT[,1]),]
cLLRCCT <- subset(cLLRCCT, X1.5 < 1984)
cLLRCCT$del <- ifelse(cLLRCCT$X1.5==1983 & cLLRCCT$X1.4 > 9,1,0)
cLLRCCT <- subset(cLLRCCT, del==0)


###Produce Appendix Figure 14

pdf("output/placebonp4ED.pdf", width=6.6, height=4.68)
hist(global4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.037, lwd=3)
dev.off()

ch <- subset(global4yr, X1 > 0.037)
ch2 <- subset(global4yr, X1 < -0.037)
1-((length(ch[,1])+length(ch2[,1]))/length(global4yr[,1]))

pdf("output/placebonp3ED.pdf", width=6.6, height=4.68)
hist(global3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.049, lwd=3)
dev.off()

ch <- subset(global3yr, X1 > 0.049)
ch2 <- subset(global3yr, X1 < -0.049)
1-((length(ch[,1])+length(ch2[,1]))/length(global3yr[,1]))

pdf("output/placebonp2ED.pdf", width=6.6, height=4.68)
hist(global2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.030, lwd=3)
dev.off()

ch <- subset(global2yr, X1 > 0.030)
ch2 <- subset(global2yr, X1 < -0.030)
1-((length(ch[,1])+length(ch2[,1]))/length(global2yr[,1]))

pdf("output/placebonpLLRIKED.pdf", width=6.6, height=4.68)
hist(LLRIK$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.023, lwd=3)
dev.off()

ch <- subset(LLRIK, X1 > 0.023)
ch2 <- subset(LLRIK, X1 < -0.023)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRIK[,1]))

pdf("output/placebonpLLRCCTED.pdf", width=6.6, height=4.68)
hist(LLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.022, lwd=3)
dev.off()

ch <- subset(LLRCCT, X1 > 0.022)
ch2 <- subset(LLRCCT, X1 < -0.022)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRCCT[,1]))


##Produce Appendix Figure 15

pdf("output/cplacebonp4ED.pdf", width=6.6, height=4.68)

hist(cglobal4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.102, lwd=3)

dev.off()

ch <- subset(cglobal4yr, X1 > 0.102)
ch2 <- subset(cglobal4yr, X1 < -0.102)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal4yr[,1]))

pdf("output/cplacebonp3ED.pdf", width=6.6, height=4.68)

cglobal3yr <- cglobal3yr[2:length(cglobal3yr[,1]),]
hist(cglobal3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.152, lwd=3)

dev.off()

ch <- subset(cglobal3yr, X1 > 0.152)
ch2 <- subset(cglobal3yr, X1 < -0.152)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal3yr[,1]))

pdf("output/cplacebonp2ED.pdf", width=6.6, height=4.68)

hist(cglobal2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.28, max(cglobal2yr$X1)))
abline(v=-0.147, lwd=3)

dev.off()

ch <- subset(cglobal2yr, X1 > 0.147)
ch2 <- subset(cglobal2yr, X1 < -0.147)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal2yr[,1]))

pdf("output/cplacebonpLLRIKED.pdf", width=6.6, height=4.68)

hist(c(cLLRIK$X1), main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.236, max(cLLRIK$X1)))
abline(v=-0.123, lwd=3)

dev.off()

ch <- subset(cLLRIK, X1 > 0.123)
ch2 <- subset(cLLRIK, X1 < -0.123)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRIK[,1]))

pdf("output/cplacebonpLLRCCTED.pdf", width=6.6, height=4.68)

hist(cLLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.144, lwd=3)

dev.off()

ch <- subset(cLLRCCT, X1 > 0.144)
ch2 <- subset(cLLRCCT, X1 < -0.144)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRCCT[,1]))

#######Low Income ED 2009: Appendix Figures 16-17

allblack <- read.csv("sedd/lowincedblack.csv")
allblack <- allblack[order(allblack$byear, allblack$bmonth),]
allblack$lognp <- log(allblack$count)
allblack$logchron <- log(allblack$HCUPChronic)
allblack$lognonchron <- log(allblack$count-allblack$HCUPChronic)

count <- 97
count2 <- 73
count3 <- 49

global4yr <- data.frame(1,1,1,1,1,1,1)
global3yr <- data.frame(1,1,1,1,1,1,1)
global2yr <- data.frame(1,1,1,1,1,1,1)
LLRIK <- data.frame(1,1,1,1,1,1)
LLRCCT <- data.frame(1,1,1,1,1,1)

cglobal4yr <- data.frame(1,1,1,1,1,1,1)
cglobal3yr <- data.frame(1,1,1,1,1,1,1)
cglobal2yr <- data.frame(1,1,1,1,1,1,1)
cLLRIK <- data.frame(1,1,1,1,1,1)
cLLRCCT <- data.frame(1,1,1,1,1,1)

allblack <- subset(allblack, byear > 1964)
allblack$marker <- seq(1:length(allblack[,1]))

for(i in c(0:177)){
counter <- count+i
counter2 <- count2+i
counter3 <- count3+i

datasetb <- subset(allblack, marker < counter & marker > i)
datasetb$time <- seq(1:96)-48
datasetb$time2 <- datasetb$time*datasetb$time
datasetb$after <- ifelse(datasetb$time > -1,1,0)

dataset3yr <- subset(allblack, marker < counter2 & marker > i)
dataset3yr$time <- seq(1:72)-36
dataset3yr$time2 <- dataset3yr$time*dataset3yr$time
dataset3yr$after <- ifelse(dataset3yr$time > -1,1,0)

dataset2yr <- subset(allblack, marker < counter3 & marker > i)
dataset2yr$time <- seq(1:48)-24
dataset2yr$time2 <- dataset2yr$time*dataset2yr$time
dataset2yr$after <- ifelse(dataset2yr$time > -1,1,0)

lastmonth <- datasetb[length(datasetb[,1]),]$bmonth
lastmonth2 <- dataset3yr[length(dataset3yr[,1]),]$bmonth
lastmonth3 <- dataset2yr[length(dataset2yr[,1]),]$bmonth

lastyear <- datasetb[length(datasetb[,1]),]$byear
lastyear2 <- dataset3yr[length(dataset3yr[,1]),]$byear
lastyear3 <- dataset2yr[length(dataset2yr[,1]),]$byear


global1 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=datasetb)
global2 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
global3 <- lm(lognp~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid1 <- lm(lognp~as.factor(bmonth), data=datasetb)
llr1b <- rdrobust(resid1$residuals, datasetb$time, bwselect="IK")
llr2b <- rdrobust(resid1$residuals, datasetb$time, bwselect="CCT")

cglobal1 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=datasetb)
cglobal2 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset3yr)
cglobal3 <- lm(logchron~after*(time+time2)+as.factor(bmonth), data=dataset2yr)
resid2 <- lm(logchron~as.factor(bmonth), data=datasetb)
cllr1b <- rdrobust(resid2$residuals, datasetb$time, bwselect="IK")
cllr2b <- rdrobust(resid2$residuals, datasetb$time, bwselect="CCT")

global4yr <- rbind(global4yr, c(summary(global1)$coefficients[2,], counter, lastmonth, lastyear))
global3yr <- rbind(global3yr, c(summary(global2)$coefficients[2,], counter, lastmonth2, lastyear2))
global2yr <- rbind(global2yr, c(summary(global3)$coefficients[2,], counter, lastmonth3, lastyear3))
LLRIK <- rbind(LLRIK, c(llr1b$coef, counter, lastmonth, lastyear))
LLRCCT <- rbind(LLRCCT, c(llr2b$coef, counter, lastmonth, lastyear))

cglobal4yr <- rbind(cglobal4yr, c(summary(cglobal1)$coefficients[2,], counter, lastmonth, lastyear))
cglobal3yr <- rbind(cglobal3yr, c(summary(cglobal2)$coefficients[2,], counter, lastmonth2, lastyear2))
cglobal2yr <- rbind(cglobal2yr, c(summary(cglobal3)$coefficients[2,], counter, lastmonth3, lastyear3))
cLLRIK <- rbind(cLLRIK, c(cllr1b$coef, counter, lastmonth, lastyear))
cLLRCCT <- rbind(cLLRCCT, c(cllr1b$coef, counter, lastmonth, lastyear))

}


global4yr <- global4yr[2:length(global4yr[,1]),]
global4yr <- subset(global4yr, X1.6 < 1984)
global4yr$del <- ifelse(global4yr$X1.6==1983 & global4yr$X1.5 > 9,1,0)
global4yr <- subset(global4yr, del==0)

cglobal4yr <- cglobal4yr[2:length(cglobal4yr[,1]),]
cglobal4yr <- subset(cglobal4yr, X1.6 < 1984)
cglobal4yr$del <- ifelse(cglobal4yr$X1.6==1983 & cglobal4yr$X1.5 > 9,1,0)
cglobal4yr <- subset(cglobal4yr, del==0)

global3yr <- global3yr[2:length(global3yr[,1]),]
global3yr <- subset(global3yr, X1.6 < 1984)
global3yr$del <- ifelse(global3yr$X1.6==1983 & global3yr$X1.5 > 9,1,0)
global3yr <- subset(global3yr, del==0)

cglobal3yr <- cglobal3yr[2:length(cglobal3yr[,1]),]
cglobal3yr <- subset(cglobal3yr, X1.6 < 1984)
cglobal3yr$del <- ifelse(cglobal3yr$X1.6==1983 & cglobal3yr$X1.5 > 9,1,0)
cglobal3yr <- subset(cglobal3yr, del==0)

global2yr <- global2yr[2:length(global2yr[,1]),]
global2yr <- subset(global2yr, X1.6 < 1984)
global2yr$del <- ifelse(global2yr$X1.6==1983 & global2yr$X1.5 > 9,1,0)
global2yr <- subset(global2yr, del==0)

cglobal2yr <- cglobal2yr[2:length(cglobal2yr[,1]),]
cglobal2yr <- subset(cglobal2yr, X1.6 < 1984)
cglobal2yr$del <- ifelse(cglobal2yr$X1.6==1983 & cglobal2yr$X1.5 > 9,1,0)
cglobal2yr <- subset(cglobal2yr, del==0)

LLRIK <- LLRIK[2:length(LLRIK[,1]),]
LLRIK <- subset(LLRIK, X1.5 < 1984)
LLRIK$del <- ifelse(LLRIK$X1.5==1983 & LLRIK$X1.4 > 9,1,0)
LLRIK <- subset(LLRIK, del==0)

cLLRIK <- cLLRIK[2:length(cLLRIK[,1]),]
cLLRIK <- subset(cLLRIK, X1.5 < 1984)
cLLRIK$del <- ifelse(cLLRIK$X1.5==1983 & cLLRIK$X1.4 > 9,1,0)
cLLRIK <- subset(cLLRIK, del==0)

LLRCCT <- LLRCCT[2:length(LLRCCT[,1]),]
LLRCCT <- subset(LLRCCT, X1.5 < 1984)
LLRCCT$del <- ifelse(LLRCCT$X1.5==1983 & LLRCCT$X1.4 > 9,1,0)
LLRCCT <- subset(LLRCCT, del==0)

cLLRCCT <- cLLRCCT[2:length(cLLRCCT[,1]),]
cLLRCCT <- subset(cLLRCCT, X1.5 < 1984)
cLLRCCT$del <- ifelse(cLLRCCT$X1.5==1983 & cLLRCCT$X1.4 > 9,1,0)
cLLRCCT <- subset(cLLRCCT, del==0)

###Appendix Figure 16

pdf("output/placebonp4EDli.pdf", width=6.6, height=4.68)

hist(global4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.056, lwd=3)

dev.off()

ch <- subset(global4yr, X1 > 0.056)
ch2 <- subset(global4yr, X1 < -0.056)
1-((length(ch[,1])+length(ch2[,1]))/length(global4yr[,1]))

pdf("output/placebonp3EDli.pdf", width=6.6, height=4.68)
hist(global3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.064, lwd=3)
dev.off()

ch <- subset(global3yr, X1 > 0.064)
ch2 <- subset(global3yr, X1 < -0.064)
1-((length(ch[,1])+length(ch2[,1]))/length(global3yr[,1]))

pdf("output/placebonp2EDli.pdf", width=6.6, height=4.68)
global2yr <- global2yr[2:length(global2yr[,1]),]
hist(global2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.038, lwd=3)
dev.off()

ch <- subset(global2yr, X1 > 0.038)
ch2 <- subset(global2yr, X1 < -0.038)
1-((length(ch[,1])+length(ch2[,1]))/length(global2yr[,1]))

pdf("output/placebonpLLRIKEDli.pdf", width=6.6, height=4.68)
LLRIK <- LLRIK[2:length(LLRIK[,1]),]
hist(LLRIK$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.035, lwd=3)
dev.off()

ch <- subset(LLRIK, X1 > 0.035)
ch2 <- subset(LLRIK, X1 < -0.035)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRIK[,1]))

pdf("output/placebonpLLRCCTEDli.pdf", width=6.6, height=4.68)
hist(LLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.036, lwd=3)
dev.off()

ch <- subset(LLRCCT, X1 > 0.036)
ch2 <- subset(LLRCCT, X1 < -0.036)
1-((length(ch[,1])+length(ch2[,1]))/length(LLRCCT[,1]))


##Appendix Figure 17

pdf("output/cplacebonp4EDli.pdf", width=6.6, height=4.68)
hist(cglobal4yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.129, lwd=3)
dev.off()

ch <- subset(cglobal4yr, X1 > 0.129)
ch2 <- subset(cglobal4yr, X1 < -0.129)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal4yr[,1]))

pdf("output/cplacebonp3EDli.pdf", width=6.6, height=4.68)

hist(cglobal3yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency")
abline(v=-0.134, lwd=3)

dev.off()

ch <- subset(cglobal3yr, X1 > 0.134)
ch2 <- subset(cglobal3yr, X1 < -0.134)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal3yr[,1]))

pdf("output/cplacebonp2ED.pdf", width=6.6, height=4.68)

hist(cglobal2yr$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.28, max(cglobal2yr$X1)))
abline(v=-0.165, lwd=3)

dev.off()

ch <- subset(cglobal2yr, X1 > 0.165)
ch2 <- subset(cglobal2yr, X1 < -0.165)
1-((length(ch[,1])+length(ch2[,1]))/length(cglobal2yr[,1]))

pdf("output/cplacebonpLLRIKEDli.pdf", width=6.6, height=4.68)

hist(c(cLLRIK$X1), main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.236, max(cLLRIK$X1)))
abline(v=-0.119, lwd=3)

dev.off()

ch <- subset(cLLRIK, X1 > 0.119)
ch2 <- subset(cLLRIK, X1 < -0.119)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRIK[,1]))

pdf("output/placebonpLLRCCTEDli.pdf", width=6.6, height=4.68)

hist(cLLRCCT$X1, main=" ", xlab="Placebo Values", ylab="Frequency", xlim=c(-0.257, max(LLRCCT$X1)))
abline(v=-0.136, lwd=3)

dev.off()

ch <- subset(cLLRCCT, X1 > 0.136)
ch2 <- subset(cLLRCCT, X1 < -0.136)
1-((length(ch[,1])+length(ch2[,1]))/length(cLLRCCT[,1]))