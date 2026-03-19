
##########################################################################
##########################################################################
#    This file contains the code for boxplots that compare various
#    variables across continents
##########################################################################
##########################################################################
install.packages("calibrate")

library(calibrate)

data <- read.csv("complete.data.iv.csv")
attach(data)
names(data)

#Create a variable for continent that is numeric (allows jittering)
cont <- rep(0, length(continent))
cont[continent=="Africa"] <- 1
cont[continent=="Asia"] <- 2
cont[continent=="Other"] <- 3
cont[continent=="South America"] <- 4

#Boxplot of log mortality by continent
pdf("bp1.pdf", width=6, height=6)
boxplot(logem4[which(baseco==1)] ~ continent[which(baseco==1)], ylab="Log Mortality", ylim=c(1, 11), outline=FALSE)
textxy(jitter(cont[which(baseco==1 & post1900==1)]), logem4[which(baseco==1 & post1900==1)], labs=shortnam[which(baseco==1 & post1900==1)], col="red")
textxy(jitter(cont[which(baseco==1 & post1900==0)]), logem4[which(baseco==1 & post1900==0)], labs=shortnam[which(baseco==1 & post1900==0)], col="blue")
legend(0.5, 11, c("Pre-1900 Independence", "Post-1900 Independence"), pch=c(19,19), col=c("blue", "red"))
dev.off()

#Boxplot of contraint on the executice in 1900 by continent
pdf("bp2.pdf", width=6, height=6)
boxplot(cons00a[which(baseco==1)] ~ continent[which(baseco==1)], ylab="Constraint on Executive 1900", ylim=c(0, 10), outline=FALSE)
textxy(jitter(cont[which(baseco==1 & post1900==1 & cons00a>0)]), cons00a[which(baseco==1 & post1900==1 & cons00a>0)], labs=shortnam[which(baseco==1 & post1900==1 & cons00a>0)], col="red")
textxy(jitter(cont[which(baseco==1 & post1900==0 & cons00a>0)]), cons00a[which(baseco==1 & post1900==0 & cons00a >0)], labs=shortnam[which(baseco==1 & post1900==0 & cons00a>0)], col="blue")
legend(0.5, 10, c("Pre-1900 Independence", "Post-1900 Independence"), pch=c(19,19), col=c("blue", "red"))
dev.off()

#Boxplot of average protection against expropriation risk
pdf("bp3.pdf", width=6, height=6)
boxplot(avexpr[which(baseco==1)] ~ continent[which(baseco==1)], ylab="Avg. Protection Against Expropriation, 1990", ylim=c(2, 13), outline=FALSE)
textxy(jitter(cont[which(baseco==1 & post1900==1)]), avexpr[which(baseco==1 & post1900==1)], labs=shortnam[which(baseco==1 & post1900==1)], col="red")
textxy(jitter(cont[which(baseco==1 & post1900==0)]), avexpr[which(baseco==1 & post1900==0)], labs=shortnam[which(baseco==1 & post1900==0)], col="blue")
legend(0.5, 13, c("Pre-1900 Independence", "Post-1900 Independence"), pch=c(19,19), col=c("blue", "red"))
dev.off()

#Boxplot of per capita GDP 1995
pdf("bp4.pdf", width=6, height=6)
boxplot(logpgp95[which(baseco==1)] ~ continent[which(baseco==1)], ylab="Per Capita GDP, 1995", ylim=c(5, 12), outline=FALSE)
textxy(jitter(cont[which(baseco==1 & post1900==1)]), logpgp95[which(baseco==1 & post1900==1)], labs=shortnam[which(baseco==1 & post1900==1)], col="red")
textxy(jitter(cont[which(baseco==1 & post1900==0)]), logpgp95[which(baseco==1 & post1900==0)], labs=shortnam[which(baseco==1 & post1900==0)], col="blue")
legend(0.5, 12, c("Pre-1900 Independence", "Post-1900 Independence"), pch=c(19,19), col=c("blue", "red"))
dev.off()

#Boxplot of Institutional Age
pdf("bp5.pdf", width=6, height=6)
boxplot(inst.age.1817[which(baseco==1)] ~ continent[which(baseco==1)], ylab="Institutional Age in 1817", ylim=c(-50,500), outline=FALSE)
textxy(jitter(cont[which(baseco==1 & post1900==1)]), inst.age.1817[which(baseco==1 & post1900==1)], labs=shortnam[which(baseco==1 & post1900==1)], col="red")
textxy(jitter(cont[which(baseco==1 & post1900==0)]), inst.age.1817[which(baseco==1 & post1900==0)], labs=shortnam[which(baseco==1 & post1900==0)], col="blue")
legend(0.5, 500, c("Pre-1900 Independence", "Post-1900 Independence"), pch=c(19,19), col=c("blue", "red"))
dev.off()

