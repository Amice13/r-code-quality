##########################################################################
##########################################################################
#    This file contains the code for a simulation testing how sensitive
#    analysis is to assigning low values for early
#    institutions (cons00a).
##########################################################################
##########################################################################

data <- read.csv("complete.data.iv.csv")
attach(data)
names(data)


#Examining the data

sum(post1900[which(baseco==1 & cons00a==1)])

cons00a[which(baseco==1 & post1900==1 & cons00a!=1)]


hist(cons00a[which(baseco==1 & post1900==1)])
hist(cons00a[which(baseco==1 & post1900==0)], col="blue")

base <- subset(data, baseco==1)


library(car)

name[which(baseco==1 & other==1 & cons00a!=1)]

########################################################################


#########################################################################
#Simulation 1: ALL COUNTRIES WITH INDEPENDENCE POST-1900 AND CONS00A 
#VALUE OF 1 IN AJR DATASET

#Separate the data for the relevant countries
missin <- subset(data, baseco==1 & post1900==1 & cons00a==1, na.rm=TRUE)
plot(density(missin$cons00a, na.rm=TRUE))
pre1900 <- subset(data, baseco==1 & post1900==0)
postgreat <- subset(data, baseco==1 & post1900==1 & cons00a>1)
nomiss <- merge(pre1900, postgreat, all=TRUE)

#Set number of simulations and create a variable to hold p-values
sims <- 1000
pvals <- rep(NA, sims)

#Run a double loop to create sample data, merge the sample with the
#rest of the data, run regression model, and save p-value
set.seed(06511)
for (i in 1:sims) {
	for (j in 1:length(missin$cons00a)) {
		missin$cons00a[j] <- sample(nomiss$cons00a, 1, replace=TRUE)
		newdata <- merge(missin, nomiss, all=TRUE)
		m1 <- lm(newdata$cons00a ~ newdata$logem4)
		pvals[i] <- summary(m1)$coefficients[2, "Pr(>|t|)"] 
	}
}

#Check that everything worked
pvals


#Save a density plot of the pvalues
png("pvalallmiss.png")
plot(density(pvals), xlab="P-Values of Simulated First-Stage Regressions", main="", ylim=c(-.25,2.5))
segments(0.05, 0, 0.05, 3, col="red", lty=2)
text(0.05,-.175, "p=0.05", col="red")
rug(pvals, col="black", ticksize = 0.025, lwd=0.5)
dev.off()


#Calculate the percentage of p-values that are greater than 0.05
percfail <- length(pvals[which(pvals>0.05)])/sims
percfail
#Result is 79.7%


#########################################################################

#Simulation 2: AFRICAN COUNTRIES WITH POST-1900 INDEPENDENCE DATES AND 
#CONS00A VALUES OF 1 IN AJR DATASET


#Separate the data for the relevant countries
afdata <- subset(data, baseco==1 & africa==1 & cons00a==1)
noaf <- subset(data, baseco==1 & africa!=1, na.rm=TRUE)
afgreat <- subset(data, baseco==1 & africa==1 & cons00a>1)
fullinfo <- merge(noaf, afgreat, all=TRUE)
plot(fullinfo$cons00a)

#Set number of simulations and create a variable to hold p-values
sims <- 1000
pvalsaf <- rep(NA, sims)

#Run a double loop to create sample data, merge the sample with the
#rest of the data, run regression model, and save p-value
set.seed(06511)
for (i in 1:sims) {
for (j in 1:length(afdata$cons00a)) {
afdata$cons00a[j] <- sample(fullinfo$cons00a, 1, replace=TRUE)
newdata <- merge(afdata, fullinfo, all=TRUE)
m1 <- lm(newdata$cons00a ~ newdata$logem4)
pvalsaf[i] <- summary(m1)$coefficients[2, "Pr(>|t|)"] 
}
}

#Check that everything worked
pvalsaf

#Save a density plot of the pvalues
png("pvalafr.png")
plot(density(pvalsaf), xlab="P-Values of Simulated First-Stage Regressions", main="", ylim=c(-.25,3.25))
segments(0.05, 0, 0.05, 3.5, col="red", lty=2)
text(0.05,-.175, "p=0.05", col="red")
rug(pvalsaf, col="black", ticksize = 0.025, lwd=0.5)
dev.off()



#Calculate the percentage of p-values that are greater than 0.05
percfailaf <- length(pvalsaf[which(pvalsaf>0.05)])/sims
percfailaf
#Result is 71.4%










