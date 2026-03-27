
# first, you need to go into the coda index file and get rid of any parameter 
# names that contain square brackets (vectorized parameters), convert [ to .
# and delete all ]; also if used a MVN, you also need to convert , to .


library(coda)
library(BRugs)

results<-read.openbugs()
coda.data<-as.data.frame(rbind(results[[1]], results[[2]], results[[3]]))


ndraws=length(coda.data[,1])
nparameters=length(coda.data[1,])


##### begin skip
# do this to lop off parameters you don't want, e.g., latent variable estimates
# note: 1318 to 1339 for g*, 2657 to 2677 for ideo.sd.mean, ideo.sd.sd, lambda_* and rho.eta

coda.index.a<-coda.index[1318:1339,]
start<-coda.index.a[1,2]
stop<-coda.index.a[length(coda.index.a[,2]),3]
coda.1.a<-coda.1[start:stop,]
coda.2.a<-coda.2[start:stop,]
coda.3.a<-coda.3[start:stop,]

coda.index.b<-coda.index[2657:2677,]
start<-coda.index.b[1,2]
stop<-coda.index.b[length(coda.index.b[,2]),3]
coda.1.b<-coda.1[start:stop,]
coda.2.b<-coda.2[start:stop,]
coda.3.b<-coda.3[start:stop,]

coda.index<-rbind(coda.index.a, coda.index.b)
coda.1<-rbind(coda.1.a, coda.1.b)
coda.2<-rbind(coda.2.a, coda.2.b)
coda.3<-rbind(coda.3.a, coda.3.b)
##### end skip


# plot(-4:4, -4:4, type = "n")# setting up coord. system

par(mfcol=c(2,2))
plot(density((-1)*coda.data$g1.1, kernel="gaussian"), main=paste("First Order Betas"), xlab=("Process Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g1.2, kernel="gaussian"), main=paste(""), xlab=("Policy Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g2.1, kernel="gaussian"), main=paste("Second Order Betas"), xlab=("Process Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g2.2, kernel="gaussian"), main=paste(""), xlab=("Policy Quality"), ylab=("Density"))


par(mfcol=c(2,2))
plot(density((-1)*coda.data$g3.1, kernel="gaussian"), main=paste("First Order Betas"), xlab=("Process Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g3.2, kernel="gaussian"), main=paste(""), xlab=("Policy Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g4.1, kernel="gaussian"), main=paste("Second Order Betas"), xlab=("Process Quality"), ylab=("Density"))
plot(density((-1)*coda.data$g4.2, kernel="gaussian"), main=paste(""), xlab=("Policy Quality"), ylab=("Density"))





x<-(seq(.3,1.5,length.out=1000))
y<-(seq(0,3.3,length.out=1000))

# prt = process table, pot = policy table, pri = process individual, poi = policy individual

# prt bounds:
upper <- rep(NA, 1000)
lower <- rep(NA, 1000)
mu.bar <- 0.2988
for (j in 1:1000) {
mu.bar <- (mu.bar+0.0012)
r <- rep(NA, 1000)
for (i in 1:1000) {
random <- as.integer(runif(1, 1, length(coda.data$g1.1)))
curve <- function(x) {
(-1)*coda.data$g1.1[random]*x+(-1)*coda.data$g2.1[random]*x^2+1}
r[i] <- curve(mu.bar)
}
upper[j] <- sort(r)[975]
lower[j] <- sort(r)[25]
}
upper.prt <- lowess(seq(0.3, 1.5, length.out=1000), upper, f=.1)
lower.prt <- lowess(seq(0.3, 1.5, length.out=1000), lower, f=.1)

# pot bounds:
upper <- rep(NA, 1000)
lower <- rep(NA, 1000)
mu.bar <- 0.2988
for (j in 1:1000) {
mu.bar <- (mu.bar+0.0012)
r <- rep(NA, 1000)
for (i in 1:1000) {
random <- as.integer(runif(1, 1, length(coda.data$g1.1)))
curve <- function(x) {
(-1)*coda.data$g1.2[random]*x+(-1)*coda.data$g2.2[random]*x^2+1}
r[i] <- curve(mu.bar)
}
upper[j] <- sort(r)[975]
lower[j] <- sort(r)[25]
}
upper.pot <- lowess(seq(0.3, 1.5, length.out=1000), upper, f=.1)
lower.pot <- lowess(seq(0.3, 1.5, length.out=1000), lower, f=.1)

# pri bounds:
upper <- rep(NA, 1000)
lower <- rep(NA, 1000)
mu.bar <- 0.2988
for (j in 1:1000) {
mu.bar <- (mu.bar+0.0012)
r <- rep(NA, 1000)
for (i in 1:1000) {
random <- as.integer(runif(1, 1, length(coda.data$g1.1)))
curve <- function(x) {
(-1)*coda.data$g3.1[random]*x+(-1)*coda.data$g4.1[random]*x^2+1}
r[i] <- curve(mu.bar)
}
upper[j] <- sort(r)[975]
lower[j] <- sort(r)[25]
}
upper.pri <- lowess(seq(0.3, 1.5, length.out=1000), upper, f=.1)
lower.pri <- lowess(seq(0.3, 1.5, length.out=1000), lower, f=.1)

# poi bounds:
upper <- rep(NA, 1000)
lower <- rep(NA, 1000)
mu.bar <- 0.2988
for (j in 1:1000) {
mu.bar <- (mu.bar+0.0012)
r <- rep(NA, 1000)
for (i in 1:1000) {
random <- as.integer(runif(1, 1, length(coda.data$g1.1)))
curve <- function(x) {
(-1)*coda.data$g3.2[random]*x+(-1)*coda.data$g4.2[random]*x^2+1}
r[i] <- curve(mu.bar)
}
upper[j] <- sort(r)[975]
lower[j] <- sort(r)[25]
}
upper.poi <- lowess(seq(0.3, 1.5, length.out=1000), upper, f=.1)
lower.poi <- lowess(seq(0.3, 1.5, length.out=1000), lower, f=.1)





par(mfrow=c(2,2))

plot(x,y, type = "n", main=paste("Process Satisfaction"), xlab=("Table Disagreement"), ylab=("Process Satisfaction Index"))# setting up coord. system
polygon(c(x, rev(x)), c(lower.prt$y, rev(upper.prt$y)), col="lightcyan", border=NA)
lines(upper.prt, type="l", col="lightsteelblue", lty=2)
lines(lower.prt, type="l", col="lightsteelblue", lty=2)
curve<-function(x) {
(-1)*mean(coda.data$g1.1)*x+(-1)*mean(coda.data$g2.1)*x^2+1}
mu.x<-curve(seq(.3,1.5,length.out=1000))
lines((seq(.3,1.5,length.out=1000)), mu.x, type="l", lwd=3, col="black")
abline(h=min(mu.x), col="black", lty=3)
abline(h=min(mu.x[seq(.3,1.5,length.out=1000)<0.8]), col="black", lty=3)
abline(h=max(mu.x), col="black", lty=3)

plot(x,y, type = "n", main=paste("Policy Satisfaction"), xlab=("Table Disagreement"), ylab=("Policy Satisfaction Index"))# setting up coord. system
polygon(c(x, rev(x)), c(lower.pot$y, rev(upper.pot$y)), col="lightcyan", border=NA)
lines(upper.pot, type="l", col="lightsteelblue", lty=2)
lines(lower.pot, type="l", col="lightsteelblue", lty=2)
curve<-function(x) {
(-1)*mean(coda.data$g1.2)*x+(-1)*mean(coda.data$g2.2)*x^2+1}
mu.x<-curve(seq(.3,1.5,length.out=1000))
lines((seq(.3,1.5,length.out=1000)), mu.x, type="l", lwd=3, col="black")
abline(h=min(mu.x), col="black", lty=3)
abline(h=min(mu.x[seq(.3,1.5,length.out=1000)<0.8]), col="black", lty=3)
abline(h=max(mu.x), col="black", lty=3)

plot(x,y, type = "n", main=paste("Process Satisfaction"), xlab=("Individual Disagreement"), ylab=("Process Satisfaction Index"))# setting up coord. system
polygon(c(x, rev(x)), c(lower.pri$y, rev(upper.pri$y)), col="lightcyan", border=NA)
lines(upper.pri, type="l", col="lightsteelblue", lty=2)
lines(lower.pri, type="l", col="lightsteelblue", lty=2)
curve<-function(x) {
(-1)*mean(coda.data$g3.1)*x+(-1)*mean(coda.data$g4.1)*x^2+1}
mu.x<-curve(seq(.3,1.5,length.out=1000))
lines((seq(.3,1.5,length.out=1000)), mu.x, type="l", lwd=3, col="black")
abline(h=min(mu.x), col="black", lty=3)
abline(h=min(mu.x[seq(.3,1.5,length.out=1000)<0.8]), col="black", lty=3)
abline(h=max(mu.x), col="black", lty=3)

plot(x,y, type = "n", main=paste("Policy Satisfaction"), xlab=("Individual Disagreement"), ylab=("Policy Satisfaction Index"))# setting up coord. system
polygon(c(x, rev(x)), c(lower.poi$y, rev(upper.poi$y)), col="lightcyan", border=NA)
lines(upper.poi, type="l", col="lightsteelblue", lty=2)
lines(lower.poi, type="l", col="lightsteelblue", lty=2)
curve<-function(x) {
(-1)*mean(coda.data$g3.2)*x+(-1)*mean(coda.data$g4.2)*x^2+1}
mu.x<-curve(seq(.3,1.5,length.out=1000))
lines((seq(.3,1.5,length.out=1000)), mu.x, type="l", lwd=3, col="black")
abline(h=min(mu.x), col="black", lty=3)
abline(h=min(mu.x[seq(.3,1.5,length.out=1000)<0.8]), col="black", lty=3)
abline(h=max(mu.x), col="black", lty=3)









### DISAGREEMENT ANALYZES #############################



## to make a caterpillar plot, extract the HPDs

## Begin Caterpillar:


indexes.table<-seq(from=1361, to=2658, by=13)
coda.data.table<-coda.data[,indexes.table]

indexes.ind<-seq(from=22, to=1317, by=13)
coda.data.ind<-coda.data[,indexes.ind]



HPDs.table<-HPDinterval(as.mcmc(coda.data.table))
HPDs.ind<-HPDinterval(as.mcmc(coda.data.ind))


# extract the quantiles (class is matrix):
mcmcstats.table<-summary(as.mcmc(coda.data.table))
mcmcstats.ind<-summary(as.mcmc(coda.data.ind))

# here is the listing of medians:
mcmcstats.table$quantiles[1:10,3]
mcmcstats.ind$quantiles[1:10,3]



## without parameter names (yes do):
mcmcranked.table<-as.data.frame(matrix(NA,300,nrow=100, ncol=3))
mcmcranked.ind<-as.data.frame(matrix(NA,300,nrow=100, ncol=3))
for (i in 1:100) {
mcmcranked.table[rank(mcmcstats.table$quantiles[,3], ties.method="first")[i],1:2]<-HPDs.table[i,]
mcmcranked.table[rank(mcmcstats.table$quantiles[,3], ties.method="first")[i],3]<-mcmcstats.table$quantiles[i,3]

mcmcranked.ind[rank(mcmcstats.ind$quantiles[,3], ties.method="first")[i],1:2]<-HPDs.ind[i,]
mcmcranked.ind[rank(mcmcstats.ind$quantiles[,3], ties.method="first")[i],3]<-mcmcstats.ind$quantiles[i,3]
}

# for 100 selected parameters (yes do):
par(mfcol=c(1,3), mar=c(4,5,2,0)+0.1, oma=c(0,0,5,0))
plot(rep(1:100),mcmcranked.table[,1], pch=19, main=paste("Table Disagreement"), xlab=("Rank"), ylab=("95 Percent HPD Interval"), ylim=c(0, max(mcmcranked.table[,2])))
points(rep(1:100),mcmcranked.table[,2], pch=19)
for (i in 1:100) {segments(i,mcmcranked.table[i,1],i,mcmcranked.table[i,2])}
for (i in 1:(100-1)) {segments(i,mcmcranked.table[i,3],(i+1),mcmcranked.table[(i+1),3], col="maroon")}
abline(h=mean(coda.data$ideo.sd.mean), lty=2)
text(60, 0.15, "SD = 0.18")
plot(rep(1:100), mcmcranked.ind[,1], pch=19, main=paste("Individual Disagreement"), xlab=("Rank"), ylab=("95 Percent HPD Interval"), ylim=c(min(mcmcranked.ind[,1]), max(mcmcranked.ind[,2])))
points(rep(1:100), mcmcranked.ind[,2], pch=19)
for (i in 1:100) {segments(i,mcmcranked.ind[i,1],i,mcmcranked.ind[i,2])}
for (i in 1:(100-1)) {segments(i,mcmcranked.ind[i,3],(i+1),mcmcranked.ind[(i+1),3], col="maroon")}
abline(h=mean(0.836), lty=2)
text(30, 3.0, "SD = 0.41")
plot(mcmcstats.ind$quantiles[,3], mcmcstats.table$quantiles[,3], pch=19, main=paste("Correlation"), xlab=("Individual Disagreement"), ylab=("Table Disagreement"))
text(2.0, 1.35, "Pearson Corr. =")
text(2.0, 1.30,  "-0.010 (p = 0.918)")
title("Table and Individual Disagreement Measures (100 Random Participants)", line=2, outer=TRUE)



##### begin skip
# with parameter names (don't do):
mcmcnames<-rep(NA,19)
for (i in 1:19) {
mcmcnames[rank(mcmcstats$quantiles[,3])[i]]<-as.character(coda.index$V1)[i]
}
mcmcranked<-as.data.frame(matrix(NA,57,nrow=19, ncol=3, dimnames=list(mcmcnames,c("Low", "High", "Median"))))
for (i in 1:19) {
mcmcranked[rank(mcmcstats$quantiles[,3])[i],1:2]<-HPDs[i,]
mcmcranked[rank(mcmcstats$quantiles[,3])[i],3]<-mcmcstats$quantiles[i,3]
}

# for all parameters (don't do):
par(mfcol=c(1,1), mar=c(5,4,4,2)+0.1)
plot(rep(1:(nparameters)),mcmcranked[,1], pch=19, main=paste("Caterpillar Plot"), xlab=("Rank"), ylab=("95 Percent HPD Interval"))
points(rep(1:(nparameters)),mcmcranked[,2], pch=19)
for (i in 1:(nparameters)) {segments(i,mcmcranked[i,1],i,mcmcranked[i,2])}
for (i in 1:(nparameters-1)) {segments(i,mcmcranked[i,3],(i+1),mcmcranked[(i+1),3], col="maroon")}
abline(h=0)
##### end skip



## end Caterpillar

# qqplots


ideology_intercepts<-read.dta("table_ideology_intercepts_goodsitesonly.dta")
disagreement_intercepts<-read.dta("table_disagreement_intercepts_goodsitesonly.dta")

ideology_intercepts<-na.exclude(ideology_intercepts)
disagreement_intercepts<-na.exclude(disagreement_intercepts)



par(mfcol=c(1,2))
z<-rnorm(1000, mean(ideology_intercepts$tableideomeans1), sd(ideology_intercepts$tableideomeans1))
qqplot(z,ideology_intercepts$tableideomeans1, xlim=c(-1.5,1), ylim=c(-1.5,1), col="maroon", xlab=paste("Theoretical Quantiles"), ylab=("Empirical Quantiles"), main=paste("Table Ideology Means"))
abline(0,1)
text(0.305,-0.97,"KS Test, p=0.61")
text(0.02,-1.325, "Skewness Test, p=0.14")
text(0.1,-1.15, "Kurtosis Test, p=0.94")

z<-rnorm(1000, mean(disagreement_intercepts$tabledissmeans), sd(disagreement_intercepts$tabledissmeans))
qqplot(z,disagreement_intercepts$tabledissmeans, xlim=c(0,2), ylim=c(0,2), col="maroon", xlab=paste("Theoretical Quantiles"), ylab=("Empirical Quantiles"), main=paste("Table Disagreement Means"))
abline(0,1)
text(1.44,0.4,"KS Test, p=0.91")
text(1.21,0.13, "Skewness Test, p=0.67")
text(1.275,0.265, "Kurtosis Test, p=0.63")






mu.eta1<-function(x) {
(-1)*mean(coda.data$g1.ideosd1)*x+(-1)*mean(coda.data$g2.ideosd1)*x^2+4}
mu.eta1.x<-mu.eta1(seq(.3,1.5,length.out=1000))

low.eta1<-function(x) {
(-1)*quantile(coda.data$g1.ideosd1,0.75)*x+(-1)*quantile(coda.data$g2.ideosd1,0.25)*x^2+4}
low.eta1.x<-low.eta1(seq(.3,1.5,length.out=1000))

high.eta1<-function(x) {
(-1)*quantile(coda.data$g1.ideosd1,0.25)*x+(-1)*quantile(coda.data$g2.ideosd1,0.75)*x^2+4}
high.eta1.x<-high.eta1(seq(.3,1.5,length.out=1000))



mu.eta2<-function(x) {
(-1)*mean(coda.data$g1.ideosd2)*x+(-1)*mean(coda.data$g2.ideosd2)*x^2+4}
mu.eta2.x<-mu.eta2(seq(.3,1.5,length.out=1000))


low.eta2<-function(x) {
(-1)*quantile(coda.data$g1.ideosd2,0.75)*x+(-1)*quantile(coda.data$g2.ideosd2,0.25)*x^2+4}
low.eta2.x<-low.eta2(seq(.3,1.5,length.out=1000))

high.eta2<-function(x) {
(-1)*quantile(coda.data$g1.ideosd2,0.25)*x+(-1)*quantile(coda.data$g2.ideosd2,0.75)*x^2+4}
high.eta2.x<-high.eta2(seq(.3,1.5,length.out=1000))



