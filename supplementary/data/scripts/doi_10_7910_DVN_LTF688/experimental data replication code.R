###
# This file generates the analysis and output for the 
# laboratory experiments presented in
# Ahlquist, John S., John Hamman, and Bradley Jones. 2015.
# "Dependency Status and Social Insurance: evidence from experiments and surveys."
# Political Sciene Research & Methods.
# File created on:07/30/2015 
# Created by: BJ
# Last edited on: 09/08	/2015
# Last edited by: BJ
# Analysis conducted in R 3.1.2 GUI 1.65 Mavericks build (6833)
###

#BEGIN

rm(list = ls())
#path <- "data location"  ###set filepath for replication data
#setwd(path)
path<-"//sscwin/dfsroot/users/bmjones3/Desktop/SocialInsuranceExperiment/AHJreplicationFiles"

library(MASS)
library(boot)
library(rms)
library(ggplot2)
library(stargazer)
library(clusterSEs)
library(xtable)

load("rep_data_exp.RData")

set.seed(132098525)

########TABLE 1: First vote on social insurance
###Models
mod1 <- glm(vote1 ~ risk.cond,
	data=repData, family = binomial(link = 'logit'))
mod1$bic<-BIC(mod1)

mod2 <- glm(vote1 ~ risk.cond + location + gender + cohab +
	work + bYear, data=repData, family = binomial(link = 'logit'))
mod2$bic<-BIC(mod2)

##Table
table1 <-stargazer(mod1, mod2, style="ajps",
 	title = "Logistic regression parameter estimates for first vote on social insurance.",
 	omit.stat = c("rsq"), type = 'text')

########TABLE 2: Second Vote on Social insurance
###Models
##The "treat" variable differs from the "risk.cond" variable in that it
##includes information about the role assignment for individuals in the
##paired single earner condition. This information would not have been 
##available to subjects on the first vote, but it does need to be included
##in the second vote

mod3 <- glm(vote2 ~ treat,
	data=repData, family = binomial(link = 'logit'))

mod4 <- glm(vote2 ~ vote1 + treat,
	data = repData, family = binomial(link = 'logit'))

mod5 <- glm(vote2 ~ treat + location + gender + cohab +
	work + bYear + unempStage1 + punempStage1, data=repData, 
	family = binomial(link = 'logit'))

mod6 <- glm(vote2 ~ vote1 + treat + location + gender + cohab +
	work + bYear + unempStage1 + punempStage1, data=repData, 
	family = binomial(link = 'logit'))

##Table
table2 <-stargazer(mod3, mod4, mod5, mod6, style="ajps",
 	title = "Logistic regression parameter estimates for second vote on social insurance.",
 	omit.stat = c("rsq"), type = 'text')


##Figure 2: Proportions voting for insurance by condition

#calculate means and confidence intervals for vote proportions
vote1.sum<-aggregate(vote1~ condition + Risk, data = repData,
	 FUN= function(x){
	 	temp<-prop.test(sum(x), length(x))
	 	return(c(temp$estimate, temp$conf.int))
	 })
vote2.sum<-aggregate(vote2~ condition + Risk + role1, data = repData,
	 FUN= function(x){
	 	temp<-prop.test(sum(x), length(x))
	 	return(c(temp$estimate, temp$conf.int))
	 })

#set up labels
vote1.sum$role1<-"active"
vote2.sum$role1[vote2.sum$role1==0]<-"passive"
vote2.sum$role1[vote2.sum$role1==1]<-"active"

#reformat to plot
vote1.sum<-as.data.frame(as.matrix(vote1.sum))
vote2.sum<-as.data.frame(as.matrix(vote2.sum))
vote1.sum<- vote1.sum[,c(1,2,6,3,4,5)]
for(i in 4:6){
	vote1.sum[,i]<-as.numeric(as.character(vote1.sum[,i]))
	vote2.sum[,i]<-as.numeric(as.character(vote2.sum[,i]))
}

#more labels
names(vote1.sum) <- names(vote2.sum) <- c("condition", "risk","role", "mean", "upper","lower")
votes<-rbind(vote1.sum, vote2.sum)
votes$vote<-c(rep("1st vote", 6), rep("2nd vote", 8))
votes$groups<-paste(votes$risk,", ", votes$role, sep="")

#plotting commands
pd <- position_dodge(.4)
p1<-ggplot(votes, aes(y=mean, x=vote, color = groups, shape = groups)) 
p1<-p1 + facet_grid(.~condition) + geom_point(size=3, position = pd) + 
	geom_errorbar(aes(ymax = upper, ymin=lower), width=0.2, position=pd) +
	theme_bw() + labs(x=NULL, y="proportion voting for insurance")
p1


##Figure 3: Relative risk of voting for insurance in the second round

##set up dummy datasets for simulation
X.ls <- c(1, #constant
	0,#HPD
	0,#HPS-P
	0,#HPS-A
	0,#HS
	0,#LPD
	0,#LPS-P
	0#LPS-A
	)
X.lpd <- c(1, #constant
	0,#HPD
	0,#HPS-P
	0,#HPS-A
	0,#HS
	1,#LPD
	0,#LPS-P
	0#LPS-A
	)
X.lpsa <- c(1, #constant
	0,#HPD
	0,#HPS-P
	0,#HPS-A
	0,#HS
	0,#LPD
	0,#LPS-P
	1#LPS-A
	)
X.lpsp <- c(1, #constant
	0,#HPD
	0,#HPS-P
	0,#HPS-A
	0,#HS
	0,#LPD
	1,#LPS-P
	0#LPS-A
	)
X.hpsp <- c(1, #constant
	0,#HPD
	1,#HPS-P
	0,#HPS-A
	0,#HS
	0,#LPD
	0,#LPS-P
	0#LPS-A
	)
X.hpsa <- c(1, #constant
	0,#HPD
	0,#HPS-P
	1,#HPS-A
	0,#HS
	0,#LPD
	0,#LPS-P
	0#LPS-A
	)
X.hpd <- c(1, #constant
	1,#HPD
	0,#HPS-P
	0,#HPS-A
	0,#HS
	0,#LPD
	0,#LPS-P
	0#LPS-A
	)
X.hs <- c(1, #constant
	0,#HPD
	0,#HPS-P
	0,#HPS-A
	1,#HS
	0,#LPD
	0,#LPS-P
	0#LPS-A
	)

##extract coefficients and var-cov matrix from model 3
b.2<-coef(mod3)
sig.2<-vcov(mod3)

##get draws from multivariate normal
B.2<-mvrnorm(10000,b.2,sig.2)

##calculate predicted probabilities
phat.pd.low2<-inv.logit(B.2%*%X.lpd)
phat.pd.hi2<-inv.logit(B.2%*%X.hpd)
phat.ps.ndep.low2<-inv.logit(B.2%*%X.lpsa)
phat.ps.ndep.hi2<-inv.logit(B.2%*%X.hpsa)
phat.ps.dep.low2<-inv.logit(B.2%*%X.lpsp)
phat.ps.dep.hi2<-inv.logit(B.2%*%X.hpsp)
phat.s.hi2<-inv.logit(B.2%*%X.hs)
phat.s.low2<-inv.logit(B.2%*%X.ls)

##calculate relative risk of different conditions compared
## against single earner condition
rr.low<-list(phat.pd.low2/phat.s.low2,
	phat.ps.ndep.low2/phat.s.low2, 
	phat.ps.dep.low2/phat.s.low2)
rr.hi<-list(phat.pd.hi2/phat.s.hi2, 
	phat.ps.ndep.hi2/phat.s.hi2, 
	phat.ps.dep.hi2/phat.s.hi2)

##get confidence intervals
rr.low.df<-rr.hi.df<-NULL
for(i in 1:length(rr.low)){
	rr.low.df<-rbind(rr.low.df,quantile(rr.low[[i]], c(.025,.5,.975)))
	rr.hi.df<-rbind(rr.hi.df,quantile(rr.hi[[i]], c(.025,.5,.975)))
}
rr.low.df<-data.frame(rr.low.df)
rr.hi.df<-data.frame(rr.hi.df)
names(rr.hi.df)<-names(rr.low.df)<-c("rrlow","rr","rrhi")
rr.low.df$cond<-rr.hi.df$cond<-c("PairedDual", "PairedSingle \n active", "PairedSingle \n passive")
rr.low.df$risk<-"low"
rr.hi.df$risk<-"high"

rr.df<-rbind(rr.low.df,rr.hi.df)

rr.plot<-ggplot(rr.df ,aes(y=rr, x=cond, color = risk)) 
rr.plot<-rr.plot + geom_point(size=4, position = pd) + 
	geom_errorbar(aes(ymax = rrhi, ymin=rrlow), width=0.2, position=pd, size=1.5) +
	theme_bw() + geom_hline(yintercept=1, linetype=2) +
	labs(x=NULL, y="Relative risk",
		title="Relative risk of voting for insurance on 2nd vote \n compared to single earner")
rr.plot

###########Difference of means tests for voting

##first vote
tab <- table(repData$vote1, repData$Risk)
prop.test(tab[2,], colSums(tab), correct = FALSE)

##second vote
tab <- table(repData$vote2, repData$Risk)
prop.test(tab[2,], colSums(tab), correct = FALSE)

##################################################Appendix Material

###Tables

#Participants by session type and location
table(repData$Risk, repData$condition, repData$location)

#Participants in the paired, single earner treatment by stage I role, risk, and location
table(repData$Risk, repData$role1, repData$location, repData$condition)[,,,3]

#Proportions voting for insurance at the first vote by dependency treatment and risk level
vote1.tab <- aggregate(vote1 ~ treat + Risk, data = repData, FUN = mean)
#counts
vote1.tab <- cbind(vote1.tab, 
	n = aggregate(rep(1, nrow(repData)) ~ treat + Risk, 
	data = repData, FUN = sum)[,3])
vote1.tab

#Proportions voting for insurance at the second vote by dependency treatment and risk level
vote2.tab <- aggregate(vote2 ~ treat + Risk, data = repData, FUN = mean)
#counts
vote2.tab <- cbind(vote2.tab, 
	n = aggregate(rep(1, nrow(repData)) ~ treat + Risk, 
	data = repData, FUN = sum)[,3])
vote2.tab

########Effort Plots

#####Evidence of learning during the experiment
roundData$zeff <- NA
for (j in 1:max(roundData$id)) {
	#standardize effort within subjects excluding any that showed no
	#effort or maxed out
	wh <- which(roundData$id == j & roundData$effort < 48 &
		roundData$effort > 0)
	roundData$zeff[wh] <- scale(roundData$effort[wh])
}

par(mfrow = c(2, 2), mar = c(2.5, 2, 4, 1))
plot(0,0, pch='', xlim = c(1,24),
	ylim = c(-3,3), xlab='round', ylab = 
	'Standardized Effort', main = "All Conditions")

for (j in 1:max(roundData$id)) {
	inds <- which(roundData$id == j)
	lines(roundData$round[inds], roundData$zeff[inds], col=grey(.7, .2))
}
avg <- aggregate(roundData$zeff ~ roundData$round, FUN = mean, na.rm=TRUE)
lines(avg[,1], avg[,2], lwd=2)

###plot hi and low risk
lo <- subset(roundData, risk == "low")
lo.avg <- aggregate(lo$zeff ~ lo$round, FUN = mean, na.rm=TRUE)
lines(lo.avg[,1], lo.avg[,2], lwd=2, lty=2, col='blue')

hi <- subset(roundData, risk == "high")
hi.avg <- aggregate(hi$zeff ~ hi$round, FUN = mean, na.rm=TRUE)
lines(hi.avg[,1], hi.avg[,2], lwd=2, lty=3, col='red')

for (k in c("PairedDual", "PairedSingle", "Single")) {

	plot(0,0, pch='', xlim = c(1,24),
		ylim = c(-3,3), xlab='', ylab = 
		'Standardized Effort', main = k)
	
	dat <- subset(roundData, condition == k)
	ids <- unique(dat$id)
	for (j in ids) {
		inds <- which(dat$id == j)
		lines(dat$round[inds], dat$zeff[inds], col=grey(.5, .2))
	}

avg <- aggregate(dat$zeff ~ dat$round, FUN = mean, na.rm=TRUE)
lines(avg[,1], avg[,2], lwd=2)


###plot hi and low risk
lo <- subset(roundData, risk == "low" & condition==k)
lo.avg <- aggregate(lo$zeff ~ lo$round, FUN = mean, na.rm=TRUE)
lines(lo.avg[,1], lo.avg[,2], lwd=2, lty=2, col='blue')

hi <- subset(roundData, risk == "high" & condition==k)
hi.avg <- aggregate(hi$zeff ~ hi$round, FUN = mean, na.rm=TRUE)
lines(hi.avg[,1], hi.avg[,2], lwd=2, lty=3, col='red')

}

legend(14.5, -1.25, c("All", "Low Risk", "High Risk"),
	lty = c(1, 2, 3), col = c("black", "blue", "red"),
	lwd = 2, bty = 'n')


###############Task effort by treatment

####Function to draw bootstrap samples from clustered data
bootSample <- function(dat) {
	n.groups <- table(dat$session_id)
	###sample from groups with replacement
	s1 <- sample(1:length(n.groups), size = length(n.groups), replace=TRUE)
	###sample from within the groups
	insamp <- which(is.element(dat$session_id, names(n.groups)))
	s2 <- sample(insamp, size = length(insamp), replace = TRUE)
	mu <- mean(dat$effort2[s2], na.rm=TRUE)
	return(mu)
}

#####Function to add standard errors to plot
addSE <- function(dat, col, w = .975, poly=FALSE, boot = FALSE) {
	means <- aggregate(dat$effort ~ dat$round, FUN = mean, na.rm=TRUE)
	s <- aggregate(dat$effort ~ dat$round, FUN = sd, na.rm=TRUE)
	n <- aggregate(!is.na(dat$effort) ~ dat$round, FUN = sum)
	err <- qt(w, df=n[,2]-1)*s[,2]/sqrt(n[,2])
	up <- means[,2]+err
	lo <- means[,2]-err

if (boot) {
	up <- lo <- rep(NA, length(err))
	for (j in 1:length(err)) {
		d <- subset(dat, round == j & !is.na(effort2))
		means <- replicate(1000, bootSample(d))
		up[j] <- quantile(means, w)
		lo[j] <- quantile(means, 1-w)
	}
}

	if (!poly) segments(x0 = 1:length(lo), x1 = 1:length(lo), 
		y0 = lo, y1= up, col = col)
	if (poly) polygon(x = c(1:length(lo), length(lo):1),
		y = c(up, rev(lo)), col = col, border=FALSE)
}

j <- 1
##########Function to generate the plot
par(mfrow = c(2,2), mar = c(2.5, 2, 4, 1))
	plot(0,0, pch='', xlim = c(1,24),
		ylim = c(15,25), xlab='', ylab = 
		'Average Effort', main = "Combined")
for (i in c("low", "high")) {
	dat2 <- subset(roundData, risk == i)
	wh <- which(dat2$effort < 1 | dat2$effort > 47)
	dat2$effort[wh] <- NA
	lty <- ifelse(i == "high", 19, 1)
	lty2 <- ifelse(i == "high", 1, 2)
	avg <-  aggregate(dat2$effort ~ dat2$round, FUN = mean, na.rm=TRUE)
	addSE(dat2, grey(.7, .5), w=.9, poly=TRUE, boot=FALSE)
	lines(avg[,1], avg[,2], col = 'black', lty = lty2)
	#points(avg[,1], avg[,2], col = 'black', pch=lty)
}
col <- c("red", "red", "blue", "blue", "green", "green")
for (k in c("PairedDual", "PairedSingle", "Single")) {
	plot(0,0, pch='', xlim = c(1,24),
		ylim = c(15,25), xlab='', ylab = 
		'Average Effort', main = k)
for (i in c("low", "high")) {
	dat2 <- subset(roundData, condition == k & risk == i)
	wh <- which(dat2$effort < 1 | dat2$effort > 47)
	dat2$effort[wh] <- NA
	addSE(dat2, grey(.7, .5), w=.9, poly=TRUE, boot = FALSE)
	lty <- ifelse(i == "high", 19, 1)
	lty2 <- ifelse(i == "high", 1, 2)
	avg <-  aggregate(dat2$effort ~ dat2$round, FUN = mean, na.rm=TRUE)
	lines(avg[,1], avg[,2], col = 'black', lty = lty2)
	#points(avg[,1], avg[,2], col = col[j], pch=lty)
	j <- 1+j
}
}
legend(15, 16.5, c("High Risk", "Low Risk"),
	pch = c(19,1), cex=.8, bty="n", lty=c(1, 2))

#END
