###
# This file generates the analysis and output for the 
# observational survey data presented in
# Ahlquist, John S., John Hamman, and Bradley Jones. 2015.
# "Dependency Status and Social Insurance: evidence from experiments and surveys."
# Political Sciene Research & Methods.
# File created on:07/30/2015 
# Created by: BJ
# Last edited on: 09/08/2015
# Last edited by: BJ
# Analysis conducted in R 3.1.2 GUI 1.65 Mavericks build (6833)
###

#BEGIN

rm(list = ls())
path<-"//sscwin/dfsroot/users/bmjones3/Desktop/SocialInsuranceExperiment/AHJreplicationFiles"

#path <- "data location"  ###set path to replication data file location
#setwd(path)


###load required packages
library(foreign)
library(Zelig)
library(ZeligChoice)
library(stargazer)
###set random seed for replication
set.seed(132098525)

###load data
load("rep_data_gss.RData")

###Table 3
#get the subset of complete cases
mis <- which(is.na(rowSums(repData[,c("pairedDual","activeSingle",
	"passivSingle","singleOut","pairedOut","selfUnemp","spouUnemp", 
	"unemp_rate","skill.max","female","anykids",
	"age","incMid","white","collGrad","ideo",
	"party","attend","y1990","y1996","y2006")])))

mod7 <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
mod8 <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
mod9 <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.max + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
mod10 <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.max + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])

table3 <- stargazer(mod7, mod8, mod9, mod10,
	single.row=TRUE, no.space=TRUE,
	type='text')


######Figure 4

###estimates from model 10
z.out <- zelig(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.max + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	model = "oprobit", data=repData[-mis,])

###simulate values for pure independents
x.low <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 0, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = 4.2,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)
###simulate values for Weak Democratic identifiers
x.high <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 0, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = 4.2,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = -2,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)

s.out <- sim(z.out, x = x.low, x1 = x.high, num=10000)
summary(s.out)

##combine the "much more" and "some more" predicted values
much <- s.out$qi[[5]][,5]
some <- s.out$qi[[5]][,4]

par(mar = c(2.5,4,4,1))
plot(density(much+some, bw=.01), xlim = c(-.1, .2), ylim=c(0,30), main =
	"Effect of Dependency Status on 
Support for Unemployment Spending", lty = 2,
	xlab = "")
abline(v = 0, col=grey(.3))
text(.10, 25,
	expression(atop(
		Pr * "(" * "More" ~ symbol("\332") ~ "Much more " * "| S, Wk. Dem.) -",
		Pr * "(" * "More" ~ symbol("\332") ~ "Much more " * "| S, Pure Ind.)")),
	cex = 0.8)

###get predictions for single worker
x.low <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 0, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = 4.2,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)
###get predictions for passive member of a single earner household with
##similar demographics
x.high <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 1, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = 4.2,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)


s.out <- sim(z.out, x = x.low, x1 = x.high, num=10000)
summary(s.out)


much <- s.out$qi[[5]][,5]
some <- s.out$qi[[5]][,4]

lines(density(much+some, bw=.01))
text(.12, 12.5,
	expression(atop(
		Pr * "(" * "More" ~ symbol("\332") ~ "Much more " * "| S) -",
		Pr * "(" * "More" ~ symbol("\332") ~ "Much more " * "| PS - passive)")),
	cex = 0.8)


legend(-.1, 10, c("Dependency", "Partisanship"),
	lty = c(1,2), col = c('black','black'))


#######Figure 5
##find average income of single earner households
inds <- which(repData$pairedDual == 0 & repData$singleOut == 0
	& repData$pairedOut == 0 & !is.na(repData$incMid))
mu.single <- mean(repData$incMid[inds])

##find average dual earner income
inds <- which(repData$pairedDual == 1)
mu.dual <- mean(repData$incMid[inds], na.rm=TRUE)

x.low <- setx(z.out, pairedDual = 1, activeSingle = 0,
	passivSingle = 0, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = mu.dual,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)
x.high <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 1, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = mu.single,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)

s.out <- sim(z.out, x = x.low, x1 = x.high, num=10000)
summary(s.out)


much <- s.out$qi[[5]][,5]
some <- s.out$qi[[5]][,4]

par(mar = c(2.5,4,4,1))
plot(density(much+some, bw=.01), xlim = c(-.1, .2), ylim=c(0,13), main =
	"Effect of Dependency Status on 
Support for Unemployment Spending", lty = 1,
	xlab = "")
abline(v = 0, col=grey(.3))

x.low <- setx(z.out, pairedDual = 0, activeSingle = 1,
	passivSingle = 0, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = mu.single,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)
x.high <- setx(z.out, pairedDual = 0, activeSingle = 0,
	passivSingle = 1, singleOut = 0, pairedOut = 0,
	selfUnemp = 0, spouUnemp = 0, anykids = 0, age = 45, incMid = mu.single,
	white = 1, collGrad = 0, female = 1, ideo = 0, party = 0,
	attend = 3, y1990 = 0, y1996 = 0, y2006 = 0,
	unemp_rate = 5.5, skill.max = 1.1)

s.out <- sim(z.out, x = x.low, x1 = x.high, num=10000)
summary(s.out)


much <- s.out$qi[[5]][,5]
some <- s.out$qi[[5]][,4]

lines(density(much+some, bw = .01), lty=2)
#fillDen(density(much+some), col = grey(.2, .2))

legend(.05, 13, c("Passive Single - Paired Dual", 
	"Passive Single - Active Single"),
	lty = c(1,2), col = c("black","black"))

#################################Appendix Material

###########mapping observational categories to treatment
N <- rep(1, nrow(repData))
#get the subset of complete cases
wh <- which(rowSums(is.na(repData[,c("pairedDual","activeSingle",
	"passivSingle","singleOut","pairedOut","selfUnemp","spouUnemp", 
	"unemp_rate","skill.max","female","anykids","spunemp",
	"age","incMid","white","collGrad","ideo",
	"party","attend","y1990","y1996","y2006")]))==0)
tab <- aggregate(N[wh] ~  treat+wrkstat+spwrksta, data = repData[wh,], FUN=sum)

lev2 <- c('iap','Working full time','Working part time',
	'Temporarily not working', 'Unemployed/Laid off',
	'Retired', 'Student', 'Homemaker', 'Other (not working)',
	'No Spouse')
levels(tab$wrkstat) <- levels(tab$spwrksta) <- lev2

colnames(tab) <- c("Category", "Own Employment", "Spouse Employment", "N")
rownames(tab) <- NULL
ord <- order(tab[,1],tab[,2],tab[,3])
tab[ord,]
tab$Category <- gsub("^[0-9]\\. ", "", tab$Category)
print(xtable(tab[ord,], digits = 0), include.rownames=FALSE)

###########Placebo tests
plac <- c("spenviro", "sphlth", "sppolice", "spschool",
	"sparms","spretire","sparts")
mis <- which(rowSums(t(apply(repData[,c(plac,"pairedDual",
	"activeSingle","passivSingle","singleOut","pairedOut",
	"selfUnemp","spouUnemp","unemp_rate","skill.max","female",
	"anykids","age","incMid","white","collGrad","ideo","party",
	"attend","y1990","y1996","y2006")], 1, is.na)))>0)

	
placebo <- list(NULL)
for (j in 1:length(plac)) {
repData$y <- repData[[plac[j]]]
placebo[[j]] <- lrm(y ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.max + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
placebo[[j]] <- robcov(placebo[[j]], cluster=repData$clus[-mis])
}

placebo.table <- stargazer(placebo,
	single.row=TRUE, no.space=TRUE,
	type='text')


##########alternative specifications of skill specificity

#get the subset of complete cases
mis <- which(is.na(rowSums(repData[,c("pairedDual","activeSingle",
	"passivSingle","singleOut","pairedOut","selfUnemp","spouUnemp", 
	"unemp_rate","skill.max","female","anykids",
	"age","incMid","white","collGrad","ideo",
	"party","attend","y1990","y1996","y2006")])))

mod10 <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.max + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
mod10a <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.min + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])
mod10b <- lrm(spunemp ~ pairedDual + activeSingle +
	passivSingle + singleOut + pairedOut + selfUnemp + spouUnemp 
	+ unemp_rate + skill.avg + female + anykids +
	age + incMid + white + collGrad + ideo +
	party + attend + y1990 + y1996 + y2006,
	x = TRUE, y = TRUE, data = repData[-mis,])

skill.spec <- stargazer(mod10, mod10a, mod10b,
	single.row=TRUE, no.space=TRUE,
	type='text')

##END