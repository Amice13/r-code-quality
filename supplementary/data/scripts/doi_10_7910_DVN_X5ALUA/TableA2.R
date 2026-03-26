## Fraga and Miller (2021) "Who Does Voter ID Keep from Voting?" ##

## Table A2 ##

require(data.table)
require(dplyr)
require(haven)

load('FragaMiller_JOP_ReplicationData.RData')

data <- subset(fragamiller_jopdata, select=c(countyname, REGDATE, Vote2008, Vote2010, Vote2012, Vote2014, Vote2016, RID, pred_whi, pred_bla, pred_his, pred_asi))

## Table A2, Models 1 and 2: Whites ##
## WARNING: County FE models take a significant amount of time to run, and require > 8GB RAM

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m1a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt, weights=pred_whi))$coefficients)
m2a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt, weights=pred_whi))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m1b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt, weights=pred_whi))$coefficients)
m2b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt, weights=pred_whi))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m1c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt, weights=pred_whi))$coefficients)
m2c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt, weights=pred_whi))$coefficients)

## Table A2, Models 3 and 4: Blacks ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m3a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt, weights=pred_bla))$coefficients)
m4a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt, weights=pred_bla))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m3b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt, weights=pred_bla))$coefficients)
m4b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt, weights=pred_bla))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m3c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt, weights=pred_bla))$coefficients)
m4c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt, weights=pred_bla))$coefficients)

## Table A2, Models 5 and 6: Latinos ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m5a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt, weights=pred_his))$coefficients)
m6a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt, weights=pred_his))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m5b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt, weights=pred_his))$coefficients)
m6b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt, weights=pred_his))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m5c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt, weights=pred_his))$coefficients)
m6c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt, weights=pred_his))$coefficients)

## Table A2, Models 7 and 8: Asians ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m7a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt, weights=pred_asi))$coefficients)
m8a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt, weights=pred_asi))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m7b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt, weights=pred_asi))$coefficients)
m8b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt, weights=pred_asi))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m7c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt, weights=pred_asi))$coefficients)
m8c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt, weights=pred_asi))$coefficients)

## Create Formatted LaTeX Table of Results ##

require(xtable)

forTable <- function(x){
	coef <- x[2,1]
	coefhi <- coef+(1.96*x[2,2])
	coeflo <- coef-(1.96*x[2,2])
	coefCI <- paste("\\footnotesize{[",round(coeflo, 3),",",round(coefhi,3),"]}", sep="")
	return(rbind(round(coef,3), coefCI))
}

tab1.1 <- rbind(forTable(m1a), forTable(m1b), forTable(m1c))
rownames(tab1.1) <- c("2014","","2012","","2010","")
tab1.2 <- rbind(forTable(m2a), forTable(m2b), forTable(m2c))
tab1.3 <- rbind(forTable(m3a), forTable(m3b), forTable(m3c))
tab1.4 <- rbind(forTable(m4a), forTable(m4b), forTable(m4c))
tab1.5 <- rbind(forTable(m5a), forTable(m5b), forTable(m5c))
tab1.6 <- rbind(forTable(m6a), forTable(m6b), forTable(m6c))
tab1.7 <- rbind(forTable(m7a), forTable(m7b), forTable(m7c))
tab1.8 <- rbind(forTable(m8a), forTable(m8b), forTable(m8c))

tab1 <- xtable(cbind(tab1.1, tab1.2, tab1.3, tab1.4, tab1.5, tab1.6, tab1.7, tab1.8))
align(tab1) <- "rcccccccc"
print(tab1, booktabs=TRUE, include.rownames=TRUE, sanitize.text.function=identity)