## Fraga and Miller (2021) "Who Does Voter ID Keep from Voting?" ##

## Table A1 ##

require(data.table)
require(dplyr)
require(haven)

load('FragaMiller_JOP_ReplicationData.RData')

data <- subset(fragamiller_jopdata, select=c(countyname, REGDATE, Vote2008, Vote2010, Vote2012, Vote2014, Vote2016, RID))

## Table A1, Models 1 and 2: Raw Difference ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m1a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt))$coefficients)
m2a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m1b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt))$coefficients)
m2b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m1c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt))$coefficients)
m2c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt))$coefficients)

## Table A1, Models 5 and 6: Lagged Dependent Variable ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m5a <- as.data.frame(summary(lm(Vote2014 ~ RID + Vote2012 + Vote2010 + Vote2008, data=data_alt))$coefficients)
m6a <- as.data.frame(summary(lm(Vote2014 ~ RID + Vote2012 + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m5b <- as.data.frame(summary(lm(Vote2012 ~ RID + Vote2010 + Vote2008, data=data_alt))$coefficients)
m6b <- as.data.frame(summary(lm(Vote2012 ~ RID + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m5c <- as.data.frame(summary(lm(Vote2010 ~ RID + Vote2008, data=data_alt))$coefficients)
m6c <- as.data.frame(summary(lm(Vote2010 ~ RID + Vote2008 + factor(countyname), data=data_alt))$coefficients)

## Table A1, Model 3: DiD via exact matching w/o County ##

require(MatchIt)
data <- subset(data, RID %in% c(0,1))

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1, select=c(RID, countyname, Vote2008, Vote2010, Vote2012, Vote2014))
data_M <- matchit(RID ~ Vote2012 + Vote2010 + Vote2008, data=data_alt, method="exact")
data_M <- match.data(data_M)
rm(data_alt)
m3a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_M, weights=data_M$weights))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1, select=c(RID, countyname, Vote2008, Vote2010, Vote2012))
data_M <- matchit(RID ~ Vote2010 + Vote2008, data=data_alt, method="exact")
data_M <- match.data(data_M)
rm(data_alt)
m3b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_M, weights=data_M$weights))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1, select=c(RID, countyname, Vote2008, Vote2010))
data_M <- matchit(RID ~ Vote2008, data=data_alt, method="exact")
data_M <- match.data(data_M)
rm(data_alt)
m3c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_M, weights=data_M$weights))$coefficients)

## Table A1, Model 4: DiD via exact matching w/County ##
## NOTE: Generation of weights derived via matching requires parallel processing and >32GB RAM. We have provided the derived weights as objects in the TabA1_Model4Weights.RData file.

load("TabA1_Model4Weights.RData")

m4a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=bigdata14, weights=bigdata14$weights))$coefficients)
m4b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=bigdata12, weights=bigdata12$weights))$coefficients)
m4c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=bigdata10, weights=bigdata10$weights))$coefficients)

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

tab1 <- xtable(cbind(tab1.1, tab1.2, tab1.3, tab1.4, tab1.5, tab1.6))
align(tab1) <- "rcccccc"
print(tab1, booktabs=TRUE, include.rownames=TRUE, sanitize.text.function=identity)