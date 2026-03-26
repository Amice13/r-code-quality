## Fraga and Miller (2021) "Who Does Voter ID Keep from Voting?" ##

## Table 3 and Table A3 ##

require(data.table)
require(dplyr)
require(haven)

load('FragaMiller_JOP_ReplicationData.RData')

data <- subset(fragamiller_jopdata, select=c(countyname, REGDATE, Vote2008, Vote2010, Vote2012, Vote2014, Vote2016, RID, identifier))

RIDclass <- read_dta("fraga_miller_reasons_replication.dta")
RIDclass <- subset(RIDclass, select=c(identifier, relocation, id_capable, hardship))
RIDclass <- data.table(RIDclass)
data <- merge(data, RIDclass, by="identifier", all.x=TRUE)

data$hardship[is.na(data$hardship)] <- 0
data$hardship <- data$hardship*data$RID
data$hardship[data$RID > 0 & data$hardship == 0] <- NA

data$relocation[is.na(data$relocation)] <- 0
data$relocation <- data$relocation*data$RID
data$relocation[data$RID > 0 & data$relocation == 0] <- NA

data$id_capable[is.na(data$id_capable)] <- 0
data$id_capable <- data$id_capable*data$RID
data$id_capable[data$RID > 0 & data$id_capable == 0] <- NA

data <- subset(data, select=-identifier)

## Table 3, Models 1 and 2: All RID ##
## WARNING: County FE models take a significant amount of time to run, and require > 8GB RAM

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
nrow(data_alt) # Appendix Page 3, 7269723
nrow(subset(data_alt, RID != 0)) # Appendix Page 3, 7559
m1a <- as.data.frame(summary(lm(Vote2014 ~ RID, data=data_alt))$coefficients)
m2a <- as.data.frame(summary(lm(Vote2014 ~ RID + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m1b <- as.data.frame(summary(lm(Vote2012 ~ RID, data=data_alt))$coefficients)
m2b <- as.data.frame(summary(lm(Vote2012 ~ RID + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m1c <- as.data.frame(summary(lm(Vote2010 ~ RID, data=data_alt))$coefficients)
m2c <- as.data.frame(summary(lm(Vote2010 ~ RID + factor(countyname), data=data_alt))$coefficients)

## Table 3, Models 3 and 4: hardship only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m3a <- as.data.frame(summary(lm(Vote2014 ~ hardship, data=data_alt))$coefficients)
m4a <- as.data.frame(summary(lm(Vote2014 ~ hardship + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m3b <- as.data.frame(summary(lm(Vote2012 ~ hardship, data=data_alt))$coefficients)
m4b <- as.data.frame(summary(lm(Vote2012 ~ hardship + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m3c <- as.data.frame(summary(lm(Vote2010 ~ hardship, data=data_alt))$coefficients)
m4c <- as.data.frame(summary(lm(Vote2010 ~ hardship + factor(countyname), data=data_alt))$coefficients)

## Table 3, Models 5 and 6: relocation only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m5a <- as.data.frame(summary(lm(Vote2014 ~ relocation, data=data_alt))$coefficients)
m6a <- as.data.frame(summary(lm(Vote2014 ~ relocation + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m5b <- as.data.frame(summary(lm(Vote2012 ~ relocation, data=data_alt))$coefficients)
m6b <- as.data.frame(summary(lm(Vote2012 ~ relocation + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m5c <- as.data.frame(summary(lm(Vote2010 ~ relocation, data=data_alt))$coefficients)
m6c <- as.data.frame(summary(lm(Vote2010 ~ relocation + factor(countyname), data=data_alt))$coefficients)

## Table 3, Models 7 and 8: id_capable only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m7a <- as.data.frame(summary(lm(Vote2014 ~ id_capable, data=data_alt))$coefficients)
m8a <- as.data.frame(summary(lm(Vote2014 ~ id_capable + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m7b <- as.data.frame(summary(lm(Vote2012 ~ id_capable, data=data_alt))$coefficients)
m8b <- as.data.frame(summary(lm(Vote2012 ~ id_capable + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m7c <- as.data.frame(summary(lm(Vote2010 ~ id_capable, data=data_alt))$coefficients)
m8c <- as.data.frame(summary(lm(Vote2010 ~ id_capable + factor(countyname), data=data_alt))$coefficients)

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

## APPENDIX TABLE A3, Laggged DV approach to Table 3

## Table A3, Models 1 and 2: Lagged Dependent Variable, All RID ##
## WARNING: County FE models take a significant amount of time to run, and require > 8GB RAM

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m1a <- as.data.frame(summary(lm(Vote2014 ~ RID + Vote2012 + Vote2010 + Vote2008, data=data_alt))$coefficients)
m2a <- as.data.frame(summary(lm(Vote2014 ~ RID + Vote2012 + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m1b <- as.data.frame(summary(lm(Vote2012 ~ RID + Vote2010 + Vote2008, data=data_alt))$coefficients)
m2b <- as.data.frame(summary(lm(Vote2012 ~ RID + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m1c <- as.data.frame(summary(lm(Vote2010 ~ RID + Vote2008, data=data_alt))$coefficients)
m2c <- as.data.frame(summary(lm(Vote2010 ~ RID + Vote2008 + factor(countyname), data=data_alt))$coefficients)

## Table A3, Models 3 and 4: Lagged Dependent Variable, hardship only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m3a <- as.data.frame(summary(lm(Vote2014 ~ hardship + Vote2012 + Vote2010 + Vote2008, data=data_alt))$coefficients)
m4a <- as.data.frame(summary(lm(Vote2014 ~ hardship + Vote2012 + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m3b <- as.data.frame(summary(lm(Vote2012 ~ hardship + Vote2010 + Vote2008, data=data_alt))$coefficients)
m4b <- as.data.frame(summary(lm(Vote2012 ~ hardship + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m3c <- as.data.frame(summary(lm(Vote2010 ~ hardship + Vote2008, data=data_alt))$coefficients)
m4c <- as.data.frame(summary(lm(Vote2010 ~ hardship + Vote2008 + factor(countyname), data=data_alt))$coefficients)

## Table A3, Models 5 and 6: Lagged Dependent Variable, relocation only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m5a <- as.data.frame(summary(lm(Vote2014 ~ relocation + Vote2012 + Vote2010 + Vote2008, data=data_alt))$coefficients)
m6a <- as.data.frame(summary(lm(Vote2014 ~ relocation + Vote2012 + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m5b <- as.data.frame(summary(lm(Vote2012 ~ relocation + Vote2010 + Vote2008, data=data_alt))$coefficients)
m6b <- as.data.frame(summary(lm(Vote2012 ~ relocation + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m5c <- as.data.frame(summary(lm(Vote2010 ~ relocation + Vote2008, data=data_alt))$coefficients)
m6c <- as.data.frame(summary(lm(Vote2010 ~ relocation + Vote2008 + factor(countyname), data=data_alt))$coefficients)

## Table A3, Models 7 and 8: Lagged Dependent Variable, id_capable only ##

data_alt <- subset(data, REGDATE <= 20141001 & Vote2016 == 1)
m7a <- as.data.frame(summary(lm(Vote2014 ~ id_capable + Vote2012 + Vote2010 + Vote2008, data=data_alt))$coefficients)
m8a <- as.data.frame(summary(lm(Vote2014 ~ id_capable + Vote2012 + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20121001 & Vote2016 == 1)
m7b <- as.data.frame(summary(lm(Vote2012 ~ id_capable + Vote2010 + Vote2008, data=data_alt))$coefficients)
m8b <- as.data.frame(summary(lm(Vote2012 ~ id_capable + Vote2010 + Vote2008 + factor(countyname), data=data_alt))$coefficients)

data_alt <- subset(data, REGDATE <= 20101001 & Vote2016 == 1)
m7c <- as.data.frame(summary(lm(Vote2010 ~ id_capable + Vote2008, data=data_alt))$coefficients)
m8c <- as.data.frame(summary(lm(Vote2010 ~ id_capable + Vote2008 + factor(countyname), data=data_alt))$coefficients)

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