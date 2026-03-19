 # Kristina Victor
 # R script for working paper
 # Started on February 8, 2012
 # Amelia and Genetic Matching
 # April 27, add in Matching (Sekhon)
 
# Loading packages:

library(foreign)
library(Amelia)
library(xtable)
library(MatchIt)
library(Matching)
library(Zelig)


setwd("/Users/bsjjones/Dropbox/Project 1 Threat and Latinos/ptt paper/PewAnalysis2015/PEW2007")

pewdata2007 <- read.dta("2007coreforimputation16.dta")
attach(pewdata2007)

object2 <- summary(pewdata2007)
object2

# Imputation 
a.out1 <- amelia(x=pewdata2007, m=15, cs=NULL, ts=NULL, ords=c("age", "language","pidL", "metro", "region", "incomecats",  "education7",  "adults", "discrimgeneral", "discrimschools", "discrimwork", "ethrespect", "ethgovserv", "ethservice", "married", "worrydeport", "qualityoflife", "childbetter", "compare_yearago", "close_follow", "amount", "helphurt", "issueattention", "issueeffort", "approve_raid", "approve_license", "local_enforce", "attentionpaid", "betterworse", "traveloutside", "useservices", "concern", "bestposition", "impactonhispanics"), noms=c("employed", "citizen", "female", "bias"), p2s=1, frontend=FALSE, set.seed=7654321)

missmap(a.out1)
summary(a.out1$imputations[[15]])
a.out15c <- as.data.frame(a.out1$imputations[[15]])
write.dta(a.out15c, file="imputed2007_16.dta")



# Genetic matching with ratio of 2:1: 

m3 <- matchit(citizen~ age + education + married + female + pid2 + incomecats + spdominant + adults, data=a.out5$imputations[[15]], method="genetic", ratio=2, distance="mahalanobis")

summary(m3, interactions=FALSE, standardize=TRUE)

m.data5 <- match.data(m3)
summary(m.data5)

# Checking for balance in the matched dataset:

mb <- MatchBalance(citizen~age + married + pid2 + female + region + incomecats + spdominant + education + adults, data=pewdata3, match.out=m.data5, nboots=1000)


# Saving the matched data by treatment and control
m.data8 <- match.data(m3, group="treat")
summary(m.data8)

m.data9 <- match.data(m3, group="control")
summary(m.data9)


# Saving the matched data in Stata
write.dta(m.data5, file="matched 300A.dta")

print(m3)
