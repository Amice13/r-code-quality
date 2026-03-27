# James Hollyer
# December 31, 2010
# Purpose: To match leaders that sign PTAs with those that do not from Baccini's
# dataset on PTAs in force.
# Data Input: bacciniACLP_collapsed.dta
# Data Output: bacciniACLP_matched.dta

# Set the working directory to that used for the PTA paper.
setwd("c:/documents and settings/james/desktop/my dropbox/ptas/baccinidata")

# Load the Necessary Packages
library(arm)
library(MatchIt)
library(Matching)
library(foreign)

# Read the data into R
ptas <- read.dta("bacciniACLP_collapsed.dta")
attach(ptas)

# The following will run a genetic matching algorithm on the collapsed Baccini
# data and will run matching diagnostics.

matchptas2 <- matchit(pta_lead_sign ~ democracy + rgdpch + openk + grgdpch, data=ptas, method="nearest", replace=FALSE, caliper=0.5)

summary(matchptas2)
matchptas2$match.matrix

plot(matchptas2, type="jitter")
plot(matchptas2, type="hist")
plot(matchptas2, type="QQ")
summary(matchptas2)

matchPTAs2<-match.data(matchptas2)

write.dta(matchPTAs2, file="baccinimatchedACLP2.dta")