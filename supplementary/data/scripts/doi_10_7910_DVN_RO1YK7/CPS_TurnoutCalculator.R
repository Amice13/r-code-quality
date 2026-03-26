#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ########
## Calculate turnout rates and measures of uncertainty from raw CPS data ##
###########################################################################

data <- read.csv("CPS_Raw_2008-2018.csv") # 903947 observations

## Remove non-interviews
data <- subset(data, HRINTSTA == 1)

## Create Geographic and Survey Information Variables
data$FIPS_State <- data$GESTFIPS
data$FIPS_County <- data$GTCO
data$FIPS_CBSA <- data$GTCBSA
data$Strata <- paste(data$State, data$GTCO, data$GTCBSA, data$GTINDVPC, sep="-")
## In 2014, looks like an error in that everyone from Hartford, CT CSA was not marked as being in Hartford, except one person. Need to change value for that one person so that they don't mess up clustering.
data$Strata[data$Strata == "CT-0-25540-1" & data$HRYEAR4 == 2014] <- "CT-0-25540-0"
data$Cluster <- paste(data$HRHHID, data$HRHHID2, sep="-")
data$Weight <- data$PWSSWGT

## Create Demographic Variables
data$Year <- data$HRYEAR4
data$Voting_Age <- NA
data$Voting_Age[data$PRTAGE < 18] <- 0
data$Voting_Age[data$PRTAGE >= 18] <- 1
data$Race <- NA
data$Race[data$PTDTRACE == 1 & data$PEHSPNON == 2] <- 0 # non-Hispanic White Alone
data$Race[data$PTDTRACE == 2] <- 1 # Black Alone (includes Hispanics, which is pre-2020 race aggregation style)
data$Hispanic <- 0
data$Hispanic[data$PEHSPNON == 1] <- 1 # Hispanic, of any race
data$Citizen <- NA
data$Citizen[data$PRCITSHP == 1 | data$PRCITSHP == 2 | data$PRCITSHP == 3 | data$PRCITSHP == 4] <- 1
data$Citizen[data$PRCITSHP == 5] <- 0

## Create Turnout Variables
data$Vote <- 0
data$Vote[data$PES1 == 1] <- 1

## Calculate Turnout

install.packages("plyr") # Only need to run this once to install packages
install.packages("dplyr") # Only need to run this once to install packages
install.packages("srvyr") # Only need to run this once to install packages

library(plyr)
library(dplyr)
library(srvyr)

source('CPS_TurnoutFunctions.R')

## Standard CPS Style
data08 <- calcTurn(data, 2008)
data10 <- calcTurn(data, 2010)
data12 <- calcTurn(data, 2012)
data14 <- calcTurn(data, 2014)
data16 <- calcTurn(data, 2016)
data18 <- calcTurn(data, 2018)

write.csv(rbind(data08, data10, data12, data14, data16, data18), "CPS_Turnout_2008-2018.csv", row.names=FALSE, na="")

## CPS with Missing Removed
data08 <- calcTurn_NoMiss(data, 2008)
data10 <- calcTurn_NoMiss(data, 2010)
data12 <- calcTurn_NoMiss(data, 2012)
data14 <- calcTurn_NoMiss(data, 2014)
data16 <- calcTurn_NoMiss(data, 2016)
data18 <- calcTurn_NoMiss(data, 2018)

write.csv(rbind(data08, data10, data12, data14, data16, data18), "CPS_Turnout_2008-2018_NoMiss.csv", row.names=FALSE, na="")

## CPS with Hur and Achen correction
data08 <- calcTurn_HurAchen(data, 2008)
data10 <- calcTurn_HurAchen(data, 2010)
data12 <- calcTurn_HurAchen(data, 2012)
data14 <- calcTurn_HurAchen(data, 2014)
data16 <- calcTurn_HurAchen(data, 2016)
data18 <- calcTurn_HurAchen(data, 2018)

write.csv(rbind(data08, data10, data12, data14, data16, data18), "CPS_Turnout_2008-2018_HurAchen.csv", row.names=FALSE, na="")