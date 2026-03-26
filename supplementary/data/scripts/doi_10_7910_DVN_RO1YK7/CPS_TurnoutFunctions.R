#####################################################################
## Replication Code for: Ansolabehere, Fraga, and Schaffner (2021) ######
## Functions for calculating turnout rates and measures of uncertainty ##
#########################################################################

## NOTE: This file is sourced by CPS_TurnoutCalculator.R. You should not need to run this file independently.

##########################
## Baseline CPS Turnout ##
##########################

calcTurn <- function(data, year){

data <- subset(data, Year == year)
data$Race <- as.character(data$Race) # otherwise get Error: Can't join on 'Race' x 'Race' because of incompatible types (character / integer)
data$Hispanic <- as.character(data$Hispanic) # otherwise get Error: Can't join on 'Race' x 'Race' because of incompatible types (character / integer)

require(plyr)
require(dplyr)
require(srvyr)

data_design <- data %>% as_survey_design(
        weights = Weight ,
        strata = Strata ,
        ids = Cluster
    )

## All Racial Groups

total <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC")) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total <- as.data.frame(total)
total$State <- "SixStates"

total_n <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_n <- as.data.frame(total_n)
total_n$State <- "National"

total_s <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC")) %>%
	group_by(State) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_s <- as.data.frame(total_s)

total <- rbind(total_s, total, total_n)
total$Race <- "Total"

## By Race

total_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1")) %>%
	group_by(Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC")) %>%
	group_by(Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))
total_r2 <- subset(total_r2, Race %in% c("0","1"))	
total_r <- as.data.frame(merge(total_r,total_r2, by="Race", all.x=TRUE))
total_r$State <- "SixStates"

total_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1")) %>%
	group_by(State, Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(total_rs, Turnout == 0, select=c(State, Race)) ## makes sure if there are state/groups with zero voters (hispanic, other race) they can just run proportions on Whites and Blacks
total_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & !(State %in% zeroVoters$State)) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs2 <- subset(total_rs2, Race %in% c("0","1"))
if(nrow(zeroVoters) > 0){
total_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs3 <- subset(total_rs3, Race %in% c("0","1"))
total_rs2 <- rbind(total_rs2, total_rs3)
}
total_rs <- as.data.frame(merge(total_rs,total_rs2, by=c("State","Race"), all.x=TRUE))
total_rs[is.na(total_rs)] <- 0

total_r <- rbind(total_r, total_rs)

totalh_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1")) %>%
	group_by(Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
totalh_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1")) %>%
	group_by(Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))	
totalh_r <- as.data.frame(merge(totalh_r,totalh_r2, by="Hispanic", all.x=TRUE))
totalh_r$State <- "SixStates"

totalh_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1")) %>%
	group_by(State, Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(totalh_rs, Turnout == 0, select=c(State, Hispanic)) ## makes sure if there are state/groups with zero voters (hispanic) they can just be dropped
totalh_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & !(State %in% zeroVoters$State)) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
if(nrow(zeroVoters) > 0){
totalh_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State & Hispanic %in% c(0,1)) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
totalh_rs2 <- rbind(totalh_rs2, totalh_rs3)
}
totalh_rs <- as.data.frame(merge(totalh_rs,totalh_rs2, by=c("State","Hispanic"), all.x=TRUE))
totalh_rs[is.na(totalh_rs)] <- 0

totalh_r <- rbind(totalh_r, totalh_rs)
totalh_r <- subset(totalh_r, Hispanic == "1", select=-Hispanic)
totalh_r$Race <- 2

total_r <- rbind(total_r, totalh_r)

total_r$Race[total_r$Race == 0] <- "White"
total_r$Race[total_r$Race == 1] <- "Black"
total_r$Race[total_r$Race == 2] <- "Hispanic"

allrows <- rbind.fill(total, total_r)
allrows$Year <- as.numeric(year)

allrows <- subset(allrows, select=c(Year, State, Race, N, Turnout, Turnout_se, Turnout_low, Turnout_upp, WeightedN_Voters, WeightedN_Voters_se, WeightedN_Voters_low, WeightedN_Voters_upp, Prop_Voters, Prop_Voters_se, Prop_Voters_low, Prop_Voters_upp))
allrows <- allrows[order(allrows$Year, allrows$State, allrows$Race),]

return(allrows)
}

####################################################
## CPS Turnout deleting missing turnout responses ##
####################################################

calcTurn_NoMiss <- function(data, year){

data <- subset(data, Year == year)
data$Race <- as.character(data$Race) # otherwise get Error: Can't join on 'Race' x 'Race' because of incompatible types (character / integer)

# Remove missing cases when calculating turnout
data$Missing <- 0
data$Missing[is.na(data$PES1) | data$PES1 %in% c(-9, -3, 4, -2, -1)] <- 1
data$Missing[data$Missing == 1 & data$PES2 == 2] <- 0

require(plyr)
require(dplyr)
require(srvyr)

data_design <- data %>% as_survey_design(
        weights = Weight ,
        strata = Strata ,
        ids = Cluster
    )
    
## All Racial Groups

total <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total <- as.data.frame(total)
total$State <- "SixStates"
total_n <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Missing == 0) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_n <- as.data.frame(total_n)
total_n$State <- "National"
total_s <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	group_by(State) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_s <- as.data.frame(total_s)

total <- rbind(total_s, total, total_n)
total$Race <- "Total"

## By Race

## By Race

total_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1") & Missing == 0) %>%
	group_by(Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	group_by(Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))
total_r2 <- subset(total_r2, Race %in% c("0","1"))	
total_r <- as.data.frame(merge(total_r,total_r2, by="Race", all.x=TRUE))
total_r$State <- "SixStates"

total_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1") & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(total_rs, Turnout == 0, select=c(State, Race)) ## makes sure if there are state/groups with zero voters (hispanic, other race) they can just run proportions on Whites and Blacks
total_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & !(State %in% zeroVoters$State) & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs2 <- subset(total_rs2, Race %in% c("0","1"))
if(nrow(zeroVoters) > 0){
total_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs3 <- subset(total_rs3, Race %in% c("0","1"))
total_rs2 <- rbind(total_rs2, total_rs3)
}
total_rs <- as.data.frame(merge(total_rs,total_rs2, by=c("State","Race"), all.x=TRUE))
total_rs[is.na(total_rs)] <- 0

total_r <- rbind(total_r, total_rs)

totalh_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
totalh_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))	
totalh_r <- as.data.frame(merge(totalh_r,totalh_r2, by="Hispanic", all.x=TRUE))
totalh_r$State <- "SixStates"

totalh_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(totalh_rs, Turnout == 0, select=c(State, Hispanic)) ## makes sure if there are state/groups with zero voters (hispanic) they can just be dropped
totalh_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & !(State %in% zeroVoters$State) & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
if(nrow(zeroVoters) > 0){
totalh_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State & Hispanic %in% c(0,1) & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
totalh_rs2 <- rbind(totalh_rs2, totalh_rs3)
}
totalh_rs <- as.data.frame(merge(totalh_rs,totalh_rs2, by=c("State","Hispanic"), all.x=TRUE))
totalh_rs[is.na(totalh_rs)] <- 0

totalh_r <- rbind(totalh_r, totalh_rs)
totalh_r <- subset(totalh_r, Hispanic == "1", select=-Hispanic)
totalh_r$Race <- 2

total_r <- rbind(total_r, totalh_r)

total_r$Race[total_r$Race == 0] <- "White"
total_r$Race[total_r$Race == 1] <- "Black"
total_r$Race[total_r$Race == 2] <- "Hispanic"

allrows <- rbind.fill(total, total_r)
allrows$Year <- as.numeric(year)

allrows <- subset(allrows, select=c(Year, State, Race, N, Turnout, Turnout_se, Turnout_low, Turnout_upp, WeightedN_Voters, WeightedN_Voters_se, WeightedN_Voters_low, WeightedN_Voters_upp, Prop_Voters, Prop_Voters_se, Prop_Voters_low, Prop_Voters_upp))
allrows <- allrows[order(allrows$Year, allrows$State, allrows$Race),]

return(allrows)
}

###############################################
## CPS Turnout with Hur and Achen correction ##
###############################################

calcTurn_HurAchen <- function(data, year){

data <- subset(data, Year == year)
data$Race <- as.character(data$Race) # otherwise get Error: Can't join on 'Race' x 'Race' because of incompatible types (character / integer)

# Remove missing cases when calculating turnout
data$Missing <- 0
data$Missing[is.na(data$PES1) | data$PES1 %in% c(-9, -3, 4, -2, -1)] <- 1
data$Missing[data$Missing == 1 & data$PES2 == 2] <- 0

## Post-stratification weight based on McDonald turnout figures, using Hur and Achen (2013) process

HurAchen <- read.csv("HurAchen_WeightAdjustments.csv", stringsAsFactors=FALSE)

data <- merge(data, HurAchen, by=c("Year", "State"), all.x=TRUE)

data$Weight[data$Vote == 1 & data$State %in% c("AL","FL","GA","LA","SC","NC")] <- data$Weight[data$Vote == 1 & data$State %in% c("AL","FL","GA","LA","SC","NC")]*data$VoteWeightFactor[data$Vote == 1 & data$State %in% c("AL","FL","GA","LA","SC","NC")]
data$Weight[data$Vote == 0 & data$State %in% c("AL","FL","GA","LA","SC","NC") & data$Missing == 0] <- data$Weight[data$Vote == 0 & data$State %in% c("AL","FL","GA","LA","SC","NC") & data$Missing == 0]*data$NoVoteWeightFactor[data$Vote == 0 & data$State %in% c("AL","FL","GA","LA","SC","NC") & data$Missing == 0]

##############

require(plyr)
require(dplyr)
require(srvyr)

data_design <- data %>% as_survey_design(
        weights = Weight ,
        strata = Strata ,
        ids = Cluster
    )

## All Racial Groups

total <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total <- as.data.frame(total)
total$State <- "SixStates"
total_n <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Missing == 0) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_n <- as.data.frame(total_n)
total_n$State <- "National"
total_s <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	group_by(State) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_s <- as.data.frame(total_s)

total <- rbind(total_s, total, total_n)
total$Race <- "Total"

## By Race

total_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1") & Missing == 0) %>%
	group_by(Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
total_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Missing == 0) %>%
	group_by(Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))
total_r2 <- subset(total_r2, Race %in% c("0","1"))	
total_r <- as.data.frame(merge(total_r,total_r2, by="Race", all.x=TRUE))
total_r$State <- "SixStates"

total_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Race %in% c("0", "1") & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(total_rs, Turnout == 0, select=c(State, Race)) ## makes sure if there are state/groups with zero voters (hispanic, other race) they can just run proportions on Whites and Blacks
total_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & !(State %in% zeroVoters$State) & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs2 <- subset(total_rs2, Race %in% c("0","1"))
if(nrow(zeroVoters) > 0){
total_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State & Missing == 0) %>%
	group_by(State, Race) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
total_rs3 <- subset(total_rs3, Race %in% c("0","1"))
total_rs2 <- rbind(total_rs2, total_rs3)
}
total_rs <- as.data.frame(merge(total_rs,total_rs2, by=c("State","Race"), all.x=TRUE))
total_rs[is.na(total_rs)] <- 0

total_r <- rbind(total_r, total_rs)

totalh_r <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
totalh_r2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95))	
totalh_r <- as.data.frame(merge(totalh_r,totalh_r2, by="Hispanic", all.x=TRUE))
totalh_r$State <- "SixStates"

totalh_rs <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(N = unweighted(n()), Turnout=survey_mean(Vote, vartype=c("se","ci"), level=0.95), WeightedN_Voters = survey_total(Vote == 1, vartype=c("se","ci")))
zeroVoters <- subset(totalh_rs, Turnout == 0, select=c(State, Hispanic)) ## makes sure if there are state/groups with zero voters (hispanic) they can just be dropped
totalh_rs2 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% c("AL", "FL", "GA", "LA", "NC", "SC") & Hispanic %in% c("0", "1") & !(State %in% zeroVoters$State) & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
if(nrow(zeroVoters) > 0){
totalh_rs3 <- data_design %>%
	filter(Citizen == 1 & Voting_Age == 1 & Vote == 1 & State %in% zeroVoters$State & Hispanic %in% c(0,1) & Missing == 0) %>%
	group_by(State, Hispanic) %>%
	summarize(Prop_Voters=survey_mean(vartype=c("se","ci"), level=0.95, na.rm=TRUE))
totalh_rs2 <- rbind(totalh_rs2, totalh_rs3)
}
totalh_rs <- as.data.frame(merge(totalh_rs,totalh_rs2, by=c("State","Hispanic"), all.x=TRUE))
totalh_rs[is.na(totalh_rs)] <- 0

totalh_r <- rbind(totalh_r, totalh_rs)
totalh_r <- subset(totalh_r, Hispanic == "1", select=-Hispanic)
totalh_r$Race <- 2

total_r <- rbind(total_r, totalh_r)

total_r$Race[total_r$Race == 0] <- "White"
total_r$Race[total_r$Race == 1] <- "Black"
total_r$Race[total_r$Race == 2] <- "Hispanic"

allrows <- rbind.fill(total, total_r)
allrows$Year <- as.numeric(year)

allrows <- subset(allrows, select=c(Year, State, Race, N, Turnout, Turnout_se, Turnout_low, Turnout_upp, WeightedN_Voters, WeightedN_Voters_se, WeightedN_Voters_low, WeightedN_Voters_upp, Prop_Voters, Prop_Voters_se, Prop_Voters_low, Prop_Voters_upp))
allrows <- allrows[order(allrows$Year, allrows$State, allrows$Race),]

return(allrows)
}