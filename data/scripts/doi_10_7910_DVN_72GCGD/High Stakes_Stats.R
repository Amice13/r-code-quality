# Code for replicating the descriptive statistics in
# "High Stakes and Low Bars: 
# How International Recognition Shapes the Conduct of Civil Wars"
# by Marika Landau-Wells
# mlw@mit.edu
# July 3, 2018
# R version 3.4.1 (2017-06-30)

# Using this code requires 2 files available from the Dataverse repository:
# Wars.csv
# Recognitions.csv

library(foreign)
library(dplyr)


# Please customize the filepath for your purposes and load the data
#setwd()
wars <- read.csv("Wars.csv", stringsAsFactors = F)
recogs <- read.csv("Recognitions.csv", stringsAsFactors = F)

#########

## 1. Descriptive Statistics for All Wars (N=61) ####

# Number of extra-constitutional transfers
sum(wars$extracon) #18

# Number of premature bilateral recognitions 
sum(wars$prem_bilat) #16

# Number of premature multilateral recognitions
sum(wars$prem_multi) #13

# Conflicts in which either
# 50% of capital city infrastructure destroyed
# or significant portion of the city's population displaced (or both)
sum(wars$cont_cap) #19 contested capitals
sum(wars$dd_cap) #12 with significant damage and/or displacement

# Years of war after first bilateral recognition
premwars <- filter(recogs, prem_bilat>0) # Use premature recognitions only

median(premwars$bilat1_wardays)/365.25 #6.8
mean(premwars$bilat1_wardays)/365.25 #9.7

# Percentage of wars with no extra-constitutional transfers
1 - sum(wars$extracon > 0)/length(wars$extracon) #75.4%



#############

## 2. Descriptive Statistics for Wars not used in Case Studies (N=56) ####

# "The Remaining Cases" exclude Cambodia I, Chad I, Chad II, Angola, and Libya
excl <- c("Cambodia I", "Chad I", "Chad II", "Angola", "Libya") #vector of conflicts to delete
remwars <- filter(wars, !conflict %in% excl) #remaining wars
remrecogs <- filter(recogs, !conflict %in% excl) #remaining recognitions

# Number of extra-constitutional transfers
sum(remwars$extracon) #12

# Number of premature bilateral recogntions
sum(remwars$prem_bilat) #11

# Number of premature recognitions where capital was held prior to bilateral recognition
pre.remrecogs <- filter(remrecogs, !conflict %in% c("South Vietnam")) # filter out S. Vietnam (not premature)
sum(pre.remrecogs$bilat1_capdays >=0) #9

# Days between capital seizure and first bilateral recogntion (all remaining recognitions)
median(remrecogs$bilat1_capdays) #2
mean(remrecogs$bilat1_capdays) #-4.25

# Conflicts in countries with natural resources
sum(remwars$res_conf) #24

# Rebel control over natural resources and third-party ally state
sum(remwars$rebc_ally) #14

# Premature recognition of rebels with control over resources
sum(remwars$rebc_pr) #2

# Number of conflicts where capital was contested at some point
sum(remwars$cont_cap) #16

# Number of wars in which a rebel group contests the capital 
# and wins widespread recogntion
capwins <- filter(remwars, extracon > 0 & cont_cap == 1)
sum(capwins$recog_multi) #7
#NOTE: While bilateral recognitions include the Taliban/Afgh case because they 
#succeed in limited recognition (Pakistan + 2 others), I exclude that case from 
#the count of "widespread recognitions" (therefore, using 7 instead of 8) 
#because the level of bilateral recognition they achieved was minimal and multilateral
#was withheld

# Number of instances in which rebel group achieves widespread recognition 
# after prioritizing capital seizure
sum(capwins$prior_cap) -1 #5 
#NOTE: for same reason as above with respect to the Taliban, I exclude the case
#from this count (hence the -1) 

# Number of victories without contesting the capital
nocapwins <- filter(remwars, cont_cap == 0)
length(nocapwins$extracon) #40 wars
sum(nocapwins$extracon) #4 recognitions

# Conflicts in which either
# 50% of capital city infrastructure destroyed
# or significant portion of the city's population displaced (or both)
sum(remwars$dd_cap) #11 wars

# Years of war after first bilateral recognition (all remaining cases)
sum(remrecogs$prem_bilat, remrecogs$coterm_bilat) #12
median(remrecogs$bilat1_wardays)/365.25 #4.9 
mean(remrecogs$bilat1_wardays)/365.25 #7.5
