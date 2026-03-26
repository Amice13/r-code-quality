# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate result on Page 21: support for deportations of criminal foreigners (pre-treatment)

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = read.csv("data/conjoint.csv", sep=",")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# create binary variable indicating agreement for the statement that criminal foreigners should be deported
dat$support_deport_agree = dat$support_deport==3 | dat$support_deport==4

# recode party voted last variable
dat$voted_last[dat$voted_last=="Gruene"] = "Greens"
dat$voted_last = factor(dat$voted_last)

# ----------------------------------------------------------------------------
# DATA ANALYSIS
# ----------------------------------------------------------------------------

# percent of respondents agreeing with the statement (=TRUE)
print(round(table(dat$support_deport_agree) / sum(table(dat$support_deport_agree))*100)[2])

# get table percent of respondents agreeing/disagreeing with the statement, by party (=TRUE)
tab_agree = sapply(levels(dat$voted_last), function(x) {
  round(table(dat$support_deport_agree[dat$voted_last==x]) / sum(table(dat$support_deport_agree[dat$voted_last==x]))*100)
})

# turn the table into a data frame
tab_agree = data.frame(t(tab_agree))

# create variable for party voted for in last election
tab_agree$voted_last = row.names(tab_agree)

# get group of voters with lowest agreement to the statement
print(tab_agree[which.min(tab_agree$TRUE.),2:3])
