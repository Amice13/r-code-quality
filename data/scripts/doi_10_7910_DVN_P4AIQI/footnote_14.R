# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate result in Footnote 14 on Page 16

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
# DATA ANALYSIS
# ----------------------------------------------------------------------------

# calculate the correlation between the two items asking about the effect of refugees on local crime
# the second item explicitely mentions refugees as those committing the crime, while the first doesn't
cor = cor.test(dat$immi_crime_local, dat$immi_crime_local_2)

# print the correlation coefficient
print(round(cor$estimate, 2))

