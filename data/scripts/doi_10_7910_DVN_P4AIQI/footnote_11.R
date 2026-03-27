# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate result in Footnote 11 (page 15)

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess_ches_merged.rds")


# ----------------------------------------------------------------------------
# DATA ANALYSIS
# ----------------------------------------------------------------------------

# correlation between party positions and voter attitudes on (immigrant) crime, only left-wing parties
cor = cor.test(dat$civlib_laworder[dat$family%in%c("socialist", "rad left", "green")], dat$immi_crime[dat$family%in%c("socialist", "rad left", "green")])

# print the correlation coefficient
print(round(cor$estimate, 3))

