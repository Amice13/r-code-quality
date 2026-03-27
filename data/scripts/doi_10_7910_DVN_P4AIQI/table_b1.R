# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Table B.1

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("xtable")
library(xtable)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set (change working directory to where it is saved on your computer)
dat = readRDS("data/panel_survey.rds")


# ----------------------------------------------------------------------------
# Analysis
# ----------------------------------------------------------------------------


### Effect of refugees on local crime ###

# create wide data frame with the distribution of values across waves
crime_dat = dat[,c("pseudonym", "ref_loc_crime", "wave")]
crime_dat = crime_dat[order(crime_dat$pseudonym, crime_dat$wave),]
crime_dat = reshape2::dcast(crime_dat, pseudonym~wave, value.var="ref_loc_crime")

#  create indicator for those for whom concern does not change across waves
crime_dat$allequal <- apply(crime_dat[,2:5], 1, 
                            function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

# remove respondents who only participated in one wave
crime_dat$two_plus_waves = rowSums(!is.na(crime_dat[,2:5]))>=2
crime_dat = crime_dat[crime_dat$two_plus_waves==T,]

# merge data set with party voted for
crime_dat = merge(crime_dat, dat[!duplicated(dat$pseudonym),c("pseudonym", "left_any", "green_any", "spd_any", "fdp_any", "cdu_any", "afd_any")])

# create table with number of respondents who changed their concern across waves
crime_tab = data.frame(Sample=c("All respondents", "Linke voter", "Green voter", "SPD voter", "FDP voter", "CDU/CSU voter", "AfD voter"),
                       N=c(nrow(crime_dat), sum(crime_dat$left_any==T), sum(crime_dat$green_any==T), sum(crime_dat$spd_any==T), sum(crime_dat$fdp_any==T), sum(crime_dat$cdu_any==T), sum(crime_dat$afd_any==T)),
                       N_changed=c(sum(crime_dat$allequal==F),
                                   sum(crime_dat$allequal==F & crime_dat$left_any==T), 
                                   sum(crime_dat$allequal==F & crime_dat$green_any==T), 
                                   sum(crime_dat$allequal==F & crime_dat$spd_any==T), 
                                   sum(crime_dat$allequal==F & crime_dat$fdp_any==T), 
                                   sum(crime_dat$allequal==F & crime_dat$cdu_any==T), 
                                   sum(crime_dat$allequal==F & crime_dat$afd_any==T)))

# Calculate the proportion of respondents who changed their concern across waves
crime_tab$changed_prop = round(crime_tab$N_changed/crime_tab$N,2)


### Effect of refugees on the local culture ###

# create wide data frame with the distribution of values across waves
culture_dat = dat[,c("pseudonym", "ref_loc_culture", "wave")]
culture_dat = culture_dat[order(culture_dat$pseudonym, culture_dat$wave),]
culture_dat = reshape2::dcast(culture_dat, pseudonym~wave, value.var="ref_loc_culture")

#  create indicator for those for whom concern does not change across waves
culture_dat$allequal <- apply(culture_dat[,2:5], 1, 
                              function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

# remove respondents who only participated in one wave
culture_dat$two_plus_waves = rowSums(!is.na(culture_dat[,2:5]))>=2
culture_dat = culture_dat[culture_dat$two_plus_waves==T,]

# merge with party voted for
culture_dat = merge(culture_dat, dat[!duplicated(dat$pseudonym),c("pseudonym", "left_any", "green_any", "spd_any", "fdp_any", "cdu_any", "afd_any")])

# create table with number of respondents who changed their concern across waves
culture_tab = data.frame(Sample=c("All respondents", "Linke voter", "Green voter", "SPD voter", "FDP voter", "CDU/CSU voter", "AfD voter"),
                         N=c(nrow(culture_dat), sum(culture_dat$left_any==T), sum(culture_dat$green_any==T), sum(culture_dat$spd_any==T), sum(culture_dat$fdp_any==T), sum(culture_dat$cdu_any==T), sum(culture_dat$afd_any==T)),
                         N_changed=c(sum(culture_dat$allequal==F),
                                     sum(culture_dat$allequal==F & culture_dat$left_any==T), 
                                     sum(culture_dat$allequal==F & culture_dat$green_any==T), 
                                     sum(culture_dat$allequal==F & culture_dat$spd_any==T), 
                                     sum(culture_dat$allequal==F & culture_dat$fdp_any==T), 
                                     sum(culture_dat$allequal==F & culture_dat$cdu_any==T), 
                                     sum(culture_dat$allequal==F & culture_dat$afd_any==T)))

# Calculate the proportion of respondents who changed their concern across waves
culture_tab$changed_prop = round(culture_tab$N_changed/culture_tab$N,2)


### Effect of refugees on the local economy ###

# create wide data frame with the distribution of values across waves
economy_dat = dat[,c("pseudonym", "ref_loc_economy", "wave")]
economy_dat = economy_dat[order(economy_dat$pseudonym, economy_dat$wave),]
economy_dat = reshape2::dcast(economy_dat, pseudonym~wave, value.var="ref_loc_economy")

#  create indicator for those for whom concern does not change across waves
economy_dat$allequal <- apply(economy_dat[,2:5], 1, 
                              function(x) all(x[!is.na(x)] == x[!is.na(x)][1]))

# remove respondents who only participated in one wave
economy_dat$two_plus_waves = rowSums(!is.na(economy_dat[,2:5]))>=2
economy_dat = economy_dat[economy_dat$two_plus_waves==T,]

# merge with party voted for
economy_dat = merge(economy_dat, dat[!duplicated(dat$pseudonym),c("pseudonym", "left_any", "green_any", "spd_any", "fdp_any", "cdu_any", "afd_any")])

# create table with number of respondents who changed their concern across waves
economy_tab = data.frame(Sample=c("All respondents", "Linke voter", "Green voter", "SPD voter", "FDP voter", "CDU/CSU voter", "AfD voter"),
                         N=c(nrow(economy_dat), sum(economy_dat$left_any==T), sum(economy_dat$green_any==T), sum(economy_dat$spd_any==T), sum(economy_dat$fdp_any==T), sum(economy_dat$cdu_any==T), sum(economy_dat$afd_any==T)),
                         N_changed=c(sum(economy_dat$allequal==F),
                                     sum(economy_dat$allequal==F & economy_dat$left_any==T), 
                                     sum(economy_dat$allequal==F & economy_dat$green_any==T), 
                                     sum(economy_dat$allequal==F & economy_dat$spd_any==T), 
                                     sum(economy_dat$allequal==F & economy_dat$fdp_any==T), 
                                     sum(economy_dat$allequal==F & economy_dat$cdu_any==T), 
                                     sum(economy_dat$allequal==F & economy_dat$afd_any==T)))

# Calculate the proportion of respondents who changed their concern across waves
economy_tab$changed_prop = round(economy_tab$N_changed/economy_tab$N,2)


### MERGE THE TABLES ### 

# combine the three tables in one to create the final table
tab = cbind.data.frame(crime_tab$Sample, crime_tab$N, crime_tab$changed_prop, culture_tab$changed_prop, economy_tab$changed_prop)

# edit the column names of the table
names(tab) = c("Sample", "N", "Crime", "Culture", "Economy")

# print Table B.1
print(tab)

# print Table B.1 to export in LaTeX
print(xtable(tab), include.rownames=F)