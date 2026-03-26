# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure B.6

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2")
library(ggplot2)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/panel_survey.rds")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# For each group of left-wing voters, generate a data frame with the average perceived issue fit on crime with each major party, by wave
crime_fit_left <- reshape2::melt(aggregate(cbind(fit_left_crime, fit_greens_crime, fit_spd_crime, fit_fdp_crime, fit_cdu_crime, fit_afd_crime)~left_any + wave, dat[dat$left_any==1,], mean), id=c("left_any", "wave"))
crime_fit_greens <- reshape2::melt(aggregate(cbind(fit_left_crime, fit_greens_crime, fit_spd_crime, fit_fdp_crime, fit_cdu_crime, fit_afd_crime)~green_any + wave, dat[dat$green_any==1,], mean), id=c("green_any", "wave"))
crime_fit_spd <- reshape2::melt(aggregate(cbind(fit_left_crime, fit_greens_crime, fit_spd_crime, fit_fdp_crime, fit_cdu_crime, fit_afd_crime)~spd_any + wave, dat[dat$spd_any==1,], mean), id=c("spd_any", "wave"))

# combine the data frames in one data frame
crime_fit <- rbind.data.frame(crime_fit_left[,2:4], crime_fit_greens[,2:4], crime_fit_spd[,2:4])

# variable indicating party voted for
crime_fit$party_voted <- rep(c("Linke\nvoter", "Green\nvoter", "SPD\nvoter"), each=12)
crime_fit$party_voted <- factor(crime_fit$party_voted, levels=c("Linke\nvoter", "Green\nvoter", "SPD\nvoter"))

# variable indicating the party that was rated
crime_fit$party_rated <- rep(rep(c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD"), each=2), 3)
crime_fit$party_rated <- factor(crime_fit$party_rated, levels=c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD"))

# variable indicating the wave (question was asked only in waves 3 and 4)
crime_fit$wave <- rep(c("Wave 3", "Wave 4"), 18)


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------
# create Figure B.6
ggplot(data=crime_fit, aes(x=party_rated, y=value)) + 
  geom_bar(stat="identity", width = .8) + 
  geom_text(aes(label=round(value)), vjust=-.5, size=3) +
  facet_grid(wave~party_voted) +
  theme_bw() +
  ylab("Mean issue fit on crime") + xlab("Party rated") +
  scale_y_continuous(limits=c(0, 52)) +
  theme(text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust=.5, hjust=1))

# save Figure B.6
ggsave("figures/figure_b6.pdf", width=7, height=5)
