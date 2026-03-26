# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure B.1

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

# change in concern across waves
crime_change = sapply(c("left_any", "green_any", "spd_any", "fdp_any", "cdu_any", "afd_any"), function(x) {
  tapply(dat$ref_loc_crime[dat[,x]==1], dat$wave[dat[,x]==1], mean, na.rm=T)
})
crime_change = data.frame(crime_change)
crime_change = reshape2::melt(crime_change)
crime_change$party = rep(c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD"), each=4)
crime_change$party = factor(crime_change$party, levels=c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD"))
crime_change$wave = rep(c("1 (07 / 2016)", "2 (01 / 2017)", "3 (06 / 2017)", "4 (12 / 2017)"), 6)


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure B.1
ggplot(data=crime_change, aes(x=wave, y=value)) + 
  geom_point() + 
  geom_line(aes(group=1)) + 
  facet_wrap(~party, scales="free_y") +
  theme_bw() +
  ylab("Mean") + xlab("Panel wave") +
  theme(text=element_text(size=20), axis.text.x=element_text(angle = -90, hjust = 0))

# save Figure B.1
ggsave("figures/figure_b1.pdf", width=7, height=5)