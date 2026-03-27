# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure D.1

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
dat = readRDS("data/soep.rds")


# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# aggregate crime concern by year and party preference using the mean
mean_concern_year_party = aggregate(crime_concern~syear+partypref, data=dat, mean)
mean_concern_year_party$partypref = factor(mean_concern_year_party$partypref, levels = c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD", "Other"))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure D.1
ggplot(data=mean_concern_year_party[mean_concern_year_party$partypref!="Other",], aes(x=syear, y=crime_concern, group=partypref, color=partypref)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Worried about crime in Germany") + xlab("Survey year") +
  scale_x_continuous(breaks=seq(1995, 2015, 5)) +
  scale_colour_manual(values=c("purple", "green", "red", "gold","black", "blue"), name="Party preference") +
  scale_y_continuous(limits = c(1,3))

# save Figure D.1
ggsave("figures/figure_d1.pdf", width = 7, height = 5)

