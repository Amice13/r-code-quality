# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure D.2

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2", "dplyr")
library(ggplot2)
library(dplyr)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/soep.rds")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# extract the first wave for each respondent 
dat_first_wave <- dat[,c("pid", "syear", "crime_concern", "partypref")] %>%
  group_by(pid) %>%
  slice_min(syear)
names(dat_first_wave) = c("pid", "syear_first", "crime_concern_first", "partypref_first")

# merge data from the first wave with the long panel data frame
dat = merge(dat, dat_first_wave, by="pid")

# calculate year difference to first wave
dat$dist_first = dat$syear - dat$syear_first

# remove obs in same year as first wave
dat = dat[dat$dist_first != 0,]

# create variable indicating same response to crime concern question as in the first wave
dat$same_crime_concern = dat$crime_concern == dat$crime_concern_first

# calculate the proportion giving same response on the crime concern question as in first wave, by distance to first wave and party preference in first wave
dat_same_first = aggregate(same_crime_concern~dist_first + partypref_first, dat, mean)
dat_same_first$partypref_first = factor(dat_same_first$partypref_first, levels=c("Linke", "Greens", "SPD", "FDP", "CDU/CSU", "AfD", "Other"))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure D.2
ggplot(data=dat_same_first[dat_same_first$partypref_first!="Other",], aes(x=dist_first, y=same_crime_concern)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  ylab("Proportion giving same response") + xlab("Years since first wave") +
  facet_wrap(~partypref_first)

# save Figure D.2
ggsave("figures/figure_d2.pdf", width = 7, height = 5)
