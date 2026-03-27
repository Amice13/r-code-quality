# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure B.5

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

# aggregate into a data frame the probability of indicating crime is one of three vote deciding issues among left-wing voters, by level of concern about local refugee crime and wave
sal_agg <- rbind.data.frame(aggregate(is_crime~ref_loc_crime+wave, data=dat[dat$left_any==1,], mean),
                            aggregate(is_crime~ref_loc_crime+wave, data=dat[dat$green_any==1,], mean),
                            aggregate(is_crime~ref_loc_crime+wave, data=dat[dat$spd_any==1,], mean))

# indicator for wave
sal_agg$wave_name <- rep(rep(c("Wave 1", "Wave 2", "Wave 3", "Wave 4"), each=4), 3)

# indicator for party voted for
sal_agg$party <- rep(c("Linke voter", "Green voter", "SPD voter"), each=16)
sal_agg$party <- factor(sal_agg$party, levels=c("Linke voter", "Green voter", "SPD voter"))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure B.5
ggplot(data=sal_agg, aes(x=ref_loc_crime,y=is_crime,group=party,color=party)) + 
  geom_point() +
  facet_wrap(~wave_name) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom", text=element_text(size=20)) +
  scale_color_manual(values=c("purple", "green", "red"), name="") +
  xlab("Concern about local refugee crime") + ylab("Probability of mentioning crime\nas a vote-deciding issue")

# save Figure B.5
ggsave("figures/figure_b5.pdf", width=7, height=5)
