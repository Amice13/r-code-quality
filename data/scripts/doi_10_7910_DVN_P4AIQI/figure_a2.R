# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure A.2

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# load necessary packages
# install.packages("ggplot2", "plotrix")
library(ggplot2)
library(plotrix)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/ess.rds")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# create an aggregated data frame with the mean level of concern by threat dimension, education, and party family
party_urban_means <- data.frame(reshape2::melt(aggregate(cbind(immi_crime, immi_cult, immi_econ)~party_fam+urban, data=dat[dat$party_fam%in%c("Radical-left", "Green", "Center-left", "Liberal", "Center-right", "Radical-right"),], mean, na.rm=T)))


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# create Figure A.2
ggplot(data=party_urban_means, aes(x=urban, y=value, group=variable, shape=variable)) +
  geom_point(size=2.5) +
  geom_line() +
  facet_wrap(~party_fam) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_shape_manual(values=c(16, 17, 15), labels=c("Crime", "Culture", "Economy"), name="") +
  ylab("Mean concern") + xlab("Urbanity") +
  theme(axis.text.x = element_text(angle = 50, vjust=1, hjust=1), text=element_text(size=16))

# save Figure A.2
ggsave("figures/figure_a2.pdf", width=7, height=5)