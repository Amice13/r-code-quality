# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure A.1

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
dat = readRDS("data/ess.rds")

# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# create an aggregated data frame with the mean level of concern by threat dimension, education, and party family
party_edu_means <- data.frame(reshape2::melt(aggregate(cbind(immi_crime, immi_cult, immi_econ)~edu+party_fam, data=dat[dat$party_fam%in%c("Radical-left", "Green", "Center-left", "Liberal", "Center-right", "Radical-right"),], FUN=mean)))

# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------


# Create Figure A.1
ggplot(data=party_edu_means, aes(x=edu, y=value, group=variable, shape=variable)) +
  geom_point(size=2.5) +
  geom_line() +
  facet_wrap(~party_fam) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_shape_manual(values=c(16, 17, 15), labels=c("Crime", "Culture", "Economy"), name="") +
  ylab("Mean concern") + xlab("Educational attainment") +
  theme(axis.text.x = element_text(angle =50, vjust=1, hjust=1), text=element_text(size=16))

# save Figure A.1
ggsave("figures/figure_a1.pdf", width=7, height=5)