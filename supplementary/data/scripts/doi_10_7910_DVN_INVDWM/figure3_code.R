
################
###FIGURE 3 ###
################

rm(list=ls())

library(ggplot2)
library(foreign)
library(readstata13)

setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/replication materials/submission_data/administrative/")

data = read.dta13("admin_figure_data.dta")

setwd("/Users/samtrachtman/Dropbox/ACA Study/Projects/Public Private Field Experiment/APSR Production/replication materials/submission_results/administrative/")
tiff(filename = "figure3.tif")
ggplot(data, aes(x = dem_2012, y = share_2015))+
  geom_point()+
  stat_smooth(method = "loess", col = "black")+
  labs(x = "Democratic Vote 2012", y = "Share of Eligible Population Enrolled") +
  theme(axis.title.y = element_text(size = rel(1.3), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.3)))+
  theme_bw()+
  scale_y_continuous( labels = scales::percent, limits = c(0, NA))+
  scale_x_continuous( labels = scales::percent)
#ggtitle("Political Partisanship and ACA Marketplace Enrollment") 
dev.off()






