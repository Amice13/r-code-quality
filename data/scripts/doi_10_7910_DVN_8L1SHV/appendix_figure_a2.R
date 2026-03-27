## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----

#############################################
# Appendix Figure A2 #
#############################################
rm(list = ls())
options(stringsAsFactors = FALSE)

### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)
library(lme4)
library(arm)
library(memisc)
library(vegan)
library(MASS)
library(haven)
library(lfe)

# Load data
load("~/Dropbox/BKN/Code/Replication/4_Data/civil_service_aggregate.Rdata")

# Make Appendix Figure A2
pdf(file="~/Dropbox/BKN/Code/Replication/women_share.pdf",width=10)
ggplot(cs_all_total, aes(x = factor(year), y = women_share)) +
  geom_bar(stat = "identity", width = .7) +
  scale_y_continuous(labels = percent_format())+ theme_bw()+ scale_fill_grey()+
  labs(x = "Year")+  labs(y = "% Female Civil Servants")
dev.off()