rm(list=ls())##clear workspace

## Use R and R package versions from April 19, 2016
install.packages(c("xlsx", "foreign", "stringr", "sandwich", "lmtest", "rms", "survey", "xtable", "lattice", "gplots", "graphics", "ggplot2", "car", "fossil", "maptools", "sp", "foreign", "zipcode", "reshape2", "plotrix"))
update.packages()

library(xlsx)
library(foreign)
library(stringr)
library(sandwich)
library(lmtest)
library(rms)
library(survey)
library(xtable)
library(lattice)
library(gplots)
library(graphics)
library(ggplot2)
library(car)
library(fossil)
library(maptools)
library(sp)
library(foreign)
library(zipcode)
library(reshape2)
library(plotrix)


as.n<-as.numeric
as.c<-as.character
as.f<-as.formula
len<-length


##Set your working directory here. Also store the file path to your working directory as the string object "wd".
wd<-"C:/Dropbox/geogaffect/replication package/final_upload_to_JOP"
setwd(wd)


##create file path for saving tables and figures
output<-paste(wd, "/output/", sep="")


### CONJOINT ANALYSIS
### Figure 2 in main text
### Figures 17-29 in Online Appendix
##Produce all results from conjoint analysis
source("conjoint.estimation.R")


### QUICKFIRE PAIRED COMPARISON ANALYSIS
### Figure 3 in main text
### Figures 3-16 in Online Appendix
source("quickfire.estimation.R")


### HEAT MAP ANALYSIS
### Figure 4 in main text
source("heatmap.estimation.R")


### NEIGHBORHOOD QUALITY PROXY ANALYSIS
### Display correlations between proxies of neighborhood quality and violent crime rates, county level discussed in Footnote 23
source("neighborhood.quality.proxy.analysis.R")


### FEASIBILITY OF SORTING ANALYSIS
### FIGURE 5 in main text
### FIGURES 32 - 55 in Online Appendix
source("feasibility.estimation.R")


### OBSERVED MOVING BEHAVIOR OF SURVEY RESPONDENTS
### Figure 6 in main text
### Figure 56 in Online Appendix
source("move.behavior.estimation.R")


### COMMUNITY PREFERENCE FACTORIAL EXPERIMENT
### Figure 2 in Online Appendix
source("factorial.experiment.estimation.R")


##OTHER APPENDIX MATERIALS
##Tables in Online Appendix A
source("other.appendix.estimation.R")








