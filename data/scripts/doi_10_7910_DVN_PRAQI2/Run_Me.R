####################
#REPLICATION FILES: MAIN FILE
#Article: "The Missionary Roots of Nationalism: Evidence From China"
#Authors: Daniel Mattingly and Ting Chen
#This Version: May 18, 2021


####################
#Description: Running this script will create all the figures and tables in the article.
#The script was written with R version 4.0.3
#It was created and tested on Mac OS X (10.14.6) and Unix (GNU Bash 3.2.57(1)) systems.
####################


##Clean the workspace
rm(list=ls())


#OPTION 1: Manually set the working directory here:

setwd(' ')

#For example, if the replication directory has been unzipped in a Mac Downloads folder, 
#the above might read something like: setwd('~/Downloads/Mattingly_Chen_JOP_Replication') 

#Alternatives: In addition, setwd() can be commented out and this script can be run from the 
#Unix command line (or the Terminal app on Macs). First navigate in the shell to the
#replication directory. Then, run the following command: R CMD BATCH Run_Me.R
#Or in an IDE like R Studio the project directory can be set by the user.

####################



#Check to see if necessary packages are installed.
#If not, the packages will be installed automatically.

packages <- c("AER", "rio", "lmtest", "stargazer", "ggplot2", "sensemakr", "rgdal")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(packages, new.packages)

#Load packages.

require("AER")
require("rio")
require("lmtest")
require("stargazer")
require("ggplot2")
require("sensemakr")
require("rgdal")



####################

#Read in the prefecture-level data
dat <- import("mattingly_chen_data.csv")

####################

#Run each of the R scripts that creates the figures (pdf format) and tables (LaTeX format).
#They will be placed in the Figures and Tables folders respectively.

source("Mattingly_Chen_Variable_Creation.R")
source("Mattingly_Chen_Main_Tables_Figures.R")
source("Mattingly_Chen_Appendix_Tables_Figures.R")
source("Mattingly_Chen_Maps.R")


