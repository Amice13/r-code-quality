################################################################################ 
################################################################################ 
#################### Discrimination in Argentinian Football ####################
################################################################################ 
################################################################################ 

# This script is the master script for the project 
# Discrimination in Argentinian Football
# by Carlos Gomez-Gonzalez, Gwen-Jiro Clochard & Helmut Dietl

rm(list = ls(all.names = TRUE))


################################################################################ 
############################## Packages ######################################## 
################################################################################ 

vec.pac= c("foreign"
           , "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "car", "lfe", "doParallel",
           "caret", "foreach", "multcomp","cowplot", "haven", "readxl", "plyr", 
           "tidyr", "writexl", "modeest", "RCT", "knitr", "plm", "stargazer",
           "broom","miceadds", "DescTools", "psych", "gt",
           "units", "sf", "maps", "rnaturalearth", "geosphere", "psych", "openxlsx",
           "blockrand", "terra", "spData", "tmap", "gplots", "scales", "car", "ggrepel",
           "Rcpp"
)

new.packages <- vec.pac[!(vec.pac %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(vec.pac, require, character.only = TRUE) 

library(psych)
library(DescTools)
library(data.table)
library(xtable)
library(RCT)
library(knitr)
library(stargazer)
library(broom)
library(ggplot2)
library(hrbrthemes)
library(plotrix)
library(glmnet)
library(missForest)
library(randomForest)
library(dplyr)
library(lmtest)
library(tidyverse)
library(sandwich)
library(miceadds)
library(gt)
library(maps)
library(sf) 
library(units)
library(rnaturalearth)
library(geosphere)
library(openxlsx)
library(blockrand)
library(terra)
library(spData)
library(tmap)
library(raster)
library(gplots)
library(scales)
library(car)
library(ggrepel)

# Descriptive table

source("C://Users//Administrateur//Desktop//Recherche//Trust//3. Do-files//create_descriptive_tables.R")


################################################################################ 
##################################### Paths ####################################
################################################################################ 


# This section gets the right paths for different actions

pathresearch <- "C://Users//Administrateur//Desktop//Recherche//"
pathproject <- paste(pathresearch,"Discrimination Futbol Argentina//", sep = "")
pathdo <- paste(pathproject,"3. Do-files", sep = "")
pathdata <- paste(pathproject,"2. Data", sep = "") 
pathtables <- paste(pathproject,"5.Tables", sep = "")
pathfigures <- paste(pathproject,"4. Figures", sep = "")



################################################################################ 
################################# Data cleaning ################################
################################################################################ 

# 
# # Cleaning of the data set for club information
# 
# source(paste(pathdo, "1_Cleaning_clubs.R", sep="//"))
# 
# # Cleaning of the country information
# 
# source(paste(pathdo, "1_Cleaning_names.R", sep="//"))
# 
# 
# ################################################################################ 
# ################################### Treatment ##################################
# ################################################################################ 
# 
# 
# # Prepare and block randomize
# # WARNING: DO NOT RUN AFTER LAUNCH OF THE EMAILS!!!!!!!!!!!!!
# # source(paste(pathdo, "99_Stratified_treatment.R", sep="//"))
# 
# 
# # Cleaning of the treatment file
# 
# source(paste(pathdo, "2_Treatment.R", sep="//"))
# 
# 
# ################################################################################ 
# ################################### Outcomes ###################################
# ################################################################################ 
# 
# 
# # Cleaning of the outcome data file
# 
# source(paste(pathdo, "3_Outcome.R", sep="//"))
# 
# 
# ################################################################################ 
# ################################# Country info #################################
# ################################################################################ 
# 
# 
# # Cleaning of the country info data file
# 
# source(paste(pathdo, "4_Country_info.R", sep="//"))
# 
# 
# ################################################################################ 
# #################################### Merging ###################################
# ################################################################################ 
# 
# 
# # Merging the sources of data
# 
# source(paste(pathdo, "5_Merging.R", sep="//"))
# 
# 
# ################################################################################ 
# ################################### Analysis ###################################
# ################################################################################ 
# 
# 
# # Performing the analysis
# 
# source(paste(pathdo, "9_Analysis.R", sep="//"))
# 
# 
# 
