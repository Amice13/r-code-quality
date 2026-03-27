# Replication code for BJPS article "Do the lights stay on? Deployment and withdrawal of peacekeepers and their effect on local economic development"
# Authors: 
	#Deniz Cil (dcil@umd.edu)
	#Hanne Fjelde (Hanne.Fjelde@pcr.uu.se)
	#Lisa Hultman (lisa.hultman@pcr.uu.se)
	#Desirée Nilsson (desiree.nilsson@pcr.uu.se)
	#Nils W. Metternich (n.metternich@ucl.ac.uk)

# Code replicates all Figures and Tables in the article and its online appendix
# Figures will be saved in specified "pathFig"
# Tables will be saved in specified "pathTab"
# R version: 4.3.1 
# OS: MacOS 14.4.1


# Please refer to the codebook for variable description
# Please notify the authors if you become aware of any issues with the code
# Please use the following citation:

	#Deniz Cil, Hanne Fjelde, Lisa Hultman, Desirée Nilsson and Nils W. Metternich
	#2024
	#Do the lights stay on? Deployment and withdrawal of peacekeepers and their effect on local economic development
	#British Journal of Political Science
	#Forthcoming
	


library(foreach)
library(doParallel)
library(reshape2)
library(geosphere)
library(rgeos)
library(sp)
library(tidyr)
library(haven)
library(stringr)
library(plyr)
library(survival)
library(raster)
library(cshapes)
library(dplyr)
library(ggplot2)
library(rgdal)
library(lme4)
library(gridExtra)
library(texreg)
library(cem)
library(plm)
library(prediction)
library(MASS)
library(tidyverse)
library(stargazer)
library(zoo)
library(fixest)
library(pglm)
library(maptools)
library(car)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


#Please update your username ("YOURUSERNAME") and paths ("YOURPATH") to the location of replication data and code. 

if(
  Sys.info()["user"]=="YOURUSERNAME"){pathData="YOURPATH";pathR="YOURPATH";pathFig="YOURPATH";pathTab="YOURPATH"}

#Example:

#if(
#Sys.info()["user"]=="nils"){pathData="~/Documents/git/peaceKeepingLight/BJPS_replication";pathR="~/Documents/git/peaceKeepingLight/BJPS_replication";pathFig="~/Desktop/figures";pathTab="~/Desktop/tables"}

setwd(pathR)
source('model_run_1.R')

setwd(pathR)
source('table_1.R')

setwd(pathR)
source('table_A7.R')

setwd(pathR)
source('table_A10.R')

setwd(pathR)
source('table_A11.R')

setwd(pathR)
source('table_A13.R')

setwd(pathR)
source('table_A14.R')

setwd(pathR)
source('model_run_2.R')

setwd(pathR)
source('table_A15.R') 

setwd(pathR)
source('table_A16.R') 

setwd(pathR)
source('model_run_3.R')

setwd(pathR)
source('table_A2_A3_A4_A5_A6.R')

setwd(pathR)
source('model_run_4.R')

setwd(pathR)
source('figure_A11.R')

setwd(pathR)
source('table_A8.R') 

setwd(pathR)
source('table_A9.R')

setwd(pathR)
source('model_run_5.R')

setwd(pathR)
source('table_A12.R')

setwd(pathR)
source('model_run_6.R')

setwd(pathR)
source('figure_3.R')

setwd(pathR)
source('model_run_7.R')

setwd(pathR)
source('figure_A1_A2_A3_A4_A5_A6.R')

setwd(pathR)
source('model_run_8.R')

setwd(pathR)
source('figure_1_A7_A8.R')

setwd(pathR)
source('model_run_10.R')

setwd(pathR)
source('figure_A10.R')

setwd(pathR)
source('model_run_11.R')

setwd(pathR)
source('figure_A12.R') 

setwd(pathR)
source('model_run_12.R')

setwd(pathR)
source('figure_A13.R') 

setwd(pathR)
source('model_run_13.R')

setwd(pathR)
source('figure_A14.R')

setwd(pathR)
source('model_run_14.R')

setwd(pathR)
source('figure_A9.R')

#setwd(pathR) # .rda required for replication too large for Harvard Dataverse. Please contact authors for data supporting Figure 2.
#source('figure_2.R')

#please run figure_4.do before running figure_4.R
setwd(pathR)
source('figure_4.R')

