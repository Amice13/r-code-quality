#####Master script to analyze data
rm(list=ls())
library(DescTools)
library(geosphere)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(reshape)
library(naniar)
library(mediation)
library(DirectEffects)
library(data.table)
library(lfe)
library(stargazer)
library(cobalt)
library(ggplot2)
library(patchwork)
library(scales)
library(sp)
library(rgeos)
library(rgdal)
library(sf)
library(spdep)
library(broom)
library(readstata13)
library(stringr)
library(fastDummies)
library(wesanderson)
library(RColorBrewer)
library(mgcv)
library(factoextra)
library(car)
library(corrplot)


### please specify path to replication folder
## In RStudio, just run the following line:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Outside of RStudio, specify manually
# HomePath <- "PATH TO REPLICATION"
# setwd(HomePath)


###
## Ethnologue polygon analysis
###
wlms_data <- readRDS("wlms/data_wlms.rds")

## Figures - Main
source("wlms/regs_wlms_fig2.R") # Figure 2 (Cash Crops, Publications & Political Relevance)

## Figures - Appendix
source("wlms/sumstats_wlms_figA4.R") # Figure A9 (Political Relevance, Ethnic Salience & Boundaries)
source("wlms/sumstats_wlms_figA9.R") # Figure A9 (Sumstats)
source("wlms/regs_wlms_figA11.R") # Figure A11 (Exclusive PREG/EPR Link Outcome)
source("wlms/regs_wlms_figA12.R") # Figure A12 (Cash Crops, Publications & Social Relevance: AMAR)
source("wlms/regs_wlms_figA27.R") # Figure A27: Treatment interactions
source("wlms/regs_wlms_figA30.R") # Figure A30: Interacting treatments with colonizer dummy
source("wlms/regs_wlms_figA33.R") # Figure A33: Disaggregating Crops. Top 5 vs. other cash crops and minerals, smallholder vs. plantation crops
source("wlms/regs_wlms_figA37.R") # Figure A37: Controlling for share of Christians per group
source("wlms/regs_wlms_figA37.R") # Figure A37: Controlling for share of Christians per group


####
## publications
####

pubs <- readRDS("publications/data_publications.rds")
source("publications/pubs_figA22-23.R") # Figures A22 -23

###
## Afrobarometer
###

ab<- readRDS("afrobarometer/data_ab.rds")
## Tables - main
source("afrobarometer/regs_ab_table1.R") # Table 1: baseline geo spec
source("afrobarometer/regs_ab_table2.R") # Table 2: baseline ethnic spec
# Tables-Appendix
source("afrobarometer/regs_ab_tableA3.R") #  Table A3:  Magnitudes Effect sizes compared to Robinson (2014)
source("afrobarometer/regs_ab_tableA8.R") # Table A8: Soas publications
source("afrobarometer/regs_ab_tableA9.R") # Table A8: pre-colonial centralisation
source("afrobarometer/regs_ab_tableA14.R") # Table A14: trust
#Figures - appendix
source("afrobarometer/sumstats_ab_figA10.R") # Figure A10 (sumstats)
source("afrobarometer/sumstats_ab_figA19.R") # Figure A19: ethnic leavers by group size
source("afrobarometer/regs_ab_figA20.R") # Figure A20: Group size for ethnic leavers
# Mediation analysis (fig A24) Beware: next codes take a few hours to run
source("afrobarometer/regs_ab_figA24a.R") # Figure A24a: Mediation - geo -modernization
source("afrobarometer/regs_ab_figA24b.R") # Figure A24a: Mediation - geo -early education
source("afrobarometer/regs_ab_figA24c.R") # Figure A24a: Mediation - geo -public sphere
source("afrobarometer/regs_ab_figA24d.R") # Figure A24a: Mediation - lang -modernisation
source("afrobarometer/regs_ab_figA24e.R") # Figure A24a: Mediation - lang -early education
source("afrobarometer/regs_ab_figA24f.R") # Figure A24a: Mediation - lang -public sphere
## Continuing with appendix
source("afrobarometer/regs_ab_figA28.R") # Figure A28: treatment interactions
source("afrobarometer/regs_ab_figA31.R") # Figure A31: coloniser identity
source("afrobarometer/regs_ab_figA40.R") # Figure A40a-d: block voting
source("afrobarometer/regs_ab_tableA10.R") # Table A10: Disaggregating Crops
source("afrobarometer/regs_ab_tableA11.R") # Table A11: Controlling for local diversity


####
## DHS
#####

rm(list=ls())
dhs <- readRDS("dhs/data_dhs.rds")
names(dhs)

## Figures for Main Paper
source("dhs/regs_dhs_fig3.R") # Fig 3: baseline geographic analysis
source("dhs/regs_dhs_fig4.R") # Fig 4: baseline ethnic models: treatments assigned to husband & male leavers only


## Figures for Appendix
source("dhs/regs_dhs_figA13.R") # Fig A13: assigning cash crops and control variables to polygon rather than more local levels & using alternative publication data (Mann & Sanders) (Fig A13)
source("dhs/regs_dhs_figA14.R") # Fig A14: Spatial IV, suitability reduced form & spatial lag model (left) & intensive margin publications (right)
source("dhs/regs_dhs_figA15.R") # Fig A15: ethnic leaver model: intensive margin publications
source("dhs/regs_dhs_figA16.R") # Fig A16: geographic models: subsetting to (male/female) ethnic stayers/leavers 
source("dhs/regs_dhs_figA17.R") # Fig A17: ethnic models: treatments assigned to wife & female leavers only
source("dhs/regs_dhs_figA18.R") # Fig A18: geographic model: controlling for precolonial statehood
source("dhs/regs_dhs_figA21.R") # Fig A21: ethnic leaver model: adding size controls
source("dhs/regs_dhs_figA25.R") # Fig A25: Mediation Models: Modernization
source("dhs/regs_dhs_figA26.R") # Fig A26: Mediation Models: Early Educated Elite
source("dhs/regs_dhs_figA29.R") # Fig A29: treatment interactions
source("dhs/regs_dhs_figA32.R") # Fig A32: interacting treatments with colonizer dummies
source("dhs/regs_dhs_figA34.R") # Fig A34: Disaggregating Crops: Top 5 vs. other cash crops and minerals, smallholder vs. plantation crops
source("dhs/regs_dhs_figA35.R") # Fig A35: interacting treatments with local-level diversity
source("dhs/regs_dhs_figA36.R") # Fig A36: controlling for local-level diversity 
source("dhs/regs_dhs_figA38.R") # Fig A38: adding directed religious couple FE 
source("dhs/regs_dhs_figA39.R") # Fig A39: Mediation Models with religious denomination dummies as mediators

## ## ## ## ## ## 
## OPTIONAL: correct standard errors for mediation models: cluster bootstrap
## ## ## ## ## ## 
# The mechanism analysis in our online appendix computes standard errors around the reported ACDEs by means of a cluster bootstrap.
# This more adequately reflects the two-step estimation procedure than just reporting clustered errors from the second-stage model.
# See Acharya, Blackwell and Sen (2016) for details.
# Figures A25, A26, and A39 each report results from 26 baseline and 26 mediation models, where each mediation model requires running two regressions.
# Bootstrapping this with 150 iterations is computationally expensive, so the scripts above skip the bootstrap and just report CIs based on clustered errors from the second stage.
# For an exact replication of Figures  A25, A26, and A39, please uncomment and run the bootstrap scripts below.
## make sure to have the following R packages installed on your system to speed things up with parallel computing. 
#install.packages("parallel")
#install.packages("foreach")
#install.packages("doParallel")
#install.packages("doRNG")
## Also, set the object ncores to the appropriate number of CPUs you can afford to use for parallelization. 

##### UNCOMMENT START
# # load parallelization
# library(foreach)
# library(doParallel)
# library(parallel)
# library(doRNG)
# # set numbers of CPUs
# ncores <- detectCores()-3
# 
# source("dhs/regs_dhs_figA25_boot.R") # Fig A25: Mediation Models: Modernization
# source("dhs/regs_dhs_figA26_boot.R") # Fig A26: Mediation Models: Early Educated Elite
# source("dhs/regs_dhs_figA39_boot.R") # Fig A39: Mediation Models with religious denomination dummies as mediators
##### UNCOMMENT END

# Each of these scripts runs for about 40 minutes on 45 CPUs on a server at ETH Zurich.
# This will take significantly longer on your local computer. 
# The differences between clustered second-stage and appropriately bootstrapped errors is minimal in the current application.
