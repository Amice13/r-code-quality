## This code can be used to replicate the results of the paper using an alternative definition of terrorism
# Robustness test: rerun models with fatal events only and make variable importance and ALE plots
# Note that region 13 is not run (Australia has not enough positive fatal events)


# 1. Set working directory in your local disk
setwd("")
getwd()

allpack <- c("raster","sp","rgdal", "rgeos", "countrycode","foreach","doParallel","rmapshaper","doSNOW",
             "parallel","ggplot2","dplyr","prettymapr","rasterVis","sp","latticeExtra","mgcv", "caret",
             "readr","plotROC","palettetown","knitr","purrr","xgboost","ranger","precrec","ggthemes", "ggpubr",
             "kableExtra", "pdp", "viridis","scales","gplots","reshape2", "xtable", "av","iml",
             "zoo","RcppRoll","stats", "spatialEco","gstat","lattice","foreign","plyr","maptools",
             "gridExtra","viridisLite","maps", "latticeExtra","lubridate","deldir","prettymapr",
             "ggthemes","grid","prioritizr", "GISTools","cowplot","sf", "nngeo","mltools","MLmetrics")

######################OPTIONS TO BE DEFINED BY THE USERS###########################################
# 2. Options
# 2. a) choose if some code will run in parallel or not and define number of cores available for parallel computing
goparallel <- TRUE# TRUE will run a foreach loop across regions, FALSE a loop without parallel computing
nbcores <- 12 #define the number of cores for parallel computing (4 cores per region if possible)
#do not use more processors than the physical ones to ensure that all regions will be completed

# 2. b) define start and end of region number to run (to run all regions keep the default values)
regini<-1  #start
regend<-12 #end (max 12 because region 13 is not considered  here)

#define if details should appear or not during parallel work (for checking use TRUE)
myverbose <- TRUE
######################OPTIONS TO BE DEFINED BY THE USERS###########################################

#3. Data initialization  

#color scheme for this research work
regcol <-c("sienna4", "#3B4992FF", "#F39B7FB2", "#EE0000FF", "#91D1C2B2","#008B45FF", "#631879FF",
           "#008280FF" ,"#BB0021FF", "#5F559BFF" ,"#A20056FF" ,"orangered3","lightgoldenrod4")
#packages that will be exported during parallel computing
lspack <- c("raster","sp","rgdal", "rgeos", "countrycode","foreach","doParallel","rmapshaper","doSNOW")
new.packages <- lspack[!(lspack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(lspack, library, character.only = TRUE)

#split the world into regions (Australia has not enough positive fatal events, therefore not considered)
regions <- c(1:12) 
regionnames <- c("A","B","C","K","L","J","I","D","H","E","G","F")

#run code to prepare the data and produce initial plots
load("initialize.Rdata")#load initial data

#prepare the data and plot the study area and the data
source("subcode/initialplot.R", echo = TRUE,local = TRUE)

#4. Run models and produce the results
#exported objects
myexp <- c("acc","allworld","alt","GTD","gr","gtdt","prios","priot","vdem", "vdemdf","regini",
           "regend",  "world", "maxy","miny","nyears","regions","WGS84","PRIO","regcol","regionnames")

#options: run in parallel or not
#at least 40-60GB RAM is required per cluster
#if not available run without parallel computing
if(isTRUE(goparallel)) {

#run the parallel loop with foreach #############################
cores=nbcores                                                   #
cl <- makeCluster(cores[1])                                     #
registerDoSNOW(cl)                                              #
#total of 13 regions, choose below when to start and end the loop
foreach(mm=regini:regend, .packages=lspack, .export = myexp)  %dopar% {
#################################################################
source("subcode/subcodes.R", echo = TRUE,local = TRUE, verbose =
         myverbose) }
 } else {

#run without parallel in a simple loop ##########################
  for (mm in regini:regend){
    source("subcode/subcodes.R", echo = TRUE,local = TRUE, verbose =
             myverbose)
  } }

if(isTRUE(goparallel)) {
     stopCluster(cl)
}

#5. ROC, PRC and AUC (XGB and XGBfatal)
source("subcode/aucCI.R", echo = TRUE,local = TRUE, verbose = myverbose)

#6. variable importance (and differences between XGB and XGBfatal)
source("subcode/variableimportance.R", echo = TRUE,local = TRUE, verbose = myverbose)

#7. ALE plots (XGB and XGBfatal)
source("subcode/ale.R", echo = TRUE,local = TRUE, verbose = myverbose)
#END
