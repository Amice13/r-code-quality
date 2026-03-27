## Copyright 2016, 2025 Elizabeth Rigby
##
## This program is free software: you can redistribute it and/or modify it 
## under the terms of the GNU General Public License as published by the Free 
## Software Foundation, either version 3 of the License, or (at your option) 
## any later version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT 
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for 
## more details.
##
## A copy of the GNU General Public License is stored in the COPYING.txt file 
## along with this program. See also <https://www.gnu.org/licenses/>.
##
###############################################################################

## Scripts for simulation of detection of birds during point count surveys

rm(list=ls())
set.seed(123)

## number of years to simulate
SIMNUM  <- 1  ##Which round is this?  useful when running many sims

## number of simulation replications
SimReps <- 30

##Survey Type 
SurveyOptions <- c("multiple", "distance", "nmixture", "removal", "raw")
NumberSites <-   c("multiple"=30, 
                   "distance"=60,  
                   "nmixture"=20,  
                   "removal"=46, 
                   "raw"=60) 

## Assign Directory containing R scripts to be run via source()
MasterDirectory <- "C:/Users/Name/Desktop/Detection/Code for Repository"

## Choose Survey Type
AnalysisMethods <-  c("multiple")  ##Which methods to run?
# AnalysisMethods <-  c("raw")  
# AnalysisMethods <-  c("distance")  
# AnalysisMethods <-  c("removal")  
# AnalysisMethods <-  c("nmixture")  

for(Method in  AnalysisMethods){
  if(Method=="multiple") {SaveDirectory <- "C:/Users/Name/Desktop/Detection/multiple"}
  if(Method=="raw")      {SaveDirectory <- "C:/Users/Name/Desktop/Detection/raw"}
  if(Method=="distance") {SaveDirectory <- "C:/Users/Name/Desktop/Detection/distance"}
  if(Method=="removal")  {SaveDirectory <- "C:/Users/Name/Desktop/Detection/removal"}
  if(Method=="nmixture") {SaveDirectory <- "C:/Users/Name/Desktop/Detection/nmixture"}
  
  if((Method %in% SurveyOptions)==FALSE) stop("ERROR: not a valid analysis method, check spelling")
  
  NSurveySites <- (NumberSites[Method])  ## No. survey sites surveyed w/in 1 year
  SurveyType <- Method
  AnalysisMethodTimer <- proc.time()
  
  for(Iteration in 1:SimReps){
    setwd(SaveDirectory)  
    setwd(MasterDirectory)
    ## run script to load functions
    source("A1 - Functions.R")
    
    ## run script to load parameters
    source("A2 - Parameters.R")
    
    SimTIMER <- proc.time()
    setwd(MasterDirectory)
    
    ## run simulation script
    source("A3 - Simulation Script.R")
    print(paste("Simulation for Iteration",Iteration,"done",
                round(proc.time()["elapsed"] - SimTIMER["elapsed"],2)))
    
    ReshapeTIMER <- proc.time()
    setwd(MasterDirectory)
    
    ## run script to reshape data
    source("A4 - Summarize Simulation Results.R")
    print(paste("Analysis for Iteration",Iteration,"done",
                round(proc.time()["elapsed"] - ReshapeTIMER["elapsed"],2)))
    #     print(proc.time() - ReshapeTIMER)    
  }
  print(paste("Analysis Method",SurveyType, "NSurveySites =",NSurveySites,"done",
              round(proc.time()["elapsed"] - AnalysisMethodTimer["elapsed"],2)))
  
  #   print(proc.time() - AnalysisMethodTimer)  
}

## Recommend running the following scripts manually, rather than by source():
## A5 - Apply All Analysis Methods to Simulation Results.R
## A6 - Tables.R


######## END SCRIPT ###########################################################
