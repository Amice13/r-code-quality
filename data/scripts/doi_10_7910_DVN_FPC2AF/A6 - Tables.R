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

## Create summary tables for results of bird detection simulation.

library(arm)
# library(unmarked)
library(reshape)
# library(DescTools)  

rm(list=ls())
NSurveySites <- 30
InformalSimName <- "Sim 5"
SimulationName <- "2023_test"
SurveyType.Intended <- "distance"

MasterDirectory <- "C:/Users/Name/Desktop/Detection/Code for Repository"
TableDirectory  <- "C:/Users/Name/Desktop/Detection/Code for Repository/tables"
SaveDirectory   <- "C:/Users/Name/Desktop/Detection/Code for Repository/data"
PlotDirectory   <- "C:/Users/Name/Desktop/Detection/Code for Repository/figures"

##Load data for plotting 
## (produced by file "A5 - Apply All Analysis Methods to Simulation Results.R")
setwd(SaveDirectory)
ifelse(SurveyType.Intended == "distance",
       # load(paste("Sim4",SurveyType.Intended,"10Percent_truncation","analysis.RData", sep="_")),    
       # load(paste("Sim4",SurveyType.Intended,"50_100_150_m","analysis.RData", sep="_")))
       load(paste("Sim4",SurveyType.Intended,"10Percent_truncation",SimulationName, ".RData", sep="_")),    
       load(paste("Sim4",SurveyType.Intended,"50_100_150",SimulationName, ".RData", sep="_")))

# if(SurveyType.Intended == "distance"){
#   Dist.TopModel <- read.csv("TopModel estimates - Uniform Poly 10 Perc Truncation.csv")
#   #   Dist.ModelAvg <- read.csv("")
#   Master.Data.All[[1]][,"Dist.TopModel.Density.y"] <- Dist.TopModel$Estimate
#   #   Master.Data.All[[1]][,"Dist.ModelAvg"] <- Dist.ModelAvg$Estimate
#   EstimatorsOnly <- c(EstimatorsOnly,"Dist.TopModel.Density.y")#,"Dist.ModelAvg")
#   EstimatorsandTrueDensity <- c(EstimatorsandTrueDensity,"Dist.TopModel.Density.y")#,"Dist.ModelAvg")
# }


#########################
##Estimate Table, all methods
# AllSurveyTypes <- c("distance")
AllSurveyTypes <- c("raw","multiple", "removal", "nmixture", "distance")
Counter <- 0

for(SurveyType.Intended in AllSurveyTypes){
  ifelse(SurveyType.Intended == "distance",
         MaxSurveyDistances <- NA,
         MaxSurveyDistances <- c(50,100,150))
  
  SaveDirectory2 <- paste(SaveDirectory,
                        SurveyType.Intended,
                        sep="/")
  
  ##Load data (produced by file "A5 - Apply All Analysis Methods to Simulation Results.R")
  setwd(SaveDirectory2)
  ifelse(SurveyType.Intended == "distance",
         # load(paste("Sim4",SurveyType.Intended,"10Percent_truncation","analysis.RData", sep="_")),    
         # load(paste("Sim4",SurveyType.Intended,"50_100_150_m","analysis.RData", sep="_")))
         load(paste("Sim4",SurveyType.Intended,"10Percent_truncation",SimulationName, ".RData", sep="_")),    
         load(paste("Sim4",SurveyType.Intended,"50_100_150",SimulationName, ".RData", sep="_")))
  
  ##Combine all estimates from all analysis methods into 1 table
  for(ii in 1:length(MaxSurveyDistances)){
    Estimator.Results.All[[ii]]["MeanRadius"] <- mean(unlist(Master.Data.All[[ii]]["MaxSurveyDistance"]))
    Estimator.Results.All[[ii]]["SurveyType"] <- SurveyType
    if("NA.num" %in% colnames(Estimator.Results.All[[ii]]) == F){
      Estimator.Results.All[[ii]]["NA.num"]  <- NA
    }
    if(Counter == 0){Estimator.Results.Allmethods <- Estimator.Results.All[[ii]]}
    if(Counter>0){Estimator.Results.Allmethods <- rbind(Estimator.Results.Allmethods,Estimator.Results.All[[ii]])}
    Counter <- Counter+1
  }
}
# head(Estimator.Results.Allmethods)
# unique(Estimator.Results.Allmethods$Estimator)

##Set up nicely formatted names of estimators
Estimator.Results.Allmethods$Estimator.Name <- NA
Estimator.Results.Allmethods$SurveyType.Name <- NA

for(ii in 1:nrow(Estimator.Results.Allmethods)){
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "RawCountPerfectDist.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density: Perfect Distance"
  }
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "RawCount.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density"
  }  
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "NicholsEst.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Nichols et al. (2000) Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "ModelAvgEst.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "unmarked Model-Averaged Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "TopModelEst.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "unmarked Top Model Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "Dist.TopModel.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Program Distance Top Model Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "Dist.ModelAvg.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Program Distance Model-Averaged Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "BoundedCount.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Bounded Count Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "IndexMaxCount.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Maximum Count Density"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "CloseTerrBirds.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Ds"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "CloseBirds.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Dp"
  } 
  if(Estimator.Results.Allmethods[ii,"Estimator"] == "CloseSingers.Density.y"){
    Estimator.Results.Allmethods[ii,"Estimator.Name"] <- "Da"
  } 
  if(Estimator.Results.Allmethods[ii,"SurveyType"] == "raw"){
    Estimator.Results.Allmethods[ii,"SurveyType.Name"] <- "Simple Counts"
  } 
  if(Estimator.Results.Allmethods[ii,"SurveyType"] == "removal"){
    Estimator.Results.Allmethods[ii,"SurveyType.Name"] <- "Removal"
  }  
  if(Estimator.Results.Allmethods[ii,"SurveyType"] == "multiple"){
    Estimator.Results.Allmethods[ii,"SurveyType.Name"] <- "Double-Observer"
  }  
  if(Estimator.Results.Allmethods[ii,"SurveyType"] == "nmixture"){
    Estimator.Results.Allmethods[ii,"SurveyType.Name"] <- "Replicated Counts"
  }  
  if(Estimator.Results.Allmethods[ii,"SurveyType"] == "distance"){
    Estimator.Results.Allmethods[ii,"SurveyType.Name"] <- "Distance Sampling"
  }  
  
} #end ii

# Estimator.Results.Allmethods[,c("Estimator", "Estimator.Name")]
# Estimator.Results.Allmethods[order(Estimator.Results.Allmethods$MeanRadius),]
# head(Estimator.Results.Allmethods)


setwd(TableDirectory)
write.csv(Estimator.Results.Allmethods, 
          file=paste("Estimator Table - All methods", 
                     " BTBW ",InformalSimName,
                     SimulationName,
                     ".csv", sep=""))


############################
## Correlation Table, all methods

corr.results.Allmethods <- data.frame("Method" = NA,
                                    "Estimator" = NA,
                                    "MeanRadius" = NA,
                                    "rho.Ds" = NA,"p.rho.Ds" = NA,
                                    "rho.Dp" = NA,"p.rho.Dp" = NA,
                                    "rho.Da" = NA,"p.rho.Da" = NA)

AllSurveyTypes <- c("raw","multiple", "removal", "nmixture", "distance")
for(SurveyType.Intended in AllSurveyTypes){
  
  ifelse(SurveyType.Intended == "distance",
         MaxSurveyDistances <- NA,
         MaxSurveyDistances <- c(50,100,150))
  
  SaveDirectory2 <- paste(SaveDirectory,
                        SurveyType.Intended,
                        sep="/")
  
  ##Load data (produced by file "A5 - Apply All Analysis Methods to Simulation Results.R")
  setwd(SaveDirectory2)
  ifelse(SurveyType.Intended == "distance", 
         # load(paste("Sim4",SurveyType.Intended,"10Percent_truncation","analysis.RData", sep="_")),    
         # load(paste("Sim4",SurveyType.Intended,"50_100_150_m","analysis.RData", sep="_"))
         load(paste("Sim4",SurveyType.Intended,"10Percent_truncation", SimulationName, ".RData", sep="_")),    
         load(paste("Sim4",SurveyType.Intended,"50_100_150", SimulationName, ".RData", sep="_"))
  )
  
  ifelse(length(EstimatorsOnly.All[[1]]) == length(EstimatorsOnly.All[[length(EstimatorsOnly.All)]]),
         NumberEst <- length(EstimatorsOnly.All[[1]]),
         stop("ERROR NumberEst cannot calc- different num of estimators used within SurveyType"))
  
  for(ii in 1:length(MaxSurveyDistances)){
    ResultsLength <- nrow(corr.results.Allmethods)
    StartRow <- ifelse(ResultsLength == 1,1,ResultsLength+1)
    EndRow <- StartRow+NumberEst-1
    MeanRadius <- mean(unlist(Master.Data.All[[ii]]["MaxSurveyDistance"]))
    
    corr.results.Allmethods[StartRow:EndRow,"Method"] <- SurveyType
    corr.results.Allmethods[StartRow:EndRow,"Estimator"] <- as.character(corr.results.All[[ii]][,"Estimator"])
    corr.results.Allmethods[StartRow:EndRow,"MeanRadius"] <- MeanRadius
    corr.results.Allmethods[StartRow:EndRow,"rho.Ds"] <- corr.results.All[[ii]][,"rho.CloseTerrBirds.Density.y"]
    corr.results.Allmethods[StartRow:EndRow,"p.rho.Ds"] <- corr.results.All[[ii]][,"p.value.CloseTerrBirds.Density.y"]
    corr.results.Allmethods[StartRow:EndRow,"rho.Dp"] <- corr.results.All[[ii]][,"rho.CloseBirds.Density.y"]     
    corr.results.Allmethods[StartRow:EndRow,"p.rho.Dp"] <- corr.results.All[[ii]][,"p.value.CloseBirds.Density.y"]
    corr.results.Allmethods[StartRow:EndRow,"rho.Da"] <- corr.results.All[[ii]][,"rho.CloseSingers.Density.y"]
    corr.results.Allmethods[StartRow:EndRow,"p.rho.Da"] <- corr.results.All[[ii]][,"p.value.CloseSingers.Density.y"]
    
  }  ## end ii
} ## end AllSurveyTypes


##Set up nicely formatted names of estimators & SurveyTypes
corr.results.Allmethods$Estimator.Name <- NA
corr.results.Allmethods$SurveyType.Name <- NA

for(ii in 1:nrow(corr.results.Allmethods)){
  if(corr.results.Allmethods[ii,"Estimator"] == "RawCountPerfectDist.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density: Perfect Distance"
  }
  if(corr.results.Allmethods[ii,"Estimator"] == "RawCount.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density"
  }  
  if(corr.results.Allmethods[ii,"Estimator"] == "NicholsEst.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Nichols et al. (2000) Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "ModelAvgEst.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Program unmarked Model-Averaged Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "TopModelEst.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Program unmarked Top Model Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "Dist.TopModel.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Program Distance Top Model Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "Dist.ModelAvg.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Program Distance Model-Averaged Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "BoundedCount.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Bounded Count Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "IndexMaxCount.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Maximum Count Density"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "CloseTerrBirds.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Ds"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "CloseBirds.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Dp"
  } 
  if(corr.results.Allmethods[ii,"Estimator"] == "CloseSingers.Density.y"){
    corr.results.Allmethods[ii,"Estimator.Name"] <- "Da"
  } 
  if(corr.results.Allmethods[ii,"Method"] == "raw"){
    corr.results.Allmethods[ii,"SurveyType.Name"] <- "Simple Counts"
  } 
  if(corr.results.Allmethods[ii,"Method"] == "removal"){
    corr.results.Allmethods[ii,"SurveyType.Name"] <- "Removal"
  }  
  if(corr.results.Allmethods[ii,"Method"] == "multiple"){
    corr.results.Allmethods[ii,"SurveyType.Name"] <- "Double-Observer"
  }  
  if(corr.results.Allmethods[ii,"Method"] == "nmixture"){
    corr.results.Allmethods[ii,"SurveyType.Name"] <- "Replicated Counts"
  }  
  if(corr.results.Allmethods[ii,"Method"] == "distance"){
    corr.results.Allmethods[ii,"SurveyType.Name"] <- "Distance Sampling"
  }  
  
} #end ii

setwd(TableDirectory)
# for(MaxSurveyDistance in MaxSurveyDistances){
#   corr.results.Allmethodssubset <- corr.results.Allmethods[corr.results.Allmethods$Survey.Radius == MaxSurveyDistance,]
#   write.csv(corr.results.Allmethodssubset, 
#             file=paste("Correlation Table - All Methods",MaxSurveyDistance,"m.csv", sep=""))
# }
write.csv(corr.results.Allmethods, 
          # file=paste("Correlation Table - All Methods ","w_Truncation_for_distance",InformalSimName,".csv", sep=""))
          file=paste("Correlation Table - All Methods ",
                     "w_Truncation_for_distance",
                     InformalSimName,
                     SimulationName, 
                     ".csv", sep=""))


#########################
##Bias Table
bias.Allmethods <- data.frame("Method" = NA,
                            "Estimator" = NA,
                            "MeanRadius" = NA,
                            "Bias.Ds" = NA,
                            "Percent.Bias.Ds" = NA,
                            "Bias.Dp" = NA,
                            "Percent.Bias.Dp" = NA,
                            "UpperCI.boot.Bias.Dp" = NA,  
                            "LowerCI.boot.Bias.Dp" = NA,  
                            "UpperCI.exact.Bias.Dp" = NA,  
                            "LowerCI.exact.Bias.Dp" = NA,  
                            "Bias.Da" = NA,
                            "Percent.Bias.Da" = NA)

RowCounter <- 1
AllSurveyTypes <- c("raw","multiple", "removal", "nmixture", "distance")
for(SurveyType.Intended in AllSurveyTypes){
  #   SurveyType.Intended <- "distance"
  
  ifelse(SurveyType.Intended == "distance",
         MaxSurveyDistances <- NA,
         MaxSurveyDistances <- c(50,100,150))
  
  SaveDirectory2 <- paste(SaveDirectory,
                        SurveyType.Intended,
                        sep="/")
  
  ##Load data for plotting (produced by file "A5 - Apply All Analysis Methods to Simulation Results.R")
  setwd(SaveDirectory2)
  ifelse(SurveyType.Intended == "distance", 
         # load(paste("Sim4",SurveyType.Intended,"10Percent_truncation","analysis.RData", sep="_")),    
         # load(paste("Sim4",SurveyType.Intended,"50_100_150_m","analysis.RData", sep="_"))
         load(paste("Sim4",SurveyType.Intended,"10Percent_truncation", SimulationName,".RData", sep="_")),    
         load(paste("Sim4",SurveyType.Intended,"50_100_150", SimulationName,".RData", sep="_"))
  ) 
  
  ##Note: table shows MEDIAN bias across all 30 years
  Counter <- 0
  for(MaxSurveyDistance in MaxSurveyDistances){
    
    Counter <- Counter+1
    Master.Data <- Master.Data.All[[Counter]]
    
    ifelse(length(EstimatorsOnly.All[[1]]) == length(EstimatorsOnly.All[[length(EstimatorsOnly.All)]]),
           EstimatorsOnly <- EstimatorsOnly.All[[1]],
           stop("ERROR NumberEst cannot calc- different num of estimators used within SurveyType"))
    
    for(Estimator in EstimatorsOnly){
      bias.dp  <-  na.omit(Master.Data[,Estimator]-Master.Data$CloseBirds.Density.y)  
      bias.Allmethods[RowCounter,"Method"] <- SurveyType
      bias.Allmethods[RowCounter,"Estimator"] <- as.character(Estimator)
      bias.Allmethods[RowCounter,"MeanRadius"] <- round(mean(unlist(Master.Data.All[[Counter]]["MaxSurveyDistance"])),0)
      bias.Allmethods[RowCounter,"Bias.Ds"] <- median(na.omit(Master.Data[,Estimator]-Master.Data$CloseTerrBirds.Density.y))
      bias.Allmethods[RowCounter,"Percent.Bias.Ds"] <- median(na.omit((Master.Data[,Estimator]-Master.Data$CloseTerrBirds.Density.y)/Master.Data$CloseTerrBirds.Density.y))
      bias.Allmethods[RowCounter,"Bias.Dp"] <- median(na.omit(Master.Data[,Estimator]-Master.Data$CloseBirds.Density.y))
      bias.Allmethods[RowCounter,"Percent.Bias.Dp"] <- median(na.omit((Master.Data[,Estimator]-Master.Data$CloseBirds.Density.y)/Master.Data$CloseBirds.Density.y))
      bias.Allmethods[RowCounter,"LowerCI.exact.Bias.Dp"]  <-  MedianCI(bias.dp, 
                                                                      conf.level = 0.95, 
                                                                      na.rm = TRUE, 
                                                                      method = "exact")["lwr.ci"] 
      bias.Allmethods[RowCounter,"UpperCI.exact.Bias.Dp"]  <-  MedianCI(bias.dp, 
                                                                      conf.level = 0.95, 
                                                                      na.rm = TRUE, 
                                                                      method = "exact")["upr.ci"] 
      
      bias.Allmethods[RowCounter,"LowerCI.boot.Bias.Dp"]  <-   MedianCI(bias.dp, 
                                                                      conf.level = 0.95, 
                                                                      na.rm = TRUE, 
                                                                      method = "boot")["lwr.ci"]  
      bias.Allmethods[RowCounter,"UpperCI.boot.Bias.Dp"]  <-   MedianCI(bias.dp, 
                                                                      conf.level = 0.95, 
                                                                      na.rm = TRUE, 
                                                                      method = "boot")["upr.ci"]
      bias.Allmethods[RowCounter,"Bias.Da"] <- median(na.omit(Master.Data[,Estimator]-Master.Data$CloseSingers.Density.y))
      bias.Allmethods[RowCounter,"Percent.Bias.Da"] <- median(na.omit((Master.Data[,Estimator]-Master.Data$CloseSingers.Density.y)/Master.Data$CloseSingers.Density.y))
      bias.Allmethods[RowCounter,c("Bias.Ds","Percent.Bias.Ds","Bias.Dp","Percent.Bias.Dp","Bias.Da","Percent.Bias.Da")] <- round(bias.Allmethods[RowCounter,c("Bias.Ds","Percent.Bias.Ds","Bias.Dp","Percent.Bias.Dp","Bias.Da","Percent.Bias.Da")],3)
      RowCounter <- RowCounter+1
    }  ## end NumberEst   
  }  ## end MaxSurveyDistances
}  ## end AllSurveyTypes

##Set up nicely formatted names of estimators & SurveyTypes
bias.Allmethods$Estimator.Name <- NA
bias.Allmethods$SurveyType.Name <- NA

for(ii in 1:nrow(bias.Allmethods)){
  if(bias.Allmethods[ii,"Estimator"] == "RawCountPerfectDist.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density with Perfect Distance Estimation"
  }
  if(bias.Allmethods[ii,"Estimator"] == "RawCount.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Simple Index Density"
  }  
  if(bias.Allmethods[ii,"Estimator"] == "NicholsEst.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Nichols et al. (2000) Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "ModelAvgEst.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Program unmarked Model-Averaged Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "TopModelEst.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Program unmarked Top Model Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "Dist.TopModel.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Program Distance Top Model Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "Dist.ModelAvg.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Program Distance Model-Averaged Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "BoundedCount.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Bounded Count Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "IndexMaxCount.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Maximum Count Density"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "CloseTerrBirds.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Ds"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "CloseBirds.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Dp"
  } 
  if(bias.Allmethods[ii,"Estimator"] == "CloseSingers.Density.y"){
    bias.Allmethods[ii,"Estimator.Name"] <- "Da"
  } 
  if(bias.Allmethods[ii,"Method"] == "raw"){
    bias.Allmethods[ii,"SurveyType.Name"] <- "Simple Counts"
  } 
  if(bias.Allmethods[ii,"Method"] == "removal"){
    bias.Allmethods[ii,"SurveyType.Name"] <- "Removal"
  }  
  if(bias.Allmethods[ii,"Method"] == "multiple"){
    bias.Allmethods[ii,"SurveyType.Name"] <- "Double-Observer"
  }  
  if(bias.Allmethods[ii,"Method"] == "nmixture"){
    bias.Allmethods[ii,"SurveyType.Name"] <- "Replicated Counts"
  }  
  if(bias.Allmethods[ii,"Method"] == "distance"){
    bias.Allmethods[ii,"SurveyType.Name"] <- "Distance Sampling"
  }  
  
} #end ii

# bias.Allmethods
setwd(TableDirectory)
write.csv(bias.Allmethods, 
          file=paste("Median Bias Table - All Methods",
                     InformalSimName, 
                     SimulationName, 
                     ".csv", sep=""))


######## END TABLES ###########################################################