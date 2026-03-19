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

## Apply analysis methods to obtain abundance and density estimates, including 
##   indices and detectability-adjusted estimates.

library(arm)
library(unmarked)
library(reshape)

rm(list=ls())

######################################
##Set up directories and file names
InformalSimName <- "Sim 5"
DATE<-format(Sys.time(), "%m_%d_%Y")

for(SurveyType.Intended in c("raw", "multiple", "removal", "nmixture","distance")){ ## analyze all methods
  #  SurveyType.Intended <- "multiple" ## choose one method to analyze
  
  SaveDirectory <- paste0("C:/Users/Name/Desktop/Detection/",
                          SurveyType.Intended)
  MasterDirectory <- "C:/Users/Name/Desktop/Detection/Code for Repository"
  # setwd(SaveDirectory)  
  setwd(MasterDirectory)
  source("A1 - Dissertation Functions.R")
  TIMER <- proc.time()
  
  SimReps <- 30
  ##For distance sampling, use ~unlimited radius; all others use 50,100,150 m
  ifelse(SurveyType.Intended=="distance",
         MaxSurveyDistances <- NA,
         MaxSurveyDistances <- c(50,100,150)) ##Cutoff (m) for analysis
  corr.results.All <- list()
  Estimator.Results.All <- list()
  Master.Data.All <- list()
  EstimatorsOnly.All <- list()
  
  corr.results.Outliers.Not.Removed.All <- list()
  Estimator.Results.Outliers.Not.Removed.All <- list()
  Master.Data.Outliers.Not.Removed.All <- list()
  EstimatorsOnly.Outliers.Not.Removed.All <- list()
  
  Counter <- 0
  for(MaxSurveyDistance in MaxSurveyDistances){
    #   MaxSurveyDistance <- 50
    Counter <- Counter+1
    
    ## For true N, exclude all birds count=0 with Dist >= MaxSurveyDistance
    ## For all survey types EXCEPT distance, RETAIN all birds count=1 with 
    ## ObsEstDist <=MaxSurveyDistance. (For distance, keep all.)
    
    ##Create list of estimators for this SurveyType
    EstimatorsOnly <- c("RawCount.Density.y",
                        if(SurveyType.Intended!="raw"){c("ModelAvgEst.Density.y",
                                                         "TopModelEst.Density.y")},                  
                        if(SurveyType.Intended=="multiple"){"NicholsEst.Density.y"},                  
                        if(SurveyType.Intended=="nmixture"){c("IndexMaxCount.Density.y",
                                                              "BoundedCount.Density.y")},
                        if(SurveyType.Intended=="distance"){c("Dist.TopModel.Density.y",
                                                              "Dist.ModelAvg.Density.y"
                        )},
                        "RawCountPerfectDist.Density.y")
    
    EstimatorsandTrueDensity <- c("RawCount.Density.y",
                                  "RawCountPerfectDist.Density.y",
                                  "NBirds.Density.y",
                                  "CloseTerrBirds.Density.y",
                                  "CloseBirds.Density.y",
                                  "CloseSingers.Density.y",
                                  if(SurveyType.Intended!="raw"){c("ModelAvgEst.Density.y",
                                                                   "TopModelEst.Density.y")},
                                  if(SurveyType.Intended=="distance"){c("Dist.TopModel.Density.y",
                                                                        "Dist.ModelAvg.Density.y"
                                  )},
                                  if(SurveyType.Intended=="multiple"){"NicholsEst.Density.y"},                            
                                  if(SurveyType.Intended=="nmixture"){c("IndexMaxCount.Density.y",
                                                                        "BoundedCount.Density.y")}
    )
    
    #######################################
    ##Set up Annual Data table (Master.Data)
    
    ##Load files by year
    ##Use grep to avoid having to customize filenames by date
    setwd(SaveDirectory)
    All.Data.Files <- list.files()
    Analysis.Files <- All.Data.Files[grep("Analysis", All.Data.Files)]
    
    ## Create master data object
    for(Iteration in 1:SimReps){
      DataIteration <- paste("_",Iteration,".R",sep="")
      File.Name <- Analysis.Files[grep(DataIteration,Analysis.Files)]
      load(File.Name)
      
      if(SurveyType!=SurveyType.Intended) stop("WRONG ANALYSIS METHOD")
      if(Iteration==1){Master.Data <- YY.data}
      if(Iteration>1){
        YY.data$y <- Iteration
        Master.Data <- rbind(Master.Data, YY.data)
      }
    }
    
    ##Clean up Master.Data
    if(SurveyType!="removal"){
      Master.Data$RemovalPeriod1Intervals <- NULL
      Master.Data$RemovalPeriod1Length <- NULL
      Master.Data$RemovalPeriod2Intervals <- NULL
      Master.Data$RemovalPeriod2Length <- NULL
      Master.Data$RemovalPeriod3Intervals <- NULL
      Master.Data$RemovalPeriod3Length <- NULL
    }
    
    ##Naming Convention for Master.Data
    ## NBirds.y = Total across all sites within year
    ## NBirds.PerSite.y = Mean value per site w/in year (=NBirds.y/NSurveySites)
    ## NBirds.Density.y = Mean density  w/in year (=NBirds.y/NSurveySites/Area)
    ## SENBirds.Density.y = SE of NBirds.Density.y across SimReps simulations
    
    ##Set up new columns
    Master.Data$MaxSurveyDistance <- NA
    
    Master.Data$NBirds.y <- NA
    Master.Data$CloseTerrBirds.y <- NA
    Master.Data$CloseBirds.y <- NA
    Master.Data$CloseSingers.y <- NA
    Master.Data$RawCount.y <- NA
    Master.Data$RawCountPerfectDist.y <- NA
    
    if(SurveyType!="raw"){
      Master.Data$ModelAvgEst.y <- NA
      Master.Data$TopModelEst.y <- NA
      Master.Data$TopModelp.y <- NA
      Master.Data$ModelAvgp.y <- NA
      Master.Data$TopModelname.y <- NA
    }
    Master.Data$NBirds.Density.y <- NA
    Master.Data$CloseTerrBirds.Density.y <- NA
    Master.Data$CloseBirds.Density.y <- NA
    Master.Data$CloseSingers.Density.y <- NA
    Master.Data$RawCount.Density.y <- NA
    Master.Data$RawCountPerfectDist.Density.y <- NA
    
    if(SurveyType!="raw"){
      Master.Data$ModelAvgEst.Density.y <- NA
      Master.Data$TopModelEst.Density.y <- NA
    }
    Master.Data$SENBirds.Density.y <- NA
    Master.Data$SECloseTerrBirds.Density.y <- NA
    Master.Data$SECloseBirds.Density.y <- NA
    Master.Data$SECloseSingers.Density.y <- NA
    Master.Data$SERawCount.Density.y <- NA
    Master.Data$SERawCountPerfectDist.Density.y <- NA
    
    # if(SurveyType!="raw"){
    #   Master.Data$SEModelAvgEst.Density.y <- NA
    #   Master.Data$SETopModelEst.Density.y <- NA
    #   Master.Data$SETopModelp.y <- NA
    #   Master.Data$SEModelAvgp.y <- NA
    # }
    if(SurveyType=="distance"){
      Master.Data$Dist.TopModel.Density.y <- NA
      Master.Data$Dist.ModelAvg.Density.y <- NA
    }
    if(SurveyType=="multiple"){
      Master.Data$NicholsEst.y <- NA 
      Master.Data$NicholsEst.Density.y <- NA 
      Master.Data$SENicholsEst.Density.y <- NA
    }
    if(SurveyType=="nmixture"){
      Master.Data$IndexMaxCount.y <- NA 
      Master.Data$IndexMaxCount.Density.y <- NA 
      Master.Data$SEIndexMaxCount.Density.y <- NA
      Master.Data$BoundedCount.y <- NA 
      Master.Data$BoundedCount.Density.y <- NA 
      Master.Data$SEBoundedCount.Density.y <- NA
    }
    
    ###########################
    ##For distance, export data to Program Distance, then read in results
    
    if(SurveyType=="distance"){
      ## PART 1: Prep data and export for use in Program Distance
      # 
      # for(Iteration in 1:SimReps){
      #   DataIteration <- paste("_",Iteration,".R",sep="")
      #   File.Name <- Analysis.Files[grep(DataIteration,Analysis.Files)]
      #   load(File.Name)
      #   if(SurveyType!=SurveyType.Intended) stop("WRONG ANALYSIS METHOD")
      #   if(Iteration==1){
      #     RR.data.All <- RR.data
      #     II.data.All <- II.data
      #   }
      #   if(Iteration>1){
      #     RR.data$y <- Iteration
      #     II.data$y <- Iteration
      #     RR.data.All <- rbind(RR.data.All,RR.data)
      #     II.data.All <- rbind(II.data.All,II.data)
      #   }
      # }
      # ##Remove unneccessary columns for export to Program Distance
      # RR.data.All$x11.yijr <- NULL
      # RR.data.All$x12.yijr <- NULL
      # RR.data.All$x21.yijr <- NULL
      # RR.data.All$x22.yijr <- NULL
      # RR.data.All$OnlyObs1.yijr <- NULL
      # RR.data.All$OnlyObs2.yijr <- NULL
      # RR.data.All$ObsEstDistAtFirstDetection.yijrObs2 <- NULL
      # RR.data.All$FirstIntervalCounted.yijrObs2 <- NULL
      # RR.data.All$FarnsEverCountedPeriod1.yijr <- NULL
      # RR.data.All$FarnsEverCountedPeriod2.yijr <- NULL
      # RR.data.All$FarnsEverCountedPeriod3.yijr <- NULL
      # RR.data.All$EverCountedPeriod1.yijr <- NULL
      # RR.data.All$EverCountedPeriod2.yijr <- NULL
      # RR.data.All$EverCountedPeriod3.yijr <- NULL
      # RR.data.All$DistAtFirstDetection.yijrObs2 <- NULL
      # RR.data.All$CountObs1.yijr <- NULL
      # RR.data.All$CountObs2.yijr <- NULL
      # RR.data.All$BothObs.yijr <- NULL
      # 
      # ## Add PercentHabitat.yi at observation level & study area at year level for Program Distance
      # RR.data.All.Det <- RR.data.All[RR.data.All$EverCounted.yijr>0,]
      # RR.data.All.Det$PercentHabitat.yi <- NA
      # ifelse(length(unique(Master.Data$StudyHabitatArea.y + Master.Data$StudyMatrixArea.y))==1,
      #        RR.data.All.Det$StudyArea.y <- Master.Data[1,"StudyHabitatArea.y"] + Master.Data[1,"StudyMatrixArea.y"],
      #        stop("Error - StudyArea.y size differed among years"))
      # 
      # for(pp in 1:nrow(RR.data.All.Det)){
      #   ii <- RR.data.All.Det[pp,"i"]
      #   yy <- RR.data.All.Det[pp,"y"]
      #   RR.data.All.Det[pp,"PercentHabitat.yi"] <- II.data.All[II.data.All$i==ii & II.data.All$y==yy,"PercentHabitat.yi"]
      # }
      # 
      # ##Export sim data to Program Distance (Before any R estimation of N or p)
      # ##NOTE: MUST BE SORTED BY SITE, THEN YEAR 
      # 
      # RR.data.All.Det$distance <- RR.data.All.Det$ObsEstDistAtFirstDetection.yijrObs1
      # setwd(SaveDirectory)
      # DATE <- format(Sys.time(), "%m_%d_%Y")
      # write.csv(RR.data.All.Det, file=paste(InformalSimName,
      #                                       "Data for Program Distance",
      #                                       DATE, ".csv", sep=""),row.names=F)
      # 
      ###################
      ## PART 2: Read in results from Program Distance (assembled manually in Excel) 
      
      ##Top Model estimate from Program Distance
      setwd(SaveDirectory)
      Dist.TopModel <- read.csv("TopModel estimates - Uniform Poly 10 Perc Truncation_Oct2016.csv")
      # Dist.TopModel <- read.csv("TopModel estimates - Uniform Poly 10 Perc Truncation.csv")
      Master.Data[,"Dist.TopModel.Density.y"] <- Dist.TopModel$Estimate
      
      ##Model-averaged estimate from Program Distance
      ## use grep to allow small errors in filenames (manually saved)
      setwd(SaveDirectory)
      All.Data.Files <- list.files()
      ProgramDistance.Files <- All.Data.Files[grep("Program_Distance", All.Data.Files)]
      
      ##Note: Density.csv file must have characters NA for Adjustment column of MCDS models
      AIC.PD <- read.csv(ProgramDistance.Files[grep("AIC.csv",ProgramDistance.Files)])
      Densities.PD <- read.csv(ProgramDistance.Files[grep("Density.csv",ProgramDistance.Files)])
      Densities.PD$Weight <- NA
      Weighty.Models <- na.omit(AIC.PD[AIC.PD$Weight>0.01,])
      ##for models with at least 1% AIC weight
      
      for(ii in 1:nrow(Weighty.Models)){
        ii.Model.Name <- as.character(Weighty.Models[ii,"Name"])
        ii.Model.Type <- ifelse(grepl("MCDS",ii.Model.Name),"MCDS","CDS")
        if(grepl("ormal",ii.Model.Name)) ii.Model.Key <- c("Half_Normal","Half-Normal")
        if(grepl("azard",ii.Model.Name)) ii.Model.Key <- "Hazard"
        if(grepl("niform",ii.Model.Name)) ii.Model.Key <- "Uniform"
        if(grepl("osine",ii.Model.Name)) ii.Model.Adj <- "Cosine"
        if(grepl("ermite",ii.Model.Name)) ii.Model.Adj <- "Hermite"
        if(grepl("olynomial",ii.Model.Name)) ii.Model.Adj <- "Polynomial"
        if(ii.Model.Type=="MCDS") ii.Model.Adj <- NA 
        
        ##Break if there are too many / few rows selected for input of AIC Weight
        if(nrow(Densities.PD[Densities.PD$Type==ii.Model.Type &
                             Densities.PD$KeyFunction %in% ii.Model.Key &
                             Densities.PD$Adjustment==ii.Model.Adj,])!=SimReps){
          stop("Error - nrow(Densities.PD) does not match SimReps")}
        
        ##Extract the model weight from AIC.PD for model ii
        if(ii.Model.Type!="MCDS"){
          Densities.PD[Densities.PD$Type==ii.Model.Type &
                         Densities.PD$KeyFunction %in% ii.Model.Key &
                         Densities.PD$Adjustment==ii.Model.Adj,
                       "Weight"] <- AIC.PD[ii,"Weight"]}
        if(ii.Model.Type=="MCDS"){
          Densities.PD[Densities.PD$Type==ii.Model.Type &
                         Densities.PD$KeyFunction %in% ii.Model.Key &
                         is.na(Densities.PD$Adjustment),
                       "Weight"] <- AIC.PD[ii,"Weight"]}
      }
      Densities.PD$Weighted.Est <- Densities.PD$Weight*Densities.PD$Estimate
      
      ##Sum up Model-Avg estimate for each year
      ## Divide by sum(Weighty.Models$Weight) b/c not all models included
      ## (only those with at least 1% of weight).  Usually sum(Weighty.Models$Weight)~=1
      for(ii in 1:SimReps){
        ii.Densities.PD <- Densities.PD[Densities.PD$Year==ii & !is.na(Densities.PD$Year),]
        Master.Data[ii,"Dist.ModelAvg.Density.y"] <- sum(ii.Densities.PD$Weighted.Est)/sum(Weighty.Models$Weight) 
      }
      
      #     Dist.ModelAvg <- read.csv("ModelAvg estimates - 10 Perc Truncation.csv")
      #     Master.Data[,"Dist.ModelAvg.Density.y"] <- Dist.ModelAvg$Estimate
    } ##end distance
    
    #######################################
    ## Load and analyze data within years: 1 year at a time
    
    ##Set up for unmarked
    # unmarkedEstAbundance <- list()
    ObserverList <- list()
    for(Iteration in 1:SimReps){
      
      ##load saved data, use grep to avoid customizing dates in filenames
      DataIteration <- paste("_",Iteration,".R",sep="")
      File.Name <- Analysis.Files[grep(DataIteration,Analysis.Files)]
      load(File.Name)
      if(SurveyType!=SurveyType.Intended) stop("WRONG ANALYSIS METHOD")
      
      ###############
      ##For SurveyType=="distance", run unmarked FIRST to calc effective radius
      ##This radius will stand-in for MaxSurveyDistance for calcs of Ds,Dp,Da 
      ##(CloseTerrBirds.yi, CloseBirds.yi, CloseSingers,yi).
      if(SurveyType=="distance"){
        
        ##If there are sites without detections, you may need to add them.
        ##e.g., levels(BB.Det$Site) <- c(levels(dists$transect), "g")
        ##Not necessary here b/c levels were included by making Site a factor @ BB stage
        
        ##Set up data
        ##Important: site "i" must be a factor
        ##update levels(i) to include sites with 0 count
        ##Select only data with detection:
        RR.data.Det <- RR.data[!is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs1),]
        
        RR.data.Det$i <- as.factor(RR.data.Det$i)
        levels(RR.data.Det$i) <- 1:NSurveySites
        #   summary(RR.data.Det$i)
        #   str(RR.data.Det$i)
        #   head(RR.data.Det)
        #   nrow(RR.data.Det)
        #   nrow(RR.data)
        
        ###Truncate upper 10% of observations
        ## 10% Truncation suggested for point transects by Buckland et al. 2001 pg 151 ("Intro to Distance Sampling")
        Num.Entries.to.Truncate <- round(nrow(RR.data.Det)*0.1,0)
        RR.data.Det <- RR.data.Det[order(RR.data.Det$ObsEstDistAtFirstDetection.yijrObs1),]
        
        ##Truncation distance = closest truncated recorded distance
        #       Truncation.Distance <- ceiling(max(RR.data.Det$ObsEstDistAtFirstDetection.yijrObs1))
        Truncation.Distance <- RR.data.Det[(nrow(RR.data.Det)-Num.Entries.to.Truncate)+1,
                                           "ObsEstDistAtFirstDetection.yijrObs1"]
        MaxSurveyDistance <- Truncation.Distance  ##For distance methods, the area surveyed is pi*truncation.distance^2
        
        ## Truncate distances
        RR.data.Det <- RR.data.Det[1:(nrow(RR.data.Det)-Num.Entries.to.Truncate),]
        
        ## Create bins for distances
        ## Use truncation distance - distances not restricted by 50/100/150 m radius
        #       un.dist.breaks <- 0:max(round(RR.data.Det$ObsEstDistAtFirstDetection.yijrObs1,0),MaxSurveyDistance)
        #       Max.Detection <- max(ceiling(RR.data.Det$ObsEstDistAtFirstDetection.yijrObs1))
        #       un.dist.breaks <- 0:Max.Detection
        un.dist.breaks <- 0:Truncation.Distance
        
        ##format data
        Dist.formatted <- formatDistData(distData=  #ObsEstDistAtFirstDetection.yijrObs1
                                           #                                  data.frame(EverCounted.y[,c("i", "MinDistance.yijr")]),
                                           data.frame(RR.data.Det[,c("i", "ObsEstDistAtFirstDetection.yijrObs1")]),
                                         distCol="ObsEstDistAtFirstDetection.yijrObs1",
                                         transectNameCol="i",
                                         dist.breaks=un.dist.breaks)
        #   summary(Dist.formatted)
        
        ## Create unmarked Frame
        Dist.frame <- unmarkedFrameDS(y=as.matrix(Dist.formatted),
                                      siteCovs=data.frame(PercentHabitat.yi=scale(II.data$PercentHabitat.yi)),
                                      dist.breaks=un.dist.breaks,
                                      survey="point",
                                      unitsIn="m")
        # summary(Dist.frame)
        ##NOTE: Important to scale covariates - problems w/ convergence otherwise
        
        #######################################
        ##Create Models
        ds.halfnorm <- distsamp(~1 ~1, Dist.frame,output="abund", keyfun="halfnorm")
        ds.hazard <- distsamp(~1 ~1, Dist.frame,output="abund", keyfun="hazard")
        ds.uniform <- distsamp(~1 ~1, Dist.frame,output="abund", keyfun="uniform")
        #       ds.halfnorm <- distsamp(~1 ~1, Dist.frame, output="density", unitsOut="ha", keyfun="halfnorm")
        #       ds.hazard <- distsamp(~1 ~1, Dist.frame,output="density", unitsOut="ha", keyfun="hazard")
        #       ds.uniform <- distsamp(~1 ~1, Dist.frame,output="density", unitsOut="ha", keyfun="uniform")
        
        ##FORMULA: ~Detection ~Abundance
        ds.halfnorm.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame,output="abund", keyfun="halfnorm")
        ds.halfnorm.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame,output="abund", keyfun="halfnorm") 
        ds.hazard.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame,output="abund", keyfun="hazard")
        ds.hazard.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame,output="abund", keyfun="hazard")
        ds.uniform.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame,output="abund", keyfun="uniform")
        ds.uniform.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame,output="abund", keyfun="uniform")
        #       ds.halfnorm.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame, output="density", unitsOut="ha", keyfun="halfnorm")
        #       ds.halfnorm.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame, output="density", unitsOut="ha", keyfun="halfnorm") 
        #       ds.hazard.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame, output="density", unitsOut="ha", keyfun="hazard")
        #       ds.hazard.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame, output="density", unitsOut="ha", keyfun="hazard")
        #       ds.uniform.Det.hab <- distsamp(~PercentHabitat.yi ~1, Dist.frame, output="density", unitsOut="ha", keyfun="uniform")
        #       ds.uniform.Abund.hab <- distsamp(~1 ~PercentHabitat.yi, Dist.frame, output="density", unitsOut="ha", keyfun="uniform")
        
        #   summary(ds.halfnorm)  
        #   summary(ds.hazard)
        #   summary(ds.uniform)
        
        #############################
        ## Model Selection
        ##
        ##Create fitList for Model Selection
        Model.fit.list <- fitList("Half.Normal" =ds.halfnorm,
                                  "Hazard"=ds.hazard,
                                  "Uniform"=ds.uniform,
                                  "Half Normal Det~%habitat"=ds.halfnorm.Det.hab,
                                  "Half Normal Abund~%habitat"=ds.halfnorm.Abund.hab,
                                  "Hazard Det~%habitat"=ds.hazard.Det.hab,
                                  "Hazard Abund~%habitat"=ds.hazard.Abund.hab,
                                  "Uniform Det~%habitat"=ds.uniform.Det.hab,
                                  "Uniform Abund~%habitat"=ds.uniform.Abund.hab)
        
        ##Create list to call models b/c fitList is unwieldy
        Model.call.list <- list("Half.Normal" =ds.halfnorm,
                                "Hazard"=ds.hazard,
                                "Uniform"=ds.uniform,
                                "Half Normal Det~%habitat"=ds.halfnorm.Det.hab,
                                "Half Normal Abund~%habitat"=ds.halfnorm.Abund.hab,
                                "Hazard Det~%habitat"=ds.hazard.Det.hab,
                                "Hazard Abund~%habitat"=ds.hazard.Abund.hab,
                                "Uniform Det~%habitat"=ds.uniform.Det.hab,
                                "Uniform Abund~%habitat"=ds.uniform.Abund.hab) 
        
        Model.Selection1 <- modSel(Model.fit.list) ##Rank by AIC
        # MultCoef <- coef(Model.fit.list)  ##Lots of coef info
        
        TopModelname <- Model.Selection1@Full$model[1]
        TopModel <- Model.call.list[[TopModelname]]
        
        ##Extract Abundance estimates for top model - distance methods (directly, not via abundance)
        # if(grepl("Abund",TopModelname)){ ##for models w/ Abund~%hab
        #   TopModelLC <- linearComb(TopModel, c(1, 0), type = "state")
        #   TopModelBackTransAbund <- backTransform(TopModelLC)}
        # #       Master.Data[Iteration,"TopModelEst.Density.y"] <- coef(backTransform(TopModelLC))}
        # 
        # if(grepl("Abund",TopModelname)==F){    ##for models w/o Abund~%hab  
        #   TopModelBackTransAbund <- backTransform(TopModel, type="state")}
        # #       Master.Data[Iteration,"TopModelEst.Density.y"] <- coef(backTransform(TopModel, type="state"))}
        # 
        # TopModelEst.PerSite.y <- coef(TopModelBackTransAbund) ##Abundance, site-specific mean
        # Master.Data[Iteration,"TopModelEst.y"] <- TopModelEst.PerSite.y*NSurveySites  ##Abundance, mean across all sites
        # #     Master.Data[Iteration,"SEAbund.TopModel"] <- SE(TopModelBackTransAbund)
        # 
        Master.Data[Iteration,"TopModelEst.y"] <- sum(predict(TopModel, type="state")$Predicted)
        
        ##Model Averaged prediction
        Master.Data[Iteration,"ModelAvgEst.y"] <- sum(predict(Model.fit.list, type="state")$Predicted)
        Master.Data[Iteration,"TopModelname.y"] <- TopModelname
        
        ################################
        #       ## Top Model: Effective radius, Effective Area, & detection
        ##NOTE: if truncated, replace "Max.Detection" with "Truncation.Distance"
        #       if(TopModelname=="Half.Normal"){
        #         Dist.sigma <- exp(coef(TopModel, type="det")) ##half-normal shape parameter (sigma)
        #         Effective.Area <- 2*pi * integrate(grhn, 0, Max.Detection, sigma=Dist.sigma)$value # effective area
        #       }
        #       
        #       if(TopModelname=="Hazard"){
        #         Dist.shape <- exp(coef(TopModel, type="det")) ##half-normal shape parameter (sigma)
        #         Dist.scale <- exp(coef(TopModel, type="scale")) ##half-normal scale parameter
        #         Effective.Area <- 2*pi * integrate(grhaz, 0, Max.Detection, shape=Dist.shape, scale=Dist.scale)$value # effective area
        #       }
        #       if(TopModelname=="Hazard Det~%habitat"){
        #         ##Note: under development
        #         Dist.shape1 <- exp(coef(TopModel, type="det"))[1] ##half-normal shape parameter (sigma)
        #         Dist.shape2 <- exp(coef(TopModel, type="det"))[2] ##half-normal shape parameter  for Percent hab covar
        #         Dist.scale <- exp(coef(TopModel, type="scale")) ##half-normal scale parameter
        #         Effective.Area <- 2*pi * integrate(grhaz, 0, Max.Detection, shape=c(Dist.shape1), scale=Dist.scale)$value # effective area
        #       }
        #       Effective.Radius <- sqrt(Effective.Area / pi) # effective radius
        #       Est.Det.Probability <- Effective.Area / (pi*Max.Detection^2) # detection probability
        
      }  ##end distance
      
      
      ## Year-specific abundance
      Master.Data[Iteration,"NBirds.y"] <- tapply(II.data$NBirds.yi,II.data$y, sum) 
      ##NOTE: YY.data$Ns.y is NOT YY.data$NBirds.y and shouldn't be used
      
      #   ## Abundance ~ %Habitat in year Iteration
      #   plot(II.data$NBirds.yi~II.data$PercentHabitat.yi, 
      #        xlim=c(0.3,1),ylim=c(115, 275), main=paste("Iteration=",Iteration)) 
      
      #########
      ##Birds with territories within radius MaxSurveyDistance: (Ns)=N*ps 
      Master.Data[Iteration,"MaxSurveyDistance"] <- MaxSurveyDistance
      RR.data$CloseTerrBirds.yij <- as.numeric(RR.data$ClosestAxesDistance.yij<=MaxSurveyDistance)
      II.data$CloseTerrBirds.yi <- tapply(RR.data$CloseTerrBirds.yij,RR.data$i,sum)
      ifelse(SurveyType=="nmixture",
             Master.Data[Iteration,"CloseTerrBirds.y"] <- (tapply(II.data$CloseTerrBirds.yi, II.data$y, sum)/NReps),
             Master.Data[Iteration,"CloseTerrBirds.y"] <- tapply(II.data$CloseTerrBirds.yi, II.data$y, sum) 
      )  ##NOTE: for replicated counts, CloseTerrBirds.y = total birds / NUMBER OF REPS
      
      #########
      ##Birds within MaxSurveyDistance (m) from observer: (Np) =N*ps*pp 
      ## At the beginning of the survey (k=0)
      RR.data$CloseBirds.yijr <- as.numeric(RR.data$Distance.yijr0<=MaxSurveyDistance)
      II.data$CloseBirds.yi <- tapply(RR.data$CloseBirds.yijr,RR.data$i,sum)
      ifelse(SurveyType=="nmixture",
             Master.Data[Iteration,"CloseBirds.y"] <- (tapply(II.data$CloseBirds.yi, II.data$y, sum)/NReps),
             Master.Data[Iteration,"CloseBirds.y"] <- tapply(II.data$CloseBirds.yi, II.data$y, sum) 
      )  ##NOTE: for replicated counts, CloseBirds.y = total birds / NUMBER OF REPS
      
      ## CloseBirds excludes all birds with Dist >= MaxSurveyDistance
      ## RawCount RETAINS all birds count=1 w/ ObsEstDist<=MaxSurveyDistance (slightly different)
      
      ##############
      ## Available Birds: (Na) =N*ps*pp*pa
      RR.data$CloseSinger.yijr <- as.numeric(RR.data$Distance.yijr0<MaxSurveyDistance & RR.data$EverSing.yijr>0)
      II.data$CloseSingers.yi <- tapply(RR.data$CloseSinger.yijr, RR.data$i, sum)
      ifelse(SurveyType=="nmixture",
             Master.Data[Iteration,"CloseSingers.y"] <- tapply(II.data$CloseSingers.yi,II.data$y, sum)/NReps,
             Master.Data[Iteration,"CloseSingers.y"] <- tapply(II.data$CloseSingers.yi,II.data$y, sum) 
      )  ##NOTE: for replicated counts, CloseSingers.y = total available birds / NUMBER OF REPS
      
      ############
      ##Raw Count: Birds detected and estimated by Observer to be w/in survey radius
      RR.data$CloseCounted.yijr <- NA
      RR.data$CloseCounted.yijr <- as.numeric(!is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs1) & RR.data$ObsEstDistAtFirstDetection.yijrObs1<=MaxSurveyDistance)
      
      if(SurveyType=="multiple"){
        RR.data$CloseCounted.yijr <- RR.data$CloseCounted.yijr + as.numeric(!is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs2) & RR.data$ObsEstDistAtFirstDetection.yijrObs2<=MaxSurveyDistance)
      }
      RR.data$CloseCounted.yijr <- ifelse(RR.data$CloseCounted.yijr>0,1,0)
      II.data[,"RawCount.yi"] <- tapply(RR.data$CloseCounted.yijr, RR.data$i,sum)
      ifelse(SurveyType=="nmixture",
             Master.Data[Iteration,"RawCount.y"] <- tapply(II.data$RawCount.yi,II.data$y, sum)/NReps,
             Master.Data[Iteration,"RawCount.y"] <- tapply(II.data$RawCount.yi,II.data$y, sum) 
      )   ##NOTE: for replicated counts, RawCount.y = total birds seen / NUMBER OF REPS
      
      
      ################
      ##Perect Distance Count - Birds detected and counted, if observer
      ## estimation of distance were perfect
      RR.data$ClosePerfectDistCounted.yijr <- NA
      RR.data$ClosePerfectDistCounted.yijr <- as.numeric(!is.na(RR.data$DistAtFirstDetection.yijrObs1) & RR.data$DistAtFirstDetection.yijrObs1<=MaxSurveyDistance)
      
      if(SurveyType=="multiple"){
        RR.data$ClosePerfectDistCounted.yijr <- RR.data$ClosePerfectDistCounted.yijr + as.numeric(!is.na(RR.data$DistAtFirstDetection.yijrObs2) & RR.data$DistAtFirstDetection.yijrObs2<=MaxSurveyDistance)
      }
      RR.data$ClosePerfectDistCounted.yijr <- ifelse(RR.data$ClosePerfectDistCounted.yijr>0,1,0)
      II.data[,"RawCountPerfectDist.yi"] <- tapply(RR.data$ClosePerfectDistCounted.yijr, RR.data$i,sum)
      ifelse(SurveyType=="nmixture",
             Master.Data[Iteration,"RawCountPerfectDist.y"] <- tapply(II.data$RawCountPerfectDist.yi,II.data$y, sum)/NReps,
             Master.Data[Iteration,"RawCountPerfectDist.y"] <- tapply(II.data$RawCountPerfectDist.yi,II.data$y, sum) 
      )   ##NOTE: for replicated counts, RawPerfectDistCount.y = total birds counted / NUMBER OF REPS
      
      
      ################
      ##Add Close Bird info to IIRR.data (for analysis when SurveyType="nmixture")
      if(SurveyType=="nmixture"){
        IIRR.data$CloseCounted.yir <- NA
        IIRR.data$ClosePerfectDistCounted.yijr <- NA
        CLOSECOUNTED.YIR <- tapply(RR.data$CloseCounted.yijr,list(RR.data$i,RR.data$r), sum) ###sum across birds j
        CLOSEPERFECTDISTCOUNTED.YIR <- tapply(RR.data$ClosePerfectDistCounted.yijr,list(RR.data$i,RR.data$r), sum) ###sum across birds j
        for(ii in 1:NSurveySites){
          for(rr in 1:NReps){
            if(nrow(CLOSECOUNTED.YIR)!=NSurveySites) stop("ERROR: sites/reps mismatch in CLOSECOUNTED.YIR")
            if(ncol(CLOSECOUNTED.YIR)!=NReps) stop("ERROR: sites/reps mismatch in CLOSECOUNTED.YIR")
            IIRR.data[IIRR.data$i==ii & IIRR.data$r==rr,
                      "CloseCounted.yir"] <- CLOSECOUNTED.YIR[ii,rr]
            IIRR.data[IIRR.data$i==ii & IIRR.data$r==rr,
                      "IIRR.data$ClosePerfectDistCounted.yijr"] <- CLOSEPERFECTDISTCOUNTED.YIR[ii,rr]
          }
        }
      }
      
      ############
      ##Nichols Estimator (Multiple observer only)
      #   ##NOTE: Assumes NReps==1 & Assumes NYears==1
      
      if(SurveyType=="multiple"){
        #     NicholsEstAbundance <- list()  
        
        ##Summarize Nichols x11 x22 data by site    
        for(ii in 1:NSurveySites){
          
          if(II.data[II.data$i==ii, "RawCount.yi"]>0) { 
            CountedClose.i <- RR.data[RR.data$CloseCounted.yijr>0 & RR.data$i==ii,]
            II.data[II.data$i==ii,"x11.yi"] <- sum(CountedClose.i$x11.yijr)
            II.data[II.data$i==ii,"x22.yi"] <- sum(CountedClose.i$x22.yijr)
            II.data[II.data$i==ii,"x12.yi"] <- sum(CountedClose.i$x12.yijr)
            II.data[II.data$i==ii,"x21.yi"] <- sum(CountedClose.i$x21.yijr)
          }
          
          if(nrow(RR.data[RR.data$CloseCounted.yijr>0 & RR.data$i==ii,])==0){
            II.data[II.data$i==ii,"x11.yi"] <- 0
            II.data[II.data$i==ii,"x22.yi"] <- 0
            II.data[II.data$i==ii,"x12.yi"] <- 0
            II.data[II.data$i==ii,"x21.yi"] <- 0
          }
        }
        
        Master.Data[Iteration,"NicholsEst.y"] <- MultObs.Nichols(x11=sum(II.data$x11.yi), 
                                                                 x21=sum(II.data$x21.yi), 
                                                                 x22=sum(II.data$x22.yi), 
                                                                 x12=sum(II.data$x12.yi))["Nhat"]
      } ## end Nichols
      
      if(SurveyType=="removal"){
        ############
        ##Removal Data: 
        ## Convert EverCounted info (which birds were observed in each period?)
        ## into Removal info (in which period was bird first observered?)
        RR.data$RemovalPeriod1 <- as.numeric(RR.data$EverCountedPeriod1.yijr>0 &
                                               !is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs1) &
                                               RR.data$ObsEstDistAtFirstDetection.yijrObs1<MaxSurveyDistance)
        RR.data$RemovalPeriod2 <- as.numeric(RR.data$EverCountedPeriod2.yijr>0 & 
                                               RR.data$EverCountedPeriod1.yijr==0 &
                                               !is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs1) &
                                               RR.data$ObsEstDistAtFirstDetection.yijrObs1<MaxSurveyDistance)
        RR.data$RemovalPeriod3 <- as.numeric(RR.data$EverCountedPeriod3.yijr>0 & 
                                               RR.data$EverCountedPeriod1.yijr==0 &
                                               RR.data$EverCountedPeriod2.yijr==0 &
                                               !is.na(RR.data$ObsEstDistAtFirstDetection.yijrObs1) &
                                               RR.data$ObsEstDistAtFirstDetection.yijrObs1<MaxSurveyDistance)
      }
      
      ############
      ## unmarked Estimators (all non-index survey types)
      
      if(SurveyType=="multiple"){
        ObserverList[[Iteration]] <- matrix(ObserverIDs, NSurveySites, NSimultaneousObservers, byrow=TRUE)
        ##access via ObserverList[[y]][i,]
        
        Mult.unmarked <- data.frame(OnlyA=rep(0,NSurveySites),
                                    OnlyB=rep(0,NSurveySites),
                                    Both=rep(0,NSurveySites))
        
        for(ii in 1:NSurveySites){    
          CountedClose.i <- RR.data[RR.data$i==ii & RR.data$CloseCounted.yijr>0,]
          Mult.unmarked[ii,"Both"] <- sum(CountedClose.i$BothObs.yijr)
          Mult.unmarked[ii,"OnlyA"] <- sum(CountedClose.i$OnlyObs1.yijr)
          Mult.unmarked[ii,"OnlyB"] <- sum(CountedClose.i$OnlyObs2.yijr)
        }
        
        ##Create unmarked Frame from data
        Mult.Frame <- unmarkedFrameMPois(y=Mult.unmarked[,c("OnlyA","OnlyB","Both")], 
                                         obsCovs=list(observer=ObserverList[[Iteration]]),
                                         siteCovs=data.frame(PercentHabitat=scale(II.data$PercentHabitat.yi)),
                                         type="double")
        ##Create Multiple Observer models
        ##Note: unmarked here uses Independent Observer approach (Alldredge et al. 2006)
        
        ##models w/ intercept (ObsA=intercept term, add coef for ObsB to get ObsB term)
        # obs.multa <- multinomPois(~observer ~1, Mult.Frame)
        # obs.abundhab.multa <- multinomPois(~observer + PercentHabitat ~1, Mult.Frame)
        
        if(Iteration!=29 | Iteration==29 & MaxSurveyDistance!=150){
          ##models with no intercept (via -1 term in abundance formula)
          ##coefficients therefore refer exactly to each obs (no addition)
          ##          multinomPois(~DETECTION ~ABUNDANCE, Mult.Frame) 
          null.mult <- multinomPois(~1 ~1, Mult.Frame)
          detobs.mult <- multinomPois(~observer-1 ~1, Mult.Frame)
          detobshab.mult <- multinomPois(~observer + PercentHabitat-1 ~1, Mult.Frame)
          detobs.abundhab.mult <- multinomPois(~observer-1 ~PercentHabitat, Mult.Frame)
          
          ##Create fitList for Model Selection
          Model.fit.list <- fitList(  "Null"            = null.mult,
                                      "Det ~ Obs"       = detobs.mult,
                                      "Det ~ Obs and Hab" = detobshab.mult,
                                      "Det~Obs, Abund~Hab"     = detobs.abundhab.mult
          )
          
          ##Create list to call models b/c fitList is unwieldy
          Model.call.list <- list(  "Null"              = null.mult,
                                    "Det ~ Obs"         = detobs.mult,
                                    "Det ~ Obs and Hab"   = detobshab.mult,
                                    "Det~Obs, Abund~Hab"= detobs.abundhab.mult
          )}
        
        if(Iteration==29 & MaxSurveyDistance==150){
          ##models with no intercept (via -1 term in abundance formula)
          ##coefficients therefore refer exactly to each obs (no addition)
          ##          multinomPois(~DETECTION ~ABUNDANCE, Mult.Frame) 
          null.mult <- multinomPois(~1 ~1, Mult.Frame)
          detobs.mult <- multinomPois(~observer-1 ~1, Mult.Frame)
          #       detobshab.mult <- multinomPois(~observer + PercentHabitat-1 ~1, Mult.Frame)
          detobs.abundhab.mult <- multinomPois(~observer-1 ~PercentHabitat, Mult.Frame)
          
          ##Create fitList for Model Selection
          Model.fit.list <- fitList(  "Null"            = null.mult,
                                      "Det ~ Obs"       = detobs.mult,
                                      #                                  "Det ~ Obs and Hab" = detobshab.mult,
                                      "Det~Obs, Abund~Hab"     = detobs.abundhab.mult
          )
          
          ##Create list to call models b/c fitList is unwieldy
          Model.call.list <- list(  "Null"              = null.mult,
                                    "Det ~ Obs"         = detobs.mult,
                                    #                                "Det ~ Obs and Hab"   = detobshab.mult,
                                    "Det~Obs, Abund~Hab"= detobs.abundhab.mult
          )
        }
        
        ##################################
        ##Add data columns for output to program DOBSERV
        II.data.DOBSERV <- II.data
        II.data.DOBSERV$MaxSurveyDistance <- MaxSurveyDistance
        II.data.DOBSERV$DOBSERV1 <- unlist(PrimaryObsNumber.yir.List)  ##numeric: which observer is the primary observer
        Iteration2Digit <- ifelse(Iteration<10,paste("0",Iteration, sep=""),as.character(Iteration))
        II.data.DOBSERV$DOBSERV2 <- paste("BT",Iteration2Digit, sep="")  ##which species & year was observed (4- character abbreviation)
        
        ##Dependent
        II.data.DOBSERV$DOBSERV3D <- NA  ##detections by primary observer (regardless of whether the secondary observer detected them)
        II.data.DOBSERV$DOBSERV4D <- NA  ##detections by the secondary observer which were not detected by the primary observer.
        
        ##Independent
        II.data.DOBSERV$DOBSERV3I <- Mult.unmarked[,"OnlyA"]  ##detections by the first observer which were not detected by the second observer
        II.data.DOBSERV$DOBSERV4I <- Mult.unmarked[,"OnlyB"]  ##detections by the second observer which were not detected by the first observer
        II.data.DOBSERV$DOBSERV5I <- Mult.unmarked[,"Both"]  ##detections by both observers
        
        for(ii in 1:NSurveySites){
          if(PrimaryObsNumber.yir.List[[1]][ii]==1){  ##Obs 1 = primary
            II.data.DOBSERV[ii,"DOBSERV3D"] <- II.data.DOBSERV[ii,"x11.yi"]
            II.data.DOBSERV[ii,"DOBSERV4D"] <- II.data.DOBSERV[ii,"x21.yi"]
          }
          if(PrimaryObsNumber.yir.List[[1]][ii]==2){  ##Obs 2 = primary
            II.data.DOBSERV[ii,"DOBSERV3D"] <- II.data.DOBSERV[ii,"x22.yi"]
            II.data.DOBSERV[ii,"DOBSERV4D"] <- II.data.DOBSERV[ii,"x12.yi"]
          }
        }
        ##Independent Observer data for DOBSERV
        II.data.DOBSERVI <- II.data.DOBSERV[,c("DOBSERV1","DOBSERV2","DOBSERV3I","DOBSERV4I","DOBSERV5I")]
        ##Dependent Observer data for DOBSERV
        II.data.DOBSERVD <- II.data.DOBSERV[,c("DOBSERV1","DOBSERV2","DOBSERV3D","DOBSERV4D")]
        
        if(Iteration==1){
          II.data.DOBSERVI.All <- II.data.DOBSERVI
          II.data.DOBSERVD.All <- II.data.DOBSERVD
        }
        if(Iteration>1){
          II.data.DOBSERVI.All <- rbind(II.data.DOBSERVI.All,II.data.DOBSERVI)
          II.data.DOBSERVD.All <- rbind(II.data.DOBSERVD.All,II.data.DOBSERVD)
        }
      }  ##end multiple
      
      if(SurveyType=="nmixture") {
        
        ##Set up unmarked frame
        NMix1 <- data.frame(y.1=rep(NA,NSurveySites),y.2=rep(NA,NSurveySites),y.3=rep(NA,NSurveySites))
        for(ii in 1:NSurveySites){
          for(rr in 1:NReps){
            NMix1[ii,rr] <- IIRR.data[IIRR.data$i==ii & IIRR.data$r==rr,"CloseCounted.yir"]
          }
        }
        NMix.Frame <- unmarkedFramePCount(NMix1,
                                          siteCovs=data.frame(PercentHabitat=scale(II.data$PercentHabitat.yi)))
        ###################
        ## Raw Max Count (nmixture ONLY)
        MAXCOUNT <- data.frame("A"=rep(NA,NSurveySites))
        for(ii in 1:NSurveySites){
          MAXCOUNT[ii,1] <- max(NMix1[ii,])
        }
        Master.Data[Iteration,"IndexMaxCount.y"] <- sum(MAXCOUNT) ##Raw Max Count
        
        ###################
        ## Bounded Count (nmixture ONLY)  From Johnson et al. 2007 - The bounded count method for analysis of lek counts
        ##Estimator = 2*max count - 2nd largest count
        BOUNDEDCOUNT <- data.frame("A"=rep(NA,NSurveySites))
        for(ii in 1:NSurveySites){
          BOUNDEDCOUNT[ii,1] <- (NMix1[ii,][order(NMix1[ii,])][,3])*2-NMix1[ii,][order(NMix1[ii,])][,2]
        }
        Master.Data[Iteration,"BoundedCount.y"] <- sum(BOUNDEDCOUNT) ## Bounded Count for year Iteration
        
        IIRR.data$Iteration <- Iteration
        if(Iteration==1){IIRR.data.All <- IIRR.data}
        if(Iteration>1){IIRR.data.All <- rbind(IIRR.data.All,IIRR.data)}
        
        ##########
        ## unmarked estimator
        ##  unmarkedFramePCount(~DETECTION ~ABUNDANCE, unframe.y) 
        
        ##Create Models
        null.nmix <- pcount(~1 ~1, NMix.Frame)
        abundhab.nmix <- pcount(~1 ~PercentHabitat, NMix.Frame)
        dethab.nmix <- pcount(~PercentHabitat ~1, NMix.Frame)
        
        ##Create list of models
        Model.fit.list <- fitList(
          "Null"       = null.nmix,
          "Abund ~ Hab" = abundhab.nmix,
          "Det ~ Hab"   = dethab.nmix )
        
        ##Create list to call models b/c fitList is unwieldy
        Model.call.list <- list(
          "Null"       = null.nmix,
          "Abund ~ Hab" = abundhab.nmix,
          "Det ~ Hab"   = dethab.nmix )
        
      }  ## end nmixture    
      
      if(SurveyType=="removal"){
        ##Specify PiFun & o2y b/c the intervals in this example are unequal (2,3,5 min) 
        instRemPiFun <- function(p) {
          M <- nrow(p)
          J <- ncol(p)
          pii <- matrix(NA, M, J)
          p[,1] <- pii[,1] <- 1 - (1 - p[,1])^2
          p[,2] <- 1 - (1 - p[,2])^3
          p[,3] <- 1 - (1 - p[,3])^5
          for(i in 2:J) {
            pii[,i] <- pii[, i - 1]/p[, i - 1] * (1 - p[, i - 1]) * p[, i]
          }
          return(pii)
        }
        
        # Associated obsToY matrix required by unmarkedFrameMPois
        o2y <- diag(3) # if y has 3 columns
        o2y[upper.tri(o2y)] <- 1
        
        unmarkedRemovalData <- data.frame("RemovalPeriod1.yi"=tapply(RR.data$RemovalPeriod1, RR.data$i,sum),
                                          "RemovalPeriod2.yi"=tapply(RR.data$RemovalPeriod2, RR.data$i,sum),
                                          "RemovalPeriod3.yi"=tapply(RR.data$RemovalPeriod3, RR.data$i,sum))
        II.data[,c("RemovalPeriod1.yi",
                   "RemovalPeriod2.yi",
                   "RemovalPeriod3.yi")] <- unmarkedRemovalData                              
        
        Removal.Frame <- unmarkedFrameMPois(unmarkedRemovalData,
                                            siteCovs=data.frame(PercentHabitat.yi=scale(II.data$PercentHabitat.yi)),
                                            piFun="instRemPiFun",
                                            obsToY=o2y)
        #     summary(Removal.Frame)
        
        ## Create removal models  
        ##FORMULA: ~Detection ~Abundance
        null.removal <- multinomPois(~1 ~1,data=Removal.Frame)#,starts=c(5,0.5))
        
        if(MaxSurveyDistance!=50 | MaxSurveyDistance==50 & Iteration!=28){
          dethab.removal <- multinomPois(~PercentHabitat.yi ~1,data=Removal.Frame)#,starts=c(5,0.5))
        }
        abundhab.removal <- multinomPois(~1 ~PercentHabitat.yi,data=Removal.Frame)#,starts=c(5,0.5))
        
        if(MaxSurveyDistance==50  & Iteration==28){
          ## Model Selection
          ##Create fitList for Model Selection
          Model.fit.list <- fitList("Null"= null.removal,
                                    # "Det ~ Hab"=dethab.removal,
                                    "Abund ~ Hab"=abundhab.removal)
          
          ##Create list to call models b/c fitList is unwieldy
          Model.call.list <- list("Null"= null.removal,
                                  # "Det ~ Hab"=dethab.removal,
                                  "Abund ~ Hab"=abundhab.removal)
        }
        if(MaxSurveyDistance!=50 | MaxSurveyDistance==50 & Iteration!=28){
          ## Model Selection
          ##Create fitList for Model Selection
          Model.fit.list <- fitList("Null"= null.removal,
                                    "Det ~ Hab"=dethab.removal,
                                    "Abund ~ Hab"=abundhab.removal)
          
          ##Create list to call models b/c fitList is unwieldy
          Model.call.list <- list("Null"= null.removal,
                                  "Det ~ Hab"=dethab.removal,
                                  "Abund ~ Hab"=abundhab.removal)
        }
      }  ##end removal
      
      
      ##For non-distance & non-raw methods, extract Abundance estimates
      if(SurveyType!="raw" & SurveyType!="distance"){
        Model.Selection1 <- modSel(Model.fit.list) ##Rank by AIC
        # MultCoef <- coef(Model.fit.list)  ##Lots of coef info
        
        TopModelname <- Model.Selection1@Full$model[1]
        TopModel <- Model.call.list[[TopModelname]]
        
        ##Extract abundance estimates for top model - NON-distance methods (abundance)
        # if(grepl("Abund",TopModelname)){ ##for models w/ Abund~%hab
        #   TopModelLC <- linearComb(TopModel, c(1, 0), type = "state")
        #   TopModelBackTransAbund <- backTransform(TopModelLC)}
        # 
        # if(grepl("Abund",TopModelname)==F){    ##for models w/o Abund~%hab  
        #   TopModelBackTransAbund <- backTransform(TopModel, type="state")}  
        # 
        # TopModelEst.PerSite.y <- coef(TopModelBackTransAbund) ##Abundance, site-specific mean
        # Master.Data[Iteration,"TopModelEst.y"] <- TopModelEst.PerSite.y*NSurveySites  ##Abundance, mean across all sites
        # #     Master.Data[Iteration,"SEAbund.TopModel"] <- SE(TopModelBackTransAbund)
        Master.Data[Iteration,"TopModelEst.y"] <- sum(predict(TopModel, type="state")$Predicted)
        
        ##Extract detection estimates for top model - non-distance only    
        ##for models w/ Det~Obs, no %Hab in Det (Abund ~ Hab OK)
        if(grepl("Obs",TopModelname) & !grepl("and",TopModelname)){ 
          TopModelLCObsA <- linearComb(TopModel, c(1, 0), type = "det")
          TopModelLCObsB <- linearComb(TopModel, c(0, 1), type = "det")
          TopModelBackTransDetObsA <- backTransform(TopModelLCObsA)
          TopModelBackTransDetObsB <- backTransform(TopModelLCObsB)
          TopModelBackTransDet <- mean(c(coef(TopModelBackTransDetObsA),
                                         coef(TopModelBackTransDetObsB)))  ##mean p across 2 observers
        }
        
        ##for models w/ Det~%hab, no Obs
        if(grepl("Det",TopModelname) & !grepl("Obs",TopModelname)){ 
          TopModelLC <- linearComb(TopModel, c(1, 0), type = "det")
          TopModelBackTransDet <- coef(backTransform(TopModelLC))} 
        
        ##for models w/ Det~ %hab + Obs
        if(grepl("Det",TopModelname) & grepl("Hab",TopModelname) & grepl("Obs",TopModelname) & !grepl("Abund",TopModelname)){
          TopModelLCObsA <- linearComb(TopModel, c(1, 0,0), type = "det") ##Detection, Obs A, avg percent Hab
          TopModelLCObsB <- linearComb(TopModel, c(0, 1,0), type = "det") ##Detection, Obs B, avg percent Hab
          TopModelBackTransDetObsA <- backTransform(TopModelLCObsA)
          TopModelBackTransDetObsB <- backTransform(TopModelLCObsB)
          TopModelBackTransDet <- mean(c(coef(TopModelBackTransDetObsA),coef(TopModelBackTransDetObsB)))  ##mean p across 2 observers
        }
        ##for models w/o Det ~ %Hab or Obs (Null for Det)
        if(grepl("Det",TopModelname)==F){      
          TopModelBackTransDet <- coef(backTransform(TopModel, type="det"))}
        
        Master.Data[Iteration,"TopModelp.y"] <- TopModelBackTransDet
        #     Master.Data[Iteration,"SETopModelp.y"] <- SE(TopModelBackTransDet)
        
        Master.Data[Iteration,"ModelAvgEst.y"] <- sum(predict(Model.fit.list, type="state")$Predicted)
        Master.Data[Iteration,"ModelAvgp.y"] <- mean(predict(Model.fit.list, type="det")$Predicted)
        #       Master.Data[Iteration,"SEModelAvgp.y"] <- mean(predict(Model.fit.list, type="det")$SE)
        Master.Data[Iteration,"TopModelname.y"] <- TopModelname
      }  ##end not-raw, not-distance
      
      print(paste(SurveyType,"Faux Year =", Iteration, 
                  if(SurveyType!="raw"){paste(", unmarked Top Model =",Model.Selection1@Full[1,"model"])}))
      
    } ##end Iteration
    ##NOTE: if SurveyType==nmixture, "K was not specified" warnings ARE expected
    
    if(SurveyType=="multiple"){
      ###Save data for output to DOBSERV
      setwd(SaveDirectory)
      write.csv(II.data.DOBSERVI.All, file=paste("Independent Obs Output for DOBSERV", 
                                                 MaxSurveyDistance,
                                                 DATE,
                                                 "m.csv", sep=""), row.names=F)
      write.csv(II.data.DOBSERVD.All, file=paste("Dependent Obs Output for DOBSERV", 
                                                 MaxSurveyDistance,
                                                 DATE,
                                                 "m.csv", sep=""), row.names=F)
    }
    
    #######################################
    ##Analyze data across years
    ##All Densities in units Birds/ha = ((Birds/unname(NSurveySites))/area m^2)*10000 m^2/ha
    
    ##True Densities
    ##True Density of birds simulated (even those beyond radius) (N/Area)
    SimulatedArea.y <- unique(II.data$Area.yi)  ##m^2
    if(length(SimulatedArea.y)>1) stop("ERROR:Unequal simulation area among sites - check parameters")
    Master.Data$NBirds.Density.y <- (Master.Data$NBirds.y/unname(NSurveySites))/SimulatedArea.y*10000
    
    ##True Density of birds with overlapping territories (birds/ha) (Ns/Area)
    Master.Data$CloseTerrBirds.Density.y <- ((Master.Data$CloseTerrBirds.y/unname(NSurveySites))/(pi*Master.Data$MaxSurveyDistance^2))*10000 ##birds present (density)
    
    ##True Density of birds present (birds/ha) (Np/Area)
    Master.Data$CloseBirds.Density.y <- ((Master.Data$CloseBirds.y/unname(NSurveySites))/(pi*Master.Data$MaxSurveyDistance^2))*10000 ##birds present (density)
    
    ##True Density of available, present birds (birds/ha) (Na/Area)
    Master.Data$CloseSingers.Density.y <- ((Master.Data$CloseSingers.y/unname(NSurveySites))/(pi*Master.Data$MaxSurveyDistance^2))*10000  ##birds available (density)
    
    ##Estimates of density 
    if(SurveyType!="raw"){
      ##Model Averaged (unmarked)
      Master.Data$ModelAvgEst.Density.y <- (((Master.Data$ModelAvgEst.y)/unname(NSurveySites))/(pi*MaxSurveyDistance^2))*10000
      ##Top Model (unmarked)
      Master.Data$TopModelEst.Density.y <- (((Master.Data$TopModelEst.y)/unname(NSurveySites))/(pi*MaxSurveyDistance^2))*10000
    }
    
    if(SurveyType=="multiple"){
      ##Estimate of density from Nichols et al. 2000
      Master.Data$NicholsEst.Density.y <- (((Master.Data$NicholsEst.y)/unname(NSurveySites))/(pi*MaxSurveyDistance^2))*10000
    }
    
    if(SurveyType=="nmixture"){
      ##Estimate of Density from Max Count (Max # of birds seen on any 1 rep /ha)
      Master.Data$IndexMaxCount.Density.y <- ((Master.Data$IndexMaxCount.y/unname(NSurveySites))/(pi*MaxSurveyDistance^2))*10000
      
      ##Estimate of Density from Bounded Count (2*max - 2nd largest /ha)
      Master.Data$BoundedCount.Density.y <- ((Master.Data$BoundedCount.y/unname(NSurveySites))/(pi*MaxSurveyDistance^2))*10000
    }
    
    ##Estimate of Density from Raw Count (total birds seen/ha) (C/Area)
    Master.Data$RawCount.Density.y <- (((Master.Data$RawCount.y)/unname(NSurveySites))/(pi*Master.Data$MaxSurveyDistance^2))*10000
    
    ##Estimate of Density from Raw Count if distance estimation were perfect
    ## (total birds seen/ha) (C/Area)
    Master.Data$RawCountPerfectDist.Density.y <- (((Master.Data$RawCountPerfectDist.y)/unname(NSurveySites))/(pi*Master.Data$MaxSurveyDistance^2))*10000
    
    Estimator.Results <- data.frame(Estimator=EstimatorsandTrueDensity,
                                    NSurveySites=unname(NSurveySites),
                                    Mean=rep(NA,length(EstimatorsandTrueDensity)),
                                    SD=rep(NA,length(EstimatorsandTrueDensity)),
                                    NA.num=rep(NA,length(EstimatorsandTrueDensity)))
    
    for(ii in 1:length(EstimatorsandTrueDensity)){
      Estimator.Results[ii,"Mean"] <- mean(na.omit(Master.Data[,EstimatorsandTrueDensity[ii]]))
      Estimator.Results[ii,"SD"] <- sd(na.omit(Master.Data[,EstimatorsandTrueDensity[ii]]))
      
      ##record number of years that estimate did not calculate
      ##(i.e., multiple obs couldn't calc because 1 obs saw 0 birds)
      Estimator.Results[ii,"NA.num"] <- sum(is.na(Master.Data[,EstimatorsandTrueDensity[ii]]))
    }
    
    Estimator.Results.Outliers.Not.Removed <- Estimator.Results
    
    ##Store Estimates before removing outliers
    ##Then modify Master.Data to replace Outliers with NA
    Master.Data.Outliers.Not.Removed <- Master.Data
    
    ###############################################
    ##Identify Outliers
    
    for(ii in 1:length(EstimatorsOnly)){
      Outlier.ii <- 1
      while(Outlier.ii>0){
        ##Extract the max estimate, calc mean & SD without it.
        ##If max is greater than mean+3*SD, it's an outlier.
        ##Replace outliers with NA, continue until max value isn't outlier.
        Estimates.ii <- Master.Data[,c(EstimatorsOnly[ii])]
        Max.Estimate.ii <- max(na.omit(Estimates.ii)) 
        Other.Estimates.ii <- Estimates.ii[Estimates.ii<Max.Estimate.ii]
        Outlier.Limit.ii <- mean(na.omit(Other.Estimates.ii))+3*sd(na.omit(Other.Estimates.ii))
        ifelse(Outlier.Limit.ii>Max.Estimate.ii, Outlier.ii <- 0,
               Master.Data[,c(EstimatorsOnly[ii])][which.max(Master.Data[,c(EstimatorsOnly[ii])])] <- NA)
      }
      ##record how many outliers removed
      Estimator.Results[Estimator.Results$Estimator==EstimatorsOnly[ii],"Outliers.Removed"] <- sum(is.na(Master.Data[,c(EstimatorsOnly[ii])]))
      
      ##Recalc mean and SD estimates with outliers removed
      Estimator.Results[Estimator.Results$Estimator==EstimatorsOnly[ii],"Mean"] <- mean(na.omit(Master.Data[,EstimatorsOnly[ii]]))
      Estimator.Results[Estimator.Results$Estimator==EstimatorsOnly[ii],"SD"] <- sd(na.omit(Master.Data[,EstimatorsOnly[ii]]))
    }
    
    ##Check that outliers are removed from estimates
    # mean(na.omit((Master.Data$ModelAvgEst.Density.y)))
    # mean(Master.Data.Outliers.Not.Removed$ModelAvgEst.Density.y)
    # Estimator.Results[Estimator.Results$Estimator=="ModelAvgEst.Density.y","Mean"]
    
    ###############################################
    ##Correlation 
    ## Calculate Pearson correlation coefficients and p-value (is coef different than zero?)
    
    for(jj in 1:2){  ##1:Master.Data w/o outliers, 2: outliers not removed
      
      ##corr.results below: before removal of outliers was included in calcs
      # corr.results <- data.frame(Estimator=EstimatorsOnly,
      #                          rho.CloseTerrBirds.Density.y=rep(NA,length(EstimatorsOnly)),
      #                          p.value.CloseTerrBirds.Density.y=rep(NA,length(EstimatorsOnly)),
      #                          rho.CloseBirds.Density.y=rep(NA,length(EstimatorsOnly)),
      #                          p.value.CloseBirds.Density.y=rep(NA,length(EstimatorsOnly)),
      #                          rho.CloseSingers.Density.y=rep(NA,length(EstimatorsOnly)),
      #                          p.value.CloseSingers.Density.y=rep(NA,length(EstimatorsOnly)))
      
      corr.results <- data.frame(Estimator=EstimatorsOnly,
                                 rho.CloseTerrBirds.Density.y=rep(NA,length(EstimatorsOnly)),
                                 p.value.CloseTerrBirds.Density.y=rep(NA,length(EstimatorsOnly)),
                                 rho.CloseBirds.Density.y=rep(NA,length(EstimatorsOnly)),
                                 p.value.CloseBirds.Density.y=rep(NA,length(EstimatorsOnly)),
                                 rho.CloseSingers.Density.y=rep(NA,length(EstimatorsOnly)),
                                 p.value.CloseSingers.Density.y=rep(NA,length(EstimatorsOnly)),
                                 z.trans.raw=rep(NA,length(EstimatorsOnly)),
                                 z.trans.Est=rep(NA,length(EstimatorsOnly)),
                                 z.trans.dif=rep(NA,length(EstimatorsOnly)),
                                 # p.critical.z.trans.dif=as.numeric(rep(NA,length(EstimatorsOnly))),
                                 # p.value.z.trans.dif=rep(NA,length(EstimatorsOnly)),
                                 # p.sig=as.numeric(rep(NA,length(EstimatorsOnly))),
                                 sigma.dif=rep(NA,length(EstimatorsOnly))
                                 # ,
                                 # z.CI=rep(NA,length(EstimatorsOnly)),
                                 # z.dif.CI.upper=rep(NA,length(EstimatorsOnly)),
                                 # z.dif.CI.lower=rep(NA,length(EstimatorsOnly)),
                                 # rho.dif.CI.upper=rep(NA,length(EstimatorsOnly)),
                                 # rho.dif.CI.lower=rep(NA,length(EstimatorsOnly))
                                 # p.value.Corr.vs.raw=rep(NA,length(EstimatorsOnly)),
                                 # p.crit.Corr.vs.raw=rep(NA,length(EstimatorsOnly)),
                                 # Is.corr.signif.not.raw=rep(NA,length(EstimatorsOnly))
      )
      
      ##For each estimator, calculate correlation coefficients and p-value
      for(ii in 1:length(EstimatorsOnly)){
        
        if(jj==1){  ##for Master.Data (outliers removed)
          ##Correlation of estimator with Density of birds w/ territories in survey area (Ns/Area)
          corr.data <- na.omit(Master.Data[,c(EstimatorsOnly[ii], "CloseTerrBirds.Density.y")])
          corr.results[ii,"rho.CloseTerrBirds.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseTerrBirds.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
          
          ##Correlation of estimator with Density of Present birds (Np/Area)
          corr.data <- na.omit(Master.Data[,c(EstimatorsOnly[ii], "CloseBirds.Density.y")])
          corr.results[ii,"rho.CloseBirds.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseBirds.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
          
          ##Correlation of estimator with Density of Present&Available birds (Na/Area)
          corr.data <- na.omit(Master.Data[,c(EstimatorsOnly[ii], "CloseSingers.Density.y")])
          corr.results[ii,"rho.CloseSingers.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseSingers.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
        }
        
        if(jj==2){##for Master.Data.Outliers.Not.Removed
          ##Correlation of estimator with Density of birds w/ territories in survey area (Ns/Area)
          corr.data <- na.omit(Master.Data.Outliers.Not.Removed[,c(EstimatorsOnly[ii], "CloseTerrBirds.Density.y")])
          corr.results[ii,"rho.CloseTerrBirds.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseTerrBirds.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
          
          ##Correlation of estimator with Density of Present birds (Np/Area)
          corr.data <- na.omit(Master.Data.Outliers.Not.Removed[,c(EstimatorsOnly[ii], "CloseBirds.Density.y")])
          corr.results[ii,"rho.CloseBirds.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseBirds.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
          
          ##Correlation of estimator with Density of Present&Available birds (Na/Area)
          corr.data <- na.omit(Master.Data.Outliers.Not.Removed[,c(EstimatorsOnly[ii], "CloseSingers.Density.y")])
          corr.results[ii,"rho.CloseSingers.Density.y"] <- cor(corr.data[,1],corr.data[,2])
          corr.results[ii,"p.value.CloseSingers.Density.y"] <- cor.test(corr.data[,1],corr.data[,2])["p.value"]
        }
        
        ##Test if correlation of estimator is different than raw count estimator
        ## for Dp only
        if(ii==1){raw.corr <- corr.results[ii,"rho.CloseBirds.Density.y"]}
        
        if(ii!=1){
          ## Transform raw count corr and estimator rho to z scores
          ## (Fisher Z transformation http://davidmlane.com/hyperstat/A50760.html)
          corr.results[ii, "z.trans.raw"] <- 0.5*log((1+raw.corr)/(1-raw.corr))
          corr.results[ii, "z.trans.Est"] <- 0.5*log((1+corr.results[ii,"rho.CloseBirds.Density.y"])/(1-corr.results[ii,"rho.CloseBirds.Density.y"]))
          
          ##Take difference between transformed correlations for raw count and current estimator
          corr.results[ii, "z.trans.dif"] <- corr.results[ii, "z.trans.raw"]-corr.results[ii, "z.trans.Est"]
          #       corr.results[ii, "p.value.z.trans.dif"] <- pnorm(-1*abs(corr.results[ii, "z.trans.dif"]))
          ## Note:p-value not useful here - calc CI instead
          
          corr.results[ii,"sigma.dif"] <- sqrt(1/(SimReps-3)+1/(SimReps-3))
          #       corr.stat <- pnorm(z.trans.dif)
          #       corr.results[ii, "p.crit.Corr.vs.raw"] <- corr.alpha
          #       corr.results[ii,"p.value.Corr.vs.raw"] <- corr.stat
          #       corr.results[ii,"Is.corr.signif.not.raw"] <- as.numeric(corr.stat<corr.alpha)
        }
      }  ##end EstimatorsOnly (ii)
      
      corr.results[,2:ncol(corr.results)] <- round(corr.results[,2:ncol(corr.results)],4)
      
      if(jj==1){corr.results.All[[Counter]] <- corr.results
      EstimatorsOnly.All[[Counter]] <- EstimatorsOnly}
      
      if(jj==2){corr.results.Outliers.Not.Removed.All[[Counter]] <- corr.results
      EstimatorsOnly.Outliers.Not.Removed.All[[Counter]] <- EstimatorsOnly}
      
    }## end jj
    
    ##Below - store results (before removal of outliers included in calcs)
    # corr.results.All[[Counter]] <- corr.results
    # Estimator.Results.All[[Counter]] <- Estimator.Results
    # Master.Data.All[[Counter]] <- Master.Data
    
    Estimator.Results.Outliers.Not.Removed.All[[Counter]] <- Estimator.Results.Outliers.Not.Removed
    Estimator.Results.All[[Counter]] <- Estimator.Results
    
    Master.Data.Outliers.Not.Removed.All[[Counter]] <- Master.Data.Outliers.Not.Removed
    Master.Data.All[[Counter]] <- Master.Data
    
  }  ##end MaxSurveyDistance (Counter)
  proc.time() - TIMER
  
  
  ##Save results for later (useful for distance, which has comp time >1hr)
  setwd(SaveDirectory)
  
  All.Distances <- paste(MaxSurveyDistances[1],MaxSurveyDistances[2],MaxSurveyDistances[3],sep="_")
  
  # ifelse(sum(is.na(MaxSurveyDistances))>0,
  #        All.Distances <- paste(MaxSurveyDistances[1],MaxSurveyDistances[2],MaxSurveyDistances[3],MaxSurveyDistances[4],sep="_"),
  #        All.Distances <- paste(MaxSurveyDistances[1],MaxSurveyDistances[2],MaxSurveyDistances[3],sep="_"))
  
  save(NSurveySites,
       SurveyType,
       SimReps,
       corr.results.All,
       corr.results.Outliers.Not.Removed.All,
       Estimator.Results.All,
       Estimator.Results.Outliers.Not.Removed.All,
       Master.Data.All,
       Master.Data.Outliers.Not.Removed.All,
       EstimatorsandTrueDensity,
       EstimatorsOnly.All,
       EstimatorsOnly.Outliers.Not.Removed.All,
       MaxSurveyDistances,
       file=paste(InformalSimName,
                  SurveyType,
                  ifelse(SurveyType=="distance",
                         "10Percent_truncation",
                         c(All.Distances,"m")
                  ),
                  DATE,
                  ".RData", sep="_")
  )
  
}  ## end SurveyType.Intended

####################
## References
####################
## Buckland, S. T., D. R. Anderson, K. P. Burnham, J. L. Laake, D. L. Borchers, and L. Thomas (2001). 
## Introduction to Distance Sampling. 
## Oxford University Press, United Kingdom.

######## END ANALYSIS METHODS #################################################