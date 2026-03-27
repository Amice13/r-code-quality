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

## Simulate detection of birds during point count surveys

##Generate data frames to track rare phenomena:
MisID <- data.frame(y=NA, i=NA, j=NA, r=NA, k=NA, o=NA)
DoubleCounted <- data.frame(y=NA, i=NA, j=NA, r=NA, k=NA, o=NA)

## Generate .yir-level & .yirk-level parameters (Independent of individual birds)
## Necessary here because reps are nested within birds in yijrk loops

## SurveyDates - assume no. of surveys/day = SurveysPerDay
## No weekends, survey order is i=1, then i=2, etc.
## No trend in abundance among iterations.
SurveyDatesAll <- StartingJulianDate+rep(0:100,each = SurveysPerDay)[1:(NSurveySites*NReps)]
SurveyDate.yir.List <- replicate(NYears, matrix(SurveyDatesAll ,NSurveySites, NReps), simplify=F)
##Access via SurveyDate.yir.List[[y]][i,r]

##Start time - how many MINUTES after sunrise does survey yir begin?
StartTimeinMinutes.yir.List <- replicate(NYears, matrix(NA ,NSurveySites, NReps), simplify=F)
for(yy in 1:NYears){
  PlannedStartTimesAll <- rep(0:(SurveysPerDay-1)*LogisticalSurveyTime,100)[1:(NSurveySites*NReps)]
  ActualStartTimesAll <- round(rnorm(n=length(PlannedStartTimesAll), mean=PlannedStartTimesAll,sd=5),2)
  StartTimeinMinutes.yir.List[[yy]] <- matrix(ActualStartTimesAll, NSurveySites, NReps)  ##units = MINUTES
}
##Access via StartTimeinMinutes.yir.List[[y]][i,r]


# ##WindSpeed: r-specific mean (.yir)
# ##Must take into account surveys on the SAME DAY
# ## rep-specific mean generated from day-specific mean
# MeanWindSpeed.yir.List <- replicate(NYears,
#                                   matrix(rep(NA,NReps*NSurveySites),
#                                          #(MeanWindSpeedDistrib(NReps*NSurveySites, MeanWindSpeedMin, MeanWindSpeedMax),
#                                          NSurveySites, NReps), simplify=F)
# for(yy in 1:NYears){
#   UniqueDates <- unique(unlist(SurveyDate.yir.List[[yy]]))
#   for(UD in UniqueDates){
#     DailyMeanWindSpeed.yr <- DailyMeanWindSpeedDistrib(1, DailyMeanWindSpeedMin, DailyMeanWindSpeedMax)
#     for(ii in 1:NSurveySites){
#       for(rr in 1:NReps){
#         if(SurveyDate.yir.List[[yy]][ii,rr]==UD){
#           MeanWindSpeed.yir.List[[yy]][ii,rr] <- RepMeanWindSpeedDistrib(1,DailyMeanWindSpeed.yr, RepMeanWindSpeedSD)
#         }
#       }
#     }    
#   }
# }

##Access via MeanWindSpeed.yir.List[[y]][i,r]


# ##WindSpeed: k-specific windspeed(.yirk) 
# ##Windspeeds follow Weibull distribution, parameters from Justus 1977
# ##Because scale parameter c includes term (1/k), Weibull becomes unstable for small values of k
# ##Therefore, for low wind speeds, use k=0.5
# WindSpeed.yirk.List <- vector("list", NYears)
# for(yy in 1:NYears){
#   WindSpeed.yirk.List[[yy]] <- vector("list", NSurveySites)
#   for(ii in 1:NSurveySites){
#     WindSpeed.yirk.List[[yy]][[ii]] <- vector("list", NReps)
#     for(rr in 1:NReps){
#       ifelse(WindVariability*sqrt(MeanWindSpeed.yir.List[[yy]][ii,rr])>1,
#              kparameter <- WindVariability*sqrt(MeanWindSpeed.yir.List[[yy]][ii,rr]),
#              kparameter <- 1)
#       cparameter <- MeanWindSpeed.yir.List[[yy]][ii,rr]/(gamma(1+1/kparameter))
#       WindSpeed.yirk.List[[yy]][[ii]][[rr]] <- rweibull(NIntervals, shape=kparameter, scale=cparameter)
#     }
#   }
# }
##Access via WindSpeed.yirk.List[[y]][[i]][[r]][k]

##Ambient Noise: r-specific 1/0 value (.yir)
MeanNoiseLevel.yir.List <- replicate(NYears, 
                                   matrix(NoiseLevelDistrib(NReps*NSurveySites),#, NoiseLevelMin, NoiseLevelMax
                                          NSurveySites, NReps), simplify=F)

##NOTE: if you want sites to have certain Noise Levels, need to generate this at the .yi level instead of the .yir (currently .yir)
##Access via MeanNoiseLevel.yir.List[[y]][i,r]

##Ambient Noise: k-specific noise (.yirk) (currently constant across rep)
Noise.yirk.List <- vector("list", NYears)
for(yy in 1:NYears){
  Noise.yirk.List[[yy]] <- vector("list", NSurveySites)
  for(ii in 1:NSurveySites){
    Noise.yirk.List[[yy]][[ii]] <- vector("list", NReps)
    for(rr in 1:NReps){
      Noise.yirk.List[[yy]][[ii]][[rr]] <- rep(MeanNoiseLevel.yir.List[[yy]][ii,rr],NIntervals)
      ##Noise Level is r-specific - to make it k-specific, add distribution above
    }
  }
}
##Access via Noise.yirk.List[[y]][[i]][[r]][k]

## crreate list of primary observers (1 or 2) for each replicate
ifelse(SurveyType=="multiple",  
       PrimaryObsNumber.yir.List <- replicate(NYears, matrix(sample(1:NSimultaneousObservers,NReps*NSurveySites, replace=T),NSurveySites, NReps), simplify=F),
       PrimaryObsNumber.yir.List <- replicate(NYears, matrix(rep(NA,NReps*NSurveySites),NSurveySites, NReps), simplify=F))
##Access via PrimaryObsNumber.yir.List[[y]][i,r]


if(SurveyType!="multiple"){  
  ##For all surveys except multiple observer, Observer identity is a replication-level variable
  ObserverIDNumber.yir.List <- replicate(NYears, matrix(sample(1:NTotalObservers,NReps*NSurveySites, replace=T),NSurveySites, NReps), simplify=F)
  ##Access via ObserverIDNumber.yir.List[[y]][i,r]
  ## for name, use ObserverIDs[ObserverIDNumber.yir.List[[y]][i,r]]
}

Leaves.yir.List <- list()
for(LeavesYears in 1:NYears){ #length(SurveyDate.yir.List)){
  Leaves.yir.List[[LeavesYears]] <- matrix(LeavesByDate[SurveyDate.yir.List[[LeavesYears]]],
                                           NSurveySites, NReps) 
}
##Access via Leaves.yir.List[[y]][i,r]

#######################################
##Generate starting values & level-specific lists for storage 
ListYOutcomes.y <- vector("list", NYears)
#ListYSummaryOutcomes.y <- vector("list", NYears)
ListIOutcomes.yi <- vector("list", NYears)
ListIRSummaryOutcomes.yi <- vector("list", NYears)
ListROutcomes.yijr <- vector("list", NYears)
# ListKOutcomes.yijrk <- vector("list", NYears)
# ListOOutcomes.yijrko <- vector("list", NYears)


for(y in 1:NYears){
  
  ##Generate year-specific parameters
  ##Areal Parameters
  StudyHabitatProportion.y <- StudyHabitatProportion(1,StudyHabitatProportionmin, StudyHabitatProportionmax)
  RangeHabitatArea.y <- RangeArea*RangeHabitatProportion.vector[y] ## Size (m^2) of habitat in species' range
  RangeMatrixArea.y <- RangeArea-RangeHabitatArea.y  ## Size (m^2) of matrix in species' range
  StudyHabitatArea.y <- StudyArea*StudyHabitatProportion.y ##Size (m^2) of habitat in Study Area
  StudyMatrixArea.y <- StudyArea-StudyHabitatArea.y  ##Size (m^2) of matrix in Study Area
  ##StudyHabitatArea.y+StudyMatrixArea.y==StudyArea
  
  ##Abundance & Density Parameters
  ContinentalPopulation.y <- ContinentalPopulationAll.vector[y] ##Continental population in year y
  RangeHabitatAbundance.y <- round(ContinentalPopulation.y*HabitatPreference.y,0) ## No. birds found in habitat in species' range
  RangeMatrixAbundance.y <- round(ContinentalPopulation.y*(1-HabitatPreference.y),0) ## No. birds found in matrix in species' range
  
  MatrixDensity.y <- RangeMatrixAbundance.y/RangeMatrixArea.y  ##Density of birds in matrix in species' range (birds/m^2)
  
  StudyHabitatAbundance.y <- round(HabitatDensity.y*StudyHabitatArea.y,0)
  StudyMatrixAbundance.y <- round(MatrixDensity.y*StudyMatrixArea.y,0) ##No. of birds in matrix in study area
  Ns.y <- StudyHabitatAbundance.y + StudyMatrixAbundance.y ##Total No. birds in Study Area
  
  HabitatProportionTheta.y <- HabitatProportionThetaAll[y] ##Concentration parameter for beta distrib. of HabitatProportion.y
  
  ##Generate starting values & level-specific lists for storage  
  ListIOutcomes.yi[[y]] <- vector("list", NSurveySites)
  ListIRSummaryOutcomes.yi[[y]] <- vector("list", NSurveySites)
  ListROutcomes.yijr[[y]] <- vector("list", NSurveySites)
  #   ListKOutcomes.yijrk[[y]] <- vector("list", NSurveySites)
  #   ListOOutcomes.yijrko[[y]] <- vector("list", NSurveySites)
  TempAll.i.Outcomes.y <- vector("list", NSurveySites)
  
  for(i in 1:NSurveySites){
    
    ##Time the simulation
    TIMER <- proc.time()
    
    ##Generate survey site-specific parameters
    PercentHabitat.yi <- rbetaAlt(1,StudyHabitatProportion.y, HabitatProportionTheta.y)
    ##Percent habitat determined by beta distribution 
    
    Area.yi <- abs(Xlim[2]-Xlim[1])*abs(Ylim[2]-Ylim[1])  ##Vicinity of survey site where birds are modeled
    LambdaHabitat.yi <- PercentHabitat.yi*Area.yi*HabitatDensity.y
    LambdaMatrix.yi <- (1-PercentHabitat.yi)*Area.yi*MatrixDensity.y
    BirdsInHabitat.yi <- rpois(1,LambdaHabitat.yi)
    BirdsInMatrix.yi <- rpois(1,LambdaMatrix.yi)
    NBirds.yi <- BirdsInMatrix.yi+ BirdsInHabitat.yi
    ##Number of birds in habitat =Poisson(%Habitat*area*Density of birds in habitat)
    ##Number of birds in matrix = Poisson(%Matrix* area*Density of birds in matrix)
    #     IsSiteGrassland.yi <- rbinom(1,1,PrGrassland.y)
    #     if(IsSiteGrassland.yi==1) {IsSiteDeciduous.yi <- 0}
    #     if(IsSiteGrassland.yi==0) {IsSiteDeciduous.yi <- rbinom(1,1,PrDeciduous.y)}
    
    if(NBirds.yi==0){
      
      ListROutcomes.yijr[[y]][[i]] <- vector("list", 1)
      #       ListKOutcomes.yijrk[[y]][[i]] <- vector("list", 1)
      #       ListOOutcomes.yijrko[[y]][[i]] <- vector("list", 1)
      
      ListROutcomes.yijr[[y]][[i]][[1]] <- vector("list", NReps)
      #       ListKOutcomes.yijrk[[y]][[i]][[1]] <- vector("list", NReps)
      #       ListOOutcomes.yijrko[[y]][[i]][[1]] <- vector("list", NReps)
      
      ##For NBirds.yi==0
      for(r in 1:NReps){
        StartTimeinIntervals.yir <- round(StartTimeinMinutes.yir.List[[y]][i,r]*IntervalsPerMinute,0)  ##Minutes*(intervals/Minute)
        Leaves.yir <- Leaves.yir.List[[y]][i,r]
        
        ##Generate starting values & level-specific lists for storage
        #         ListKOutcomes.yijrk[[y]][[i]][[1]][[r]] <- vector("list", NIntervals)
        #         ListOOutcomes.yijrko[[y]][[i]][[1]][[r]] <- vector("list", NIntervals)
        
        ##For NBirds.yi==0
        for(k in 1:NIntervals){
          TimeinIntervals.yirk <- StartTimeinIntervals.yir+ (k-1) ##Intervals since sunrise (in intervals, 0=sunrise)
          NoiseLevel.yirk <- Noise.yirk.List[[y]][[i]][[r]][k]
          ##Ambient noise is rep-specific only
          #           WindSpeed.yijrk <- as.numeric(WindSpeed.yirk.List[[y]][[i]][[r]][k])
          ##NOTE: WindIndex.yijrk does not apply: would require bird location, but NBirds.yi==0)
          
          ##Generate starting values & level-specific lists for storage
          #           ListOOutcomes.yijrko[[y]][[i]][[1]][[r]][[k]] <- vector("list", NSimultaneousObservers)
          
          ##For NBirds.yi==0
          for(o in 1:NSimultaneousObservers){
            
            if(SurveyType!="multiple"){  
              ObserverIDNumber.yijrko <- ObserverIDNumber.yir.List[[y]][i,r]}
            
            if(SurveyType=="multiple"){  
              ObserverIDNumber.yijrko <- o}
            
            Count.yijrko <- 0  ## No false positives if NBirds.yi==0
            
            ## Outcomes Complete    #####################################
            
            
            ## Index all o-specific objects for NBirds.yi==0
            TempOutcomes.o <- list(
              "Count.yijrko"=Count.yijrko,                
              "Pdintercept.yijrko"=NA,
              "Pdslope.yijrko"=NA,
              "Perceptibility.yijrko"=NA,
              "ObserverIDNumber.yijrko"=ifelse(SurveyType=="multiple",ObserverIDNumber.yijrko,NA),
              "Detected.yijrko "=NA,
              "CorrectID.yijrko"=NA,
              "PrCorrectID.yijrko"=NA,
              "PrDoubleCount.yijrko"=NA,
              "DoubleCounted.yijrko"=NA,
              "ObsEstimatedDistance.yijrko"=NA
            )
            
            ##Store o-specific objects and summaries for NBirds.yi==0
            #             ListOOutcomes.yijrko[[y]][[i]][[1]][[r]][[k]][[o]] <- list(
            #               "DoubleCounted.yijrko"=NA,
            #               "Detected.yijrko"=NA,  
            #               "CorrectID.yijrko"=NA,
            #               "DoubleCounted.yijrko"=NA,
            #               "Count.yijrk"=0 ##Currently, no false positives if NBirds.yi==0
            #             )
            
          }  ## o for NBirds.yi==0
          
          ## Index all k-specific objects for NBirds.yi==0
          TempOutcomes.k <- list(
            "TimeinIntervals.yirk"=TimeinIntervals.yirk,
            #             "WindSpeed.yijrk"=WindSpeed.yijrk,
            #             "WindIndex.yijrk"=NA,
            "NoiseLevel.yirk"=NoiseLevel.yirk,
            "SingingMode.yijrk"=NA,
            "SingingState.yijrk"=NA,
            "PrSing.yijrk"=NA,
            "Sings.yijrk"=NA,
            "PSNS.yijrk"=NA,
            "q1.yijrk"=NA,
            "PrBirdMoves.yijrk"=NA,
            "DoesBirdMove.yijrk"=NA,
            "Distance.yijrk"=NA,
            "Location.x.yijrk"=NA,
            "Location.y.yijrk"=NA,
            "SongWeight.yijrk"=NA
          )
          
          #           ##Store-k-level objects and summaries for NBirds.yi==0
          #           ListKOutcomes.yijrk[[y]][[i]][[1]][[r]][[k]] <- list(
          #             "TimeinIntervals.yirk"=TimeinIntervals.yirk,
          #             "WindSpeed.yijrk"=WindSpeed.yijrk,
          #             "NoiseLevel.yirk"=NoiseLevel.yirk
          #           )
          
        }  ##k for NBirds.yi==0
        
        ##Make List of rep-specific outcomes (summary across intervals)
        ## Index all r-specific objects for NBirds.yi==0
        TempOutcomes.r <- list(
          "Leaves.yir"=Leaves.yir,
          #"ObserverIDNumber.yir"=ObserverIDNumber.yijrko, ##ObserverIDNumber.yir.List[[y]][i,r],
          #"MeanObservedWindspeed.yijr"=MeanObservedWindspeed.yijr,
          "MeanObservedNoise.yijr"= MeanObservedNoiseLevel.yir,
          "Count.yijr"=0, ## No false positives if NBirds.yi==0 ##max(unlist(TempOutcomes.k)["Count.yijrk"]),
          "MeanObservedDistance.yijrk"=NA
        ) 
        ##Store-r-level objects and summaries for NBirds.yi==0
        ListROutcomes.yijr[[y]][[i]][[1]][[r]] <- TempOutcomes.r 
        
      }  ##r for NBirds.yi==0
      
    }  ## end bracket to "if(NBirds.yi==0){"
    
    if(NBirds.yi>0){
      
      ##Generate Mean & SD area for bird territories at site i 
      ##If density is sufficiently high, compress area so that only 50% of total area is filled by OverlapUDTerr % UD ellipses
      ##For compressed territories, SD is multiplied by ratio (compressed area/uncompressed territory size)
      AllowedTerrSizeOE.yi <- (0.5*Area.yi)/NBirds.yi
      
      ##What is the ratio in size between the %UD ellipse used to calc overlap and the final UD for territories?
      ##See Equations 12 and 13 from Jennrich and Turner 1969
      ##For example: Calculate allowed size for 95% ellipses from 80% UD
      ##For all bivariate normal territories:
      ## Area of 80% UD ellipse = 0.53724 * Area of 95% UD ellipse
      ## Area of 95% UD ellipse = 1.86135 * Area of an 80% UD ellipse 
      OverlapCoef <- log((1-OverlapUDTerr)^-2)
      FinalCoef <- log((1-UDTerr)^-2) 
      RatioTerrUDOverlapUD <- FinalCoef/OverlapCoef
      AllowedTerrSize.yi <- AllowedTerrSizeOE.yi*RatioTerrUDOverlapUD
      MeanTerrArea.yi <- min(c(AllowedTerrSize.yi, MeanTerrArea)) ##input smaller of 2 values
      SDTerrArea.yi <- min(c((AllowedTerrSize.yi/MeanTerrArea)*SDTerrArea, SDTerrArea))
      
      ##generate all spatial.yij for site yi (i level b/c they must not overlap)
      SpatialList.yi <- list()
      SpatialCounter <- 1
      
      while(length(SpatialList.yi)<NBirds.yi){
        ##Make a candidate set of spatial parameters
        SpatialCandidate.yij <- GenSpatialParameters(HRAreamean=MeanTerrArea.yi,
                                                   HRAreaSD=SDTerrArea.yi,
                                                   PercentUD.yij=UDTerr,
                                                   ylim=Ylim, 
                                                   xlim=Xlim,
                                                   OverlapUD=OverlapUDTerr)
        
        ##For birds other than the first:
        if(length(SpatialList.yi)>0){
          CandidateDistances <- DistanceCenters(SpatialCandidate.yij,SpatialList.yi)
          ClosestNeighborDistance <- min(na.omit(CandidateDistances))
          ##Max and min Diameter based on desired % UD ellipse - more closely packed territories
          ## % UD Ellipse determined by parameter OverlapUDTerr
          MaximumDiameter <- SpatialCandidate.yij[["aOE.yij"]] + 
            max(unname(as.numeric(unlist(SpatialList.yi)[names(unlist(SpatialList.yi))=="aOE.yij"])))
          MinimumDiameter <- SpatialCandidate.yij[["bOE.yij"]] + 
            min(unname(as.numeric(unlist(SpatialList.yi)[names(unlist(SpatialList.yi))=="bOE.yij"])))
          
          ## If nearest neighbor is FARTHER than candidate major radius + 
          ## largest existing major radius, save candidate
          if(ClosestNeighborDistance > MaximumDiameter){ 
            SpatialList.yi[[length(SpatialList.yi)+1]] <- SpatialCandidate.yij
          }
          
          ## If nearest neighbor is CLOSER than candidate major radius + 
          ## largest existing major radius, but FARTHER than candidate 
          ## minor radius + smallest existing minor radius, then go 
          ## through comparisons to see if 
          ## candidate overlaps any existing territories or vice versa.
          if(ClosestNeighborDistance < MaximumDiameter &
               ClosestNeighborDistance > MinimumDiameter){
            
            #See if candidate overlaps any bird territories already generated  
            Overlap <- AxesCheckInsideEllipsesVectorizedOE(SpatialCandidate.yij,SpatialList.yi)
            if(Overlap==FALSE){
              
              ##See if any existing territory overlaps candidate territory
              SumUp <- 0
              for(jjj in 1:length(SpatialList.yi)){
                SumUp <- SumUp + AxesCheckInsideEllipseOE(SpatialList.yi[[jjj]], SpatialCandidate.yij)
              }
              ##if there are no overlaps, save that territory 
              if(SumUp==0) {
                SpatialList.yi[[length(SpatialList.yi)+1]] <- SpatialCandidate.yij
              }
            }
          }
        }
        
        ##Generate territory for bird #1
        if(length(SpatialList.yi)==0){
          SpatialList.yi[[1]] <- SpatialCandidate.yij }
        SpatialCounter <- SpatialCounter+1
        if(SpatialCounter>5000) stop("5000 attempts to generate territories exceeded - check density & NBirds.yi")
      }  ## bracket refers to: while(length(SpatialList.yi)<NBirds.yi){
      
      ##Plot ellipses for site i territories
      #       par(mfrow=c(1,1))
      #       plot(NULL,NULL,xlim=Xlim, ylim=Ylim)
      #       
      #       for(iii in 1:length(SpatialList.yi)){
      #         lines(GenEllipsePlot(SpatialList.yi[[iii]]))
      #       }
      # length(SpatialList.yi)
      ##Generate starting values & level-specific lists for storage
      ListROutcomes.yijr[[y]][[i]] <- vector("list", NBirds.yi)
      #       ListKOutcomes.yijrk[[y]][[i]] <- vector("list", NBirds.yi)
      #       ListOOutcomes.yijrko[[y]][[i]] <- vector("list", NBirds.yi)
      TempAll.j.Outcomes.i <- vector("list", NBirds.yi)
      
      #TempAll.roj.Outcomes.i <- vector("list", NReps)
      TempAll.rj.Outcomes.i <- vector("list", NReps)
      for(rr in 1:NReps){
        TempAll.rj.Outcomes.i[[rr]] <- vector("list", NBirds.yi)
      }
      
      for(j in 1:NBirds.yi){
        
        ##Generate Bird-specific parameters & distributions
        Spatial.yij <- SpatialList.yi[[j]]
        #           GenSpatialParameters(MeanTerrArea,SDTerrArea,
        #                                           ylim=Ylim, xlim=Xlim)
        
        ##Generate starting values & level-specific lists for storage        
        ListROutcomes.yijr[[y]][[i]][[j]] <- vector("list", NReps)
        #         ListKOutcomes.yijrk[[y]][[i]][[j]] <- vector("list", NReps)
        #         ListOOutcomes.yijrko[[y]][[i]][[j]] <- vector("list", NReps)
        TempAll.r.Outcomes.j <- vector("list", NReps)
        
        for(r in 1:NReps){
          
          ##Generate replication-specific parameters 
          StartTimeinIntervals.yir <- round(StartTimeinMinutes.yir.List[[y]][i,r]*IntervalsPerMinute,0)  ##Minutes*(intervals/Minute)
          Leaves.yir <- Leaves.yir.List[[y]][i,r]
          
          ##Generate starting values & level-specific lists for storage
          #           ListKOutcomes.yijrk[[y]][[i]][[j]][[r]] <- vector("list", NIntervals)
          #           ListOOutcomes.yijrko[[y]][[i]][[j]][[r]] <- vector("list", NIntervals)
          TempAll.k.Outcomes.r <- vector("list", NIntervals)
          TempAll.ko.Outcomes.r <- vector("list", NSimultaneousObservers)
          #TempAll.roj.Outcomes.i[[r]] <- vector("list", NSimultaneousObservers) 
          
          
          for(oo in 1:NSimultaneousObservers){
            TempAll.ko.Outcomes.r[[oo]] <- vector("list", NIntervals)
            #TempAll.roj.Outcomes.i[[r]][[oo]] <- vector("list", NBirds.yi)  
          }
          
          
          ##Small k-level lists - overwritten for each rep 
          #  TempOutcomes.k <- list()
          
          #           WasBirdCounted.yijr <- 0
          #           WasBirdCountedObs.yijr <- c(0,0)
          #           DistanceFirstCount.yijr <- rep(NA,NSimultaneousObservers)
          #           MinDistance.yijr <- NA
          #           
          #           
          
          for(k in 1:NIntervals){
            
            ##Generate interval-specific ENVIRONMENT
            
            TimeinIntervals.yirk <- StartTimeinIntervals.yir+ (k-1) ##time of survey interval (in intervals)
            NoiseLevel.yirk <- Noise.yirk.List[[y]][[i]][[r]][k]
            ##Ambient noise is rep-specific only
            
            ##Generate interval-specific bird BEHAVIOR
            
            ##Spatial: Does it Move?
            
            PrBirdMoves.yijrk <- PrBirdMoves(N=1,MeanMovementRate=BackgroundMovementRate)
         
            ##Generate initial spatial information for k=1
            if(k==1){require(arm)
                     Location.yijr0 <- GenLocations(1,Spatial.yij)  ##Initial Location
                     Distance.yijr0 <- ObsDistance(Location.yijr0[1],Location.yijr0[2])[["Distance.yijrk"]]
                     
                     ##Assumes no movement (flush) of birds due to obs arriving
                     ##Thus, location at t=1 = location at t=0
                     Location.yijrk <- Location.yijr0 
                     
                     ##Below: Allow birds to flush at t=0 due to observer arrival
                     #                      PrBirdMovesObs.yijr1 <- PrBirdMoveObs(Distance=ObsDistance(Location.yijr0[1],Location.yijr0[2])[["Distance.yijrk"]])
                     #                      #                      PrBirdMovesObs.yijr1 <- invlogit(1-0.1*ObsDistance(Location.yijr0[1],Location.yijr0[2])[["Distance.yijrk"]])
                     #                      ##Distance-dependent Pr(bird moves due to obs) - drops to ~0.2 at 20 m
                     #                      DoesBirdMoveObs.yijr1 <- rbinom(1,1,PrBirdMovesObs.yijr1)
                     #                      
                     #                      if(DoesBirdMoveObs.yijr1==0) Location.yijrk <- Location.yijr0 ##If no movement, location at t=1 = location at t=0
                     #                      if(DoesBirdMoveObs.yijr1!=0){                      ##If bird moves,
                     #                        Distance.yijr0 <- ObsDistance(Location.yijr0[1],Location.yijr0[2])[["Distance.yijrk"]]  ## calc distance at t=0
                     #                        CandidateDistance <- 0
                     #                        CandidateDistanceCounter <- 0
                     #                        while(CandidateDistance < Distance.yijr0)     
                     #                        {CandidateLocation <- GenLocations(1,Spatial.yij)  ##generate new locations until one is farther than location at t=0
                     #                         CandidateDistance <- ObsDistance(CandidateLocation[1],CandidateLocation[2])[["Distance.yijrk"]]
                     #                         CandidateDistanceCounter <- CandidateDistanceCounter+1
                     #                         if(CandidateDistanceCounter>1000) break}
                     #                        if(CandidateDistanceCounter>1000){Location.yijrk <- Location.yijr0}
                     #                        if(CandidateDistanceCounter<=1000){Location.yijrk <- CandidateLocation}
                     #                      }
            }
            ##Generate spatial information for k>1
            if(k!=1){ 
              DoesBirdMove.yijrk <- rbinom(1,1,PrBirdMoves.yijrk)
              if(DoesBirdMove.yijrk==1) {Location.yijrk <- GenLocations(1,Spatial.yij)} 
            }
            Distance.yijrk <- ObsDistance(Location.yijrk[1],Location.yijrk[2])[["Distance.yijrk"]]
            
            ##Track smallest distance between observer & bird j
            #             if(k==1){MinDistance.yijr <- Distance.yijrk}
            #             if(k!=1){MinDistance.yijr <- min(Distance.yijrk,MinDistance.yijr)}            
            #             
            
            #             WindSpeed.yijrk <- as.numeric(WindSpeed.yirk.List[[y]][[i]][[r]][k])
            #             WindIndex.yijrk <- WindSpeed.yijrk*cos(ObsDistance(Location.yijrk[1],Location.yijrk[2])[["BirdAngleCompass.yijrk"]])
            ##WindIndex assumes that wind is from due North at all sites (ok b/c bird territories are spatially random wrt observer)
            
            
            ##Availability: Does It Sing?
            
            ##Maximum daily song rate for this species on day = SurveyDate.yir.List[[y]][i,r]
            MaxDailySongRate.yir <- MaxDailySongRate(SurveyDate.yir.List[[y]][i,r])
            
            ##What is the effect (weight) of time of day on the max song rate?
            SongWeight.yijrk <- TimeOfDayWeight(TimeinIntervals.yirk, IntervalsPerHour)
            
            ##What is interval-specific Pr(Bird Sings within ReportedPrSingMin minutes?)
            PrSing.yijrk <- MaxDailySongRate.yir*SongWeight.yijrk
            
            ##Here, PrSing.yijrk refers to Pr(bird sings at least 1x in ReportedPrSingMin min (usually 5 or 10))
            ##Therefore, use DataIntervals = ReportedPrSingMin (min) *60 (sec/min)*(1/IntervalLength) (Intervals/sec)
            ##units for DataIntervals: (min) * (60sec/min) * (Intervals/sec) = Intervals
            ##PrSing calc is AFTER spatial to allow Pa ~ Distance to obs
            
            OptimizePSNS.yijrk <- optimize(SolvePSNS,                
                                          interval=c(0,0.05),   ##NOTE: **Starting values** are VERY important here, suggest using interval=c(0,0.05) for all
                                          PrSing=PrSing.yijrk,
                                          PSS=PSS.yijr,
                                          DataIntervals=ReportedPrSingMin*60*(1/IntervalLength),
                                          maximum=F)
            if(OptimizePSNS.yijrk$objective>0.05) {stop("Failure to optimize PSNS")}
            PSNS.yijrk <- OptimizePSNS.yijrk$minimum
            q1.yijrk <- MarkovSS(PSS.yijr,PSNS.yijrk)[1]
            
            ##Determine Singing Mode (1/0) for interval k
            if(k==1){SingingMode.yijrk <- rbinom(1,1,q1.yijrk)  }
            if(k>1){
              PreviousSingingMode <- TempAll.k.Outcomes.r[[k-1]]["SingingMode.yijrk"]
              SingingMode.yijrk <- AutoCInstant(PreviousSingingMode,PSS.yijr,PSNS.yijrk)
            }
            
            ## Determine Singing (1/0) for interval k, given Singing Mode
            if(SingingMode.yijrk==0) {Sings.yijrk <- 0;  SingingState.yijrk <- NA}
            if(SingingMode.yijrk==1 & k==1) {
              SingingState.yijrk <- sample(SingingStates.fine,1)
              ifelse(SingingState.yijrk=="S",Sings.yijrk <- 1,Sings.yijrk <- 0)
            }
            if(SingingMode.yijrk==1 & k>1){
              if(PreviousSingingMode==0) SingingState.yijrk <- sample(SingingStates.fine,1)
              
              if(PreviousSingingMode==1) {
                PreviousSingingState <- TempAll.k.Outcomes.r[[k-1]]["SingingState.yijrk"]
                
                if(PreviousSingingState=="S"|PreviousSingingState=="NS1"){  
                  PreviousSingingStateMatrix <- matrix(as.numeric(SingingStates.fine==PreviousSingingState),1,4)
                  NewSingingStateMatrix <- PreviousSingingStateMatrix%*%TransitionMatrix.fine
                  SingingState.yijrk <- SingingStates.fine[as.numeric(NewSingingStateMatrix%*%c(1,2,3,4))]
                }
                if(PreviousSingingState=="NS2"){
                  ifelse(rbinom(1,1,TransitionMatrix.fine[3,1])==1,
                         SingingState.yijrk <- "S",
                         SingingState.yijrk <- "NS3")
                }
                if(PreviousSingingState=="NS3"){
                  ifelse(rbinom(1,1,TransitionMatrix.fine[4,1])==1,
                         SingingState.yijrk <- "S",
                         SingingState.yijrk <- "NS3")
                }
                ifelse(SingingState.yijrk=="S",Sings.yijrk <- 1,Sings.yijrk <- 0)
              }
              
            }
            
            
            ##Generate starting values & level-specific lists for storage
            #             ListOOutcomes.yijrko[[y]][[i]][[j]][[r]][[k]] <- vector("list", NSimultaneousObservers)
            TempAll.o.Outcomes.k <- vector("list", NSimultaneousObservers)
            
            for(o in 1:NSimultaneousObservers){  ##For NBirds.yi>0
              
              ##Perceptibility: Is it detected?
              if(SurveyType!="multiple"){  
                ObserverIDNumber.yijrko <- ObserverIDNumber.yir.List[[y]][i,r]}
              
              if(SurveyType=="multiple"){  
                ObserverIDNumber.yijrko <- o}
              
              Pdintercept.yijrko <-  (SpeciesIntercept+
                                      #                                       WindIndexIEffect*WindIndex.yijrk +
                                      ObserverIEffects[,ObserverIDNumber.yijrko] +
                                      #                                       GrasslandIEffect*IsSiteGrassland.yi +
                                      #                                       DeciduousIEffect*IsSiteDeciduous.yi +
                                      LeavesIEffect*Leaves.yir +
                                      NoiseIEffect*NoiseLevel.yirk +
                                      NoiseXLeavesEffect*NoiseLevel.yirk*Leaves.yir)
              
              Pdslope.yijrko <- (DistanceEffect.yijr +
                                  #                                   WindIndexSEffect*WindIndex.yijrk +
                                  ObserverSEffects[,ObserverIDNumber.yijrko] +
                                  #                                   GrasslandSEffect*IsSiteGrassland.yi +
                                  #                                   DeciduousSEffect*IsSiteDeciduous.yi +
                                  LeavesSEffect*Leaves.yir +
                                  NoiseSEffect*NoiseLevel.yirk)
              
              Perceptibility.yijrko <- invlogit(Pdintercept.yijrko+Pdslope.yijrko*Distance.yijrk)
              
              if(Sings.yijrk==0) {Detected.yijrko <- 0}
              if(Sings.yijrk==1) {Detected.yijrko <- rbinom(1,1,Perceptibility.yijrko)}
              
              PrCorrectID.yijrko <- PrCorrectID
              
              if(Detected.yijrko ==0) {CorrectID.yijrko <- NA}
              if(Detected.yijrko ==1) {CorrectID.yijrko <- rbinom(1,1,PrCorrectID.yijrko)}
              
              PrDoubleCount.yijrko <- PrDoubleCount
              
              if(is.na(CorrectID.yijrko)==T) {DoubleCounted.yijrko <- NA}
              if(is.na(CorrectID.yijrko)==F){
                if(CorrectID.yijrko==0) {DoubleCounted.yijrko <- 0}
                if(CorrectID.yijrko==1) {DoubleCounted.yijrko <- rbinom(1,1,PrDoubleCount.yijrko)}
              }
              Count.yijrko <- sum(na.omit(0+CorrectID.yijrko + DoubleCounted.yijrko))
              
              #               if(Count.yijrko>0 & WasBirdCountedObs.yijr[o]==0){
              #                 ##for first count of bird, record distance to obs
              #                 DistanceFirstCount.yijr[o] <- Distance.yijrk}
              
              #               if(Count.yijrko>0) {WasBirdCounted.yijr <- 1
              #                                   WasBirdCountedObs.yijr[o] <- 1}
              
              ## Observer estimation of distance
              ObsEstimatedDistance.yijrko <- ObserverEstDistance(Distance.yijrk, 
                                                               ObserverDistanceCategories, 
                                                               Output="stochastic")
              ##Prevent negative estimated distances
              if(ObsEstimatedDistance.yijrko<0){ObsEstimatedDistance.yijrko <- 1}
              ## Outcomes Complete    #####################################
              
              
              ##Track Rare phenomena
              if(is.na(CorrectID.yijrko)==F){
                if(CorrectID.yijrko==0) {MisID[nrow(MisID)+1,] <- c(y,i,j,r,k,o)}
              }
              if(is.na(DoubleCounted.yijrko)==F){
                if(DoubleCounted.yijrko==1) {DoubleCounted[nrow(DoubleCounted)+1,] <- c(y,i,j,r,k,o)}
              }
              
              ## Index all o-specific objects for NBirds.yi>0
              TempOutcomes.o <- list(
                "Pdintercept.yijrko"=as.numeric(Pdintercept.yijrko),
                "Pdslope.yijrko"=as.numeric(Pdslope.yijrko),
                "Perceptibility.yijrko"=as.numeric(Perceptibility.yijrko),
                "Detected.yijrko"=as.numeric(Detected.yijrko),
                "PrCorrectID.yijrko"=as.numeric(PrCorrectID.yijrko),
                "CorrectID.yijrko"=as.numeric(CorrectID.yijrko),
                "PrDoubleCount.yijrko"=as.numeric(PrDoubleCount.yijrko),
                "DoubleCounted.yijrko"=as.numeric(DoubleCounted.yijrko),
                "Count.yijrko"=as.numeric(Count.yijrko),
                "ObsEstimatedDistance.yijrko"=as.numeric(ObsEstimatedDistance.yijrko)
                #"ObserverIDNumber.yijrko"=ifelse(SurveyType=="multiple",ObserverIDNumber.yijrko,NA)
              )
              
              ##Store o-level objects and summaries
              #               ListOOutcomes.yijrko[[y]][[i]][[j]][[r]][[k]][[o]] <- TempOutcomes.o
              #               
              #               if(SurveyType!="multiple"){
              #                 ListOOutcomes.yijrko[[y]][[i]][[j]][[r]][[k]][[o]] <- TempOutcomes.o
              #               }
              
              
              ##Temp Store all TempOutcomes.o for each k
              TempAll.o.Outcomes.k[[o]] <- TempOutcomes.o
              TempAll.ko.Outcomes.r[[o]][[k]] <- TempOutcomes.o
              
            }  ##o
            
            ## Check that parameters are properly specified
            if(SurveyType %in% SurveyOptions==FALSE) stop("SurveyType is invalid or misspelled")
            
            ## Index all k-specific objects for NBirds.yi>0            
            TempOutcomes.k <- list(
              "TimeinIntervals.yirk"=as.numeric(TimeinIntervals.yirk),
              #               "WindSpeed.yijrk"=as.numeric(WindSpeed.yijrk),
              #               "WindIndex.yijrk"=as.numeric(WindIndex.yijrk),
              "NoiseLevel.yirk"=as.numeric(NoiseLevel.yirk),
              "SingingMode.yijrk"=as.numeric(SingingMode.yijrk),
              "SingingState.yijrk"=SingingState.yijrk,
              "PrSing.yijrk"=as.numeric(PrSing.yijrk),
              "Sings.yijrk"=as.numeric(Sings.yijrk),
              "PSNS.yijrk"=as.numeric(PSNS.yijrk),
              "q1.yijrk"=as.numeric(q1.yijrk),
              "PrBirdMoves.yijrk"=as.numeric(PrBirdMoves.yijrk),
              "DoesBirdMove.yijrk"=ifelse(k>1,as.numeric(DoesBirdMove.yijrk),NA),
              "Distance.yijrk"=as.numeric(Distance.yijrk),
              "Location.x.yijrk"=as.numeric(Location.yijrk[1]),
              "Location.y.yijrk"=as.numeric(Location.yijrk[2]),
              "SongWeight.yijrk"=as.numeric(SongWeight.yijrk),
              "TempAll.o.Outcomes.k"=TempAll.o.Outcomes.k
            )
            
            #             EverDetectedByObs.yijro,
            #             EverCorrectIDByObs.yijro,
            #             EverDoubleCounted.yijro,
            #             Count.yijro
            
            ##Create k-level summaries          
            
            
            Summaries.k <-
              list(
                #TempAll.o.Outcomes.k[[1]][["DoubleCounted.yijrko"]],
                #TempAll.o.Outcomes.k[[1]][["Detected.yijrko "]]
                
              )
            
            #             ##Store-k-level objects and summaries
            #             ListKOutcomes.yijrk[[y]][[i]][[j]][[r]][[k]] <- list(
            #               "XLocation.yijrk"=Location.yijrk[1],
            #               "YLocation.yijrk"=Location.yijrk[2],
            #               "SingingMode.yijrk"=SingingMode.yijrk,
            #               "SingingState.yijrk"=SingingState.yijrk,
            #               "Sings.yijrk"=Sings.yijrk)
            
            ##Temp Store all TempOutcomes.k for each r
            TempAll.k.Outcomes.r[[k]] <- TempOutcomes.k
            #                                      append(TempOutcomes.k,
            #                                      TempAll.o.Outcomes.k)
            
            
          } ##k
          
          ## Index all r-specific objects
          TempOutcomes.r <- list(
            "StartTimeinIntervals.yir"=StartTimeinIntervals.yir,
            "Leaves.yir"=Leaves.yir,
            "PSS.yijr"=PSS.yijr,
            "MaxDailySongRate.yir"=MaxDailySongRate.yir
          )
          
          
          ##Create r-level summaries
          
          ## summaries.yijro
          #"MeanPerceptibility.yijro"=MeanPerceptibility.yijro,
          if(k!=NIntervals) stop("Intervals not complete! r summaries will be invalid!")
          
          #           TempAll.ko.Outcomes.r[[o]][[k]]
          PreSummaries.r <- (vector("list", NSimultaneousObservers))
          
          for(oo in 1:NSimultaneousObservers){
            EverDetectedByObs.yijro <- (sum(as.numeric(unlist(TempAll.ko.Outcomes.r[[oo]])[
              names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="Detected.yijrko"] )))
            EverDetectedByObs.yijro <- ifelse(EverDetectedByObs.yijro>0,1,0)
            
            EverCorrectIDByObs.yijro <- ifelse(EverDetectedByObs.yijro>0,
                                             sum(na.omit(as.numeric(unlist(TempAll.ko.Outcomes.r[[oo]])[
                                               names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="CorrectID.yijrko"] ))),
                                             NA)
            if(!is.na(EverCorrectIDByObs.yijro)){ifelse(EverCorrectIDByObs.yijro>0,1,0)}
            
            EverDoubleCounted.yijro <- ifelse(EverDetectedByObs.yijro>0,
                                            sum(na.omit(as.numeric(unlist(TempAll.ko.Outcomes.r[[oo]])[
                                              names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="DoubleCounted.yijrko"] ))),
                                            NA)
            if(!is.na(EverDoubleCounted.yijro)){EverDoubleCounted.yijro <- ifelse(EverDoubleCounted.yijro>0,1,0)}
            
            Count.yijro <- max(as.numeric(unlist(TempAll.ko.Outcomes.r[[oo]])[
              names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="Count.yijrko"] ))
            
            if(Count.yijro>0){
              FirstIntervalCounted.yijro <- which(unlist(TempAll.ko.Outcomes.r[[oo]])[names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="Count.yijrko"]>0)[1]
              DistAtFirstDetection.yijro <- as.numeric(unlist(unname(TempAll.k.Outcomes.r[[FirstIntervalCounted.yijro]]["Distance.yijrk"])))
              ObsEstDistAtFirstDetection.yijro <- as.numeric(unlist(TempAll.ko.Outcomes.r[[oo]])[names(unlist(TempAll.ko.Outcomes.r[[oo]]))=="ObsEstimatedDistance.yijrko"][FirstIntervalCounted.yijro])
            }
            if(Count.yijro==0){
              FirstIntervalCounted.yijro <- NA
              DistAtFirstDetection.yijro <- NA
              ObsEstDistAtFirstDetection.yijro <- NA
            }
            
            PreSummaries.r[[oo]] <- list(
              "EverDetectedByObs.yijro"=EverDetectedByObs.yijro,
              "EverCorrectIDByObs.yijro"=EverCorrectIDByObs.yijro,
              "EverDoubleCounted.yijro"=EverDoubleCounted.yijro,
              "Count.yijro"=Count.yijro,
              "FirstIntervalCounted.yijro"=FirstIntervalCounted.yijro,
              "DistAtFirstDetection.yijro"=DistAtFirstDetection.yijro,
              "ObsEstDistAtFirstDetection.yijro"=ObsEstDistAtFirstDetection.yijro
            )
            
            #TempAll.roj.Outcomes.i[[r]][[oo]][[j]] <- PreSummaries.r
          }  ##oo 
          
          EverDetected.yijr <- ifelse(sum(as.numeric(unlist(PreSummaries.r)[names(unlist(PreSummaries.r))=="EverDetectedByObs.yijro"]))>0,
                                           1,0)
          EverCorrectID.yijr <- ifelse(sum(as.numeric(unlist(PreSummaries.r)[names(unlist(PreSummaries.r))=="EverCorrectIDByObs.yijro"]))>0,
                                     1,0)
          EverDoubleCounted.yijr <- ifelse(sum(as.numeric(unlist(PreSummaries.r)[names(unlist(PreSummaries.r))=="EverDoubleCounted.yijro"]))>0,
                                         1,0)
          EverCounted.yijr <- ifelse(sum(as.numeric(unlist(PreSummaries.r)[names(unlist(PreSummaries.r))=="Count.yijro"]))>0,
                                   1,0)
          
          FirstIntervalCounted.yijrObs1 <- as.numeric(unname(unlist(PreSummaries.r[[1]]["FirstIntervalCounted.yijro"])))
          DistAtFirstDetection.yijrObs1 <- as.numeric(unname(unlist(PreSummaries.r[[1]]["DistAtFirstDetection.yijro"])))
          ObsEstDistAtFirstDetection.yijrObs1 <- as.numeric(unname(unlist(PreSummaries.r[[1]]["ObsEstDistAtFirstDetection.yijro"])))
          
          if(SurveyType=="multiple"){
            
            ###NOTE: this section does not account for Double Counting
            if(PrDoubleCount!=0){stop("ERROR: Code does not accept PrDoubleCount>0")}
            
            FirstIntervalCounted.yijrObs2 <- as.numeric(unname(unlist(PreSummaries.r[[2]]["FirstIntervalCounted.yijro"])))
            DistAtFirstDetection.yijrObs2 <- as.numeric(unname(unlist(PreSummaries.r[[2]]["DistAtFirstDetection.yijro"])))
            ObsEstDistAtFirstDetection.yijrObs2 <- as.numeric(unname(unlist(PreSummaries.r[[2]]["ObsEstDistAtFirstDetection.yijro"])))
            
            CountObs1.yijr <- as.numeric(unlist(unname(PreSummaries.r[[1]]["Count.yijro"])))
            CountObs2.yijr <- as.numeric(unlist(unname(PreSummaries.r[[2]]["Count.yijro"])))    
            
            OnlyObs1.yijr <- 0
            OnlyObs2.yijr <- 0
            BothObs.yijr <- 0
            
            x11.yijr <- 0
            x21.yijr <- 0
            x22.yijr <- 0
            x12.yijr <- 0            
            
            if(CountObs1.yijr!=0 | CountObs2.yijr!=0){  #If at least 1 obs had Count=1
              
              if(CountObs1.yijr>0 & CountObs2.yijr>0) {BothObs.yijr <- 1}
              if(CountObs1.yijr>0 & CountObs2.yijr==0) {OnlyObs1.yijr <- 1}
              if(CountObs1.yijr==0 & CountObs2.yijr>0) {OnlyObs2.yijr <- 1}
              
              if(PrimaryObsNumber.yir.List[[y]][i,r]==1){ ##ObsA is primary obs
                if(CountObs1.yijr==0) {x21.yijr <- 1}
                if(CountObs1.yijr==1) {x11.yijr <- 1}
              }
              
              if(PrimaryObsNumber.yir.List[[y]][i,r]==2){ ##ObsB is primary obs
                if(CountObs2.yijr==0) {x12.yijr <- 1}
                if(CountObs2.yijr==1) {x22.yijr <- 1}
              }
            }
          }
          
          if(SurveyType=="removal"){
            if(RemovalPeriods!=3) stop("Removal Periods does not = 3. Need to change removal period summary in Simulation")
            
            if(EverCounted.yijr==0){
              EverCountedPeriod1.yijr <- 0
              EverCountedPeriod2.yijr <- 0
              EverCountedPeriod3.yijr <- 0
              FarnsEverCountedPeriod1.yijr <- 0
              FarnsEverCountedPeriod2.yijr <- 0
              FarnsEverCountedPeriod3.yijr <- 0
            }
            
            if(EverCounted.yijr>0){
              Period1 <- TempAll.ko.Outcomes.r[[o]][1:RemovalPeriod1Intervals]
              Period2 <- TempAll.ko.Outcomes.r[[o]][(RemovalPeriod1Intervals+1):(RemovalPeriod1Intervals+RemovalPeriod2Intervals)]
              Period3 <- TempAll.ko.Outcomes.r[[o]][(RemovalPeriod1Intervals+RemovalPeriod2Intervals+1):NIntervals]
              
              FarnsPeriod1 <- TempAll.ko.Outcomes.r[[o]][1:FarnsRemovalPeriod1Intervals]
              FarnsPeriod2 <- TempAll.ko.Outcomes.r[[o]][(FarnsRemovalPeriod1Intervals+1):(FarnsRemovalPeriod1Intervals+FarnsRemovalPeriod2Intervals)]
              FarnsPeriod3 <- TempAll.ko.Outcomes.r[[o]][(FarnsRemovalPeriod1Intervals+FarnsRemovalPeriod2Intervals+1):NIntervals]
              
              EverCountedPeriod1.yijr <- ifelse(sum(as.numeric(unlist(Period1)[names(unlist(Period1))=="Count.yijrko"]))>0,1,0)
              EverCountedPeriod2.yijr <- ifelse(sum(as.numeric(unlist(Period2)[names(unlist(Period2))=="Count.yijrko"]))>0,1,0)
              EverCountedPeriod3.yijr <- ifelse(sum(as.numeric(unlist(Period3)[names(unlist(Period3))=="Count.yijrko"]))>0,1,0)
              
              FarnsEverCountedPeriod1.yijr <- ifelse(sum(as.numeric(unlist(FarnsPeriod1)[names(unlist(FarnsPeriod1))=="Count.yijrko"]))>0,1,0)
              FarnsEverCountedPeriod2.yijr <- ifelse(sum(as.numeric(unlist(FarnsPeriod2)[names(unlist(FarnsPeriod2))=="Count.yijrko"]))>0,1,0)
              FarnsEverCountedPeriod3.yijr <- ifelse(sum(as.numeric(unlist(FarnsPeriod3)[names(unlist(FarnsPeriod3))=="Count.yijrko"]))>0,1,0)
            }
          }
          
          EverSing.yijr <- ifelse(sum(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="Sings.yijrk"]))>0,
                                1,0)
          EverSingingMode.yijr <- ifelse(sum(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="SingingMode.yijrk"]))>0,
                                       1,0)
          EverMove.yijr <- ifelse(sum(na.omit(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="DoesBirdMove.yijrk"])))>0,
                                1,0)
          NumberSongs.yijr <- sum(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="Sings.yijrk"]))
          NumberMoves.yijr <- sum(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="DoesBirdMove.yijrk"][2:NIntervals])  )
          MinDistance.yijr <- min(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="Distance.yijrk"]))
          MeanDistance.yijr <- mean(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="Distance.yijrk"]))
          ClosestAxesDistance.yij <- AxesClosestObs(Spatial.yij) ##For territory Spatial.yij, distance to closest of 4 major/minor axes
          #           MeanObservedWindspeed.yijr <- mean(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="WindSpeed.yijrk"]))
          MeanObservedNoise.yijr <- mean(as.numeric(unlist(TempAll.k.Outcomes.r)[names(unlist(TempAll.k.Outcomes.r))=="NoiseLevel.yirk"]))
          
          ##NOTE: Noiselevel is r-specific
          ## Therefore, NoiseLevel.yirk==Noise.yirk.List[[y]][[i]][[r]][k] for any k
          
          Summaries.r <- list(
            #"PreSummaries.r"=PreSummaries.r,
            "EverDetected.yijr"=EverDetected.yijr,
            "EverCorrectID.yijr"=EverCorrectID.yijr,
            "EverDoubleCounted.yijr"=EverDoubleCounted.yijr,
            "EverCounted.yijr"= EverCounted.yijr,
            "EverSing.yijr"=EverSing.yijr,
            "EverSingingMode.yijr"=EverSingingMode.yijr,
            "EverMove.yijr"=EverMove.yijr,
            "NumberSongs.yijr"=NumberSongs.yijr,
            "NumberMoves.yijr"=NumberMoves.yijr,
            "MinDistance.yijr"=MinDistance.yijr,
            "CountObs1.yijr"=ifelse(SurveyType=="multiple",CountObs1.yijr,NA),
            "CountObs2.yijr"=ifelse(SurveyType=="multiple",CountObs2.yijr,NA),
            "x11.yijr"=ifelse(SurveyType=="multiple",x11.yijr,NA),
            "x12.yijr"=ifelse(SurveyType=="multiple",x12.yijr,NA),
            "x21.yijr"=ifelse(SurveyType=="multiple",x21.yijr,NA),
            "x22.yijr"=ifelse(SurveyType=="multiple",x22.yijr,NA),
            "OnlyObs1.yijr"=ifelse(SurveyType=="multiple",OnlyObs1.yijr,NA),
            "OnlyObs2.yijr"=ifelse(SurveyType=="multiple",OnlyObs2.yijr,NA),
            "BothObs.yijr"=ifelse(SurveyType=="multiple",BothObs.yijr,NA),    
            "EverCountedPeriod1.yijr"=ifelse(SurveyType=="removal",EverCountedPeriod1.yijr,NA),
            "EverCountedPeriod2.yijr"=ifelse(SurveyType=="removal",EverCountedPeriod2.yijr,NA),
            "EverCountedPeriod3.yijr"=ifelse(SurveyType=="removal",EverCountedPeriod3.yijr,NA),
            "FarnsEverCountedPeriod1.yijr"=ifelse(SurveyType=="removal",FarnsEverCountedPeriod1.yijr,NA),
            "FarnsEverCountedPeriod2.yijr"=ifelse(SurveyType=="removal",FarnsEverCountedPeriod2.yijr,NA),
            "FarnsEverCountedPeriod3.yijr"=ifelse(SurveyType=="removal",FarnsEverCountedPeriod3.yijr,NA),
            
            # "ObserverIDNumber.yir"=ifelse(SurveyType!="multiple",ObserverIDNumber.yijrko,NA),
            #             "MeanObservedWindspeed.yijr"=MeanObservedWindspeed.yijr,
            "MeanObservedNoise.yijr"=MeanObservedNoise.yijr,
            "MeanDistance.yijr"=MeanDistance.yijr,
            "Distance.yijr0"=Distance.yijr0,
            "ClosestAxesDistance.yij"=ClosestAxesDistance.yij,
            "FirstIntervalCounted.yijrObs1"=FirstIntervalCounted.yijrObs1,
            "DistAtFirstDetection.yijrObs1"=DistAtFirstDetection.yijrObs1,
            "ObsEstDistAtFirstDetection.yijrObs1"=ObsEstDistAtFirstDetection.yijrObs1,
            "FirstIntervalCounted.yijrObs2"=ifelse(SurveyType=="multiple",FirstIntervalCounted.yijrObs2,NA),
            "DistAtFirstDetection.yijrObs2"=ifelse(SurveyType=="multiple",DistAtFirstDetection.yijrObs2,NA),
            "ObsEstDistAtFirstDetection.yijrObs2"=ifelse(SurveyType=="multiple",ObsEstDistAtFirstDetection.yijrObs2,NA)
          )
          
          #"Count.yijr"= max(unlist(TempOutcomes.o)["Count.yijrko"]),
          
          ##Store-r-level objects and summaries for NBirds.yi>0
          ListROutcomes.yijr[[y]][[i]][[j]][[r]] <- append(TempOutcomes.r,Summaries.r) #TempOutcomes.r 
          TempAll.rj.Outcomes.i[[r]][[j]] <- Summaries.r
          
          ##Temp Store all TempOutcomes.r for each j
          TempAll.r.Outcomes.j[[r]] <- TempOutcomes.r
          
        } ##r
        
        ## Index all j-specific objects
        if(NBirds.yi>0){
          TempOutcomes.j <- list(
            "Spatial.yij"=Spatial.yij
          )
        }
        
        if(NBirds.yi==0){
          TempOutcomes.j <- list(
            "Spatial.yij"=NA
          )
        }
        
        ##Create j-level summaries
        
        ##Store-j-level objects and summaries
        ## Currently there is no ListJOutcomes.yij 
        
        
        ##Temp Store all TempOutcomes.j for each i
        TempAll.j.Outcomes.i[[j]] <- TempOutcomes.j
        
      } ##j
      
      
    }  ##bracket refers to "if(NBirds.yi>0){"
    
    
    
    ## Index all i-specific objects  
    if(NBirds.yi==0){
      TempOutcomes.i <- list(
        "PercentHabitat.yi"=PercentHabitat.yi,
        "Area.yi"=Area.yi,
        "LambdaHabitat.yi"=LambdaHabitat.yi,
        "LambdaMatrix.yi"=LambdaMatrix.yi,
        "BirdsInHabitat.yi"=BirdsInHabitat.yi,
        "BirdsInMatrix.yi"=BirdsInMatrix.yi,
        "NBirds.yi"=NBirds.yi,
        #         "IsSiteGrassland.yi"=IsSiteGrassland.yi,
        #         "IsSiteDeciduous.yi"=IsSiteDeciduous.yi,
        "AllowedTerrSize.yi"=NA,
        "MeanTerrArea.yi"=NA,
        "SDTerrArea.yi"=NA
        #"SpatialList.yi"=NA
      )
    }
    
    if(NBirds.yi>0){
      ## Realized mean & sd of area of 95% UD ellipses for territories
      RealTerrAreaMean.yi <- mean(unlist(SpatialList.yi)[names(unlist(SpatialList.yi))=="Area.yij"])
      RealTerrAreaSD.yi <- sd(unlist(SpatialList.yi)[names(unlist(SpatialList.yi))=="Area.yij"])
      
      TempOutcomes.i <- list(
        "RealTerrAreaMean.yi"=RealTerrAreaMean.yi,
        "RealTerrAreaSD.yi"=RealTerrAreaSD.yi,
        "PercentHabitat.yi"=PercentHabitat.yi,
        "Area.yi"=Area.yi,
        "LambdaHabitat.yi"=LambdaHabitat.yi,
        "LambdaMatrix.yi"=LambdaMatrix.yi,
        "BirdsInHabitat.yi"=BirdsInHabitat.yi,
        "BirdsInMatrix.yi"=BirdsInMatrix.yi,
        "NBirds.yi"=NBirds.yi,
        #         "IsSiteGrassland.yi"=IsSiteGrassland.yi,
        #         "IsSiteDeciduous.yi"=IsSiteDeciduous.yi,
        "AllowedTerrSize.yi"=AllowedTerrSize.yi,
        "MeanTerrArea.yi"=MeanTerrArea.yi,
        "SDTerrArea.yi"=MeanTerrArea.yi
        #"SpatialList.yi"=SpatialList.yi  ##Left out b/c info is stored at j level
      )
    }
    
    ##Create i-level summaries  
    
    #     TempAll.rj.Outcomes.i[[r]][[j]][["EverDetected.yijr"]]
    #     length(TempAll.rj.Outcomes.i[[1]])
    
    Summaries.ir <- vector("list", NReps)
    for(rr in 1:NReps){
      
      EverCount.yir <- sum(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverDetected.yijr"] ))
      EverMove.yir <- sum(na.omit(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverMove.yijr"] )))
      EverDoubleCounted.yir <- sum(na.omit(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverDoubleCounted.yijr"] )))
      EverCorrectID.yir <- sum(na.omit(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverCorrectID.yijr"] )))
      EverSingingMode.yir <- sum(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverSingingMode.yijr"] ))
      EverSing.yir <- sum(as.numeric(unlist(TempAll.rj.Outcomes.i[[rr]])[
        names(unlist(TempAll.rj.Outcomes.i[[rr]]))=="EverSing.yijr"] ))
      
      Summaries.ir[[rr]] <- list(
        "EverCount.yir"=EverCount.yir,
        "EverMove.yir"=EverMove.yir,
        "EverDoubleCounted.yir"=EverDoubleCounted.yir,
        "EverCorrectID.yir"=EverCorrectID.yir,
        "EverSingingMode.yir"=EverSingingMode.yir,
        "EverSing.yir"=EverSing.yir
      )
    }
    
    ##Store-i-level objects and summaries  
    
    ListIOutcomes.yi[[y]][[i]] <- TempOutcomes.i
    ListIRSummaryOutcomes.yi[[y]][[i]] <- Summaries.ir
    
    print(paste("site", i))
    print(proc.time() - TIMER)
    
    ##Temp Store all TempOutcomes.i for each y
    TempAll.i.Outcomes.y[[i]] <- TempOutcomes.i
    
  } ##i
  
  
  ## Index all y-specific objects (Both NBirds.yi==0 & NBirds.yi>0)
  TempOutcomes.y <- list(
    # "SurveyType"=SurveyType,
    "Iteration"=Iteration,
    "Ns.y"= Ns.y,
    "ContinentalPopulation.y"= ContinentalPopulation.y,
    "RangeHabitatArea.y"= RangeHabitatArea.y,
    "RangeMatrixArea.y"= RangeMatrixArea.y,
    "StudyHabitatArea.y"= StudyHabitatArea.y,
    "StudyMatrixArea.y"= StudyMatrixArea.y,
    "RangeHabitatAbundance.y"=RangeHabitatAbundance.y,
    "RangeMatrixAbundance.y"= RangeMatrixAbundance.y,
    "MatrixDensity.y"= MatrixDensity.y, 
    "StudyHabitatAbundance.y"= StudyHabitatAbundance.y,
    "StudyMatrixAbundance.y"= StudyMatrixAbundance.y,
    "HabitatProportionTheta.y"= HabitatProportionTheta.y,
    "StudyHabitatProportion.y"=StudyHabitatProportion.y,
    "SurveyLength"=SurveyLength,
    "RemovalPeriod1Length"=ifelse(SurveyType=="removal",RemovalPeriod1Length,NA),
    "RemovalPeriod2Length"=ifelse(SurveyType=="removal",RemovalPeriod2Length,NA),
    "RemovalPeriod3Length"=ifelse(SurveyType=="removal",RemovalPeriod3Length,NA),
    "RemovalPeriod1Intervals"=ifelse(SurveyType=="removal",RemovalPeriod1Intervals,NA),
    "RemovalPeriod2Intervals"=ifelse(SurveyType=="removal",RemovalPeriod2Intervals,NA),
    "RemovalPeriod3Intervals"=ifelse(SurveyType=="removal",RemovalPeriod3Intervals,NA)
  )
  
  ## Create y-level summaries
  
  MeanPercentHabitat.y <- mean(as.numeric(unlist(ListIOutcomes.yi[[y]])[names(unlist(ListIOutcomes.yi[[y]]))=="PercentHabitat.yi"]))
  #mean(ListIOutcomes.yi[[y]][["PercentHabitat.yi"]])
  
  Summaries.y <- list(
    "MeanPercentHabitat.y"=MeanPercentHabitat.y
  )
  
  
  ## Store-y-level objects and summaries
  ListYOutcomes.y[[y]] <- append(TempOutcomes.y,Summaries.y)
  #ListYSummaryOutcomes.y[[y]] <- Summaries.y
  
  #print(paste(SurveyType,"Iteration", Iteration, "Complete"))
  
  # ###########################################################
  # ##Save dataset for year y for future analysis
  DATE <- format(Sys.time(), "%m_%d_%Y")
  SimName <- paste("Sim",SIMNUM,SurveyType,"Iteration", Iteration, sep="_")
  
  setwd(SaveDirectory)
  
  save(NYears, NSurveySites, NReps, SurveyType, SurveyOptions, 
       NIntervals, NSimultaneousObservers, NTotalObservers, ObserverIDs,
       PrimaryObsNumber.yir.List, 
       ListYOutcomes.y,
       ListIOutcomes.yi,
       ListROutcomes.yijr,
       ListIRSummaryOutcomes.yi,
       #        ListKOutcomes.yijrk,
       #        ListOOutcomes.yijrko, 
       file=paste("BTBW_",DATE,"_",SimName,".RData", sep=""))
  
  setwd(MasterDirectory)
  
  #   rm(ListYOutcomes.y)    
  #   rm(ListIOutcomes.yi)
  #   rm(ListIRSummaryOutcomes.yi)
  #   rm(ListROutcomes.yijr)
  #   rm(ListKOutcomes.yijrk)
  #   rm(ListOOutcomes.yijrko)
  
  
} ##y


##Clean up data frames tracking rare phenomena
if(nrow(MisID)>1) MisID <- na.omit(MisID)
if(nrow(DoubleCounted)>1) DoubleCounted <- na.omit(DoubleCounted)

######################
## REFERENCES
######################
# 
# Jennrich, R. I., and F. B. Turner (1969). 
# Measurement of non-circular home range. 
# Journal of Theoretical Biology 22:227-237.
#
# Justus, C. G., Hargraves, W. R., Mikhail, A., & Graber, D. (1978). 
# Methods for estimating wind speed frequency distributions. 
# Journal of Applied Meteorology (1962-1982), 350-353.

######## END SIMULATION #################################################