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

## Input parameters for simulation of detection of birds during point count surveys

## load necessary packages
library(arm)
library(reshape)

##########################
## Scenario Parameters  
##########################

## Note: Survey type and number of survey sites are specified in master script 
##   (A0 - Run simulation via source.R).
## They must be specified here to run this script independently.
## Survey Type 
# SurveyType <- "multiple"  
# SurveyType <- "removal" 
# SurveyType <- "nmixture" 
# SurveyType <- "distance" 

# SurveyOptions <- c("multiple", "distance", "nmixture", "removal")
# NSurveySites <- 1  ## No. of survey sites surveyed within one iteration (year, etc.) 


##########################
## Basic Modeling Parameters
##########################

NYears <- 1           ## No. of Years or Seasons across which surveys take place
NReps <- ifelse(SurveyType=="nmixture",3,1)          ## No. of replications (No. of times each survey site is surveyed WITHIN season)
IntervalLength <- 2 ## No. of SECONDS of one unit of time during which bird may vocalize (or not)
IntervalsPerHour <- (60*60)/IntervalLength  ##No. of intervals in 1 hour
IntervalsPerMinute <- IntervalsPerHour/60  ##No. of intervals in 1 MINUTE
SurveyLength <- ifelse(SurveyType == "removal",10,3)   ## No. of MINUTES that 1 bird survey lasts
NIntervals <- (SurveyLength*60)/IntervalLength ##No. of intervals of IntervalLength that make up each survey
if(as.integer(NIntervals) != NIntervals){stop("ERROR - Number of intervals must be an integer.  Adjust parameter SurveyLength or IntervalLength.")}

if(SurveyType=="removal"){
  RemovalPeriods <- 3
  RemovalPeriod1Length <- 2
  RemovalPeriod2Length <- 3
  RemovalPeriod3Length <- 5
  
  FarnsRemovalPeriod1Length <- 3
  FarnsRemovalPeriod2Length <- 2
  FarnsRemovalPeriod3Length <- 5
  
  RemovalPeriod1Intervals <- (RemovalPeriod1Length/SurveyLength)*NIntervals
  RemovalPeriod2Intervals <- (RemovalPeriod2Length/SurveyLength)*NIntervals
  RemovalPeriod3Intervals <- (RemovalPeriod3Length/SurveyLength)*NIntervals
  
  FarnsRemovalPeriod1Intervals <- (FarnsRemovalPeriod1Length/SurveyLength)*NIntervals
  FarnsRemovalPeriod2Intervals <- (FarnsRemovalPeriod2Length/SurveyLength)*NIntervals
  FarnsRemovalPeriod3Intervals <- (FarnsRemovalPeriod3Length/SurveyLength)*NIntervals
  
  if(NIntervals != sum(RemovalPeriod1Intervals,RemovalPeriod2Intervals,RemovalPeriod3Intervals) |
     NIntervals != sum(FarnsRemovalPeriod1Intervals,FarnsRemovalPeriod2Intervals,FarnsRemovalPeriod3Intervals)){
    stop("Intervals do not evenly divide among Removal periods")
  }
}

Xlim <- c(-1000,1000) ##Min and max x coordinate for xy grid generated around observer
Ylim <- c(-1000,1000) ##Min and max y coordinate for xy grid generated around observer


##########################
## Population parameters
##########################

ContinentalPopulation <- 2100000/2 #rep(2100000/2, NYears) ## Number of male birds in continental population
ContinentalPopulationCoef <- rep(1,NYears)
ContinentalPopulationAll.vector <- sort(ContinentalPopulationCoef * ContinentalPopulation)
##BTBW pop. estimate=2,100,000 PAIRS, from Partners in Flight: 
## divide by 2 to get number of males.
## original link (accessed 2016) http://rmbo.org/pifpopestimates/Database.aspx
## updated link (accessed 2022) https://pif.birdconservancy.org/population-estimates-database/

HabitatPreference.y <- 0.9 ## % of total population found in habitat (as opposed to matrix) across entire range
RangeArea <- 3.62*10^11  ##(m^2)  Size of entire species' range (7*10^11 m^2 is the approx. size of Texas)
## RangeArea link (accessed 2016) http://www.birdlife.org/datazone/species/factsheet/22721673  
## updated RangeArea link (accessed 2022, range has changed): http://datazone.birdlife.org/species/factsheet/22721673 

StudyArea <- 3000*10000  ##(m^2) Size of study area (ps ~ StudyArea/RangeArea)
## NOTE: StudyArea is 10x size of Hubbard Brook
## https://hubbardbrook.org/about

HabitatDensity.y <- 0.534/10000 #0.62/10000 ##Mean density (birds/m^2) for habitat within species range
## from Holmes 1986, adjusted from individuals/10HA to males/m^2

RangeHabitatProportion.vector <- (HabitatPreference.y*ContinentalPopulationAll.vector)/(HabitatDensity.y*RangeArea)  
## % of species' entire range that is habitat (as opposed to matrix)

StudyHabitatProportion <- function(N=1, min, max){  ##Distribution for mean %Habitat for entire study area 
  Value <- runif(N, min, max)
  return(Value)
}
StudyHabitatProportionmin <- 0.7  ##Study area is between 70% and 100% habitat (mean for each Iteration)
StudyHabitatProportionmax <- 1
## If study area is range-wide, use HabitatProportionSurveyedArea.y <- RangewideHabitatProportion.y
## If study area is a subset of the range, use expected mean % habitat among sites (e.g., we picked sites with ~60% habitat)

HabitatProportionThetaAll <- rep(8,NYears)   ##Concentration parameter for Beta distribution - see rbetaAlt function


##########################
## Environmental / Temporal parameters
##########################

##Survey timing set up
StartingJulianDate <- 150  ##First day of surveys
SurveysPerDay <- ifelse(SurveyType == "removal",6,7)
LogisticalSurveyTime <- ifelse(SurveyType == "removal",30,23)  ## mean MINUTES taken to conduct 1 survey, including travel time
EndingJulianDate <- 220  ##Last possible day of surveys


##########################
## Biological Parameters
##########################

## Assume leaves are PRESENT in forest for all surveys.
FirstLeavesDay <- StartingJulianDate #+ 10  ##First day when Leaves=1
LeavesByDate <- c(rep(0,FirstLeavesDay-1), ##before Leaf Out
                  rep(1,(EndingJulianDate-FirstLeavesDay+1)))
##LeavesByDate is an index, where LeavesByDate[Day] indicates if there were leaves (0/1) on Julian Date "Day"

BackgroundMovementRate <- 0.005  ## Species-specific probability of bird moving between interval t=0 and t=1  
## 0.005 results in 78% of birds moving at least once in 10 min period. 
## For 3 passerine species, Granholm (1983) found the minimum probability of movement within a 10-minute period was 36%, 64%, and 72%.

MeanTerrArea <- 3.60*10000 ##3.6HA, from Sherry and Holmes 1985, pg 288 in "Habitat Selection in birds", Martin Cody ed. (m^2)  
SDTerrArea <- 10000 ##1 HA            ##SD of territory area for this species (m^2)
OverlapUDTerr <- 0.565  ## %Utilization Distribution at which overlap of territories is accessed
UDTerr <- 0.95  ##% Utilization Distribution modeled for all final bird territories

##########################
## Perceptibility parameters
##########################

##NOTE: Ambient Noise is binary (present/absent) at the amplitude Alldredge tested
PrNoise <- 0.15 ## probability that background noise occurred

## Alternate version of Noise (currently unused)
##  NoiseLevelDistrib <- round(runif(10000,0,10),2) # runif(1,0,10)
#   NoiseLevelMin <- 0
#   NoiseLevelMax <- 10

## NOTE: Wind speed is included in simulation code but unused at this time.
# DailyMeanWindSpeedMin <- 0.01  ##mean daily wind speed (minimum), using true zero can create errors
# DailyMeanWindSpeedMax <- 10   ## mean daily wind speed (maximum)
# RepMeanWindSpeedSD <- 0.5  ## SD daily wind speed 
##MeanWindSpeedDistrib <- round(runif(10000,0,10),2) ##WindSpeed distribution
# WindVariability <- 0.94  ## Medium variability, from Justus 1977 
## WindVariability <- 1.05 ## Low variability, from Justus 1977 
## WindVariability <- 0.83 ## High variability, from Justus 1977 
# WindIndexIEffect <- 0 ##effect of wind index on intercept of Logit(Pd)

## NOTE: other vegetation types (below) are included in simulation code but unused at this time.
# PrGrassland.y <- rep(0,NYears) ##Probability that site i is grassland (as opposed to forest)
# PrDeciduous.y <- rep(0,NYears) ##If site i is forest, Probability that site i is Deciduous (as opposed to Mixed Pine-Deciduous)
# GrasslandIEffect <- 0 ##effect of grassland categorical variable on intercept of Logit(Pd)
# DeciduousIEffect <- 0 ##effect of deciduous categorical variable on intercept of Logit(Pd)

## Parameters below affecting perceptibility.yijrko use coefficients from Pacifici et al. (2008), 
## estimated for BTBW in mixed pine-hardwood forest with leaves present.
SpeciesIntercept <- 11.816 ## Logit(Pd) intercept for species in scenario of interest
LeavesIEffect <- 1.47 ##effect of leaves categorical variable on intercept of Logit(Pd)
NoiseIEffect <- 0.1035 ##effect of ambient noise on intercept of Logit(Pd)
DistanceEffect.yijr <- -0.0644 ## mean slope of Logit(Pd)
# WindIndexSEffect <- 0 ##effect of wind index on slope of Logit(Pd)
# GrasslandSEffect <- 0 ##effect of grassland categorical variable on slope of Logit(Pd) (interaction of Distance and grassland)
# DeciduousSEffect <- 0 ##effect of deciduous categorical variable on slope of Logit(Pd) (interaction of Distance and Deciduous)
LeavesSEffect <- -0.0444 ##effect of leaves categorical variable on slope of Logit(Pd) (interaction of Distance and Leaves)
NoiseSEffect <- -0.0233 ##effect of ambient noise on slope of Logit(Pd)
NoiseXLeavesEffect <- -0.9528  ## interaction effect of ambient noise and presence of leaves

NTotalObservers <- 2  ## Number of total observers
NSimultaneousObservers <- ifelse(SurveyType=="multiple",2,1) ## Number of simultaneous observers
ObserverVariation <- 0.4097  ## Observer variation; 0.5*SD of observers = 0.4097 for BTBW (Pacifici 2008)

## for single-observer surveys, Observer ID is a replication-level variable
ObserverIDs <- LETTERS[1:NTotalObservers] ## Observer ID
ObserverIEffects <- data.frame("A"=0.002425+0.4097, ## effect of Observer ID on intercept of Logit(Pd)
                               "B"=0.002425-0.4097)  
ObserverSEffects <- data.frame("A"=0, ## effect of Observer ID on slope of Logit(Pd)
                               "B"=0)

PrCorrectID <- 1  ## Probability that a bird is correctly identified
PrDoubleCount <- 0 ## Probability that one bird is counted as 2)

##Observer estimation of distance
ObserverDistanceCategories <- data.frame(
  ## From Alldrege et al. 2007 "A field evaluation of distance measurement error..."
  
  ##Alldredge distance-dependent error curve:
  #     "Distance"=c(0,23,37,52,65,75,86,98),
  #     "meanerror"=c(0,18.5,-1.6,8.8,14.8,5.5,-2.1,-7.9),
  #     "sderror"=c(0,18.9,11,22.3,22.1,19.2,17.4,16.8))
  
  ##Alldredge error curve based on overall mean & SD error:
  "Distance" = c(0,62.286),
  "meanerror" = c(0,7.6),
  "sderror" = c(0,21.4)
)

##Distance above uses mean of measured distances in Alldredge (2007)= 62.286
##mean(c(23,37,52,65,75,86,98)) == 62.28571
#   
#   ##Plot the mean & SD error for Obs estimated Distance using above values
#   plot(NULL, NULL, xlim=c(0,210), ylim=c(0,210))
#   for(ii in 1:200){
#     MEAN <- ObserverEstDistance(ii,ObserverDistanceCategories,Output="meanonly")
#     SDD <- ObserverEstDistance(ii,ObserverDistanceCategories,Output="sdonly")
#     points(ii,MEAN)
#     points(ii,MEAN+SDD, col="blue")
#     points(ii,MEAN-SDD,col="blue")
#   }
#   lines(c(0,200), c(0,200))


##########################
## Availability Parameters
##########################

##Coarse-scale Markov song Parameters
PSS.yijr <- 0.98    ##Pr(Bird sings during next interval, given that it did sing before)

##Fine-scale Markov song parameters (Species/Scenario-specific)
SongLength <- 2.1/IntervalLength ##Number of intervals for avg. song length (interval=2 sec)
PauseLength <- 6.6/IntervalLength ##Number of intervals for avg. pause between songs
q1.fine <- SongLength/(SongLength+PauseLength)

SingingStates.fine <- c("S", "NS1", "NS2", "NS3")
TransitionMatrix.fine <- matrix(c(0,0,0.08,0.8,1,0,0,0,0,1,0,0,0,0,0.92,0.2), 
                                nrow=4, 
                                ncol=4)  ##Transition matrix for fine-scale autocorrelation of Singing
## above TransitionMatrix.fine produces Pauses w/mean length 6.3 seconds (SD=1.25 sec, q1=0.241) (BTBW values)
## Values determined via simulation.

ReportedPrSingMin <- 10  ## x minutes in reported song rates: Pr(Bird j sings w/in x minutes)
## PrSing.yijrk refers to Pr(bird sings at least 1x in ReportedPrSingMin [usually 5 or 10] minutes)


##########################
## Scenario Distributions
##########################

## Generate a k-specific probability of movement
PrBirdMoves <- function(N=1, MeanMovementRate){
  PrBirdMoves.yijrk <- MeanMovementRate + rnorm(N,0,0.0005) 
  if(sum(PrBirdMoves.yijrk<0)>0) stop("Negative values generated for PrBirdMoves - reevaluate variation!")
  return(abs(PrBirdMoves.yijrk))
}
#hist(PrBirdMoves(10000,BackgroundMovementRate))

## Generate probability that a bird flushes due to observer
PrBirdMoveObs <- function(Intercept=1, Slope=-0.1, Distance){
  PrMoveObs <- invlogit(Intercept+Slope*Distance)
  return(PrMoveObs)
}
## Plot Pr(Flush)~Distance
#plot(1:200,PrBirdMoveObs(Distance=1:200)) 

## Maximum daily song rate (Pr bird j sings w/in ReportedPrSingMin min), based on Julian Date
MaxDailySongRate <- function(Day){
  if(Day < 122) return(0)  ##before May 1, birds not on breeding grounds
  if(Day >= 122 & Day <= 197) return(0.9)  ## High PRSing, 1 May - 15 July
  if(Day > 197 & Day <= 244) return(0.5)   ## Med PRSing, 16 July - 31 August
  if(Day > 244 & Day <= 274) return(0.2)   ## Low PRSing, 1-30 September 
  if(Day > 274) return(0)   ##Oct 1 and after, birds not on breeding grounds 
  
  ## MODERATE SINGER VERSION
  #   if(Day<122 ) return(0)  ##before , birds not on breeding grounds
  #   if(Day>=122 & Day<=197) return(0.5)  ## High PRSing, 1 May - 15 July
  #   if(Day>197 & Day<=244) return(0.25)   ## Med PRSing, 16 July - 31 August
  #   if(Day>244 & Day<=274) return(0.1)   ## Low PRSing, 1-30 September 
  #   if(Day>274) return(0)   ##Oct 1 and after, birds not on breeding grounds 
  
  ## WEAK SINGER VERSION
  #   if(Day<122 ) return(0)  ##before , birds not on breeding grounds
  #   if(Day>=122 & Day<=197) return(0.3)  ## High PRSing, 1 May - 15 July
  #   if(Day>197 & Day<=244) return(0.1)   ## Med PRSing, 16 July - 31 August
  #   if(Day>244 & Day<=274) return(0.1)   ## Low PRSing, 1-30 September 
  #   if(Day>274) return(0)   ##Oct 1 and after, birds not on breeding grounds 
  
}
## plot the maximum daily song rate throughout the year (Jan 1 - Dec 31)
# plot(NULL,NULL, 
#      ylim=c(0,1), xlim=c(0,370),
#      xlab = "Ordinal Date", ylab = "Max Daily SOng Rate")
# for(dd in 1:365){
#   points(dd,MaxDailySongRate(dd))
# }

#######
## Song rate weight, based on Time of Day
##  Units of Time.yijrk = No. intervals since sunrise (0=sunrise exactly)
##  To visualize, assume sun rises 6 AM daily.
##  Parameterized with BTBW data from P. Blancher, Environment Canada (personal communication)
TimeOfDayWeight <- function(TimeinIntervals, IntervalsPerHour){
  if(TimeinIntervals <= -0.25*IntervalsPerHour) return(0.2)  ## before 15 min before sunrise
  if(TimeinIntervals > -0.25*IntervalsPerHour & TimeinIntervals <= 0*IntervalsPerHour) return(0.6)  ##between 15 min before sunrise and sunrise
  if(TimeinIntervals > 0*IntervalsPerHour & TimeinIntervals <= 0.5*IntervalsPerHour) return(0.8)  ##between sunrise and 0.5 hours after sunrise
  if(TimeinIntervals > 0.5*IntervalsPerHour & TimeinIntervals <= 1.5*IntervalsPerHour) return(1)  ##between 0.5 hours after sunrise and 1.5 hours after sunrise
  if(TimeinIntervals > 1.5*IntervalsPerHour & TimeinIntervals <= 6*IntervalsPerHour) return(0.8)  ##between 1.5 hours after sunrise and 6 hours after sunrise
  if(TimeinIntervals > 6*IntervalsPerHour) return(0.2)   ##after 6 hours after sunrise (~noon)
}
## plot the time of day weight throughout the day (0 = sunrise)
# plot(NULL,NULL, ylim=c(0,1), xlim=c(-6*IntervalsPerHour, 18*IntervalsPerHour),
#      xlab = "Intervals since sunrise", ylab = "Time of day weight")
# for(ii in seq(-6*IntervalsPerHour, 18*IntervalsPerHour,100)){
#   points(ii,TimeOfDayWeight(ii,IntervalsPerHour))
# }
# lines(c(0,0),c(0,1))


## Mean Noise Level for rep r
##  NOTE: Noise is binary (present/absent) at the amplitude Alldredge tested
NoiseLevelDistrib <- function(NumberTrials){
  rbinom(NumberTrials,1,PrNoise) 
}

##  Alternate version based on NoiseLevelMin & NoiseLevelMax parameters, above.
# NoiseLevelDistrib <- function(Number, Min, Max){
#   round(runif(Number,Min,Max),2) # runif(1,0,10)
# }

## Mean WindSpeed for Day.yr
##  Based on DailyMeanWindSpeedMin & DailyMeanWindSpeedMax parameters, above
# DailyMeanWindSpeedDistrib <- function(Number, Min, Max){
#   round(runif(Number,Min,Max),2) # runif(1,0,10)
# }

## Mean WindSpeed for rep r
##  Based on mean wind speed produced by above function & RepMeanWindSpeedSD
# RepMeanWindSpeedDistrib <- function(Number, Mean, SD){
#   round(rlnormAlt(Number,Mean,SD),2) # runif(1,0,10)
# }

##########################
## References
##########################

# Alldredge, M. W., T. R. Simons, and K. H. Pollock (2007c). 
# A field evaluation of distance measurement error in auditory avian point count surveys. 
# Journal of Wildlife Management 71:2759-2766.
# 
# BirdLife International (2016). 
# Species factsheet: Dendroica caerulescens. 
# original link 2016: http://www. birdlife. org/datazone/species/factsheet/22721673.
# updated link 2022, range has changed): http://datazone.birdlife.org/species/factsheet/22721673 
# 
# Granholm, S. L. (1983). 
# Bias in density estimated due to movement of birds. 
# The Condor 85:243-248.
# 
# Holmes, R. T., T. W. Sherry, and F. W. Sturges (1986). 
# Bird community dynamics in a temperate deciduous forest: long-term trends at Hubbard Brook. 
# Ecological Monographs 56:201-220.
#
# Hubbard Brook. About the Hubbard Brook Ecosystem Study. https://hubbardbrook.org/about
# 
# Justus, C. G., Hargraves, W. R., Mikhail, A., & Graber, D. (1978). 
# Methods for estimating wind speed frequency distributions. 
# Journal of Applied Meteorology (1962-1982), 350-353.
# 
# Pacifici, K., T. R. Simons, K. H. Pollock (2008). 
# Effects of vegetation and background noise on the detection process in auditory avian point-count surveys. 
# The Auk 125:600-607.
# 
# Partners in Flight (2015). 
# Population Estimates Database.
## original link (accessed 2016) http://rmbo.org/pifpopestimates/Database.aspx
## updated link (accessed 2022) https://pif.birdconservancy.org/population-estimates-database/
# 
# Sherry, T. W., and R. T. Holmes (1985). 
# Dispersion patterns and habitat responses of birds in northern hardwoods forests. 
# Habitat Selection in Birds. (M. L. Cody, Editor). Academic Press, FL, USA.

### END PARAMETERS #############################################################