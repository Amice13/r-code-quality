
setwd("D:/Manuscript1/RCode")


library(foreign)
library(Hmisc)
library(extrafont)
library(ggpmisc)
library(EcoHydRology)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(sf)
library(gridExtra)
library(broom)
library(car)
library(vcd)
library(MASS)
library(scales)
library(stringr)
library(reshape2)
library(corrplot)
library(stringr)
library(lubridate)
library(forecast)
library(fpp2)
library(urca)
library(viridis)
library(keras)
library(tensorflow)
library(extRemes)
library(ggpattern)

windowsFonts()
loadfonts(device = "win")

#Climate model results analysis. Downscaled climate models produced by Kenneth Kunkel and Andrew Ballinger
##Uploading climate model data. Have several different models, and RCP 4.5 and RCP 8.5 warming scenarios. All should be in the RCode folder
models <- c("ACCESS1-0", "ACCESS1-3", "bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CCSM4", "CMCC-CM", "CNRM-CM5", "CSIRO-MK3-6-0", "GFDL-ESM2G", "GFDL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "MPI-ESM-LR", "MPI-ESM-MR", "NorESM1-M") 
models_underscore <- c("ACCESS1_0", "ACCESS1_3", "bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM", "CanESM2", "CCSM4", "CMCC_CM", "CNRM_CM5", "CSIRO_MK3_6_0", "GFDL_ESM2G", "GFDL_ESM2M", "HadGEM2_CC", "HadGEM2_ES", "inmcm4", "IPSL_CM5A_LR", "IPSL_CM5A_MR", "MIROC5", "MIROC_ESM", "MIROC_ESM_CHEM", "MPI_ESM_LR", "MPI_ESM_MR", "NorESM1_M")
subModels <- c("r1i1p1", "r6i1p1")
emissions <- c("rcp45", "rcp85")
variables <- c("pr", "tasmax", "tasmin")


for(i in 1:length(models)){
  #Have to make a special if statement because the subModel variable is different for CCSM4 model and no others
  if(models[i] == "CCSM4"){j <- 2}else{j <- 1}
  
  for(k in 1:length(emissions)){
    for(m in 1:length(variables)){
      #Importing csvs
      eval(parse(text = paste(models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], " <- data.frame(read.table(\"D:/Manuscript1/RCode/Valdivia_models/Valdivia_Pichoy.", models[i], ".", subModels[j], ".", emissions[k], ".", variables[m], ".1950.2100.txt\", header = TRUE))", sep = "")))
      #Establishing column names
      eval(parse(text = paste("if(variables[m] == \"pr\"){colnames(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], ")[4] <- \"precip_mm\"} else if(variables[m] == \"tasmax\") {colnames(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], ")[4] <- \"T_max_celsius\"} else {colnames(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], ")[4] <- \"T_min_celsius\"}", sep = "")))
      #Attaching model name
      eval(parse(text = paste(models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$Model <- models_underscore[i]", sep = "")))
      #Creating a FullDate column using the year, month, and day columns, then converting it to a date-time variable that R can work with
      eval(parse(text = paste(models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$FullDate <- paste(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$year, ", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$month, ", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$day, sep = \"-\")", sep = "")))
      eval(parse(text = paste(models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$FullDate <- as.POSIXct(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], "$FullDate, format = \"%Y-%m-%d\", tz = \"UTC\")", sep = "")))
    }
  }
}


##Creating an "ensemble" dataframe for each variable type that is just the concatenated form of all model tables, separated by emissions scenario (rcp45 or rcp85)
k = 1
for(k in 1:length(emissions)){
  for(m in 1:length(variables)){
    eval(parse(text = paste("ensembleFrame_", emissions[k], "_", variables[m], " <- ", models_underscore[1], "_", subModels[j], "_", emissions[k], "_", variables[m], sep = "")))
    for(i in 2:length(models)){
      if(models[i] == "CCSM4"){j <- 2}else{j <- 1}
      eval(parse(text = paste("ensembleFrame_", emissions[k], "_", variables[m], " <- rbind(ensembleFrame_", emissions[k], "_", variables[m], ", ", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], ")", sep = "")))
    }
  }
}


##Removing all the individual model csvs to clear up memory
for(i in 1:length(models)){
  if(models[i] == "CCSM4"){j <- 2}else{j <- 1}
  
  for(k in 1:length(emissions)){
    for(m in 1:length(variables)){
      eval(parse(text = paste("rm(", models_underscore[i], "_", subModels[j], "_", emissions[k], "_", variables[m], ")", sep = "")))
    }
  }
}
k = 1
n = 1
##Putting Tmin, Tmax, and precip all in one data frame; don't need to run the loop for precip, since I am basing the original data frame off of that file. Note: this takes a LONG time to run
emissions <- "rcp85"
for(k in 1:length(emissions)){
  eval(parse(text = paste("ensembleFrame_", emissions[k], " <- ensembleFrame_", emissions[k], "_pr", sep = "")))
  eval(parse(text = paste("ensembleFrame_", emissions[k], "$T_min <- NA", sep = "")))
  eval(parse(text = paste("ensembleFrame_", emissions[k], "$T_max <- NA", sep = "")))
  eval(parse(text = paste("ensembleFrame_", emissions[k], "$FullDate <- paste(ensembleFrame_", emissions[k], "$year, ensembleFrame_", emissions[k], "$month, ensembleFrame_", emissions[k], "$day, sep = \"-\")", sep = "")))
  eval(parse(text = paste("ensembleFrame_", emissions[k], "$FullDate <- as.POSIXct(ensembleFrame_", emissions[k], "$FullDate, format = \"%Y-%m-%d\", tz = \"UTC\")", sep = "")))
  eval(parse(text = paste("for(n in 1:length(ensembleFrame_", emissions[k], "$year)){ensembleFrame_", emissions[k], "$T_min[n] <- ensembleFrame_", emissions[k], "_", variables[3], "[which(ensembleFrame_", emissions[k], "_", variables[3], "$FullDate == ensembleFrame_", emissions[k], "$FullDate[n] & ensembleFrame_", emissions[k], "_", variables[3], "$Model == ensembleFrame_", emissions[k], "$Model[n]), 4]}", sep = "")))
  eval(parse(text = paste("for(n in 1:length(ensembleFrame_", emissions[k], "$year)){ensembleFrame_", emissions[k], "$T_max[n] <- ensembleFrame_", emissions[k], "_", variables[2], "[which(ensembleFrame_", emissions[k], "_", variables[2], "$FullDate == ensembleFrame_", emissions[k], "$FullDate[n] & ensembleFrame_", emissions[k], "_", variables[2], "$Model == ensembleFrame_", emissions[k], "$Model[n]), 4]}", sep = "")))
}

ensembleFrame_rcp85_tasmin[which(ensembleFrame_rcp85_tasmin$FullDate == ensembleFrame_rcp85$FullDate[n] & ensembleFrame_rcp85_tasmin$Model == ensembleFrame_rcp85$Model[n]), 4]


write.csv(ensembleFrame_rcp45, "ensembleFrame_rcp45_wModels.csv")
write.csv(ensembleFrame_rcp85, "ensembleFrame_rcp85_wModels.csv")

#Reading ensemble frame csvs from previous step so that the above work does not need to be run again. Previous steps take a LONG time
ensembleFrame_rcp45 <- read.csv("ensembleFrame_rcp45_wModels.csv")
ensembleFrame_rcp45$FullDate <- as.POSIXct(ensembleFrame_rcp45$FullDate, format = "%Y-%m-%d", tz = "UTC")
ensembleFrame_rcp45 <- ensembleFrame_rcp45[-1]

ensembleFrame_rcp85 <- read.csv("ensembleFrame_rcp85_wModels.csv")
ensembleFrame_rcp85$FullDate <- as.POSIXct(ensembleFrame_rcp85$FullDate, format = "%Y-%m-%d", tz = "UTC")
ensembleFrame_rcp85 <- ensembleFrame_rcp85[-1]



##Calculating annual rainfall for all modeled years for all models
annualRainfall_rcp45 <- data.frame(matrix(nrow = 0, ncol = 4, NA))
colnames(annualRainfall_rcp45) <- c("FullDate", "Model", "Warming", "Precip_annual")
annualRainfall_rcp85 <- data.frame(matrix(nrow = 0, ncol = 4, NA))
colnames(annualRainfall_rcp85) <- c("FullDate", "Model", "Warming", "Precip_annual")


emissions <- c("rcp45", "rcp85")
warming <- c("RCP 4.5", "RCP 8.5")


emissions <- c("rcp85")
warming <- c("RCP 8.5")

yearsForModels <- unique(ensembleFrame_rcp85$year)

i = 1
j = 1
k = 1

for(k in 1:length(emissions)){
  for(i in 1:length(models_underscore)){
    if(models[i] == "CCSM4"){j <- 2}else{j <- 1}
    for(j in 1:length(yearsForModels)){
      tempFrame <- data.frame(matrix(nrow = 1, ncol = 4, NA))
      colnames(tempFrame) <- c("FullDate", "Model", "Warming", "precip_annual")
      
      tempFrame$FullDate <- as.POSIXct(paste(yearsForModels[j], "-01-01", sep = ""), format = "%Y-%m-%d", tz = "UTC")
      tempFrame$Model <- models_underscore[i]
      tempFrame$Warming <- warming[k]
      eval(parse(text = paste("tempFrame$precip_annual <- sum(na.omit(ensembleFrame_", emissions[k], "$precip_mm[which(format(ensembleFrame_", emissions[k], "$FullDate, \"%Y\") == yearsForModels[j] & ensembleFrame_", emissions[k], "$Model == \"", models_underscore[i], "\")]))", sep = "")))
      eval(parse(text = paste("annualRainfall_", emissions[k], " <- rbind(annualRainfall_", emissions[k], ", tempFrame)", sep = "")))
    }
  }
}

##Writing annual rainfall to csvs so I do not have to run this analysis again
#write.csv(annualRainfall_rcp45, "annualRainfall_rcp45.csv")
#write.csv(annualRainfall_rcp85, "annualRainfall_rcp85.csv")


##Reading annual rainfall data from csvs
annualRainfall_rcp45 <- read.csv("annualRainfall_rcp45.csv")
annualRainfall_rcp85 <- read.csv("annualRainfall_rcp85.csv")
annualRainfall_rcp45$FullDate <- as.POSIXct(annualRainfall_rcp45$FullDate, format = "%Y-%m-%d", tz = "UTC")
annualRainfall_rcp85$FullDate <- as.POSIXct(annualRainfall_rcp85$FullDate, format = "%Y-%m-%d", tz = "UTC")
annualRainfall_rcp45 <- annualRainfall_rcp45[,-1]
annualRainfall_rcp85 <- annualRainfall_rcp85[,-1]
#annualRainfall_rcp45 <- annualRainfall_rcp45[which(annualRainfall_rcp45$Model != "HadGEM2_CC" & annualRainfall_rcp45$Model != "HadGEM2_ES"),]
#annualRainfall_rcp85 <- annualRainfall_rcp85[which(annualRainfall_rcp85$Model != "HadGEM2_CC" & annualRainfall_rcp85$Model != "HadGEM2_ES"),]

##Adding historical precip to annual rainfall frame
historicalPrecip <- data.frame(read.csv("D:/Manuscript1/RCode/Valdivia_Pichoy.csv"))
##Have to fix the dates because they are in a goofy format
historicalPrecip$Date <- as.POSIXct(historicalPrecip$Date, format = "%m/%d/%y", tz = "UTC")
historicalPrecip$Date[1:6940] <-  historicalPrecip$Date[1:6940] - dyears(100)
colnames(historicalPrecip)[1] <- "FullDate"
colnames(historicalPrecip)[5] <- "Precip_mm_mean"
##Setting up models for comparison with historical data on which they were trained
historicalPrecip$Model <- "Observed"

historicalPrecip$Precip_mm_mean[is.na(historicalPrecip$Precip_mm_mean)] <- 0

yearsForModels <- unique(format(historicalPrecip$FullDate, "%Y"))
for(i in 1:length(yearsForModels)){
  tempFrame <- data.frame(matrix(nrow = 1, ncol = 4, NA))
  colnames(tempFrame) <- c("FullDate", "Model", "Warming", "precip_annual")
  
  tempFrame$FullDate <- as.POSIXct(paste(yearsForModels[i], "-01-01", sep = ""), format = "%Y-%m-%d", tz = "UTC")
  tempFrame$Model <- "Observed"
  tempFrame$Warming <- ""
  tempFrame$precip_annual <- sum(historicalPrecip$Precip_mm_mean[which(format(historicalPrecip$FullDate, "%Y") == yearsForModels[i])])
  annualRainfall_rcp45 <- rbind(annualRainfall_rcp45, tempFrame)
  annualRainfall_rcp85 <- rbind(annualRainfall_rcp85, tempFrame)
}

i = 1
j = 1


##Calculating ensemble average and adding it to annual rainfall files
yearsForModels <- as.character(seq(1950,2100, 1))
i = 100
j = 1
k = 1
m = 1


for(j in 1:length(emissions)){
  for(i in 1:length(yearsForModels)){
    
    tempFrame <- data.frame(matrix(nrow = 1, ncol = 4, NA))
    colnames(tempFrame) <- c("FullDate", "Model", "Warming", "precip_annual")
    
    tempFrame$FullDate <- as.POSIXct(paste(yearsForModels[i], "-01-01", sep = ""), format = "%Y-%m-%d", tz = "UTC")
    tempFrame$Model <- "Ensemble average"
    eval(parse(text = paste("tempFrame$Warming <- warming[j]", sep = "")))
    eval(parse(text = paste("tempFrame$precip_annual <- mean(annualRainfall_", emissions[j], "$precip_annual[which(format(annualRainfall_", emissions[j], "$FullDate, \"%Y\") == yearsForModels[i] & annualRainfall_", emissions[j], "$Model != \"Observed\")], na.rm = TRUE)", sep = "")))
    eval(parse(text = paste("annualRainfall_", emissions[j], " <- rbind(annualRainfall_", emissions[j], ", tempFrame)", sep = "")))
  }
}



###Writing these dataframes to csvs for ease of later access
#write.csv(annualRainfall_rcp45, "annualRainfall_rcp45.csv")
#write.csv(annualRainfall_rcp85, "annualRainfall_rcp85.csv")


##Calculating mean and standard error of precipitation for the historical period for the historical period (1950-2015), the historical period and the time since then until this study (1950-2021), the future period (2016-2080), and the whole period (1950-2080)
se <- function(x) sqrt(var(x) / length(x))

mean(annualRainfall_rcp45$precip_annual[which(annualRainfall_rcp45$Model == "Observed" & format(annualRainfall_rcp45$FullDate, "%Y") >= "1986" & format(annualRainfall_rcp45$FullDate, "%Y") < "2016")])
se(annualRainfall_rcp45$precip_annual[which(annualRainfall_rcp45$Model == "Observed" & format(annualRainfall_rcp45$FullDate, "%Y") >= "1986" & format(annualRainfall_rcp45$FullDate, "%Y") < "2016")])
mean(annualRainfall_rcp45$precip_annual[which(annualRainfall_rcp45$Model == "Ensemble average" & format(annualRainfall_rcp45$FullDate, "%Y") >= "2051" & format(annualRainfall_rcp45$FullDate, "%Y") < "2081")])
se(annualRainfall_rcp45$precip_annual[which(annualRainfall_rcp45$Model == "Ensemble average" & format(annualRainfall_rcp45$FullDate, "%Y") >= "2051" & format(annualRainfall_rcp45$FullDate, "%Y") < "2081")])
mean(annualRainfall_rcp85$precip_annual[which(annualRainfall_rcp85$Model == "Ensemble average" & format(annualRainfall_rcp85$FullDate, "%Y") >= "2051" & format(annualRainfall_rcp85$FullDate, "%Y") < "2081")])
se(annualRainfall_rcp85$precip_annual[which(annualRainfall_rcp85$Model == "Ensemble average" & format(annualRainfall_rcp85$FullDate, "%Y") >= "2051" & format(annualRainfall_rcp85$FullDate, "%Y") < "2081")])

rainfallObserved <- annualRainfall_rcp45[which(annualRainfall_rcp45$Model == "Observed"),]


###binding additional historical annual rainfall amounts to the observed frame in rcp 4.5 only
additionalPrecipitationRecords <- data.frame(matrix(nrow = 6, ncol = 4, NA))
colnames(additionalPrecipitationRecords) <- c("FullDate", "Model", "Warming", "precip_annual")
additionalPrecipitationRecords$FullDate <- c("2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")
additionalPrecipitationRecords$FullDate <- as.POSIXct(additionalPrecipitationRecords$FullDate, format = "%Y-%m-%d", tz = "UTC")
additionalPrecipitationRecords$Model <- "Observed"
additionalPrecipitationRecords$Warming <- ""
additionalPrecipitationRecords$precip_annual <- c(1271.8, 1705.9, 1569.6, 1071.3, 1400.9, 949.0)

annualRainfall_rcp45 <- rbind(annualRainfall_rcp45, additionalPrecipitationRecords)


###Setting up data frames to put all the r squareds and p values into


models_underscore <- c("ACCESS1_0", "ACCESS1_3", "bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM", "CanESM2", "CCSM4", "CMCC_CM", "CNRM_CM5", "CSIRO_MK3_6_0", "GFDL_ESM2G", "GFDL_ESM2M", "HadGEM2_CC", "HadGEM2_ES", "inmcm4", "IPSL_CM5A_LR", "IPSL_CM5A_MR", "MIROC5", "MIROC_ESM", "MIROC_ESM_CHEM", "MPI_ESM_LR", "MPI_ESM_MR", "NorESM1_M")
modelsForSummary <- c(models_underscore, "Ensemble average")


trendDataFrame_rcp45 <- data.frame(matrix(nrow = 8, ncol = length(modelsForSummary), NA))
trendDataFrame_rcp85 <- data.frame(matrix(nrow = 8, ncol = length(modelsForSummary), NA))
colnames(trendDataFrame_rcp45) <- c(models, "Ensemble average")
colnames(trendDataFrame_rcp85) <- c(models, "Ensemble average")

rownames(trendDataFrame_rcp45) <- c("HP R-squared", "HP p-value", "HPP R-squared", "HPP p-value", "FP R-squared", "FP p-value", "WP R-squared", "WP p-value")
rownames(trendDataFrame_rcp85) <- c("HP R-squared", "HP p-value", "HPP R-squared", "HPP p-value", "FP R-squared", "FP p-value", "WP R-squared", "WP p-value")

historicalPeriod <- c(as.POSIXct("1969-01-01", format = "%Y-%m-%d", tz = "UTC"), as.POSIXct("2015-01-01", format = "%Y-%m-%d", tz = "UTC"))
historicalPeriodPlus <- c(as.POSIXct("1969-01-01", format = "%Y-%m-%d", tz = "UTC"), as.POSIXct("2021-01-01", format = "%Y-%m-%d", tz = "UTC"))
futurePeriod <- c(as.POSIXct("2016-01-01", format = "%Y-%m-%d", tz = "UTC"), as.POSIXct("2080-01-01", format = "%Y-%m-%d", tz = "UTC"))
wholePeriod <- c(as.POSIXct("1969-01-01", format = "%Y-%m-%d", tz = "UTC"), as.POSIXct("2080-01-01", format = "%Y-%m-%d", tz = "UTC"))

emissions <- c("rcp45", "rcp85")

modelsForSummary[j]
i = 1
j = 13
for(i in 1:length(emissions)){
  for(j in 1:length(modelsForSummary)){
    eval(parse(text = paste("historicalPeriodData <- annualRainfall_", emissions[i], "[which(annualRainfall_", emissions[i], "$FullDate >= historicalPeriod[1] & annualRainfall_", emissions[i], "$FullDate <= historicalPeriod[2] & annualRainfall_", emissions[i], "$Model == modelsForSummary[j]),]", sep = "")))
    lm_historicalPeriod <- lm(data = historicalPeriodData, precip_annual ~ FullDate)
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[1,j] <- summary(lm_historicalPeriod)$r.squared", sep = "")))
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[2,j] <- glance(lm_historicalPeriod)$p.value", sep = "")))
    
    eval(parse(text = paste("historicalPeriodPlusData <- annualRainfall_", emissions[i], "[which(annualRainfall_", emissions[i], "$FullDate >= historicalPeriodPlus[1] & annualRainfall_", emissions[i], "$FullDate <= historicalPeriodPlus[2] & annualRainfall_", emissions[i], "$Model == modelsForSummary[j]),]", sep = "")))
    lm_historicalPeriodPlus <- lm(data = historicalPeriodPlusData, precip_annual ~ FullDate)
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[3,j] <- summary(lm_historicalPeriodPlus)$r.squared", sep = "")))
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[4,j] <- glance(lm_historicalPeriodPlus)$p.value", sep = "")))
    
    eval(parse(text = paste("futurePeriodData <- annualRainfall_", emissions[i], "[which(annualRainfall_", emissions[i], "$FullDate >= futurePeriod[1] & annualRainfall_", emissions[i], "$FullDate <= futurePeriod[2] & annualRainfall_", emissions[i], "$Model == modelsForSummary[j]),]", sep = "")))
    lm_futurePeriod <- lm(data = futurePeriodData, precip_annual ~ FullDate)
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[5,j] <- summary(lm_futurePeriod)$r.squared", sep = "")))
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[6,j] <- glance(lm_futurePeriod)$p.value", sep = "")))
    
    eval(parse(text = paste("wholePeriodData <- annualRainfall_", emissions[i], "[which(annualRainfall_", emissions[i], "$FullDate >= wholePeriod[1] & annualRainfall_", emissions[i], "$FullDate <= wholePeriod[2] & annualRainfall_", emissions[i], "$Model == modelsForSummary[j]),]", sep = "")))
    lm_wholePeriod <- lm(data = wholePeriodData, precip_annual ~ FullDate)
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[7,j] <- summary(lm_wholePeriod)$r.squared", sep = "")))
    eval(parse(text = paste("trendDataFrame_", emissions[i], "[8,j] <- glance(lm_wholePeriod)$p.value", sep = "")))
  }
}


###Writing these dataframes to csvs for ease of later access
write.csv(trendDataFrame_rcp45, "trendDataFrame_rcp45.csv")
write.csv(trendDataFrame_rcp85, "trendDataFrame_rcp85.csv")


##Preparing rainfall data for plotting
###Colors for plot, and other plot characteristics
makoColors <- viridis::viridis(length(unique(annualRainfall_rcp45$Model)) - 2)
additionalColors <- viridis::mako(5)
additionalColors <- c(additionalColors[2], additionalColors[1])

makoColors_black <- c(makoColors, additionalColors)

scaleSize <- c(rep(0.5, 23), 1.5, 1.5)

###Setting 0s to NAs so they do not plot
annualRainfall_rcp45$precip_annual[which(annualRainfall_rcp45$precip_annual == 0)] <- NA
annualRainfall_rcp85$precip_annual[which(annualRainfall_rcp85$precip_annual == 0)] <- NA


###Renaming $Model to be consistent with official naming conventions
for(i in 1:length(annualRainfall_rcp45$FullDate)){
  for(j in 1:length(models)){
    if(annualRainfall_rcp45$Model[i] == models_underscore[j]){
      annualRainfall_rcp45$Model[i] <- models[j]
    }
  }
}

for(i in 1:length(annualRainfall_rcp85$FullDate)){
  for(j in 1:length(models)){
    if(annualRainfall_rcp85$Model[i] == models_underscore[j]){
      annualRainfall_rcp85$Model[i] <- models[j]
    }
  }
}

annualRainfall_rcp45$Model <- factor(annualRainfall_rcp45$Model, levels = c(models, "Ensemble average", "Observed"))
annualRainfall_rcp85$Model <- factor(annualRainfall_rcp85$Model, levels = c(models, "Ensemble average", "Observed"))



###Removing all observational data before 1969 because of inconsistent monitoring gauge coverage
annualRainfall_rcp45 <- annualRainfall_rcp45[-which(format(annualRainfall_rcp45$FullDate, "%Y") < "1969" | format(annualRainfall_rcp45$FullDate, "%Y") > "2080"),]
annualRainfall_rcp85 <- annualRainfall_rcp85[-which(format(annualRainfall_rcp85$FullDate, "%Y") < "1969" | format(annualRainfall_rcp85$FullDate, "%Y") > "2080"),]

###These two have legends, so I can cut it and put it on the figure for it
g_annualRainfall_rcp45 <- ggplot(annualRainfall_rcp45, aes(x = FullDate, y = precip_annual, group = Model)) +
  geom_line(aes(color = Model, size = Model)) + 
  theme(legend.position = "") +
  scale_x_continuous(limits = c(as.POSIXct("1955-01-01", format = "%Y-%m-%d"), as.POSIXct("2085-01-01", format = "%Y-%m-%d")), breaks = c(as.POSIXct("1960-01-01", format = "%Y-%m-%d"), as.POSIXct("1980-01-01", format = "%Y-%m-%d"), as.POSIXct("2000-01-01", format = "%Y-%m-%d"), as.POSIXct("2020-01-01", format = "%Y-%m-%d"), as.POSIXct("2040-01-01", format = "%Y-%m-%d"), as.POSIXct("2060-01-01", format = "%Y-%m-%d"), as.POSIXct("2080-01-01", format = "%Y-%m-%d")), labels = c("1960", "1980", "2000", "2020", "2040", "2060", "2080"), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 3550), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), expand = c(0,0)) + 
  labs(y = "Rainfall (mm)", x = "") +
  theme_classic() +
  geom_vline(xintercept = as.POSIXct("2080-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2021-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  scale_color_manual(values = makoColors_black) +
  scale_fill_manual(values = makoColors_black) +
  scale_size_manual(values = scaleSize) +
  theme(legend.position = "right", text = element_text(family = "serif", size = 20)) 


g_annualRainfall_rcp85 <- ggplot(annualRainfall_rcp85, aes(x = FullDate, y = precip_annual, group = Model)) +
  geom_line(aes(color = Model, size = Model)) + 
  theme(legend.position = "") +
  scale_x_continuous(limits = c(as.POSIXct("1955-01-01", format = "%Y-%m-%d"), as.POSIXct("2085-01-01", format = "%Y-%m-%d")), breaks = c(as.POSIXct("1960-01-01", format = "%Y-%m-%d"), as.POSIXct("1980-01-01", format = "%Y-%m-%d"), as.POSIXct("2000-01-01", format = "%Y-%m-%d"), as.POSIXct("2020-01-01", format = "%Y-%m-%d"), as.POSIXct("2040-01-01", format = "%Y-%m-%d"), as.POSIXct("2060-01-01", format = "%Y-%m-%d"), as.POSIXct("2080-01-01", format = "%Y-%m-%d")), labels = c("1960", "1980", "2000", "2020", "2040", "2060", "2080"), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 3550), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), expand = c(0,0)) + 
  labs(y = "Rainfall (mm)", x = "") +
  theme_classic() +
  geom_vline(xintercept = as.POSIXct("2080-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2021-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  scale_color_manual(values = makoColors_black) +
  scale_fill_manual(values = makoColors_black) +
  scale_size_manual(values = scaleSize) +
  theme(legend.position = "right", text = element_text(family = "serif", size = 20)) 


jpeg("annualRainfall_modeled_wLegend.jpg", width = 11, height = 8.5, units = "in", res = 300)
g_annualRainfall_rcp45
dev.off()

###These two are without legends
g_annualRainfall_rcp45 <- ggplot(annualRainfall_rcp45, aes(x = FullDate, y = precip_annual, group = Model)) +
  geom_line(aes(color = Model, size = Model)) + 
  theme(legend.position = "") +
  scale_x_continuous(limits = c(as.POSIXct("1955-01-01", format = "%Y-%m-%d"), as.POSIXct("2085-01-01", format = "%Y-%m-%d")), breaks = c(as.POSIXct("1960-01-01", format = "%Y-%m-%d"), as.POSIXct("1980-01-01", format = "%Y-%m-%d"), as.POSIXct("2000-01-01", format = "%Y-%m-%d"), as.POSIXct("2020-01-01", format = "%Y-%m-%d"), as.POSIXct("2040-01-01", format = "%Y-%m-%d"), as.POSIXct("2060-01-01", format = "%Y-%m-%d"), as.POSIXct("2080-01-01", format = "%Y-%m-%d")), labels = c("1960", "1980", "2000", "2020", "2040", "2060", "2080"), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 3550), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), expand = c(0,0)) + 
  labs(y = "Rainfall (mm)", x = "") +
  theme_classic() +
  geom_vline(xintercept = as.POSIXct("2080-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2015-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2021-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  scale_color_manual(values = makoColors_black) +
  scale_fill_manual(values = makoColors_black) +
  scale_size_manual(values = scaleSize) +
  theme(legend.position = "", text = element_text(family = "serif", size = 20))



g_annualRainfall_rcp85 <- ggplot(annualRainfall_rcp85, aes(x = FullDate, y = precip_annual, group = Model)) +
  geom_line(aes(color = Model, size = Model)) +
  theme(legend.position = "") +
  scale_x_continuous(limits = c(as.POSIXct("1955-01-01", format = "%Y-%m-%d"), as.POSIXct("2085-01-01", format = "%Y-%m-%d")), breaks = c(as.POSIXct("1960-01-01", format = "%Y-%m-%d"), as.POSIXct("1980-01-01", format = "%Y-%m-%d"), as.POSIXct("2000-01-01", format = "%Y-%m-%d"), as.POSIXct("2020-01-01", format = "%Y-%m-%d"), as.POSIXct("2040-01-01", format = "%Y-%m-%d"), as.POSIXct("2060-01-01", format = "%Y-%m-%d"), as.POSIXct("2080-01-01", format = "%Y-%m-%d")), labels = c("1960", "1980", "2000", "2020", "2040", "2060", "2080"), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 3550), breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500), expand = c(0,0)) + 
  labs(y = "Rainfall (mm)", x = "") +
  theme_classic() +
  geom_vline(xintercept = as.POSIXct("2080-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2015-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  geom_vline(xintercept = as.POSIXct("2021-01-01", format = "%Y-%m-%d"), linetype = "dashed", color = "gray", size = 1) + 
  scale_color_manual(values = makoColors_black) +
  scale_fill_manual(values = makoColors_black) +
  scale_size_manual(values = scaleSize) +
  theme(legend.position = "", text = element_text(family = "serif", size = 20)) 


jpeg("annualRainfall_modeled_noLegend.jpg", width = 11, height = 8.5, units = "in", res = 300)
ggarrange(g_annualRainfall_rcp45, g_annualRainfall_rcp85, nrow = 2)
dev.off()


##Creating ensemble frame for the averages and standard deviations of Tmin, Tmax, and Precip
ensembleAverages_rcp45 <- data.frame(matrix(nrow = 55115, ncol = 9, NA))
colnames(ensembleAverages_rcp45) <- c("FullDate", "T_min", "T_min_sd", "T_max", "T_max_sd", "T_mean", "T_mean_sd", "Precip_mm_mean", "Precip_mm_mean_sd")
ensembleAverages_rcp45$FullDate <- unique(ensembleFrame_rcp45$FullDate)
i = 1
for(i in 1:length(ensembleAverages_rcp45$FullDate)){
  t_min <- ensembleFrame_rcp45$T_min[which(ensembleFrame_rcp45$FullDate == ensembleAverages_rcp45$FullDate[i])]
  ensembleAverages_rcp45$T_min[i] <- as.numeric(sprintf(mean(t_min[!is.na(t_min)]), fmt = '%#.2f'))
  ensembleAverages_rcp45$T_min_sd[i] <- as.numeric(sprintf(sd(t_min[!is.na(t_min)]), fmt = '%#.2f'))
  
  t_max <- ensembleFrame_rcp45$T_max[which(ensembleFrame_rcp45$FullDate == ensembleAverages_rcp45$FullDate[i])]
  ensembleAverages_rcp45$T_max[i] <- as.numeric(sprintf(mean(t_max[!is.na(t_max)]), fmt = '%#.2f'))
  ensembleAverages_rcp45$T_max_sd[i] <- as.numeric(sprintf(sd(t_max[!is.na(t_max)]), fmt = '%#.2f'))
  
  t_mean <- c(ensembleFrame_rcp45$T_min[which(ensembleFrame_rcp45$FullDate == ensembleAverages_rcp45$FullDate[i])], ensembleFrame_rcp45$T_max[which(ensembleFrame_rcp45$FullDate == ensembleAverages_rcp45$FullDate[i])])
  ensembleAverages_rcp45$T_mean[i] <- as.numeric(sprintf(mean(t_mean[!is.na(t_mean)]), fmt = '%#.2f'))
  ensembleAverages_rcp45$T_mean_sd[i] <- as.numeric(sprintf(sd(t_mean[!is.na(t_mean)]), fmt = '%#.2f'))
  
  precip <- ensembleFrame_rcp45$precip_mm[which(ensembleFrame_rcp45$FullDate == ensembleAverages_rcp45$FullDate[i])]
  ensembleAverages_rcp45$Precip_mm_mean[i] <- as.numeric(sprintf(mean(precip[!is.na(precip)]), fmt = '%#.2f'))
  ensembleAverages_rcp45$Precip_mm_mean_sd[i] <- as.numeric(sprintf(sd(precip[!is.na(precip)]), fmt = '%#.2f'))
}

ensembleAverages_rcp85 <- data.frame(matrix(nrow = 55115, ncol = 9, NA))
colnames(ensembleAverages_rcp85) <- c("FullDate", "T_min", "T_min_sd", "T_max", "T_max_sd", "T_mean", "T_mean_sd", "Precip_mm_mean", "Precip_mm_mean_sd")
ensembleAverages_rcp85$FullDate <- unique(ensembleFrame_rcp85$FullDate)
i = 1
for(i in 1:length(ensembleAverages_rcp85$FullDate)){
  t_min <- ensembleFrame_rcp85$T_min[which(ensembleFrame_rcp85$FullDate == ensembleAverages_rcp85$FullDate[i])]
  ensembleAverages_rcp85$T_min[i] <- as.numeric(sprintf(mean(t_min[!is.na(t_min)]), fmt = '%#.2f'))
  ensembleAverages_rcp85$T_min_sd[i] <- as.numeric(sprintf(sd(t_min[!is.na(t_min)]), fmt = '%#.2f'))
  
  t_max <- ensembleFrame_rcp85$T_max[which(ensembleFrame_rcp85$FullDate == ensembleAverages_rcp85$FullDate[i])]
  ensembleAverages_rcp85$T_max[i] <- as.numeric(sprintf(mean(t_max[!is.na(t_max)]), fmt = '%#.2f'))
  ensembleAverages_rcp85$T_max_sd[i] <- as.numeric(sprintf(sd(t_max[!is.na(t_max)]), fmt = '%#.2f'))
  
  t_mean <- c(ensembleFrame_rcp85$T_min[which(ensembleFrame_rcp85$FullDate == ensembleAverages_rcp85$FullDate[i])], ensembleFrame_rcp85$T_max[which(ensembleFrame_rcp85$FullDate == ensembleAverages_rcp85$FullDate[i])])
  ensembleAverages_rcp85$T_mean[i] <- as.numeric(sprintf(mean(t_mean[!is.na(t_mean)]), fmt = '%#.2f'))
  ensembleAverages_rcp85$T_mean_sd[i] <- as.numeric(sprintf(sd(t_mean[!is.na(t_mean)]), fmt = '%#.2f'))
  
  precip <- ensembleFrame_rcp85$precip_mm[which(ensembleFrame_rcp85$FullDate == ensembleAverages_rcp85$FullDate[i])]
  ensembleAverages_rcp85$Precip_mm_mean[i] <- as.numeric(sprintf(mean(precip[!is.na(precip)]), fmt = '%#.2f'))
  ensembleAverages_rcp85$Precip_mm_mean_sd[i] <- as.numeric(sprintf(sd(precip[!is.na(precip)]), fmt = '%#.2f'))
}


#Estimating return period of storms in the historical period, using actual good stats ***This is used in the final version****

historicalPrecip_monthlyExtremes <- data.frame(matrix(nrow = 0, ncol = 4, NA))
colnames(historicalPrecip_monthlyExtremes) <- c("Index", "FullDate", "Precip_mm", "Model")
listOfYears <- unique(format(historicalPrecip$FullDate, "%Y"))
i = 1
j = 1

for(i in 1:length(listOfYears)){
  monthsInYear <- unique(format(historicalPrecip$FullDate[which(format(historicalPrecip$FullDate, "%Y") == listOfYears[i])], "%m"))
  
  for(j in 1:length(monthsInYear)){
    tempRow <- data.frame(matrix(nrow = 1, ncol = 4, NA))
    colnames(tempRow) <- c("Index", "FullDate", "Precip_mm", "Model")
    tempRow[1,2] <- paste(listOfYears[i], monthsInYear[j], "01", sep = "-")
    tempRow[1,3] <- max(historicalPrecip$Precip_mm_mean[which(format(historicalPrecip$FullDate, "%Y") == listOfYears[i] & format(historicalPrecip$FullDate, "%m") == monthsInYear[j])], na.rm = TRUE)
    if(!is.finite(tempRow[1,3])) {tempRow[1,3] <- NA}
    tempRow[4] <- "Observed"
    historicalPrecip_monthlyExtremes <- rbind(historicalPrecip_monthlyExtremes, tempRow)
  }
}

historicalPrecip_monthlyExtremes$FullDate <- as.POSIXct(historicalPrecip_monthlyExtremes$FullDate, format = "%Y-%m-%d", tz = "UTC")
historicalPrecip_monthlyExtremes <- historicalPrecip_monthlyExtremes[,-1]

##Adding data on more recent (2016-2021) extreme monthly precipitation to other historical file (1950-2015), then binding it to historical data
additionalHistoricalPrecip <- read.csv("AdditionalMonthlyPrecipData.csv")
additionalHistoricalPrecip <- additionalHistoricalPrecip[,1:3]
additionalHistoricalPrecip$FullDate <- as.POSIXct(additionalHistoricalPrecip$FullDate, format = "%Y-%m-%d", tz = "UTC")


historicalPrecip_monthlyExtremes <- rbind(historicalPrecip_monthlyExtremes, additionalHistoricalPrecip)

##Figuring out the return periods for the whole historical period (1950-2021)
returnPeriods <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(returnPeriods) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")


yearsInData <- unique(format(historicalPrecip_monthlyExtremes$FullDate, format = "%Y", tz = "UTC"))
i = 1
for(i in 1:(length(yearsInData)-29)){
  movingWindow <- data.frame(matrix(nrow = length(historicalPrecip_monthlyExtremes$FullDate[format(historicalPrecip_monthlyExtremes$FullDate, "%Y") < as.numeric(yearsInData[i]) + 29 & format(historicalPrecip_monthlyExtremes$FullDate, "%Y") >= as.numeric(yearsInData[i])]), ncol = 7))
  colnames(movingWindow) <- c("FullDate", "Precip", "Rank", "Fa", "ReturnPeriod", "ReturnPeriodLog")
  historicalData <- historicalPrecip_monthlyExtremes[which(as.numeric(format(historicalPrecip_monthlyExtremes$FullDate, "%Y")) < (as.numeric(yearsInData[i]) + 29) & format(historicalPrecip_monthlyExtremes$FullDate, "%Y") >= as.numeric(yearsInData[i])),]
  movingWindow$FullDate <- historicalData$FullDate
  movingWindow$Precip <- historicalData$Precip_mm
  movingWindow$Precip[is.na(movingWindow$Precip)] <- 0
  movingWindow_matrix <- as.matrix(movingWindow$Precip)
  fit_movingWindow <- fevd(movingWindow_matrix, type = "GEV")
  rl_movingWindow <- return.level(x = fit_movingWindow, return.period = c(1200), do.ci = TRUE, alpha = 0.05)
  returnPeriod_100 <- unname(rl_movingWindow[2])
  returnPeriod_100_lower <- unname(rl_movingWindow[1])
  returnPeriod_100_upper <- unname(rl_movingWindow[3])
  
  periodFrame <- data.frame(as.POSIXct(paste(as.numeric(yearsInData[i]) + 29, "-01-01", sep = ""), format = "%Y-%m-%d", tz = "UTC"), "Observed", "", returnPeriod_100_lower, returnPeriod_100, returnPeriod_100_upper)
  colnames(periodFrame) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")
  returnPeriods <- rbind(returnPeriods, periodFrame)
}

write.csv(returnPeriods, "returnPeriods_observedOnly.csv")

returnPeriods <- read.csv("returnPeriods_observedOnly.csv")
returnPeriods$FullDate <- as.POSIXct(returnPeriods$FullDate, format = "%Y-%m-%d", tz = "UTC")
returnPeriods <- returnPeriods[-1]

#Doing the same as above but for the model data
ensembleFrame_rcp45 <- read.csv("ensembleFrame_rcp45_wModels.csv")
ensembleFrame_rcp45$FullDate <- as.POSIXct(ensembleFrame_rcp45$FullDate, format = "%Y-%m-%d", tz = "UTC")
ensembleFrame_rcp45 <- ensembleFrame_rcp45[-1]

ensembleFrame_rcp85 <- read.csv("ensembleFrame_rcp85_wModels.csv")
ensembleFrame_rcp85$FullDate <- as.POSIXct(ensembleFrame_rcp85$FullDate, format = "%Y-%m-%d", tz = "UTC")
ensembleFrame_rcp85 <- ensembleFrame_rcp85[-1]


i = 1
j = 1
k = 1
m = 1
n = 1
modelPrecip_monthlyExtremes_rcp45 <- data.frame(matrix(nrow = 0, ncol = 3, NA))
colnames(modelPrecip_monthlyExtremes_rcp45) <- c("FullDate", "Precip_mm", "Model")
modelPrecip_monthlyExtremes_rcp85 <- data.frame(matrix(nrow = 0, ncol = 3, NA))
colnames(modelPrecip_monthlyExtremes_rcp85) <- c("FullDate", "Precip_mm", "Model")



for(m in 1:length(emissions)){
  for(k in 1:length(models_underscore)){
    if(models_underscore[k] == "CCSM4"){n <- 2}else{n <- 1}
    eval(parse(text = paste("listOfYears <- unique(format(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$FullDate, \"%Y\"))", sep = "")))
    
    for(i in 1:length(listOfYears)){
      eval(parse(text = paste("monthsInYear <- unique(format(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$FullDate[which(format(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$FullDate, \"%Y\") == listOfYears[i])], \"%m\"))", sep = "")))
      
      for(j in 1:length(monthsInYear)){
        tempRow <- data.frame(matrix(nrow = 1, ncol = 3, NA))
        colnames(tempRow) <- c("FullDate", "Precip_mm", "Model")
        tempRow[1,1] <- paste(listOfYears[i], monthsInYear[j], "01", sep = "-")
        eval(parse(text = paste("tempRow[1,2] <- max(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$precip_mm[which(format(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$FullDate, \"%Y\") == listOfYears[i] & format(", models_underscore[k], "_", subModels[n], "_", emissions[m], "_pr$FullDate, \"%m\") == monthsInYear[j])], na.rm = TRUE)", sep = "")))
        if(!is.finite(tempRow[1,2])) {tempRow[1,3] <- NA}
        tempRow[1,3] <- models_underscore[k]
        eval(parse(text = paste("modelPrecip_monthlyExtremes_", emissions[m], " <- rbind(modelPrecip_monthlyExtremes_", emissions[m], ", tempRow)", sep = "")))
      }
    }
  }
}

modelPrecip_monthlyExtremes_rcp45$FullDate <- as.POSIXct(modelPrecip_monthlyExtremes_rcp45$FullDate, format = "%Y-%m-%d", tz = "UTC")
modelPrecip_monthlyExtremes_rcp85$FullDate <- as.POSIXct(modelPrecip_monthlyExtremes_rcp85$FullDate, format = "%Y-%m-%d", tz = "UTC")
#write.csv(modelPrecip_monthlyExtremes_rcp45, "modelPrecip_monthlyExtremes_rcp45.csv")
#write.csv(modelPrecip_monthlyExtremes_rcp85, "modelPrecip_monthlyExtremes_rcp85.csv")


modelPrecip_monthlyExtremes_rcp45 <- read.csv("modelPrecip_monthlyExtremes_rcp45.csv")
modelPrecip_monthlyExtremes_rcp45 <- modelPrecip_monthlyExtremes_rcp45[,-1]
modelPrecip_monthlyExtremes_rcp45$FullDate <- as.POSIXct(modelPrecip_monthlyExtremes_rcp45$FullDate, format = "%Y-%m-%d", tz = "UTC")
modelPrecip_monthlyExtremes_rcp85 <- read.csv("modelPrecip_monthlyExtremes_rcp85.csv")
modelPrecip_monthlyExtremes_rcp85 <- modelPrecip_monthlyExtremes_rcp85[,-1]
modelPrecip_monthlyExtremes_rcp85$FullDate <- as.POSIXct(modelPrecip_monthlyExtremes_rcp85$FullDate, format = "%Y-%m-%d", tz = "UTC")

models_underscore <- c("ACCESS1_0", "ACCESS1_3", "bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM", "CanESM2", "CCSM4", "CMCC_CM", "CNRM_CM5", "CSIRO_MK3_6_0", "GFDL_ESM2G", "GFDL_ESM2M", "HadGEM2_CC", "HadGEM2_ES", "inmcm4", "IPSL_CM5A_LR", "IPSL_CM5A_MR", "MIROC5", "MIROC_ESM", "MIROC_ESM_CHEM", "MPI_ESM_LR", "MPI_ESM_MR", "NorESM1_M")
warming <- c("RCP 4.5", "RCP 8.5")
j = 1
i = 1
k = 1
returnPeriods_rcp45 <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(returnPeriods_rcp45) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")
returnPeriods_rcp85 <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(returnPeriods_rcp85) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")

fit_movingWindow
for(m in 1:length(emissions)){
  for(k in 1:length(models_underscore)){
    
    eval(parse(text = paste("yearsInData <- unique(format(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate[which(modelPrecip_monthlyExtremes_", emissions[m], "$Model == models_underscore[k])], format = \"%Y\", tz = \"UTC\"))", sep = "")))
    for(i in 1:(length(yearsInData)-29)){
      eval(parse(text = paste("movingWindow <- data.frame(matrix(nrow = length(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate[format(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate, \"%Y\") < as.numeric(yearsInData[i]) + 29 & format(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate, \"%Y\") >= as.numeric(yearsInData[i]) & modelPrecip_monthlyExtremes_", emissions[m], "$Model == models_underscore[k]]), ncol = 7))", sep = "")))
      colnames(movingWindow) <- c("FullDate", "Precip", "Rank", "Fa", "ReturnPeriod", "ReturnPeriodLog")
      eval(parse(text = paste("historicalData <- modelPrecip_monthlyExtremes_", emissions[m], "[which(as.numeric(format(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate, \"%Y\")) < (as.numeric(yearsInData[i]) + 29) & format(modelPrecip_monthlyExtremes_", emissions[m], "$FullDate, \"%Y\") >= as.numeric(yearsInData[i]) & modelPrecip_monthlyExtremes_", emissions[m], "$Model == models_underscore[k]),]", sep = "")))
      movingWindow$FullDate <- historicalData$FullDate
      movingWindow$Precip <- historicalData$Precip_mm
      movingWindow$Precip[is.na(movingWindow$Precip)] <- 0
      movingWindow_matrix <- as.matrix(movingWindow$Precip)
      movingWindow_matrix <- movingWindow_matrix[is.finite(movingWindow_matrix)]
      
      fit_movingWindow <- fevd(movingWindow_matrix, type = "GEV")
      rl_movingWindow <- return.level(x = fit_movingWindow, return.period = c(1200), do.ci = TRUE, alpha = 0.05)
      returnPeriod_100 <- unname(rl_movingWindow[2])
      returnPeriod_100_lower <- unname(rl_movingWindow[1])
      returnPeriod_100_upper <- unname(rl_movingWindow[3])
      
      periodFrame_models <- data.frame(as.POSIXct(paste(as.numeric(yearsInData[i]) + 29, "-01-01", sep = ""), format = "%Y-%m-%d", tz = "UTC"), models_underscore[k], warming[m], returnPeriod_100_lower, returnPeriod_100, returnPeriod_100_upper)
      colnames(periodFrame_models) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")
      eval(parse(text = paste("returnPeriods_", emissions[m], " <- rbind(returnPeriods_", emissions[m], ", periodFrame_models)", sep = "")))
    }
  }
}

##Writing return periods frame
write.csv(returnPeriods_rcp45, "returnPeriods_rcp45.csv")
write.csv(returnPeriods_rcp85, "returnPeriods_rcp85.csv")


i = 1
j = 1
k = 1
m = 1

for(m in 1:length(emissions)){
  eval(parse(text = paste("yearsInData <- unique(format(returnPeriods_", emissions[m], "$FullDate, format = \"%Y\", tz = \"UTC\"))", sep = "")))
  yearsInData <- sort(yearsInData)
  eval(parse(text = paste("returnPeriods_ensembleMean <- data.frame(matrix(nrow = length(yearsInData), ncol = 6))", sep = "")))
  colnames(returnPeriods_ensembleMean) <- c("FullDate", "Model", "Warming", "Lower", "Mean", "Upper")
  
  for(i in 1:length(yearsInData)){
    returnPeriods_ensembleMean$FullDate[i] <- paste(yearsInData[i], "-01-01", sep = "")
    returnPeriods_ensembleMean$Model[i] <- "Ensemble average"
    returnPeriods_ensembleMean$Warming[i] <- warming[m]
    eval(parse(text = paste("returnPeriods_ensembleMean$Lower[i] <- mean(returnPeriods_", emissions[m], "$Lower[which(format(returnPeriods_", emissions[m], "$FullDate, format = \"%Y\") == yearsInData[i])])", sep = "")))
    eval(parse(text = paste("returnPeriods_ensembleMean$Mean[i] <- mean(returnPeriods_", emissions[m], "$Mean[which(format(returnPeriods_", emissions[m], "$FullDate, format = \"%Y\") == yearsInData[i])])", sep = "")))
    eval(parse(text = paste("returnPeriods_ensembleMean$Upper[i] <- mean(returnPeriods_", emissions[m], "$Upper[which(format(returnPeriods_", emissions[m], "$FullDate, format = \"%Y\") == yearsInData[i])])", sep = "")))
  }
  eval(parse(text = paste("returnPeriods_", emissions[m], " <- rbind(returnPeriods_", emissions[m], ", returnPeriods_ensembleMean)", sep = "")))
  
}



returnPeriods_rcp45 <- rbind(returnPeriods_rcp45, returnPeriods[which(format(returnPeriods$FullDate, "%Y") > "2015"),])
returnPeriods_rcp45$Warming[which(returnPeriods_rcp45$Model == "Observed")] <- "RCP 4.5"

returnPeriods_rcp85 <- rbind(returnPeriods_rcp85, returnPeriods[which(format(returnPeriods$FullDate, "%Y") > "2015"),])
returnPeriods_rcp85$Warming[which(returnPeriods_rcp85$Model == "Observed")] <- "RCP 8.5"

write.csv(returnPeriods_rcp45, "returnPeriods_rcp45.csv")
write.csv(returnPeriods_rcp85, "returnPeriods_rcp85.csv")

