#### 12-09_25_overall_analysis.r ####
### Load libraries
library(MASS)
library(foreign)
library(Matching)
library(xtable)
library(snow)
library(stats)
library(arm)
library(car)
library(np)
library(KernSmooth)
library(boot)
library(sandwich)
library(coda)
library(rbounds)
library(gdata)
library(gtools)
library(lme4)
library(sfsmisc)
library(coin)
library(compiler)
library(coxme)
library(beanplot)
library(RItools)


### Define directories
working.dir <- "/Users/Allan/Dropbox/!!Papers/Liberal Peace/12-02-21_ISQ_commentary/DOR_ISQ_2013_Replication/M_Rep"

getwd()
setwd(working.dir)



data1 <- read.dta("pvalues.dta")
data2 <- read.dta("pvaluesbdm.dta")
data3 <- read.dta("pvaluesh10dm.dta")

names(data1)
names(data1)=c("specification",   "specnumber" ,     "n"    ,           "DemCoefficient" , "DemSE"  ,         "CIElCoefficient", "CIElSE",          "Dempvalues",      "CIElpvalues" )
names(data2) <- names(data1)
names(data3) <- names(data1)



data <- rbind(data1, data2, data3)

#Correcting the labeling
data$specification <- gsub("'''", "''", data$specification)

#dropping Mousseau specifications
data <- data[5:length(data[,1]),]

#Calculations for claim: "Controlling for measures of life insurance expenditure, DemocracyLow (or its counterpart) is highly significant (p<0.01) for 68% of the models, and weakly significant (p<0.1) for 89% of them. "


totalDOnlyModels <- sum(grepl("n", data$specification))
totalDOnlyModels

totalCIEmodels <-  data$specification[is.na(data$CIElpvalues)==FALSE ]
length(totalCIEmodels)

totalMImodels <- sum(grepl("M", data$specification) | grepl("L", data$specification))
totalMImodels

#Claim: "Democracy has a peaceful association in all (140) models that could be estimated; in 24 models BothDemocracy10 perfectly predicts peace and thus the coefficient cannot be estimated. Of the 120 estimable specifications controlling for Life Insurance per Capita, the coefficient for Democracy is significant at the p<0.1 level in 107 of them (89%), and is highly significant (p<0.01) in 79 (66%) of them. When only considering estimable models with multiply imputed versions of Life Insurance per Capita, Democracy has a significant peaceful association in 77 out of 80 (96%) of them, and these effects are especially pronounced for BothDemocracy (Fig 2) and BothDemocracy10 (Fig 3)." 
#Claim: "Controlling for measures of life insurance expenditure, DemocracyLow (or its counterpart) is highly significant (p<0.01) for 66% of the models, and weakly significant (p<0.1) for 89% of them. "

length(data$specification[data$DemCoefficient<0 & is.na(data$Dempvalues)==FALSE])
length(data$specification[is.na(data$Dempvalues)==FALSE])

#List of specifications that could not be estimated.
data$specification[is.na(data$Dempvalues)==TRUE]

#List of specifications that could be estimated with CIEl.
data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE]
length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE])

#Dem significant p<0.1 in this many models that include CIEl
length(data$Dempvalues[data$Dempvalues<0.1 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE])
  
length(data$Dempvalues[data$Dempvalues<0.1 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE]) /
  length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE])


#Dem significant p<0.01 in this many models that include CIEl
length(data$Dempvalues[data$Dempvalues<0.01 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE])

length(data$Dempvalues[data$Dempvalues<0.01 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE]) / 
  length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE])



#Dem significantin this many models that include MI versions of CIEl

#Total MI models
data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))]
length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))])

#MI models sig p<0.1
data$specification[data$Dempvalues<0.1 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))]
length(data$specification[data$Dempvalues<0.1 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))])

length(data$specification[data$Dempvalues<0.1 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))]) / 
  length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))])

#MI models sig p<0.01
data$specification[data$Dempvalues<0.01 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))]
length(data$specification[data$Dempvalues<0.01 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))])

length(data$specification[data$Dempvalues<0.01 & is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))]) / 
  length(data$specification[is.na(data$Dempvalues)==FALSE & is.na(data$CIElpvalues)==FALSE & (grepl("M", data$specification) | grepl("L", data$specification))])
