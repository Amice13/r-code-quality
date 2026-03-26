require(tidyr)
require(dplyr)
require(readr)
require(Rmisc)
require(MASS)
require(car)
require(MuMIn)
require(broom)

rm(list=ls())

#This is the script used to run statistical analysis for the manuscript titled "Fin-fin interactions during locomotion in a simplified biomimetic fish model"
    #Note that the variable "powVel" refers to the propulsive economy

#Load in data
finalData <- read.csv(DataForStatsAnalysis.csv)

#Create data set for fin phasing analysis (excluding the control foil)
noCtrl <- dplyr::filter(finalData,Position=="farr"|Position=="near")

#Subset all data by stiffness
coralData <- dplyr::filter(finalData, Material=="corl")
pinkData <- dplyr::filter(finalData, Material=="pink")


#Now get a data set for the analysis that excludes the control foil

#Standardize the data, add Frequency squared
scaledIndep <- noCtrl
scaledIndep[,c(3,6,7,8)] <- scale(noCtrl[,c(3,6,7,8)])
scaledIndep$phase2 <- scaledIndep$angPhaseTop^2
scaledIndep$freq2 <- scaledIndep$Frequency^2





  #Check residual plots for base models to determine heteroskedasticity
plot(lm(Fx~Frequency+Position+Material,data=finalData),which=1)
plot(lm(Fx~Frequency+Position+Material,data=finalData),which=2)

plot(lm(Fy~Frequency+Position+Material,data=finalData),which=1)
plot(lm(Fy~Frequency+Position+Material,data=finalData),which=2)

plot(lm(SPS~Frequency+Position+Material,data=finalData),which=1)
plot(lm(SPS~Frequency+Position+Material,data=finalData),which=2)

plot(lm(power~Frequency+Position+Material,data=finalData),which=1)
plot(lm(power~Frequency+Position+Material,data=finalData),which=2)

plot(lm(powvel~Frequency+Position+Material,data=finalData),which=1)
plot(lm(powvel~Frequency+Position+Material,data=finalData),which=2)




  #Check models to see if quadratic term reduces heteroskedasticity (in models that had this problem initially)
finalData$Position <- factor(finalData$Position,levels=c('ctrl','farr','near'))

lmNorm1 <- lm(Fx~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm1)
  plot(lmNorm1,which=1)
  plot(lmNorm1,which=2)

lmNorm2 <- lm(Fy~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm2)
  plot(lmNorm2,which=1)
  plot(lmNorm2,which=2)
  
lmNorm3 <- lm(SPS~Frequency+Position+Material,data=finalData) 
  summary(lmNorm3)
  plot(lmNorm3,which=1)
  plot(lmNorm3,which=2)
  
lmNorm4 <- lm(power~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm4)
  plot(lmNorm4,which=1)
  plot(lmNorm4,which=2)
  
lmNorm5 <- lm(powvel~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm5)
  plot(lmNorm5,which=1)
  plot(lmNorm5,which=2)

  
  #Reorder the levels of "Position" so that I can get all pairwise comparisons
finalData$Position <- factor(finalData$Position,levels=c('farr','near','ctrl'))
lmNorm1.2 <- lm(Fx~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm1.2)

lmNorm2.2 <- lm(Fy~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm2.2)

lmNorm3.2 <- lm(SPS~Frequency+Position+Material,data=finalData)
  summary(lmNorm3.2)

lmNorm4.2 <- lm(power~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm4.2)

lmNorm5.2 <- lm(powvel~stats::poly(Frequency,2)+Position+Material,data=finalData) #Squared term is significant
  summary(lmNorm5.2)



  
  
  # AICc
  #evaluate by comparing plots of each model and R^2 values. I want high R^2 and low heteroskedasticity
  
  #Note that ONLY the independent variables are standardized. So the interpretation is that the dependent variable changes by 1 *unit* for each standard deviation change in the independent variable

  scaledIndep$Position <- factor(scaledIndep$Position,levels=c('near','farr'))
  scaledIndep$Material <- factor(scaledIndep$Material,levels=c('pink','corl'))
  
  #First run AICc to pick models
  
  lmx <- lm(Fx~Frequency+freq2+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  stepAICc(lmx)
  lmx2Z <- lm(Fx~stats::poly(Frequency,2)+Material+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  AICc(lmx2Z)
  summary(lmx2Z)
  
  
  
  lmy <- lm(Fy~Frequency+freq2+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  stepAICc(lmy)
  lmy2Z <- lm(Fy~stats::poly(Frequency,2)+Material+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  AICc(lmy2Z)
  summary(lmy2Z)
  
  
  
  lmz <- lm(SPS~Frequency+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  stepAICc(lmz)
  lmz2Z <- lm(SPS~Frequency+Position+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  AICc(lmz2Z)
  summary(lmz2Z)
  
  
  
  lmzzz <- lm(power~Frequency+freq2+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  stepAICc(lmzzz)
  lmzzz2Z <- lm(power~stats::poly(Frequency,2)+Material+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  AICc(lmzzz2Z)
  summary(lmzzz2Z)
  
  
  
  lmw <- lm(powvel~Frequency+freq2+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  stepAICc(lmw)
  lmw2Z <- lm(powvel~stats::poly(Frequency,2)+Position+Material+cmAmpDor+cmAmpCaud+Position*angPhaseTop+Position*phase2, data=scaledIndep)
  AICc(lmw2Z)
  summary(lmw2Z)
  
  
  #Now find AICc summary statistics
    #This was done for each model manually
  
  lmSPSAIC1 <- glm(SPS~Frequency+angPhaseTop+phase2+Position+Material+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  summary(lmSPSAIC1)
  lmSPSAIC2 <- glm(SPS~Frequency+angPhaseTop+phase2+Position+cmAmpDor+cmAmpCaud+Position:angPhaseTop+Position:phase2, data=scaledIndep)
  summary(lmSPSAIC2)

  AICc(lmSPSAIC1,lmSPSAIC2)
  
  MuMIn::Weights(AICc(lmSPSAIC1,lmSPSAIC2))
  

  df.residual(lmSPSAIC1)
  df.residual(lmSPSAIC2)
  
  
  
  ### Calculate percent effect sizes for changes in phase angle on SPS
      #use data set scaledIndep for true model values, noCtrl to get uncorrected values

  
  pinkNear <- dplyr::filter(scaledIndep,Position=="near"&Material=="pink")
  
  pinkNearFake <- pinkNear[which(pinkNear$SPS==max(pinkNear$SPS)),]
  pinkNearFake[2,] <- pinkNearFake[1,]
  pinkNearFake$angPhaseTop[2] <- min(pinkNear$angPhaseTop)
  pinkNearFake$phase2[2] <- pinkNearFake$angPhaseTop[2]^2
  pnDiff <- predict.lm(lmz2Z,pinkNearFake)[1]-predict.lm(lmz2Z,pinkNearFake)[2]
  pinkNearReal <- dplyr::filter(noCtrl,Position=="near"&Material=="pink")
  pnWoulda <- max(pinkNearReal$SPS)-pnDiff
  pnPerc <- 1-max(pinkNearReal$SPS)/pnWoulda
  
  
  
  pinkFar <- dplyr::filter(scaledIndep,Position=="farr"&Material=="pink")
  
  pinkFarFake <- pinkFar[which(pinkFar$SPS==max(pinkFar$SPS)),]
  pinkFarFake[2,] <- pinkFarFake[1,]
  pinkFarFake$angPhaseTop[2] <- min(pinkFar$angPhaseTop)
  pinkFarFake$phase2[2] <- pinkFarFake$angPhaseTop[2]^2
  pfDiff <- predict.lm(lmz2Z,pinkFarFake)[1]-predict.lm(lmz2Z,pinkFarFake)[2]
  pinkFarReal <- dplyr::filter(noCtrl,Position=="farr"&Material=="pink")
  pfWoulda <- max(pinkFarReal$SPS)-pfDiff
  pfPerc <- 1-max(pinkFarReal$SPS)/pfWoulda
  
  
  
  coralNear <- dplyr::filter(scaledIndep,Position=="near"&Material=="corl")
  
  coralNearFake <- coralNear[which(coralNear$SPS==max(coralNear$SPS)),]
  coralNearFake[2,] <- coralNearFake[1,]
  coralNearFake$angPhaseTop[2] <- min(coralNear$angPhaseTop)
  coralNearFake$phase2[2] <- coralNearFake$angPhaseTop[2]^2
  cnDiff <- predict.lm(lmz2Z,coralNearFake)[1]-predict.lm(lmz2Z,coralNearFake)[2]
  coralNearReal <- dplyr::filter(noCtrl,Position=="near"&Material=="corl")
  cnWoulda <- max(coralNearReal$SPS)-cnDiff
  cnPerc <- 1-max(coralNearReal$SPS)/cnWoulda
  
  
  
  coralFar <- dplyr::filter(scaledIndep,Position=="farr"&Material=="corl")
  
  coralFarFake <- coralFar[which(coralFar$SPS==max(coralFar$SPS)),]
  coralFarFake[2,] <- coralFarFake[1,]
  coralFarFake$angPhaseTop[2] <- min(coralFar$angPhaseTop)
  coralFarFake$phase2[2] <- coralFarFake$angPhaseTop[2]^2
  cfDiff <- predict.lm(lmz2Z,coralFarFake)[1]-predict.lm(lmz2Z,coralFarFake)[2]
  coralFarReal <- dplyr::filter(noCtrl,Position=="farr"&Material=="corl")
  cfWoulda <- max(coralFarReal$SPS)-cfDiff
  cfPerc <- 1-max(coralFarReal$SPS)/cfWoulda
  
  
  c(pnPerc,pfPerc,cnPerc,cfPerc)