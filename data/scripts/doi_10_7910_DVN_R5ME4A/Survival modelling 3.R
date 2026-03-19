#######################################################
# Title: Updated survival modelling, incorporating
# body condition from all seasons to overwinter 
# survival
# Name: Julius G. Bright Ross
# Date: Mar 23, 2020
# Last updated: July 11, 2020
# Description: Modelling survival from one year to
# the next as a function of body condition, weather,
# the interaction between the two, and density;
# version 3 incorporates male testes condition and 
# autumn reproductive status in females
#######################################################

## Load in data and required packages ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("Condition modelling output.RData")

survDict <- list()

library(mgcv)        #For building GAM/GAMM models
library(tidyverse)   #For many reasons
library(corrplot)    #To examine correlations between covariates
library(standardize) #To standardize covariates
library(sjPlot)      #To visualise relationships
library(cowplot)     #To build multi-panel figures
library(emmeans)     #For post-hoc comparisons

# Useful functions #

inv_logit <- function(data) {
  return(exp(data)/(1+exp(data)))
}

reverseStandardize <- function(variable, toReverse) {
  sdVar    <- sd(variable)
  meanVar  <- mean(variable)
  
  reversed <- toReverse*sdVar + meanVar
  
  return(reversed)
}

pStore <- function(pValue) {
  if (pValue < 0.001) {
    return("< 0.001")
  } else if (0.001 <= pValue & pValue <= 0.099) {
    return(paste("=", format(round(pValue, 3), nsmall = 3)))
  } else {
    return(paste("=", format(round(pValue, 2), nsmall = 2)))
  }
}

asteriskMarker <- function(coefficient, pValue) {
  
  coefficient <- round(coefficient, dict$rounding + 1) #Because we have some pretty small values
  
  if (pValue < 0.001) {
    coefReport <- paste(coefficient, "***", sep = "")
  } else if (0.001 <= pValue & pValue < 0.01) {
    coefReport <- paste(coefficient, "**", sep = "")
  } else if (0.01 <= pValue & pValue < 0.05) {
    coefReport <- paste(coefficient, "*", sep = "")
  } else {
    coefReport <- as.character(coefficient)
  }
  
  return(coefReport)
}

thresholdVariation <- function(model, modelData,
                               otherTerm, minChange, maxChange) {
  constructionSet <- modelData %>% select(-c("BCIResiduals", otherTerm))
  numericTracker  <- rep(NA, ncol(constructionSet))
  
  for (i in 1:ncol(constructionSet)) {
    numericTracker[i] <- is.numeric(constructionSet[,i])
    
    if (numericTracker[i] == TRUE) {
      constructionSet[,i] <- mean(constructionSet[,i])
    } else {
      constructionSet[,i] <- constructionSet[1,i]
    }
  }
  
  predictionSetMax <- constructionSet %>% 
    cbind(modelData %>% 
            select(c("BCIResiduals", otherTerm))) 
  
  predictionSetMax[,otherTerm] <- max(predictionSetMax[,otherTerm])
  
  predictionSetMin <- constructionSet %>% 
    cbind(modelData %>% 
            select(c("BCIResiduals", otherTerm)))
  
  predictionSetMin[,otherTerm] <- min(predictionSetMax[,otherTerm])
  
  predictionSetMax <- predictionSetMax %>%
    subset(minChange <= BCIResiduals & BCIResiduals <= maxChange)
  
  predictionSetMin <- predictionSetMin %>%
    subset(minChange <= BCIResiduals & BCIResiduals <= maxChange)
  
  predictionsMax <- predict(model, newdata = predictionSetMax, type = "response", se.fit = TRUE)
  predictionsMin <- predict(model, newdata = predictionSetMin, type = "response", se.fit = TRUE)
  
  predictionsMax <- inv_logit(predictionsMax$fit)
  predictionsMin <- inv_logit(predictionsMin$fit)
  
  changeMax <- max(predictionsMax) - min(predictionsMax)
  changeMin <- max(predictionsMin) - min(predictionsMin)
  
  return(c(changeMax, changeMin))
  
}

ageBin <- function(Ages, standardized = FALSE, originalAges) {
  
  if (standardized == TRUE) {
    
    originalMean <- mean(originalAges)
    originalSD   <- sd(originalAges)
    
    Ages <- Ages*originalSD + originalMean
    
  } 
  
  lowBin  <- which(Ages < ageBins[2])
  
  highBin <- which(ageBins[2] <= Ages)
  
  Ages[lowBin]  <- ageBins[1]
  Ages[highBin] <- ageBins[2] 
  
  if (standardized == TRUE) {
    
    Ages <- (Ages - originalMean)/originalSD
    
  }
  
  return(Ages)
  
}

allButAverager <- function(data, terms, defaults) {
  otherSet <- select(data, -terms)
  termSet  <- select(data, terms)
  
  for (i in 1:ncol(otherSet)) {
    if (is.numeric(otherSet[,i])) {
      otherSet[,i] <- mean(otherSet[,i])
    } else if (names(otherSet)[i] %in% names(defaults)) {
      otherSet[,i] <- defaults[names(otherSet)[i]]
    } else {
      otherSet[,i] <- otherSet[1,i]
    }
  }
  
  fullSet <- cbind(otherSet, termSet)
  return(fullSet)
}

### Combining all residuals into one modelling dataframe ###

allCompleteCondition <- rbind(springCompleteCondition,
                              summerCompleteCondition, 
                              autumnCompleteCondition) %>%
  mutate(Season = factor(Season))

### Getting residuals after controlling for sex, age, and day of year ###

controlModel <- gam(BCI ~ s(dayOfYear) + s(Age) + Sex,
                    data = allCompleteCondition)

allCompleteCondition <- allCompleteCondition %>%
  mutate(BCIPredicted = predict(controlModel),
         BCIResiduals = BCI - BCIPredicted)

### Adding in annual weather metrics ###

annualWeather <- read.csv("C:/Users/jbrig/Desktop/Badgers/Weather data/2019 update/Annual weather figures_updated.csv")

names(annualWeather)[1] <- c("Year")
names(annualWeather)[4] <- c("alpha.temp.year")

allCompleteCondition <- allCompleteCondition %>%
  left_join(annualWeather, by = "Year")

### Adding in a column for whether it's a male with descended testes or not ###

testesDescended <- rep("0", nrow(allCompleteCondition))

descPos <- which(allCompleteCondition$seasonTestes == "Descended")

testesDescended[descPos] <- "1"

allCompleteCondition$testesDescended <- testesDescended

### Diagnostics on the residuals ###

ggplot(allCompleteCondition, aes(x = dayOfYear, y = BCIPredicted, colour = Age, shape = Sex)) + 
  geom_point()
ggplot(allCompleteCondition, aes(x = dayOfYear, y = BCIResiduals, colour = Age, shape = Sex)) + 
  geom_point()
#^Make sure we have logical relationships; we do, although slightly overfitted summer dayOfYear
#spline. 


### First we model survival to the next year as a function of BCI residual in each season and ###
### annual weather metrics.                                                                   ###

springCompleteCondition <- subset(allCompleteCondition, Season == "Spring")
summerCompleteCondition <- subset(allCompleteCondition, Season == "Summer")
autumnCompleteCondition <- subset(allCompleteCondition, Season == "Autumn")

## Spring modelling ##

# Diagnostics #

varsOfInterest  <- c("Year", "Weight", "totalN", "BCIResiduals",
                     "mean.temp.year", "sigma.temp.year", 
                     "alpha.temp.year", "mean.rain.year",
                     "cv.rain.year")

# Checking collinearity #

corrplot(cor(springCompleteCondition[, varsOfInterest]), method = "number")
#There's some collinearity in the weather covariates, but none are 0.6 or higher. 
#Worth keeping an eye on the SEs for these covariates.

# Standardizing #

springCompleteCondition$Survived.year <- factor(springCompleteCondition$Survived.year)
springCompleteCondition$BCIResiduals  <- c(springCompleteCondition$BCIResiduals)

springobj <- standardize(Survived.year ~ Age + Sex + testesDescended +
                           seasonTestes + Reproductive.female + 
                           BCIResiduals +
                           mean.temp.year + sigma.temp.year +
                           alpha.temp.year + mean.rain.year +
                           cv.rain.year +
                           totalN,
                         data = springCompleteCondition,
                         family = binomial)

springobj$data$Sett <- springCompleteCondition$Sett

springobj$data <- springobj$data %>%
  mutate_if(is.matrix, as.vector)

springobj$data$seasonTestes <- relevel(springobj$data$seasonTestes, ref = "Ascended")

# Reworking model formula #

springobj$formula1 <- update(springobj$formula, ~. - Age - Sex -
                               Reproductive.female -
                               testesDescended + 
                               s(Age, by = Sex) -                     #Rework to have spline for age
                               BCIResiduals +
                               s(BCIResiduals) +                      #Giving a non-linear effect for BCI
                               BCIResiduals:mean.temp.year + 
                               BCIResiduals:sigma.temp.year +
                               BCIResiduals:alpha.temp.year +
                               BCIResiduals:mean.rain.year + 
                               BCIResiduals:cv.rain.year +            #In weather interactions
                               BCIResiduals:totalN +                  #In density interaction
                               BCIResiduals:Reproductive.female +            
                               BCIResiduals:testesDescended +         #In reproductive status interactions
                               BCIResiduals:Age)                      #In age interaction

springGlobalModel <- gamm(formula = springobj$formula1,
                          data    = springobj$data,
                          family  = binomial,
                          random  = list(Sett = ~1))

springModelFinal <- springGlobalModel

springobj$formulaMain <- springobj$formula1

sprSum <- summary(springModelFinal$gam)

sprSplinep   <- sprSum$s.table[which(rownames(sprSum$s.table) == "s(BCIResiduals)"),4]
sprSplineedf <- sprSum$s.table[which(rownames(sprSum$s.table) == "s(BCIResiduals)"),1]

dict$spr.surv.bci.p   <- pStore(sprSplinep)
dict$spr.surv.bci.edf <- round(sprSplineedf, 1)

# Visualising #

plot_model(springGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "Age [-1,0,2]"))
plot_model(springGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "Reproductive.female"))
plot_model(springGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "seasonTestes"))
plot_model(springGlobalModel$gam, type = "pred", terms = c("Age [-1, 0, 2]", "Sex"))

# Checking differences among testes variable #

springModelFinal$gam$call <- quote(gamm(formula = springobj$formula1,
                                        data    = springobj$data,
                                        family  = binomial,
                                        random  = list(Sett = ~1)))

meansSpr <- summary(emmeans(springModelFinal, specs = pairwise ~ seasonTestes))$contrasts

survDict$spring.fem.vs.rep.p <- pStore(meansSpr$p.value[which(meansSpr$contrast == "reproductiveFemale,1,0,Female - nonrepFemale,0,0,Female")])

# Checking collinearity isn't an issue #

springobj$formula2 <- update(springobj$formula, ~. - Age - Sex -
                               testesDescended - Reproductive.female + 
                               s(Age, by = Sex) -                     #Rework to have spline for age
                               BCIResiduals +
                               s(BCIResiduals) +                      #Giving a non-linear effect for BCI
                               BCIResiduals:mean.temp.year + 
                               BCIResiduals:sigma.temp.year -
                               alpha.temp.year -
                               mean.rain.year + 
                               BCIResiduals:cv.rain.year +            #In weather interactions
                               BCIResiduals:totalN +                  #In density interaction
                               BCIResiduals:Reproductive.female +     #In reproductive status interactions
                               BCIResiduals:testesDescended +
                               BCIResiduals:Age)                      #In age interaction

springCheckModel <- gamm(formula  = springobj$formula2,
                          data    = springobj$data,
                          family  = binomial,
                          random  = list(Sett = ~1)) #All good, no substantial change by removing those

## Summer modelling ##

# Checking collinearity #

corrplot(cor(summerCompleteCondition[, varsOfInterest]), method = "number")
#Similar to the spring set, although a couple are a little closer to 0.6 because of the distribution 
#of annual records

# Standardizing #

summerCompleteCondition$Survived.year <- factor(summerCompleteCondition$Survived.year)
summerCompleteCondition$BCIResiduals  <- c(summerCompleteCondition$BCIResiduals)

summerobj <- standardize(Survived.year ~ Age +
                           Sex + seasonTestes + 
                           Reproductive.female + testesDescended +
                           BCIResiduals +
                           mean.temp.year + sigma.temp.year +
                           alpha.temp.year + mean.rain.year +
                           cv.rain.year +
                           totalN,
                         data = summerCompleteCondition,
                         family = binomial)

summerobj$data$Sett <- summerCompleteCondition$Sett

summerobj$data <- summerobj$data %>%
  mutate_if(is.matrix, as.vector)

summerobj$data$seasonTestes <- relevel(summerobj$data$seasonTestes, ref = "Ascended")

# Reworking model formula #

summerobj$formula1 <- update(summerobj$formula, ~. - Age - Sex -
                               Reproductive.female - testesDescended + 
                               s(Age, by = Sex) -                     #Rework to have spline for age
                               BCIResiduals +
                               s(BCIResiduals) +                      #Giving a non-linear effect for BCI
                               BCIResiduals:mean.temp.year + 
                               BCIResiduals:sigma.temp.year +
                               BCIResiduals:alpha.temp.year +
                               BCIResiduals:mean.rain.year + 
                               BCIResiduals:cv.rain.year +            #In weather interactions
                               BCIResiduals:totalN +                  #In density interaction
                               BCIResiduals:Reproductive.female +     #In reproductive status interactions
                               BCIResiduals:testesDescended +
                               BCIResiduals:Age)                      #In age interaction

summerGlobalModel <- gamm(formula = summerobj$formula1,
                          data    = summerobj$data,
                          family  = binomial,
                          random  = list(Sett = ~1))

summerModelFinal <- summerGlobalModel

summerobj$formulaMain <- summerobj$formula1

sumSum <- summary(summerModelFinal$gam)

sumSplinep   <- sumSum$s.table[which(rownames(sumSum$s.table) == "s(BCIResiduals)"),4]
sumSplineedf <- sumSum$s.table[which(rownames(sumSum$s.table) == "s(BCIResiduals)"),1]

repintp <- sumSum$p.pv[which(names(sumSum$p.pv) == "BCIResiduals:Reproductive.female1")]

dict$sum.surv.bci.p      <- pStore(sumSplinep)
dict$sum.surv.bci.edf    <- round(sumSplineedf, 1)
survDict$summer.repint.p <- pStore(repintp)

plot_model(summerGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "Age [-0.8, 1.8]"))
plot_model(summerGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "Reproductive.female"))
plot_model(summerGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "cv.rain.year [-0.98, 3]"))
plot_model(summerGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "alpha.temp.year [-2.2, 1.9]"))

# Checking differences among testes variable #

summerModelFinal$gam$call <- quote(gamm(formula = summerobj$formula1,
                                        data    = summerobj$data,
                                        family  = binomial,
                                        random  = list(Sett = ~1)))

meansSum <- summary(emmeans(summerModelFinal, specs = pairwise ~ seasonTestes))$contrasts

survDict$summer.fem.vs.rep.p <- pStore(meansSum$p.value[which(meansSum$contrast == "reproductiveFemale,1,0,Female - nonrepFemale,0,0,Female")])

# Visualising effects of summer BCI under different CV conditions #

summerPred <- allButAverager(data     = summerobj$data,
                             terms    = c("cv.rain.year", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Descended",
                                                   testesDescended = "1"))

ageBins <- c(1.9, 3) # Using bins for a non-age thing 

summerPred$cv.rain.year <- ageBin(summerPred$cv.rain.year,
                                  standardized = TRUE,
                                  originalAges = summerCompleteCondition$cv.rain.year)

summerSurvPredM <- predict(summerModelFinal$gam, 
                           newdata = summerPred, 
                           se.fit = TRUE)

summerPred <- allButAverager(data     = summerPred,
                             terms    = c("cv.rain.year", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale",
                                                   testesDescended = "0"))

summerSurvPredF <- predict(summerModelFinal$gam, 
                           newdata = summerPred, 
                           se.fit = TRUE)

summerPred$predictions <- (summerSurvPredM$fit + summerSurvPredF$fit)/2
summerPred$upper <- summerPred$predictions + 1.96*(summerSurvPredM$se.fit + summerSurvPredF$se.fit)/2
summerPred$lower <- summerPred$predictions - 1.96*(summerSurvPredM$se.fit + summerSurvPredF$se.fit)/2

summerPred$cv.rain.year <- factor(reverseStandardize(variable = summerCompleteCondition$cv.rain.year,
                                                     toReverse = summerPred$cv.rain.year))

ggplot(summerPred, aes(x = BCIResiduals, y = I(inv_logit(predictions)))) + 
  geom_point(aes(colour = cv.rain.year)) +
  geom_ribbon(aes(ymax = I(inv_logit(upper)), 
                  ymin = I(inv_logit(lower)), 
                  fill = cv.rain.year), 
              alpha = 0.2)

rm(summerPred, summerSurvPredF, summerSurvPredM, ageBins)

# Effect of higher density on survival #

summerPred <- allButAverager(data     = summerobj$data,
                             terms    = c("totalN"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Descended",
                                                   testesDescended = "1"))

ageBins <- c(200, 320) # Using bins for a non-age thing 

summerPred$totalN <- ageBin(summerPred$totalN,
                            standardized = TRUE,
                            originalAges = summerCompleteCondition$totalN)

summerPredFit <- predict(summerModelFinal$gam, newdata = summerPred, se.fit = TRUE)

summerPred$predictions <- inv_logit(summerPredFit$fit)*100
summerPred$se <- inv_logit(summerPredFit$fit + summerPredFit$se.fit)*100 - inv_logit(summerPredFit$fit)*100

dict$ni.max <- max(allCompleteCondition$totalN) %>% round()
dict$ni.min <- min(allCompleteCondition$totalN) %>% round()
dict$ni.hi  <- ageBins[2]
dict$ni.lo  <- ageBins[1]

ni.lo.mean <- summerPred$predictions[which(summerPred$totalN == min(summerPred$totalN))][1] %>%
  round(1)
ni.hi.mean <- summerPred$predictions[which(summerPred$totalN == max(summerPred$totalN))][1] %>%
  round(1)
ni.lo.se   <- summerPred$se[which(summerPred$totalN == min(summerPred$totalN))][1] %>%
  round(1)
ni.hi.se   <- summerPred$se[which(summerPred$totalN == max(summerPred$totalN))][1] %>%
  round(1)

dict$ni.diff <- ni.lo.mean - ni.hi.mean
dict$ni.maxdiff <- ni.lo.mean + ni.lo.se - (ni.hi.mean - ni.hi.se)
dict$ni.mindiff <- ni.lo.mean - ni.lo.se - (ni.hi.mean + ni.hi.se)

rm(summerPred, summerPredFit, ageBins)

## Autumn modelling ##

# Checking collinearity #

corrplot(cor(autumnCompleteCondition[, varsOfInterest]), method = "number")
#Better than either of the previous ones

# Standardizing #

autumnCompleteCondition$Survived.year <- factor(autumnCompleteCondition$Survived.year)
autumnCompleteCondition$BCIResiduals  <- c(autumnCompleteCondition$BCIResiduals)

autumnobj <- standardize(Survived.year ~ Age +
                           Sex + 
                           seasonTestes + Reproductive.female + testesDescended + 
                           BCIResiduals +
                           mean.temp.year + sigma.temp.year +
                           alpha.temp.year + mean.rain.year +
                           cv.rain.year +
                           totalN,
                         data   = autumnCompleteCondition,
                         family = binomial)

autumnobj$data$Sett <- autumnCompleteCondition$Sett

autumnobj$data <- autumnobj$data %>%
  mutate_if(is.matrix, as.vector)

autumnobj$data$seasonTestes <- relevel(autumnobj$data$seasonTestes, ref = "Ascended")

# Reworking model formula #

autumnobj$formula1 <- update(autumnobj$formula, ~. - Age - Sex -
                               testesDescended - Reproductive.female +
                               s(Age, by = Sex) -                     #Rework to have spline for age
                               BCIResiduals +
                               s(BCIResiduals) +                      #Giving a non-linear effect for BCI
                               BCIResiduals:mean.temp.year + 
                               BCIResiduals:sigma.temp.year +
                               BCIResiduals:alpha.temp.year +
                               BCIResiduals:mean.rain.year + 
                               BCIResiduals:cv.rain.year +            #In weather interactions
                               BCIResiduals:totalN +                  #In density interaction
                               BCIResiduals:Age +                     #In age interaction
                               BCIResiduals:Reproductive.female +     #In rep. status interactions
                               BCIResiduals:testesDescended)                      

autumnGlobalModel <- gamm(formula = autumnobj$formula1,
                          data    = autumnobj$data,
                          family  = binomial,
                          random  = list(Sett = ~1))

autumnModelFinal <- autumnGlobalModel

autumnobj$formulaMain <- autumnobj$formula1

autSum <- summary(autumnModelFinal$gam)

autSplinep   <- autSum$s.table[which(rownames(autSum$s.table) == "s(BCIResiduals)"),4]
autSplineedf <- autSum$s.table[which(rownames(autSum$s.table) == "s(BCIResiduals)"),1]

dict$aut.surv.bci.p   <- pStore(autSplinep)
dict$aut.surv.bci.edf <- round(autSplineedf, 1)

plot_model(autumnGlobalModel$gam, type = "pred", terms = c("BCIResiduals"))
plot_model(autumnGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "Age [-1, 2]"))
plot_model(autumnGlobalModel$gam, type = "pred", terms = c("BCIResiduals", "mean.rain.year [-1.9, 2.1]"))
plot_model(autumnGlobalModel$gam, type = "pred", terms = c("mean.rain.year", "BCIResiduals"))
#^Last of these is particularly informative

## Comparing the best model looking at annual metrics to the best model with season-specific metrics ##

varsOfInterest <- c("Year", "Weight", "totalN", "BCIResiduals",
                    "autumnTemp", "autumnRain", "autumnSigma", "autumnCV",
                    "winterTemp", "winterRain", "winterSigma", "winterCV",
                    "nextSpringTemp", "nextSpringRain", "nextSpringSigma", "nextSpringCV")

corrplot(cor(autumnCompleteCondition[,varsOfInterest]), method = "number")

spr.pca <- prcomp(autumnCompleteCondition[,c("nextSpringTemp", "nextSpringRain", 
                                             "nextSpringSigma", "nextSpringCV")])

spr.summary <- summary(spr.pca)

autumnCompleteCondition$Sp.1 <- spr.pca$x[,1]
autumnCompleteCondition$Sp.2 <- spr.pca$x[,2]

varsOfInterest  <- c("Year", "Weight", "totalN", "BCIResiduals",
                     "autumnTemp", "autumnRain", "autumnSigma", "autumnCV",
                     "winterTemp", "winterRain", "winterSigma", "winterCV",
                     "Sp.1", "Sp.2")

corrplot(cor(autumnCompleteCondition[,varsOfInterest]), method = "number")

autumnobj2 <- standardize(Survived.year ~ Age +
                            Sex + seasonTestes + Reproductive.female + testesDescended +
                            BCIResiduals +
                            autumnTemp + autumnRain + autumnSigma + autumnCV +
                            winterTemp + winterRain + winterSigma + winterCV +
                            Sp.1 + Sp.2 +
                            totalN,
                          data   = autumnCompleteCondition,
                          family = binomial)

autumnobj2$data$Sett <- autumnCompleteCondition$Sett

autumnobj2$data <- autumnobj2$data %>%
  mutate_if(is.matrix, as.vector)

autumnobj2$data$seasonTestes <- relevel(autumnobj2$data$seasonTestes, ref = "Ascended")

autumnobj2$formula1 <- update(autumnobj2$formula, ~. - Age - Sex -
                                Reproductive.female - testesDescended + 
                                s(Age, by = Sex) -                     #Rework to have spline for age
                                BCIResiduals +
                                s(BCIResiduals) +                      #Giving a non-linear effect for BCI
                                BCIResiduals:autumnTemp + 
                                BCIResiduals:autumnRain +
                                BCIResiduals:autumnSigma +
                                BCIResiduals:autumnCV +
                                BCIResiduals:winterTemp +
                                BCIResiduals:winterRain +
                                BCIResiduals:winterSigma + 
                                BCIResiduals:winterCV +
                                BCIResiduals:Sp.1 +
                                BCIResiduals:Sp.2 +
                                BCIResiduals:totalN +                  #In density interaction
                                BCIResiduals:Reproductive.female +     #In rep. status interactions
                                BCIResiduals:testesDescended +
                                BCIResiduals:Age)                      #In age interaction



autumnGlobalModel2 <- gamm(formula = autumnobj2$formula1,
                           data    = autumnobj2$data,
                           family  = binomial,
                           random  = list(Sett = ~1))

# Checking differences among testes variable #

autumnModelFinal$gam$call <- quote(gamm(formula = autumnobj$formula1,
                                        data    = autumnobj$data,
                                        family  = binomial,
                                        random  = list(Sett = ~1)))

meansAut <- summary(emmeans(autumnModelFinal, specs = pairwise ~ seasonTestes))$contrasts

survDict$autumn.fem.vs.rep.p <- pStore(meansAut$p.value[which(meansAut$contrast == "reproductiveFemale,1,0,Female - nonrepFemale,0,0,Female")])

### Building fat-based survival models ###

## Spring fat model ##

springFatCondition <- springCompleteCondition %>%
  subset(Fat.score < 5) %>%
  mutate(Fat.score = factor(Fat.score))

survDict$n.spring.5 <- springCompleteCondition %>% 
  subset(Fat.score == "5") %>%
  nrow()

springfatobj <- standardize(Survived.year ~ Age + seasonTestes +
                              Sex + 
                              Reproductive.female + 
                              Fat.score +
                              mean.temp.year + sigma.temp.year +
                              alpha.temp.year + mean.rain.year +
                              cv.rain.year +
                              totalN,
                            data = springFatCondition,
                            family = binomial)

springfatobj$formulaMain <- update(springobj$formulaMain, ~. - s(BCIResiduals) - 
                                     Reproductive.female - testesDescended -
                                     BCIResiduals:seasonTestes-
                                     BCIResiduals:mean.temp.year -
                                     BCIResiduals:sigma.temp.year - 
                                     BCIResiduals:alpha.temp.year -
                                     BCIResiduals:mean.rain.year - 
                                     BCIResiduals:cv.rain.year - 
                                     BCIResiduals:totalN - 
                                     BCIResiduals:Reproductive.female - 
                                     BCIResiduals:testesDescended - 
                                     BCIResiduals:Age +
                                     Fat.score)

springfatobj$data <- springfatobj$data %>%
  mutate_if(is.matrix, as.vector)

springfatobj$data$Sett <- as.factor(springFatCondition$Sett)
springfatobj$data$seasonTestes <- relevel(springfatobj$data$seasonTestes, ref = "Ascended")

springFatModel <- gamm(formula = springfatobj$formulaMain,
                       data    = springfatobj$data,
                       family  = binomial,
                       random  = list(Sett = ~1), niterPQL = 50)

plot_model(springFatModel$gam, type = "pred", terms = c("Fat.score"))

## Summer model ##

summerFatCondition <- summerCompleteCondition %>%
  subset(!is.na(Fat.score)) %>%
  mutate(Fat.score = factor(Fat.score))

summerfatobj <- standardize(Survived.year ~ Age + seasonTestes +
                              Sex + Reproductive.female + 
                              Fat.score +
                              mean.temp.year + sigma.temp.year +
                              alpha.temp.year + mean.rain.year +
                              cv.rain.year +
                              totalN,
                            data = summerFatCondition,
                            family = binomial)

summerfatobj$formulaMain <- update(summerobj$formulaMain, ~. - s(BCIResiduals) - 
                                     BCIResiduals:mean.temp.year -
                                     BCIResiduals:sigma.temp.year - 
                                     BCIResiduals:alpha.temp.year -
                                     BCIResiduals:mean.rain.year - 
                                     BCIResiduals:cv.rain.year - 
                                     BCIResiduals:totalN - 
                                     BCIResiduals:Reproductive.female -
                                     BCIResiduals:testesDescended -
                                     BCIResiduals:Age +
                                     Fat.score)

summerfatobj$data <- summerfatobj$data %>%
  mutate_if(is.matrix, as.vector)

summerfatobj$data$seasonTestes <- relevel(summerfatobj$data$seasonTestes, ref = "Ascended")

summerfatobj$data$Sett <- as.factor(summerFatCondition$Sett)

summerFatModel <- gamm(formula = summerfatobj$formulaMain,
                       data    = summerfatobj$data,
                       family  = binomial,
                       random  = list(Sett = ~1), niterPQL = 50)

plot_model(summerFatModel$gam, type = "pred", terms = c("Fat.score"))

## Autumn model ##

autumnFatCondition <- autumnCompleteCondition %>%
  subset(!is.na(Fat.score)) %>%
  mutate(Fat.score = factor(Fat.score))

autumnfatobj <- standardize(Survived.year ~ Age + seasonTestes + Reproductive.female +
                              Sex + 
                              Fat.score +
                              mean.temp.year + sigma.temp.year +
                              alpha.temp.year + mean.rain.year +
                              cv.rain.year +
                              totalN,
                            data = autumnFatCondition,
                            family = binomial)

autumnfatobj$formulaMain <- update(autumnobj$formulaMain, ~. - s(BCIResiduals) - 
                                     BCIResiduals:Reproductive.female - 
                                     BCIResiduals:testesDescended -
                                     BCIResiduals:mean.temp.year -
                                     BCIResiduals:sigma.temp.year - 
                                     BCIResiduals:alpha.temp.year -
                                     BCIResiduals:mean.rain.year - 
                                     BCIResiduals:cv.rain.year - 
                                     BCIResiduals:totalN - 
                                     BCIResiduals:Age +
                                     Fat.score)

autumnfatobj$data <- autumnfatobj$data %>%
  mutate_if(is.matrix, as.vector)

autumnfatobj$data$Sett <- as.factor(autumnFatCondition$Sett)
autumnfatobj$data$seasonTestes <- relevel(autumnfatobj$data$seasonTestes, ref = "Ascended")

autumnFatModel <- gamm(formula = autumnfatobj$formulaMain,
                       data    = autumnfatobj$data,
                       family  = binomial,
                       random  = list(Sett = ~1), niterPQL = 50)

plot_model(autumnFatModel$gam, type = "pred", terms = c("Fat.score"))

## Testing differences between fat groups ##

springFatModel$gam$call <- quote(gamm(formula = springfatobj$formulaMain,
                                      data    = springfatobj$data,
                                      family  = binomial,
                                      random  = list(Sett = ~1)))

summerFatModel$gam$call <- quote(gamm(formula = summerfatobj$formulaMain,
                                      data    = summerfatobj$data,
                                      family  = binomial,
                                      random  = list(Sett = ~1)))

autumnFatModel$gam$call <- quote(gamm(formula = autumnfatobj$formulaMain,
                                      data    = autumnfatobj$data,
                                      family  = binomial,
                                      random  = list(Sett = ~1)))

meansSprFat <- summary(emmeans(springFatModel, specs = pairwise ~ Fat.score))$contrasts
meansSumFat <- summary(emmeans(summerFatModel, specs = pairwise ~ Fat.score))$contrasts
meansAutFat <- summary(emmeans(autumnFatModel, specs = pairwise ~ Fat.score))$contrasts
meansSprFat2 <- summary(emmeans(springFatModel, specs = pairwise ~ seasonTestes))$contrasts
meansSumFat2 <- summary(emmeans(summerFatModel, specs = pairwise ~ seasonTestes))$contrasts
meansAutFat2 <- summary(emmeans(autumnFatModel, specs = pairwise ~ seasonTestes))$contrasts

### Visualising effects of reproductive vs. nonreproductive females ###

springPred <- allButAverager(data     = springobj$data,
                             terms    = c("BCIResiduals", "Reproductive.female", "Sex", "seasonTestes",
                                          "testesDescended"),
                             defaults = NULL)

springPredictions <- predict(springModelFinal$gam,
                             newdata = springPred,
                             se.fit = TRUE)

springPred$predictions <- inv_logit(springPredictions$fit)
springPred$upper       <- inv_logit(springPredictions$fit + springPredictions$se.fit)
springPred$lower       <- inv_logit(springPredictions$fit - springPredictions$se.fit)

ggplot(springPred, aes(x = BCIResiduals, y = predictions)) + 
  geom_point(aes(colour = seasonTestes)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = seasonTestes), 
              alpha = 0.2)

summerPred <- allButAverager(data     = summerobj$data,
                             terms    = c("BCIResiduals", "Reproductive.female", "Sex", "seasonTestes",
                                          "testesDescended"),
                             defaults = NULL)

summerPredictions <- predict(summerModelFinal$gam,
                             newdata = summerPred,
                             se.fit = TRUE)

summerPred$predictions <- inv_logit(summerPredictions$fit)
summerPred$upper       <- inv_logit(summerPredictions$fit + summerPredictions$se.fit)
summerPred$lower       <- inv_logit(summerPredictions$fit - summerPredictions$se.fit)

ggplot(summerPred, aes(x = BCIResiduals, y = predictions)) + 
  geom_point(aes(colour = seasonTestes)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = seasonTestes), 
              alpha = 0.2)

maxSpring <- springPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == max(predictions))
minSpring <- springPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == min(predictions))
maxSummer <- summerPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == max(predictions))
minSummer <- summerPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == min(predictions))

survDict$spring.nonrep.diff <- round(100*(maxSpring$predictions - minSpring$predictions))
survDict$summer.nonrep.diff <- round(100*(maxSummer$predictions - minSummer$predictions))

survDict$spring.nonrep.maxdiff <- round(100*(maxSpring$upper - minSpring$lower))
survDict$spring.nonrep.mindiff <- round(100*(maxSpring$lower - minSpring$upper))
survDict$summer.nonrep.maxdiff <- round(100*(maxSummer$upper - minSummer$lower))
survDict$summer.nonrep.mindiff <- round(100*(maxSummer$lower - minSummer$upper))

rm(springPred, summerPred, springPredictions, summerPredictions)

### Quantifying effects of BCI on survival in each season (for effect size reporting) ###

springPredM1 <- allButAverager(data     = springobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Descended",
                                                     testesDescended = "1"))

springPredictionsM1 <- predict(springGlobalModel$gam,
                              newdata = springPredM1,
                              se.fit = TRUE)

springPredM1$predictions <- inv_logit(springPredictionsM1$fit)
springPredM1$upper       <- inv_logit(springPredictionsM1$fit + 1.96*springPredictionsM1$se.fit)
springPredM1$lower       <- inv_logit(springPredictionsM1$fit - 1.96*springPredictionsM1$se.fit)

springPredM2 <- allButAverager(data     = springobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Intermediate",
                                                     testesDescended = "1"))

springPredictionsM2 <- predict(springGlobalModel$gam,
                               newdata = springPredM2,
                               se.fit = TRUE)

springPredM2$predictions <- inv_logit(springPredictionsM2$fit)
springPredM2$upper       <- inv_logit(springPredictionsM2$fit + 1.96*springPredictionsM2$se.fit)
springPredM2$lower       <- inv_logit(springPredictionsM2$fit - 1.96*springPredictionsM2$se.fit)

springPredM3 <- allButAverager(data     = springobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Ascended",
                                                     testesDescended = "1"))

springPredictionsM3 <- predict(springGlobalModel$gam,
                               newdata = springPredM3,
                               se.fit = TRUE)

springPredM3$predictions <- inv_logit(springPredictionsM3$fit)
springPredM3$upper       <- inv_logit(springPredictionsM3$fit + 1.96*springPredictionsM3$se.fit)
springPredM3$lower       <- inv_logit(springPredictionsM3$fit - 1.96*springPredictionsM3$se.fit)

springPredF1 <- allButAverager(data     = springobj$data,
                              terms    = c("BCIResiduals"),
                              defaults = data.frame(Reproductive.female = "1",
                                                    Sex = "Female",
                                                    seasonTestes = "reproductiveFemale",
                                                    testesDescended = "0"))

springPredictionsF1 <- predict(springModelFinal$gam,
                              newdata = springPredF1,
                              se.fit = TRUE)

springPredF1$predictions <- inv_logit(springPredictionsF1$fit)
springPredF1$upper       <- inv_logit(springPredictionsF1$fit + 1.96*springPredictionsF1$se.fit)
springPredF1$lower       <- inv_logit(springPredictionsF1$fit - 1.96*springPredictionsF1$se.fit)

springPredF2 <- allButAverager(data     = springobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Female",
                                                     seasonTestes = "nonrepFemale",
                                                     testesDescended = "0"))

springPredictionsF2 <- predict(springModelFinal$gam,
                               newdata = springPredF2,
                               se.fit = TRUE)

springPredF2$predictions <- inv_logit(springPredictionsF2$fit)
springPredF2$upper       <- inv_logit(springPredictionsF2$fit + 1.96*springPredictionsF2$se.fit)
springPredF2$lower       <- inv_logit(springPredictionsF2$fit - 1.96*springPredictionsF2$se.fit)

springPred <- springPredF1
springPred$predictions <- apply(cbind(springPredM1$predictions,
                                      springPredM2$predictions,
                                      springPredM3$predictions,
                                      springPredF1$predictions,
                                      springPredF2$predictions),
                                1,
                                weighted.mean,
                                w = c(777, 67, 42, 468, 719))
springPred$lower <- apply(cbind(springPredM1$lower,
                                springPredM2$lower,
                                springPredM3$lower,
                                springPredF1$lower,
                                springPredF2$lower),
                          1,
                          weighted.mean,
                          w = c(777, 67, 42, 468, 719))
springPred$upper <- apply(cbind(springPredM1$upper,
                                springPredM2$upper,
                                springPredM3$upper,
                                springPredF1$upper,
                                springPredF2$upper),
                          1,
                          weighted.mean,
                          w = c(777, 67, 42, 468, 719))



ggplot(springPred, aes(x = BCIResiduals, y = predictions)) + 
  geom_point() +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower), 
              alpha = 0.2)

springPred$predictions2 <- predict.gam(springModelFinal$gam, terms = "BCIResiduals", type = "response")

ggplot(springPred, aes(x = BCIResiduals, y = predictions2)) + 
  geom_point() +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower), 
              alpha = 0.2)

summerPredM1 <- allButAverager(data     = summerobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Descended",
                                                     testesDescended = "1"))

summerPredictionsM1 <- predict(summerGlobalModel$gam,
                               newdata = summerPredM1,
                               se.fit = TRUE)

summerPredM1$predictions <- inv_logit(summerPredictionsM1$fit)
summerPredM1$upper       <- inv_logit(summerPredictionsM1$fit + 1.96*summerPredictionsM1$se.fit)
summerPredM1$lower       <- inv_logit(summerPredictionsM1$fit - 1.96*summerPredictionsM1$se.fit)

summerPredM2 <- allButAverager(data     = summerobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Intermediate",
                                                     testesDescended = "1"))

summerPredictionsM2 <- predict(summerGlobalModel$gam,
                               newdata = summerPredM2,
                               se.fit = TRUE)

summerPredM2$predictions <- inv_logit(summerPredictionsM2$fit)
summerPredM2$upper       <- inv_logit(summerPredictionsM2$fit + 1.96*summerPredictionsM2$se.fit)
summerPredM2$lower       <- inv_logit(summerPredictionsM2$fit - 1.96*summerPredictionsM2$se.fit)

summerPredM3 <- allButAverager(data     = summerobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Male",
                                                     seasonTestes = "Ascended",
                                                     testesDescended = "1"))

summerPredictionsM3 <- predict(summerGlobalModel$gam,
                               newdata = summerPredM3,
                               se.fit = TRUE)

summerPredM3$predictions <- inv_logit(summerPredictionsM3$fit)
summerPredM3$upper       <- inv_logit(summerPredictionsM3$fit + 1.96*summerPredictionsM3$se.fit)
summerPredM3$lower       <- inv_logit(summerPredictionsM3$fit - 1.96*summerPredictionsM3$se.fit)

summerPredF1 <- allButAverager(data     = summerobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "1",
                                                     Sex = "Female",
                                                     seasonTestes = "reproductiveFemale",
                                                     testesDescended = "0"))

summerPredictionsF1 <- predict(summerModelFinal$gam,
                               newdata = summerPredF1,
                               se.fit = TRUE)

summerPredF1$predictions <- inv_logit(summerPredictionsF1$fit)
summerPredF1$upper       <- inv_logit(summerPredictionsF1$fit + 1.96*summerPredictionsF1$se.fit)
summerPredF1$lower       <- inv_logit(summerPredictionsF1$fit - 1.96*summerPredictionsF1$se.fit)

summerPredF2 <- allButAverager(data     = summerobj$data,
                               terms    = c("BCIResiduals"),
                               defaults = data.frame(Reproductive.female = "0",
                                                     Sex = "Female",
                                                     seasonTestes = "nonrepFemale",
                                                     testesDescended = "0"))

summerPredictionsF2 <- predict(summerModelFinal$gam,
                               newdata = summerPredF2,
                               se.fit = TRUE)

summerPredF2$predictions <- inv_logit(summerPredictionsF2$fit)
summerPredF2$upper       <- inv_logit(summerPredictionsF2$fit + 1.96*summerPredictionsF2$se.fit)
summerPredF2$lower       <- inv_logit(summerPredictionsF2$fit - 1.96*summerPredictionsF2$se.fit)

summerPred <- summerPredF1
summerPred$predictions <- apply(cbind(summerPredM1$predictions,
                                      summerPredM2$predictions,
                                      summerPredM3$predictions,
                                      summerPredF1$predictions,
                                      summerPredF2$predictions),
                                1,
                                weighted.mean,
                                w = c(750, 161, 90, 437, 857))
summerPred$lower <- apply(cbind(summerPredM1$lower,
                                summerPredM2$lower,
                                summerPredM3$lower,
                                summerPredF1$lower,
                                summerPredF2$lower),
                          1,
                          weighted.mean,
                          w = c(750, 161, 90, 437, 857))
summerPred$upper <- apply(cbind(summerPredM1$upper,
                                summerPredM2$upper,
                                summerPredM3$upper,
                                summerPredF1$upper,
                                summerPredF2$upper),
                          1,
                          weighted.mean,
                          w = c(750, 161, 90, 437, 857))


ggplot(summerPred, aes(x = BCIResiduals, y = predictions)) + 
  geom_point() +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower), 
              alpha = 0.2)

summerPred <- allButAverager(data     = summerobj$data,
                             terms    = c("BCIResiduals", "Reproductive.female", "Sex", "seasonTestes",
                                          "testesDescended"),
                             defaults = NULL)

summerPredictions <- predict(summerModelFinal$gam,
                             newdata = summerPred,
                             se.fit = TRUE)

summerPred$predictions <- inv_logit(summerPredictions$fit)
summerPred$upper       <- inv_logit(summerPredictions$fit + summerPredictions$se.fit)
summerPred$lower       <- inv_logit(summerPredictions$fit - summerPredictions$se.fit)

ggplot(summerPred, aes(x = BCIResiduals, y = predictions)) + 
  geom_point(aes(colour = seasonTestes)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = seasonTestes), 
              alpha = 0.2)

maxSpring <- springPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == max(predictions))
minSpring <- springPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == min(predictions))
maxSummer <- summerPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == max(predictions))
minSummer <- summerPred %>% subset(Reproductive.female == "0" & Sex == "Female") %>%
  subset(predictions == min(predictions))

survDict$spring.nonrep.diff <- round(100*(maxSpring$predictions - minSpring$predictions))
survDict$summer.nonrep.diff <- round(100*(maxSummer$predictions - minSummer$predictions))

survDict$spring.nonrep.maxdiff <- round(100*(maxSpring$upper - minSpring$lower))
survDict$spring.nonrep.mindiff <- round(100*(maxSpring$lower - minSpring$upper))
survDict$summer.nonrep.maxdiff <- round(100*(maxSummer$upper - minSummer$lower))
survDict$summer.nonrep.mindiff <- round(100*(maxSummer$lower - minSummer$upper))

rm(springPred, summerPred, springPredictions, summerPredictions)

### Making figures ###

ageBins <- c(2, 9)

# Spring dataframes #

springPred <- allButAverager(data     = springobj$data,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Descended",
                                                   testesDescended = "1")) 

springPred$Age <- ageBin(springPred$Age,
                         standardized = TRUE,
                         originalAges = springCompleteCondition$Age)

springSurvPredM <- predict(springModelFinal$gam, 
                           newdata = springPred, 
                           se.fit = TRUE)

springPred <- allButAverager(data     = springPred,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female", 
                                                   seasonTestes = "nonrepFemale",
                                                   testesDescended = "0"))

springSurvPredF <- predict(springModelFinal$gam, 
                           newdata = springPred, 
                           se.fit = TRUE)

springPred$predictions <- (springSurvPredM$fit + springSurvPredF$fit)/2
springPred$upper <- springPred$predictions + 1.96*(springSurvPredM$se.fit + springSurvPredF$se.fit)/2
springPred$lower <- springPred$predictions - 1.96*(springSurvPredM$se.fit + springSurvPredF$se.fit)/2

springPred$Age <- factor(springPred$Age)

springSurvAgeFig <- ggplot(springPred, aes(x = BCIResiduals, y = I(inv_logit(predictions)))) + 
  geom_point(aes(colour = Age)) +
  geom_ribbon(aes(ymax = I(inv_logit(upper)), 
                  ymin = I(inv_logit(lower)), 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Survival probability") + xlab("Age-, sex-, and calendar-\ncorrected spring BCI") + 
  theme_bw() +
  ylim(c(0, 1)) + 
  scale_color_manual(name = "Age", 
                     breaks = as.character(levels(springPred$Age)),
                     labels = as.character(ageBins),
                     values = c("#2E9388", "#5E1B16")) + 
  scale_fill_manual(name = "Age", 
                    breaks = as.character(levels(springPred$Age)),
                    labels = as.character(ageBins),
                    values = c("#2E9388", "#5E1B16")) +
  guides(fill = FALSE, colour = FALSE)

springFatPred <- allButAverager(data     = springfatobj$data,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Reproductive.female = "0",
                                                      Sex = "Male",
                                                      seasonTestes = "Descended",
                                                      testesDescended = "1")) 

springSurvFatPredM <- predict(springFatModel$gam, 
                           newdata = springFatPred, 
                           se.fit = TRUE)

springFatPred <- allButAverager(data     = springFatPred,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Reproductive.female = "0",
                                                      Sex = "Female",
                                                      seasonTestes = "nonrepFemale",
                                                      testesDescended = "0"))

springSurvFatPredF <- predict(springFatModel$gam, 
                           newdata = springFatPred, 
                           se.fit = TRUE)

springFatPred$predictions <- (springSurvFatPredM$fit + springSurvFatPredF$fit)/2
springFatPred$upper <- springFatPred$predictions + 1.96*(springSurvFatPredM$se.fit + springSurvFatPredF$se.fit)/2
springFatPred$lower <- springFatPred$predictions - 1.96*(springSurvFatPredM$se.fit + springSurvFatPredF$se.fit)/2

springSurvFatFig <- ggplot(springFatPred %>% distinct(Fat.score, .keep_all = TRUE), 
                           aes(x = Fat.score, y = I(inv_logit(predictions)))) + 
  geom_point() + 
  geom_errorbar(aes(ymin = I(inv_logit(lower)),
                    ymax = I(inv_logit(upper))), 
                width = 0.2) +
  ylab("Survival probability") + xlab("Spring fat score") +
  theme_bw() +
  ylim(c(0, 1))

# Summer dataframes #

summerPred <- allButAverager(data     = summerobj$data,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Descended",
                                                   testesDescended = "1")) 

summerPred$Age <- ageBin(summerPred$Age,
                         standardized = TRUE,
                         originalAges = summerCompleteCondition$Age)

summerSurvPredM <- predict(summerModelFinal$gam, 
                           newdata = summerPred, 
                           se.fit = TRUE)

summerPred <- allButAverager(data     = summerPred,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale",
                                                   testesDescended = "0"))

summerSurvPredF <- predict(summerModelFinal$gam, 
                           newdata = summerPred, 
                           se.fit = TRUE)

summerPred$predictions <- (summerSurvPredM$fit + summerSurvPredF$fit)/2
summerPred$upper <- summerPred$predictions + 1.96*(summerSurvPredM$se.fit + summerSurvPredF$se.fit)/2
summerPred$lower <- summerPred$predictions - 1.96*(summerSurvPredM$se.fit + summerSurvPredF$se.fit)/2

summerPred$Age <- factor(summerPred$Age)

summerSurvAgeFig <- ggplot(summerPred, aes(x = BCIResiduals, y = I(inv_logit(predictions)))) + 
  geom_point(aes(colour = Age)) +
  geom_ribbon(aes(ymax = I(inv_logit(upper)), 
                  ymin = I(inv_logit(lower)), 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Survival probability") + xlab("Age-, sex-, and calendar-\ncorrected summer BCI") + 
  theme_bw() +
  ylim(c(0, 1)) + 
  scale_color_manual(name = "Age", 
                     breaks = as.character(levels(summerPred$Age)),
                     labels = as.character(ageBins),
                     values = c("#2E9388", "#5E1B16")) + 
  scale_fill_manual(name = "Age", 
                    breaks = as.character(levels(summerPred$Age)),
                    labels = as.character(ageBins),
                    values = c("#2E9388", "#5E1B16")) +
  guides(fill = FALSE, colour = FALSE)

summerFatPred <- allButAverager(data     = summerfatobj$data,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Reproductive.female = "0",
                                                      Sex = "Male",
                                                      seasonTestes = "Descended",
                                                      testesDescended = "1")) 

summerSurvFatPredM <- predict(summerFatModel$gam, 
                              newdata = summerFatPred, 
                              se.fit = TRUE)

summerFatPred <- allButAverager(data     = summerFatPred,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Reproductive.female = "0",
                                                      Sex = "Female",
                                                      seasonTestes = "nonrepFemale",
                                                      testesDescended = "0"))

summerSurvFatPredF <- predict(summerFatModel$gam, 
                              newdata = summerFatPred, 
                              se.fit = TRUE)

summerFatPred$predictions <- (summerSurvFatPredM$fit + summerSurvFatPredF$fit)/2
summerFatPred$upper <- summerFatPred$predictions + 1.96*(summerSurvFatPredM$se.fit + summerSurvFatPredF$se.fit)/2
summerFatPred$lower <- summerFatPred$predictions - 1.96*(summerSurvFatPredM$se.fit + summerSurvFatPredF$se.fit)/2

summerSurvFatFig <- ggplot(summerFatPred %>% distinct(Fat.score, .keep_all = TRUE), 
                           aes(x = Fat.score, y = I(inv_logit(predictions)))) + 
  geom_point() + 
  geom_errorbar(aes(ymin = I(inv_logit(lower)),
                    ymax = I(inv_logit(upper))), 
                width = 0.2) +
  ylab("Survival probability") + xlab("Summer fat score") +
  theme_bw() +
  ylim(c(0, 1))

# Autumn dataframes #

autumnPred <- allButAverager(data     = autumnobj$data,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Sex = "Male",
                                                   Reproductive.female = "0",
                                                   seasonTestes = "Ascended",
                                                   testesDescended = "0")) 

autumnPred$Age <- ageBin(autumnPred$Age,
                         standardized = TRUE,
                         originalAges = autumnCompleteCondition$Age)

autumnSurvPredM <- predict(autumnModelFinal$gam, 
                           newdata = autumnPred, 
                           se.fit = TRUE)

autumnPred <- allButAverager(data     = autumnPred,
                             terms    = c("Age", "BCIResiduals"),
                             defaults = data.frame(Sex = "Female",
                                                   Reproductive.female = "0",
                                                   seasonTestes = "nonrepFemale",
                                                   testesDescended = "0"))

autumnSurvPredF <- predict(autumnModelFinal$gam, 
                           newdata = autumnPred, 
                           se.fit = TRUE)

autumnPred$predictions <- (autumnSurvPredM$fit + autumnSurvPredF$fit)/2
autumnPred$upper <- autumnPred$predictions + 1.96*(autumnSurvPredM$se.fit + autumnSurvPredF$se.fit)/2
autumnPred$lower <- autumnPred$predictions - 1.96*(autumnSurvPredM$se.fit + autumnSurvPredF$se.fit)/2

autumnPred$Age <- factor(autumnPred$Age)

autumnSurvAgeFig <- ggplot(autumnPred, aes(x = BCIResiduals, y = I(inv_logit(predictions)))) + 
  geom_point(aes(colour = Age)) +
  geom_ribbon(aes(ymax = I(inv_logit(upper)), 
                  ymin = I(inv_logit(lower)), 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Survival probability") + xlab("Age-, sex-, and calendar-\ncorrected autumn BCI") + 
  theme_bw() +
  ylim(c(0, 1)) + 
  scale_color_manual(name = "Age", 
                       breaks = as.character(levels(autumnPred$Age)),
                       labels = as.character(ageBins),
                     values = c("#2E9388", "#5E1B16")) + 
  scale_fill_manual(name = "Age", 
                      breaks = as.character(levels(autumnPred$Age)),
                      labels = as.character(ageBins),
                    values = c("#2E9388", "#5E1B16")) +
  guides(fill = FALSE, colour = FALSE)

autumnFatPred <- allButAverager(data     = autumnfatobj$data,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Sex = "Male",
                                                      Reproductive.female = "0",
                                                      seasonTestes = "Ascended",
                                                      testesDescended = "0")) 

autumnSurvFatPredM <- predict(autumnFatModel$gam, 
                              newdata = autumnFatPred, 
                              se.fit = TRUE)

autumnFatPred <- allButAverager(data     = autumnFatPred,
                                terms    = c("Fat.score"),
                                defaults = data.frame(Sex = "Female",
                                                      Reproductive.female = "0",
                                                      seasonTestes = "nonrepFemale",
                                                      testesDescended = "0"))

autumnSurvFatPredF <- predict(autumnFatModel$gam, 
                              newdata = autumnFatPred, 
                              se.fit = TRUE)

autumnFatPred$predictions <- (autumnSurvFatPredM$fit + autumnSurvFatPredF$fit)/2
autumnFatPred$upper <- autumnFatPred$predictions + 1.96*(autumnSurvFatPredM$se.fit + autumnSurvFatPredF$se.fit)/2
autumnFatPred$lower <- autumnFatPred$predictions - 1.96*(autumnSurvFatPredM$se.fit + autumnSurvFatPredF$se.fit)/2

autumnSurvFatFig <- ggplot(autumnFatPred %>% distinct(Fat.score, .keep_all = TRUE), 
                           aes(x = Fat.score, y = I(inv_logit(predictions)))) + 
  geom_point() + 
  geom_errorbar(aes(ymin = I(inv_logit(lower)),
                    ymax = I(inv_logit(upper))), 
                width = 0.2) +
  ylab("Survival probability") + xlab("Autumn fat score") +
  theme_bw() +
  ylim(c(0, 1))

## Making figure of autumn rainfall-BCI relationship ##

load("Condition plots to combine with survival.RData")

autumnPred2 <- allButAverager(data     = autumnobj$data,
                              terms    = c("mean.rain.year", "BCIResiduals"),
                              defaults = data.frame(Sex = "Male",
                                                    Reproductive.female = "0",
                                                    seasonTestes = "Ascended",
                                                    testesDescended = "0")) 

ageBins <- c(1.5, 2.5) # Using the bin function for a non-age field

autumnPred2$mean.rain.year <- ageBin(autumnPred2$mean.rain.year,
                                     standardized = TRUE,
                                     originalAges = autumnCompleteCondition$mean.rain.year)

autumnSurvPredM <- predict(autumnModelFinal$gam, 
                           newdata = autumnPred2, 
                           se.fit = TRUE)

autumnPred2 <- allButAverager(data     = autumnPred2,
                              terms    = c("mean.rain.year", "BCIResiduals"),
                              defaults = data.frame(Sex = "Female",
                                                    Reproductive.female = "0",
                                                    seasonTestes = "nonrepFemale",
                                                    testesDescended = "0"))

autumnSurvPredF <- predict(autumnModelFinal$gam, 
                           newdata = autumnPred2, 
                           se.fit = TRUE)

autumnPred2$predictions <- (autumnSurvPredM$fit + autumnSurvPredF$fit)/2
autumnPred2$upper <- inv_logit(autumnPred2$predictions + 1.96*(autumnSurvPredM$se.fit + autumnSurvPredF$se.fit)/2)
autumnPred2$lower <- inv_logit(autumnPred2$predictions - 1.96*(autumnSurvPredM$se.fit + autumnSurvPredF$se.fit)/2)
autumnPred2$predictions <- inv_logit(autumnPred2$predictions)

autumnPred2$mean.rain.year <- factor(autumnPred2$mean.rain.year)

autumnRainSurvPlot <- ggplot(autumnPred2, aes(x = BCIResiduals, y = predictions)) +
  geom_line(aes(y = predictions, colour = mean.rain.year)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = mean.rain.year), 
              alpha = 0.2) + 
  ylab("Survival probability") + xlab("Age-, sex-, and calendar-\ncorrected autumn BCI") + 
  theme_bw() +
  scale_color_manual(breaks = as.character(levels(autumnPred2$mean.rain.year)),
                       labels = as.character(ageBins),
                       values = c("#079b9b", "#051D3A")) + 
  scale_fill_manual(breaks = as.character(levels(autumnPred2$mean.rain.year)),
                      labels = as.character(ageBins),
                      values = c("#079b9b", "#051D3A")) +
  scale_y_continuous(limits = c(0, 1)) +
  guides(fill = FALSE, colour = FALSE)



# Putting it together with other plots #

summerWeatherBCIPlot <- plot_grid(summerConditionPlot, autumnConditionPlot,
                                  autumnConditionPlot2, autumnRainSurvPlot,
                                  nrow = 2,
                                  ncol = 2, align = "hv")

## Saving all of these ##

save(springSurvAgeFig, springSurvFatFig,
     summerSurvAgeFig, summerSurvFatFig,
     autumnSurvAgeFig, autumnSurvFatFig, 
     file = "Survival plots.RData")

### Making tables of contrasts for the appendix ###

load("Condition contrast table.RData")

survivalContrasts <- rbind(conditionContrasts, 
                           meansSpr,
                           meansSum,
                           meansAut,
                           meansSprFat,
                           meansSprFat2,
                           meansSumFat,
                           meansSumFat2,
                           meansAutFat,
                           meansAutFat2)

modelVector <- c(modelVector,
                 rep("Survival ~ Spr. BCI", nrow(meansSpr)),
                 rep("Survival ~ Sum. BCI", nrow(meansSum)),
                 rep("Survival ~ Aut. BCI", nrow(meansAut)),
                 rep("Survival ~ Spr. Fat score", nrow(meansSprFat) + nrow(meansSprFat2)),
                 rep("Survival ~ Sum. Fat score", nrow(meansSumFat) + nrow(meansSumFat2)),
                 rep("Survival ~ Aut. Fat score", nrow(meansSumFat) + nrow(meansSumFat2)))

save(survivalContrasts, modelVector, file = "Survival contrast table.RData")

### Saving stuff ###

save(survDict, dict, summerWeatherBCIPlot, file = "Survival modelling output.RData")

save(springModelFinal, springFatModel,
     summerModelFinal, summerFatModel,
     autumnModelFinal, autumnFatModel,
     file = "Survival models.RData")

springSurvobj <- springobj
summerSurvobj <- summerobj
autumnSurvobj <- autumnobj

save(springSurvobj, summerSurvobj, autumnSurvobj, file = "Survival model objects.RData")

# model <- autumnGlobalModel$gam
# modelData <- autumnobj$data
# otherTerm <- c("alpha.temp.year")
# minChange <- -1
# maxChange <- 1
# 
# thresholdVariation <- function(model, modelData,
#                                otherTerm, minChange, maxChange) {
#   constructionSet <- modelData %>% select(-c("BCIResiduals", otherTerm))
#   numericTracker  <- rep(NA, ncol(constructionSet))
#   
#   for (i in 1:ncol(constructionSet)) {
#     numericTracker[i] <- is.numeric(constructionSet[,i])
#     
#     if (numericTracker[i] == TRUE) {
#       constructionSet[,i] <- mean(constructionSet[,i])
#     } else {
#       constructionSet[,i] <- constructionSet[1,i]
#     }
#   }
#   
#   predictionSetMax <- constructionSet %>% 
#     cbind(modelData %>% 
#             select(c("BCIResiduals", otherTerm))) 
#   
#   predictionSetMax[,otherTerm] <- max(predictionSetMax[,otherTerm])
#   
#   predictionSetMin <- constructionSet %>% 
#     cbind(modelData %>% 
#             select(c("BCIResiduals", otherTerm)))
#   
#   predictionSetMin[,otherTerm] <- min(predictionSetMin[,otherTerm])
#   
#   predictionSetMax <- predictionSetMax %>%
#     subset(minChange <= BCIResiduals & BCIResiduals <= maxChange)
#   
#   predictionSetMin <- predictionSetMin %>%
#     subset(minChange <= BCIResiduals & BCIResiduals <= maxChange)
#   
#   predictionsMax <- predict(model, newdata = predictionSetMax, type = "response", se.fit = TRUE)
#   predictionsMin <- predict(model, newdata = predictionSetMin, type = "response", se.fit = TRUE)
#   
#   predictionsMax <- inv_logit(predictionsMax$fit)
#   predictionsMin <- inv_logit(predictionsMin$fit)
#   
#   changeMax <- max(predictionsMax) - min(predictionsMax)
#   changeMin <- max(predictionsMin) - min(predictionsMin)
#   
#   return(c(changeMax, changeMin))
#   
# }
# 
# 
