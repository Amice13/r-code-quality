#######################################################
# Title: Predicting responses to climate change 2
# Name: Julius G. Bright Ross
# Date: Sep 28, 2020
# Last updated: Sep 30, 2020; Jun 5, 2021 for figure
# tweaks
# Details: Importing metrics from UKCP data,
# and using these to predict what would happen to the 
# population under an RCP 8.5 emissions scenario.
# Version 2 is more targeted, and just attempts to 
# predict things for a couple of specific conditions.
#######################################################

## Libraries and workspace ##

rm(list = ls())

library(tidyverse)
library(mgcv)
library(standardize)
library(nlme)
library(lme4)
library(quantreg)
library(cowplot)

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")

load("Condition modelling output.RData")

springModelCondition <- springModelFinal
summerModelCondition <- summerModelFinal
autumnModelCondition <- autumnModelFinal

rm(dict, sum.pca, nonRepPredict2, nonRepPredict9, yesRepPredict2, yesRepPredict9,
   springModelFinal, summerModelFinal, autumnModelFinal)

load("Survival models.RData")

springModelSurvival <- springModelFinal
summerModelSurvival <- summerModelFinal
autumnModelSurvival <- autumnModelFinal

rm(springFatModel, summerFatModel, autumnFatModel,
   springModelFinal, summerModelFinal, autumnModelFinal)

load("Survival model objects.RData")

load("C:/Users/jbrig/Desktop/Badgers/Weather data/UKCP data/Seasonal_figures.RData")
load("C:/Users/jbrig/Desktop/Badgers/Weather data/UKCP data/Annual_figures.RData")
load("C:/Users/jbrig/Desktop/Badgers/Weather data/UKCP data/Window_figures.RData")

Window_figures$Year <- Window_figures$Year %>% as.character() %>% as.numeric()
Window_figures$Projection <- Window_figures$Projection %>% as.character()

annualWeather <- read.csv("C:/Users/jbrig/Desktop/Badgers/Weather data/2019 update/Annual weather figures_updated.csv")

names(annualWeather)[1] <- "Year"
names(annualWeather)[4] <- "alpha.temp.year"

Annual_figures <- Annual_figures %>%
  bind_rows(annualWeather %>% mutate(Projection = "Real"))

oldWindow_figures <- read.csv("C:/Users/jbrig/Desktop/Badgers/Weather data/2019 update/Dry window weather figures.csv")

color.summer   <- "#9C9C3A"
color.autumn   <- "#AC499B"

## Custom functions ##

inv_logit <- function(data) {
  
  return(exp(data)/(1 + exp(data)))
  
}

allButAverager <- function(data, terms, defaults) {
  
  if (!is.null(terms)) {
    otherSet <- select(data, -terms)
    termSet  <- select(data, terms)
  } else {
    otherSet <- data
  }
  
  for (i in 1:ncol(otherSet)) {
    if (is.numeric(otherSet[,i])) {
      otherSet[,i] <- mean(otherSet[,i])
    } else if (names(otherSet)[i] %in% names(defaults)) {
      otherSet[,i] <- defaults[names(otherSet)[i]]
    } else {
      otherSet[,i] <- otherSet[1,i]
    }
  }
  
  if (!is.null(terms)) {
    fullSet <- cbind(otherSet, termSet)
  } else {
    fullSet <- otherSet
  }
  
  return(fullSet)
}

varBin <- function(stdVariable, standardized = FALSE, originalVariable, varBins) {
  
  if (standardized == TRUE) {
    
    originalMean <- mean(originalVariable)
    originalSD   <- sd(originalVariable)
    
    Variable <- stdVariable*originalSD + originalMean
    
  } 
  
  lowBin  <- which(Variable < varBins[2])
  
  highBin <- which(varBins[2] <= Variable)
  
  Variable[lowBin]  <- varBins[1]
  Variable[highBin] <- varBins[2] 
  
  if (standardized == TRUE) {
    
    Variable <- (Variable - originalMean)/originalSD
    
  }
  
  return(Variable)
  
}

standardizer <- function(ValuesToStandardize, VariableName, OriginalData) {
  
  OriginalVariable <- OriginalData[,VariableName]
  
  meanOrg <- mean(OriginalVariable)
  sdOrg   <- sd(OriginalVariable)
  
  stdVar <- (ValuesToStandardize - meanOrg)/sdOrg
  
  return(stdVar)
  
}

reverseStandardize <- function(ValuesToReverse, VariableName, OriginalData) {
  OriginalVariable <- OriginalData[,VariableName]
  
  meanOrg <- mean(OriginalVariable)
  sdOrg   <- sd(OriginalVariable)
  
  revVar <- ValuesToReverse*sdOrg + meanOrg
  
  return(revVar)
  
}


### Predicting summer and autumn BCI for old and young badgers (assuming male, predominant testes condition),
### and autumn BCI for reproductive and non-reproductive female badgers, varying only the temperature in
### summer. We'll do this by predicting
### according to the relationship in our BCI models, then creating 1000 badgers' BCIs according to
### that as a mean and with the sd equal to the individual random effect sd. Then, we'll convert these
### BCIs to BCIResiduals and predict their survival probability.

## First, we'll set up single-row frames averaging out all rows and setting covariates to appropriate defaults

repFemaleFrame <- allButAverager(data = autumnobj$data, 
                                 terms = NULL,
                                 defaults = data.frame(Reproductive.female = "1",
                                                       seasonTestes = "reproductiveFemale",
                                                       descendedTestes = "0",
                                                       Sex = "Female"))[1,] %>%
  mutate(Season = "Autumn")
nonrepFemaleFrame <- allButAverager(data = autumnobj$data, 
                                    terms = NULL,
                                    defaults = data.frame(Reproductive.female = "0",
                                                          seasonTestes = "nonrepFemale",
                                                          descendedTestes = "0",
                                                          Sex = "Female"))[1,]  %>%
  mutate(Season = "Autumn")
oldBadgerFrameSum <- allButAverager(data = summerobj$data, 
                                    terms = NULL,
                                    defaults = data.frame(Reproductive.female = "0",
                                                          seasonTestes = "Descended",
                                                          descendedTestes = "1",
                                                          Sex = "Male"))[1,] %>%
  mutate(Season = "Summer")
youngBadgerFrameSum <- allButAverager(data = summerobj$data, 
                                      terms = NULL,
                                      defaults = data.frame(Reproductive.female = "0",
                                                            seasonTestes = "Descended",
                                                            descendedTestes = "1",
                                                            Sex = "Male"))[1,] %>%
  mutate(Season = "Summer")
oldBadgerFrameAut <- allButAverager(data = autumnobj$data, 
                                 terms = NULL,
                                 defaults = data.frame(Reproductive.female = "0",
                                                       seasonTestes = "Ascended",
                                                       descendedTestes = "0",
                                                       Sex = "Male"))[1,] %>%
  mutate(Season = "Autumn")
youngBadgerFrameAut <- allButAverager(data = autumnobj$data, 
                                   terms = NULL,
                                   defaults = data.frame(Reproductive.female = "0",
                                                         seasonTestes = "Ascended",
                                                         descendedTestes = "0",
                                                         Sex = "Male"))[1,] %>%
  mutate(Season = "Autumn")

# Standardizing age covariates by respective seasons:

oldBadgerFrameSum$Age <- standardizer(ValuesToStandardize = 9,
                                      VariableName = "Age",
                                      OriginalData = summerCompleteCondition)
oldBadgerFrameAut$Age <- standardizer(ValuesToStandardize = 9,
                                      VariableName = "Age",
                                      OriginalData = autumnCompleteCondition)
youngBadgerFrameSum$Age <- standardizer(ValuesToStandardize = 2,
                                        VariableName = "Age",
                                        OriginalData = summerCompleteCondition)
youngBadgerFrameAut$Age <- standardizer(ValuesToStandardize = 2,
                                        VariableName = "Age",
                                        OriginalData = autumnCompleteCondition)

# Putting them all together in two different dataframes:

summerInputs <- youngBadgerFrameSum %>%
  bind_rows(oldBadgerFrameSum)

autumnInputs <- repFemaleFrame %>%
  bind_rows(nonrepFemaleFrame,
            oldBadgerFrameAut,
            youngBadgerFrameAut)

## Setting up data frames to hold our BCI predictions and running a big loop to calculate them.

summerPredictionFrame <- data.frame()
autumnPredictionFrame <- data.frame()

yearsToLoop <- c(seq(1990, 2016), unique(Window_figures$Year))

for (i in yearsToLoop) {
  
  # First, we'll get standardized metrics for our summer temperature for each year (per projection):
  
  if (i < 2020) {
    
    summerWeather <- oldWindow_figures %>% 
      subset(year == i & Window == 30) %>%
      mutate(dryTemp30 = mean.temp.dry) %>%
      select(dryTemp30)
    
    summerStd <- summerWeather %>%
      mutate(dryTemp30 = standardizer(ValuesToStandardize = dryTemp30,
                                      VariableName = "dryTemp30",
                                      OriginalData = summerCompleteCondition),
             Year = i,
             Projection = "Real")
    
    autumnStd <- summerWeather %>%
      mutate(dryTemp30 = standardizer(ValuesToStandardize = dryTemp30,
                                      VariableName = "dryTemp30",
                                      OriginalData = autumnCompleteCondition),
             Year = i,
             Projection = "Real")
    
  } else {
    
    summerWeather <- Window_figures %>%
      subset(Year == i) %>% 
      mutate(dryTemp30 = mean.temp.dry) 
    
    summerStd <- summerWeather %>%
      mutate(dryTemp30 = standardizer(ValuesToStandardize = dryTemp30,
                                      VariableName = "dryTemp30",
                                      OriginalData = summerCompleteCondition)) %>%
      select(Year, Projection, dryTemp30)
    
    autumnStd <- summerWeather %>%
      mutate(dryTemp30 = standardizer(ValuesToStandardize = dryTemp30,
                                      VariableName = "dryTemp30",
                                      OriginalData = autumnCompleteCondition)) %>%
      select(Year, Projection, dryTemp30)
    
    
  } # Done making our standardized weather covariate
  
  summerInputs2 <- summerStd %>%
    merge(summerInputs %>% 
            select(-dryTemp30) %>%
            mutate(Year = i),
          by = "Year") # This should join them and create new rows for all combinations of projection
    
  autumnInputs2 <- autumnStd %>%
    merge(autumnInputs %>% 
            select(-dryTemp30) %>%
            mutate(Year = i),
          by = "Year")
  
  # Predicting by seasonal model:
  
  summerInputs2$Predicted <- predict(summerModelCondition$gam, 
                                     newdata = summerInputs2)
  autumnInputs2$Predicted <- predict(autumnModelCondition$gam,
                                     newdata = autumnInputs2)
  
  summerPredictionFrame <- summerPredictionFrame %>%
    bind_rows(summerInputs2)
  
  autumnPredictionFrame <- autumnPredictionFrame %>%
    bind_rows(autumnInputs2)
  
}

ggplot(summerPredictionFrame %>% subset(Projection %in% c("Real", "p1")), 
       aes(x = Year, y = I(reverseStandardize(ValuesToReverse = Predicted,
                                              VariableName = "BCI",
                                              OriginalData = summerCompleteCondition)), 
           colour = factor(Age))) + 
  geom_point() + geom_smooth() + facet_grid(.~factor(Age))

ggplot(autumnPredictionFrame %>% subset(Projection %in% c("Real", "p1") & 
                                          Sex == "Male"), 
       aes(x = Year, y = I(reverseStandardize(ValuesToReverse = Predicted,
                                              VariableName = "BCI",
                                              OriginalData = autumnCompleteCondition)), 
           colour = factor(Age))) + 
  geom_point() + geom_smooth() + facet_grid(.~factor(Age))

ggplot(autumnPredictionFrame %>% subset(Projection %in% c("Real", "p1") &
                                          Sex == "Female"), 
       aes(x = Year, y = I(reverseStandardize(ValuesToReverse = Predicted,
                                              VariableName = "BCI",
                                              OriginalData = autumnCompleteCondition)), 
           colour = seasonTestes)) + 
  geom_point() + geom_smooth() + facet_grid(.~seasonTestes)

#^All look like they worked okay

## Now, we'll take each prediction and use the individual random intercept to generate variable
## BCI values against which to use our survival model

# We'll have to get both models from outside, to get the random effect without heterogeneous variance:

load("Summer model without heterogeneous variance.RData")
load("Autumn model without heterogeneous variance.RData")

summerEffects <- VarCorr(summerMod$mer)
autumnEffects <- VarCorr(autumnMod$mer)

summerSD <- attr(summerEffects$Individual, "stddev")
autumnSD <- attr(autumnEffects$Individual, "stddev")

# Now, we'll run through summer first:

summerIndPredictions <- data.frame()

set.seed(9453278) #WildCRU in numpad

for (i in 1:nrow(summerPredictionFrame)) {
  
  newBCIs <- slice(summerPredictionFrame, rep(i, 1000)) 
  newBCIs$BCIPredicted <- rnorm(mean = newBCIs$Predicted,
                                sd = summerSD,
                                n = 1000)
  
  summerIndPredictions <- summerIndPredictions %>%
    bind_rows(newBCIs)
  
  if (i/75 == floor(i/75)) {
    print(paste(round(i/nrow(summerPredictionFrame), 2)*100, "% done.", sep = ""))
  }
  
}

# Now, autumn:

autumnIndPredictions <- data.frame()

for (i in 1:nrow(autumnPredictionFrame)) {
  
  newBCIs <- slice(autumnPredictionFrame, rep(i, 1000)) 
  newBCIs$BCIPredicted <- rnorm(mean = newBCIs$Predicted,
                                sd = autumnSD,
                                n = 1000)
  
  autumnIndPredictions <- autumnIndPredictions %>%
    bind_rows(newBCIs)
  
  if (i/75 == floor(i/75)) {
    print(paste(round(i/nrow(autumnPredictionFrame), 2)*100, "% done.", sep = ""))
  }
  
}

## Now, unstandardizing all the predicted individual BCIs:

summerIndPredictions <- summerIndPredictions %>%
  mutate(unstdBCIPredicted = reverseStandardize(ValuesToReverse = BCIPredicted,
                                                VariableName = "BCI",
                                                OriginalData = summerCompleteCondition))

autumnIndPredictions <- autumnIndPredictions %>%
  mutate(unstdBCIPredicted = reverseStandardize(ValuesToReverse = BCIPredicted,
                                                VariableName = "BCI",
                                                OriginalData = autumnCompleteCondition))

ggplot(summerIndPredictions %>% subset(Projection %in% c("Real", "p1")),
       aes(x = factor(Year), y = unstdBCIPredicted)) +
  geom_boxplot() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Male"),
       aes(x = factor(Year), y = unstdBCIPredicted)) +
  geom_boxplot() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Female"),
       aes(x = factor(Year), y = unstdBCIPredicted)) +
  geom_boxplot() +
  facet_grid(.~seasonTestes)

#^Looks logical

## Now, we have to calculate the residual of these BCI predictions, relative to
## day of year, age, and sex:

allCompleteCondition <- rbind(springCompleteCondition,
                              summerCompleteCondition, 
                              autumnCompleteCondition) %>%
  mutate(Season = factor(Season))

controlModel <- gam(BCI ~ s(dayOfYear) + s(Age) + Sex,
                    data = allCompleteCondition)

# We need residuals in our seasonal datasets so we can match standardization:

summerCompleteCondition <- summerCompleteCondition %>%
  mutate(BCIPredicted = predict(controlModel, 
                                newdata = summerCompleteCondition),
         BCIResiduals = BCI - BCIPredicted)

autumnCompleteCondition <- autumnCompleteCondition %>%
  mutate(BCIPredicted = predict(controlModel, 
                                newdata = autumnCompleteCondition),
         BCIResiduals = BCI - BCIPredicted)

# Now, we'll calculate the BCI predictions (by DOY, sex, and age) and residuals, standardized, for our predictions:

summerMeanDay <- mean(summerCompleteCondition$dayOfYear)
autumnMeanDay <- mean(autumnCompleteCondition$dayOfYear)

summerIndPredictions <- summerIndPredictions %>%
  mutate(unstdBCIPredicted2 = predict(controlModel,
                                      newdata = data.frame(Sex = Sex,
                                                           dayOfYear = summerMeanDay,
                                                           Age = reverseStandardize(ValuesToReverse = Age,
                                                                                    VariableName = "Age",
                                                                                    OriginalData = summerCompleteCondition))),
         unstdBCIResidual = unstdBCIPredicted - unstdBCIPredicted2,
         BCIResiduals = standardizer(ValuesToStandardize = unstdBCIResidual,
                                     VariableName = "BCIResiduals",
                                     OriginalData = summerCompleteCondition))

autumnIndPredictions <- autumnIndPredictions %>%
  mutate(unstdBCIPredicted2 = predict(controlModel,
                                      newdata = data.frame(Sex = Sex,
                                                           dayOfYear = autumnMeanDay,
                                                           Age = reverseStandardize(ValuesToReverse = Age,
                                                                                    VariableName = "Age",
                                                                                    OriginalData = autumnCompleteCondition))),
         unstdBCIResidual = unstdBCIPredicted - unstdBCIPredicted2,
         BCIResiduals = standardizer(ValuesToStandardize = unstdBCIResidual,
                                     VariableName = "BCIResiduals",
                                     OriginalData = autumnCompleteCondition))

ggplot(summerIndPredictions %>% subset(Projection %in% c("Real", "p1")),
       aes(x = factor(Year), y = unstdBCIPredicted2)) +
  geom_point() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Male"),
       aes(x = factor(Year), y = unstdBCIPredicted)) +
  geom_point() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Female"),
       aes(x = factor(Year), y = unstdBCIPredicted)) +
  geom_point() +
  facet_grid(.~seasonTestes)

ggplot(summerIndPredictions %>% subset(Projection %in% c("Real", "p1")),
       aes(x = factor(Year), y = BCIResiduals)) +
  geom_boxplot() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Male"),
       aes(x = factor(Year), y = BCIResiduals)) +
  geom_boxplot() +
  facet_grid(.~factor(Age))

ggplot(autumnIndPredictions %>% subset(Projection %in% c("Real", "p1") & Sex == "Female"),
       aes(x = factor(Year), y = BCIResiduals)) +
  geom_boxplot() +
  facet_grid(.~seasonTestes)

## Now, we combine with the appropriate seasonal mean covariates and predict survival (in logistic form):

summerForPrediction <- allButAverager(summerSurvobj$data,
                                      terms = NULL,
                                      defaults = NULL) %>%
  select(-Age, -Sex, -seasonTestes, -Reproductive.female, -testesDescended, -BCIResiduals) %>% 
  slice(rep(1, nrow(summerIndPredictions))) %>%
  bind_cols(summerIndPredictions %>%
              select(BCIResiduals, Age, Sex, seasonTestes, Reproductive.female, descendedTestes, Year, Projection)) %>%
  mutate(testesDescended = descendedTestes)

summerForPrediction$SurvivalPredicted <- predict(summerModelSurvival$gam, 
                                                 newdata = summerForPrediction)

autumnForPrediction <- allButAverager(autumnSurvobj$data,
                                      terms = NULL,
                                      defaults = NULL) %>%
  select(-Age, -Sex, -seasonTestes, -Reproductive.female, -testesDescended, -BCIResiduals) %>% 
  slice(rep(1, nrow(autumnIndPredictions))) %>%
  bind_cols(autumnIndPredictions %>%
              select(BCIResiduals, Age, Sex, seasonTestes, Reproductive.female, descendedTestes, Year, Projection)) %>%
  mutate(testesDescended = descendedTestes)

autumnForPrediction$SurvivalPredicted <- predict(autumnModelSurvival$gam, 
                                                 newdata = autumnForPrediction)



ggplot(summerForPrediction %>% subset(Projection %in% c("Real", "p1")), 
       aes(x = factor(Year), y = SurvivalPredicted)) + 
  geom_boxplot() + 
  facet_grid(.~factor(Age)) + 
  xlab("Year") + ylab("Survival probability")

ggplot(autumnForPrediction %>% subset(Projection %in% c("Real", "p1") & Sex == "Male"), 
       aes(x = factor(Year), y = SurvivalPredicted)) + 
  geom_boxplot() + 
  facet_grid(.~factor(Age)) + 
  xlab("Year") + ylab("Survival probability")

ggplot(autumnForPrediction %>% subset(Projection %in% c("Real", "p1") & Sex == "Female"), 
       aes(x = factor(Year), y = SurvivalPredicted)) + 
  geom_boxplot() + 
  facet_grid(.~seasonTestes) + 
  xlab("Year") + ylab("Survival probability")

## Okay, now that we have our predictions, we can do quantile regression on them in their untransformed
## state.

summerAge <- summerForPrediction 
autumnAge <- autumnForPrediction %>% subset(Sex == "Male")
autumnRep <- autumnForPrediction %>% subset(Sex == "Female")

projectionLevels <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12")

summerAgeModelFrame <- data.frame()
autumnAgeModelFrame <- data.frame()
autumnRepModelFrame <- data.frame()

for (i in projectionLevels) {
  
  ## Quantile regression on the bottom 10% of the summer age datasets:
  
  summerQuant <- summerAge %>% 
    subset(Projection %in% c(i, "Real")) %>%
    mutate(Projection = i,
           SurvivalPredicted = as.vector(SurvivalPredicted))
  
  # First, young badgers:
  
  summerQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = summerQuant %>% subset(Age < 0))
  
  summerAgeModelFrame <- summerAgeModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = summerQuantMod$coefficients[1],
                         Slope = summerQuantMod$coefficients[2],
                         Age = 2))
  
  # Now, old badgers:
  
  summerQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = summerQuant %>% subset(Age > 0))
  
  summerAgeModelFrame <- summerAgeModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = summerQuantMod$coefficients[1],
                         Slope = summerQuantMod$coefficients[2],
                         Age = 9))
  
  ## Next, we do the autumn age datasets:
  
  autumnQuant <- autumnAge %>% 
    subset(Projection %in% c(i, "Real")) %>%
    mutate(Projection = i, 
           SurvivalPredicted = as.vector(SurvivalPredicted))
  
  # First, young badgers:
  
  autumnQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = autumnQuant %>% subset(Age < 0))
  
  autumnAgeModelFrame <- autumnAgeModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = autumnQuantMod$coefficients[1],
                         Slope = autumnQuantMod$coefficients[2],
                         Age = 2))
  
  # Then, old badgers:
  
  autumnQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = autumnQuant %>% subset(Age > 0))
  
  autumnAgeModelFrame <- autumnAgeModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = autumnQuantMod$coefficients[1],
                         Slope = autumnQuantMod$coefficients[2],
                         Age = 9))
  
  ## Finally, we do the same thing for reproductive and non-reproductive females:
  
  autumnQuant <- autumnRep %>% 
    subset(Projection %in% c(i, "Real")) %>%
    mutate(Projection = i,
           SurvivalPredicted = as.vector(SurvivalPredicted))
  
  # First, nonreproductive badgers:
  
  autumnQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = autumnQuant %>% subset(seasonTestes == "nonrepFemale"))
  
  autumnRepModelFrame <- autumnRepModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = autumnQuantMod$coefficients[1],
                         Slope = autumnQuantMod$coefficients[2],
                         Rep.status = "Nonreproductive"))
  
  # Finally, reproductive females:
  
  autumnQuantMod <- rq(SurvivalPredicted ~ Year,
                       tau = 0.2,
                       data = autumnQuant %>% subset(seasonTestes == "reproductiveFemale"))
  
  autumnRepModelFrame <- autumnRepModelFrame %>%
    bind_rows(data.frame(Projection = i,
                         Intercept = autumnQuantMod$coefficients[1],
                         Slope = autumnQuantMod$coefficients[2],
                         Rep.status = "Reproductive"))
  
}

## Now, using those relationships to come up with predictions for each year between start of study
## and end of projections, then inverse-logistic-ing the predictions

Year <- seq(min(yearsToLoop), max(yearsToLoop))

# First, we'll do the summer age difference:

summerAgePredictions <- data.frame()

for (i in 1:nrow(summerAgeModelFrame)) {
  
  untransformedPredictions <- summerAgeModelFrame$Intercept[i] + summerAgeModelFrame$Slope[i]*Year
  
  transformedPredictions <- inv_logit(untransformedPredictions)
  
  summerAgePredictions <- summerAgePredictions %>%
    bind_rows(summerAgeModelFrame %>%
                slice(rep(i, length(Year))) %>%
                mutate(Year = Year,
                       Survival = transformedPredictions))
  
}

ggplot(summerAgePredictions, aes(x = Year, y = Survival, colour = Projection)) + 
  geom_line() + facet_grid(.~factor(Age))


# Next, autumn age differences:

autumnAgePredictions <- data.frame()

for (i in 1:nrow(autumnAgeModelFrame)) {
  
  untransformedPredictions <- autumnAgeModelFrame$Intercept[i] + autumnAgeModelFrame$Slope[i]*Year
  
  transformedPredictions <- inv_logit(untransformedPredictions)
  
  autumnAgePredictions <- autumnAgePredictions %>%
    bind_rows(autumnAgeModelFrame %>%
                slice(rep(i, length(Year))) %>%
                mutate(Year = Year,
                       Survival = transformedPredictions))
  
}

ggplot(autumnAgePredictions, aes(x = Year, y = Survival, colour = Projection)) + 
  geom_line() + facet_grid(.~factor(Age))

# Finally, autumn reproductive differences:

autumnRepPredictions <- data.frame()

for (i in 1:nrow(autumnRepModelFrame)) {
  
  untransformedPredictions <- autumnRepModelFrame$Intercept[i] + autumnRepModelFrame$Slope[i]*Year
  
  transformedPredictions <- inv_logit(untransformedPredictions)
  
  autumnRepPredictions <- autumnRepPredictions %>%
    bind_rows(autumnRepModelFrame %>%
                slice(rep(i, length(Year))) %>%
                mutate(Year = Year,
                       Survival = transformedPredictions))
  
}

ggplot(autumnRepPredictions, aes(x = Year, y = Survival, colour = Projection)) + 
  geom_line() + facet_grid(.~Rep.status)

## Summaries: 

# Difference in summer age:

summerOldMean <- ((summerAgePredictions %>% 
                     subset(Year == 2080 & Age == 9) %>% 
                     select(Survival) %>% 
                     pull() %>% mean()) - 
                    (summerAgePredictions %>% 
                       subset(Year == 2020 & Age == 9) %>%
                       select(Survival) %>%
                       pull() %>% mean()))*100

summerOldMin <- ((summerAgePredictions %>% 
                     subset(Year == 2080 & Age == 9) %>% 
                     select(Survival) %>% 
                     pull() %>% min()) - 
                    (summerAgePredictions %>% 
                       subset(Year == 2020 & Age == 9) %>%
                       select(Survival) %>%
                       pull() %>% max()))*100

summerOldMax <- ((summerAgePredictions %>% 
                    subset(Year == 2080 & Age == 9) %>% 
                    select(Survival) %>% 
                    pull() %>% max()) - 
                   (summerAgePredictions %>% 
                      subset(Year == 2020 & Age == 9) %>%
                      select(Survival) %>%
                      pull() %>% min()))*100

summerYngMean <- ((summerAgePredictions %>% 
                     subset(Year == 2080 & Age == 2) %>% 
                     select(Survival) %>% 
                     pull() %>% mean()) - 
                    (summerAgePredictions %>% 
                       subset(Year == 2020 & Age == 2) %>%
                       select(Survival) %>%
                       pull() %>% mean()))*100

summerYngMin <- ((summerAgePredictions %>% 
                    subset(Year == 2080 & Age == 2) %>% 
                    select(Survival) %>% 
                    pull() %>% max()) - 
                   (summerAgePredictions %>% 
                      subset(Year == 2020 & Age == 2) %>%
                      select(Survival) %>%
                      pull() %>% min()))*100

summerYngMax <- ((summerAgePredictions %>% 
                    subset(Year == 2080 & Age == 2) %>% 
                    select(Survival) %>% 
                    pull() %>% min()) - 
                   (summerAgePredictions %>% 
                      subset(Year == 2020 & Age == 2) %>%
                      select(Survival) %>%
                      pull() %>% max()))*100

# Difference in autumn age:

autumnOldMean <- ((autumnAgePredictions %>% 
                     subset(Year == 2080 & Age == 9) %>% 
                     select(Survival) %>% 
                     pull() %>% mean()) - 
                    (autumnAgePredictions %>% 
                       subset(Year == 2020 & Age == 9) %>%
                       select(Survival) %>%
                       pull() %>% mean()))*100

autumnOldMin <- ((autumnAgePredictions %>% 
                    subset(Year == 2080 & Age == 9) %>% 
                    select(Survival) %>% 
                    pull() %>% max()) - 
                   (autumnAgePredictions %>% 
                      subset(Year == 2020 & Age == 9) %>%
                      select(Survival) %>%
                      pull() %>% min()))*100

autumnOldMax <- ((autumnAgePredictions %>% 
                    subset(Year == 2080 & Age == 9) %>% 
                    select(Survival) %>% 
                    pull() %>% min()) - 
                   (autumnAgePredictions %>% 
                      subset(Year == 2020 & Age == 9) %>%
                      select(Survival) %>%
                      pull() %>% max()))*100

# Difference in autumn rep status:

autumnRepMean <- ((autumnRepPredictions %>% 
                     subset(Year == 2080 & Rep.status == "Reproductive") %>% 
                     select(Survival) %>% 
                     pull() %>% mean()) - 
                    (autumnRepPredictions %>% 
                       subset(Year == 2020 & Rep.status == "Reproductive") %>%
                       select(Survival) %>%
                       pull() %>% mean()))*100

autumnRepMin <- ((autumnRepPredictions %>% 
                    subset(Year == 2080 & Rep.status == "Reproductive") %>% 
                    select(Survival) %>% 
                    pull() %>% max()) - 
                   (autumnRepPredictions %>% 
                      subset(Year == 2020 & Rep.status == "Reproductive") %>%
                      select(Survival) %>%
                      pull() %>% min()))*100

autumnRepMax <- ((autumnRepPredictions %>% 
                    subset(Year == 2080 & Rep.status == "Reproductive") %>% 
                    select(Survival) %>% 
                    pull() %>% min()) - 
                   (autumnRepPredictions %>% 
                      subset(Year == 2020 & Rep.status == "Reproductive") %>%
                      select(Survival) %>%
                      pull() %>% max()))*100

autumnNonMean <- ((autumnRepPredictions %>% 
                    subset(Year == 2080 & Rep.status == "Nonreproductive") %>% 
                    select(Survival) %>% 
                    pull() %>% mean()) - 
                   (autumnRepPredictions %>% 
                      subset(Year == 2020 & Rep.status == "Nonreproductive") %>%
                      select(Survival) %>%
                      pull() %>% mean()))*100

autumnNonMin <- ((autumnRepPredictions %>% 
                    subset(Year == 2080 & Rep.status == "Nonreproductive") %>% 
                    select(Survival) %>% 
                    pull() %>% min()) - 
                   (autumnRepPredictions %>% 
                      subset(Year == 2020 & Rep.status == "Nonreproductive") %>%
                      select(Survival) %>%
                      pull() %>% max()))*100

autumnNonMax <- ((autumnRepPredictions %>% 
                    subset(Year == 2080 & Rep.status == "Nonreproductive") %>% 
                    select(Survival) %>% 
                    pull() %>% max()) - 
                   (autumnRepPredictions %>% 
                      subset(Year == 2020 & Rep.status == "Nonreproductive") %>%
                      select(Survival) %>%
                      pull() %>% min()))*100

## Getting average temperature change ##

Window_figures %>% 
  bind_rows(oldWindow_figures %>% 
              subset(Window == 30 & year > 1989) %>% 
              mutate(Year = 2020,
                     mean.temp.dry = mean(mean.temp.dry)) %>%
              distinct(Year, .keep_all = TRUE) %>%
              slice(rep(1, 12)) %>%
              mutate(Projection = paste("p", seq(1, 12), sep = "")) %>%
              select(Year, mean.temp.dry, Projection)) %>%
  group_by(Projection) %>%
  mutate(Change = (mean(mean.temp.dry[Year > 2074]) - mean.temp.dry[Year == 2020])) %>%
  ungroup() %>%
  subset(Year == 2080) %>%
  select(Change) %>%
  pull() %>%
  mean()

## Getting mean values 1990-2020 for different categories

mean2sum <- round((summerAgePredictions %>%
                     subset(Year < 2021 & Age == 2) %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

mean9sum <- round((summerAgePredictions %>%
                     subset(Year < 2021 & Age == 9) %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

mean2aut <- round((autumnAgePredictions %>%
                     subset(Year < 2021 & Age == 2) %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

mean9aut <- round((autumnAgePredictions %>%
                     subset(Year < 2021 & Age == 9) %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

meanRaut <- round((autumnRepPredictions %>%
                     subset(Year < 2021 & Rep.status == "Reproductive") %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

meanNaut <- round((autumnRepPredictions %>%
                     subset(Year < 2021 & Rep.status == "Nonreproductive") %>%
                     select(Survival) %>%
                     pull() %>% mean())*100, 1) %>%
  paste("%", sep = "")

## Making plot dataframe

totalPredictions <- summerAgePredictions %>%
  mutate(Metric = paste("Age", Age, "Summer"),
         Season = "Summer") %>%
  bind_rows(autumnAgePredictions %>%
              mutate(Metric = paste("Age", Age, "Autumn"),
                     Season = "Autumn"), 
            autumnRepPredictions %>%
              mutate(Metric = paste(Rep.status, "Autumn"),
                     Season = "Autumn")) %>%
  group_by(Metric, Projection) %>%
  mutate(Change = (Survival[Year == 2080] - Survival[Year == 2020])*100) %>%
  ungroup() %>%
  mutate(Metric = factor(Metric, levels = c("Age 2 Summer", "Age 9 Summer",
                                            "Age 2 Autumn", "Age 9 Autumn",
                                            "Nonreproductive Autumn", "Reproductive Autumn")))

totalPredictions <- totalPredictions %>%
  bind_rows(data.frame(Label = c(mean2sum, mean9sum, 
                                 mean2aut, mean9aut, 
                                 meanNaut, meanRaut),
                       Metric = c("Age 2 Summer",
                                  "Age 9 Summer",
                                  "Age 2 Autumn",
                                  "Age 9 Autumn",
                                  "Nonreproductive Autumn",
                                  "Reproductive Autumn"),
                       Change = c(0.25, -0.25, -0.25, 0.25, -0.25, 0.25),
                       Season = c("Summer", "Summer",
                                  "Autumn", "Autumn",
                                  "Autumn", "Autumn")))

percChangeFig <- ggplot(totalPredictions %>% subset(is.na(Label)), 
       aes(x = Metric, y = Change, color = Season)) + 
  geom_boxplot() + 
  xlab("Population subgroup") + 
  ylab("Survival probability change 2020-2080\nfor lowest 20% of BCI scores") + 
  geom_text(data = totalPredictions %>% subset(!is.na(Label)),
            aes(label = Label, color = NULL)) + 
  scale_x_discrete(breaks = c("Age 2 Summer","Age 9 Summer","Age 2 Autumn",
                              "Age 9 Autumn", "Nonreproductive Autumn", "Reproductive Autumn"),
                   labels = c("Age 2", "Age 9", "Age 2", "Age 9",
                              "Non-reproductive\nFemale", "Reproductive\nFemale")) +
  scale_y_continuous(breaks = seq(-4, 1),
                     labels = c("-4%", "-3%", "-2%", "-1%", "0%", "1%")) +
  theme_bw() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(breaks = c("Autumn", "Summer"), 
                     values = c(color.autumn, color.summer))+
  guides(color = FALSE)

save_plot(percChangeFig, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Projection changes.png",
          base_width = 6, base_height = 3.75,
          dpi = 600)

## Alternative percentage change figure ##

altFrame <- totalPredictions %>% subset(is.na(Label)) %>%
  group_by(Age, Season, Metric) %>%
  mutate(Mean = mean(Change),
         SD   = sd(Change),
         Add  = "One level") %>%
  ungroup() %>%
  distinct(Age, Season, Metric, .keep_all = TRUE)

fill.autumn <- "#ffffff"
fill.summer <- "#ffffff"

color.summer <- "#9C9C3A"
color.autumn <- "#a85d01"

altPercFig <- ggplot(altFrame, aes(x = Metric, y = Mean, fill = Season, colour = Season)) + 
  geom_col(width = 0.5) + 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),
                width = 0.1) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() + 
  scale_fill_manual(breaks = c("Autumn", "Summer"), 
                    values = c(fill.autumn, fill.summer)) + 
  scale_color_manual(breaks = c("Autumn", "Summer"),
                     values = c(color.autumn, color.summer)) +
  scale_x_discrete(breaks = c("Age 2 Summer","Age 9 Summer","Age 2 Autumn",
                              "Age 9 Autumn", "Nonreproductive Autumn", "Reproductive Autumn"),
                   labels = c("Age 2", "Age 9", "Age 2", "Age 9",
                              "Non-reproductive\nFemale", "Reproductive\nFemale")) + 
  scale_y_continuous(breaks = seq(-4, 1),
                     labels = c("-4%", "-3%", "-2%", "-1%", "0%", "1%")) +
  guides(colour = FALSE,
         fill   = FALSE) + 
  xlab("Population subgroup") + 
  ylab("Survival probability change 2020-2080\nfor lowest 20% of BCI scores")

save_plot(altPercFig, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger condition/Manuscript/Figures/Projection changes_alternative.png",
          base_width = 6, base_height = 3.75,
          dpi = 600)
