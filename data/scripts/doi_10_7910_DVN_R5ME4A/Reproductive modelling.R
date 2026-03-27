#######################################################
# Title: Modelling reproduction as a function of 
# autumnal body condition
# Name: Julius G. Bright Ross
# Date: Mar 5, 2020
# Last updated: May 28, 2020
# Description: Modelling reproduction in the spring
# as a function of condition in the previous autumn
#######################################################

## Load in data and required packages ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("Condition modelling output.RData")

library(mgcv)        #For building GAM/GAMM models
library(tidyverse)   #For many reasons
library(corrplot)    #To examine correlations between covariates
library(standardize) #To standardize covariates
library(sjPlot)      #To visualise relationships
library(cowplot)     #To build multi-panel figures
library(emmeans)     #To test effects between fat levels

repDict <- list()

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


### Getting one complete modelling dataframe ###

allCompleteCondition <- conditionSurvival %>% 
  subset(Age > 0 & !is.na(BCI) & !is.na(totalN))

### Getting residuals after controlling for sex, age, and day of year ###

controlModel <- gam(BCI ~ s(dayOfYear) + s(Age) + Sex,
                    data = allCompleteCondition)

allCompleteCondition <- allCompleteCondition %>%
  mutate(BCIPredicted = predict(controlModel),
         BCIResiduals = BCI - BCIPredicted) %>%
  mutate_if(is.matrix, as.vector)

### Standardizing ###

autumnCompleteCondition <- subset(allCompleteCondition, 
                                  Sex == "Female" & Season == "Autumn" & 
                                    !is.na(Reproduced.next) & !is.na(Reproductive.female)) %>%
  mutate(Fat.score = factor(Fat.score))

autumnobj <-  standardize(Reproduced.next ~ Age + BCIResiduals +
                            winterTemp + winterRain + 
                            winterSigma + winterCV + 
                            nextSpringTemp + nextSpringRain +
                            nextSpringSigma + nextSpringCV +
                            totalN + Reproductive.female,
                          data = autumnCompleteCondition,
                          family = binomial)

continuousVars <- setdiff(names(autumnobj$data), c("Reproduced.next", "Reproductive.female"))
corrplot(cor(autumnobj$data[,continuousVars]), method = "number")
#Worth trying a PCA

weather.pca <- prcomp(autumnobj$data[,c("winterTemp", "winterRain", "winterSigma", "winterCV",
                                        "nextSpringTemp", "nextSpringRain", "nextSpringSigma", "nextSpringCV")])

weather.summary <- summary(weather.pca)

dict$weather.pca.cumvar   <- 100*round(unname(weather.summary$importance[,4][3]), dict$rounding)

autumnobj$data$pc1    <- weather.pca$x[,1]
autumnobj$data$pc2    <- weather.pca$x[,2]
autumnobj$data$pc3    <- weather.pca$x[,3]
autumnobj$data$pc4    <- weather.pca$x[,4]

autumnobj$data$Sett <- autumnCompleteCondition$Sett

autumnobj$data <- autumnobj$data %>%
  mutate_if(is.matrix, as.vector)

autumnobj$formula1 <- update(autumnobj$formula, 
                             ~ . - Age - BCIResiduals +
                               s(Age) + s(BCIResiduals) +
                               BCIResiduals:totalN + 
                               BCIResiduals:Age + 
                               BCIResiduals:Reproductive.female -
                               winterTemp - winterRain - winterSigma - winterCV -
                               nextSpringTemp - nextSpringRain - nextSpringSigma - nextSpringCV +
                               pc1 + pc2 + pc3 + pc4)

### Modelling ###

repModelGlobal <- gamm(formula = autumnobj$formula1,
                       data    = autumnobj$data,
                       random  = list(Sett = ~1),
                       family  = binomial)

plot_model(repModelGlobal$gam, type = "pred", terms = c("BCIResiduals"))
plot_model(repModelGlobal$gam, type = "pred", terms = c("pc3"))
plot_model(repModelGlobal$gam, type = "pred", terms = c("Reproductive.female"))

## Saving some numbers ##

repSum <- summary(repModelGlobal$gam)

repNp <- repSum$p.pv[which(names(repSum$p.pv) == "totalN")]
repDict$repNp <- pStore(repNp)

repPred <- allButAverager(data     = autumnobj$data,
                          terms    = c("Reproductive.female"),
                          defaults = NULL)

repPredictions <- predict(repModelGlobal$gam,
                             newdata = repPred,
                             se.fit = TRUE)

repPred$predictions <- inv_logit(repPredictions$fit)
repPred$upper       <- inv_logit(repPredictions$fit + repPredictions$se.fit)
repPred$lower       <- inv_logit(repPredictions$fit - repPredictions$se.fit)

maxRep <- subset(repPred, Reproductive.female == "1")[1,]
minRep <- subset(repPred, Reproductive.female == "0")[1,]

repDict$rep.nonrep.diff <- round(100*(maxRep$predictions - minRep$predictions))

repDict$rep.nonrep.maxdiff <- round(100*(maxRep$upper - minRep$lower))
repDict$rep.nonrep.mindiff <- round(100*(maxRep$lower - minRep$upper))

### Fat model ###

autumnFatCondition <- autumnCompleteCondition %>%
  subset(!is.na(Fat.score))

autumnfatobj <- standardize(Reproduced.next ~ Age + Fat.score +
                              Reproductive.female +
                              winterTemp + winterRain + 
                              winterSigma + winterCV + 
                              nextSpringTemp + nextSpringRain +
                              nextSpringSigma + nextSpringCV +
                              totalN,
                            data = autumnFatCondition,
                            family = binomial)

continuousVars <- setdiff(names(autumnfatobj$data), c("Reproduced.next", "Fat.score", "Reproductive.female"))
corrplot(cor(autumnfatobj$data[,continuousVars]), method = "number")

# Just applying another PCA

weather.pca.fat <- prcomp(autumnfatobj$data[,c("winterTemp", "winterRain", "winterSigma", "winterCV",
                                        "nextSpringTemp", "nextSpringRain", "nextSpringSigma", "nextSpringCV")])

weather.fat.summary <- summary(weather.pca.fat)

autumnfatobj$data$pc1    <- weather.pca.fat$x[,1]
autumnfatobj$data$pc2    <- weather.pca.fat$x[,2]
autumnfatobj$data$pc3    <- weather.pca.fat$x[,3]
autumnfatobj$data$pc4    <- weather.pca.fat$x[,4]

autumnfatobj$data$Sett <- autumnFatCondition$Sett

autumnfatobj$data <- autumnfatobj$data %>%
  mutate_if(is.matrix, as.vector)

autumnfatobj$formula1 <- update(autumnobj$formula1, 
                             ~ . - s(BCIResiduals) -Age:BCIResiduals - 
                               BCIResiduals:totalN - BCIResiduals:Reproductive.female +
                               Fat.score)

### Modelling ###

repModelGlobalFat <- gamm(formula = autumnfatobj$formula1,
                          data    = autumnfatobj$data,
                          random  = list(Sett = ~1),
                          family  = binomial)

plot_model(repModelGlobalFat$gam, type = "pred", terms = "Fat.score")

repModelGlobalFat$gam$call <- quote(gamm(formula = autumnfatobj$formula1,
                                         data    = autumnfatobj$data,
                                         random  = list(Sett = ~1),
                                         family  = binomial))

meansFat <- summary(emmeans(repModelGlobalFat, specs = pairwise ~ Fat.score))$contrasts

repDict$fat.2.4.p <- pStore(meansFat$p.value[which(meansFat$contrast == "2 - 4")])

## Getting differences between fat levels ##

repModelGlobalFat$gam$call <- quote(gamm(formula = autumnfatobj$formula1,
                                         data    = autumnfatobj$data,
                                         random  = list(Sett = ~1),
                                         family  = binomial))

meansRepFat <- summary(emmeans(repModelGlobalFat, specs = pairwise ~ Fat.score))$contrasts

### Making figure with survival plots as well ###

bciPred <- allButAverager(data     = autumnobj$data,
                          terms    = c("BCIResiduals"),
                          defaults = data.frame(Sex = "Male")) 

bciRepPredM <- predict(repModelGlobal$gam, 
                       newdata = bciPred, 
                       se.fit = TRUE)

bciPred <- allButAverager(data     = bciPred,
                          terms    = c("BCIResiduals"),
                          defaults = data.frame(Sex = "Female"))

bciRepPredF <- predict(repModelGlobal$gam, 
                       newdata = bciPred, 
                       se.fit = TRUE)

bciPred$predictions <- (bciRepPredM$fit + bciRepPredF$fit)/2
bciPred$upper <- bciPred$predictions + 1.96*(bciRepPredM$se.fit + bciRepPredF$se.fit)/2
bciPred$lower <- bciPred$predictions - 1.96*(bciRepPredM$se.fit + bciRepPredF$se.fit)/2

bciRepFig <- ggplot(bciPred, aes(x = BCIResiduals, y = I(inv_logit(predictions)))) + 
  geom_point() +
  geom_ribbon(aes(ymax = I(inv_logit(upper)), 
                  ymin = I(inv_logit(lower))), 
              alpha = 0.2) + 
  ylab("Reproduction probability") + xlab("Age-, sex-, and calendar-\ncorrected autumn BCI") + 
  theme_bw() +
  ylim(c(0, 1)) 

fatPred <- allButAverager(data     = autumnfatobj$data,
                          terms    = c("Fat.score"),
                          defaults = data.frame(Sex = "Male")) 

fatRepPredM <- predict(repModelGlobalFat$gam, 
                       newdata = fatPred, 
                       se.fit = TRUE)

fatPred <- allButAverager(data     = fatPred,
                          terms    = c("Fat.score"),
                          defaults = data.frame(Sex = "Female"))

fatRepPredF <- predict(repModelGlobalFat$gam, 
                       newdata = fatPred, 
                       se.fit = TRUE)

fatPred$predictions <- (fatRepPredM$fit + fatRepPredF$fit)/2
fatPred$upper <- fatPred$predictions + 1.96*(fatRepPredM$se.fit + fatRepPredF$se.fit)/2
fatPred$lower <- fatPred$predictions - 1.96*(fatRepPredM$se.fit + fatRepPredF$se.fit)/2

fatRepFig <- ggplot(fatPred %>% distinct(Fat.score, .keep_all = TRUE), 
                           aes(x = Fat.score, y = I(inv_logit(predictions)))) + 
  geom_point() + 
  geom_errorbar(aes(ymin = I(inv_logit(lower)),
                      ymax = I(inv_logit(upper))),
                width = 0.2) +
  ylab("Reproduction probability") + xlab("Autumn fat score") +
  theme_bw() +
  ylim(c(0, 1))

## Fusing with survival plots into one big figure ##

load("Survival plots.RData")

fitnessFigure <- plot_grid(springSurvAgeFig, springSurvFatFig,
                           summerSurvAgeFig, summerSurvFatFig,
                           autumnSurvAgeFig, autumnSurvFatFig,
                           bciRepFig, fatRepFig,
                           nrow = 4,
                           ncol = 2,
                           align = "hv")

save(fitnessFigure, file = "Fitness figure.RData")

## Saving models for table creation elsewhere ##

save(repModelGlobal, repModelGlobalFat, file = "Reproductive models.RData")

## Saving terms we'd like to reference in text ##

save(repDict, file = "Reproductive numbers.RData")
