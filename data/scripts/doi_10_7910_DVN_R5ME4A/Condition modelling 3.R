#######################################################
# Title: Modelling individual body condition in each
# season based on environmental drivers
# Name: Julius G. Bright Ross
# Date: Feb 8, 2020
# Last updated: Jun 12, 2020; Jun 5, 2021 for figure
# tweaks
# Description: Modelling each season's body condition
# as a function of a series of individual and 
# population-wide metrics; version 2 incorporates 
# male testes condition and female reproductive 
# status into the autumn model; version 3 incorporates
# interaction between descended testes and age
#######################################################

## Preparing workspace ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("ConditionFrame.RData")

library(mgcv)        #For building GAM/GAMM models
library(tidyverse)   #For many reasons
library(corrplot)    #To examine correlations between covariates
library(standardize) #To standardize covariates
library(lubridate)   #For month extraction
library(sjPlot)      #For model examination
library(cowplot)     #For plot_grid
library(emmeans)     #To compare all factor levels

# Useful functions #

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

customPlotter <- function(model, term, modelData, originalData, response) {
  constructionSet <- modelData %>% select(-c(names(modelData)[grepl(term, names(modelData))]))
  numericTracker  <- rep(NA, ncol(constructionSet))
  
  for (i in 1:ncol(constructionSet)) {
    numericTracker[i] <- is.numeric(constructionSet[,i])
    
    if (numericTracker[i] == TRUE) {
      constructionSet[,i] <- mean(constructionSet[,i])
    } else {
      constructionSet[,i] <- constructionSet[1,i]
    }
  }
  
  predictionSet <- constructionSet %>% 
    cbind(modelData %>% select(c(names(modelData)[grepl(term, names(modelData))])))
  
  predictions <- predict(model, newdata = predictionSet, type = "response", se.fit = TRUE)
  
  predictionSet <- predictionSet %>%
    mutate(Prediction = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit),
           Upper      = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit + 1.96*predictions$se.fit),
           Lower      = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit - 1.96*predictions$se.fit),
           ProperX    = reverseStandardize(variable = originalData[, term],
                                           toReverse = predictionSet[, term]))
  
  plottedPredictions <- ggplot(predictionSet, 
                               aes(x = ProperX, 
                                   y = Prediction)) + 
    geom_line() +
    geom_ribbon(aes(ymax = Upper, ymin = Lower), 
                alpha = 0.1) + 
    xlab(term) +
    ylab(response)
  
  
  return(plottedPredictions)
}

bciInterpreter <- function(model, modelData, originalData, term, stat) {
  constructionSet <- modelData %>% select(-c(names(modelData)[grepl(term, names(modelData))]))
  numericTracker  <- rep(NA, ncol(constructionSet))
  
  for (i in 1:ncol(constructionSet)) {
    numericTracker[i] <- is.numeric(constructionSet[,i])
    
    if (numericTracker[i] == TRUE) {
      constructionSet[,i] <- mean(constructionSet[,i])
    } else {
      constructionSet[,i] <- constructionSet[1,i]
    }
  }
  
  predictionSet <- constructionSet %>% 
    cbind(modelData %>% select(c(names(modelData)[grepl(term, names(modelData))])))
  
  predictions <- predict(model, newdata = predictionSet, type = "response", se.fit = TRUE)
  
  predictionSet <- predictionSet %>%
    mutate(Prediction = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit),
           Upper      = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit + 1.96*predictions$se.fit),
           Lower      = reverseStandardize(variable = originalData$BCI,
                                           toReverse = predictions$fit - 1.96*predictions$se.fit),
           ProperX    = reverseStandardize(variable = originalData[, term],
                                           toReverse = predictionSet[, term]))
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

## Setting up universal colors ##

dict$color.lowrain  <- "#079b9b"
dict$color.highrain <- "#051D3A"
dict$color.2        <- "#2E9388"
dict$color.9        <- "#5E1B16"
dict$color.spring   <- "#71BCAF"
dict$color.summer   <- "#9C9C3A"
dict$color.autumn   <- "#AC499B"

## Subsetting to adults only ##

adultFrame <- subset(conditionSurvival, Age > 0)

## Checking correlation of fat score and BCI ##

checkFrame <- adultFrame %>% 
  subset(!is.na(Fat.score) & !is.na(BCI)) %>%
  group_by(Individual) %>%
  mutate(AvgBCI = mean(BCI),
         AvgFat = mean(Fat.score)) %>%
  ungroup() %>%
  mutate(Fat.score = factor(Fat.score))

checkModel  <- lm(BCI ~ Fat.score, data = checkFrame)
checkModel2 <- lm(Fat.score %>% as.numeric() ~ BCI, data = checkFrame)

dict$fatToBCIp      <- pStore(summary(checkModel2)$coefficients[2,4])
dict$fatToBCIr2     <- round(summary(checkModel2)$r.squared, dict$rounding)
dict$first.fat.year <- min(checkFrame$Year)
dict$last.fat.year  <- max(checkFrame$Year)

# ggplot(checkFrame, aes(x = Fat.score, y = BCI)) + geom_boxplot() + xlab("Fat score")
# ggplot(checkFrame %>%
#          distinct(Individual, .keep_all = TRUE), aes(x = AvgBCI, y = AvgFat)) + 
#   geom_jitter()

indCheckModel <- lm(AvgBCI ~ AvgFat, 
                    data = checkFrame %>% 
                      distinct(Individual, .keep_all = TRUE))

### Simple a-seasonal model and metrics ##

validSeasons <- subset(adultFrame, 100 < dayOfYear & dayOfYear < 345)

overallModel <- gamm(BCI ~ s(Age, by = Sex) + s(dayOfYear, by = Sex) + Sex, 
                     data = validSeasons,
                     random = list(Individual = ~1))

latestDayOfYear <- max(validSeasons$dayOfYear)
latestDate <- validSeasons$Date[which(validSeasons$dayOfYear == latestDayOfYear)[1]]

dict$last.date.trapped <- paste(months(latestDate), day(latestDate))

weightComp <- validSeasons[complete.cases(validSeasons$Weight),]
bciComp    <- validSeasons[complete.cases(validSeasons$BCI),]

simpleModelWeight <- lm(Weight ~ Sex*Season, 
                        data = weightComp)

simpleModelBCI    <- lm(BCI ~ Sex*Season, 
                        data = bciComp)

dict$last.bcimod.year  <- max(bciComp$Year)
dict$first.bcimod.year <- min(bciComp$Year)

weightPredictions    <- predict(simpleModelWeight, se.fit = T)
conditionPredictions <- predict(simpleModelBCI, se.fit = T)

sprWeight <- c(weightPredictions$fit[which(weightComp$Season == "Spring" & weightComp$Sex == "Female")][1],
               weightPredictions$fit[which(weightComp$Season == "Spring" & weightComp$Sex == "Male")][1])
sumWeight <- c(weightPredictions$fit[which(weightComp$Season == "Summer" & weightComp$Sex == "Female")][1],
               weightPredictions$fit[which(weightComp$Season == "Summer" & weightComp$Sex == "Male")][1])
autWeight <- c(weightPredictions$fit[which(weightComp$Season == "Autumn" & weightComp$Sex == "Female")][1],
               weightPredictions$fit[which(weightComp$Season == "Autumn" & weightComp$Sex == "Male")][1])
sprFat    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Spring") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE),
               validSeasons %>% subset(Sex == "Male" & Season == "Spring") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE))
sumFat    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Summer") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE),
               validSeasons %>% subset(Sex == "Male" & Season == "Summer") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE))
autFat    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Autumn") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE),
               validSeasons %>% subset(Sex == "Male" & Season == "Autumn") %>% select(Fat.score) %>% pull() %>% mean(na.rm = TRUE))
sprBCI    <- c(conditionPredictions$fit[which(bciComp$Season == "Spring" & bciComp$Sex == "Female")][1],
               conditionPredictions$fit[which(bciComp$Season == "Spring" & bciComp$Sex == "Male")][1])
sumBCI    <- c(conditionPredictions$fit[which(bciComp$Season == "Summer" & bciComp$Sex == "Female")][1],
               conditionPredictions$fit[which(bciComp$Season == "Summer" & bciComp$Sex == "Male")][1])
autBCI    <- c(conditionPredictions$fit[which(bciComp$Season == "Autumn" & bciComp$Sex == "Female")][1],
               conditionPredictions$fit[which(bciComp$Season == "Autumn" & bciComp$Sex == "Male")][1])

sprWeightSD <- c(sd(weightComp$Weight[which(weightComp$Season == "Spring" & weightComp$Sex == "Female")]),
                 sd(weightComp$Weight[which(weightComp$Season == "Spring" & weightComp$Sex == "Male")]))
sumWeightSD <- c(sd(weightComp$Weight[which(weightComp$Season == "Summer" & weightComp$Sex == "Female")]),
                 sd(weightComp$Weight[which(weightComp$Season == "Summer" & weightComp$Sex == "Male")]))
autWeightSD <- c(sd(weightComp$Weight[which(weightComp$Season == "Autumn" & weightComp$Sex == "Female")]),
                 sd(weightComp$Weight[which(weightComp$Season == "Autumn" & weightComp$Sex == "Male")]))
sprFatSD    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Spring") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE),
                 validSeasons %>% subset(Sex == "Male" & Season == "Spring") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE))
sumFatSD    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Summer") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE),
                 validSeasons %>% subset(Sex == "Male" & Season == "Summer") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE))
autFatSD    <- c(validSeasons %>% subset(Sex == "Female" & Season == "Autumn") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE),
                 validSeasons %>% subset(Sex == "Male" & Season == "Autumn") %>% select(Fat.score) %>% pull() %>% sd(na.rm = TRUE))
sprBCISD    <- c(sd(bciComp$BCI[which(bciComp$Season == "Spring" & bciComp$Sex == "Female")]),
                 sd(bciComp$BCI[which(bciComp$Season == "Spring" & bciComp$Sex == "Male")]))
sumBCISD    <- c(sd(bciComp$BCI[which(bciComp$Season == "Summer" & bciComp$Sex == "Female")]),
                 sd(bciComp$BCI[which(bciComp$Season == "Summer" & bciComp$Sex == "Male")]))
autBCISD    <- c(sd(bciComp$BCI[which(bciComp$Season == "Autumn" & bciComp$Sex == "Female")]),
                 sd(bciComp$BCI[which(bciComp$Season == "Autumn" & bciComp$Sex == "Male")]))

dict$spr.avg.m <- round(sprBCI[2], 3)
dict$spr.avg.f <- round(sprBCI[1], 3)
dict$sum.avg.m <- round(sumBCI[2], 3)
dict$sum.avg.f <- round(sumBCI[1], 3)
dict$aut.avg.m <- round(autBCI[2], 3)
dict$aut.avg.f <- round(autBCI[1], 3)

dict$spr.sd.m <- round(sprBCISD[2], dict$rounding)
dict$spr.sd.f <- round(sprBCISD[1], dict$rounding)
dict$sum.sd.m <- round(sumBCISD[2], dict$rounding)
dict$sum.sd.f <- round(sumBCISD[1], dict$rounding)
dict$aut.sd.m <- round(autBCISD[2], dict$rounding)
dict$aut.sd.f <- round(autBCISD[1], dict$rounding)

summaryTable <- data.frame(round(c(sprFat, sprBCI), dict$rounding), 
                           round(c(sprFatSD, sprBCISD), dict$rounding), 
                           round(c(sumFat, sumBCI), dict$rounding),
                           round(c(sumFatSD, sumBCISD), dict$rounding),
                           round(c(autFat, autBCI), dict$rounding), 
                           round(c(autFatSD, autBCISD), dict$rounding))
names(summaryTable) <- c("sprMetric", "sprSD", "sumMetric", "sumSD", "autMetric", "autSD")

sex.summary         <- c("Female", "Male", "Female", "Male")
metric.summary      <- c("Fat score (1-5)", "Fat score (1-5)", "BCI", "BCI")
seasonTitle.summary <- c("", "", "Spring", "Spring", "Summer", "Summer", "Autumn", "Autumn")

### Tweaking the existing fields ###

adultFrame$seasonTestes[which(adultFrame$Sex == "Female")] <- "nonrepFemale"
adultFrame$seasonTestes[which(adultFrame$Reproductive.female == "1")] <- "reproductiveFemale"
adultFrame$descendedTestes <- NA
adultFrame$descendedTestes[which(adultFrame$seasonTestes == "Descended")] <- 1
adultFrame$descendedTestes[which(adultFrame$seasonTestes != "Descended")] <- 0

### Spring modelling ###

# Diagnosing quadratics #

springCondition         <- subset(adultFrame, Season == "Spring")
springCompleteCondition <- springCondition %>% 
  subset(!is.na(BCI) & !is.na(totalN) & !is.na(Reproductive.female) & 
           (!is.na(seasonTestes) | Sex == "Female"))

#springCompleteCondition$seasonTestes[which(springCompleteCondition$Sex == "Female")] <- "Female"

varsOfInterest  <- c("Year", "Weight", "totalN",
                     "springTemp", "springRain", "springSigma", "springCV",
                     "preWinterTemp", "preWinterRain", "preWinterSigma", "preWinterCV")

for (i in 1:length(varsOfInterest)) {
  covar <- varsOfInterest[i]
  plot(springCompleteCondition$BCI ~ springCompleteCondition[, covar], 
       xlab = covar, ylab = c("BCI"))
}
#Notes: 
#Evidence for quadratic spring temp and spring rain relationship

# Diagnosing collinearity #

corrplot(cor(springCompleteCondition[, varsOfInterest]), method = "number")
#All looks good! Spring CV and spring rain are quite close to worrisome though

## Modelling ##

springobj <- standardize(BCI ~ Age + dayOfYear + seasonTestes + Reproductive.female + descendedTestes +
                           poly(springTemp, 2) + poly(springRain, 2) + 
                           springSigma + springCV + 
                           preWinterTemp + preWinterRain +
                           preWinterSigma + preWinterCV +
                           totalN + 
                           Reproductive.female:poly(springTemp, 2) +
                           Reproductive.female:poly(springRain, 2) + 
                           Reproductive.female:springSigma +
                           Reproductive.female:springCV + 
                           Reproductive.female:preWinterTemp + 
                           Reproductive.female:preWinterRain + 
                           Reproductive.female:preWinterSigma + 
                           Reproductive.female:preWinterCV +
                           Reproductive.female:totalN +
                           Age:poly(springTemp, 2) +
                           Age:poly(springRain, 2) + 
                           Age:springSigma +
                           Age:springCV + 
                           Age:preWinterTemp + 
                           Age:preWinterRain + 
                           Age:preWinterSigma + 
                           Age:preWinterCV +
                           Age:totalN + 
                           Age:descendedTestes + 
                           Age:Reproductive.female, 
                         data = springCompleteCondition)

springobj$data <- springobj$data %>%
  select(-poly_springTemp, -poly_springRain) %>%
  mutate_if(is.matrix, as.vector) %>%
  cbind(springobj$data$poly_springTemp[,1],
        springobj$data$poly_springTemp[,2],
        springobj$data$poly_springRain[,1],
        springobj$data$poly_springRain[,2])

names(springobj$data)[(ncol(springobj$data) - 3):ncol(springobj$data)] <- c("springTemp",
                                                                            "springTemp2",
                                                                            "springRain",
                                                                            "springRain2")

continuousVars <- setdiff(names(springobj$data), 
                          c("BCI", "Age", "dayOfYear", "Sex", 
                            "Reproductive.female", "old.age", "seasonTestes", "descendedTestes"))

corrplot(cor(springobj$data[,continuousVars]), method = "number")
#^looks fine! A few values closer to 0.6 than I'd like, but nothing really high

springobj$formula <- update(springobj$formula, 
                            ~ . - Age - dayOfYear - 
                              Reproductive.female -
                              descendedTestes + 
                              s(Age) + s(dayOfYear) - 
                              poly_springTemp - poly_springRain - 
                              Reproductive.female:poly_springTemp - 
                              Reproductive.female:poly_springRain -
                              Age:poly_springTemp -
                              Age:poly_springRain + 
                              springTemp + springTemp2 +
                              springRain + springRain2 + 
                              Reproductive.female:springTemp + 
                              Reproductive.female:springRain +
                              Age:springTemp + 
                              Age:springRain)
#^This updates the formula to allow splines. 

springobj$data$Individual <- springCompleteCondition$Individual
springobj$data$Sett       <- springCompleteCondition$Sett
springobj$data$seasonTestes <- relevel(springobj$data$seasonTestes, ref = "Ascended")

springN <- springCompleteCondition$totalN/sd(springCompleteCondition$totalN)
#^This is so that we're not stuck in the scaled space (need only positive values)

springModelStandard <- gamm(formula = springobj$formula, 
                            data = springobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail")

springModelFixed    <- gamm(formula = springobj$formula, 
                            data = springobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varFixed( ~ springN))

springModelPower    <- gamm(formula = springobj$formula, 
                            data = springobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varPower(form = ~ springN))

springModelExp      <- gamm(formula = springobj$formula, 
                            data = springobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varExp(form = ~ springN))

AIC(springModelStandard$lme, springModelFixed$lme, springModelPower$lme,
    springModelExp$lme) #None show any improvement

springVarAnova <- anova(springModelStandard$lme, springModelFixed$lme)

dict$spring.var.aic <- round(springVarAnova$AIC[2] - springVarAnova$AIC[1], dict$rounding)

springGlobalModel <- gamm(formula = springobj$formula, 
                          data = springobj$data, 
                          random = list(Sett = ~1, Individual = ~1),
                          na.action = "na.fail")

springobj$mainFormula <- springobj$formula

dict$num.spring.bci <- nrow(springobj$data)
dict$num.ind.spring.bci <- springobj$data %>% distinct(Individual) %>% nrow()
dict$spring.bci.r2  <- round(summary(springGlobalModel$gam)$r.sq, dict$rounding)

dict$spr.mean.bci.repfem <- springCompleteCondition %>% 
  subset(Reproductive.female == "1") %>%
  select(BCI) %>%
  pull() %>% mean() %>% round(3)
dict$spr.mean.bci.nonrepfem <- springCompleteCondition %>% 
  subset(Reproductive.female == "0" & Sex == "Female") %>%
  select(BCI) %>%
  pull() %>% mean() %>% round(3)

dict$spr.test.gap <- (springCompleteCondition %>%
                        subset(seasonTestes == "Ascended") %>%
                        select(BCI) %>%
                        pull() %>% mean() - (springCompleteCondition %>%
                                               subset(seasonTestes == "Descended") %>%
                                               select(BCI) %>%
                                               pull() %>% mean())) %>% round(3)
dict$spr.gap.perc <- 100*dict$spr.test.gap/(springCompleteCondition %>%
                                              subset(seasonTestes == "Ascended") %>%
                                              select(BCI) %>%
                                              pull() %>% mean())

# Checking to see what the deal is with reproductive females #

plot_model(springGlobalModel$gam, type = "pred", terms = c("seasonTestes"))
plot_model(springGlobalModel$gam, type = "pred", terms = c("Age", "Reproductive.female"))
ggplot(springobj$data, aes(x = springCompleteCondition$Sex, y = BCI)) + geom_boxplot() + facet_grid(.~Reproductive.female)

# Section including some dropping of interaction terms (not used anymore): #

# #Drop all interactions except pre-winter sigma
# springobj$formula2 <- update(springobj$formula, ~. -Reproductive.female:poly_springTemp -
#                                Reproductive.female:poly_springRain - 
#                                Reproductive.female:springSigma -
#                                Reproductive.female:springCV - 
#                                Reproductive.female:preWinterTemp - 
#                                Reproductive.female:preWinterRain -  
#                                Reproductive.female:preWinterCV -
#                                Reproductive.female:totalN -
#                                old.age:poly_springRain -
#                                old.age:poly_springTemp -
#                                old.age:springSigma -
#                                old.age:springCV -
#                                old.age:preWinterRain -
#                                old.age:preWinterSigma - 
#                                old.age:preWinterCV)
# 
# springModel <- gamm(formula = springobj$formula2, 
#                     data = springobj$data, 
#                     random = list(Individual = ~1),
#                     na.action = "na.fail")
# 
# springobj$formula3 <- update(springobj$formula2, ~. - preWinterTemp:old.age)


springModelFinal <- springGlobalModel

# Some diagnostics #

customPlotter(model = springModelFinal$gam,
              term = "springTemp",
              modelData = springobj$data,
              originalData = springCompleteCondition,
              response = "BCI")

customPlotter(model = springModelFinal$gam,
              term = "springRain",
              modelData = springobj$data,
              originalData = springCompleteCondition,
              response = "BCI")

# Storing useful terms in dict #

springSummaryLme <- summary(springModelFinal$lme)
springSummaryGam <- summary(springModelFinal$gam)

# Storing specific comparisons of reproductive females against other females # 

springModelFinal$gam$call <- quote(gamm(formula = springobj$formula, 
                                        data = springobj$data, 
                                        random = list(Sett = ~1, Individual = ~1),
                                        na.action = "na.fail"))

meansSpr <- summary(emmeans(springModelFinal, specs = pairwise ~ seasonTestes))$contrasts

dict$spring.fem.vs.rep.p <- pStore(meansSpr$p.value[which(meansSpr$contrast == "reproductiveFemale,1,0 - nonrepFemale,0,0")])
dict$spring.des.vs.asc.p <- pStore(meansSpr$p.value[which(meansSpr$contrast == "Descended,0,1 - Ascended,0,0")])
dict$spring.des.vs.int.p <- pStore(meansSpr$p.value[which(meansSpr$contrast == "Descended,0,1 - Intermediate,0,0")])

# n.age.inter.pos  <- which(names(springSummaryGam$p.pv) == "totalN:old.ageTRUE")
# dict$spring.n.age.inter.p <- pStore(unname(springSummaryGam$p.pv[n.age.inter.pos]))

### Summer modelling ###

# Diagnosing quadratics #

summerCondition         <- subset(adultFrame, Season == "Summer")
summerCompleteCondition <- summerCondition %>% 
  subset(!is.na(BCI) & !is.na(totalN) & !is.na(Reproductive.female) & 
           (!is.na(seasonTestes) | Sex == "Female"))

#summerCompleteCondition$seasonTestes[which(summerCompleteCondition$Sex == "Female")] <- "Female"

varsOfInterest  <- c("Year", "Weight", "totalN",
                     "summerTemp", "summerRain", "summerSigma", "summerCV",
                     "dryTemp30", "dryRain30", "drySigma30", "dryCV30",
                     "dryTemp90", "dryRain90", "drySigma90", "dryCV90")

for (i in 1:length(varsOfInterest)) {
  covar <- varsOfInterest[i]
  plot(summerCompleteCondition$BCI ~ summerCompleteCondition[,covar], 
       xlab = covar, ylab = "BCI")
}
#Notes: 
#No evidence for quadratic terms

## Making a standardized object ##

summerobj <- standardize(BCI ~ Age + dayOfYear + seasonTestes + Sex + Reproductive.female + descendedTestes +
                           summerTemp + summerRain + 
                           summerSigma + summerCV + 
                           dryTemp30 + dryRain30 +
                           drySigma30 + dryCV30 + 
                           dryTemp90 + dryRain90 +
                           drySigma90 + dryCV90 +
                           totalN,
                         data = summerCompleteCondition)

summerobj$data <- summerobj$data %>%
  mutate_if(is.matrix, as.vector) 

continuousVars <- setdiff(names(summerobj$data), 
                          c("BCI", "Age", "dayOfYear", "Sex", 
                            "Reproductive.female", "old.age", "seasonTestes", "descendedTestes"))

corrplot(cor(summerobj$data[,continuousVars]), method = "number")
#We've got issues. N is fine, but obviously we have a lot of correlation
#within and between dry periods and summer. Moreover, we have correlation 
#essentially everywhere except between (kind of) rain and temp, 
#and sigma and rain. Best option is probably to make a PCA.
#However, the 30-day window doesn't seem to have many covariance issues.
#We'll leave that one as-is (no conversion).

sum.pca   <- prcomp(summerobj$data[,c("summerTemp", "summerRain", "summerSigma", "summerCV")])
dry90.pca <- prcomp(summerobj$data[,c("dryTemp90", "dryRain90", "drySigma90", "dryCV90")])

sum.summary   <- summary(sum.pca)
dry90.summary <- summary(dry90.pca)

dict$sum.pca.cumvar   <- 100*round(unname(sum.summary$importance[,2][3]), dict$rounding)
dict$dry90.pca.cumvar <- 100*round(unname(dry90.summary$importance[,2][3]), dict$rounding)

summerobj$data$S.1    <- sum.pca$x[,1]
summerobj$data$S.2    <- sum.pca$x[,2]
summerobj$data$D.90.1 <- dry90.pca$x[,1]
summerobj$data$D.90.2 <- dry90.pca$x[,2]

# Updating formula #

summerobj$formula1 <- update(summerobj$formula, 
                             ~ . - Age - Sex - Reproductive.female - descendedTestes -
                               dayOfYear + s(Age) + s(dayOfYear) -
                               summerTemp - summerRain - summerSigma - summerCV - 
                               dryTemp30 - dryRain30 - drySigma30 - dryCV30 - 
                               dryTemp90 - dryRain90 - drySigma90 - dryCV90 + 
                               S.1 + S.2 + 
                               Reproductive.female:S.1 + Reproductive.female:S.2 +
                               Reproductive.female:totalN +
                               Age:S.1 + Age:S.2 + Age:totalN + 
                               descendedTestes:Age + 
                               Reproductive.female:Age)

summerobj$formula2 <- update(summerobj$formula1, 
                             ~ . - S.1 - S.2 + dryTemp30 + dryRain30 +
                               drySigma30 + dryCV30 -
                               Reproductive.female:S.1 - Reproductive.female:S.2 + 
                               Reproductive.female:dryTemp30 + Reproductive.female:dryRain30 +
                               Reproductive.female:drySigma30 + Reproductive.female:dryCV30 -
                               Age:S.1 - Age:S.2 +
                               Age:dryTemp30 + Age:dryRain30 +
                               Age:drySigma30 + Age:dryCV30)

summerobj$formula3 <- update(summerobj$formula2, 
                             ~ . - dryTemp30 - dryRain30 -
                               drySigma30 - dryCV30 + 
                               D.90.1 + D.90.2 -
                               Reproductive.female:dryTemp30 - Reproductive.female:dryRain30 -
                               Reproductive.female:drySigma30 - Reproductive.female:dryCV30 + 
                               Reproductive.female:D.90.1 + Reproductive.female:D.90.2 -
                               Age:dryTemp30 - Age:dryRain30 -
                               Age:drySigma30 - Age:dryCV30 + 
                               Age:D.90.1 + Age:D.90.2)

summerobj$data$Individual <- summerCompleteCondition$Individual
summerobj$data$Sett       <- summerCompleteCondition$Sett
summerobj$data$seasonTestes <- relevel(summerobj$data$seasonTestes, ref = "Ascended")

## Modelling ##

summerModRand1 <- gamm(formula = summerobj$formula1, 
                       data = summerobj$data, 
                       random = list(Sett = ~1, Individual = ~1),
                       na.action = "na.fail")

summerModRand2 <- gamm(formula = summerobj$formula2, 
                       data = summerobj$data, 
                       random = list(Sett = ~1, Individual = ~1),
                       na.action = "na.fail")

summerModRand3 <- gamm(formula = summerobj$formula3, 
                       data = summerobj$data, 
                       random = list(Sett = ~1, Individual = ~1),
                       na.action = "na.fail")

AIC(summerModRand1$lme, summerModRand2$lme, summerModRand3$lme) #The 30-day period is best, by far

summerWindAnova <- anova(summerModRand1$lme, summerModRand2$lme)

dict$summer.wind.aic <- paste("=", round(summerWindAnova$AIC[1] - summerWindAnova$AIC[2], dict$rounding))
dict$summer.wind.p   <- pStore(pValue = summerWindAnova$`p-value`[2])
dict$summer.wind.lrt <- summerWindAnova$L.Ratio[2]
dict$summer.wind.df  <- summerWindAnova$df[2]

# Now comparing variance structures #

summerN <- summerCompleteCondition$totalN/sd(summerCompleteCondition$totalN)

summerModelStandard <- gamm(formula = summerobj$formula2, 
                            data = summerobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail")

summerModelFixed    <- gamm(formula = summerobj$formula2, 
                            data = summerobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varFixed(~summerN))

summerModelPower    <- gamm(formula = summerobj$formula2, 
                            data = summerobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varPower(form = ~summerN))

summerModelExp      <- gamm(formula = summerobj$formula2, 
                            data = summerobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varExp(form = ~summerN))

AIC(summerModelStandard$lme, summerModelFixed$lme, summerModelPower$lme,
    summerModelExp$lme) #Exponential and power show substantial improvement

summerVarAnova <- anova(summerModelStandard$lme, summerModelExp$lme)

dict$summer.var.LRT   <- paste("=",round(summerVarAnova$L.Ratio[2], dict$rounding))
dict$summer.var.LRTp  <- pStore(summerVarAnova$`p-value`[2])
dict$summer.var.LRTdf <- paste("=", summerVarAnova$df[2] - summerVarAnova$df[1])

summerobj$formulaStd <- update(summerobj$formula2, ~.-seasonTestes + Sex)

summerGlobalModel <- gamm(formula = summerobj$formula2, 
                          data = summerobj$data, 
                          random = list(Sett = ~1, Individual = ~1),
                          na.action = "na.fail",
                          weights = varExp(form = ~summerN))

summerNoVarModel <- gamm(formula = summerobj$formula2, 
                         data = summerobj$data, 
                         random = list(Sett = ~1, Individual = ~1),
                         na.action = "na.fail")

summerStandModel  <- gamm(formula = summerobj$formulaStd, 
                          data = summerobj$data, 
                          random = list(Sett = ~1, Individual = ~1),
                          na.action = "na.fail",
                          weights = varExp(form = ~summerN))

AIC(summerGlobalModel$lme, summerStandModel$lme) # With testes is better

summerobj$mainFormula <- summerobj$formula2

dict$num.summer.bci <- nrow(summerobj$data)
dict$num.ind.summer.bci <- summerobj$data %>% distinct(Individual) %>% nrow()
dict$summer.bci.r2  <- round(summary(summerGlobalModel$gam)$r.sq, dict$rounding)

dict$sum.mean.bci.repfem <- summerCompleteCondition %>% 
  subset(Reproductive.female == "1") %>%
  select(BCI) %>%
  pull() %>% mean() %>% round(3)
dict$sum.mean.bci.nonrepfem <- summerCompleteCondition %>% 
  subset(Reproductive.female == "0" & Sex == "Female") %>%
  select(BCI) %>%
  pull() %>% mean() %>% round(3)

dict$sum.min.bci.nonrepfem <- summerCompleteCondition %>% 
  subset(Reproductive.female == "0" & Sex == "Female") %>%
  select(BCI) %>%
  pull() %>% min() %>% round(3)

dict$sum.test.gap <- (summerCompleteCondition %>%
                        subset(seasonTestes == "Ascended") %>%
                        select(BCI) %>%
                        pull() %>% mean() - (summerCompleteCondition %>%
                                               subset(seasonTestes == "Descended") %>%
                                               select(BCI) %>%
                                               pull() %>% mean())) %>% round(3)

dict$sum.gap.perc <- 100*dict$sum.test.gap/(summerCompleteCondition %>%
                                              subset(seasonTestes == "Ascended") %>%
                                              select(BCI) %>%
                                              pull() %>% mean())

summerSummary <- summary(summerGlobalModel$gam)
dict$sum.age.ut.p <- pStore(summerSummary$p.pv[which(names(a$p.pv) == "dryTemp30:Age")])

#Drop all interactions except old age with totalN and reproductive female with S1
# summerobj$formula2 <- update(summerobj$formula1, ~. -Reproductive.female:S.2 -
#                                Reproductive.female:totalN - 
#                                old.age:S.1 -
#                                old.age:S.2)

#See what the effect of dropping dayOfYear does to the model 

# summerNoCalendarFor <- update(summerobj$formula, 
#                               ~ . - Age - dayOfYear + s(Age) -
#                                 summerTemp - summerRain - summerSigma - summerCV - 
#                                 dryTemp30 - dryRain30 - drySigma30 - dryCV30 - 
#                                 dryTemp90 - dryRain90 - drySigma90 - dryCV90 + 
#                                 S.1 + S.2 + 
#                                 Reproductive.female:S.1 + Reproductive.female:S.2 +
#                                 Reproductive.female:totalN -
#                                 old.age +
#                                 old.age:S.1 + old.age:S.2 + old.age:totalN)
# 
# summerNoCalendarMod <- gamm(formula = summerNoCalendarFor,
#                             data = summerobj$data, 
#                             random = list(Sett = ~1, Individual = ~1),
#                             na.action = "na.fail",
#                             weights = varExp(form = ~summerN))

summerModelFinal <- summerGlobalModel

## Some diagnostics ##

plot_model(summerModelFinal$gam, type = "pred", terms = c("dryTemp30", "Age [-1, 2]"))
plot_model(summerModelFinal$gam, type = "pred", terms = c("dryTemp30", "Reproductive.female"))
plot_model(summerModelFinal$gam, type = "pred", terms = c("totalN", "Age [-1, 2]"))

## Storing useful metrics ##

summerSummaryLme    <- summary(summerModelFinal$lme)
dict$summer.var.exp <- 2*round(summerSummaryLme$modelStruct$varStruct, 3)[[1]]
#^2 because I believe that's the equation used

# Storing specific comparisons of reproductive females against other females # 

summerGlobalModel$gam$call <- quote(gamm(formula = summerobj$formula2, 
                                         data = summerobj$data, 
                                         random = list(Sett = ~1, Individual = ~1),
                                         na.action = "na.fail"))

meansSum <- summary(emmeans(summerGlobalModel, specs = pairwise ~ seasonTestes))$contrasts

dict$summer.fem.vs.rep.p <- pStore(meansSum$p.value[which(meansSum$contrast == "reproductiveFemale,1,0 - nonrepFemale,0,0")])
dict$summer.des.vs.asc.p <- pStore(meansSum$p.value[which(meansSum$contrast == "Descended,0,1 - Ascended,0,0")])

### Autumn modelling ###

autumnCondition         <- subset(adultFrame, Season == "Autumn")
autumnCompleteCondition <- autumnCondition %>% 
  subset(!is.na(BCI) & !is.na(totalN) & !is.na(Reproductive.female) &
           (!is.na(seasonTestes) | Sex == "Female"))

#autumnCompleteCondition$seasonTestes[which(autumnCompleteCondition$Sex == "Female")] <- "Female"

# Diagnosing quadratics #

varsOfInterest  <- c("Year", "Weight", "totalN",
                     "dryTemp30", "dryRain30", "drySigma30", "dryCV30",
                     "autumnTemp", "autumnRain", "autumnSigma", "autumnCV")

for (i in 1:length(varsOfInterest)) {
  covar <- varsOfInterest[i]
  plot(autumnCompleteCondition$BCI ~ autumnCompleteCondition[,covar], 
       xlab = covar, ylab = "BCI")
}
#Notes: 
#Evidence for quadratic autumn CV

## Making a standardized object ##

autumnobj <- standardize(BCI ~ Age + dayOfYear + Sex + seasonTestes +
                           Reproductive.female + descendedTestes +
                           dryTemp30 + dryRain30 + 
                           drySigma30 + dryCV30 + 
                           autumnTemp + autumnRain +
                           autumnSigma + 
                           poly(autumnCV, 2) +
                           totalN,
                         data = autumnCompleteCondition)

autumnobj$data$Individual <- autumnCompleteCondition$Individual
autumnobj$data$Sett       <- autumnCompleteCondition$Sett
autumnobj$data$seasonTestes <- relevel(autumnobj$data$seasonTestes, ref = "Ascended")

autumnobj$data <- autumnobj$data %>%
  select(-poly_autumnCV) %>%
  mutate_if(is.matrix, as.vector) %>%
  cbind(autumnobj$data$poly_autumnCV[,1],
        autumnobj$data$poly_autumnCV[,2])

names(autumnobj$data)[(ncol(autumnobj$data)-1):ncol(autumnobj$data)] <- c("autumnCV", "autumnCV2")

continuousVars <- setdiff(names(autumnobj$data), 
                          c("BCI", "Age", "dayOfYear", "Sex", "Individual", 
                            "old.age", "Sett", "seasonTestes", "Reproductive.female", "descendedTestes"))

corrplot(cor(autumnobj$data[,continuousVars]), method = "number")
#All good! Except as usual CV30 and rain30, which are close to -0.6

#Applying the same PCA to autumn data

# newMetrics <- data.frame(predict(sum.pca, newdata = autumnobj$data))
# S.1 <- newMetrics$PC1
# S.2 <- newMetrics$PC2
# autumnobj$data <- cbind(autumnobj$data, S.1, S.2)
# 
# continuousVars <- setdiff(names(autumnobj$data), 
#                           c("BCI", "Age", "dayOfYear", "Sex", "Individual", "old.age",
#                             "summerTemp", "summerRain", "summerSigma", "summerCV", "Sett"))
# 
# corrplot(cor(autumnobj$data[,continuousVars]), method = "number")
#All cleaned up! Autumn sigma and S.1 are 0.42 but that should be fine
# 
# plot(autumnobj$data$BCI ~ autumnobj$data$S.1)
# plot(autumnobj$data$BCI ~ autumnobj$data$S.2)
# #^No sign of quadratics really

## Updating formula ##

autumnobj$formula1 <- update(autumnobj$formula, 
                             ~. - Sex - Age - dayOfYear - Reproductive.female - descendedTestes -
                               poly_autumnCV - 
                               Age:poly_autumnCV + 
                               autumnCV + autumnCV2 +
                               s(Age) + s(dayOfYear) +
                               Age:dryTemp30 + Age:dryRain30 +
                               Age:drySigma30 + Age:dryCV30 +
                               Age:autumnTemp + Age:autumnRain + 
                               Age:autumnCV + Age:autumnSigma +
                               Age:totalN + 
                               Reproductive.female:dryTemp30 + Reproductive.female:dryRain30 +
                               Reproductive.female:drySigma30 + Reproductive.female:dryCV30 +
                               Reproductive.female:autumnTemp + Reproductive.female:autumnRain + 
                               Reproductive.female:autumnCV + Reproductive.female:autumnSigma +
                               Reproductive.female:totalN + 
                               descendedTestes:Age +
                               Reproductive.female:Age)

autumnN <- autumnCompleteCondition$totalN/sd(autumnCompleteCondition$totalN)

autumnModelStandard <- gamm(formula = autumnobj$formula1, 
                            data = autumnobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail")

autumnModelFixed    <- gamm(formula = autumnobj$formula1, 
                            data = autumnobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varFixed(~ autumnN))

autumnModelPower    <- gamm(formula = autumnobj$formula1, 
                            data = autumnobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varPower(form = ~ autumnN))

autumnModelExp      <- gamm(formula = autumnobj$formula1, 
                            data = autumnobj$data, 
                            random = list(Sett = ~1, Individual = ~1),
                            na.action = "na.fail",
                            weights = varExp(form = ~ autumnN))

AIC(autumnModelStandard$lme, autumnModelFixed$lme, autumnModelPower$lme,
    autumnModelExp$lme) #Fixed now actually has a narrowly better AIC

autumnVarAnova <- anova(autumnModelStandard$lme, autumnModelFixed$lme)

dict$autumn.var.aic <- round(autumnVarAnova$AIC[1] - autumnVarAnova$AIC[2], 1)

autumnobj$formulaStd <- update(autumnobj$formula1, ~. -seasonTestes + Sex + Reproductive.female)

autumnGlobalMod <- gamm(formula = autumnobj$formula1, 
                        data = autumnobj$data, 
                        random = list(Sett = ~1, Individual = ~1),
                        na.action = "na.fail",
                        weights = varFixed(~ autumnN))
autumnNoVarMod <- gamm(formula = autumnobj$formula1, 
                       data = autumnobj$data, 
                       random = list(Sett = ~1, Individual = ~1),
                       na.action = "na.fail")

autumnStandMod  <- gamm(formula = autumnobj$formulaStd, 
                        data = autumnobj$data, 
                        random = list(Sett = ~1, Individual = ~1),
                        na.action = "na.fail",
                        weights = varFixed(~ autumnN))

AIC(autumnGlobalMod$lme, autumnStandMod$lme) #Yep, better with testes, but not as dramatically

autumnobj$mainFormula <- autumnobj$formula1

dict$num.autumn.bci <- nrow(autumnobj$data)
dict$num.ind.autumn.bci <- autumnobj$data %>% distinct(Individual) %>% nrow()
dict$autumn.bci.r2  <- round(summary(autumnGlobalMod$gam)$r.sq, dict$rounding)

dict$aut.test.gap <- (autumnCompleteCondition %>%
                        subset(seasonTestes == "Ascended") %>%
                        select(BCI) %>%
                        pull() %>% mean() - (autumnCompleteCondition %>%
                                               subset(seasonTestes == "Descended") %>%
                                               select(BCI) %>%
                                               pull() %>% mean())) %>% round(3)

dict$aut.gap.perc <- 100*dict$aut.test.gap/(autumnCompleteCondition %>%
                                              subset(seasonTestes == "Ascended") %>%
                                              select(BCI) %>%
                                              pull() %>% mean())

# Old section with dropping non-significant interaction terms: #

# autumnobj$formula1   <- update(autumnobj$formula, ~. - old.age:S.1 - old.age:S.2 -
#                                  old.age:autumnTemp - old.age:autumnRain - 
#                                  old.age:poly_autumnCV - old.age:autumnSigma)
# 
# autumnModel      <- gamm(formula = autumnobj$formula1, 
#                          data = autumnobj$data, 
#                          random = list(Individual = ~1),
#                          na.action = "na.fail")
# 
# autumnobj$formula2 <- update(autumnobj$formula1, ~. - old.age:totalN)

autumnModelFinal <- autumnGlobalMod

## Diagnostics ##

customPlotter(model = autumnModelFinal$gam, 
              term = "autumnCV", 
              modelData = autumnobj$data, 
              originalData = autumnCompleteCondition,
              response = "BCI")

plot_model(autumnModelFinal$gam, type = "pred", terms = c("totalN", "Age [-1, 2]"))
plot_model(autumnModelFinal$gam, type = "pred", terms = c("autumnRain", "Age[-1, 2]"))
plot_model(autumnModelFinal$gam, type = "pred", terms = c("dryTemp30", "Age[-1, 2]"))
plot_model(autumnModelFinal$gam, type = "pred", terms = c("dryTemp30", "Reproductive.female"))

## Storing interesting things for model ##

autumnSummaryGam <- summary(autumnModelFinal$gam)

sex.pos           <- which(names(autumnSummaryGam$p.pv) == "seasonTestesnonrepFemale")
dict$autumn.sex.p <- pStore(autumnSummaryGam$p.pv[sex.pos])

# Storing specific comparisons of reproductive females against other females # 

autumnGlobalMod$gam$call <- quote(gamm(formula = autumnobj$formula1, 
                                       data = autumnobj$data, 
                                       random = list(Sett = ~1, Individual = ~1),
                                       na.action = "na.fail"))

meansAut <- summary(emmeans(autumnGlobalMod, specs = pairwise ~ seasonTestes))$contrasts

dict$autumn.fem.vs.rep.p <- pStore(meansAut$p.value[which(meansAut$contrast == "reproductiveFemale,1,0 - nonrepFemale,0,0")])
dict$autumn.des.vs.asc.p <- pStore(meansAut$p.value[which(meansAut$contrast == "Descended,0,1 - Ascended,0,0")])

# Storing a prediction of what BCI a female would be if she was reproductive vs. not at two different ages #

autBCIPred <- autumnobj$data

autBCIPred <- allButAverager(autumnobj$data, 
                             terms    = c("Age"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale"))

autBCIPred$Age <- autBCIPred$Age[which(autumnCompleteCondition$Age == 2)][1]
nonRepPredict2 <- reverseStandardize(variable = autumnCompleteCondition$BCI, 
                                     toReverse = predict(autumnModelFinal$gam, newdata = autBCIPred))[1]

autBCIPred <- autumnobj$data

autBCIPred <- allButAverager(autumnobj$data, 
                             terms    = c("Age"), 
                             defaults = data.frame(Reproductive.female = "1",
                                                   Sex = "Female",
                                                   seasonTestes = "reproductiveFemale"))

autBCIPred$Age <- autBCIPred$Age[which(autumnCompleteCondition$Age == 2)][1]
yesRepPredict2 <- reverseStandardize(variable = autumnCompleteCondition$BCI, 
                                     toReverse = predict(autumnModelFinal$gam, newdata = autBCIPred))[1]

autBCIPred <- autumnobj$data

autBCIPred <- allButAverager(autumnobj$data, 
                             terms    = c("Age"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale"))

autBCIPred$Age <- autBCIPred$Age[which(autumnCompleteCondition$Age == 9)][1]
nonRepPredict9 <- reverseStandardize(variable = autumnCompleteCondition$BCI, 
                                     toReverse = predict(autumnModelFinal$gam, newdata = autBCIPred))[1]

autBCIPred <- autumnobj$data

autBCIPred <- allButAverager(autumnobj$data, 
                             terms    = c("Age"), 
                             defaults = data.frame(Reproductive.female = "1",
                                                   Sex = "Female",
                                                   seasonTestes = "reproductiveFemale"))

autBCIPred$Age <- autBCIPred$Age[which(autumnCompleteCondition$Age == 9)][1]
yesRepPredict9 <- reverseStandardize(variable = autumnCompleteCondition$BCI, 
                                     toReverse = predict(autumnModelFinal$gam, newdata = autBCIPred))[1]


# s1.old.inter.pos           <- which(names(autumnSummaryGam$p.pv) == "S.1:old.ageTRUE")
# dict$autumn.s1.old.inter.p <- pStore(autumnSummaryGam$p.pv[s1.old.inter.pos])
# 
# rain.old.inter.pos           <- which(names(autumnSummaryGam$p.pv) == "autumnRain:old.ageTRUE")
# dict$autumn.rain.old.inter.p <- pStore(autumnSummaryGam$p.pv[rain.old.inter.pos])

## Predicting spring with average values to extrapolate for day of year ##

# springPred <- springobj$data
# springPred <- springPred %>%
#   mutate(totalN         = mean(totalN),
#          springSigma    = mean(springSigma),
#          springCV       = mean(springCV),
#          preWinterTemp  = mean(preWinterTemp),
#          preWinterRain  = mean(preWinterRain),
#          preWinterSigma = mean(preWinterSigma),
#          preWinterCV    = mean(preWinterCV))
# 
# 
# springPred$old.age[springPred$old.age == TRUE]                        <- FALSE
# springPred$Reproductive.female[springPred$Reproductive.female == "1"] <- "0"
# 
# # Making matrices for the model structure #
# poly_springTemp <- cbind(rep(mean(springPred$poly_springTemp[,1]), nrow(springPred)),
#                                        rep(mean(springPred$poly_springTemp[,2]), nrow(springPred)))
# springPred$poly_springTemp <- poly_springTemp
# 
# poly_springRain <- cbind(rep(mean(springPred$poly_springRain[,1]), nrow(springPred)),
#                          rep(mean(springPred$poly_springRain[,2]), nrow(springPred)))
# springPred$poly_springRain <- poly_springRain
# 
# springPred$totalN         <- matrix(springPred$totalN, ncol = 1)
# springPred$springSigma    <- matrix(springPred$springSigma)
# springPred$springCV       <- matrix(springPred$springCV)
# springPred$preWinterTemp  <- matrix(springPred$preWinterTemp)
# springPred$preWinterRain  <- matrix(springPred$preWinterRain)
# springPred$preWinterSigma <- matrix(springPred$preWinterSigma)
# springPred$preWinterCV    <- matrix(springPred$preWinterCV)
# 
# # Predicting and putting back into data #
# 
# predictions <- predict(springModelFinal$gam, newdata = springPred)
# 
# springCompleteCondition$BCIPredicted <- c(reverseStandardize(springCompleteCondition$BCI,
#                                                              predictions))
# 
# springCompleteCondition$BCIResiduals <- springCompleteCondition$BCI - springCompleteCondition$BCIPredicted
# 
# ## Predicting summer with average values to extrapolate for day of year ##
# 
# summerPred <- summerobj$data
# summerPred <- summerPred %>%
#   mutate(totalN = mean(totalN),
#          S.1    = mean(S.1),
#          S.2    = mean(S.2))
# 
# summerPred$old.age[which(summerPred$old.age == TRUE)]                 <- FALSE
# summerPred$Reproductive.female[summerPred$Reproductive.female == "1"] <- "0"
# 
# summerPred$totalN <- matrix(summerPred$totalN, ncol = 1)
# 
# predictions <- predict(summerModelFinal$gam, newdata = summerPred)
# 
# summerCompleteCondition$BCIPredicted <- c(reverseStandardize(summerCompleteCondition$BCI,
#                                                              predictions))
# summerCompleteCondition$BCIResiduals <- summerCompleteCondition$BCI - summerCompleteCondition$BCIPredicted
# 
# ## Predicting autumn with average values to extrapolate for day of year ##
# 
# autumnPred <- autumnobj$data
# autumnPred <- autumnPred %>%
#   mutate(totalN = mean(totalN),
#          S.1    = mean(S.1),
#          S.2    = mean(S.2))
# 
# autumnPred$old.age[which(autumnPred$old.age == TRUE)]                 <- FALSE
# 
# autumnPred$totalN      <- matrix(autumnPred$totalN, ncol = 1)
# autumnPred$autumnTemp  <- matrix(rep(mean(autumnPred$autumnTemp)), nrow(autumnPred))
# autumnPred$autumnRain  <- matrix(rep(mean(autumnPred$autumnRain)), nrow(autumnPred))
# autumnPred$autumnSigma <- matrix(rep(mean(autumnPred$autumnSigma)), nrow(autumnPred))
# 
# autumnCV <- cbind(rep(mean(autumnPred$poly_autumnCV[,1]), nrow(autumnPred)),
#                   rep(mean(autumnPred$poly_autumnCV[,2]), nrow(autumnPred)))
# autumnPred$poly_autumnCV <- autumnCV
# 
# predictions <- predict(autumnModelFinal$gam, newdata = autumnPred)
# 
# autumnCompleteCondition$BCIPredicted <- c(reverseStandardize(autumnCompleteCondition$BCI,
#                                                              predictions))
# autumnCompleteCondition$BCIResiduals <- autumnCompleteCondition$BCI - autumnCompleteCondition$BCIPredicted

## Last dict stuff ##

dict$total.males <- c(springCompleteCondition$Individual[which(springCompleteCondition$Sex == "Male")], 
                      summerCompleteCondition$Individual[which(summerCompleteCondition$Sex == "Male")], 
                      autumnCompleteCondition$Individual[which(autumnCompleteCondition$Sex == "Male")]) %>% 
  unique() %>% length()

dict$total.females <- c(springCompleteCondition$Individual[which(springCompleteCondition$Sex == "Female")], 
                        summerCompleteCondition$Individual[which(summerCompleteCondition$Sex == "Female")], 
                        autumnCompleteCondition$Individual[which(autumnCompleteCondition$Sex == "Female")]) %>% 
  unique() %>% length()


## Saving output for RMarkdown and other scripts ##

save(conditionSurvival, dict, sum.pca,
     springCompleteCondition, summerCompleteCondition, autumnCompleteCondition,
     springModelFinal, summerModelFinal, autumnModelFinal,
     springobj, summerobj, autumnobj, summerN, autumnN,
     nonRepPredict2, nonRepPredict9, yesRepPredict2, yesRepPredict9,
     file = "Condition modelling output.RData")

save.image(file = "Condition modelling all objects.RData")

### Making a table summarising our models ###

spr <- summary(springModelFinal$gam)
sum <- summary(summerModelFinal$gam)
aut <- summary(autumnModelFinal$gam)

coefNames <- list("seasonTestesnonrepFemale", "seasonTestesreproductiveFemale", "seasonTestesIntermediate",
                  "seasonTestesDescended", "totalN", 
                  "totalN:Age", c("Reproductive.female1:Age", "Age:Reproductive.female1"),
                  "preWinterTemp", "preWinterRain", "preWinterSigma", "preWinterCV",
                  "preWinterSigma:Reproductive.female1", "preWinterRain:Age",
                  "springTemp", "springTemp2", "springRain", "springRain2",
                  "springSigma", "springCV",
                  "dryTemp30", "dryRain30", "drySigma30", "dryCV30",
                  "dryTemp30:Reproductive.female1",
                  "dryTemp30:Age",
                  "autumnTemp", "autumnRain", "autumnSigma", "autumnCV", "autumnCV2",
                  "autumnRain:Age")
rnames <- c("Female", "Rep. female", "Int. testes", "Desc. testes",  "N", 
            "N:Age", "Rep. female:Age",
            "\u03BCT", "\u03BCR", "sigmaT", "CVR",
            "sigmaT:Rep. female", "\u03BCR:Age",
            "\u03BCT", "\u03BCT^2", "\u03BCR", "\u03BCR^2", 
            "sigmaT", "CVR",
            "\u03BCT", "\u03BCR", "sigmaT", "CVR",
            "\u03BCT:Rep. female", 
            "\u03BCT:Age",
            "\u03BCT", "\u03BCR", "sigmaT", "CVR", "CVR^2",
            "\u03BCR:Age")
season <- c(NA, NA, NA,
            NA, NA, NA, NA,
            "Winter", "Winter", "Winter", "Winter", 
            "Winter", "Winter",
            "Spring", "Spring", "Spring", "Spring",
            "Spring", "Spring",
            "Summer", "Summer", "Summer", "Summer",
            "Summer", "Summer",
            "Autumn", "Autumn", "Autumn", "Autumn", "Autumn",
            "Autumn")
seasonTitle <- c("", "", "Spring model", "Spring model",
                 "Summer model", "Summer model",
                 "Autumn model", "Autumn model")

sprCoefs <- rep(NA, length(rnames))
sumCoefs <- rep(NA, length(rnames))
autCoefs <- rep(NA, length(rnames))

sprSEs   <- rep(NA, length(rnames))
sumSEs   <- rep(NA, length(rnames))
autSEs   <- rep(NA, length(rnames))

sprNames <- names(spr$p.coeff)
sumNames <- names(sum$p.coeff)
autNames <- names(aut$p.coeff)

for (i in 1:length(rnames)) {
  coefName <- coefNames[[i]]
  sprCoefPos <- which(sprNames %in% coefName)
  
  if (length(sprCoefPos) > 0) {
    sprCoefs[i] <- asteriskMarker(spr$p.coeff[sprCoefPos],
                                  spr$p.pv[sprCoefPos])
    sprSEs[i]   <- as.character(round(spr$se[sprCoefPos], dict$rounding + 1))
  }
  
  sumCoefPos <- which(sumNames %in% coefName)
  
  if (length(sumCoefPos) > 0) {
    sumCoefs[i] <- asteriskMarker(sum$p.coeff[sumCoefPos],
                                  sum$p.pv[sumCoefPos])
    sumSEs[i]   <- as.character(round(sum$se[sumCoefPos], dict$rounding + 1))
  }
  
  autCoefPos <- which(autNames %in% coefName)
  
  if (length(autCoefPos) > 0) {
    autCoefs[i] <- asteriskMarker(aut$p.coeff[autCoefPos],
                                  aut$p.pv[autCoefPos])
    autSEs[i]   <- as.character(round(aut$se[autCoefPos], dict$rounding + 1))
  }
}

conditionModelFrame <- data.frame(season, rnames, sprCoefs, sprSEs, 
                                  sumCoefs, sumSEs,
                                  autCoefs, autSEs)

## Making full tables for all terms for the appendix ##

coefNamesApp <- list("seasonTestesnonrepFemale", "seasonTestesreproductiveFemale", 
                     "seasonTestesIntermediate", "seasonTestesDescended", "totalN", 
                     "totalN:Age", "totalN:Reproductive.female1", 
                     c("Reproductive.female1:Age", "Age:Reproductive.female1"),
                     c("descendedTestes1:Age", "Age:descendedTestes1"),
                     "preWinterTemp", "preWinterRain", "preWinterSigma", "preWinterCV",
                     c("preWinterTemp:Reproductive.female1", "Reproductive.female1:preWinterTemp"),
                     c("Reproductive.female1:preWinterRain", "preWinterRain:Reproductive.female1"),
                     c("Reproductive.female1:preWinterSigma", "preWinterSigma:Reproductive.female1"),
                     c("Reproductive.female1:preWinterCV", "preWinterCV:Reproductive.female1"),
                     c("preWinterTemp:Age", "Age:preWinterTemp"),
                     c("Age:preWinterRain", "preWinterRain:Age"),
                     c("Age:preWinterSigma", "preWinterSigma:Age"),
                     c("Age:preWinterCV", "preWinterCV:Age"),
                     "springTemp", "springTemp2", "springRain", "springRain2",
                     "springSigma", "springCV",
                     c("springTemp:Reproductive.female1", "Reproductive.female1:springTemp"),
                     c("Reproductive.female1:springRain", "springRain:Reproductive.female1"),
                     c("Reproductive.female1:springSigma", "springSigma:Reproductive.female1"),
                     c("Reproductive.female1:springCV", "springCV:Reproductive.female1"),
                     c("springTemp:Age", "Age:springTemp"),
                     c("Age:springRain", "springRain:Age"),
                     c("Age:springSigma", "springSigma:Age"),
                     c("Age:springCV", "springCV:Age"),
                     "dryTemp30", "dryRain30", "drySigma30", "dryCV30",
                     c("dryTemp30:Reproductive.female1", "Reproductive.female1:dryTemp30"),
                     c("Reproductive.female1:dryRain30", "dryRain30:Reproductive.female1"),
                     c("Reproductive.female1:drySigma30", "drySigma30:Reproductive.female1"),
                     c("Reproductive.female1:dryCV30", "dryCV30:Reproductive.female1"),
                     c("dryTemp30:Age", "Age:dryTemp30"),
                     c("Age:dryRain30", "dryRain30:Age"),
                     c("Age:drySigma30", "drySigma30:Age"),
                     c("Age:dryCV30", "dryCV30:Age"),
                     "autumnTemp", "autumnRain", "autumnSigma", "autumnCV", "autumnCV2",
                     c("autumnTemp:Reproductive.female1", "Reproductive.female1:autumnTemp"),
                     c("Reproductive.female1:autumnRain", "autumnRain:Reproductive.female1"),
                     c("Reproductive.female1:autumnSigma", "autumnSigma:Reproductive.female1"),
                     c("Reproductive.female1:autumnCV", "autumnCV:Reproductive.female1"),
                     c("autumnTemp:Age", "Age:autumnTemp"),
                     c("Age:autumnRain", "autumnRain:Age"),
                     c("Age:autumnSigma", "autumnSigma:Age"),
                     c("Age:autumnCV", "autumnCV:Age"))
rnamesApp <- c("Female", "Rep. female", 
               "Int. testes", "Desc. testes", "N", 
               "N:Age", "N:Rep.female", "Rep.female:Age", "Desc. testes:Age",
               "\u03BCT", "\u03BCR", "sigmaT", "CVR",
               "\u03BCT:Rep.female", "\u03BCR:Rep.female", "sigmaT:Rep.female", "CVR:Rep.female",
               "\u03BCT:Age", "\u03BCR:Age", "sigmaT:Age", "CVR:Age",
               "\u03BCT", "\u03BCT^2", "\u03BCR", "\u03BCR^2", 
               "sigmaT", "CVR",
               "\u03BCT:Rep.female", "\u03BCR:Rep.female", "sigmaT:Rep.female", "CVR:Rep.female",
               "\u03BCT:Age", "\u03BCR:Age", "sigmaT:Age", "CVR:Age",
               "\u03BCT", "\u03BCR", "sigmaT", "CVR",
               "\u03BCT:Rep.female", "\u03BCR:Rep.female", "sigmaT:Rep.female", "CVR:Rep.female",
               "\u03BCT:Age", "\u03BCR:Age", "sigmaT:Age", "CVR:Age",
               "\u03BCT", "\u03BCR", "sigmaT", "CVR", "CVR^2",
               "\u03BCT:Rep. female", "\u03BCR:Rep. female", "sigmaT:Rep. female", "CVR:Rep. female",
               "\u03BCT:Age", "\u03BCR:Age", "sigmaT:Age", "CVR:Age")
seasonApp <- c(NA, NA, NA,
               NA, NA, NA, NA, NA, NA,
               "Winter", "Winter", "Winter", "Winter", 
               "Winter", "Winter", "Winter", "Winter",
               "Winter", "Winter", "Winter", "Winter",
               "Spring", "Spring", "Spring", "Spring", "Spring", "Spring",
               "Spring", "Spring", "Spring", "Spring",
               "Spring", "Spring", "Spring", "Spring",
               "Summer", "Summer", "Summer", "Summer",
               "Summer", "Summer", "Summer", "Summer",
               "Summer", "Summer", "Summer", "Summer",
               "Autumn", "Autumn", "Autumn", "Autumn", "Autumn",
               "Autumn", "Autumn", "Autumn", "Autumn",
               "Autumn", "Autumn", "Autumn", "Autumn")
seasonTitleApp <- c("", "", "Spring model", "Spring model", "Spring model",
                    "Summer model", "Summer model", "Summer model",
                    "Autumn model", "Autumn model", "Autumn model")

sprCoefsApp <- rep(NA, length(rnamesApp))
sumCoefsApp <- rep(NA, length(rnamesApp))
autCoefsApp <- rep(NA, length(rnamesApp))

sprSEsApp   <- rep(NA, length(rnamesApp))
sumSEsApp   <- rep(NA, length(rnamesApp))
autSEsApp   <- rep(NA, length(rnamesApp))

sprPsApp    <- rep(NA, length(rnamesApp))
sumPsApp    <- rep(NA, length(rnamesApp))
autPsApp    <- rep(NA, length(rnamesApp))

sprNamesApp <- names(spr$p.coeff)
sumNamesApp <- names(sum$p.coeff)
autNamesApp <- names(aut$p.coeff)

for (i in 1:length(coefNamesApp)) {
  coefName <- coefNamesApp[[i]]
  sprCoefPos <- which(sprNamesApp %in% coefName)
  
  if (length(sprCoefPos) > 0) {
    sprCoefsApp[i] <- asteriskMarker(spr$p.coeff[sprCoefPos],
                                     spr$p.pv[sprCoefPos])
    sprSEsApp[i]   <- as.character(round(spr$se[sprCoefPos], dict$rounding + 1))
    sprPsApp[i]    <- round(spr$p.pv[sprCoefPos], dict$rounding + 1)
  }
  
  sumCoefPos <- which(sumNames %in% coefName)
  
  if (length(sumCoefPos) > 0) {
    sumCoefsApp[i] <- asteriskMarker(sum$p.coeff[sumCoefPos],
                                     sum$p.pv[sumCoefPos])
    sumSEsApp[i]   <- as.character(round(sum$se[sumCoefPos], dict$rounding + 1))
    sumPsApp[i]    <- round(sum$p.pv[sumCoefPos], dict$rounding + 1)
  }
  
  autCoefPos <- which(autNames %in% coefName)
  
  if (length(autCoefPos) > 0) {
    autCoefsApp[i] <- asteriskMarker(aut$p.coeff[autCoefPos],
                                     aut$p.pv[autCoefPos])
    autSEsApp[i]   <- as.character(round(aut$se[autCoefPos], dict$rounding + 1))
    autPsApp[i]    <- round(aut$p.pv[autCoefPos], dict$rounding + 1)
  }
}

conditionModelFrameApp <- data.frame(seasonApp, rnamesApp,
                                     sprCoefsApp, sprSEsApp, sprPsApp,
                                     sumCoefsApp, sumSEsApp, sumPsApp,
                                     autCoefsApp, autSEsApp, autPsApp)

save(conditionModelFrame, rnames, season, seasonTitle, 
     summaryTable, sex.summary, metric.summary, seasonTitle.summary,
     conditionModelFrameApp, rnamesApp, seasonApp, seasonTitleApp, 
     file = "Condition tables.RData")


### Exporting pairwise contrasts (to be added to in Survival modelling and Reproductive modelling) ###

conditionContrasts <- rbind(meansSpr, meansSum, meansAut)
attr(conditionContrasts, which = "mesg") <- NULL

modelVector <- c(rep("Spring BCI~", nrow(meansSpr)),
                 rep("Summer BCI~", nrow(meansSum)), 
                 rep("Autumn BCI~", nrow(meansAut)))

save(conditionContrasts, modelVector, file = "Condition contrast table.RData")

### Making interesting plots ###

dict$color.spring <- "#076000"
dict$color.autumn <- "#a85d01"

## Making plot of seasonal and age-based metrics ##

agesToSample <- c(2, 9)

conditionFigureHolder        <- data.frame(matrix(nrow = 0, ncol = ncol(validSeasons)))
names(conditionFigureHolder) <- names(validSeasons)

for (i in 1:length(agesToSample)) {
  temp <- validSeasons
  
  temp <- temp %>%
    mutate(Age = agesToSample[i])
  
  predictions <- predict(overallModel$gam, newdata = temp, se.fit = T)
  
  temp <- temp %>%
    mutate(Pred  = predictions$fit, 
           Upper = Pred + 1.96*predictions$se.fit,
           Lower = Pred - 1.96*predictions$se.fit)
  
  conditionFigureHolder <- rbind(conditionFigureHolder, temp)
  
}

conditionFigureHolder$Age <- factor(conditionFigureHolder$Age)

midpointDates  <- c("15/04/2000", "15/05/2000", "15/06/2000", "15/07/2000", 
                    "15/08/2000", "15/09/2000", "15/10/2000", "15/11/2000")
monthPositions <- yday(as.Date(midpointDates))
monthPositions <- monthPositions[c(2, 4, 6, 8)]

descriptiveConditionFig <- ggplot(conditionFigureHolder, aes(x = dayOfYear)) + 
  geom_boxplot(data = validSeasons,
               aes(y = BCI, colour = Season, group = Season)) +
  geom_line(aes(y = Pred, colour = Age), alpha = 0.7) +
  scale_colour_manual(breaks = c("2", "9", "Autumn", "Spring", "Summer"),
                      values = c(dict$color.2, dict$color.9,
                                 dict$color.autumn, dict$color.spring, dict$color.summer)) +
  geom_ribbon(aes(ymax = Upper, ymin = Lower, fill = Age), alpha = 0.2) +
  scale_fill_manual(breaks = c("2", "9"),
                    values = c(dict$color.2, dict$color.9)) +
  xlab("Month") + 
  scale_x_continuous(breaks = monthPositions,
                     labels = c("May", "Jul.", "Sep.", "Nov.")) + 
  facet_grid(.~Sex) + 
  theme_bw() +
  guides(fill = FALSE, colour = FALSE)

## Relationship between age and dry-30 temperature ##

ageBins <- c(2, 9)

# Summer first #

summerPred <- summerobj$data

#Just adding a min temp record for old individuals

coldPos <- which(summerPred$dryTemp30 == min(summerPred$dryTemp30))
summerPred$Age[coldPos][1] <- max(summerPred$Age)

summerPred <- allButAverager(summerPred, 
                             terms    = c("Age", "dryTemp30", "BCI"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Descended"))

summerPred$Age <- ageBin(Ages         = summerPred$Age, 
                         standardized = TRUE, 
                         originalAges = summerCompleteCondition$Age)

summerPredM <- predict(summerModelFinal$gam, newdata = summerPred, se.fit = TRUE)

summerPred <- allButAverager(summerPred, 
                             terms    = c("Age", "dryTemp30", "BCI"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale"))

summerPredF <- predict(summerModelFinal$gam, newdata = summerPred, se.fit = TRUE)

summerPred$predictions <- (summerPredM$fit + summerPredF$fit)/2
summerPred$upper <- summerPred$predictions + 1.96*(summerPredM$se.fit + summerPredF$se.fit)/2
summerPred$lower <- summerPred$predictions - 1.96*(summerPredM$se.fit + summerPredF$se.fit)/2

summerPred$Age <- factor(summerPred$Age)

# Now autumn #

autumnPred <- autumnobj$data

autumnPred <- allButAverager(autumnPred, 
                             terms    = c("Age", "dryTemp30", "BCI"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Male",
                                                   seasonTestes = "Ascended"))

autumnPred$Age <- ageBin(Ages         = autumnPred$Age, 
                         standardized = TRUE, 
                         originalAges = autumnCompleteCondition$Age)

autumnPredM <- predict(autumnModelFinal$gam, newdata = autumnPred, se.fit = TRUE)

autumnPred <- allButAverager(autumnPred, 
                             terms    = c("Age", "dryTemp30", "BCI"), 
                             defaults = data.frame(Reproductive.female = "0",
                                                   Sex = "Female",
                                                   seasonTestes = "nonrepFemale"))

autumnPredF <- predict(autumnModelFinal$gam, newdata = autumnPred, se.fit = TRUE)

autumnPred$predictions <- (autumnPredM$fit + autumnPredF$fit)/2
autumnPred$upper <- autumnPred$predictions + 1.96*(autumnPredM$se.fit + autumnPredF$se.fit)/2
autumnPred$lower <- autumnPred$predictions - 1.96*(autumnPredM$se.fit + autumnPredF$se.fit)/2

autumnPred$Age <- factor(autumnPred$Age)

# Now the old vs. young effects of autumn rainfall #

autumnPred2 <- autumnobj$data

autumnPred2 <- allButAverager(autumnPred2, 
                              terms    = c("Age", "autumnRain", "BCI"), 
                              defaults = data.frame(Reproductive.female = "0",
                                                    Sex = "Male",
                                                    seasonTestes = "Ascended"))

autumnPred2$Age <- ageBin(Ages         = autumnPred2$Age, 
                          standardized = TRUE, 
                          originalAges = autumnCompleteCondition$Age)

autumnPredM <- predict(autumnModelFinal$gam, newdata = autumnPred2, se.fit = TRUE)

autumnPred2 <- allButAverager(autumnPred2, 
                              terms    = c("Age", "autumnRain", "BCI"), 
                              defaults = data.frame(Reproductive.female = "0",
                                                    Sex = "Female",
                                                    seasonTestes = "nonrepFemale"))

autumnPredF <- predict(autumnModelFinal$gam, newdata = autumnPred2, se.fit = TRUE)

autumnPred2$predictions <- (autumnPredM$fit + autumnPredF$fit)/2
autumnPred2$upper <- autumnPred2$predictions + 1.96*(autumnPredM$se.fit + autumnPredF$se.fit)/2
autumnPred2$lower <- autumnPred2$predictions - 1.96*(autumnPredM$se.fit + autumnPredF$se.fit)/2

autumnPred2$Age <- factor(autumnPred2$Age)

# Putting into figures #

maxBCI <- max(c(summerPred$upper, autumnPred$upper, autumnPred2$upper))
minBCI <- min(c(summerPred$lower, autumnPred$lower, autumnPred2$lower))

summerConditionPlot <- ggplot(summerPred, aes(x = summerCompleteCondition$dryTemp30, y = predictions)) +
  geom_line(aes(y = predictions, colour = Age)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Summer BCI") + xlab("Mean daily temperature in driest 30-day period") + 
  theme_bw() +
  scale_color_manual(name = "Age", 
                     breaks = as.character(levels(summerPred$Age)),
                     labels = as.character(ageBins),
                     values = c(dict$color.2, dict$color.9)) + 
  scale_fill_manual(name = "Age", 
                    breaks = as.character(levels(summerPred$Age)),
                    labels = as.character(ageBins),
                    values = c(dict$color.2, dict$color.9)) +
  scale_y_continuous(limits = c((minBCI - 0.1), (maxBCI + 0.1)),
                     sec.axis = sec_axis(~.*sd(summerCompleteCondition$BCI) + mean(summerCompleteCondition$BCI))) +
  scale_x_continuous(breaks = seq(ceiling(min(summerCompleteCondition$dryTemp30)), round(max(summerCompleteCondition$dryTemp30)), by = 2)) +
  guides(color = FALSE, fill = FALSE)


autumnConditionPlot <- ggplot(autumnPred, aes(x = autumnCompleteCondition$dryTemp30, y = predictions)) +
  geom_line(aes(y = predictions, colour = Age)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Autumn BCI") + xlab("Mean daily temperature in driest 30-day period") + 
  theme_bw() +
  scale_color_manual(name = "Age", 
                     breaks = as.character(levels(autumnPred$Age)),
                     labels = as.character(ageBins),
                     values = c(dict$color.2, dict$color.9)) + 
  scale_fill_manual(name = "Age", 
                    breaks = as.character(levels(autumnPred$Age)),
                    labels = as.character(ageBins),
                    values = c(dict$color.2, dict$color.9)) +
  scale_y_continuous(limits = c((minBCI - 0.1), (maxBCI + 0.1)),
                     sec.axis = sec_axis(~.*sd(autumnCompleteCondition$BCI) + mean(autumnCompleteCondition$BCI))) +
  scale_x_continuous(breaks = seq(ceiling(min(autumnCompleteCondition$dryTemp30)), round(max(autumnCompleteCondition$dryTemp30)), by = 2))+
  guides(color = FALSE, fill = FALSE)

autumnConditionPlot2 <- ggplot(autumnPred2, aes(x = autumnCompleteCondition$autumnRain, y = predictions)) +
  geom_line(aes(y = predictions, colour = Age)) +
  geom_ribbon(aes(ymax = upper, 
                  ymin = lower, 
                  fill = Age), 
              alpha = 0.2) + 
  ylab("Autumn BCI") + xlab("Mean daily autumnal rainfall (mm)") + 
  theme_bw() +
  scale_color_manual(name = "Age", 
                     breaks = as.character(levels(autumnPred$Age)),
                     labels = as.character(ageBins),
                     values = c(dict$color.2, dict$color.9)) + 
  scale_fill_manual(name = "Age", 
                    breaks = as.character(levels(autumnPred$Age)),
                    labels = as.character(ageBins),
                    values = c(dict$color.2, dict$color.9)) +
  scale_y_continuous(limits = c((minBCI - 0.1), (maxBCI + 0.1)),
                     sec.axis = sec_axis(~.*sd(autumnCompleteCondition$BCI) + mean(autumnCompleteCondition$BCI))) +
  guides(color = FALSE, fill = FALSE)

## Export figures ##

save(descriptiveConditionFig, file = "Condition plots.RData")

save(summerConditionPlot, autumnConditionPlot, autumnConditionPlot2, file = "Condition plots to combine with survival.RData")
