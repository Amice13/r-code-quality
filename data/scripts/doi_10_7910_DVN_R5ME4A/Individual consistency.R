########################################################
# Title: Exploring individual body condition consistency
# Name: Julius G. Bright Ross
# Date: Mar 10, 2020
# Last updated: July 2, 2020
# Description: Computing percentile positions of 
# individual condition and exploring their correlates
########################################################

## Sourcing in condition modelling output ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("Condition modelling output.RData")

consDict <- list()

library(mgcv)
library(dplyr)
library(ggplot2)
library(cowplot)

## Useful functions ##

pStore <- function(pValue) {
  if (pValue < 0.001) {
    return("< 0.001")
  } else if (0.001 <= pValue & pValue <= 0.099) {
    return(paste("=", format(round(pValue, 3), nsmall = 3)))
  } else {
    return(paste("=", format(round(pValue, 2), nsmall = 2)))
  }
}

reverseStandardize <- function(variable, toReverse) {
  sdVar    <- sd(variable)
  meanVar  <- mean(variable)
  
  reversed <- toReverse*sdVar + meanVar
  
  return(reversed)
}

## Predicting from final models and computing residuals ##

#Note: We're using predictions without sett or individual considered; 
#uncomment below to include sett

# springCompleteCondition$Predicted <- predict(springModelFinal$lme, level = 3)
# summerCompleteCondition$Predicted <- predict(summerModelFinal$lme, level = 3)
# autumnCompleteCondition$Predicted <- predict(autumnModelFinal$lme, level = 3)

springCompleteCondition$Predicted <- predict(springModelFinal$gam)
summerCompleteCondition$Predicted <- predict(summerModelFinal$gam)
autumnCompleteCondition$Predicted <- predict(autumnModelFinal$gam)

springCompleteCondition$Residual  <- springCompleteCondition$BCI - 
  reverseStandardize(variable = springCompleteCondition$BCI, toReverse = springCompleteCondition$Predicted)

summerCompleteCondition$Residual <- summerCompleteCondition$BCI -
  reverseStandardize(variable = summerCompleteCondition$BCI, toReverse = summerCompleteCondition$Predicted)

autumnCompleteCondition$Residual <- autumnCompleteCondition$BCI -
  reverseStandardize(variable = autumnCompleteCondition$BCI, toReverse = autumnCompleteCondition$Predicted)

## Organising these into one dataframe ##

collatedCondition <- springCompleteCondition %>%
  select(Season, Year, Individual, Residual, BCI, Sex, Age, Reproductive.female, seasonTestes) %>%
  rbind(
    summerCompleteCondition %>% 
      select(Season, Year, Individual, Residual, BCI, Sex, Age, Reproductive.female, seasonTestes),
    autumnCompleteCondition %>%
      select(Season, Year, Individual, Residual, BCI, Sex, Age, Reproductive.female, seasonTestes)
    
  ) %>% 
  mutate(Individual = as.character(Individual))

## Computing percentile position relative to the whole population (not just in over 9 records) ##

year       <- c()
season     <- c()
individual <- c()
percentile <- c()
residual   <- c()
poolSize   <- c()

differentYears <- unique(collatedCondition$Year)

for (i in differentYears) {
  
  collatedSubset <- subset(collatedCondition, Year == i)
  
  # Spring first #
  
  springSubset <- subset(collatedSubset, Season == "Spring") %>%
    arrange(Residual)
  
  if (nrow(springSubset) > 0) {
    residualPile <- springSubset$Residual
    
    uniqueIndividuals <- unique(springSubset$Individual)
    
    for (j in 1:length(uniqueIndividuals)) {
      ind <- uniqueIndividuals[j]
      
      indPos       <- which(springSubset$Individual == ind)
      indResiduals <- mean(residualPile[indPos])
      
      indPerc <- (mean(indPos)/nrow(springSubset))*100
      
      stopifnot (length(ind) == length(indResiduals)) 
      
      individual <- c(individual, ind)
      residual   <- c(residual, indResiduals)
      percentile <- c(percentile, indPerc)
    }
    
    poolSize <- c(poolSize, rep(length(uniqueIndividuals), length(uniqueIndividuals)))
    season   <- c(season, rep("Spring", length(uniqueIndividuals)))
    year     <- c(year, rep(i, length(uniqueIndividuals)))
  }

  
  # Now summer #
  
  summerSubset <- subset(collatedSubset, Season == "Summer") %>%
    arrange(Residual)
  
  if (nrow(summerSubset) > 0) {
    residualPile <- summerSubset$Residual
    
    uniqueIndividuals <- unique(summerSubset$Individual)
    
    for (j in 1:length(uniqueIndividuals)) {
      ind <- uniqueIndividuals[j]
      
      indPos       <- which(summerSubset$Individual == ind)
      indResiduals <- mean(residualPile[indPos])
      
      indPerc <- (mean(indPos)/nrow(summerSubset))*100
      
      stopifnot (length(ind) == length(indResiduals)) 
      
      individual <- c(individual, ind)
      residual   <- c(residual, indResiduals)
      percentile <- c(percentile, indPerc)
    }
    
    poolSize <- c(poolSize, rep(length(uniqueIndividuals), length(uniqueIndividuals)))
    season   <- c(season, rep("Summer", length(uniqueIndividuals)))
    year     <- c(year, rep(i, length(uniqueIndividuals)))
  }
  
  
  # Finally, autumn #
  
  autumnSubset <- subset(collatedSubset, Season == "Autumn") %>%
    arrange(Residual)
  
  if (nrow(autumnSubset) > 0) {
    residualPile <- autumnSubset$Residual
    
    uniqueIndividuals <- unique(autumnSubset$Individual)
    
    for (j in 1:length(uniqueIndividuals)) {
      ind <- uniqueIndividuals[j]
      
      indPos       <- which(autumnSubset$Individual == ind)
      indResiduals <- mean(residualPile[indPos])
      
      indPerc <- (mean(indPos)/nrow(autumnSubset))*100
      
      stopifnot (length(ind) == length(indResiduals)) 
      
      individual <- c(individual, ind)
      residual   <- c(residual, indResiduals)
      percentile <- c(percentile, indPerc)
    }
    
    poolSize <- c(poolSize, rep(length(uniqueIndividuals), length(uniqueIndividuals)))
    season   <- c(season, rep("Autumn", length(uniqueIndividuals)))
    year     <- c(year, rep(i, length(uniqueIndividuals)))
  }
  
}

## Combining into a data frame and subsetting to individuals with 10 or more observations ##

residualFrame <- data.frame(year, season, individual, residual, percentile, poolSize) %>%
  subset(individual %in% names(table(individual)[table(individual) >= 10]))

names(residualFrame) <- c("Year", "Season", "Individual", "Residual", 
                          "Percentile", "Poolsize")

residualFrame <- merge(residualFrame, 
           collatedCondition %>% 
             distinct(Individual, Year, Season, .keep_all = TRUE) %>%
             select(-Residual), 
           by = c("Individual", "Year", "Season"))

residualFrame <- residualFrame %>% 
  group_by(Individual) %>%
  mutate(AvgPercentile = mean(Percentile),
         SdPercentile = sd(Percentile),
         MaxPercentile = max(Percentile),
         MinPercentile = min(Percentile)) %>%
  ungroup() %>%
  group_by(Individual, Season) %>%
  mutate(SeasonAvgPercentile = mean(Percentile),
         SeasonSdPercentile = sd(Percentile)) %>%
  ungroup()

consDict$total.n <- residualFrame %>%
  distinct(Individual, .keep_all = TRUE) %>%
  nrow()



## Giving them all cohorts to subset on ##

load("C:/Users/jbrig/Desktop/Badgers/Badger condition/R Code/Prepared badger data_9-4-20.RData")
load("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets/Reproductive individuals.RData")

cohortFrame <- data.frame(Individual = badgerIndices, Cohort = cohortAll)

names(cohortFrame)[2] <- c("Cohort")
names(totalOutput)[1] <- "Individual"

residualFrame <- residualFrame %>% 
  merge(cohortFrame, all.x = TRUE, by = "Individual")

## Making a frame for realised lifespan ##

lastCapture <- rep(NA, length(badgerIndices))

for (i in 1:length(badgerIndices)) {
  lastCapture[i] <- max(which(originalAliveYearly[i,] == 1)) + MNA$Year[1] - 1
}

ageFrame <- data.frame(Individual  = badgerIndices,
                       MaxAge      = apply(ageYearly, 1, max),
                       lastCapture = lastCapture)

residualFrameMaxAge <- residualFrame %>%
  distinct(Individual, .keep_all = TRUE) %>%
  merge(ageFrame, by = "Individual", all.x = TRUE) %>%
  subset(lastCapture <= (max(MNA$Year) - 2)) #Limiting to 2 years from end of study

consDict$last.maxage.yr   <- max(MNA$Year) - 2
consDict$maxage.n.male    <- residualFrameMaxAge %>% subset(Sex == "Male") %>% nrow()
consDict$maxage.n.female  <- residualFrameMaxAge %>% subset(Sex == "Female") %>% nrow()
consDict$median.maxage    <- median(residualFrameMaxAge$MaxAge)

## Making a frame for total reproductive output ##

residualFrameTotalRep <- residualFrame %>% 
  distinct(Individual, .keep_all = TRUE) %>%
  subset(Cohort <= reproductiveIndividuals %>%
           select(Cohort) %>%
           pull() %>% as.character() %>% as.numeric() %>%
           max()) %>% #Limiting to those within 8 years of end of pedigree
  merge(totalOutput %>% select(Individual, Total_offspring), 
        by = "Individual", all.x = TRUE)

consDict$last.lrs.yr   <- reproductiveIndividuals %>%
  select(Cohort) %>%
  pull() %>% as.character() %>% as.numeric() %>%
  max()
consDict$lrs.n.male    <- residualFrameTotalRep %>% subset(Sex == "Male") %>% nrow()
consDict$lrs.n.female  <- residualFrameTotalRep %>% subset(Sex == "Female") %>% nrow()

## Making a frame for age at first reproduction ##

residualFrameAlpha <- residualFrame %>% 
  distinct(Individual, .keep_all = TRUE) %>%
  merge(totalOutput %>% select(Individual, Alpha),
        by = "Individual", all.x = TRUE) %>%
  subset(!is.na(Alpha)) #Limiting to those who did breed

malePos <- which(residualFrameAlpha$Sex == "Male")
residualFrameAlpha$Alpha[malePos] <- residualFrameAlpha$Alpha[malePos] - 1
#^Correcting male alpha scores to be when they mated not bred

consDict$alpha.n.male   <- residualFrameAlpha %>% subset(Sex == "Male") %>% nrow()
consDict$alpha.n.female <- residualFrameAlpha %>% subset(Sex == "Female") %>% nrow()

### Building models ###

## Realised lifespan ##

maleAvgLife   <- glm(MaxAge ~ AvgPercentile,
                     data = residualFrameMaxAge %>% subset(Sex == "Male"))
femaleAvgLife <- glm(MaxAge ~ AvgPercentile,
                     data = residualFrameMaxAge %>% subset(Sex == "Female"))

maleSdLife   <- glm(MaxAge ~ SdPercentile,
                    data = residualFrameMaxAge %>% subset(Sex == "Male"))
femaleSdLife <- glm(MaxAge ~ SdPercentile,
                    data = residualFrameMaxAge %>% subset(Sex == "Female"))

maleAvgSum <- summary(maleAvgLife)

consDict$p.m.avg.life <- pStore(maleAvgSum$coefficients[,4][which(row.names(maleAvgSum$coefficients) == "AvgPercentile")])

maleSdSum <- summary(maleSdLife)

consDict$p.m.sd.life <- pStore(maleSdSum$coefficients[,4][which(row.names(maleSdSum$coefficients) == "SdPercentile")])


## Total offspring ##

maleAvgOff   <- glm(Total_offspring ~ AvgPercentile,
                    data = residualFrameTotalRep %>% subset(Sex == "Male"),
                    family = poisson)
femaleAvgOff <- glm(Total_offspring ~ AvgPercentile,
                    data = residualFrameTotalRep %>% subset(Sex == "Female"),
                    family = poisson)

maleSdOff   <- glm(Total_offspring ~ SdPercentile,
                   data = residualFrameTotalRep %>% subset(Sex == "Male"),
                   family = poisson)
femaleSdOff <- glm(Total_offspring ~ SdPercentile,
                   data = residualFrameTotalRep %>% subset(Sex == "Female"),
                   family = poisson)

maleAvgSum <- summary(maleAvgOff)

consDict$p.m.avg.lrs <- pStore(maleAvgSum$coefficients[,4][which(row.names(maleAvgSum$coefficients) == "AvgPercentile")])

femaleAvgSum <- summary(femaleAvgOff)

consDict$p.f.avg.lrs <- pStore(femaleAvgSum$coefficients[,4][which(row.names(femaleAvgSum$coefficients) == "AvgPercentile")])


maleSdSum <- summary(maleSdOff)

consDict$p.m.sd.lrs <- pStore(maleSdSum$coefficients[,4][which(row.names(maleSdSum$coefficients) == "SdPercentile")])

## Age at first reproduction ##

maleAvgAlpha   <- glm(Alpha ~ AvgPercentile,
                      data = residualFrameAlpha %>% subset(Sex == "Male"),
                    family = poisson)
femaleAvgAlpha <- glm(Alpha ~ AvgPercentile,
                    data = residualFrameAlpha %>% subset(Sex == "Female"),
                    family = poisson)

maleSdAlpha   <- glm(Alpha ~ SdPercentile,
                   data = residualFrameAlpha %>% subset(Sex == "Male"),
                   family = poisson)
femaleSdAlpha <- glm(Alpha ~ SdPercentile,
                   data = residualFrameAlpha %>% subset(Sex == "Female"),
                   family = poisson)

maleSdSum <- summary(maleSdAlpha)

consDict$p.m.sd.alpha <- pStore(maleSdSum$coefficients[,4][which(row.names(maleSdSum$coefficients) == "SdPercentile")])

### Interesting figures ###

## First, sd-perc ~ mu-perc ##

bananaPlot <- ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE),
                     aes(x = AvgPercentile, y = SdPercentile)) + 
  geom_point() +
  theme_bw() + 
  ylab(expression(sigma[Perc])) +
  xlab(expression(mu[Perc])) + ylim(c(0, 35)) + xlim(c(0, 100))

## Then, range-perc ~ mu-perc ##

linesPlot <- ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE), 
                    aes(x = AvgPercentile, y = Percentile)) + 
  geom_pointrange(aes(ymin = MinPercentile, ymax = MaxPercentile, y = AvgPercentile), size = 0.2) +
  ylab("Range of BCI percentiles occupied") + xlab(expression(paste("Average lifetime BCI percentile (", mu[Perc], ")", sep = "")))+ 
  theme_bw()

ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE), 
                       aes(x = AvgPercentile, y = Percentile)) + 
  geom_pointrange(aes(ymin = MinPercentile, ymax = MaxPercentile, y = AvgPercentile), size = 0.2) +
  ylab("Range of BCI percentiles occupied") + xlab(expression(paste("Average lifetime BCI percentile (", mu[Perc], ")", sep = "")))+ 
  theme_bw() + geom_vline(xintercept = 24, color = "blue") + geom_vline(xintercept = 79, color = "red")

consDict$type1.perc <- (residualFrame %>% distinct(Individual, .keep_all = TRUE) %>% 
                          subset(AvgPercentile < 24) %>% nrow())/(residualFrame %>% 
                                                                    distinct(Individual, .keep_all = TRUE) %>%
                                                                    nrow())*100
consDict$type2.perc <- (residualFrame %>% distinct(Individual, .keep_all = TRUE) %>% 
                          subset(AvgPercentile > 24 & AvgPercentile < 79) %>% nrow())/(residualFrame %>% 
                                                                                         distinct(Individual, .keep_all = TRUE) %>%
                                                                                         nrow())*100
consDict$type3.perc <- (residualFrame %>% distinct(Individual, .keep_all = TRUE) %>% 
                          subset(AvgPercentile > 79) %>% nrow())/(residualFrame %>% 
                                                                    distinct(Individual, .keep_all = TRUE) %>%
                                                                    nrow())*100

# Storing some interesting numbers here # 

consDict$perc.below.25 <- (nrow(residualFrame %>% 
                                  distinct(Individual, .keep_all = TRUE) %>%
                                  subset(AvgPercentile <= 25))/nrow(residualFrame %>%
                                                                      distinct(Individual, .keep_all = TRUE))*100) %>%
  round(1)

consDict$perc.above.75 <- (nrow(residualFrame %>% 
                                  distinct(Individual, .keep_all = TRUE) %>%
                                  subset(AvgPercentile >= 75))/nrow(residualFrame %>%
                                                                      distinct(Individual, .keep_all = TRUE))*100) %>%
  round(1)

consDict$nested.perc.lrs <- (nrow(residualFrameTotalRep %>% 
                                    distinct(Individual, .keep_all = TRUE) %>%
                                    subset(AvgPercentile <= 25 & Total_offspring > 0))/nrow(residualFrame %>% 
                                                                                              distinct(Individual, .keep_all = TRUE) %>%
                                                                                              subset(AvgPercentile <= 25)))*100 

## Now the big figure of all the relationships ##

# Max lifespan #

ageMpred   <- predict(maleAvgLife, se.fit = TRUE)
ageFpred   <- predict(femaleAvgLife, se.fit = TRUE)
ageMpredsd <- predict(maleSdLife, se.fit = TRUE)
ageFpredsd <- predict(femaleSdLife, se.fit = TRUE)

pred    <- c(ageMpred$fit, ageFpred$fit)
upper   <- pred + 1.96*c(ageMpred$se.fit, ageFpred$se.fit)
lower   <- pred - 1.96*c(ageMpred$se.fit, ageFpred$se.fit)
predsd  <- c(ageMpredsd$fit, ageFpredsd$fit)
uppersd <- predsd + 1.96*c(ageMpredsd$se.fit, ageFpredsd$se.fit)
lowersd <- predsd - 1.96*c(ageMpredsd$se.fit, ageFpredsd$se.fit)
Sex     <- c(rep("Male", length(ageMpred$fit)), rep("Female", length(ageFpred$fit)))

maxAge  <- c(residualFrameMaxAge %>% subset(Sex == "Male") %>% select(MaxAge) %>% pull(),
            residualFrameMaxAge %>% subset(Sex == "Female") %>% select(MaxAge) %>% pull())

avgPerc <- c(residualFrameMaxAge %>% subset(Sex == "Male") %>% select(AvgPercentile) %>% pull(),
             residualFrameMaxAge %>% subset(Sex == "Female") %>% select(AvgPercentile) %>% pull())
sdPerc  <- c(residualFrameMaxAge %>% subset(Sex == "Male") %>% select(SdPercentile) %>% pull(),
             residualFrameMaxAge %>% subset(Sex == "Female") %>% select(SdPercentile) %>% pull())

maxAgePred <- data.frame(maxAge, pred, upper, lower, predsd, uppersd, lowersd, Sex, avgPerc, sdPerc)

maxAgeAvg <- ggplot(maxAgePred, aes(x = avgPerc, y = maxAge, colour = Sex, fill = Sex)) + 
  geom_point() + 
  geom_line(data = maxAgePred %>% subset(Sex == "Male"), 
            aes(x = avgPerc, y = pred)) + 
  geom_ribbon(data = maxAgePred %>% subset(Sex == "Male"), 
              aes(x = avgPerc, 
                  y = pred,
                  ymax = upper, 
                  ymin = lower,
                  fill = Sex),
              alpha = 0.2) +
  theme_bw() +
  ylab("Realised lifespan") +
  xlab("Average lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

maxAgeSd <- ggplot(maxAgePred, aes(x = sdPerc, y = maxAge, colour = Sex, fill = Sex)) + 
  geom_point() + 
  geom_line(data = maxAgePred %>% subset(Sex == "Male"), 
            aes(x = sdPerc, y = predsd)) + 
  geom_ribbon(data = maxAgePred %>% subset(Sex == "Male"), 
              aes(x = sdPerc, 
                  y = predsd,
                  ymax = uppersd, 
                  ymin = lowersd,
                  fill = Sex),
              alpha = 0.2) +
  theme_bw() +
  ylab("Realised lifespan") +
  xlab("SD of lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

# Total offspring #

offMpred   <- predict(maleAvgOff, se.fit = TRUE)
offFpred   <- predict(femaleAvgOff, se.fit = TRUE)
offMpredsd <- predict(maleSdOff, se.fit = TRUE)
offFpredsd <- predict(femaleSdOff, se.fit = TRUE)

pred    <- c(offMpred$fit, offFpred$fit)
upper   <- exp(pred + 1.96*c(offMpred$se.fit, offFpred$se.fit))
lower   <- exp(pred - 1.96*c(offMpred$se.fit, offFpred$se.fit))
pred    <- exp(pred)
predsd  <- c(offMpredsd$fit, offFpredsd$fit)
uppersd <- exp(predsd + 1.96*c(offMpredsd$se.fit, offFpredsd$se.fit))
lowersd <- exp(predsd - 1.96*c(offMpredsd$se.fit, offFpredsd$se.fit))
predsd  <- exp(predsd)
Sex     <- c(rep("Male", length(offMpred$fit)), rep("Female", length(offFpred$fit)))

LRS  <- c(residualFrameTotalRep %>% subset(Sex == "Male") %>% select(Total_offspring) %>% pull(),
             residualFrameTotalRep %>% subset(Sex == "Female") %>% select(Total_offspring) %>% pull())

avgPerc <- c(residualFrameTotalRep %>% subset(Sex == "Male") %>% select(AvgPercentile) %>% pull(),
             residualFrameTotalRep %>% subset(Sex == "Female") %>% select(AvgPercentile) %>% pull())
sdPerc  <- c(residualFrameTotalRep %>% subset(Sex == "Male") %>% select(SdPercentile) %>% pull(),
             residualFrameTotalRep %>% subset(Sex == "Female") %>% select(SdPercentile) %>% pull())

LRSPred <- data.frame(pred, upper, lower, predsd, uppersd, lowersd, Sex, avgPerc, sdPerc)

LRSAvg <- ggplot(LRSPred, aes(x = avgPerc, y = LRS, colour = Sex)) + 
  geom_point() + 
  geom_line(data = LRSPred %>% subset(Sex == "Female"),
            aes(x = avgPerc, y = pred)) + 
  geom_ribbon(data = LRSPred %>% subset(Sex == "Female"),
              aes(x = avgPerc, y = pred, ymax = upper, 
                  ymin = lower,
                  fill = Sex),
              alpha = 0.2) +
  theme_bw() +
  ylab("Lifetime reproductive success") +
  xlab("Average lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

LRSSd <- ggplot(LRSPred, aes(x = sdPerc, y = LRS, colour = Sex, fill = Sex)) + 
  geom_point() + 
  geom_line(data = LRSPred %>% subset(Sex == "Male"),
            aes(x = sdPerc, y = predsd)) + 
  geom_ribbon(data = LRSPred %>% subset(Sex == "Male"),
              aes(x = sdPerc, y = predsd, ymax = uppersd, 
                  ymin = lowersd,
                  fill = Sex),
              alpha = 0.2) +
  theme_bw() +
  ylab("Lifetime reproductive success") +
  xlab("SD of lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

# Alpha #

aMpred   <- predict(maleAvgAlpha, se.fit = TRUE)
aFpred   <- predict(femaleAvgAlpha, se.fit = TRUE)
aMpredsd <- predict(maleSdAlpha, se.fit = TRUE)
aFpredsd <- predict(femaleSdAlpha, se.fit = TRUE)

pred    <- c(aMpred$fit, aFpred$fit)
upper   <- exp(pred + 1.96*c(aMpred$se.fit, aFpred$se.fit))
lower   <- exp(pred - 1.96*c(aMpred$se.fit, aFpred$se.fit))
pred    <- exp(pred)
predsd  <- c(aMpredsd$fit, aFpredsd$fit)
uppersd <- exp(predsd + 1.96*c(aMpredsd$se.fit, aFpredsd$se.fit))
lowersd <- exp(predsd - 1.96*c(aMpredsd$se.fit, aFpredsd$se.fit))
predsd  <- exp(predsd)

Sex     <- c(rep("Male", length(aMpred$fit)), rep("Female", length(aFpred$fit)))

Alpha   <- c(residualFrameAlpha %>% subset(Sex == "Male") %>% select(Alpha) %>% pull(),
             residualFrameAlpha %>% subset(Sex == "Female") %>% select(Alpha) %>% pull())

avgPerc <- c(residualFrameAlpha %>% subset(Sex == "Male") %>% select(AvgPercentile) %>% pull(),
             residualFrameAlpha %>% subset(Sex == "Female") %>% select(AvgPercentile) %>% pull())
sdPerc  <- c(residualFrameAlpha %>% subset(Sex == "Male") %>% select(SdPercentile) %>% pull(),
             residualFrameAlpha %>% subset(Sex == "Female") %>% select(SdPercentile) %>% pull())

alphaPred <- data.frame(Alpha, pred, upper, lower, predsd, uppersd, lowersd, Sex, avgPerc, sdPerc)

alphaAvg <- ggplot(alphaPred, aes(x = avgPerc, y = Alpha, colour = Sex, fill = Sex)) + 
  geom_point() +
  theme_bw() +
  ylab("Age at first reproduction") +
  xlab("Average lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

alphaSd <- ggplot(alphaPred, aes(x = sdPerc, y = Alpha, colour = Sex, fill = Sex)) + 
  geom_point() + 
  geom_line(data = alphaPred %>% subset(Sex == "Male"),
            aes(x = sdPerc, y = predsd)) + 
  geom_ribbon(data = alphaPred %>% subset(Sex == "Male"),
              aes(x = sdPerc, y = predsd, ymax = uppersd, 
                  ymin = lowersd,
                  fill = Sex),
              alpha = 0.2) +
  theme_bw() +
  ylab("Age at first reproduction") +
  xlab("SD of lifetime BCI percentile") +
  guides(colour = FALSE, fill = FALSE)

consistencyPlots <- plot_grid(maxAgeAvg, maxAgeSd,
                              LRSAvg, LRSSd,
                              alphaAvg, alphaSd,
                              ncol = 2,
                              nrow = 3, 
                              align = "hv",
                              labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"))

## Simulation of null model ##

numberOfBadgers <- length(unique(collatedCondition$Individual))

steps <- collatedCondition %>% 
  group_by(Individual) %>%
  mutate(numSteps = n()) %>%
  ungroup %>%
  distinct(Individual, .keep_all = TRUE) %>%
  select(numSteps) %>%
  pull()

consDict$mean.captures <- round(mean(steps), 2)
consDict$max.captures  <- max(steps)
consDict$min.captures  <- min(steps)

means <- collatedCondition %>%
  group_by(Individual) %>%
  mutate(meanRes = mean(Residual)) %>%
  ungroup() %>%
  distinct(Individual, .keep_all = TRUE) %>%
  select(meanRes) %>%
  pull()

sdev <- collatedCondition %>%
  group_by(Individual) %>%
  mutate(numSteps = n()) %>%
  subset(numSteps > 1) %>%
  mutate(varResidual = sd(Residual)) %>%
  ungroup() %>%
  distinct(Individual, .keep_all = TRUE) %>%
  select(varResidual) %>%
  pull() %>%
  mean()

yearWeights <- collatedCondition %>%
  group_by(Year, Season) %>%
  distinct(Individual, Season, .keep_all = TRUE) %>%
  mutate(numIndividuals = n()) %>%
  ungroup() %>%
  distinct(Year, Season, .keep_all = TRUE) %>%
  select(numIndividuals) %>%
  pull()

consDict$num.seasons <- length(yearWeights)

yearSelectionFunction <- function(yearWeights, maxAge) {
  numYears  <- length(yearWeights)
  years     <- seq(1, numYears)
  
  firstYear <- numYears
  
  while ((firstYear + maxAge) > numYears) {
    firstYear <- sample(years, size = 1, prob = yearWeights)
  }
  
  return(firstYear)
}

individual <- c()
residual   <- c()
year       <- c()

set.seed(9453278) #WildCRU in numpad  

for (i in 1:numberOfBadgers) {
  indSteps <- sample(steps, 1)
  indMean  <- sample(means, 1)
  
  records  <- rnorm(n = indSteps, mean = indMean, sd = sdev)
  
  startingYear <- yearSelectionFunction(yearWeights = yearWeights, maxAge = indSteps)
  
  individual <- c(individual, rep(i, indSteps))
  residual   <- c(residual, records)
  year       <- c(year, seq(startingYear, startingYear + indSteps - 1))
}

simulationFrame <- data.frame(individual, residual, year) %>%
  arrange(residual)

percentile <- c()

for (i in 1:nrow(simulationFrame)) {
  rowYear <- simulationFrame$year[i]
  
  yearStack <- simulationFrame %>%
    subset(year == rowYear) %>%
    select(residual) %>%
    pull()
  
  position   <- which(yearStack == simulationFrame$residual[i])
  percentile <- c(percentile, position*100/length(yearStack))
  
}

simulationFrame$percentile <- percentile

simulationFrame <- simulationFrame %>%
  group_by(individual) %>%
  mutate(nRecords = n(),
         avgPercentile = mean(percentile),
         SdPercentile = sd(percentile),
         minPercentile = min(percentile),
         maxPercentile = max(percentile)) %>%
  ungroup() %>%
  subset(nRecords >= 10)

suppBananaPlot <- ggplot(simulationFrame %>% distinct(individual, .keep_all = TRUE), 
                         aes(x = avgPercentile, y = SdPercentile)) + 
  geom_point() + 
  ylab(expression(sigma[Perc])) +
  xlab(expression(mu[Perc])) +
  theme_bw() + ylim(c(0, 35)) + xlim(c(0, 100))

ggplot(simulationFrame %>% distinct(individual, .keep_all = TRUE), 
       aes(x = avgPercentile, y = percentile)) + 
  geom_pointrange(aes(ymax = maxPercentile, ymin = minPercentile, y = avgPercentile)) + 
  theme_bw()

ggplot(simulationFrame, aes(x = residual, y = percentile)) + geom_point()


save(bananaPlot, linesPlot, consistencyPlots, consDict, suppBananaPlot, 
     file = "Consistency plots and info.RData")

# 
# hist(residualFrame$AvgPercentile)
# 
# ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE), 
#        aes(x = AvgPercentile, y = MaxAge)) + 
#   geom_point() + geom_smooth()
# 
# ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE), 
#        aes(x = SdPercentile, y = MaxAge)) + 
#   geom_point() + geom_smooth()
# 
# ggplot(residualFrame, aes(x = AvgPercentile, y = SdPercentile)) + 
#   geom_point() + geom_smooth()
# 
# ggplot(residualFrame %>% distinct(Individual, Season, .keep_all = TRUE), 
#        aes(x = SeasonAvgPercentile, y = MaxAge)) + 
#   geom_point() + geom_smooth() + facet_grid(.~Season)
# 
# ggplot(residualFrame %>% distinct(Individual, Season, .keep_all = TRUE), 
#        aes(x = SeasonAvgPercentile, y = SeasonSdPercentile)) + 
#   geom_point() + geom_smooth() + facet_grid(.~Season)
# 
# 
# summary(glm(MaxAge ~ AvgPercentile, 
#             data = residualFrame %>%
#               distinct(Individual, .keep_all = TRUE)))
# 
# summary(glm(MaxAge ~ SdPercentile, 
#             data = residualFrame %>%
#               distinct(Individual, .keep_all = TRUE)))
# 
# summary(glm(Total_offspring ~ AvgPercentile + Sex, 
#             data = residualFrameRep))
# 
# lowestHighest <- rep(NA, nrow(residualFrame))
# 
# avgVector <- residualFrame %>%
#   distinct(Individual, .keep_all = TRUE) %>%
#   select(AvgPercentile) %>%
#   pull()
# 
# lowQuantile  <- quantile(avgVector, probs = 0.2)
# highQuantile <- quantile(avgVector, probs = 0.8)
# 
# lowestHighest[which(residualFrame$AvgPercentile <= lowQuantile)]  <- "Low"
# lowestHighest[which(residualFrame$AvgPercentile >= highQuantile)] <- "High"
# 
# residualFrame$lowestHighest <- lowestHighest
# 
# ggplot(subset(residualFrame, !is.na(lowestHighest)), 
#        aes(x = Year, y = Percentile, group = Individual, colour = lowestHighest)) + 
#   geom_line() + facet_grid(.~Season)
# 
# ggplot(residualFrame %>% distinct(Individual, .keep_all = TRUE),
#        aes(x = lowestHighest, y = SdPercentile)) + geom_boxplot() 
# 
# summary(glm(MaxAge ~ lowestHighest, 
#             data = residualFrame %>% 
#               subset(!is.na(lowestHighest)) %>%
#               distinct(Individual, .keep_all = TRUE)))
# 
# ggplot(residualFrame, aes(x = AvgPercentile, y= MaxAge)) + geom_point() + facet_grid(.~Sex)
# 
# summary(glm(MaxAge ~ AvgPercentile, 
#             data = residualFrame %>%
#               distinct(Individual, .keep_all = TRUE)))
# 
# summary(glm(SdPercentile ~ lowestHighest, 
#             data = residualFrame %>% 
#               subset(!is.na(lowestHighest)) %>% 
#               distinct(Individual, .keep_all = TRUE)))
# 
# load("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets/Reproductive individuals.RData")
# 
# a <- merge(residualFrame, reproductiveIndividuals, all.x =  TRUE, by = "Individual")
# a$Total_offspring[is.na(a$Total_offspring)] <- 0
# 
# summary(glm(Alpha ~ lowestHighest, 
#             data = a %>% 
#               subset(!is.na(lowestHighest)) %>% 
#               distinct(Individual, .keep_all = TRUE)))
# 
# ggplot(a %>% distinct(Individual, .keep_all = TRUE),
#        aes(x = AvgPercentile, y = Alpha)) + 
#   geom_point()
# 
# summary(glm(Alpha ~ AvgPercentile,
#             data = a %>% 
#               distinct(Individual, .keep_all = TRUE)))
# 
# ggplot(a %>% distinct(Individual, .keep_all = TRUE),
#        aes(x = AvgPercentile, y = Total_offspring)) + 
#   geom_point()
# 
# summary(glm(Total_offspring ~ lowestHighest, 
#             data = a %>% 
#               subset(!is.na(lowestHighest)) %>% 
#               distinct(Individual, .keep_all = TRUE),
#             family = poisson))
# 
# summary(glm(Total_offspring ~ AvgPercentile, 
#             data = a %>% 
#               subset(!is.na(lowestHighest)) %>% 
#               distinct(Individual, .keep_all = TRUE),
#             family = poisson))
# 
# 
# ggplot(residualFrame, aes(x = AvgPercentile, y = SdPercentile, colour = Sex)) + geom_point() 
# 
# ggplot(residualFrame %>% 
#          subset(AvgPercentile < 25 | AvgPercentile > 75),
#        aes(x = Individual, y = Percentile)) + geom_boxplot()
# 


