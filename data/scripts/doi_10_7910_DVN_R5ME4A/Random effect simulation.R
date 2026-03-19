########################################################
# Title: Testing effect of subsampling rate on 
# condition mixed-models
# Name: Julius G. Bright Ross
# Date: Feb 8, 2020
# Last updated: Apr 16, 2020
# Description: Taking subsampling rate produced by
# the lack of repeat captures of individuals in the 
# population and seeing what effect it has on 
# successful estimation of both fixed and random effects
########################################################

## Preparing workspace ##

rm(list = ls())

setwd("C:/Users/jbrig/Desktop/Badgers/Badger condition/Processed datasets")
load("Condition Modelling output.RData")

library(tidyverse)   #For many reasons
library(lme4)        #For lme4 model

randict <- list() #Extra dictionary for referencing

## Storing some numbers for reference in text ##

completeCondition <- rbind(springCompleteCondition, summerCompleteCondition, autumnCompleteCondition)

badgerLevels        <- table(completeCondition$Individual)
randict$singletons  <- length(which(badgerLevels == 1))
randict$perc.single <- round(100*randict$singletons/length(badgerLevels))

## Setting up the model framework ##

springBadgers <- unique(springCompleteCondition$Individual)
springNumber  <- length(springBadgers)

# Generating "data"
set.seed(9453278) #"WildCRU in numpad

N  <- springNumber*50
x1 <- runif(N)
x2 <- runif(N)
x3 <- runif(N)

variances <- VarCorr(springModelFinal$lme)[,1]

intraInd  <- sqrt(as.numeric(variances[which(names(variances) == "(Intercept)")][2]))
residual  <- sqrt(as.numeric(variances[which(names(variances) == "Residual")]))

RanEf      <- rep(1:springNumber, each = 50)              #ID if we had a complete dataset of 50 records/ind
intercepts <- rnorm(springNumber, mean = 0, 
                    sd = intraInd)                        #Individual intercept based on spring model

eta <- 1 + 0.2*x1 - 0.75*x2 + 0.51*x3 + intercepts[RanEf] #No noise
y   <- rnorm(N, mean = eta, sd = residual)                #Adding in noise equal to spring model

fRanEf <- factor(RanEf)

testModel <- lmer(y ~ x1 + x2 + x3 + (1|fRanEf))

MyData <- data.frame(y = y,
                     x1 = x1,
                     x2 = x2,
                     x3 = x3,
                     RanEf = RanEf)

unbal.index <- springCompleteCondition %>%
  mutate(Individual = factor(Individual)) %>%
  select(Individual) %>%
  table() %>%
  sort()


set.seed(9453278) #"WildCRU in numpad

value       <- c()
iteration   <- c()
coefficient <- c()

for (i in 1:1000) { #Build a model from 1000 subsets of the same subsampling type as the real spring data
  
  unbal.data <- NULL
  
  for (j in 1:springNumber) { #Here we subset to the same number of records as we actually have per individual
    unbal.rows   <- MyData[MyData$RanEf == j,]
    SelectedRows <- sample(1:50, unbal.index[j])
    unbal.data   <- rbind(unbal.data, unbal.rows[SelectedRows,])
  }
  
  unbal.data$fRanEf <- factor(unbal.data$RanEf)
  
  testModel2    <- lmer(y ~ x1 + x2 + x3 + (1|fRanEf), data = unbal.data)
  summaryHolder <- summary(testModel2)
  
  value       <- c(value, c(testModel2@beta,                       #These are the coefficients
                            sqrt(summaryHolder$varcor$fRanEf[1]))) #This is the SD of the random intercept
  iteration   <- c(iteration, rep(i, 5))
  coefficient <- c(coefficient, c("Intercept", "x1", "x2", "x3", "Individual (sd)"))
}

testFrame        <- data.frame(iteration, value, coefficient)
names(testFrame) <- c("Iteration", "Value", "Coefficient")

meanx1 <- mean(subset(testFrame, Coefficient == "x1")$Value)
meanx2 <- mean(subset(testFrame, Coefficient == "x2")$Value)

ggplot(testFrame, aes(x = Coefficient, y = Value)) + geom_boxplot()

## Storing deviation from individual SD ##

sdFrame  <- subset(testFrame, Coefficient == "Individual (sd)")
original <- intraInd
upperDev <- quantile(sdFrame$Value, 0.975)
lowerDev <- quantile(sdFrame$Value, 0.025)

randict$num.spring.badgers <- springNumber
randict$spring.sd          <- round(intraInd, 2)
randict$spring.residual    <- round(residual, 2)
randict$perc.below         <- round(100*nrow(subset(sdFrame, Value < intraInd))/nrow(sdFrame), 2)
randict$avg.difference     <- round(mean(abs(sdFrame$Value - intraInd)), 3)

randict$lower.rand.dev <- unname(round(lowerDev*100/original))
randict$upper.rand.dev <- unname(round(upperDev*100/original))

## Making a plot of the intercept estimates ##

randomEffectFig <- ggplot(sdFrame, aes(x = Value)) + 
  geom_histogram(binwidth = 0.003) + 
  ylab("Number of iterations") + xlab("Estimated individual intercept SD") +
  xlim(0, 0.55) + ylim(0, 85) +
  geom_line(aes(x = rep(randict$spring.sd, 1000), y = seq(0, 100, length.out = 1000)),
            colour = "#b21301", size = 1) + 
  theme_bw()

## Saving the output ##

save(randict, randomEffectFig, file = "Random effect simulation output.RData")
