#######################################################################
# Title: Computing various metrics for abundance estimation/indication
# Name: Julius G. Bright Ross
# Date created: January 5, 2021
# Last updated: October 26, 2021
# Description: Creating baseline population based on real badger data,
# then sampling and computing iterative estimates of abundance using
# enumeration methods and Cormack-Jolly-Seber models
#######################################################################

#Do not hesitate to reach out to the author for clarification: quote the line of the code that is providing confusion
#in an email to jbrightross@gmail.com

# NOTE: this analysis depends on matrices that have been parameterised with badger data. The creation of these matrices 
# has been done elsewhere and follows the in-text instructions; we strongly recommend creating your own for 
# any assessment of the suitability of eMNA to your data. For compatibility with this code, you'll want the following:
# - trueAlive, a matrix with rows for individuals and columns for years. 1s indicate an individual was alive in a given
#   year; 0s indicate it was not. These are true records, not samplings--that comes later.
# - trueAge, a matrix with the same structure as trueAlive, but where each position indicates the individual's age in
#   that year.
# - trueTeeth, a matrix with the same structure as trueAlive and trueAge, but where each position indicates the 
#   toothwear of the individual on a scale of 1 to 5. Obviously, this will not hold true for other species and datasets;
#   instead, we recommend replacing this metric with one that correlates well with age in the study species in question. 
#   The matrix can still be called trueTeeth, or simply run a query to replace all mentions of this matrix with your
#   bespoke metric matrix's name.
# - toothAgeMod, a model that contains the relationship between the age-predictor metric (in this case, tooth wear) 
#   and age. Again, the name must be changed for consistency in the code.
# - trueSex, a vector with the sex of each individual corresponding to the row in the life-history and context matrices.

## Preparing workspace and loading data ##

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: REPLACE DIRECTORIES WITH APPROPRIATE LOCAL FOLDERS

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Prepared badger data.RData")

## Setting out three trapping efficiency distributions. First, we'll try and approximate the shape of the wild population's trapping efficiency distribution.
## Second, we'll assume high trapping efficiency, a uniform distribution between 85-95%
## Third, we'll investigate low trapping efficiency, based on Parmenter et al. 2003's data on small mammal populations.
## We recommend tailoring the te.generator function to generate a range of capture efficiencies that may be experienced in the program of study.

# Let's look at the real population's trapping efficiencies: 

hist(trueCaptureEfficiency$Trapping_efficiency[3:(nrow(trueCaptureEfficiency)-4)], breaks = seq(50, 100, by = 2.5)) #Removing trapping efficiencies from marginal years

# Seems real trapping efficiency is largely between 67.5-85%, but occasionally falls lower:

hist(trueCaptureEfficiency$Trapping_efficiency[which(trueCaptureEfficiency$Trapping_efficiency < 85 & trueCaptureEfficiency$Trapping_efficiency > 70)]) #Pretty uniform in this range
hist(trueCaptureEfficiency$Trapping_efficiency[which(trueCaptureEfficiency$Trapping_efficiency < 67.5 & trueCaptureEfficiency$Trapping_efficiency > 0)],
     breaks = seq(50, 70, by = 2.5)) #Basically all between 57.5 and 65; one low at 50

# We'll sample real trapping efficiency (below) by breaking it into two processes based on the relative frequency of these two categories. 
# Based on the uniform distribution of the top category, we'll sample that as a uniform process, but the lower one is more 
# centered on something in the 62.5% range, so we'll sample it as a Gaussian process.

numAbove <- length(which(trueCaptureEfficiency$Trapping_efficiency[3:(nrow(trueCaptureEfficiency) - 4)] >= 70))
numBelow <- length(which(trueCaptureEfficiency$Trapping_efficiency[3:(nrow(trueCaptureEfficiency) - 4)] < 70))

avgBelow <- trueCaptureEfficiency[3:(nrow(trueCaptureEfficiency) - 4), ] %>%
  select(Trapping_efficiency) %>%
  subset(Trapping_efficiency < 70) %>%
  pull() %>% mean()
sdBelow  <- trueCaptureEfficiency[3:(nrow(trueCaptureEfficiency) - 4), ] %>%
  select(Trapping_efficiency) %>%
  subset(Trapping_efficiency < 70) %>%
  pull() %>% sd()

# Now, let's look at the distribution of small mammal trapping efficiencies:

parmenterTEs <- c(46/88, 27/58, 42/81, 39/65, 18/22, 20/33, 24/33, 15/26, 15/18)*100 #Taken from the paper cited in main manuscript

hist(parmenterTEs, breaks = seq(40, 100, by = 2.5)) #Excepting some quite high trapping efficiencies that largely represent small
                                                    #sample sizes, it seems most trapping efficiencies fall between 45 and 62.5%.
                                                    #We'll make it a little more drastic, and call it 45-55%

te.generator <- function(numYears, type) {
  
  # So, first, if we're dealing with real trapping efficiency--this will likely change for species other than badgers:
  
  if (type == "Real") {
    
    # Make a vector with that many years with either high- or low-trapping efficiency based on the ratio in the data
    
    hiLo <- sample(x       = c(rep("high", numAbove),
                               rep("low", numBelow)),
                   size    = numYears,
                   replace = TRUE)
    
    teSampled <- rep(NA, numYears)
    
    teSampled[hiLo == "high"] <- runif(min = 67.5,
                                       max = 85,
                                       n   = length(which(hiLo == "high")))
    
    # We'll make the assignment of low scores contingent on them existing, otherwise R is liable to throw a fit:
    
    if (length(which(hiLo == "low")) > 0) {
      teSampled[hiLo == "low"]  <- rnorm(mean = avgBelow,
                                         sd   = sdBelow,
                                         n    = length(which(hiLo == "low")))
    } #Close check on low scores
    
    # If by very weird odds we end up with numbers above 100% or under 0%, we'll bounce it back:
    
    while (length(which(teSampled > 100 | teSampled < 0)) > 0) {
      teSampled[teSampled > 100 | teSampled < 0] <- rnorm(mean = avgBelow,
                                                          sd   = sdBelow,
                                                          n    = length(which(hiLo == "low"))) #(it'll have been generated by the Gaussian)
    }
    
  } #Done generating trapping efficiency that matches the real distribution
  
  if (type == "High") {
    
    teSampled <- runif(min = 85, max = 95, n = numYears)
    
  } #Done generating high trapping efficiency numbers
  
  if (type == "Low") {
    
    teSampled <- runif(min = 45, max = 55, n = numYears)
    
  }
  
  return(teSampled)
  
} #Closing te.generator function

# Let's test the function on our real data and see how well it approximates the shape of our real trapping efficiencies:

set.seed(9453278) #WildCRU in numpad

teData <- data.frame(te = c(trueCaptureEfficiency$Trapping_efficiency[3:(nrow(trueCaptureEfficiency) - 4)],
                            te.generator(numYears = 200, type = "Real")),
                     realOrNot = c(rep("Real", nrow(trueCaptureEfficiency[3:(nrow(trueCaptureEfficiency) - 4),])),
                                   rep("Generated", 200)))

ggplot(teData, aes(x = te, fill = realOrNot)) + geom_histogram(binwidth = 2.5) #Looks good! Overall approximating the shape well

## Defining functions for efficient sampling and density estimation. We'll be running a single sampling to make sure it works ##

set.seed(9453278) #WildCRU in numpad

# First, a function to sample the "true" matrix. This will generate trapping efficiencies using the above function (for a given TE type), then 
# sample each year according to that year's trapping efficiency

sample.matrix <- function(trueAliveMatrix, teType, yearsSampled) {
  
  trueTE <- te.generator(numYears = yearsSampled, type = teType)/100 #Named "true" to differentiate from the estimated values we'll make that are blind to the true data
  
  sampled <- matrix(0,
                    nrow = nrow(trueAliveMatrix),
                    ncol = ncol(trueAliveMatrix))
  
  # Sample each year according to the TE generated above:
  
  for (j in 1:yearsSampled) {
    realColumn  <- trueAlive[,j]
    
    alivePositions <- which(realColumn == 1)
    numPositions   <- length(alivePositions)
    numToSample    <- round(numPositions*trueTE[j])
    
    sampledPositions <- sample(x       = alivePositions,
                               size    = numToSample,
                               replace = FALSE)
    
    sampled[sampledPositions, j] <- 1
  } #Closing year-by-year sampling
  
  return(sampled)
  
} #Closing sample.matrix function

set.seed(9453278) #WildCRU in numpad

sampledExample  <- sample.matrix(trueAliveMatrix = trueAlive, teType = "Real", yearsSampled = 30)
sampledExample2 <- sample.matrix(trueAliveMatrix = trueAlive, teType = "High", yearsSampled = 30)
sampledExample3 <- sample.matrix(trueAliveMatrix = trueAlive, teType = "Low", yearsSampled = 30)

set.seed(9453278) #WildCRU in numpad
sampledExample4 <- sample.matrix(trueAliveMatrix = trueAlive, teType = "Real", yearsSampled = 10) #Works with fewer data points

sampledExample5 <- sampledExample 
sampledExample5[,11:30] <- 0      #Good for testing just the first 10 years, to make sure lack of data doesn't break functions

sampledDensity <- data.frame(Initial.Estimate = c(apply(sampledExample, 2, sum),
                                                  apply(sampledExample2, 2, sum),
                                                  apply(sampledExample3, 2, sum)),
                             Trapping.Efficiency = c(rep("Real", ncol(sampledExample)),
                                                     rep("High", ncol(sampledExample)),
                                                     rep("Low", ncol(sampledExample))),
                             Year = rep(1:ncol(sampledExample), 3))

ggplot(sampledDensity, aes(x = Year, y = Initial.Estimate, color = Trapping.Efficiency)) +
  geom_line()

# Next, a function to tell us if an individual's first capture was cub or adult--crucial for age estimation. Based on our true alive matrix,
# we'll be able to tell if the first capture was a cub (as we would be able to in the wild):

cub.adult.vector <- function(sampledMatrix, trueAliveMatrix, trueAgeMatrix) {
  
  cubOrAdult <- c()
  
  # For each individual in the sampled matrix:
  
  for (i in 1:nrow(sampledMatrix)) {
    
    # Need to check if there's a capture for the individual at all. If there is, we check to see if it's its first capture and if that
    # age is 0 (second step important for badgers born before the beginning of the study):
    
    if (length(which(sampledMatrix[i,] == 1)) > 0) {
      
      firstRealPos    <- min(which(trueAliveMatrix[i,] == 1))
      firstSampledPos <- min(which(sampledMatrix[i,] == 1))
      
      ageAtFirstSample <- trueAgeMatrix[i, firstSampledPos]
      
      ifelse(firstRealPos == firstSampledPos & ageAtFirstSample == 0,
             cubOrAdult <- c(cubOrAdult, "Cub"),
             cubOrAdult <- c(cubOrAdult, "Adult"))
      
    } else {
      
      cubOrAdult <- c(cubOrAdult, "Untrapped")
      
    } #Done with the vector creation if/else statements
  } #Done running through each individual
  
  return(cubOrAdult)
  
} #Closing cub.adult.vector function

cubOrAdultExample <- cub.adult.vector(sampledMatrix   = sampledExample,
                                      trueAliveMatrix = trueAlive,
                                      trueAgeMatrix   = trueAge)

cubOrAdultExample2 <- cub.adult.vector(sampledMatrix   = sampledExample5,
                                       trueAliveMatrix = trueAlive,
                                       trueAgeMatrix   = trueAge)

# Now that we have a sampled grid of badgers, we can fill in the gaps when badgers must have been alive:

fill.gaps <- function(sampledMatrix, cubOrAdultVector) {
  
  # For each individual in the sampled matrix
  
  for (i in 1:nrow(sampledMatrix)) {
    
    individualRow <- sampledMatrix[i,]
    
    if (cubOrAdultVector[i] != "Untrapped") {
      
      firstPosition <- min(which(individualRow == 1))
      lastPosition  <- max(which(individualRow == 1))
      
      # So long as it wasn't a 1-capture individual, we fill in everything between as 1s:
      
      if (firstPosition != lastPosition) {
        
        individualRow[firstPosition:lastPosition] <- 1
        
        sampledMatrix[i,] <- individualRow
        
      } #Done filling gaps
      
    } #Done checking if this individual was sampled at all
    
  } #Done filling gaps for each individual 
  
  return(sampledMatrix)
  
}

filledSampledExample <- fill.gaps(sampledMatrix    = sampledExample,
                                  cubOrAdultVector = cubOrAdultExample)

filledSampledExample2 <- fill.gaps(sampledMatrix    = sampledExample5,
                                   cubOrAdultVector = cubOrAdultExample2) #Looks good until the last couple of years where things should deteriorate

# Now, a function to back-age individuals first caught as adults based on their toothwear in first capture:

back.age <- function(filledSampledMatrix, cubOrAdultVector, trueToothMatrix) {
  
  adultFirstCaptures <- which(cubOrAdultVector == "Adult")
  
  # Run through all the individuals whose first capture was as an adult:
  
  for (i in adultFirstCaptures) {
    
    firstPosition  <- min(which(filledSampledMatrix[i,] == 1))
    
    firstToothwear <- trueToothMatrix[i, firstPosition]
    
    predictedAge <- predict(toothAgeMod, newdata = data.frame(Toothwear = firstToothwear))
    predictedAge <- round(predictedAge)
    
    # Now, if the predicted age at first capture is 0 (as it would be for toothwear = 1), we assume it's a yearling (we know not a cub):
    
    if (predictedAge == 0) {
      predictedAge <- 1
    } #Done fixing cub model outputs
    
    # Now, we run back to either the time when the badger would have been born or the first year of the study, and insert 1s:
    
    minEnd <- max(1, firstPosition - predictedAge)
    
    filledSampledMatrix[i, minEnd:firstPosition] <- 1
    
  } #Done running through each adult-first-capture assigning toothwear
  
  return(filledSampledMatrix)
  
} #Closing back.age function

agedFilledSampledExample <- back.age(filledSampledMatrix = filledSampledExample, 
                                     cubOrAdultVector    = cubOrAdultExample, 
                                     trueToothMatrix     = trueTeeth)

agedFilledSampledExample2 <- back.age(filledSampledMatrix = filledSampledExample2, 
                                      cubOrAdultVector    = cubOrAdultExample2, 
                                      trueToothMatrix     = trueTeeth)              #Good, still robust

# As a similar, side process, we'll make a function that creates an age matrix for our captures. For badgers first caught
# as cubs, we just start at 0 and count up. For badgers first caught as adults, we again figure out what age they were caught
# at, based on toothwear at first capture, and project back:

age.matrix <- function(filledSampledMatrix, cubOrAdultVector, trueToothMatrix) {
  
  ageSampled <- matrix(0, 
                       nrow = nrow(filledSampledMatrix),
                       ncol = ncol(filledSampledMatrix))
  
  # First, let's deal with ones first caught as cubs:
  
  cubPositions <- which(cubOrAdultVector == "Cub")
  
  for (i in cubPositions) {
    
    firstPosition <- min(which(filledSampledMatrix[i,] == 1))
    lastPosition  <- ncol(filledSampledMatrix)
    
    ageSampled[i, firstPosition:lastPosition] <- seq(0, (lastPosition - firstPosition))
    
  } #Done dealing with all badgers first caught as cubs
  
  # Now, ones first caught as adults:
  
  adultPositions <- which(cubOrAdultVector == "Adult")
  
  for (i in adultPositions) {
    
    firstPosition <- min(which(filledSampledMatrix[i,] == 1))
    
    firstToothwear <- trueToothMatrix[i, firstPosition]
    
    predictedAge <- predict(toothAgeMod, newdata = data.frame(Toothwear = firstToothwear))
    predictedAge <- round(predictedAge)
    
    # Now, if the predicted age at first capture is 0 (as it would be for toothwear = 1), we assume it's a yearling (we know not a cub):
    
    if (predictedAge == 0) {
      
      predictedAge <- 1
      
    } #Done fixing cub model outputs
    
    # Now, we find the first point when an individual would have been alive, and make a sequence from that to the end of the study:
    
    positionBorn <- firstPosition - predictedAge
    endOfStudy   <- ncol(filledSampledMatrix)
    
    ageSequence <- seq(0, endOfStudy - positionBorn)
    
    # If the individual was born before the beginning of the study, we grab the last 30 of this sequence; otherwise we 
    # grab the whole thing and stick it in where it belongs:
    
    if (length(ageSequence) > 30) {
      
      ageSequence <- ageSequence[(length(ageSequence) - 29):length(ageSequence)] #29 because we want it to go to position 1, not 0
      
      ageSampled[i,] <- ageSequence
      
    } else {
      
      ageSampled[i, positionBorn:endOfStudy] <- ageSequence
      
    } #Done dealing with whether or not badger was born before/after the study began
    
  } #Done dealing with badgers first caught as adults
  
  # Note, although badgers in this imaginary population can't be over age 10, this will be fixed later--this is simply a
  # record of how old they'd be if they were alive in year x
  
  return(ageSampled)
  
} #Closing age.matrix function

ageExample <- age.matrix(filledSampledMatrix = filledSampledExample,
                         cubOrAdultVector    = cubOrAdultExample,
                         trueToothMatrix     = trueTeeth)

ageExample2 <- age.matrix(filledSampledMatrix = filledSampledExample2,
                          cubOrAdultVector    = cubOrAdultExample2,
                          trueToothMatrix     = trueTeeth)              #Still robust

# Sanity check: Make sure the ages of individuals first caught as cubs are the same in this and the original:

caughtAsCubs <- which(cubOrAdultExample == "Cub")

for (i in caughtAsCubs) {
  
  sampledAges  <- ageExample[i,]
  originalAges <- trueAge[i,]
  
  sampledAges[which(sampledAges > max(originalAges))] <- 0 #Because sampled assumes it could have survived
  
  if (sum(originalAges == sampledAges) != 30) {
    print(paste("Error: wrong age assigned for row", i))
  } #Done checking for mismatches 
  
} #Done checking all cub rows

# Now, a function that will estimate trapping efficiencies from the badgers we know to have been alive but that weren't sampled.
# Note, when we back-age individuals using toothwear, we can't be 100% sure that those back-filled captures are accurate,
# so we'll only estimate trapping efficiency using the "filled" sample--in other words, observations we're certain about.

estimate.te <- function(sampledMatrix, filledSampledMatrix) {
  
  sampledSums <- apply(sampledMatrix, 2, sum)
  filledSums  <- apply(filledSampledMatrix, 2, sum)
  
  estimatedTEs <- sampledSums/filledSums
  
  # For the first and last slots, we'll take the average trapping efficiency and use that:
  
  estimatedTEs[c(1, length(estimatedTEs))] <- mean(estimatedTEs[-c(1, length(estimatedTEs))])
  
  return(estimatedTEs)
  
} #Closing estimate.te function

# And now an important one: adding in estimations of likelihood of continuing alive in the population based on trapping efficiency:

modify.by.efficiency <- function(sampledMatrix, filledSampledMatrix, agedFilledSampledMatrix, cubOrAdultVector, ageSampledMatrix) {
  
  # First, we call the trapping efficiency estimation function, using the matrix with no back-aging:
  
  estimatedTEs <- estimate.te(sampledMatrix       = sampledMatrix,
                              filledSampledMatrix = filledSampledMatrix)
  
  # Next, for rows with records for badgers, we go through each successive record and create a likelihood of remaining alive
  
  caughtAtSomePoint <- which(cubOrAdultVector != "Untrapped")
  
  for (i in caughtAtSomePoint) {
    
    lifeHistory <- agedFilledSampledMatrix[i,]
    ageHistory  <- ageSampledMatrix[i,]
    lastAlive   <- max(which(lifeHistory > 0))
    
    # If they were caught at end of study, nothing to do. Otherwise:
    
    if (lastAlive != length(lifeHistory)) {
      
      # For every spot from the first untrapped point to the end of the study
      
      for (j in (lastAlive + 1):length(lifeHistory)) {
        
        lifeHistory[j] <- lifeHistory[j - 1]*(1 - estimatedTEs[j])
        
      }
      
      # We check the age matrix, to cut off age at 10--we don't allow our badgers to live past 10 in this imaginary population
      
      if (max(ageHistory) > 10) {
        
        impossibleAges <- which(ageHistory > 10)
        
        lifeHistory[impossibleAges] <- 0
        
      } #Done cutting out possibility of survival past age 10
      
      # Now we put these into the back-aged, gap-filled matrix:
      
      agedFilledSampledMatrix[i,] <- lifeHistory
      
    } #Done running through those not caught at end of study
    
  } #Done running through each badger
  
  return(agedFilledSampledMatrix)
  
} #Closing modify.by.efficiency function

eModifiedAgedFilledSampledExample <- modify.by.efficiency(sampledMatrix           = sampledExample,
                                                          filledSampledMatrix     = filledSampledExample,
                                                          agedFilledSampledMatrix = agedFilledSampledExample,
                                                          cubOrAdultVector        = cubOrAdultExample,
                                                          ageSampledMatrix        = ageExample)

eModifiedAgedFilledSampledExample2 <- modify.by.efficiency(sampledMatrix           = sampledExample5,
                                                           filledSampledMatrix     = filledSampledExample2,
                                                           agedFilledSampledMatrix = agedFilledSampledExample2,
                                                           cubOrAdultVector        = cubOrAdultExample2,
                                                           ageSampledMatrix        = ageExample2)                 #Robust to this point


stepwiseIncreases <- data.frame(Density = c(apply(trueAlive, 2, sum),
                                            apply(sampledExample, 2, sum),
                                            apply(filledSampledExample, 2, sum),
                                            apply(agedFilledSampledExample, 2, sum),
                                            apply(eModifiedAgedFilledSampledExample, 2, sum)),
                                Step    = c(rep("True", ncol(trueAlive)),
                                            rep("Initial sampling", ncol(trueAlive)),
                                            rep("Gaps filled", ncol(trueAlive)),
                                            rep("Back-aged", ncol(trueAlive)),
                                            rep("Efficiency-modified", ncol(trueAlive))),
                                Year    = rep(c(1:30), 5)) %>%
  mutate(Step = factor(Step, levels = c("True", "Efficiency-modified", "Back-aged", "Gaps filled", "Initial sampling")))

ggplot(stepwiseIncreases, aes(x = Year, y = Density, colour = Step)) + geom_line()

# Alternatively, we'll also estimate density using the package "marked", using a CJS model:

estimate.cjs <- function(sampledMatrix, ageSampledMatrix, trueSexVector, yearsSampled) {
  
  # First, we want to cap our ages at 10, to improve computation efficiency (we know survival = 0 beyond 10) and make 
  # our age matrix into a dataframe, with specific column names to interface with the marked package
  
  ageCapped <- ageSampledMatrix[,1:yearsSampled] #Here and below, we'll trim out years not actually "surveyed" because we don't want false 0s (they should be)
  ageCapped[which(ageCapped > 10)] <- ">10"
  
  # Next, we make a little filter to make sure we're only taking rows with some observations
  
  sampledTrimmed <- sampledMatrix[,1:yearsSampled]
  detected <- apply(sampledTrimmed, 1, sum) > 0
  
  # Next, we trim out the age at the last point because it provides no information: no survival past end of study
  
  ageCapped <- data.frame(ageCapped[detected, 1:(yearsSampled - 1)]) 
  names(ageCapped) <- paste("AGE", seq(1, (yearsSampled - 1)), sep = "")
  
  # And we make a little character string out of observation records
  
  ch <- apply(sampledTrimmed[detected,], 1, paste, collapse = "")
  
  # Now, we combine these into our "data":
  
  cjsData <- data.frame(ch  = ch,                              
                        sex = trueSexVector[detected]) %>%
    mutate(ch  = as.character(ch),
           sex = as.character(sex)) %>%
    bind_cols(ageCapped) 
  
  # Processing data for crm function:
  
  cjs.proc <- process.data(cjsData, model = "cjs")
  
  design.Phi <- list(static = c("sex"), time.varying = c("AGE"))
  design.p   <- list()
  
  design.parameters <- list(Phi = design.Phi, p = design.p)
  
  cjs.ddl <- make.design.data(cjs.proc, parameters = design.parameters)
  
  Phi.sa <- list(formula = ~sex*AGE)
  p.t    <- list(formula = ~time)
  
  # The model!
  
  cjsModel <- crm(cjs.proc, cjs.ddl, hessian = FALSE, model.parameters = list(Phi = Phi.sa, p = p.t))
  
  # Extracting p for each year, the fraction of existing individuals detected:
  
  ptable <- cjsModel$results$reals$p %>% arrange(by = time)
  
  # Calculating the number of individuals sampled, then dividing by the number of individuals (excepting first position, which isn't estimated)
  
  sampledN   <- apply(sampledTrimmed, 2, sum)
  estimatedN <- sampledN[-1]/ptable$estimate
  
  # Putting it into a little dataframe, then returning that
  
  cjsFrame <- data.frame(estimatedN = estimatedN,
                         year       = seq(2, yearsSampled))
  
  return(cjsFrame)
  
} #Done making a CJS estimate of density

cjsExample <- estimate.cjs(sampledMatrix    = sampledExample,
                           ageSampledMatrix = ageExample,
                           trueSexVector    = trueSex,
                           yearsSampled     = 30)

cjsExample2 <- estimate.cjs(sampledMatrix    = sampledExample5,
                            ageSampledMatrix = ageExample2,
                            trueSexVector    = trueSex,
                            yearsSampled     = 10)           #Slightly different from one another, probably because the Phi.sa relationship is different based on the amount of data

# Now, we're going to make a pair of functions for estimating survival relationships from our sampled data.
# To do so, one will create a dataframe from individuals with known ages in the population (ones first caught as cubs) and the
# next will model their survival as a function of their age, sex, and population density (all individuals). We'll do this in two ways: 
# 1) By simply modelling 1/0 survived/did not
# 2) By doing the same, but weighting observations by the trapping efficiency in the year they died--in other words, assuming that
# if we're more uncertain about an individual's death at a certain age, we should consider that observation less certain than otherwise

survival.frame.creation <- function(eModifiedAgedFilledSampledMatrix, ageSampledMatrix, cubOrAdultVector, trueSexVector) {
  
  # Setting up holders for the information
  
  standardIDHolder       <- c()
  standardSurvivalHolder <- c()
  standardAgeHolder      <- c()
  standardYearHolder     <- c()
  
  alternativeIDHolder       <- c()
  alternativeSurvivalHolder <- c()
  alternativeWeightHolder   <- c()
  alternativeAgeHolder      <- c()
  alternativeYearHolder     <- c()
  
  knownAgePositions <- which(cubOrAdultVector == "Cub")
  
  for (i in knownAgePositions) {
    
    # Note, both of these are blind to the real data--they're sampled, but for individuals of known age
    
    ageHistory  <- ageSampledMatrix[i,]
    lifeHistory <- eModifiedAgedFilledSampledMatrix[i,]
    
    # It doesn't make sense to do this for individuals born in the last year of the study:
    
    positionBorn <- min(which(lifeHistory == 1))
    
    if (positionBorn != ncol(ageSampledMatrix)) {
      
      lastPositionDetected <- max(which(lifeHistory == 1))
      
      # We now have the issue of individuals still alive at the end of the study. 
      # For those, we can get 1s for years we know they
      # survived, but we need to omit any 0s for the end of their life--we can't know if they died at that point or not.
      
      if (lastPositionDetected == length(lifeHistory)) {
        
        # So, first we make our standard survival record:
        
        standardSurvival <- c(rep(1, lastPositionDetected - positionBorn))      #Makes 1s for all years alive, no 0
        standardAge      <- ageHistory[positionBorn:(lastPositionDetected - 1)] #Gets all ages known to live past
        standardYear     <- c(positionBorn:(lastPositionDetected - 1))
        
        # Our weighted survival record for these ones is identical to the standard (just alive, weight of 1)
        
        alternativeSurvival <- standardSurvival
        alternativeWeight   <- rep(1, length(alternativeSurvival))
        alternativeAge      <- standardAge
        alternativeYear     <- standardYear
        
        # alternativeSurvival <- c()
        # alternativeWeight   <- c()
        # alternativeAge      <- c()
        # alternativeYear     <- c()
        
      } else {
        
        # Standard survival record:
        
        standardSurvival <- c(rep(1, lastPositionDetected - positionBorn), 0) #1s for all years alive, one 0 at the end
        standardAge      <- ageHistory[positionBorn:lastPositionDetected]     #Now just all ages
        standardYear     <- c(positionBorn:lastPositionDetected)
        
        # And weighted survival record now, where we're essentially doing the same thing, but inserting weights based on TE of "death" point:
        
        # alternativeSurvived       <- rep(1, lastPositionDetected - positionBorn)                  #Years known to live past
        # alternativeMaybeSurvivedW <- lifeHistory[(lastPositionDetected + 1):length(lifeHistory)]  #Years after last known survival
        # alternativeMaybeDiedW     <- 1 - alternativeMaybeSurvivedW                                #1-prob(alive) for those
        
        alternativeSurvived       <- rep(1, lastPositionDetected - positionBorn) #Years known to live past
        alternativeMaybeSurvivedW <- lifeHistory[(lastPositionDetected + 1)]     #Year after last known survival
        alternativeMaybeDiedW     <- 1 - alternativeMaybeSurvivedW               #1-prob(alive) for that one
        
        # This part is necessary because when cubs die, they have no "survived" years:
        
        if (length(alternativeSurvived) > 0) {
          ageSurvived <- ageHistory[positionBorn:(lastPositionDetected - 1)]  #Age for known life points
          yrSurvived  <- c(positionBorn:(lastPositionDetected - 1))
        } else {
          ageSurvived <- c()
          yrSurvived  <- c()
        }
        
        ageMaybeSurvivedOrNot     <- ageHistory[(lastPositionDetected)] #Age for year after last known survival
        yrMaybeSurvivedOrNot      <- c(lastPositionDetected)            #The year for that age
        
        # alternativeSurvival <- c(alternativeSurvived,
        #                          rep(1, length(alternativeMaybeSurvivedW)),
        #                          rep(0, length(alternativeMaybeDiedW)))
        # alternativeWeight   <- c(alternativeSurvived,
        #                          alternativeMaybeSurvivedW,
        #                          alternativeMaybeDiedW)
        # alternativeAge      <- c(ageSurvived,
        #                          ageMaybeSurvivedOrNot,
        #                          ageMaybeSurvivedOrNot)
        # alternativeYear     <- c(yrSurvived,
        #                          yrMaybeSurvivedOrNot,
        #                          yrMaybeSurvivedOrNot)
        
        alternativeSurvival <- c(alternativeSurvived, 0)
        alternativeWeight   <- rep(alternativeMaybeDiedW, length(alternativeSurvived) + 1)
        alternativeAge      <- c(ageSurvived, ageMaybeSurvivedOrNot)
        alternativeYear     <- c(yrSurvived, yrMaybeSurvivedOrNot)
        
      } #Done dealing with separate processes for individuals caught in the last year, first year of the study
      
    } #Done excluding individuals born in last year of study
    
    # Adding each field into our holders:
    
    standardIDHolder       <- c(standardIDHolder, rep(i, length(standardSurvival)))
    standardSurvivalHolder <- c(standardSurvivalHolder, standardSurvival)
    standardAgeHolder      <- c(standardAgeHolder, standardAge)
    standardYearHolder     <- c(standardYearHolder, standardYear)
    
    alternativeIDHolder       <- c(alternativeIDHolder, rep(i, length(alternativeSurvival)))
    alternativeSurvivalHolder <- c(alternativeSurvivalHolder, alternativeSurvival)
    alternativeWeightHolder   <- c(alternativeWeightHolder, alternativeWeight)
    alternativeAgeHolder      <- c(alternativeAgeHolder, alternativeAge)
    alternativeYearHolder     <- c(alternativeYearHolder, alternativeYear)
    
  } #Done running through each individual
  
  # Okay, now we can put them together into one dataframe
  
  survivalFrame <- data.frame(ID       = c(standardIDHolder, 
                                           alternativeIDHolder),
                              Type     = c(rep("Standard", length(standardIDHolder)), 
                                           rep("Alternative", length(alternativeIDHolder))),
                              Age      = c(standardAgeHolder,
                                           alternativeAgeHolder),
                              Survived = c(standardSurvivalHolder,
                                           alternativeSurvivalHolder),
                              Weight   = c(rep(NA, length(standardIDHolder)),
                                           alternativeWeightHolder),
                              Year     = c(standardYearHolder,
                                           alternativeYearHolder)
  )
  
  # And now we add in sex and density, and subset to under age 10 (impossible to estimate because we kill them off):
  
  densityVector <- apply(eModifiedAgedFilledSampledMatrix, 2, sum)
  
  survivalFrame <- survivalFrame %>%
    mutate(Survived = factor(Survived),
           Sex      = factor(trueSexVector[survivalFrame$ID]),
           Density  = densityVector[survivalFrame$Year]) %>%
    subset(Age < 10)
  
  return(survivalFrame)
  
} #Closing survival.frame.creation function

survivalFrameExample <- survival.frame.creation(eModifiedAgedFilledSampledMatrix = eModifiedAgedFilledSampledExample,
                                                ageSampledMatrix                 = ageExample,
                                                cubOrAdultVector                 = cubOrAdultExample,
                                                trueSexVector                    = trueSex) 

survival.relationships <- function(survivalDataset, density) {
  
  # First, discount observations from the first two and last three years, as these are less trustworthy sampling years:
  
  survivalDataset <- survivalDataset %>% subset(2 < Year & Year < (max(Year) - 3)) 
  
  # Second, split them apart:
  
  standardFrame <- survivalDataset %>% subset(Type == "Standard")
  
  alternativeFrame <- survivalDataset %>% subset(Type == "Alternative")
  
  # Third, model:
  
  standardModel <- gam(Survived ~ Sex + s(Age, by = Sex, k = 4) + Density, 
                       family = binomial,
                       data   = standardFrame)
  
  alternativeModel <- gam(Survived ~ Sex + s(Age, by = Sex, k = 4) + Density, 
                          family  = quasibinomial, 
                          data    = alternativeFrame,
                          weights = Weight)
  
  # Fourth, come up with predictions off these:
  
  newdataFrame <- data.frame(Age     = rep(seq(0, 9, length.out = 10), 2),
                             Sex     = c(rep("Male", 10), rep("Female", 10)),
                             Density = rep(density, 20))
  
  predictionFrame <- rbind(newdataFrame, newdataFrame) %>%
    mutate(Prediction = c(predict(standardModel, type = "response", newdata = newdataFrame),
                          predict(alternativeModel, type = "response", newdata = newdataFrame)),
           Type       = c(rep("Standard", nrow(newdataFrame)),
                          rep("Alternative", nrow(newdataFrame))))
  
  return(predictionFrame)
  
} #Closing survival.relationships function

predictionExample <- survival.relationships(survivalDataset = survivalFrameExample, density = 225)

## Saving image so that we can run code below without going through all the iterations or manually running
## function creation steps. 

#NOTE: Change directory to user's directory

save.image(file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Functions image.RData")

## To come up with a comparative evaluation of the methods, we will iteratively sample the population over 30 years,
## the approximate duration of the field project these data are based on, and estimate population abundance using 
## all these functions. We will do this 3 times: one for each trapping efficiency level. As these iterations take
## quite a while, I designed them to be run over three nights, which is why the code is "chunked" the way it is

densityHolder    <- c()
yearHolder       <- c()
typeHolder       <- c()
iterationHolder  <- c()
teTypeHolder     <- c()

cjsIterHolder    <- c()
cjsTEHolder      <- c()
cjsYearHolder    <- c()
cjsDensityHolder <- c()

survPredHolder   <- c()
survSexHolder    <- c()
survAgeHolder    <- c()
survTypeHolder   <- c()
survIterHolder   <- c()
survTEHolder     <- c()

numIterations <- 1000

set.seed(9453278) #WildCRU (the author's affiliation) in numpad

for (iteration in 1:333) { #We'll do this in three chunks, to run over nights--we'll save it separately each time, then combine
  
  for (teType in c("Real", "High", "Low")) {
    
    # First, we sample:
    
    iterationSampled <- sample.matrix(trueAliveMatrix = trueAlive, 
                                      teType          = teType, 
                                      yearsSampled    = 30)
    
    # Now, we get a vector of cub-or-adult measurements:
    
    iterationCubAdultVector <- cub.adult.vector(sampledMatrix   = iterationSampled,
                                                trueAliveMatrix = trueAlive,
                                                trueAgeMatrix   = trueAge)
    
    # Next, we fill in our gaps:
    
    iterationFilledSampled <- fill.gaps(sampledMatrix    = iterationSampled,
                                        cubOrAdultVector = iterationCubAdultVector)
    
    # We make a matrix of individual ages based on our gap-filled matrix: 
    
    iterationAgeSampled <- age.matrix(filledSampledMatrix = iterationFilledSampled,
                                      cubOrAdultVector    = iterationCubAdultVector,
                                      trueToothMatrix     = trueTeeth)
    
    # Next, we back-age individuals first caught as adults:
    
    iterationAgedFilledSampled <- back.age(filledSampledMatrix = iterationFilledSampled,
                                           cubOrAdultVector    = iterationCubAdultVector,
                                           trueToothMatrix     = trueTeeth)
    
    # Finally, we modify by trapping efficiency, which is calculated in this function using the filled but not aged data:
    
    iterationModifiedAgedFilledSampled <- modify.by.efficiency(sampledMatrix           = iterationSampled,
                                                               filledSampledMatrix     = iterationFilledSampled,
                                                               agedFilledSampledMatrix = iterationAgedFilledSampled,
                                                               cubOrAdultVector        = iterationCubAdultVector, 
                                                               ageSampledMatrix        = iterationAgeSampled)
    
    # Now, we store each step's density and information for plotting:
    
    densityHolder   <- c(densityHolder, 
                         apply(iterationSampled, 2, sum),
                         apply(iterationFilledSampled, 2, sum),
                         apply(iterationAgedFilledSampled, 2, sum),
                         apply(iterationModifiedAgedFilledSampled, 2, sum))
    yearHolder      <- c(yearHolder, 
                         rep(c(1:30), 4))
    typeHolder      <- c(typeHolder,
                         c(rep("Initial sampling", 30),
                           rep("Gaps filled", 30),
                           rep("Back-aged", 30),
                           rep("Efficiency-modified", 30)))
    iterationHolder <- c(iterationHolder, 
                         rep(iteration, 30*4))
    teTypeHolder    <- c(teTypeHolder,
                         rep(teType, 30*4))
    
    # And our CJS model predictions from marked:
    
    iterationCJSEstimates <- estimate.cjs(sampledMatrix    = iterationSampled, 
                                          ageSampledMatrix = iterationAgeSampled,
                                          trueSexVector    = trueSex, 
                                          yearsSampled     = 30)
    
    cjsIterHolder    <- c(cjsIterHolder, rep(iteration, nrow(iterationCJSEstimates)))
    cjsTEHolder      <- c(cjsTEHolder, rep(teType, nrow(iterationCJSEstimates)))
    cjsYearHolder    <- c(cjsYearHolder, iterationCJSEstimates$year)
    cjsDensityHolder <- c(cjsDensityHolder, iterationCJSEstimates$estimatedN)
    
    # We can now make our survival dataframe:
    
    iterationSurvivalFrame <- survival.frame.creation(eModifiedAgedFilledSampledMatrix = iterationModifiedAgedFilledSampled,
                                                      ageSampledMatrix                 = iterationAgeSampled,
                                                      cubOrAdultVector                 = iterationCubAdultVector,
                                                      trueSexVector                    = trueSex)
    
    # And make predictions off it:
    
    iterationSurvivalPredictions <- survival.relationships(survivalDataset = iterationSurvivalFrame, 
                                                           density         = 225)
    
    survPredHolder  <- c(survPredHolder, iterationSurvivalPredictions$Prediction)
    survSexHolder   <- c(survSexHolder, iterationSurvivalPredictions$Sex)
    survAgeHolder   <- c(survAgeHolder, iterationSurvivalPredictions$Age)
    survTypeHolder  <- c(survTypeHolder, iterationSurvivalPredictions$Type)
    survIterHolder  <- c(survIterHolder, rep(iteration, nrow(iterationSurvivalPredictions)))
    survTEHolder    <- c(survTEHolder, rep(teType, nrow(iterationSurvivalPredictions)))
    
  } #Done doing the whole process for each level of trapping efficiency
  
  
  if (iteration/25 == floor(iteration/25)) {
    print(paste(round(iteration*100/numIterations), "% done."))
  }
  
} #Done with all iterations of sampling and estimating density/survival relationships

#NOTE: Change directory

save.image(file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_333.RData")

## Second night run:

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_333.RData")

set.seed(9453278) #WildCRU in numpad

for (iteration in 334:667) { #Second chunk of iterations
  
  for (teType in c("Real", "High", "Low")) {
    
    # First, we sample:
    
    iterationSampled <- sample.matrix(trueAliveMatrix = trueAlive, 
                                      teType          = teType,
                                      yearsSampled    = 30)
    
    # Now, we get a vector of cub-or-adult measurements:
    
    iterationCubAdultVector <- cub.adult.vector(sampledMatrix   = iterationSampled,
                                                trueAliveMatrix = trueAlive,
                                                trueAgeMatrix   = trueAge)
    
    # Next, we fill in our gaps:
    
    iterationFilledSampled <- fill.gaps(sampledMatrix    = iterationSampled,
                                        cubOrAdultVector = iterationCubAdultVector)
    
    # We make a matrix of individual ages based on our gap-filled matrix: 
    
    iterationAgeSampled <- age.matrix(filledSampledMatrix = iterationFilledSampled,
                                      cubOrAdultVector    = iterationCubAdultVector,
                                      trueToothMatrix     = trueTeeth)
    
    # Next, we back-age individuals first caught as adults:
    
    iterationAgedFilledSampled <- back.age(filledSampledMatrix = iterationFilledSampled,
                                           cubOrAdultVector    = iterationCubAdultVector,
                                           trueToothMatrix     = trueTeeth)
    
    # Finally, we modify by trapping efficiency, which is calculated in this function using the filled but not aged data:
    
    iterationModifiedAgedFilledSampled <- modify.by.efficiency(sampledMatrix           = iterationSampled,
                                                               filledSampledMatrix     = iterationFilledSampled,
                                                               agedFilledSampledMatrix = iterationAgedFilledSampled,
                                                               cubOrAdultVector        = iterationCubAdultVector, 
                                                               ageSampledMatrix        = iterationAgeSampled)
    
    # Now, we store each step's density and information for plotting:
    
    densityHolder   <- c(densityHolder, 
                         apply(iterationSampled, 2, sum),
                         apply(iterationFilledSampled, 2, sum),
                         apply(iterationAgedFilledSampled, 2, sum),
                         apply(iterationModifiedAgedFilledSampled, 2, sum))
    yearHolder      <- c(yearHolder, 
                         rep(c(1:30), 4))
    typeHolder      <- c(typeHolder,
                         c(rep("Initial sampling", 30),
                           rep("Gaps filled", 30),
                           rep("Back-aged", 30),
                           rep("Efficiency-modified", 30)))
    iterationHolder <- c(iterationHolder, 
                         rep(iteration, 30*4))
    teTypeHolder    <- c(teTypeHolder,
                         rep(teType, 30*4))
    
    # And our CJS model predictions from marked:
    
    iterationCJSEstimates <- estimate.cjs(sampledMatrix    = iterationSampled, 
                                          ageSampledMatrix = iterationAgeSampled,
                                          trueSexVector    = trueSex,  
                                          yearsSampled     = 30)
    
    cjsIterHolder    <- c(cjsIterHolder, rep(iteration, nrow(iterationCJSEstimates)))
    cjsTEHolder      <- c(cjsTEHolder, rep(teType, nrow(iterationCJSEstimates)))
    cjsYearHolder    <- c(cjsYearHolder, iterationCJSEstimates$year)
    cjsDensityHolder <- c(cjsDensityHolder, iterationCJSEstimates$estimatedN)
    
    # We can now make our survival dataframe:
    
    iterationSurvivalFrame <- survival.frame.creation(eModifiedAgedFilledSampledMatrix = iterationModifiedAgedFilledSampled,
                                                      ageSampledMatrix                 = iterationAgeSampled,
                                                      cubOrAdultVector                 = iterationCubAdultVector,
                                                      trueSexVector                    = trueSex)
    
    # And make predictions off it:
    
    iterationSurvivalPredictions <- survival.relationships(survivalDataset = iterationSurvivalFrame, 
                                                           density         = 225)
    
    survPredHolder  <- c(survPredHolder, iterationSurvivalPredictions$Prediction)
    survSexHolder   <- c(survSexHolder, iterationSurvivalPredictions$Sex)
    survAgeHolder   <- c(survAgeHolder, iterationSurvivalPredictions$Age)
    survTypeHolder  <- c(survTypeHolder, iterationSurvivalPredictions$Type)
    survIterHolder  <- c(survIterHolder, rep(iteration, nrow(iterationSurvivalPredictions)))
    survTEHolder    <- c(survTEHolder, rep(teType, nrow(iterationSurvivalPredictions)))
    
  } #Done doing the whole process for each level of trapping efficiency
  
  
  if (iteration/25 == floor(iteration/25)) {
    print(paste(round(iteration*100/numIterations), "% done."))
  }
  
} #Done with all iterations of sampling and estimating density/survival relationships

#NOTE: Change directory

save.image(file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_667.RData")

## Final night run:

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_667.RData")

set.seed(9453278) #WildCRU in numpad

for (iteration in 668:1000) { #Final chunk, on the final night
  
  for (teType in c("Real", "High", "Low")) {
    
    # First, we sample:
    
    iterationSampled <- sample.matrix(trueAliveMatrix = trueAlive, 
                                      teType          = teType,
                                      yearsSampled    = 30)
    
    # Now, we get a vector of cub-or-adult measurements:
    
    iterationCubAdultVector <- cub.adult.vector(sampledMatrix   = iterationSampled,
                                                trueAliveMatrix = trueAlive,
                                                trueAgeMatrix   = trueAge)
    
    # Next, we fill in our gaps:
    
    iterationFilledSampled <- fill.gaps(sampledMatrix    = iterationSampled,
                                        cubOrAdultVector = iterationCubAdultVector)
    
    # We make a matrix of individual ages based on our gap-filled matrix: 
    
    iterationAgeSampled <- age.matrix(filledSampledMatrix = iterationFilledSampled,
                                      cubOrAdultVector    = iterationCubAdultVector,
                                      trueToothMatrix     = trueTeeth)
    
    # Next, we back-age individuals first caught as adults:
    
    iterationAgedFilledSampled <- back.age(filledSampledMatrix = iterationFilledSampled,
                                           cubOrAdultVector    = iterationCubAdultVector,
                                           trueToothMatrix     = trueTeeth)
    
    # Finally, we modify by trapping efficiency, which is calculated in this function using the filled but not aged data:
    
    iterationModifiedAgedFilledSampled <- modify.by.efficiency(sampledMatrix           = iterationSampled,
                                                               filledSampledMatrix     = iterationFilledSampled,
                                                               agedFilledSampledMatrix = iterationAgedFilledSampled,
                                                               cubOrAdultVector        = iterationCubAdultVector, 
                                                               ageSampledMatrix        = iterationAgeSampled)
    
    # Now, we store each step's density and information for plotting:
    
    densityHolder   <- c(densityHolder, 
                         apply(iterationSampled, 2, sum),
                         apply(iterationFilledSampled, 2, sum),
                         apply(iterationAgedFilledSampled, 2, sum),
                         apply(iterationModifiedAgedFilledSampled, 2, sum))
    yearHolder      <- c(yearHolder, 
                         rep(c(1:30), 4))
    typeHolder      <- c(typeHolder,
                         c(rep("Initial sampling", 30),
                           rep("Gaps filled", 30),
                           rep("Back-aged", 30),
                           rep("Efficiency-modified", 30)))
    iterationHolder <- c(iterationHolder, 
                         rep(iteration, 30*4))
    teTypeHolder    <- c(teTypeHolder,
                         rep(teType, 30*4))
    
    # And our CJS model predictions from marked:
    
    iterationCJSEstimates <- estimate.cjs(sampledMatrix    = iterationSampled, 
                                          ageSampledMatrix = iterationAgeSampled,
                                          trueSexVector    = trueSex,  
                                          yearsSampled     = 30)
    
    cjsIterHolder    <- c(cjsIterHolder, rep(iteration, nrow(iterationCJSEstimates)))
    cjsTEHolder      <- c(cjsTEHolder, rep(teType, nrow(iterationCJSEstimates)))
    cjsYearHolder    <- c(cjsYearHolder, iterationCJSEstimates$year)
    cjsDensityHolder <- c(cjsDensityHolder, iterationCJSEstimates$estimatedN)
    
    # We can now make our survival dataframe:
    
    iterationSurvivalFrame <- survival.frame.creation(eModifiedAgedFilledSampledMatrix = iterationModifiedAgedFilledSampled,
                                                      ageSampledMatrix                 = iterationAgeSampled,
                                                      cubOrAdultVector                 = iterationCubAdultVector,
                                                      trueSexVector                    = trueSex)
    
    # And make predictions off it:
    
    iterationSurvivalPredictions <- survival.relationships(survivalDataset = iterationSurvivalFrame, 
                                                           density         = 225)
    
    survPredHolder  <- c(survPredHolder, iterationSurvivalPredictions$Prediction)
    survSexHolder   <- c(survSexHolder, iterationSurvivalPredictions$Sex)
    survAgeHolder   <- c(survAgeHolder, iterationSurvivalPredictions$Age)
    survTypeHolder  <- c(survTypeHolder, iterationSurvivalPredictions$Type)
    survIterHolder  <- c(survIterHolder, rep(iteration, nrow(iterationSurvivalPredictions)))
    survTEHolder    <- c(survTEHolder, rep(teType, nrow(iterationSurvivalPredictions)))
    
  } #Done doing the whole process for each level of trapping efficiency
  
  
  if (iteration/25 == floor(iteration/25)) {
    print(paste(round(iteration*100/numIterations), "% done."))
  }
  
} #Done with all iterations of sampling and estimating density/survival relationships

#NOTE: Change directory

save.image(file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_1000.RData")

## Putting these together and saving them for posterity:

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Image_iteration_1000.RData")

densitySimulations <- data.frame(Density   = c(densityHolder, 
                                               cjsDensityHolder,
                                               rep(apply(trueAlive, 2, sum), 3)),
                                 Year      = c(yearHolder, 
                                               cjsYearHolder,
                                               rep(c(1:30), 3)),
                                 Step      = factor(c(typeHolder, 
                                                      rep("CJS", length(cjsDensityHolder)), 
                                                      rep("True", 30*3)),
                                                    levels = c("True", "CJS", "Efficiency-modified", "Back-aged",
                                                               "Gaps filled", "Initial sampling")),
                                 Iteration = c(iterationHolder, 
                                               cjsIterHolder,
                                               rep(max(iterationHolder) + 1, 30*3)),
                                 Trapping.Efficiency = factor(c(teTypeHolder, 
                                                                cjsTEHolder,
                                                                rep("Real", 30), 
                                                                rep("High", 30), 
                                                                rep("Low", 30)),
                                                              levels = c("Real", "High", "Low")))

# ggplot(densitySimulations %>% subset(Step %in% c("CJS", "True")), 
#        aes(x = Year, y = Density, color = Step)) + 
#   geom_line(data = densitySimulations %>% subset(Step == "CJS"),
#             aes(group = paste(Step, Iteration)), alpha = 0.1) +
#   geom_line(data = densitySimulations %>% subset(Step == "True"),
#             aes(group = )) +
#   facet_grid(.~Trapping.Efficiency)

originalSurvivalModelPredictions <- data.frame(Age    = rep(seq(0, 9, length.out = 10), 2),
                                               Sex    = c(rep("Male", 10), rep("Female", 10)),
                                               totalN = rep(225, 20))

originalSurvivalModelPredictions <- originalSurvivalModelPredictions %>%
  mutate(Prediction = predict(survMod, type = "response", newdata = originalSurvivalModelPredictions),
         Density    = totalN)

survivalSimulations <- data.frame(Age        = c(survAgeHolder, originalSurvivalModelPredictions$Age),
                                  Sex        = c(survSexHolder, originalSurvivalModelPredictions$Sex),
                                  Prediction = c(survPredHolder, originalSurvivalModelPredictions$Prediction),
                                  Iteration  = c(survIterHolder, rep(NA, nrow(originalSurvivalModelPredictions))),
                                  Type       = c(survTypeHolder, rep("Original", nrow(originalSurvivalModelPredictions))),
                                  Trapping.Efficiency = c(survTEHolder, rep("Original", nrow(originalSurvivalModelPredictions))))

#save(densitySimulations, survivalSimulations, file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Simulations.RData")

## Now, we can get stats from the different steps/methods:

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Simulations.RData")

trueDensities <- densitySimulations %>%
  subset(Step == "True") %>%
  distinct(Year, .keep_all = TRUE) %>%
  select(Year, Density)

differences <- densitySimulations %>%
  mutate(TrueDensity = trueDensities$Density[Year],
         Difference  = Density - TrueDensity)

#This one will be for comparing CJS to enumeration methods, removing the first year:
differencesAllButFirst <- differences %>%                             
  subset(Year %in% seq(2, 30)) %>%
  group_by(Trapping.Efficiency, Step) %>%
  mutate(mean     = mean(Difference),
         sd       = sd(Difference),
         percMean = mean(Difference*100/TrueDensity),
         percSD   = sd(Difference*100/TrueDensity)) %>%
  ungroup() %>%
  distinct(Trapping.Efficiency, Step, .keep_all = TRUE) %>%
  select(Trapping.Efficiency, Step, mean, sd, percMean, percSD) %>%
  arrange(Step, Trapping.Efficiency)

#This one will be for showing how enumeration improves after trimming terminal years:
differencesGoodYears <- differences %>% 
  subset(Year %in% seq(2, 27)) %>%
  group_by(Trapping.Efficiency, Step) %>%
  mutate(mean     = mean(Difference),
         sd       = sd(Difference),
         percMean = mean(Difference*100/TrueDensity),
         percSD   = sd(Difference*100/TrueDensity)) %>%
  ungroup() %>%
  distinct(Trapping.Efficiency, Step, .keep_all = TRUE) %>%
  select(Trapping.Efficiency, Step, mean, sd, percMean, percSD) %>%
  arrange(Step, Trapping.Efficiency)

#This one is trimmed down for plots
plotDifferences <- differences %>% 
  subset(Year %in% seq(2, 30)) %>% 
  mutate(Perc.Of.True        = 100*Density/TrueDensity,
         Trapping.Efficiency = factor(Trapping.Efficiency, levels = c("Low", "Real", "High")),
         Step                = factor(Step, levels = c("True", "Initial sampling", "Gaps filled", "Back-aged", "Efficiency-modified", "CJS")))

#This is just to see how survival probability is underestimated at age 0
survivalAge0 <- survivalSimulations %>%
  subset(Age == 0) %>%
  group_by(Sex, Type, Trapping.Efficiency) %>%
  mutate(Average.Prediction   = mean(Prediction)) %>%
  ungroup() %>%
  distinct(Sex, Type, Trapping.Efficiency, .keep_all = TRUE)

#And overestimated at age 9
survivalAge9 <- survivalSimulations %>%
  subset(Age == 9) %>%
  group_by(Sex, Type, Trapping.Efficiency) %>%
  mutate(Average.Prediction   = mean(Prediction)) %>%
  ungroup() %>%
  distinct(Sex, Type, Trapping.Efficiency, .keep_all = TRUE)

## Finally, we can make figures:

library(cowplot)

redColor            <- "#b21301"
purpleColor         <- "#811B7C"
lightestBlueColor   <- "#8DD3C7"
lightBlueColor      <- "#5CBDC2"
blueColor           <- "#2bb0bc"
yellowColor         <- "#A6761D"
greenColor          <- "#176e23"
maleColor           <- "#44AA99"
femaleColor         <- "#882255"

# First, we'll do three plots, to be stitched together later. The first of these will have MNA and eMNA contrasted with true density.
# The second will have CJS-derived estimates contrasted with true density.
# The third will have all three estimates represented as a fraction of true density.

plot1Frame <- densitySimulations %>%
  mutate(Trapping.Efficiency = factor(Trapping.Efficiency, levels = c("Low", "Real", "High")),
         TrueDensity         = trueDensities$Density[Year],
         Percentage          = 100*Density/TrueDensity,
         Step                = as.character(Step))

test <- plot1Frame %>% subset(Iteration %in% sample(c(1:1000), 50)) #To see if it works, with less data

firstRow <- ggplot(plot1Frame %>% subset(Step %in% c("Efficiency-modified", "Gaps filled")),
       aes(x = Year, y = Density, color = Step, group = paste(Iteration, Step))) +
  geom_line(alpha = 0.015) +
  geom_line(data = plot1Frame %>% subset(Step == "True")) +
  scale_color_manual(breaks = c("Efficiency-modified", "Gaps filled", "True"),
                     values = c(blueColor, greenColor, redColor)) +
  theme_bw() +
  guides(color = FALSE) +
  ylab("Enumeration\nabundance estimates") +
  ylim(c(80, 300)) +
  xlab(NULL) +
  scale_x_continuous(breaks = c(1, 10, 20, 30), expand = c(0.03, 0.03)) +
  facet_wrap(.~Trapping.Efficiency) + 
  theme(panel.grid.minor.x = element_blank())

secondRow <- ggplot(plot1Frame %>% subset(Step == "CJS"),
       aes(x = Year, y = Density, color = Step, group = paste(Iteration, Step))) +
  geom_line(alpha = 0.015) +
  geom_line(data = plot1Frame %>% subset(Step == "True")) +
  scale_color_manual(breaks = c("CJS", "True"),
                     values = c(yellowColor, redColor)) +
  theme_bw() +
  guides(color = FALSE) +
  ylab("Probabilistic\nabundance estimate\n") +
  ylim(c(80, 300)) +
  xlab(NULL) +
  scale_x_continuous(breaks = c(1, 10, 20, 30), expand = c(0.03, 0.03)) +
  facet_wrap(.~Trapping.Efficiency) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.minor.x = element_blank())

thirdRow <- ggplot(plot1Frame %>% 
         subset(Step %in% c("CJS", "Gaps filled", "Efficiency-modified")) %>%
           group_by(Year, Step, Trapping.Efficiency) %>%
           mutate(meanPercentage = mean(Percentage),
                  upper = quantile(Percentage, .975),
                  lower = quantile(Percentage, .025)) %>%
           ungroup() %>%
           distinct(Year, Step, Trapping.Efficiency, .keep_all = TRUE),
       aes(x = factor(Year), y = meanPercentage, color = Step)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = redColor) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(1),
                  size     = 0.25) +
  scale_color_manual(breaks = c("CJS", "Efficiency-modified", "Gaps filled"),
                     values = c(yellowColor, blueColor, greenColor)) +
  guides(color = FALSE) +
  ylab("Percentage of\ntrue abundance") +
  xlab("Year") +
  scale_x_discrete(breaks = c("1", "10", "20", "30")) +
  theme_bw() +
  facet_wrap(.~Trapping.Efficiency) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

library(patchwork)

yearsFigure <- firstRow/secondRow/thirdRow 

#NOTE: Change output directory

save_plot(yearsFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/Comparison over years 3.png",
          base_width = 10, base_height = 8,
          dpi = 600)
    
# Next, we'll make a plot of how well each method works overall:

stepsFigure <- ggplot(plotDifferences %>% subset(Step != "True"), 
       aes(x = Trapping.Efficiency, y = Perc.Of.True, color = Step)) + 
  geom_boxplot() + 
  geom_hline(yintercept = 100, color = redColor, linetype = "dashed") + 
  theme_bw() +
  scale_color_manual(breaks = c("Initial sampling", "Gaps filled", "Back-aged", "Efficiency-modified", "CJS"),
                     values = c(purpleColor, lightestBlueColor, lightBlueColor, blueColor, yellowColor)) + 
  xlab("Capture efficiency") + 
  ylab("Percentage of\ntrue abundance") + 
  guides(color = FALSE)

#NOTE: Change output directory

save_plot(stepsFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/Steps of adding information 1.png",
          base_width = 10, base_height = 4,
          dpi = 600)

# Finally, a plot of survival-age relationships, by sex and trapping efficiency levels:

plotSurvival <- survivalSimulations %>%
  mutate(Sex = as.character(Sex),
         Trapping.Efficiency = factor(Trapping.Efficiency, levels = c("Original", "Low", "Real", "High"))) %>%
  subset(Trapping.Efficiency != "Original")

plotSurvival$Sex[which(plotSurvival$Sex == "1")] <- "Female"
plotSurvival$Sex[which(plotSurvival$Sex == "2")] <- "Male"

originalSurvival <- survivalSimulations %>% 
  mutate(Sex = as.character(Sex)) %>%
  subset(Trapping.Efficiency == "Original") %>%
  select(-Trapping.Efficiency) %>%
  expand_grid(Trapping.Efficiency = c("High", "Low", "Real")) %>%
  mutate(Trapping.Efficiency = factor(Trapping.Efficiency, levels = c("Low", "Real", "High")))

originalSurvival$Sex[which(originalSurvival$Sex == "1")] <- "Female"
originalSurvival$Sex[which(originalSurvival$Sex == "2")] <- "Male"

survivalRelationships <- ggplot(plotSurvival %>% subset(Type == "Alternative"), 
       aes(x = Age, y = Prediction, color = Sex, group = Iteration)) + 
  geom_line(alpha = 0.05) + 
  geom_line(data = originalSurvival,
            aes(color = NULL)) +
  facet_grid(Sex~Trapping.Efficiency) + 
  guides(color = FALSE) + 
  theme_bw()  +
  ylab("Survival probability") + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8)) + 
  scale_color_manual(breaks = c("Female", "Male"),
                     values = c(femaleColor, maleColor))

#NOTE: Change output directory

save_plot(survivalRelationships, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/Survival relationships 1.png",
          base_width = 10, base_height = 8,
          dpi = 600)

### The next step of the analysis is to look at how abbreviating the consecutive number of years 
### for which data are collected will affect estimability through this method. We will be repeating
### the procedure adopted above, but will be collecting data for each number between 1 and 30. 
### Future studies can use any combination; simply change "seq(1, 30)" to whatever sequence it is
### desired to simulate

rm(list = ls())

library(tidyverse)
library(mgcv)
library(sjPlot)
library(marked)

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Functions image.RData")

studyLengthHolder <- c()
densityHolder     <- c()
yearHolder        <- c()
typeHolder        <- c()
iterationHolder   <- c()
teTypeHolder      <- c()

survPredHolder   <- c()
survSexHolder    <- c()
survAgeHolder    <- c()
survTypeHolder   <- c()
survIterHolder   <- c()
survTEHolder     <- c()

numIterations <- 100

set.seed(9453278) #WildCRU in numpad

for (iteration in 1:numIterations) { 
  
  for (studyLength in seq(1, 30)) { #This is the number of "years" of data being "collected" (sampled from the set underlying population)
    
    for (teType in c("Real", "High", "Low")) {
      
      # First, we sample:
      
      iterationSampled <- sample.matrix(trueAliveMatrix = trueAlive, 
                                        teType          = teType, 
                                        yearsSampled    = studyLength)
      
      # Now, we get a vector of cub-or-adult measurements:
      
      iterationCubAdultVector <- cub.adult.vector(sampledMatrix   = iterationSampled,
                                                  trueAliveMatrix = trueAlive,
                                                  trueAgeMatrix   = trueAge)
      
      # Next, we fill in our gaps:
      
      iterationFilledSampled <- fill.gaps(sampledMatrix    = iterationSampled,
                                          cubOrAdultVector = iterationCubAdultVector)
      
      # We make a matrix of individual ages based on our gap-filled matrix: 
      
      iterationAgeSampled <- age.matrix(filledSampledMatrix = iterationFilledSampled,
                                        cubOrAdultVector    = iterationCubAdultVector,
                                        trueToothMatrix     = trueTeeth)
      
      # Next, we back-age individuals first caught as adults:
      
      iterationAgedFilledSampled <- back.age(filledSampledMatrix = iterationFilledSampled,
                                             cubOrAdultVector    = iterationCubAdultVector,
                                             trueToothMatrix     = trueTeeth)
      
      # Finally, we modify by trapping efficiency, which is calculated in this function using the filled but not aged data:
      
      iterationModifiedAgedFilledSampled <- modify.by.efficiency(sampledMatrix           = iterationSampled,
                                                                 filledSampledMatrix     = iterationFilledSampled,
                                                                 agedFilledSampledMatrix = iterationAgedFilledSampled,
                                                                 cubOrAdultVector        = iterationCubAdultVector, 
                                                                 ageSampledMatrix        = iterationAgeSampled)
      
      # Now, we store each step's density and information for plotting:
      
      studyLengthHolder <- c(studyLengthHolder, 
                             rep(studyLength, studyLength*4))
      densityHolder     <- c(densityHolder, 
                             apply(as.matrix(iterationSampled[,1:studyLength]), 2, sum),
                             apply(as.matrix(iterationFilledSampled[,1:studyLength]), 2, sum),
                             apply(as.matrix(iterationAgedFilledSampled[,1:studyLength]), 2, sum),
                             apply(as.matrix(iterationModifiedAgedFilledSampled[,1:studyLength]), 2, sum))
      yearHolder        <- c(yearHolder, 
                             rep(c(1:studyLength), 4))
      typeHolder        <- c(typeHolder,
                             c(rep("Initial sampling", studyLength),
                               rep("Gaps filled", studyLength),
                               rep("Back-aged", studyLength),
                               rep("Efficiency-modified", studyLength)))
      iterationHolder   <- c(iterationHolder, 
                             rep(iteration, studyLength*4))
      teTypeHolder      <- c(teTypeHolder,
                             rep(teType, studyLength*4))
      
      } #Done doing the whole process for each level of trapping efficiency
    
    } #Done with this iteration's range of study lengths
  
  if (iteration/(numIterations/20) == floor(iteration/(numIterations/20))) {
    print(paste(round(iteration*100/numIterations), "% done."))
  } #Done with all iterations of sampling and estimating density relationship
}

## Now let's put it together and, for each year, compare the estimated value to the "true" value

yearsSet <- data.frame(studyLengthHolder, densityHolder, yearHolder, typeHolder, iterationHolder, teTypeHolder) %>%
  mutate(Study.Length        = studyLengthHolder,
         Density             = densityHolder,
         Year                = yearHolder,
         Metric              = typeHolder,
         Iteration           = iterationHolder,
         Trapping.Efficiency = teTypeHolder)

percOfTrue <- rep(NA, nrow(yearsSet))

for (i in 1:length(percOfTrue)) {
  yearInQuestion       <- yearsSet$yearHolder[i]
  trueDensityToCompare <- apply(trueAlive, 2, sum)[yearInQuestion]
  
  percOfTrue[i] <- 100*yearsSet$densityHolder[i]/trueDensityToCompare
}

yearsSet <- yearsSet %>%
  dplyr::select(Study.Length, Density, Year, Metric, Iteration, Trapping.Efficiency) %>%
  mutate(Percentage.of.true  = percOfTrue,
         Iteration           = factor(Iteration),
         Trapping.Efficiency = factor(Trapping.Efficiency, levels = c("Low", "Real", "High")),
         Metric              = factor(Metric, levels = c("Initial sampling", "Gaps filled", "Back-aged", "Efficiency-modified")))

#NOTE: Change output directory

save.image(file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Study length run image.RData")

## Next step will be to summarize the average deviation for a study length setting, across all iterations.
## We'll also calculate the standard deviation of that estimate, to add error bars.

rm(list = ls())

#NOTE: Change directory

load("C:/Users/jbrig/Desktop/Badgers/Badger demographics/R code/MNA paper code/Study length run image.RData")

redColor            <- "#b21301"
purpleColor         <- "#811B7C"
lightestBlueColor   <- "#8DD3C7"
blueColor           <- "#2bb0bc"

summaryByStudyLength <- yearsSet %>%
  group_by(Study.Length, Metric, Trapping.Efficiency) %>%
  mutate(Average.Percentage = mean(Percentage.of.true),
         Percentage.Upper   = quantile(Percentage.of.true, .95),
         Percentage.Lower   = quantile(Percentage.of.true, .05)) %>%
  distinct(Study.Length, Metric, Trapping.Efficiency, .keep_all = TRUE)

asymptoteFigure <- ggplot(summaryByStudyLength %>% subset(Metric %in% c("Initial sampling", "Gaps filled", "Efficiency-modified"))) +
  geom_line(aes(x = Study.Length, y = Average.Percentage, colour = Metric)) +
  geom_errorbar(aes(x = Study.Length, ymax = Percentage.Upper, ymin = Percentage.Lower,
                    colour = Metric), width = 0.5, position = "dodge") +
  geom_hline(yintercept = 100, colour = redColor, linetype = "dotted") +
  scale_colour_manual(breaks = c("Initial sampling", "Gaps filled", "Efficiency-modified"),
                      values = c(purpleColor, lightestBlueColor, blueColor)) +
  facet_wrap(.~Trapping.Efficiency, nrow = 3) + 
  theme_bw() +
  xlab("Study length") +
  ylab("Average Percentage\nof true abundance") + 
  guides(colour = FALSE)

#NOTE: Change output directory

save_plot(asymptoteFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/Asymptote figure 5.png",
          base_width = 6, base_height = 10,
          dpi = 600)

# Here we can find out how many years it takes for average estimation to go over 90% for eMNA
summaryByStudyLength %>% 
  subset(Metric %in% c("Efficiency-modified") &
           Average.Percentage >= 90) %>% 
  mutate(Average.Bias = -(100 - Average.Percentage)) %>%
  View() 

# And for estimation to go over 80% at low capture efficiency
summaryByStudyLength %>% 
  subset(Metric %in% c("Efficiency-modified") &
           Trapping.Efficiency == "Low" &
           Average.Percentage >= 80) %>% 
  mutate(Average.Bias = -(100 - Average.Percentage)) %>%
  View() 

# We'll make the same dataset trimming out terminal years for the appendix figure
summaryByStudyLengthGoodYears <- yearsSet %>%
  subset(Year > 1 & (Study.Length - Year) > 2) %>%
  group_by(Study.Length, Metric, Trapping.Efficiency) %>%
  mutate(Average.Percentage = mean(Percentage.of.true),
         Percentage.Upper   = quantile(Percentage.of.true, .95),
         Percentage.Lower   = quantile(Percentage.of.true, .05)) %>%
  distinct(Study.Length, Metric, Trapping.Efficiency, .keep_all = TRUE)

asymptoteSuppFigure <- ggplot(summaryByStudyLengthGoodYears %>% subset(Metric %in% c("Initial sampling", "Gaps filled", "Efficiency-modified"))) +
  geom_line(aes(x = Study.Length, y = Average.Percentage, colour = Metric)) +
  geom_errorbar(aes(x = Study.Length, ymax = Percentage.Upper, ymin = Percentage.Lower,
                    colour = Metric), width = 0.5, position = "dodge") +
  geom_hline(yintercept = 100, colour = redColor, linetype = "dotted") +
  scale_colour_manual(breaks = c("Initial sampling", "Gaps filled", "Efficiency-modified"),
                      values = c(purpleColor, lightestBlueColor, blueColor)) +
  facet_wrap(.~Trapping.Efficiency, nrow = 3) + 
  theme_bw() +
  xlab("Study length") +
  ylab("Average Percentage\nof true abundance") + 
  guides(colour = FALSE) + 
  xlim(0, 30)

#NOTE: Change output directory

save_plot(asymptoteSuppFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/Asymptote supplement figure S1.png",
          base_width = 6, base_height = 10,
          dpi = 600)


## Let's try to summarize the deviation by year of study and how it changes as longitudinal data are added in

summaryByYear <- yearsSet %>%
  mutate(Year = factor(Year),
         Study.Length = factor(Study.Length)) %>%
  group_by(Year, Study.Length, Metric, Trapping.Efficiency) %>%
  mutate(Average.Percentage = mean(Percentage.of.true)) %>%
  ungroup() %>%
  mutate(Study.Length       = as.numeric(as.character(Study.Length)),
         Year               = as.numeric(as.character(Year)),
         YearFromEnd        = Study.Length - Year)

uShapeFigure <- ggplot(summaryByYear %>% subset(Metric == "Efficiency-modified") %>% distinct(Study.Length, Trapping.Efficiency, YearFromEnd, .keep_all = TRUE), 
       aes(x = YearFromEnd, y = Average.Percentage)) +
  facet_wrap(.~Trapping.Efficiency, ncol = 3) +
  geom_line(aes(group = Study.Length, colour = Study.Length)) +
  geom_hline(yintercept = 100, colour = "#b21301", linetype = "dotted") +
  guides(colour = FALSE) +
  theme_bw() + 
  xlab("Years from end of study") + ylab("Average Percentage\nof true abundance")

#NOTE: Change output directory

save_plot(uShapeFigure, 
          file = "C:/Users/jbrig/Desktop/Badgers/Badger demographics/Methods manuscript/Figures/U-shape figure 4.png",
          base_width = 10, base_height = 4,
          dpi = 600)

