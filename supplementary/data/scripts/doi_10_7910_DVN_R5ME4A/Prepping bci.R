#######################################################
# Title: Preparing BCI data for individuals
# Name: Julius G. Bright Ross
# Date: Mar 18, 2019
# Last updated: Mar 18, 2019
# Description: Pulling individual residuals off
# seasonal, age, and sex-standardised means
#######################################################

## NOTE: MUST RUN EVERYTHING UP TO AND INCLUDING PARENTAL TIMELINES ##
## AND FERTILITY.R                                                  ##

currentcontents_bci <- ls()
currentwd_bci <- getwd()

library(mgcv)

## Making a BCI frame for each season ##

badgers_off$Body.Length <- as.numeric(as.character(badgers_off$Body.Length))

numbadgers <- length(badgerIndices)
numyears <- nrow(MNA)

bci <- function(weights, body.lengths) {
  W <- log(weights)
  L <- log(body.lengths)
  vec <- W/L
  
  return(mean(vec, na.rm = TRUE))
}

bciSpring <- data.frame(matrix(nrow = numbadgers, ncol = numyears))
bciSummer <- data.frame(matrix(nrow = numbadgers, ncol = numyears))
bciAutumn <- data.frame(matrix(nrow = numbadgers, ncol = numyears))
bciWinter <- data.frame(matrix(nrow = numbadgers, ncol = numyears))

relevantAges <- data.frame(matrix(nrow = numbadgers, ncol = numyears))
relevantAgeClasses <- data.frame(matrix(nrow = numbadgers, ncol = numyears))

for (i in 1:numbadgers) {
  badger <- badgerIndices[i]
  badgerObs <- subset(badgers_off, Tattoo == badger)

  uniqueYears <- unique(badgerObs$yearcode)
  numUniqueYears <- length(uniqueYears)
  
  for (j in 1:numUniqueYears) {
    
    yearSubset <- subset(badgerObs, yearcode == uniqueYears[j])
    
    springSubset <- subset(yearSubset, seasoncode == "a")
    summerSubset <- subset(yearSubset, seasoncode == "b")
    autumnSubset <- subset(yearSubset, seasoncode == "c")
    winterSubset <- subset(yearSubset, seasoncode == "d")
    
    bciSpring[i, uniqueYears[j]+1] <- bci(weights = springSubset$Weight,
                                        body.lengths = springSubset$Body.Length)
    bciSummer[i, uniqueYears[j]+1] <- bci(weights = summerSubset$Weight,
                                        body.lengths = summerSubset$Body.Length)
    bciAutumn[i, uniqueYears[j]+1] <- bci(weights = autumnSubset$Weight,
                                        body.lengths = autumnSubset$Body.Length)
    bciWinter[i, uniqueYears[j]+1] <- bci(weights = winterSubset$Weight,
                                        body.lengths = winterSubset$Body.Length)
  }
  
  relevantAges[i, uniqueYears+1] <- ageYearly[i, uniqueYears+1]
  relevantAgeClasses[i, uniqueYears+1] <- ageClassYearly[i, uniqueYears+1]
}

names(bciSpring) <- names(aliveYearly)
names(bciSummer) <- names(aliveYearly)
names(bciAutumn) <- names(aliveYearly)
names(bciWinter) <- names(aliveYearly)

names(relevantAges) <- names(aliveYearly)
names(relevantAgeClasses) <- names(aliveYearly)

## Putting into a single dataframe for model creation ##

individual <- c()
sex <- c()
year <- c()
bci <- c()
season <- c()
age <- c()
ageclass <- c()
density <- c()

malePositions <- c(which(badgerSex == "Male"))
femalePositions <- c(which(badgerSex == "Female"))

for (i in 1:numyears) {
  individual <- c(individual, rep(badgerIndices, 4))
  sex <- c(sex, rep(badgerSex, 4))
  year <- c(year, rep(MNA$Year[i], 4*numbadgers))
  bci <- c(bci, bciSpring[,i], bciSummer[,i], bciAutumn[,i], bciWinter[,i])
  season <- c(season, rep("Spring", numbadgers), rep("Summer", numbadgers), 
              rep("Autumn", numbadgers), rep("Winter", numbadgers))
  age <- c(age, rep(relevantAges[,i], 4))
  ageclass <- c(ageclass, rep(relevantAgeClasses[,i], 4))
  
  densityHolder <- rep(NA, numbadgers)
  densityHolder[malePositions] <- MNA$Male_adults[i] + MNA$Male_cubs[i]
  densityHolder[femalePositions] <- MNA$Female_adults[i] + MNA$Female_cubs[i]
  
  density <- c(density, rep(densityHolder, 4))
}

bciFrame <- data.frame(individual, sex, year, bci, season, age, ageclass, density)
rm(list = c("individual", "sex", "year", "bci", "season", "age", "ageclass", "density"))
bciFrame <- bciFrame[complete.cases(bciFrame),]

## Cutting out the 0.1 % on either end that are clearly erroneous and individuals ##
## that are likely too old to actually have been that age                         ##

lowest <- quantile(bciFrame$bci, .001)
highest <- quantile(bciFrame$bci, .999)
bciFrame <- subset(bciFrame, lowest < bci & bci < highest)
bciFrame <- subset(bciFrame, age < 15)

## Making two models: one trying to encompass a higher percentage of the variability ##
## in individual BCI; the other just giving deviation from natural differences       ##

fullMod <- lm(bci ~ age + I(age^2) + I(log(age+1)) + density + year + year:age + season + sex, 
                  data = bciFrame)

partMod <- lm(bci ~ age + I(age^2) + I(log(age+1)) + season + sex,
              data = bciFrame)

gamMod <- mgcv::gam(bci ~ s(age, by = season, bs = "ds") + sex + season, 
                    data = bciFrame)

#AIC(partMod, gamMod)

bciFrame$residual <- bciFrame$bci - predict(gamMod) 

## Putting into annual dataframe ##

year <- c()
individual <- c()
bciAnnual <- c()
age <- c()
survived <- c()
density <- c()
sex <- c()

for (i in 1:numbadgers) {
  badgerObs <- subset(bciFrame, individual == badgerIndices[i])
  yrsAlive <- unique(levels(factor(badgerObs$year)))
  numYrsAlive <- length(yrsAlive)
  
  if (numYrsAlive > 0) {
    for (j in 1:numYrsAlive) {
      yrObs <- subset(badgerObs, year == yrsAlive[j])
      
      bciAnnual <- c(bciAnnual, mean(yrObs$residual))
      year <- c(year, yrObs$year[1])
      age <- c(age, yrObs$age[1])
      density <- c(density, yrObs$density[1])
    }
    
    survived <- c(survived, rep(1, (numYrsAlive-1)), 0)
    
    sex <- c(sex, rep(badgerSex[i], numYrsAlive))
    individual <- c(individual, rep(badgerIndices[i], numYrsAlive))
  }
  
}

bciAnnual <- data.frame(individual, year, bciAnnual, sex, age, survived, density)

## Re-setting for continuing script ##

setwd(currentwd_bci)

keep_bci <- c(currentcontents_bci, "bciAnnual", "bciFrame")

rm(list = setdiff(ls(), keep_bci))
