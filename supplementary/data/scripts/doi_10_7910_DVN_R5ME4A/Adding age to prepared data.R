#######################################################
# Title: Adding age to prepared data
# Name: Julius G. Bright Ross
# Date: Apr 27, 2021
# Last updated: Apr 27, 2021
# Just adding in the age data we need for basically
# any effing operation on the badgers_off dataset
#######################################################

rm(list = ls())

load("C:/Users/jbrig/Desktop/Badgers/Badger condition/R code/Prepared badger data_9-4-20.RData")

library(tidyverse)

## Okay, running through each heckin' row of the badger data, cross-referencing with the ageYearly dataset, and adding an Age column ##

badgers_off$ActualAge <- NA

for (i in 1:nrow(badgers_off)) {
  year   <- badgers_off$Year[i]
  badger <- badgers_off$Tattoo[i]
  
  yearPos <- which(MNA$Year == year)
  indPos  <- which(badgerIndices == badger)
  
  badgers_off$ActualAge[i] <- ageYearly[indPos, yearPos]
}

save.image("C:/Users/jbrig/Desktop/Badgers/Badger condition/R code/Prepared badger data_27-4-21.RData")
