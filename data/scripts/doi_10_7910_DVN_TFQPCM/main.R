library(ergm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(punitroots)


## ~25 hours
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Paper/Contagion.R")

## ~28 hours
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Paper/Homophily.R")

## ~28 hours
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Paper/Contagion+Homophily.R")

## ~28 hours
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Paper/Homophily+Timeshock.R")

## ~5 minutes
## Can only run after "Contagion.R", "Homophily.R", "Contagion+Homohily.R" and "Homophily+Timeshock.R"
source("Paper/Plots.R")


