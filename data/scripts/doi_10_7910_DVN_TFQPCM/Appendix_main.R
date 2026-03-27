########### APPENDIX ###########

library(ergm)
library(dplyr)
library(ggplot2)
library(tidyr)
library(punitroots)
library(sna)
library(tseries)
library(collapse)


## ~25 hours
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Appendix/rawDataNet.R")

## ~10 minutes
## Can only run after "rawDataNet.R"
source("Appendix/rawdatPlots.R")

## ~30 hours 
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Appendix/NetDenSTtest.R")     

## ~5 minutes
## Can only run after "NetDenSTtest.R" and "Homophily+Timeshock.R" (in Paper Folder)
source("Appendix/Appendix_Plots.R")

## ~15 minutes
## Stand alone script and can be run simultaneously with other stand alone scrips. 
source("Appendix/polity.R")

