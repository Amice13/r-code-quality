############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list = ls())
library(bcp)
library(strucchange)
library(foreign)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)
set.seed(10162010)
source('Macros/rescale.R')
source('Macros/change_point.R')

rolldata <- read.table("../Data/CoxMcCubbins.txt")  
rollrate <- ts(rolldata[,2]/rolldata[,3], start = 45)   

##########################
## compute breakpoints   #
##########################
## note that we are regressing time series of majority pty roll rates on a constant--presumably we could include covariates

bp.rollrate <- breakpoints(rollrate ~ 1)           ## find breakpoints using Bai and Perron method
summary(bp.rollrate)                               ## report results
plot(bp.rollrate)                                  ## make plot
ci.rollrate <- confint(bp.rollrate, level = 0.90)  ## add confidence intervals
breakdates(ci.rollrate)

## compute F-stats for breakpoint analysis
fs.rollrate <- Fstats(rollrate ~ 1)
plot(fs.rollrate)
breakpoints(fs.rollrate)
lines(breakpoints(fs.rollrate))

n        <- length(rollrate)
congrow  <- seq(1,61,10)
conglabs <- as.character(seq(45,105,10))
yrlabs   <- as.character(seq(1877,1999,20))

p1 <- change_point(avgdf    = rollrate, 
                   mcmc_n   = 10000, 
                   margin_t = c(3, 3, 0, 2.95), 
                   margin_b = c(-2, 3, 3, 3), 
                   outfile  = '../Figures/Figure 4-2.pdf',
                   axis.title.x.size = 11, 
                   axis.title.y.size = 11, 
                   axis.text.size    = 11, 
                   axis.text.x.size  = 11, 
                   axis.text.y.size  = 11)
