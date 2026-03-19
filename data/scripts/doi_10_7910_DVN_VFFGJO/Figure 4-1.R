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

## Change point analysis of Petersen data           
coalsize.ts <- ts(read.table("../Data/Petersen.txt"), start=47)
coalsize    <- as.matrix(read.table("../Data/Petersen.txt"))
n           <- length(coalsize.ts)

congrow     <- c(1,4,9,14,19,24,29,33)
conglabs    <- c("47","50","55","60","65","70","75","79")
yrlabs      <- c("1881","1887","1897","1907","1917","1927","1937","1945")

petersen    <- change_point(avgdf = coalsize, outfile = '../Figures/Figure 4-1a.pdf')

## to check output for barry-hartigan:
pt <- seq(from=47,length.out=length(coalsize))
rbind(pt, petersen$bcp.out$posterior.prob)

## for bai-perron:
petersen$bp
petersen$ci

## Change point analysis with Stathis data          
## note that Stathis does not have legislation for 54, and 58 
dta.all      <- na.omit(read.dta("../Data/Stathis.dta"))
coalsize.avg <- tapply(dta.all$coalition,dta.all$congress,mean)
coalsize.ts  <- ts(coalsize.avg,start=39)
n            <- length(coalsize.ts)

congrow      <- c(1,7,12,16,20,25,30,35,39)
conglabs     <- c("39","45","50","55","60","65","70","75","79")
yrlabs       <- c("1865","1877","1887","1897","1907","1917","1927","1937","1945")

stathis      <- change_point(avgdf = coalsize.avg, outfile = '../Figures/Figure 4-1b.pdf')

## to check output for barry-hartigan:
ps <- seq(from=39, length.out=length(coalsize.avg))
cbind.data.frame(names(coalsize.avg), stathis$bcp.out$posterior.prob)
cbind(ps, stathis$bcp.out$posterior.prob)

## for bai-perron:
stathis$bp
stathis$ci

## Change point analysis with Clinton Lapinski data 
cl_dta.all      <- read.dta("../Data/ClintonLapinski.dta")
cl_dta          <- na.omit(subset(cl_dta.all,select=c(new_pcty,congress,sig_score,treaty_amend), sig_score > .25 & treaty_amend == 0))
coalsize.cl.avg <- tapply(cl_dta$new_pcty,cl_dta$congress,mean)
coalsize.cl.ts  <- ts(coalsize.cl.avg,start=45)
n               <- length(coalsize.cl.ts)

congrow         <- c(1,5,9,14,19,24,29,33)
conglabs        <- c("45","50","55","60","65","70","75","79")
yrlabs          <- c("1877","1887","1897","1907","1917","1927","1937","1945")

cl              <- change_point(avgdf = coalsize.cl.avg, outfile = '../Figures/Figure 4-1c.pdf')

## to check output for barry-hartigan:
cbind.data.frame(names(coalsize.cl.avg), cl$bcp.out$posterior.prob)

## for bai-perron:
cl$bp
cl$ci
