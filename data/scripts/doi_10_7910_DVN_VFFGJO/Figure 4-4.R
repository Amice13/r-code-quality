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

bg        <- read.dta("../Data/BrunellGrofman.dta")
pctdiv.ts <- ts(bg$percentdivided)
pctdiv    <- bg$percentdivided
n         <- length(pctdiv.ts)

## compute breakpoints--note that we are regressing time series of
## coalition size on a constant--presumably we could include covariates 

## priors:
## f(p) 1 /p_0, O < p < p_0,
## f(w) =I/w_0, O < w < wO,

## The restrictions on the ranges of p and w are designed to make the
## technique effective in situations where there are not too many
## changes (p small) and where the changes that do occur are of a
## reasonable size (w small). 

congrow  <- seq(1,109,4)
yrlabs   <- as.character(seq(1788,2004,8))
for (i in seq(2, length(yrlabs), 2)) yrlabs[i]  <- ""
conglabs <- yrlabs

p2 <- change_point(avgdf  = pctdiv, 
                   mcmc_n = 50000, 
                   p = .2, 
                   w = .2,
                   outfile  = '../Figures/Figure 4-4.pdf',
                   margin_t = c(3, 3, 0.5, 5.8), 
                   margin_b = c(-2, 3, 3, 3), 
                   xtitle   = 'Year', 
                   xlabs    = yrlabs, 
                   x.angle  = 45, 
                   x.hjust  = 1, 
                   axis.title.x.size = 11, 
                   axis.title.y.size = 11, 
                   axis.text.size    = 9, 
                   axis.text.x.size  = 8, 
                   axis.text.y.size  = 9)
