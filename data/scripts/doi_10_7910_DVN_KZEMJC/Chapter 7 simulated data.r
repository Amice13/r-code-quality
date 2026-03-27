#####################
#Time Series Analysis for the Social Sciences
#Box-Steffensmeier, Freeman, Hitt, and Pevehouse
#
#Chapter 7 replication
#####################

#####################
#Simulating Data for Near/Fractional Integration Examples
#Figure 7.1
#####################

remove(list = ls())
library(foreign)
library(fracdiff)
set.seed(10221985)

time <- seq(1:400)

near.int <- arima.sim(list(order = c(1,0,0), ar = .9), n = 400)

frac.int.sim <- fracdiff.sim(400, d = .45)
frac.int <- as.ts(frac.int.sim$series)

sims <- data.frame(cbind(time,near.int,frac.int))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims, file = "fracint_sims.dta")
