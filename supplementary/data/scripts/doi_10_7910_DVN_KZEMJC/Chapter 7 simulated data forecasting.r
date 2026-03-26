#####################
#Time Series Analysis for the Social Sciences
#Box-Steffensmeier, Freeman, Hitt, and Pevehouse
#
#Chapter 7 replication (forecasting section)
#####################

#####################
#Simulating Data for Figure 7.5
#####################

remove(list = ls())
library(foreign)
set.seed(10221985)

time <- seq(1:5000)
ser1 <- arima.sim(list(order = c(1,1,1), ar = .9, ma = -.4), n = 5000)

ser2 <- arima.sim(list(order = c(1,1,1), ar = .9, ma = -.4), n = 5000)

ser3 <- arima.sim(list(order = c(1,1,1), ar = .9, ma = -.4), n = 5000)

ser4 <- arima.sim(list(order = c(1,1,1), ar = .9, ma = -.4), n = 5000)

sims <- data.frame(cbind(as.ts(time),ser1,ser2,ser3,ser4))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")
write.dta(sims, file = "forecastCurve_sims.dta")

######################
#Simulating data for Figures 7.6-7.10
######################

remove(list = ls())
library(foreign)
set.seed(10221985)


time2 <- seq(1:400)

rw1 <- cumsum(rnorm(400, mean = 0, sd = .5))
rw2 <- cumsum(rnorm(400, mean = 0, sd = .5))
rw3 <- cumsum(rnorm(400, mean = 0, sd = .5))
rw4 <- cumsum(rnorm(400, mean = 0, sd = .5))

rws <- cbind(as.ts(rw1,rw2,rw3,rw4))

ar2.1 <- arima.sim(list(order = c(2,0,0), ar = c(.6,.3)), n = 400)
ar2.2 <- arima.sim(list(order = c(2,0,0), ar = c(.6,.3)), n = 400)
ar2.3 <- arima.sim(list(order = c(2,0,0), ar = c(.6,.3)), n = 400)
ar2.4<- arima.sim(list(order = c(2,0,0), ar = c(.6,.3)), n = 400)

ar2s <- cbind(ar2.1,ar2.2,ar2.3,ar2.4)

ma1.1 <- arima.sim(list(order = c(0,0,1), ma = .7), n = 400)
ma1.2 <- arima.sim(list(order = c(0,0,1), ma = .7), n = 400)
ma1.3 <- arima.sim(list(order = c(0,0,1), ma = .7), n = 400)
ma1.4 <- arima.sim(list(order = c(0,0,1), ma = .7), n = 400)

ma1s <- cbind(ma1.1,ma1.2,ma1.3,ma1.4)

arima110.1 <- arima.sim(list(order = c(1,1,0), ar = .7), n = 400)
arima110.2 <- arima.sim(list(order = c(1,1,0), ar = .7), n = 400)
arima110.3 <- arima.sim(list(order = c(1,1,0), ar = .7), n = 400)
arima110.4 <- arima.sim(list(order = c(1,1,0), ar = .7), n = 400)

arima110s <- cbind(arima110.1,arima110.2,arima110.3,arima110.4)

arima011.1 <- arima.sim(list(order = c(0,1,1), ma = -.6), n = 400)
arima011.2 <- arima.sim(list(order = c(0,1,1), ma = -.6), n = 400)
arima011.3 <- arima.sim(list(order = c(0,1,1), ma = -.6), n = 400)
arima011.4 <- arima.sim(list(order = c(0,1,1), ma = -.6), n = 400)

arima011s <- cbind(arima011.1,arima011.2,arima011.3,arima011.4)

sims.2 <- data.frame(cbind(as.ts(time2),rws,ar2s,ma1s,arima110s,arima011s))

setwd("C:/Users/matthitt/Dropbox/TSASS Book/data")

write.dta(sims.2, file = "forecast_sims.dta")