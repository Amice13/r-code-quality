#####################
#Time Series Analysis for the Social Sciences
#Box-Steffensmeier, Freeman, Hitt, and Pevehouse
#
#Chapter 2 replication
#####################

#####################
#Simulating Data for ACF/PACF Examples
#Figures 2.2 and 2.3
#####################
library(foreign)
set.seed(10221985)

time <- seq(1:500)

white.noise <- arima.sim(list(order = c(0,0,0)), n = 500)
white.noise <- white.noise[!is.na(white.noise)]

ar1.7 <- arima.sim(list(order = c(1,0,0), ar = 0.7), n = 500)

ar1.7neg <- arima.sim(list(order = c(1,0,0), ar = -0.7), n = 500)

ar2 <- arima.sim(list(order = c(2,0,0), ar = c(0.75,-0.4)), n = 500)

ma1 <- arima.sim(list(order = c(0,0,1), ma = 0.7), n = 500)

ma2 <- arima.sim(list(order = c(0,0,2), ma = c(0.75, -0.4)), n= 500)

arma <- arima.sim(list(order = c(1,0,1), ar = 0.75, ma = 0.7), n = 500)

sim.mat <- cbind(time,white.noise,ar1.7,ar1.7neg,ar2,ma1,ma2,arma)
sims <- data.frame(sim.mat)
write.dta(sims,file = "C:/Users/matthitt/Dropbox/TSASS Book/data/univariate_sims.dta")
