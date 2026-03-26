
# Table A.7

if (!require("pacman")) install.packages("pacman")


p_load(readstata13, ggplot2) # these two packages may need to be installed
p_load(data.table, AER)  #https://cran.r-project.org/web/packages/AER/AER.pdf 
set.seed(111) # random number generator
setwd("/Users/manirouhirad/Dropbox/Panama S2/Draft Manuscripts/Natural and Physical Capital Complementarity") # just need to change this
dt = read.dta13("./data_SEM_tolls_32719.dta")
dt = data.table(dt)

#IV - not exactly replicating. 
ivreg1 <- ivreg(
  tolls ~ monthly_lake_level_2 + monthly_lake_level_2:expansion + dummy_price_11 +dummy_price_13 + season | 
    dummy_price_11 +dummy_price_13 + season + monthly_rainfall_in_lag_1+ monthly_rainfall_in_lag_1:expansion,
  data = dt, x = TRUE)
summary(ivreg1)




error.scale <- 1
error <- rnorm(dim(dt)[[1]],0, error.scale*(ivreg1$sigma))  


intercept <- ivreg1$coefficients[3]*mean(dt$dummy_price_11) +
  ivreg1$coefficients[4]*mean(dt$dummy_price_13) +
  ivreg1$coefficients[1]


stage1 <- as.data.frame(ivreg1$x$projected)

sim.data<-as.data.frame(
  cbind(stage1$monthly_lake_level_2, 
        dt$expansion,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2+error)
)
colnames(sim.data) <- c("lakelevel", "expan", "tolls", "s.tolls")

reg1 <- lm(s.tolls ~ lakelevel + expan:lakelevel, data = sim.data)
reg2 <- lm(s.tolls ~ lakelevel, data = sim.data)
reg3 <- lm(s.tolls ~ lakelevel+ expan:lakelevel + expan, data = sim.data)

#no error

rega <- lm(tolls ~ lakelevel + expan:lakelevel, data = sim.data)
summary(rega)


error.scale <- .01
error <- rnorm(dim(dt)[[1]],0, error.scale*(ivreg1$sigma))  


intercept <- ivreg1$coefficients[3]*mean(dt$dummy_price_11) +
  ivreg1$coefficients[4]*mean(dt$dummy_price_13) +
  ivreg1$coefficients[1]


stage1 <- as.data.frame(ivreg1$x$projected)

sim.data<-as.data.frame(
  cbind(stage1$monthly_lake_level_2, 
        dt$expansion,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2+error)
)
colnames(sim.data) <- c("lakelevel", "expan", "tolls", "s.tolls")

reg4 <- lm(s.tolls ~ lakelevel + expan:lakelevel, data = sim.data)
reg5 <- lm(s.tolls ~ lakelevel, data = sim.data)
reg6 <- lm(s.tolls ~ lakelevel+ expan:lakelevel + expan, data = sim.data)



error.scale <- 10
error <- rnorm(dim(dt)[[1]],0, error.scale*(ivreg1$sigma))  

intercept <- ivreg1$coefficients[3]*mean(dt$dummy_price_11) +
  ivreg1$coefficients[4]*mean(dt$dummy_price_13) +
  ivreg1$coefficients[1]

stage1   <- as.data.frame(ivreg1$x$projected)
sim.data <- as.data.frame(
  cbind(stage1$monthly_lake_level_2, 
        dt$expansion,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2,
        intercept + ivreg1$coefficients[2]*stage1$monthly_lake_level_2 + ivreg1$coefficients[6]*dt$expansion*stage1$monthly_lake_level_2+error)
)
colnames(sim.data) <- c("lakelevel", "expan", "tolls", "s.tolls")

reg7 <- lm(s.tolls ~ lakelevel + expan:lakelevel, data = sim.data)
reg8 <- lm(s.tolls ~ lakelevel, data = sim.data)
reg9 <- lm(s.tolls ~ lakelevel+ expan:lakelevel + expan, data = sim.data)


stargazer(rega, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, type = "text")