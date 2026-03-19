#### MI_Dem_CIE.r ####
### Load libraries
library(MASS)
library(foreign)
library(Matching)
library(xtable)
library(snow)
library(stats)
library(arm)
library(car)
library(np)
library(KernSmooth)
library(boot)
library(sandwich)
library(coda)
library(rbounds)
library(gdata)
library(gtools)
library(lme4)
library(sfsmisc)
library(coin)
library(compiler)
library(coxme)
library(beanplot)
library(RItools)



### Define directories
working.dir <- "/Users/Allan/Dropbox/!!Papers/Liberal Peace/12-02-21_ISQ_commentary/DOR_ISQ_2013_Replication/M_Rep"

### Set working directory
getwd()
setwd(working.dir)

data <- read.dta("country_year_mimpute.dta")
names(data)

require(Amelia)
summary(data)
data[1,]

#Converting MidEast factor into an indicator variable, with 1 as no, and 2 as yes. 
data$rmideast <- as.numeric(data$rmideast)

##Set seed
set.seed(1234)

a.out121117 <- amelia(data, m=20, ts="year", cs="ccode", polytime=2, empri=0.01*nrow(data), 
                idvars=c("abbrev"), intercs=TRUE, 
                lags=c("lngdppc","lngdppcsq","lngdppccube", "mid", "fmid", "war"), 
                      leads=c("lngdppc","lngdppcsq","lngdppccube", "mid", "fmid", "war"))
#empri provides a ridge prior for numerical stability. 

a.out.total <- a.out121117

#save(a.out.total, file="imputationsTotal121117.RData")

write.amelia(obj=a.out.total, file.stem="outdatatotal121117", format="dta")