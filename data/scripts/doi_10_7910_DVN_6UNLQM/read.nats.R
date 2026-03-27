rm(list=ls())
library(sas7bdat)
library(survey)

#nats 2012-2013
datos <- read.sas7bdat("data/2012-2013-NATS-Format/nats2012.sas7bdat")
datos$current.daily.cig.smoker <- 0
datos$current.daily.cig.smoker[datos$SMOK100==1 & datos$SMOKNOW %in% c(1,2)] <- 1
datos$current.daily.cig.smoker <- as.factor(datos$current.daily.cig.smoker)
datos$cigtry <- as.factor(datos$SMOKFIRSTAGE)

#nats 2009-2010
nats0910 <- read.sas7bdat("data/2009-2010-sas/nats_09_10.sas7bdat")
nats0910$current.daily.cig.smoker <- 0
nats0910$current.daily.cig.smoker[nats0910$SMOK100==1 & nats0910$SMOKNOW %in% c(1,2)] <- 1
nats0910$current.daily.cig.smoker <- as.factor(nats0910$current.daily.cig.smoker)
nats0910$cigtry <- as.factor(nats0910$SMOKWHOLAGE)
nats0910$WT_NATIONAL <- nats0910$WT_national

datos.all <- rbind(datos[,c("AGE","current.daily.cig.smoker","cigtry","WT_NATIONAL")],
                  nats0910[,c("AGE","current.daily.cig.smoker","cigtry","WT_NATIONAL")])

datos.svy <-  svydesign(ids=~1, weights=~WT_NATIONAL, data=datos.all)

#% of currently daily smokers among 35-39 year olds who had ever tried a cigarette
#SMOKFIRSTAGE : How old were you the first time you smoked part or all of a cigarette?
#SMOK100: Have you smoked at least 100 cigarettes in your entire life?
#SMOKNOW: Do you know smoke cigarettes every day, some days, or not at all?

current.cig.cig.init.pt.est.se <- svyby(~current.daily.cig.smoker, ~cigtry, subset(datos.svy, AGE %in% c(35:39) & cigtry %in% as.character(12:29)), svyciprop, na.rm=TRUE)
save(current.cig.cig.init.pt.est.se, file="data/current.cig.cig.init.pt.est.se.Rdata")