rm(list=ls())
library(sas7bdat)
library(survey)

#read nsduh data
#source: https://www.datafiles.samhsa.gov/study-dataset/national-survey-drug-use-and-health-2014-nsduh-2014-ds0001-nid16876
load("data/NSDUH-2014-DS0001-data-r.rda") datos <- da36361.0001
datos$AGE2 <- factor(datos$AGE2)
levels(datos$AGE2) <- c(12:21,22,24,26,30,35,50,65)
datos$cigever <- factor(datos$cigever)
levels(datos$cigever) <- c(1,0)
datos$current.daily.cig.smoker <- 0
datos$current.daily.cig.smoker[datos$CIG100LF==1 & datos$cigrec==1] <- 1
datos$current.daily.cig.smoker <- as.factor(datos$current.daily.cig.smoker)
datos$cigtry <- as.factor(datos$cigtry)
datos.svy <-  svydesign(ids=~1, weights=~ANALWT_C, data=datos)

#never tried cigarettes
never.tried.cig.pt.est.se <- svyby(~cigever, ~AGE2, datos.svy, svyciprop, na.rm=TRUE)
names(never.tried.cig.pt.est.se)[2] <- "cignever"
never.tried.cig.pt.est.se <- rbind(never.tried.cig.pt.est.se,
                                   subset(never.tried.cig.pt.est.se, AGE2=="22"),
                                   subset(never.tried.cig.pt.est.se, AGE2=="24"))
never.tried.cig.pt.est.se$AGE2 <- as.character(never.tried.cig.pt.est.se$AGE2)
never.tried.cig.pt.est.se$AGE2[never.tried.cig.pt.est.se$AGE2=="22"] <- as.character(22:23) #NSDUH combines 22-23 year olds
never.tried.cig.pt.est.se$AGE2[never.tried.cig.pt.est.se$AGE2=="24"] <- as.character(24:25) #NSDUH combines 24-25 year olds
never.tried.cig.pt.est.se <- never.tried.cig.pt.est.se[order(never.tried.cig.pt.est.se$AGE2),]
rownames(never.tried.cig.pt.est.se) <- c(12:17,18:25,26,30,35,50,65)
save(never.tried.cig.pt.est.se, file="~/Dropbox/smoking/wfu/data/never.tried.cig.pt.est.se.Rdata")

#% of currently daily smokers among 25-39 year olds who had ever tried a cigarette
#cigtry: How old were you the first time you smoked part or all of a cigarette?
#cig100lf: Have you smoked at least 100 cigarettes in your entire life?
#cigrec: How long has it been since you last smoked part or all of a cigarette?
current.cig.cig.init.pt.est.se <- svyby(~current.daily.cig.smoker, ~cigtry, subset(datos.svy, AGE2 %in% c("35") & cigtry %in% as.character(12:29)), svyciprop, na.rm=TRUE)
current.cig.cig.init.pt.est.se["25",2] <- mean(c(current.cig.cig.init.pt.est.se["24",2],current.cig.cig.init.pt.est.se["27",2]))
current.cig.cig.init.pt.est.se["25",3] <- mean(c(current.cig.cig.init.pt.est.se["24",3],current.cig.cig.init.pt.est.se["27",3]))
current.cig.cig.init.pt.est.se["26",2] <- mean(c(current.cig.cig.init.pt.est.se["24",2],current.cig.cig.init.pt.est.se["27",2]))
current.cig.cig.init.pt.est.se["26",3] <- mean(c(current.cig.cig.init.pt.est.se["24",3],current.cig.cig.init.pt.est.se["27",3]))
save(current.cig.cig.init.pt.est.se, file="data/current.cig.cig.init.pt.est.se.Rdata")