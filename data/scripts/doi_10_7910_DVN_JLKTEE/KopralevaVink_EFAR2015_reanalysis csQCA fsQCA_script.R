# clear the workspace
rm(list=ls())

# Install and load QCA
library("QCA", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

# define data frame cs
KV.cs <- matrix(nrow = 10, ncol = 5) 

# view the -empty- data frame
View(KV.cs)

# set wd
setwd("~/Desktop")

# import data "KopralevaVink_EFAR2015" from your workspace
# indicate that variable "case" includes the row.names (however, by including this command you will no longer be able to identify this variable for case labels)
# if you want to displays scattergraphs with case labels = $case then exclude the row.names command 
# in that case when reporting cases in output you will see case numbers
KV.data <- read.csv("KopralevaVink_EFAR2015.csv",sep=",",header=TRUE, row.names="case")
View(KV.data)

#import data into data frame
KV.cs <- KV.data

# view the data in the data frame
View(KV.cs)

summary(KV.data)
mode(KV.data)

# define data frame fs
KV.fs <- matrix(nrow = 10, ncol = 5) 

# view the -empty- fs data frame
View(KV.fs)

#import data into fs data frame
KV.fs <- KV.data

# view the data in the data frame
View(KV.fs)

summary(KV.data)
mode(KV.data)

# NB in the EFAR paper we only present the fuzzy-set analysis, 
# but if you are interested, you could also do a crisp-set analysis
# cs calibration
# use thresholds from Kopraleva & Vink
# note that $tradems are already dichoptimized
KV.cs$sanctions <- calibrate(KV.data$sanctions, type = "crisp", logistic = TRUE, thresholds = c(0.5))
KV.cs$tradeeu <- calibrate(KV.data$tradeeu, type = "crisp", logistic = TRUE,
                                 thresholds = c(0.5))
KV.cs$demo <- calibrate(KV.data$demo, type = "crisp", logistic = TRUE,
                              thresholds = c(5.5))
KV.cs$victims <- calibrate(KV.data$victims, type = "crisp", logistic = TRUE,
                                 thresholds = c(100))
View(KV.cs)

# fs calibration
# use thresholds from Kopraleva & Vink
# note that $sanctions and $tradems are already calibrated
KV.fs$tradeeu <- round(calibrate(KV.data$tradeeu, type = "fuzzy", logistic = TRUE,
	thresholds = c(0.07, 0.5, 1.12)), 2)
KV.fs$demo <- round(calibrate(KV.data$demo, type = "fuzzy", logistic = TRUE,
	thresholds = c(-10, 5.5, 10)), 2)
KV.fs$victims <- round(calibrate(KV.data$victims, type = "fuzzy", logistic = TRUE,
	thresholds = c(0, 100, 621)), 2)
View(KV.fs)

# necessity analysis 1 cs
## you can adjust the inclusion and coverage cut-offs
KVnec1cs <- superSubset(KV.cs, outcome = "sanctions", 
                      conditions = c("tradeeu", "tradems", "demo", "victims"),
                      incl.cut = 0.90, cov.cut = 0.52)
KVnec1cs

# necessity analysis 0 cs
KVnec0cs <- superSubset(KV.cs, outcome = "sanctions", neg.out = TRUE, 
                      conditions = c("tradeeu", "tradems", "demo", "victims"),
                      incl.cut = 0.90, cov.cut = 0.52)
KVnec0cs

# necessity analysis 1 fs
## you can adjust the inclusion and coverage cut-offs
KVnec1 <- superSubset(KV.fs, outcome = "sanctions", 
    conditions = c("tradeeu", "tradems", "demo", "victims"),
    incl.cut = 0.90, cov.cut = 0.52)
KVnec1

# necessity analysis 0 fs
KVnec0 <- superSubset(KV.fs, outcome = "sanctions", neg.out = TRUE, 
    conditions = c("tradeeu", "tradems", "demo", "victims"),
    incl.cut = 0.90, cov.cut = 0.52)
KVnec0

# plot TRADEMS
# first install and load package "calibrate" to use "textxy" command
plot(KV.fs$tradems, (1-KV.fs$sanctions), pch = 19,
     xlab = "Trade with EU member state", ylab = "Absence of sanctions",
     abline(0, 1))
textxy(KV.fs$tradems, (1-KV.fs$sanctions), KV.fs$case, cex = 0.7, offset = -1.1)

# create truth table 1 cs
KVtt1cs <- truthTable(KV.cs, outcome = "sanctions", incl.cut1 = 0.8,
                    conditions = c("tradeeu", "tradems", "demo", "victims"),
                    show.cases = TRUE, sort.by = c("incl", "n"))
KVtt1cs

# Boolean minimiation 1 (complex) cs
KVcomplex1cs <- eqmcc(KVtt1cs, details = TRUE, show.cases = TRUE)
KVcomplex1cs

# Boolean minimiation 1 (parsimonious) cs 
KVpars1cs <- eqmcc(KVtt1cs, include = "?",  
                 details = TRUE, show.cases = TRUE)
KVpars1cs

# Check Simplifying Assumptions
KVpars1cs$SA$M1

# create truth table 0 cs
KVtt0cs <- truthTable(KV.cs, outcome = "sanctions", neg.out = TRUE, 
                    conditions = c("tradeeu", "tradems", "demo", "victims"),
                    incl.cut1 = 0.8, show.cases = TRUE, sort.by = c("incl", "n"))
KVtt0cs

# Boolean minimiation 0 (complex) cs
KVcomplex0cs <- eqmcc(KVtt0cs, details = TRUE, show.cases = TRUE)
KVcomplex0cs

# Boolean minimiation 0 (parsimonious) cs
KVpars0cs <- eqmcc(KVtt0cs, include = "?",  
                 details = TRUE, show.cases = TRUE)
KVpars0cs

# create truth table 1 fs
KVtt1fs <- truthTable(KV.fs, outcome = "sanctions", incl.cut1 = 0.8,
  conditions = c("tradeeu", "tradems", "demo", "victims"),
	show.cases = TRUE, sort.by = c("incl", "n"))
KVtt1fs

# Boolean minimiation 1 (complex) fs
KVcomplex1fs <- eqmcc(KVtt1fs, details = TRUE, show.cases = TRUE)
KVcomplex1fs

# Boolean minimiation 1 (parsimonious) fs 
KVpars1fs <- eqmcc(KVtt1fs, include = "?",  
	details = TRUE, show.cases = TRUE)
KVpars1fs

# Check Simplifying Assumptions
KVpars1fs$SA$M1

# you can also plot the sufficiency relation
# for example for the parsimonious solution of the 1 outcome, lets plot tradems and sanctions
plot((1-KV.fs$tradems), KV.fs$sanctions, pch = 19,
     xlab = "No trade with MS", ylab = "Sanctions",
     abline(0, 1))
textxy((1-KV.fs$tradems), KV.fs$sanctions, KV.fs$case, cex = 0.7, offset = 0.9, pos = 1)

# create truth table 0 fs
KVtt0fs <- truthTable(KV.fs, outcome = "sanctions", neg.out = TRUE, 
  conditions = c("tradeeu", "tradems", "demo", "victims"),
  incl.cut1 = 0.8, show.cases = TRUE, sort.by = c("incl", "n"))
KVtt0fs

# Boolean minimiation 0 (complex) fs
KVcomplex0fs <- eqmcc(KVtt0fs, details = TRUE, show.cases = TRUE)
KVcomplex0fs

# Boolean minimiation 0 (parsimonious) fs
KVpars0fs <- eqmcc(KVtt0fs, include = "?",  
	details = TRUE, show.cases = TRUE)
KVpars0fs

# you can also plot the sufficiency relation
# for example for the parsimonious solution of the 0 outcome, lets plot tradeeu and sanctions
# to get the case labels we import without row.names="case"
KV.data <- read.csv("KopralevaVink_EFAR2015.csv",sep=",",header=TRUE)
KV.fs <- KV.data
KV.fs$tradeeu <- round(calibrate(KV.data$tradeeu, type = "fuzzy", logistic = TRUE,
                                 thresholds = c(0.07, 0.5, 1.12)), 2)
KV.fs$demo <- round(calibrate(KV.data$demo, type = "fuzzy", logistic = TRUE,
                              thresholds = c(-10, 5.5, 10)), 2)
KV.fs$victims <- round(calibrate(KV.data$victims, type = "fuzzy", logistic = TRUE,
                                 thresholds = c(0, 100, 621)), 2)
View(KV.fs)
plot((KV.fs$tradems*KV.fs$demo), (1-KV.fs$sanctions), pch = 19,
     xlab = "Trade with MS*Democracy", ylab = "No sanctions",
     abline(0, 1))
textxy((KV.fs$tradems*KV.fs$demo), (1-KV.fs$sanctions), KV.fs$case, cex = 0.7, offset = 0.9, pos = 1)

