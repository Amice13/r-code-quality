### Calibration Robustness Checks for 
### Exploring the Conditions for Youth Representation: 
### A Qualitative Comparative Analysis of Party Parliamentary Groups

setwd("C:/Users/fe300/Desktop/Research Article - QCA Youth organizations/Revise 1")


library(QCA)
library(SetMethods)
library(tidyverse)


QCA.Data.calibrated <- read_csv("Data QCA - calibrated - August2023.csv")


### Condition and Outcome variation tests
## BASELINE -- ARI35 coding TH at 0.6
QCA.Data.calibrated$qca.outcome.ARI35 <- calibrate(QCA.Data.calibrated$ARI35, type = "fuzzy", method = "direct", c(0.05, 0.6, 0.95), logistic = TRUE)
QCA.Data.calibrated$qca.outcome.ARI35 <- round(QCA.Data.calibrated$qca.outcome.ARI35, 3)

## moving point of ambiguity to the negative direction -0.02 = 0.58
QCA.Data.calibrated$qca.outcome.ARI35 <- calibrate(QCA.Data.calibrated$ARI35, type = "fuzzy", method = "direct", c(0.05, 0.58, 0.95), logistic = TRUE)
QCA.Data.calibrated$qca.outcome.ARI35 <- round(QCA.Data.calibrated$qca.outcome.ARI35, 3)
## lower bound 0.58

## moving point of ambiguity to the negative direction +0.04 = 0.64
QCA.Data.calibrated$qca.outcome.ARI35 <- calibrate(QCA.Data.calibrated$ARI35, type = "fuzzy", method = "direct", c(0.05, 0.64, 0.95), logistic = TRUE)
QCA.Data.calibrated$qca.outcome.ARI35 <- round(QCA.Data.calibrated$qca.outcome.ARI35, 3)
## lower bound 0.64

### Conditions
###

### C1 - Youth index - crisp set data - no calibration applied



### C2 - GDP per capita - BASELINE 
QCA.Data.calibrated$qca.gdpcap <- calibrate(QCA.Data.calibrated$Gdp.per.cap, type = "fuzzy", method = "direct", c(25000, 36000, 55000), logistic = TRUE)
QCA.Data.calibrated$qca.gdpcap <- round(QCA.Data.calibrated$qca.gdpcap, 2)

## moving point of ambiguity to the negative direction -4000 = 32000
QCA.Data.calibrated$qca.gdpcap <- calibrate(QCA.Data.calibrated$Gdp.per.cap, type = "fuzzy", method = "direct", c(25000, 32000, 55000), logistic = TRUE)
QCA.Data.calibrated$qca.gdpcap <- round(QCA.Data.calibrated$qca.gdpcap, 2)

## moving point of ambiguity to the negative direction +1000 = 37000
QCA.Data.calibrated$qca.gdpcap <- calibrate(QCA.Data.calibrated$Gdp.per.cap, type = "fuzzy", method = "direct", c(25000, 37000, 55000), logistic = TRUE)
QCA.Data.calibrated$qca.gdpcap <- round(QCA.Data.calibrated$qca.gdpcap, 2)



### C3 - Elec System - Crisp Set Data - no calibration applied 



### C4 - Decentralization - BASELINE
QCA.Data.calibrated$qca.decentral <- calibrate(QCA.Data.calibrated$Decentralization, type = "fuzzy", method = "direct", c(0.1, 0.48, 0.9), logistic = TRUE)
QCA.Data.calibrated$qca.decentral <- round(QCA.Data.calibrated$qca.decentral, 2)

### moving point of ambiguity to the negative direction -0.01 = 0.47
QCA.Data.calibrated$qca.decentral <- calibrate(QCA.Data.calibrated$Decentralization, type = "fuzzy", method = "direct", c(0.1, 0.47, 0.9), logistic = TRUE)
QCA.Data.calibrated$qca.decentral <- round(QCA.Data.calibrated$qca.decentral, 2)

### moving point of ambiguity to the positive direction +0.22 = 0.70
QCA.Data.calibrated$qca.decentral <- calibrate(QCA.Data.calibrated$Decentralization, type = "fuzzy", method = "direct", c(0.1, 0.70, 0.9), logistic = TRUE)
QCA.Data.calibrated$qca.decentral <- round(QCA.Data.calibrated$qca.decentral, 2)



### C5 - Progressiveness - BASELINE
QCA.Data.calibrated$qca.progressive <- calibrate(QCA.Data.calibrated$Manifesto.progcons, type = "fuzzy", method = "direct", c(10, -8, -20), logistic = TRUE)
QCA.Data.calibrated$qca.progressive <- round(QCA.Data.calibrated$qca.progressive, 3)

### moving point of ambiguity to the negative direction -3 = -11
QCA.Data.calibrated$qca.progressive <- calibrate(QCA.Data.calibrated$Manifesto.progcons, type = "fuzzy", method = "direct", c(10, -11, -20), logistic = TRUE)
QCA.Data.calibrated$qca.progressive <- round(QCA.Data.calibrated$qca.progressive, 3)

### moving point of ambiguity to the positive direction +2 = -6
QCA.Data.calibrated$qca.progressive <- calibrate(QCA.Data.calibrated$Manifesto.progcons, type = "fuzzy", method = "direct", c(10, -6, -20), logistic = TRUE)
QCA.Data.calibrated$qca.progressive <- round(QCA.Data.calibrated$qca.progressive, 3)



### C6 - Young Party - BASELINE
QCA.Data.calibrated$qca.youngparty <- calibrate(QCA.Data.calibrated$party.age, type = "fuzzy", method = "direct", c(50, 19.9, 3), logistic = TRUE)
QCA.Data.calibrated$qca.youngparty <- round(QCA.Data.calibrated$qca.youngparty, 3)

### moving point of ambiguity to the negative direction -10 = 9.9
QCA.Data.calibrated$qca.youngparty <- calibrate(QCA.Data.calibrated$party.age, type = "fuzzy", method = "direct", c(50, 9.9, 3), logistic = TRUE)
QCA.Data.calibrated$qca.youngparty <- round(QCA.Data.calibrated$qca.youngparty, 3)

### moving point of ambiguity to the positive direction +1 = 20.9
QCA.Data.calibrated$qca.youngparty <- calibrate(QCA.Data.calibrated$party.age, type = "fuzzy", method = "direct", c(50, 20.9, 3), logistic = TRUE)
QCA.Data.calibrated$qca.youngparty <- round(QCA.Data.calibrated$qca.youngparty, 3)


##################
##################

### QCA Data Pipeline (formatting)
QCA.Data.calibrated -> QCA.Data

QCA.Data <- as.data.frame(QCA.Data)
rownames(QCA.Data) <- do.call(paste,c(QCA.Data[c("ISO3","party.abb")],sep="-"))
# QCA.Data.calibrated$name_vdem -> rownames(QCA.Data)

#drop nas in decentral
drop_na(QCA.Data, qca.decentral) -> QCA.Data

###


TruthTable.pars <- truthTable(QCA.Data[, c(33,34,35,36,39,41,43)], outcome = "qca.outcome.ARI35", conditions = "qca.youthindex, qca.gdpcap, qca.elecsystem, qca.decentral, qca.progressive, qca.youngparty",
                              complete = TRUE, show.cases = TRUE, 
                              sort.by = c("incl", "n"), n.cut = 1, incl.cut = 0.8)
## show truth table
TruthTable.pars


solution.ps <- minimize(TruthTable.pars, outcome = "qca.outcome.ARI35", include = "1, ?",
                        row.dom = TRUE, details = TRUE, show.cases = TRUE, use.tilde = TRUE,
                        method = "CCubes")

## parsimonious solution (Table XX) 
solution.ps

