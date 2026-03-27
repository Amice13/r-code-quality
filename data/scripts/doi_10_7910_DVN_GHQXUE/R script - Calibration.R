## Set WD and load packages/files

rm(list = ls())

library(SetMethods)

## Import data 
dataraw <- read.csv("Raw scores for resources and salience.csv", header = T, row.names = 1)
head(dataraw)

##### Calibration of resources #####
RES <- calibrate(dataraw$RESraw, type = "fuzzy", method = "direct",
                     thresholds = c(600000, 1000000, 5000000))
RES <- round(RES, digits = 2)
RES

# This corresponds to RES in the file "Complete calibrated data.csv"

# XY plot
plot(dataraw$RESraw, RES)
abline(h=0.5)

#### Calibration of expert opinion (politicisation) #####
expert <- calibrate(dataraw$EXPERT.SAL, type = "fuzzy", 
                    thresholds = c(2, 3, 4))
expert <- round(SALcal, digits = 2)
expert

# This corresponds to the column 'Expert opinion' in the table of complete indiator measurements in the appendix  

# XY plot
plot(dataraw$EXPERT.SAL, expert)
abline(h = 0.5)

##### Calibration of media salience #####
# Load data files # 
SALUKraw <- read.csv("Raw scores for media salience - UK.csv", header = T, row.names = 1)
head(SALUKraw)

SALITraw <- read.csv("Raw scores for media salience - IT.csv", header = T, row.names = 1)
head(SALITraw)

# Calibrate UK
SALUKcal <- calibrate(SALUKraw$SALRAW, method = "direct", 
                   type = "fuzzy",
                   thresholds = c(0, 0.01, 0.1))
SALUKcal <- round(SALUKcal, digits = 2)
head(SALUKcal)

# This corresponds to the column 'Salience' in the table of complete indiator measurements in the appendix  

# XY plot
plot(SALUKraw$SALRAW, SALUKcal)
abline(h = 0.5)

# Calibrate IT
SALITcal <- calibrate(SALITraw$SALRAW, method = "direct", 
                   type = "fuzzy", 
                   thresholds = c(0, 0.08, 0.2))
SALITcal <- round(SALITcal, digits = 2)
head(SALITcal)

# This corresponds to the column 'Salience' in the table of complete indiator measurements in the appendix  

# XY plot
plot(SALITraw$SALRAW, SALITcal)
abline(h = 0.5)