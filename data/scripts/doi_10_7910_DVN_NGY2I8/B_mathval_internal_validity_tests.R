library(rio)
library(haven)
library(readr)


advtrafficde <- read_csv("data/import/advtrafficde.csv")

class(advtrafficde)
class(advtrafficde$Date)
summary(advtrafficde$Date)

advtrafficde$month <- seq(from = 1, to = 60, by = 1)

advtrafficde[1:60,c("Date","month")]
table(advtrafficde$Date, advtrafficde$month, useNA = "always")

## Turn Variable Date into Date-Format ##
advtrafficde$tsDate <- seq.Date(as.Date("2016-01-01"), as.Date("2020-12-01"), by = "month")


## Reduce Number of Variables by removing duplicates "Schengen" & "EU" across all groups
## Analysis will only focus on domestic (DE), Continental (Europe), and intercont (non-Europe) Traffic

# following Data-Frame omits EU and Schengen
trafficDE_simplified <- advtrafficde[, c("month", "tsDate","A_PaxTerminalTot","A_PAXScheduledTot","A_PAXScheduledDE",
                                         "A_PAXScheduledEurope","A_PAXScheduled NonEurope","A_OtherCommercTraffic","A_PAXTransit",
                                         "A_TotTraffic","B_PaxTerminalTot","B_PAXScheduledTot","B_PAXScheduledDE","B_PAXScheduledEurope",
                                         "B_PAXScheduled NonEurope","B_OtherCommercTraffic","B_PAXTransit","B_TotTraffic",
                                         "C_PaxTerminalTot","C_PAXScheduledTot","C_PAXScheduledDE","C_PAXScheduledEurope","C_PAXScheduled NonEurope",
                                         "C_OtherCommercTraffic","C_PAXTransit","C_TotTraffic","D_PaxTerminalTot",
                                         "D_PAXScheduledTot","D_PAXScheduledDE","D_PAXScheduledEurope","D_PAXScheduled NonEurope",
                                         "D_OtherCommercTraffic","D_PAXTransit","D_TotTraffic")]

####################################################
##### Test of Dataset's Mathematical Validity ######
####################################################

#### Test mathematic validity of all variables ####
mathval <- trafficDE_simplified[, c("month")]

## Test following statements for all x = A,B,C,D to ensure mathematic validity ##
#
# 1) Total Traffic x_TotTraffic should equal x_PaxTerminalTot + x_PaxTransit:
#         x_TotTraffic = x_PaxTerminalTot + x_PaxTransit
#
# 2) Total Terminal Traffic should equal sum of total scheduled and other Commercial Traffic: 
#         x_PaxTerminalTot = x_PAXScheduledTot + x_OtherCommercTraffic
#
# 3) Total Scheduled Traffic should equal sum of destinations: 
#         x_PAXScheduledTot = x_PAXScheduledDE + x_PAXScheduledEurope + x_PAXScheduled NonEurope
#
# If all three statements hold across all observations and all variables, then dataset is mathematically valid!
#
####################################################
#
#
## Validity Tests for x = B ##

# 1) B_TotTraffic = B_PaxTerminalTot + B_PaxTransit

mathval$B_TotTraffic <- trafficDE_simplified[, c("B_TotTraffic")]
mathval$B_TotTraffic == trafficDE_simplified$B_TotTraffic # Checks if all variables were copied correctly
mathval$test_B_TotTraffic <- trafficDE_simplified$B_PaxTerminalTot + trafficDE_simplified$B_PAXTransit
mathval$test_B_TotTraffic == mathval$B_TotTraffic # Holds if all observations 1-60 are true


# 2) B_PaxTerminalTot = B_PAXScheduledTot + B_OtherCommercTraffic

mathval$B_PaxTerminalTot <- trafficDE_simplified[, c("B_PaxTerminalTot")]
mathval$B_PaxTerminalTot == trafficDE_simplified$B_PaxTerminalTot # Checks if all variables were copied correctly
mathval$test_B_PaxTerminalTot <- trafficDE_simplified$B_PAXScheduledTot + trafficDE_simplified$B_OtherCommercTraffic
mathval$test_B_PaxTerminalTot == trafficDE_simplified$B_PaxTerminalTot # Holds if all observations 1-60 are true


# 3) B_PAXScheduledTot = B_PAXScheduledDE + B_PAXScheduledEurope + B_PAXScheduled NonEurope

mathval$B_PAXScheduledTot <- trafficDE_simplified[, c("B_PAXScheduledTot")]
mathval$B_PAXScheduledTot == trafficDE_simplified$B_PAXScheduledTot # Checks if all variables were copied correctly
mathval$test_B_PAXScheduledTot <- trafficDE_simplified$B_PAXScheduledDE + trafficDE_simplified$B_PAXScheduledEurope + trafficDE_simplified$"B_PAXScheduled NonEurope"
mathval$test_B_PAXScheduledTot == mathval$B_PAXScheduledTot # Holds if all observations 1-60 are true

####################################################

## Prerequisite Validity Test: Statements 1-3 must result in TRUE for all observations i = [1:60] and must be tested on each Variable X = A,B,C,D
## If above prerequisite holds, dataset is internally mathematically valid, and no errors were caused during data import and transformation
#
# ANALYSIS MAY CONTINUE ONLY IF PREREQUISITE IS MET

rm(mathval) # Delete Validation Dataset after tests are completed

####################################################
#### Conclusion of Mathematical Validity Tests  ####
####################################################