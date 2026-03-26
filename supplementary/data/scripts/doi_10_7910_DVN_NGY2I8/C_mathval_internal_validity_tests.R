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

## Validity Tests for x = C ##

# 1) C_TotTraffic = C_PaxTerminalTot + C_PaxTransit

mathval$C_TotTraffic <- trafficDE_simplified[, c("C_TotTraffic")]
mathval$C_TotTraffic == trafficDE_simplified$C_TotTraffic # Checks if all variables were copied correctly
mathval$test_C_TotTraffic <- trafficDE_simplified$C_PaxTerminalTot + trafficDE_simplified$C_PAXTransit
mathval$test_C_TotTraffic == mathval$C_TotTraffic # Holds if all observations 1-60 are true


# 2) C_PaxTerminalTot = C_PAXScheduledTot + C_OtherCommercTraffic

mathval$C_PaxTerminalTot <- trafficDE_simplified[, c("C_PaxTerminalTot")]
mathval$C_PaxTerminalTot == trafficDE_simplified$C_PaxTerminalTot # Checks if all variables were copied correctly
mathval$test_C_PaxTerminalTot <- trafficDE_simplified$C_PAXScheduledTot + trafficDE_simplified$C_OtherCommercTraffic
mathval$test_C_PaxTerminalTot == trafficDE_simplified$C_PaxTerminalTot # Holds if all observations 1-60 are true


# 3) C_PAXScheduledTot = C_PAXScheduledDE + C_PAXScheduledEurope + C_PAXScheduled NonEurope

mathval$C_PAXScheduledTot <- trafficDE_simplified[, c("C_PAXScheduledTot")]
mathval$C_PAXScheduledTot == trafficDE_simplified$C_PAXScheduledTot # Checks if all variables were copied correctly
mathval$test_C_PAXScheduledTot <- trafficDE_simplified$C_PAXScheduledDE + trafficDE_simplified$C_PAXScheduledEurope + trafficDE_simplified$"C_PAXScheduled NonEurope"
mathval$test_C_PAXScheduledTot == mathval$C_PAXScheduledTot # Holds if all observations 1-60 are true

####################################################

## Prerequisite Validity Test: Statements 1-3 must result in TRUE for all observations i = [1:60] and must be tested on each Variable X = A,B,C,D
## If above prerequisite holds, dataset is internally mathematically valid, and no errors were caused during data import and transformation
#
# ANALYSIS MAY CONTINUE ONLY IF PREREQUISITE IS MET

rm(mathval) # Delete Validation Dataset after tests are completed

####################################################
#### Conclusion of Mathematical Validity Tests  ####
####################################################