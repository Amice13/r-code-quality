#########################################################
## (1) Preparing X_TotTraffic Time Series for ARIMA ##
#########################################################



ts_A_TotTraffic <- trafficDE_simplified[, c("A_TotTraffic")] + 1
ts_B_TotTraffic <- trafficDE_simplified[, c("B_TotTraffic")] + 1
ts_C_TotTraffic <- trafficDE_simplified[, c("C_TotTraffic")] + 1
ts_D_TotTraffic <- trafficDE_simplified[, c("D_TotTraffic")] + 1

# Overview of ts_A_TotTraffic Data-structure
class(ts_A_TotTraffic)
class(ts_B_TotTraffic)
class(ts_C_TotTraffic)
class(ts_D_TotTraffic)

ts_A_TotTraffic <- ts(ts_A_TotTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_TotTraffic <- ts(ts_B_TotTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_TotTraffic <- ts(ts_C_TotTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_TotTraffic <- ts(ts_D_TotTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_A_TotTraffic Data-structure
class(ts_A_TotTraffic)
class(ts_B_TotTraffic)
class(ts_C_TotTraffic)
class(ts_D_TotTraffic)


#########################################################
## (2) Preparing X_PaxTransit Time Series for ARIMA ##
#########################################################


ts_A_PaxTransit <- trafficDE_simplified[, c("A_PAXTransit")] + 1
ts_B_PaxTransit <- trafficDE_simplified[, c("B_PAXTransit")] + 1
ts_C_PaxTransit <- trafficDE_simplified[, c("C_PAXTransit")] + 1
ts_D_PaxTransit <- trafficDE_simplified[, c("D_PAXTransit")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxTransit)
class(ts_B_PaxTransit)
class(ts_C_PaxTransit)
class(ts_D_PaxTransit)

ts_A_PaxTransit <- ts(ts_A_PaxTransit, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxTransit <- ts(ts_B_PaxTransit, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxTransit <- ts(ts_C_PaxTransit, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxTransit <- ts(ts_D_PaxTransit, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_PaxTransit)
class(ts_B_PaxTransit)
class(ts_C_PaxTransit)
class(ts_D_PaxTransit)


#########################################################
## (3) Preparing X_PaxTerminalTot Time Series for ARIMA ##
#########################################################


ts_A_PaxTerminalTot <- trafficDE_simplified[, c("A_PaxTerminalTot")] + 1
ts_B_PaxTerminalTot <- trafficDE_simplified[, c("B_PaxTerminalTot")] + 1
ts_C_PaxTerminalTot <- trafficDE_simplified[, c("C_PaxTerminalTot")] + 1
ts_D_PaxTerminalTot <- trafficDE_simplified[, c("D_PaxTerminalTot")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxTerminalTot)
class(ts_B_PaxTerminalTot)
class(ts_C_PaxTerminalTot)
class(ts_D_PaxTerminalTot)

ts_A_PaxTerminalTot <- ts(ts_A_PaxTerminalTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxTerminalTot <- ts(ts_B_PaxTerminalTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxTerminalTot <- ts(ts_C_PaxTerminalTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxTerminalTot <- ts(ts_D_PaxTerminalTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_PaxTerminalTot)
class(ts_B_PaxTerminalTot)
class(ts_C_PaxTerminalTot)
class(ts_D_PaxTerminalTot)


#########################################################
## (4) Preparing X_PaxScheduledTot Time Series for ARIMA ##
#########################################################


ts_A_PaxScheduledTot <- trafficDE_simplified[, c("A_PAXScheduledTot")] + 1
ts_B_PaxScheduledTot <- trafficDE_simplified[, c("B_PAXScheduledTot")] + 1
ts_C_PaxScheduledTot <- trafficDE_simplified[, c("C_PAXScheduledTot")] + 1
ts_D_PaxScheduledTot <- trafficDE_simplified[, c("D_PAXScheduledTot")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledTot)
class(ts_B_PaxScheduledTot)
class(ts_C_PaxScheduledTot)
class(ts_D_PaxScheduledTot)

ts_A_PaxScheduledTot <- ts(ts_A_PaxScheduledTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxScheduledTot <- ts(ts_B_PaxScheduledTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxScheduledTot <- ts(ts_C_PaxScheduledTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxScheduledTot <- ts(ts_D_PaxScheduledTot, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledTot)
class(ts_B_PaxScheduledTot)
class(ts_C_PaxScheduledTot)
class(ts_D_PaxScheduledTot)


#########################################################
## (5) Preparing X_OtherCommercTraffic Time Series for ARIMA ##
#########################################################


ts_A_OtherCommercTraffic <- trafficDE_simplified[, c("A_OtherCommercTraffic")] + 1
ts_B_OtherCommercTraffic <- trafficDE_simplified[, c("B_OtherCommercTraffic")] + 1
ts_C_OtherCommercTraffic <- trafficDE_simplified[, c("C_OtherCommercTraffic")] + 1
ts_D_OtherCommercTraffic <- trafficDE_simplified[, c("D_OtherCommercTraffic")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_OtherCommercTraffic)
class(ts_B_OtherCommercTraffic)
class(ts_C_OtherCommercTraffic)
class(ts_D_OtherCommercTraffic)

ts_A_OtherCommercTraffic <- ts(ts_A_OtherCommercTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_OtherCommercTraffic <- ts(ts_B_OtherCommercTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_OtherCommercTraffic <- ts(ts_C_OtherCommercTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_OtherCommercTraffic <- ts(ts_D_OtherCommercTraffic, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_OtherCommercTraffic)
class(ts_B_OtherCommercTraffic)
class(ts_C_OtherCommercTraffic)
class(ts_D_OtherCommercTraffic)


#########################################################
## (6) Preparing X_PaxScheduledDE Time Series for ARIMA ##
#########################################################


ts_A_PaxScheduledDE <- trafficDE_simplified[, c("A_PAXScheduledDE")] + 1
ts_B_PaxScheduledDE <- trafficDE_simplified[, c("B_PAXScheduledDE")] + 1
ts_C_PaxScheduledDE <- trafficDE_simplified[, c("C_PAXScheduledDE")] + 1
ts_D_PaxScheduledDE <- trafficDE_simplified[, c("D_PAXScheduledDE")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledDE)
class(ts_B_PaxScheduledDE)
class(ts_C_PaxScheduledDE)
class(ts_D_PaxScheduledDE)

ts_A_PaxScheduledDE <- ts(ts_A_PaxScheduledDE, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxScheduledDE <- ts(ts_B_PaxScheduledDE, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxScheduledDE <- ts(ts_C_PaxScheduledDE, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxScheduledDE <- ts(ts_D_PaxScheduledDE, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledDE)
class(ts_B_PaxScheduledDE)
class(ts_C_PaxScheduledDE)
class(ts_D_PaxScheduledDE)


#########################################################
## (7) Preparing X_PaxScheduledEurope Time Series for ARIMA ##
#########################################################


ts_A_PaxScheduledEurope <- trafficDE_simplified[, c("A_PAXScheduledEurope")] + 1
ts_B_PaxScheduledEurope <- trafficDE_simplified[, c("B_PAXScheduledEurope")] + 1
ts_C_PaxScheduledEurope <- trafficDE_simplified[, c("C_PAXScheduledEurope")] + 1
ts_D_PaxScheduledEurope <- trafficDE_simplified[, c("D_PAXScheduledEurope")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledEurope)
class(ts_B_PaxScheduledEurope)
class(ts_C_PaxScheduledEurope)
class(ts_D_PaxScheduledEurope)

ts_A_PaxScheduledEurope <- ts(ts_A_PaxScheduledEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxScheduledEurope <- ts(ts_B_PaxScheduledEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxScheduledEurope <- ts(ts_C_PaxScheduledEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxScheduledEurope <- ts(ts_D_PaxScheduledEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of new ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledEurope)
class(ts_B_PaxScheduledEurope)
class(ts_C_PaxScheduledEurope)
class(ts_D_PaxScheduledEurope)


#########################################################
## (8) Preparing X_PaxScheduledNonEurope Time Series for ARIMA ##
#########################################################


ts_A_PaxScheduledNonEurope <- trafficDE_simplified[, c("A_PAXScheduled NonEurope")] + 1
ts_B_PaxScheduledNonEurope <- trafficDE_simplified[, c("B_PAXScheduled NonEurope")] + 1
ts_C_PaxScheduledNonEurope <- trafficDE_simplified[, c("C_PAXScheduled NonEurope")] + 1
ts_D_PaxScheduledNonEurope <- trafficDE_simplified[, c("D_PAXScheduled NonEurope")] + 1

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledNonEurope)
class(ts_B_PaxScheduledNonEurope)
class(ts_C_PaxScheduledNonEurope)
class(ts_D_PaxScheduledNonEurope)

ts_A_PaxScheduledNonEurope <- ts(ts_A_PaxScheduledNonEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_B_PaxScheduledNonEurope <- ts(ts_B_PaxScheduledNonEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_C_PaxScheduledNonEurope <- ts(ts_C_PaxScheduledNonEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series
ts_D_PaxScheduledNonEurope <- ts(ts_D_PaxScheduledNonEurope, start=c(2016,1),end = c(2020,12),frequency = 12) # turn into time-series

# Overview of ts_X_PaxTransit Data-structure
class(ts_A_PaxScheduledNonEurope)
class(ts_B_PaxScheduledNonEurope)
class(ts_C_PaxScheduledNonEurope)
class(ts_D_PaxScheduledNonEurope)



