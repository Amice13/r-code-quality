####################################################
####### Making Data Stationary (VAR & Mean) ########
####################################################

# Homogenize variance using log(x)

plot.new()
frame()
par(mfcol=c(2,2), oma = c(2,2,4,2)) # outer margin area (OMA) = c(bottom, left, top, right

plot(log(ts_A_PaxTransit),
     xlab = "Time (Year)",
     ylab = "log(ts_A_PaxTransit)",
     main = "Group A")

plot(log(ts_C_PaxTransit),
     xlab = "Time (Year)",
     ylab = "log(ts_C_PaxTransit)",
     main = "Group C")

plot(log(ts_B_PaxTransit),
     xlab = "Time (Year)",
     ylab = "log(ts_A_PaxTransit)",
     main = "Group B")

plot(log(ts_D_PaxTransit),
     xlab = "Time (Year)",
     ylab = "log(ts_A_PaxTransit)",
     main = "Group D")

title(main=paste0("Total Transit Passengers (x_PaxTransit)\n",
                  "Logarithm - Variance Homogenized"), 
      outer=TRUE, line=0)

#############################################################

# Defining number of Differentiations

# Homogenize Mean using diff(x)
# Note: How many differences for most accurate Model?
# => Choose Differentiation with lowest Standard Error [{Quelle einfügen?}]

sd(diff(log(ts_A_PaxTransit), differences=1))
sd(diff(log(ts_A_PaxTransit), differences=2))
sd(diff(log(ts_A_PaxTransit), differences=3))

## => Differentiate once (stdev = 0.4987372)

sd(diff(log(ts_B_PaxTransit), differences=1))
sd(diff(log(ts_B_PaxTransit), differences=2))
sd(diff(log(ts_B_PaxTransit), differences=3))

## => Differentiate once (stdev = 0.8463416)

sd(diff(log(ts_C_PaxTransit), differences=1))
sd(diff(log(ts_C_PaxTransit), differences=2))
sd(diff(log(ts_C_PaxTransit), differences=3))

## => Differentiate once (stdev = 0.5607596)

sd(diff(log(ts_D_PaxTransit), differences=1))
sd(diff(log(ts_D_PaxTransit), differences=2))
sd(diff(log(ts_D_PaxTransit), differences=3))

## => Differentiate once (stdev = 1.002619)

#############################################################

## Create new Data Object with stationarity!

## Residuals Test shows that the Data is not yet homogenized!

# Unit Root KPSS Stationarity Test (Package fUnitRoot)
my_urkpssTest <- function (x, type, lags, use.lag, doplot) {
  x <- as.vector(x)
  urca <- urca::ur.kpss(x, type = type[1], lags = lags[1], use.lag = use.lag)
  output = capture.output(urca::summary(urca))[-(1:4)]
  output = output[-length(output)]
  for (i in 1:length(output)) output[i] = paste(" ", output[i])
  ans = list(name = "ur.kpss", test = urca, output = output)
  if (doplot) 
    plot(urca)
  new("fHTEST", call = match.call(), data = list(x = x), 
      test = ans, title = "KPSS Unit Root Test", description = description())
}

my_urkpssTest(ts_A_PaxTransit, type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group A - Total Transit Passengers [A_PaxTransit]\n",
                  "KPSS Unit Root Test Result: No Stationarity"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(ts_B_PaxTransit, type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group B - Total Transit Passengers [B_PaxTransit]\n",
                  "KPSS Unit Root Test Result: No Stationarity"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(ts_C_PaxTransit, type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group C - Total Transit Passengers [C_PaxTransit]\n",
                  "KPSS Unit Root Test Result: No Stationarity"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(ts_D_PaxTransit, type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group D - Total Transit Passengers [D_PaxTransit]\n",
                  "KPSS Unit Root Test Result: No Stationarity"), 
      outer=TRUE, line=0)

########################

## Result of KPSS Unit Root Test: ACF and PACF of Residuals are not patternless and do not tend towards zero!

## Create new Data Object with variance stationarity (log)
# Do not diff(x) here, this comes later in ARIMA

tsstationary_A_PaxTransit = log(ts_A_PaxTransit) 
tsstationary_B_PaxTransit = log(ts_B_PaxTransit) 
tsstationary_C_PaxTransit = log(ts_C_PaxTransit) 
tsstationary_D_PaxTransit = log(ts_D_PaxTransit) 

# Plot Homogenized Time Series
plot.new()
frame()
par(mfcol=c(2,2), oma = c(2,2,4,2)) # outer margin area (OMA) = c(bottom, left, top, right
plot(diff(tsstationary_A_PaxTransit, difference=1),
     xlab = "Time (Year)",
     ylab = "diff(log(ts_A_PaxTransit)",
     main = "Airport Group A")

plot(diff(tsstationary_C_PaxTransit, difference=1),
     xlab = "Time (Year)",
     ylab = "diff(log(ts_C_PaxTransit)",
     main = "Airport Group C")

plot(diff(tsstationary_B_PaxTransit, difference=1),
     xlab = "Time (Year)",
     ylab = "diff(log(ts_B_PaxTransit)",
     main = "Airport Group B")

plot(diff(tsstationary_D_PaxTransit, difference=1),
     xlab = "Time (Year)",
     ylab = "diff(log(ts_D_PaxTransit)",
     main = "Airport Group D")

title(main=paste0("Total Transit Passengers (x_PaxTransit)\n",
                  "Variance & Mean Homogenized"), 
      outer=TRUE, line=0)



## Check: All Values return true? If not: Mistake was made while transcribing into Log or Diff!

diff(tsstationary_A_PaxTransit, difference = 1) == diff(log(ts_A_PaxTransit))
diff(tsstationary_B_PaxTransit, difference = 1) == diff(log(ts_B_PaxTransit))
diff(tsstationary_C_PaxTransit, difference = 1) == diff(log(ts_C_PaxTransit))
diff(tsstationary_D_PaxTransit, difference = 1) == diff(log(ts_D_PaxTransit))

# Repeat Residuals Test: Data is now homogenized!

my_urkpssTest(diff(tsstationary_A_PaxTransit, difference=1), type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group A - Total Transit Passengers [A_PaxTransit]\n",
                  "KPSS Unit Root Test Result: Timeseries is Stationary"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(diff(tsstationary_B_PaxTransit, difference=1), type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group B - Total Transit Passengers [B_PaxTransit]\n",
                  "KPSS Unit Root Test Result: Timeseries is Stationary"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(diff(tsstationary_C_PaxTransit, difference=1), type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group C - Total Transit Passengers [C_PaxTransit]\n",
                  "KPSS Unit Root Test Result: Timeseries is Stationary"), 
      outer=TRUE, line=0)

########################

my_urkpssTest(diff(tsstationary_D_PaxTransit, difference=1), type = c("tau"), lags = c("long"),
              use.lag = NULL, doplot = TRUE)

title(main=paste0("Group D - Total Transit Passengers [D_PaxTransit]\n",
                  "KPSS Unit Root Test Result: Timeseries is Stationary"), 
      outer=TRUE, line=0)

########################

####################################################
# Data reprocessing complete#
####################################################