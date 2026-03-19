####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxScheduledNonEurope), lag.max=length(tsstationary_A_PaxScheduledNonEurope),
    main = "ACF - A Intercont./Non-Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_A_PaxScheduledNonEurope), lag.max=length(tsstationary_A_PaxScheduledNonEurope),
     main = "Partial ACF - A Intercont./Non-Europe Scheduled Pax Annual Trend")

title(main=paste0("Group A - Intercont./Non-Europe Scheduled Pax (A_PaxScheduledNonEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxScheduledNonEurope), lag.max=length(tsstationary_B_PaxScheduledNonEurope),
    main = "ACF - B Intercont./Non-Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_B_PaxScheduledNonEurope), lag.max=length(tsstationary_B_PaxScheduledNonEurope),
     main = "Partial ACF - Intercont./Non-Europe Scheduled Pax Annual Trend")

title(main=paste0("Group B - Intercont./Non-Europe Scheduled Pax (B_PaxScheduledNonEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxScheduledNonEurope), lag.max=length(tsstationary_C_PaxScheduledNonEurope),
    main = "ACF - C Intercont./Non-Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_C_PaxScheduledNonEurope), lag.max=length(tsstationary_C_PaxScheduledNonEurope),
     main = "Partial ACF - C Intercont./Non-Europe Scheduled Pax Annual Trend")

title(main=paste0("Group C - Intercont./Non-Europe Scheduled Pax (C_PaxScheduledNonEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxScheduledNonEurope), lag.max=length(tsstationary_D_PaxScheduledNonEurope),
    main = "ACF - D Intercont./Non-Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_D_PaxScheduledNonEurope), lag.max=length(tsstationary_D_PaxScheduledNonEurope),
     main = "Partial ACF - D Intercont./Non-Europe Scheduled Pax Annual Trend")

title(main=paste0("Group D - Intercont./Non-Europe Scheduled Pax (D_PaxScheduledNonEurope)"), 
      outer=TRUE, line=0)


## determining q value
# Count lines (Betrag, keine negativwerte!) in ACF Plot from left to right (left most line is "0")
# disqualify lines which lie outside of blue boundaries
# take the first line from the left which is in the boundaries and subtract 1
# this gives the q value
# in other words: the line before the line which qualifies first will be q value

## determining p value
# same procedure as q but with pacf function

## determining d value
# number of differentiations

## THIS IS THE MOST CRUCIAL PART IN ARIMA MODEL! 
# ONLY PART WHERE HUMAN JUDGEMENT IS NECESSARY!


####################################################
##  Fitting the ARIMA Model - c(p,d,q) ##
####################################################

##  Predicting the Future one (1) year

## Begin building fitted ARIMA Model ("fit_A_PaxScheduledNonEurope")
# Note: We use arima(tsstationary_A_PaxScheduledNonEurope), since this is the stablized model

# The "pred_X_PaxScheduledNonEurope" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxScheduledNonEurope <- stats::arima(tsstationary_A_PaxScheduledNonEurope,c(1,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxScheduledNonEurope = predict(fit_A_PaxScheduledNonEurope,n.ahead = 12)
pred1_A_PaxScheduledNonEurope <- round(2.718^pred_A_PaxScheduledNonEurope$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxScheduledNonEurope,pred1_A_PaxScheduledNonEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxScheduledNonEurope - ARIMA (1,1,0)",
        ylab = "Pax Numbers") 


fit_B_PaxScheduledNonEurope <- stats::arima(tsstationary_B_PaxScheduledNonEurope,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxScheduledNonEurope = predict(fit_B_PaxScheduledNonEurope,n.ahead = 12)
pred1_B_PaxScheduledNonEurope <- round(2.718^pred_B_PaxScheduledNonEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxScheduledNonEurope,pred1_B_PaxScheduledNonEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxScheduledNonEurope - ARIMA (2,1,0)",
        ylab = "Pax Numbers") 


fit_C_PaxScheduledNonEurope <- stats::arima(tsstationary_C_PaxScheduledNonEurope,c(0,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxScheduledNonEurope = predict(fit_C_PaxScheduledNonEurope,n.ahead = 12)
pred1_C_PaxScheduledNonEurope <- round(2.718^pred_C_PaxScheduledNonEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxScheduledNonEurope,pred1_C_PaxScheduledNonEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxScheduledNonEurope - ARIMA (0,1,2)",
        ylab = "Pax Numbers") 


fit_D_PaxScheduledNonEurope <- stats::arima(tsstationary_D_PaxScheduledNonEurope,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxScheduledNonEurope = predict(fit_D_PaxScheduledNonEurope,n.ahead = 12)
pred1_D_PaxScheduledNonEurope <- round(2.718^pred_D_PaxScheduledNonEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxScheduledNonEurope,pred1_D_PaxScheduledNonEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxScheduledNonEurope - ARIMA (2,1,0)",
        ylab = "Pax Numbers") 




