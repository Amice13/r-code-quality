####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxScheduledEurope), lag.max=length(tsstationary_A_PaxScheduledEurope),
    main = "ACF - A Cont./Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_A_PaxScheduledEurope), lag.max=length(tsstationary_A_PaxScheduledEurope),
     main = "Partial ACF - A Cont./Europe Scheduled Pax Annual Trend")

title(main=paste0("Group A - Cont./Europe Scheduled Pax (A_PaxScheduledEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxScheduledEurope), lag.max=length(tsstationary_B_PaxScheduledEurope),
    main = "ACF - B Cont./Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_B_PaxScheduledEurope), lag.max=length(tsstationary_B_PaxScheduledEurope),
     main = "Partial ACF - Cont./Europe Scheduled Pax Annual Trend")

title(main=paste0("Group B - Cont./Europe Scheduled Pax (B_PaxScheduledEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxScheduledEurope), lag.max=length(tsstationary_C_PaxScheduledEurope),
    main = "ACF - C Cont./Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_C_PaxScheduledEurope), lag.max=length(tsstationary_C_PaxScheduledEurope),
     main = "Partial ACF - C Cont./Europe Scheduled Pax Annual Trend")

title(main=paste0("Group C - Cont./Europe Scheduled Pax (C_PaxScheduledEurope)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxScheduledEurope), lag.max=length(tsstationary_D_PaxScheduledEurope),
    main = "ACF - D Cont./Europe Scheduled Pax Annual Trend")

pacf(diff(tsstationary_D_PaxScheduledEurope), lag.max=length(tsstationary_D_PaxScheduledEurope),
     main = "Partial ACF - D Cont./Europe Scheduled Pax Annual Trend")

title(main=paste0("Group D - Cont./Europe Scheduled Pax (D_PaxScheduledEurope)"), 
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
##  Fitting the ARIMA Model - c(p,d,q) = c(1,1,0) ##
####################################################

##  Predicting the Future one (1) year

## Begin building fitted ARIMA Model ("fit_A_PaxScheduledEurope")
# Note: We use arima(tsstationary_A_PaxScheduledEurope), since this is the stablized model

# The "pred_X_PaxScheduledEurope" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxScheduledEurope <- stats::arima(tsstationary_A_PaxScheduledEurope,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxScheduledEurope = predict(fit_A_PaxScheduledEurope,n.ahead = 12)
pred1_A_PaxScheduledEurope <- round(2.718^pred_A_PaxScheduledEurope$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxScheduledEurope,pred1_A_PaxScheduledEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxScheduledEurope - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_PaxScheduledEurope <- stats::arima(tsstationary_B_PaxScheduledEurope,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxScheduledEurope = predict(fit_B_PaxScheduledEurope,n.ahead = 12)
pred1_B_PaxScheduledEurope <- round(2.718^pred_B_PaxScheduledEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxScheduledEurope,pred1_B_PaxScheduledEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxScheduledEurope - ARIMA (4,1,0)",
        ylab = "Pax Numbers") 


fit_C_PaxScheduledEurope <- stats::arima(tsstationary_C_PaxScheduledEurope,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxScheduledEurope = predict(fit_C_PaxScheduledEurope,n.ahead = 12)
pred1_C_PaxScheduledEurope <- round(2.718^pred_C_PaxScheduledEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxScheduledEurope,pred1_C_PaxScheduledEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxScheduledEurope - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_D_PaxScheduledEurope <- stats::arima(tsstationary_D_PaxScheduledEurope,c(3,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxScheduledEurope = predict(fit_D_PaxScheduledEurope,n.ahead = 12)
pred1_D_PaxScheduledEurope <- round(2.718^pred_D_PaxScheduledEurope$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxScheduledEurope,pred1_D_PaxScheduledEurope,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxScheduledEurope - ARIMA (3,1,0)",
        ylab = "Pax Numbers") 




