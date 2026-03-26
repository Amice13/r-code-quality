####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxScheduledTot), lag.max=length(tsstationary_A_PaxScheduledTot),
    main = "ACF - A Total Scheduled Pax Annual Trend")

pacf(diff(tsstationary_A_PaxScheduledTot), lag.max=length(tsstationary_A_PaxScheduledTot),
     main = "Partial ACF - A Total Scheduled Pax Annual Trend")

title(main=paste0("Group A - Total Scheduled Pax (A_PaxScheduledTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxScheduledTot), lag.max=length(tsstationary_B_PaxScheduledTot),
    main = "ACF - B Total Scheduled Pax Annual Trend")

pacf(diff(tsstationary_B_PaxScheduledTot), lag.max=length(tsstationary_B_PaxScheduledTot),
     main = "Partial ACF - Total Scheduled Pax Annual Trend")

title(main=paste0("Group B - Total Scheduled Pax (B_PaxScheduledTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxScheduledTot), lag.max=length(tsstationary_C_PaxScheduledTot),
    main = "ACF - C Total Scheduled Pax Annual Trend")

pacf(diff(tsstationary_C_PaxScheduledTot), lag.max=length(tsstationary_C_PaxScheduledTot),
     main = "Partial ACF - C Total Scheduled Pax Annual Trend")

title(main=paste0("Group C - Total Scheduled Pax (C_PaxScheduledTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxScheduledTot), lag.max=length(tsstationary_D_PaxScheduledTot),
    main = "ACF - D Total Scheduled Pax Annual Trend")

pacf(diff(tsstationary_D_PaxScheduledTot), lag.max=length(tsstationary_D_PaxScheduledTot),
     main = "Partial ACF - D Total Scheduled Pax Annual Trend")

title(main=paste0("Group D - Total Scheduled Pax (D_PaxScheduledTot)"), 
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

## Begin building fitted ARIMA Model ("fit_A_PaxScheduledTot")
# Note: We use arima(tsstationary_A_PaxScheduledTot), since this is the stablized model

# The "pred_X_PaxScheduledTot" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxScheduledTot <- stats::arima(tsstationary_A_PaxScheduledTot,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxScheduledTot = predict(fit_A_PaxScheduledTot,n.ahead = 12)
pred1_A_PaxScheduledTot <- round(2.718^pred_A_PaxScheduledTot$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxScheduledTot,pred1_A_PaxScheduledTot,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxScheduledTot - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_PaxScheduledTot <- stats::arima(tsstationary_B_PaxScheduledTot,c(2,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxScheduledTot = predict(fit_B_PaxScheduledTot,n.ahead = 12)
pred1_B_PaxScheduledTot <- round(2.718^pred_B_PaxScheduledTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxScheduledTot,pred1_B_PaxScheduledTot,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxScheduledTot - ARIMA (2,1,1)",
        ylab = "Pax Numbers") 


fit_C_PaxScheduledTot <- stats::arima(tsstationary_C_PaxScheduledTot,c(1,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxScheduledTot = predict(fit_C_PaxScheduledTot,n.ahead = 12)
pred1_C_PaxScheduledTot <- round(2.718^pred_C_PaxScheduledTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxScheduledTot,pred1_C_PaxScheduledTot,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxScheduledTot - ARIMA (4,1,1)",
        ylab = "Pax Numbers") 


fit_D_PaxScheduledTot <- stats::arima(tsstationary_D_PaxScheduledTot,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxScheduledTot = predict(fit_D_PaxScheduledTot,n.ahead = 12)
pred1_D_PaxScheduledTot <- round(2.718^pred_D_PaxScheduledTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxScheduledTot,pred1_D_PaxScheduledTot,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxScheduledTot - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 




