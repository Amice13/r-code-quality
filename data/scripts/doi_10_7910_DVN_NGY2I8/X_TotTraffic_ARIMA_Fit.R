####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_TotTraffic), lag.max=length(tsstationary_A_TotTraffic),
    main = "ACF - A Total Pax Annual Trend")

pacf(diff(tsstationary_A_TotTraffic), lag.max=length(tsstationary_A_TotTraffic),
     main = "Partial ACF - A Total Pax Annual Trend")

title(main=paste0("Group A - Total Traffic incl. Transit (A_TotTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_TotTraffic), lag.max=length(tsstationary_B_TotTraffic),
    main = "ACF - B Total Pax Annual Trend")

pacf(diff(tsstationary_B_TotTraffic), lag.max=length(tsstationary_B_TotTraffic),
     main = "Partial ACF - Total Pax Annual Trend")

title(main=paste0("Group B - Total Traffic incl. Transit (B_TotTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_TotTraffic), lag.max=length(tsstationary_C_TotTraffic),
    main = "ACF - C Total Pax Annual Trend")

pacf(diff(tsstationary_C_TotTraffic), lag.max=length(tsstationary_C_TotTraffic),
     main = "Partial ACF - C Total Pax Annual Trend")

title(main=paste0("Group C - Total Traffic incl. Transit (C_TotTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_TotTraffic), lag.max=length(tsstationary_D_TotTraffic),
    main = "ACF - D Total Pax Annual Trend")

pacf(diff(tsstationary_D_TotTraffic), lag.max=length(tsstationary_D_TotTraffic),
     main = "Partial ACF - D Total Pax Annual Trend")

title(main=paste0("Group D - Total Traffic incl. Transit (D_TotTraffic)"), 
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

## Begin building fitted ARIMA Model ("fit_A_TotTraffic")
# Note: We use arima(tsstationary_A_TotTraffic), since this is the stablized model

# The "pred_X_TotTraffic" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_TotTraffic <- stats::arima(tsstationary_A_TotTraffic,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_A_TotTraffic = predict(fit_A_TotTraffic,n.ahead = 12)
pred1_A_TotTraffic <- round(2.718^pred_A_TotTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_TotTraffic,pred1_A_TotTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast A_TotTraffic - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_TotTraffic <- stats::arima(tsstationary_B_TotTraffic,c(0,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_B_TotTraffic = predict(fit_B_TotTraffic,n.ahead = 12)
pred1_B_TotTraffic <- round(2.718^pred_B_TotTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_TotTraffic,pred1_B_TotTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast B_TotTraffic - ARIMA (2,1,5)",
        ylab = "Pax Numbers") 


fit_C_TotTraffic <- stats::arima(tsstationary_C_TotTraffic,c(1,1,4), seasonal = list(order=c(0,0,0), period=12))
pred_C_TotTraffic = predict(fit_C_TotTraffic,n.ahead = 12)
pred1_C_TotTraffic <- round(2.718^pred_C_TotTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_TotTraffic,pred1_C_TotTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast C_TotTraffic - ARIMA (2,1,4)",
        ylab = "Pax Numbers") 


fit_D_TotTraffic <- stats::arima(tsstationary_D_TotTraffic,c(2,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_D_TotTraffic = predict(fit_D_TotTraffic,n.ahead = 12)
pred1_D_TotTraffic <- round(2.718^pred_D_TotTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_TotTraffic,pred1_D_TotTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast D_TotTraffic - ARIMA (3,1,5)",
        ylab = "Pax Numbers") 




