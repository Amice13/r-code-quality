####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_OtherCommercTraffic), lag.max=length(tsstationary_A_OtherCommercTraffic),
    main = "ACF - A Other Commercial Traffic Annual Trend")

pacf(diff(tsstationary_A_OtherCommercTraffic), lag.max=length(tsstationary_A_OtherCommercTraffic),
     main = "Partial ACF - A Other Commercial Traffic Annual Trend")

title(main=paste0("Group A - Other Commercial Traffic (A_OtherCommercTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_OtherCommercTraffic), lag.max=length(tsstationary_B_OtherCommercTraffic),
    main = "ACF - B Other Commercial Traffic Annual Trend")

pacf(diff(tsstationary_B_OtherCommercTraffic), lag.max=length(tsstationary_B_OtherCommercTraffic),
     main = "Partial ACF - Other Commercial Traffic Annual Trend")

title(main=paste0("Group B - Other Commercial Traffic (B_OtherCommercTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_OtherCommercTraffic), lag.max=length(tsstationary_C_OtherCommercTraffic),
    main = "ACF - C Other Commercial Traffic Annual Trend")

pacf(diff(tsstationary_C_OtherCommercTraffic), lag.max=length(tsstationary_C_OtherCommercTraffic),
     main = "Partial ACF - C Other Commercial Traffic Annual Trend")

title(main=paste0("Group C - Other Commercial Traffic (C_OtherCommercTraffic)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_OtherCommercTraffic), lag.max=length(tsstationary_D_OtherCommercTraffic),
    main = "ACF - D Other Commercial Traffic Annual Trend")

pacf(diff(tsstationary_D_OtherCommercTraffic), lag.max=length(tsstationary_D_OtherCommercTraffic),
     main = "Partial ACF - D Other Commercial Traffic Annual Trend")

title(main=paste0("Group D - Other Commercial Traffic (D_OtherCommercTraffic)"), 
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

## Begin building fitted ARIMA Model ("fit_A_OtherCommercTraffic")
# Note: We use arima(tsstationary_A_OtherCommercTraffic), since this is the stablized model

# The "pred_X_OtherCommercTraffic" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_OtherCommercTraffic <- stats::arima(tsstationary_A_OtherCommercTraffic,c(1,1,1), seasonal = list(order=c(0,0,1), period=12))
pred_A_OtherCommercTraffic = predict(fit_A_OtherCommercTraffic,n.ahead = 12)
pred1_A_OtherCommercTraffic <- round(2.718^pred_A_OtherCommercTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_OtherCommercTraffic,pred1_A_OtherCommercTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast A_OtherCommercTraffic - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_OtherCommercTraffic <- stats::arima(tsstationary_B_OtherCommercTraffic,c(0,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_B_OtherCommercTraffic = predict(fit_B_OtherCommercTraffic,n.ahead = 12)
pred1_B_OtherCommercTraffic <- round(2.718^pred_B_OtherCommercTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_OtherCommercTraffic,pred1_B_OtherCommercTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast B_OtherCommercTraffic - ARIMA (1,1,1)",
        ylab = "Pax Numbers") 


fit_C_OtherCommercTraffic <- stats::arima(tsstationary_C_OtherCommercTraffic,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_C_OtherCommercTraffic = predict(fit_C_OtherCommercTraffic,n.ahead = 12)
pred1_C_OtherCommercTraffic <- round(2.718^pred_C_OtherCommercTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_OtherCommercTraffic,pred1_C_OtherCommercTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast C_OtherCommercTraffic - ARIMA (2,1,0)",
        ylab = "Pax Numbers") 


fit_D_OtherCommercTraffic <- stats::arima(tsstationary_D_OtherCommercTraffic,c(2,1,2), seasonal = list(order=c(0,0,1), period=12))
pred_D_OtherCommercTraffic = predict(fit_D_OtherCommercTraffic,n.ahead = 12)
pred1_D_OtherCommercTraffic <- round(2.718^pred_D_OtherCommercTraffic$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_OtherCommercTraffic,pred1_D_OtherCommercTraffic,log="y",lty = c(1,3),
        main = "12-Month Forecast D_OtherCommercTraffic - ARIMA (2,1,2)",
        ylab = "Pax Numbers") 




