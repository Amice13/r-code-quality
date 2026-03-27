####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxTransit), lag.max=length(tsstationary_A_PaxTransit),
    main = "ACF - A Transit Pax Annual Trend")

pacf(diff(tsstationary_A_PaxTransit), lag.max=length(tsstationary_A_PaxTransit),
     main = "Partial ACF - A Transit Pax Annual Trend")

title(main=paste0("Group A - Transit Pax (A_PaxTransit)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxTransit), lag.max=length(tsstationary_B_PaxTransit),
    main = "ACF - B Transit Pax Annual Trend")

pacf(diff(tsstationary_B_PaxTransit), lag.max=length(tsstationary_B_PaxTransit),
     main = "Partial ACF - Transit Pax Annual Trend")

title(main=paste0("Group B - Transit Pax (B_PaxTransit)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxTransit), lag.max=length(tsstationary_C_PaxTransit),
    main = "ACF - C Transit Pax Annual Trend")

pacf(diff(tsstationary_C_PaxTransit), lag.max=length(tsstationary_C_PaxTransit),
     main = "Partial ACF - C Transit Pax Annual Trend")

title(main=paste0("Group C - Transit Pax (C_PaxTransit)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxTransit), lag.max=length(tsstationary_D_PaxTransit),
    main = "ACF - D Transit Pax Annual Trend")

pacf(diff(tsstationary_D_PaxTransit), lag.max=length(tsstationary_D_PaxTransit),
     main = "Partial ACF - D Transit Pax Annual Trend")

title(main=paste0("Group D - Transit Pax (D_PaxTransit)"), 
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

## Begin building fitted ARIMA Model ("fit_A_PaxTransit")
# Note: We use arima(tsstationary_A_PaxTransit), since this is the stablized model

# The "pred_X_PaxTransit" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxTransit <- stats::arima(tsstationary_A_PaxTransit,c(3,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxTransit = predict(fit_A_PaxTransit,n.ahead = 12)
pred1_A_PaxTransit <- round(2.718^pred_A_PaxTransit$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxTransit,pred1_A_PaxTransit,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxTransit - ARIMA (3,1,0)",
        ylab = "Pax Numbers") 


fit_B_PaxTransit <- stats::arima(tsstationary_B_PaxTransit,c(3,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxTransit = predict(fit_B_PaxTransit,n.ahead = 12)
pred1_B_PaxTransit <- round(2.718^pred_B_PaxTransit$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxTransit,pred1_B_PaxTransit,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxTransit - ARIMA (3,1,2)",
        ylab = "Pax Numbers") 


fit_C_PaxTransit <- stats::arima(tsstationary_C_PaxTransit,c(3,1,3), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxTransit = predict(fit_C_PaxTransit,n.ahead = 12)
pred1_C_PaxTransit <- round(2.718^pred_C_PaxTransit$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxTransit,pred1_C_PaxTransit,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxTransit - ARIMA (3,1,3)",
        ylab = "Pax Numbers") 


fit_D_PaxTransit <- stats::arima(tsstationary_D_PaxTransit,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxTransit = predict(fit_D_PaxTransit,n.ahead = 12)
pred1_D_PaxTransit <- round(2.718^pred_D_PaxTransit$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxTransit,pred1_D_PaxTransit,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxTransit - ARIMA (2,1,0)",
        ylab = "Pax Numbers") 




