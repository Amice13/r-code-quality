####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxTerminalTot), lag.max=length(tsstationary_A_PaxTerminalTot),
    main = "ACF - A Total Terminal Pax Annual Trend")

pacf(diff(tsstationary_A_PaxTerminalTot), lag.max=length(tsstationary_A_PaxTerminalTot),
     main = "Partial ACF - A Total Terminal Pax Annual Trend")

title(main=paste0("Group A - Total Terminal Pax (A_PaxTerminalTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxTerminalTot), lag.max=length(tsstationary_B_PaxTerminalTot),
    main = "ACF - B Total Terminal Pax Annual Trend")

pacf(diff(tsstationary_B_PaxTerminalTot), lag.max=length(tsstationary_B_PaxTerminalTot),
     main = "Partial ACF - Total Terminal Pax Annual Trend")

title(main=paste0("Group B - Total Terminal Pax (B_PaxTerminalTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxTerminalTot), lag.max=length(tsstationary_C_PaxTerminalTot),
    main = "ACF - C Total Terminal Pax Annual Trend")

pacf(diff(tsstationary_C_PaxTerminalTot), lag.max=length(tsstationary_C_PaxTerminalTot),
     main = "Partial ACF - C Total Terminal Pax Annual Trend")

title(main=paste0("Group C - Total Terminal Pax (C_PaxTerminalTot)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxTerminalTot), lag.max=length(tsstationary_D_PaxTerminalTot),
    main = "ACF - D Total Terminal Pax Annual Trend")

pacf(diff(tsstationary_D_PaxTerminalTot), lag.max=length(tsstationary_D_PaxTerminalTot),
     main = "Partial ACF - D Total Terminal Pax Annual Trend")

title(main=paste0("Group D - Total Terminal Pax (D_PaxTerminalTot)"), 
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

## Begin building fitted ARIMA Model ("fit_A_PaxTerminalTot")
# Note: We use arima(tsstationary_A_PaxTerminalTot), since this is the stablized model

# The "pred_X_PaxTerminalTot" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxTerminalTot <- stats::arima(tsstationary_A_PaxTerminalTot,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxTerminalTot = predict(fit_A_PaxTerminalTot,n.ahead = 12)
pred1_A_PaxTerminalTot <- round(2.718^pred_A_PaxTerminalTot$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxTerminalTot,pred1_A_PaxTerminalTot,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxTerminalTot - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_PaxTerminalTot <- stats::arima(tsstationary_B_PaxTerminalTot,c(2,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxTerminalTot = predict(fit_B_PaxTerminalTot,n.ahead = 12)
pred1_B_PaxTerminalTot <- round(2.718^pred_B_PaxTerminalTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxTerminalTot,pred1_B_PaxTerminalTot,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxTerminalTot - ARIMA (2,1,1)",
        ylab = "Pax Numbers") 


fit_C_PaxTerminalTot <- stats::arima(tsstationary_C_PaxTerminalTot,c(2,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxTerminalTot = predict(fit_C_PaxTerminalTot,n.ahead = 12)
pred1_C_PaxTerminalTot <- round(2.718^pred_C_PaxTerminalTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxTerminalTot,pred1_C_PaxTerminalTot,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxTerminalTot - ARIMA (4,1,1)",
        ylab = "Pax Numbers") 


fit_D_PaxTerminalTot <- stats::arima(tsstationary_D_PaxTerminalTot,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxTerminalTot = predict(fit_D_PaxTerminalTot,n.ahead = 12)
pred1_D_PaxTerminalTot <- round(2.718^pred_D_PaxTerminalTot$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxTerminalTot,pred1_D_PaxTerminalTot,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxTerminalTot - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 




