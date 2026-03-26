####################################################
## Begin ARIMA Analysis - determine c(p,d,q) ##
####################################################

#Now data is stationary enough for any kind of test
#AR I MA - Auto-Regressive Integrated Moving Average

plot.new()
frame()
par(mfcol=c(2,1), oma = c(1,1,2,1))

acf(diff(tsstationary_A_PaxScheduledDE), lag.max=length(tsstationary_A_PaxScheduledDE),
    main = "ACF - A Domestic/DE Scheduled Pax Annual Trend")

pacf(diff(tsstationary_A_PaxScheduledDE), lag.max=length(tsstationary_A_PaxScheduledDE),
     main = "Partial ACF - A Domestic/DE Scheduled Pax Annual Trend")

title(main=paste0("Group A - Domestic/DE Scheduled Pax (A_PaxScheduledDE)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_B_PaxScheduledDE), lag.max=length(tsstationary_B_PaxScheduledDE),
    main = "ACF - B Domestic/DE Scheduled Pax Annual Trend")

pacf(diff(tsstationary_B_PaxScheduledDE), lag.max=length(tsstationary_B_PaxScheduledDE),
     main = "Partial ACF - Domestic/DE Scheduled Pax Annual Trend")

title(main=paste0("Group B - Domestic/DE Scheduled Pax (B_PaxScheduledDE)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_C_PaxScheduledDE), lag.max=length(tsstationary_C_PaxScheduledDE),
    main = "ACF - C Domestic/DE Scheduled Pax Annual Trend")

pacf(diff(tsstationary_C_PaxScheduledDE), lag.max=length(tsstationary_C_PaxScheduledDE),
     main = "Partial ACF - C Domestic/DE Scheduled Pax Annual Trend")

title(main=paste0("Group C - Domestic/DE Scheduled Pax (C_PaxScheduledDE)"), 
      outer=TRUE, line=0)

#####

acf(diff(tsstationary_D_PaxScheduledDE), lag.max=length(tsstationary_D_PaxScheduledDE),
    main = "ACF - D Domestic/DE Scheduled Pax Annual Trend")

pacf(diff(tsstationary_D_PaxScheduledDE), lag.max=length(tsstationary_D_PaxScheduledDE),
     main = "Partial ACF - D Domestic/DE Scheduled Pax Annual Trend")

title(main=paste0("Group D - Domestic/DE Scheduled Pax (D_PaxScheduledDE)"), 
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

## Begin building fitted ARIMA Model ("fit_A_PaxScheduledDE")
# Note: We use arima(tsstationary_A_PaxScheduledDE), since this is the stablized model

# The "pred_X_PaxScheduledDE" prediction values are in Log Form
# Convert them to the original form using rounded e value (becomes more difficult with more than one diff.)
# important: use ts.plot instead of simple plot(x)

##### Final ARIMA Models fitting #####

fit_A_PaxScheduledDE <- stats::arima(tsstationary_A_PaxScheduledDE,c(1,1,2), seasonal = list(order=c(0,0,0), period=12))
pred_A_PaxScheduledDE = predict(fit_A_PaxScheduledDE,n.ahead = 12)
pred1_A_PaxScheduledDE <- round(2.718^pred_A_PaxScheduledDE$pred,0) # round to zero, as we are looking at Pax Numbers
plot.new()
frame()
par(mfcol=c(1,1), oma = c(1,1,1,1))
ts.plot(ts_A_PaxScheduledDE,pred1_A_PaxScheduledDE,log="y",lty = c(1,3),
        main = "12-Month Forecast A_PaxScheduledDE - ARIMA (1,1,2)",
        ylab = "Pax Numbers") 


fit_B_PaxScheduledDE <- stats::arima(tsstationary_B_PaxScheduledDE,c(1,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_B_PaxScheduledDE = predict(fit_B_PaxScheduledDE,n.ahead = 12)
pred1_B_PaxScheduledDE <- round(2.718^pred_B_PaxScheduledDE$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_B_PaxScheduledDE,pred1_B_PaxScheduledDE,log="y",lty = c(1,3),
        main = "12-Month Forecast B_PaxScheduledDE - ARIMA (1,1,1)",
        ylab = "Pax Numbers") 


fit_C_PaxScheduledDE <- stats::arima(tsstationary_C_PaxScheduledDE,c(1,1,1), seasonal = list(order=c(0,0,0), period=12))
pred_C_PaxScheduledDE = predict(fit_C_PaxScheduledDE,n.ahead = 12)
pred1_C_PaxScheduledDE <- round(2.718^pred_C_PaxScheduledDE$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_C_PaxScheduledDE,pred1_C_PaxScheduledDE,log="y",lty = c(1,3),
        main = "12-Month Forecast C_PaxScheduledDE - ARIMA (1,1,1)",
        ylab = "Pax Numbers") 


fit_D_PaxScheduledDE <- stats::arima(tsstationary_D_PaxScheduledDE,c(2,1,0), seasonal = list(order=c(0,0,0), period=12))
pred_D_PaxScheduledDE = predict(fit_D_PaxScheduledDE,n.ahead = 12)
pred1_D_PaxScheduledDE <- round(2.718^pred_D_PaxScheduledDE$pred,0) # round to zero, as we are looking at Pax Numbers
ts.plot(ts_D_PaxScheduledDE,pred1_D_PaxScheduledDE,log="y",lty = c(1,3),
        main = "12-Month Forecast D_PaxScheduledDE - ARIMA (2,1,0)",
        ylab = "Pax Numbers") 




