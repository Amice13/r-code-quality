
## Seasonal Fitting
## Produces Regression Table for Methods section which demonstrates why no seasonal testing was done

fit1_A_TotTraffic <- stats::arima(tsstationary_A_TotTraffic,c(1,1,2), seasonal = list(order=c(0,0,0), period=12)) # Final Model!
fit2_A_TotTraffic <- stats::arima(tsstationary_A_TotTraffic,c(1,1,2), seasonal = list(order=c(0,0,1), period=12))
fit3_A_TotTraffic <- stats::arima(tsstationary_A_TotTraffic,c(1,1,2), seasonal = list(order=c(0,1,0), period=12))
fit4_A_TotTraffic <- stats::arima(tsstationary_A_TotTraffic,c(1,1,2), seasonal = list(order=c(0,1,1), period=12))

library(stargazer)
summary(fit_A_TotTraffic)
stargazer(fit_A_TotTraffic)

dep.varnames <- c("seasonal (0,0,0)",
                  "seasonal (0,0,1)",
                  "seasonal (0,1,0)",
                  "seasonal (0,1,1)")

stargazer(fit1_A_TotTraffic,
          fit2_A_TotTraffic,
          fit3_A_TotTraffic,
          fit4_A_TotTraffic,
          type = "html", 
          out = "results/arima_seasonaltest_TotTraffic.html", 
          title = "ARIMA(1,1,2) of Group A Total Traffic with different seasonal adjustments",
          dep.var.labels.include = FALSE,
          column.labels = dep.varnames,
          model.numbers = FALSE,
          style = "aer")
