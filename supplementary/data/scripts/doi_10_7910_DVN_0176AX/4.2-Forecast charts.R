###############################################
#model used for daily accounted crimes
###############################################
#clear all variables in workspace
rm(list=ls()) 

###############################################
#read DB in a dataset named 'bd'

#csv from Macbook
bd<- read.csv(header = TRUE, sep = ",", "/Users/fred/Desktop/BH/2-Entry.csv")

#xlsx from Macbook
#bd<- read_xlsx("/Users/fred/Downloads/R Studio/ARIMA.xlsx")

#csv from Windows
#bd<- read.csv("C:/Users/p3-chefe/Desktop/BH/2-Entry.csv")



###############################################
#CHOOSE CRIME
#bd <- bd[,-(3:10)]   #ROBBERY
#bd <- bd[,-(2)]   #THEFT
#bd <- bd[,-(2:3)]   #HOMICIDE
#bd <- bd[,-(2:4)]   #ROBBERY walker
#bd <- bd[,-(2:5)]   #ROBBERY vehicles
#bd <- bd[,-(2:6)]   #ROBBERY residential
#bd <- bd[,-(2:7)]   #THEFT walker
#bd <- bd[,-(2:8)]   #THEFT vehicles
bd <- bd[,-(2:9)]   #THEFT residential

###############################################
bd <- bd[,-(3:11)]



###############################################
#create 'ts' value with time series data
#frequency can be days(365), weeks(52), months(12), quarters(4), etc
#start=c shows YEAR, MONTH
ts<-ts(bd[,2],frequency=52,start=c(2016, 1))


#Quantile-Comparison Plot
library(car)
qqPlot(bd[,2], xlab = "Normal Quantiles", main = "Quantile-Comparison Plot", ylab = "Empirical Quantiles")


###############################################
#Preliminary Time Series analysis
###############################################
#chart time series 
library(ggplot2)
library(forecast)

autoplot(ts) +
  ggtitle("Weekly Crimes - BH - 2016-2018") +
  ylab("Crimes") +
  xlab("Epidemiological Week")
#high spikes or low trough usually indicate outliers 


library(ggpubr)
ggdensity(ts,
          main = "Density plot of crimes",
          xlab = "Weekly crime rate", ylab = "Density")


###############################################
#Decomposition of the Time Series
###############################################
cts = decompose(ts)
autoplot(cts) +
  ggtitle("Decomposition of Crimes Time Series - BH - 2016-2018") +
  xlab("Epidemiological Week")

#thoroug analysis shows trend, seasonality and some noise


###############################################
#Visual check breaking each period
###############################################
#shows seasonality over the year
ggsubseriesplot(ts)+
  ggtitle("Weekly crimes seasonaity - BH - 2016-2018") +
  ylab("Crimes") +
  xlab("Epidemiological Week")







###############
#Outliers removal

library(forecast)

forecastclean <- nnetar(tsclean(ts))
ts<-ts(forecastclean$x,frequency=52,start=c(2016, 1))

library(car)

qqPlot(forecastclean$x, xlab = "Normal Quantiles", main = "Trimmed Quantiles without Outliers", ylab = "Empirical Quantiles")
#As close as the points fall approximately along this reference line, 
# more probably the data has normality.
ggdensity(forecastclean$x,
          main = "Density plot of crimes",
          xlab = "Weekly crime rate", ylab = "Density")


#Create new set of box&whiskers after outliers removal
boxplot(ts)


#trimmed chart time series 
autoplot(ts) +
  ggtitle("Trimmed Weekly Crimes (without outliers) - BH - 2016-2018") +
  ylab("Crimes") +
  xlab("Epidemiological Week")

# Separate pre and post COVID data from Time Series
ts <- subset(ts, end = 208)
ts2 <- subset(ts, start = 209)



###############################################
#Remove trend from data (transform trended into sationary)
###############################################
#take the first difference of the data to remove trend
#removes change of the data around the data itself
dts<-diff(ts)
autoplot(dts) +
  ggtitle("Change in daily crimes - BH - 2016-2018") +
  ylab("Crimes") +
  xlab("Epidemiological Week")
#chart has little variation (showing outliers have being removed), but should be flat


###############################################
#Benchmark models to forecast
###############################################
#MODEL 1
#ARIMA model (autoregressive integrated moving average)
#with auto adjust OVER RAW DATA
arima<-auto.arima(ts) 

# MODEL 2
#SARIMA - adds seasonal to the ARIMA model
#performs arima with auto adjust OVER DESEASONALIZED
#arima<-auto.arima(dts) 

###############################################
#Review summary of model
print(summary(arima))


###############################################
#check of ARIMA model residuals
acf(arima$residuals, plot = TRUE, lag.max = 52) #shows lags used in the MA part of the model
# lag = 1, is a hole season, in this case, 52 weeks
pacf(arima$residuals) #shows lags used in the AR part of the model

###############################################
#get accuracy of the forecast
(1-pnorm(abs(arima$coef)/sqrt(diag(arima$var.coef))))*2
pvalues<-(1-pnorm(abs(arima$coef)/sqrt(diag(arima$var.coef))))*2
sprintf("%.2f", pvalues)
#####
#FALTA FAZER O JEITO DE PEGAR ISSO E TABELAR
#####



#####
#Perform Ljung-Box test
checkresiduals(arima)


###############################################
#Forecasting using the CHOSEN model
###############################################
#we chose the ARIMA model, based on analyses
#create 'forecast' dataset based on ARIMA above, for 74 weeks
forecast<-forecast(arima,h=75) 
#chart forecast

autoplot(forecast) +
  ggtitle("Weekly Crime Forecast - BH - 2020-2021") +
  ylab("Crimes") +
  xlab("Epidemiological Week")
#chart zooming on forecast
autoplot(forecast, include = 12) +
  ggtitle("Weekly Theafts Forecast- BH - 2020-2021") +
  ylab("Crimes") +
  xlab("Epidemiological Week")
#chart forecast RESIDUALS to check variance
autoplot(forecast$residuals) +
  ggtitle("Residuals from crime forecast") +
  ylab("Forecast residuals") +
  xlab("Year") 

###############################################
#sample quantiles OVER theoretic quantiles
#dots ploted away, too above or too below the qqline indicates variance
#the longer the BAR, the greater the 'lag'
qqnorm(forecast$residuals)
qqline(forecast$residuals)

###############################################
#report auto correlation of the forecast model
acf(forecast$residuals, plot = TRUE, lag.max = 52) #auto-correlation
pacf(forecast$residuals) #passive-correlation
#ideal -> ACF and PACF shows bars within the blue lines 
#blue lines are the 95% confidence level intervals
#(means this model fits the data  well)


###############################################
#report ARIMA data 
###TEM QUE DESENVOLVER TUDO
rep<-as.data.frame(capture.output(arima))
colnames(rep) [1] <- "R"
