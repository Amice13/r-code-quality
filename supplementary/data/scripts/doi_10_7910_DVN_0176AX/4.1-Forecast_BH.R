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
#bd<- read.csv("C:/Users/Frederico/Desktop/BH/2-Entry.csv")

colnames(bd) [2] <- "Roubo"
colnames(bd) [3] <- "Furto"
colnames(bd) [4] <- "Homicidio"
colnames(bd) [5] <- "Roubo a Transeuntes"
colnames(bd) [6] <- "Roubo a veiculos"
colnames(bd) [7] <- "Roubo a residencias"
colnames(bd) [8] <- "Furto a transeuntes"
colnames(bd) [9] <- "Furto a Veiculos"
colnames(bd) [10] <- "Furto a residencias"

						
###############################################
#CHOOSE CRIME
#bd <- bd[,-(3:10)]   #ROBBERY
#bd <- bd[,-(2)]   #THEFT
#bd <- bd[,-(2:3)]   #HOMICIDE
#bd <- bd[,-(2:4)]   #ROBBERY street
#bd <- bd[,-(2:5)]   #ROBBERY vehicles
#bd <- bd[,-(2:6)]   #ROBBERY residential
#bd <- bd[,-(2:7)]   #THEFT street
#bd <- bd[,-(2:8)]   #THEFT vehicles
bd <- bd[,-(2:9)]   #THEFT residential

###############################################


# Separate pre and post COVID data from Time Series
library(dplyr)
bd1 <-bd %>% slice(1:208)
bd2 <-bd %>% slice(208:282)

###############################################
#create 'ts' value with time series data
#frequency can be days(365), weeks(52), months(12), quarters(4), etc
#start=c shows YEAR, MONTH
ts1<-ts(bd1[,2],frequency=52,start=c(2016, 1))
ts2<-ts(bd2[,2],frequency=52,start=c(2019, 52))
###############
#If outliers are detected, Time Series will have to be trimmed
library(forecast)
forecastclean <- nnetar(tsclean(ts1))
tsfit<-ts(forecastclean$x,frequency=52,start=c(2016, 1))




###############################################
#Benchmark models to forecast
###############################################
#MODEL 1
#ARIMA model (autoregressive integrated moving average)
#with auto adjust OVER RAW DATA
#arima<-auto.arima(ts1) 


#MODEL 2
#ARIMA model (autoregressive integrated moving average)
#with auto adjust OVER FITTED (outlier free) DATA
arima<-auto.arima(tsfit) 

###############################################
# MODEL 3
#performs arima with auto adjust OVER DIFFERENCIATED TIME SERIES
#Time Series (TS) has trend and  seasonality
#the first difference series (dts) has no trend but still has seasonality
#arima<-auto.arima(dts)


###############################################
# MODEL 4
#SARIMA - adds seasonal to the ARIMA model
#library(astsa)
#sarima<-sarima(ts1, 2, 0, 3, 1, 0, 0,52) #p, d, q, P, D, Q, S have to be calculated manually


###############################################
#Forecasting using the CHOSEN model
###############################################
#we chose the ARIMA model, based on analyses
#create 'forecast' dataset based on ARIMA above, for 74 weeks

forecast<-forecast(arima,h=75, level =c(80, 95, 99)) 

#in case of using manual SARIMA
#print(arima)
#to print AR, MA and S values
#sarima<-sarima.for(ts, n.ahead=75, 1, 0, 0, 0, 0, 1,52, gg=TRUE, col=4, main='arf', plot.all = TRUE)
#forecast SARIMA using p,d,q,P=,D= ,Q=0, S= from the best auto.arima fit


###############################################
#export forecast
write.csv(forecast, file = "/Users/fred/Desktop/BH/Charts/5.0-forecast2020.csv")
#write.csv(forecast, file = "C:/Users/Frederico/Desktop/BH/5.0-forecast2020.csv")



###############################################
#Create unique chart
###############################################
# Convert pred from list to data frame object
library(ggfortify)
chart1 <- as_tibble(fortify(forecast))

# rename  columns 
library(dplyr)
chart1<-rename(chart1, Forecast = 'Point Forecast')
chart1<-rename(chart1, Low99 = 'Lo 99')
chart1<-rename(chart1, High99 = 'Hi 99')
chart1<-rename(chart1, Low95 = 'Lo 95')
chart1<-rename(chart1, High95 = 'Hi 95')
chart1<-rename(chart1, Low80 = 'Lo 80')
chart1<-rename(chart1, High80 = 'Hi 80')


### Avoid the gap between data and forcast
# Find the last non missing values in obs then use that
# one to initialize the forecast column
firstNAForecast <- min(which(complete.cases(chart1$Forecast)))
chart1[firstNAForecast-1,4]<-chart1[firstNAForecast-1,2]


### Plot complete time series with forecast and actual result, including CI's
clrs <- c("blue", "red", "orange", "darkgoldenrod4")
chart<-ggplot(chart1, aes(x = Index)) +
  geom_ribbon(aes(ymin = Low99, ymax = High99, fill = "99%")) +
  geom_ribbon(aes(ymin = Low95, ymax = High95, fill = "95%")) +
  geom_ribbon(aes(ymin = Low80, ymax = High80, fill = "80%")) +
  autolayer(ts1, series='2016 a 2019 outliers')+
  autolayer(tsfit, series='2016 a 2019') + 
  autolayer(ts2, series='2020 a 2021 realizado') + 
  geom_line(aes(y = Forecast, group = 3, colour = "2020 a 2021 previsto"), size = 0.75) +
  scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
  scale_fill_brewer(name = "Intervals") +
  guides(colour = guide_legend(order = 2), fill = guide_legend(order = 1)) +
  theme_bw(base_size = 11)+
  ggtitle(colnames(bd[2]))+
  xlab(forecast$method) +
  ylab("Crimes por semana") +
  guides(colour=guide_legend(title="Crimes", reverse = FALSE), 
         fill=guide_legend(title="Intervalo de Confianca", reverse = FALSE))+
  scale_color_manual(values=clrs) 
  


#####
#Perform Ljung-Box test
# If the test gives p>0.05, the time series (residuals) pass the test.
checkresiduals(arima, lag = 52)

#Robbery ARIMA(3,1,4)(1,0,0)[52] with drift - Ljung-Box p-value = 0.008806
#Theft ARIMA(1,0,0)(0,0,1)[52] with zero mean - Ljung-Box p-value = 0.9152
#Homicide ARIMA(2,1,3)(1,0,0)[52] - Ljung-Box p-value = 0.286
#Robbery street ARIMA(1,1,1)(1,0,0)[52] with drift - Ljung-Box p-value = 0.09291
#Robbery Vehicle ARIMA(3,1,0)(1,0,1)[52] with drift -  Ljung-Box p-value = 0.4087
#Robbery Residential ARIMA(3,1,1)(1,0,0)[52]- Ljung-Box p-value = 0.1035
#Theft street ARIMA(3,1,2)(1,0,0)[52] with drift - Ljung-Box p-value 0.7253 
#Theft Vehicle  ARIMA(1,0,1)(1,0,0)[52] with non-zero mean - Ljung-Box p-value = 0.7488
#Theft residential ARIMA(2,1,3)(1,0,0)[52] with drift - Ljung-Box p-value = 0.6548

png(file = "/Users/fred/Desktop/BH/Charts/5.2.png", bg = "transparent", width = 1592, height = 1110, res = 300)
chart
dev.off()

png(file = "/Users/fred/Desktop/BH/Charts/5.3.png", bg = "transparent", width = 1592, height = 1110, res = 300)
checkresiduals(arima, test = FALSE, plot = TRUE)
dev.off()


library(magick)
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html
mainchart1 <- image_read('/Users/fred/Desktop/BH/Charts/5.2.png')
mainchart2 <- image_read('/Users/fred/Desktop/BH/Charts/5.3.png')
mainchart <- c(mainchart1, mainchart2)
image_write(image_append(image_scale(mainchart, "3184x1110"), stack = TRUE), path = "/Users/fred/Desktop/BH/Charts/5.1.png", format = "png")
