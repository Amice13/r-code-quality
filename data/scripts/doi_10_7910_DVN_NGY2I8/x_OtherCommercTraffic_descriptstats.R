#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_OtherCommercTraffic)
str(ts_A_OtherCommercTraffic)
start(ts_A_OtherCommercTraffic)
end(ts_A_OtherCommercTraffic)
frequency(ts_A_OtherCommercTraffic)
cycle(ts_A_OtherCommercTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_OtherCommercTraffic<- ts(ts_A_OtherCommercTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_OtherCommercTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Other Commercial Traffic")

#Plot annual general trend
plot(aggregate(ts_A_OtherCommercTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Other Commercial Traffic Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_OtherCommercTraffic~cycle(seasonal_ts_A_OtherCommercTraffic),
        main = "Group A Other Commercial Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_OtherCommercTraffic) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_OtherCommercTraffic)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_OtherCommercTraffic)
str(ts_B_OtherCommercTraffic)
start(ts_B_OtherCommercTraffic)
end(ts_B_OtherCommercTraffic)
frequency(ts_B_OtherCommercTraffic)
cycle(ts_B_OtherCommercTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_OtherCommercTraffic<- ts(ts_B_OtherCommercTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_OtherCommercTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Other Commercial Traffic")

#Plot annual general trend
plot(aggregate(ts_B_OtherCommercTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Other Commercial Traffic Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_OtherCommercTraffic~cycle(seasonal_ts_B_OtherCommercTraffic),
        main = "Group B Other Commercial Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_OtherCommercTraffic) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_OtherCommercTraffic)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_OtherCommercTraffic)
str(ts_C_OtherCommercTraffic)
start(ts_C_OtherCommercTraffic)
end(ts_C_OtherCommercTraffic)
frequency(ts_C_OtherCommercTraffic)
cycle(ts_C_OtherCommercTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_OtherCommercTraffic<- ts(ts_C_OtherCommercTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_OtherCommercTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Other Commercial Traffic")

#Plot annual general trend
plot(aggregate(ts_C_OtherCommercTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Other Commercial Traffic Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_OtherCommercTraffic~cycle(seasonal_ts_C_OtherCommercTraffic),
        main = "Group C Other Commercial Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_OtherCommercTraffic) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_OtherCommercTraffic)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_OtherCommercTraffic)
str(ts_D_OtherCommercTraffic)
start(ts_D_OtherCommercTraffic)
end(ts_D_OtherCommercTraffic)
frequency(ts_D_OtherCommercTraffic)
cycle(ts_D_OtherCommercTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_OtherCommercTraffic<- ts(ts_D_OtherCommercTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_OtherCommercTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Other Commercial Traffic")

#Plot annual general trend
plot(aggregate(ts_D_OtherCommercTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Other Commercial Traffic Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_OtherCommercTraffic~cycle(seasonal_ts_D_OtherCommercTraffic),
        main = "Group D Other Commercial Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_OtherCommercTraffic) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_OtherCommercTraffic)))

### End of Descriptive Statistics Group D ###
#############################################