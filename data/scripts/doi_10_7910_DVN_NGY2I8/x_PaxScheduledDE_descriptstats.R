#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxScheduledDE)
str(ts_A_PaxScheduledDE)
start(ts_A_PaxScheduledDE)
end(ts_A_PaxScheduledDE)
frequency(ts_A_PaxScheduledDE)
cycle(ts_A_PaxScheduledDE)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxScheduledDE<- ts(ts_A_PaxScheduledDE, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxScheduledDE,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Domestic/DE Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxScheduledDE, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Domestic/DE Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxScheduledDE~cycle(seasonal_ts_A_PaxScheduledDE),
        main = "Group A Domestic/DE Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxScheduledDE) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_PaxScheduledDE)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxScheduledDE)
str(ts_B_PaxScheduledDE)
start(ts_B_PaxScheduledDE)
end(ts_B_PaxScheduledDE)
frequency(ts_B_PaxScheduledDE)
cycle(ts_B_PaxScheduledDE)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxScheduledDE<- ts(ts_B_PaxScheduledDE, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxScheduledDE,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Domestic/DE Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxScheduledDE, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Domestic/DE Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxScheduledDE~cycle(seasonal_ts_B_PaxScheduledDE),
        main = "Group B Domestic/DE Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxScheduledDE) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_PaxScheduledDE)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxScheduledDE)
str(ts_C_PaxScheduledDE)
start(ts_C_PaxScheduledDE)
end(ts_C_PaxScheduledDE)
frequency(ts_C_PaxScheduledDE)
cycle(ts_C_PaxScheduledDE)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxScheduledDE<- ts(ts_C_PaxScheduledDE, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxScheduledDE,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Domestic/DE Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxScheduledDE, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Domestic/DE Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxScheduledDE~cycle(seasonal_ts_C_PaxScheduledDE),
        main = "Group C Domestic/DE Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxScheduledDE) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_PaxScheduledDE)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxScheduledDE)
str(ts_D_PaxScheduledDE)
start(ts_D_PaxScheduledDE)
end(ts_D_PaxScheduledDE)
frequency(ts_D_PaxScheduledDE)
cycle(ts_D_PaxScheduledDE)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxScheduledDE<- ts(ts_D_PaxScheduledDE, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxScheduledDE,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Domestic/DE Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxScheduledDE, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Domestic/DE Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxScheduledDE~cycle(seasonal_ts_D_PaxScheduledDE),
        main = "Group D Domestic/DE Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxScheduledDE) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_PaxScheduledDE)))

### End of Descriptive Statistics Group D ###
#############################################