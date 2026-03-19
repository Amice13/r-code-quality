#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxScheduledTot)
str(ts_A_PaxScheduledTot)
start(ts_A_PaxScheduledTot)
end(ts_A_PaxScheduledTot)
frequency(ts_A_PaxScheduledTot)
cycle(ts_A_PaxScheduledTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxScheduledTot<- ts(ts_A_PaxScheduledTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxScheduledTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Total Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxScheduledTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxScheduledTot~cycle(seasonal_ts_A_PaxScheduledTot),
        main = "Group A Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxScheduledTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_PaxScheduledTot)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxScheduledTot)
str(ts_B_PaxScheduledTot)
start(ts_B_PaxScheduledTot)
end(ts_B_PaxScheduledTot)
frequency(ts_B_PaxScheduledTot)
cycle(ts_B_PaxScheduledTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxScheduledTot<- ts(ts_B_PaxScheduledTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxScheduledTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Total Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxScheduledTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxScheduledTot~cycle(seasonal_ts_B_PaxScheduledTot),
        main = "Group B Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxScheduledTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_PaxScheduledTot)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxScheduledTot)
str(ts_C_PaxScheduledTot)
start(ts_C_PaxScheduledTot)
end(ts_C_PaxScheduledTot)
frequency(ts_C_PaxScheduledTot)
cycle(ts_C_PaxScheduledTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxScheduledTot<- ts(ts_C_PaxScheduledTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxScheduledTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Total Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxScheduledTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxScheduledTot~cycle(seasonal_ts_C_PaxScheduledTot),
        main = "Group C Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxScheduledTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_PaxScheduledTot)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxScheduledTot)
str(ts_D_PaxScheduledTot)
start(ts_D_PaxScheduledTot)
end(ts_D_PaxScheduledTot)
frequency(ts_D_PaxScheduledTot)
cycle(ts_D_PaxScheduledTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxScheduledTot<- ts(ts_D_PaxScheduledTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxScheduledTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Total Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxScheduledTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxScheduledTot~cycle(seasonal_ts_D_PaxScheduledTot),
        main = "Group D Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxScheduledTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_PaxScheduledTot)))

### End of Descriptive Statistics Group D ###
#############################################