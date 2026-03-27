#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxScheduledNonEurope)
str(ts_A_PaxScheduledNonEurope)
start(ts_A_PaxScheduledNonEurope)
end(ts_A_PaxScheduledNonEurope)
frequency(ts_A_PaxScheduledNonEurope)
cycle(ts_A_PaxScheduledNonEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxScheduledNonEurope<- ts(ts_A_PaxScheduledNonEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxScheduledNonEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Intercont./Non-Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxScheduledNonEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Intercont./Non-Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxScheduledNonEurope~cycle(seasonal_ts_A_PaxScheduledNonEurope),
        main = "Group A Intercont./Non-Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxScheduledNonEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_PaxScheduledNonEurope)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxScheduledNonEurope)
str(ts_B_PaxScheduledNonEurope)
start(ts_B_PaxScheduledNonEurope)
end(ts_B_PaxScheduledNonEurope)
frequency(ts_B_PaxScheduledNonEurope)
cycle(ts_B_PaxScheduledNonEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxScheduledNonEurope<- ts(ts_B_PaxScheduledNonEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxScheduledNonEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Intercont./Non-Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxScheduledNonEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Intercont./Non-Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxScheduledNonEurope~cycle(seasonal_ts_B_PaxScheduledNonEurope),
        main = "Group B Intercont./Non-Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxScheduledNonEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_PaxScheduledNonEurope)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxScheduledNonEurope)
str(ts_C_PaxScheduledNonEurope)
start(ts_C_PaxScheduledNonEurope)
end(ts_C_PaxScheduledNonEurope)
frequency(ts_C_PaxScheduledNonEurope)
cycle(ts_C_PaxScheduledNonEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxScheduledNonEurope<- ts(ts_C_PaxScheduledNonEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxScheduledNonEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Intercont./Non-Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxScheduledNonEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Intercont./Non-Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxScheduledNonEurope~cycle(seasonal_ts_C_PaxScheduledNonEurope),
        main = "Group C Intercont./Non-Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxScheduledNonEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_PaxScheduledNonEurope)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxScheduledNonEurope)
str(ts_D_PaxScheduledNonEurope)
start(ts_D_PaxScheduledNonEurope)
end(ts_D_PaxScheduledNonEurope)
frequency(ts_D_PaxScheduledNonEurope)
cycle(ts_D_PaxScheduledNonEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxScheduledNonEurope<- ts(ts_D_PaxScheduledNonEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxScheduledNonEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Intercont./Non-Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxScheduledNonEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Intercont./Non-Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxScheduledNonEurope~cycle(seasonal_ts_D_PaxScheduledNonEurope),
        main = "Group D Intercont./Non-Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxScheduledNonEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_PaxScheduledNonEurope)))

### End of Descriptive Statistics Group D ###
#############################################