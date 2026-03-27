#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxScheduledEurope)
str(ts_A_PaxScheduledEurope)
start(ts_A_PaxScheduledEurope)
end(ts_A_PaxScheduledEurope)
frequency(ts_A_PaxScheduledEurope)
cycle(ts_A_PaxScheduledEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxScheduledEurope<- ts(ts_A_PaxScheduledEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxScheduledEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Cont./Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxScheduledEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Cont./Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxScheduledEurope~cycle(seasonal_ts_A_PaxScheduledEurope),
        main = "Group A Cont./Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxScheduledEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_PaxScheduledEurope)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxScheduledEurope)
str(ts_B_PaxScheduledEurope)
start(ts_B_PaxScheduledEurope)
end(ts_B_PaxScheduledEurope)
frequency(ts_B_PaxScheduledEurope)
cycle(ts_B_PaxScheduledEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxScheduledEurope<- ts(ts_B_PaxScheduledEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxScheduledEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Cont./Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxScheduledEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Cont./Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxScheduledEurope~cycle(seasonal_ts_B_PaxScheduledEurope),
        main = "Group B Cont./Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxScheduledEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_PaxScheduledEurope)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxScheduledEurope)
str(ts_C_PaxScheduledEurope)
start(ts_C_PaxScheduledEurope)
end(ts_C_PaxScheduledEurope)
frequency(ts_C_PaxScheduledEurope)
cycle(ts_C_PaxScheduledEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxScheduledEurope<- ts(ts_C_PaxScheduledEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxScheduledEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Cont./Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxScheduledEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Cont./Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxScheduledEurope~cycle(seasonal_ts_C_PaxScheduledEurope),
        main = "Group C Cont./Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxScheduledEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_PaxScheduledEurope)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxScheduledEurope)
str(ts_D_PaxScheduledEurope)
start(ts_D_PaxScheduledEurope)
end(ts_D_PaxScheduledEurope)
frequency(ts_D_PaxScheduledEurope)
cycle(ts_D_PaxScheduledEurope)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxScheduledEurope<- ts(ts_D_PaxScheduledEurope, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxScheduledEurope,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Cont./Europe Scheduled Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxScheduledEurope, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Cont./Europe Scheduled Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxScheduledEurope~cycle(seasonal_ts_D_PaxScheduledEurope),
        main = "Group D Cont./Europe Scheduled Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxScheduledEurope) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_PaxScheduledEurope)))

### End of Descriptive Statistics Group D ###
#############################################