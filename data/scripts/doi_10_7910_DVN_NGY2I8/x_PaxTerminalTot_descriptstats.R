#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxTerminalTot)
str(ts_A_PaxTerminalTot)
start(ts_A_PaxTerminalTot)
end(ts_A_PaxTerminalTot)
frequency(ts_A_PaxTerminalTot)
cycle(ts_A_PaxTerminalTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxTerminalTot<- ts(ts_A_PaxTerminalTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxTerminalTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Total Terminal Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxTerminalTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Terminal Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxTerminalTot~cycle(seasonal_ts_A_PaxTerminalTot),
        main = "Group A Terminal Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxTerminalTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# # plot(decompose((ts_A_PaxTerminalTot)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxTerminalTot)
str(ts_B_PaxTerminalTot)
start(ts_B_PaxTerminalTot)
end(ts_B_PaxTerminalTot)
frequency(ts_B_PaxTerminalTot)
cycle(ts_B_PaxTerminalTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxTerminalTot<- ts(ts_B_PaxTerminalTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxTerminalTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Total Terminal Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxTerminalTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Terminal Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxTerminalTot~cycle(seasonal_ts_B_PaxTerminalTot),
        main = "Group B Terminal Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxTerminalTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# # plot(decompose((ts_B_PaxTerminalTot)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxTerminalTot)
str(ts_C_PaxTerminalTot)
start(ts_C_PaxTerminalTot)
end(ts_C_PaxTerminalTot)
frequency(ts_C_PaxTerminalTot)
cycle(ts_C_PaxTerminalTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxTerminalTot<- ts(ts_C_PaxTerminalTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxTerminalTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Total Terminal Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxTerminalTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Terminal Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxTerminalTot~cycle(seasonal_ts_C_PaxTerminalTot),
        main = "Group C Terminal Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxTerminalTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# # plot(decompose((ts_C_PaxTerminalTot)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxTerminalTot)
str(ts_D_PaxTerminalTot)
start(ts_D_PaxTerminalTot)
end(ts_D_PaxTerminalTot)
frequency(ts_D_PaxTerminalTot)
cycle(ts_D_PaxTerminalTot)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxTerminalTot<- ts(ts_D_PaxTerminalTot, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxTerminalTot,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Total Terminal Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxTerminalTot, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Terminal Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxTerminalTot~cycle(seasonal_ts_D_PaxTerminalTot),
        main = "Group D Terminal Traffic Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxTerminalTot) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# # plot(decompose((ts_D_PaxTerminalTot)))

### End of Descriptive Statistics Group D ###
#############################################