#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_PaxTransit)
str(ts_A_PaxTransit)
start(ts_A_PaxTransit)
end(ts_A_PaxTransit)
frequency(ts_A_PaxTransit)
cycle(ts_A_PaxTransit)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_PaxTransit<- ts(ts_A_PaxTransit, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_PaxTransit,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Total Transit Pax")

#Plot annual general trend
plot(aggregate(ts_A_PaxTransit, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Transit Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_PaxTransit~cycle(seasonal_ts_A_PaxTransit),
        main = "Group A Transit Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_A_PaxTransit) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_PaxTransit)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_PaxTransit)
str(ts_B_PaxTransit)
start(ts_B_PaxTransit)
end(ts_B_PaxTransit)
frequency(ts_B_PaxTransit)
cycle(ts_B_PaxTransit)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_PaxTransit<- ts(ts_B_PaxTransit, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_PaxTransit,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Total Transit Pax")

#Plot annual general trend
plot(aggregate(ts_B_PaxTransit, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Transit Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_PaxTransit~cycle(seasonal_ts_B_PaxTransit),
        main = "Group B Transit Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_B_PaxTransit) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_PaxTransit)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_PaxTransit)
str(ts_C_PaxTransit)
start(ts_C_PaxTransit)
end(ts_C_PaxTransit)
frequency(ts_C_PaxTransit)
cycle(ts_C_PaxTransit)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_PaxTransit<- ts(ts_C_PaxTransit, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_PaxTransit,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Total Transit Pax")

#Plot annual general trend
plot(aggregate(ts_C_PaxTransit, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Transit Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_PaxTransit~cycle(seasonal_ts_C_PaxTransit),
        main = "Group C Transit Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_C_PaxTransit) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_PaxTransit)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_PaxTransit)
str(ts_D_PaxTransit)
start(ts_D_PaxTransit)
end(ts_D_PaxTransit)
frequency(ts_D_PaxTransit)
cycle(ts_D_PaxTransit)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_PaxTransit<- ts(ts_D_PaxTransit, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_PaxTransit,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Total Transit Pax")

#Plot annual general trend
plot(aggregate(ts_D_PaxTransit, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Transit Pax Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_PaxTransit~cycle(seasonal_ts_D_PaxTransit),
        main = "Group D Transit Pax Seasonal Trend 
    (2016-2019)") 

rm(seasonal_ts_D_PaxTransit) # no longer needed in analysis, remove to prevent potential mistakes

####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_PaxTransit)))

### End of Descriptive Statistics Group D ###
#############################################