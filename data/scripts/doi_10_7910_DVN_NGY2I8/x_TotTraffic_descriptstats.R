#############################################
### Descriptive Statistics Group A ###

# Check for errors in TS Strucutre
class(ts_A_TotTraffic)
str(ts_A_TotTraffic)
start(ts_A_TotTraffic)
end(ts_A_TotTraffic)
frequency(ts_A_TotTraffic)
cycle(ts_A_TotTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_A_TotTraffic<- ts(ts_A_TotTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_A_TotTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Total Pax")

#Plot annual general trend
plot(aggregate(ts_A_TotTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group A Total Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_A_TotTraffic~cycle(seasonal_ts_A_TotTraffic),
        main = "Group A Total Traffic Seasonal Trend 
    (2016-2019)") 


####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_A_TotTraffic)))

### End of Descriptive Statistics Group A ###
#############################################


### Descriptive Statistics Group B ###

# Check for errors in TS Strucutre
class(ts_B_TotTraffic)
str(ts_B_TotTraffic)
start(ts_B_TotTraffic)
end(ts_B_TotTraffic)
frequency(ts_B_TotTraffic)
cycle(ts_B_TotTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_B_TotTraffic<- ts(ts_B_TotTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_B_TotTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Total Pax")

#Plot annual general trend
plot(aggregate(ts_B_TotTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group B Total Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_B_TotTraffic~cycle(seasonal_ts_B_TotTraffic),
        main = "Group B Total Traffic Seasonal Trend 
    (2016-2019)") 


####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_B_TotTraffic)))

### End of Descriptive Statistics Group B ###
#############################################


### Descriptive Statistics Group C ###

# Check for errors in TS Strucutre
class(ts_C_TotTraffic)
str(ts_C_TotTraffic)
start(ts_C_TotTraffic)
end(ts_C_TotTraffic)
frequency(ts_C_TotTraffic)
cycle(ts_C_TotTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_C_TotTraffic<- ts(ts_C_TotTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_C_TotTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Total Pax")

#Plot annual general trend
plot(aggregate(ts_C_TotTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group C Total Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_C_TotTraffic~cycle(seasonal_ts_C_TotTraffic),
        main = "Group C Total Traffic Seasonal Trend 
    (2016-2019)") 



####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_C_TotTraffic)))

### End of Descriptive Statistics Group C ###
#############################################


### Descriptive Statistics Group D ###

# Check for errors in TS Strucutre
class(ts_D_TotTraffic)
str(ts_D_TotTraffic)
start(ts_D_TotTraffic)
end(ts_D_TotTraffic)
frequency(ts_D_TotTraffic)
cycle(ts_D_TotTraffic)

# Time Series omitting 2020 to check seasonality before crisis
seasonal_ts_D_TotTraffic<- ts(ts_D_TotTraffic, start=c(2016,1),end = c(2019,12),frequency = 12)

# Descriptive Stats to understand TS Structure before analysis
plot.new()
frame()
par(mfcol=c(3,1))
plot(ts_D_TotTraffic,
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Total Pax")

#Plot annual general trend
plot(aggregate(ts_D_TotTraffic, FUN = mean),
     xlab = "Time (Year)",
     ylab = "Passengers",
     main = "Group D Total Annual Trend")

#Boxplot helps visualize seasonality of data (peak in summer months)
boxplot(seasonal_ts_D_TotTraffic~cycle(seasonal_ts_D_TotTraffic),
        main = "Group D Total Traffic Seasonal Trend 
    (2016-2019)") 


####################################################
# Plot Seasonal examples

plot.new()
frame()
par(mfcol=c(2,2), oma = c(1,1,2,1))

boxplot(seasonal_ts_A_TotTraffic~cycle(seasonal_ts_A_TotTraffic),
        main = "Group A - Large Community Airports") 

boxplot(seasonal_ts_B_TotTraffic~cycle(seasonal_ts_B_TotTraffic),
        main = "Group B - National Airports") 

boxplot(seasonal_ts_C_TotTraffic~cycle(seasonal_ts_C_TotTraffic),
        main = "Group C - Large Regional Airports") 

boxplot(seasonal_ts_D_TotTraffic~cycle(seasonal_ts_D_TotTraffic),
        main = "Group D - Small Regional Airports") 

title(main="Total Traffic (incl. Transit) Seasonal Trend (2016-2019)", 
      outer=TRUE, line=0)


####################################################
# Decompose Time Series Data for further descriptive insights
## Observed – actual data plot
## Trend – the overall upward or downward movement of the data points
## Seasonal – any monthly/yearly pattern of the data points
## Random – unexplainable part of the data

# plot(decompose((ts_D_TotTraffic)))

### End of Descriptive Statistics Group D ###
#############################################

rm(seasonal_ts_A_TotTraffic) # no longer needed in analysis, remove to prevent potential mistakes
rm(seasonal_ts_B_TotTraffic) # no longer needed in analysis, remove to prevent potential mistakes
rm(seasonal_ts_C_TotTraffic) # no longer needed in analysis, remove to prevent potential mistakes
rm(seasonal_ts_D_TotTraffic) # no longer needed in analysis, remove to prevent potential mistakes