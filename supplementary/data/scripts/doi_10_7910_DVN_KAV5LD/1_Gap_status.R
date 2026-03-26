library (lubridate)
library(imputeTS)
library (lubridate)
library(signal)


### Overview on the gap status and extract statistics 

data <- read.csv("daily_data.csv")
data$Date <- ymd(data$Date)

ggplot_na_distribution(data$CO2.Flux, x_axis_labels = data$Date, alpha_missing = 0.1, 
                       title = "CO2 Flux missing values of Principe 06/2018 to 7/2022",
                       subtitle = "", color_missing_border = "tan",
                       color_points = "red")


ggplot_na_gapsize (data$CO2.Flux, xlab = "CO2 flux measurment",
                   title = "CO2 Flux missing values of Principe 06/2018 to 7/2022",
                   subtitle = "")

statsNA (data$CO2.Flux)

############################################

### Smoothing 

data <- read.csv("30min_data.csv")

# plotting 30min intervals flux
plot(data$CO2.Flux, type = "l")


m <- data$CO2.Flux
m <- na.omit(m)
plot(m, type = "l")


smooth_new <- sgolayfilt(m, p=1, n= 143, m=0)
plot(smooth_new, type = "l")

write.csv(smooth_new, "CO2-sm143.csv")
