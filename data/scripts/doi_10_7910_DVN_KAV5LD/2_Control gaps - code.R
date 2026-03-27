
##### The script will produce control gaps based on required percentage 
##### Read each variable time series - using smooth data 

########### 28% total and each 2% 

### read data 
data <- read.csv("Principe 2013-2022-NO_GAP-CO2.csv")

### plot data to check before creating control gaps 

plot_free_gaps <- plot(data$X3D_CO2_smooth, type = "l")


set.seed(123) # for reproducibility
n <- nrow(data) # total number of rows in the data frame
gapsize <- floor(n * 2 / 100) # gap size, 11.7% of the total number of rows
gapstarts <- sort(sample(1:(n-14*gapsize), size = 14, replace = FALSE)) # start points of the three gaps


# replace values in the gaps with NA's
data[gapstarts[1]:(gapstarts[1]+gapsize-1), ] <- NA
data[(gapstarts[2]+gapsize):(gapstarts[2]+2*gapsize-1), ] <- NA
data [(gapstarts[3]+2*gapsize):(gapstarts[3]+3*gapsize-1), ] <- NA
data [(gapstarts[4]+3*gapsize):(gapstarts[4]+4*gapsize-1), ] <- NA
data [(gapstarts[5]+4*gapsize):(gapstarts[5]+5*gapsize-1), ] <- NA
data [(gapstarts[6]+5*gapsize):(gapstarts[6]+6*gapsize-1), ] <- NA
data [(gapstarts[7]+6*gapsize):(gapstarts[7]+7*gapsize-1), ] <- NA
data [(gapstarts[8]+7*gapsize):(gapstarts[8]+8*gapsize-1), ] <- NA
data [(gapstarts[9]+8*gapsize):(gapstarts[9]+9*gapsize-1), ] <- NA
data [(gapstarts[10]+9*gapsize):(gapstarts[10]+10*gapsize-1), ] <- NA
data [(gapstarts[11]+10*gapsize):(gapstarts[11]+11*gapsize-1), ] <- NA
data [(gapstarts[12]+11*gapsize):(gapstarts[12]+12*gapsize-1), ] <- NA
data [(gapstarts[13]+12*gapsize):(gapstarts[13]+13*gapsize-1), ] <- NA
data [(gapstarts[14]+13*gapsize):(gapstarts[14]+14*gapsize-1), ] <- NA

plot_2_gaps <- plot(data$X3D_CO2_smooth, type = "l")

write.csv(data, "CO2- 2%-14 gaps.csv")


######################### 5 Gaps each 5%

data <- read.csv("Principe 2013-2022-NO_GAP-CO2.csv")

plot(data$X3D_CO2_smooth, type = "l")


set.seed(123) # for reproducibility
n <- nrow(data) # total number of rows in the data frame
gapsize <- floor(n * 5 / 100) # gap size, 11.7% of the total number of rows
gapstarts <- sort(sample(1:(n-5*gapsize), size = 5, replace = FALSE)) # start points of the three gaps


# replace values in the gaps with NA's
data[gapstarts[1]:(gapstarts[1]+gapsize-1), ] <- NA
data[(gapstarts[2]+gapsize):(gapstarts[2]+2*gapsize-1), ] <- NA
data [(gapstarts[3]+2*gapsize):(gapstarts[3]+3*gapsize-1), ] <- NA
data [(gapstarts[4]+3*gapsize):(gapstarts[4]+4*gapsize-1), ] <- NA
data [(gapstarts[5]+4*gapsize):(gapstarts[5]+5*gapsize-1), ] <- NA

plot_5_gaps <- plot(data$X3D_CO2_smooth, type = "l")


write.csv(data, "CO2- 5%-5 gaps.csv")


############################# 3 Gaps each 8% 

data <- read.csv("Principe 2013-2022-NO_GAP-CO2.csv")

plot(data$X3D_CO2_smooth, type = "l")


set.seed(123) # for reproducibility
n <- nrow(data) # total number of rows in the data frame
gapsize <- floor(n * 8 / 100) # gap size, 11.7% of the total number of rows
gapstarts <- sort(sample(1:(n-3*gapsize), size = 3, replace = FALSE)) # start points of the three gaps

# replace values in the gaps with NA's
data[gapstarts[1]:(gapstarts[1]+gapsize-1), ] <- NA
data[(gapstarts[2]+gapsize):(gapstarts[2]+2*gapsize-1), ] <- NA
data [(gapstarts[3]+2*gapsize):(gapstarts[3]+3*gapsize-1), ] <- NA


plot_8_gaps <- plot(data$X3D_CO2_smooth, type = "l")

write.csv(data, "CO2-8%-3 gaps.csv")


############################# 2 Gaps each 10% 

data <- read.csv("Principe 2013-2022-NO_GAP-CO2.csv")

plot(data$X3D_CO2_smooth, type = "l")


set.seed(123) # for reproducibility
n <- nrow(data) # total number of rows in the data frame
gapsize <- floor(n * 10/ 100) # gap size, 11.7% of the total number of rows
gapstarts <- sort(sample(1:(n-2*gapsize), size = 2, replace = FALSE)) # start points of the three gaps

# replace values in the gaps with NA's
data[gapstarts[1]:(gapstarts[1]+gapsize-1), ] <- NA
data[(gapstarts[2]+gapsize):(gapstarts[2]+2*gapsize-1), ] <- NA



plot_10_gaps <- plot(data$X3D_CO2_smooth, type = "l")


write.csv(data, "CO2- new10%-2 gaps.csv")


############################# 1 Gap each 13%

data <- read.csv("Principe 2013-2022-NO_GAP-CO2.csv")

plot(data$X3D_CO2_smooth, type = "l")


set.seed(123) # for reproducibility
n <- nrow(data) # total number of rows in the data frame
gapsize <- floor(n * 13/ 100) # gap size, 11.7% of the total number of rows
gapstarts <- sort(sample(1:(n-1*gapsize), size = 1, replace = FALSE)) # start points of the three gaps

# replace values in the gaps with NA's
data[gapstarts[1]:(gapstarts[1]+gapsize-1), ] <- NA


plot_13_gaps <- plot(data$X3D_CO2_smooth, type = "l")



write.csv(data, "CO2-13%-1 gap.csv")


########### END
