
##--------------------------------------
## Install and load any needed libraries
##--------------------------------------

library(lubridate)

##--------------------------------------
## Load the data
##--------------------------------------

data <- read.csv("Mines_Park_pH_Fault.csv", stringsAsFactors = FALSE)
attr(data$dateTime, "tzone") <- "America/Denver"

head(data)

colnames(data)
dim(data)
head(data$dateTime)
tail(data$dateTime)


#April 24, 2010, 10:00 AM.

month.val <- month(data$dateTime)
day.val <- day(data$dateTime)
hour.val <- hour(data$dateTime)
fault.time <- which(month.val==4 & day.val==24 & hour.val==10)[1]
data$dateTime[fault.time]


######################
# Plot of pH
# The biological community cannot survive if
# the pH is less than 6.5
# It clearly dips below this value
######################

ts.plot(data$ras_ph, xlab="Index", ylab="pH")
abline(h=6.5,col=4,lwd=2)
abline(v=fault.time,col=2,lwd=2)

######################
# Plot of MBR Flow
######################
ts.plot(data$mbr_1_perm_flow, xlab="Index", ylab="MBR 1, Permeate Flow")
abline(v=fault.time,col=2,lwd=2)

ts.plot(data$mbr_2_perm_flow, xlab="Index", ylab="MBR 2, Permeate Flow")
abline(v=fault.time,col=2,lwd=2)

######################
# Sorting the variables into
# groups
######################
## cyclic variables
cyclic_vars <- c("cos_daily", 
                     "sin_daily", 
                     "cos_2hour", 
                     "sin_2hour", 
                     "cos_hourly", 
                     "sin_hourly")
##control variables
control_vars <- c("bio_1_blow_flow", 
                  "bio_2_blow_flow", 
                  "mbr_1_tmp", 
                  "mbr_2_tmp",
                  "ambient_temp", 
                  "bio_1_phase_1", 
                  "bio_1_phase_2", 
                  "bio_2_phase_1",
                  "bio_2_phase_2", 
                  "mbr_1_mode_1", 
                  "mbr_1_mode_2", 
                  "mbr_1_mode_4",
                  "mbr_2_mode_1", 
                  "mbr_2_mode_2", 
                  "mbr_2_mode_4")

## response variables
response_vars <- c("mbr_1_perm_flow", 
                   "mbr_2_perm_flow", 
                   "ras_temp",
                   "bio_1_do", 
                   "bio_2_do", 
                   "mbr_1_level", 
                   "mbr_2_level", 
                   "perm_turb", 
                   "sewage_flow", 
                   "bio_1_level", 
                   "bio_2_level",
                   "bio_1_temp", 
                   "bio_2_temp", 
                   "bio_1_tss", 
                   "bio_2_tss", 
                   "perm_tank_level", 
                   "ras_do", 
                   "ras_ph", 
                   "perm_cond", 
                   "ras_tss")
                   
#Other predictors                  
scale_predictors <- c("bio_1_blow_flow",
                      "bio_2_blow_flow", 
                      "ambient_temp",
                      "mbr_1_tmp", 
                      "mbr_2_tmp")

x.cols<-NULL
for(i in 1:length(scale_predictors)){
	x.cols<-c(x.cols,which(colnames(data)==scale_predictors[i]))}
for(i in 1:length(control_vars)){
	x.cols<-c(x.cols,which(colnames(data)==control_vars[i]))}
for(i in 1:length(cyclic_vars)){
	x.cols<-c(x.cols,which(colnames(data)==cyclic_vars[i]))}

y.cols<-NULL
for(i in 1:length(response_vars)){
	y.cols<-c(y.cols,which(colnames(data)==response_vars[i]))}


XX<-data[,x.cols]
YY<-data[,y.cols]

dim(XX)
dim(YY)