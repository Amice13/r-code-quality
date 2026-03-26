######
# author: Anthony Harding (tony.harding@gatech.edu)
# Georgia Institute of Technology, School of Public Policy
######

library(ggplot2)
library(dplyr)
library(MASS)

# Set working Directory
path = "C:/Users/aharding6/GaTech Dropbox/Anthony Harding/Tobetransferred/ConsistentClimateConvergence - Copy/Replication"
setwd(path)

# Calculate Temperature innovation a la Bilal and Kanzig (2024)


# Read in NOAA data
dfNOAA <- read.csv("Data/input/HistoricalWeather/NOAAData.csv",skip=4)

# Clean to get annual average
dfNOAA$Year <- as.numeric(substr(dfNOAA$Date,1,4))
dfNOAA <- dfNOAA %>%
  group_by(Year) %>%
  summarize(Anomaly=mean(Anomaly))
dfNOAA <- data.frame(dfNOAA)

ggplot(data=dfNOAA) + 
  geom_line(aes(Year,Anomaly)) +
  theme_classic()


# Generate leads and lags
dfNOAA$Anomalyp2 <- c(dfNOAA$Anomaly[3:175],NA,NA)
dfNOAA$Anomalym1 <- c(NA,dfNOAA$Anomaly[1:174])
dfNOAA$Anomalym2 <- c(NA,NA,dfNOAA$Anomaly[1:173])


NOAAmdl <- lm(Anomalyp2~Anomaly+Anomalym1+Anomalym2,data=dfNOAA[dfNOAA$Year>1950 & dfNOAA$Year<2024,])
NOAAmdl.coef <- summary(NOAAmdl)$coefficients[,1]
NOAAmdl.vcv <- vcov(NOAAmdl)

dfNOAA$NOAAinnovationp2 <- dfNOAA$Anomalyp2 - (NOAAmdl.coef[1]+NOAAmdl.coef[2]*dfNOAA$Anomaly+NOAAmdl.coef[3]*dfNOAA$Anomalym1+NOAAmdl.coef[4]*dfNOAA$Anomalym2)
dfNOAA$NOAAinnovation <- c(NA,NA,dfNOAA$NOAAinnovationp2[1:173])


ggplot(data=dfNOAA[dfNOAA$Year>1948 & dfNOAA$Year<=2023,]) +
  geom_line(aes(Year,NOAAinnovation),color="red",linewidth=1) +
  geom_hline(yintercept=0) +
  theme_classic() + theme(axis.line.x=element_blank()) +
  scale_x_continuous(breaks=c(1960,1980,2000,2020))

write.csv(dfNOAA,file="Data/output/NOAATemperatureInnovation.csv")


monte.coeff <- mvrnorm(1000,NOAAmdl.coef,NOAAmdl.vcv)


shock <- array(dim=c(1000,17))
shock[,c(1:4)] <- 0
shock[,5] <- 1
for(i in c(4:15)){
  shock_temp <- monte.coeff[,1]+monte.coeff[,2]*shock[,i-1]+monte.coeff[,3]*shock[,i-2]+monte.coeff[,4]*shock[,i-3]
  shock[,i+2] <- shock_temp
}

shock.plot <- data.frame(year=c(0:10))
shock.plot$upper <- apply(shock,2,quantile,probs=0.95)[5:15]
shock.plot$lower <- apply(shock,2,quantile,probs=0.05)[5:15]
shock.plot$mean <- apply(shock,2,mean)[5:15]

ggplot(data=shock.plot) +
  geom_ribbon(aes(x=year,ymin=lower,ymax=upper), fill = "grey70",alpha=0.5) +
  geom_line(aes(year,mean)) +
  scale_x_continuous(limits=c(0,10),breaks=seq(0,10,2)) +
  ylab("C") + theme_classic()






# Read in MWv5 data
load("Data/output/MWv5/GlobalTemp.dta")

# Clean to get annual average
dfMWv5 <- data.frame("Year" = c(1900:2017))
dfMWv5$Anomaly <- tc

ggplot(data=dfMWv5) + 
  geom_line(aes(Year,Anomaly)) +
  theme_classic()


# Generate leads and lags
dfMWv5$Anomalyp2 <- c(dfMWv5$Anomaly[3:118],NA,NA)
dfMWv5$Anomalym1 <- c(NA,dfMWv5$Anomaly[1:117])
dfMWv5$Anomalym2 <- c(NA,NA,dfMWv5$Anomaly[1:116])


MWv5mdl <- lm(Anomalyp2~Anomaly+Anomalym1+Anomalym2,data=dfMWv5[dfMWv5$Year>1950 & dfMWv5$Year<2024,])
MWv5mdl.coef <- summary(MWv5mdl)$coefficients[,1]
MWv5mdl.vcv <- vcov(MWv5mdl)

dfMWv5$MWv5innovationp2 <- dfMWv5$Anomalyp2 - (MWv5mdl.coef[1]+MWv5mdl.coef[2]*dfMWv5$Anomaly+MWv5mdl.coef[3]*dfMWv5$Anomalym1+MWv5mdl.coef[4]*dfMWv5$Anomalym2)
dfMWv5$MWv5innovation <- c(NA,NA,dfMWv5$MWv5innovationp2[1:116])


ggplot(data=dfMWv5[dfMWv5$Year>1948 & dfMWv5$Year<=2023,]) +
  geom_line(aes(Year,MWv5innovation),color="red") +
  theme_classic() + scale_x_continuous(breaks=c(1960,1980,2000,2020))

write.csv(dfMWv5,file="Data/output/MWv5TemperatureInnovation.csv")


monte.coeff <- mvrnorm(1000,MWv5mdl.coef,MWv5mdl.vcv)


shock <- array(dim=c(1000,13))
shock[,c(1:2)] <- 0
shock[,3] <- 1
for(i in c(4:13)){
  shock_temp <- monte.coeff[,1]+monte.coeff[,2]*shock[,i-1]+monte.coeff[,3]*shock[,i-2]+monte.coeff[,4]*shock[,i-3]
  shock[,i] <- shock_temp
}

shock.plot <- data.frame(year=c(1:10))
shock.plot$upper <- apply(shock,2,quantile,probs=0.95)[3:12]
shock.plot$lower <- apply(shock,2,quantile,probs=0.05)[3:12]
shock.plot$mean <- apply(shock,2,mean)[3:12]

ggplot(data=shock.plot) +
  geom_ribbon(aes(x=year,ymin=lower,ymax=upper), fill = "grey70",alpha=0.5) +
  geom_line(aes(year,mean))


