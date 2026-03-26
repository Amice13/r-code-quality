#PoP Graphics

#04/12/21

#Packages
require(ggplot2)
require(plm)
require(lattice)
require(latticeExtra)
require(plyr)
require(dplyr)

#R&R code:
#Load data daily case/policy: 093020
df<-read.csv(file.choose())
#Added population data into 091020 death data
deaths<-read.csv(file.choose())

#Changes to all graphs:
#Reviewer asked for a grouping of departments by category instead of alphabetically:
df$State_C = factor(df$State, levels=c('Cochabamba','La Paz','Oruro', 'Beni', 'Pando', 'Potosi', 'Chuquisaca', 'Santa Cruz', 'Tarija'))
deaths$State_C <- factor(deaths$Dept, levels=c('Cochabamba','La Paz','Oruro', 'Beni', 'Pando', 'Potosi', 'Chuquisaca', 'Santa Cruz', 'Tarija'))
#There is one NA row
df <- df[-c(1801), ]

#To show trends, make per 100,000 case and death data
df$CasesPer100000 <- (df$Daily_Cases/df$Population)*100000
df$DeathsPer100000 <- (df$Deaths_Daily/df$Population)*100000
deaths$COVIDPer100000 <- (deaths$Reported_COVID_Deaths/deaths$Pop)*100000
deaths$Total2020Per100000 <- (deaths$Total_Reported_SERECI_Deaths/deaths$Pop)*100000
deaths$AvgPer100000 <- (deaths$Month_Dept_Avg_Deaths/deaths$Pop)*100000

#Policy
p <- ggplot(df, aes(x = Days, y = Policy_Index_Adjusted_Time)) + geom_line() + 
  scale_x_continuous(name="Month", breaks=c(5,84,176), labels=c("March 10", "June 1", "Sept 1")) +
  ylab("Public policy index") + guides(fill=FALSE) + theme_minimal() + geom_vline(xintercept=84, linetype="dashed", color = "grey") +
  geom_vline(xintercept=176, linetype="dashed", color = "grey")
p + facet_wrap(~State_C)

#Mobility
b <- ggplot(df, aes(x = Days, y = Mobility_index)) + geom_line() + scale_x_continuous(name="Month", breaks=c(5,84,176), 
  labels=c("March 10", "June 1", "Sept 1")) +
  ylab("Daily cellphone mobility (% change from baseline)") + guides(fill=FALSE) + theme_minimal() + geom_hline(yintercept=0, linetype="dashed", color = "gray") + 
  geom_vline(xintercept=84, linetype="dashed", color = "grey") +
  geom_vline(xintercept=176, linetype="dashed", color = "grey")
  b+facet_wrap(~State_C)

#Cases
c <- ggplot(df, aes(x = Days, y = CasesPer100000)) + geom_line() + 
  scale_x_continuous(name="Month", breaks=c(5,84,176), labels=c("March 10", "June 1", "Sept 1")) +
  ylab("Daily cases per 100,000") + guides(fill=FALSE) + theme_minimal() + 
  geom_vline(xintercept=84, linetype="dashed", color = "grey") +
  geom_vline(xintercept=176, linetype="dashed", color = "grey")
c + facet_wrap(~State_C)

#Death data
ggplot(deaths, aes(x = Month1)) + 
  geom_line(aes(y = COVIDPer100000), linetype="dashed", color = "dark grey") +
  geom_line(aes(y = Total2020Per100000), color = "black") +
  geom_line(aes(y = AvgPer100000), color="grey") + 
  scale_x_continuous(name="Month", breaks=c(3.5,6,8), labels=c("March", "June", "September")) +
  ylab("Monthly deaths per 100,000") + 
  guides(fill=FALSE) + 
  theme_minimal() +
  facet_wrap(~State_C)

