
require(ggplot2)
require(directlabels)
require(plyr)
cuCountryData <- read.csv("Figure1-countryCuGraphsAUSJPNData.csv", header = TRUE)


AUS<-subset(cuCountryData, iso=="AUS")
ggplot(data=AUS, aes(x=year, y=cu_gdp), fill=iso, group=interaction(id, iso)) + 
  geom_bar(stat="identity", position="dodge", fill="grey", colour = "white", width=1) + 
  xlab("Year")+ylab("Current account balance (% GDP)")+ ggtitle("Australia") +
  theme_bw(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(1960, 2015), breaks=seq(1960, 2015, by=10))                                                                                                                                                                                                                                                   
ggsave(file="AustraliaCu1960-2015.pdf", width=4, height=4)

JPN<-subset(cuCountryData, iso=="JPN")
ggplot(data=JPN, aes(x=year, y=cu_gdp), fill=iso, group=interaction(id, iso)) + 
  geom_bar(stat="identity", position="dodge", fill="grey", colour = "white", width=1) + 
  xlab("Year")+ylab("Current account balance (% GDP)")+ggtitle("Japan") +
  theme_bw(base_size = 15) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(1960, 2015), breaks=seq(1960, 2015, by=10))                                                                                                                                                                                                                                                   
ggsave(file="JapanCu1960-2015.pdf", width=4, height=4)


