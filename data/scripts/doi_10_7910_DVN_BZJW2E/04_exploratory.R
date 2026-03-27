
##--------------------------------------
## Install and load any needed libraries
##--------------------------------------

library(ggplot2)
library(lubridate)

##--------------------------------------
## Load the data
##--------------------------------------

data <- read.csv("colorado_tds.csv", stringsAsFactors = FALSE)


head(data)

colnames(data)
dim(data)
head(data$date)
tail(data$date)



######################
# Time Series plots of TDS
######################


ggplot(data, aes(x=as.Date(date), y=lees))+
	geom_line()+
	xlab("Date")+
	ylab("Lees Ferry TDS")



ggplot(data, aes(x=as.Date(date), y=hoover))+
	geom_line()+
	xlab("Date")+
	ylab("Hoover Dam TDS")




