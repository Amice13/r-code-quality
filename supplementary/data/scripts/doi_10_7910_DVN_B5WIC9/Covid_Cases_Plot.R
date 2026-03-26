####################################
# Script Name: Covid Cases Graph
#
# Purpose: Create a graph that shows the number confirmed Covid-19 cases in Zambia and Malawi
#
# Author: Erica Ann Metheney
#
# Contact: data@gld.gu.se
#
# Instructions: 
#        1) Change the file path in the setwd command to be the Replication Materials folder
#
# Notes: 
#         Survey Dates
#         MCSR1: May 5, 2020 to June 2, 2020
#         ZCSR1: July 2, 2020 to August 13, 2020
#         MCSR3: March 9, 2021 to May 1, 2021
#
#         Variables to Use
#         new_cases_smoothed
#
#
######################################

# NECESSARY PACKAGES

library(readxl)
library(ggplot2)
library(dplyr)

# SET WORKING DIRECTORY

setwd("C:/Users/xmeter/University of Gothenburg/GLD Projects - Documents/Papers/Stereotypes Paper/Replication Materials")

# IMPORT DATA
owid_covid_data <- read_excel("Data/owid-covid-data.xlsx")
datMA =  subset(owid_covid_data,location == "Malawi")
datZA =  subset(owid_covid_data,location == "Zambia")
rm(owid_covid_data)

# CHANGE DATE FORMAT
datMA$Date = as.Date(datMA$date)
datZA$Date = as.Date(datZA$date)

# CREATE FIGURE
# 1. Add smoothed daily cases curve
# 2. Add vertical lines of survey dates.

colors = c("Zambia" = "blue","Zambia Smoothed" = "midnightblue","Malawi" = "grey75","Malawi Smoothed" = "grey23","Fielding" = "darkgrey")

df.dates = datMA$Date

ggplot() +
  annotate(geom = "rect",xmin=as.Date("2021-03-09"), xmax=as.Date("2021-05-01"), ymin=0, ymax=Inf,fill = "grey75",alpha = 0.5)+
  annotate(geom = "rect",xmin=as.Date("2020-07-02"), xmax=as.Date("2020-08-13"), ymin=0, ymax=Inf,fill = "black",alpha = 0.6)+
  annotate(geom = "rect",xmin=as.Date("2020-05-05"), xmax=as.Date("2020-06-02"), ymin=0, ymax=Inf,fill = "grey75",alpha = 0.5)+
  geom_line(data = datZA, aes(x=Date,y = new_cases_smoothed,group = 1,color = "new_cases_smoothed"),color = "black",size = 1.25)+
  geom_line(data = datMA, aes(x=Date,y = new_cases_smoothed,group = 1,color = "new_cases_smoothed"),size = 1.25)+
  scale_x_date(date_breaks = "months",date_labels = "%b %y")+
  scale_y_continuous(breaks = seq(0,2000,by = 250))+
  labs(y = "Number of Daily New Cases",x = "")+
  scale_color_manual("Legend",values = c("Zambia"="black","Malawi"="grey75"))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  theme_minimal()+
  theme(
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 90),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.key.size = unit(2,"line"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  )
