rm(list=ls())
library(tidyverse); library(scales);library(lubridate); library(ggpubr); library(ggsci); library(artyfarty)

setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.1/")


# Specify time period
min <- as.Date("2020-01-01")
max <- as.Date("2021-02-28")

# Import data from our world in data
global_dta <-read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/owid-covid-data.csv", sep=",", header = TRUE, stringsAsFactors = TRUE)
names(global_dta)

#Filter relavant data for Kenya
kenya_dta <-global_dta%>%
  filter(location=="Kenya")%>%
  select(location,date,total_cases,new_cases,new_cases_smoothed, stringency_index)%>%
  mutate(date=as.Date(date, format="%Y-%m-%d"))

#Plot the SARS-CoV-2 epidemic for Kenya up to February 2021
plot1 <- ggplot(kenya_dta, aes(x=date, y=new_cases_smoothed))+
  geom_line()+
  labs(y="+ves nationwide", x="")+
  theme_scientific()+
  scale_y_continuous(limits = c(0,1400), minor_breaks=seq(0, 1400, 200), breaks=seq(0, 1400, 400))+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"),limits = c(min, max))+
  geom_vline(xintercept = as.Date("2020-05-21"), size=0.25, linetype="longdash", color="red")+
  geom_vline(xintercept = as.Date("2020-09-16"), size=0.25, linetype="longdash", color="blue")+
  geom_text(x=as.Date("2020-03-15"), y=1300, label="Introductions", size=3)+
  geom_text(x=as.Date("2020-08-01"), y=1300, label="Wave 1", size=3)+
  geom_text(x=as.Date("2020-11-15"), y=1300, label="Wave 2", size=3)+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle =0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.65, 0.90),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=4, title = "County"), size=T)

pdf("Fig.1B.pdf", width = 4, height = 2, family = "Helvetica")
print(plot1)
dev.off()

#Plot the SARS-CoV-2 epidemic curve  for Coastal Kenya up to February 2021
coast_national_dta <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/coastal_counties_smoothed.csv", header = T, sep=",", stringsAsFactors = T)
coast_national_dta <- coast_national_dta%>%
  mutate(date=as.Date(date, "%d-%b-%y"))

plot2 <- ggplot(coast_national_dta, aes(x=date, y=Coast))+
  geom_line()+
  labs(y="+ves coastal counties", x="")+
  theme_scientific()+
  scale_y_continuous(limits = c(0,60), minor_breaks=seq(0, 60, 10), breaks=seq(0, 60, 20))+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"),limits = c(min, max))+
  geom_vline(xintercept = as.Date("2020-05-21"), size=0.25, linetype="longdash", color="red")+
  geom_vline(xintercept = as.Date("2020-09-16"), size=0.25, linetype="longdash", color="blue")+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle =0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.65, 0.90),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=4, title = "County"), size=T)
pdf("Fig.1C.pdf", width = 3.8, height = 2, family = "Helvetica")
print(plot2)
dev.off()


#Plot the COVID-19 stringency index for Kenya up to February 2021

plot3 <- ggplot(kenya_dta, aes(x=date, y=stringency_index))+
  geom_line()+
  labs(y="Stringency Index", x="Month in 2020/21")+
  theme_scientific()+
  scale_y_continuous(limits = c(0,100), minor_breaks=seq(0, 100, 12.5), breaks=seq(0, 100, 25))+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"), limits = c(min, max))+
  geom_vline(xintercept = as.Date("2020-05-21"), size=0.25, linetype="longdash", color="red")+
  geom_vline(xintercept = as.Date("2020-09-16"), size=0.25, linetype="longdash", color="blue")+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle =0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.65, 0.90),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=4, title = ""), size=T)

pdf("Fig.1D.pdf", width = 4, height = 2, family = "Helvetica")
print(plot3)
dev.off()