# Plotting lineages by county # Figure 3
#1. Clear any data in memory, load necessary packages and set working directory

rm(list=ls())
library(tidyverse); library(stringr); library(lubridate); library(scales); library(RColorBrewer)
library(wesanderson); library(artyfarty); library(janitor)

setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.3/")
min <- as.Date("2020-03-01")
max <- as.Date("2021-02-28")

#2. Import all required data
load("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.3/Lineages_dta_11Aug2021.RData")
names(Lineages_dta)

#Identify lineages to show by assessing distribution: If Frequency is >5 OR Variant of Interest OR varaint of Concern

#lineage_distribution <- tabyl(county_lineage_dta, Lineage)%>%
#  rename(Freq=n)%>%
#  filter(Freq>5 |Lineage=="B.1.1.7"|Lineage=="B.1.525"|Lineage=="A.23")

major_lineages <- c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                    "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8")


county_lineages_dta <- Lineages_dta%>%
  mutate(Count=1)%>%
  mutate(Lineage=as.character(Lineage))%>%
  mutate(key_lineages=ifelse(Lineage%in%major_lineages, Lineage, "Other Kenya"))%>%
  mutate(key_lineages=factor(key_lineages, levels=c("A","A.23","A.23.1","B","B.1","B.1.1","B.1.1.33","B.1.1.7","B.1.333",
                                                    "B.1.351","B.1.525","B.1.530","B.1.535","B.1.549","B.1.596.1","B.1.612","N.8", "Other Kenya")))%>%
  mutate(studyphase=as.character(ifelse(datecollection <"2020-05-21", "Introductions phase", "Wave 1")))%>%
  mutate(studyphase= replace(studyphase, datecollection >"2020-09-16", "Wave 2"))%>%
  mutate(phase_code=as.character(ifelse(studyphase=="Introductions phase", "IN", " Other")))%>%
  mutate(phase_code=replace(phase_code,studyphase=="Wave 1", "W1"))%>%
  mutate(phase_code=replace(phase_code,studyphase=="Wave 2","W2"))%>%
  mutate(phase_code=factor(phase_code, levels=c("IN", "W1", "W2")))


key_lineages<-tabyl(county_lineages_dta, Lineage)






  
plot_color=c("#000000","#C0C0C0","#696969","#FF0000","#F2D2BD",
             "#800000","#C9A9A6","#00FF00","#008000","#00FFFF","#BA6B57", "#FFA500","#9933FF",
             "#A4C639","#0000FF","#CCCCFF","#FF00FF","#55ACEE")

#1. Plot weekly pango lineage distribution by county across the study period
plot1 <- ggplot(county_lineages_dta, aes(x=Forthnight, y=Count,fill=key_lineages))+
  stat_summary(fun=sum, geom="bar", position = "fill")+
  scale_fill_manual(values=plot_color)+
  labs(y="Biweekly fraction of genomes", x="Month in 2020/21")+
  theme_scientific()+
  scale_x_date(breaks ="2 month", date_minor_breaks="1 month", labels = date_format("%b"), limits = c(min,max))+
  facet_grid(county_rep~., scales="free")+
  facet_wrap(county_rep~., ncol=2)+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 9, angle=0),
        strip.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = "bottom",
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.20, 'cm'),
        legend.spacing.y = unit(0.20, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 11),
        strip.background = element_rect(fill="white", color = "white"),
        panel.spacing.x = unit(1.2,"lines"),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(nrow=2, title = "Lineage"), size=T)
pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.3/Fig.3.pdf", width =8, height = 5)
print(plot1)
dev.off()