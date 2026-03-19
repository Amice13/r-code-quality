
#load packages
#Figure 8
rm(list=ls())

library("readxl"); library(tidyverse); library(ggrepel); library(ggalluvial); library(lubridate); library(treeio); 
library(RColorBrewer); library(wesanderson);library(artyfarty); library(scales); library(devtools);library(janitor)


plot_color <- c("#FF0000", "#FFFF00", "#00EAFF", "#AA00FF", "#FF7F00", "#BFFF00", "#0095FF","#FF00AA","#FFD400", "#6AFF00", "#0040FF","#EDB9B9", "#B9D7ED", 
                "#E7E9B9", "#DCB9ED", "#B9EDE0", "#8F2323", "#23628F", "#8F6A23", "#6B238F", "#4F8F23", "#000000", "#737373", "#CCCCCC")

# Set working directiory
setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.8/")

#import data from the treetime output
importexport<-read.csv('./annottated_tree_events.csv')
importexport$EventTime<-as.numeric(importexport$EventTime)
importexport$Date <- as.Date(format(date_decimal(importexport$EventTime), "%Y-%m-%d"))
importexport$Connection <- factor(paste(importexport$Origin,importexport$Destination, sep="-"))
importexport$Monthly=as.Date(cut(importexport$Date,breaks = "1 month"))
importexport$BiWeekly=as.Date(cut(importexport$Date,breaks = "2 weeks"))
importexport$Weekly=as.Date(cut(importexport$Date,breaks = "1 week"))


importexport <- importexport%>%
  mutate(EventType=ifelse(Origin!="Global", "Intercounty", "Import"))%>%
  mutate(EventType=ifelse(Destination=="Global", "Export", EventType))%>%
  filter(Origin!='UNKNOWN')%>%
  mutate(studyphase=as.character(ifelse(Date <"2020-05-21", "Introductions phase", "Wave 1")))%>%
  mutate(studyphase= replace(studyphase, Date >"2020-09-16", "Wave 2"))%>%
  mutate(phase_code=as.character(ifelse(studyphase=="Introductions phase", "IN", " Other")))%>%
  mutate(phase_code=replace(phase_code,studyphase=="Wave 1", "W1"))%>%
  mutate(phase_code=replace(phase_code,studyphase=="Wave 2","W2"))%>%
  mutate(phase_code=factor(phase_code, levels=c("IN", "W1", "W2")))

#Generating the alluvium plot/flow diagram
Plot1<-ggplot(subset(importexport,Connection!='Global-Global' & Connection!='UNKNOWN-Global'),
               aes(axis1 = Origin, axis2 = Destination)) +
  geom_alluvium(width = 1/7, aes(fill=EventType),decreasing = TRUE, alpha=0.65) +
  geom_stratum(width = 1/7, fill='white', color='black', decreasing = TRUE) +
  geom_label_repel(label.padding=0.25, size=3,stat = "stratum", min.y = 2,decreasing = TRUE, colour = "black", aes(label = after_stat(stratum)))+ ## Note: Can be uncommented to show country labels
  #scale_fill_manual(values=c(plot_color, wes_palette(2, name = "Darjeeling1", type = "discrete")[c(-4,-5)],"black",wes_palette(5, name = "Cavalcanti1", type = "discrete"),terrain.colors(2),"khaki","gray77",rainbow(10)[c(-1,-2,-4)],as.character(wes_palette(4, name = "Royal1", type = "discrete")),as.character(wes_palette(5, name = "Zissou1", type = "discrete")),"pink","gray32","tomato1"))+
  scale_fill_manual(values=plot_color[c(4,9,18)])+
  scale_x_discrete(limits = c("Origin", "Destination"), expand = c(.08, .08)) +
  scale_y_continuous(limits = c(0,360), minor_breaks = seq(0,360, 30), breaks = seq(0,360,60))+
  labs(y="Number of Events")+
  #theme_minimal()+
  theme_scientific() +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.position = "bottom",
        #legend.position = c(0.80, 0.80),
        legend.key.size = unit(0.35, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 11),
        legend.title =element_text(size = 11),
        legend.background = element_rect(fill="#FFFFFF", color = NA),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(nrow=1, title = "Event Type", title.position = "left"), size=T)

pdf("Fig.8A.pdf", width = 4, height =4)
Plot1
dev.off()


Plot2<- importexport%>%
  mutate(present=1)%>%
  group_by(Monthly, EventType)%>%
  summarise(Freq=sum(present))%>%
  ggplot(aes(x=Monthly, y=Freq))+
  #scale_fill_manual(values = wes_palette(n=3, name="GrandBudapest1"))+
  scale_fill_manual(values = pal("fancy"))+
  geom_col(aes(fill=EventType), color="black")+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "3 month")+
  scale_y_continuous(limits = c(0,45), breaks = seq(0,45,10), minor_breaks = seq(0,45, 5))+
  labs(x="", y="Count")+
  theme_scientific()+
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.45, 0.90),
        #legend.position = "bottom",
        legend.key.size = unit(0.35, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        legend.spacing.y = unit(0.15, 'cm'),
        legend.text = element_text(size = 11),
        legend.title =element_text(size = 11),
        legend.background=element_rect(fill = alpha("white", 0)))+
  #legend.box.background = element_blank())+
  guides(fill=guide_legend(nrow=1, title = "Event Type"), size=T)
pdf("Fig.8B.pdf", width = 4, height = 4)
print(Plot2)
dev.off()



#Mombasa
color_coast <- c("#A9A9A9","#FF0000","#00FF00",'#0000FF',"#00FFFF",'#FFA500',"#FF00FF")# 
#color_coast <- c("#A9A9A9","#FF0000","#00FF00","#00FFFF",'#FFA500',"#FF00FF")# 

import_dta<-subset(importexport,(Destination!='Global'))%>%
  mutate(county=Destination)%>%
  mutate(county=factor(county, levels=c("Global","Mombasa",	"Kilifi",	 "Kwale","Taita Taveta",  "Tana River",	 "Lamu")))%>%
  mutate(Origin=factor(Origin, levels=c("Global","Mombasa",	"Kilifi",	 "Kwale","Taita Taveta",  "Tana River",	 "Lamu")))%>%
  group_by(Monthly, county,Origin)%>%
  mutate(present=1)%>%
  summarise(Freq=sum(present))%>%
  ggplot(aes(x=Monthly, y=Freq))+
  geom_col(aes(fill=Origin), width = 24)+
  facet_wrap(~county)+
  #scale_color_manual(values = color_coast)+
  scale_fill_manual(values = color_coast)+
  theme_scientific()+
  facet_grid(county_rep~., scales="free")+
  facet_wrap(county~., ncol=3)+
  scale_x_date(breaks ="3 month", date_minor_breaks="1 month", labels = date_format("%b\n%Y"))+
  labs(y="Monthly # of import events", x="")+
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle=0),
        strip.text.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = "bottom",
        legend.key.size = unit(0.20, "cm"),
        legend.spacing.x = unit(0.20, 'cm'),
        legend.spacing.y = unit(0.20, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_text(size = 10),
        strip.background = element_rect(fill="white", color = "white"),
        panel.spacing.x = unit(1.2,"lines"),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(nrow=1, title = "Key (Origin):"), size=T)
pdf("Fig.8.pdf", width =8, height = 4)
print(import_dta)
dev.off()

