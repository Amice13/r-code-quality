#Code for plotting pairwise distances
#Written by:  Dr Nyaigoti Agoti

# First import all necessary libraries
rm(list=ls())
library(tidyverse)
library(extrafont)
library(reshape2)
library(scales)
library(lubridate)
library(artyfarty)
library(janitor)

setwd("/Users/cnyaigoti/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.5/")
#color_coast <- c("#A9A9A9","#FF0000","#00FF00",'#FFA500',"#FF00FF")# 
color_coast <- c("#A9A9A9","#FF0000","#00FF00",'#0000FF','#FFA500',"#00FFFF","#FF00FF")
coast_counties <- c("Mombasa", "Kilifi", "Kwale", "Taita_Taveta", "Tana_River", "Lamu")

my_data <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Data/subsampled_global/subsampleone/tempest/new_tree.csv", stringsAsFactors = F, sep = ",", header=T)%>%
  separate(tip, into=c("lineage", "location", "sample_id", "collectiondate"), sep="\\|", remove=F)%>%
  mutate(site=ifelse(location%in%coast_counties, location, "Global" ))%>%
  mutate(site=recode(site, Taita_Taveta ="Taita Taveta", Tana_River="Tana River"))%>%
  mutate(site=factor(site, levels=c("Global","Mombasa",	"Kilifi",	 "Kwale","Taita Taveta",  "Tana River",	 "Lamu")))%>%
  mutate(sampledate=as.Date(collectiondate, format="%Y-%m-%d"))

figure <-my_data%>%
  ggplot(aes(x=sampledate, y=distance))+
  geom_point(aes(color=site), size=1.5)+
  theme_scientific()+
  scale_color_manual(values = color_coast)+
  labs(x="", y="Root-to-tip divergence")+
  stat_smooth(se=T, method="lm", colour="black", size=1)+
  scale_x_date(breaks ="3 month", date_minor_breaks="1 month", labels = date_format("%b\n%Y"))+
  scale_y_continuous(limits=c(0,0.002), minor_breaks = seq(0 , 0.002, 0.0005), breaks = seq(0 , 0.002, 0.0005))+
  #scale_y_continuous(minor_breaks = c(0 , 0.001, 0.005), breaks = c(0 , 0.003, 0.001))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none",
        legend.key.size = unit(0.15, "cm"),
        legend.spacing.x = unit(0.15, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_blank(),
        legend.box.background = element_blank())+
  guides(color=guide_legend(ncol=1, title = "County"), size=T)

lm_eqn <- function(my_data){
  m <-lm(my_data$distance ~ my_data$sampledate, my_data);
  eq <- substitute(italic(y) == a +b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a=format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 =format(summary(m)$r.squared, digits=3)))
  as.character(as.expression(eq));
}

figure  <- figure + annotate("text", label = lm_eqn(my_data), parse=TRUE, x = as.Date("2021-02-15", format="%Y-%m-%d"), 
                             y =  0.0018, size =3, colour ="black", hjust=1)


pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.5/Fig.5B.pdf", width = 3.5, height = 4.5, family = "Helvetica")
print(figure)
dev.off()

