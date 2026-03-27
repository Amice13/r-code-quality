rm(list = ls())

library(tidyverse); library(dplyr);library("lubridate"); library(ggtree); library(tidytree)
library(treeio); library(artyfarty);library(wesanderson); library(janitor)

setwd("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.6/B.1.596.1")

tree<-read.newick('~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.6/B.1.596.1/timetree.nwk')

coastal_counties <- c("Mombasa", "Kilifi", "Kwale", "Taita Taveta", "Tana River", "Lamu")
color_coast <- c("#FF0000","#00FF00","#00FFFF",'#FFA500','#0000FF',"#FF00FF")


metadata_df <- read.csv("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.6/B.1.596.1/meta_globe+Ken_subsampleone_B.1.596.1_wave2.csv")%>%
  mutate(coastal=ifelse(county_origin%in%coastal_counties, "Kenya", "Global"))%>%
  mutate(origin=ifelse(coastal=="Kenya", county_origin, coastal))%>%
  mutate(origin=factor(origin, levels=c("Global", "Mombasa", "Kilifi", "Kwale", "Taita Taveta", "Tana River", "Lamu")))

recent_date <- format(date_decimal(max(metadata_df$date)), "%Y-%m-%d")
recent_date


p <- ggtree(tree, mrsd=recent_date,as.Date=TRUE, color='grey40',size=0.2) + theme_tree2()+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "3 month", minor_breaks = "1 month")+
  expand_limits(y = 60)+
  theme(axis.text.x = element_text(size=10,angle=0))
p


global_tre <-  p%<+% metadata_df+ 
  geom_tippoint(aes(subset=(coastal=='Kenya')),fill='white',size=2,stroke=0.2,color='grey60',shape=21)+
  geom_tippoint(aes(subset=(coastal=='Kenya'),fill=origin,),size=2, stroke=0.2,shape=21)+
  scale_color_manual(values = color_coast)+
  scale_fill_manual(values = color_coast)+
  #scale_fill_manual(values=c(wes_palette(5, name = "Darjeeling1", type = "discrete")[c(-4,-5)],"black",wes_palette(5, name = "Cavalcanti1", type = "discrete"),terrain.colors(2),"khaki","gray77",rainbow(10)[c(-1,-2,-4)],as.character(wes_palette(4, name = "Royal1", type = "discrete")),as.character(wes_palette(5, name = "Zissou1", type = "discrete")),"pink","gray32","tomato1"))+
  labs(y="Number of sequences", x="", title = "")+
  theme_scientific()+
  #scale_x_continuous(labels = function(x)format(x, scientific = FALSE))+
  #scale_x_continuous(limits=c(0,0.0004), minor_breaks = seq(0 , 0.0004, 0.00005), breaks = seq(0 , 0.0004, 0.0001))+
  scale_y_continuous(limits=c(0,60), minor_breaks = seq(0 , 60, 10), breaks = seq(0 , 60, 20))+
  theme(axis.title.x = element_text(size = 12),
        axis.title.y =  element_text(size = 12),
       # axis.ticks.y =element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y= element_text(size = 12),
        #panel.grid.minor = element_blank(),
        #panel.grid.major=element_blank(),
        #axis.line.y = element_blank(),
        #legend.position = c(0.50, 0.98),
       legend.position = "none",
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        legend.background = element_rect(fill="#FFFFFF", color = NA),
        #panel.grid = element_line(size=0.0, color="white"),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=2, title = "County"), size=T)

  
pdf("~/Dropbox/COVID-19/SECONDWAVE/revisedcode/Figures/Fig.6/B.1.596.1/Fig.6D.pdf", width = 3.5, height = 4)
print(global_tre)
dev.off()
