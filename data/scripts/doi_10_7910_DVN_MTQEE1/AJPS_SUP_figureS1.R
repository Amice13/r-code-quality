#This code produces figure S1, which is an analysis of greencard holders

library(lattice)
library(foreign)
library(gridExtra)
library(ggplot2)
library(grid)
library(ggthemes)

#setwd("/Users/bsjjones/Dropbox/AJPS R&R/Analysis Files")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Dataverse Files for AJPS")

#Input Data Set with Probability Estimates

d2<-read.dta("yearly_greencard.dta")
attach(d2)

# Grab data all groups
     time <- d2[c(1:8),]
     upper=time$pr1 + qnorm(.975)*time$se
     lower=time$pr1 + qnorm(.025)*time$se
     Probability<-time$pr1
     #gorder<-factor(time$grouporder)
timedata<-cbind(time, upper, lower, Probability)
#Plot 2: Yearly Estimates between-groups
gB<-ggplot(data = timedata, aes(x = Status, y = Probability, ymin = lower, ymax = upper, color=Status)) +
  geom_point(position = position_dodge(width = 0.2), size=1) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.05)  +
  #coord_flip() + 
  ylim(.15, .85) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
   scale_colour_manual(values=c("darkorange", "cadetblue3"))+  
   labs(title="Discrimination and Greencard Status") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.ticks.x = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position="bottom",
        legend.key.size = unit(.5, "cm"),
        legend.text = element_text(size=7),
        plot.title = element_text(size=10)) 
 gBnew<-gB + facet_grid(yname~Year)+theme(strip.text.x = element_text(size = 8), strip.text.y = element_text(size=8))
 
 gBnew
