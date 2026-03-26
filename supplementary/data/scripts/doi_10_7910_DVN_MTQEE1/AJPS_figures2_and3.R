
#This file reproduces Figures 2 and 3 in manuscript.  It requires the Stata dataset 
#pooled_probs.dta and yearly_probs.dta (saved as Stata v. 12).  Note that newer versions
#of ggplot2 may have changed coding of the _bw theme used here.  If so, the plot's
#appearance may be slightly different than what is reported in the paper. 


library(lattice)
library(foreign)
library(gridExtra)
library(ggplot2)
library(grid)
library(ggthemes)


#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Dataverse files for AJPS")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")

#Input Data Set with Probability Estimates

d1<-read.dta("pooled_probs.dta")
attach(d1)

#Plot 1: Figure 2: Pooled Estimates 
# Grab data all groups
     contrast <- d1[c(1:12),]
     upper=contrast$pr1 + qnorm(.975)*contrast$se
     lower=contrast$pr1 + qnorm(.025)*contrast$se
     Probability<-contrast$pr1
     plotord2<-factor(contrast$plotord)

contrastdata<-cbind(contrast, upper, lower, Probability, plotord2)
     
gA<-ggplot(data = contrastdata, aes(x = plotord2, y = Probability, ymin = lower, ymax = upper, color=Group)) +
  geom_point(position = position_dodge(width = 0.4), size=2, aes(shape=Group)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.2)  +
  #coord_flip() + 
  ylim(.25, .75) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
   scale_colour_manual(values=c("gray60", "gray90", "gray80", "black"))+  
   #labs(title="Beliefs about Discrimination and Latino Attributes, 2002-2010") +
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
        legend.position=c(1,1),
        legend.key.size = unit(.5, "cm"),
        legend.text = element_text(size=6),
        #legend.text = element_text(size=6),
        plot.title = element_text(size=8)) 
 gAnew<-gA + facet_grid(. ~ yname)+theme(strip.text.x = element_text(size = 8))
 gAnew<-gAnew+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


#Plot 2: Yearly Estimates within group 

#Input Data Set with Probability Estimates


d3<-read.dta("pooled_probs.dta")
attach(d3)
#Plot 1: Figure 2: Pooled Estimates 
# Grab data all groups
     contrast2 <- d3[c(13:24),]
     upper=contrast2$pr1 + qnorm(.975)*contrast2$se
     lower=contrast2$pr1 + qnorm(.025)*contrast2$se
     Probability<-contrast2$pr1

plotord2<-factor(contrast2$plotord)

contrast2data<-cbind(contrast2, upper, lower, Probability, plotord2)

gC<-ggplot(data = contrast2data, aes(x = plotord2, y = Probability, ymin = lower, ymax = upper, color=Group)) +
  geom_point(position = position_dodge(width = 0.4), size=2, aes(shape=Group)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.2)  +
  #coord_flip() + 
  ylim(.25, .75) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
   scale_colour_manual(values=c("gray60", "gray90", "gray80", "black"))+  
   #labs(title="Beliefs about Discrimination and Latino Attributes, 2002-2010") +
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
        legend.position=c(1,1),
        legend.key.size = unit(.5, "cm"),
        legend.text = element_text(size=6),
        plot.title = element_text(size=8)) 
 gCnew<-gC + facet_grid(. ~ yname)+theme(strip.text.x = element_text(size = 8))
gCnew<-gCnew+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Plot 3: Yearly Estimates within group Group

#Input Data Set with Probability Estimates


d2<-read.dta("yearly_probs.dta")
attach(d2)

# Grab data all groups
     time <- d2[c(1:60),]
     upper=time$pr1 + qnorm(.975)*time$se
     lower=time$pr1 + qnorm(.025)*time$se
     Probability<-time$pr1
     gorder<-factor(time$grouporder)
timedata<-cbind(time, upper, lower, Probability, gorder)
#Plot 2: Yearly Estimates between-groups
gB<-ggplot(data = timedata, aes(x = gorder, y = Probability, ymin = lower, ymax = upper, color=Group)) +
  geom_point(position = position_dodge(width = 0.5), size=1.25, aes(shape=Group)) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.5)  +
  #coord_flip() + 
  ylim(.15, .85) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
   scale_colour_manual(values=c("gray60", "gray90", "gray80", "black"))+  
   #labs(title="Discrimination and Latino Attributes, By Year") +
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
        legend.justification=c(1,0),
        legend.position=c(1,0),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size=6),
        #legend.title=element_blank(),
        #legend.justification=c(1,1),
        #legend.position="none",
        #legend.key.size = unit(.5, "cm"),
        #legend.text = element_text(size=7),
        plot.title = element_text(size=10)) 
 gBnew<-gB + facet_grid(.~Year)+theme(strip.text.x = element_text(size = 8), strip.text.y = element_text(size=8))
gBnew<-gBnew+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))



d4<-read.dta("yearly_probs.dta")
attach(d4)

# Grab data all groups
     time2 <- d4[c(61:120),]
     upper=time2$pr1 + qnorm(.975)*time2$se
     lower=time2$pr1 + qnorm(.025)*time2$se
     Probability<-time2$pr1
     gorder<-factor(time2$grouporder)
time2data<-cbind(time2, upper, lower, Probability, gorder)
#Plot 2: Yearly Estimates between-groups
gD<-ggplot(data = time2data, aes(x = gorder, y = Probability, ymin = lower, ymax = upper, color=Group)) +
  geom_point(position = position_dodge(width = 0.7), size=1.25, aes(shape=Group)) +
  geom_errorbar(position = position_dodge(width = 0.7), width = 0.5)  +
  #coord_flip() + 
  ylim(.15, .85) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
   scale_colour_manual(values=c("gray60", "gray90", "gray80", "black"))+  
   #labs(title="Discrimination and Latino Attributes, By Year") +
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
        legend.position=c(1,1),
        legend.key.size = unit(.3, "cm"),
        legend.text = element_text(size=6),
        #legend.title=element_blank(),
        #legend.justification=c(1,1),
        #legend.position="none",
        #legend.key.size = unit(.5, "cm"),
        #legend.text = element_text(size=7),
        plot.title = element_text(size=10)) 
 gDnew<-gD + facet_grid(.~Year)+theme(strip.text.x = element_text(size = 8), strip.text.y = element_text(size=8))
gDnew<-gDnew+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


#Figure 2
gAB<-grid.arrange(gAnew, gBnew, nrow=2, top="Proximity to 'canonical immigrant' associated with beliefs about group discrimination")

#Figure 3
gCD<-grid.arrange(gCnew, gDnew, nrow=2, top="Reports of experienced discrimination exhibit no clear pattern")


rm(list=ls())
