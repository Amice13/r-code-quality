
#This file reproduces Figure 1 in manuscript.  It requires the Stata dataset 
#figure1_probabilities.dta (saved as Stata v. 12).  Note that newer versions
#of ggplot2 may have changed coding of the _bw theme used here.  If so, the plot's
#appearance may be slightly different than what is reported in the paper. Also note that
#in the lower left panel, the result for bilingual and English language is plotted 
#in a different order than in main paper (in the manuscript, English is plotted first followed
#by bilinguals).


library(lattice)
library(foreign)
library(gridExtra)
library(grid)
library(ggplot2)
library(ggthemes)


#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Dataverse files for AJPS")

#Input Data Set with Probability Estimates

d1<-read.dta("figure1_probabilities.dta")
attach(d1)

#Plot 1: Citizenship Differences 

# Grab data for citizen/noncitizen contrast
cit <- d1[c(1:2, 16:17),]
    upper=cit$pr1 + qnorm(.975)*cit$se
     lower=cit$pr1 + qnorm(.025)*cit$se
     Probability<-cit$pr1

citdata<-cbind(cit,  Probability)

g1<-ggplot(data = citdata, aes(x = group, y = Probability, ymin = lower, ymax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.1) +
  #coord_flip() + 
  ylim(.2, .8) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
  scale_colour_manual(values = c("black", "gray50")) + 
  labs(title="Citizens and Noncitizens", x="Citizenship Status") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.text = element_text(size=6),
         legend.key.size = unit(.25, "cm"),
        plot.title = element_text(size=10)) 
g1new<-g1 + facet_grid(. ~ depmeasure)+theme(strip.text.x = element_text(size = 8))
g1new<-g1new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))

     
#Plot 2: Generational Differences 
 
# Grab data for citizen/noncitizen contrast
gen <- d1[c(3:7, 18:22),]
   upper=gen$pr1 + qnorm(.975)*gen$se
     lower=gen$pr1 + qnorm(.025)*gen$se
     Probability<-gen$pr1

gendata<-cbind(gen,  Probability)

g2<-ggplot(data = gendata, aes(x = group, y = Probability,  ymin = lower, ymax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.1) +
  #coord_flip() + 
  ylim(.2, .9) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
  scale_colour_grey() + 
  labs(title="Generational Status", x="Generation") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.text = element_text(size=6),
        legend.key.size = unit(.25, "cm"),
        plot.title = element_text(size=10)) 
g2new<-g2 + facet_grid(. ~ depmeasure)+theme(strip.text.x = element_text(size = 8))
g2new<-g2new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


     
#Plot 3: Language Differences for Beliefs Item
# Grab data for citizen/noncitizen contrast
lang <- d1[c(8:10, 23:25),]
upper=lang$pr1 + qnorm(.975)*lang$se
     lower=lang$pr1 + qnorm(.025)*lang$se
     Probability<-lang$pr1
  
langdata<-cbind(lang,  Probability)
     
langdata=langdata[order(langdata$plotorder),]
langdata$group=factor(langdata$group,levels=langdata$group)
g3<-ggplot(data = langdata, aes(x = factor(group), y = Probability,  ymin = lower, ymax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.1) +
  #coord_flip() + 
  ylim(.2, .9) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
  scale_colour_grey() + 
  labs(title="Language Use", x="Language") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.text = element_text(size=6),
        legend.key.size = unit(.25, "cm"),
        plot.title = element_text(size=10)) 
g3new<-g3 + facet_grid(. ~ depmeasure)+theme(strip.text.x = element_text(size = 8))
g3new<-g3new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))



#Plot 4: Time Differences for Beliefs Item
# Grab data for citizen/noncitizen contrast
time <- d1[c(11:15, 26:30),]

upper=time$pr1 + qnorm(.975)*time$se
     lower=time$pr1 + qnorm(.025)*time$se
     Probability<-time$pr1
  
timedata<-cbind(time,  Probability)
     

g4<-ggplot(data = timedata, aes(x = factor(group), y = Probability,  ymin = lower, ymax = upper, color = group)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(position = position_dodge(width = 0.4), width = 0.1) +
  #coord_flip() + 
  ylim(.2, .9) + geom_hline(yintercept=.50, color="lightgrey", linetype="dashed") + 
  scale_colour_grey() + 
  labs(title="Yearly Measures", x="Year") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        legend.title=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.text = element_text(size=6),
        legend.key.size = unit(.25, "cm"),
        plot.title = element_text(size=10)) 
g4new<-g4 + facet_grid(. ~ depmeasure)+theme(strip.text.x = element_text(size = 8))
g4new<-g4new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))


#pdf("figure1_citizen.pdf", width=4, height=6)
figure1<-grid.arrange(g1new, g2new, g3new, g4new, ncol=2, nrow=2, top="Discrimination, Latino Attributes, and Time")
#dev.off()
