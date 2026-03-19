

#This file reproduces Figure 5 in manuscript.  It requires the Stata dataset 
#outcome_measures.dta (saved as Stata v. 12).  Note that newer versions
#of ggplot2 may have changed coding of the _bw theme used here.  If so, the plot's
#appearance may be slightly different than what is reported in the paper. Also it 
#may be necessary to first generate the individual plots and then execute the grid.arrange
#cmd.  For some reason, this plot will not always compile correctly if the entire code
#is run.  


library(lattice)
library(foreign)
library(gridExtra)
library(grid)
library(ggthemes)
library(ggplot2)




#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")

#Input Data Set with Probability Estimates

d1<-read.dta("outcome_measures.dta")
attach(d1)


#Plot 1: All Beliefs
# Grab data 
all <- d1[c(126:150),]
     upper=exp(all$b1 + qnorm(.975)*all$se)
     lower=exp(all$b1 + qnorm(.025)*all$se)
     upper2=all$or1 + qnorm(.975)*all$orse
     lower2=all$or1 + qnorm(.025)*all$orse
     OR<-all$or1

alldata<-cbind(all, upper, lower, upper2, lower2, OR)

dall=alldata[order(alldata$zratio),]
dall$depmeasure=factor(dall$depmeasure,levels=dall$depmeasure)
g1<-ggplot(data =dall, aes(x = factor(depmeasure), y = zratio, color = group)) +
  geom_point(position = position_dodge(width = 0.75)) +
  #geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-6.5, 6.5) + geom_hline(yintercept=2.00, color="darkgrey", linetype="dashed") + 
  geom_hline(yintercept=-2.00, color="darkgrey", linetype="dashed") +
  scale_colour_manual(values = c("gray40")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g1new<-g1 + facet_grid(.~varname)+theme(strip.text.x = element_text(size = 8))
 #g1new<-g1new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#panel.background = element_blank(), axis.line = element_line(colour = "black"))

#############


#Plot 2: All Direct
# Grab data 
allD <- d1[c(351:375),]
     upper=exp(allD$b1 + qnorm(.975)*allD$se)
     lower=exp(allD$b1 + qnorm(.025)*allD$se)
     upper2=allD$or1 + qnorm(.975)*allD$orse
     lower2=allD$or1 + qnorm(.025)*allD$orse
     OR<-allD$or1

allDdata<-cbind(allD, upper, lower, upper2, lower2, OR)

dallD=allDdata[order(allDdata$zratio),]
dallD$depmeasure=factor(dallD$depmeasure,levels=dallD$depmeasure)
g2<-ggplot(data =dallD, aes(x = factor(depmeasure), y = zratio, color = group)) +
  geom_point(position = position_dodge(width = 0.75)) +
  #geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-6.5, 6.5) + geom_hline(yintercept=2.00, color="darkgrey", linetype="dashed") + 
  geom_hline(yintercept=-2.00, color="darkgrey", linetype="dashed") +
  scale_colour_manual(values = c("gray70")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.title=element_blank(),
        legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g2new<-g2 + facet_grid(.~varname)+theme(strip.text.x = element_text(size = 8))
 #g2new<-g2new+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#panel.background = element_blank(), axis.line = element_line(colour = "black"))

#############

gA<-grid.arrange(g1new, g2new, nrow=1, ncol=2, top=textGrob("Implications of Discrimination on Social and Political Attitudes", gp=gpar(fontsize=10)))