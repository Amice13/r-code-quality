library(lattice)
library(foreign)
library(gridExtra)
library(ggplot2)
library(grid)

#setwd("/Users/bsjjones/Dropbox/AJPS R&R/Analysis Files")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")
#setwd("/Users/bsjjones/Dropbox/Miscellanous")
#setwd("/Users/bradfordjones/Dropbox/AJPS R&R/Analysis Files")

#Input Data Set with Probability Estimates

d1<-read.dta("outcome_measures_sorted.dta")
attach(d1)


#Plot 1: Trust-in-Government
# Grab data 
trust <- d1[c(415:432),]
     upper=(trust$b1 + qnorm(.975)*trust$se)
     lower=(trust$b1 + qnorm(.025)*trust$se)
     upper2=trust$or1 + qnorm(.975)*trust$orse
     lower2=trust$or1 + qnorm(.025)*trust$orse
     OR<-trust$or1

trustdata<-cbind(trust, upper, lower, upper2, lower2, OR)

require(dplyr)
trustdata <- trustdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dtrust=trustdata[order(trustdata$alpha_group),]
#dtrust$group=factor(dtrust$alpha_group,levels=dtrust$alpha_group)
g1<-ggplot(data =trustdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.5) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g1new<-g1 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############
#Quality of Life#

d2<-read.dta("outcome_measures_sorted.dta")
attach(d2)


# Grab data 
quality <- d2[c(307:324),]
     upper=(quality$b1 + qnorm(.975)*quality$se)
     lower=(quality$b1 + qnorm(.025)*quality$se)
     upper2=quality$or1 + qnorm(.975)*quality$orse
     lower2=quality$or1 + qnorm(.025)*quality$orse
     OR<-quality$or1

qualitydata<-cbind(quality, upper, lower, upper2, lower2, OR)

require(dplyr)
qualitydata <- qualitydata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dquality=qualitydata[order(qualitydata$alpha_group),]
#dquality$group=factor(dquality$alpha_group,levels=dquality$alpha_group)
g2<-ggplot(data =qualitydata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.5) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g2new<-g2 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#Self Identification as American#

d3<-read.dta("outcome_measures_sorted.dta")
attach(d3)


# Grab data 
selfid <- d2[c(325:342),]
     upper=(selfid$b1 + qnorm(.975)*selfid$se)
     lower=(selfid$b1 + qnorm(.025)*selfid$se)
     upper2=selfid$or1 + qnorm(.975)*selfid$orse
     lower2=selfid$or1 + qnorm(.025)*selfid$orse
     OR<-selfid$or1

selfiddata<-cbind(selfid, upper, lower, upper2, lower2, OR)

require(dplyr)
selfiddata <- selfiddata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dselfid=selfiddata[order(selfiddata$alpha_group),]
#dselfid$group=factor(dselfid$alpha_group,levels=dselfid$alpha_group)
g3<-ggplot(data =selfiddata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.5) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g3new<-g3 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#Beliefs about shared Identity#

d4<-read.dta("outcome_measures_sorted.dta")
attach(d4)


# Grab data 
share <- d4[c(343:360),]
     upper=(share$b1 + qnorm(.975)*share$se)
     lower=(share$b1 + qnorm(.025)*share$se)
     upper2=share$or1 + qnorm(.975)*share$orse
     lower2=share$or1 + qnorm(.025)*share$orse
     OR<-share$or1

sharedata<-cbind(share, upper, lower, upper2, lower2, OR)

require(dplyr)
sharedata <- sharedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dshare=sharedata[order(sharedata$alpha_group),]
#dshare$group=factor(dshare$alpha_group,levels=dshare$alpha_group)
g4<-ggplot(data =sharedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.7) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g4new<-g4 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#############
#Beliefs about talkimmd Identity#

d5<-read.dta("outcome_measures_sorted.dta")
attach(d5)


# Grab data 
talkimm <- d5[c(379:396),]
     upper=(talkimm$b1 + qnorm(.975)*talkimm$se)
     lower=(talkimm$b1 + qnorm(.025)*talkimm$se)
     upper2=talkimm$or1 + qnorm(.975)*talkimm$orse
     lower2=talkimm$or1 + qnorm(.025)*talkimm$orse
     OR<-talkimm$or1

talkimmdata<-cbind(talkimm, upper, lower, upper2, lower2, OR)

require(dplyr)
talkimmdata <- talkimmdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dtalkimm=talkimmdata[order(talkimmdata$alpha_group),]
#dtalkimm$group=factor(dtalkimm$alpha_group,levels=dtalkimm$alpha_group)
g5<-ggplot(data =talkimmdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.6, 2.02) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g5new<-g5 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############

#############
#Acceptance of Immigrants#

d6<-read.dta("outcome_measures_sorted.dta")
attach(d6)


# Grab data 
accept <- d6[c(1:18),]
     upper=(accept$b1 + qnorm(.975)*accept$se)
     lower=(accept$b1 + qnorm(.025)*accept$se)
     upper2=accept$or1 + qnorm(.975)*accept$orse
     lower2=accept$or1 + qnorm(.025)*accept$orse
     OR<-accept$or1

acceptdata<-cbind(accept, upper, lower, upper2, lower2, OR)

require(dplyr)
acceptdata <- acceptdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#daccept=acceptdata[order(acceptdata$alpha_group),]
#daccept$group=factor(daccept$alpha_group,levels=daccept$alpha_group)
g6<-ggplot(data =acceptdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.6, 2.02) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g6new<-g6 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############

#Latino on Ballot

d7<-read.dta("outcome_measures_sorted.dta")
attach(d7)



# Grab data 
ballot <- d7[c(199:216),]
     upper=(ballot$b1 + qnorm(.975)*ballot$se)
     lower=(ballot$b1 + qnorm(.025)*ballot$se)
     upper2=ballot$or1 + qnorm(.975)*ballot$orse
     lower2=ballot$or1 + qnorm(.025)*ballot$orse
     OR<-ballot$or1

ballotdata<-cbind(ballot, upper, lower, upper2, lower2, OR)

require(dplyr)
ballotdata <- ballotdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dballot=ballotdata[order(ballotdata$alpha_group),]
#dballot$group=factor(dballot$alpha_group,levels=dballot$alpha_group)
g7<-ggplot(data =ballotdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.90) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g7new<-g7 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#############
#Descriptive Representation

d8<-read.dta("outcome_measures_sorted.dta")
attach(d8)



# Grab data 
descript <- d8[c(127:144),]
     upper=(descript$b1 + qnorm(.975)*descript$se)
     lower=(descript$b1 + qnorm(.025)*descript$se)
     upper2=descript$or1 + qnorm(.975)*descript$orse
     lower2=descript$or1 + qnorm(.025)*descript$orse
     OR<-descript$or1

descriptdata<-cbind(descript, upper, lower, upper2, lower2, OR)

require(dplyr)
descriptdata <- descriptdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#ddescript=descriptdata[order(descriptdata$alpha_group),]
#ddescript$group=factor(ddescript$alpha_group,levels=ddescript$alpha_group)
g8<-ggplot(data =descriptdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.90) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g8new<-g8 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############
#Worry about Deportation

d9<-read.dta("outcome_measures_sorted.dta")
attach(d9)



# Grab data 
worry <- d9[c(109:126),]
     upper=(worry$b1 + qnorm(.975)*worry$se)
     lower=(worry$b1 + qnorm(.025)*worry$se)
     upper2=worry$or1 + qnorm(.975)*worry$orse
     lower2=worry$or1 + qnorm(.025)*worry$orse
     OR<-worry$or1

worrydata<-cbind(worry, upper, lower, upper2, lower2, OR)

require(dplyr)
worrydata <- worrydata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dworry=worrydata[order(worrydata$alpha_group),]
#dworry$group=factor(dworry$alpha_group,levels=dworry$alpha_group)
g9<-ggplot(data =worrydata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 1.90) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
  scale_colour_manual(values = c("deepskyblue3")) + 
  ##labs(y="Z-ratio for Log-Odds Estimate", x="Outcome Measure") +
  theme_bw() +
  theme(#panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
        #panel.grid.major.x = element_blank(),
        #panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size=6),
        axis.text.x = element_text(size=6),
         axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        #axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g9new<-g9 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############






grid.arrange(g1new, g2new, g9new, g6new, g3new, g4new, g5new, g7new, g8new, ncol=3, nrow=3, top="Latino Attitudes and Discrimination Reporting" )



