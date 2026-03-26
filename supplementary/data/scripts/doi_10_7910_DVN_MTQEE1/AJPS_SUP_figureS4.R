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


#Plot 1: Become Citizen
# Grab data 
become <- d1[c(19:36),]
     upper=(become$b1 + qnorm(.975)*become$se)
     lower=(become$b1 + qnorm(.025)*become$se)
     upper2=become$or1 + qnorm(.975)*become$orse
     lower2=become$or1 + qnorm(.025)*become$orse
     OR<-become$or1

becomedata<-cbind(become, upper, lower, upper2, lower2, OR)

require(dplyr)
becomedata <- becomedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dbecome=becomedata[order(becomedata$alpha_group),]
#dbecome$group=factor(dbecome$alpha_group,levels=dbecome$alpha_group)
g1<-ggplot(data =becomedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g1new<-g1 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############
#Constitution#

d2<-read.dta("outcome_measures_sorted.dta")
attach(d2)


# Grab data 
const <- d2[c(91:108),]
     upper=(const$b1 + qnorm(.975)*const$se)
     lower=(const$b1 + qnorm(.025)*const$se)
     upper2=const$or1 + qnorm(.975)*const$orse
     lower2=const$or1 + qnorm(.025)*const$orse
     OR<-const$or1

constdata<-cbind(const, upper, lower, upper2, lower2, OR)

require(dplyr)
constdata <- constdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dconst=constdata[order(constdata$alpha_group),]
#dconst$group=factor(dconst$alpha_group,levels=dconst$alpha_group)
g2<-ggplot(data =constdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g2new<-g2 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#Speak English#

d3<-read.dta("outcome_measures_sorted.dta")
attach(d3)


# Grab data 
speak <- d2[c(361:378),]
     upper=(speak$b1 + qnorm(.975)*speak$se)
     lower=(speak$b1 + qnorm(.025)*speak$se)
     upper2=speak$or1 + qnorm(.975)*speak$orse
     lower2=speak$or1 + qnorm(.025)*speak$orse
     OR<-speak$or1

speakdata<-cbind(speak, upper, lower, upper2, lower2, OR)

require(dplyr)
speakdata <- speakdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dspeak=speakdata[order(speakdata$alpha_group),]
#dspeak$group=factor(dspeak$alpha_group,levels=dspeak$alpha_group)
g3<-ggplot(data =speakdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g3new<-g3 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#Citizen Influence#

d4<-read.dta("outcome_measures_sorted.dta")
attach(d4)


# Grab data 
influence <- d4[c(73:90),]
     upper=(influence$b1 + qnorm(.975)*influence$se)
     lower=(influence$b1 + qnorm(.025)*influence$se)
     upper2=influence$or1 + qnorm(.975)*influence$orse
     lower2=influence$or1 + qnorm(.025)*influence$orse
     OR<-influence$or1

influencedata<-cbind(influence, upper, lower, upper2, lower2, OR)

require(dplyr)
influencedata <- influencedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dinfluence=influencedata[order(influencedata$alpha_group),]
#dinfluence$group=factor(dinfluence$alpha_group,levels=dinfluence$alpha_group)
g4<-ggplot(data =influencedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g4new<-g4 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#############
#Follow Politics#

d5<-read.dta("outcome_measures_sorted.dta")
attach(d5)


# Grab data 
follow <- d5[c(163:180),]
     upper=(follow$b1 + qnorm(.975)*follow$se)
     lower=(follow$b1 + qnorm(.025)*follow$se)
     upper2=follow$or1 + qnorm(.975)*follow$orse
     lower2=follow$or1 + qnorm(.025)*follow$orse
     OR<-follow$or1

followdata<-cbind(follow, upper, lower, upper2, lower2, OR)

require(dplyr)
followdata <- followdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dfollow=followdata[order(followdata$alpha_group),]
#dfollow$group=factor(dfollow$alpha_group,levels=dfollow$alpha_group)
g5<-ggplot(data =followdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g5new<-g5 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############

#############
#Most Concern#

d6<-read.dta("outcome_measures_sorted.dta")
attach(d6)


# Grab data 
concern <- d6[c(253:270),]
     upper=(concern$b1 + qnorm(.975)*concern$se)
     lower=(concern$b1 + qnorm(.025)*concern$se)
     upper2=concern$or1 + qnorm(.975)*concern$orse
     lower2=concern$or1 + qnorm(.025)*concern$orse
     OR<-concern$or1

concerndata<-cbind(concern, upper, lower, upper2, lower2, OR)

require(dplyr)
concerndata <- concerndata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dconcern=concerndata[order(concerndata$alpha_group),]
#dconcern$group=factor(dconcern$alpha_group,levels=dconcern$alpha_group)
g6<-ggplot(data =concerndata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g6new<-g6 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############

#Pol Dont Care

d7<-read.dta("outcome_measures_sorted.dta")
attach(d7)



# Grab data 
dontcare <- d7[c(271:288),]
     upper=(dontcare$b1 + qnorm(.975)*dontcare$se)
     lower=(dontcare$b1 + qnorm(.025)*dontcare$se)
     upper2=dontcare$or1 + qnorm(.975)*dontcare$orse
     lower2=dontcare$or1 + qnorm(.025)*dontcare$orse
     OR<-dontcare$or1

dontcaredata<-cbind(dontcare, upper, lower, upper2, lower2, OR)

require(dplyr)
dontcaredata <- dontcaredata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#ddontcare=dontcaredata[order(dontcaredata$alpha_group),]
#ddontcare$group=factor(ddontcare$alpha_group,levels=ddontcare$alpha_group)
g7<-ggplot(data =dontcaredata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
        #legend.title=element_blank(),
        #legend.justification=c(1,0),
        legend.position="none",
        plot.title = element_text(size=10)) 
g7new<-g7 + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


grid.arrange(g1new, g2new, g3new, g4new, g7new, g5new, g6new, ncol=3, nrow=3, top="Latino Attitudes and Discrimination Reporting" )



