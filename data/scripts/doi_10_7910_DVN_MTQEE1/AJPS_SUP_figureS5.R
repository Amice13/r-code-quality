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


#Plot 1: Fence
# Grab data 
fence <- d1[c(37:54),]
     upper=(fence$b1 + qnorm(.975)*fence$se)
     lower=(fence$b1 + qnorm(.025)*fence$se)
     upper2=fence$or1 + qnorm(.975)*fence$orse
     lower2=fence$or1 + qnorm(.025)*fence$orse
     OR<-fence$or1

fencedata<-cbind(fence, upper, lower, upper2, lower2, OR)

require(dplyr)
fencedata <- fencedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dfence=fencedata[order(fencedata$alpha_group),]
#dfence$group=factor(dfence$alpha_group,levels=dfence$alpha_group)
fence<-ggplot(data =fencedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
fencenew<-fence + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############
#Border Patrol#

d2<-read.dta("outcome_measures_sorted.dta")
attach(d2)


# Grab data 
patrol <- d2[c(55:72),]
     upper=(patrol$b1 + qnorm(.975)*patrol$se)
     lower=(patrol$b1 + qnorm(.025)*patrol$se)
     upper2=patrol$or1 + qnorm(.975)*patrol$orse
     lower2=patrol$or1 + qnorm(.025)*patrol$orse
     OR<-patrol$or1

patroldata<-cbind(patrol, upper, lower, upper2, lower2, OR)

require(dplyr)
patroldata <- patroldata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dpatrol=patroldata[order(patroldata$alpha_group),]
#dpatrol$group=factor(dpatrol$alpha_group,levels=dpatrol$alpha_group)
patrol<-ggplot(data =patroldata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-2, 2) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
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
patrolnew<-patrol + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#License#

d3<-read.dta("outcome_measures_sorted.dta")
attach(d3)


# Grab data 
license <- d2[c(145:162),]
     upper=(license$b1 + qnorm(.975)*license$se)
     lower=(license$b1 + qnorm(.025)*license$se)
     upper2=license$or1 + qnorm(.975)*license$orse
     lower2=license$or1 + qnorm(.025)*license$orse
     OR<-license$or1

licensedata<-cbind(license, upper, lower, upper2, lower2, OR)

require(dplyr)
licensedata <- licensedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dlicense=licensedata[order(licensedata$alpha_group),]
#dlicense$group=factor(dlicense$alpha_group,levels=dlicense$alpha_group)
license<-ggplot(data =licensedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.75, 1.75) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
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
licensenew<-license + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############



#############
#ID Card#

d4<-read.dta("outcome_measures_sorted.dta")
attach(d4)


# Grab data 
idcard <- d4[c(181:198),]
     upper=(idcard$b1 + qnorm(.975)*idcard$se)
     lower=(idcard$b1 + qnorm(.025)*idcard$se)
     upper2=idcard$or1 + qnorm(.975)*idcard$orse
     lower2=idcard$or1 + qnorm(.025)*idcard$orse
     OR<-idcard$or1

idcarddata<-cbind(idcard, upper, lower, upper2, lower2, OR)

require(dplyr)
idcarddata <- idcarddata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#didcard=idcarddata[order(idcarddata$alpha_group),]
#didcard$group=factor(didcard$alpha_group,levels=didcard$alpha_group)
idcard<-ggplot(data =idcarddata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 2.5) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
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
idcardnew<-idcard + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#############
#Local Attention#

d5<-read.dta("outcome_measures_sorted.dta")
attach(d5)


# Grab data 
attn <- d5[c(217:234),]
     upper=(attn$b1 + qnorm(.975)*attn$se)
     lower=(attn$b1 + qnorm(.025)*attn$se)
     upper2=attn$or1 + qnorm(.975)*attn$orse
     lower2=attn$or1 + qnorm(.025)*attn$orse
     OR<-attn$or1

attndata<-cbind(attn, upper, lower, upper2, lower2, OR)

require(dplyr)
attndata <- attndata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dattn=attndata[order(attndata$alpha_group),]
#dattn$group=factor(dattn$alpha_group,levels=dattn$alpha_group)
attn<-ggplot(data =attndata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
attnnew<-attn + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############

#############
#Local Police#

d6<-read.dta("outcome_measures_sorted.dta")
attach(d6)


# Grab data 
police <- d6[c(235:252),]
     upper=(police$b1 + qnorm(.975)*police$se)
     lower=(police$b1 + qnorm(.025)*police$se)
     upper2=police$or1 + qnorm(.975)*police$orse
     lower2=police$or1 + qnorm(.025)*police$orse
     OR<-police$or1

policedata<-cbind(police, upper, lower, upper2, lower2, OR)

require(dplyr)
policedata <- policedata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dpolice=policedata[order(policedata$alpha_group),]
#dpolice$group=factor(dpolice$alpha_group,levels=dpolice$alpha_group)
police<-ggplot(data =policedata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
policenew<-police + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#Punishment
#############

d7<-read.dta("outcome_measures_sorted.dta")
attach(d7)



# Grab data 
punish <- d7[c(289:306),]
     upper=(punish$b1 + qnorm(.975)*punish$se)
     lower=(punish$b1 + qnorm(.025)*punish$se)
     upper2=punish$or1 + qnorm(.975)*punish$orse
     lower2=punish$or1 + qnorm(.025)*punish$orse
     OR<-punish$or1

punishdata<-cbind(punish, upper, lower, upper2, lower2, OR)

require(dplyr)
punishdata <- punishdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dpunish=punishdata[order(punishdata$alpha_group),]
#dpunish$group=factor(dpunish$alpha_group,levels=dpunish$alpha_group)
punish<-ggplot(data =punishdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
  geom_point(position = position_dodge(width = 0.75)) +
  geom_errorbar(position = position_dodge(width = 0.75), width = 0.1) +
  coord_flip() + 
  ylim(-1.5, 2.05) + geom_hline(yintercept=0.00, color="darkgrey", linetype="dashed") + 
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
punishnew<-punish + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############

#############
#Troops
d8<-read.dta("outcome_measures_sorted.dta")
attach(d8)



# Grab data 
troops <- d8[c(397:414),]
     upper=(troops$b1 + qnorm(.975)*troops$se)
     lower=(troops$b1 + qnorm(.025)*troops$se)
     upper2=troops$or1 + qnorm(.975)*troops$orse
     lower2=troops$or1 + qnorm(.025)*troops$orse
     OR<-troops$or1

troopsdata<-cbind(troops, upper, lower, upper2, lower2, OR)

require(dplyr)
troopsdata <- troopsdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#dtroops=troopsdata[order(troopsdata$alpha_group),]
#dtroops$group=factor(dtroops$alpha_group,levels=dtroops$alpha_group)
troops<-ggplot(data =troopsdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
troopsnew<-troops + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############


#############
#Raids

d9<-read.dta("outcome_measures_sorted.dta")
attach(d9)



# Grab data 
raids <- d9[c(433:450),]
     upper=(raids$b1 + qnorm(.975)*raids$se)
     lower=(raids$b1 + qnorm(.025)*raids$se)
     upper2=raids$or1 + qnorm(.975)*raids$orse
     lower2=raids$or1 + qnorm(.025)*raids$orse
     OR<-raids$or1

raidsdata<-cbind(raids, upper, lower, upper2, lower2, OR)

require(dplyr)
raidsdata <- raidsdata %>% mutate(alpha_group = factor(alpha_group), 
                    alpha_group = factor(alpha_group, levels = rev(levels(alpha_group))))

#draids=raidsdata[order(raidsdata$alpha_group),]
#draids$group=factor(draids$alpha_group,levels=draids$alpha_group)
raids<-ggplot(data =raidsdata, aes(x = alpha_group, y = b1, ymin=lower, ymax=upper, color = depmeasure)) +
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
raidsnew<-raids + facet_grid(depmeasure~varname)+theme(strip.text = element_text(size = 7))

#############






grid.arrange(fencenew, patrolnew, licensenew, idcardnew, attnnew, policenew,      punishnew, troopsnew, raidsnew, ncol=3, nrow=3, top="Latino Attitudes on Immigration Policy and Discrimination Reporting" )



