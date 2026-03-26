## R code for creating the figures for "Selecting the "Best"? Competing Dimensions of Politician Quality in the Developing World"?
##By Ananish Chaudhuri, Vegard Ivversen, Francesca R. Jensenius, Pushkar Maitra

library(foreign)
library(haven)
library(graphics)
library(estimatr)
library(Rmisc)
library(ltm)

setwd("/Users/francesj/UiO Dropbox/Francesca Refsum Jensenius/Shared/India_politician_changes/Selection_Paper/Replication Files") #Set to directory where the data is saved

dta<-read_stata("selection_paper.dta")
dim(dta)
names(dta)
head(dta)
table(dta$politician_status )
dta<-dta[dta$politician_status!=2,] #Remove outgoing politicians (experienced politicians not running re-election in 2018)

dta$group<-ifelse(dta$politician_status ==0, "Citizen", NA)
dta$group<-ifelse(dta$politician_status ==1, "Inexperienced", as.character(dta$group))
dta$group<-ifelse(dta$politician_status ==3, "Re-elected", as.character(dta$group))
dta$group<- factor(dta$group, levels=c("Citizen", "Inexperienced", "Re-elected"))
table(dta$group)

dta$group_small<- ifelse(dta$group=="Citizen", "Citizen", "Politician")
table(dta$group_small)

tapply(dta$age, dta$group, mean)
tapply(dta$leadershipexp_pol, dta$group, mean)

#COMPETENCE

pdf(file="Figures/Fig1_competence.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$educyrs_UPD, dta$group_small, mean)
CIs <- tapply(dta$educyrs_UPD, dta$group_small, CI, ci = 0.65)

m1<-lm_robust(educyrs_UPD~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

summary(m1)
summary(m2)

summary(m1)$coefficients[2,4]

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))


offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Educational attainment")
    ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

m1<-lm_robust(occupation_relevant ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$occupation_relevant, dta$group_small, mean)
CIs <- tapply(dta$occupation_relevant, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*3.5), main="Relevant occupation")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
  
     
### Raven score
m1<-lm_robust(ravensum ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)
summary(m1)


cor(dta$die_6[dta$group_small=="Politician"], dta$politicalknowledge[dta$group_small=="Politician"])
cor(dta$die_6[dta$group_small=="Politician"], dta$ravensum[dta$group_small=="Politician"])


m1<-lm_robust(die_6 ~ politicalknowledge, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group_small=="Politician",])
summary(m1)
m1<-lm_robust(die_6 ~ ravensum, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group_small=="Politician",])
summary(m1)


means <- tapply(dta$ravensum, dta$group_small, mean)
CIs <- tapply(dta$ravensum, dta$group_small, CI, ci = 0.65)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Cognitive ability")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

 ###politicalknowledge

m1<-lm_robust(politicalknowledge ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$politicalknowledge, dta$group_small, mean)
CIs <- tapply(dta$politicalknowledge, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Political knowledge")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   
  
   
###INTEGRITY

   
summary(dta$die_6)
summary(log(dta$landown+1))

#Checking for wealth effect
m_robust1<-lm_robust(die_6 ~ group_small, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust2<-lm_robust(die_6 ~ land_yes, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust3<-lm_robust(die_6 ~ group_small+ land_yes, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust4<-lm_robust(die_6 ~ log(landown+1), clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust5<-lm_robust(die_6 ~ group_small + log(landown+1), clusters=as.factor(GP), se_type = "stata", data=dta)

summary(m_robust4)

library(texreg)

texreg(list(m_robust1, m_robust2, m_robust3))
texreg(list(m_robust1, m_robust4, m_robust5))

m_robust6<-lm_robust(die_6 ~ ravensum, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust7<-lm_robust(die_6 ~ group_small +ravensum, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust8<-lm_robust(die_6 ~ group_small*ravensum, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust9<-lm_robust(die_6 ~ aspirepolitics_YN, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust10<-lm_robust(die_6 ~ group_small + aspirepolitics_YN, clusters=as.factor(GP), se_type = "stata", data=dta)
m_robust11<-lm_robust(die_6 ~ group_small*aspirepolitics_YN, clusters=as.factor(GP), se_type = "stata", data=dta)

texreg(list(m_robust6, m_robust7, m_robust8, m_robust9))
texreg(list(m_robust9, m_robust10, m_robust11))

pdf(file="Figures/Fig2_integrity.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$dictator_offer, dta$group_small, mean)
CIs <- tapply(dta$dictator_offer, dta$group_small, CI, ci = 0.65)

m1<-lm_robust(dictator_offer~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

summary(m1)


summary(m1)$coefficients[2,4]
summary(m2)$coefficients[2,4]

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Dictator game")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
 
   
###CP100
m1<-lm_robust(CP100 ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$CP100, dta$group_small, mean)
CIs <- tapply(dta$CP100, dta$group_small, CI, ci = 0.65)

summary(m1)


pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Contribution to public good")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   

##Trust game

m1<-lm_robust(propret_avg ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$propret_avg, dta$group_small, mean)
CIs <- tapply(dta$propret_avg, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Trust response")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)


###HONESTY
m1<-lm_robust(die_6 ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)
means <- tapply(dta$die_6, dta$group_small, mean)
CIs <- tapply(dta$die_6, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Die-tossing task")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   

###MOTIVATION

dta$Selfefficacy_index<-dta$efficacy_1_high+dta$efficacy_2_high+dta$efficacy_3_high #dta$efficacy_4_low #dta$efficacy_5_high

dta$Selfesteem_index<-(dta$selfesteem_2_high + dta$selfesteem_4_high+ dta$selfesteem_5_high+ dta$selfesteem_6_high)

dta$Trust_index<-(dta$trust_1_high + dta$trust_2_high + dta$trust_3_high + dta$trust_4_high)

cronbach.alpha(cbind(dta$efficacy_1_high,dta$efficacy_2_high,dta$efficacy_3_high))
cronbach.alpha(cbind(dta$selfesteem_2_high, dta$selfesteem_4_high, dta$selfesteem_5_high, dta$selfesteem_6_high)) 
cronbach.alpha(cbind(dta$trust_1_high, dta$trust_2_high, dta$trust_3_high, dta$trust_4_high)) #

table(dta$aspirepolitics_YN) 

tapply(dta$selfesteem_1_high, dta$group_small, mean)

pdf(file="Figures/Fig3_motivation.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$Selfefficacy_index, dta$group_small, mean)
CIs <- tapply(dta$Selfefficacy_index, dta$group_small, CI, ci = 0.65)

m1<-lm_robust(Selfefficacy_index~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

summary(m1)
summary(m1)$coefficients[2,4]

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Self-efficacy")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)


### Selfesteem_index

m1<-lm_robust(Selfesteem_index ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)
means <- tapply(dta$Selfesteem_index, dta$group_small, mean)
CIs <- tapply(dta$Selfesteem_index, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Self-esteem")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
   
   ###Trust_index
m1<-lm_robust(Trust_index ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$Trust_index, dta$group_small, mean)
CIs <- tapply(dta$Trust_index, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Trust")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
#Asirations
m1<-lm_robust(aspirepolitics_YN ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$aspirepolitics_YN, dta$group_small, mean)
CIs <- tapply(dta$aspirepolitics_YN, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Aspirations")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   
   ###INCLUSIVENESS
dta$minority<-ifelse(dta$hindu==0, 1, 0)

pdf(file="Figures/Fig4_inclusiveness.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

m1<-lm_robust(totalleadnum ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$totalleadnum, dta$group_small, mean)
CIs <- tapply(dta$totalleadnum, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*3), main="Politically networked family")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

### land_yes

m1<-lm_robust(land_yes ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$land_yes, dta$group_small, mean)
CIs <- tapply(dta$land_yes, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Land ownership")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   

###minority
   
m1<-lm_robust(minority ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$minority, dta$group_small, mean)
CIs <- tapply(dta$minority, dta$group_small, CI, ci = 0.65)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Religious minority")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
means <- tapply(dta$female, dta$group_small, mean)
CIs <- tapply(dta$female, dta$group_small, CI, ci = 0.65)


m1<-lm_robust(female~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

summary(m1)

pvals<- summary(m1)$coefficients[2,4]
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Share women")
        ylims <- max(means[1:2]) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1]), y1=c(CIs[[1]][3], CIs[[2]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   


##############SAME FOR CITIZENS -- INEXPERIENCED --EXPERIENCED

pdf(file="Figures/Fig1_competence_app.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$educyrs_UPD, dta$group, mean)
CIs <- tapply(dta$educyrs_UPD, dta$group, CI, ci = 0.65)


m1<-lm_robust(educyrs_UPD~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(educyrs_UPD~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

summary(m1)
summary(m2)

summary(m1)$coefficients[2,4]
summary(m2)$coefficients[2,4]

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))


offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Educational attainment")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)


m1<-lm_robust(occupation_relevant ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(occupation_relevant ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])


means <- tapply(dta$occupation_relevant, dta$group, mean)
CIs <- tapply(dta$occupation_relevant, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*3.5), main="Relevant occupation")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
  
     
   ### Raven score
  dta$group_small<-ifelse(dta$group=="Citizen", 1, NA)
  dta$group_small<-ifelse(dta$group=="Re-elected" | dta$group=="Inexperienced", 0, dta$group_small)

m1<-lm_robust(ravensum ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(ravensum ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(ravensum ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)


means <- tapply(dta$ravensum, dta$group, mean)
CIs <- tapply(dta$ravensum, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Cognitive ability")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

   
   ###politicalknowledge
m1<-lm_robust(politicalknowledge ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(politicalknowledge ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

means <- tapply(dta$politicalknowledge, dta$group, mean)
CIs <- tapply(dta$politicalknowledge, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Political knowledge")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   
      
   ###INTEGRITY

   
pdf(file="Figures/Fig2_integrity_app.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$dictator_offer, dta$group, mean)
CIs <- tapply(dta$dictator_offer, dta$group, CI, ci = 0.65)


m1<-lm_robust(dictator_offer~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(dictator_offer~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

summary(m1)
summary(m2)

summary(m1)$coefficients[2,4]
summary(m2)$coefficients[2,4]

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))


offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Dictator game")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
 
   
   ###CP100

m1<-lm_robust(CP100 ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(CP100 ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(CP100 ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Inexperienced",])


means <- tapply(dta$CP100, dta$group, mean)
CIs <- tapply(dta$CP100, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Contribution to public good")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   

##Trust game
  dta$group_small<-ifelse(dta$group=="Citizen", 1, NA)
  dta$group_small<-ifelse(dta$group=="Re-elected" | dta$group=="Inexperienced", 0, dta$group_small)

m1<-lm_robust(propret_avg ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(propret_avg ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(propret_avg ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Inexperienced",])
m3<-lm_robust(propret_avg ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)


means <- tapply(dta$propret_avg, dta$group, mean)
CIs <- tapply(dta$propret_avg, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Trust response")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

###HONESTY
  dta$group_small<-ifelse(dta$group=="Citizen", 1, NA)
  dta$group_small<-ifelse(dta$group=="Re-elected" | dta$group=="Inexperienced", 0, dta$group_small)

m1<-lm_robust(die_6 ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(die_6 ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(die_6 ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)

means <- tapply(dta$die_6, dta$group, mean)
CIs <- tapply(dta$die_6, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Die-tossing task")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   
   
   ###MOTIVATION

dta$Selfefficacy_index<-dta$efficacy_1_high+dta$efficacy_2_high+dta$efficacy_3_high #dta$efficacy_4_low #dta$efficacy_5_high

dta$Selfesteem_index<-(dta$selfesteem_2_high + dta$selfesteem_4_high+ dta$selfesteem_5_high+ dta$selfesteem_6_high)

dta$Trust_index<-(dta$trust_1_high + dta$trust_2_high + dta$trust_3_high + dta$trust_4_high)

cronbach.alpha(cbind(dta$efficacy_1_high,dta$efficacy_2_high,dta$efficacy_3_high))
cronbach.alpha(cbind(dta$selfesteem_2_high, dta$selfesteem_4_high, dta$selfesteem_5_high, dta$selfesteem_6_high)) 
cronbach.alpha(cbind(dta$trust_1_high, dta$trust_2_high, dta$trust_3_high, dta$trust_4_high)) #

table(dta$aspirepolitics_YN) 

tapply(dta$selfesteem_1_high, dta$group, mean)

pdf(file="Figures/Fig3_motivation_app.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))

means <- tapply(dta$Selfefficacy_index, dta$group, mean)
CIs <- tapply(dta$Selfefficacy_index, dta$group, CI, ci = 0.65)

m1<-lm_robust(Selfefficacy_index~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(Selfefficacy_index~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

summary(m1)
summary(m2)

summary(m1)$coefficients[2,4]
summary(m2)$coefficients[2,4]

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))


offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Self-efficacy")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)


### Selfesteem_index
m1<-lm_robust(Selfesteem_index ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(Selfesteem_index ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

means <- tapply(dta$Selfesteem_index, dta$group, mean)
CIs <- tapply(dta$Selfesteem_index, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Self-esteem")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
   
   ###Trust_index
m1<-lm_robust(Trust_index ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(Trust_index ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

means <- tapply(dta$Trust_index, dta$group, mean)
CIs <- tapply(dta$Trust_index, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Trust")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
#Asirations
  dta$group_small<-ifelse(dta$group=="Citizen", 1, NA)
  dta$group_small<-ifelse(dta$group=="Re-elected" | dta$group=="Inexperienced", 0, dta$group_small)

m1<-lm_robust(aspirepolitics_YN ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(aspirepolitics_YN ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(aspirepolitics_YN ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)


means <- tapply(dta$aspirepolitics_YN, dta$group, mean)
CIs <- tapply(dta$aspirepolitics_YN, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Aspirations")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   dev.off()
   

   ###INCLUSIVENESS

pdf(file="Figures/Fig4_inclusiveness_app.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(3, 3, 3, 2))


m1<-lm_robust(totalleadnum ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(totalleadnum ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])
m3<-lm_robust(totalleadnum ~group_small, clusters=as.factor(GP), se_type = "stata", data=dta)


means <- tapply(dta$totalleadnum, dta$group, mean)
CIs <- tapply(dta$totalleadnum, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)
summary(m3)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*3), main="Politically networked family")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)


### land_yes
m1<-lm_robust(land_yes ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(land_yes ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

means <- tapply(dta$land_yes, dta$group, mean)
CIs <- tapply(dta$land_yes, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Land ownership")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
   
   ###minority
m1<-lm_robust(minority ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(minority ~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

means <- tapply(dta$minority, dta$group, mean)
CIs <- tapply(dta$minority, dta$group, CI, ci = 0.65)

summary(m1)
summary(m2)

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))

offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Religious minority")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)
   
means <- tapply(dta$female, dta$group, mean)
CIs <- tapply(dta$female, dta$group, CI, ci = 0.65)

m1<-lm_robust(female~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Re-elected",])
m2<-lm_robust(female~group, clusters=as.factor(GP), se_type = "stata", data=dta[dta$group!="Citizen",])

summary(m1)
summary(m2)

summary(m1)$coefficients[2,4]
summary(m2)$coefficients[2,4]

pvals<- c(summary(m1)$coefficients[2,4], summary(m2)$coefficients[2,4])
pvals<-ifelse(pvals<0.01, "p<0.01", paste("p=", round(pvals,2), sep=""))


offset_y<-max(means)*0.15
offset_x<-0.1

myplot<-barplot(means, las=1, ylim=c(0, max(means)+  offset_y*2), main="Share women")
    ylims <- c(max(means[1:2]), max(means[2:3])) + offset_y*0.8
    segments(x0= myplot[-length(myplot)]+ offset_x, y0=ylims, x1= myplot[-1]-offset_x, y1=ylims)
    segments(x0=c(myplot[-length(myplot)]+ offset_x, myplot[-1]-offset_x),
             y0=ylims, y1=ylims-offset_y/3)
    text(myplot[-length(myplot)]+diff(myplot[1:2])/2, ylims+ offset_y/2, labels=pvals)
    text(myplot , 0, pos=3, labels=ifelse(means<1, signif(means, digits=2), round(means, digits=1)))  
arrows(x0=myplot, x1=myplot, y0=c(CIs[[1]][1], CIs[[2]][1], CIs[[3]][1]), y1=c(CIs[[1]][3], CIs[[2]][3], CIs[[3]][3]), lwd = 1, angle = 90, code = 3, length = 0.05)

   dev.off()
   