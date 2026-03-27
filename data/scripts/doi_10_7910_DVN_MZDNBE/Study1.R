rm(list=ls())

library(texreg)
library(lmtest)

setwd("/Users/amandaclayton/Dropbox/All Male Panels/2 - Male Ambition/Replication Files")

modeldata<-read.csv('Study1.csv')

modeldata<-subset(modeldata, modeldata$ManCheckAll ==1) #only those that pass manipulation checks

Men<-subset(modeldata, Female==0) #
Women<-subset(modeldata, Female==1) #

#Treatment: 1 = Inclusive, 2 = FF, 3 = Control 

##Values for Table 1: Combined Ambition Scale
##AMBITION
tapply(Women $Ambition, Women $Treatment, mean)
t.test(Women $Ambition[Women $Treatment==2], Women $Ambition[Women $Treatment==3]) #FF v. Control
t.test(Women $Ambition[Women $Treatment==1], Women $Ambition[Women $Treatment==3]) #IF v. Control


tapply(Men $Ambition, Men $Treatment, mean)
t.test(Men $Ambition[Men $Treatment==1], Men $Ambition[Men $Treatment==3]) 
t.test(Men $Ambition[Men $Treatment==2], Men $Ambition[Men $Treatment==3]) 

#Men v. Women 

#Difference b/n men and women in the control condition 
t.test(Men $Ambition[Men $Treatment==3], Women $Ambition[Women $Treatment==3]) #men: 1.95 v. women: 1.77 sig at 0.05

MenMeans<-tapply(Men $Ambition, Men $Treatment, mean)[c(2,3,1)]
WomenMeans<-tapply(Women $Ambition, Women $Treatment, mean)[c(2,3,1)]

t.test(Men $Ambition[Men $Treatment==2], Men $Ambition[Men $Treatment==3]) #FF v. Control
t.test(Men $Ambition[Men $Treatment==1], Men $Ambition[Men $Treatment==3]) #IF v. Control


##FIGURE 4
par(mfrow=c(1,2))

barplot(MenMeans, beside=T, main="Reported Ambition \n Men", cex.main=0.8, ylim=c(1,4), xpd=FALSE, ylab=c("mean"), xaxt="n", args.legend = list(x = "topleft", cex = .8))
axis(1, at=c(0.7, 1.9, 3.1), labels=c("Founding \n Fathers", "Documents \n (Control)", "Inclusive \n Founders"), cex.axis=0.7)
box(bty="l")
segments(x0= 0.7, y0= 2.239556 +(1* 0.09126728), x1= 0.7, y1= 2.239556-(1* 0.09126728), lwd=2, col='black')
segments(x0=1.9, y0= 1.945347 +(1* 0.09157411), x1=1.9, y1= 1.945347-(1* 0.09157411), lwd=2, col='black')
segments(x0= 3.1, y0= 1.937456 +(1* 0.0954143), x1= 3.1, y1= 1.937456-(1* 0.0954143), lwd=2, col='black')


barplot(WomenMeans, beside=T, main="Reported Ambibition \n Women", cex.main=0.8, ylim=c(1,4), xpd=FALSE, ylab=c("mean"), xaxt="n", args.legend = list(x = "topleft", cex = .8))
axis(1, at=c(0.7, 1.9, 3.1), labels=c("Founding \n Fathers", "Documents \n (Control)", "Inclusive \n Founders"), cex.axis=0.7)
box(bty="l")
segments(x0= 0.7, y0= 1.896480 +(1* 0.09582971), x1= 0.7, y1= 1.896480-(1* 0.09582971), lwd=2, col='black')
segments(x0=1.9, y0= 1.766789 +(1* 0.09493772), x1=1.9, y1= 1.766789-(1* 0.09493772), lwd=2, col='black')
segments(x0= 3.1, y0= 1.961884 +(1* 0.08724469), x1= 3.1, y1= 1.961884-(1* 0.08724469), lwd=2, col='black')

t.test(Women $Ambition[Women $Treatment==2], Women $Ambition[Women $Treatment==3]) #FF v. Control
t.test(Women $Ambition[Women $Treatment==1], Women $Ambition[Women $Treatment==3]) #IF v. Control

t.test(Women $Ambition[Women $Treatment==2], Men $Ambition[Men $Treatment==2]) #men v. women in FF
t.test(Women $Ambition[Women $Treatment==3], Men $Ambition[Men $Treatment==3]) #men v. women in control

##Values for Table 2

prop.table(table(Men $RunOffice[Men $Treatment==2]))
prop.table(table(Men $RunOffice[Men $Treatment==3]))

#White men v. Non-white men 

WhiteMen<-subset(Men, White ==1) #n =
tapply(WhiteMen $Ambition, WhiteMen $Treatment, mean)

t.test(WhiteMen $Ambition[WhiteMen $Treatment==1], WhiteMen $Ambition[WhiteMen $Treatment==3]) #not sign
t.test(WhiteMen $Ambition[WhiteMen $Treatment==2], WhiteMen $Ambition[WhiteMen $Treatment==3]) #highly sig

NonwhiteMen<-subset(Men, White ==0) #n = 98
tapply(NonwhiteMen $Ambition, NonwhiteMen $Treatment, mean)

t.test(NonwhiteMen $Ambition[NonwhiteMen $Treatment==1], NonwhiteMen $Ambition[NonwhiteMen $Treatment==3]) #not sig
t.test(NonwhiteMen $Ambition[NonwhiteMen $Treatment==2], NonwhiteMen $Ambition[NonwhiteMen $Treatment==3]) #not sig

WMenMeans<-tapply(WhiteMen $Ambition, WhiteMen $Treatment, mean)[c(2,3,1)]
NWMenMeans<-tapply(NonwhiteMen $Ambition, NonwhiteMen $Treatment, mean)[c(2,3,1)]

##FIGURE 5
par(mfrow=c(1,2))

barplot(WMenMeans, beside=T, main="Reported Ambition \n White Men", cex.main=0.8, ylim=c(1,4), xpd=FALSE, ylab=c("mean"), xaxt="n", args.legend = list(x = "topleft", cex = .8))
axis(1, at=c(0.7, 1.9, 3.1), labels=c("Founding \n Fathers", "Documents \n (Control)", "Inclusive \n Founders"), cex.axis=0.7)
box(bty="l")
segments(x0= 0.7, y0= 2.178608 +(1* 0.0994536), x1= 0.7, y1= 2.178608-(1* 0.0994536), lwd=2, col='black')
segments(x0=1.9, y0= 1.851448 +(1* 0.0994536), x1=1.9, y1= 1.851448-(1* 0.0994536), lwd=2, col='black')
segments(x0= 3.1, y0= 1.861823 +(1* 0.0994536), x1= 3.1, y1= 1.861823-(1* 0.0994536), lwd=2, col='black')


barplot(NWMenMeans, beside=T, main="Reported Ambibition \n Non-White Men", cex.main=0.8, ylim=c(1,4), xpd=FALSE, ylab=c("mean"), xaxt="n", args.legend = list(x = "topleft", cex = .8))
axis(1, at=c(0.7, 1.9, 3.1), labels=c("Founding \n Fathers", "Documents \n (Control)", "Inclusive \n Founders"), cex.axis=0.7)
box(bty="l")
segments(x0= 0.7, y0= 2.428652 +(1* 0.2363997), x1= 0.7, y1= 2.428652-(1* 0.2363997), lwd=2, col='black')
segments(x0=1.9, y0= 2.344415 +(1* 0.2363997), x1=1.9, y1= 2.344415-(1* 0.2363997), lwd=2, col='black')
segments(x0= 3.1, y0= 2.215589 +(1* 0.2363997), x1= 3.1, y1= 2.215589-(1* 0.2363997), lwd=2, col='black')

