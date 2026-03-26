# library
install.packages(c("gdata"))
library(gdata)

# Read in the data
partyLeaderDat <- read.xls("Party_leader_data.xlsx",sheet=1,header=T)

# Get rid of special issue parties from the analysis
partyLeaderDat <- subset(partyLeaderDat, parfam_subject!="sip special issue")

# Identify whether party is left or right
partyLeaderDat$left <- ifelse(partyLeaderDat$PartyCamp==1,1,0)

# T.tests
t.test(partyLeaderDat$Age~partyLeaderDat$left) 
t.test(partyLeaderDat$years_in_nat~partyLeaderDat$left)
t.test(partyLeaderDat$Highest_edu_nr~partyLeaderDat$left)
t.test(partyLeaderDat$Tweets~partyLeaderDat$left)
partyLeaderDat$years_on_twitter <- 2019-partyLeaderDat$On.Twitter.since
t.test(partyLeaderDat$years_on_twitter~partyLeaderDat$left) 
t.test(partyLeaderDat$Facebook.likes~partyLeaderDat$left)
t.test(partyLeaderDat$Size.of.PR.office~partyLeaderDat$left)

#########################
##### Figure OA6.1
#########################

pdf("plot_media_savvy_comparisons.pdf",width=7.5,height=7)

par(mfrow=c(2,4))

range(partyLeaderDat$years_in_nat)
left <- subset(partyLeaderDat, left==1)$years_in_nat
right <- subset(partyLeaderDat, left==0)$years_in_nat
m        = c(mean(left,na.rm=T), mean(right,na.rm=T))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m, ylim=c(0,20),xpd=FALSE, ylab="Years in National Office",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$Size.of.PR.office)
left <- subset(partyLeaderDat, left==1)$Size.of.PR.office
right <- subset(partyLeaderDat, left==0)$Size.of.PR.office
m        = c(mean(left,na.rm=T), mean(right,na.rm=T))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m,ylim=c(0,7), xpd=FALSE, ylab="Size of PR Office",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$Highest_edu_nr)
left <- subset(partyLeaderDat, left==1)$Highest_edu_nr
right <- subset(partyLeaderDat, left==0)$Highest_edu_nr
m        = c(mean(left), mean(right))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m, ylim=c(0,4), xpd=FALSE, ylab="Education Level",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$Age)
left <- subset(partyLeaderDat, left==1)$Age
right <- subset(partyLeaderDat, left==0)$Age
m        = c(mean(left), mean(right))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m, ylim=c(0,60), xpd=FALSE, ylab="Age",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$Tweets)
left <- subset(partyLeaderDat, left==1)$Tweets
right <- subset(partyLeaderDat, left==0)$Tweets
m        = c(mean(left,na.rm=T), mean(right))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m, ylim=c(0,20000), xpd=FALSE, ylab="Tweets",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$years_on_twitter)
left <- subset(partyLeaderDat, left==1)$years_on_twitter
right <- subset(partyLeaderDat, left==0)$years_on_twitter
m        = c(mean(left,na.rm=T), mean(right))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m,ylim=c(0,10), xpd=FALSE, ylab="Years on Twitter",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

range(partyLeaderDat$Facebook.likes)
left <- subset(partyLeaderDat, left==1)$Facebook.likes
right <- subset(partyLeaderDat, left==0)$Facebook.likes
m        = c(mean(left,na.rm=T), mean(right,na.rm=T))
names(m) = c("Left","Right")
se       = c(sd(left,na.rm=T)/sqrt(length(na.omit(left))), 
             sd(right,na.rm=T)/sqrt(length(na.omit(right))))
bp = barplot(m, ylim=c(0,300000), xpd=FALSE, ylab="Facebook Likes",cex.names=1.5,cex.axis=1.5,cex.lab=1.5)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

dev.off()