#
#  Barry Edwards
#  Mediation research replication material
#
#  Remove all objects just to be safe
#
rm(list=ls(all=TRUE))
#
#

######################################### LOAD THE DATA 

systemData <- read.csv("http://www.poliscidata.com/replication/systemSettlementRates.csv")




######################################### CREATE PLOT (FIGURE 1) 


# PLEASE NOTE: You may wish to set a working directory on your hard drive (setwd) uncomment the following
# two lines to create high-resolution figures.
# 
# setwd('H:/mediation data/final figures/')
# png(file="programSettlementRates.png",width=6,height=3.5, units="in", pointsize=9, res=900)



plotYear <- systemData$year



par(mar=c(3.1,5.5,1.1,1.1))
plot(x="",y="",xlim=range(plotYear),ylim=c(20,100),ylab="Percentage of Cases Reaching\nFull or Partial Settlement (%)",xlab="",
     cex.lab=1.4,cex.main=1.5,font=2,axes=F)

axis(side=1,at=seq(min(plotYear ),max(plotYear ),by=1),font=2)
axis(side=2,at=seq(20,100,by=10),font=2,las=2)

lines(x=plotYear,y=systemData$Superior.Courts*100,lty=1,lwd=2)
lines(x=plotYear,y=systemData$State.Courts*100,lty=2,lwd=2)
lines(x=plotYear,y=systemData$Probate.Courts*100,lty=3,lwd=2)
lines(x=plotYear,y=systemData$Magistrate.Courts*100,lty=4,lwd=2,col="black")
lines(x=plotYear,y=systemData$Juvenile.Courts*100,lty=5,lwd=2,col="black")

box()
legend(x=2001.25, y=35, lty=c(1,2,3,4,5), lwd=c(2,2,2,2,2), ncol=2, col=c('black','black','black','black','black'),
       legend=c('Superior Courts', 'State Courts','Probate Courts','Magistrate Courts','Juvenile Courts'),cex=.75)

# PLEASE NOTE: Uncomment the following line if you are creating high-resolution figures
# dev.off()


######################################### CREATE PLOT (FIGURE 2) 



systemData  <- read.csv("http://www.poliscidata.com/replication/systemSettlementRatesWOmed.csv")


# PLEASE NOTE: You may wish to uncomment the following line to create high-resolution figures.
# 
# png(file="programEarlySettlements.png",width=6,height=3.5, units="in", pointsize=9, res=1200)

plotYear <- systemData$year

par(mar=c(3.1,5.5,1.1,1.1))
plot(x="",y="",xlim=range(plotYear),ylim=c(0,50),ylab="Percentage of ADR-Referred Cases\nSettling Prior to Mediation (%)",xlab="",
     cex.lab=1.4,cex.main=1.5,font=2,axes=F)

axis(side=1,at=seq(min(plotYear ),max(plotYear ),by=1),font=2)
axis(side=2,at=seq(0,50,by=10),font=2,las=2)

lines(x=plotYear,y=systemData$Superior.Courts*100,lty=1,lwd=2)
lines(x=plotYear,y=systemData$State.Courts*100,lty=2,lwd=2)
lines(x=plotYear,y=systemData$Probate.Courts*100,lty=3,lwd=2)
lines(x=plotYear,y=systemData$Magistrate.Courts*100,lty=4,lwd=2,col="black")
lines(x=plotYear,y=systemData$Juvenile.Courts*100,lty=5,lwd=2,col="black")

box()
legend(x=1997, y=50, lty=c(1,2,3,4,5), lwd=c(2,2,2,2,2), ncol=2, col=c('black','black','black','black','black'),
       legend=c('Superior Courts', 'State Courts','Probate Courts','Magistrate Courts','Juvenile Courts'),cex=.75)

# PLEASE NOTE: Uncomment the followintg line if you are creating high-resolution figures
# dev.off()




