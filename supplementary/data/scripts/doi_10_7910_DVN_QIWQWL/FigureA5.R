
rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)


V.58<-GC.votes.58
V.62<-GC.votes.62

V.58.total<-sum(V.58)
V.58.spend<-sum(V.58[GC.spending.58>0])
P.V.spend.58<-100*(V.58.spend/V.58.total)

V.62.total<-sum(V.62)
V.62.spend<-sum(V.62[GC.spending.62>0])
P.V.spend.62<-100*(V.62.spend/V.62.total)

voters<-sum(regvoters.60,na.rm=T)
voters.58.spend<-sum(regvoters.60[GC.spending.58>0],na.rm=T)
voters.62.spend<-sum(regvoters.60[GC.spending.62>0],na.rm=T)
P.voters.spend.58<-100*(voters.58.spend/voters)
P.voters.spend.62<-100*(voters.62.spend/voters)


par(mfrow=c(2,2),mar=c(8,4,3,1),xpd=NA)

barplot(c(P.V.spend.58,100-P.V.spend.58),ylim=c(0,100),col=c("gray","white"),xlab="1958",axes=T)
text(.75,50,"45%")
text(2,60,"55%")
legend(0.3,-45,"payments to brokers",fill="gray")
barplot(c(P.V.spend.62,100-P.V.spend.62),ylim=c(0,100),col=c("gray","white"),xlab="1962",axes=F)
text(.75,76,"71%")
text(2,34,"29%")
legend(-.3,-45,"no payments to brokers",fill="white")

text(-.5,120,"Percentage of Capanema's Total Votes", cex=1)

barplot(c(P.voters.spend.58,100-P.voters.spend.58),ylim=c(0,100),col=c("gray","white"),xlab="1958",axes=T)
text(.75,9,"4%")
text(2,101,"96%")
legend(0.3,-45,"payments to brokers",fill="gray")
barplot(c(P.voters.spend.62,100-P.voters.spend.62),ylim=c(0,100),col=c("gray","white"),xlab="1962",axes=F)
text(.75,13,"8%")
text(2,97,"92%")
legend(-.3,-45,"no payments to brokers",fill="white")

text(-.4,120,"Percentage of Registered Voters", cex=1)