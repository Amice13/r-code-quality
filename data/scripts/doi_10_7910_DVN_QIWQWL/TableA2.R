
rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

data.lim<-data[municipality!="Belo Horizonte",]
detach(data)
attach(data.lim)

V.58<-GC.votes.58
V.62<-GC.votes.62

D.votes <- (V.62-V.58)/100

D.spend<-(GC.spending.62/3.699763) - GC.spending.58

D.spend.inc <- ifelse(D.spend>0,1,0)
D.spend.dec <- ifelse(D.spend<0,1,0)
D.spend.0   <- ifelse(D.spend==0,1,0)

D.votes.S.inc <- D.votes[D.spend.inc==1]
D.votes.S.dec <- D.votes[D.spend.dec==1]
D.votes.S.0   <- D.votes[D.spend.0==1]

N.inc<-length(D.votes.S.inc)
N.dec<-length(D.votes.S.dec)
N.0<-length(D.votes.S.0)


mu.inc<-mean(D.votes.S.inc)
sd.inc<-sd(D.votes.S.inc)
q25.inc<-quantile(D.votes.S.inc,0.25)
q50.inc<-quantile(D.votes.S.inc,0.50)
q75.inc<-quantile(D.votes.S.inc,0.75)
min.inc<-min(D.votes.S.inc)
max.inc<-max(D.votes.S.inc)

payments.inc<-round(c(mu.inc,sd.inc,q25.inc,q50.inc,q75.inc,min.inc,max.inc,N.inc),digits=3)


mu.dec<-mean(D.votes.S.dec)
sd.dec<-sd(D.votes.S.dec)
q25.dec<-quantile(D.votes.S.dec,0.25)
q50.dec<-quantile(D.votes.S.dec,0.50)
q75.dec<-quantile(D.votes.S.dec,0.75)
min.dec<-min(D.votes.S.dec)
max.dec<-max(D.votes.S.dec)

payments.dec<-round(c(mu.dec,sd.dec,q25.dec,q50.dec,q75.dec,min.dec,max.dec,N.dec),digits=3)


mu.0<-mean(D.votes.S.0)
sd.0<-sd(D.votes.S.0)
q25.0<-quantile(D.votes.S.0,0.25)
q50.0<-quantile(D.votes.S.0,0.50)
q75.0<-quantile(D.votes.S.0,0.75)
min.0<-min(D.votes.S.0)
max.0<-max(D.votes.S.0)

payments.0<-round(c(mu.0,sd.0,q25.0,q50.0,q75.0,min.0,max.0,N.0),digits=3)


SS.tab<-cbind(payments.inc,payments.dec,payments.0)

row.names(SS.tab)<-c("mean","s.d","25%","50%","75%","min","max","N")

SS.tab


