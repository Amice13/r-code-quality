
rm(list=ls(all=TRUE))

data 		<- read.delim("GCspending.txt",header=TRUE)	# Read into memory a tab delimited file and assign it to a data frame ds
attach(data)

data.lim<-data[municipality!="Belo Horizonte",]
detach(data)
attach(data.lim)

V.58<-GC.votes.58/100
V.62<-GC.votes.62/100

V.58.spend1<-V.58[GC.spending.58>0]
V.58.spend0<-V.58[GC.spending.58==0]

N.58.spend1<-length(V.58.spend1)
N.58.spend0<-length(V.58.spend0)

V.62.spend1<-V.62[GC.spending.62>0]
V.62.spend0<-V.62[GC.spending.62==0]

N.62.spend1<-length(V.62.spend1)
N.62.spend0<-length(V.62.spend0)

#boxdata58<-list(V.58.spend1,V.58.spend0)
#boxdata62<-list(V.62.spend1,V.62.spend0)

#par(mfrow=c(2,1))

#plot(c(0,800),c(0,3),type="n",xlab="",ylab="")
#boxplot(boxdata58,horizontal=T,add=T)
#plot(c(0,800),c(0,3),type="n",xlab="",ylab="")
#boxplot(boxdata62,horizontal=T,add=T)


mu.58.1<-mean(V.58.spend1)
sd.58.1<-sd(V.58.spend1)
q25.58.1<-quantile(V.58.spend1,0.25)
q50.58.1<-quantile(V.58.spend1,0.50)
q75.58.1<-quantile(V.58.spend1,0.75)
min.58.1<-min(V.58.spend1)
max.58.1<-max(V.58.spend1)

payments.58<-round(c(mu.58.1,sd.58.1,q25.58.1,q50.58.1,q75.58.1,min.58.1,max.58.1,N.58.spend1),digits=3)


mu.58.0<-mean(V.58.spend0)
sd.58.0<-sd(V.58.spend0)
q25.58.0<-quantile(V.58.spend0,0.25)
q50.58.0<-quantile(V.58.spend0,0.50)
q75.58.0<-quantile(V.58.spend0,0.75)
min.58.0<-min(V.58.spend0)
max.58.0<-max(V.58.spend0)

no.payments.58<-round(c(mu.58.0,sd.58.0,q25.58.0,q50.58.0,q75.58.0,min.58.0,max.58.0,N.58.spend0),digits=3)

mu.62.1<-mean(V.62.spend1)
sd.62.1<-sd(V.62.spend1)
q25.62.1<-quantile(V.62.spend1,0.25)
q50.62.1<-quantile(V.62.spend1,0.50)
q75.62.1<-quantile(V.62.spend1,0.75)
min.62.1<-min(V.62.spend1)
max.62.1<-max(V.62.spend1)

payments.62<-round(c(mu.62.1,sd.62.1,q25.62.1,q50.62.1,q75.62.1,min.62.1,max.62.1,N.62.spend1),digits=3)

mu.62.0<-mean(V.62.spend0)
sd.62.0<-sd(V.62.spend0)
q25.62.0<-quantile(V.62.spend0,0.25)
q50.62.0<-quantile(V.62.spend0,0.50)
q75.62.0<-quantile(V.62.spend0,0.75)
min.62.0<-min(V.62.spend0)
max.62.0<-max(V.62.spend0)

no.payments.62<-round(c(mu.62.0,sd.62.0,q25.62.0,q50.62.0,q75.62.0,min.62.0,max.62.0,N.62.spend0),digits=3)

SS.tab<-cbind(payments.58,no.payments.58,payments.62,no.payments.62)

row.names(SS.tab)<-c("mean","s.d","25%","50%","75%","min","max","N")

SS.tab


