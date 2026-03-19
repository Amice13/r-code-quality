
setwd("~/Dropbox/io_endogenous_replication/main_text_do/figures/figure_2/")


ma <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}



data<-read.csv(file="figure_2.csv")





NW<-tapply(data[data$Brit_Neth==1,]$Parliament,data[data$Brit_Neth==1,]$Year,mean,na.rm=T)

S<-tapply(data[data$South_E==1,]$Parliament,data[data$South_E==1,]$Year,mean,na.rm=T)

ES<-tapply(data[data$East_E==1,]$Parliament,data[data$East_E==1,]$Year,mean,na.rm=T)

CE<-tapply(data[data$Central_E==1,]$Parliament,data[data$Central_E==1,]$Year,mean,na.rm=T)


par(mfrow=c(1,3), mar=c(4,4.5,6.25,1))



avg<-tapply(data$Parliament,data$Year,mean,na.rm=T )

avg_I<-tapply(data[data$Sovereign==1,]$Parliament,data[data$Sovereign==1,]$Year,mean,na.rm=T )

avg_N<-tapply(data[data$Sovereign==0,]$Parliament,data[data$Sovereign==0,]$Year,mean,na.rm=T )



plot(1201:1789, ma(avg,5),type="n",axes=F,xlab="",ylab="Proportion of Countries w/ a Parliament",ylim=c(0,.9),main="By Independence Status")

grid()


lines(1201:1789, ma(avg,5),lty=1,lwd=2)
lines(1201:1789, ma(avg_I,5),lty=2,lwd=2)
lines(1201:1789, ma(avg_N,5),lty=3,lwd=2)

text(1722.5,.1425,"Not Independent",cex=.75)

text(1730,.51,"Independent",cex=.75)

text(1650,.42,"Overall",cex=.75)


axis(1);axis(2,seq(0,1,by=.2))







plot(1201:1789, ma(NW,5),type="n",xlab="",ylab="",axes=F,ylim=c(0,.9),main="Northwest and Central Europe")

grid()

lines(1201:1789, ma(NW,5),lwd=1.5,lty=1)


lines(1201:1789, ma(CE,5),lwd=1.5,lty=2)



axis(1);axis(2,seq(0,1,by=.2))

text(1737.5,.75,"Northwest",cex=.75)
text(1250,.6,"Central",cex=.75)



plot(1201:1789, ma(CE,5),type="n",xlab="",ylab="",axes=F,ylim=c(0,.9),main="Southern Europe & Eastern Europe/Scandinavia")

grid()


lines(1201:1789,ma(ES,5),lwd=1.5, lty=2)


text(1730,.75,"Eastern & Scandinavia",cex=.75)


text(1265.5,.525,"Southern",cex=.75)
lines(1201:1789,ma(S,5),lwd=1.5, lty=1)

axis(1);axis(2,seq(0,1,by=.2))







mtext("Proportion of Units With Parliaments", side=3, outer=T,cex=2,font=2,line=-2)

