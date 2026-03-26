library(forecast)
library(foreign)
library(doBy)
setwd("D:\\Ikaro Barreto\\EFISciencia\\Fernando Hillebrand") #pasta de trabalho
sew<-read.table("conj1.txt",h=T)
wdp<-read.table("conj2.txt",h=T)
####SEW
sew2<-split(sew,list(sew$FID,sew$Month))
grid1<-expand.grid(0:52,5:11)
pr<-matrix(,nrow=371,ncol=1)
ob<-matrix(,nrow=371,ncol=1)
for(i in 1:371){
  f1<-arima(sew2[[i]][1:39,5],order=arimaorder(auto.arima(sew2[[i]][1:39,5])),xreg=sew2[[i]][1:39,13:19])
  pr[i]<-predict(f1,newxreg=sew2[[i]][40,13:19])$pred[[1]]
  f1<-NULL
  ob[i]<-sew2[[i]][40,5]
}
od<-matrix(,nrow=371,ncol=3)
for(i in 1:371){
  od[i,]<-arimaorder(auto.arima(sew2[[i]][1:39,5]))
  }

grid1$pred<-pr
grid1$obsr<-ob
colnames(grid1)<-c("FID","Month","Predicted","Observed")
grid1$Error<-grid1$Observed-grid1$Predicted
grid1<-as.data.frame(grid1)
is.data.frame(grid1)
sqrt(tapply((grid1$Error)^2,grid1$Month,mean)/53)

####WDP
wdp2<-split(wdp,list(wdp$FID,wdp$Month))
grid2<-expand.grid(0:65,5:11)
pr2<-matrix(,nrow=462,ncol=1)
ob2<-matrix(,nrow=462,ncol=1)
for(i in 1:462){
  f1<-arima(wdp2[[i]][1:39,5],order=arimaorder(auto.arima(wdp2[[i]][1:39,5])),xreg=wdp2[[i]][1:39,13:19])
  pr2[i]<-predict(f1,newxreg=wdp2[[i]][40,13:19])$pred[[1]]
  f1<-NULL
  ob2[i]<-wdp2[[i]][40,5]
  arimaorder(auto.arima(wdp2[[i]][1:39,5]))
}
od2<-matrix(,nrow=462,ncol=3)
for(i in 1:462){
  od2[i,]<-arimaorder(auto.arima(wdp2[[i]][1:39,5]))
}

for(i in 1:462){
  f1<-arima(wdp2[[i]][1:39,5],order=arimaorder(auto.arima(wdp2[[i]][1:39,5])),xreg=wdp2[[i]][1:39,13:19])
  pr2[i]<-predict(f1,newxreg=wdp2[[i]][40,13:19])$pred[[1]]
  f1<-NULL
  ob2[i]<-wdp2[[i]][40,5]
  arimaorder(auto.arima(wdp2[[i]][1:39,5]))
}


grid2$pred<-pr2
grid2$obsr<-ob2
colnames(grid2)<-c("FID","Month","Predicted","Observed")
grid2$Error<-grid2$Observed-grid2$Predicted
grid2<-as.data.frame(grid2)
sqrt(tapply((grid2$Error)^2,grid2$Month,mean)/66)

aorder1<-cbind.data.frame(grid1[,1:2],od)
aorder2<-cbind.data.frame(grid2[,1:2],od2)

##

write.table(grid1,"sew.txt",row.names = F)
write.table(grid2,"wdp.txt",row.names = F)
write.table(aorder1,"sew_arimaorder.txt",row.names = F)
write.table(aorder2,"wdp_arimaorder.txt",row.names = F)
