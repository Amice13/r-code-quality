
#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings in rddensity to drop observations with missing values, especially for
#the bootstrap, which repeats the warnings 10000 times.
 suppressWarnings({
    
    
bf<-rddensity(data$arrival_date_run)
b1<-rddensity(data[data$comm==1,]$arrival_date_run,h=31)
b0<-rddensity(data[data$comm==0,]$arrival_date_run,h=31)
b11<-rddensity(data[data$comm==1,]$arrival_date_run)
b01<-rddensity(data[data$comm==0,]$arrival_date_run)
b1_plot<-rdplotdensity(b11,data[data$comm==1,]$arrival_date_run,plotN=50)
b0_plot<-rdplotdensity(b01,data[data$comm==0,]$arrival_date_run,plotN=50)
bf_plot<-rdplotdensity(bf,data$arrival_date_run,plotN=50)


b_res<-list(bf,b1,b0,b11,b01)


b_out<-data.frame(matrix("&",8,10))
cols<-seq(2,10,by=2)

for(i in 1:5){
  b_out[1,cols[i]]<-round(b_res[[i]]$hat$diff,3)
  b_out[2,cols[i]]<- round(b_res[[i]]$test$t_jk,3)
  b_out[3,cols[i]]<- paste(paste("(",round(b_res[[i]]$test$p_jk,3),sep=""),")",sep="")
  b_out[7,cols[i]]<-b_res[[i]]$h$left
  b_out[8,cols[i]]<-paste( b_res[[i]]$N$eff_left, b_res[[i]]$N$eff_right, sep="/" ) 
}

b_out[3,]<-ifelse(b_out[3,]=="(0)", "(0.000)",b_out[3,])
b_out<-cbind(b_out,"\\")

b_out<-cbind( c("","","","","","", "Bandwidth","N (L/R)") ,b_out)

names(b_out)<-c("&","1.","&","2.","&","3.","&","4.", "&", "5.", "\\","")


data_comm<-data[data$comm==1,]
data_ncomm<-data[data$comm==0,]
data_comm_full_bw<-data[data$comm==1 & data$arrival_date_run %in% seq(-31,31),]
data_ncomm_full_bw<-data[data$comm==0 & data$arrival_date_run %in% seq(-31,31),]
data_comm_part_bw<-data[data$comm==1 & data$arrival_date_run %in% seq(-48,48),]
data_ncomm_part_bw<-data[data$comm==0 & data$arrival_date_run %in% seq(-44,44),]



out_rddens<-list()
out_rddensbw<-list()
set.seed(24)

for(i in 1:10000){
  
  bc1<-rddensity(data_comm[sample(1:nrow(data_comm),nrow(data_comm),replace=T),]$arrival_date_run,h=31)
  bn0<-rddensity(data_ncomm[sample(1:nrow(data_ncomm),nrow(data_ncomm),replace=T),]$arrival_date_run,h=31)
  bc11<-rddensity(data_comm[sample(1:nrow(data_comm),nrow(data_comm),replace=T),]$arrival_date_run,h=b11$h$right)
  bn01<-rddensity(data_ncomm[sample(1:nrow(data_ncomm),nrow(data_ncomm),replace=T),]$arrival_date_run,h=b01$h$right)
  out_rddens[[i]]  <-  c(bc1$hat$diff - bn0$hat$diff,bc11$hat$diff - bn01$hat$diff)
  bc1bw<-rddensity(data_comm_full_bw[sample(1:nrow(data_comm_full_bw),nrow(data_comm_full_bw),replace=T),]$arrival_date_run,h=31)
  bn0bw<-rddensity(data_ncomm_full_bw[sample(1:nrow(data_comm_full_bw),nrow(data_ncomm_full_bw),replace=T),]$arrival_date_run,h=31)
  bc11bw<-rddensity(data_comm_part_bw[sample(1:nrow(data_comm_part_bw),nrow(data_comm_part_bw),replace=T),]$arrival_date_run,h=b11$h$right)
  bn01bw<-rddensity(data_ncomm_part_bw[sample(1:nrow(data_ncomm_part_bw),nrow(data_ncomm_part_bw),replace=T),]$arrival_date_run,h=b01$h$right)
  out_rddensbw[[i]]  <-  c(bc1bw$hat$diff - bn0bw$hat$diff,bc11bw$hat$diff - bn01bw$hat$diff)
  
}


diff_CI<-round(apply(do.call(rbind,out_rddens),2,quantile,c(.025,.975)),3)
diff_CIbw<-round(apply(do.call(rbind,out_rddensbw),2,quantile,c(.025,.975)),3)


b_out[4,cols[4]+1]<-round((b11$hat$diff - b01$hat$diff),3)
b_out[5,cols[4]+1]<-paste(paste("(", paste(diff_CI[1,2],diff_CI[2,2],sep=","),sep=""),")",sep="")
b_out[6,cols[4]+1]<-paste(paste("[", paste(diff_CIbw[1,2],diff_CIbw[2,2],sep=","),sep=""),"]",sep="")
b_out[4,cols[2]+1]<-round(b1$hat$diff - b0$hat$diff,3)
b_out[5,cols[2]+1]<-paste(paste("(", paste(diff_CI[1,1],diff_CI[2,1],sep=","),sep=""),")",sep="")
b_out[6,cols[2]+1]<-paste(paste("[", paste(diff_CIbw[1,1],diff_CIbw[2,1],sep=","),sep=""),"]",sep="")

print(b_out,row.names=F)

  })
#})







#########Figure A8#####
length_comm<-tapply(data$comm,data$arrival_date_run,sum)
n_data<-data.frame(length_comm,as.numeric(names(length_comm)))
names(n_data)<-c("n","date")


n_data$n_prop<-n_data$n/sum(n_data$n)

par(mfrow=c(1,2))


pdf("figure_output/Figure_A8.pdf",12,6)


plot(n_data$date, n_data$n_prop,xlim=c(-60,60),ylim=c(-.006,.015),type="n",axes=F, xlab="Arrival Date in Spain",ylab="f(x)")
grid()

#points(n_data$date, n_data$n_prop,pch=16,col="grey",cex=.5)




#lines(b0_plot$Estr$Estimate[,1],b0_plot$Estr$Estimate[,6],col="dark blue",lwd=2)
polygon(
  c(b0_plot$Estr$Estimate[,1], rev(b0_plot$Estr$Estimate[,1]) ), 
  c(b0_plot$Estr$Estimate[,6]+1.96*b0_plot$Estr$Estimate[,8],rev(b0_plot$Estr$Estimate[,6]-1.96*b0_plot$Estr$Estimate[,8] )),
  col=rgb(0,.25,.75,.25),border = rgb(0,.15,.85,.25)
)  





#lines(b0_plot$Estl$Estimate[,1],b0_plot$Estl$Estimate[,6],col="dark blue",lwd=2)
polygon(
  c(b0_plot$Estl$Estimate[,1], rev(b0_plot$Estl$Estimate[,1]) ), 
  c(b0_plot$Estl$Estimate[,6]+1.96*b0_plot$Estl$Estimate[,8],rev(b0_plot$Estl$Estimate[,6]-1.96*b0_plot$Estl$Estimate[,8] )),
  col=rgb(0,.25,.75,.25),border = rgb(0,.15,.85,.25)
)  



#lines(b1_plot$Estr$Estimate[,1],b1_plot$Estr$Estimate[,6],col="dark red",lwd=2)
polygon(
  c(b1_plot$Estr$Estimate[,1], rev(b1_plot$Estr$Estimate[,1]) ), 
  c(b1_plot$Estr$Estimate[,6]+1.96*b1_plot$Estr$Estimate[,8],rev(b1_plot$Estr$Estimate[,6]-1.96*b1_plot$Estr$Estimate[,8] )),
  col=rgb(.75,.25,0,.25),border = rgb(.85,.15,0,.25)
)  





#lines(b1_plot$Estl$Estimate[,1],b1_plot$Estl$Estimate[,6],col="dark red",lwd=2)
polygon(
  c(b1_plot$Estl$Estimate[,1], rev(b1_plot$Estl$Estimate[,1]) ), 
  c(b1_plot$Estl$Estimate[,6]+1.96*b1_plot$Estl$Estimate[,8],rev(b1_plot$Estl$Estimate[,6]-1.96*b1_plot$Estl$Estimate[,8] )),
  col=rgb(.75,.25,0,.25),border =rgb(.85,.15,0,.25) 
)  









axis(1);axis(2)



text(12,-.0061,"Foreign Enlistment Act Enforced",col="black",cex=.75)
#lines(c(0,0), c(0,.01),col="dark red", lty=2,lwd=2)
#abline(v=0,lty=2,col="dark red") 
arrows(.5,-.0065,60,-.0065,length=.05,lwd=2,col="black")

text(2,.011,"¬ CP",cex=.75, col="dark blue")
text(-7,.004,"CP",cex=.75, col="dark red")

mtext("Density Discontinuity Estimates",cex=1.5,font=2,line=1)
mtext("Communist & Non-Communist Fighters",cex=.75,font=1,line=0)




plot(n_data$date, n_data$n_prop,xlim=c(-60,60),ylim=c(-.011,.015),type="n",axes=F, xlab="Arrival Date in Spain",ylab="f(x)")
grid()

#points(n_data$date, n_data$n_prop,pch=16,col="grey",cex=.5)




#lines(bf_plot$Estr$Estimate[,1],bf_plot$Estr$Estimate[,6],col="black",lwd=2)
polygon(
  c(bf_plot$Estr$Estimate[,1], rev(bf_plot$Estr$Estimate[,1]) ), 
  c(bf_plot$Estr$Estimate[,6]+1.96*bf_plot$Estr$Estimate[,8],rev(bf_plot$Estr$Estimate[,6]-1.96*bf_plot$Estr$Estimate[,8] )),
  col=rgb(.1,.1,.1,.15),border = rgb(.1,.1,.1,.25)
)  





#lines(bf_plot$Estl$Estimate[,1],bf_plot$Estl$Estimate[,6],col="black",lwd=2)
polygon(
  c(bf_plot$Estl$Estimate[,1], rev(bf_plot$Estl$Estimate[,1]) ), 
  c(bf_plot$Estl$Estimate[,6]+1.96*bf_plot$Estl$Estimate[,8],rev(bf_plot$Estl$Estimate[,6]-1.96*bf_plot$Estl$Estimate[,8] )),
  col=rgb(.1,.1,.1,.15),border = rgb(.1,.1,.1,.25)
)  


axis(1);axis(2)



text(12,-.011,"Foreign Enlistment Act Enforced",col="black",cex=.75)
#lines(c(0,0), c(0,.01),col="dark red", lty=2,lwd=2)
#abline(v=0,lty=2,col="dark red") 
arrows(.5,-.0115,60,-.0115,length=.05,lwd=2,col="black")


mtext("Density Discontinuity Estimates",cex=1.5,font=2,line=1)
mtext("Full Sample",cex=.75,font=1,line=0)




dev.off()

























