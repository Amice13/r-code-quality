
#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings due to the dummies package, which works but triggers warnings for any R version since R 3.6.  
  suppressWarnings({
  
data<-data[which(data$leave_duration < 1000),]
data<-data[data$leave_duration > 0,]

country_dums<-dummy(data$country)
arrival_dums<-dummy(data$arrival_date_run)
age_dums<-dummy(data$age)
class_dums<-dummy(data$Class)

cause<-ifelse(data$killed==1,2,NA)
cause<-ifelse(data$killed==0 & data$status==0,0,cause)
cause<-ifelse(data$status==1,1,cause)


data$cause<-cause


fit.leave1<-crr(data$leave_duration,data$cause,data$comm,failcode=1,cencode=0)
fit.kill1<-crr(data$leave_duration,data$cause,data$comm,failcode=2,cencode=0)

fit.leave2<-crr(data$leave_duration,data$cause,data.frame(data$comm,cbind(arrival_dums[,2:ncol(arrival_dums)])),failcode=1,cencode=0)
fit.kill2<-crr(data$leave_duration,data$cause,data.frame(data$comm,cbind(arrival_dums[,2:ncol(arrival_dums)])),failcode=2,cencode=0)

fit.leave3<-crr(data$leave_duration,data$cause,data.frame(data$comm,cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:ncol(country_dums)])),failcode=1,cencode=0)
fit.kill3<-crr(data$leave_duration,data$cause,data.frame(data$comm,cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,ncol(country_dums)])),failcode=2,cencode=0)


crr_mods<-list(fit.leave1,fit.kill1,fit.leave2,fit.kill2, fit.leave3,fit.kill3 )

out_crr<-data.frame(matrix("&",2,12))

cols<-seq(2,12,by=2)

for(i in 1:length(crr_mods)){
  out_crr[1,cols[i]]<-round(summary(crr_mods[[i]])$coef[1,1],3)
  out_crr[2,cols[i]]<-paste(paste("(",round(summary(crr_mods[[i]])$coef[1,3],3),sep=""),")",sep="")
}


names(out_crr)[cols]<-c("Depart","KIA","Depart","KIA","Depart","KIA")
names(out_crr)[cols-1]<-"&"

out_crr<-cbind(out_crr,"\\")
names(out_crr)[ncol(out_crr)]<-"\\"

row.names(out_crr)[1]<-"Communist"
row.names(out_crr)[2]<-""

print(out_crr)





### Figure A6 primitives ###

pred_leave_t1<-predict(fit.leave1,cov1=data.frame(comm=1))
pred_leave_t0<-predict(fit.leave1,cov1=data.frame(comm=0))


pred_kill_t1<-predict(fit.kill1,cov1=data.frame(comm=1))
pred_kill_t0<-predict(fit.kill1,cov1=data.frame(comm=0))



cuminc_leave_t1<-rep(0,max(data$leave_duration))
counter<-1


for(i in 1:(max(data$leave_duration))){
  +if(counter == 1){
    if(pred_leave_t1[1,1]==i){
      cuminc_leave_t1[i] <- pred_leave_t1[1,2]
      counter <- counter + 1
    } else cuminc_leave_t1[i] <- 0
  } else if(counter > nrow(pred_leave_t1)){
    cuminc_leave_t1[i] <- pred_leave_t1[nrow(pred_leave_t1),2]
  } else if(pred_leave_t1[counter,1]==i){
    cuminc_leave_t1[i] <- pred_leave_t1[counter,2]
    counter <- counter + 1
  } else cuminc_leave_t1[i] <- pred_leave_t1[counter-1,2]
}






cuminc_kill_t1<-rep(0,max(data$leave_duration))
counter<-1


for(i in 1:(max(data$leave_duration))){
  +if(counter == 1){
    if(pred_kill_t1[1,1]==i){
      cuminc_kill_t1[i] <- pred_kill_t1[1,2]
      counter <- counter + 1
    } else cuminc_kill_t1[i] <- 0
  } else if(counter > nrow(pred_kill_t1)){
    cuminc_kill_t1[i] <- pred_kill_t1[nrow(pred_kill_t1),2]
  } else if(pred_kill_t1[counter,1]==i){
    cuminc_kill_t1[i] <- pred_kill_t1[counter,2]
    counter <- counter + 1
  } else cuminc_kill_t1[i] <- pred_kill_t1[counter-1,2]
}







cuminc_leave_t0<-rep(0,max(data$leave_duration))
counter<-1


for(i in 1:(max(data$leave_duration))){
  +if(counter == 1){
    if(pred_leave_t0[1,1]==i){
      cuminc_leave_t0[i] <- pred_leave_t0[1,2]
      counter <- counter + 1
    } else cuminc_leave_t0[i] <- 0
  } else if(counter > nrow(pred_leave_t0)){
    cuminc_leave_t0[i] <- pred_leave_t0[nrow(pred_leave_t0),2]
  } else if(pred_leave_t0[counter,1]==i){
    cuminc_leave_t0[i] <- pred_leave_t0[counter,2]
    counter <- counter + 1
  } else cuminc_leave_t0[i] <- pred_leave_t0[counter-1,2]
}






cuminc_kill_t0<-rep(0,max(data$leave_duration))
counter<-1


for(i in 1:(max(data$leave_duration))){
  +if(counter == 1){
    if(pred_kill_t0[1,1]==i){
      cuminc_kill_t0[i] <- pred_kill_t0[1,2]
      counter <- counter + 1
    } else cuminc_kill_t0[i] <- 0
  } else if(counter > nrow(pred_kill_t0)){
    cuminc_kill_t0[i] <- pred_kill_t0[nrow(pred_kill_t0),2]
  } else if(pred_kill_t0[counter,1]==i){
    cuminc_kill_t0[i] <- pred_kill_t0[counter,2]
    counter <- counter + 1
  } else cuminc_kill_t0[i] <- pred_kill_t0[counter-1,2]
}








####Figure A6 output####

pdf("figure_output/Figure_A6.pdf",6,6)

plot(cuminc_leave_t1+cuminc_kill_t1,ylim=c(0,1),axes=F,xlab="Days",ylab="Cumulative Incidence",type="n" )
grid()


lines(1:max(data$leave_duration), cuminc_leave_t1+cuminc_kill_t1,lwd=2,col="red")

lines(1:max(data$leave_duration),cuminc_kill_t1,lwd=2,col="red",lty=2)


lines(1:max(data$leave_duration), cuminc_leave_t0+cuminc_kill_t0,lwd=2)

lines(1:max(data$leave_duration), cuminc_kill_t0,lwd=2,lty=2)

text(700,.52,"Depart Spain",cex=.75,font=2)
text(700,.175,"KIA",cex=.75,font=2)

text(800,.75,"CP",cex=.75,col="red")
text(795,.925,"¬CP",cex=.75,col="black")


axis(1,seq(0,850,by=100) ,cex.axis=.65);axis(2)

#axis(1, at = c( seq(-850,0,by=50), seq(0,850,by=50) )  , labels=c( seq(850,0,by=-50), seq(0,850,by=50) ) ,cex.axis=.5  )   



dev.off()

  })
#})
