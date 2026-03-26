
#remove(list=ls())
source("code/build.R")

#suppressMessages({
  
library(showtext)
showtext_auto()

bins<-unique(c(rev(-1*seq(0,360,by=1)),seq(0,660,by=1)))

data$bin<-NA

for(i in 1:length(bins)){
  
  data$bin<-ifelse(data$arrival_date_run > bins[i] & data$arrival_date_run <= bins[i+1],bins[i],data$bin)
  
  
}


means_comm<-tapply(data$comm,data$bin,mean,na.rm=T)
length_comm<-tapply(data$comm,data$bin,length)
length_comm[as.numeric(names(length_comm)) > -56 & as.numeric(names(length_comm)) < 56]


pdf("figure_output/Figure_4.pdf",6,6)
par(family = "helvetica_light", font = 1, 
    font.axis = 1, font.lab = 1, font.main = 1)


plot(as.numeric(names(means_comm))+.5,means_comm,xlim=c(-60,60), pch=16,col="grey",
     cex=length_comm/15,axes=F,xlab="Arrival in Spain (days)",
     ylab="Pr(Communist)",cex.axis=0.8)


loess_down<-loess(comm~arrival_date_run,data[data$arrival_date_run < 0,])
loess_up<-loess(comm~arrival_date_run,data[data$arrival_date_run >= 0,])


pred_down<-predict(loess_down, newdata = seq(-60,0,by=1),se=T)
pred_down<-data.frame(cbind( -60:0, pred_down$fit, pred_down$se.fit  ))

names(pred_down)<-c("run","fit","se")
pred_down$ci_hi<-pred_down$fit + 1.96*pred_down$se
pred_down$ci_lo<-pred_down$fit - 1.96*pred_down$se

pred_down<-pred_down[!is.na(pred_down$fit),]
pred_down$run<-pred_down$run+1


pred_up<-predict(loess_up, newdata = seq(0,60,by=1),se=T)
pred_up<-data.frame(cbind( 0:60, pred_up$fit, pred_up$se.fit  ))
names(pred_up)<-c("run","fit","se")
pred_up$ci_hi<-pred_up$fit + 1.96*pred_up$se
pred_up$ci_lo<-pred_up$fit - 1.96*pred_up$se

pred_up<-pred_up[!is.na(pred_up$fit),]
pred_up$run<-pred_up$run-1


polygon(c(pred_up$run, rev(pred_up$run)),
        c(pred_up$ci_hi, rev(pred_up$ci_lo)),
        col=rgb(.1,.1,.1,.1), border = F        
)
lines(pred_up$run,pred_up$fit,lwd=2)


polygon(c(pred_down$run, rev(pred_down$run)),
        c(pred_down$ci_hi, rev(pred_down$ci_lo)),
        col=rgb(.1,.1,.1,.1), border = F        
)

lines(pred_down$run,pred_down$fit, lwd=2)


axis(1,seq(-100,100,by=10),cex.axis=.9);axis(2,cex.axis=.9,las=1)
text(30,.03,"Foreign Enlistment Act enforced",col="black",cex=1)
arrows(.01,.01,60,.01,length=.05,lwd=1.5,col="black")

abline(v=0,lty=2,lwd=2)

dev.off()

#})

