#remove(list=ls())
source("code/build.R")

#suppressMessages({
#Suppress warnings due to the dummies package, which works but triggers warnings for any R version since R 3.6.
#Suppress warnings of mass points detected in the running variable in rdd. We discuss this on p. 28 of the article. 
#Suppress warnings of dropping redundant fixed effect variables due to multicollinearity.
  suppressWarnings({


country_dums<-dummy(data$country)
age_dums<-dummy(data$age)
class_dums<-dummy(data$Class)
occ_dums<-dummy(data$Occupation)


out_rob_rd<-list()

for(i in -5:5){
  
  aa<-rdrobust(y=data$comm,x=data$arrival_date_run+i,cluster=data$country, masspoints = "adjust")
  aa4<-rdrobust(y=data$comm,x=data$arrival_date_run+i,cluster=data$country,covs = cbind(age_dums[,2:ncol(age_dums)],data$woman,country_dums[,2:36]), masspoints = "adjust")
  
  out_rob_rd[[i*-1+6]]<-list(aa,aa4)
  
}


pdf("figure_output/Figure_A7.pdf",10,6)


plot(-5:5,seq(-.25,.4,length=11),type="n", axes=F,xlab = "+/- FEA Enforcement Announced",ylab=expression(hat(tau)))
grid()


for(i in 1:11){
  points(i-6,out_rob_rd[[i]][[1]]$Estimate[2],pch=16,col="black")
  lines(c(i-6,i-6),out_rob_rd[[i]][[1]]$ci[3,],col="black",lwd=1.5)
  lines(c(i-6-.05,i-6+.05),c(out_rob_rd[[i]][[1]]$ci[3,1],out_rob_rd[[i]][[1]]$ci[3,1])
        ,col="black",lwd=1.5)
  
  lines(c(i-6-.05,i-6+.05),c(out_rob_rd[[i]][[1]]$ci[3,2],out_rob_rd[[i]][[1]]$ci[3,2])
        ,col="black",lwd=1.5)
}

abline(h=0,lty=2,col="dark red")
axis(1,seq(-5,5));axis(2)


dev.off()


 })
#})  