####################################################
# 
# Replication of figures/results in online appendix
# 
####################################################

######################################
### Appendix C.3 Convergence Diagnostics
### Table 1 values
######################################

#load required packages
library(pscl)
library(coda)
library(rstan)
#setwd("Uncomment and enter name of folder containing RData files")

# In the below code, loading the RData files which contain the Stan objects -- the outputs from the joint estimations -- 
# requires the installation of RStan. Please consult https://mc-stan.org/users/interfaces/rstan.html  
# for instructions on downloading and installing RStan.

rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
lst<-c("ST20_LSQ_replication_IA_estimation.RData",
       "ST20_LSQ_replication_NE_estimation.RData",
       "ST20_LSQ_replication_WI_estimation.RData")

repos_con<-list(list("Iowa"),list("Nebraska"),list("Wisconsin"))
for(a in 1:3){
  load(lst[a])
  if(a%in%c(1,3)){rm(list=setdiff(ls(),c("stan.fit3","repos_con","a","lst")))}
  if(a%in%c(2)){rm(list=setdiff(ls(),c("stan.fit4","repos_con","a","lst")))}
  gc();gc();gc();gc()
  if(a%in%c(1,3)){pos<-extract(stan.fit3,permuted=F,inc_warmup=F,"pos")}
  if(a%in%c(1,3)){tau<-extract(stan.fit3,permuted=F,inc_warmup=F,"tau")}
  if(a%in%c(1,3)){beta<-extract(stan.fit3,permuted=F,inc_warmup=F,"beta")}
  if(a%in%c(1,3)){theta<-extract(stan.fit3,permuted=F,inc_warmup=F,"theta")}
  if(a%in%c(2)){pos<-extract(stan.fit4,permuted=F,inc_warmup=F,"pos")}
  if(a%in%c(2)){tau<-extract(stan.fit4,permuted=F,inc_warmup=F,"tau")}
  if(a%in%c(2)){beta<-extract(stan.fit4,permuted=F,inc_warmup=F,"beta")}
  if(a%in%c(2)){theta<-extract(stan.fit4,permuted=F,inc_warmup=F,"theta")}
  #for each set of item parameters, calculate R-hat (Gelman and Rubin)
  ab<-list();for(i in 1:dim(pos)[[3]]){
    ab[[i]]<-gelman.diag(mcmc.list(mcmc(pos[,1,i]),mcmc(pos[,2,i]),mcmc(pos[,3,i])))
  }
  repos_con[[a]]$gelman_pos<-ab
  ab<-list();for(i in 1:dim(tau)[[3]]){
    ab[[i]]<-gelman.diag(mcmc.list(mcmc(tau[,1,i]),mcmc(tau[,2,i]),mcmc(tau[,3,i])))
  }
  repos_con[[a]]$gelman_tau<-ab
  ab<-list();for(i in 1:dim(beta)[[3]]){
    ab[[i]]<-gelman.diag(mcmc.list(mcmc(beta[,1,i]),mcmc(beta[,2,i]),mcmc(beta[,3,i])))
  }
  repos_con[[a]]$gelman_beta<-ab
  ab<-list();for(i in 1:dim(theta)[[3]]){
    ab[[i]]<-gelman.diag(mcmc.list(mcmc(theta[,1,i]),mcmc(theta[,2,i]),mcmc(theta[,3,i])))
  }
  repos_con[[a]]$gelman_theta<-ab
}

gelman_pos<-list();gelman_tau<-list();gelman_beta<-list();gelman_theta<-c()
for(j in 1:3){
  g6<-matrix(NA,nrow=length(repos_con[[j]]$gelman_pos),ncol=2)
  g7<-matrix(NA,nrow=length(repos_con[[j]]$gelman_tau),ncol=2)
  g8<-matrix(NA,nrow=length(repos_con[[j]]$gelman_beta),ncol=2)
  g9<-matrix(NA,nrow=length(repos_con[[j]]$gelman_theta),ncol=2)
  for(i in 1:length(repos_con[[j]]$gelman_pos)){g6[i,]<-unlist(repos_con[[j]]$gelman_pos[i])}
  gelman_pos[[j]]<-g6
  for(i in 1:length(repos_con[[j]]$gelman_tau)){g7[i,]<-unlist(repos_con[[j]]$gelman_tau[i])}
  gelman_tau[[j]]<-g7
  for(i in 1:length(repos_con[[j]]$gelman_beta)){g8[i,]<-unlist(repos_con[[j]]$gelman_beta[i])}
  gelman_beta[[j]]<-g8
  for(i in 1:length(repos_con[[j]]$gelman_theta)){g9[i,]<-unlist(repos_con[[j]]$gelman_theta[i])}
  gelman_theta[[j]]<-g9
}

length(which(gelman_pos[[1]][,1]>1.1)) #1
length(which(gelman_pos[[2]][,1]>1.1)) #0
length(which(gelman_pos[[3]][,1]>1.1)) #0

length(which(gelman_pos[[1]][,1]>1.1))/nrow(gelman_pos[[1]]) #0.0005299417
length(which(gelman_pos[[2]][,1]>1.1))/nrow(gelman_pos[[2]]) #0
length(which(gelman_pos[[3]][,1]>1.1))/nrow(gelman_pos[[3]]) #0

length(which(gelman_tau[[1]][,1]>1.1)) #27
length(which(gelman_tau[[2]][,1]>1.1)) #51
length(which(gelman_tau[[3]][,1]>1.1)) #29

length(which(gelman_tau[[1]][,1]>1.1))/nrow(gelman_tau[[1]]) #0.005665128
length(which(gelman_tau[[2]][,1]>1.1))/nrow(gelman_tau[[2]]) #0.02191663
length(which(gelman_tau[[3]][,1]>1.1))/nrow(gelman_tau[[3]]) #0.004419384

length(which(gelman_beta[[1]][,1]>1.1)) #19
length(which(gelman_beta[[2]][,1]>1.1)) #39
length(which(gelman_beta[[3]][,1]>1.1)) #18

length(which(gelman_beta[[1]][,1]>1.1))/nrow(gelman_beta[[1]]) #0.003986572
length(which(gelman_beta[[2]][,1]>1.1))/nrow(gelman_beta[[2]]) #0.01675978
length(which(gelman_beta[[3]][,1]>1.1))/nrow(gelman_beta[[3]]) #0.002743066

length(which(gelman_theta[[1]][,1]>1.1)) #0
length(which(gelman_theta[[2]][,1]>1.1)) #6   
length(which(gelman_theta[[3]][,1]>1.1)) #0

length(which(gelman_theta[[1]][,1]>1.1))/nrow(gelman_theta[[1]]) #0
length(which(gelman_theta[[2]][,1]>1.1))/nrow(gelman_theta[[2]]) #0.01271186
length(which(gelman_theta[[3]][,1]>1.1))/nrow(gelman_theta[[3]]) #0

###########################################
### Appendix D
### Appendix Figure 1 Comparing SQ Estimates
###########################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

png(filename=paste(getwd(),"/AFigure1.png",sep=""),width=1200,height=700)
par(mfrow=c(2,3),mar=c(0.3,0.3,0.3,0.3),mai=c(0.6,0.7,1,0.3),pty="s")
#Comparisons of unadjusted estimates with cosponsorship-based estimates
plot(xlim=c(-5,5),asp=1,df_IA_bills$sq3_est[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(1) Iowa",xlab="Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq3_est[which(is.na(df_IA_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq3_est[which(is.na(df_IA_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = 0.06'),expression(b[1]*' = 1.31')))
plot(asp=1,df_NE_bills$sq3_est[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(2) Nebraska",xlab="Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq3_est[which(is.na(df_NE_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq3_est[which(is.na(df_NE_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = -0.40'),expression(b[1]*' =  0.95')))
plot(asp=1,df_WI_bills$sq3_est[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(3) Wisconsin",xlab="Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq3_est[which(is.na(df_WI_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq3_est[which(is.na(df_WI_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = 0.20'),expression(b[1]*' = 1.11')))

#0.8885057  
cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq3_est[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#0.7685121
cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq3_est[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#0.8717348
cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq3_est[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")

#Comparisons of assumed bill position-based estimates with cosponsorship-based estimates
plot(xlim=c(-5,5),asp=1,df_IA_bills$sq4_ba3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(4) Iowa",xlab="Sponsorship-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq4_ba3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq4_ba3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2)
legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = -0.25'),expression(b[1]*' =  1.04')))
plot(asp=1,df_NE_bills$sq4_ba3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(5) Nebraska",xlab="Sponsorship-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq4_ba3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq4_ba3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2)
legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = -0.05'),expression(b[1]*' = 1.08')))
plot(asp=1,df_WI_bills$sq4_ba3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],cex.lab=2.25,cex.axis=1.75,cex.main=3,main="(6) Wisconsin",xlab="Sponsorship-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq4_ba3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq4_ba3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2)
legend(bty="n","bottomright",cex=1.9,c(expression(b[0]*' = 0.38'),expression(b[1]*' = 0.85')))

dev.off()

#0.9561281
cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq4_ba3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#0.9125975
cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq4_ba3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#0.9511175
cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq4_ba3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")

##########################################################################
### Appendix F.1
### Appendix Figure 2: Testing Gatekeeping Hypothesis (unadjusted estimates)
##########################################################################
Sys.setlocale(locale="C");rm(list=ls());gc();gc();gc()
load("ST20_LSQ_replication_Appendix_F1_1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure2.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq3_est)==F))
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq3_est)==F))
i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))
n1<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq3_est)==F))
w11<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq3_est)==F))
w21<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-pn)

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq3_est)==F))
i11<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i12<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i13<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq3_est)==F))
i21<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i22<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i23<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq3_est)==F))
w11<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w12<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w13<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq3_est)==F))
w21<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w22<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w23<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 3
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h))

i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s))

n1<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p))

w11<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

dev.off()

###################################################################################
### Appendix F.1
### Appendix Figure 3: Testing Gatekeeping Hypothesis (cosponsorship-based estimates)
###################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_F1_2.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure3.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025),na.rm=T)
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975),na.rm=T)
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,na.rm=T)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))
i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
n1<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))
w11<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))
w21<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-pn)

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))
i11<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i12<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i13<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))
i21<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025),na.rm=T)
i22<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975),na.rm=T)
i23<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,na.rm = T)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))
w11<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w12<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w13<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))
w21<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w22<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w23<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 3
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025),na.rm=T)
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975),na.rm=T)
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),na.rm = T)

i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025),na.rm = T)
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975),na.rm = T)
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),na.rm = T)

n1<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p))

w11<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

dev.off()

################################################################################
### Appendix F.1
### Appendix Figure 4: Testing Gatekeeping Hypothesis (Bill assumptions estimates)
################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_F1_3.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure4.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq4_ba3)==F))
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq4_ba3)==F))
i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))
n1<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq4_ba3)==F))
w11<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq4_ba3)==F))
w21<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-pn)

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq4_ba3)==F))
i11<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i12<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i13<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq4_ba3)==F))
i21<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i22<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i23<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq4_ba3)==F))
w11<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w12<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w13<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq4_ba3)==F))
w21<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w22<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w23<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 3
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h))

i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s))

n1<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p))

w11<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

dev.off()

################################################################################
### Appendix F.2
### Appendix Figure 5: Testing Gridlock Hypothesis: Unadjusted Estimates
################################################################################
Sys.setlocale(locale="C");rm(list=ls());gc();gc();gc()
load("ST20_LSQ_replication_Appendix_F2_1.RData")

png(filename=paste(getwd(),"/AFigure5.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq3_est)==F))
i11<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i12<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i13<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$nonzero_fp_cutpoint==0& df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))/length(which(df_NE_bills$nonzero_fp_cutpoint==0&is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq3_est)==F))
w11<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w12<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w13<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(c(min(i11,n1,w11)-0.1),c(max(i12,n2,w12)+0.1)),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0.1),expression('GR2'['Pivotal Politics']*' - GR2'['Null']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq3_est)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq3_est)==F))
text(cex=1.2,x=i13,y=2.05,paste(round(mean((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)),2),"-",round(pn,2)))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
pn<-length(which(df_NE_bills$nonzero_fp_cutpoint==0& df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))/length(which(df_NE_bills$nonzero_fp_cutpoint==0&is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq3_est)==F))
text(cex=1.2,x=n3,y=1.55,paste(round(mean(ne_kic_p/(ne_ooc_p+ne_kic_p)),2),"-",round(pn,2)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq3_est)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq3_est)==F))
text(cex=1.2,x=w13,y=1.05,paste(round(mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)),2),"-",round(pn,2)))

dev.off()

################################################################################
### Appendix F.2
### Appendix Figure 6: Testing Gridlock Hypothesis: Cosponsorship-Based Estimates
################################################################################
Sys.setlocale(locale="C");rm(list=ls());gc();gc();gc()
load("ST20_LSQ_replication_Appendix_F2_2.RData")

png(filename=paste(getwd(),"/AFigure6.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq2_co3)==F))
i11<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025),na.rm = T)
i12<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975),na.rm = T)
i13<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,na.rm = T)

pn<-length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))/length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq2_co3)==F))
w11<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w12<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w13<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(-0.1,0.3),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0.1),expression('GR2'['Pivotal Politics']*' - GR2'['Null']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq2_co3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq2_co3)==F))
text(cex=1.2,x=i13,y=2.05,paste(round(mean((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)),2),"-",round(pn,2)))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
pn<-length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))/length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
text(cex=1.2,x=n3,y=1.55,paste(round(mean(ne_kic_p/(ne_ooc_p+ne_kic_p)),2),"-",round(pn,2)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq2_co3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq2_co3)==F))
text(cex=1.2,x=w13,y=1.05,paste(round(mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)),2),"-",round(pn,2)))

dev.off()

################################################################################
### Appendix F.2
### Appendix Figure 7: Testing Gridlock Hypothesis: Bill-Sponsorship-Based Estimates
################################################################################
Sys.setlocale(locale="C");rm(list=ls());gc();gc();gc()
load("ST20_LSQ_replication_Appendix_F2_3.RData")

png(filename=paste(getwd(),"/AFigure7.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq4_ba3)==F))
i11<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i12<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i13<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))/length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq4_ba3)==F))
w11<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w12<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w13<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(-0.1,0.3),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0.1),expression('GR2'['Pivotal Politics']*' - GR2'['Null']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq4_ba3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq4_ba3)==F))
text(cex=1.2,x=i13,y=2.05,paste(round(mean((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)),2),"-",round(pn,2)))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
pn<-length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))/length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq4_ba3)==F))
text(cex=1.2,x=n3,y=1.55,paste(round(mean(ne_kic_p/(ne_ooc_p+ne_kic_p)),2),"-",round(pn,2)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq4_ba3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq4_ba3)==F))
text(cex=1.2,x=w13,y=1.05,paste(round(mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)),2),"-",round(pn,2)))

dev.off()

######################################################################################################################
### Appendix G
### Appendix Figure 8: Position-Taking on Bills
######################################################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_G.RData")

png(filename=paste(getwd(),"/AFigure8.png",sep=""),width=1000,height=1300)
par(mfrow=c(6,3),mai=c(2,0.75,1.5,1.75),mar=c(3.5,2,2,2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1),yaxt="n")
text(cex=3,adj=1,"",y=1,x=0)
plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.8,adj=0,"Any Lobbying Declaration",y=1.05,x=0.04)
plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.8,adj=0,"Lobbying For or Against Bills",y=1.05,x=-0.02)
plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=3,adj=1,"Iowa House",y=1.55,x=1)

df_IA_bills2<-df_IA_bills2[order(df_IA_bills2$RCID2,df_IA_bills2$RCID),]

fc1<-df_IA_bills2$any[which(df_IA_bills2$chamber=="H"&df_IA_bills2$floor_stat_NEW==1&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-df_IA_bills2$any[which(df_IA_bills2$chamber=="H"&df_IA_bills2$floor_stat_NEW==0&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]

#function to winsorize by the 5th percentile on each end of the data
#source: https://exploratory.io/note/kanaugust/How-to-winsorize-by-the-5th-percentile-on-each-end-of-my-data-vAB2fkQ0VQ
winsor1<-function (x, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
ll<-max(fc0,fc1)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2.5,yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)
fc1<-df_IA_bills2$posneg[which(df_IA_bills2$chamber=="H"&df_IA_bills2$floor_stat_NEW==1&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-df_IA_bills2$posneg[which(df_IA_bills2$chamber=="H"&df_IA_bills2$floor_stat_NEW==0&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2.5,ylim=c(0,ll),yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=3,adj=1,"Iowa Senate",y=1.55,x=1)

fc1<-df_IA_bills2$any[which(df_IA_bills2$chamber=="S"&df_IA_bills2$floor_stat_NEW==1&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-df_IA_bills2$any[which(df_IA_bills2$chamber=="S"&df_IA_bills2$floor_stat_NEW==0&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
ll<-max(fc0,fc1)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2,yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

fc1<-df_IA_bills2$posneg[which(df_IA_bills2$chamber=="S"&df_IA_bills2$floor_stat_NEW==1&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-df_IA_bills2$posneg[which(df_IA_bills2$chamber=="S"&df_IA_bills2$floor_stat_NEW==0&duplicated(df_IA_bills2$RCID2)==F&grepl("HR_",df_IA_bills2$RCID2)==F&grepl("SR_",df_IA_bills2$RCID2)==F)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2,ylim=c(0,ll),yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=3,adj=1,"Nebraska Legislature",y=1.55,x=1)

#without appropriations bills or resolutions (including constitutional amendment (for comparability))
df_NE_bills2<-df_NE_bills2[order(df_NE_bills2$RCID2,df_NE_bills$RCID),]

fc1<-df_NE_bills2$any[which(df_NE_bills2$floor_stat_NEW==1&duplicated(df_NE_bills2$RCID2)==F&grepl("LR",df_NE_bills2$RCID2)==F&grepl("[0-9]A",df_NE_bills2$RCID2)==F)]
fc0<-df_NE_bills2$any[which(df_NE_bills2$floor_stat_NEW==0&duplicated(df_NE_bills2$RCID2)==F&grepl("LR",df_NE_bills2$RCID2)==F&grepl("[0-9]A",df_NE_bills2$RCID2)==F)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
ll<-max(fc0,fc1)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2,yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

fc1<-df_NE_bills2$posneg[which(df_NE_bills2$floor_stat_NEW==1&duplicated(df_NE_bills2$RCID2)==F&grepl("LR",df_NE_bills2$RCID2)==F&grepl("[0-9]A",df_NE_bills2$RCID2)==F)]
fc0<-df_NE_bills2$posneg[which(df_NE_bills2$floor_stat_NEW==0&duplicated(df_NE_bills2$RCID2)==F&grepl("LR",df_NE_bills2$RCID2)==F&grepl("[0-9]A",df_NE_bills2$RCID2)==F)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
boxplot(fc1,fc0,main="",cex.axis=1.5,cex.main=2,ylim=c(0,ll),yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=3,adj=1,"Wisconsin Assembly",y=1.55,x=1)

df_WI_bills2<-df_WI_bills2[order(df_WI_bills2$RCID2,df_WI_bills$RCID),]

fc1<-df_WI_bills2$any[which(df_WI_bills2$chamber=="A"&df_WI_bills2$floor_stat_NEW==1&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-df_WI_bills2$any[which(df_WI_bills2$chamber=="A"&df_WI_bills2$floor_stat_NEW==0&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
ll<-max(fc0,fc1)
boxplot(main="",fc1,fc0,cex.axis=1.5,cex.main=2,yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

fc1<-df_WI_bills2$posneg[which(df_WI_bills2$chamber=="A"&df_WI_bills2$floor_stat_NEW==1&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-df_WI_bills2$posneg[which(df_WI_bills2$chamber=="A"&df_WI_bills2$floor_stat_NEW==0&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
boxplot(main="",fc1,fc0,cex.axis=1.5,cex.main=2,ylim=c(0,ll),yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=3.0,adj=1,"Wisconsin Senate",y=1.55,x=1)

fc1<-df_WI_bills2$any[which(df_WI_bills2$chamber=="S"&df_WI_bills2$floor_stat_NEW==1&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-df_WI_bills2$any[which(df_WI_bills2$chamber=="S"&df_WI_bills2$floor_stat_NEW==0&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
ll<-max(fc0,fc1)
boxplot(main="",fc1,fc0,cex.axis=1.5,cex.main=2,yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

fc1<-df_WI_bills2$posneg[which(df_WI_bills2$chamber=="S"&df_WI_bills2$floor_stat_NEW==1&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-df_WI_bills2$posneg[which(df_WI_bills2$chamber=="S"&df_WI_bills2$floor_stat_NEW==0&duplicated(df_WI_bills2$RCID2)==F&grepl("AR",df_WI_bills2$RCID2)==F&grepl("SR",df_WI_bills2$RCID2)==F&grepl("CR",df_WI_bills2$RCID2)==F&grepl("[A-Z]JR",df_WI_bills2$RCID2)==F&grepl("_0",df_WI_bills2$RCID2)==T)]
fc0<-winsor1(fc0);fc1<-winsor1(fc1);t.test(fc1,fc0)
boxplot(main="",fc1,fc0,cex.main=2,ylim=c(0,ll),yaxt="n");axis(2, at=seq(0, ll, by=5), las=2,cex.axis=2)
mtext(c("Floor Consideration","No Floor Consideration"),at=c(1,2),side=1,cex=1.15,line=2)

dev.off()

######################################################################################################################
### Appendix G
### Appendix Figure 9: Comparison of Ideal Point Estimates and CFscores
######################################################################################################################
rm(list=ls());gc();gc();gc();sSys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_G.RData")

png(filename=paste(getwd(),"/AFigure9.png",sep=""), width=1200 , height=450)
par(mfrow=c(1,3),mai=c(0.65,0.65,0.65,0.65))

plot(yaxt="n",cex.main=2.25,cex.lab=1.9,cex.axis=2,asp=1,main="(1) Iowa",xlab="CFscore Scale",ylab="Ideal Point Scale",lwd=2.5,ylim=c(-3.1,3.1),xlim=c(-3.1,3.1),i_id16$bonica2_cf_most_co_overall[which(i_id16$corp_trade%in%c(0)==T&i_id16$bonica2_nd_most_overall>1)],as.numeric(i_id16$ideal[which(i_id16$corp_trade%in%c(0)==T&i_id16$bonica2_nd_most_overall>1)]))
axis(2,at=seq(-3,3,1),cex.axis=2)

plot(yaxt="n",cex.main=2.25,cex.lab=1.9,cex.axis=2,asp=1,main="(2) Nebraska",xlab="CFscore Scale",ylab="Ideal Point Scale",lwd=2.5,ylim=c(-3.1,3.1),xlim=c(-3.1,3.1),n_id16$bonica2_cf_most_co_overall[which(n_id16$corp_trade%in%c(0)==T&n_id16$bonica2_nd_most_overall>1)],as.numeric(n_id16$ideal[which(n_id16$corp_trade%in%c(0)==T&n_id16$bonica2_nd_most_overall>1)]))
axis(2,at=seq(-3,3,1),cex.axis=2)

plot(yaxt="n",cex.main=2.25,cex.lab=1.9,cex.axis=2,asp=1,main="(3) Wisconsin",xlab="CFscore Scale",ylab="Ideal Point Scale",lwd=2.5,ylim=c(-3.1,3.1),xlim=c(-3.1,3.1),w_id16$bonica2_cf_most_co_overall[which(w_id16$corp_trade%in%c(0)==T&w_id16$bonica2_nd_most_overall>1)],as.numeric(w_id16$ideal[which(w_id16$corp_trade%in%c(0)==T&w_id16$bonica2_nd_most_overall>1)]))
axis(2,at=seq(-3,3,1),cex.axis=2)

dev.off()

cor.test(use="pairwise.complete.obs",i_id16$bonica2_cf_most_co_overall[which(i_id16$corp_trade%in%c(0)==T&i_id16$bonica2_nd_most_overall>1)],as.numeric(i_id16$ideal[which(i_id16$corp_trade%in%c(0)==T&i_id16$bonica2_nd_most_overall>1)]))
#n=63 0.85

cor.test(use="pairwise.complete.obs",n_id16$bonica2_cf_most_co_overall[which(n_id16$corp_trade%in%c(0)==T&n_id16$bonica2_nd_most_overall>1)],as.numeric(n_id16$ideal[which(n_id16$corp_trade%in%c(0)==T&n_id16$bonica2_nd_most_overall>1)]))
#n=35 0.73

cor.test(use="pairwise.complete.obs",w_id16$bonica2_cf_most_co_overall[which(w_id16$corp_trade%in%c(0)==T&w_id16$bonica2_nd_most_overall>1)],as.numeric(w_id16$ideal[which(w_id16$corp_trade%in%c(0)==T&w_id16$bonica2_nd_most_overall>1)]))
#n=56 0.80

##########################################
### Appendix H.1
### Appendix Figure 10: Testing Gatekeeping Hypothesis (692 Robustness)
##########################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H1_1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure10.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
n1<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-pn)

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i12<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i13<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i22<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i23<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w12<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w13<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w22<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w23<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 3
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h))

i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s))

n1<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p))

w11<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

dev.off()

#############################################################
### Appendix H.1
# Correlation between estimates using matrix of 9,9,2 vs. 6,9,2
#############################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H1_1.RData")

#0.8819749
cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#ne 0.811458 
cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#wi 0.8956284
cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")

#number of comparisons
#IA 157
2+cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter
#NE 185
2+cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter
#WI 1117
2+cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter

#number of bills in the sample
#IA House 10769
length(which(grepl("_0",df_IA_bills$RCID2)==T&df_IA_bills$chamber=="H"&grepl("CR",df_IA_bills$RCID2)==F&grepl("SR",df_IA_bills$RCID2)==F&grepl("HR",df_IA_bills$RCID2)==F&is.na(df_IA_bills$reason_exc_duplicated)==T&is.na(df_IA_bills$reason_exc_nogov)==T&is.na(df_IA_bills$reason_exc_budget)&is.na(df_IA_bills$reason_exc_sbFollowUpEst)))
#IA Senate 8205
length(which(grepl("_0",df_IA_bills$RCID2)==T&df_IA_bills$chamber=="S"&grepl("CR",df_IA_bills$RCID2)==F&grepl("SR",df_IA_bills$RCID2)==F&grepl("HR",df_IA_bills$RCID2)==F&is.na(df_IA_bills$reason_exc_duplicated)==T&is.na(df_IA_bills$reason_exc_nogov)==T&is.na(df_IA_bills$reason_exc_budget)&is.na(df_IA_bills$reason_exc_sbFollowUpEst)))
#NE  8329
length(which(grepl("_0",df_NE_bills$RCID2)==T&grepl("LR",df_NE_bills$RCID2)==F&is.na(df_NE_bills$reason_exc_dup)==T&is.na(df_NE_bills$reason_exc_appr)==T&df_NE_bills$reason_exc_budget==0))+length(which(grepl("CA",df_NE_bills$RCID2)==T))
#WI Assembly 4720
length(which(grepl("_0",df_WI_bills$RCID2)==T&df_WI_bills$chamber=="A"&grepl("[A-Z]JR",df_WI_bills$RCID2)==F&grepl("SR",df_WI_bills$RCID2)==F&grepl("AR",df_WI_bills$RCID2)==F&is.na(df_WI_bills$reason_exc_dup)==T&is.na(df_WI_bills$reason_exc_res)==T&df_WI_bills$reason_exc_budget==0))
#WI Senate 6947
length(which(grepl("_0",df_WI_bills$RCID2)==T&df_WI_bills$chamber=="S"&grepl("[A-Z]JR",df_WI_bills$RCID2)==F&grepl("SR",df_WI_bills$RCID2)==F&grepl("AR",df_WI_bills$RCID2)==F&is.na(df_WI_bills$reason_exc_dup)==T&is.na(df_WI_bills$reason_exc_res)==T&df_WI_bills$reason_exc_budget==0))

#number of estimates in the sample
c1<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
c2<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
c3<-length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
c4<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
c5<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))

#number of estimates with floor consideration
cc1<-length(which(df_IA_bills$floor_stat_NEW==1&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
cc2<-length(which(df_IA_bills$floor_stat_NEW==1&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
cc3<-length(which(df_NE_bills$floor_stat_NEW==1&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
cc4<-length(which(df_WI_bills$floor_stat_NEW==1&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
cc5<-length(which(df_WI_bills$floor_stat_NEW==1&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))

cc1;cc2;cc3;cc4;cc5
#IA House 353 / 929 (38%)
#IA Senate 347 / 731 (48%)
#NE 79 / 735 (11%)
#WI Assembly 406 / 754 (54%)
#WI Senate 272 / 538 (51%)
c1;c2;c3;c4;c5

#proportion of target population
929/10769 # IA House 0.08626613
731/8205 # IA Senate 0.08909202
735/8329 # NE 0.08824589
754/4720 # WI Assembly 0.1597458
538/6947 # WI Senate  0.0774435

#increase to 992 sample
c(929-686)/686 #35%
c(731-564)/564 #30%
c(735-538)/538 #37%
c(754-606)/606 #24%
c(538-437)/437 #23%

#additional bills from using 692 instead of 992
c(929-686)+c(731-564)+c(735-538)+c(754-606)+c(538-437)

d1<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))
d2<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))
d3<-length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
d4<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))
d5<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))

d1 # 67
c1 # 687
d2 # 40
c2 # 562
d3 # 132
c3 # 538
d4 # 511
c4 # 606
d5 # 371
c5 # 437

#number of estimates in the sample
c1+c2+c3+c4+c5 #3687 ;  c(3687-2830)/2830
d1+d2+d3+d4+d5 #1121

c(2830-1121)/1121

#number of estimates in the sample
e1<-length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
e2<-length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
e3<-length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
e4<-length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
e5<-length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))

e1;e2;e3;e4;e5
# IA H 728
# IA S 536
# NE 735
# WI A 754
# WI S 538

####################################################################
### Appendix H.1
### Appendix Figure 11: Testing Gridlock Hypothesis (692 Robustness)
####################################################################
rm(list=ls());gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H1_2.RData")

png(filename=paste(getwd(),"/AFigure11.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i12<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i13<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-c(0+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(0+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n1<-quantile(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w12<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w13<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(-0.1,0.3),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0.1),expression('GR2'['Pivotal Politics']*' - GR2'['Null']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
text(cex=1.2,x=i13,y=2.05,paste(round(mean((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)),2),"-",round(pn,2)))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
pn<-c(0+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(0+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
text(cex=1.2,x=n3,y=1.55,paste(round(mean(c(ne_kic_p)/c(ne_ooc_p+ne_kic_p)),2),"-",round(pn,2)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
text(cex=1.2,x=w13,y=1.05,paste(round(mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)),2),"-",round(pn,2)))

dev.off()

####################################################################################
### Appendix H.2
### Appendix Figure 12 Testing Gatekeeping Hypothesis (alternative Null) 
####################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure12.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(1-ia_ooc_h/(ia_ooc_h+ia_kic_h),c(0.025))
i12<-quantile(1-ia_ooc_h/(ia_ooc_h+ia_kic_h),c(0.975))
i13<-mean(1-ia_ooc_h/(ia_ooc_h+ia_kic_h))

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(1-ia_ooc_s/(ia_ooc_s+ia_kic_s),c(0.025))
i22<-quantile(1-ia_ooc_s/(ia_ooc_s+ia_kic_s),c(0.975))
i23<-mean(1-ia_ooc_s/(ia_ooc_s+ia_kic_s))

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
n1<-quantile(1-ne_ooc/(ne_ooc+ne_kic),c(0.025))
n2<-quantile(1-ne_ooc/(ne_ooc+ne_kic),c(0.975))
n3<-mean(1-ne_ooc/(ne_ooc+ne_kic))

n12<-quantile(1-c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d),c(0.025))
n22<-quantile(1-c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d),c(0.975))
n32<-mean(1-c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d))

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(1-ooc_a/(ooc_a+kic_a),c(0.025))
w12<-quantile(1-ooc_a/(ooc_a+kic_a),c(0.975))
w13<-mean(1-ooc_a/(ooc_a+kic_a))

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(1-ooc_s/(ooc_s+kic_s),c(0.025))
w22<-quantile(1-ooc_s/(ooc_s+kic_s),c(0.975))
w23<-mean(1-ooc_s/(ooc_s+kic_s))

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(0,1),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.5),expression('PC'['Null (No Agenda Control)']*' - PC'['Party Cartel']),cex=1.5)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(1-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(1-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(1-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h))

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(1-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(1-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(1-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s))

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))
n1<-quantile(1-ne_ooc_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(1-ne_ooc_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(1-ne_ooc_p/(ne_ooc_p+ne_kic_p))

n12<-quantile(1-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.025))
n22<-quantile(1-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.975))
n32<-mean(1-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d))

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(1-ooc_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(1-ooc_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(1-ooc_a_p/(ooc_a_p+kic_a_p))

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(1-ooc_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(1-ooc_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(1-ooc_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(0,1),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.5),expression('PC'['Null (No Agenda Control)']*' - PC'['Pivotal Politics']),cex=1.5)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

##PANEL 3
i11<-quantile(ia_ooc_h/(ia_ooc_h+ia_kic_h)-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(ia_ooc_h/(ia_ooc_h+ia_kic_h)-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(ia_ooc_h/(ia_ooc_h+ia_kic_h)-ia_ooc_p_h/(ia_ooc_p_h+ia_kic_p_h))

i21<-quantile(ia_ooc_s/(ia_ooc_s+ia_kic_s)-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(ia_ooc_s/(ia_ooc_s+ia_kic_s)-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(ia_ooc_s/(ia_ooc_s+ia_kic_s)-ia_ooc_p_s/(ia_ooc_p_s+ia_kic_p_s))

n1<-quantile(ne_ooc/(ne_ooc+ne_kic)-ne_ooc_p/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(ne_ooc/(ne_ooc+ne_kic)-ne_ooc_p/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(ne_ooc/(ne_ooc+ne_kic)-ne_ooc_p/(ne_ooc_p+ne_kic_p))

n12<-quantile(c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d)-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.025))
n22<-quantile(c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d)-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.975))
n32<-mean(c(ne_ooc+ne_ooc_d)/(ne_ooc+ne_kic+ne_ooc_d)-c(ne_ooc_p+ne_ooc_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d))

w11<-quantile(ooc_a/(ooc_a+kic_a)-ooc_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(ooc_a/(ooc_a+kic_a)-ooc_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(ooc_a/(ooc_a+kic_a)-ooc_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(ooc_s/(ooc_s+kic_s)-ooc_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(ooc_s/(ooc_s+kic_s)-ooc_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(ooc_s/(ooc_s+kic_s)-ooc_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.4,0.4),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0),expression('PC'['Party Cartel']*' - PC'['Pivotal Politics']),cex=1.5)
points(x=i13,y=5,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(c(n1,n2),c(3,3),lwd=2)
points(x=n3,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)

dev.off()

####################################################################
### Appendix H.2
### Appendix Figure 13 Testing Gridlock Hypothesis (alternative null)
####################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication2.RData")

png(filename=paste(getwd(),"/AFigure13.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(1-(ia_ooc_p_h+ia_ooc_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s),c(0.025))
i12<-quantile(1-(ia_ooc_p_h+ia_ooc_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s),c(0.975))
i13<-mean(1-(ia_ooc_p_h+ia_ooc_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s))

pn<-c(0+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(0+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n1<-quantile(1-c(ne_ooc_p)/(ne_ooc_p+ne_kic_p),c(0.025))
n2<-quantile(1-c(ne_ooc_p)/(ne_ooc_p+ne_kic_p),c(0.975))
n3<-mean(1-c(ne_ooc_p)/(ne_ooc_p+ne_kic_p))

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(1-(ooc_a_p+ooc_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p),c(0.025))
w12<-quantile(1-(ooc_a_p+ooc_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p),c(0.975))
w13<-mean(1-(ooc_a_p+ooc_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(0,1),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0.5),expression('PC'['Null (No Agenda Control)']*' - PC'['Pivotal Politics']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
pn<-c(0+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(0+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))

dev.off()

####################################################################################
### Appendix H.3
### Appendix Figure 14: Testing Gatekeeping Hypothesis with robustness checks for 
### Nebraska cutpoints and timing of disclosures in Iowa and Wisconsin
####################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

png(filename=paste(getwd(),"/AFigure14.png",sep=""),width=1200,height=650)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.5,adj=1,"Iowa House",y=5,x=1)
text(cex=2.5,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.5,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.5,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.5,adj=1,"Wisconsin Senate",y=1,x=1)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025)) ; i11
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975)) ; i12
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn) ; i13

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025)) ; i21
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975)) ; i22
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn) ; i23

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/c(length(which(df_NE_bills$nonzero_fp_cutpoint==1))+length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n1<-quantile(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-pn,c(0.025)) ; n1
n2<-quantile(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-pn,c(0.975)) ; n2
n3<-mean(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-pn) ; n3

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/c(length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n10<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.025)) ; n10
n20<-quantile(ne_kic/(ne_ooc+ne_kic)-pn,c(0.975)); n20
n30<-mean(ne_kic/(ne_ooc+ne_kic)-pn); n30

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025)) ; w11
w12<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975)); w12
w13<-mean(kic_a/(ooc_a+kic_a)-pn); w13

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025)) ; w21
w22<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975)); w22
w23<-mean(kic_s/(ooc_s+kic_s)-pn); w23

Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H3_1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i110<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.025)); i110
i120<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn,c(0.975)); i120
i130<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-pn); i130

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i210<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.025)); i210
i220<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn,c(0.975)); i220
i230<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-pn); i230

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w110<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.025)); w110
w120<-quantile(kic_a/(ooc_a+kic_a)-pn,c(0.975)); w120
w130<-mean(kic_a/(ooc_a+kic_a)-pn); w130

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w210<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.025)); w210
w220<-quantile(kic_s/(ooc_s+kic_s)-pn,c(0.975)); w220
w230<-mean(kic_s/(ooc_s+kic_s)-pn); w230

plot(xlab="",bty="n",lwd=2,cex.main=3,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
lines(x=c(i110,i120),y=c(5,5),pch=19,lwd=2,col="grey")
points(x=i130,y=5,pch=19,lwd=2,col="grey")
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(x=c(i210,i220),y=c(4,4),pch=19,lwd=2,col="grey")
points(x=i230,y=4,pch=19,lwd=2,col="grey")
lines(c(n1,n2),c(3,3),lwd=2,col="grey")
points(x=n3,y=3,pch=19,lwd=2,col="grey")
lines(c(n10,n20),c(3,3),lwd=2)
points(x=n30,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
lines(x=c(w110,w120),y=c(2,2),pch=19,lwd=2,col="grey")
points(x=w130,y=2,pch=19,lwd=2,col="grey")
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)
lines(x=c(w210,w220),y=c(1,1),pch=19,lwd=2,col="grey")
points(x=w230,y=1,pch=19,lwd=2,col="grey")

Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

##PANEL 2
pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i12<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i13<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i21<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i22<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i23<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/c(length(which(df_NE_bills$nonzero_fp_cutpoint==1))+length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n1<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d)-pn,c(0.025))
n2<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d)-pn,c(0.975))
n3<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d)-pn)

pn<-length(which(df_NE_bills$floor_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F))/c(length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n10<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n20<-quantile(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n30<-mean(ne_kic_p/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w12<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w13<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w21<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w22<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w23<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H3_1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq5_aug3)==F))
i110<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.025))
i120<-quantile(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn,c(0.975))
i130<-mean(ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h)-pn)

pn<-length(which(df_IA_bills$floor_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq5_aug3)==F))
i210<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i220<-quantile(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i230<-mean(ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq5_aug3)==F))
w110<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.025))
w120<-quantile(kic_a_p/(ooc_a_p+kic_a_p)-pn,c(0.975))
w130<-mean(kic_a_p/(ooc_a_p+kic_a_p)-pn)

pn<-length(which(df_WI_bills$floor_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq5_aug3)==F))
w210<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.025))
w220<-quantile(kic_s_p/(ooc_s_p+kic_s_p)-pn,c(0.975))
w230<-mean(kic_s_p/(ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
lines(x=c(i110,i120),y=c(5,5),pch=19,lwd=2,col="grey")
points(x=i130,y=5,pch=19,lwd=2,col="grey")
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(x=c(i210,i220),y=c(4,4),pch=19,lwd=2,col="grey")
points(x=i230,y=4,pch=19,lwd=2,col="grey")
lines(c(n1,n2),c(3,3),lwd=2,col="grey")
points(x=n3,y=3,pch=19,lwd=2,col="grey")
lines(c(n10,n20),c(3,3),lwd=2)
points(x=n30,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
lines(x=c(w110,w120),y=c(2,2),pch=19,lwd=2,col="grey")
points(x=w130,y=2,pch=19,lwd=2,col="grey")
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)
lines(x=c(w210,w220),y=c(1,1),pch=19,lwd=2,col="grey")
points(x=w230,y=1,pch=19,lwd=2,col="grey")

##PANEL 3
Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

i11<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i12<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i13<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h))

i21<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i22<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i23<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s))

n1<-quantile(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.025))
n2<-quantile(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d),c(0.975))
n3<-mean(ne_kic/(ne_ooc+ne_kic+ne_ooc_d)-ne_kic_p/(ne_ooc_p+ne_kic_p+ne_ooc_p_d))

n10<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.025))
n20<-quantile(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p),c(0.975))
n30<-mean(ne_kic/(ne_ooc+ne_kic)-ne_kic_p/(ne_ooc_p+ne_kic_p))

w11<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w12<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w13<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w21<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w22<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w23<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H3_1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

i110<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.025))
i120<-quantile(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h),c(0.975))
i130<-mean(ia_kic_h/(ia_ooc_h+ia_kic_h)-ia_kic_p_h/(ia_ooc_p_h+ia_kic_p_h))

i210<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.025))
i220<-quantile(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s),c(0.975))
i230<-mean(ia_kic_s/(ia_ooc_s+ia_kic_s)-ia_kic_p_s/(ia_ooc_p_s+ia_kic_p_s))

w110<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.025))
w120<-quantile(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p),c(0.975))
w130<-mean(kic_a/(ooc_a+kic_a)-kic_a_p/(ooc_a_p+kic_a_p))

w210<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.025))
w220<-quantile(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p),c(0.975))
w230<-mean(kic_s/(ooc_s+kic_s)-kic_s_p/(ooc_s_p+kic_s_p))

plot(bty="n",lwd=2,cex.main=3,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=i13,y=5,pch=19,lwd=2)
lines(x=c(i110,i120),y=c(5,5),pch=19,lwd=2,col="grey")
points(x=i130,y=5,pch=19,lwd=2,col="grey")
abline(v=0,lty=3,lwd=2)
lines(c(i21,i22),c(4,4),lwd=2)
points(x=i23,y=4,pch=19,lwd=2)
lines(x=c(i210,i220),y=c(4,4),pch=19,lwd=2,col="grey")
points(x=i230,y=4,pch=19,lwd=2,col="grey")
lines(c(n1,n2),c(3,3),lwd=2,col="grey")
points(x=n3,y=3,pch=19,lwd=2,col="grey")
lines(c(n10,n20),c(3,3),lwd=2)
points(x=n30,y=3,pch=19,lwd=2)
lines(c(w11,w12),c(2,2),lwd=2)
lines(x=c(w110,w120),y=c(2,2),pch=19,lwd=2,col="grey")
points(x=w130,y=2,pch=19,lwd=2,col="grey")
points(x=w13,y=2,pch=19,lwd=2)
lines(c(w21,w22),c(1,1),lwd=2)
points(x=w23,y=1,pch=19,lwd=2)
lines(x=c(w210,w220),y=c(1,1),pch=19,lwd=2,col="grey")
points(x=w230,y=1,pch=19,lwd=2,col="grey")

dev.off()

###########################################################################
### Appendix H.3
### Appendix Figure 15 Testing Gridlock Hypothesis, robustness checks with 
### Nebraska cutpoints and Iowa and Wisconsin Timing
###########################################################################
rm(list=ls());gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication2.RData")

png(filename=paste(getwd(),"/AFigure15.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

##PANEL 1
pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
i11<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i12<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i13<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-c(length(which(df_NE_bills$nonzero_fp_cutpoint==1&df_NE_bills$enac_stat_NEW==0))+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(length(which(df_NE_bills$nonzero_fp_cutpoint==1&is.na(df_NE_bills$enac_stat_NEW)==F))+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n10<-quantile(c(ne_kic_p+ne_kic_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d+ne_kic_p)-pn,c(0.025))
n20<-quantile(c(ne_kic_p+ne_kic_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d+ne_kic_p)-pn,c(0.975))
n30<-mean(c(ne_kic_p+ne_kic_p_d)/(ne_ooc_p+ne_kic_p+ne_ooc_p_d+ne_kic_p)-pn)

pn<-c(length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
n1<-quantile(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn,c(0.025))
n2<-quantile(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn,c(0.975))
n3<-mean(c(ne_kic_p)/(ne_ooc_p+ne_kic_p)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
w11<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w12<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w13<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

Sys.setlocale(locale="C")
load("ST20_LSQ_replication_Appendix_H3_2.RData")

pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
i110<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.025))
i120<-quantile((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn,c(0.975))
i130<-mean((ia_kic_p_s+ia_kic_p_h)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)-pn)

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
w110<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.025))
w120<-quantile((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn,c(0.975))
w130<-mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)-pn)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(2,2),xlim=c(-0.4,0.4),ylim=c(1,2.1),type="l")
mtext(side=1,line=5,at=c(0),expression('GR2'['Pivotal Politics']*' - GR2'['Null']),cex=1.75)
points(x=i13,y=2,pch=19,lwd=2)
lines(x=c(i110,i120),y=c(2,2),pch=19,lwd=2,col="grey")
points(x=i130,y=2,pch=19,lwd=2,col="grey")

pn<-length(which(df_IA_bills$enac_stat_NEW==0&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))/length(which(is.na(df_IA_bills$enac_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber%in%c("H","S")&is.na(df_IA_bills$sq5_aug3)==F))
text(cex=1.2,x=i130,y=2.05,paste(round(mean((ia_kic_p_h+ia_kic_p_s)/(ia_ooc_p_h+ia_kic_p_h+ia_ooc_p_s+ia_kic_p_s)),2),"-",round(pn,2)))
abline(v=0,lty=3,lwd=2)
lines(c(n1,n2),c(1.5,1.5),lwd=2)
points(x=n3,y=1.5,pch=19,lwd=2)
lines(x=c(n10,n20),y=c(1.5,1.5),pch=19,lwd=2,col="grey")
points(x=n30,y=1.5,pch=19,lwd=2,col="grey")

pn<-c(length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$nonzero_fp_cutpoint==1))+length(which(df_NE_bills$enac_stat_NEW==0&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))/c(length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$nonzero_fp_cutpoint==1))+length(which(is.na(df_NE_bills$enac_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq5_aug3)==F)))
text(cex=1.2,x=n30,y=1.55,paste(round(mean(c(ne_kic_p+ne_kic_p_d)/c(ne_ooc_p+ne_kic_p+ne_ooc_p_d+ne_kic_p)),2),"-",round(pn,2)))
lines(c(w11,w12),c(1,1),lwd=2)
points(x=w13,y=1,pch=19,lwd=2)
lines(x=c(w110,w120),y=c(1,1),pch=19,lwd=2,col="grey")
points(x=w130,y=1,pch=19,lwd=2,col="grey")

pn<-length(which(df_WI_bills$enac_stat_NEW==0&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))/length(which(is.na(df_WI_bills$enac_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber%in%c("A","S")&is.na(df_WI_bills$sq5_aug3)==F))
text(cex=1.2,x=w130,y=1.05,paste(round(mean((kic_a_p+kic_s_p)/(ooc_a_p+kic_a_p+ooc_s_p+kic_s_p)),2),"-",round(pn,2)))

dev.off()

#########################################################################################################
### Appendix H.4
### Appendix Figure 16 Comparing Gatekeeping for Chambers under Democratic and Republican Control 
### Compare Parties Democrats Republicans Differences Party
#########################################################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

png(filename=paste(getwd(),"/AFigure16.png",sep=""),width=1200,height=650)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

ia_ooc_dems<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_ooc_dems_p<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p
ia_kic_dems<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_kic_dems_p<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p

ia_ooc_reps<-ooc81s+ooc81h+ooc82h+ooc83h+ooc84h+ooc85h+ooc86h
ia_ooc_reps_p<-ooc81s_p+ooc81h_p+ooc82h_p+ooc83h_p+ooc84h_p+ooc85h_p+ooc86h_p
ia_kic_reps<-kic81s+kic81h+kic82h+kic83h+kic84h+kic85h+kic86h
ia_kic_reps_p<-kic81s_p+kic81h_p+kic82h_p+kic83h_p+kic84h_p+kic85h_p+kic86h_p

wi_ooc_dems<-ooc98s+ooc99a+ooc99s
wi_ooc_dems_p<-ooc98s_p+ooc99a_p+ooc99s_p
wi_kic_dems<-kic98s+kic99a+kic99s
wi_kic_dems_p<-kic98s_p+kic99a_p+kic99s_p

wi_ooc_reps<-ooc96a+ooc97a+ooc98a+ooc100a+ooc101a+ooc102a+ooc96s+ooc97s+ooc99s+ooc100s+ooc101s+ooc102s
wi_ooc_reps_p<-ooc96a_p+ooc97a_p+ooc98a_p+ooc100a_p+ooc101a_p+ooc102a_p+ooc96s_p+ooc97s_p+ooc99s_p+ooc100s_p+ooc101s_p+ooc102s_p
wi_kic_reps<-kic96a+kic97a+kic98a+kic100a+kic101a+kic102a+kic96s+kic97s+kic99s+kic100s+kic101s+kic102s
wi_kic_reps_p<-kic96a_p+kic97a_p+kic98a_p+kic100a_p+kic101a_p+kic102a_p+kic96s_p+kic97s_p+kic99s_p+kic100s_p+kic101s_p+kic102s_p

ooc_reps<-ia_ooc_reps+wi_ooc_reps
kic_reps<-ia_kic_reps+wi_kic_reps
ooc_dems<-ia_ooc_dems+wi_ooc_dems
kic_dems<-ia_kic_dems+wi_kic_dems

ooc_reps_p<-ia_ooc_reps_p+wi_ooc_reps_p
kic_reps_p<-ia_kic_reps_p+wi_kic_reps_p
ooc_dems_p<-ia_ooc_dems_p+wi_ooc_dems_p
kic_dems_p<-ia_kic_dems_p+wi_kic_dems_p
#upper lower
ooc_u<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s+ooc96s+ooc97s+ooc99s+ooc100s+ooc101s+ooc102s+ooc98s+ooc99s
kic_u<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s+kic96s+kic97s+kic99s+kic100s+kic101s+kic102s+kic98s+kic99s
ooc_l<-ooc81h+ooc82h+ooc83h+ooc84h+ooc85h+ooc86h+ooc96a+ooc97a+ooc99a+ooc100a+ooc101a+ooc102a+ooc98a+ooc99a
kic_l<-kic81h+kic82h+kic83h+kic84h+kic85h+kic86h+kic96a+kic97a+kic99a+kic100a+kic101a+kic102a+kic98a+kic99a

ooc_u_p<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p+ooc96s_p+ooc97s_p+ooc99s_p+ooc100s_p+ooc101s_p+ooc102s_p+ooc98s_p+ooc99s
kic_u_p<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p+kic96s_p+kic97s_p+kic99s_p+kic100s_p+kic101s_p+kic102s_p+kic98s_p+kic99s
ooc_l_p<-ooc81h_p+ooc82h_p+ooc83h_p+ooc84h_p+ooc85h_p+ooc86h_p+ooc96a_p+ooc97a_p+ooc99a_p+ooc100a_p+ooc101a_p+ooc102a_p+ooc98a_p+ooc99a
kic_l_p<-kic81h_p+kic82h_p+kic83h_p+kic84h_p+kic85h_p+kic86h_p+kic96a_p+kic97a_p+kic99a_p+kic100a_p+kic101a_p+kic102a_p+kic98a_p+kic99a

a10<-quantile(c(kic_l)/c(ooc_l+kic_l)-c(kic_l_p)/c(ooc_l_p+kic_l_p),c(0.025))
a20<-quantile(c(kic_l)/c(ooc_l+kic_l)-c(kic_l_p)/c(ooc_l_p+kic_l_p),c(0.975))
a30<-mean(c(kic_l)/c(ooc_l+kic_l)-c(kic_l_p)/c(ooc_l_p+kic_l_p))

b10<-quantile(c(kic_u)/c(ooc_u+kic_u)-c(kic_u_p)/c(ooc_u_p+kic_u_p),c(0.025))
b20<-quantile(c(kic_u)/c(ooc_u+kic_u)-c(kic_u_p)/c(ooc_u_p+kic_u_p),c(0.975))
b30<-mean(c(kic_u)/c(ooc_u+kic_u)-c(kic_u_p)/c(ooc_u_p+kic_u_p))

a1<-quantile(c(kic_dems)/c(ooc_dems+kic_dems)-c(kic_dems_p)/c(ooc_dems_p+kic_dems_p),c(0.025))
a2<-quantile(c(kic_dems)/c(ooc_dems+kic_dems)-c(kic_dems_p)/c(ooc_dems_p+kic_dems_p),c(0.975))
a3<-mean(c(kic_dems)/c(ooc_dems+kic_dems)-c(kic_dems_p)/c(ooc_dems_p+kic_dems_p))

b1<-quantile(c(kic_reps)/c(ooc_reps+kic_reps)-c(kic_reps_p)/c(ooc_reps_p+kic_reps_p),c(0.025))
b2<-quantile(c(kic_reps)/c(ooc_reps+kic_reps)-c(kic_reps_p)/c(ooc_reps_p+kic_reps_p),c(0.975))
b3<-mean(c(kic_reps)/c(ooc_reps+kic_reps)-c(kic_reps_p)/c(ooc_reps_p+kic_reps_p))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(0,3))
text(cex=2.5,adj=1,"Lower Chambers",y=3,x=1)
text(cex=2.5,adj=1,"Upper Chambers",y=2,x=1)
text(cex=2.5,adj=1,"Democratic Control",y=1,x=1)
text(cex=2.5,adj=1,"Republican Control",y=0,x=1)

plot(bty="n",lwd=2,cex.main=3,main="",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(a10,a20),c(3,3),xlim=c(-0.4,0.4),ylim=c(0,3),type="l")
mtext(side=1,line=5,at=c(0),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.75)
points(x=a30,y=3,pch=19,lwd=2)
lines(x=c(b10,b20),y=c(2,2),pch=19,lwd=2)
points(x=b30,y=2,pch=19,lwd=2)
lines(x=c(a1,a2),y=c(1,1),pch=19,lwd=2)
points(x=a3,y=1,pch=19,lwd=2)
lines(x=c(b1,b2),y=c(0,0),pch=19,lwd=2)
points(x=b3,y=0,pch=19,lwd=2)
abline(v=0,lty=3,lwd=2)

dev.off()
