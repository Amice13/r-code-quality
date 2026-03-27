Sys.setlocale(locale="C")
#library(pscl)
#library(rstan)
rm(list=ls());gc()
default_pars<-par()
#setwd("Uncomment and enter name of folder containing RData files")

################################################
# 
# Replication of figures/results in the article
# 
################################################

###########################################################
##### Figure 1 showing blockout zone and gridlock interval
###########################################################
rm(list=ls());gc()
f<-0.7;p<--1;m<-0;v<--2
seqs<-seq(-4,4,0.001)
bp<-c();for(i in 1:length(seqs)){
  b<-seq(-4,4,0.001)
  b<-b[which(abs(f-b)<=abs(f-seqs[i]))];   b1<-b[which(abs(p-b)<=abs(p-seqs[i]))]
  b2<-b[which(abs(p-b)>abs(p-seqs[i]))];   b3<-b2[which(abs(v-b2)<abs(v-seqs[i]))]
  b1<-b1[which(abs(b1)==min(abs(m-b1)))];   b3<-b3[which(abs(b3)==min(abs(m-b3)))]
  if(length(b1)==0&length(b3)>0){bp[i]<-b3}
  if(length(b1)>0&length(b3)==0){bp[i]<-b1}
  if(length(b1)>0&length(b3)>0){
    if(abs(m-b1)<abs(m-b3)){bp[i]<-b1}
    if(abs(m-b1)>abs(m-b3)){bp[i]<-b3}}
  if(is.na(bp[i])==T){bp[i]<-seqs[i]}
}
bpp<-c();pm<--0.5;for(i in 1:length(seqs)){
  if(abs(pm-seqs[i])>abs(pm-m)){bpp[i]<-m}
  if(abs(pm-seqs[i])==abs(pm-m)){bpp[i]<-m}
  if(abs(pm-seqs[i])<abs(pm-m)){bpp[i]<-seqs[i]}
}

pdf(paste(getwd(),"/Figure1.pdf",sep=""),width=12,height=6)
par(mfrow=c(1,2),mar=c(1,2.5,1,0.9),mai=c(1,1.5,1,0.5))

plot(cex.main=2.0,cex.axis=2,cex.lab=2,
     main="Majority Party Blockout Zone\n(Party Cartel Theory)",xlim=c(-2.2,2.2),
     seqs,bpp,type="l",ylim=c(-2.2,1),xaxt="n",yaxt="n",xlab="Status Quo Position",ylab="");
rect(border=NA,2*pm-m,-2.3,m,1.1,col = rgb(0.5,0.5,0.5,1/4))
lines(seqs,bpp,type="l")
axis(2,at=c(m,pm,2*pm-m),labels=c("m","D","2D-m"),las=2,cex.axis=1.5)
axis(1,at=c(m,pm,2*pm-m),labels=c("m","D","2D-m"),cex.axis=1.5)
mtext(side=2,text="Outcome",line=5,cex=2)

plot(cex.main=2.0,cex.axis=2,cex.lab=2,
     main="Gridlock Interval\n(Pivotal Politics Theory)",xlim=c(-2.2,2.2),
     seqs,bp,type="l",ylim=c(-2,1),xaxt="n",yaxt="n",xlab="Status Quo Position",ylab="");
mtext(side=2,text="Outcome",line=4.5,cex=2)
axis(2,at=c(m,f,p,v),labels=c("m","f","p","v"),las=2,cex.axis=1.5)
axis(1,at=c(m,f,p,v),labels=c("m","f","p","v"),cex.axis=1.5)
rect(border=NA,p,-2.3,f,1.1,col = "grey")
lines(seqs,bp,type="l")

dev.off()
 
########################################
#Figure 2 Comparing SQ Estimates
########################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

pdf(paste(getwd(),"/Figure2.pdf",sep=""),width=12,height=7)
par(mfrow=c(1,3),mar=c(0.3,0.3,0.3,0.3),mai=c(0.3,0.7,1,0.3),pty="s")

plot(xlim=c(-5,5),asp=1,df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],cex.lab=1.75,cex.axis=1.75,cex.main=3,main="(1) Iowa",xlab="Adjusted Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]~df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend("bottomright",bty="n",cex=2,c(expression(b[0]*' =  0.00'),expression(b[1]*' =  1.17')))
plot(asp=1,df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],cex.lab=1.75,cex.axis=1.75,cex.main=3,main="(2) Nebraska",xlab="Adjusted Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]~df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend("bottomright",bty="n",cex=2,c(expression(b[0]*' = -0.29'),expression(b[1]*' =   1.13')))
plot(asp=1,df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],cex.lab=1.75,cex.axis=1.75,cex.main=3,main="(3) Wisconsin",xlab="Adjusted Vote-Based SQ Estimates",ylab="Cosponsorship-Based SQ Estimates")
lines(seq(-6,6,1),seq(-6,6,1),lty=2,col="blue",lwd=2)
abline(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]),col="red",lwd=2)
ab<-summary(lm(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]~df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)]))$coefficient[,1]
round(ab[1],2);round(ab[2],2);legend("bottomright",bty="n",cex=2,c(expression(b[0]*' =  0.10'),expression(b[1]*' =  1.20')))

dev.off()

#0.9051394 (0.91)
cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#ne 0.8029848
cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")
#wi 0.8896031
cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")

#number of comparisons
#IA 107
2+cor.test(df_IA_bills$sq2_co3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],df_IA_bills$sq3_est_adj3[which(is.na(df_IA_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter
#NE 132
2+cor.test(df_NE_bills$sq2_co3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],df_NE_bills$sq3_est_adj3[which(is.na(df_NE_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter
#WI 882
2+cor.test(df_WI_bills$sq2_co3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],df_WI_bills$sq3_est_adj3[which(is.na(df_WI_bills$floor_stat_NEW)==F)],use="pairwise.complete.obs")$parameter

#number of bills in the sample
#IA House 10769
length(which(grepl("_0",df_IA_bills$RCID2)==T&df_IA_bills$chamber=="H"&grepl("CR",df_IA_bills$RCID2)==F&grepl("SR",df_IA_bills$RCID2)==F&grepl("HR",df_IA_bills$RCID2)==F&is.na(df_IA_bills$reason_exc_duplicated)==T&is.na(df_IA_bills$reason_exc_nogov)==T&is.na(df_IA_bills$reason_exc_budget)&is.na(df_IA_bills$reason_exc_sbFollowUpEst)))
#IA Senate 8205
length(which(grepl("_0",df_IA_bills$RCID2)==T&df_IA_bills$chamber=="S"&grepl("CR",df_IA_bills$RCID2)==F&grepl("SR",df_IA_bills$RCID2)==F&grepl("HR",df_IA_bills$RCID2)==F&is.na(df_IA_bills$reason_exc_duplicated)==T&is.na(df_IA_bills$reason_exc_nogov)==T&is.na(df_IA_bills$reason_exc_budget)&is.na(df_IA_bills$reason_exc_sbFollowUpEst)))
#NE  8329
length(which(grepl("_0",df_NE_bills$RCID2)==T&grepl("LR",df_NE_bills$RCID2)==F&is.na(df_NE_bills$reason_exc_dup)==T&is.na(df_NE_bills$reason_exc_appr)==T&df_NE_bills$reason_exc_budget==0))+length(which(grepl("CA",df_NE_bills$RCID2)==T))
#WI Assembly 6947
length(which(grepl("_0",df_WI_bills$RCID2)==T&df_WI_bills$chamber=="A"&grepl("[A-Z]JR",df_WI_bills$RCID2)==F&grepl("SR",df_WI_bills$RCID2)==F&grepl("AR",df_WI_bills$RCID2)==F&is.na(df_WI_bills$reason_exc_dup)==T&is.na(df_WI_bills$reason_exc_res)==T&df_WI_bills$reason_exc_budget==0))
#WI Senate 4720
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
#IA House 330 / 686 (48%)
#IA Senate 321 / 564 (57%)
#NE 74 / 538 (13%)
#WI Assembly 359 / 606 (59%)
#WI Senate 246 / 437 (56%)
c1;c2;c3;c4;c5
c1+c2+c3+c4+c5

#proportion of target population
686/10769 # IA House 0.06370137
564/8205 # IA Senate 0.06873857
538/8329 # NE 0.06459359
606/6947 # WI Assembly 0.0872319
437/4720 # WI Senate  0.09258475
  
d1<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="H"&is.na(df_IA_bills$sq2_co3)==F))
d2<-length(which(is.na(df_IA_bills$floor_stat_NEW)==F&df_IA_bills$session%in%c("81","82","83","84","85","86")==T&df_IA_bills$chamber=="S"&is.na(df_IA_bills$sq2_co3)==F))
d3<-length(which(is.na(df_NE_bills$floor_stat_NEW)==F&df_NE_bills$session%in%c("098","099","100","101","102","103","104")==T&is.na(df_NE_bills$sq2_co3)==F))
d4<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="A"&is.na(df_WI_bills$sq2_co3)==F))
d5<-length(which(is.na(df_WI_bills$floor_stat_NEW)==F&df_WI_bills$session%in%c("096","097","098","099","100","101","102")==T&df_WI_bills$chamber=="S"&is.na(df_WI_bills$sq2_co3)==F))

d1 # 67
c1 # 686
d2 # 40
c2 # 564
d3 # 132
c3 # 538
d4 # 511
c4 # 606
d5 # 371
c5 # 437

#number of estimates in the sample
c1+c2+c3+c4+c5 #2831
d1+d2+d3+d4+d5 #1121

#increase in the number of estimates
c(2832-1121)/1121

##########################################
###Figure 3 Testing Gatekeeping Hypothesis
##########################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

ia_kic_s<-kic81s+kic82s+kic83s+kic84s+kic85s+kic86s
ia_ooc_s<-ooc81s+ooc82s+ooc83s+ooc84s+ooc85s+ooc86s
ia_kic_p_s<-kic81s_p+kic82s_p+kic83s_p+kic84s_p+kic85s_p+kic86s_p
ia_ooc_p_s<-ooc81s_p+ooc82s_p+ooc83s_p+ooc84s_p+ooc85s_p+ooc86s_p

pdf(paste(getwd(),"/Figure3.pdf",sep=""),width=12,height=6.5)
par(mfrow=c(1,4),mar=c(1,0.2,1,0.2),mai=c(1,0.2,1,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,5))
text(cex=2.25,adj=1,"Iowa House",y=5,x=1)
text(cex=2.25,adj=1,"Iowa Senate",y=4,x=1)
text(cex=2.25,adj=1,"Nebraska Legislature",y=3,x=1)
text(cex=2.25,adj=1,"Wisconsin Assembly",y=2,x=1)
text(cex=2.25,adj=1,"Wisconsin Senate",y=1,x=1)

##PANEL 1
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

plot(xlab="",bty="n",lwd=2,cex.main=2.5,main="(1) Party Cartel",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Null']),cex=1.25)
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

plot(bty="n",lwd=2,cex.main=2.5,main="(2) Pivotal Politics",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Pivotal Politics']*' - GR'['Null']),cex=1.25)
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

plot(bty="n",lwd=2,cex.main=2.5,main="(3) Party Cartel vs.\n      Pivotal Politics ",yaxt="n",ylab="",cex.axis=2,cex.lab=2.2,xlab="",c(i11,i12),c(5,5),xlim=c(-0.2,0.6),ylim=c(1,5),type="l")
mtext(side=1,line=5,at=c(0.2),expression('GR'['Party Cartel']*' - GR'['Pivotal Politics']),cex=1.25)
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

########################################
### Figure 4 Showing SQ Estimates
########################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication1.RData")

#rename ideal point objects
mtheta_ia<-mtheta_992tau_ia
mtheta_ne<-mtheta_992tau_ne
mtheta_wi<-mtheta_992tau_wi

pdf(paste(getwd(),"/Figure4.pdf",sep=""),width=14,height=16)
par(mfrow=c(5,2),mar=c(1,1,1,1),mai=c(0.7,0.8,1,0.2))

#function for plotting panels with SQ distributions and close-up view of theoretically relevant intervals in Iowa
sq_figs_ia<-function(sen,dc,oc,main,gleg1,gleg2,p,ye,splitc,main2){
  #plot(col="black",cex.axis=2.3,cex.main=2.5,cex=2.3,cex.lab=2.3,lwd=2.3,density(dc,na.rm=T),ylim=c(0,max(na.rm=T,max(na.rm=T,density(dc,na.rm=T)$y),max(na.rm=T,density(oc,na.rm=T)$y))+0.2),xlim=c(-max(na.rm=T,max(na.rm=T,abs(dc)),max(na.rm=T,abs(oc)))-0.2,max(na.rm=T,max(na.rm=T,abs(dc)),max(na.rm=T,abs(oc)))+0.2),xlab="Status Quo Position",
  #     main=main,yaxt="n")
  if(length(dc)>1&length(oc)<2){plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(dc,na.rm=T),ylim=c(0,max(na.rm=T,max(na.rm=T,density(dc,na.rm=T)$y))+1.5),xlim=c(c(min(mtheta_ia[,2])-1),c(max(mtheta_ia[,2])+1)),xlab="Ideal Point Scale",
                                     main=main,yaxt="n")}
  if(length(dc)>1&length(oc)>1){plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(dc,na.rm=T),ylim=c(0,max(na.rm=T,max(na.rm=T,density(dc,na.rm=T)$y),max(na.rm=T,density(oc,na.rm=T)$y))+0.4),xlim=c(c(min(mtheta_ia[,2])-1),c(max(mtheta_ia[,2])+1)),xlab="Ideal Point Scale",
                                     main=main,yaxt="n")}
  if(length(dc)>1&length(oc)>1){lines(lwd=2.3,density(na.omit(oc,na.rm=T)),col="red")};
  if(length(dc)<2&length(oc)>1){plot(col="red",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(oc,na.rm=T),ylim=c(0,max(na.rm=T,max(na.rm=T,density(oc,na.rm=T)$y))+0.4),xlim=c(c(min(mtheta_ia[,2])-1),c(max(mtheta_ia[,2])+1)),xlab="Ideal Point Scale",
                                     main=main,yaxt="n")};

  if(ye==81&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[3]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[4]])}
  if(ye==81&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[4]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[3]])}
  if(ye==82&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[5]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[6]])}
  if(ye==82&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[6]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[5]])}
  if(ye==83&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[7]][[2]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[8]])}
  if(ye==83&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[8]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[7]][[2]])}
  if(ye==84&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[9]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[10]][[1]])}
  if(ye==84&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[10]][[1]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[9]])}
  if(ye==85&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[11]][[1]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[12]][[1]])}
  if(ye==85&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[12]][[1]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[11]][[1]])}
  if(ye==86&sen==0){lgn<-gsub("_IA","",pivot_repos[[1]][[13]][[1]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[14]])}
  if(ye==86&sen==1){lgn<-gsub("_IA","",pivot_repos[[1]][[14]]);lgn2<-gsub("_IA","",pivot_repos[[1]][[13]][[1]])}
  if(splitc==0){lgnp<-lgn[grep(p,lgn)]}
  if(splitc==1){lgnp1<-lgn[grep(p[1],lgn)]}
  if(splitc==1){lgnp2<-lgn[grep(p[2],lgn)]}

  #get pivots
  #
  if(ye==81){gov<-mean(mtheta_ia[grep("Vilsack Administration",rownames(mtheta_ia)),2])}
  if(ye%in%c(82,83)){gov<-mean(mtheta_ia[grep("Culver Adminis",rownames(mtheta_ia)),2])}
  if(ye%in%c(84,85,86)){gov<-mean(mtheta_ia[grep("Branstad Adminis",rownames(mtheta_ia)),2])}
  me<-median(mtheta_ia[rownames(mtheta_ia)%in%lgn,2]);me2<-median(mtheta_ia[rownames(mtheta_ia)%in%lgn2,2])
  
  if(gov>=max(me,me2)){p2<-min(max(sort(mtheta_ia[rownames(mtheta_ia)%in%lgn,2])[67],sort(mtheta_ia[rownames(mtheta_ia)%in%lgn2,2])[34]),gov);p1<-min(me,me2)}
  if(me>gov&gov>me2){p2<-max(me,me2)};{p1<-min(me,me2)}
  if(me<gov&gov<me2){p2<-max(me,me2)};{p1<-min(me,me2)}
  if(gov<=min(me,me2)){p1<-max(min(sort(mtheta_ia[rownames(mtheta_ia)%in%lgn,2])[34],sort(mtheta_ia[rownames(mtheta_ia)%in%lgn2,2])[17]),gov);p2<-max(me,me2)}
  
  #blockout zone (light)
  if(splitc==0){b1<-2*median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgnp,2])-median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2])}
  b2<-median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2])
  if(splitc==1){b11<-2*median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgnp1,2])-median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2])}
  if(splitc==1){b12<-2*median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgnp2,2])-median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2])}
  if(splitc==1){rect(border=NA,xleft=min(na.rm=T,b11,b12),ybottom=0,xright=median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),ytop=500.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==1){rect(border=NA,xleft=median(na.rm=T,mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),ybottom=0,xright=max(b11,b12),ytop=500.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==0){rect(border=NA,xleft=min(na.rm=T,b1,b2),ybottom=0,xright=max(na.rm=T,b1,b2),ytop=500.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(na.rm=T,p1,p2),ybottom=0,xright=max(na.rm=T,p1,p2),ytop=500.1,col = rgb(0.5,0.5,0.5,2/3))
  #overlap zone (a lot darker): get intersection
  c1<-c();c2<-c();c11<-c();c12<-c()
  if(splitc==0){c11<-round(seq(min(na.rm=T,b1,b2),max(na.rm=T,b1,b2),0.001),3)}
  if(splitc==0){c12<-round(seq(min(na.rm=T,p1,p2),max(na.rm=T,p1,p2),0.001),3)}
  if(splitc==0){try(c1<-min(na.rm=T,intersect(c11,c12)));try(c2<-max(na.rm=T,intersect(c11,c12)))}
  if(splitc==0){if(c1==Inf|c1==-Inf){c1<-c()}; if(c2==Inf|c2==-Inf){c2<-c()}}
  if(splitc==1){c11<-round(seq(min(na.rm=T,b11,b12),max(na.rm=T,b11,b12),0.001),3)}
  if(splitc==1){c12<-round(seq(min(na.rm=T,p1,p2),max(na.rm=T,p1,p2),0.001),3)}
  if(splitc==1){try(c1<-min(na.rm=T,intersect(c11,c12)));try(c2<-max(na.rm=T,intersect(c11,c12)))}
  if(splitc==1){if(c1==Inf|c1==-Inf){c1<-c()}; if(c2==Inf|c2==-Inf){c2<-c()}}
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=500.1,col = rgb(0.5,0.5,0.5,1/1))}
  #redraw lines to overright dark shaded part
  if(length(dc)>1){lines(lwd=2.3,density(dc,na.rm=T))}
  if(length(oc)>1){lines(lwd=2.3,density(oc,na.rm=T),col="red")}
  #rug plot
  if(length(dc)>0){rug(na.omit(dc))};
  if(length(oc)>0){rug(na.omit(oc),col="red")}
  #show median (if split chamber)
  clip(x1=-10,x2=10,y1=0,y2=30)
  abline(v=median(mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),lty=2)
  if(splitc==0){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5, lwd=c(2,2,1),"topleft",col=c("red","black","black"),lty=c(1,1,2),pch=c(NA,NA,NA),c(paste("Floor Consideration, n=",length(oc),sep=""),paste("No Floor Consideration, n=",length(dc),sep=""),"Chamber Median"))}
  if(splitc==1){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5, lwd=c(2,2,1),"topleft",col=c("red","black","black"),lty=c(1,1,2),pch=c(NA,NA,NA),c(paste("Floor Consideration, n=",length(oc),sep=""),paste("No Floor Consideration, n=",length(dc),sep=""),"Chamber Median"))}
  if(splitc==0){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5,lwd=c(0,0,0),"topright",col=c(rgb(0.5,0.5,0.5,1/3),rgb(0.5,0.5,0.5,2/3),rgb(0.5,0.5,0.5,1/1)),lty=c(0,0,0),pch=c(15,15,15),c("Blockout Zone","Gridlock Interval","Intersection"))}
  if(splitc==1){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5, lwd=c(0,0,0),"topright",col=c(rgb(0.5,0.5,0.5,1/3),rgb(0.5,0.5,0.5,2/3),rgb(0.5,0.5,0.5,1/1)),lty=c(0,0,0),pch=c(15,15,15),c("Blockout Zone","Gridlock Interval","Intersection"))}
  
  ############
  #second plot
  ############
  #if(splitc==0){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b1,b2,p1,p2)-0.25,max(b1,b2,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  #if(splitc==1){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b11,b12,p1,p2)-0.25,max(b11,b12,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  if(splitc==0&sen==0){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-3,3),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==1&sen==0){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-3,3),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==0&sen==1){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-3,3),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==1&sen==1){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-3,3),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  
  #blockout zone (light)
  if(splitc==1){rect(border=NA,xleft=min(b11,b12,na.rm=T),ybottom=0,xright=median(mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==1){rect(border=NA,xleft=median(mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),ybottom=0,xright=max(na.rm=T,b11,b12),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==0){rect(border=NA,xleft=min(na.rm=T,b1,b2),ybottom=0,xright=max(na.rm=T,b1,b2),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(na.rm=T,p1,p2),ybottom=0,xright=max(na.rm=T,p1,p2),ytop=2.1,col = rgb(0.5,0.5,0.5,2/3))
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=2.1,col = rgb(0.5,0.5,0.5,1/1))}
  
  if(length(dc)>0){for(i in 1:length(dc)){lines(c(dc[i],dc[i]),c(0,1),col="black")}}
  if(length(oc)>0){for(i in 1:length(oc)){lines(c(oc[i],oc[i]),c(1,2),col="red")}}
  clip(x1=-10,x2=10,y1=0,y2=3)
  abline(v=median(mtheta_ia[rownames(mtheta_ia)%in%lgn,2]),lty=2)  
}
sq_figs_ia(sen=0,splitc=0,dc=df_IA_bills$sq5_aug3[intersect(grep("IA85_H",df_IA_bills$RCID),which(df_IA_bills$floor_stat_NEW==0&is.na(df_IA_bills$sq5_aug3)==F))],oc=df_IA_bills$sq5_aug3[intersect(grep("IA85_H",df_IA_bills$RCID),which(df_IA_bills$floor_stat_NEW==1&is.na(df_IA_bills$sq5_aug3)==F))],main="(1) 85th Iowa House\n(2013-2014, Republican Majority)",main2="(2) 85th Iowa House\n(2013-2014, Republican Majority)",gleg1="IA84101",gleg2="IA84201",p="\\(IA/R",ye="85")
sq_figs_ia(sen=1,splitc=0,dc=df_IA_bills$sq5_aug3[intersect(grep("IA84_S",df_IA_bills$RCID),which(df_IA_bills$floor_stat_NEW==0&is.na(df_IA_bills$sq5_aug3)==F))],oc=df_IA_bills$sq5_aug3[intersect(grep("IA84_S",df_IA_bills$RCID),which(df_IA_bills$floor_stat_NEW==1&is.na(df_IA_bills$sq5_aug3)==F))],main="(3) 84th Iowa Senate\n(2011-2012, Democratic Majority)",main2="(4) 84th Iowa Senate\n(2011-2012, Democratic Majority)",gleg1="IA84201",gleg2="IA84101",p="\\(IA/D",ye="84")
#function for plotting panels with SQ distributions and close-up view of theoretically relevant intervals in Nebraska
sq_figs_ne<-function(ex,sen,dc,oc,main,main2,gleg1,gleg2,p,ye,splitc,chairoc,chairdc,c5){
  dc<-dc[which(abs(dc)<5)] 
  oc<-oc[which(abs(oc)<5)] 
  if(ex==0){plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(na.omit(dc)),ylim=c(0,max(max(density(na.omit(dc))$y,na.rm=T),max(density(na.omit(oc,na.rm=T),na.rm=T)$y))+0.2),xlim=c(-max(max(abs(dc),na.rm=T),max(abs(oc),na.rm=T))-0.2,max(max(abs(dc),na.rm = T),max(abs(oc),na.rm=T))+0.2),xlab="Ideal Point Scale",
                 main=main,yaxt="n")}
  if(ex==1){plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(na.omit(dc)),ylim=c(0,max(density(na.omit(dc))$y)+0.65),xlim=c(min(-max(na.rm=T,abs(dc)+0.2),-1),max(na.rm=T,abs(dc)+0.2,1)),xlab="Ideal Point Scale",
                 main=main,yaxt="n")}
  if(ex==0){lines(lwd=2.3,density(na.omit(oc)),col="red")};
  if(ex==2){plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,NA,ylim=c(0,1),xlim=c(-2,2),xlab="Ideal Point Scale",
                 main=main,yaxt="n")}
  
  if(ye=="98"){lgn<-gsub("_NE","",pivot_repos[[2]][[1]])}
  if(ye=="99"){lgn<-gsub("_NE","",pivot_repos[[2]][[2]])}
  if(ye=="100"){lgn<-gsub("_NE","",pivot_repos[[2]][[3]][[1]])}
  if(ye=="101"){lgn<-gsub("_NE","",pivot_repos[[2]][[4]][[1]])}
  if(ye=="102"){lgn<-gsub("_NE","",pivot_repos[[2]][[5]][[1]])}
  if(ye=="103"){lgn<-gsub("_NE","",pivot_repos[[2]][[6]][[1]])}
  if(ye=="104"){lgn<-gsub("_NE","",pivot_repos[[2]][[7]][[1]])}
  
  if(splitc==0){lgnp<-lgn[grep(p,lgn)]}
  
  #get pivots
  p1<-sort(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])[17]
  p2<-sort(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])[33]
  
  #blockout zone (light)
  b1<-2*median(mtheta_ne[rownames(mtheta_ne)%in%lgnp,1])-median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])
  b2<-median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])
  if(splitc==1){b11<-2*median(mtheta_ne[rownames(mtheta_ne)%in%lgnp1,1])-median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])}
  if(splitc==1){b12<-2*median(mtheta_ne[rownames(mtheta_ne)%in%lgnp2,1])-median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1])}
  if(splitc==1){rect(border=NA,xleft=min(b11,b12),ybottom=0,xright=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),ytop=1.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==1){rect(border=NA,xleft=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),ybottom=0,xright=max(b11,b12),ytop=1.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==0){rect(border=NA,xleft=min(b1,b2),ybottom=0,xright=max(b1,b2),ytop=500.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(p1,p2),ybottom=0,xright=max(p1,p2),ytop=500.1,col = rgb(0.5,0.5,0.5,2/3))
  #overlap zone (a lot darker): get intersection
  c1<-c();c2<-c();c11<-c();c12<-c()
  if(splitc==0){c11<-round(seq(min(b1,b2),max(b1,b2),0.001),3)}
  if(splitc==0){c12<-round(seq(min(p1,p2),max(p1,p2),0.001),3)}
  if(splitc==0){try(c1<-min(intersect(c11,c12)));try(c2<-max(intersect(c11,c12)))}
  if(splitc==0){if(c1==Inf|c1==-Inf){c1<-c()}; if(c2==Inf|c2==-Inf){c2<-c()}}
  if(splitc==1){c11<-round(seq(min(b11,b12),max(b11,b12),0.001),3)}
  if(splitc==1){c12<-round(seq(min(p1,p2),max(p1,p2),0.001),3)}
  if(splitc==1){try(c1<-min(intersect(c11,c12)));try(c2<-max(intersect(c11,c12)))}
  if(splitc==1){if(c1==Inf|c1==-Inf){c1<-c()}; if(c2==Inf|c2==-Inf){c2<-c()}}
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=500.1,col = rgb(0.5,0.5,0.5,1/1))}
  #redraw lines to overright dark shaded part
  if(ex==0|ex==1){lines(lwd=2.3,density(na.omit(dc)))}
  if(ex==0){lines(lwd=2.3,density(na.omit(oc)),col="red")}
  #rug plot
  if(ex==0|ex==1){rug(na.omit(dc))};if(ex==0){rug(na.omit(oc),col="red")}
  if(ex==1&length(na.omit(oc))>0){rug(na.omit(oc),col="red")}
  #show median (if split chamber)
  if(splitc==0){legend(cex=1.1,x.intersp = 0.5,pt.cex=3.5, bg="white",lwd=c(2,2,1),"topleft",col=c("red","black","black"),lty=c(1,1,2),pch=c(NA,NA,NA),c(paste("Floor Consideration, n=",length(oc),sep=""),paste("No Floor Consideration, n=",length(dc),sep=""),"Chamber Median"))}
  if(splitc==1){legend(cex=1.1,x.intersp = 0.5,pt.cex=3.5, bg="white",lwd=c(2,2,1),"topleft",col=c("red","black","black"),lty=c(1,1,2),pch=c(NA,NA,NA),c(paste("Floor Consideration, n=",length(oc),sep=""),paste("No Floor Consideration, n=",length(dc),sep=""),"Chamber Median"))}
  if(splitc==0){legend(cex=1.1,x.intersp = 0.5,pt.cex=3.5, bg="white",lwd=c(0,0,0),"topright",col=c(rgb(0.5,0.5,0.5,1/3),rgb(0.5,0.5,0.5,2/3),rgb(0.5,0.5,0.5,1/1)),lty=c(0,0,0),pch=c(15,15,15),c("Blockout Zone","Gridlock Interval","Intersection"))}
  if(splitc==1){legend(cex=1.1,x.intersp = 0.5,pt.cex=3.5, bg="white",lwd=c(0,0,0),"topright",col=c(rgb(0.5,0.5,0.5,1/3),rgb(0.5,0.5,0.5,2/3),rgb(0.5,0.5,0.5,1/1)),lty=c(0,0,0),pch=c(15,15,15),c("Blockout Zone","Gridlock Interval","Intersection"))}
  clip(x1=-10,x2=10,y1=0,y2=500)
  abline(v=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),lty=2)
  
  ############
  #second plot
  ############
  #if(splitc==0){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b1,b2,p1,p2)-0.25,max(b1,b2,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  #if(splitc==1){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b11,b12,p1,p2)-0.25,max(b11,b12,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  if(splitc==0&sen==0){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-1.5,1.5),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==1&sen==0){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-1.5,1.5),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==0&sen==1){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-1.5,1.5),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==1&sen==1){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=main2,NA,xlim=c(-1.5,1.5),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  
  #blockout zone (light)
  if(splitc==1){rect(border=NA,xleft=min(b11,b12),ybottom=0,xright=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==1){rect(border=NA,xleft=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),ybottom=0,xright=max(b11,b12),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  if(splitc==0){rect(border=NA,xleft=min(b1,b2),ybottom=0,xright=max(b1,b2),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(p1,p2),ybottom=0,xright=max(p1,p2),ytop=2.1,col = rgb(0.5,0.5,0.5,2/3))
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=2.1,col = rgb(0.5,0.5,0.5,1/1))}
  
  for(i in 1:length(dc)){lines(c(dc[i],dc[i]),c(0,1),col="black")}
  if(ex==0){for(i in 1:length(oc)){lines(c(oc[i],oc[i]),c(1,2),col="red")}}
  if(ex==1&length(na.omit(oc))>0){for(i in 1:length(oc)){lines(c(oc[i],oc[i]),c(1,2),col="red")}}
  
  clip(x1=-10,x2=10,y1=0,y2=3)
  abline(v=median(mtheta_ne[rownames(mtheta_ne)%in%lgn,1]),lty=2)
  
}
sq_figs_ne(ye="101",ex=0,chairoc=df_NE_bills$chair[which(df_NE_bills$session=="101"&df_NE_bills$floor_stat_NEW==1&is.na(df_NE_bills$sq5_aug3)==F)] ,chairdc=df_NE_bills$chair[which(df_NE_bills$session=="101"&df_NE_bills$floor_stat_NEW==0&is.na(df_NE_bills$sq5_aug3)==F)],c5="NE/",splitc=0,dc=df_NE_bills$sq5_aug3[which(df_NE_bills$session=="101"&df_NE_bills$floor_stat_NEW==0&is.na(df_NE_bills$sq5_aug3)==F)],oc=df_NE_bills$sq5_aug3[which(df_NE_bills$session=="101"&df_NE_bills$floor_stat_NEW==1&is.na(df_NE_bills$sq5_aug3)==F)],main="(5) 101st Nebraska Legislature\n(2009-2010)",main2="(6) 101st Nebraska Legislature\n(2009-2010)",gleg1="NE101101",p="\\(NE/R",sen=1)
#function for plotting panels with SQ distributions and close-up view of theoretically relevant intervals in Wisconsin
sq_figs_wi<-function(sen,dc,oc,main,gleg1,gleg2,p,ye,splitc,m2){
  oc<-oc[which(abs(oc)<5)]
  dc<-dc[which(abs(dc)<5)]
  if(length(oc)==0&length(dc)==0){plot(NA,main="")}
  if(length(oc)==0&length(dc)==0){plot(NA,main="");next()}
  plot(col="black",cex.axis=2.5,cex.main=2.5,cex=2.3,cex.lab=2.5,lwd=2.3,density(dc),ylim=c(0,max(max(density(dc)$y),max(density(oc)$y))+0.6),xlim=c(min(mtheta_wi[,1]),max(mtheta_wi[,1])),xlab="Ideal Point Scale",
       main=main,yaxt="n")
  lines(lwd=2.3,density(oc),col="red");
  if(ye=="96"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[1]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[2]][[1]])}
  if(ye=="96"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[2]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[1]][[1]])}
  if(ye=="97"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[3]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[4]][[1]])}
  if(ye=="97"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[4]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[3]][[1]])}
  if(ye=="98"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[5]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[6]][[1]])}
  if(ye=="98"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[6]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[5]][[1]])}
  if(ye=="99"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[7]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[8]][[1]])}
  if(ye=="99"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[8]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[7]][[1]])}
  if(ye=="100"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[9]][[4]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[10]][[1]])}
  if(ye=="100"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[10]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[9]][[4]])}
  if(ye=="101"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[11]][[2]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[12]][[1]])}
  if(ye=="101"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[12]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[11]][[2]])}
  if(ye=="102"&sen==0){lgn<-gsub("_WI","",pivot_repos[[3]][[13]][[1]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[14]][[2]])}
  if(ye=="102"&sen==1){lgn<-gsub("_WI","",pivot_repos[[3]][[14]][[2]]);lgn2<-gsub("_WI","",pivot_repos[[3]][[13]][[1]])}
  
  if(splitc==0){lgnp<-lgn[grep(p,lgn)]}

  if(sen==0&ye=="96"){p1<-max(na.rm=T,min(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[34],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[12]),
                              mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==0&ye=="96"){p2<-max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="96"){p1<-max(na.rm=T,min(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[12],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[34]),
                              mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="96"){p2<-max(na.rm=T,max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  if(sen==0&ye=="97"){p1<-max(na.rm=T,min(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[34],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[12]),
                              mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==0&ye=="97"){p2<-max(na.rm=T,max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  if(sen==1&ye=="97"){p1<-max(na.rm=T,min(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[12],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[34]),
                              mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="97"){p2<-max(na.rm=T,max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  gov<-mean(mtheta_wi[grep("Doyle Adminis",rownames(mtheta_wi)),1])
  me<-median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]);me2<-median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])

  if(sen==0&ye=="98"){p1<-min(median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==0&ye=="98"){p2<-max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="98"){p1<-min(median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="98"){p2<-max(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==0&ye=="99"){p1<-min(median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==0&ye=="99"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[66],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[22]),mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="99"){p1<-min(median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="99"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[22],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[66]),mean(na.rm=T,mtheta_wi[,1][grep("Doyle Adminis",rownames(mtheta_wi))]))}
  if(sen==0&ye=="100"){p1<-min(na.rm=T,min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  if(sen==0&ye=="100"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[66],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[22]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="100"){p1<-min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="100"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[22],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[66]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  if(sen==0&ye=="101"){p1<-min(na.rm=T,min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  if(sen==0&ye=="101"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[66],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[22]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="101"){p1<-min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="101"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[22],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[66]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  if(sen==0&ye=="102"){p1<-min(na.rm=T,min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])))}
  if(sen==0&ye=="102"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[66],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[22]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  if(sen==1&ye=="102"){p1<-min(na.rm=T,median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),median(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1]))}
  if(sen==1&ye=="102"){p2<-min(na.rm=T,max(na.rm=T,sort(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])[22],sort(mtheta_wi[rownames(mtheta_wi)%in%lgn2,1])[66]),
                               mean(na.rm=T,mtheta_wi[,1][grep("Walker Adminis",rownames(mtheta_wi))]))}
  
  #blockout zone (light)
  b1<-2*median(mtheta_wi[rownames(mtheta_wi)%in%lgnp,1])-median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])
  b2<-median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1])
  if(splitc==0){rect(border=NA,xleft=min(b1,b2),ybottom=0,xright=max(b1,b2),ytop=35.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(na.rm=T,p1,p2),ybottom=0,xright=max(na.rm=T,p1,p2),ytop=500.1,col = rgb(0.5,0.5,0.5,2/3))
  #overlap zone (a lot darker): get intersection
  c1<-c();c2<-c();c11<-c();c12<-c()
  c11<-round(seq(min(na.rm=T,b1,b2),max(na.rm=T,b1,b2),0.001),3)
  c12<-round(seq(min(na.rm=T,p1,p2),max(na.rm=T,p1,p2),0.001),3)
  try(c1<-min(na.rm=T,intersect(c11,c12)));try(c2<-max(na.rm=T,intersect(c11,c12)))
  if(c1==Inf|c1==-Inf){c1<-c()}; if(c2==Inf|c2==-Inf){c2<-c()}
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=500.1,col = rgb(0.5,0.5,0.5,1/1))}
  #redraw lines to overright dark shaded part
  lines(lwd=2.3,density(dc))
  lines(lwd=2.3,density(oc),col="red")
  #rug plot
  rug(na.omit(dc));rug(na.omit(oc),col="red")
  #show median (if split chamber)
  clip(x1=-10,x2=10,y1=0,y2=50)
  abline(v=median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),lty=2)
  if(splitc==0){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5, lwd=c(2,2,1),"topleft",col=c("red","black","black"),lty=c(1,1,2),pch=c(NA,NA,NA),c(paste("Floor Consideration, n=",length(oc),sep=""),paste("No Floor Consideration, n=",length(dc),sep=""),"Chamber Median"))}
  if(splitc==0){legend(x.intersp = 0.5,cex=1.1,bg="white", pt.cex=3.5,lwd=c(0,0,0),"topright",col=c(rgb(0.5,0.5,0.5,1/3),rgb(0.5,0.5,0.5,2/3),rgb(0.5,0.5,0.5,1/1)),lty=c(0,0,0),pch=c(15,15,15),c("Blockout Zone","Gridlock Interval","Intersection"))}
  
  ############
  #second plot
  ############
  #if(splitc==0){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b1,b2,p1,p2)-0.25,max(b1,b2,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  #if(splitc==1){plot(cex.axis=2,cex.main=2,cex.lab=2,main=main,NA,xlim=c(min(b11,b12,p1,p2)-0.25,max(b11,b12,p1,p2)+0.25),ylim=c(0,2),yaxt="n",ylab="",xlab="Status Quo Positions")}
  if(splitc==0&sen==0){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=m2,NA,xlim=c(-2.9,2.9),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  if(splitc==0&sen==1){plot(cex.axis=2.5,cex.main=2.5,cex.lab=2.5,main=m2,NA,xlim=c(-2.9,2.9),ylim=c(0,2),yaxt="n",ylab="",xlab="Ideal Point Scale")}
  
  #blockout zone (light)
  if(splitc==0){rect(border=NA,xleft=min(b1,b2),ybottom=0,xright=max(b1,b2),ytop=2.1,col = rgb(0.5,0.5,0.5,1/3))}
  #gridlock zone (darker)
  rect(border=NA,xleft=min(na.rm=T,p1,p2),ybottom=0,xright=max(na.rm=T,p1,p2),ytop=2.1,col = rgb(0.5,0.5,0.5,2/3))
  #intersection
  if(length(c1)>0&length(c2)>0){
    rect(border=NA,xleft=c1,ybottom=0,xright=c2,ytop=2.1,col = rgb(0.5,0.5,0.5,1/1))}
  for(i in 1:length(dc)){lines(c(dc[i],dc[i]),c(0,1),col="black")}
  for(i in 1:length(oc)){lines(c(oc[i],oc[i]),c(1,2),col="red")}
  clip(x1=-10,x2=10,y1=0,y2=3)
  abline(v=median(mtheta_wi[rownames(mtheta_wi)%in%lgn,1]),lty=2)
}
sq_figs_wi(sen=0,splitc=0,dc=df_WI_bills$sq5_aug3[which(df_WI_bills$session=="096"&df_WI_bills$chamber=="A"&df_WI_bills$floor_stat_NEW==0&is.na(df_WI_bills$sq5_aug3)==F)],oc=df_WI_bills$sq5_aug3[which(df_WI_bills$session=="096"&df_WI_bills$chamber=="A"&df_WI_bills$floor_stat_NEW==1&is.na(df_WI_bills$sq5_aug3)==F)],main="(7) 96th Wisconsin Assembly\n(2003-2004, Republican Majority)",m2="(8) 96th Wisconsin Assembly\n(2003-2004, Republican Majority)",gleg1="WI096101",gleg2="WI096201",p="\\(WI/R",ye="96")
sq_figs_wi(sen=1,splitc=0,dc=df_WI_bills$sq5_aug3[which(df_WI_bills$session=="099"&df_WI_bills$chamber=="S"&df_WI_bills$floor_stat_NEW==0&is.na(df_WI_bills$sq5_aug3)==F)],oc=df_WI_bills$sq5_aug3[which(df_WI_bills$session=="099"&df_WI_bills$chamber=="S"&df_WI_bills$floor_stat_NEW==1&is.na(df_WI_bills$sq5_aug3)==F)],main="(9) 99th Wisconsin Senate\n(2009-2010, Democratic Majority)",m2="(10) 99th Wisconsin Senate\n(2009-2010, Democratic Majority)",gleg1="WI096101",gleg2="WI096201",p="\\(WI/D",ye="99")

dev.off()

####################################################################
### Figure 5 Testing Gridlock Hypothesis
####################################################################
rm(list=ls());gc();gc();gc();Sys.setlocale(locale="C")
load("ST20_LSQ_replication2.RData")

pdf(paste(getwd(),"/Figure5.pdf",sep=""),width=12,height=6.5)
par(mfrow=c(1,2),mar=c(1,0.2,1,0.2),mai=c(1.5,0.2,0.5,0.2))

plot(frame.plot=F,NA,yaxt="n",xaxt="n",main="",xlab="",ylab="",xlim=c(0,1),ylim=c(1,2.1))
text(cex=2.5,adj=1,"Iowa",y=2,x=1)
text(cex=2.5,adj=1,"Nebraska",y=1.5,x=1)
text(cex=2.5,adj=1,"Wisconsin",y=1,x=1)

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
