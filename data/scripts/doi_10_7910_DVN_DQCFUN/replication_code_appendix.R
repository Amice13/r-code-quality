##############################################################################
#
# Replication code for Online Appendix to
# ``Moderation or Strategy? Political Giving by Corporations and Trade Groups''
#       by Sebastian Thieme
#
# Files used:   ST19_JOP_replication.RData
#               ST19_JOP_replication_B5H1.RData   
#               ST19_JOP_replication_C2.RData
#               ST19_JOP_replication_C2_ia.Rdata
#               ST19_JOP_replication_C2_ne.Rdata
#               ST19_JOP_replication_C2_wi.Rdata
#               ST19_JOP_replication_C2_nocip2020.Rdata
#               ST19_JOP_replication_C2_cip2020.Rdata
#               ST19_JOP_replication_C2_cip920.Rdata
#               ST19_JOP_replication_all_npat_only.Rdata
#               ST19_JOP_replication_all_sig_only.RData
#               ST19_JOP_replication_CIP_asm.RData
#               ST19_JOP_replication_H2.RData
#               ST19_JOP_replication_H3.RData
#               ST19_JOP_replication_H4_2.RData
#               ST19_JOP_replication_H5.RData
#
# Files produced: cutpoints_w_a_wo_RCs.png (Figure 4)
#                 ia_ne_wi_dist.png        (Figure 5)
#                 compare_ideal_npat.png   (Figure 6)
#                 all_dist13.png           (Figure 7)
#                 npat_comparison_all.png  (Figure 8)
#                 CIP_asm.png       (Figure 9)
#                 compareCFIdeal2.png      (Figure 10)
#                 AppendixG2.png           (Figure 11)
#                 artificial_extremism_check1.png        (Figure 12)
#                 bill_subjects.png        (Figure 13)
#                 vote_subjects.png        (Figure 14)
#                 artificial_extremism_check2.png        (Figure 15)
#                 robustness_ct1.png       (Figure 16)
#                 robustness_ct2.png       (Figure 17)  
#                 robustness_minvote_params_hist_cor.png (Figure 18)
#                 robustness_minvote_params_comp-1.png   (Figure 19)
#                 robustness_minvote_params_comp-2.png   (Figure 20)
#                 robustness_minvote_params_comp-3.png   (Figure 21)
#                 robustness_CIP.png       (Figure 22)
#                 cf_and_cw-ideal.png      (Figure 23)
#                 prop_mod_cont.png        (Figure 24)
#                 representative_legislators.png         (Figure 25) 
#                 robustness_lobbying_principals.png     (Figure 26)
#
#   Note: To faciliate replication, this code is organized in a way that allows  
#         for replication of the results section by section, so that, 
#         e.g., replication of results in Appendix C does not rely on 
#         replication code from Appendix B.
#         
###############################################################################

#########################
# Results in Appendix B.5
# Figure 4
#########################
#remove all other objects 
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
#load necessary data (change file path if RData file not in working directory) 
load("~/ST19_JOP_replication_B5H1.RData")
############
# Overview of relevant objects:
# lbills: bills & resolutions that were lobbied
# allbills: all bills & resolutions in the sample
# allbills2: all bills & resolutions excluding study bills
# lbills2: lobbied bills & resolutions, excluding study bills, which cannot have roll calls
# lbills3: lobbied bills & resolutions (only for and against)
# lbills4: lobbied bills & resolutions (only for and against), excluding study bills, which cannot have roll calls
# bills: bill ids, and estimated item parameters of votes on bills in the sample 

#get bills that have a roll call
rcbills<-unique(bills$statesessionbill[which(grepl("_",bills$roll_call_id)==F)])
#what is the share of lobbied bills that get to the floor
length(which(lbills%in%rcbills==T))/length(lbills) #0.090
#now, what is the share of non-lobbied bills that reach the floor  
length(which(allbills%in%lbills==F))/length(allbills) #0.209
length(which(rcbills%in%lbills2==T))/length(lbills2) #0.103
length(which(allbills2%in%lbills2==F))/length(allbills2) #0.193
#only for and against
length(which(rcbills%in%lbills3==T))/length(lbills3) #0.096
length(which(allbills2%in%lbills3==F))/length(allbills2) #0.275

#the share of lobbied bills (for or against) (excluding study bills) that end up with
#a roll call vote
length(which(lbills4%in%rcbills==T))/length(lbills4) #0.1099275
#the share of lobbied bills (for or against) (excluding study bills) that end up with
#a roll call vote
length(which(lbills3%in%rcbills==T))/length(lbills3) #0.0962
#the share of lobbied bills (any position) (excluding study bills) that end up on the floor
length(which(lbills2%in%rcbills==T))/length(lbills2) #0.1033169
#the share of lobbied bills (any position) (including study bills) that end up on the floor
length(which(lbills%in%rcbills==T))/length(lbills) #0.09036854

length(which(allbills2%in%lbills4==F))/length(allbills2) #0.2749545
length(which(allbills2%in%lbills2==F))/length(allbills2) #0.1931101

#select bills which get a roll call vote
bwrc<-bills$statesessionbill[!grepl("_",bills$roll_call_id)] 
#select bills with vote on initial bill version
bwzv<-bills$statesessionbill[grep("_",bills$roll_call_id)] 
intersect(bwrc,bwzv)
length(bwrc)
length(bwrc)-length(intersect(bwrc,bwzv))
length(bwzv)
#now limit to significant disc par
dir.create("ST19_JOP_replication")

##################################
##Figure 4 (p. 20 Online Appendix)
##################################
png(filename = paste(getwd(),"ST19_JOP_replication","cutpoints_w_a_wo_RCs.png",sep="/"),height = 500,width = 800)
par(mfrow=c(1,1),mai=c(1,1.05,0.5,0.3))
plot(cex.lab=2,cex=2,lwd=3,cex.axis=2,main=NA,xlab="Ideal Point Scale",density(bills$cutpoint[which(bills$statesessionbill%in%setdiff(bwzv,bwrc)==F&abs(bills$cutpoint)<5&bills$sign==1)])) 
lines(lty=2,lwd=3,col="darkgray",density(bills$cutpoint[which(bills$statesessionbill%in%setdiff(bwzv,bwrc)==T&abs(bills$cutpoint)<5&bills$sign==1)])) #zero version vote, no roll call
legend(cex=2,"topleft",legend=c("Bills w/ Floor Votes","Bills w/o Floor Votes"),col=c("black","darkgray"),lty=c(1,2),lwd=3)
dev.off()
length(bills$cutpoint[which(bills$statesessionbill%in%setdiff(bwzv,bwrc)==F&abs(bills$cutpoint)<5&bills$sign==1)])
length(bills$cutpoint[which(bills$statesessionbill%in%setdiff(bwzv,bwrc)==T&abs(bills$cutpoint)<5&bills$sign==1)])
########################
#Results in Appendix C.2
########################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")

library(pscl)
library(coda)
#################################################################################################
# Note: Recalculating results for Appendix C.2 will take a lot of time (likely several hours).
#       If you would like to skip to the already calculated results,
#       you may load the file: ST19_JOP_replication_C2.RData (see below)
#################################################################################################

#create a list of all required RData files, containing draws from the posterior distribution
fs<-c("~/ST19_JOP_replication_C2_ia.RData"
  ,"~/ST19_JOP_replication_C2_ne.RData"
  ,"~/ST19_JOP_replication_C2_wi.RData"
  ,"~/ST19_JOP_replication_C2_nocip2020.RData"
  ,"~/ST19_JOP_replication_C2_cip2020.RData"
  ,"~/ST19_JOP_replication_C2_cip920.RData"
)

#create list to save results for each RData file
repos51<-as.list(rep(NA,length(fs)))
#loop
for(j12 in 1:length(fs)){
  print(j12)
  rm(list=setdiff(ls(),c("repos51","fs","j12")))
  gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();
  Sys.sleep(2)
  #load RData file
  load(fs[j12])
  #only keep results from before and new data
  rm(list=setdiff(ls(),c("repos51","fs","j12","t5")))
  gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();
  Sys.sleep(2)
  #ideal points
  ab<-list()
  for(i in 1:nrow(t5[[1]]$xbar)){ab[[i]]<-gelman.diag(mcmc.list(mcmc(t5[[1]]$x[,i,1]),mcmc(t5[[2]]$x[,i,1]),mcmc(t5[[3]]$x[,i,1])))}
  repos51[[j12]]$gelmanx<-ab
  #beta1
  ab2<-list();
  if(length(t5[[1]]$betabar)>0){
    for(i in 1:nrow(t5[[1]]$betabar)){
      ab2[[i]]<-gelman.diag(mcmc.list(mcmc(t5[[1]]$beta[,i,1]),mcmc(t5[[2]]$beta[,i,1]),mcmc(t5[[3]]$beta[,i,1])))
    };
    repos51[[j12]]$gelmanb1<-ab2;
  #beta2
    ab3<-list();
    for(i in 1:nrow(t5[[1]]$betabar)){
      ab3[[i]]<-gelman.diag(mcmc.list(mcmc(t5[[1]]$beta[,i,2]),mcmc(t5[[2]]$beta[,i,2]),mcmc(t5[[3]]$beta[,i,2])))
    };
    repos51[[j12]]$gelmanb2<-ab3;
  #heidelberger and welch's diagnostic  
    repos51[[j12]]$heidel_b11<-(heidel.diag(mcmc(t5[[1]]$beta[,,1])))
    repos51[[j12]]$heidel_b12<-(heidel.diag(mcmc(t5[[2]]$beta[,,1])))
    repos51[[j12]]$heidel_b13<-(heidel.diag(mcmc(t5[[3]]$beta[,,1])))
    repos51[[j12]]$heidel_b21<-(heidel.diag(mcmc(t5[[1]]$beta[,,2])))
    repos51[[j12]]$heidel_b22<-(heidel.diag(mcmc(t5[[2]]$beta[,,2])))
    repos51[[j12]]$heidel_b23<-(heidel.diag(mcmc(t5[[3]]$beta[,,2])))
  }
  repos51[[j12]]$heidel_x11<-(heidel.diag(mcmc(t5[[1]]$x[,,1])))
  repos51[[j12]]$heidel_x12<-(heidel.diag(mcmc(t5[[2]]$x[,,1])))
  repos51[[j12]]$heidel_x13<-(heidel.diag(mcmc(t5[[3]]$x[,,1])))
}
rm(list=setdiff(ls(),c("repos51","fs","j12")))
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();
#!save.image("ST19_JOP_replication_C2.RData")

##################################################################
# Note: To skip to results, load RData file below and continue...
##################################################################
#load("~/ST19_JOP_replication_C2.RData")
gel<-list()
for(j in 1:6){
  g9<-matrix(NA,nrow=length(repos51[[j]]$gelmanx),ncol=2)
  for(i in 1:length(repos51[[j]]$gelmanx)){g9[i,]<-unlist(repos51[[j]]$gelmanx[i])}
  gel[[j]]<-g9
}
#Table 1, Column 1
length(which(gel[[1]][,1]>1.1)) #0
length(which(gel[[2]][,1]>1.1)) #0
length(which(gel[[3]][,1]>1.1)) #0
length(which(gel[[4]][,1]>1.1)) #1
length(which(gel[[5]][,1]>1.1)) #0
length(which(gel[[6]][,1]>1.1)) #0
#Table 1, Column 2
for(j in 1:6){
  print(length(repos51[[j]]$gelmanb1))
}
#Table 1, Columns 3 & 4
gelb1<-list()
for(j in 1:6){
  if(length(repos51[[j]]$gelmanb1)==0){next}
  g9<-matrix(NA,nrow=length(repos51[[j]]$gelmanb1),ncol=2)
  for(i in 1:length(repos51[[j]]$gelmanb1)){g9[i,]<-unlist(repos51[[j]]$gelmanb1[i])}
  gelb1[[j]]<-g9
}
#Table 1, Column 3
length(which(gelb1[[1]][,1]>1.1)) #9
length(which(gelb1[[2]][,1]>1.1)) #0
length(which(gelb1[[3]][,1]>1.1)) #124
length(which(gelb1[[4]][,1]>1.1)) #97
length(which(gelb1[[5]][,1]>1.1)) #105
length(which(gelb1[[6]][,1]>1.1)) #29
#Table 1, Column 4
length(which(gelb1[[1]][,1]>1.1))/length(gelb1[[1]][,1]) #0.2%
length(which(gelb1[[2]][,1]>1.1))/length(gelb1[[2]][,1]) #0%
length(which(gelb1[[3]][,1]>1.1))/length(gelb1[[3]][,1]) #2.3%
length(which(gelb1[[4]][,1]>1.1))/length(gelb1[[4]][,1]) #0.8%
length(which(gelb1[[5]][,1]>1.1))/length(gelb1[[5]][,1]) #0.9%
length(which(gelb1[[6]][,1]>1.1))/length(gelb1[[6]][,1]) #0.2%
#Table 1, Columns 5 & 6
gelb2<-list()
for(j in 1:6){
  if(length(repos51[[j]]$gelmanb2)==0){next}
  g9<-matrix(NA,nrow=length(repos51[[j]]$gelmanb2),ncol=2)
  for(i in 1:length(repos51[[j]]$gelmanb2)){g9[i,]<-unlist(repos51[[j]]$gelmanb2[i])}
  gelb2[[j]]<-g9
}
#Table 1, Column 5
length(which(gelb2[[1]][,1]>1.1)) #0
length(which(gelb2[[2]][,1]>1.1)) #0
length(which(gelb2[[3]][,1]>1.1)) #7
length(which(gelb2[[4]][,1]>1.1)) #8
length(which(gelb2[[5]][,1]>1.1)) #10
length(which(gelb2[[6]][,1]>1.1)) #2
#Table 1, Column 6
length(which(gelb2[[1]][,1]>1.1))/length(gelb2[[1]][,1]) #0%
length(which(gelb2[[2]][,1]>1.1))/length(gelb2[[2]][,1]) #0%
length(which(gelb2[[3]][,1]>1.1))/length(gelb2[[3]][,1]) #0.1%
length(which(gelb2[[4]][,1]>1.1))/length(gelb2[[4]][,1]) #<0.1%
length(which(gelb2[[5]][,1]>1.1))/length(gelb2[[5]][,1]) #<0.1%
length(which(gelb2[[6]][,1]>1.1))/length(gelb2[[6]][,1]) #<0.1%

#Heidelberger & Welch's convergence diagnostic
heidelx<-list();
for(j in 1:length(repos51)){
  heidelx[[j]]<-list(repos51[[j]]$heidel_x11,repos51[[j]]$heidel_x12,repos51[[j]]$heidel_x13)
}

length(which(heidelx[[1]][[1]][,4]==0))/length(heidelx[[1]][[1]][,4])
length(which(heidelx[[1]][[2]][,4]==0))/length(heidelx[[1]][[2]][,4])
length(which(heidelx[[1]][[3]][,4]==0))/length(heidelx[[1]][[3]][,4])
length(which(heidelx[[2]][[1]][,4]==0))/length(heidelx[[2]][[1]][,4])
length(which(heidelx[[2]][[2]][,4]==0))/length(heidelx[[2]][[2]][,4])
length(which(heidelx[[2]][[3]][,4]==0))/length(heidelx[[2]][[3]][,4])
length(which(heidelx[[3]][[1]][,4]==0))/length(heidelx[[3]][[1]][,4])
length(which(heidelx[[3]][[2]][,4]==0))/length(heidelx[[3]][[2]][,4])
length(which(heidelx[[3]][[3]][,4]==0))/length(heidelx[[3]][[3]][,4])
length(which(heidelx[[4]][[1]][,4]==0))/length(heidelx[[4]][[1]][,4])
length(which(heidelx[[4]][[2]][,4]==0))/length(heidelx[[4]][[2]][,4])
length(which(heidelx[[4]][[3]][,4]==0))/length(heidelx[[4]][[3]][,4])
length(which(heidelx[[5]][[1]][,4]==0))/length(heidelx[[5]][[1]][,4])
length(which(heidelx[[5]][[2]][,4]==0))/length(heidelx[[5]][[2]][,4])
length(which(heidelx[[5]][[3]][,4]==0))/length(heidelx[[5]][[3]][,4])
length(which(heidelx[[6]][[1]][,4]==0))/length(heidelx[[6]][[1]][,4])
length(which(heidelx[[6]][[2]][,4]==0))/length(heidelx[[6]][[2]][,4])
length(which(heidelx[[6]][[3]][,4]==0))/length(heidelx[[6]][[3]][,4])

#########################
#Results in Appendix C.3
#########################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc()
#install.packages("pscl")
library(pscl)
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication.RData")
########
#
########
lobs<-which(!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",rnames)==T&!grepl("/NP",rnames)==T)

rc<-rollcall(df_9_20_3)
pred<-predict(res) #!only keep 1, obscure names? yes, and shuffle columns, polarity?
pred$overall.percent #overall prediction: 92.5% correct

#get positions so that can weight percentage correctly
posw<-apply(rc$votes,1,function(x) length(which(is.na(x)==F)))
#percent correct for lobbying principals: 88.1%
sum(pred$legis.percent[lobs]*(posw[lobs]/sum(posw[lobs])))/100
#get majority positions
majp<-apply(rc$votes,2,function(x) names(sort(table(x),decreasing = T))[1])
#get majority proportions
majprop<-apply(rc$votes,2,function(x) max(table(x))/sum(table(x)))
minprop<-apply(rc$votes,2,function(x) min(table(x))/sum(table(x)))
minvot<-apply(rc$votes,2,function(x) min(table(x)))
cerror<-c();for(i in 1:ncol(pred$correct)){cerror[i]<-length(which(pred$correct[,i]==F))}
###########################
#APRE for all observations
###########################
sum(minvot-cerror)/sum(minvot) #APRE for all 0.78

#majp<-as.numeric(majp)
majp<-as.numeric(majp)
minvotl<-c();for(i in 1:ncol(rc$votes)){minvotl[i]<-length(which(is.na(rc$votes[lobs,i])==F&rc$votes[lobs,i]!=majp[i]))}
cerrorl<-c();for(i in 1:ncol(pred$correct)){cerrorl[i]<-length(which(pred$correct[lobs,i]==F))}
####################################
#APRE for lobbying principals: 0.61
####################################
sum(minvotl-cerrorl)/sum(minvotl)

###############
# Appendix D.1
# Figure 5
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
########
#
########
load("~/ST19_JOP_replication_C2_ia.RData")
res<-t5[[1]];rm(t5);gc();gc()
t82<-data.frame(name=rownames(res$xbar),ideal=as.numeric(res$xbar),stringsAsFactors = F)
t82$name[which(t82$name=="BNSF Railway Co.")]<-"BNSF Railway Co._IA"
t82$name[which(t82$name=="National Federation of Independent Business (NFIB)")]<-"National Federation of Independent Business (NFIB)_IA"

t82<-merge(t82,cont_sc,by=c("name"))
png(filename = paste(getwd(),"ST19_JOP_replication","ia_ne_wi_dist.png",sep="/"),height = 600,width = 1600)
par(mar=c(0.65,0.65,0.65,0.65),mai=c(0.65,0.65,0.65,0.65),mfrow=c(1,3))
plot(cex.axis=2,cex.main=2,main="(1) Iowa State Legislators (2003-2016)\n and Lobbying Principals (2005-2016)",cex.lab=2.5,lty=1,lwd=2,density(t82$ideal[grep("IA/D/H",t82$name)]),col="blue",xlim=c(-3.25,3.25),ylim=c(0,2.5),
     xlab="Ideal Point Scale") 
lines(lty=2,lwd=2,density(t82$ideal[union(grep("IA/D/S",t82$name),grep("IA/D/H_S",t82$name))]),col="blue")
lines(lty=1,lwd=2,density(t82$ideal[grep("IA/R/H",t82$name)]),col="red")
lines(lty=2,lwd=2,density(t82$ideal[union(grep("IA/R/S",t82$name),grep("IA/R/H_S",t82$name))]),col="red")
#all but corp and trade
lines(lwd=2,density(t82$ideal[setdiff(which(t82$X!=""),which(t82$corp_trade=="1"|t82$corp_trade=="2"))]),col="green",lty=2)
#corp and trade groups
lines(lwd=2,density(t82$ideal[which(t82$corp_trade=="1"|t82$corp_trade=="2")]),col="green")
legend("topright",adj=0,cex=1.5,lwd=c(2,2,2,2,2,2,2),col=c("blue","blue","red","red","green","green"),lty=c(1,2,1,2,1,2),c("Democratic Representatives","Democratic Senators","Republican Representatives","Republican Senators","Corps. & Trade Groups","Other Lobbying Principals"))

load("~/ST19_JOP_replication_C2_ne.RData")
res<-t5[[1]];rm(t5);gc();gc()
t82<-data.frame(name=rownames(res$xbar),ideal=as.numeric(res$xbar),stringsAsFactors = F)
t82$name[t82$name=="BNSF Railway Co."]<-"BNSF Railway Co._OTH"
t82$name[t82$name=="National Federation of Independent Business (NFIB)"]<-"National Federation of Independent Business (NFIB)_NE"

t82<-merge(t82,cont_sc,by=c("name"))

plot(cex.axis=2,cex.main=2,main="(2) Nebraska State Legislators (2003-2016)\n and Lobbying Principals (2003-2016)",cex.lab=2.5,lty=1,lwd=2,density(t82$ideal[grep("NE/D/S",t82$name)]),col="blue",xlim=c(-4.5,4.5),ylim=c(0,1.5),
     xlab="Ideal Point Scale") 
lines(lty=1,lwd=2,density(t82$ideal[grep("NE/R/S",t82$name)]),col="red")
lines(lty=1,lwd=2,density(t82$ideal[grep("NE/I/S",t82$name)]),col="purple")
#all but corp and trade
lines(lwd=2,density(t82$ideal[setdiff(which(t82$X!=""),which(t82$corp_trade=="1"|t82$corp_trade=="2"))]),col="green",lty=2)
#corp and trade groups
lines(lwd=2,density(t82$ideal[which(t82$corp_trade=="1"|t82$corp_trade=="2")]),col="green")
legend("topright",adj=0,cex=1.5,lwd=c(2,2,2,2,2),col=c("blue","red","purple","green","green"),lty=c(1,1,1,1,2),c("Democratic Senators","Republican Senators","Independent Senators","Corps. & Trade Groups","Other Lobbying Principals"))

load("~/ST19_JOP_replication_C2_wi.RData")

res<-t5[[1]];rm(t5);gc();gc()
t82<-data.frame(name=rownames(res$xbar),ideal=as.numeric(res$xbar),stringsAsFactors = F)
t82$name[t82$name=="BNSF Railway Co."]<-"BNSF Railway Co._OTH"
t82$name[t82$name=="National Federation of Independent Business (NFIB)"]<-"National Federation of Independent Business (NFIB)_WI"

t82<-merge(t82,cont_sc,by=c("name"))

plot(cex.axis=2,cex.main=2,main="(3) Wisconsin State Legislators (2003-2016)\n and Lobbying Principals (2003-2016)",cex.lab=2.5,lty=1,lwd=2,density(-t82$ideal[grep("WI/D/H",t82$name)]),col="blue",xlim=c(-3,3),ylim=c(0,2.7),
     xlab="Ideal Point Scale")
lines(lty=2,lwd=2,density(-t82$ideal[union(grep("WI/D/S",t82$name),grep("WI/D/H_S",t82$name))]),col="blue")
lines(lty=1,lwd=2,density(-t82$ideal[grep("WI/R/H",t82$name)]),col="red")
lines(lty=1,lwd=2,density(-t82$ideal[grep("WI/I/H",t82$name)]),col="purple")
lines(lty=2,lwd=2,density(-t82$ideal[union(grep("WI/R/S",t82$name),grep("WI/R/H_S",t82$name))]),col="red")
#all but corp and trade
lines(lwd=2,density(-t82$ideal[setdiff(which(t82$X!=""),which(t82$corp_trade=="1"|t82$corp_trade=="2"))]),col="green",lty=2)
#corp and trade groups
lines(lwd=2,density(-t82$ideal[which(t82$corp_trade=="1"|t82$corp_trade=="2")]),col="green")
legend("topright",adj=0,cex=1.5,lwd=c(2,2,2,2,2,2,2),col=c("blue","blue","red","red","purple","green","green"),lty=c(1,2,1,2,1,1),c("Dem. Assembly Members","Democratic Senators","Rep. Assembly Members","Republican Senators","Ind. Assembly Members","Corps. & Trade Groups","Other Lobbying Principals"))
dev.off()

rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()

###############
# Appendix D.2
# Figure 6
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_C2_nocip2020.RData")
########
#
########
res<-t5[[1]];rm(t5);gc();gc();gc()
t82<-data.frame(name=rownames(res$xbar),ideal=as.numeric(res$xbar),stringsAsFactors = F)

#merge npat scores
t82<-merge(t82,cont_sc,by=c("name"))
t82$col<-"green";t82$col[grep("IA/",t82$name)]<-"blue";t82$col[grep("NE/",t82$name)]<-"brown"
png(filename = paste(getwd(),"ST19_JOP_replication","compare_ideal_npat.png",sep="/"),height = 672,width = 672)
par(mar=c(1,1,1,1),mai=c(1.1,1.1,0.6,0.6))
plot(cex.lab=1.5,cex.axis=1.5,lwd=2,pch=1,cex=1.5,t82$npat_score,-t82$ideal,col=t82$col,xlim=c(-3,3),ylim=c(-3,3)
     ,xlab="NPAT Common Space Score Scale",ylab="Ideal Point Scale") 
legend(x=-3.25,y=3.25,adj=0,cex=1.5,pch=c(1,1,1),pt.lwd=2,col=c("blue","brown","green"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators"))
dev.off()

##############
# Appendix D.3
# Figure 7
##############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication.RData")
########
#
########
t82<-merge(est,cont_sc,by=c("name"))

png(filename = paste(getwd(),"ST19_JOP_replication","all_dist13.png",sep="/"),height = 672,width = 1200)
par(mar=c(1,1,1,1),mai=c(1,1,0.7,0.7))
plot(cex.axis=2.5,cex.lab=2.5,lty=1,lwd=2,main="",density(t82$ideal[setdiff(grep("IA/D/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="blue",xlim=c(-3.5,3.5),ylim=c(0,3),
     xlab="Ideal Point Scale") 
lines(lty=1,lwd=2,density(t82$ideal[setdiff(grep("IA/R/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="red")
lines(lty=2,lwd=2,density(t82$ideal[setdiff(grep("NE/D/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="blue")
lines(lty=2,lwd=2,density(t82$ideal[setdiff(grep("NE/R/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="red")
lines(lty=3,lwd=2,density(t82$ideal[setdiff(grep("WI/D/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="blue")
lines(lty=3,lwd=2,density(t82$ideal[setdiff(grep("WI/R/[A-Z]",t82$name),c(grep("/NP",t82$name),grep("_OTH",t82$name)))]),col="red")
#all but corp and trade
lines(lwd=2,density(t82$ideal[setdiff(which(t82$X!=""),which(t82$corp_trade=="1"|t82$corp_trade=="2"))]),col="green",lty=2)
#corp and trade groups
lines(lwd=2,density(t82$ideal[which(t82$corp_trade=="1"|t82$corp_trade=="2")]),col="green")
legend("topright",adj=0,cex=1.5,lwd=c(2,2,2,2,2,2,2,2),col=c("blue","red","blue","red","blue","red","green","green"),
       lty=c(1,1,2,2,3,3,1,2),c("Iowa Democratic Legislators","Iowa Republican Legislators",
                                "Nebraska Democratic Legislators","Nebraska Republican Legislators","Wisconsin Democratic Legislators","Wisconsin Republican Legislators","Corps. and Trade Groups","Other Lobbying Principals"))
dev.off()

#############
# Appendix E
# Figure 8
#############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_C2_nocip2020.RData")
########
#
########
#Panel 1
res<-t5[[1]];rm(t5);gc();gc();gc()
t82<-data.frame(name=rownames(res$xbar),ideal=as.numeric(res$xbar),stringsAsFactors = F)

#merge estimates with npat scores
t82<-merge(t82,cont_sc,by=c("name"))
t82$col<-"green";t82$col[grep("IA/",t82$name)]<-"blue";t82$col[grep("NE/",t82$name)]<-"brown"
#start figure
png(filename = paste(getwd(),"ST19_JOP_replication","npat_comparison_all.png",sep="/"),height = 560,width = 1530)
par(mar=c(1,0.65,1,0.65),mai=c(1,0.65,1,0.65),mfcol=c(1,3))
#plot
plot(cex.main=2,main="(1) NPAT Common Space Scores Plotted Against\n Jointly Estimated Legislator Ideal Points\n (Using Interest Groups and PCT Respondents as Bridges)",cex.axis=2,cex.lab=2,lwd=2,pch=1,cex=1,
     t82$npat_score,-t82$ideal,col=t82$col,xlim=c(-3,3),ylim=c(-3,3)
     ,xlab="NPAT Common Space Score Scale",ylab="Ideal Point Scale") #;main="Face Validity Test: Legislator Ideal Point Estimates # Iowa (black) and Wisconsin (green)"
legend("topleft",adj=0,cex=2.2,pch=c(1,1,1),pt.lwd=2,col=c("blue","brown","green"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators"))
rm(list=ls())
#Panel 2
load("~/ST19_JOP_replication_all_npat_only.RData")

#merge with estimates
t82<-merge(t82,cont_sc,by=c("name"))
t82$col<-"green";t82$col[grep("IA/",t82$name)]<-"blue";t82$col[grep("NE/",t82$name)]<-"brown"
#plot
plot(cex.main=2,main="(2) NPAT Common Space Scores Plotted Against\n Jointly Estimated Legislator Ideal Points\n (using PCT Respondents as Bridges)",cex.axis=2,cex.lab=2,lwd=2,pch=1,cex=1,
     t82$npat_score,-t82$ideal,col=t82$col,xlim=c(-3,3),ylim=c(-3,3)
     ,xlab="NPAT Common Space Score Scale",ylab="Ideal Point Scale") #;main="Face Validity Test: Legislator Ideal Point Estimates # Iowa (black) and Wisconsin (green)"
legend("topleft",adj=0,cex=2.2,pch=c(1,1,1),pt.lwd=2,col=c("blue","brown","green"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators"))
rm(list=ls())
#Panel 3
load("~/ST19_JOP_replication_all_sig_only.RData")
#merge with estimates
t82<-merge(t82,cont_sc,by=c("name"))
t82$col<-"green";t82$col[grep("IA/",t82$name)]<-"blue";t82$col[grep("NE/",t82$name)]<-"brown"
#plot
plot(cex.main=2,main="(3) NPAT Common Space Scores Plotted Against\n Jointly Estimated Legislator Ideal Points\n (using Interest Groups as Bridges)",cex.axis=2,cex.lab=2,lwd=2,pch=1,cex=1,t82$npat_score,-t82$ideal,col=t82$col,xlim=c(-3,3),ylim=c(-3,3)
     ,xlab="NPAT Common Space Score Scale",ylab="Ideal Point Scale") #;main="Face Validity Test: Legislator Ideal Point Estimates # Iowa (black) and Wisconsin (green)"
legend("topleft",adj=0,cex=2.2,pch=c(1,1,1),pt.lwd=2,col=c("blue","brown","green"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators"))
dev.off()

###############
# Appendix E
# Figure 9
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_CIP_asm.RData")
########
#
########
#start figure
tp<-which(abs(a10$t)>=1.96)
tm<-which(abs(a10$t)<1.96)
ia<-grep("IA/",rownames(a10))
ne<-grep("NE/",rownames(a10))
wi<-grep("WI/",rownames(a10))

png(filename = paste(getwd(),"ST19_JOP_replication","CIP_asm.png",sep="/"),height = 672,width = 1600)
par(mar=c(1,1.5,1,1),mai=c(1,1.5,1,1),mfrow=c(1,2))
plot(col="white",cex.main=1.75,cex.axis=1.75,cex.lab=1.75,main="(1) Bridging Actors That Violate\n the Common Ideal Point Assumption",xlim=c(-3,3),ylim=c(-3,3),a10[,1],a10[,2],xlab="Ideal Point Estimate from Votes in State A",ylab="Ideal Point Estimate from Votes in State B \n or from Political Courage Test (PCT) Responses");
points(a10[intersect(tp,ne),1],a10[intersect(tp,ne),2],col="brown",lwd=2)
points(a10[intersect(tp,wi),1],a10[intersect(tp,wi),2],col="green",lwd=2)
points(a10[intersect(tp,ia),1],a10[intersect(tp,ia),2],col="blue",lwd=2)
points(a10[setdiff(tp,grep("[A-Z][A-Z]/[A-Z]/[A-Z]",rownames(a10))),1],a10[setdiff(tp,grep("[A-Z][A-Z]/[A-Z]/[A-Z]",rownames(a10))),2],col="red",lwd=2)
legend(cex=1.5,pch=1,"topleft",pt.lwd=2,col=c("blue","brown","green","red"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators","Lobbying Principals"))
lines(seq(-3,3,1),seq(-3,3,1),lty=3,col="black")
plot(col="white",cex.main=1.75,cex.axis=1.75,cex.lab=1.75,main="(2) Bridging Actors That Do Not Violate\n the Common Ideal Point Assumption",xlim=c(-3,3),ylim=c(-3,3),a10[,1],a10[,2],xlab="Ideal Point Estimate from Votes in State A",ylab="Ideal Point Estimate from Votes in State B \n or from Political Courage Test (PCT) Responses");
points(a10[intersect(tm,ne),1],a10[intersect(tm,ne),2],col="brown",lwd=2)
points(a10[intersect(tm,wi),1],a10[intersect(tm,wi),2],col="green",lwd=2)
points(a10[intersect(tm,ia),1],a10[intersect(tm,ia),2],col="blue",lwd=2)
points(a10[setdiff(tm,grep("[A-Z][A-Z]/[A-Z]/[A-Z]",rownames(a10))),1],a10[setdiff(tm,grep("[A-Z][A-Z]/[A-Z]/[A-Z]",rownames(a10))),2],col="red",lwd=2)
legend(cex=1.5,pch=1,"topleft",pt.lwd=2,col=c("blue","brown","green","red"),c("Iowa Legislators","Nebraska Legislators","Wisconsin Legislators","Lobbying Principals"))
lines(seq(-3,3,1),seq(-3,3,1),lty=3,col="black")
dev.off()

###############
# Appendix G
# Figure 10
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication.RData")
########
#
########
est2<-merge(est,cont_sc,by=c("name")) #merge est2 with cont_sc

#remove all observations with missing contributor/recipient scores 
#and observations where CFscore is based on one distinct contributor
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
#remove observations for which cf estimate is based on one distinct contributor
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]

#preparation: calculating legislator quantiles
#get ids for legislators (not candidates only)
ilgs<-setdiff(union(grep("\\([A-Z][A-Z]/[A-Z]/H",est2$name),grep("\\([A-Z][A-Z]/[A-Z]/S",est2$name)),c(grep("/NP",est2$name),grep("_OTH",est2$name)))

llc<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs])))
lli<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$ideal[ilgs],times=est2$weights[ilgs])))
#get identifiers for democratic and republican legislators
dl<-setdiff(grep("\\([A-Z][A-Z]/D/",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
rl<-setdiff(grep("\\([A-Z][A-Z]/R/",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
#start figure
png(filename = paste(getwd(),"ST19_JOP_replication","compareCFIdeal2.png",sep="/"),height = 773,width = 773)
par(mar=c(1,1,1,1),mai=c(1.5,1.5,1.5,1.5))
plot(cex.main=2,main=NA,cex=2,lwd=2,
     cex.axis=2,xlab="",ylab="",xlim=c(-2.5,2.5),ylim=c(-3,3),
     est2$bonica2_cf_most_overall[which(est2$corp_trade==0)],est2$ideal[which(est2$corp_trade==0)])
abline(lwd=2,col="darkgray",h=median(rep(est2$ideal[rl],est2$weights[rl])))
abline(lty=2,lwd=2,col="darkgray",h=median(rep(est2$ideal[dl],times=est2$weights[dl])))
abline(lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[rl],times=est2$weights[rl])))
abline(lty=2,lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[dl],times=est2$weights[dl])))
lines(llc,lli,lwd=2,col="darkgray",lty=3)
mtext(side=1,line=2.3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.75)
mtext(side=2,line=2.6,at=c(-1.6,1.6),c("liberal","conservative"),cex=1.75)
mtext(side=1,line=4.65,at=c(0),c("CFscore Scale"),cex=2.3)
mtext(side=2,line=4.65,at=c(0),c("Ideal Point Scale"),cex=2.3)
legend(cex=1.3,"topleft",legend=c("Lobbying Organizations","Dem. Median","Rep. Median"),pch=c(1,NA,NA),lty=c(NA,2,1),col=c("black","darkgray","darkgray"),lwd=c(NA,2,2),pt.lwd=c(2,NA,NA))
dev.off()

#correlation: 0.8
cor(est2$bonica2_cf_most_overall[which(est2$corp_trade==0)],est2$ideal[which(est2$corp_trade==0)])
###############
# Appendix G
# Figure 11
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
#load results and auxiliary data
load("~/ST19_JOP_replication.RData")
########
#
########

##############################
# Preparing Data for Analysis
##############################

est2<-est #est2
est2<-merge(est2,cont_sc,by=c("name")) #merge est2 with cont_sc
######################################
## law firms not coded as corporations
est2$corp_trade[grep("C/W",est2$cat)]<-"0"
length(est2$corp_trade[grep("C/W",est2$cat)]) #18

#high and low contributors : est2
#create variable (H vs. L)
est2$HL<-""
est2$HL[which(log10(est2$bonica2_am_most_overall)>=5)]<-"H"
est2$HL[which(log10(est2$bonica2_am_most_overall)<5)]<-"L"

#Keep full table for Panel 1 of Figure 1
est1<-est2 
#remove all observations with missing contributor/recipient scores 
#and observations where CFscore is based on one distinct contributor
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
#remove observations for which cf estimate is based on one distinct contributor
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]

#preparation: calculating legislator quantiles
#get ids for legislators (not candidates only)
ilgs<-setdiff(union(grep("\\([A-Z][A-Z]/[A-Z]/H",est2$name),grep("\\([A-Z][A-Z]/[A-Z]/S",est2$name)),c(grep("/NP",est2$name),grep("_OTH",est2$name)))

llc<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs])))
lli<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$ideal[ilgs],times=est2$weights[ilgs])))

qq3<-c();qql<-c();qqh<-c()
#ids for corporations and trade groups
ict<-which(est2$corp_trade==1|est2$corp_trade==2)
#ids for low-level contributors
ilc<-which(est2$HL=="L")
#ids for high-level contributors
ihc<-which(est2$HL=="H")
k<-1
for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[ict]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[ict])
  qql[k]<-length(which(est2$bonica2_cf_most_overall[intersect(ict,ilc)]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[intersect(ict,ilc)])
  qqh[k]<-length(which(est2$bonica2_cf_most_overall[intersect(ict,ihc)]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[intersect(ict,ihc)])
  k<-k+1
}
qq3<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[ict]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(ict))
qql<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[intersect(ict,ilc)]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(intersect(ict,ilc)))
qqh<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[intersect(ict,ihc)]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(intersect(ict,ihc)))

ict2<-which(rnames%in%est2$name[ict]==T)
ictl2<-which(rnames%in%est2$name[intersect(ict,ilc)]==T)
icth2<-which(rnames%in%est2$name[intersect(ict,ihc)]==T)
ilgs2<-which(rnames%in%est2$name[ilgs]==T)
ms2<-matrix(NA,ncol=3000,nrow=101)
msh<-matrix(NA,ncol=3000,nrow=101)
msl<-matrix(NA,ncol=3000,nrow=101)
k<-1
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    qqq<-quantile(probs=c(i),rep(est_x[s,ilgs2,1],times=est$weights[ilgs2]))
    ms2[k,s-1]<-length(which(est_x[s,ict2,1]<qqq))
    msl[k,s-1]<-length(which(est_x[s,ictl2,1]<qqq))
    msh[k,s-1]<-length(which(est_x[s,icth2,1]<qqq))
  }
  if(k%in%seq(1,101,10)){print(paste(round(k/101,2)*100,"%",sep=""))};  k<-k+1
}

ms2<-ms2/length(ict2)
msh<-msh/length(icth2)
msl<-msl/length(ictl2)

ilgs1<-setdiff(union(grep("\\([A-Z][A-Z]/[A-Z]/H",est1$name),grep("\\([A-Z][A-Z]/[A-Z]/S",est1$name)),c(grep("/NP",est1$name),grep("_OTH",est1$name)))
ict1<-which(est1$corp_trade=="1"|est1$corp_trade=="2")

#####################################
#Plot Panels 1-6 in Figure 11
#####################################
png(filename=paste(getwd(),"ST19_JOP_replication","AppendixG2.png",sep="/"),width=1800,height=1200)
par(mar=c(1,1,1,1),mai=c(0.95,1,1,0.95),mfrow=c(2,3))
#############
#Panel 1
plot(cex.lab=2,cex.main=3,cex.axis=2,lwd=2,xlab="",
     main="(1) Position-Based Estimates",
     lty=2,density(est1$ideal[which(est1$corp_trade=="2")]),xlim=c(-3,3),ylim=c(0,0.7))
lines(lwd=2,lty=3,density(est1$ideal[which(est1$corp_trade=="1")]))
lines(lwd=2,lty=1,density(est1$ideal[ilgs1]))
abline(lwd=2,col="darkgray",v=median(rep(est1$ideal[intersect(grep("\\([A-Z][A-Z]/R/",est1$name),ilgs1)],times=est1$weights[intersect(grep("\\([A-Z][A-Z]/R/",est1$name),ilgs1)])))
abline(lwd=2,lty=2,col="darkgray",v=median(rep(est1$ideal[intersect(grep("\\([A-Z][A-Z]/D/",est1$name),ilgs1)],times=est1$weights[intersect(grep("\\([A-Z][A-Z]/D/",est1$name),ilgs1)])))
mtext(side=1,line=3,at=c(-1.6,1.6),c("liberal","conservative"),cex=1.75)
mtext(side=1,line=6,at=c(0),c("Ideal Point Scale"),cex=2.25)
legend(cex=2.1,legend=c("Corporations","Trade Groups","Legislators","Democratic Median","Republican Median"),"topleft",
       lwd=2,col=c("black","black","black","darkgray","darkgray"),lty=c(3,2,1,2,1))
#########
#70 corporations or trade groups more conservative than median Republican
length(which(est1$ideal[ict1]>median(rep(est1$ideal[intersect(grep("\\([A-Z][A-Z]/R/",est1$name),ilgs1)],times=est1$weights[intersect(grep("\\([A-Z][A-Z]/R/",est1$name),ilgs1)]))))
#746 legislators
length(which(est1$ideal[ilgs1]> -5))
length(which(est1$ideal[ict1]> -5))
#84 corporations
length(which(est1$ideal[which(est1$corp_trade=="1")]> -5))
#171 trade associations
length(which(est1$ideal[which(est1$corp_trade=="2")]> -5))
#############
#Panel 2
plot(cex.lab=2,cex.main=3,cex.axis=2,lwd=2,xlab="",
     main="(2) Contribution-Based Estimates",ylim=c(0,1.8),
     lty=2,density(na.omit(est2$bonica2_cf_most_overall[which(est2$corp_trade=="2")])),xlim=c(-2.5,2.5))
lines(lty=3,lwd=2,density(na.omit(est2$bonica2_cf_most_overall[which(est2$corp_trade=="1")])))
lines(lwd=2,density(na.omit(est2$bonica2_cf_r[ilgs])))
abline(lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)])))
abline(lty=2,lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)])))
legend(cex=2.1,legend=c("Corporations","Trade Groups","Legislators","Democratic Median","Republican Median"),"topleft",
       lwd=2,col=c("black","black","black","darkgray","darkgray"),lty=c(3,2,1,2,1))
mtext(side=1,line=3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.75)
mtext(side=1,line=6,at=c(0),c("CFscore Scale"),cex=2.25)

length(which(na.omit(est2$bonica2_cf_most_overall[ict])<median(na.rm=T,rep(est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)]))))
length(which(na.omit(est2$bonica2_cf_most_overall[ict])>median(na.rm=T,rep(est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)]))))
#####
#13 corporations and trade groups more conservative than median Republican
length(which(est2$bonica2_cf_most_overall[ict]>median(rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)]))))
#676 legislators
length(which(est2$bonica2_cf_r[ilgs]> -5))
#175 corporations and trade groups
length(which(est2$bonica2_cf_most_overall[ict]> -5))
#70 corporations 
length(which(est2$bonica2_cf_most_overall[which(est2$corp_trade=="1")]> -5))
#105 trade associations
length(which(est2$bonica2_cf_most_overall[which(est2$corp_trade=="2")]> -5))
#corps and trade assn. with extreme conservative contribution record
length(which(est2$bonica2_cf_most_overall[ict]>median(rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)]))))
#corps and trade assn. with extreme liberal contribution record
length(which(est2$bonica2_cf_most_overall[ict]<median(rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)]))))
(175-13)/175
#############
# Panel 3
plot(pch="C",cex=2,cex.main=3,main="(3) Comparison for Corps. and Trade Groups",cex.axis=2,xlab="",ylab="",xlim=c(-2,2),
     ylim=c(-2.75,2.75),est2$bonica2_cf_most_overall[which(est2$corp_trade=="1")],
     est2$ideal[which(est2$corp_trade=="1")])
points(cex=2,pch="T",est2$bonica2_cf_most_overall[which(est2$corp_trade=="2")],est2$ideal[which(est2$corp_trade=="2")])
abline(lwd=2,col="darkgray",h=median(rep(est2$ideal[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)])))
abline(lty=2,lwd=2,col="darkgray",h=median(rep(est2$ideal[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)])))
abline(lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)])))
abline(lty=2,lwd=2,col="darkgray",v=median(na.rm=T,rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)])))
lines(llc,lli,lwd=2.5,col="darkgray",lty=3)
mtext(side=1,line=3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.75)
mtext(side=2,line=3,at=c(-1.6,1.6),c("liberal","conservative"),cex=1.75)
mtext(side=1,line=6,at=c(0),c("CFscore Scale"),cex=2.25)
mtext(side=2,line=6,at=c(0),c("Ideal Point Scale"),cex=2.25)

legend(cex=2.1,"bottomright",legend=c("Corporations","Trade Groups","Democratic Median","Republican Median","Legislator Q-Q Plot"),pch=c("C","T",NA,NA,NA),lty=c(NA,NA,2,1,3),col=c("black","black","darkgray","darkgray","darkgray"),lwd=c(NA,NA,2,2,2),pt.lwd=c(2,2,NA,NA,NA))

#results in article:
#comparing revealed preferences using legislator quantiles
clist_c<-est2$bonica2_cf_most_overall[est2$corp_trade==1]
clist_i<- est2$ideal[est2$corp_trade==1]
options(warn=1)
beab<-c(rep(0,length(clist_c)))
for(i in 1:length(clist_c)){
  if(lli[which(abs(clist_c[i]-llc)==min(abs(clist_c[i]-llc)))]<clist_i[i]){beab[i]<-1}
}
tlist_c<-est2$bonica2_cf_most_overall[est2$corp_trade==2]
tlist_i<- est2$ideal[est2$corp_trade==2]
beab2<-c(rep(0,length(tlist_c)))
for(i in 1:length(tlist_c)){
  if(lli[which(abs(tlist_c[i]-llc)==min(abs(tlist_c[i]-llc)))]<tlist_i[i]){beab2[i]<-1}
}
#plot(col="gray",llc,lli)
#points(tlist_c[beab2==1],tlist_i[beab2==1],pch="T")
#points(clist_c[beab==1],clist_i[beab==1],pch="C")
length(which(beab==1))+length(which(beab2==1))
(length(beab)+length(beab2))
#82% of corporations and trade groups reveal more conservative policy preferences
#than implied by their contribution behavior
(length(which(beab==1))+length(which(beab2==1)))/(length(beab)+length(beab2))
#points(tlist_c[beab2==0],tlist_i[beab2==0],pch="T",col="red")
#points(clist_c[beab==0],clist_i[beab==0],pch="C",col="red")

#results in article:
#comparing extremism in position-taking and political giving with median legislator
#as the baseline for extremism

##details
rm0<-median(na.rm = T,rep(est2$ideal[grep("[A-Z][A-Z]/R/",est2$name)],times=est2$weights[grep("[A-Z][A-Z]/R/",est2$name)]))
dm0<-median(na.rm = T,rep(est2$ideal[grep("[A-Z][A-Z]/D/",est2$name)],times=est2$weights[grep("[A-Z][A-Z]/D/",est2$name)]))
pcc<-length(which(est2$ideal[est2$corp_trade=="1"]>dm0&
                    est2$ideal[est2$corp_trade=="1"]<rm0))/length(na.omit(est2$ideal[est2$corp_trade=="1"]))
pctg<-length(which(est2$ideal[est2$corp_trade=="2"]>dm0&
                     est2$ideal[est2$corp_trade=="2"]<rm0))/length(na.omit(est2$ideal[est2$corp_trade=="2"]))
(pcc*length(which(est2$corp_trade=="1")))/
  (length(which(est2$corp_trade=="1"|est2$corp_trade=="2")))+(pctg*length(which(est2$corp_trade=="2")))/(length(which(est2$corp_trade=="1"|est2$corp_trade=="2")))

rm<-median(na.rm = T,rep(est2$bonica2_cf_r[grep("[A-Z][A-Z]/R/",est2$name)],times=est2$weights[grep("[A-Z][A-Z]/R/",est2$name)]))
dm<-median(na.rm = T,rep(est2$bonica2_cf_r[grep("[A-Z][A-Z]/D/",est2$name)],times=est2$weights[grep("[A-Z][A-Z]/D/",est2$name)]))
pcc<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade=="1"]>dm&
                    est2$bonica2_cf_most_overall[est2$corp_trade=="1"]<rm))/length(na.omit(est2$bonica2_cf_most_overall[est2$corp_trade=="1"]))
pctg<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade=="2"]>dm&
                     est2$bonica2_cf_most_overall[est2$corp_trade=="2"]<rm))/length(na.omit(est2$bonica2_cf_most_overall[est2$corp_trade=="2"]))
(pcc*length(which(est2$corp_trade=="1")))/
  (length(ict))+(pctg*length(which(est2$corp_trade=="2")))/(length(ict))
#look at which orgs with moderate contribution record have extreme prefs
numer<-length(intersect(which(est2$bonica2_cf_most_overall[ict]>dm&
                                est2$bonica2_cf_most_overall[ict]<rm),which(est2$ideal[ict]<dm0|est2$ideal[ict]>rm0)))
denom<-length(which(est2$bonica2_cf_most_overall[ict]>dm&
                      est2$bonica2_cf_most_overall[ict]<rm))
#Results:
#41 out of 162 organization with `moderate' contribution record reveal
#extreme policy preferences
numer
numer/denom

#Results:
#correlation between contribution- and position-taking behavior 
#for corporations and trade groups in the sample: 0.20
cor(est2$ideal[ict],est2$bonica2_cf_most_overall[ict],use="pairwise.complete.obs")
#correlation between contribution- and position-taking behavior 
#for other organizations in the sample: 0.80
cor(est2$ideal[setdiff(which(est2$X!=""),ict)],est2$bonica2_cf_most_overall[setdiff(which(est2$X!=""),ict)],use="pairwise.complete.obs")

#########################
#Panel 4
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(4) High- and Low-Level Contributors", qq3,col=rep(c("black","white"),50))#;lines(qq2,col="red")
for(i in 1:101){
  if(i%%2==0){next}
  points(cex=2,lwd=2,i,qq3[i],col="black")
  points(cex=2,lwd=2,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2[i,])))
  lines(cex=2,lwd=2,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2[i,]),quantile(probs=c(0.975),ms2[i,])))
}
mtext(side=2,line=4,at=c(0.5),c("Proportion of Corps. and Trade Groups"),cex=2)
mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2.25)
legend(cex=2.6,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)

#Results: 
#contribution measure
#percentage of corps. & trade groups less conservative
#than 40th legislator quantile: 2.3%
qq3[41]
#percentage of corps. & trade groups less conservative
#than 60th legislator quantile: 86.3
qq3[61]
#difference: 84pp
qq3[61]-qq3[41]
#similar increase for position-based measure between
#40th and 86th legislator quantile
mean(ms2[41,])
mean(ms2[87,])
mean(ms2[87,])-mean(ms2[41,])


#############
# Panel 5
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(5) Low-Level Contributors", qql,col=rep(c("black","white"),50))#;lines(qq2,col="red")
for(i in 1:101){
  if(i%%2==0){next}
  points(cex=2,lwd=2,i,qql[i],col="black")
  points(cex=2,lwd=2,col="darkgray",c(i),c(quantile(probs=c(0.5),msl[i,])))
  lines(cex=2,lwd=2,col="darkgray",c(i,i),c(quantile(probs=c(0.025),msl[i,]),quantile(probs=c(0.975),msl[i,])))
}
legend(cex=2.6,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
mtext(side=2,line=4,at=c(0.5),c("Proportion of Corps. and Trade Groups"),cex=2)
mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2.25)

qql[41]
qql[61]
qql[61]-qql[41]
mean(msl[41,])
mean(msl[73,])
mean(msl[73,])-mean(msl[41,])

#############
#Panel 6
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(6) High-Level Contributors", qqh,col=rep(c("black","white"),50))#;lines(qq2,col="red")
for(i in 1:101){
  if(i%%2==0){next}
  points(cex=2,lwd=2,i,qqh[i],col="black")
  points(cex=2,lwd=2,col="darkgray",c(i),c(quantile(probs=c(0.5),msh[i,])))
  lines(cex=2,lwd=2,col="darkgray",c(i,i),c(quantile(probs=c(0.025),msh[i,]),quantile(probs=c(0.975),msh[i,])))
}
legend(cex=2.6,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
mtext(side=2,line=4,at=c(0.5),c("Proportion of Corps. and Trade Groups"),cex=2)
mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2.25)
dev.off()

#Results:
#contribution measure for high-level contributors
#percentage of corps. & trade groups less conservative
#than 40th legislator quantile: 1.9%
qqh[41]
#percentage of corps. & trade groups less conservative
#than 60th legislator quantile: 95.2%
qqh[61]
#difference 93.3 pp
qqh[61]-qqh[41]
mean(msh[41,])
mean(msh[97,])
#similar increase for position-based measure between
#40th and 96th legislator quantile
mean(msh[97,])-mean(msh[41,])

#Results:
#counts of low- and high-level corporations and trade groups
length(which(est2$corp_trade=="1"&est2$HL=="L"))
length(which(est2$corp_trade=="2"&est2$HL=="L"))
length(which(est2$corp_trade=="1"&est2$HL=="H"))
length(which(est2$corp_trade=="2"&est2$HL=="H"))

###############
# Appendix H.1
# Figure 12
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication.RData")
#####
#
#####
est2<-merge(est,cont_sc,by=c("name"))

rownames(res$betabar)<-colnames(df_9_20_3)
rownames(res$xbar)<-est2$name
cp<-c();for(i in 1:ncol(res$beta[,,1])){
  cp[i]<-mean(res$beta[,i,2]/res$beta[,i,1])
}

bs1<-apply(res$beta[,,1],2,function(x) quantile(x,0.025))
bs2<-apply(res$beta[,,1],2,function(x) quantile(x,0.975))
beta_si<-bs1*bs2
beta_sig<-rep(1,length(beta_si));beta_sig[which(beta_si<0)]<-0 #only take those not including zero
#identifiers for corporations and trade groups
sigs<-which(est2$corp_trade==1|est2$corp_trade==2) 

rownames(df_9_20_3)<-est2$name
cp_list<-list()
for( i in 1:length(sigs)){
  cp_list$name[i]<-est2$name[sigs[i]]
  cp_list$nonNA[[i]]<-c(apply(df_9_20_3[sigs[i],],1,function(x) colnames(df_9_20_3)[which(is.na(x)==F)]))
}
options(warn=1)
excl<-unique(c(which(beta_sig==0),grep("NPCT",rownames(res$betabar)),which(abs(cp)>max(res$xbar)),which(cp<min(res$xbar)))) #without NPCT
for(i in 1:length(cp_list$name)){
  cp_list$cps[[i]]<-cp[setdiff(which(rownames(res$betabar)%in%cp_list$nonNA[[i]]==T),excl)]
  cp_list$ks[[i]]<-ks.test(cp[-c(excl)],cp_list$cps[[i]])[[2]]
  cp_list$vtp[[i]]<-unlist(var.test(x=cp_list$cps[[i]],cp[-c(excl)],alternative = "less",ratio=1)[3])
 }

cp_list2<-list()
sigs2<-which(!grepl("[A-Z][A-Z]/[A-Z]/[A-Z]",est2$name)==T&!grepl("/NP",est2$name)==T)
for( i in 1:length(sigs2)){
  cp_list2$name[i]<-rownames(df_9_20_3)[sigs2[i]]
  cp_list2$nonNA[[i]]<-c(apply(df_9_20_3[sigs2[i],],1,function(x) colnames(df_9_20_3)[which(is.na(x)==F)]))
}
#options(warn=1)
excl<-unique(c(which(beta_sig==0),grep("NPCT",rownames(res$betabar)),which(abs(cp)>max(res$xbar)),which(cp<min(res$xbar)))) #without NPCT
for(i in 1:length(cp_list2$name)){
  cp_list2$cps[[i]]<-cp[setdiff(which(rownames(res$betabar)%in%cp_list2$nonNA[[i]]==T),excl)]
  cp_list2$ks[[i]]<-ks.test(cp[-c(excl)],cp_list2$cps[[i]])[[2]]
  cp_list2$vtp[[i]]<-unlist(var.test(x=cp_list2$cps[[i]],cp[-c(excl)],alternative = "less",ratio=1)[3])
}

#results for corporations and trade groups only
ks.test(est2$ideal[sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]],
        est2$ideal[sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]]) #p-value=0.42
var.test(est2$ideal[sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]],
         est2$ideal[sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]]) #p-value=0.29
#what is length of each?
length(sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]) #59 corps & trade groups susceptible
length(sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]) #214 not susceptible
#proportion of extreme groups
dl<-setdiff(grep("\\([A-Z][A-Z]/D/[A-Z]",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
rl<-setdiff(grep("\\([A-Z][A-Z]/R/[A-Z]",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
#20.3% of susceptible corporations and trade groups have extreme ideal points
(length(which(est2$ideal[sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]]>median(rep(est2$ideal[rl],times=est2$weight[rl]))))+length(which(est2$ideal[sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]]<median(rep(est2$ideal[dl],times=est2$weight[dl])))))/length(sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)])
#28.5% of non-susceptible corporations and trade groups have extreme ideal points
(length(which(est2$ideal[sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]]>median(rep(est2$ideal[rl],times=est2$weight[rl]))))+length(which(est2$ideal[sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]]<median(rep(est2$ideal[dl],times=est2$weight[dl])))))/length(sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)])

#results for all organizations
ks.test(est2$ideal[sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)]],
        est2$ideal[sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)]]) #p-value=0.19
var.test(est2$ideal[sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)]],
         est2$ideal[sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)]]) #p-value=0.89
#what is the length of each?
length(sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)])
length(sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)])

dl<-setdiff(grep("\\([A-Z][A-Z]/D/[A-Z]",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
rl<-setdiff(grep("\\([A-Z][A-Z]/R/[A-Z]",est2$name),c(grep("/NP",est2$name),grep("_OTH",est2$name)))
#proportion of extreme organizations
#34.5% of susceptible organizations (all types) have extreme ideal points
(length(which(est2$ideal[sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)]]>median(rep(est2$ideal[rl],times=est2$weight[rl]))))+length(which(est2$ideal[sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)]]<median(rep(est2$ideal[dl],times=est2$weight[dl])))))/length(sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)])
#31.2% of non-susceptible organizations (all types) have extreme ideal points
(length(which(est2$ideal[sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)]]>median(rep(est2$ideal[rl],times=est2$weight[rl]))))+length(which(est2$ideal[sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)]]<median(rep(est2$ideal[dl],times=est2$weight[dl])))))/length(sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)])

###################
#Appendix Figure 12
###################
png(filename=paste(getwd(),"ST19_JOP_replication","artificial_extremism_check1.png",sep="/"),width=1000,height=430)
par(mfrow=c(1,2),mar=c(1,1,1,1),mai=c(1,1.05,1,1))
#Panel 1
plot(cex.main=2,ylim=c(0,0.8),xlim=c(-3,3),lwd=1.5,cex.axis=1.5, cex.lab=2, main="(1) Corporations and Trade Groups",xlab="Ideal Point Scale",density(est2$ideal[sigs[which(cp_list$ks<0.05&cp_list$vtp<0.05)]]),lty=2)
lines(lwd=1.5,density(est2$ideal[sigs[which(cp_list$ks>0.05|cp_list$vtp>0.05)]]),col="black")
#Panel 2
plot(cex.main=2,ylim=c(0,0.8),xlim=c(-3,3),lwd=1.5,cex.axis=1.5, cex.lab=2, main="(2) All Lobbying Principals",xlab="Ideal Point Scale",density(est2$ideal[sigs2[which(cp_list2$ks<0.05&cp_list2$vtp<0.05)]]),lty=2)
lines(lwd=1.5,density(est2$ideal[sigs2[which(cp_list2$ks>0.05|cp_list2$vtp>0.05)]]),col="black")
dev.off()

###################
#Appendix Figure 13
###################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_B5H1.RData")
#####
#
#####
png(filename=paste(getwd(),"ST19_JOP_replication","bill_subjects.png",sep="/"),width=800,height=800)
par(mar=c(0.5,0.5,0.5,0.5),mai=c(0.03,4.1,4.1,0.03))
plot(NA,bty="n",xlim=c(0,ncol(hm)),ylim=c(0,nrow(hm)),xlab="",ylab="",xaxt="n",yaxt="n")
for(i in 1:nrow(hm)){
  for(j in 1:ncol(hm)){
    rect(border=NULL,xleft=(j-1),xright=j,ybottom=(12-i),ytop = (12-i+1),col=cs[i,j])
    text(x=j-0.5,y=12-i+0.5,round(hm[i,j],2)*100,cex=1.5,font=2)
  }
}
#add shorter sector names
crpn<-sort(unique(t1t5$CRP),decreasing = T)
crpn[which(crpn=="Finance, Insurance, and Real Estate")]<-"Finance, Insurance & Real Estate"
crpn[which(crpn=="Energy & Natural Resources")]<-"Energy & Nat. Resources"
mtext(side=2,crpn,at=seq(0.5,length(unique(t1t5$CRP)),1),las=2,cex=1.5)
#add shorter policy area names
polc<-unique(t1t5$PolicyArea)

polc[which(polc=="Labor and Employment")]<-"Labor & Employment"
polc[which(polc=="Health and Social Welfare")]<-"Health & Social Welfare"
polc[which(polc=="Transportation, Telecommunications, and Technology")]<-"Transport., Telecomms. & Tech."
polc[which(polc=="Economy, Business and Finance")]<-"Economy, Business & Finance"
polc[which(polc=="Energy, Environment, and Natural Resources")]<-"Energy, Environm. & Nat. Resources"
polc[which(polc=="Community Development and Housing")]<-"Community Developm. & Housing"
mtext(side=3,polc,at=seq(0.5,length(unique(t1t5$PolicyArea)),1),las=2,cex=1.5)
dev.off()

###################
#Appendix Figure 14
###################

png(filename=paste(getwd(),"ST19_JOP_replication","vote_subjects.png",sep="/"),width=800,height=800)
par(mar=c(0.5,0.5,0.5,0.5),mai=c(0.03,4.1,4.1,0.03))
plot(NA,bty="n",xlim=c(0,ncol(hm2)),ylim=c(0,nrow(hm2)),xlab="",ylab="",xaxt="n",yaxt="n")
for(i in 1:nrow(hm2)){
  for(j in 1:ncol(hm2)){
    rect(border=NULL,xleft=(j-1),xright=j,ybottom=(nrow(hm2)-i),ytop = (nrow(hm2)-i+1),col=cs2[i,j])
    text(x=j-0.5,y=nrow(hm2)-i+0.5,round(hm2[i,j],2)*100,cex=1.5,col="black",font=2)
  }
}
#add shorter sector names
crpn<-as.character(sort(unique(t1t6$CRP),decreasing = T))
crpn[which(crpn=="Finance, Insurance, and Real Estate")]<-"Finance, Insurance & Real Estate"
mtext(side=2,crpn,at=seq(0.5,length(unique(t1t6$CRP)),1),las=2,cex=1.5)
#add shorter policy area names
polc<-as.character(unique(t1t6$PolicyArea))
polc[which(polc=="Transportation, Telecommunications, and Technology")]<-"Transport., Telecomms. & Tech."
polc[which(polc=="Economy, Business and Finance")]<-"Economy, Business & Finance"
polc[which(polc=="Energy, Environment, and Natural Resources")]<-"Energy, Environm. & Nat. Resources"
polc[which(polc=="Community Development and Housing")]<-"Community Developm. & Housing"
polc[which(polc=="Labor and Employment")]<-"Labor & Employment"
polc[which(polc=="Health and Social Welfare")]<-"Health & Social Welfare"
mtext(side=3,polc,at=seq(0.5,length(unique(t1t6$PolicyArea)),1),las=2,cex=1.5)
dev.off()

###################
#Appendix Figure 15
###################
png(filename=paste(getwd(),"ST19_JOP_replication","artificial_extremism_check2.png",sep="/"),width=1300,height=450)
par(mfrow=c(1,3),mar=c(1,1,1,1),mai=c(0.9,1.05,0.55,0.25))
plot(xlim=c(0,1),cex=2,cex.main=2.2,cex.lab=2.2,cex.axis=2,main="(1) Three Most Polarized Policy Areas",xlab="",
     ylab="Proportion of Extreme Organizations",CRPextreme$avgprop_pos_extpol3[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)])
mtext(cex=1.5,"Avg. Proportion of Positions\nin Most Polarized Policy Areas",side=1,line=5)
text(cex=1.5,gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]),
     x=c(0,0.04,-0.3,-0.01,0.03,0.02,0.02,0.03,0.03,0.02,0.02,0.03)+nchar(gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]))/100+CRPextreme$avgprop_pos_extpol3[!grepl("Candidate",CRPextreme$CRP)],
     y=c(0,0,0,0,0,0,0,0,0,0.015,-0.015,0)+CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)])
cor.test(CRPextreme$prop_pos_extpol3[!grepl("Candidate",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)],use="pairwise.complete.obs")
plot(xlim=c(0,1),cex=2,cex.main=2.2,cex.lab=2.2,cex.axis=2,main="(2) Five Most Polarized Policy Areas",xlab="",
     ylab="Proportion of Extreme Organizations",CRPextreme$avgprop_pos_extpol5[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)])
text(cex=1.5,gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]),
     x=c(0,0.04,-0.31,0,0.03,0.02,0.02,0.02,0.03,0.02,0.02,0.03)+nchar(gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]))/100+CRPextreme$avgprop_pos_extpol5[!grepl("Candidate",CRPextreme$CRP)],
     y=c(0,0,0,0,0,0,0,0,0,0.015,-0.015,0)+CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)])
mtext(cex=1.4,"Avg. Proportion of Positions\nin Most Polarized Policy Areas",side=1,line=5)
cor.test(CRPextreme$prop_pos_extpol5[!grepl("Candidate",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)],use="pairwise.complete.obs")
plot(xlim=c(0,1),cex=2,cex.main=2.2,cex.lab=2.2,cex.axis=2,main="(3) Seven Most Polarized Policy Areas",xlab="",
     ylab="Proportion of Extreme Organizations",CRPextreme$avgprop_pos_extpol7[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)&!grepl("Legislat",CRPextreme$CRP)])
text(cex=1.5,gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]),
     x=c(0.02,0.13,-0.01,0.02,-0.03,-0.02,-0.02,-0.02,-0.04,-0.01,-0.01,-0.04)-nchar(gsub("Finance, Insurance, and Real Estate","Fin., Insur. & Real Estate",CRPextreme$CRP[!grepl("Candidate",CRPextreme$CRP)]))/100+CRPextreme$avgprop_pos_extpol7[!grepl("Candidate",CRPextreme$CRP)],
     y=c(0,0,0,0,0,0,0,0,0,0.012,-0.01,0)+CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)])
mtext(cex=1.4,"Avg. Proportion of Positions\nin Most Polarized Policy Areas",side=1,line=5)
cor.test(CRPextreme$prop_pos_extpol7[!grepl("Candidate",CRPextreme$CRP)],CRPextreme$prop_ex[!grepl("Candidate",CRPextreme$CRP)],use="pairwise.complete.obs")
dev.off()

###############
# Appendix H.2
# Figure 16
###############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H2.RData")
# # #polarity clear
#  est_9_20_3_1a_nu$ideal<-est_9_20_3_1a_nu$ideal*-1
#  est_9_20_3_1b_nu$ideal<-est_9_20_3_1b_nu$ideal*-1
#  est_9_20_3_2b_nu$ideal<-est_9_20_3_2b_nu$ideal*-1

#####
# 
#####
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_ct1.png",sep="/"),width=1200,height=1200)
par(mfrow=c(2,2),mar=c(1,1,1,1),mai=c(1,1.05,1,1)) #show comparisons to original estimation
tt<-merge(t82,est_9_20_3_1a_nu,by=c("name"))
prin<-tt[!grepl("/NP",tt$name)&!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name),]
c1<-abs(cor(tt$ideal.x,tt$ideal.y))
c2<-abs(cor(prin$ideal.x,prin$ideal.y))

plot(lwd=1.5,cex.lab=2,cex.main=2,cex.axis=2,main="(1) Comparison to Estimates Based on\nVotes Lobbied by Corps. (Only Roll Call Votes)",
     xlab="Ideal Point Scale (Est. Based on All Included Votes)",ylab="Ideal Point Scale (Est. Based on Lobbied Votes)",cex=1.5,xlim=c(-3,3),ylim=c(-3,3),
     tt$ideal.x[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)],tt$ideal.y[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)])
points(prin$ideal.x,prin$ideal.y,col="darkgray",cex=1.4,lwd=1.5)
legend("topleft",legend=c("Legislators/Candidates","Lobbying Principals"),cex = 1.8,pt.lwd=2,pch=c(1,1),col=c("black","darkgray"))
legend(bty="n",cex=1.8,"bottomright",legend=c(paste("Overall: r=",round(c1,2),sep=""),paste("Lobbying Principals: r=",round(c2,2),sep="")))

tt<-merge(t82,est_9_20_3_2a_nu,by=c("name"))
prin<-tt[!grepl("/NP",tt$name)&!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name),]

c1<-abs(cor(tt$ideal.x,tt$ideal.y))
c2<-abs(cor(prin$ideal.x,prin$ideal.y))

plot(lwd=1.5,cex.lab=2,cex.main=2,cex.axis=2,main="(2) Comparison to Estimates Based on\nVotes Lobbied by Corps.",
     xlab="Ideal Point Scale (Est. Based on All Included Votes)",ylab="Ideal Point Scale (Est. Based on Lobbied Votes)",cex=1.5,xlim=c(-3,3),ylim=c(-3,3),
     tt$ideal.x[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)],tt$ideal.y[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)])
points(lwd=1.5,prin$ideal.x,prin$ideal.y,col="darkgray",cex=1.4)
legend("topleft",legend=c("Legislators/Candidates","Lobbying Principals"),cex = 1.8,pt.lwd=2,pch=c(1,1),col=c("black","darkgray"))
legend(bty="n",cex=1.8,"bottomright",legend=c(paste("Overall: r=",round(c1,2),sep=""),paste("Lobbying Principals: r=",round(c2,2),sep="")))

tt<-merge(t82,est_9_20_3_1b_nu,by=c("name"))
prin<-tt[!grepl("/NP",tt$name)&!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name),]
c1<-abs(cor(tt$ideal.x,tt$ideal.y))
c2<-abs(cor(prin$ideal.x,prin$ideal.y))

plot(lwd=1.5,cex.lab=2,cex.main=2,cex.axis=2,main="(3) Comparison to Estimates Based on Votes Lobbied\nby Corps. & Trade Groups (Only Roll Call Votes)",
     xlab="Ideal Point Scale (Est. Based on All Included Votes)",ylab="Ideal Point Scale (Est. Based on Lobbied Votes)",cex=1.5,xlim=c(-3,3),ylim=c(-3,3),
     tt$ideal.x[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)],tt$ideal.y[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)])
points(prin$ideal.x,prin$ideal.y,col="darkgray",cex=1.4,lwd=1.5)
legend("topleft",legend=c("Legislators/Candidates","Lobbying Principals"),cex = 1.8,pt.lwd=2,pch=c(1,1),col=c("black","darkgray"))
legend(bty="n",cex=1.8,"bottomright",legend=c(paste("Overall: r=",round(c1,2),sep=""),paste("Lobbying Principals: r=",round(c2,2),sep="")))

tt<-merge(t82,est_9_20_3_2b_nu,by=c("name"))
cor(tt$ideal.x,tt$ideal.y)
prin<-tt[!grepl("/NP",tt$name)&!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name),]
c1<-abs(cor(tt$ideal.x,tt$ideal.y))
c2<-abs(cor(prin$ideal.x,prin$ideal.y))

plot(lwd=1.5,cex.lab=2,cex.main=2,cex.axis=2,main="(4) Comparison to Estimates Based\non Votes Lobbied by Corps. & Trade Groups",
     xlab="Ideal Point Scale (Est. Based on All Included Votes)",ylab="Ideal Point Scale (Est. Based on Lobbied Votes)",cex=1.5,xlim=c(-3,3),ylim=c(-3,3),
     tt$ideal.x[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)],tt$ideal.y[grep("\\([A-Z][A-Z]/[A-Z]/[A-Z]",tt$name)])
points(prin$ideal.x,prin$ideal.y,col="darkgray",cex=1.4,lwd=1.5)
legend("topleft",legend=c("Legislators/Candidates","Lobbying Principals"),cex = 1.8,pt.lwd=2,pch=c(1,1),col=c("black","darkgray"))
legend(bty="n",cex=1.8,"bottomright",legend=c(paste("Overall: r=",round(c1,2),sep=""),paste("Lobbying Principals: r=",round(c2,2),sep="")))
dev.off()

##############
# Appendix H.2
# Figure 17
##############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H2.RData")
#####
#
#####

#1a
est2<-merge(est_9_20_3_1a_nu,cont_sc,by=c("name"))
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]
k<-1;qq3<-c();for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))],times=est2$weights[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))]))))/length(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2])
  k<-k+1
}
ms2<-matrix(NA,ncol=3000,nrow=101);k<-1
cts<-which(rnames[[1]]%in%est2$est_name[est2$corp_trade==1|est2$corp_trade==2]==T)
lgs<-which(rnames[[1]]%in%est2$est_name[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$est_name),grep("/NP",est2$est_name)),grep("_OTH",est2$est_name))]==T)
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    ms2[k,s-1]<-length(which(-ct_9203_1a[s,cts,1]<quantile(probs=c(i),rep(-ct_9203_1a[s,lgs,1],times=t5[lgs]))))
  }
  print(k);  k<-k+1
}
ms2<-ms2/length(which(est2$corp_trade==1|est2$corp_trade==2))

qq3list<-list(qq3)
ms2list<-list(ms2)

#1b
est2<-merge(est_9_20_3_1b_nu,cont_sc,by=c("name"))
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]
k<-1;qq3<-c();for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))],times=est2$weights[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))]))))/length(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2])
  k<-k+1
}
ms2<-matrix(NA,ncol=3000,nrow=101);k<-1
cts<-which(rnames[[2]]%in%est2$est_name[est2$corp_trade==1|est2$corp_trade==2]==T)
lgs<-which(rnames[[2]]%in%est2$est_name[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$est_name),grep("/NP",est2$est_name)),grep("_OTH",est2$est_name))]==T)
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    ms2[k,s-1]<-length(which(-ct_9203_1b[s,cts,1]<quantile(probs=c(i),rep(-ct_9203_1b[s,lgs,1],times=t6[lgs]))))
  }
  print(k);  k<-k+1
}
ms2<-ms2/length(which(est2$corp_trade==1|est2$corp_trade==2))

qq3list<-list(qq3list[[1]],qq3)
ms2list<-list(ms2list[[1]],ms2)
#2a
est2<-merge(est_9_20_3_2a_nu,cont_sc,by=c("name"))
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]
k<-1;qq3<-c();for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))],times=est2$weights[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))]))))/length(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2])
  k<-k+1
}
ms2<-matrix(NA,ncol=3000,nrow=101);k<-1
cts<-which(rnames[[3]]%in%est2$est_name[est2$corp_trade==1|est2$corp_trade==2]==T)
lgs<-which(rnames[[3]]%in%est2$est_name[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$est_name),grep("/NP",est2$est_name)),grep("_OTH",est2$est_name))]==T)
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    ms2[k,s-1]<-length(which(ct_9203_2a[s,cts,1]<quantile(probs=c(i),rep(ct_9203_2a[s,lgs,1],times=t7[lgs]))))
  }
  print(k);  k<-k+1
}
ms2<-ms2/length(which(est2$corp_trade==1|est2$corp_trade==2))

qq3list<-list(qq3list[[1]],qq3list[[2]],qq3)
ms2list<-list(ms2list[[1]],ms2list[[2]],ms2)
#2b
est2<-merge(est_9_20_3_2b_nu,cont_sc,by=c("name"))
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]
k<-1;qq3<-c();for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))],times=est2$weights[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$name),grep("/NP",est2$name)),grep("_OTH",est2$name))]))))/length(est2$bonica2_cf_most_overall[est2$corp_trade==1|est2$corp_trade==2])
  k<-k+1
}
ms2<-matrix(NA,ncol=3000,nrow=101);k<-1
cts<-which(rnames[[4]]%in%est2$est_name[est2$corp_trade==1|est2$corp_trade==2]==T)
lgs<-which(rnames[[4]]%in%est2$est_name[setdiff(setdiff(grep("\\([A-Z][A-Z]/[A-Z]/",est2$est_name),grep("/NP",est2$est_name)),grep("_OTH",est2$est_name))]==T)
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    ms2[k,s-1]<-length(which(-ct_9203_2b[s,cts,1]<quantile(probs=c(i),rep(-ct_9203_2b[s,lgs,1],times=t8[lgs]))))
  }
  print(k);  k<-k+1
}
ms2<-ms2/length(which(est2$corp_trade==1|est2$corp_trade==2))

qq3list<-list(qq3list[[1]],qq3list[[2]],qq3list[[3]],qq3)
ms2list<-list(ms2list[[1]],ms2list[[2]],ms2list[[3]],ms2)

############
# Figure 17
############
finames<-c("(1) Comparison to Estimates Based on\nVotes Lobbied by Corps. (Only Roll Call Votes)",
           "(3) Comparison to Estimates Based on Votes Lobbied\nby Corps. & Trade Groups (Only Roll Call Votes)",
           "(2) Comparison to Estimates Based on\nVotes Lobbied by Corps.",
           "(4) Comparison to Estimates Based\non Votes Lobbied by Corps. & Trade Groups")
#Fig 4-1 (robustness)
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_ct2.png",sep="/"),width=1300,height=1300)
par(mfrow=c(2,2),mar=c(0.5,0.5,0.5,0.5),mai=c(1,1.5,1,0.5))
for(k in c(1,3,2,4)){
  plot(cex.axis=2.5,cex.lab=2,cex=2,cex.main=2.25,xlab="",ylab="",
       main=paste(finames[k] ,sep=""), qq3list[[k]],col=rep(c("black","white"),50)) #;lines(qq2,col="red")
  for(i in 1:101){
    if(i%%2==0){next}
    points(cex=2,lwd=2.5,i,qq3list[[k]][i],col="black")
    points(cex=2,lwd=2.5,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2list[[k]][i,])))
    lines(cex=2,lwd=2.5,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2list[[k]][i,]),quantile(probs=c(0.975),ms2list[[k]][i,])))
  }
  mtext(side=2,line=5,at=c(0.5),c("Prop. of Corporations & Trade Groups"),cex=2)
  mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2)
  legend(cex=1.9,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
}
dev.off()

##############
# Appendix H.3
# Figure 18
##############
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H3.RData")

#############################################################################################
# The object 'est_all' contains all estimates from vote matrices with varying 
# minimum vote parameters the list names indicate these parameters in the following way:
# est_[min. # of votes per item]_[min. # of votes per actor]_[min. # of votes in minority]
#############################################################################################

cmb1<-matrix(NA,nrow=length(est_all),ncol=length(est_all))
cmb2<-matrix(NA,nrow=length(est_all),ncol=length(est_all))
cmp1<-matrix(NA,nrow=length(est_all),ncol=length(est_all))
cmp2<-matrix(NA,nrow=length(est_all),ncol=length(est_all))

for(i in 1:length(est_all)){
  for(j in 1:length(est_all)){
    if(j>=i){next}
    #merge estimates from vote matrices with different minimum vote parameters
    mmm<-merge(est_all[[i]],est_all[[j]],by=c("name"))
    #identifiers for lobbying principals
    prin<-mmm[!grepl("/NP",mmm$name)&!grepl("\\([A-Z][A-Z]/[A-Z]/[A-Z]",mmm$name),]
    #correlations
    #all observations
    cmb1[i,j]<-cor(mmm$ideal.x,mmm$ideal.y)
    cmb2[i,j]<-cor(mmm$ideal.x,mmm$ideal.y,method="spearman")
    #lobbying principals only
    cmp1[i,j]<-cor(prin$ideal.x,prin$ideal.y)
    cmp2[i,j]<-cor(prin$ideal.x,prin$ideal.y,method="spearman")
  }
}
#summaries of correlations
#all observations
#pearson
summary(as.vector(abs(cmb1))) 
#spearman
summary(as.vector(abs(cmb2)))
#lobbying principals only
#pearson
summary(as.vector(abs(cmp1))) 
#spearman
summary(as.vector(abs(cmp2)))

png(filename=paste(getwd(),"ST19_JOP_replication","robustness_minvote_params_hist_cor.png",sep="/"),width=800,height=500)
par(mfrow=c(1,2),mar=c(1,1,1,1),mai=c(1.05,1,1,1))
hist(abs(cmb1[1:27,1:27]),xlim=c(0.96,1),xlab="Correlation",main="(1) All Common Estimates",cex.main=2,cex.lab=2,cex.axis=2)
hist(abs(cmp1[1:27,1:27]),xlim=c(0.96,1),xlab="Correlation",main="(2) All Common Estimates\nof Lobbying Principals",cex.main=2,cex.lab=2,cex.axis=2)
dev.off()

################
# Appendix H.3
# Figures 19-22
################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H3.RData")

#############################################################################################
# The objects 'ms2list' and 'qq3list' contain the proportions of corporations and trade groups 
# with ideal points (ms2list) and CFscores (qq3list) than a given legislator quantile.
#
# The proportions were computed using the estimates in the object 
# est_all_x (ST19_JOP_replication.RData) and in the same way as for the objects ms2 
# and qq3 (see, e.g., replication_code_article.R)
# each list item is associated with estimates based on a vote matrix with a particular
# combination of minimum vote parameters:
# est_[min. # of votes per item]_[min. # of votes per actor]_[min. # of votes in minority]_x
#############################################################################################

##########
#Figure 19 
##########
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_minvote_params_comp-1.png",sep="/"),width=1300,height=1300)
par(mfrow=c(3,3),mar=c(0.5,0.5,0.5,0.5),mai=c(1,1.5,1,0.5))
for(k in 1:9){
  plot(cex.axis=2.5,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
       main=paste("(",k,") ",finames[k] ,sep=""), qq3list[[k]],col=rep(c("black","white"),50)) #;lines(qq2,col="red")
  for(i in 1:101){
    if(i%%2==0){next}
    points(cex=2,lwd=2.5,i,qq3list[[k]][i],col="black")
    points(cex=2,lwd=2.5,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2list[[k]][i,])))
    lines(cex=2,lwd=2.5,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2list[[k]][i,]),quantile(probs=c(0.975),ms2list[[k]][i,])))
  }
  mtext(side=2,line=5,at=c(0.5),c("Prop. of Corporations\n& Trade Groups"),cex=1.75)
  mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2)
  legend(cex=1.9,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
}
dev.off()
##########
#Figure 20
##########
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_minvote_params_comp-2.png",sep="/"),width=1300,height=1300)
par(mfrow=c(3,3),mar=c(0.5,0.5,0.5,0.5),mai=c(1,1.5,1,0.5))
for(k in 10:18){
  plot(cex.axis=2.5,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
       main=paste("(",k-9,") ",finames[k] ,sep=""), qq3list[[k]],col=rep(c("black","white"),50)) #;lines(qq2,col="red")
  for(i in 1:101){
    if(i%%2==0){next}
    points(cex=2,lwd=2.5,i,qq3list[[k]][i],col="black")
    points(cex=2,lwd=2.5,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2list[[k]][i,])))
    lines(cex=2,lwd=2.5,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2list[[k]][i,]),quantile(probs=c(0.975),ms2list[[k]][i,])))
  }
  mtext(side=2,line=5,at=c(0.5),c("Prop. of Corporations\n& Trade Groups"),cex=1.75)
  mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2)
  legend(cex=1.9,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
}
dev.off()
##########
#Figure 21
##########
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_minvote_params_comp-3.png",sep="/"),width=1300,height=1300)
par(mfrow=c(3,3),mar=c(0.5,0.5,0.5,0.5),mai=c(1,1.5,1,0.5))
for(k in 19:27){
  plot(cex.axis=2.5,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
       main=paste("(",k-18,") ",finames[k] ,sep=""), qq3list[[k]],col=rep(c("black","white"),50)) #;lines(qq2,col="red")
  for(i in 1:101){
    if(i%%2==0){next}
    points(cex=2,lwd=2.5,i,qq3list[[k]][i],col="black")
    points(cex=2,lwd=2.5,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2list[[k]][i,])))
    lines(cex=2,lwd=2.5,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2list[[k]][i,]),quantile(probs=c(0.975),ms2list[[k]][i,])))
  }
  mtext(side=2,line=5,at=c(0.5),c("Prop. of Corporations\n& Trade Groups"),cex=1.75)
  mtext(side=1,line=4,at=c(50),c("Legislator Quantile"),cex=2)
  legend(cex=1.9,pt.lwd =c(2,NA),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
}
dev.off()

################
# Appendix H.4
# Figure 22
################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
#load results and auxiliary data
load("~/ST19_JOP_replication.RData")
##############################
# Preparing Data for Analysis
##############################

est2<-est #est2
est2<-merge(est2,cont_sc,by=c("name")) #merge est2 with cont_sc
############################################################################
# Crucial for robustness check: exclude NFIB and BNSFs
############################################################################
est2<-est2[-c(grep("BNSF",est2$name),grep("NFIB",est2$name)),]

#remove all observations with missing contributor/recipient scores 
#and observations where CFscore is based on one distinct contributor
est2<-est2[-which(is.na(est2$bonica2_cf_r)==T&is.na(est2$bonica2_cf_most_overall)==T),]
#remove observations for which cf estimate is based on one distinct contributor
est2<-est2[-which(est2$bonica2_nd_most_overall==1),]

#preparation: calculating legislator quantiles
#get ids for legislators (not candidates only)
ilgs<-setdiff(union(grep("\\([A-Z][A-Z]/[A-Z]/H",est2$name),grep("\\([A-Z][A-Z]/[A-Z]/S",est2$name)),c(grep("/NP",est2$name),grep("_OTH",est2$name)))

llc<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs])))
lli<-apply(matrix(seq(0,1,0.001),ncol=1),1, function(x) quantile(na.rm=T,probs = x,rep(est2$ideal[ilgs],times=est2$weights[ilgs])))

qq3<-c();qql<-c();qqh<-c()
#ids for corporations and trade groups
ict<-which(est2$corp_trade==1|est2$corp_trade==2)
#ids for low-level contributors
ilc<-which(est2$HL=="L")
#ids for high-level contributors
ihc<-which(est2$HL=="H")
k<-1
for(i in seq(0,1,0.01)){
  qq3[k]<-length(which(est2$bonica2_cf_most_overall[ict]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[ict])
  qql[k]<-length(which(est2$bonica2_cf_most_overall[intersect(ict,ilc)]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[intersect(ict,ilc)])
  qqh[k]<-length(which(est2$bonica2_cf_most_overall[intersect(ict,ihc)]<
                         quantile(probs=c(i),na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(est2$bonica2_cf_most_overall[intersect(ict,ihc)])
  k<-k+1
}
qq3<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[ict]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(ict))
qql<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[intersect(ict,ilc)]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(intersect(ict,ilc)))
qqh<-apply(matrix(seq(0,1,0.01),ncol=1),1,function(x) length(which(est2$bonica2_cf_most_overall[intersect(ict,ihc)]<
                                                                     quantile(probs=x,na.rm=T,rep(est2$bonica2_cf_r[ilgs],times=est2$weights[ilgs]))))/length(intersect(ict,ihc)))

ict2<-which(rnames%in%est2$name[ict]==T)
ictl2<-which(rnames%in%est2$name[intersect(ict,ilc)]==T)
icth2<-which(rnames%in%est2$name[intersect(ict,ihc)]==T)
ilgs2<-which(rnames%in%est2$name[ilgs]==T)
ms2<-matrix(NA,ncol=3000,nrow=101)
msh<-matrix(NA,ncol=3000,nrow=101)
msl<-matrix(NA,ncol=3000,nrow=101)
k<-1
for(i in seq(0,1,0.01)){
  for(s in 2:3001){
    qqq<-quantile(probs=c(i),rep(est_x[s,ilgs2,1],times=est$weights[ilgs2]))
    ms2[k,s-1]<-length(which(est_x[s,ict2,1]<qqq))
    msl[k,s-1]<-length(which(est_x[s,ictl2,1]<qqq))
    msh[k,s-1]<-length(which(est_x[s,icth2,1]<qqq))
  }
  if(k%in%seq(1,101,10)){print(paste(round(k/101,2)*100,"%",sep=""))};  k<-k+1
}

ms2<-ms2/length(ict2)
msh<-msh/length(icth2)
msl<-msl/length(ictl2)

ilgs1<-setdiff(union(grep("\\([A-Z][A-Z]/[A-Z]/H",est1$name),grep("\\([A-Z][A-Z]/[A-Z]/S",est1$name)),c(grep("/NP",est1$name),grep("_OTH",est1$name)))
ict1<-which(est1$corp_trade=="1"|est1$corp_trade=="2")

png(filename=paste(getwd(),"ST19_JOP_replication","robustness_CIP.png",sep="/"),width=800,height=800)
par(mfrow=c(1,1),mar=c(1,1,1,1),mai=c(1,1.2,0.5,0.5))
plot(cex.axis=2.5,cex.lab=2.5,cex=2.5,cex.main=3,xlab="Legislator Quantile",ylab="Prop. of Corporations & Trade Groups",main=NA,
     qq3,col=rep(c("black","white"),50)) 
for(i in 1:101){
  if(i%%2==0){next}
  points(cex=2,lwd=2.5,i,qq3[i],col="black")
  points(cex=2,lwd=2.5,col="darkgray",c(i),c(quantile(probs=c(0.5),ms2[i,])))
  lines(cex=2,lwd=2.5,col="darkgray",c(i,i),c(quantile(probs=c(0.025),ms2[i,]),quantile(probs=c(0.975),ms2[i,])))
}
legend(cex=1.9,pt.lwd =c(2.5,2.5),lwd=c(NA,2),legend=c("CFscores","Ideal Points"),col=c("black","darkgray"),"topleft",pch=1)
dev.off()

################
# Appendix H.4
# Figure 23
################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H4_2.RData")
#########################################################################################
# Overview:
# The object 'contributions' contains 2002-2016 contributions data from
# the NIMSP (www.followthemoney.org)
# the object 'est_9_20_3' contains the ideal points used in the main analysis
# the object 'ianewid' is used to link names across the tables
# the object 'cont_sc' includes (among other things) CFscores  
# and categorizations of contributing organizations
# 'leg_weights' shows the number of sessions in which a legislator 
# (or lobbying organization, but this is not used) was active
# 'cnames' (not used for this section) & rnames, contain column names & rownames of
# different vote matrices (all combinations of the minimum vote assumptions, see Appendix H.3)
#########################################################################################

#load required packages
#between-set identification via multiple overimputation (Blackwell, Honacker, and King 2017)
#install.packages("Amelia")
library(Amelia)

##############################
## Clean up contributions data
##############################

#exclude recipients without linked ideal points
contributions<-contributions[which(contributions$iname!=""),]
#only include direct contributions
contributions<-contributions[which(contributions$Type_of_Transaction=="DIR"),]
#exclude contributions to self
contributions<-contributions[which(contributions$iname!=contributions$cname),]
contributions<-contributions[which(contributions$Career_Summary!=contributions$Contributor),]
contributions<-contributions[!grepl("personal funds",tolower(contributions$Occupation)),]
#
rem<-c(which(contributions$Career_Summary=="CHRISTENSON, GEORGE L"&contributions$Contributor=="CHRISTENSON, GEORGE"),
       which(contributions$Career_Summary=="DEYOE, DAVE"&contributions$Contributor=="DEYOE, DAVID"),
       which(contributions$Career_Summary=="ELGIN, JEFFREY C"&contributions$Contributor=="ELGIN, JEFFERY C"),
       which(contributions$Career_Summary=="ELGIN, JEFFREY C"&contributions$Contributor=="ELGIN, JEFFREY C & TERESA & JILL"),
       which(contributions$Career_Summary=="FALLON, ED"&contributions$Contributor=="FALLON, EDWARD"),
       which(contributions$Career_Summary=="FALLON, ED"&contributions$Contributor=="FALLON, EDWARD B"),
       which(contributions$Career_Summary=="FALLON, ED"&contributions$Contributor=="FALLON, EDWARD S"),
       which(contributions$Career_Summary=="FITZGERALD, JEFF"&contributions$Contributor=="FITZGERALD, JEFFREY M"),
       which(contributions$Career_Summary=="FREESE, STEPHEN J"&contributions$Contributor=="FREESE, STEPHEN"),
       which(contributions$Career_Summary=="FRISKE, DONALD R"&contributions$Contributor=="FRISKE, DONALD R & ELIZABETH"),
       which(contributions$Career_Summary=="GALBRAITH, DAVID"&contributions$Contributor=="GALBRAITH, DAVID L"),
       which(contributions$Career_Summary=="GALLOWAY, PAMELA GAIL"&contributions$Contributor=="GALLOWAY, PAMELA G"),
       which(contributions$Career_Summary=="GROENE, MICHAEL"&contributions$Contributor=="GROENE, MIKE"),
       which(contributions$Career_Summary=="GROENE, MICHAEL"&contributions$Contributor=="GROENE, MIKE & BARB"),
       which(contributions$Career_Summary=="HANSON, CURTIS D (CURT)"&contributions$Contributor=="HANSON, CURTIS D"),
       which(contributions$Career_Summary=="HILKEMANN, ROBERT BOB"&contributions$Contributor=="HILKEMANN, RON AMP; DONNA"),
       which(contributions$Career_Summary=="HOFFMAN, CLARENCE C"&contributions$Contributor=="HOFFMAN, CLARENCE & JOHN & LOUIS"),
       which(contributions$Career_Summary=="LEMAHIEU, DANIEL"&contributions$Contributor=="LEMAHIEU, DANIEL R"),
       which(contributions$Career_Summary=="KNABE, JOHN W"&contributions$Contributor=="KNABE, JOHN"),
       which(contributions$Career_Summary=="HOFFMAN, CLARENCE C"&contributions$Contributor=="HOFFMAN, CLARENCE & LYNN"),
       which(contributions$Career_Summary=="LARSON, CHRISTOPHER J (CHRIS)"&contributions$Contributor=="LARSON, CHRISTOPHER"),          
       which(contributions$Career_Summary=="HOGG, ROBERT M"&contributions$Contributor=="HOGG, ROB"),
       which(contributions$Career_Summary=="HOOGESTRAAT, TOM B"&contributions$Contributor=="HOOGESTRANT, TOM"),
       which(contributions$Career_Summary=="HOGG, ROBERT M"&contributions$Contributor=="HOGG, ROBERT"),
       which(contributions$Career_Summary=="LEACH, DAVE"&contributions$Contributor=="LEACH, DAVE & ARLO"),
       which(contributions$Career_Summary=="LAWTON, CATHARINE M"&contributions$Contributor=="LAWTON, CATHARINE M & PETE"),
       which(contributions$Career_Summary=="MANN, JEFF"&contributions$Contributor=="MANN, JEFFREY A"),
       which(contributions$Career_Summary=="MORONEY, RICHARD"&contributions$Contributor=="MORONEY, RICHARD K"),
       which(contributions$Career_Summary=="MILLER, HELEN"&contributions$Contributor=="MILLER, HELEN N"),
       which(contributions$Career_Summary=="MEYER, KURT"&contributions$Contributor=="MEYER, KURT L"),
       which(contributions$Career_Summary=="MEISTERLING, MARY"&contributions$Contributor=="MEISTERLING, RICHARD & MARY"),
       which(contributions$Career_Summary=="TAYLOR, LENA CAROLYN"&contributions$Contributor=="TAYLOR, LENA J"),
       which(contributions$Career_Summary=="TAYLOR, RICHARD"&contributions$Contributor=="TAYLOR, RICHARD D"),
       which(contributions$Career_Summary=="TAYLOR, ROBERT (ROB)"&contributions$Contributor=="TAYLOR, ROB & CHRISTI"),
       which(contributions$Career_Summary=="TAYLOR, ROBERT (ROB)"&contributions$Contributor=="TAYLOR, ROBERT W"),
       which(contributions$Career_Summary=="TAYLOR, ROBERT (ROB)"&contributions$Contributor=="TAYLOR, ROBERT W"),
       which(contributions$Career_Summary=="TAYLOR, ROBERT (ROB)"&contributions$Contributor=="TAYLOR, ROBERT W."),
       which(contributions$Career_Summary=="PETTIS, MARK L"&contributions$Contributor=="PETTIS, MARK"),
       which(contributions$Career_Summary=="SHOULTZ, DON"&contributions$Contributor=="SHOULTZ, DON & DIANNE"),
       which(contributions$Career_Summary=="THOMPSON, ED"&contributions$Contributor=="THOMPSON, EDWARD"),
       which(contributions$Career_Summary=="SCHUERER, NEAL"&contributions$Contributor=="SCHUERER, NEAL C"),
       which(contributions$Career_Summary=="THOMPSON, ED"&contributions$Contributor=="THOMPSON, EDWARD A"),
       which(contributions$Career_Summary=="RIESSEN, JOHN"&contributions$Contributor=="RIESSEN, JOHN H"),
       which(contributions$Career_Summary=="THOMPSON, ED"&contributions$Contributor=="THOMPSON, EDWARD F"),
       which(contributions$Career_Summary=="RIELLY, THOMAS J"&contributions$Contributor=="RIELLY, TOM"),
       which(contributions$Career_Summary=="VAN FOSSEN, JAMIE"&contributions$Contributor=="VAN FOSSEN, JAMES R"),
       which(contributions$Career_Summary=="VRUWINK, AMY SUE"&contributions$Contributor=="VRUWINK, AMY"),
       which(contributions$Career_Summary=="WOOD, JEFF"&contributions$Contributor=="WOOD, JEFFREY"),
       which(contributions$Career_Summary=="HULSEY, BRETT"&contributions$Contributor=="HULSEY, BRETT D"),
       which(contributions$Career_Summary=="ZIEMAN, MARK"&contributions$Contributor=="ZIEMAN, MARK L"),
       which(contributions$Career_Summary=="ZIEGLER, THOMAS P"&contributions$Contributor=="ZIEGLER, TOM"),
       which(contributions$Career_Summary=="WOODS, SCOTT ALLAN"&contributions$Contributor=="WOODS, SCOTT A"))
contributions<-contributions[-rem,]

#get rid of negative contributions (few observations, mostly errors)
contributions<-contributions[which(contributions$Amount>0),]

#remove legislators without ideal points
if(length(which(contributions$iname=="Grobschmidt (WI/D/S)"))>0){contributions<-contributions[-c(which(contributions$iname=="Grobschmidt (WI/D/S)")),]}
if(length(which(contributions$iname=="Riley (WI/D/H)"))>0){contributions<-contributions[-c(which(contributions$iname=="Riley (WI/D/H)")),]}
if(length(which(contributions$iname=="Shibilski (WI/D/S)"))>0){contributions<-contributions[-c(which(contributions$iname=="Shibilski (WI/D/S)")),]}

#check that contribution record matches correct party
contributions$party<-"";pp<-unique(contributions$iname)
for(i in 1:length(pp)){print(i);
  contributions$party[which(contributions$iname==pp[i])]<-substr(pp[i],gregexpr("/",pp[i])[[1]][1]+1,gregexpr("/",pp[i])[[1]][2]-1)
}
table(contributions$party,contributions$Specific_Party)
contributions[which(contributions$party=="Independent"&contributions$Specific_Party=="DEMOCRATIC"),]
#delete Morrow, Janice L. (WI/Independent/NP) bc all here contributions are from when she was running as democrat
contributions<-contributions[-which(contributions$iname=="Morrow, Janice L. (WI/Independent/NP)"&contributions$Specific_Party=="DEMOCRATIC"),]
contributions[which(contributions$party=="Libertarian"&contributions$Specific_Party=="DEMOCRATIC"),]
contributions<-contributions[-which(contributions$iname=="Mohn, Craig (WI/Libertarian/NP)"&contributions$Specific_Party=="DEMOCRATIC"),]
contributions[which(contributions$party=="R"&contributions$Specific_Party=="REAGAN-WASHINGTON TEA"),]
contributions<-contributions[-which(contributions$iname=="Hou-Seye, Job E. (WI/R/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions[which(contributions$party=="Independent"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Sarnowski, John M. (WI/Independent/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Finley, John K. (WI/Independent/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
#correct party
contributions$iname[which(contributions$iname=="Hulsey, B. (WI/I/H)"&contributions$Specific_Party=="DEMOCRATIC")]<-"Hulsey, B. (WI/D/H)"
#in 2016, chip maxwell no longer independent
contributions<-contributions[-which(contributions$iname=="Maxwell, C. (NE/I/S)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Geary, Christopher N. (NE/Non-partisan/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Nelson, Mark (IA/Libertarian/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Sponholz, Brad (WI/Libertarian/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
contributions<-contributions[-which(contributions$iname=="Thompson, Ed (WI/Libertarian/NP)"&contributions$Specific_Party=="REPUBLICAN"),]
#wrong david johnson...
contributions<-contributions[-which(contributions$iname=="Johnson, D. (IA/R/S)"&contributions$Specific_Party=="DEMOCRATIC"),]
#wrong party
contributions<-contributions[-which(contributions$iname=="Struyk (IA/R/H)"&contributions$Specific_Party=="DEMOCRATIC"),]
#wrong party
#contributions<-contributions[-which(contributions$iname=="Nass (WI/R/H_S)"&contributions$Specific_Party=="DEMOCRATIC"),]
#democrat in  primary 2010 and republican in general..
contributions<-contributions[-which(contributions$iname=="Moore, B. (IA/R/H)"&contributions$Specific_Party=="DEMOCRATIC"),]
#wrong chuck larson...
#contributions<-contributions[-which(contributions$iname=="Larson, Charles W. (IA/R/S)"&contributions$Specific_Party=="DEMOCRATIC"),]

contributions$iname[which(contributions$iname=="Ziegelbauer, R. (WI/D/H)"&contributions$Specific_Party=="INDEPENDENT")]<-"Ziegelbauer, R. (WI/I/H)"
#not running as partisan
contributions<-contributions[-which(contributions$iname=="Pridemore (WI/R/H)"&contributions$Office_Sought=="SUPERINTENDENT OF PUBLIC INSTRUCTION"),]

cons<-unique(contributions$cname)
tagg<-aggregate(contributions$Amount, by=list(cname=contributions$cname),FUN="sum")
colnames(tagg)<-c("cname","TotAmount")
contributions<-merge(contributions,tagg,by=c("cname"))
contributions$frac<-contributions$Amount/contributions$TotAmount
#weighted ideal points
contributions$wideal<-contributions$ideal*contributions$frac
tagg1<-aggregate(contributions$wideal, by=list(cname=contributions$cname),FUN="sum")
# 
colnames(tagg1)<-c("cname","imean")
contributions<-merge(contributions,tagg1,by=c("cname"))

t8<-est_9_20_3
t8$imean<-NA
cons<-intersect(t8$name,contributions$cname)
for(i in 1:length(cons)){
  t8$imean[t8$name==cons[i]]<-contributions$imean[which(contributions$cname==cons[i])[1]]
}
t8<-merge(t8,cont_sc,by=c("name"))

#get contribution-weighted ideal point 
#exclude contributions from corporations and trade groups & membership organizations
#use single-issue,ideological & labor PACs, party and candidate committees
wideal_r<-data.frame(iname=unique(contributions$iname),stringsAsFactors = F)
tinw3<-contributions[union(intersect(which(contributions$Broad_Sector%in%c("Labor","Ideology/Single Issue","Party","Candidate Contributions")==T),which(contributions$Type_of_Contributor=="Non-Individual")),which(contributions$Type_of_Contributor=="Individual")),] #Labor, Ideology/Single Issue, Candidate Contributions, party
#also, reduce by excluding those with less than 2 distinct contributions
nn<-sort(table(tinw3$cname),decreasing = T);nn<-nn[-which(nn<2)]
tinw4<-tinw3[which(tinw3$cname%in%names(nn)==T),]

wideal_r$rmean<-NA;wideal_r$rmean2<-NA;wideal_r$rmean3<-NA;wideal_r$rmean4<-NA;wideal_r$cmean<-NA;wideal_r$cmean2<-NA;wideal_r$ideal<-NA; wideal_r$bonica2_cf_r<-NA;
wideal_r$one<-0;
for(i in 1:nrow(wideal_r)){
  #this code is unnecessarily slow..
  print(i);#get all contributions to cand
  cc1<-tinw3$Amount[which(tinw3$iname==wideal_r$iname[i])]
  cc2<-tinw3$Amount[which(tinw3$iname==wideal_r$iname[i]&grepl("/NP",tinw3$iname)==F)]
  cc3<-tinw4$Amount[which(tinw4$iname==wideal_r$iname[i])]
  cc4<-tinw4$Amount[which(tinw4$iname==wideal_r$iname[i]&grepl("/NP",tinw4$iname)==F)]
  #get fractions
  #with inw2 and test2
  cc1f<-cc1/sum(cc1,na.rm=T)
  cc2f<-cc2/sum(cc2,na.rm=T)
  #with inw4 and test4
  cc3f<-cc3/sum(cc3,na.rm=T)
  cc4f<-cc4/sum(cc4,na.rm=T)
  #get all ideal points from cons
  cc1i<-tinw3$imean[tinw3$iname==wideal_r$iname[i]]
  cc2i<-tinw3$imean[setdiff(which(tinw3$iname==wideal_r$iname[i]),grep("/NP",tinw3$iname))]
  cc3i<-tinw4$imean[tinw4$iname==wideal_r$iname[i]]
  cc4i<-tinw4$imean[which(tinw4$iname==wideal_r$iname[i]&grepl("/NP",tinw4$iname)==F)]
  #multiply by fractions
  wideal_r$rmean[i]<-weighted.mean(c(cc1i),c(cc1f))
  wideal_r$rmean2[i]<-weighted.mean(c(cc2i),c(cc2f))
  wideal_r$rmean3[i]<-weighted.mean(c(cc3i),c(cc3f))
  wideal_r$rmean4[i]<-weighted.mean(c(cc4i),c(cc4f))
  wideal_r$cmean[i]<-tinw3$imean[which(tinw3$cname==wideal_r$iname[i])[1]]
  wideal_r$cmean2[i]<-tinw4$imean[which(tinw4$cname==wideal_r$iname[i])[1]]
  wideal_r$ideal[i]<-t8$ideal[which(wideal_r$iname[i]==t8$name)[1]]
  wideal_r$bonica2_cf_r[i]<-t8$bonica2_cf_r[which(wideal_r$iname[i]==t8$name)[1]]
  if(length(c(cc1i))<8){wideal_r$one[i]<-1}
}

gc();gc();gc()
#between-set identification via multiple overimputation (Blackwell, Honacker, and King 2017)

af<-as.data.frame(as.matrix(cbind(wideal_r$cmean[which(is.na(wideal_r$rmean)==F&is.na(wideal_r$cmean)==F)],wideal_r$rmean[which(is.na(wideal_r$rmean)==F&is.na(wideal_r$cmean)==F)])))
af1<-as.data.frame(as.matrix(cbind(wideal_r$rmean[which(is.na(wideal_r$rmean)==F&is.na(wideal_r$cmean)==F)],wideal_r$cmean[which(is.na(wideal_r$rmean)==F&is.na(wideal_r$cmean)==F)])))

ksu_mo2<-matrix(NA,11,6)
prk<-c(0.01,seq(0.1,1,0.1))
for(k in 1:11){
  mo<-c();mo2<-c();llm<-c();mo1<-c();llm1<-c()
  for(j in 1:20){
    tt<-moPrep(af1,V2 ~ V2,error.proportion =prk[k])
    tt1<-amelia(x=tt,m=10)
    re<-c();re1<-c();for(i in 1:10){
      re[i]<-coefficients(lm(tt1$imputations[[i]][,1]~tt1$imputations[[i]][,2]))[2]
      re1[i]<-coefficients(lm(tt1$imputations[[i]][,1]~tt1$imputations[[i]][,2]))[1]
    }
    mo1[j]<-mean(re)
    mo2[j]<-mean(re1)
  }
  ksu_mo2[k,1]<-quantile(mo1,c(0.025))
  ksu_mo2[k,2]<-mean(mo1)
  ksu_mo2[k,3]<-quantile(mo1,c(0.975))
  ksu_mo2[k,4]<-quantile(mo2,c(0.025))
  ksu_mo2[k,5]<-mean(mo2)
  ksu_mo2[k,6]<-quantile(mo2,c(0.975))
}

#adjustment coefficients
ksu_mo2[6,c(5,2)]

#df1<-unique(inw2[grep("[a-z]",inw2$cname),c(95,93)])
t8$imean_adj<-ksu_mo2[6,c(5)]+ksu_mo2[6,c(2)]*t8$imean
#which is right projection?

#look at figure showing organizations with at least two contributions on my side
contributions$count<-1;agg<-aggregate(contributions$count,by=list(cname=contributions$cname),FUN="sum")
agg2<-aggregate(contributions$count,by=list(iname=contributions$iname),FUN="sum")
nms<-agg$cname[c(intersect(grep("[a-z]",agg$cname),setdiff(which(agg$x>1),union(grep("\\([A-Z][A-Z]/[A-Z]/",agg$cname),grep("[0-9]",agg$cname)))))]

#include one on my side but exclude one on the other side
t83<-t8[which(t8$bonica2_nd_most_overall>1),]
#include if more than one on either side
t84<-t8[which(t8$bonica2_nd_most_overall>1&t8$name%in%nms==T),]

############
# Figure 23
# Panel 1
############
widl<-wideal_r[which(is.na(wideal_r$rmean)==F&is.na(wideal_r$bonica2_cf_r)==F&!grepl("/NP",wideal_r$iname)==T),]

#make comparison between cfscores and cw recipient scores for legislators
png(filename=paste(getwd(),"ST19_JOP_replication","cf_and_cw-ideal.png",sep="/"),width=1200,height=1200)
#png(filename="/Users/sebastianthieme/Dropbox/2 IAL/Paper/JOPshort/Revise_and_Resubmit/figures/cf_vs_cw-ideal_leg.png",height = 800,width=800)
par(mfrow=c(2,2),mar=c(1,1,1,1),mai=c(1.2,1.5,1.1,0.5))
plot(cex.axis=2,xlab="",ylab="",cex=2,cex.main=2.5,main="(1) Legislator's Recipient CFscores\nand CW-Ideal Points",
     widl$bonica2_cf_r,widl$rmean) #select rmean or rmean3
mtext(side=1,line=3,at=c(-1.1,1.1),c("liberal","conservative"),cex=1.90)
mtext(side=2,line=3,at=c(-1.25,1.25),c("liberal","conservative"),cex=1.90)
mtext(side=1,line=5.5,at=c(0),c("CFscore Scale"),cex=2.2)
mtext(side=2,line=6,at=c(0),c("Contribution-Weighted Ideal Point Scale"),cex=2.2)
#calculate correlation
cor(use="pairwise.complete.obs",widl$bonica2_cf_r,widl$rmean)
cor(use="pairwise.complete.obs",widl$bonica2_cf_r,widl$rmean2)
nrow(widl)

########
#Panel 2
########
plot(pch="C",cex=2,cex.main=2.5,main="(2) Comparison between CFscores and\nContribution-Weighted Ideal Points",cex.axis=2,xlab="",ylab="",xlim=c(-2,2),
     ylim=c(-2.75,2.75),t84$bonica2_cf_most_overall[t84$corp_trade=="1"],t84$imean_adj[t84$corp_trade=="1"])
points(cex=2,pch="T",t84$bonica2_cf_most_overall[t84$corp_trade=="2"],t84$imean_adj[t84$corp_trade=="2"])
points(cex=2,pch=1,lwd=2,col="darkgray",t84$bonica2_cf_most_overall[t84$corp_trade!="1"&t84$corp_trade!="2"],t84$imean_adj[t84$corp_trade!="1"&t84$corp_trade!="2"])
mtext(side=1,line=3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.85)
mtext(side=2,line=3,at=c(-1.6,1.6),c("liberal","conservative"),cex=1.85)
mtext(side=1,line=5.5,at=c(0),c("CFscore Scale"),cex=2.2)
mtext(side=2,line=6,at=c(0),c("Contribution-Weighted Ideal Point Scale"),cex=2.2)
#correlation for corps.  & trade groups: 0.73
cor(t84$bonica2_cf_most_overall[t84$corp_trade%in%c("1","2")],t84$imean_adj[t84$corp_trade%in%c("1","2")])
#correlation for other organizations: 0.89
cor(t84$bonica2_cf_most_overall[t84$corp_trade!="1"&t84$corp_trade!="2"],t84$imean_adj[t84$corp_trade!="1"&t84$corp_trade!="2"])
#overall correlation: 0.86
cor(t84$bonica2_cf_most_overall,t84$imean_adj)
#add weights
ww<-data.frame(weights=leg_weights$est_9_20_3,iname=rnames$est_9_20_3,stringsAsFactors = F)
ww$iname[intersect(grep("_IA",ww$iname),grep("\\(IA/",ww$iname))]<-gsub("_IA","",ww$iname[intersect(grep("_IA",ww$iname),grep("\\(IA/",ww$iname))])
ww$iname[intersect(grep("_NE",ww$iname),grep("\\(NE/",ww$iname))]<-gsub("_NE","",ww$iname[intersect(grep("_NE",ww$iname),grep("\\(NE/",ww$iname))])
ww$iname[intersect(grep("_WI",ww$iname),grep("\\(WI/",ww$iname))]<-gsub("_WI","",ww$iname[intersect(grep("_WI",ww$iname),grep("\\(WI/",ww$iname))])
if(length(grep("weights",colnames(widl)))>0){widl<-widl[,-grep("weights",colnames(widl))]}
widl<-merge(widl,ww,by=c("iname"))

abline(lwd=2,col="darkgray",h=median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))
abline(lwd=2,lty=2,col="darkgray",h=median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))
abline(lwd=2,col="darkgray",v=median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))
abline(lty=2,lwd=2,col="darkgray",v=median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))
legend(cex=1.75,"topleft",legend=c("Corporations","Trade Groups","Other Groups","Dem. Median","Rep. Median"),pch=c("C","T","O",NA,NA),lty=c(NA,NA,NA,2,1),col=c("black","black","darkgray","darkgray","darkgray"),lwd=c(NA,NA,NA,2,2),pt.lwd=c(2,2,2,NA,NA))

#what is the percentage of moderate cfscores 
t84$od_cf<-NA;t84$od_cf[which(t84$bonica2_cf_most_overall<median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t84$od_cf[which(t84$bonica2_cf_most_overall>median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t84$bonica2_cf_most_overall<median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t84$od_cf[which(t84$bonica2_cf_most_overall>median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
t84$od_cw<-NA;t84$od_cw[which(t84$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t84$od_cw[which(t84$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t84$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t84$od_cw[which(t84$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
cor.test(t84$od_cf,t84$od_cw,method="spearman",use="pairwise.complete.obs")
table(t84$od_cf,t84$od_cw)
#calculate percentage including only those that have both measures
table(t84$od_cf[t84$corp_trade=="1"|t84$corp_trade=="2"],t84$od_cw[t84$corp_trade=="1"|t84$corp_trade=="2"])
#correspondence between two measures in which corporations & trade groups are moderate/extreme: 94%
(149+6+1)/(149+6+1+1+4+5)

#for all
t8$od_cf<-NA;t8$od_cf[which(t8$bonica2_cf_most_overall<median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t8$od_cf[which(t8$bonica2_cf_most_overall>median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t8$bonica2_cf_most_overall<median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t8$od_cf[which(t8$bonica2_cf_most_overall>median(na.rm=T,rep(widl$bonica2_cf_r[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
t8$od_cw<-NA;t8$od_cw[which(t8$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t8$od_cw[which(t8$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t8$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t8$od_cw[which(t8$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
cor.test(t8$od_cf,t8$od_cw,method="spearman",use="pairwise.complete.obs")
table(t8$od_cf,t8$od_cw)
#calculate percentage including only those that have both measures
table(t8$od_cf[t8$corp_trade=="1"|t8$corp_trade=="2"],t8$od_cw[t8$corp_trade=="1"|t8$corp_trade=="2"])
(154+5+2)/(154+7+1+1+2+5+5)

#proportion of moderate corp and trade pacs in main analysis
153/164

##how many corporations and ideal points for new measure
length(which(is.na(t84$imean_adj[t84$cat=="C"])==F))
length(which(is.na(t84$imean_adj[t84$cat=="T"])==F))

t85<-t8[which(t8$name%in%nms==T),]
t85$cat<-"O";
t85$cat[which(t85$name%in%cont_sc$name[substr(cont_sc$cat,1,1)=="C"])]<-"C"
t85$cat[which(t85$name%in%cont_sc$name[substr(cont_sc$cat,1,1)=="T"])]<-"T"
#how many 
length(which(is.na(t85$imean_adj2[t85$cat=="C"])==F))
length(which(is.na(t85$imean_adj2[t85$cat=="T"])==F))
length(which(is.na(t85$imean_adj2[t85$cat=="O"])==F))

widl2<-wideal_r[!grepl("/NP",wideal_r$iname),]
if(length(grep("weights",colnames(widl2)))>0){widl2<-widl2[,-grep("weights",colnames(widl2))]}
colnames(ww)<-c("weights","iname")
widl2<-merge(widl2,ww,by=c("iname"))
#########
#Panel 3
#########
plot(lwd=2,main="(3) All Contribution-Weighted Ideal Points",cex.lab=1.5,cex.axis=1.5,xlab="",cex=2,cex.main=2.5,density(na.omit(widl$rmean)),ylim=c(0,2))
lines(lwd=2,density(t85$imean_adj[t85$corp_trade=="1"]),lty=3)
lines(lwd=2,density(t85$imean_adj[t85$corp_trade=="2"]),lty=2)
abline(lwd=2,v=c(median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/R/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/R/",widl2$iname)]))),col="gray")
abline(lwd=2,v=c(median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/D/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/D/",widl2$iname)]))),col="gray",lty=2)
rm<-median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/R/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/R/",widl2$iname)]))
dm<-median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/D/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/D/",widl2$iname)]))
legend(cex=1.75,"topleft",legend=c("Corporations","Trade Groups","Legislators","Democratic Median","Republican Median"),lwd=2,lty=c(3,2,1,2,1),col=c("black","black","black","gray","gray"))
mtext(side=1,line=5.5,at=c(0),c("Contribution-Weighted Ideal Point Scale"),cex=2.2)
mtext(side=1,line=3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.75)

rm<-median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/R/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/R/",widl2$iname)]))
dm<-median(na.rm = T,rep(widl2$rmean[grep("[A-Z][A-Z]/D/",widl2$iname)],times=widl2$weights[grep("[A-Z][A-Z]/D/",widl2$iname)]))
pcc<-length(which(t85$imean_adj[t85$corp_trade=="1"]>dm&
                    t85$imean_adj[t85$corp_trade=="1"]<rm))/length(t85$imean_adj[t85$corp_trade=="1"])
pctg<-length(which(t85$imean_adj[t85$corp_trade=="2"]>dm&
                     t85$imean_adj[t85$corp_trade=="2"]<rm))/length(t85$imean_adj[t85$corp_trade=="2"])
(pcc*71)/(71+105)+(pctg*105)/(71+105)
#91% of corp and trade groups moderate (93% in main analysis, see below)
#comparison to cfscores (with at least 2)
if(length(grep("weights",colnames(t8)))>0){t8<-t8[,-grep("weights",colnames(t8))]}
colnames(ww)<-c("weights","name")
t8<-merge(t8,ww,by=c("name"))
#main analysis
rm<-median(na.rm = T,rep(t8$bonica2_cf_r[setdiff(grep("[A-Z][A-Z]/R/",t8$name),union(grep("_OTH",t8$name),grep("NP",t8$name)))],times=t8$weights[setdiff(grep("[A-Z][A-Z]/R/",t8$name),union(grep("_OTH",t8$name),grep("NP",t8$name)))]))
dm<-median(na.rm = T,rep(t8$bonica2_cf_r[setdiff(grep("[A-Z][A-Z]/D/",t8$name),union(grep("_OTH",t8$name),grep("NP",t8$name)))],times=t8$weights[setdiff(grep("[A-Z][A-Z]/D/",t8$name),union(grep("_OTH",t8$name),grep("NP",t8$name)))]))
pcc<-length(which(t83$bonica2_cf_most_overall[t83$corp_trade=="1"]>dm&
                    t83$bonica2_cf_most_overall[t83$corp_trade=="1"]<rm))/length(t83$bonica2_cf_most_overall[t83$corp_trade=="1"])
pctg<-length(which(t83$bonica2_cf_most_overall[t83$corp_trade=="2"]>dm&
                     t83$bonica2_cf_most_overall[t83$corp_trade=="2"]<rm))/length(t83$bonica2_cf_most_overall[t83$corp_trade=="2"])
#main analysis: 93%
(pcc*83)/(83+105)+(pctg*105)/(83+105)

#########
# Panel 4
#########
plot(pch="C",cex=2,cex.main=2.5,main="(4) Comparison for Corps. & Trade Groups",cex.axis=2,xlab="",ylab="",xlim=c(-2,2),
     ylim=c(-2.75,2.75),t84$imean_adj[t84$corp_trade=="1"],
     t84$ideal[t84$corp_trade=="1"])
points(cex=2,pch="T",t84$imean_adj[t84$corp_trade=="2"],t84$ideal[t84$corp_trade=="2"])
mtext(side=1,line=3,at=c(-1.35,1.35),c("liberal","conservative"),cex=1.75)
mtext(side=2,line=3,at=c(-1.6,1.6),c("liberal","conservative"),cex=1.75)
mtext(side=2,line=6,at=c(0),c("Ideal Point Scale"),cex=2.2)
mtext(side=1,line=5.5,at=c(0),c("Contribution-Weighted Ideal Point Scale"),cex=2.2)

abline(lwd=2,col="darkgray",v=median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))
abline(lty=2,lwd=2,col="darkgray",v=median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))
abline(lwd=2,col="darkgray",h=median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))
abline(lty=2,lwd=2,col="darkgray",h=median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))
#construct q-q plot for legislators
############
#legislator to legislator plot
k<-1
lli<-c();llc<-c();for(i in seq(0,1,0.001)){
  llc[k]<-quantile(na.rm=T,probs=c(i),rep(widl$rmean,times=widl$weights))
  lli[k]<-quantile(na.rm=T,probs=c(i),rep(widl$ideal,times=widl$weights))
  k<-k+1}
lines(lty=3,llc,lli)
#legend
legend(cex=1.6,"bottomright",legend=c("Corporations","Trade Groups","Democratic Median","Republican Median","Legislator Q-Q Plot"),pch=c("C","T",NA,NA,NA),lty=c(NA,NA,2,1,3),col=c("black","black","darkgray","darkgray","darkgray"),lwd=c(NA,NA,2,2,2),pt.lwd=c(2,2,NA,NA,NA))
dev.off()

#look at correlations
cor.test(use="pairwise.complete.obs",t85$imean[t85$cat=="O"],t85$ideal[t85$cat=="O"])
cor.test(use="pairwise.complete.obs",t85$imean[t85$cat!="O"],t85$ideal[t85$cat!="O"])

#look at which are more conservative/liberal
clist_c<-t84$imean_adj[which(t84$corp_trade=="1"&is.na(t84$imean_adj)==F&is.na(t84$ideal)==F)]
clist_i<-t84$ideal[which(t84$corp_trade=="1"&is.na(t84$imean_adj)==F&is.na(t84$ideal)==F)]
beab<-c(rep(0,length(clist_c)))
for(i in 1:length(clist_c)){
  if(lli[which(abs(clist_c[i]-llc)==min(abs(clist_c[i]-llc)))]<clist_i[i]){beab[i]<-1}
}
tlist_c<-t84$imean_adj[which(t84$corp_trade=="2"&is.na(t84$imean_adj)==F&is.na(t84$ideal)==F)]
tlist_i<-t84$ideal[which(t84$corp_trade=="2"&is.na(t84$imean_adj)==F&is.na(t84$ideal)==F)]
beab2<-c(rep(0,length(tlist_c)))
for(i in 1:length(tlist_c)){
  if(lli[which(abs(tlist_c[i]-llc)==min(abs(tlist_c[i]-llc)))]<tlist_i[i]){beab2[i]<-1}
}
plot(col="gray",llc,lli)
points(tlist_c[beab2==1],tlist_i[beab2==1],pch="T")
points(clist_c[beab==1],clist_i[beab==1],pch="C")
points(tlist_c[beab2==0],tlist_i[beab2==0],pch="T",col="red")
points(clist_c[beab==0],clist_i[beab==0],pch="C",col="red")

#Using the quantile-to-quantile legislator plot shows that
# 115 of 166 organizations (69%) have a more conservative ideal point
# than would be implied by their con- tribution record.
length(which(beab==1))+length(which(beab2==1))
(length(which(beab==1))+length(which(beab2==1)))/(length(beab)+length(beab2))

#look at comparison of moderate and extreme
#what is the percentage of moderate cfscores 
t84$od_cw<-NA;t84$od_cw[which(t84$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t84$od_cw[which(t84$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t84$imean_adj<median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t84$od_cw[which(t84$imean_adj>median(na.rm=T,rep(widl$rmean[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
t84$od_id<-NA;t84$od_id[which(t84$ideal<median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)])))]<-1
t84$od_id[which(t84$ideal>median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/D/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/D/",widl$iname)]))&t84$ideal<median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-2
t84$od_id[which(t84$ideal>median(na.rm=T,rep(widl$ideal[grep("\\([A-Z][A-Z]/R/",widl$iname)],times=widl$weights[grep("\\([A-Z][A-Z]/R/",widl$iname)])))]<-3
cor.test(t84$od_cw,t84$od_id,method="spearman",use="pairwise.complete.obs")
table(t84$od_cw[t84$corp_trade!="1"&t84$corp_trade!="2"],t84$od_id[t84$corp_trade!="1"&t84$corp_trade!="2"])
#Of the 153 corporations and trade groups with a moderate contribution record, 
#the revealed policy preferences of 35 (23%) are extremely conservative.
table(t84$od_cw[t84$corp_trade=="1"|t84$corp_trade=="2"],t84$od_id[t84$corp_trade=="1"|t84$corp_trade=="2"])

################
# Appendix H.5
# Figure 24
################
rm(list=ls())
gc();gc();gc();gc();gc();gc();gc();gc();gc();gc();gc()
Sys.setlocale(locale="C")
dir.create("ST19_JOP_replication")
#####
load("~/ST19_JOP_replication_H5.RData")
################################################################################################
# Overview:
# the object 'bonica2' contains the dime 2.0 contributor dataset (as well as added variable am);
# the object 'rec' contains the dime 2.0 recipient dataset;
# the object 'smc_i' contains the shor & mccarty 2018 individual legislator data;
# the object 'ne_party' is a table connecting legislator names with ICPSR2 (permits merging with dime data);
# the object 'lobp' shows registered lobbying principals in 2016 in all 50 states. These data
# were collected from the website of the NIMSP (followthemoney.org) 
# the object 'ct' contains addiional corp/trade classifications for lobbying principals
################################################################################################
#remove unnecessary columns
bonica2<-bonica2[,-grep("amount_19[8-9]",colnames(bonica2))]
bonica2<-bonica2[,-grep("amount_2000",colnames(bonica2))]
bonica2<-bonica2[,-grep("first_cycle_active",colnames(bonica2))]
gc();gc();gc();gc();gc();gc();
#calculate total amount contributed between 2002 and 2014 
bonica2$am2<-bonica2$amount_2002+bonica2$amount_2004+bonica2$amount_2006+bonica2$amount_2008+bonica2$amount_2010+bonica2$amount_2012+bonica2$amount_2014
#limit by is_corp, because a lot of projected are legislators or others
table(bonica2$is_corp,bonica2$is.projected) #and all corp are projected
#limit by whether last contribution was made after 2000
#limit by projected (as proxy for corp/trade) and whether more than one record
tt<-bonica2[bonica2$last_cycle_active>2000&bonica2$num.records>1&bonica2$num.distinct>1&bonica2$is_corp=="corp",]
#count how many obs before start cleaning up
t10<-nrow(tt)
#removing observations that are not corporations or trade groups
tt<-tt[!grepl("local",tt$most.recent.contributor.name),]
tt<-tt[!grepl("international union",tt$most.recent.contributor.name),]
tt<-tt[!grepl(" transit union",tt$most.recent.contributor.name),]
tt<-tt[!grepl(" transportation union",tt$most.recent.contributor.name),]
tt<-tt[!grepl(" union of roofers",tt$most.recent.contributor.name),]
tt<-tt[!grepl(" police union ",tt$most.recent.contributor.name),]
tt<-tt[!grepl(" comm union ",tt$most.recent.contributor.name),]
tt<-tt[!grepl("democratic",tt$most.recent.contributor.name),]
tt<-tt[!grepl("republican",tt$most.recent.contributor.name),]
tt<-tt[!grepl("working families ",tt$most.recent.contributor.name),]
tt<-tt[!grepl("dnc",tt$most.recent.contributor.name),]
tt<-tt[-which(tt$most.recent.contributor.name==""),]
tt<-tt[!grepl("andrew cuomo",tt$most.recent.contributor.name),]
tt<-tt[!grepl("afl cio",tt$most.recent.contributor.name),]
tt<-tt[!grepl("afl-cio",tt$most.recent.contributor.name),]
tt<-tt[!grepl("afscme",tt$most.recent.contributor.name),]
tt<-tt[!grepl("aclu",tt$most.recent.contributor.name),]
tt<-tt[!grepl("sierra club",tt$most.recent.contributor.name),]
tt<-tt[!grepl("planned parenthood",tt$most.recent.contributor.name),]
tt<-tt[!grepl("trades council",tt$most.recent.contributor.name),]
tt<-tt[!grepl("engineers council",tt$most.recent.contributor.name),]
tt<-tt[!grepl("council\\, charlie",tt$most.recent.contributor.name),]
tt<-tt[!grepl("labor council",tt$most.recent.contributor.name),]
tt<-tt[!grepl("council of hotel employee",tt$most.recent.contributor.name),]
tt<-tt[!grepl("council of carpenter",tt$most.recent.contributor.name),]
tt<-tt[!grepl("atu cope",tt$most.recent.contributor.name),]
tt<-tt[!grepl("democrat-farmer",tt$most.recent.contributor.name),]
tt<-tt[!grepl("democrat",tt$most.recent.contributor.name),]
tt<-tt[!grepl("cmte to reelect",tt$most.recent.contributor.name),]
tt<-tt[!grepl("cmte to elect",tt$most.recent.contributor.name),]
tt<-tt[!grepl("re election committee",tt$most.recent.contributor.name),]
tt<-tt[!grepl("in congress committee",tt$most.recent.contributor.name),]
tt<-tt[!grepl("right to life",tt$most.recent.contributor.name),]
tt<-tt[!grepl("committee to elect",tt$most.recent.contributor.name),]
tt<-tt[!grepl("committee to reelect",tt$most.recent.contributor.name),]
tt<-tt[!grepl("campaign cmte",tt$most.recent.contributor.name),]
tt<-tt[!grepl("police",tt$most.recent.contributor.name),]
tt<-tt[!grepl("workers",tt$most.recent.contributor.name),]
tt<-tt[!grepl("labor federation",tt$most.recent.contributor.name),]
tt<-tt[!grepl("fire fighter",tt$most.recent.contributor.name),]
tt<-tt[!grepl("locomotive engineers",tt$most.recent.contributor.name),]
tt<-tt[!grepl("firefighter",tt$most.recent.contributor.name),]
tt<-tt[!grepl("teamster",tt$most.recent.contributor.name),]
tt<-tt[!grepl("plumbers \\&",tt$most.recent.contributor.name),]
tt<-tt[!grepl("labor alliance",tt$most.recent.contributor.name),]
tt<-tt[!grepl("seiu",tt$most.recent.contributor.name),]
tt<-tt[!grepl("drive committee",tt$most.recent.contributor.name),]
tt<-tt[!grepl("presidential cmte",tt$most.recent.contributor.name),]
tt<-tt[!grepl("libertarian",tt$most.recent.contributor.name),]
tt<-tt[!grepl("indians",tt$most.recent.contributor.name),]
tt<-tt[!grepl("pistol asso",tt$most.recent.contributor.name),]
tt<-tt[!grepl("naacp",tt$most.recent.contributor.name),]
tt<-tt[!grepl("safari club",tt$most.recent.contributor.name),]
tt<-tt[!grepl("state trooper",tt$most.recent.contributor.name),]
tt<-tt[!grepl("troopers asso",tt$most.recent.contributor.name),]
tt<-tt[!grepl("troopers found",tt$most.recent.contributor.name),]
tt<-tt[!grepl("independence party",tt$most.recent.contributor.name),]
tt<-tt[!grepl("national women",tt$most.recent.contributor.name),]
tt<-tt[!grepl("conservative party",tt$most.recent.contributor.name),]
tt<-tt[!grepl("conservatives",tt$most.recent.contributor.name),]
tt<-tt[!grepl("longshore",tt$most.recent.contributor.name),]
tt<-tt[!grepl("volunteer",tt$most.recent.contributor.name),]
tt<-tt[!grepl("education asso",tt$most.recent.contributor.name),]
tt<-tt[!grepl("REPUBLICAN",tt$most.recent.contributor.name),]
tt<-tt[!grepl("american federation of state county and municipal employees",tt$most.recent.contributor.name),]
tt<-tt[-c(which(tt$most.recent.contributor.name=="blank")),]
tt<-tt[!grepl("american federation of teachers",tt$most.recent.contributor.name),]
tt<-tt[-c(which(tt$most.recent.contributor.name=="national foundation of women legislators inc")),]
tt<-tt[-c(setdiff(grep("teachers",tt$most.recent.contributor.name),
                  c(grep("excellence",tt$most.recent.contributor.name),
                    grep("insurance",tt$most.recent.contributor.name),
                    grep("title",tt$most.recent.contributor.name),
                    grep("tools",tt$most.recent.contributor.name)))),]

nn<-names(table(tt$most.recent.contributor.name[grep(" for ",tt$most.recent.contributor.name)]))
#additional pacs that are not from corporations or trade groups
no<-c("for city","for congress","for us senate","for district ","for commissioner","for county ","for governor",
      "for auditor","yes on","for state senate","for assemly","for senate","for council","for state house","for representative",
      "for new york","for texas","for wyoming","for missouri","working families"," gop ","republican","democrat",
      "for water board","for sheriff","for house ","for u s congress","for mayor","for da ","for supervisor",
      "for iowa","for dccc","for brooklyn","for vt ","for nh ","working famlies","for assembly","for govenor","for nc ",
      "for adrian"," for pa","for state ","for fresno","candidate","for attorney","for supreme","for school board",
      "for indiana","for u\\.s\\. congress","voters for ","citizens for","for south dako","for john corn",
      "for connecticut","for south caroli","for insurance commission","for president","for delegate",
      "for colorado","for senator ","for insurance commision","for treasurer","for georgia","for stan ","for montana",
      "for hospital boar","georgians for ","for the house","city council"," commissioner","for burlington","for lieutenant",
      "andrew cuomo for","for luther strange","taxpayers for","new yorkers for","montanans for","for legislature","for judge ",
      "for orange county","for one georgia","for maine","for louisiana","for d3 supervisor","klein for st","board atu",
      "for florida","friends for","for ma ","for judge","for lt governor","for freeholder","for oregon","for audrey",
      "for la council","for clerk","for california","national organization for women","alaskans for","for illinois","for house")
nn2<-c();for(i in 1:length(nn)){nn2[i]<-0;for(j in 1:length(no)){if(length(grep(no[j],nn[i]))>0){nn2[i]<-1};print(i)}}
#no candidate committees
tt<-tt[tt$most.recent.contributor.name%in%nn[nn2==1]==F,]
tt$state<-toupper(tt$state)
tt$most.recent.contributor.name[grep("afl",tt$most.recent.contributor.name)]
sort(table(tt$most.recent.contributor.name),decreasing = T)[1:100]
table(tt$most.recent.contributor.employer)
#remove most common professional associations
tt<-tt[!grepl("dental association",tt$most.recent.contributor.name)&
         !grepl("nurses association",tt$most.recent.contributor.name)&
         !grepl("dental hygienists",tt$most.recent.contributor.name)&
         !grepl("association of realtors",tt$most.recent.contributor.name)&
         !grepl("association of architects",tt$most.recent.contributor.name)&
         !grepl("association of health underwriters",tt$most.recent.contributor.name)&
         !grepl("society of proessional engineers",tt$most.recent.contributor.name)&
         !grepl("school administrators",tt$most.recent.contributor.name)&
         !grepl("state bar association",tt$most.recent.contributor.name)&
         !grepl("medical society",tt$most.recent.contributor.name)&
         !grepl("chiropractic association",tt$most.recent.contributor.name)&
         !grepl("osteopathic medical association",tt$most.recent.contributor.name)&
         !grepl("osteopathic medical society",tt$most.recent.contributor.name)&
         !grepl("veterinary medical association",tt$most.recent.contributor.name)&
         !grepl("society of anesthesiologist",tt$most.recent.contributor.name)&
         !grepl("speech language hearing",tt$most.recent.contributor.name)&
         !grepl("physicians association",tt$most.recent.contributor.name)&
         !grepl("optometrist association",tt$most.recent.contributor.name)&
         !grepl("association of optometrists",tt$most.recent.contributor.name)&
         !grepl("optometric association",tt$most.recent.contributor.name)&
         !grepl("optometric society",tt$most.recent.contributor.name)&
         !grepl("association of orthopaedic surgeons",tt$most.recent.contributor.name)&
         !grepl("surgeons professional association",tt$most.recent.contributor.name)&
         !grepl("society of eye physicians",tt$most.recent.contributor.name)&
         !grepl("association of american physicians",tt$most.recent.contributor.name),]
t10-nrow(tt)
#1224 obs removed
nrow(tt)
#137182 remaining sample
gc()
#do not need columns
tt<-tt[,-grep("amount",colnames(tt))]
gc();gc();gc();gc()

#get legislators in states
rec$state<-toupper(rec$state)
rec2<-rec[rec$cycle>2000,]
rec2<-rec2[rec2$winner=="W"&rec2$nimsp.candidate.status!="Withdrew",]
rec2<-rec2[rec2$seat=="state:lower"|rec2$seat=="state:upper"|rec2$seat=="state:senate"|rec2$seat=="state:house",]
#only take winning candidates..exclude challengers
#federal
rec3<-rec[rec$cycle>2000,]
rec3<-rec3[rec3$winner=="W"&rec3$nimsp.candidate.status!="Withdrew",]
rec3<-rec3[rec3$seat=="federal:house"|rec3$seat=="federal:senate",]

table(rec2$state)
#correct
rec2$state[rec2$state=="Pa"]<-"PA"
rec2$state[rec2$state=="PE"]<-"PA"
#get correct party id
ne_party$party<-"";for(i in 1:nrow(ne_party)){ne_party$party[i]<-substr(ne_party$name[i],gregexpr("/",ne_party$name[i])[[1]][1]+1,gregexpr("/",ne_party$name[i])[[1]][2]-1)}
ne_party$name[which(ne_party$name%in%setdiff(ne_party$name[grep("\\(NE/",ne_party$name)],c(ne_party$name[grep("_OTH",ne_party$name)],ne_party$name[grep("NP",ne_party$name)]))==T)]
ne_party$ICPSR2[which(ne_party$name%in%setdiff(ne_party$name[grep("\\(NE/",ne_party$name)],c(ne_party$name[grep("_OTH",ne_party$name)],ne_party$name[grep("NP",ne_party$name)]))==T&ne_party$ICPSR2=="")]
options(warn=2)
rec2$party2<-rec2$party;for(i in 1:nrow(rec2)){
  if(rec2$ICPSR2[i]=="21533"){rec2$party2[i]<-"R";next;}
  if(rec2$ICPSR2[i]=="NE6109"){rec2$party2[i]<-"R";next;}
  if(rec2$state[i]=="NE"){if(length(intersect(rec2$ICPSR2[i],ne_party$ICPSR2))==1&rec2$ICPSR2[i]!=""&is.na(rec2$ICPSR2[i])==F){rec2$party2[i]<-ne_party$party[which(ne_party$ICPSR2==rec2$ICPSR2[i]&!grepl("_OTH",ne_party$name)&!grepl("/NP",ne_party$name))]}}
}
rec2$party2[rec2$party2=="D"]<-"100"
rec2$party2[rec2$party2=="R"]<-"200"
rec2$party2[rec2$party2=="I"]<-"328"

tt<-tt[order(tt$am2,decreasing = T),]
#all contributions greater than 0
tt3<-tt[which(tt$am2>0),]
#contributions greater or equal to 100000 & at least 2 distinct contributions
tt4<-tt3[tt3$num.distinct>1&tt3$am2>=100000,]

repos<-data.frame(state=c("USall1","USall2","US-DC","INW",unique(rec2$state)),prop_mod=NA,stringsAsFactors = F)
#moderate in relation to all legislators (common baseline)
nms<-c("USall1","USall2","US-DC","INW",unique(rec2$state));for(i in 1:length(nms)){
  if(nms[i]!="USall1"&nms[i]!="USall2"&nms[i]!="US-DC"&nms[i]!="INW"){repos$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state==nms[i]]<median(na.rm=T,rec2$recipient.cfscore[rec2$party2=="200"])&tt3$contributor.cfscore[tt3$state==nms[i]]>median(na.rm=T,rec2$recipient.cfscore[rec2$party2=="100"])))/length(tt3$contributor.cfscore[tt3$state==nms[i]])}
  if(nms[i]=="USall1"){repos$prop_mod[i]<-length(which(tt4$contributor.cfscore<median(na.rm=T,rec3$recipient.cfscore[rec3$party=="200"])&tt4$contributor.cfscore>median(na.rm=T,rec3$recipient.cfscore[rec3$party=="100"])))/length(tt4$contributor.cfscore)}
  if(nms[i]=="USall2"){repos$prop_mod[i]<-length(which(tt3$contributor.cfscore<median(na.rm=T,rec3$recipient.cfscore[rec3$party=="200"])&tt3$contributor.cfscore>median(na.rm=T,rec3$recipient.cfscore[rec3$party=="100"])))/length(tt3$contributor.cfscore)}
  if(nms[i]=="US-DC"){repos$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state=="DC"]<median(na.rm=T,rec3$recipient.cfscore[rec3$party=="200"])&tt3$contributor.cfscore[tt3$state=="DC"]>median(na.rm=T,rec3$recipient.cfscore[rec3$party=="100"])))/length(tt3$contributor.cfscore[tt3$state=="DC"])}
  if(nms[i]=="INW"){repos$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"]<median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="200")])&tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"]>median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="100")])))/length(tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"])}
}

png(filename=paste(getwd(),"ST19_JOP_replication","prop_mod_cont.png",sep="/"),width=800,height=800)
par(mar=c(1,1,1,1),mai=c(1.25,1.25,1.25,1.25))
hist(cex.main=2,cex.lab=2,cex.axis=2,border="darkgray",xlab="Proportion of Moderate Contributors",main=NA, repos$prop_mod[repos$state!="USall1"&repos$state!="USall2"&repos$state!="US-DC"&repos$state!="INW"],col="gray")
abline(v=median(repos$prop_mod[repos$state!="USall1"&repos$state!="USall2"&repos$state!="US-DC"&repos$state!="INW"]),lwd=2)
abline(v=repos$prop_mod[repos$state=="USall1"],col="black")
abline(v=repos$prop_mod[repos$state=="USall2"],col="black",lty=2)
abline(v=repos$prop_mod[repos$state=="US-DC"],col="black",lty=3)
abline(v=repos$prop_mod[repos$state=="INW"],col="black",lty=4,lwd=2)
legend(cex=1.5,"topleft",legend=c("Median State","IA,NE,WI pooled","Congress 1","Congress 2","Congress 3"),
       lty=c(1,4,3,1,2),lwd=c(2,2,1,1,1))
dev.off()
#The distribution is unimodal and the median state’s proportion is 62%
median(repos$prop_mod[repos$state!="USall1"&repos$state!="USall2"&repos$state!="US-DC"&repos$state!="INW"])
#Pooling contributors across Iowa, Nebraska, and Wisconsin results in a very similar proportion
#of about 65%
repos$prop_mod[repos$state=="INW"]
#Further, more than half of all states have a proportion within twelve percentage points of 65%,
length(which(repos$prop_mod[5:nrow(repos)]>=0.53&repos$prop_mod[5:nrow(repos)]<=0.77))

repos$prop_mod[repos$state=="US-DC"] #68%
repos$prop_mod[repos$state=="USall1"] #84%
repos$prop_mod[repos$state=="USall2"] #73%

repos$prop_mod[repos$state=="IA"] #74%
repos$prop_mod[repos$state=="NE"] #64%
repos$prop_mod[repos$state=="WI"] #60%

#calculate state-specific baseline (fn 74)
repos2<-data.frame(state=c("USall","US-DC","INW",unique(rec2$state)),prop_mod=NA,stringsAsFactors = F)
#moderate in relation to state-specific baseline
nms<-c("USall","US-DC","INW",unique(rec2$state));for(i in 1:length(nms)){
  if(nms[i]!="USall"&nms[i]!="US-DC"){repos2$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state==nms[i]]<median(na.rm=T,rec2$recipient.cfscore[rec2$state==nms[i]&rec2$party=="200"])&tt3$contributor.cfscore[tt3$state==nms[i]]>median(na.rm=T,rec2$recipient.cfscore[rec2$state==nms[i]&rec2$party=="100"])))/length(tt3$contributor.cfscore[tt3$state==nms[i]])}
  if(nms[i]=="USall"){repos2$prop_mod[i]<-length(which(tt3$contributor.cfscore<median(na.rm=T,rec3$recipient.cfscore[rec3$party=="200"])&tt3$contributor.cfscore>median(na.rm=T,rec3$recipient.cfscore[rec3$party=="100"])))/length(tt3$contributor.cfscore)}
  if(nms[i]=="US-DC"){repos2$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state=="DC"]<median(na.rm=T,rec3$recipient.cfscore[rec3$party=="200"])&tt3$contributor.cfscore[tt3$state=="DC"]>median(na.rm=T,rec3$recipient.cfscore[rec3$party=="100"])))/length(tt3$contributor.cfscore[tt3$state=="DC"])}
  if(nms[i]=="NE"){repos2$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state==nms[i]]<median(na.rm=T,rec2$recipient.cfscore[rec2$state==nms[i]&rec2$party2=="200"])&tt3$contributor.cfscore[tt3$state==nms[i]]>median(na.rm=T,rec2$recipient.cfscore[rec2$state==nms[i]&rec2$party2=="100"])))/length(tt3$contributor.cfscore[tt3$state==nms[i]])}
  if(nms[i]=="INW"){repos2$prop_mod[i]<-length(which(tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"]<median(na.rm=T,rec2$recipient.cfscore[intersect(which(rec2$state=="NE"|rec2$state=="IA"|rec2$state=="WI"),which(rec2$party2=="200"))])&tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"]>median(na.rm=T,rec2$recipient.cfscore[intersect(which(rec2$state=="NE"|rec2$state=="IA"|rec2$state=="WI"),which(rec2$party2=="100"))])))/length(tt3$contributor.cfscore[tt3$state=="IA"|tt3$state=="NE"|tt3$state=="WI"])}
}
repos2$prop_mod[repos2$state=="IA"] #76%
repos2$prop_mod[repos2$state=="NE"] #33%
repos2$prop_mod[repos2$state=="WI"] #77%
median(repos2$prop_mod[repos2$state!="USall"&repos2$state!="US-DC"&repos2$state!="INW"]) #59%

############
# Figure 25
############

for(i in grep("e2[0-9][0-9][0-9]",colnames(smc_i))){
  smc_i[which(is.na(smc_i[,i])==T),i]<-0
}
#anybody, who was legislator between 2003 and 2016
smc_i$ino0316<-smc_i$senate2003+smc_i$senate2004+smc_i$senate2005+smc_i$senate2006+
  smc_i$senate2007+smc_i$senate2008+smc_i$senate2009+smc_i$senate2010+smc_i$senate2011+
  smc_i$senate2012+smc_i$senate2013+smc_i$senate2014+smc_i$senate2015+smc_i$senate2016+
  smc_i$house2003+smc_i$house2004+smc_i$house2005+smc_i$house2006+
  smc_i$house2007+smc_i$house2008+smc_i$house2009+smc_i$house2010+smc_i$house2011+
  smc_i$house2012+smc_i$house2013+smc_i$house2014+smc_i$house2015+smc_i$house2016

st<-data.frame(state=unique(smc_i$st),medR=NA,medD=NA,stringsAsFactors = F)
for(i in 1:nrow(st)){
  st$medD[i]<-median(rep(smc_i$np_score[(which(smc_i$st==st$state[i]&smc_i$party=="D"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st==st$state[i]&smc_i$party=="D"&smc_i$ino0316>0))]),na.rm=T)
  st$medR[i]<-median(rep(smc_i$np_score[(which(smc_i$st==st$state[i]&smc_i$party=="R"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st==st$state[i]&smc_i$party=="R"&smc_i$ino0316>0))]),na.rm=T)
}

png(filename=paste(getwd(),"ST19_JOP_replication","representative_legislators.png",sep="/"),width=1300,height=1300)
par(mfrow=c(2,2),mar=c(1,1,1,1),mai=c(1,1.25,1,0.1))
cols<-gray(1:8/8,alpha = 1)
##########
# Panel 1
##########
hist(cex.lab=2.5,cex.axis=2.5,cex.main=2.5,
     main="(1) Party Medians of Legislators\nin the States (NPAT scores)",
     st$medD,xlim=c(-2,2),col="gray",breaks=10,border="gray",ylim=c(0,23),
     xlab="NPAT Common Space Scores")
hist(st$medR,xlim=c(-2,2),border="black",breaks=10,add=T)
#median democratic median
abline(v=median(st$medD),lwd=3,lty=1) #-0.796
#median republican median
abline(col=cols[3],v=median(st$medR),lwd=3) #0.7835
#overall (across all states)
abline(lty=4,v=median(rep(smc_i$np_score[(which(smc_i$party=="D"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$party=="D"&smc_i$ino0316>0))]),na.rm=T),lwd=2)
#-0.82
abline(lty=4,col=cols[3],v=median(rep(smc_i$np_score[(which(smc_i$party=="R"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$party=="R"&smc_i$ino0316>0))]),na.rm=T),lwd=2)
#0.802
#median democrat across three states
abline(v=median(rep(smc_i$np_score[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="D"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="D"&smc_i$ino0316>0))]),na.rm=T),lty=3,lwd=3)
#-1.023
abline(col=cols[3],v=median(rep(smc_i$np_score[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="R"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="R"&smc_i$ino0316>0))]),na.rm=T),lwd=3,lty=3)
#0.714
#legend
legend(cex=2,"topleft",legend=c("Dem. State Medians","Rep. State Medians","Median Dem. State Median","Median Rep. State Median","Overall Dem. Median","Overall Rep. Median","Dem. Median (IA, NE, WI)","Rep. Median (IA, NE, WI)"),
       lty=c(NA,NA,1,1,4,4,3,3),seg.len=c(2.5),lwd=c(NA,NA,3,3,2,2,3,3),fill=c("gray","white",NA,NA,NA,NA,NA,NA),border=c("gray","black",NA,NA,NA,NA,NA,NA),col=c("gray","black","black",cols[3],"black",cols[3],"black",cols[3]))
##########
# Panel 2
##########
#show polarization across states
hist(cex.lab=2.5,cex.axis=2.5,cex.main=2.5,main="(2) Polarization in the States (NPAT scores)",
     st$medR-st$medD,col="gray",breaks=10,border="gray",ylim=c(0,13),
     xlab="NPAT Common Space Scale")
abline(v=median(st$medR-st$medD),lwd=3,lty=1) 
#median democrat across three states
abline(v=median(rep(smc_i$np_score[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="R"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="R"&smc_i$ino0316>0))]),na.rm=T)
       -median(rep(smc_i$np_score[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="D"&smc_i$ino0316>0))],times=smc_i$ino0316[(which(smc_i$st%in%c("IA","NE","WI")&smc_i$party=="D"&smc_i$ino0316>0))]),na.rm=T),lty=3,lwd=3)
#pooled across all legislatures
abline(v=median(rep(smc_i$np_score[which(smc_i$party=="R"&smc_i$ino0316>0)],times=smc_i$ino0316[which(smc_i$party=="R"&smc_i$ino0316>0)]),na.rm=T)
       -median(rep(smc_i$np_score[which(smc_i$party=="D"&smc_i$ino0316>0)],times=smc_i$ino0316[which(smc_i$party=="D"&smc_i$ino0316>0)]),na.rm=T),lty=2,lwd=3)
#legend
legend(cex=2,"topright",legend=c("Distance b/w Party Medians","Median Distance Across Legislatures","Distance (Pooling All Legislatures)", "Distance (Pooling IA, NE, WI"),
       lty=c(NA,1,2,3),seg.len=c(2.5),lwd=c(NA,3,3,3),fill=c("gray",NA,NA,NA),border=c("gray",NA,NA,NA),col=c("gray","black","black","black"))
###

repos<-data.frame(state=c("INW",unique(rec2$state)),medR=NA,medD=NA,stringsAsFactors = F)
#moderate in relation to all legislators (common baseline)
nms<-c("INW",unique(rec2$state));for(i in 1:length(nms)){
  if(nms[i]!="INW"){repos$medR[i]<-median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="200"&rec2$state==nms[i])])}
  if(nms[i]!="INW"){repos$medD[i]<-median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="100"&rec2$state==nms[i])])}
  if(nms[i]=="INW"){repos$medR[i]<-median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="200"&rec2$state%in%c("IA","NE","WI"))])}
  if(nms[i]=="INW"){repos$medD[i]<-median(na.rm=T,rec2$recipient.cfscore[which(rec2$party2=="100"&rec2$state%in%c("IA","NE","WI"))])}
}
##########
# Panel 3
##########
hist(cex.lab=2.5,cex.axis=2.5,cex.main=2.5,
     main="(3) Party Medians of Legislators\nin the States and in Congress (CFscores)",
     repos$medD[!grepl("INW",repos$state)],xlim=c(-2,2),col="gray",breaks=10,border="gray",ylim=c(0,23),
     xlab="Common Space CFscores Scores")
hist(repos$medR[!grepl("INW",repos$state)],xlim=c(-2,2),border="black",breaks=10,add=T)
#median democratic median
abline(v=median(repos$medD[!grepl("INW",repos$state)]),lwd=3,lty=1)
#median republican median
abline(col=cols[3],v=median(repos$medR[!grepl("INW",repos$state)]),lwd=3) 

#median democrat across three states
abline(v=median(rec2$recipient.cfscore[which(rec2$party2=="100"&rec2$state%in%c("IA","NE","WI"))],na.rm=T),lty=3,lwd=3)
abline(col=cols[3],v=median(rec2$recipient.cfscore[which(rec2$party2=="200"&rec2$state%in%c("IA","NE","WI"))],na.rm=T),lty=3,lwd=3)
#congress
abline(lty=2,v=median(rec3$recipient.cfscore[which(rec3$party=="100")],na.rm=T),lwd=2)
abline(lty=2,col=cols[3],v=median(rec3$recipient.cfscore[which(rec3$party=="200")],na.rm=T),lwd=2)
legend(cex=2,"topright",legend=c("Dem. State Medians","Rep. State Medians","Median Dem. State Median","Median Rep. State Median","Dem. Median (IA, NE, WI)","Rep. Median (IA, NE, WI)","Dem. Median (Congress)","Rep. Median (Congress)"),
       lty=c(NA,NA,1,1,3,3,2,2),seg.len=c(2.5),lwd=c(NA,NA,3,3,2,2,3,3),fill=c("gray","white",NA,NA,NA,NA,NA,NA),border=c("gray","black",NA,NA,NA,NA,NA,NA),col=c("gray","black","black",cols[3],"black",cols[3],"black",cols[3]))
##########
# Panel 4
##########
hist(cex.lab=2.5,cex.axis=2.5,cex.main=2.5,
     main="(4) Polarization in the States\nand in Congress (CFscores)",
     repos$medR[!grepl("INW",repos$state)]-repos$medD[!grepl("INW",repos$state)],xlim=c(0,3),col="gray",breaks=10,border="gray",ylim=c(0,23),
     xlab="Common Space CFscores Scores")
#median of differences across states
abline(v=median(repos$medR[!grepl("INW",repos$state)]-repos$medD[!grepl("INW",repos$state)]),lwd=3,lty=1) 

#median democrat across three states
abline(v=median(rec2$recipient.cfscore[which(rec2$party2=="200"&rec2$state%in%c("IA","NE","WI"))],na.rm=T)-median(rec2$recipient.cfscore[which(rec2$party2=="100"&rec2$state%in%c("IA","NE","WI"))],na.rm=T),lty=3,lwd=3)
#congress
abline(lty=2,v=median(rec3$recipient.cfscore[which(rec3$party=="200")],na.rm=T)-median(rec3$recipient.cfscore[which(rec3$party=="100")],na.rm=T),lwd=2)
#legend
legend(cex=2,"topright",legend=c("Distance b/w Party Medians","Median Distance Across Legislatures", "Distance (Pooling IA, NE, WI","Distance (Congress)"),
       lty=c(NA,1,3,2),seg.len=c(2.5),lwd=c(NA,3,3,3),fill=c("gray",NA,NA,NA),border=c("gray",NA,NA,NA),col=c("gray","black","black","black"))
dev.off()


################
# Figure 26
################

##get lobbying principals from NIMSP
lobp<-lobp[lobp$Year=="2016",]
#harmonize different spellings
lobp$Client[lobp$Client=="3M"]<-"3M COMPANY"
lobp$Client[lobp$Client=="3M COMPANIES & ITS AFFILIATES"]<-"3M COMPANY"
lobp$Client[lobp$Client=="3M CO"]<-"3M COMPANY"
lobp$Client[lobp$Client=="3M COMPANY FKA 3M TRAFFIC SAFETY & SECURITY DIVISION"]<-"3M COMPANY"
lobp$Client[lobp$Client=="3M HEALTH INFORMATION SYSTEMS INC FKA TREO SOLUTIONS LLC"]<-"3M COMPANY"
lobp$Client[lobp$Client=="3M TRAFFIC SAFETY & SECURITY DIVISION"]<-"3M COMPANY"
lobp$Client[lobp$Client=="ASTRAZENECA PHARMACEUTICALS, LP"]<-"ASTRAZENECA"
lobp$Client[lobp$Client=="BANK OF AMERICA C / O WENDY Y JAMISON SENIOR VICE PRESIDENT PUBLIC POLICY"]<-"BANK OF AMERICA"
lobp$Client[lobp$Client=="BANK OF AMERICA CORPORATION C / O WENDY U JAMISON SENIOR VICE PRESIDENT PUBLIC POLICY"]<-"BANK OF AMERICA"
lobp$Client[lobp$Client=="BAYER PROPERTIES LLC"]<-"BAYER PROPERTIES"
lobp$Client[lobp$Client=="BNSF RAILWAY CORPORATE HEADQUARTERS"]<-"BNSF RAILWAY"
lobp$Client[lobp$Client=="NFIB NATL FED OF IND BUSINESS"]<-"NATIONAL FEDERATION OF INDEPENDENT BUSINESS"
lobp$Client[lobp$Client=="CELGENE CORPORATION"]<-"CELGENE CORP"
lobp$Client[lobp$Client=="CHEMISTRY COUNCIL OF NJ / CELGENE CORP"]<-"CELGENE CORP"
lobp$Client[lobp$Client=="CELGENE PHARMACEUTICAL"]<-"CELGENE CORP"
lobp$Client[lobp$Client=="INTERNATIONAL PREMIUM CIGAR & PIPE RETAILERS"]<-"INTERNATIONAL PREMIUM CIGAR & PIPE RETAILERS ASSOCIATION"
lobp$Client[lobp$Client=="CIGAR ASSN. / AMERICA"]<-"CIGAR ASSOCIATION OF AMERICA"
lobp$Client[lobp$Client=="GENERAL CIGAR COMPANY, INC."]<-"GENERAL CIGAR COMPANY INC"
lobp$Client[lobp$Client=="GENERAL CIGAR COMPANY, INC"]<-"GENERAL CIGAR COMPANY INC"
lobp$Client[lobp$Client=="GENERAL CIGAR COMPANY"]<-"GENERAL CIGAR COMPANY INC"
lobp$Client[lobp$Client=="GENERAL CIGAR"]<-"GENERAL CIGAR COMPANY INC"
lobp$Client[lobp$Client=="CVS-HEALTH"]<-"CVS CAREMARK"
lobp$Client[lobp$Client=="APPLE INC C / O NIELSEN MURKSOME ET AL"]<-"APPLE INC"
lobp$Client[lobp$Client=="EXPRESS SCRIPTS HOLDING CO"]<-"EXPRESS SCRIPTS"
lobp$Client[lobp$Client=="FIRST DATA"]<-"FIRST DATA CORP"
lobp$Client[lobp$Client=="FIRST DATA GOVERNMENT SOLUTIONS, LP"]<-"FIRST DATA CORP"
lobp$Client[lobp$Client=="GENERAL MOTORS LLC"]<-"GENERAL MOTORS"
lobp$Client[lobp$Client=="GENERAL MOTORS LLC FORMERLY GENERAL MOTORS COMPANY"]<-"GENERAL MOTORS"
lobp$Client[lobp$Client=="BURTON-LIESE ON BEHALF OF IBM"]<-"IBM"
lobp$Client[lobp$Client=="INTEL SECURITY FKA MCAFEE"]<-"INTEL SECURITY PUBLIC SECTOR LLC"
lobp$Client[lobp$Client=="INTEL SECURITY PUBLIC SECTOR LLC FKA INTEL SECURITY INC"]<-"INTEL SECURITY PUBLIC SECTOR LLC"
lobp$Client[lobp$Client=="JPMORGAN CHASE HOLDINGS LLC"]<-"JP MORGAN CHASE"
lobp$Client[lobp$Client=="JPMORGAN CHASE BANK, NA"]<-"JP MORGAN CHASE"
lobp$Client[lobp$Client=="JPMORGAN, CHASE & CO"]<-"JP MORGAN CHASE"
lobp$Client[lobp$Client=="KINDER, MORGAN"]<-"KINDER MORGAN"
lobp$Client[lobp$Client=="MAGELLAN MID-STREAM PARTNERS L.P."]<-"MAGELLAN MIDSTREAM PARTNERS LP"
lobp$Client[lobp$Client=="MAGELLAN MID-STREAM PARTNERS L P"]<-"MAGELLAN MIDSTREAM PARTNERS LP"
lobp$Client[lobp$Client=="MAGELLAN MID-STREAM PARTNERS, LP"]<-"MAGELLAN MIDSTREAM PARTNERS LP"
lobp$Client[lobp$Client=="MAGELLAN MID-STREAM PARTNERS, L.P."]<-"MAGELLAN MIDSTREAM PARTNERS LP"
lobp$Client[lobp$Client=="MAGELLAN MID-STREAM PARTNERS"]<-"MAGELLAN MIDSTREAM PARTNERS LP"
lobp$Client[lobp$Client=="MERCK SHARP & DOHME CORPORATION"]<-"MERCK SHARP & DOHME CORP"
lobp$Client[lobp$Client=="MERCK SHARP & DOHME CORPORATION & AFFILIATES"]<-"MERCK SHARP & DOHME CORP"
lobp$Client[lobp$Client=="RECREATION VEHICLE INDUSTRY ASSOCIATION"]<-"RECREATIONAL VEHICLE INDUSTRY ASSOCIATION"

lobp2<-lobp[lobp$Active.Contributor.=="yes",]

lobp2<-lobp2[!grepl("AFL-CIO",lobp2$Client),]
lobp2<-lobp2[!grepl("LOCAL [0-9]",lobp2$Client),]
lobp2<-lobp2[!grepl("LOCAL UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("AARP",lobp2$Client),]
lobp2<-lobp2[!grepl("ACLU",lobp2$Client),]
lobp2<-lobp2[!grepl("NURSES",lobp2$Client),]
lobp2<-lobp2[!grepl("CITY OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("AFSCME",lobp2$Client),]
lobp2<-lobp2[!grepl("SIERRA CLUB",lobp2$Client),]
lobp2<-lobp2[!grepl("PLANNED PARENTHOOD",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY AUDITORS",lobp2$Client),]
lobp2<-lobp2[!grepl("LEUKEMIA & LYMPHOMA SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("TEAMSTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL BOARDS ASS",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATION ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("DENTAL ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("FIRE FIGH",lobp2$Client),]
lobp2<-lobp2[!grepl("FIRE CHIEF",lobp2$Client),]
lobp2<-lobp2[!grepl("POLICE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("POLICE FRATERNAL ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("FRATERNAL ORDER OF POLICE",lobp2$Client),]
lobp2<-lobp2[!grepl("POLICE OFFICERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("POLICE CHIEFS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("POLICE",lobp2$Client),]
lobp2<-lobp2[!grepl("NAACP",lobp2$Client),]
lobp2<-lobp2[!grepl("TROOPER",lobp2$Client),]
lobp2<-lobp2[!grepl("SAFARI CLUB",lobp2$Client),]
lobp2<-lobp2[!grepl("SEIU",lobp2$Client),]
lobp2<-lobp2[!grepl("OF PLUMBERS",lobp2$Client),]
lobp2<-lobp2[!grepl("PISTOL",lobp2$Client),]
lobp2<-lobp2[!grepl("LABORERS",lobp2$Client),]
lobp2<-lobp2[!grepl("LOCOMOTIVE ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF PROFESSIONAL ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("OPERATING ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("ENGINEERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PROFESSIONAL ENGINEERS OF",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY ENGINEERS ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF CIVIL ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("BOARD OF ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("ORGANIZATION OF STATE ENGINEERS",lobp2$Client),]
lobp2<-lobp2[!grepl("ENGINEERS COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("ENGINEERS' BENEFICIAL",lobp2$Client),]
lobp2<-lobp2[!grepl("ENGINEERS IN CALIFORNIA",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNCIL OF CARPENTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNCIL OF ELECTRICAL WOR",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNCIL OF CHURCES",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNCIL OF BRICKLAY",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNCIL OF CHURCES",lobp2$Client),]
lobp2<-lobp2[!grepl("DISTRICT COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("LEAGUE OF MUNICIPALITIES",lobp2$Client),]
lobp2<-lobp2[!grepl("FEDERATION OF TEACHERS",lobp2$Client),]
lobp2<-lobp2[!grepl("TEACHERS UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICANS FOR PROSPERITY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN LUNG ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("EVERYTOWN FOR GUN SAFETY",lobp2$Client),]
lobp2<-lobp2[!grepl("NATURE CONSERVANCY",lobp2$Client),]
lobp2<-lobp2[!grepl("COLLEGE BOARD",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN CANCER SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("PEW CHARITABLE TRUSTS",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITED TRANSPORTATION UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN SOCIETY FOR THE PREVENTION OF CRUELTY TO ANIMALS",lobp2$Client),]
lobp2<-lobp2[!grepl("SAVE THE CHILDREN",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSUMERS UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("EXCELLENCE IN EDUCATION NATIONAL",lobp2$Client),]
lobp2<-lobp2[!grepl("TEACH FOR AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN DIABETES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("INNOCENCE PROJECT",lobp2$Client),]
lobp2<-lobp2[!grepl("INSTITUTE FOR JUSTICE",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN RED CROSS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOUTHERN POVERTY LAW CENTER",lobp2$Client),]
lobp2<-lobp2[!grepl("BRENNAN CENTER FOR JUSTICE",lobp2$Client),]
lobp2<-lobp2[!grepl("AUTISM SPEAKS",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMAN RIGHTS CAMPAIGN",lobp2$Client),]
lobp2<-lobp2[!grepl("CONVENTION OF STATES ACTION",lobp2$Client),]
lobp2<-lobp2[!grepl("MEDICAL ASSOCIATION OF THE STATE OF ALABAMA",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN FEDERATION FOR CHILDREN",lobp2$Client),]
lobp2<-lobp2[!grepl("ALABAMA OCCUPATIONAL THERAPY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN INSTITUTE OF ARCHITECTS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICANS UNITED FOR SEPARATION OF CHURCH & STATE",lobp2$Client),]
lobp2<-lobp2[!grepl("STUDENTSFIRST",lobp2$Client),]
lobp2<-lobp2[!grepl("POARCH BAND OF CREEK INDIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("CITIZENS FOR A BETTER ALABAMA",lobp2$Client),]
lobp2<-lobp2[!grepl("PODIATRIC MEDICAL ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE EMPLOYEES ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("HOME SCHOOL LEGAL DEFENSE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSERVATION ALABAMA ACTION FUND",lobp2$Client),]
lobp2<-lobp2[!grepl("CAMPAIGN FOR THE FAIR SENTENCING OF YOUTH",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNITY MENTAL HEALTH BOARDS",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATION RETIREES ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("PHYSICAL THERAPY ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("CHIROPRACTIC ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("PILOTS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ALABAMA CIVIL JUSTICE REFORM COMMITTEE",lobp2$Client),]
lobp2<-lobp2[!grepl("STAND FOR CHILDREN",lobp2$Client),]
lobp2<-lobp2[!grepl("UNION OF CONCERNED SCIENTISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("DUCKS UNLIMITED",lobp2$Client),]
lobp2<-lobp2[!grepl("BROTHERHOOD OF MAINTENANCE OF WAY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN TORT REFORM ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ALABAMA STATE UNIVERSITY",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL ASSOCIATION OF SHEET METAL AIR RAIL",lobp2$Client),]
lobp2<-lobp2[!grepl("FOOD & WATER WATCH",lobp2$Client),]
lobp2<-lobp2[!grepl("CYSTIC FIBROSIS FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("BEST FRIENDS ANIMAL SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("MARSY\\'S LAW FOR ALL",lobp2$Client),]
lobp2<-lobp2[!grepl("MOTHERS AGAINST DRUNK DRIVING / MAAD",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL UNION OF ELEVATOR CONSTRUCTORS",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSERVATION LAW FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("CONCERNED WOMEN FOR AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("AIDS HEALTHCARE FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("THE TRUST FOR PUBLIC LAND",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN SOCIETY OF INTERIOR DESIGNERS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN ASSOCIATION OF UNIVERSITY WOMEN",lobp2$Client),]
lobp2<-lobp2[!grepl("AGUDATH ISRAEL OF AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("ELECTRICAL WORKERS UNIDENTIFIED",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATION REFORM NOW",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSERVATION CAMPAIGN",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF PEDIATRIC",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICA VOTES",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITE HERE! UNIDENTIFIED",lobp2$Client),]
lobp2<-lobp2[!grepl("A BROTHERHOOD AIMED TOWARD EDUCATION",lobp2$Client),]
lobp2<-lobp2[!grepl("SMART-TRANSPORTATION DIVISION",lobp2$Client),]
lobp2<-lobp2[!grepl("PARTNERSHIP TO FIGHT CHRONIC DISEASE",lobp2$Client),]
lobp2<-lobp2[!grepl("EQUAL JUSTICE USA",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITED MINE WORKERS OF AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITE HERE",lobp2$Client),]
lobp2<-lobp2[!grepl("STUDENTSFIRST",lobp2$Client),]
lobp2<-lobp2[!grepl("STEELWORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("TROUT UNLIMITED",lobp2$Client),]
lobp2<-lobp2[!grepl("RECREATIONAL FISHING ALLIANCE",lobp2$Client),]
lobp2<-lobp2[!grepl("PET FOOD INSTITUTE",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF RETIRED TEACHERS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN SOCIETY OF COMPOSERS AUTHORS & PUBLISHERS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN ASSOCIATION UNIVERSITY PROFESSORS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN CONGRESS OF OBSTETRICIANS & GYNECOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN ACADEMY OF OPHTHALMOLOGY",lobp2$Client),]
lobp2<-lobp2[!grepl("ALLIANCE FOR CHILDHOOD EDUCATION",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF CPA",lobp2$Client),]
lobp2<-lobp2[!grepl("CONFERENCE OF MUNICIPALITIES",lobp2$Client),]
lobp2<-lobp2[!grepl("CATHOLIC HOSPITALS",lobp2$Client),]
lobp2<-lobp2[!grepl("COLLEGE OF EMERGENCY PHYS",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC SCHOOL SUPERINTE",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF OPTOME",lobp2$Client),]
lobp2<-lobp2[!grepl("AGAINST GUN VIOLENCE",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNICATIONS WORKERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY OFFICIALS",lobp2$Client),]
lobp2<-lobp2[!grepl("EMPLOYEES UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("MEDICAL ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF NONPROFIT",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF NURSE",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE COUNCIL OF SERVICE EMPLOY",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE MEDICAL SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("TRIAL LAWYERS ASSOC",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNITY COLLEGE",lobp2$Client),]
lobp2<-lobp2[!grepl("DEMOS",lobp2$Client),]
lobp2<-lobp2[!grepl("FREEDOM FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("FRIEDMAN FOUNDATION FOR EDUCATIONAL CHOICE",lobp2$Client),]
lobp2<-lobp2[!grepl("FRIENDS OF THE EARTH",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY POTAWATOMI",lobp2$Client),]
lobp2<-lobp2[!grepl("FOUNDATION FOR INDIVIDUAL RIGHTS IN EDUCATION",lobp2$Client),]
lobp2<-lobp2[!grepl("FREEDOM FOR ALL AMERICANS",lobp2$Client),]
lobp2<-lobp2[!grepl("FOUNDATION FOR GOVERNMENT ACCOUNTABILITY",lobp2$Client),]
lobp2<-lobp2[!grepl("FORWARD TOGETHER",lobp2$Client),]
lobp2<-lobp2[!grepl("PSYCHOLOGICAL ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("MOTORCYCLE RIDERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("MAINTENANCE OF WAY EMP",lobp2$Client),]
lobp2<-lobp2[!grepl("KNIGHTS OF COLUMBUS",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL LONGSHORE",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL FUND FOR ANIMAL WELFARE",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL ASSOCIATION OF BRIDGE STRUCTURAL ORNAMENTAL & REINFORCING IRON WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("COURT REPORTERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PEOPLE FOR THE AMERICAN WAY",lobp2$Client),]
lobp2<-lobp2[!grepl("PEOPLE FOR THE ETHICAL TREATMENT OF ANIMALS",lobp2$Client),]
lobp2<-lobp2[!grepl("TRIBE OF INDIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("NATIONAL TRUST FOR HISTORIC PRESERVATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NATIONAL WILDLIFE FEDERATION",lobp2$Client),]
lobp2<-lobp2[!grepl("MOVEON.ORG",lobp2$Client),]
lobp2<-lobp2[!grepl("MYWIRELESS.ORG",lobp2$Client),]
lobp2<-lobp2[!grepl("SHEET METAL AIR RAIL & TRANSPORTATION WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SIMON WIESENTHAL CENTER",lobp2$Client),]
lobp2<-lobp2[!grepl("SAVE OUR SOCIETY FROM DRUGS",lobp2$Client),]
lobp2<-lobp2[!grepl("REASON FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC CITIZEN",lobp2$Client),]
lobp2<-lobp2[!grepl("PSYCHIATRIC ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("WILDLIFE FEDERATION",lobp2$Client),]
lobp2<-lobp2[!grepl("RIGHT TO LIFE",lobp2$Client),]
lobp2<-lobp2[!grepl("RIGHT TO WORK",lobp2$Client),]
lobp2<-lobp2[!grepl("ELECTRICAL WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF CRIMINAL DEFENSE LAWYERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF ANESTHESIOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF PATHOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF SOCIAL WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("FUNERAL DIRECTORS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl(" BAR ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF ASSESSING OFFICERS",lobp2$Client),]
lobp2<-lobp2[!grepl("1000 FRIENDS OF",lobp2$Client),]
lobp2<-lobp2[!grepl("WORKING FAMILIES PARTY",lobp2$Client),]
lobp2<-lobp2[!grepl("WILDLIFE CONSERVATION SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("ABATE OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("AAA ",lobp2$Client),]
lobp2<-lobp2[!grepl("AAUW OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("OPTOMETRIC ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC SCHOOLS",lobp2$Client),]
lobp2<-lobp2[!grepl("ALASKA WOMENS LOBBY",lobp2$Client),]
lobp2<-lobp2[!grepl("ORTHOPEDIC SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF EYE PHYSICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("A STATE FAIR",lobp2$Client),]
lobp2<-lobp2[!grepl("I STATE FAIR",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC EMPLOYEES ASSOCATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PHARMACISTS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("DOMESTIC VIOLENCE",lobp2$Client),]
lobp2<-lobp2[!grepl("OPTOMETRIC ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("CHIROPRACTIC SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNITY SCHOOLS",lobp2$Client),]
lobp2<-lobp2[!grepl("SPECTRUM HEALTH SYSTEMS",lobp2$Client),]
lobp2<-lobp2[!grepl("SPECIAL OLYMPICS",lobp2$Client),]
lobp2<-lobp2[!grepl("SPECIAL DISTRICT ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF GOVERNMENTS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOUTHERN EDUCATION FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("WATER CONSERVANCY DISTRICT",lobp2$Client),]
lobp2<-lobp2[!grepl("SOUTHERN ALLIANCE FOR CLEAN ENERGY",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE UNIVERSITY",lobp2$Client),]
lobp2<-lobp2[!grepl("AUTHORITY",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMANE SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMANE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("LAW ENFORCEMENT OFFICERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL DISTRICT",lobp2$Client),]
lobp2<-lobp2[!grepl("HIGH SCHOOL DISTRICTS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY COMMISSIONERS",lobp2$Client),]
lobp2<-lobp2[!grepl("OF PHYSICIAN ASSISTANTS",lobp2$Client),]
lobp2<-lobp2[!grepl("OF OPHTHALMOLOGY",lobp2$Client),]
lobp2<-lobp2[!grepl("OF PROFESSIONAL LAND SURVEYORS",lobp2$Client),]
lobp2<-lobp2[!grepl("DEFENSE TRIAL ATTORNEY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSERVATION LEAGUE",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF ADDICTION COUNSELORS",lobp2$Client),]
lobp2<-lobp2[!grepl(" BAND OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("SERVICE EMPLOYEES ",lobp2$Client),]
lobp2<-lobp2[!grepl("SHEET METAL ",lobp2$Client),]
lobp2<-lobp2[!grepl("CITY OF",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY EMPLOYEES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY OFFICE OF EDUCATION",lobp2$Client),]
lobp2<-lobp2[!grepl(" AIDS FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC UTILITIES COMMISSION",lobp2$Client),]
lobp2<-lobp2[!grepl(" INDIAN NATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF CPAS",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF LOUISIANA CPA",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC UTILITY DIST",lobp2$Client),]
lobp2<-lobp2[!grepl("RIVERSIDE HEALTH SYSTEM",lobp2$Client),]
lobp2<-lobp2[!grepl("BROTHERHOOD OF CORRECTIONAL OFFICERS",lobp2$Client),]
lobp2<-lobp2[!grepl("COALITION FOR THE HOMELESS",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL PRINCIPALS",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY DEPUTY SHERIFF",lobp2$Client),]
lobp2<-lobp2[!grepl("CORRECTIONS EMPLOYEES",lobp2$Client),]
lobp2<-lobp2[!grepl("CORRECTIONS EMPLOYEES",lobp2$Client),]
lobp2<-lobp2[!grepl(" SCHOOL ADMINISTRATORS",lobp2$Client),]
lobp2<-lobp2[!grepl(" STATE SUPERVISORS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOC STUDENTS OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSN OF WA HOUSING AUTHORITIES",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSN OF WA ST PUBLIC FACILITIES DISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ARLINGTON COUNTY",lobp2$Client),]
lobp2<-lobp2[!grepl("ADIRONDACK COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("ACUPUNCTURE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ACUPUNCTURE SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN CIVIL LIBERTIES UNION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN COLLEGE ",lobp2$Client),]
lobp2<-lobp2[!grepl("COLLEGE OF CARDIOLOGY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN HEART ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN JEWISH ARCHIVES",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN MUSEUM OF NATURAL HISTORY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN MASSAGE THERAPY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN PHYSICAL SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN PLANNING ASS",lobp2$Client),]
lobp2<-lobp2[!grepl("INSITUTE OF ARCHITECTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ALZHEIMERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("AMALGAMATED TRANSIT UNION ",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN ACADEMY OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("CARPENTERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("LEAGUE OF CONSERVATION VOTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("JUDGES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PROFESSIONAL CONSTRUCTION INSPECTORS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF SCHOOL BUSINESS OFFICIALS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF SCHOOL PSYCHOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF RECREATION & PARK DISTRICTS",lobp2$Client),]
lobp2<-lobp2[!grepl("MARRIAGE & FAMILY THERAP",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF PROFESSIONAL EMPLOYEES",lobp2$Client),]
lobp2<-lobp2[!grepl("OF PROFESSIONAL SCIENTISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF PSYCHIATRIC TECHNICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSESSORS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("BOYS & GIRLS CLUBS ",lobp2$Client),]
lobp2<-lobp2[!grepl("BOY SCOUTS OF AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("BIG BROTHERS BIG SISTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("AUTO WORKERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("AUDUBON ",lobp2$Client),]
lobp2<-lobp2[!grepl("DISABILITY RIGHTS ",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF ADMINISTRATION",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF STATE",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF ADMINISTRATIVE SERVICES",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF CHILDREN & FAMILIES",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF HEALTH",lobp2$Client),]
lobp2<-lobp2[!grepl("DEPARTMENT OF STATE",lobp2$Client),]
lobp2<-lobp2[!grepl("DELTA WATERFOWL ",lobp2$Client),]
lobp2<-lobp2[!grepl("OF SCHOOL ADMINISTRATORS",lobp2$Client),]
lobp2<-lobp2[!grepl("MEDICAL SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl(" BOWHUNTERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("INVEST IN EDUCATION COALITION",lobp2$Client),]
lobp2<-lobp2[!grepl(" ASSOCIATION FOR JUSTICE",lobp2$Client),]
lobp2<-lobp2[!grepl(" ACADEMY OF TRIAL LAWYERS",lobp2$Client),]
lobp2<-lobp2[!grepl(" ACADEMY OF FAMILY PHYSICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL ORGANIZATION OF MASTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("INTERNATIONAL UNION OF PAINTERS",lobp2$Client),]
lobp2<-lobp2[!grepl("INLANDBOATMEN",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE BUILDING & CONSTRUCTION TRADES COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl(" CPA SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY FOR ADVANCED PRACTICE NURSING",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY FOR RESPIRATORY CARE",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF ORTHOTISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("AGAINST HANDGUN VIOLENCE",lobp2$Client),]
lobp2<-lobp2[!grepl(" CITIZENS FOR LIFE",lobp2$Client),]
lobp2<-lobp2[!grepl(" COLLEGE OF OPTOMETRY",lobp2$Client),]
lobp2<-lobp2[!grepl("ILLINOIS CAMPAIGN FOR POLITICAL REFORM",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL SOCIAL WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF MINORITIES IN GOVERNMENT",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF AUDIOLOGY",lobp2$Client),]
lobp2<-lobp2[!grepl(" ACUPUNTURE FEDERATION",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY CLERKS ",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC EMPLOYEES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("OPTOMETRIC PHYSICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMANE OREGON PAC",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMAN RIGHTS WATCH",lobp2$Client),]
lobp2<-lobp2[!grepl("HUMAN LIFE OF WA",lobp2$Client),]
lobp2<-lobp2[!grepl(" INDIAN ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("HILLSBOROUGH COUNTY",lobp2$Client),]
lobp2<-lobp2[!grepl("HEALTH OFFICERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PHTHALMOLOGICAL SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("GUN OWNERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("GUN SENSE ",lobp2$Client),]
lobp2<-lobp2[!grepl("GUN VIOLENCE PREVENTION PAC",lobp2$Client),]
lobp2<-lobp2[!grepl("CATHOLIC CONFERENCE",lobp2$Client),]
lobp2<-lobp2[!grepl("DENTAL HYGIENTISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF AUDIOLOGY",lobp2$Client),]
lobp2<-lobp2[!grepl("S LAW FOR ALL",lobp2$Client),]
lobp2<-lobp2[!grepl("MARCH OF DIMES",lobp2$Client),]
lobp2<-lobp2[!grepl(" SHERIFFS ",lobp2$Client),]
lobp2<-lobp2[!grepl("DISTRICT ATTORNEYS ASS",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF MEDICAL PSYCHOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF FAMILY PHYSICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY LIFEGUARD ",lobp2$Client),]
lobp2<-lobp2[!grepl(" COUNTY REPUBLICAN ",lobp2$Client),]
lobp2<-lobp2[!grepl("LONG ISLAND PROGRESSIVE COALITION",lobp2$Client),]
lobp2<-lobp2[!grepl("LEAGUE OF WOMEN VOTERS",lobp2$Client),]
lobp2<-lobp2[!grepl(" OF HEALTH UNDERWRITERS",lobp2$Client),]
lobp2<-lobp2[!grepl("SPEECH",lobp2$Client),]
lobp2<-lobp2[!grepl("YMCA",lobp2$Client),]
lobp2<-lobp2[!grepl("WISCONSIN VOICES",lobp2$Client),]
lobp2<-lobp2[!grepl("WOMEN'S FUND OF GREATER OMAHA, INC.",lobp2$Client),]
lobp2<-lobp2[!grepl("WISCONSIN PHYSICIANS SERVICE INSURANCE",lobp2$Client),]
lobp2<-lobp2[!grepl(" INSTITUTE OF CPAS",lobp2$Client),]
lobp2<-lobp2[!grepl(" GUN OWNERS",lobp2$Client),]
lobp2<-lobp2[!grepl("WISCONSIN DEMOCRACY CAMPAIGN",lobp2$Client),]
lobp2<-lobp2[!grepl(" COUNTIES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("DENTAL HYGIENISTS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("BEAR HUNTERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("WINNEBAGO TRIBE OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("VOICES FOR CHILDREN IN NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("VOCES DE LA FRONTERA",lobp2$Client),]
lobp2<-lobp2[!grepl("WATER COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("UNO CHAPTER OF THE AAUP",lobp2$Client),]
lobp2<-lobp2[!grepl("UNIVERSITY OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("THEDACARE",lobp2$Client),]
lobp2<-lobp2[!grepl("THRIVENT FINANCIAL FOR LUTHERANS",lobp2$Client),]
lobp2<-lobp2[!grepl("TEAM IOWA PAC",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE BAR OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("STOCKBRIDGE MUNSEE COMMUNITY",lobp2$Client),]
lobp2<-lobp2[!grepl("FEDERATION OF LABOR",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC POWER DISTRICT",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL ADMINISTRATORS OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("BOARD OF COMMISSIONERS",lobp2$Client),]
lobp2<-lobp2[!grepl("NATION OF THE ",lobp2$Client),]
lobp2<-lobp2[!grepl("RIVER ALLIANCE OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("RESEARCH NEBRASKA INC",lobp2$Client),]
lobp2<-lobp2[!grepl("RACCOON RIVER WATERSHED ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("PROJECT EXTRA MILE",lobp2$Client),]
lobp2<-lobp2[!grepl("PRO-LIFE ",lobp2$Client),]
lobp2<-lobp2[!grepl("PROFESSIONAL ENGINEERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("PLATTE INSTITUTE FOR ECONOMIC RESEARCH",lobp2$Client),]
lobp2<-lobp2[!grepl("PHARMACY SOCIETY OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("NATURAL RESOURCES DISTRICT",lobp2$Client),]
lobp2<-lobp2[!grepl("PAPIO VALLEY PRESERVATION ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("OFFICE OF CONSUMER ADVOCATE",lobp2$Client),]
lobp2<-lobp2[!grepl("PUBLIC POWER DISTRICT",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKANS FOR WORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("STATE HISTORICAL SOCIETY ",lobp2$Client),]
lobp2<-lobp2[!grepl(" SHERIFFS",lobp2$Client),]
lobp2<-lobp2[!grepl("RADIOLOGIC TECHNO",lobp2$Client),]
lobp2<-lobp2[!grepl("ATHLETIC TRAINERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("REGIONAL OFFICIALS COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("LIBRARY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("MUNICIPAL POWER POOL",lobp2$Client),]
lobp2<-lobp2[!grepl("4 MEDICAL CANNABIS",lobp2$Client),]
lobp2<-lobp2[!grepl("DENTAL HYGIENIST",lobp2$Client),]
lobp2<-lobp2[!grepl("DENTAL ASSISTANT",lobp2$Client),]
lobp2<-lobp2[!grepl("CRIMINAL DEFENSE ATTORNE",lobp2$Client),]
lobp2<-lobp2[!grepl("EMERGENCY MEDICAL SERVICES ASS",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY COURT ASSOC",lobp2$Client),]
lobp2<-lobp2[!grepl("COUNTY ATTORNEYS",lobp2$Client),]
lobp2<-lobp2[!grepl("CHIROPRACTIC PHYSICIANS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF TRIAL ATTOR",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF SCHOOL BOARDS",lobp2$Client),]
lobp2<-lobp2[!grepl("FORMER STATE LEGISLATORS",lobp2$Client),]
lobp2<-lobp2[!grepl("REGIONAL ADMINISTRATORS",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA APPLESEED",lobp2$Client),]
lobp2<-lobp2[!grepl("AIRPORT OFFICIALS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF AREA AGENCIES ON AGING",lobp2$Client),]
lobp2<-lobp2[!grepl("NATURIST ACTION COMMITTEE",lobp2$Client),]
lobp2<-lobp2[!grepl("ACADEMY OF EYE PHY",lobp2$Client),]
lobp2<-lobp2[!grepl("ORIENTAL MEDICIN",lobp2$Client),]
lobp2<-lobp2[!grepl("MILWAUKEE COUNTY",lobp2$Client),]
lobp2<-lobp2[!grepl("MILWAUKEE AREA TECHNICAL COLLEGE",lobp2$Client),]
lobp2<-lobp2[!grepl("MENOMINEE INDIAN TRIBE OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("PRACTICAL NURSE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("LEAGUE OF NEBRASKA MUNICIPALITIES",lobp2$Client),]
lobp2<-lobp2[!grepl("LEGAL ACTION OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("LEGAL AID OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("LEAGUE OF HUMAN DIGNITY",lobp2$Client),]
lobp2<-lobp2[!grepl("LEARNING COMMUNITY OF DOUGLAS & SARPY COUNTIES",lobp2$Client),]
lobp2<-lobp2[!grepl("LANCASTER COUNTY BOARD OF COMMISSIONERS",lobp2$Client),]
lobp2<-lobp2[!grepl("PSYCHIATRIC SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("NATURAL HERITAGE FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl(" PHARMACY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ENTAL HYGIENISTS ASSO",lobp2$Client),]
lobp2<-lobp2[!grepl("IOWA ENVIRONMENTAL COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("IOWA CITIZENS FOR COMMUNITY IMPROVEMENT",lobp2$Client),]
lobp2<-lobp2[!grepl("ATHLETIC TRAINERS ",lobp2$Client),]
lobp2<-lobp2[!grepl("GAMBLING WITH THE GOOD LIFE",lobp2$Client),]
lobp2<-lobp2[!grepl("CITIZENS FOR A SCENIC WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("CITIZEN ACTION OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMON CAUSE ",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNITY ADVOCATES",lobp2$Client),]
lobp2<-lobp2[!grepl("COMPASSION & CHOICES",lobp2$Client),]
lobp2<-lobp2[!grepl("CONSERVATION DISTRICTS OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("DANE COUNTY",lobp2$Client),]
lobp2<-lobp2[!grepl("DOUGLAS COUNTY NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATION SERVICE UNIT",lobp2$Client),]
lobp2<-lobp2[!grepl("EASTERN NEBRASKA HUMAN SERVICES AGENCY",lobp2$Client),]
lobp2<-lobp2[!grepl("FAIR WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("FRIENDS OF NEBRASKA PARKS",lobp2$Client),]
lobp2<-lobp2[!grepl("FRIENDS OF PUBLIC HEALTH IN NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("HEALTH CENTER ASSOCIATION OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("HEALTH, BRYAN",lobp2$Client),]
lobp2<-lobp2[!grepl("HOUSING POLICY NETWORK",lobp2$Client),]
lobp2<-lobp2[!grepl("IOWA ASSOCIATION OF INDEPENDENT COLLEGES & UNIVERSITIES",lobp2$Client),]
lobp2<-lobp2[!grepl("LEADINGAGE",lobp2$Client),]
lobp2<-lobp2[!grepl("MADONNA REHABILITATION HOSPITAL",lobp2$Client),]
lobp2<-lobp2[!grepl("MARQUETTE UNIVERSITY",lobp2$Client),]
lobp2<-lobp2[!grepl("MAYO CLINIC HEALTH SYSTEM",lobp2$Client),]
lobp2<-lobp2[!grepl("METROPOLITAN UTILITIES DISTRICT ",lobp2$Client),]
lobp2<-lobp2[!grepl("METROPOLITAN AREA PLANNING AGENCY",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA ASSOCIATION FOR THE GIFTED",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA CULTURAL ENDOWMENT",lobp2$Client),]
lobp2<-lobp2[!grepl("FIREARMS OWNERS ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA METHODIST HEALTH SYSTEM",lobp2$Client),]
lobp2<-lobp2[!grepl("SCHOOL ACTIVITIES ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA RURAL ELECTRIC ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA WATER",lobp2$Client),]
lobp2<-lobp2[!grepl("THE FAMILY LEADER",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITED CITIES OF SARPY COUNTY",lobp2$Client),]
lobp2<-lobp2[!grepl("STEELWORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("WHITETAILS OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("WELLMARK BLUE CROSS BLUE SHIELD OF IOWA",lobp2$Client),]
lobp2<-lobp2[!grepl("WISCONSIN FAMILY ACTION",lobp2$Client),]
lobp2<-lobp2[!grepl("COALITION AGAINST SEXUAL ASSAULT",lobp2$Client),]
lobp2<-lobp2[!grepl("BLUE CROSS BLUE SHIELD OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("BRAIN INJURY ASSOCIATION OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("CENTER FOR PEOPLE IN NEED",lobp2$Client),]
lobp2<-lobp2[!grepl("CENTER FOR RURAL AFFAIRS",lobp2$Client),]
lobp2<-lobp2[!grepl("CENTRAL NEBRASKA PUBLIC POWER & IRRIGATION",lobp2$Client),]
lobp2<-lobp2[!grepl("CENTRAL IOWA BUILDING & CONSTRUCTION TRADES COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("CHILDREN & FAMILY COALITION OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("CHILDREN'S RESPITE CARE CENTER",lobp2$Client),]
lobp2<-lobp2[!grepl("CHILDREN & FAMILY COALITION OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("CHILDRENS HOSPITAL & MEDICAL CENTER",lobp2$Client),]
lobp2<-lobp2[!grepl("CHILDRENS HOSPITAL OF WISCONSIN",lobp2$Client),]
lobp2<-lobp2[!grepl("COALITION OF WISCONSIN AGING GROUPS",lobp2$Client),]
lobp2<-lobp2[!grepl("COMMUNITY ALLIANCE INC",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATE NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("FOOD BANK OF THE HEARTLAND",lobp2$Client),]
lobp2<-lobp2[!grepl("BOLD NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF SURGICAL TECHNOLOGISTS",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF STATE PROSECUTORS",lobp2$Client),]
lobp2<-lobp2[!grepl("ARC OF ",lobp2$Client),]
lobp2<-lobp2[!grepl("BRAIN INJURY ASSOCIATION OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("9TO5 NATIONAL ASSOCIATION OF WORKING WOMEN",lobp2$Client),]
lobp2<-lobp2[!grepl("ARTHRITIS FOUNDATION",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF INDEPENDENT COLLEGES & UNIVERSITIES OF NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("CORRECTIONS OFFICERS ASS",lobp2$Client),]
lobp2<-lobp2[!grepl("EDUCATIONAL SERVICE UNIT ",lobp2$Client),]
lobp2<-lobp2[!grepl("FREEDOM TO SERVE IN FAITH COALITION",lobp2$Client),]
lobp2<-lobp2[!grepl("FIRST FIVE NEBRASKA",lobp2$Client),]
lobp2<-lobp2[!grepl("FOUR OAKS",lobp2$Client),]
lobp2<-lobp2[!grepl("FROEDTERT HEALTH",lobp2$Client),]
lobp2<-lobp2[!grepl("SOCIETY OF ARCHITECTS",lobp2$Client),]
lobp2<-lobp2[!grepl("WEA INSURANCE CORPORATION",lobp2$Client),]
lobp2<-lobp2[!grepl("UNITYPOINT HEALTH",lobp2$Client),]
lobp2<-lobp2[!grepl("RURAL WISCONSIN HEALTH COOPERATIVE",lobp2$Client),]
lobp2<-lobp2[!grepl("REGIONS II & V",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKANS FOR ALTERNATIVES TO THE DEATH PENALTY",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA STATE COLLEGE SYSTEM",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA STATE IRRIGATION ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NEBRASKA STATE PEST CONTROL ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("NURSE PRACTITIONERS",lobp2$Client),]
lobp2<-lobp2[!grepl("OCCUPATIONAL THERAPY ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("LABOR UNITY COUNCIL",lobp2$Client),]
lobp2<-lobp2[!grepl("AMERICAN SOCIETY OF PENSION PROFESSIONALS",lobp2$Client),]
lobp2<-lobp2[!grepl("IOWANS FOR TAX RELIEF",lobp2$Client),]
lobp2<-lobp2[!grepl("METRO AREA TRANSIT (O-METRO)",lobp2$Client),]
lobp2<-lobp2[!grepl("PENFIELD CHILDRENS CENTER",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF BEHAVIORAL HEALTH ORGANIZATIONS",lobp2$Client),]
lobp2<-lobp2[!grepl("WISCONSIN PUBLIC POWER",lobp2$Client),]
lobp2<-lobp2[!grepl("NATIONAL ASSOCIATION OF HOUSING & REDEVELOPMENT OFFICIALS, NEBRASKA CHAPTER",lobp2$Client),]
lobp2<-lobp2[!grepl("CREIGHTON UNIVERSITY",lobp2$Client),]
lobp2<-lobp2[!grepl("TIAA",lobp2$Client),]
lobp2<-lobp2[!grepl("NATIONAL RIFLE ASSOCIATION",lobp2$Client),]
lobp2<-lobp2[!grepl("STEELWORKERS",lobp2$Client),]
lobp2<-lobp2[!grepl("NATIONAL MULTIPLE SCLEROSIS SOCIETY",lobp2$Client),]
lobp2<-lobp2[!grepl("VOLUNTEERS OF AMERICA",lobp2$Client),]
lobp2<-lobp2[!grepl("ASSOCIATION OF CALIFORNIA WATER AGENCIES",lobp2$Client),]
#remove any orgs that are not corps or trade groups
lobp2<-lobp2[-which(lobp2$Client%in%toupper(ct$name[ct$corp_trade==0])==T),]

#harmonize additional names across states
lobp2$Client[lobp2$Client=="MARATHON PETROLEUM COMPANY LP"]<-"MARATHON PETROLEUM CORPORATION"
lobp2$Client[lobp2$Client=="LOCKRIDGE GRINDAL & NAUEN"]<-"LOCKRIDGE GRINDAL NAUEN PLLP"
lobp2$Client[lobp2$Client=="LNE GROUP"]<-"LNE GROUP LLC"
lobp2$Client[lobp2$Client=="LABORATORY CORPORATION OF AMERICA"]<-"LABORATORY CORPORATION OF AMERICA HOLDINGS"
lobp2$Client[lobp2$Client=="CARFAX"]<-"CARFAX INC"
lobp2$Client[lobp2$Client=="CARFAX"]<-"CARFAX INC"
lobp2$Client[lobp2$Client=="MARINE INDUSTRIES ASSOCIATION OF PALM BEACH COUNTY"]<-"MARINE INDUSTRIES ASSOCIATION OF PALM BEACH COUNTY INC"
lobp2$Client[lobp2$Client=="CARFAX"]<-"CARFAX INC"
lobp2$Client[lobp2$Client=="HALL BOOTH SMITH"]<-"HALL BOOTH SMITH PC"
lobp2$Client[lobp2$Client=="HEARTLAND CREDIT UNION ASSOCIATION FKA MISSOURI CREDIT UNION ASSOCIATION"]<-"HEARTLAND CREDIT UNION ASSOCIATION"
lobp2$Client[lobp2$Client=="HMS HOLDINGS CORP"]<-"HMS HOLDINGS CORPORATION"
lobp2$Client[lobp2$Client=="HMSHOST"]<-"CARFAX INC"
lobp2$Client[lobp2$Client=="HMSHOST CORPORATION"]<-"HMS HOST CORPORATION"
lobp2$Client[lobp2$Client=="AMERICAN RENTAL ASSOCIATION THRU MULTISTATE"]<-"AMERICAN RENTAL ASSOCIATION"
lobp2$Client[lobp2$Client=="CASH AMERICA"]<-"CASH AMERICA INTERNATIONAL"
lobp2$Client[lobp2$Client=="INJURED WORKERS PHARMACY"]<-"INJURED WORKERS PHARMACY, LLC"
lobp2$Client[lobp2$Client=="DEEPWATER WIND"]<-"DEEPWATER WIND LLC"
lobp2$Client[lobp2$Client=="IMAGINE SCHOOLS"]<-"IMAGINE SCHOOLS LLC"
lobp2$Client[lobp2$Client=="IMAGINE LEARNING"]<-"IMAGINE LEARNING INC"
lobp2$Client[lobp2$Client=="HUDSON GROUP"]<-"HUDSON GROUP LLC"
lobp2$Client[lobp2$Client=="CARFAX"]<-"CARFAX INC"
lobp2$Client[lobp2$Client=="HOLOGIC"]<-"HOLOGIC INC"
lobp2$Client[lobp2$Client=="HONDA NORTH AMERICA THRU MULTISTATE"]<-"HONDA NORTH AMERICA"
lobp2$Client[lobp2$Client=="INTOXIMETERS"]<-"INTOXIMETERS INC"
lobp2$Client[lobp2$Client=="INVENERGY"]<-"INVENERGY LLC"
lobp2$Client[lobp2$Client=="BLAKE ENTERPRISES"]<-"BLAKE ENTERPRISES LLC"
lobp2$Client[lobp2$Client=="BLUEBIRD BIO"]<-"BLUEBIRD BIO INC"
lobp2$Client[lobp2$Client=="BURTON-LIESE"]<-"BURTON & LIESE GOVERNMENT RELATIONS"
lobp2$Client[lobp2$Client=="BURTON & LIESE"]<-"BURTON & LIESE GOVERNMENT RELATIONS"
lobp2$Client[lobp2$Client=="CAPITAL STRATEGIES"]<-"CAPITAL STRATEGIES INC"
lobp2$Client[lobp2$Client=="CAPITOL GROUP"]<-"CAPITOL GROUP INC"
lobp2$Client[lobp2$Client=="CAPITOL RESOURCES INC"]<-"CAPITOL RESOURCES INC INC"
lobp2$Client[lobp2$Client=="CARASOFT TECHNOLOGY CORPORATION"]<-"CARAHSOFT TECHNOLOGY CORPORATION"
lobp2$Client[lobp2$Client=="CARR RIGGS & INGRAM LLC"]<-"CARR, RIGGS & INGRAM LLC"
lobp2$Client[lobp2$Client=="CASCON GROUP"]<-"CASCON GROUP LLC"
lobp2$Client[lobp2$Client=="ADDUS HEALTH CARE"]<-"ADDUS HEALTH CARE INC"
lobp2$Client[lobp2$Client=="ADVANCE DISPOSAL"]<-"ADVANCED DISPOSAL SERVICES INC"
lobp2$Client[lobp2$Client=="ADVANCED DISPOSAL"]<-"ADVANCED DISPOSAL SERVICES INC"
lobp2$Client[lobp2$Client=="ADVANCED DISPOSAL SERVICES"]<-"ADVANCED DISPOSAL SERVICES INC"
lobp2$Client[lobp2$Client=="ARMSTRONG, TEASDALE"]<-"ARMSTRONG TEASDALE LLP"
lobp2$Client[lobp2$Client=="ROBERT M LEVY & ASSOCIATES"]<-"ROBERT M LEVY & ASSOCIATES INC"
lobp2$Client[lobp2$Client=="SUNEDISON"]<-"SUNEDISON, INC."
lobp2$Client[lobp2$Client=="SANDERSON FARMS"]<-"SANDERSON FARMS INC"
lobp2$Client[lobp2$Client=="SAP PUBLIC SERVICES"]<-"SAP PUBLIC SERVICES INC"
lobp2$Client[lobp2$Client=="SIMON PROPERTY GROUP"]<-"SIMON PROPERTY GROUP INC"
lobp2$Client[lobp2$Client=="FUEL CELL ENERGY INC"]<-"FUELCELL ENERGY INC"
lobp2$Client[lobp2$Client=="LEVINE LEICHTMAN CAPITAL PARTNERS"]<-"LEVINE LEICHTMAN CAPITAL PARTNERS INC"
lobp2$Client[lobp2$Client=="LAFARGE NORTH AMERICA INC HOLCIM US INC AGGREGATE INDUSTRIES MANAGEMENT INC"]<-"LAFARGE NORTH AMERICA"
lobp2$Client[lobp2$Client=="ALBERMARLE CORPC"]<-"ALBEMARLE CORP"
lobp2$Client[lobp2$Client=="SPACE EXPLORATION TECHNOLOGIES"]<-"SPACE EXPLORATION TECHNOLOGIES CORP"
lobp2$Client[lobp2$Client=="SOUTHERN GLAZER'S WINE & SPIRITS"]<-"SOUTHERN GLAZERS WINE & SPIRITS"
lobp2$Client[lobp2$Client=="SOUTHERN COMPANY"]<-"SOUTHERN COMPANY GAS"
lobp2$Client[lobp2$Client=="ABBOTT"]<-"ABBOTT LABORATORIES"

#calculate jaccard index for clients
stm<-matrix(NA,nrow=length(table(lobp2$State)),ncol=length(table(lobp2$State)))
st<-sort(unique(lobp2$State))
for(i in 1:nrow(stm)){
  for(j in 1:ncol(stm)){
    if(j>=i){next}
    stm[i,j]<-length(intersect(lobp2$Client[lobp2$State==st[i]],lobp2$Client[lobp2$State==st[j]]))/length(union(lobp2$Client[lobp2$State==st[i]],lobp2$Client[lobp2$State==st[j]]))
  }
}
png(filename=paste(getwd(),"ST19_JOP_replication","robustness_lobbying_principals.png",sep="/"),width=1300,height=1300)
par(mfrow=c(2,2),mar=c(1,1,1,1),mai=c(1,1,1.2,0.5))
hist(stm,xlab="Jaccard Index",main="(1) Comparisons Between All States",
     cex.main=3,cex.lab=2.5,cex.axis=2.5);summary(as.vector(stm))
abline(v=median(stm,na.rm = T),lwd=3)
length(unique(lobp2$Client));median(stm,na.rm = T)
hist(c(stm[which(st=="IOWA"),],stm[,which(st=="IOWA")]),xlab="Jaccard Index",
     cex.main=3,cex.lab=2.5,cex.axis=2.5,main="(2) Comparisons Between Iowa\nand Other States")
abline(v=median(na.rm=T,(c(stm[which(st=="IOWA"),],stm[,which(st=="IOWA")]))),lwd=3)
hist(c(stm[which(st=="NEBRASKA"),],stm[,which(st=="NEBRASKA")]),xlab="Jaccard Index",
     cex.main=3,cex.lab=2.5,cex.axis=2.5,main="(3) Comparisons Between Nebraska\nand Other States")
abline(v=median(na.rm=T,(c(stm[which(st=="NEBRASKA"),],stm[,which(st=="NEBRASKA")]))),lwd=3)
hist(c(stm[which(st=="WISCONSIN"),],stm[,which(st=="WISCONSIN")]),
     cex.main=3,cex.lab=2.5,cex.axis=2.5,xlab="Jaccard Index",main="(4) Comparisons Between Wisconsin\nand Other States")
abline(v=median(na.rm=T,(c(stm[which(st=="WISCONSIN"),],stm[,which(st=="WISCONSIN")]))),lwd=3)
dev.off()