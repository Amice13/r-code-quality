##############################################################################
#
# Replication code for 
# ``Moderation or Strategy? Political Giving by Corporations and Trade Groups''
#       by Sebastian Thieme
#
# files used:     ST19_JOP_replication.RData
#                 (inlcudes draws from the posterior distribution,
#                  point estimates, and auxiliary data)
# files produced: main_results.png
#                 (Figure 1 in the article)
###############################################################################

Sys.setlocale(locale="C")
gc();gc();gc();gc();gc();gc();gc()
#load results (change file path if RData file not in working directory) and auxiliary data
load("~/ST19_JOP_replication.RData")
##########################
# Overview:
#
# The object 'cont_sc' is a table that contains Shor-McCarty NPAT scores (2018),
# DIME recipient CFscores, contributor CFscores and IDs (Bonica 2016),
# sector codings based on the 13 sectors from the Center for Responsive Politics
# (https://www.opensecrets.org/downloads/crp/CRP_Categories.txt), as well as
# separate contributor classifications based on FEC classifications (see Appendix B.1).
# See the codebook for additional details.
#
# The object 'df_9_20_3' is the vote matrix used to estimate the ideal points presented
# in the article. N.B. To combine with 'cont_sc' using the name variable, please
# assign object 'rnames' as rownames to 'df_9_20_3' first.
#
# The object 'res' contains the output from the first of three chains from the ideal point
# estimation using 'ideal' (package pscl).
#
# The objects 'est_x' and 'est' (respectively) contain the MCMC output and means from 
# the first of three chains with respect to the ideal point of each politician or lobbying
# principal. Compared to 'res', the polarity of estimates was reversed to conform to the 
# common interpretation of Democrats on the left and Republicans on the right
# (see Jackman, Simon. 2001. ''Analysis of Roll Call Data via Bayesian Simulation:
# Identification, Estimation, Inference, and Model Checking.''. Political
# Analysis 9(3), 231 for why the rotational invariance - especially in conjunction
# with selecting the option 'eigen' to generate starting values - is not an issue.)
#
# The object 'leg_weights' shows the number of sessions in which a legislator 
# or lobbying organization was active.
##############################################################################################

##############################
# Preparing Data for Analysis
##############################

est2<-est #est2
est2<-merge(est2,cont_sc,by=c("name")) #merge est2 with cont_sc
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
#Main Results: Panels 1-6 in Figure 1
#####################################
dir.create("ST19_JOP_replication")
png(filename=paste(getwd(),"ST19_JOP_replication","main_results.png",sep="/"), width=1800,height=1200)
par(mar=c(1,1,1,1),mai=c(0.95,1,1,0.95),mfrow=c(2,3))
#############
#Panel A
plot(cex.lab=2,cex.main=3,cex.axis=2,lwd=2,xlab="",
     main="(A) Position-Based Estimates",
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
#102 corporations
length(which(est1$ideal[which(est1$corp_trade=="1")]> -5))
#171 trade associations
length(which(est1$ideal[which(est1$corp_trade=="2")]> -5))
#############
#Panel B
plot(cex.lab=2,cex.main=3,cex.axis=2,lwd=2,xlab="",
     main="(B) Contribution-Based Estimates",ylim=c(0,1.8),
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
#188 corporations and trade groups
length(which(est2$bonica2_cf_most_overall[ict]> -5))
#83 corporations 
length(which(est2$bonica2_cf_most_overall[which(est2$corp_trade=="1")]> -5))
#105 trade associations
length(which(est2$bonica2_cf_most_overall[which(est2$corp_trade=="2")]> -5))
#corps and trade assn. with extreme conservative contribution record
length(which(est2$bonica2_cf_most_overall[ict]>median(rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/R/",est2$name),ilgs)]))))
#corps and trade assn. with extreme liberal contribution record
length(which(est2$bonica2_cf_most_overall[ict]<median(rep(est2$bonica2_cf_r[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)],times=est2$weights[intersect(grep("\\([A-Z][A-Z]/D/",est2$name),ilgs)]))))
(188-14)/188
#############
# Panel C
plot(pch="C",cex=2,cex.main=3,main="(C) Comparison for Corps. and Trade Groups",cex.axis=2,xlab="",ylab="",xlim=c(-2,2),
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
#80% of corporations and trade groups reveal more conservative policy preferences
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
#41 out of 174 organization with `moderate' contribution record reveal
#extreme policy preferences
numer
numer/denom

#Results:
#correlation between contribution- and position-taking behavior 
#for corporations and trade groups in the sample: 0.27
cor(est2$ideal[ict],est2$bonica2_cf_most_overall[ict],use="pairwise.complete.obs")
#correlation between contribution- and position-taking behavior 
#for other organizations in the sample: 0.80
cor(est2$ideal[setdiff(which(est2$X!=""),ict)],est2$bonica2_cf_most_overall[setdiff(which(est2$X!=""),ict)],use="pairwise.complete.obs")

#########################
#Panel D
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(D) High- and Low-Level Contributors", qq3,col=rep(c("black","white"),50))#;lines(qq2,col="red")
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
#than 40th legislator quantile: 3.7%
qq3[41]
#percentage of corps. & trade groups less conservative
#than 60th legislator quantile: 87.2
qq3[61]
#difference: 84pp
qq3[61]-qq3[41]
#similar increase for position-based measure between
#40th and 86th legislator quantile
mean(ms2[41,])
mean(ms2[91,])
mean(ms2[87,])-mean(ms2[41,])


#############
# Panel E
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(E) Low-Level Contributors", qql,col=rep(c("black","white"),50))#;lines(qq2,col="red")
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
mean(msl[87,])
mean(msl[87,])-mean(msl[41,])

#############
#Panel F
plot(cex.axis=2,cex.lab=2,cex=2,cex.main=3,xlab="",ylab="",
     main="(F) High-Level Contributors", qqh,col=rep(c("black","white"),50))#;lines(qq2,col="red")
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
#results: 
#contribution measure for high-level contributors
#percentage of corps. & trade groups less conservative
#than 40th legislator quantile: 1.8%
qqh[41]
#percentage of corps. & trade groups less conservative
#than 60th legislator quantile: 95.3%
qqh[61]
#difference 93.5 pp
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


