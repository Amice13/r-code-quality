#########################################################
#Author: M. Kenwick
#Date: Sep 20, 2019
#Purpose: Generates figures and runs analyses reported in the main text
#########################################################
rm(list=ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stats4)
library(MASS)
set.seed(1989)

#Set the working directory to the top-level of the replication folder
setwd('~/Dropbox/ctrl_rep_final')
#Load and prep data
load("irt_modeling/data_prepped.RData")
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")

#Extract model estiamtes and append to data object
data$theta_s<-apply(output_s$x,2,mean)
data$theta_s_sd<-apply(output_s$x,2,sd)
theta_s_draws<-output_s$x
  
theta_d_draws<-t(apply(output$x,1,scale))
data$theta_d<-apply(theta_d_draws,2,mean)
data$theta_d_sd<-apply(theta_d_draws,2,sd)

write.csv(dplyr::select(data,gwf_casename,ccode,year,theta_s,theta_s_sd,theta_d,theta_d_sd),
          file="output/civ_ctrl_scores_isq.csv",na="",row.names=F)

data$theta_s_up<-data$theta_s + (1.96*data$theta_s_sd)
data$theta_s_lw<-data$theta_s - (1.96*data$theta_s_sd)
data$theta_d_up<-data$theta_d + (1.96*data$theta_d_sd)
data$theta_d_lw<-data$theta_d - (1.96*data$theta_d_sd)

#Figure 1: Distribution of Mean Posterior Estimates of Civilian Control
col<-brewer.pal(9,"Reds")
data$age_col<-col[3]
data$age_col[data$gwf_duration>10]<-col[4]
data$age_col[data$gwf_duration>20]<-col[5]
data$age_col[data$gwf_duration>30]<-col[6]
data$age_col[data$gwf_duration>40]<-col[7]
data$age_col[data$gwf_duration>50]<-col[8]
data$age_col[data$gwf_duration>60]<-col[9]

pdf("plots/figure1.pdf",height=7,width=14)
par(mar=c(4,4,3,3),  oma = c(0,0,0,0),mfrow=c(1,2))

plot(NULL,
     xlim = c(-5,5), 
     ylim = c(0,.82), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(data$theta_s,bw=.2),col=alpha("gray80",1),border=NA) 
polygon(density(data$theta_d,bw=.2),col=alpha("#313695",.8),border=NA)  
axis(side=1, at=seq(-4,6,2), las=1, cex.axis=1.25,mgp=c(3,.8,0))
axis(side=2,at=seq(0,.8,.2), las=2, cex.axis=1.25)
#title("(a) Mean Posterior Estimates", cex.main=1.5)
mtext("Density", side = 2,line=2.75,cex=1.5) # label bottom axis
mtext("Civilian Control", side = 1, line=2,cex=1.5) # label bottom axis
text(-1.4,.81,"Static Estimates",pos=2,col=alpha("gray60",1),cex=1.5)
text(-1.4,.76,"Drift Estimates",pos=2,col=alpha("#313695",1),cex=1.5)
box()

plot(NULL,
     ylim = c(-3.25,4), 
     xlim = c(-3,1), 
     axes = F, xlab = NA, ylab = NA) 
points(data$theta_s,data$theta_d,  col=alpha(data$age_col,.7),bg=alpha(data$age_col,.1), cex=1, pch=21,lwd=.5 )
axis(side=2, at=seq(-3,4,1), las=1, cex.axis=1.25)
axis(side=1, at=seq(-3,1,1), las=1, cex.axis=1.25)
abline(a=0,b=1, col="black",lty=1, lwd=1.5)
text(-1.75,   4,"Regime Age",pos=2,col="black",cex=1.25)
text(-1.75,3.7,"1-10 years",pos=2,col=col[3],cex=1.25)
text(-1.75,3.4,"12-20 years",pos=2,col=col[4],cex=1.25)
text(-1.75,3.1,"21-30 years",pos=2,col=col[5],cex=1.25)
text(-1.75,2.8,"31-40 years",pos=2,col=col[6],cex=1.25)
text(-1.75,2.5,"41-50 years",pos=2,col=col[7],cex=1.25)
text(-1.75,2.2,"50-60 years",pos=2,col=col[8],cex=1.25)
text(-1.75,1.9,"61+ years",pos=2,col=col[9],cex=1.25)
mtext("Civilian Control (Dynamic Drift)", side = 2,line=2.75, cex=1.25) # label bottom axis
mtext("Civilian Control (Static)", side = 1, line=2,cex=1.25) # label bottom axis
box()


dev.off()


#Table 2: Top Ten Cases of Highest Disagreement in Model Estimates
means = data %>%
  group_by(gwf_casename) %>%
  mutate(theta_s_mean = round(mean(theta_s),3)) %>%
  mutate(theta_d_mean = round(mean(theta_d),3)) %>%
  mutate(diff = abs(theta_d_mean-theta_s_mean)) %>%
  dplyr::select(gwf_casename,theta_d_mean,theta_s_mean,diff) %>%
  arrange(desc(diff)) %>%
  unique()
means
print(as.data.frame(means[1:10,]))




#Figure 2: Mean Estimates of Civilian Control for Civilianized Regimes
#observed_drift_yrs records the number of years a regime has experienced drift
#since entering the data. drift_ind captures whether a regime is civilianized
id<-seq(1:nrow(data))
means_s<-matrix(ncol=max(observed_drift_yrs),nrow=nrow(theta_s_draws))
for(jj in 1:max(observed_drift_yrs)){
  x<-theta_s_draws[,id[observed_drift_yrs==jj & drift_ind==1]]
  means_s[,jj]<-apply(x,1,mean)
}
means<-matrix(ncol=max(observed_drift_yrs),nrow=nrow(theta_d_draws))
for(jj in 1:max(observed_drift_yrs)){
  x<-theta_d_draws[,id[observed_drift_yrs==jj & drift_ind==1]]
  means[,jj]<-apply(x,1,mean)
}
age<-seq(1:max(observed_drift_yrs))
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25

pdf(file="plots/figure2.pdf",height=6,width=7)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4,2,2),
  mfrow= c(1,1)
)
plot(NULL,# create empty plot
     xlim = c(1,65), # set xlim by guessing
     ylim = c(-1,2), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels  
grid()
for (ii in 0:65){
  lines(c(age_data$age[ii]-.25,age_data$age[ii]-.25),c(age_data$mean_up_s[ii], age_data$mean_lw_s[ii]),col="gray60", lwd=1.75)
  points(age_data$age[ii]-.25,age_data$mean_s[ii],bg=c("gray90"), col="gray60",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:65){
  lines(c(age_data$age[ii]+.25,age_data$age[ii]+.25),c(age_data$mean_up[ii], age_data$mean_lw[ii]),col="#313695", lwd=1.75)
  points(age_data$age[ii]+.25,age_data$mean[ii],bg=c("#74add1"), col="#313695",  cex=.65, pch=21, lwd=1.5)
}
lab=c(0,(seq(5,65,by=5)))
axis(1, at=lab, labels=lab,cex.axis=1)
axis(4,at=NULL, labels=FALSE,tick=FALSE,col="white",cex.axis=1)
axis(side=2,  las=1,cex.axis=1)
mtext(side = 1, "Years of Drift", line =2.5,cex=1.25) # label bottom axis
mtext(side = 2, "Mean Civilian Control", line=2.25, cex=1.25)# add title
text(0,1.74,"Static",pos=4,col="gray60",cex=1.5)
text(0,1.9,"Dynamic Drift",pos=4,col="#313695", cex=1.5)
box()
dev.off()



#Figure 3: Summary of Posterior Predictive Checks
#Note: Leave-one-item-out posterior predictions were generated
#using a series of IRT models iteratively dropping one item out of estimation
#The scripts used to run these models are located in irt_modeling/loio/drift and
#irt_modeling/loio/static. The estiamtes are extracted from the model output files using
#loio_estimates_extract
#Posterior predictions are generated in the loio folder, using
#the loio_estimates_extract script
load('output/pcp_output.RData')
load('output/loio_oos_pcp.RData')

NAMES<-c("Military Entry","CGV Military Regime","Military Leader",
         "GWF Military Regime","Prior Military Regime","ARD Military Regime",
         "Leader Military Experience","Military Cabinet Ministers",
         "Mil. Involvement in Politics","Militarism Index")
pdf("plots/figure3.pdf", width=18,height=8)
par(mfrow=c(1,2),mar=c(4.5,15,3,3))
boxplot(d_postpred[1,]-s_postpred[1,],
        d_postpred[2,]-s_postpred[2,],
        d_postpred[3,]-s_postpred[3,],
        d_postpred[4,]-s_postpred[4,],
        d_postpred[5,]-s_postpred[5,],
        d_postpred[6,]-s_postpred[6,],
        d_postpred[7,]-s_postpred[7,],
        d_postpred[8,]-s_postpred[8,],
        d_postpred[9,]-s_postpred[9,],
        d_postpred[10,]-s_postpred[10,],
        col=c(rep("#74add1",7),"#74add1",rep("#74add1",2)),
        main="Summary of Posterior Predictions \n(In Sample)",horizontal=T,
        xlim=c(0.5,11),ylim=c(-.075,0.101),cex.axis=1.2
)
axis(2,labels=NAMES,at=seq(1,10,1),las=2,cex.axis=1.2)
abline(v=0, col = "orangered", lty = 5)
mtext(side = 1, "Difference in Proportion Correctly Predicted",las=0,line=2.5,cex=1.4) 
mtext(side = 1, expression((PCP[Drift] - PCP[Static])),las=0,line=3.5,cex=1) 
text(0,11,"Static Model Superior   ",pos=2,col="gray40")
text(.10,11," Drift Model Superior ",pos=2,col="#313695")
arrows(-.07,11.02,-0.0775,11.02, xpd = TRUE, lwd=1.5, length=.05,col="gray40")
arrows(0.0975,11.02,.105,11.02, xpd = TRUE, lwd=1.5,length=.05,col="#313695" )
box() 

boxplot(pcp_d[1,]-pcp_s[1,],
        pcp_d[2,]-pcp_s[2,],
        pcp_d[3,]-pcp_s[3,],
        pcp_d[4,]-pcp_s[4,],
        pcp_d[5,]-pcp_s[5,],
        pcp_d[6,]-pcp_s[6,],
        pcp_d[7,]-pcp_s[7,],
        pcp_d[8,]-pcp_s[8,],
        pcp_d[9,]-pcp_s[9,],
        pcp_d[10,]-pcp_s[10,],
        col=c(rep("#74add1",6),"gray80",rep("#74add1",3)),
        main="Summary of Posterior Predictions \n(Out of Sample)",horizontal=T,
        xlim=c(0.5,11),ylim=c(-.075,0.101),cex.axis=1.2
)
axis(2,labels=NAMES,at=seq(1,10,1),las=2,cex.axis=1.2)
abline(v=0, col = "orangered", lty = 5)
mtext(side = 1, "Difference in Proportion Correctly Predicted",las=0,line=2.5,cex=1.4) 
mtext(side = 1, expression((PCP[Drift] - PCP[Static])),las=0,line=3.5,cex=1) 
text(0,11,"Static Model Superior   ",pos=2,col="gray40")
text(.10,11," Drift Model Superior ",pos=2,col="#313695")
arrows(-.07,11.02,-0.0775,11.02, xpd = TRUE, lwd=1.5, length=.05,col="gray40")
arrows(0.0975,11.02,.105,11.02, xpd = TRUE, lwd=1.5,length=.05,col="#313695" )
box()
dev.off()



#Figure 4: Civilian Control in 2010 – Comparing Turkey
subset.d <- subset(data, year==2010)  
POSTERIORS<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_s,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_s,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_s,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_s,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_s,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_s,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_s
)

ub<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_s_up,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_s_up
)

lb<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_s_lw,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_s_lw
)


POSTERIORS_drft<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_d,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_d,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_d,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_d,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_d,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_d,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_d
)

ub_drft<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_d_up,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_d_up
)

lb_drft<-c(
  subset.d[subset.d$gwf_country=="Canada",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Costa Rica",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Saudi Arabia",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Turkey",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Iraq",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Thailand",]$theta_d_lw,
  subset.d[subset.d$gwf_country=="Myanmar",]$theta_d_lw
)

names<-c("Canada","Costa Rica","Saudi Arabia","Turkey","Iraq","Thailand","Myanmar")
INDEX<-1:7
INDEX <- INDEX[order(POSTERIORS_drft, decreasing=TRUE)]
POSTERIORS<-POSTERIORS[INDEX]
POSTERIORS_drft<-POSTERIORS_drft[INDEX]
ub_drft<-ub_drft[INDEX]
lb_drft<-lb_drft[INDEX]
ub<-ub[INDEX]
lb<-lb[INDEX]
names<-names[INDEX]

pdf("plots/figure4.pdf",width=9,height=7.5)
par(mfrow=c(2,1),mar=c(4,2,1,2), font=2, font.lab=1, cex=1, cex.axis=.5, oma=c(1,0,2,0))
plot(POSTERIORS, (1:7), ylim=c(0,8.5),xlim=c(-3.35,2.15),cex=.5, ylab="", xlab=" ", yaxt="n", main="Static Estimates, 2010",axes = F,cex.main=1.25)
mtext(side = 1, "Civilian Control", line=1.75, cex=1.25,font=1)# add title
for(i in 1:7){
  lines(c(lb[i], ub[i]), c(i,i), col=("gray60"), lwd=2)
  
}
points(POSTERIORS, (1:7), col="gray40", bg="gray60", cex=.75, pch=21,lwd=1.5)

for (i in 1:length(names)){
  text(POSTERIORS[i],i-.2,names[i],pos=3,col="gray60",cex=1)
}
axis(side=1, las=1, cex.axis=1.25,mgp=c(3,.7,0),at=seq(-3,2,1))
lines(c(lb[4], ub[4]), c(4,4), col=("gray20"), lwd=2)
points(POSTERIORS[4], 4,bg=c("gray60"), col="gray20", cex=.75, pch=21,lwd=1.5)
text(POSTERIORS[4],4-.2,names[4],pos=3,col="gray20",cex=1)
box()

plot(POSTERIORS_drft, (1:7), ylim=c(0,8.5),xlim=c(-4.5,4.5),cex=.5, ylab="", xlab=" ", yaxt="n", main="Dynamic Drift Estimates, 2010",axes = F,cex.main=1.25)
mtext(side = 1, "Civilian Control", line=1.75, cex=1.25,font=1)# add title
for(i in 1:7){
  lines(c(lb_drft[i], ub_drft[i]), c(i,i), col="#74add1", lwd=2)
  
}
points(POSTERIORS_drft, (1:7), col="#74add1", bg="#74add1", cex=.75, pch=21,lwd=1.5)

for (i in 1:length(names)){
  text(POSTERIORS_drft[i],i-.2,names[i],pos=3,col="#74add1",cex=1)
}
axis(side=1, las=1, cex.axis=1.25,mgp=c(3,.7,0),seq(-4,4,2))
lines(c(lb_drft[4], ub_drft[4]), c(4,4), col=("#313695"), lwd=2)
points(POSTERIORS_drft[4], 4, bg=c("#74add1"), col="#313695", cex=.75, pch=21,lwd=1.5)
text(POSTERIORS_drft[4],4-.2,names[4],pos=3,col="#313695",cex=1)
box()
dev.off()



#Figure 5: Measurement Model Performance in Forecasting
library(PRROC)
library(foreign)
coups<-read.dta("irt_modeling/pt_coup.dta")
coups$coup<-ifelse(coups$coup1>0,1,0)
coups<-subset(coups,year>2010)
coups = coups %>%
  group_by(ccode) %>%
  mutate(anycoup = max(coup)) %>%
  dplyr::select(ccode,anycoup) %>%
  unique()

fml1<-"anycoup~theta_s_d"
fml2<-"anycoup~theta_d_d"

sims<-1000
auc1<-rep(NA,sims)
auc2<-rep(NA,sims)
prc1<-rep(NA,sims)
prc2<-rep(NA,sims)

for(dd in 1:sims){
  
  theta_s_d<-theta_s_draws[dd,]
  theta_d_d<-theta_d_draws[dd,]
  
  data2<-cbind(data,theta_s_d,theta_d_d)
  data2<-dplyr::select(data2,ccode,gwf_country,year,theta_s_d,theta_d_d)
  data2<-subset(data2,year==2010)
  data2<- merge(data2, coups, by=c('ccode'),all.x=T,all.y=F)
  data2<-na.omit(data2)
  
  data2$partition <- sample(rep(1:10, length.out = nrow(data2)))
  pred1<-rep(NA,nrow(data2))
  pred2<-rep(NA,nrow(data2))

  for(ii in 1:10){
    fit1<-glm(fml1, data=data2[data2$partition!=ii,],family=binomial)
    fit2<-glm(fml2, data=data2[data2$partition!=ii,],family=binomial)
    pred1[data2$partition==ii]<-plogis(predict(fit1,newdata=data2[data2$partition==ii,]))
    pred2[data2$partition==ii]<-plogis(predict(fit2,newdata=data2[data2$partition==ii,]))
  }
  
  auc1[dd]<-roc.curve(scores.class0=pred1[data2$anycoup==1],scores.class1=pred1[data2$anycoup==0])$auc
  auc2[dd]<-roc.curve(scores.class0=pred2[data2$anycoup==1],scores.class1=pred2[data2$anycoup==0])$auc
  prc1[dd]<-pr.curve(scores.class0=pred1[data2$anycoup==1],scores.class1=pred1[data2$anycoup==0])$auc.davis.goadrich
  prc2[dd]<-pr.curve(scores.class0=pred2[data2$anycoup==1],scores.class1=pred2[data2$anycoup==0])$auc.davis.goadrich
}

pdf("plots/figure5.pdf",height=5,width=8)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4.5,2,2),
  mfrow= c(1,2)
)
#Model Performance Forecasting Coups
boxplot(auc1,auc2,col=c("gray80","#74add1"),names=c("",""),
        col.lab="red",cex.axis=1.25,cex.main=1.5,
        ylim=c(0,1),las=2,notch=F,
        main="")
mtext("Static",line=1,side=1,at=1,col="gray50",cex=1.5)
mtext("Dynamic Drift",line=1,side=1,at=2,col="#313695",cex=1.5)
mtext("Area Under the ROC Curve",side=2,line=2.85,cex=1.25)

boxplot(prc1,prc2,col=c("gray80","#74add1"),names=c("",""),
        col.lab="red",cex.axis=1.25,cex.main=1.5,las=2,
        main="",notch=F)
mtext("Static",line=1,side=1,at=1,col="gray50",cex=1.5)
mtext("Dynamic Drift",line=1,side=1,at=2,col="#313695",cex=1.5)
mtext("Area Under the Precision Recall Curve",side=2,line=3.45,cex=1.25)
dev.off()

#Difference in means
t.test(auc1,auc2)
t.test(prc1,prc2)


