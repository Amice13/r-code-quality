# Step 1: Load Requisite Packages ---------------------------------------------

rm(list=ls())
library(ggplot2)
library(dplyr)
library(foreign)
library(haven)
library(jtools)
library(huxtable)
library(officer)
library(flextable)
library(VGAM)




# Step 2: Set Directory and Seed-----------------------------------------------
#Paste the wording directory of the replication folder below
#e.g., primary_path <- "~/Dropbox/cm_experiment_rep/"
primary_path <- ""
setwd(primary_path)
set.seed(10101)




# Step 3: Functions for Plotting, Data Transformation, and P-values------------


pvalue<-function(boots){
  min(1 - length(which(boots <= 0))/length(boots),
      1 - length(which(boots >= 0))/length(boots))*2
  }

plot_density<-function(boots){
  test<-list()
  for(ii in 1:4){
    labels<-c("Stay Out","Engage","Success", "Failure")
    test<-plot(NULL,
               xlim = (c(-.25,.25)), 
               ylim = c(0,15), 
               axes = F, xlab = NA, ylab = NA) 
    grid(col="gray80")
    polygon(density(boots[,ii]),col=alpha("#a6bddb",.8),border="black",lwd=1.5)  
    axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
    abline(v=0)
    title(as.character(labels[ii]), cex.main=1)
    mtext("Treatment Effect", side = 1, line=2.25,cex=.75) 
    abline(v=mean(boots[,ii]),lty=2,lwd=1.5,col="#d73027")
    text(-.25, 12.5,  
         bquote(p==.(round(pvalue(boots[,ii]) ,3)
         ))
         ,cex=1.5,col="black",pos=4)
    box()
  }
}

plot_density_o<-function(boots){
  test<-list()
  for(ii in 1:4){
    labels<-c("Stay Out","Engage","Success", "Failure")
    test<-plot(NULL,
               xlim = (c(-.55,.55)), 
               ylim = c(0,6), 
               axes = F, xlab = NA, ylab = NA) 
    grid(col="gray80")
    polygon(density(boots[,ii]),col=alpha("#a6bddb",.8),border="black",lwd=1.5)  
    axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
    abline(v=0)
    title(as.character(labels[ii]), cex.main=1)
    mtext("Treatment Effect", side = 1, line=2.25,cex=.75) 
    abline(v=mean(boots[,ii]),lty=2,lwd=1.5,col="#d73027")
    text(-.45, 5,  
         bquote(p==.(round(pvalue(boots[,ii]) ,3)
         ))
         ,cex=1.5,col="black",pos=4)
    box()
  }
}


plot_density_blame<-function(boots,boots_attn){
  par(mfrow=c(2,1),las=1,mar=c(4,2,2,2),oma=c(1,1,1,1))
  test<-list()
  for(ii in 1:2){
    labels<-c("Treatment Effect on Propensity to Blame President and Advisors Equally",
              "Treatment Effect on Propensity to Primarily Blame Presidential Advisors")
    test<-plot(NULL,
               xlim = (c(-.4,.4)), 
               ylim = c(0,11), 
               axes = F, xlab = NA, ylab = NA) 
    grid(col="gray80")
    polygon(density(boots[,ii]),col=alpha("#a6bddb",.85), 
            border="black",lwd=1.5)  
    axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
    abline(v=0)
    title(as.character(labels[ii]), cex.main=1)
    mtext("Effect of Consulting with Military Advisors", 
          side = 1, line=2.25,cex=1.25) 
    abline(v=mean(boots[,ii]),lty=2,lwd=1.5,col="#313695")
    p<-bquote(p==.(round(pvalue(boots[,ii]) ,3)))
    #HARD CODING THIS IN b/c P<0.001 in one case
    text<-c("p = 0.012","p < 0.001")
    text(-.4, 10,text[ii],cex=1.5,col="black",pos=4)
    box()
  }
}

plot_density_credit<-function(boots,boots_attn){
  par(mfrow=c(2,1),las=1,mar=c(4,2,2,2),oma=c(1,1,1,1))
  test<-list()
  for(ii in 1:2){
    labels<-c("Treatment Effect on Propensity to Credit President and Advisors Equally",
              "Treatment Effect on Propensity to Primarily Credit Presidential Advisors")
    test<-plot(NULL,
               xlim = (c(-.4,.4)), 
               ylim = c(0,11), 
               axes = F, xlab = NA, ylab = NA) 
    grid(col="gray80")
    polygon(density(boots[,ii]),col=alpha("#a6bddb",.85),
            border="black",lwd=1.5)  
    axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
    abline(v=0)
    title(as.character(labels[ii]), cex.main=1)
    mtext("Effect of Consulting with Military Advisors",
          side = 1, line=2.25,cex=1.25) # label bottom axis
    abline(v=mean(boots[,ii]),lty=2,lwd=1.5,col="#313695")
    p<-bquote(p==.(round(pvalue(boots[,ii]) ,3)))
    text(-.4, 10,p,cex=1.5,col="black",pos=4)
    box()
  }
}

plot_density_failure_costs<-function(boots){
  par(mfrow=c(3,1),las=1,mar=c(4,2,2,2),oma=c(1,1,1,1))
  test<-list()
  for(ii in 1:3){
    labels<-c("Advisor has Military Affiliation","Advisor is Civilian",
              "Difference in Effects")
    test<-plot(NULL,
               xlim = (c(-.4,.4)), 
               ylim = c(0,11), 
               axes = F, xlab = NA, ylab = NA) 
    grid(col="gray80")
    polygon(density(boots[,ii]),col=alpha("#a6bddb",.85),
            border="black",lwd=1.5)  
    axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
    abline(v=0)
    title(as.character(labels[ii]), cex.main=1)
    if(ii<3){
      mtext("Treatment Effect", side = 1, line=2.25,cex=.75)
    }else{mtext("Difference in Treatment Effects", 
                side = 1, line=2.25,cex=.75)}
    abline(v=mean(boots[,ii]),lty=2,lwd=1.5,col="#313695")
    text(-.4, 9,  
         bquote(p==.(round(pvalue(boots[,ii]) ,3)
         ))
         ,cex=1.5,col="black",pos=4)
    box()
  }
}




# Step 4: Manipulation Checks--------------------------------------------------

data <- read.csv(paste0(primary_path,'experiment/survey_results.csv'))

#Number of respondents
nrow(data)

#Manipulation checks (Appendix A1.6)
table(data$outcome_manip[data$true_out==1]==data$true_out[data$true_out==1])
data$out_correct <- ifelse(data$outcome_manip==data$true_out,1,0)
data$miltreat_correct <- ifelse((data$civtreat==1 & data$consult==3) |
                                  ((data$miltreat==1 & data$consult==1)),1,0)

#Numbers reported in main text
mean(data$miltreat_correct[data$miltreat==1], na.rm=T)
mean(data$miltreat_correct[data$civtreat==1], na.rm=T)
mean(data$out_correct[data$out==1], na.rm=T)
mean(data$out_correct[data$engage==1], na.rm=T)
mean(data$out_correct[data$success==1], na.rm=T)
mean(data$out_correct[data$fail==1], na.rm=T)

#Figure A1.5
par(mfrow=c(1,1))
pdf('results/fig_a1_5.pdf',width=7,height=5)
barplot(c(mean(data$out_correct[data$out==1], na.rm=T),
        mean(data$out_correct[data$engage==1], na.rm=T),
        mean(data$out_correct[data$success==1], na.rm=T),
        mean(data$out_correct[data$fail==1], na.rm=T)),
        ylim=c(0,1),names=c("Stay Out","Engage","Success","Failure"),
        col="orangered3",
        main="Figure A1.5: Respondents Who Correctly Recall Outcome")
dev.off()

#Figure A1.6
pdf('results/fig_a1_6.pdf',width=7,height=5)
barplot(c(mean(data$miltreat_correct[data$civtreat==1], na.rm=T),
          mean(data$miltreat_correct[data$miltreat==1], na.rm=T)),
        ylim=c(0,1),names=c("Civilian","Military"),col="steelblue",
        main="Figure A1.6: Respondents Who Correctly Advisor Affiliation")
dev.off()




# Step 5: Primary Analyses-----------------------------------------------------

# Step 5.1: Primary Analyses (Approval)----------------------------------------
sims <- 4000
bootResults <- matrix(NA, nrow=sims, ncol=7)
bootMeans <- matrix(NA, nrow=sims, ncol=8)

for(ii in 1:sims){
  resample<-dplyr::sample_n(data,size=nrow(data),replace=T)
  Ap <- mean(resample$out_civ_approve_d,na.rm=T)
  Am <- mean(resample$out_mil_approve_d,na.rm=T)
  Bp <- mean(resample$engage_civ_approve_d,na.rm=T)
  Bm <- mean(resample$engage_mil_approve_d,na.rm=T)
  Cp <- mean(resample$success_civ_approve_d,na.rm=T)
  Cm <- mean(resample$success_mil_approve_d,na.rm=T)
  Dp <- mean(resample$fail_civ_approve_d,na.rm=T)
  Dm <- mean(resample$fail_mil_approve_d,na.rm=T)
  bootMeans[ii,1]<-Ap
  bootMeans[ii,2]<-Am
  bootMeans[ii,3]<-Bp
  bootMeans[ii,4]<-Bm
  bootMeans[ii,5]<-Cp
  bootMeans[ii,6]<-Cm
  bootMeans[ii,7]<-Dp
  bootMeans[ii,8]<-Dm
  
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}

means2<-data.frame(mean=apply(bootMeans,2,mean),
                   condition=rep(c("Civilian","Military"),4),
                   out=c("Stay out","Stay out","Engage","Engage",
                         "Success","Success","Failure","Failure"),
                   standard_error=apply(bootMeans,2,sd
                   ))
means2$out <- factor(means2$out,levels = c("Stay out", "Engage", 
                                           "Success", "Failure"))
means2$mean<-means2$mean*100
means2$standard_error<-means2$standard_error*100

pdf('results/fig1_a.pdf',width=6.624,height=7.008)
par(mar=c(5,6,2,2))
test<-barplot(rbind(means2[rev(c(2,4,6,8)),1],means2[rev(c(1,3,5,7)),1]),
              beside=T, col=c('#08519c','#c6dbef'),xlim=c(0,100),
              names=rev(c("Stay out",'Engage','Success','Failure')),
              xlab="Percent Approving of Presidential Decision-Making",
              horiz=T,las=2)
arrows(means2[rev(c(1,3,5,7)),1] - means2[rev(c(1,3,5,7)),4] * 1.96,test[2,], 
       means2[rev(c(1,3,5,7)),1] + means2[rev(c(1,3,5,7)),4] * 1.96, lwd = 1.5,
       test[2,],angle = 90,
       code = 3, length = 0.05)
arrows(means2[rev(c(2,4,6,8)),1] - means2[rev(c(2,4,6,8)),4] * 1.96,test[1,], 
       means2[rev(c(2,4,6,8)),1] + means2[rev(c(2,4,6,8)),4] * 1.96, lwd = 1.5,
       test[1,],angle = 90,
       code = 3, length = 0.05)
text(100,10.75,"Military Advisor",pos=2,col='#08519c')
text(100,11.25,"Civilian Advisor",pos=2,col='#9ecae1')
text(x=rep(0,4),y=test[1,],round(means2[rev(c(2,4,6,8)),1],1),pos=4,col="white")
text(x=rep(0,4),y=test[2,],round(means2[rev(c(1,3,5,7)),1],1),pos=4,col="black")
dev.off()


pdf("results/fig1_b.pdf",height=6.5,width=7)
par(mfrow=c(4,1),las=1,mar=c(4,4,2,2))
plot_density(bootResults)
dev.off()


#Non-bootstrapped estimates
means_out<-c(mean(data$out_civ_approve_d,na.rm=T),
             mean(data$out_mil_approve_d,na.rm=T))
means_en<-c(mean(data$engage_civ_approve_d,na.rm=T),
            mean(data$engage_mil_approve_d,na.rm=T))
means_fail<-c(mean(data$fail_civ_approve_d,na.rm=T),
              mean(data$fail_mil_approve_d,na.rm=T))
means_suc<-c(mean(data$success_civ_approve_d,na.rm=T),
             mean(data$success_mil_approve_d,na.rm=T))
sterror<-function(x){
  sd(x,na.rm=T)/sqrt(sum(!is.na(x)))
}
means<-data.frame(mean=c(means_out,means_en,means_fail,means_suc),
                  condition=rep(c("Civilian","Military"),4),
                  out=c("Stay out","Stay out","Engage","Engage",
                        "Failure","Failure","Success","Success"),
                  standard_error=c(
                    sterror(data$out_civ_approve_d),
                    sterror(data$out_mil_approve_d),
                    sterror(data$engage_civ_approve_d),
                    sterror(data$engage_mil_approve_d),
                    sterror(data$fail_civ_approve_d),
                    sterror(data$fail_mil_approve_d),
                    sterror(data$success_civ_approve_d),
                    sterror(data$success_mil_approve_d)
                  ))
means2<-data.frame(mean=apply(bootMeans,2,mean),
                   condition=rep(c("Civilian","Military"),4),
                   out=c("Stay out","Stay out","Engage","Engage","Success",
                         "Success","Failure","Failure"),
                   standard_error=apply(bootMeans,2,sd
                   ))
means2$out <- factor(means2$out,levels = c("Stay out", "Engage", 
                                           "Success", "Failure"))
means2


# Step 5.2: Primary Analyses (Credit/Blame)------------------------------------
bootResults <- matrix(NA, nrow=sims, ncol=4)
bootResults_suc_means <- matrix(NA, nrow=sims, ncol=6)
bootResults_fail_means <- matrix(NA, nrow=sims, ncol=6)

for(ii in 1:sims){
  resample<-dplyr::sample_n(data,size=nrow(data),replace=T)
  civ_suc_pres <- as.numeric(table(resample$success_civ_credit_o)[3]/
                               sum(table(resample$success_civ_credit_o)))
  civ_suc_both <- as.numeric(table(resample$success_civ_credit_o)[2]/
                               sum(table(resample$success_civ_credit_o)))
  civ_suc_adv <- as.numeric(table(resample$success_civ_credit_o)[1]/
                              sum(table(resample$success_civ_credit_o)))
  
  mil_suc_pres <- as.numeric(table(resample$success_mil_credit_o)[3] /
                               sum(table(resample$success_mil_credit_o)))
  mil_suc_both <- as.numeric(table(resample$success_mil_credit_o)[2] /
                               sum(table(resample$success_mil_credit_o)))
  mil_suc_adv <- as.numeric(table(resample$success_mil_credit_o)[1] /
                              sum(table(resample$success_mil_credit_o)))  
  
  civ_fail_pres <- as.numeric(table(resample$fail_civ_blame_o)[3] /
                                sum(table(resample$fail_civ_blame_o)))
  civ_fail_both <- as.numeric(table(resample$fail_civ_blame_o)[2] /
                                sum(table(resample$fail_civ_blame_o)))
  civ_fail_adv <- as.numeric(table(resample$fail_civ_blame_o)[1] /
                               sum(table(resample$fail_civ_blame_o)))
  
  mil_fail_pres <- as.numeric(table(resample$fail_mil_blame_o)[3] /
                                sum(table(resample$fail_mil_blame_o)))
  mil_fail_both <- as.numeric(table(resample$fail_mil_blame_o)[2] /
                                sum(table(resample$fail_mil_blame_o)))
  mil_fail_adv <- as.numeric(table(resample$fail_mil_blame_o)[1] /
                               sum(table(resample$fail_mil_blame_o)))  
  
  bootResults[ii,1]<-(mil_suc_both-mil_suc_pres)-(civ_suc_both-civ_suc_pres)
  bootResults[ii,2]<-(mil_suc_adv-mil_suc_pres)-(civ_suc_adv-civ_suc_pres)
  bootResults[ii,3]<-(mil_fail_both-mil_fail_pres)-(civ_fail_both-civ_fail_pres)
  bootResults[ii,4]<-(mil_fail_adv-mil_fail_pres)-(civ_fail_adv-civ_fail_pres)
  
  bootResults_suc_means[ii,1]<-civ_suc_pres
  bootResults_suc_means[ii,2]<-civ_suc_both
  bootResults_suc_means[ii,3]<-civ_suc_adv
  
  bootResults_suc_means[ii,4]<-mil_suc_pres
  bootResults_suc_means[ii,5]<-mil_suc_both
  bootResults_suc_means[ii,6]<-mil_suc_adv 
  
  bootResults_fail_means[ii,1]<-civ_fail_pres
  bootResults_fail_means[ii,2]<-civ_fail_both
  bootResults_fail_means[ii,3]<-civ_fail_adv
  
  bootResults_fail_means[ii,4]<-mil_fail_pres
  bootResults_fail_means[ii,5]<-mil_fail_both
  bootResults_fail_means[ii,6]<-mil_fail_adv 
  
}
boots_suc<-bootResults[,1:2]
boots_fail<-bootResults[,3:4]

#Pvalues for success (not displayed visually in manuscript)
plot_density_credit(boots_suc)

#Pvalues for failure (not displayed visually in manuscript)
plot_density_blame(boots_fail)

means2<-data.frame(mean=apply(bootResults_fail_means,2,mean), 
                   condition=c(rep(c("Civilian"),3),rep(c("Military"),3)),
                   out=c("President","Both","Advisors","President",
                         "Both","Advisors"),
                   standard_error=c(
                     sd(bootResults_fail_means[,1]),
                     sd(bootResults_fail_means[,2]),
                     sd(bootResults_fail_means[,3]),
                     sd(bootResults_fail_means[,4]),
                     sd(bootResults_fail_means[,5]),
                     sd(bootResults_fail_means[,6])
                   ))
means2$out <- factor(means2$out,levels = c("President", "Both", "Advisors"))
means2$mean<-means2$mean*100
means2$standard_error<-means2$standard_error*100
means2<-means2[c(4,5,6,1,2,3),]

pdf('results/fig2_b.pdf',width=6.5,height=9)
par(mar=c(5,6,2,2))
test<-barplot(rbind(means2[rev(c(1,2,3)),1],means2[rev(c(4,5,6)),1]),beside=T,
              col=c('#08519c','#c6dbef'),xlim=c(0,100),
              names=rev(c("President",'Both','Advisors')),
              xlab="Percent",horiz=T,las=2)
arrows(means2[rev(c(1,2,3)),1] - means2[rev(c(1,2,3)),4] * 1.96,test[1,], 
       means2[rev(c(1,2,3)),1] + means2[rev(c(1,2,3)),4] * 1.96, lwd = 1.5,
       test[1,],angle = 90,
       code = 3, length = 0.05)
arrows(means2[rev(c(4,5,6)),1] - means2[rev(c(4,5,6)),4] * 1.96,test[2,], 
       means2[rev(c(4,5,6)),1] + means2[rev(c(4,5,6)),4] * 1.96, lwd = 1.5,
       test[2,],angle = 90,
       code = 3, length = 0.05)
text(100,10.75,"Military Advisor",pos=2,col='#08519c')
text(100,11.25,"Civilian Advisor",pos=2,col='#9ecae1')
text(x=rep(0,3),y=test[1,],round(means2[rev(c(1,2,3)),1],1),pos=4,col="white")
text(x=rep(0,3),y=test[2,],round(means2[rev(c(4,5,6)),1],1),pos=4,col="black")
text(100,7.75,"Military Advisor",pos=2,col='#08519c',cex=1.3)
text(100,8.25,"Civilian Advisor",pos=2,col='#9ecae1',cex=1.3)
dev.off()

means2<-data.frame(mean=apply(bootResults_suc_means,2,mean),
                   condition=c(rep(c("Civilian"),3),rep(c("Military"),3)),
                   out=c("President","Both","Advisors","President","Both",
                         "Advisors"),
                   standard_error=c(
                     sd(bootResults_suc_means[,1]),
                     sd(bootResults_suc_means[,2]),
                     sd(bootResults_suc_means[,3]),
                     sd(bootResults_suc_means[,4]),
                     sd(bootResults_suc_means[,5]),
                     sd(bootResults_suc_means[,6])
                   ))

means2$out <- factor(means2$out,levels = c("President", "Both", "Advisors"))
means2$mean<-means2$mean*100
means2$standard_error<-means2$standard_error*100
means2<-means2[c(4,5,6,1,2,3),]

pdf('results/fig2_a.pdf',width=6.5,height=9)
par(mar=c(5,6,2,2))
test<-barplot(rbind(means2[rev(c(1,2,3)),1],means2[rev(c(4,5,6)),1]),beside=T,
              col=c('#08519c','#c6dbef'),xlim=c(0,100),
              names=rev(c("President",'Both','Advisors')),
              xlab="Percent",horiz=T,las=2)
arrows(means2[rev(c(1,2,3)),1] - means2[rev(c(1,2,3)),4] * 1.96,test[1,], 
       means2[rev(c(1,2,3)),1] + means2[rev(c(1,2,3)),4] * 1.96, lwd = 1.5,
       test[1,],angle = 90,
       code = 3, length = 0.05)
arrows(means2[rev(c(4,5,6)),1] - means2[rev(c(4,5,6)),4] * 1.96,test[2,], 
       means2[rev(c(4,5,6)),1] + means2[rev(c(4,5,6)),4] * 1.96, lwd = 1.5,
       test[2,],angle = 90,
       code = 3, length = 0.05)
text(100,10.75,"Military Advisor",pos=2,col='#08519c')
text(100,11.25,"Civilian Advisor",pos=2,col='#9ecae1')
text(x=rep(0,3),y=test[1,],round(means2[rev(c(1,2,3)),1],1),pos=4,col="white")
text(x=rep(0,3),y=test[2,],round(means2[rev(c(4,5,6)),1],1),pos=4,col="black")
text(100,7.75,"Military Advisor",pos=2,col='#08519c',cex=1.3)
text(100,8.25,"Civilian Advisor",pos=2,col='#9ecae1',cex=1.3)
dev.off()


# Step 5.3: Primary Analyses (Trust)-------------------------------------------
bootResults <- matrix(NA, nrow=sims, ncol=7)
for(ii in 1:sims){
  resample<-dplyr::sample_n(data,size=nrow(data),replace=T)
  Ap <- mean(resample$pol_out_pright_d,na.rm=T)
  Am <- mean(resample$mil_out_pright_d,na.rm=T)
  Bp <- mean(resample$civ_in_pright_d,na.rm=T)
  Bm <- mean(resample$mil_in_pright_d,na.rm=T)
  Cp <- mean(resample$pol_success_pright_d,na.rm=T)
  Cm <- mean(resample$mil_success_pright_d,na.rm=T)
  Dp <- mean(resample$civ_fail_pright_d,na.rm=T)
  Dm <- mean(resample$mil_fail_pright_d,na.rm=T)
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}

# Summary statistics on Trust

# Mean trust in the civilian-engage condition
mean(data$civ_in_pright_d, na.rm=T)
# Mean trust in the civilian-failure outcome
mean(data$civ_fail_pright_d, na.rm=T)

# Mean trust in the civilian-engage condition
mean(data$mil_in_pright_d, na.rm=T)
# Mean trust in the military-failure outcome
mean(data$mil_fail_pright_d, na.rm=T)

bootResults<-bootResults[,5:7]
pdf("results/fig_a1_7.pdf",height=5.625,width=4.5)
plot_density_failure_costs(bootResults)
dev.off()

# Trust in advisors
bootResults <- matrix(NA, nrow=sims, ncol=7)
for(ii in 1:sims){
  resample<-dplyr::sample_n(data,size=nrow(data),replace=T)
  Ap <- mean(resample$civ_out_aright_d,na.rm=T)
  Am <- mean(resample$mil_out_aright_d,na.rm=T)
  Bp <- mean(resample$civ_in_aright_d,na.rm=T)
  Bm <- mean(resample$mil_in_aright_d,na.rm=T)
  Cp <- mean(resample$civ_success_aright_d,na.rm=T)
  Cm <- mean(resample$mil_success_aright_d,na.rm=T)
  Dp <- mean(resample$civ_fail_aright_d,na.rm=T)
  Dm <- mean(resample$mil_fail_aright_d,na.rm=T)
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}

# Mean trust in the civilian-engage condition
mean(data$civ_in_aright_d, na.rm=T)
# Mean trust in the civilian-failure outcome
mean(data$civ_fail_aright_d, na.rm=T)
# Mean trust in the civilian-engage condition
mean(data$mil_in_aright_d, na.rm=T)
# Mean Trust in the military-failure outcome
mean(data$mil_fail_aright_d, na.rm=T)

bootResults<-bootResults[,5:7]
pdf("results/fig_a1_8.pdf",height=5.625,width=4.5)
plot_density_failure_costs(bootResults)
dev.off()




# Step 6: Balance and Descriptive (Appendix A1.3)------------------------------

# Descriptive stats (Table A1.1) 
round(table(data$male)/sum(table(data$male)), 3) * 100
round(table(data$party_order)/sum(table(data$party_order)), 3) * 100
round(table(data$education)/sum(table(data$education)), 3) * 100
round(table(data$age_cat2)/sum(table(data$age_cat2)), 3) * 100


data_m<-subset(data, miltreat==1)
data_c<-subset(data, civtreat==1)
sims<-2000
bootResults <- matrix(NA, nrow=sims, ncol=12)
for(ii in 1:sims){
  bootResults[ii,1] <- mean(na.omit(resample_m$male)) - 
    mean(na.omit(resample_p$male))
  bootResults[ii,2] <- mean(na.omit(resample_m$cons)) - 
    mean(na.omit(resample_p$cons))
  bootResults[ii,3] <- mean(na.omit(resample_m$white)) - 
    mean(na.omit(resample_p$white))
  bootResults[ii,4] <- mean(na.omit(resample_m$black)) - 
    mean(na.omit(resample_p$black))
  bootResults[ii,5] <- mean(na.omit(resample_m$milservice)) - 
    mean(na.omit(resample_p$milservice))
  bootResults[ii,6] <- mean(na.omit(resample_m$party_order)) - 
    mean(na.omit(resample_p$party_order))
  bootResults[ii,7] <- mean(na.omit(resample_m$age_cat)) - 
    mean(na.omit(resample_p$age_cat))
  bootResults[ii,8] <- mean(na.omit(resample_m$wealthy)) - 
    mean(na.omit(resample_p$wealthy))
  bootResults[ii,9] <- mean(na.omit(resample_m$vote)) - 
    mean(na.omit(resample_p$vote))
  bootResults[ii,10] <- mean(na.omit(resample_m$news_daily)) - 
    mean(na.omit(resample_p$news_daily))
  bootResults[ii,11] <- mean(na.omit(resample_m$hisp_d)) - 
    mean(na.omit(resample_p$hisp_d))
  bootResults[ii,12] <- mean(na.omit(resample_m$college)) - 
    mean(na.omit(resample_p$college))
  }

# Balance on mil vs. civ treatment
data_m<-subset(data, miltreat==1)
data_c<-subset(data, civtreat==1)
sims <- 4000
bootResults <- matrix(NA, nrow=sims, ncol=12)
for(ii in 1:sims){
  resample_m<-dplyr::sample_n(data_m,size=nrow(data_m),replace=T)
  resample_p<-dplyr::sample_n(data_c,size=nrow(data_c),replace=T)
  bootResults[ii,1] <- mean(na.omit(resample_m$male)) - 
    mean(na.omit(resample_p$male))
  bootResults[ii,2] <- mean(na.omit(resample_m$cons)) - 
    mean(na.omit(resample_p$cons))
  bootResults[ii,3] <- mean(na.omit(resample_m$white)) - 
    mean(na.omit(resample_p$white))
  bootResults[ii,4] <- mean(na.omit(resample_m$black)) - 
    mean(na.omit(resample_p$black))
  bootResults[ii,5] <- mean(na.omit(resample_m$milservice)) - 
    mean(na.omit(resample_p$milservice))
  bootResults[ii,6] <- mean(na.omit(resample_m$party_order)) - 
    mean(na.omit(resample_p$party_order))
  bootResults[ii,7] <- mean(na.omit(resample_m$age_cat)) - 
    mean(na.omit(resample_p$age_cat))
  bootResults[ii,8] <- mean(na.omit(resample_m$wealthy)) - 
    mean(na.omit(resample_p$wealthy))
  bootResults[ii,9] <- mean(na.omit(resample_m$vote)) - 
    mean(na.omit(resample_p$vote))
  bootResults[ii,10] <- mean(na.omit(resample_m$news_daily)) - 
    mean(na.omit(resample_p$news_daily))
  bootResults[ii,11] <- mean(na.omit(resample_m$hisp_d)) - 
    mean(na.omit(resample_p$hisp_d))
  bootResults[ii,12] <- mean(na.omit(resample_m$college)) - 
    mean(na.omit(resample_p$college))
}

NAMES<-c("Male","Conservative","Caucasian","African American",
         "Military Service", "Party Identification",
         "Age Category", "Income over $100K", "Voted",
         "Reads News Daily","Hispanic","Four-year Degree")


pdf("results/fig_a1_1_left.pdf",height=8,width=7)
par(mfrow=c(1,1),mar=c(5,9,3,3))
plot(NULL,
     xlim = (c(-.5,.9)), 
     ylim = c(0,12), 
     axes = F, xlab = NA, ylab = NA) 
  for(ii in 1:12){
    abline(h=ii-1,lty=2,col="gray80")
  }
  for(ii in 1:12){
    polygon(x=density(bootResults[,ii])$x,
            y=(density(bootResults[,ii])$y/max(density(bootResults[,ii])$y))+ii-1,
            col=alpha("#a6bddb",.5),border="black",lwd=1.5)  
  }
  axis(side=2, at=1:length(NAMES)-1, labels=NAMES, las=2)
  axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
  abline(v=0)
  mtext("Difference in Means \n(Military Treatment Group - Civilian Treatment Group)", side = 1, line=3.25,cex=1,las=1)
  for(ii in 1:12){
    text(.55, ii-.75,  
         bquote(p==.(round(pvalue(bootResults[,ii]),3)
         ))
         ,cex=1.5,col=alpha("#377eb8",1),pos=4)
  }
box()
dev.off()


#Balance on outcome treatment
data_in<-subset(data, engage==1)
data_out<-subset(data, out==1)
data_suc<-subset(data, success==1)
data_fail<-subset(data, fail==1)

pdf("results/fig_a1_1_right.pdf",height=8,width=7)
par(mfrow=c(1,1),mar=c(5,9,3,3))
  for(ii in 1:sims){
    resample_m<-dplyr::sample_n(data_in,size=nrow(data_in),replace=T)
    resample_p<-dplyr::sample_n(data_out,size=nrow(data_out),replace=T)
    bootResults[ii,1] <- mean(na.omit(resample_m$male)) - 
      mean(na.omit(resample_p$male))
    bootResults[ii,2] <- mean(na.omit(resample_m$cons)) - 
      mean(na.omit(resample_p$cons))
    bootResults[ii,3] <- mean(na.omit(resample_m$white)) - 
      mean(na.omit(resample_p$white))
    bootResults[ii,4] <- mean(na.omit(resample_m$black)) - 
      mean(na.omit(resample_p$black))
    bootResults[ii,5] <- mean(na.omit(resample_m$milservice)) - 
      mean(na.omit(resample_p$milservice))
    bootResults[ii,6] <- mean(na.omit(resample_m$party_order)) - 
      mean(na.omit(resample_p$party_order))
    bootResults[ii,7] <- mean(na.omit(resample_m$age_cat)) - 
      mean(na.omit(resample_p$age_cat))
    bootResults[ii,8] <- mean(na.omit(resample_m$wealthy)) - 
      mean(na.omit(resample_p$wealthy))
    bootResults[ii,9] <- mean(na.omit(resample_m$vote)) - 
      mean(na.omit(resample_p$vote))
    bootResults[ii,10] <- mean(na.omit(resample_m$news_daily)) - 
      mean(na.omit(resample_p$news_daily))
    bootResults[ii,11] <- mean(na.omit(resample_m$hisp_d)) - 
      mean(na.omit(resample_p$hisp_d))
    bootResults[ii,12] <- mean(na.omit(resample_m$college)) - 
      mean(na.omit(resample_p$college))
  }

plot(NULL,
     xlim = (c(-.5,.8)), 
     ylim = c(0,11.75), 
     axes = F, xlab = NA, ylab = NA) 
  for(ii in 1:12){
    abline(h=ii-1,lty=2,col="gray80")
  }
  for(ii in 1:12){
    polygon(x=density(bootResults[,ii])$x,
            y=(density(bootResults[,ii])$y/
                 max(density(bootResults[,ii])$y))+ii-1,
            col=alpha("#a6bddb",.5),border="#a6bddb",lwd=1.5)  
  }
  axis(side=2, at=1:length(NAMES)-1, labels=NAMES, las=2)
  axis(side=1, las=1,cex.axis=1.25,mgp=c(3,.8,0))
  abline(v=0)
  mtext("Difference in Means \n(`Stay Out' is Reference Category)", 
        side = 1, line=3.25,cex=1,las=1)
  for(ii in 1:12){
    text(.55, ii-.25,  
         bquote(p==.(round(pvalue(bootResults[,ii]),3)
         ))
         ,cex=1,col=alpha("#377eb8",1),pos=4)
  }
  box()
  text(-.55,11.75,"Engage",col="#377eb8",pos=4)
  

  for(ii in 1:sims){
    resample_m<-dplyr::sample_n(data_suc,size=nrow(data_suc),replace=T)
    resample_p<-dplyr::sample_n(data_out,size=nrow(data_out),replace=T)
    bootResults[ii,1] <- mean(na.omit(resample_m$male)) - 
      mean(na.omit(resample_p$male))
    bootResults[ii,2] <- mean(na.omit(resample_m$cons)) - 
      mean(na.omit(resample_p$cons))
    bootResults[ii,3] <- mean(na.omit(resample_m$white)) - 
      mean(na.omit(resample_p$white))
    bootResults[ii,4] <- mean(na.omit(resample_m$black)) - 
      mean(na.omit(resample_p$black))
    bootResults[ii,5] <- mean(na.omit(resample_m$milservice)) - 
      mean(na.omit(resample_p$milservice))
    bootResults[ii,6] <- mean(na.omit(resample_m$party_order)) - 
      mean(na.omit(resample_p$party_order))
    bootResults[ii,7] <- mean(na.omit(resample_m$age_cat)) - 
      mean(na.omit(resample_p$age_cat))
    bootResults[ii,8] <- mean(na.omit(resample_m$wealthy)) - 
      mean(na.omit(resample_p$wealthy))
    bootResults[ii,9] <- mean(na.omit(resample_m$vote)) - 
      mean(na.omit(resample_p$vote))
    bootResults[ii,10] <- mean(na.omit(resample_m$news_daily)) - 
      mean(na.omit(resample_p$news_daily))
    bootResults[ii,11] <- mean(na.omit(resample_m$hisp_d)) - 
      mean(na.omit(resample_p$hisp_d))
    bootResults[ii,12] <- mean(na.omit(resample_m$college)) - 
      mean(na.omit(resample_p$college))
  }

  for(ii in 1:12){
    polygon(x=density(bootResults[,ii])$x,
            y=(density(bootResults[,ii])$y/max(density(bootResults[,ii])$y))+ii-1,
            col=alpha("#4daf4a",.5),border="#4daf4a",lwd=1.5)  
  }
  for(ii in 1:12){
    text(.55, ii-.5,  
       bquote(p==.(round(pvalue(bootResults[,ii]),3)))
         ,cex=1,col=alpha("#4daf4a",1),pos=4)
  }
  text(-.55,11.5,"Success",col="#4daf4a",pos=4)

  for(ii in 1:sims){
    resample_m<-dplyr::sample_n(data_fail,size=nrow(data_fail),replace=T)
    resample_p<-dplyr::sample_n(data_out,size=nrow(data_out),replace=T)
    bootResults[ii,1] <- mean(na.omit(resample_m$male)) - 
      mean(na.omit(resample_p$male))
    bootResults[ii,2] <- mean(na.omit(resample_m$cons)) - 
      mean(na.omit(resample_p$cons))
    bootResults[ii,3] <- mean(na.omit(resample_m$white)) - 
      mean(na.omit(resample_p$white))
    bootResults[ii,4] <- mean(na.omit(resample_m$black)) - 
      mean(na.omit(resample_p$black))
    bootResults[ii,5] <- mean(na.omit(resample_m$milservice)) - 
      mean(na.omit(resample_p$milservice))
    bootResults[ii,6] <- mean(na.omit(resample_m$party_order)) - 
      mean(na.omit(resample_p$party_order))
    bootResults[ii,7] <- mean(na.omit(resample_m$age_cat)) - 
      mean(na.omit(resample_p$age_cat))
    bootResults[ii,8] <- mean(na.omit(resample_m$wealthy)) - 
      mean(na.omit(resample_p$wealthy))
    bootResults[ii,9] <- mean(na.omit(resample_m$vote)) - 
      mean(na.omit(resample_p$vote))
    bootResults[ii,10] <- mean(na.omit(resample_m$news_daily)) - 
      mean(na.omit(resample_p$news_daily))
    bootResults[ii,11] <- mean(na.omit(resample_m$hisp_d)) - 
      mean(na.omit(resample_p$hisp_d))
    bootResults[ii,12] <- mean(na.omit(resample_m$college)) - 
      mean(na.omit(resample_p$college))
  }
  
  for(ii in 1:12){
    polygon(x=density(bootResults[,ii])$x,
            y=(density(bootResults[,ii])$y/max(density(bootResults[,ii])$y))+ii-1,
            col=alpha("#e41a1c",.5),border="#e41a1c",lwd=1.5)  
  }
  for(ii in 1:12){
    text(.55, ii-.75,  
         bquote(p==.(round(pvalue(bootResults[,ii]),3)
         ))
         ,cex=1,col=alpha("#e41a1c",1),pos=4)
  }
  text(-.55,11.25,"Fail",col="#e41a1c",pos=4)
  text(-.55,12,"Treatment Condition:",col="Black",pos=4)
dev.off()




# Step 7: Regression Analysis (Appendix A1.4)----------------------------------


# Step 7.1: Regressions on approval and trust
# Approval (Table A1.2)
m1 <- lm(approve~miltreat+hisp_d+milservice+male+age_cat,
         data=subset(data,out==1))
m2 <- lm(approve~miltreat+hisp_d+milservice+male+age_cat,
         data=subset(data,engage==1))
m3 <- lm(approve~miltreat+hisp_d+milservice+male+age_cat,
         data=subset(data,success==1))
m4 <- lm(approve~miltreat+hisp_d+milservice+male+age_cat,
         data=subset(data,fail==1))
export_summs(m1, m2, m3, m4, scale = F, 
             transform.response = F,to.file ='docx', digits=3,
             file.name="results/table_a1_2.docx")
# Trust (Table A1.4)
trust_pres1<-lm(pright~fail+hisp_d+milservice+male+age_cat,
                data=subset(data,miltreat==1 & (fail==1 | engage==1)))
trust_pres2<-lm(pright~fail+hisp_d+milservice+male+age_cat,
                data=subset(data,civtreat==1 & (fail==1 | engage==1)))
data$fail_miltreat<-data$fail*data$miltreat
trust_pres3<-lm(pright~fail+miltreat+fail_miltreat+hisp_d+
                  milservice+male+age_cat,
                data=subset(data,fail==1 | engage==1))

trust_adv1<-lm(aright~fail+hisp_d+milservice+male+age_cat,
                data=subset(data,miltreat==1 & (fail==1 | engage==1)))
trust_adv2<-lm(aright~fail+hisp_d+milservice+male+age_cat,
                data=subset(data,civtreat==1 & (fail==1 | engage==1)))
trust_adv3<-lm(aright~fail+miltreat+fail_miltreat+hisp_d+
                  milservice+male+age_cat,
                data=subset(data,fail==1 | engage==1))
export_summs(trust_pres1,trust_pres2,trust_pres3,
             trust_adv1,trust_adv2,trust_adv3, scale = F, 
             transform.response = F,to.file ='docx',digits=3,
             file.name="results/table_a1_3.docx")


# Credit/blame (Table A1.4)
mlogit1<-vglm(blame~miltreat,data=subset(data,fail==1),family=multinomial)
mlogit2<-vglm(credit~miltreat,data=subset(data,success==1),family=multinomial)
mlogit3<-vglm(blame~miltreat+hisp_d+milservice+male+age_cat,
              data=subset(data,fail==1),family=multinomial)
mlogit4<-vglm(credit~miltreat+hisp_d+milservice+male+age_cat,
              data=subset(data,success==1),family=multinomial)
summary(mlogit1)
export_summs(mlogit1,mlogit3,mlogit2,mlogit4, scale = F, 
             transform.response = F,to.file ='docx',digits=3,
             file.name="results/table_a1_4.docx")


# Step 7.2: Sensitivity to speeders (approval)
outs_coef<-c()
outs_se<-c()
ins_coef<-c()
ins_se<-c()
sucs_coef<-c()
sucs_se<-c()
fails_coef<-c()
fails_se<-c()
for(ii in seq(1,121,10)){
  s3m1<-lm(approve~miltreat,data=subset(data,out==1 & duration_in_seconds>=ii))
  outs_coef<-append(outs_coef,s3m1$coefficients[2])
  outs_se<-append(outs_se,sqrt(diag(vcov(s3m1)))[2]) 
  s3m2<-lm(approve~miltreat,
           data=subset(data,engage==1 & duration_in_seconds>=ii))
  ins_coef<-append(ins_coef,s3m2$coefficients[2])
  ins_se<-append(ins_se,sqrt(diag(vcov(s3m2)))[2])
  s3m3<-lm(approve~miltreat,
           data=subset(data,success==1 & duration_in_seconds>=ii))
  sucs_coef<-append(sucs_coef,s3m3$coefficients[2])
  sucs_se<-append(sucs_se,sqrt(diag(vcov(s3m3)))[2])
  s3m4<-lm(approve~miltreat,data=subset(data,fail==1 & duration_in_seconds>=ii))
  fails_coef<-append(fails_coef,s3m4$coefficients[2])
  fails_se<-append(fails_se,sqrt(diag(vcov(s3m4)))[2])
}
pdf("results/fig_a1_2.pdf", height=10, width=10)
par(NULL, 
    mar=c(4,4,2,2),
    mfrow=c(2,2))
plot(NULL,
     xlim = c(0,13), 
     ylim=c(-.25,.25),
     axes = F, xlab = NA, ylab = NA) 
rect(-1,mean(outs_coef-(1.96*outs_se)),14,mean(outs_coef+(1.96*outs_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
rect(-1,mean(outs_coef-(1.645*outs_se)),14,mean(outs_coef+(1.645*outs_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
for(ii in 1:13){
  lines(c(ii,ii),
        c(outs_coef[ii]-(1.96*outs_se[ii]),outs_coef[ii]+(1.96*outs_se[ii])),
        col="black", lwd=1.75)
}
points(seq(1,13,1),outs_coef,bg=alpha("gray80",1), col=alpha("black",1),
       lwd=1.75,cex=1, pch=21)
abline(h=0, col="#b2182b",lty=5, lwd=1)
axis(2,cex.axis=.85,las=1)
axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
title("Stay Out")
mtext(side = 1, "Seconds", line =2.5,cex=1) 

plot(NULL,
     xlim = c(0,13), 
     ylim=c(-.25,.25),
     axes = F, xlab = NA, ylab = NA) 
rect(-1,mean(ins_coef-(1.96*ins_se)),14,mean(ins_coef+(1.96*ins_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
rect(-1,mean(ins_coef-(1.645*ins_se)),14,mean(ins_coef+(1.645*ins_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
for(ii in 1:13){
  lines(c(ii,ii),
        c(ins_coef[ii]-(1.96*ins_se[ii]),ins_coef[ii]+(1.96*ins_se[ii])),
        col="black", lwd=1.75)
}
points(seq(1,13,1),ins_coef,bg=alpha("gray80",1), col=alpha("black",1),
       lwd=1.75,cex=1, pch=21)
abline(h=0, col="#b2182b",lty=5, lwd=1)
axis(2,cex.axis=.85,las=1)
axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
title("Engage")
mtext(side = 1, "Seconds", line =2.5,cex=1) # label bottom axis

plot(NULL,
     xlim = c(0,13), 
     ylim=c(-.25,.25),
     axes = F, xlab = NA, ylab = NA) 
rect(-1,mean(sucs_coef-(1.96*sucs_se)),14,mean(sucs_coef+(1.96*sucs_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
rect(-1,mean(sucs_coef-(1.645*sucs_se)),14,mean(sucs_coef+(1.645*sucs_se)),
     col = rgb(0.5,0.5,0.5,1/9),border=F)
for(ii in 1:13){
  lines(c(ii,ii),
        c(sucs_coef[ii]-(1.96*sucs_se[ii]),sucs_coef[ii]+(1.96*sucs_se[ii])),
        col="black", lwd=1.75)
}
points(seq(1,13,1),sucs_coef,bg=alpha("gray80",1), col=alpha("black",1),
       lwd=1.75,cex=1, pch=21)
abline(h=0, col="#b2182b",lty=5, lwd=1)
axis(2,cex.axis=.85,las=1)
axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
title("Success")
mtext(side = 1, "Seconds", line =2.5,cex=1) # label bottom axis

plot(NULL,
     xlim = c(0,13), 
     ylim=c(-.25,.25),
     axes = F, xlab = NA, ylab = NA) 
rect(-1,mean(fails_coef-(1.96*fails_se)),14,
     mean(fails_coef+(1.96*fails_se)),col = rgb(0.5,0.5,0.5,1/9),border=F)
rect(-1,mean(fails_coef-(1.645*fails_se)),
     14,mean(fails_coef+(1.645*fails_se)),col = rgb(0.5,0.5,0.5,1/9),border=F)
for(ii in 1:13){
  lines(c(ii,ii),
        c(fails_coef[ii]-(1.96*fails_se[ii]),
          fails_coef[ii]+(1.96*fails_se[ii])),
        col="black", lwd=1.75)
}
points(seq(1,13,1),fails_coef,bg=alpha("gray80",1), 
       col=alpha("black",1), lwd=1.75,cex=1, pch=21)
abline(h=0, col="#b2182b",lty=5, lwd=1)
axis(2,cex.axis=.85,las=1)
axis(1,cex.axis=.85,las=1,at=seq(1,13,1), labels=seq(1,121,10)-1)
title("Failure")
mtext(side = 1, "Seconds", line =2.5,cex=1) 
dev.off()


# Step 7.3: Sensitivity to speeders (credit and blame)
suc_coef_both<-c()
suc_coef_adv<-c()
suc_se_both<-c()
suc_se_adv<-c()
fail_coef_both<-c()
fail_coef_adv<-c()
fail_se_both<-c()
fail_se_adv<-c()
for(ii in seq(1,121,10)){
  fit1<-vglm(credit~miltreat,data=subset(data,success==1 &  duration_in_seconds>=ii),family=multinomial)
  suc_coef_adv<-append(suc_coef_adv,fit1@coefficients[3])
  suc_coef_both<-append(suc_coef_both,fit1@coefficients[4])
  suc_se_adv<-append(suc_se_adv,sqrt(diag(vcov(fit1)))[3]) 
  suc_se_both<-append(suc_se_both,sqrt(diag(vcov(fit1)))[4]) 
  
  fit2<-vglm(blame~miltreat,data=subset(data,fail==1 & duration_in_seconds>=ii),family=multinomial)
  fail_coef_adv<-append(fail_coef_adv,fit2@coefficients[3])
  fail_coef_both<-append(fail_coef_both,fit2@coefficients[4])
  fail_se_adv<-append(fail_se_adv,sqrt(diag(vcov(fit2)))[3]) 
  fail_se_both<-append(fail_se_both,sqrt(diag(vcov(fit2)))[4]) 
}
fail_coef_both<-as.numeric(fail_coef_both)
fail_se_both<-as.numeric(fail_se_both)
fail_coef_adv<-as.numeric(fail_coef_adv)
fail_se_adv<-as.numeric(fail_se_adv)
suc_coef_adv<-as.numeric(suc_coef_adv)
suc_coef_both<-as.numeric(suc_coef_both)
suc_se_adv<-as.numeric(suc_se_adv)
suc_se_both<-as.numeric(suc_se_both)

pdf("results/fig_a1_3_blame.pdf", height=6, width=10)
par(mfrow=c(1,2))
plot(NULL,
     xlim = c(0,13), 
     ylim=c(-2,2),
     axes = F, xlab = NA, ylab = NA) 
  rect(-1,mean(fail_coef_adv-(1.96*fail_se_adv)),13,
       mean(fail_coef_adv+(1.96*fail_se_adv)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  rect(-1,mean(fail_coef_adv-(1.645*fail_se_adv)),13,
       mean(fail_coef_adv+(1.645*fail_se_adv)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  for(ii in 1:13){
    lines(c(ii,ii),
          c(fail_coef_adv[ii]-(1.96*fail_se_adv[ii]),
            fail_coef_adv[ii]+(1.96*fail_se_adv[ii])),
          col="black", lwd=1.75)
  }
  points(seq(1,13,1),fail_coef_adv,bg=alpha("gray80",1), 
         col=alpha("black",1),lwd=1.75,cex=1, pch=21)
  abline(h=0, col="#b2182b",lty=5, lwd=1)
  axis(2,cex.axis=.85,las=1)
  axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
  title("Blame Advisors")
  mtext(side = 1, "Seconds", line =2.5,cex=1)
  
plot(NULL,
     xlim = c(0,13), 
     ylim=c(-2,2),
     axes = F, xlab = NA, ylab = NA) 
  rect(-1,mean(fail_coef_both-(1.96*fail_se_both)),13,
       mean(fail_coef_both+(1.96*fail_se_both)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  rect(-1,mean(fail_coef_both-(1.645*fail_se_both)),13,
       mean(fail_coef_both+(1.645*fail_se_both)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  for(ii in 1:13){
    lines(c(ii,ii),
          c(fail_coef_both[ii]-(1.96*fail_se_both[ii]),
            fail_coef_both[ii]+(1.96*fail_se_both[ii])),
          col="black", lwd=1.75)
  }
  points(seq(1,13,1),fail_coef_both,bg=alpha("gray80",1), 
         col=alpha("black",1),lwd=1.75,cex=1, pch=21)
  abline(h=0, col="#b2182b",lty=5, lwd=1)
  axis(2,cex.axis=.85,las=1)
  axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
  title("Blame Advisors and President Equally")
  mtext(side = 1, "Seconds", line =2.5,cex=1)
dev.off()
  
pdf("results/fig_a1_3_credit.pdf", height=6, width=10)
par(mfrow=c(1,2))
plot(NULL,
     xlim = c(0,13), 
     ylim=c(-2,2),
     axes = F, xlab = NA, ylab = NA) 
  rect(-1,mean(suc_coef_adv-(1.96*suc_se_adv)),
       13,mean(suc_coef_adv+(1.96*suc_se_adv)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  rect(-1,mean(suc_coef_adv-(1.645*suc_se_adv)),
       13,mean(suc_coef_adv+(1.645*suc_se_adv)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  for(ii in 1:13){
    lines(c(ii,ii),
          c(suc_coef_adv[ii]-(1.96*suc_se_adv[ii]),
            suc_coef_adv[ii]+(1.96*suc_se_adv[ii])),
          col="black", lwd=1.75)
  }
  points(seq(1,13,1),suc_coef_adv,bg=alpha("gray80",1), 
         col=alpha("black",1),lwd=1.75,cex=1, pch=21)
  abline(h=0, col="#b2182b",lty=5, lwd=1)
  axis(2,cex.axis=.85,las=1)
  axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
  title("Credit Advisors")
  mtext(side = 1, "Seconds", line =2.5,cex=1)
  
plot(NULL,
     xlim = c(0,13), 
     ylim=c(-2,2),
     axes = F, xlab = NA, ylab = NA) 
  rect(-1,mean(suc_coef_both-(1.96*suc_se_both)),
       13,mean(suc_coef_both+(1.96*suc_se_both)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  rect(-1,mean(suc_coef_both-(1.645*suc_se_both)),
       13,mean(suc_coef_both+(1.645*suc_se_both)),
       col = rgb(0.5,0.5,0.5,1/9),border=F)
  for(ii in 1:13){
    lines(c(ii,ii),
          c(suc_coef_both[ii]-(1.96*suc_se_both[ii]),
            suc_coef_both[ii]+(1.96*suc_se_both[ii])),
          col="black", lwd=1.75)
  }
  points(seq(1,13,1),suc_coef_both,bg=alpha("gray80",1), 
         col=alpha("black",1),lwd=1.75,cex=1, pch=21)
  abline(h=0, col="#b2182b",lty=5, lwd=1)
  axis(2,cex.axis=.85,las=1)
  axis(1,cex.axis=.85,las=1,at=seq(1,13,1),labels=seq(1,121,10)-1)
  title("Credit Advisors and President Equally")
  mtext(side = 1, "Seconds", line =2.5,cex=1)
dev.off()
  




# Step 8: Partisanship Effects (Appendix A1.5)---------------------------------


# Step 8.1: Partisanship Regressions ------------------------------------------
data$int_party_miltreat<-data$party_order*data$miltreat

#Interaction Term
sm1<-lm(approve~miltreat+party_order+int_party_miltreat,data=data)
sm2<-lm(approve~miltreat+party_order+int_party_miltreat,data=subset(data,out==1))
sm3<-lm(approve~miltreat+party_order+int_party_miltreat,data=subset(data,engage==1))
sm4<-lm(approve~miltreat+party_order+int_party_miltreat,data=subset(data,success==1))
sm5<-lm(approve~miltreat+party_order+int_party_miltreat,data=subset(data,fail==1))
export_summs(sm2,sm3,sm4,sm5,sm1, scale = F, 
             transform.response = F,to.file ='docx',digits=3,
             file.name="results/table_a1_5.docx")



# Step 8.2: Bar plots (Fig A1.4) ----------------------------------------------

# Democrats
sims <- 4000
bootResults <- matrix(NA, nrow=sims, ncol=7)
bootMeans <- matrix(NA, nrow=sims, ncol=8)
for(ii in 1:sims){
  resample<-dplyr::sample_n(subset(data, party_order<0),size=nrow(data),replace=T)
  Ap <- mean(resample$out_civ_approve_d,na.rm=T)
  Am <- mean(resample$out_mil_approve_d,na.rm=T)
  Bp <- mean(resample$engage_civ_approve_d,na.rm=T)
  Bm <- mean(resample$engage_mil_approve_d,na.rm=T)
  Cp <- mean(resample$success_civ_approve_d,na.rm=T)
  Cm <- mean(resample$success_mil_approve_d,na.rm=T)
  Dp <- mean(resample$fail_civ_approve_d,na.rm=T)
  Dm <- mean(resample$fail_mil_approve_d,na.rm=T)
  bootMeans[ii,1]<-Ap
  bootMeans[ii,2]<-Am
  bootMeans[ii,3]<-Bp
  bootMeans[ii,4]<-Bm
  bootMeans[ii,5]<-Cp
  bootMeans[ii,6]<-Cm
  bootMeans[ii,7]<-Dp
  bootMeans[ii,8]<-Dm
  
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}

means2<-data.frame(mean=apply(bootMeans,2,mean),
                   condition=rep(c("Civilian","Military"),4),
                   out=c("Stay out","Stay out","Engage","Engage","Success","Success","Failure","Failure"),
                   standard_error=apply(bootMeans,2,sd
                   ))
means2$out <- factor(means2$out,levels = c("Stay out", "Engage", "Success", "Failure"))
means2$mean<-means2$mean*100
means2$standard_error<-means2$standard_error*100

pdf('results/fig_a1_4a.pdf',width=6.624,height=7.008)
  par(mar=c(5,6,2,2))
  test<-barplot(rbind(means2[rev(c(2,4,6,8)),1],means2[rev(c(1,3,5,7)),1]),beside=T,
                col=c('#08519c','#c6dbef'),xlim=c(0,100),
                names=rev(c("Stay out",'Engage','Success','Failure')),
                xlab="Percent Approving of Presidential Decision-Making",horiz=T,las=2)
  arrows(means2[rev(c(1,3,5,7)),1] - means2[rev(c(1,3,5,7)),4] * 1.96,test[2,], 
         means2[rev(c(1,3,5,7)),1] + means2[rev(c(1,3,5,7)),4] * 1.96, lwd = 1.5,
         test[2,],angle = 90,
         code = 3, length = 0.05)
  arrows(means2[rev(c(2,4,6,8)),1] - means2[rev(c(2,4,6,8)),4] * 1.96,test[1,], 
         means2[rev(c(2,4,6,8)),1] + means2[rev(c(2,4,6,8)),4] * 1.96, lwd = 1.5,
         test[1,],angle = 90,
         code = 3, length = 0.05)
  text(100,10.75,"Military Advisor",pos=2,col='#08519c')
  text(100,11.25,"Civilian Advisor",pos=2,col='#9ecae1')
  text(x=rep(0,4),y=test[1,],round(means2[rev(c(2,4,6,8)),1],1),pos=4,col="white")
  text(x=rep(0,4),y=test[2,],round(means2[rev(c(1,3,5,7)),1],1),pos=4,col="black")
  title("(a) Presidential Approval, Democrats Only")
dev.off()


# Republicans
sims <- 4000
bootResults <- matrix(NA, nrow=sims, ncol=7)
bootMeans <- matrix(NA, nrow=sims, ncol=8)
for(ii in 1:sims){
  resample<-dplyr::sample_n(subset(data, party_order>0),size=nrow(data),replace=T)
  Ap <- mean(resample$out_civ_approve_d,na.rm=T)
  Am <- mean(resample$out_mil_approve_d,na.rm=T)
  Bp <- mean(resample$engage_civ_approve_d,na.rm=T)
  Bm <- mean(resample$engage_mil_approve_d,na.rm=T)
  Cp <- mean(resample$success_civ_approve_d,na.rm=T)
  Cm <- mean(resample$success_mil_approve_d,na.rm=T)
  Dp <- mean(resample$fail_civ_approve_d,na.rm=T)
  Dm <- mean(resample$fail_mil_approve_d,na.rm=T)
  bootMeans[ii,1]<-Ap
  bootMeans[ii,2]<-Am
  bootMeans[ii,3]<-Bp
  bootMeans[ii,4]<-Bm
  bootMeans[ii,5]<-Cp
  bootMeans[ii,6]<-Cm
  bootMeans[ii,7]<-Dp
  bootMeans[ii,8]<-Dm
  
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}

means2<-data.frame(mean=apply(bootMeans,2,mean),
                   condition=rep(c("Civilian","Military"),4),
                   out=c("Stay out","Stay out","Engage","Engage","Success","Success","Failure","Failure"),
                   standard_error=apply(bootMeans,2,sd
                   ))
means2$out <- factor(means2$out,levels = c("Stay out", "Engage", "Success", "Failure"))
means2$mean<-means2$mean*100
means2$standard_error<-means2$standard_error*100

pdf('results/fig_a1_4b.pdf',width=6.624,height=7.008)
  par(mar=c(5,6,2,2))
  test<-barplot(rbind(means2[rev(c(2,4,6,8)),1],means2[rev(c(1,3,5,7)),1]),
                beside=T, col=c('#08519c','#c6dbef'),xlim=c(0,100),
                names=rev(c("Stay out",'Engage','Success','Failure')),
                xlab="Percent Approving of Presidential Decision-Making",horiz=T,las=2)
  arrows(means2[rev(c(1,3,5,7)),1] - means2[rev(c(1,3,5,7)),4] * 1.96,test[2,], 
         means2[rev(c(1,3,5,7)),1] + means2[rev(c(1,3,5,7)),4] * 1.96, lwd = 1.5,
         test[2,],angle = 90,
         code = 3, length = 0.05)
  arrows(means2[rev(c(2,4,6,8)),1] - means2[rev(c(2,4,6,8)),4] * 1.96,test[1,], 
         means2[rev(c(2,4,6,8)),1] + means2[rev(c(2,4,6,8)),4] * 1.96, lwd = 1.5,
         test[1,],angle = 90,
         code = 3, length = 0.05)
  text(100,10.75,"Military Advisor", pos=2,col='#08519c')
  text(100,11.25,"Civilian Advisor", pos=2,col='#9ecae1')
  text(x=rep(0,4),y=test[1,], round(means2[rev(c(2,4,6,8)),1],1),
       pos=4,col="white")
  text(x=rep(0,4),y=test[2,], round(means2[rev(c(1,3,5,7)),1],1),
       pos=4,col="black")
  title("(b) Presidential Approval, Republicans Only")
dev.off()


#Supplemental: basic t-tests for ordinal indicator (footnote 7)
t.test(data$approve_o[data$miltreat==1 & data$out==1],
       data$approve_o[data$civtreat==1 & data$out==1])
t.test(data$approve_o[data$miltreat==1 & data$fail==1],
       data$approve_o[data$civtreat==1 & data$fail==1])
t.test(data$approve_o[data$miltreat==1 & data$engage==1],
       data$approve_o[data$civtreat==1 & data$engage==1])
t.test(data$approve_o[data$miltreat==1 & data$success==1],
       data$approve_o[data$civtreat==1 & data$success==1])

sims<-2000
bootResults <- matrix(NA, nrow=sims, ncol=7)
for(ii in 1:sims){
  resample<-dplyr::sample_n(data,size=nrow(data),replace=T)
  Ap <- mean(resample$civout_approve,na.rm=T)
  Am <- mean(resample$milout_approve,na.rm=T)
  Bp <- mean(resample$civen_approve,na.rm=T)
  Bm <- mean(resample$milen_approve,na.rm=T)
  Cp <- mean(resample$civsuccess_approve,na.rm=T)
  Cm <- mean(resample$milsuccess_approve,na.rm=T)
  Dp <- mean(resample$civfail_approve,na.rm=T)
  Dm <- mean(resample$milfail_approve,na.rm=T)
  bootResults[ii,1]<-Am-Ap
  bootResults[ii,2]<-Bm-Bp
  bootResults[ii,3]<-Cm-Cp
  bootResults[ii,4]<-Dm-Dp
  
  bootResults[ii,5]<-Dm-Bm
  bootResults[ii,6]<-Dp-Bp
  bootResults[ii,7]<-(Dm-Bm)-(Dp-Bp)
}
summary(bootResults)

par(mfrow=c(4,1),las=1,mar=c(4,4,2,2))
plot_density_o(bootResults)



