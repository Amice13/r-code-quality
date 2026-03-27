rm(list=ls())
library(dplyr)
library(ggplot2)
library(cregg)
library(rio)
library(lubridate)

#Set working directory (change this to your local directory)
setwd('~/Dropbox/cm_project2/KenwickMaxey_JCR2024_replication/')

##############################################################################
###Experiment 1: Conjoint-only
##############################################################################
load('survey1.RData')

res$agency <- relevel(res$agency, "The U.S. Army") #releveling for plot

amces <- cregg::cj(
  res,
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "amce"
) #Calculate AMCEs

#Re-arranging order of attributes for plot
amces[1:4,] <- amces[c(4,2,3,1),]
amces$id <- 1+nrow(amces) - seq(1:nrow(amces))
amces[nrow(amces) + 1,] <- c(rep(NA,10),6.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),12.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),18.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),24.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),30.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),37.5)
amces[nrow(amces) + 1,] <- c(rep(NA,10),41.5)
amces$id[5:11] <- rev(amces$id[5:11])
amces <- dplyr::arrange(amces,id)

####
#Fig 1
####
pdf("figures/fig1.pdf",height=8,width=15)
par(mfrow=c(1,2),mar=c(5,14.5,4,1))
plot(NULL,
     xlim = c(-.2,.2), 
     ylim = c(1,47), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="orangered",lty=1, lwd=1)
for (i in 1:47){
  abline(h=i,lty=1,col="gray90",lwd=.5)
  lines(c(amces$"estimate"[i]-(1.645*amces$"std.error"[i]),
          amces$"estimate"[i]+(1.645*amces$"std.error"[i])),
        c(i,i),col="black", lwd=5)
  lines(c(amces$"estimate"[i]-(1.96*amces$"std.error"[i]),
          amces$"estimate"[i]+(1.96*amces$"std.error"[i])),
        c(i,i),col="black", lwd=3)
}
points(amces$"estimate",seq(1,length(amces$"estimate"),1),
       bg=c("gray80"), col="black",  cex=1, pch=21,lwd=2 )
text(-.2,5,"Effect on \nTerrorism",pos=4,cex=1)
abline(h=7,lwd=2)
text(-.2,12,"Effect on \nDrug Trafficking",pos=4,cex=.75)
abline(h=14,lwd=2)
text(-.2,19,"Effect on \nHuman Trafficking",pos=4,cex=.75)
abline(h=21,lwd=2)
text(-.2,26,"Effect on \nIllegal Immigration",pos=4,cex=.75)
abline(h=28,lwd=2)
text(-.2,34,"Budgetary Cost",pos=4,cex=.75)
abline(h=35,lwd=2)
text(-.2,42,"Strategy",pos=4,cex=.75)
abline(h=43,lwd=2)
text(-.2,47,"Agency",pos=4,cex=.75)
axis(1,cex.axis=.8)
axis(2,at=seq(1,47,1),labels=amces$level[1:47],las=2,cex.axis=.75)
mtext(side = 1, "AMCE", line =3,cex=.8)
box()


mms <- cregg::cj(
  res,
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm"
)
mms[1:4,] <- mms[c(4,2,3,1),]

#Again, re-arranging order for plot
mms$id <- 1+nrow(mms) - seq(1:nrow(mms))
mms[nrow(mms) + 1,] <- c(rep(NA,10),6.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),12.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),18.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),24.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),30.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),37.5)
mms[nrow(mms) + 1,] <- c(rep(NA,10),41.5)
mms$id[5:11] <- rev(mms$id[5:11])
mms <- dplyr::arrange(mms,id)

plot(NULL,
     xlim = c(.35,.65), 
     ylim = c(1,47), 
     axes = F, xlab = NA, ylab = NA)
abline(v=.5, col="orangered",lty=1, lwd=1)
for (i in 1:47){
  abline(h=i,lty=1,col="gray90",lwd=.5)
  lines(c(mms$"estimate"[i]-(1.645*mms$"std.error"[i]),
          mms$"estimate"[i]+(1.645*mms$"std.error"[i])),
        c(i,i),col="black", lwd=5)
  lines(c(mms$"estimate"[i]-(1.96*mms$"std.error"[i]),
          mms$"estimate"[i]+(1.96*mms$"std.error"[i])),
        c(i,i),col="black", lwd=3)
}
points(mms$"estimate",seq(1,length(mms$"estimate"),1),
       bg=c("gray80"), col="black",  cex=1, pch=21,lwd=2 )
text(.35,5,"Effect on \nTerrorism",pos=4,cex=.75)
abline(h=7,lwd=2)
text(.35,12,"Effect on \nDrug Trafficking",pos=4,cex=.75)
abline(h=14,lwd=2)
text(.35,19,"Effect on \nHuman Trafficking",pos=4,cex=.75)
abline(h=21,lwd=2)
text(.35,26,"Effect on \nIllegal Immigration",pos=4,cex=.75)
abline(h=28,lwd=2)
text(.35,34,"Budgetary Cost",pos=4,cex=.75)
abline(h=35,lwd=2)
text(.35,42,"Strategy",pos=4,cex=.75)
abline(h=43,lwd=2)
text(.35,47,"Agency",pos=4,cex=.75)
axis(1,cex.axis=.8)
axis(2,at=seq(1,47,1),labels=mms$level[1:47],las=2,cex.axis=.75)
mtext(side = 1, "Marginal Mean", line =3,cex=.8)
box()
dev.off()


####
#Fig 2 (Party Breakdown)
####
#Marginal means, Democrats + independents
mms_dem <- cregg::cj(
  subset(res,rep==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm"
)
mms_dem #see for MMs reported in main text
mms_dem[1:4,] <- mms_dem[c(4,2,3,1),]

mms_dem$id <- 1+nrow(mms_dem) - seq(1:nrow(mms_dem))
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),6.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),12.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),18.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),24.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),30.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),37.5)
mms_dem[nrow(mms_dem) + 1,] <- c(rep(NA,10),41.5)
mms_dem$id[5:11] <- rev(mms_dem$id[5:11])
mms_dem <- dplyr::arrange(mms_dem,id)

pdf("figures/fig2.pdf",height=8,width=15)
par(mfrow=c(1,2),mar=c(5,14.5,4,1))
plot(NULL,
     xlim = c(.25,.75), 
     ylim = c(1,47), 
     axes = F, xlab = NA, ylab = NA)
abline(v=.5, col="orangered",lty=1, lwd=1)
for (i in 1:47){
  abline(h=i,lty=1,col="gray90",lwd=.5)
  lines(c(mms_dem$"estimate"[i]-(1.645*mms_dem$"std.error"[i]),
          mms_dem$"estimate"[i]+(1.645*mms_dem$"std.error"[i])),
        c(i,i),col="steelblue4", lwd=5)
  lines(c(mms_dem$"estimate"[i]-(1.96*mms_dem$"std.error"[i]),
          mms_dem$"estimate"[i]+(1.96*mms_dem$"std.error"[i])),
        c(i,i),col="steelblue4", lwd=3)
}
points(mms_dem$"estimate",seq(1,length(mms_dem$"estimate"),1),
       bg=c("steelblue1"), col="steelblue4",  cex=1, pch=21,lwd=2 )
text(.25,5,"Effect on \nTerrorism",pos=4,cex=.75)
abline(h=7,lwd=2)
text(.25,12,"Effect on \nDrug Trafficking",pos=4,cex=.75)
abline(h=14,lwd=2)
text(.25,19,"Effect on \nHuman Trafficking",pos=4,cex=.75)
abline(h=21,lwd=2)
text(.25,26,"Effect on \nIllegal Immigration",pos=4,cex=.75)
abline(h=28,lwd=2)
text(.25,34,"Budgetary Cost",pos=4,cex=.75)
abline(h=35,lwd=2)
text(.25,42,"Strategy",pos=4,cex=.75)
abline(h=43,lwd=2)
text(.25,47,"Agency",pos=4,cex=.75)
axis(1,cex.axis=.8)
axis(2,at=seq(1,47,1),labels=mms_dem$level[1:47],las=2,cex.axis=.75)
mtext(side = 1, "Marginal Means", line =3,cex=.8)
box()

#Marginal means, Republicans
mms_rep <- cregg::cj(
  subset(res,rep==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm"
)
mms_rep#see for MMSs reported in text

mms_rep[1:4,] <- mms_rep[c(4,2,3,1),]
mms_rep$id <- 1+nrow(mms_rep) - seq(1:nrow(mms_rep))
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),6.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),12.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),18.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),24.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),30.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),37.5)
mms_rep[nrow(mms_rep) + 1,] <- c(rep(NA,10),41.5)
mms_rep$id[5:11] <- rev(mms_rep$id[5:11])
mms_rep <- dplyr::arrange(mms_rep,id)

plot(NULL,
     xlim = c(.25,.75), 
     ylim = c(1,47), 
     axes = F, xlab = NA, ylab = NA)
abline(v=.5, col="orangered",lty=1, lwd=1)
for (i in 1:47){
  abline(h=i,lty=1,col="gray90",lwd=.5)
  lines(c(mms_rep$"estimate"[i]-(1.645*mms_rep$"std.error"[i]),
          mms_rep$"estimate"[i]+(1.645*mms_rep$"std.error"[i])),
        c(i,i),col="orangered3", lwd=5)
  lines(c(mms_rep$"estimate"[i]-(1.96*mms_rep$"std.error"[i]),
          mms_rep$"estimate"[i]+(1.96*mms_rep$"std.error"[i])),
        c(i,i),col="orangered3", lwd=3)
}
points(mms_rep$"estimate",seq(1,length(mms_rep$"estimate"),1),
       bg=c("orangered"), col="orangered4",  cex=1, pch=21,lwd=2 )
text(.25,5,"Effect on \nTerrorism",pos=4,cex=.75)
abline(h=7,lwd=2)
text(.25,12,"Effect on \nDrug Trafficking",pos=4,cex=.75)
abline(h=14,lwd=2)
text(.25,19,"Effect on \nHuman Trafficking",pos=4,cex=.75)
abline(h=21,lwd=2)
text(.25,26,"Effect on \nIllegal Immigration",pos=4,cex=.75)
abline(h=28,lwd=2)
text(.25,34,"Budgetary Cost",pos=4,cex=.75)
abline(h=35,lwd=2)
text(.25,42,"Strategy",pos=4,cex=.75)
abline(h=43,lwd=2)
text(.25,47,"Agency",pos=4,cex=.75)
axis(1,cex.axis=.8)
axis(2,at=seq(1,47,1),labels=mms_rep$level[1:47],las=2,cex.axis=.75)
mtext(side = 1, "Marginal Mean", line =3,cex=.8)
box()
dev.off()

####
#Figure 3 (Difference in Preferences for Militarized Strategies)
####
res$rep <- as.factor(res$rep)
strats <- rev(c("Root Causes in Border Region","Enhancing Cooperation with Locals", 
                "Tracking with Tech.","Blocking at Crossings", "Stopping Outside Crossings", 
                "Patrolling Walls","Heavily Armed Patrols"))
party_diff <- cregg::cj(res,
                        chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
                        id = ~ id,
                        estimate = "mm_diff",
                        by = ~ rep)
party_diff #see for p-values reported in text

pdf("figures/fig3.pdf",height=5.5,width=7.5)
par(
  oma = c(0,0,0,0),
  mar = c(6,15,3,3),
  mfrow= c(1,1)
)
plot(NULL,
     xlim = c(-.2,.2), 
     ylim = c(0,7), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(party_diff$"estimate"[party_diff$feature=="STRATEGY"][i]-(1.645*party_diff$"std.error"[party_diff$feature=="STRATEGY"][i]),
          party_diff$"estimate"[party_diff$feature=="STRATEGY"][i]+(1.645*party_diff$"std.error"[party_diff$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=5)
  lines(c(party_diff$"estimate"[party_diff$feature=="STRATEGY"][i]-(1.96*party_diff$"std.error"[party_diff$feature=="STRATEGY"][i]),
          party_diff$"estimate"[party_diff$feature=="STRATEGY"][i]+(1.96*party_diff$"std.error"[party_diff$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=3)
}
points(party_diff$"estimate"[party_diff$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
mtext(side = 1, "Difference in Marginal Means", line =2.5,cex=1) # label bottom axis
dev.off()



####
#Figure A5 (Difference in Preferences, Full Visualization)
####
pdf("figures/figA5.pdf",height=10,width=8)
par(mar=c(5,14.5,4,1))
#Again, transforming order for plot
party_diff$id <- 1+nrow(party_diff) - seq(1:nrow(party_diff))
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),6.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),12.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),18.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),24.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),30.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),37.5)
party_diff[nrow(party_diff) + 1,] <- c(rep(NA,12),41.5)
party_diff$id[5:11] <- rev(party_diff$id[5:11])
party_diff <- dplyr::arrange(party_diff,id)
plot(NULL,
     xlim = c(-.2,.2), 
     ylim = c(1,47), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="orangered",lty=1, lwd=1)
for (i in 1:47){
  abline(h=i,lty=1,col="gray90",lwd=.5)
  lines(c(party_diff$"estimate"[i]-(1.645*party_diff$"std.error"[i]),
          party_diff$"estimate"[i]+(1.645*party_diff$"std.error"[i])),
        c(i,i),col="black", lwd=5)
  lines(c(party_diff$"estimate"[i]-(1.96*party_diff$"std.error"[i]),
          party_diff$"estimate"[i]+(1.96*party_diff$"std.error"[i])),
        c(i,i),col="black", lwd=3)
}
points(party_diff$"estimate",seq(1,length(party_diff$"estimate"),1),
       bg=c("gray80"), col="black",  cex=1, pch=21,lwd=2 )
text(-.2,5,"Effect on \nTerrorism",pos=4,cex=.75)
abline(h=7,lwd=2)
text(-.2,12,"Effect on \nDrug Trafficking",pos=4,cex=.75)
abline(h=14,lwd=2)
text(-.2,19,"Effect on \nHuman Trafficking",pos=4,cex=.75)
abline(h=21,lwd=2)
text(-.2,26,"Effect on \nIllegal Immigration",pos=4,cex=.75)
abline(h=28,lwd=2)
text(-.2,34,"Budgetary Cost",pos=4,cex=.75)
abline(h=35,lwd=2)
text(-.2,42,"Strategy",pos=4,cex=.75)
abline(h=43,lwd=2)
text(-.2,47,"Agency",pos=4,cex=.75)
#abline(h=48)
axis(1,cex.axis=.8)
axis(2,at=seq(1,47,1),labels=party_diff$level[1:47],las=2,cex.axis=.75)
mtext(side = 1, "Difference in Marginal Mean", line =3,cex=.8)
box()
dev.off()




##############################################################################
###Survey 2: Analysis of Treatments
##############################################################################
par(NULL)
load('survey2.RData')

res_tr$loss<-as.factor(ifelse(res_tr$treatment_condition==4 | res_tr$treatment_condition==2 ,1,0 ))

strats <- rev(c("Root Causes in Border Region","Enhancing Cooperation with Locals", 
                "Tracking with Tech.","Blocking at Crossings", "Stopping Outside Crossings", 
                "Patrolling Walls","Heavily Armed Patrols"))
#Generate Treatment Effects, NOTE: 
#treatment_condition 1: Domestic status Retention
#treatment_condition 2: Domestic status Loss
#treatment_condition 3: Intl status Retention
#treatment_condition 4: Intl status Loss
status_loss_intl <- cj(
  subset(res_tr,treatment_condition >2),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_intl #See for p-values in main text

status_loss_dom <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_dom # See for p-values in main text

####
#Figure 4 - Effects of Loss Treatments on Strategy Preferences
####
pdf("figures/fig4.pdf",height=6.5,width=15.5)
par(
  oma = c(0,0,0,0),
  mar = c(6,15,3,3),
  mfrow= c(1,2)
)
plot(NULL,
     xlim = c(-.1,.1), 
     ylim = c(0,8), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]-(1.645*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i]),
          status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]+(1.645*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=5)
  lines(c(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]-(1.96*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i]),
          status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]+(1.96*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=3)
}
points(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=1) # label bottom axis
title("International Status Loss")
box()

plot(NULL,
     xlim = c(-.1,.1), 
     ylim = c(0,8), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]-(1.645*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i]),
          status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]+(1.645*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=5)
  lines(c(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]-(1.96*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i]),
          status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]+(1.96*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=3)
}
points(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=1) # label bottom axis
title("Domestic Status Loss")
box()
dev.off()


####
#Figure 5 - Effects of Loss Treatments on Strategy Preferences, by Party
####
#Dem/ind, intl treatment
status_loss_intl_d <- cj(
  subset(res_tr,treatment_condition >2 & rep==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
#Republican, intl treatment
status_loss_intl_r <- cj(
  subset(res_tr,treatment_condition >2 & rep==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
#Republican, domestic treatment
status_loss_dom_r <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & rep==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
#Den/ind, domestic treatment
status_loss_dom_d <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & rep==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
#See for p-values in main text
status_loss_intl_d
status_loss_intl_r
status_loss_dom_r
status_loss_dom_d

pdf("figures/fig5.pdf",height=7.7,width=11)
par(
  oma = c(0,0,0,0),
  mar = c(6,15,2.5,2.5),
  mfrow= c(2,2)
)
plot(NULL,
     xlim = c(-.133,.133), 
     ylim = c(0,8.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]-(1.645*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i]),
          status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]+(1.645*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i])),
        c(i,i),col="steelblue", lwd=5)
  lines(c(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]-(1.96*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i]),
          status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]+(1.96*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i])),
        c(i,i),col="steelblue4", lwd=3)
}
points(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"],seq(1,7,1),
       bg=c("steelblue1"), col="steelblue4",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.133,8,"Democrats + Independents",pos=4,col="steelblue")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=.8) # label bottom axis
title("International Status Loss")
box()

plot(NULL,
     xlim = c(-.133,.133), 
     ylim = c(0,8.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]-(1.645*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i]),
          status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]+(1.645*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i])),
        c(i,i),col="steelblue", lwd=5)
  lines(c(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]-(1.96*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i]),
          status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]+(1.96*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i])),
        c(i,i),col="steelblue4", lwd=3)
}
points(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"],seq(1,7,1),
       bg=c("steelblue1"), col="steelblue4",  cex=1.5, pch=21,lwd=2 )

axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.133,8,"Democrats + Independents",pos=4,col="steelblue")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=.8) # label bottom axis
title("Domestic Status Loss")
box()

#par(mar = c(6,15,0,3))
plot(NULL,
     xlim = c(-.133,.133), 
     ylim = c(0,8.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]-(1.645*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i]),
          status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]+(1.645*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i])),
        c(i,i),col="orangered3", lwd=5)
  lines(c(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]-(1.96*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i]),
          status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]+(1.96*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i])),
        c(i,i),col="orangered3", lwd=3)
}
points(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"],seq(1,7,1),
       bg=c("orangered"), col="orangered4",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.133,8,"Republicans",pos=4,col="orangered3")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=.8) # label bottom axis
title("International Status Loss")
box()

plot(NULL,
     xlim = c(-.133,.133), 
     ylim = c(0,8.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]-(1.645*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i]),
          status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]+(1.645*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i])),
        c(i,i),col="orangered3", lwd=5)
  lines(c(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]-(1.96*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i]),
          status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]+(1.96*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i])),
        c(i,i),col="orangered3", lwd=3)
}
points(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"],seq(1,7,1),
       bg=c("orangered"), col="orangered4",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.133,8,"Republicans",pos=4,col="orangered3")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=.8) # label bottom axis
title("Domestic Status Loss")
box()
dev.off()
