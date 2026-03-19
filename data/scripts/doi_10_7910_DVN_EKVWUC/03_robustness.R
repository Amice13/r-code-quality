rm(list=ls())
library(dplyr)
library(ggplot2)
library(cregg)
library(rio)
library(lubridate)

#Set working directory (change this to your local directory)
setwd('~/Dropbox/cm_project2/KenwickMaxey_JCR2024_replication/')
load('survey2.RData')

#Variable for whether a respondent is in either loss condition
res_tr$loss<-as.factor(ifelse(res_tr$treatment_condition==4 | 
                                res_tr$treatment_condition==2 ,1,0 ))
#Strategy names for plotting
strats <- rev(c("Root Causes in Border Region","Enhancing Cooperation with Locals", 
                "Tracking with Tech.","Blocking at Crossings", 
                "Stopping Outside Crossings", 
                "Patrolling Walls","Heavily Armed Patrols"))
#Agency names for plotting
agencies <- rev(c("Customs and Border Protection","National Guard", 
                  "Army","Local Law Enforcement"))

##############################################################################
###Results among individuals passing attention/manipulation check
##############################################################################
status_loss_intl_atn <- cj(
  subset(res_tr,treatment_condition >2 & attn2_pass==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_dom_atn <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & attn2_pass==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_intl_man <- cj(
  subset(res_tr,treatment_condition >2 & manip_pass==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_dom_man <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & manip_pass==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_intl <- cj(
  subset(res_tr,treatment_condition >2),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_dom <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

####
#Figure A8
####
pdf("figures/figA6.pdf",height=7.5,width=15.5)
par(
  oma = c(0,0,0,0),
  mar = c(6,15,3,3),
  mfrow= c(1,2)
)
plot(NULL,
     xlim = c(-.13,.13), 
     ylim = c(0,8), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]-(1.645*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i]),
          status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]+(1.645*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="black", lwd=5)
  lines(c(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]-(1.96*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i]),
          status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"][i]+(1.96*status_loss_intl$"std.error"[status_loss_intl$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="black", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_intl_atn$"estimate"[status_loss_intl_atn$feature=="STRATEGY"][i]-(1.645*status_loss_intl_atn$"std.error"[status_loss_intl_atn$feature=="STRATEGY"][i]),
          status_loss_intl_atn$"estimate"[status_loss_intl_atn$feature=="STRATEGY"][i]+(1.645*status_loss_intl_atn$"std.error"[status_loss_intl_atn$feature=="STRATEGY"][i])),
        c(i,i),col="gray50", lwd=5)
  lines(c(status_loss_intl_atn$"estimate"[status_loss_intl_atn$feature=="STRATEGY"][i]-(1.96*status_loss_intl_atn$"std.error"[status_loss_intl_atn$feature=="STRATEGY"][i]),
          status_loss_intl_atn$"estimate"[status_loss_intl_atn$feature=="STRATEGY"][i]+(1.96*status_loss_intl_atn$"std.error"[status_loss_intl_atn$feature=="STRATEGY"][i])),
        c(i,i),col="gray50", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_intl_man$"estimate"[status_loss_intl_man$feature=="STRATEGY"][i]-(1.645*status_loss_intl_man$"std.error"[status_loss_intl_man$feature=="STRATEGY"][i]),
          status_loss_intl_man$"estimate"[status_loss_intl_man$feature=="STRATEGY"][i]+(1.645*status_loss_intl_man$"std.error"[status_loss_intl_man$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="gray80", lwd=5)
  lines(c(status_loss_intl_man$"estimate"[status_loss_intl_man$feature=="STRATEGY"][i]-(1.96*status_loss_intl_man$"std.error"[status_loss_intl_man$feature=="STRATEGY"][i]),
          status_loss_intl_man$"estimate"[status_loss_intl_man$feature=="STRATEGY"][i]+(1.96*status_loss_intl_man$"std.error"[status_loss_intl_man$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="gray80", lwd=3)
}

points(status_loss_intl$"estimate"[status_loss_intl$feature=="STRATEGY"],seq(1.2,7.2,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
points(status_loss_intl_atn$"estimate"[status_loss_intl$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="gray50",  cex=1.5, pch=22,lwd=2 )
points(status_loss_intl_man$"estimate"[status_loss_intl$feature=="STRATEGY"],seq(.8,6.8,1),
       bg=c("white"), col="gray80",  cex=1.5, pch=23,lwd=2 )


points(-.13,0.5,bg=c("gray80"), col="black",  cex=1, pch=21,lwd=2 )
text(-.13,0.5,"Full Sample",pos=4,col="black")
points(-.13,0.25,bg=c("gray80"), col="gray50",  cex=1, pch=22,lwd=2 )
text(-.13,0.25,"Passed Attention Check",pos=4,col="gray50")
points(-.13,0,bg=c("white"), col="gray80",  cex=1, pch=23,lwd=2 )
text(-.13,0,"Passed Manipulation Check",pos=4,col="gray70")

axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
#text(-.19,0.1,"Difference",pos=4,col="black")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=1) # label bottom axis
title("International Status Loss")
box()

plot(NULL,
     xlim = c(-.13,.13), 
     ylim = c(0,8), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]-(1.645*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i]),
          status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]+(1.645*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="black", lwd=5)
  lines(c(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]-(1.96*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i]),
          status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"][i]+(1.96*status_loss_dom$"std.error"[status_loss_dom$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="black", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_dom_atn$"estimate"[status_loss_dom_atn$feature=="STRATEGY"][i]-(1.645*status_loss_dom_atn$"std.error"[status_loss_dom_atn$feature=="STRATEGY"][i]),
          status_loss_dom_atn$"estimate"[status_loss_dom_atn$feature=="STRATEGY"][i]+(1.645*status_loss_dom_atn$"std.error"[status_loss_dom_atn$feature=="STRATEGY"][i])),
        c(i,i),col="gray50", lwd=5)
  lines(c(status_loss_dom_atn$"estimate"[status_loss_dom_atn$feature=="STRATEGY"][i]-(1.96*status_loss_dom_atn$"std.error"[status_loss_dom_atn$feature=="STRATEGY"][i]),
          status_loss_dom_atn$"estimate"[status_loss_dom_atn$feature=="STRATEGY"][i]+(1.96*status_loss_dom_atn$"std.error"[status_loss_dom_atn$feature=="STRATEGY"][i])),
        c(i,i),col="gray50", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_dom_man$"estimate"[status_loss_dom_man$feature=="STRATEGY"][i]-(1.645*status_loss_dom_man$"std.error"[status_loss_dom_man$feature=="STRATEGY"][i]),
          status_loss_dom_man$"estimate"[status_loss_dom_man$feature=="STRATEGY"][i]+(1.645*status_loss_dom_man$"std.error"[status_loss_dom_man$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="gray80", lwd=5)
  lines(c(status_loss_dom_man$"estimate"[status_loss_dom_man$feature=="STRATEGY"][i]-(1.96*status_loss_dom_man$"std.error"[status_loss_dom_man$feature=="STRATEGY"][i]),
          status_loss_dom_man$"estimate"[status_loss_dom_man$feature=="STRATEGY"][i]+(1.96*status_loss_dom_man$"std.error"[status_loss_dom_man$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="gray80", lwd=3)
}

points(status_loss_dom$"estimate"[status_loss_dom$feature=="STRATEGY"],seq(1.2,7.2,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
points(status_loss_dom_atn$"estimate"[status_loss_dom$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="gray50",  cex=1.5, pch=22,lwd=2 )
points(status_loss_dom_man$"estimate"[status_loss_dom$feature=="STRATEGY"],seq(.8,6.8,1),
       bg=c("white"), col="gray80",  cex=1.5, pch=23,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
#text(-.19,0.1,"Difference",pos=4,col="black")
mtext(side = 1, "Treatment Effect\n(Difference in Marginal Means)", line =3,cex=1) # label bottom axis
title("Domestic Status Loss")
box()
dev.off()

##############################################################################
###Results among individuals identifying as Hispanic or non-white
##############################################################################
table(data_tr$white_nonhisp) #988 individuals of 3240

status_loss_intl_poc <- cj(
  subset(res_tr,treatment_condition >2 & white_nonhisp==0 ),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_dom_poc <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)


####
#Figure A8
####
pdf("figures/figA8.pdf",height=6,width=12)
par(
  oma = c(0,0,0,0),
  mar = c(5,15,5,5),
  mfrow= c(1,2)
)
plot(NULL,
     xlim = c(-.13,.13), 
     ylim = c(1,4), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 4:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="AGENCY"][i]-(1.645*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="AGENCY"][i]),
          status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="AGENCY"][i]+(1.645*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="AGENCY"][i])),
        c(i,i),col="black", lwd=5)
  lines(c(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="AGENCY"][i]-(1.96*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="AGENCY"][i]),
          status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="AGENCY"][i]+(1.96*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="AGENCY"][i])),
        c(i,i),col="black", lwd=3)
}
points(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="AGENCY"],seq(1,4,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,4,1),labels=agencies,las=2)
#text(-.13,0.1,"Republicans",pos=4,col="orangered2")
mtext(side = 1, "Domestic Status Loss Treatment Effect\n(Difference in Marginal Means)", line =3.5,cex=1) # label bottom axis
title("Agency")

plot(NULL,
     xlim = c(-.13,.13), 
     ylim = c(0,7), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="STRATEGY"][i]-(1.645*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="STRATEGY"][i]),
          status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="STRATEGY"][i]+(1.645*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=5)
  lines(c(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="STRATEGY"][i]-(1.96*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="STRATEGY"][i]),
          status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="STRATEGY"][i]+(1.96*status_loss_dom_poc$"std.error"[status_loss_dom_poc$feature=="STRATEGY"][i])),
        c(i,i),col="black", lwd=3)
}
points(status_loss_dom_poc$"estimate"[status_loss_dom_poc$feature=="STRATEGY"],seq(1,7,1),
       bg=c("gray80"), col="black",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
#text(-.13,0.1,"Republicans",pos=4,col="orangered2")
mtext(side = 1, "Domestic Status Loss Treatment Effect\n(Difference in Marginal Means)", line =3.5,cex=1) # label bottom axis
title("Strategy")
dev.off()




##############################################################################
###Decomposing Independents
##############################################################################
status_loss_intl_d <- cj(
  subset(res_tr,treatment_condition >2 & dem==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_intl_r <- cj(
  subset(res_tr,treatment_condition >2 & rep==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_intl_i <- cj(
  subset(res_tr,treatment_condition >2 & party_order==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_dom_r <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & rep==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)
status_loss_dom_d <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & dem==1),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

status_loss_dom_i <- cj(
  subset(res_tr,treatment_condition <=2 & white_nonhisp==1 & party_order==0),
  chosen ~ agency + strategy + budget + immig +ht+dt+terror ,
  id = ~ id,
  estimate = "mm_diff",
  by = ~ loss
)

####
#Figure A7
####
pdf("figures/figA7.pdf",height=7.7,width=13)
par(
  oma = c(0,0,0,0),
  mar = c(6,15,3,3),
  mfrow= c(1,2)
)
plot(NULL,
     xlim = c(-.25,.25), 
     ylim = c(0,7.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]-(1.645*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i]),
          status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]+(1.645*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="steelblue", lwd=5)
  lines(c(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]-(1.96*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i]),
          status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"][i]+(1.96*status_loss_intl_d$"std.error"[status_loss_intl_d$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="steelblue4", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]-(1.645*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i]),
          status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]+(1.645*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="orangered3", lwd=5)
  lines(c(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]-(1.96*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i]),
          status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"][i]+(1.96*status_loss_intl_r$"std.error"[status_loss_intl_r$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="orangered3", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_intl_i$"estimate"[status_loss_intl_i$feature=="STRATEGY"][i]-(1.645*status_loss_intl_i$"std.error"[status_loss_intl_i$feature=="STRATEGY"][i]),
          status_loss_intl_i$"estimate"[status_loss_intl_i$feature=="STRATEGY"][i]+(1.645*status_loss_intl_i$"std.error"[status_loss_intl_i$feature=="STRATEGY"][i])),
        c(i,i),col="darkorchid3", lwd=5)
  lines(c(status_loss_intl_i$"estimate"[status_loss_intl_i$feature=="STRATEGY"][i]-(1.96*status_loss_intl_i$"std.error"[status_loss_intl_i$feature=="STRATEGY"][i]),
          status_loss_intl_i$"estimate"[status_loss_intl_i$feature=="STRATEGY"][i]+(1.96*status_loss_intl_i$"std.error"[status_loss_intl_i$feature=="STRATEGY"][i])),
        c(i,i),col="darkorchid3", lwd=3)
}
points(status_loss_intl_i$"estimate"[status_loss_intl_i$feature=="STRATEGY"],seq(1,7,1),
       bg=c("darkorchid1"), col="darkorchid4",  cex=1.5, pch=21,lwd=2 )
points(status_loss_intl_d$"estimate"[status_loss_intl_d$feature=="STRATEGY"],seq(1.2,7.25,1),
       bg=c("steelblue1"), col="steelblue4",  cex=1.5, pch=21,lwd=2 )
points(status_loss_intl_r$"estimate"[status_loss_intl_r$feature=="STRATEGY"],seq(.8,6.8,1),
       bg=c("orangered"), col="orangered4",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.25,0.5,"Democrats",pos=4,col="steelblue")
text(-.25,0.25,"Independents",pos=4,col="darkorchid4")
text(-.25,0.0,"Republicans",pos=4,col="orangered3")
mtext(side = 1, "Difference in Marginal Means", line =2.5,cex=1) # label bottom axis
title("International Status Loss \nTreatment Effect")

plot(NULL,
     xlim = c(-.25,.25), 
     ylim = c(0,7.5), 
     axes = F, xlab = NA, ylab = NA)
abline(v=0, col="gray30",lty=5, lwd=.5)
for (i in 7:1){
  abline(h=i, lwd=0.5,col="gray80")
  lines(c(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]-(1.645*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i]),
          status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]+(1.645*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="steelblue", lwd=5)
  lines(c(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]-(1.96*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i]),
          status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"][i]+(1.96*status_loss_dom_d$"std.error"[status_loss_dom_d$feature=="STRATEGY"][i])),
        c(i+.2,i+.2),col="steelblue4", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]-(1.645*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i]),
          status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]+(1.645*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="orangered3", lwd=5)
  lines(c(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]-(1.96*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i]),
          status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"][i]+(1.96*status_loss_dom_r$"std.error"[status_loss_dom_r$feature=="STRATEGY"][i])),
        c(i-.2,i-.2),col="orangered3", lwd=3)
}
for (i in 7:1){
  lines(c(status_loss_dom_i$"estimate"[status_loss_dom_i$feature=="STRATEGY"][i]-(1.645*status_loss_dom_i$"std.error"[status_loss_dom_i$feature=="STRATEGY"][i]),
          status_loss_dom_i$"estimate"[status_loss_dom_i$feature=="STRATEGY"][i]+(1.645*status_loss_dom_i$"std.error"[status_loss_dom_i$feature=="STRATEGY"][i])),
        c(i,i),col="darkorchid4", lwd=5)
  lines(c(status_loss_dom_i$"estimate"[status_loss_dom_i$feature=="STRATEGY"][i]-(1.96*status_loss_dom_i$"std.error"[status_loss_dom_i$feature=="STRATEGY"][i]),
          status_loss_dom_i$"estimate"[status_loss_dom_i$feature=="STRATEGY"][i]+(1.96*status_loss_dom_i$"std.error"[status_loss_dom_i$feature=="STRATEGY"][i])),
        c(i,i),col="darkorchid4", lwd=3)
}
points(status_loss_dom_i$"estimate"[status_loss_dom_i$feature=="STRATEGY"],seq(1,7,1),
       bg=c("darkorchid1"), col="darkorchid4",  cex=1.5, pch=21,lwd=2 )
points(status_loss_dom_d$"estimate"[status_loss_dom_d$feature=="STRATEGY"],seq(1.2,7.25,1),
       bg=c("steelblue1"), col="steelblue4",  cex=1.5, pch=21,lwd=2 )
points(status_loss_dom_r$"estimate"[status_loss_dom_r$feature=="STRATEGY"],seq(.8,6.8,1),
       bg=c("orangered"), col="orangered4",  cex=1.5, pch=21,lwd=2 )
axis(1,cex.axis=.8)
axis(2,at=seq(1,7,1),labels=strats,las=2)
text(-.25,0.5,"Democrats",pos=4,col="steelblue")
text(-.25,0.25,"Independents",pos=4,col="darkorchid4")
text(-.25,0.0,"Republicans",pos=4,col="orangered3")
mtext(side = 1, "Difference in Marginal Means", line =2.5,cex=1) # label bottom axis
title("Domestic Status Loss \nTreatment Effect")
dev.off()


############################################################################################################
#Descriptives (Table A1)
rm(list=ls())
#Experiment 1: Conjoint Only
load('survey1.RData')

#Party
data$indep <- ifelse(data$party_order==0,1,0)
data$indep[is.na(data$party_order)] <- NA
data$dem <- ifelse(data$party_order < 0,1,0)
data$dem[is.na(data$party_order)] <- NA
data$gender[data$gender==""] <- NA
data$age[data$age==""] <- NA
data$race[data$race==""] <- NA
data$education[data$education==""] <- NA
data$milserve[data$milserve==""] <- NA
data$hispanic[data$hispanic==""] <- NA

round(table(data$gender)/sum(table(data$gender)),3)
round(table(data$age)/sum(table(data$age)),3)
round(table(data$party_order)/sum(table(data$party_order)),3)
round(table(data$race) /sum(table(data$race)),3)
round(table(data$education)/sum(table(data$education)),3)
round(table(data$milserve)/sum(table(data$milserve)),3)
round(table(data$hispanic)/sum(table(data$hispanic)),3)


####
#Experiment 2: Treatments
####
load('survey2.RData')

data_tr$indep <- ifelse(data_tr$party_order==0,1,0)
data_tr$indep[is.na(data_tr$party_order)] <- NA
data_tr$dem <- ifelse(data_tr$party_order < 0,1,0)
data_tr$dem[is.na(data_tr$party_order)] <- NA
data_tr$gender[data_tr$gender==""] <- NA
data_tr$age[data_tr$age==""] <- NA
data_tr$race[data_tr$race==""] <- NA
data_tr$education[data_tr$education==""] <- NA
data_tr$milserve[data_tr$milserve==""] <- NA
data_tr$hispanic[data_tr$hispanic==""] <- NA

round(table(data_tr$gender)/sum(table(data_tr$gender)),3)
round(table(data_tr$age)/sum(table(data_tr$age)),3)
round(table(data_tr$party_order)/sum(table(data_tr$party_order)),3)
round(table(data_tr$race) /sum(table(data_tr$race)),3)
round(table(data_tr$education)/sum(table(data_tr$education)),3)
round(table(data_tr$milserve)/sum(table(data_tr$milserve)),3)
round(table(data_tr$hispanic)/sum(table(data_tr$hispanic)),3)
