#########################################################
#Author: M. Kenwick
#Date: Sep 20, 2019
#Purpose: Generates figures and runs analyses reported in the Appendix
#########################################################
#Script Generates figures and runs analyses reported in the main text
rm(list=ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(foreign)
set.seed(1989)

#Load and prep data
setwd('~/Dropbox/ctrl_rep_final')
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")
load("irt_modeling/data_prepped.RData")

#Extract model estiamtes and append to data object
data$theta_s<-apply(output_s$x,2,mean)
data$theta_s_sd<-apply(output_s$x,2,sd)
theta_s_draws<-output_s$x

theta_d_draws<-t(apply(output$x,1,scale))
data$theta_d<-apply(theta_d_draws,2,mean)
data$theta_d_sd<-apply(theta_d_draws,2,sd)

data$theta_s_up<-data$theta_s + (1.96*data$theta_s_sd)
data$theta_s_lw<-data$theta_s - (1.96*data$theta_s_sd)
data$theta_d_up<-data$theta_d + (1.96*data$theta_d_sd)
data$theta_d_lw<-data$theta_d - (1.96*data$theta_d_sd)


###Figure A1: Correlation Plot
data2<-data[ ,c(4:8,10:14)]
colnames(data2)<-c("Military Entry","CGV Military Regime","Military Leader",
                  "GWF Military Regime","Prior Military Regime","ARD Military Regime",
                  "Leader Military Experience","Military Cabinet Ministers",
                  "Mil. Involvement in Politics","Militarism Index")
library(corrplot)
cor_mat = cor(data2,use="pairwise.complete.obs")

pdf("plots/appendix/a1.pdf",width=5,height=5)
par(mfrow=c(1,1),mar=c(1,1,1,1),  oma = c(0,1,0,0))
corrplot(cor_mat, method="color", tl.pos="lt", type="full", tl.col="black", 
         tl.cex=0.7, tl.srt=45, addCoefasPercent = F) 
dev.off()


#Figure A.2: Drift Parameter Across Age of Civilianized Regimes
drfn<-output$drift
drift_hi <- apply(drfn, 2, quantile, 0.975 )
drift_lo <- apply(drfn, 2, quantile, 0.025 )
drift_mean<-apply(drfn, 2, mean )
age<-seq(1:length(drift_hi))

drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}

drift_hi2 <- apply(drfn2, 2, quantile, 0.975 )
drift_lo2 <- apply(drfn2, 2, quantile, 0.025 )
drift_mean2<-apply(drfn2, 2, mean )

pdf('plots/appendix/a2.pdf',width=13,height=8)
par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,5,4,2),
  mfrow= c(1,2)
)
plot(NULL,# create empty plot
     xlim = c(0,64), 
     ylim = c(-.15,.29), 
     axes = F, xlab = NA, ylab = NA) 
abline(h=0, col="gray80", lwd=1)
for (ii in 1:65){
  lines(c(age[ii],age[ii]),c(drift_hi[ii], drift_lo[ii]),col="#313695", lwd=1.25)
  points(age[ii],drift_mean[ii],bg=c("#74add1"), col="#313695",lwd=1,cex=.5, pch=21 )
}
axis(1,at=c(0,10,20,30,40,50,60,65),labels=c('0','10','20','30','40','50','60','65+'))
axis(2)
mtext(side=2,"Drift Parameter Value",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()

plot(NULL,# create empty plot
     xlim = c(0,65), 
     ylim = c(-.5,2), 
     axes = F, xlab = NA, ylab = NA)  
abline(h=0, col="gray80",lty=1, lwd=1)
for (ii in 1:length(age)){
  lines(c(age[ii],age[ii]),c(drift_hi2[ii], drift_lo2[ii]),col="#313695", lwd=1.25)
  points(age[ii],drift_mean2[ii],bg=c("#74add1"), col="#313695",lwd=1,cex=.5, pch=21 )
}
axis(1)
axis(2)
mtext(side=2,"Cumulative Drift",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()
dev.off()


###
#####################
#ICCs
#####################
#Table A.2: Comparison of Model Difficulty and Discrimination Parameters
alpha_k2_s<-output_s$alpha_k2
beta_k2_s<-output_s$beta_k2
beta_k3_s<-output_s$beta_k3
c_k3_s<-output_s$c_k3
beta_k4_s<-output_s$beta_k4
c_k4_s<-output_s$c_k4
beta_k5_s<-output_s$beta_k5
c_k5_s<-output_s$c_k5

alpha_k2_d<-output$alpha_k2
beta_k2_d<-output$beta_k2
beta_k3_d<-output$beta_k3
c_k3_d<-output$c_k3
beta_k4_d<-output$beta_k4
c_k4_d<-output$c_k4
beta_k5_d<-output$beta_k5
c_k5_d<-output$c_k5

PARAMETERS_s_k2 = cbind(apply(alpha_k2_s,2,mean),apply(alpha_k2_s,2,sd),apply(alpha_k2_s, 2, quantile, 0.975 ),apply(alpha_k2_s, 2, quantile, 0.025 ),
                        apply(beta_k2_s,2,mean),apply(beta_k2_s,2,sd),apply(beta_k2_s, 2, quantile, 0.975 ),apply(beta_k2_s, 2, quantile, 0.025 ) )
colnames(PARAMETERS_s_k2)<-c("c_mean","c_sd","c_975","c_025","b_mean","b_sd","b_975","b_025")
rownames(PARAMETERS_s_k2)<-c('svolik_milentry',
                             "cgv_mil",
                             "dpi_millead",
                             "gwf_mil",
                             "gwf_mil_prior",
                             "ard_mil_any")
PARAMETERS_d_k2 = cbind(apply(alpha_k2_d,2,mean),apply(alpha_k2_d,2,sd),apply(alpha_k2_d, 2, quantile, 0.975 ),apply(alpha_k2_d, 2, quantile, 0.025 ),
                        apply(beta_k2_d,2,mean),apply(beta_k2_d,2,sd),apply(beta_k2_d, 2, quantile, 0.975 ),apply(beta_k2_d, 2, quantile, 0.025 ) )
colnames(PARAMETERS_d_k2)<-c("c_mean","c_sd","c_975","c_025","b_mean","b_sd","b_975","b_025")
rownames(PARAMETERS_d_k2)<-c('svolik_milentry',
                             "cgv_mil",
                             "dpi_millead",
                             "gwf_mil",
                             "gwf_mil_prior",
                             "ard_mil_any")

PARAMETERS_s_k3_1 = c(mean(c_k3_s[,1,1]),
                      sd(c_k3_s[,1,1]),
                      quantile(c_k3_s[,1,1],probs=c(0.975,0.025)),
                      mean(c_k3_s[,1,2]),
                      sd(c_k3_s[,1,2]),
                      quantile(c_k3_s[,1,2],probs=c(0.975,0.025)),
                      mean(beta_k3_s[,1]),
                      sd((beta_k3_s[,1])),
                      quantile(beta_k3_s[,1],probs=c(0.975,0.025))
)
PARAMETERS_s_k3_2 = c(mean(c_k3_s[,2,1]),
                      sd(c_k3_s[,2,1]),
                      quantile(c_k3_s[,2,1],probs=c(0.975,0.025)),
                      mean(c_k3_s[,2,2]),
                      sd(c_k3_s[,2,2]),
                      quantile(c_k3_s[,2,2],probs=c(0.975,0.025)),
                      mean(beta_k3_s[,2]),
                      sd((beta_k3_s[,2])),
                      quantile(beta_k3_s[,2],probs=c(0.975,0.025))
)
PARAMETERS_s_k3<-rbind(PARAMETERS_s_k3_1,PARAMETERS_s_k3_2)

colnames(PARAMETERS_s_k3)<-c("c1_mean","c1_sd","c1_975","c1_025",
                             "c2_mean","c2_sd","c2_975","c2_025",
                             "b_mean","b_sd","b_975","b_025")
rownames(PARAMETERS_s_k3)<-c("hs_lead_scale","mpg_cat")

PARAMETERS_d_k3_1 = c(mean(c_k3_d[,1,1]),
                      sd(c_k3_d[,1,1]),
                      quantile(c_k3_d[,1,1],probs=c(0.975,0.025)),
                      mean(c_k3_d[,1,2]),
                      sd(c_k3_d[,1,2]),
                      quantile(c_k3_d[,1,2],probs=c(0.975,0.025)),
                      mean(beta_k3_d[,1]),
                      sd((beta_k3_d[,1])),
                      quantile(beta_k3_d[,1],probs=c(0.975,0.025))
)
PARAMETERS_d_k3_2 = c(mean(c_k3_d[,2,1]),
                      sd(c_k3_d[,2,1]),
                      quantile(c_k3_d[,2,1],probs=c(0.975,0.025)),
                      mean(c_k3_d[,2,2]),
                      sd(c_k3_d[,2,2]),
                      quantile(c_k3_d[,2,2],probs=c(0.975,0.025)),
                      mean(beta_k3_d[,2]),
                      sd((beta_k3_d[,2])),
                      quantile(beta_k3_d[,2],probs=c(0.975,0.025))
)
PARAMETERS_d_k3<-rbind(PARAMETERS_d_k3_1,PARAMETERS_d_k3_2)

colnames(PARAMETERS_d_k3)<-c("c1_mean","c1_sd","c1_975","c1_025",
                             "c2_mean","c2_sd","c2_975","c2_025",
                             "b_mean","b_sd","b_975","b_025")
rownames(PARAMETERS_d_k3)<-c("hs_lead_scale","mpg_cat")

PARAMETERS_s_k4 = c(mean(c_k4_s[,1,1]),sd(c_k4_s[,1,1]),quantile(c_k4_s[,1,1],probs=c(0.975,0.025)),
                    mean(c_k4_s[,1,2]),sd(c_k4_s[,1,2]),quantile(c_k4_s[,1,2],probs=c(0.975,0.025)),
                    mean(c_k4_s[,1,3]),sd(c_k4_s[,1,3]),quantile(c_k4_s[,1,3],probs=c(0.975,0.025)),
                    apply(beta_k4_s,2,mean),apply(beta_k4_s,2,sd),apply(beta_k4_s, 2, quantile, 0.975 ),
                    apply(beta_k4_s, 2, quantile, 0.025 ))

names(PARAMETERS_s_k4)<-c("c1_mean","c1_sd","c1_975","c1_025",
                          "c2_mean","c2_sd","c2_975","c2_025",
                          "c3_mean","c3_sd","c3_975","c3_025",
                          "b_mean","b_sd","b_975","b_025")
PARAMETERS_d_k4 = c(mean(c_k4_d[,1,1]),sd(c_k4_d[,1,1]),quantile(c_k4_d[,1,1],probs=c(0.975,0.025)),
                    mean(c_k4_d[,1,2]),sd(c_k4_d[,1,2]),quantile(c_k4_d[,1,2],probs=c(0.975,0.025)),
                    mean(c_k4_d[,1,3]),sd(c_k4_d[,1,3]),quantile(c_k4_d[,1,3],probs=c(0.975,0.025)),
                    apply(beta_k4_d,2,mean),apply(beta_k4_d,2,sd),apply(beta_k4_d, 2, quantile, 0.975 ),
                    apply(beta_k4_d, 2, quantile, 0.025 ))

names(PARAMETERS_d_k4)<-c("c1_mean","c1_sd","c1_975","c1_025",
                          "c2_mean","c2_sd","c2_975","c2_025",
                          "c3_mean","c3_sd","c3_975","c3_025",
                          "b_mean","b_sd","b_975","b_025")

PARAMETERS_s_k5 = c(mean(c_k5_s[,1,1]),sd(c_k5_s[,1,1]),quantile(c_k5_s[,1,1],probs=c(0.975,0.025)),
                    mean(c_k5_s[,1,2]),sd(c_k5_s[,1,2]),quantile(c_k5_s[,1,2],probs=c(0.975,0.025)),
                    mean(c_k5_s[,1,3]),sd(c_k5_s[,1,3]),quantile(c_k5_s[,1,3],probs=c(0.975,0.025)),
                    mean(c_k5_s[,1,4]),sd(c_k5_s[,1,4]),quantile(c_k5_s[,1,4],probs=c(0.975,0.025)),
                    apply(beta_k5_s,2,mean),apply(beta_k5_s,2,sd),apply(beta_k5_s, 2, quantile, 0.975 ),
                    apply(beta_k5_s, 2, quantile, 0.025 ))
names(PARAMETERS_s_k5)<-c("c1_mean","c1_sd","c1_975","c1_025",
                          "c2_mean","c2_sd","c2_975","c2_025",
                          "c3_mean","c3_sd","c3_975","c3_025",
                          "c4_mean","c4_sd","c4_975","c4_025",
                          "b_mean","b_sd","b_975","b_025")
PARAMETERS_d_k5 = c(mean(c_k5_d[,1,1]),sd(c_k5_d[,1,1]),quantile(c_k5_d[,1,1],probs=c(0.975,0.025)),
                    mean(c_k5_d[,1,2]),sd(c_k5_d[,1,2]),quantile(c_k5_d[,1,2],probs=c(0.975,0.025)),
                    mean(c_k5_d[,1,3]),sd(c_k5_d[,1,3]),quantile(c_k5_d[,1,3],probs=c(0.975,0.025)),
                    mean(c_k5_d[,1,4]),sd(c_k5_d[,1,4]),quantile(c_k5_d[,1,4],probs=c(0.975,0.025)),
                    apply(beta_k5_d,2,mean),apply(beta_k5_d,2,sd),apply(beta_k5_d, 2, quantile, 0.975 ),
                    apply(beta_k5_d, 2, quantile, 0.025 ))
names(PARAMETERS_d_k5)<-c("c1_mean","c1_sd","c1_975","c1_025",
                          "c2_mean","c2_sd","c2_975","c2_025",
                          "c3_mean","c3_sd","c3_975","c3_025",
                          "c4_mean","c4_sd","c4_975","c4_025",
                          "b_mean","b_sd","b_975","b_025")
table<-matrix(nrow=27,ncol=6,NA)
names<-c('svolik_milentry_b','svolik_milentry_a',
         "cgv_mil_b","cgv_mil_a",
         "dpi_millead_b","dpi_millead_a",
         "gwf_mil_b","gwf_mil_a",
         "gwf_mil_prior_b","gwf_mil_prior_a",
         "ard_mil_any_b","ard_mil_any_a",
         "hs_lead_scale_b","hs_lead_scale_a1","hs_lead_scale_a2",
         "mpg_cat_b","mpg_cat_a1","mpg_cat_a2",
         "svolik_military_scale_b", "svolik_military_scale_a1", "svolik_military_scale_a2", "svolik_military_scale_3",
         "weeks_milindex_simple_b","weeks_milindex_simple_a1","weeks_milindex_simple_a2","weeks_milindex_simple_a3","weeks_milindex_simple_a4")
rownames(table)<-names
colnames(table)<-c("s","d","s_up","d_up","s_down","d_down")
table[1,]<-c(PARAMETERS_s_k2['svolik_milentry','b_mean'], PARAMETERS_d_k2['svolik_milentry','b_mean'],
             PARAMETERS_s_k2['svolik_milentry','b_025'], PARAMETERS_d_k2['svolik_milentry','b_025'],
             PARAMETERS_s_k2['svolik_milentry','b_975'],PARAMETERS_s_k2['svolik_milentry','b_975'])
table[2,]<-c(PARAMETERS_s_k2['svolik_milentry','c_mean'], PARAMETERS_d_k2['svolik_milentry','c_mean'],
             PARAMETERS_s_k2['svolik_milentry','c_025'], PARAMETERS_d_k2['svolik_milentry','c_025'],
             PARAMETERS_s_k2['svolik_milentry','c_975'],PARAMETERS_s_k2['svolik_milentry','c_975'])
table[3,]<-c(PARAMETERS_s_k2['cgv_mil','b_mean'], PARAMETERS_d_k2['cgv_mil','b_mean'],
             PARAMETERS_s_k2['cgv_mil','b_025'], PARAMETERS_d_k2['cgv_mil','b_025'],
             PARAMETERS_s_k2['cgv_mil','b_975'],PARAMETERS_s_k2['cgv_mil','b_975'])
table[4,]<-c(PARAMETERS_s_k2['cgv_mil','c_mean'], PARAMETERS_d_k2['cgv_mil','c_mean'],
             PARAMETERS_s_k2['cgv_mil','c_025'], PARAMETERS_d_k2['cgv_mil','c_025'],
             PARAMETERS_s_k2['cgv_mil','c_975'],PARAMETERS_s_k2['cgv_mil','c_975'])
table[5,]<-c(PARAMETERS_s_k2['dpi_millead','b_mean'], PARAMETERS_d_k2['dpi_millead','b_mean'],
             PARAMETERS_s_k2['dpi_millead','b_025'], PARAMETERS_d_k2['dpi_millead','b_025'],
             PARAMETERS_s_k2['dpi_millead','b_975'],PARAMETERS_s_k2['dpi_millead','b_975'])
table[6,]<-c(PARAMETERS_s_k2['dpi_millead','c_mean'], PARAMETERS_d_k2['dpi_millead','c_mean'],
             PARAMETERS_s_k2['dpi_millead','c_025'], PARAMETERS_d_k2['dpi_millead','c_025'],
             PARAMETERS_s_k2['dpi_millead','c_975'],PARAMETERS_s_k2['dpi_millead','c_975'])
table[7,]<-c(PARAMETERS_s_k2['gwf_mil','b_mean'], PARAMETERS_d_k2['gwf_mil','b_mean'],
             PARAMETERS_s_k2['gwf_mil','b_025'], PARAMETERS_d_k2['gwf_mil','b_025'],
             PARAMETERS_s_k2['gwf_mil','b_975'],PARAMETERS_s_k2['gwf_mil','b_975'])
table[8,]<-c(PARAMETERS_s_k2['gwf_mil','c_mean'], PARAMETERS_d_k2['gwf_mil','c_mean'],
             PARAMETERS_s_k2['gwf_mil','c_025'], PARAMETERS_d_k2['gwf_mil','c_025'],
             PARAMETERS_s_k2['gwf_mil','c_975'],PARAMETERS_s_k2['gwf_mil','c_975'])
table[9,]<-c(PARAMETERS_s_k2['gwf_mil_prior','b_mean'], PARAMETERS_d_k2['gwf_mil_prior','b_mean'],
             PARAMETERS_s_k2['gwf_mil_prior','b_025'], PARAMETERS_d_k2['gwf_mil_prior','b_025'],
             PARAMETERS_s_k2['gwf_mil_prior','b_975'],PARAMETERS_s_k2['gwf_mil_prior','b_975'])
table[10,]<-c(PARAMETERS_s_k2['gwf_mil_prior','c_mean'], PARAMETERS_d_k2['gwf_mil_prior','c_mean'],
              PARAMETERS_s_k2['gwf_mil_prior','c_025'], PARAMETERS_d_k2['gwf_mil_prior','c_025'],
              PARAMETERS_s_k2['gwf_mil_prior','c_975'],PARAMETERS_s_k2['gwf_mil_prior','c_975'])
table[11,]<-c(PARAMETERS_s_k2['ard_mil_any','b_mean'], PARAMETERS_d_k2['ard_mil_any','b_mean'],
              PARAMETERS_s_k2['ard_mil_any','b_025'], PARAMETERS_d_k2['ard_mil_any','b_025'],
              PARAMETERS_s_k2['ard_mil_any','b_975'],PARAMETERS_s_k2['ard_mil_any','b_975'])
table[12,]<-c(PARAMETERS_s_k2['ard_mil_any','c_mean'], PARAMETERS_d_k2['ard_mil_any','c_mean'],
              PARAMETERS_s_k2['ard_mil_any','c_025'], PARAMETERS_d_k2['ard_mil_any','c_025'],
              PARAMETERS_s_k2['ard_mil_any','c_975'],PARAMETERS_s_k2['ard_mil_any','c_975'])
table[13,]<-c(PARAMETERS_s_k3['hs_lead_scale','b_mean'], PARAMETERS_d_k3['hs_lead_scale','b_mean'],
              PARAMETERS_s_k3['hs_lead_scale','b_025'],  PARAMETERS_d_k3['hs_lead_scale','b_025'],
              PARAMETERS_s_k3['hs_lead_scale','b_975'],  PARAMETERS_d_k3['hs_lead_scale','b_975'])
table[14,]<-c(PARAMETERS_s_k3['hs_lead_scale','c1_mean'], PARAMETERS_d_k3['hs_lead_scale','c1_mean'],
              PARAMETERS_s_k3['hs_lead_scale','c1_025'],  PARAMETERS_d_k3['hs_lead_scale','c1_025'],
              PARAMETERS_s_k3['hs_lead_scale','c1_975'],  PARAMETERS_d_k3['hs_lead_scale','c1_975'])
table[15,]<-c(PARAMETERS_s_k3['hs_lead_scale','c2_mean'], PARAMETERS_d_k3['hs_lead_scale','c2_mean'],
              PARAMETERS_s_k3['hs_lead_scale','c2_025'],  PARAMETERS_d_k3['hs_lead_scale','c2_025'],
              PARAMETERS_s_k3['hs_lead_scale','c2_975'],  PARAMETERS_d_k3['hs_lead_scale','c2_975'])
table[16,]<-c(PARAMETERS_s_k3['mpg_cat','b_mean'], PARAMETERS_d_k3['mpg_cat','b_mean'],
              PARAMETERS_s_k3['mpg_cat','b_025'],  PARAMETERS_d_k3['mpg_cat','b_025'],
              PARAMETERS_s_k3['mpg_cat','b_975'],  PARAMETERS_d_k3['mpg_cat','b_975'])
table[17,]<-c(PARAMETERS_s_k3['mpg_cat','c1_mean'], PARAMETERS_d_k3['mpg_cat','c1_mean'],
              PARAMETERS_s_k3['mpg_cat','c1_025'],  PARAMETERS_d_k3['mpg_cat','c1_025'],
              PARAMETERS_s_k3['mpg_cat','c1_975'],  PARAMETERS_d_k3['mpg_cat','c1_975'])
table[18,]<-c(PARAMETERS_s_k3['mpg_cat','c2_mean'], PARAMETERS_d_k3['mpg_cat','c2_mean'],
              PARAMETERS_s_k3['mpg_cat','c2_025'],  PARAMETERS_d_k3['mpg_cat','c2_025'],
              PARAMETERS_s_k3['mpg_cat','c2_975'],  PARAMETERS_d_k3['mpg_cat','c2_975'])
table[19,]<-c(PARAMETERS_s_k4['b_mean'], PARAMETERS_d_k4['b_mean'],
              PARAMETERS_s_k4['b_025'],  PARAMETERS_d_k4['b_025'],
              PARAMETERS_s_k4['b_975'],  PARAMETERS_d_k4['b_975'])
table[20,]<-c(PARAMETERS_s_k4['c1_mean'], PARAMETERS_d_k4['c1_mean'],
              PARAMETERS_s_k4['c1_025'],  PARAMETERS_d_k4['c1_025'],
              PARAMETERS_s_k4['c1_975'],  PARAMETERS_d_k4['c1_975'])
table[21,]<-c(PARAMETERS_s_k4['c2_mean'], PARAMETERS_d_k4['c2_mean'],
              PARAMETERS_s_k4['c2_025'],  PARAMETERS_d_k4['c2_025'],
              PARAMETERS_s_k4['c2_975'],  PARAMETERS_d_k4['c2_975'])
table[22,]<-c(PARAMETERS_s_k4['c3_mean'], PARAMETERS_d_k4['c3_mean'],
              PARAMETERS_s_k4['c3_025'],  PARAMETERS_d_k4['c3_025'],
              PARAMETERS_s_k4['c3_975'],  PARAMETERS_d_k4['c3_975'])
table[23,]<-c(PARAMETERS_s_k5['b_mean'], PARAMETERS_d_k5['b_mean'],
              PARAMETERS_s_k5['b_025'],  PARAMETERS_d_k5['b_025'],
              PARAMETERS_s_k5['b_975'],  PARAMETERS_d_k5['b_975'])
table[24,]<-c(PARAMETERS_s_k5['c1_mean'], PARAMETERS_d_k5['c1_mean'],
              PARAMETERS_s_k5['c1_025'],  PARAMETERS_d_k5['c1_025'],
              PARAMETERS_s_k5['c1_975'],  PARAMETERS_d_k5['c1_975'])
table[25,]<-c(PARAMETERS_s_k5['c2_mean'], PARAMETERS_d_k5['c2_mean'],
              PARAMETERS_s_k5['c2_025'],  PARAMETERS_d_k5['c2_025'],
              PARAMETERS_s_k5['c2_975'],  PARAMETERS_d_k5['c2_975'])
table[26,]<-c(PARAMETERS_s_k5['c3_mean'], PARAMETERS_d_k5['c3_mean'],
              PARAMETERS_s_k5['c3_025'],  PARAMETERS_d_k5['c3_025'],
              PARAMETERS_s_k5['c4_975'],  PARAMETERS_d_k5['c3_975'])
table[27,]<-c(PARAMETERS_s_k5['c4_mean'], PARAMETERS_d_k5['c4_mean'],
              PARAMETERS_s_k5['c4_025'],  PARAMETERS_d_k5['c4_025'],
              PARAMETERS_s_k5['c4_975'],  PARAMETERS_d_k5['c4_975'])
recol<-c('s','s_up',"s_down","d","d_up","d_down")
table<-table[,recol]
write.csv(round(table,3), "icc_table.csv")



###Figure A.3/4: Item Characteristic Curves - Static/Drift Model
NAMES <- c(
  "Pr(Military Entry)",
  "Pr(CGV Military Regime)",      
  "Pr(Military Leader)",     
  "Pr(GWF Military Regime)",        
  "Pr(Prior Military Regime)",
  "Pr(ARD Military Regime)",
  "Pr(Leader Miltary Experience)",
  "Pr(Miltary Participation in Government)",
  "Pr(Military Involvement in Politics)",
  "Pr(Militarism Index)")

pdf("plots/appendix/a3.pdf", width=6.5,height=8)
par(mfrow=c(4,3), mar=c(2,0,0,0),xpd=NA,oma = c(1,0,0,0))
SIM <- 1000
plot(0,0, xlim=c(0,1), ylim=c(0,1), type="n", yaxt="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
mtext("Static Model", cex=1.5, line=-6)

for(j in 1:6){
  alpha <- rnorm(SIM, mean=PARAMETERS_s_k2[j,'c_mean'], sd=PARAMETERS_s_k2[j,'c_sd'])
  beta <- rnorm(SIM, mean=PARAMETERS_s_k2[j,'b_mean'], sd=PARAMETERS_s_k2[j,'b_sd'])
  x <- seq(from=-4, to=4, by=.01)
  values<- 1- plogis(alpha + beta %*% t(x))
  
  plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
  lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
  polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
  text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
  axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
  axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
  lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
}

j = 7
alpha <- rnorm(SIM, mean=PARAMETERS_s_k3[1,'c1_mean'], sd=PARAMETERS_s_k3[1,'c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_s_k3[1,'b_mean'], sd=PARAMETERS_s_k3[1,'b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_s_k3[1,'c2_mean'], sd=PARAMETERS_s_k3[1,'c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")

j = 8
alpha <- rnorm(SIM, mean=PARAMETERS_s_k3[2,'c1_mean'], sd=PARAMETERS_s_k3[2,'c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_s_k3[2,'b_mean'], sd=PARAMETERS_s_k3[2,'b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_s_k3[2,'c2_mean'], sd=PARAMETERS_s_k3[2,'c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")

j = 9
alpha <- rnorm(SIM, mean=PARAMETERS_s_k4['c1_mean'], sd=PARAMETERS_s_k4['c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_s_k4['b_mean'], sd=PARAMETERS_s_k4['b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_s_k4['c2_mean'], sd=PARAMETERS_s_k4['c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_s_k4['c3_mean'], sd=PARAMETERS_s_k4['c3_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")

j = 10
alpha <- rnorm(SIM, mean=PARAMETERS_s_k5['c1_mean'], sd=PARAMETERS_s_k5['c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_s_k5['b_mean'], sd=PARAMETERS_s_k5['b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_s_k5['c2_mean'], sd=PARAMETERS_s_k5['c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_s_k5['c3_mean'], sd=PARAMETERS_s_k5['c3_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA ) 
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_s_k5['c4_mean'], sd=PARAMETERS_s_k5['c4_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
dev.off()

pdf("plots/appendix/a4.pdf", width=6.5,height=8)
par(mfrow=c(4,3), mar=c(2,0,0,0),xpd=NA,oma = c(1,0,0,0))
SIM <- 1000
plot(0,0, xlim=c(0,1), ylim=c(0,1), type="n", yaxt="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
mtext("Drift Model", cex=1.5, line=-6)
for(j in 1:6){
  alpha <- rnorm(SIM, mean=PARAMETERS_d_k2[j,'c_mean'], sd=PARAMETERS_d_k2[j,'c_sd'])
  beta <- rnorm(SIM, mean=PARAMETERS_d_k2[j,'b_mean'], sd=PARAMETERS_d_k2[j,'b_sd'])
  x <- seq(from=-4, to=4, by=.01)
  values<- 1- plogis(alpha + beta %*% t(x))
  plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
  lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
  polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
  text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
  axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
  axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
  lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
}
j = 7
alpha <- rnorm(SIM, mean=PARAMETERS_d_k3[1,'c1_mean'], sd=PARAMETERS_d_k3[1,'c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_d_k3[1,'b_mean'], sd=PARAMETERS_d_k3[1,'b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_d_k3[1,'c2_mean'], sd=PARAMETERS_d_k3[1,'c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
j = 8
alpha <- rnorm(SIM, mean=PARAMETERS_d_k3[2,'c1_mean'], sd=PARAMETERS_d_k3[2,'c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_d_k3[2,'b_mean'], sd=PARAMETERS_d_k3[2,'b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_d_k3[2,'c2_mean'], sd=PARAMETERS_d_k3[2,'c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
j = 9
alpha <- rnorm(SIM, mean=PARAMETERS_d_k4['c1_mean'], sd=PARAMETERS_d_k4['c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_d_k4['b_mean'], sd=PARAMETERS_d_k4['b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_d_k4['c2_mean'], sd=PARAMETERS_d_k4['c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_d_k4['c3_mean'], sd=PARAMETERS_d_k4['c3_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
j = 10
alpha <- rnorm(SIM, mean=PARAMETERS_d_k5['c1_mean'], sd=PARAMETERS_d_k5['c1_sd'])
beta <- rnorm(SIM, mean=PARAMETERS_d_k5['b_mean'], sd=PARAMETERS_d_k5['b_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
plot(NULL, ylim=c(0,1.275), type="l", lwd=2, col=alpha("#f46d43",.4), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-5.75,5.75),bty="n")
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
text(-5.8,1.15,NAMES[j],  cex=1, pos=4)     
axis(side=1, at=c(-4,-2,0,2,4),cex.axis=.8,mgp=c(3,.5,0))
axis(side=2, at=c(0.01,.2,.4,.6,.8,1), labels=c("0.0","0.2","0.4","0.6","0.8","1.0"), tick=T, pos=-4.1, las=2, cex.axis=.8)    
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
mtext(side = 1, "Latent Civilian Control", line=1.5, cex=.5)# add title
alpha <- rnorm(SIM, mean=PARAMETERS_d_k5['c2_mean'], sd=PARAMETERS_d_k5['c2_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_d_k5['c3_mean'], sd=PARAMETERS_d_k5['c3_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA ) 
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
alpha <- rnorm(SIM, mean=PARAMETERS_d_k5['c4_mean'], sd=PARAMETERS_d_k5['c4_sd'])
x <- seq(from=-4, to=4, by=.01)
values<- 1- plogis(alpha + beta %*% t(x))
lines(x, apply(values,2,quantile, 0.975), ylim=c(0,1), type="l", lty=1, lwd=2, col=alpha("#f46d43",.4))
polygon(c(x,rev(x)),c(apply(values,2,quantile, 0.975), rev(apply(values,2,quantile, 0.025))),col=alpha("#f46d43",.4), border=NA )
lines(x, apply(values,2,mean), ylim=c(0,1), type="l", lwd=1.5, col="#4575b4")
dev.off()



###Figure A.5: Estimates of Uncertainty
rbPal <- colorRampPalette(c('red','blue'))
data$Col <- rbPal(10)[as.numeric(cut(data$theta_d,breaks = 10))]
pdf("plots/appendix/a5.pdf",height=7,width=7)
par(mar=c(4,4,3,3),  oma = c(0,0,0,0),mfrow=c(1,1))
plot(NULL,# create empty plot
     xlim = c(0.075,.9), 
     ylim = c(0.075,.9), 
     axes = F, xlab = NA, ylab = NA)  
abline(a=0,b=1, col="gray80",lty=1, lwd=1)
box()
grid()
points(data$theta_d_sd,data$theta_s_sd,bg=alpha("gray80",.4), col=alpha(data$Col,1),lwd=1,cex=.7, pch=21 )
axis(1)
axis(2)
mtext(side=2,"Static Model Standard Deviations ",line=2)
mtext(side=1,"Drift Model Standard Deviations",line=2.5)
box()
dev.off()



###Figure A.6: Summary of Posterior Predictive Checks, in Sample
load('output/pcp_output.RData')
load('output/loio_oos_pcp.RData')
y_lo<-c(.75,.92,.9,.9,.77,.9,.72,.45,.76,.61)
y_up<-c(.81,.96,.95,.94,.815,.945,.79,.575,.84,.75)
NAMES<-c("Military Entry","CGV Military Regime","Military Leader",
         "GWF Military Regime","Prior Military Regime","ARD Military Regime",
         "Leader Military Experience","Military Cabinet Ministers",
         "Mil. Involvement in Politics","Militarism Index")
pdf("plots/appendix/a6.pdf", width=8,height=8)
par(mfrow=c(4,3),mar=c(3,4,3,3))
for(ii in 1:10){
  boxplot(s_postpred[ii,],d_postpred[ii,], xaxt='n',
          xlab=c("Static","Dynamic","Drift"),
          main=NAMES[ii],lwd=.75,ylim=c(y_lo[ii],y_up[ii]),
          col=c("gray60","#74add1"),las=2)
  mtext("PCP", side = 2,line=2.75, cex=.8) 
  abline(h=0, col = "black", lty = 5)
}
dev.off()



###Figure A.7: Summary of Posterior Predictive Checks, Leave One Item Out Cross Validation
y_up<-c(.78,.92,.915,.92,.8,.92,.65,.49,.755,.57)
y_lo<-c(.71,.86,.85,.88,.77,.87,.55,.42,.69,.48)
pdf("plots/appendix/a7.pdf", width=8,height=8)
par(mfrow=c(4,3),mar=c(3,4,3,3))
for(ii in 1:10){
  boxplot(pcp_s[ii,],pcp_d[ii,], xaxt='n',
          xlab=c("Static","Dynamic","Drift"),
          main=NAMES[ii],lwd=.75,ylim=c(y_lo[ii],y_up[ii]),
          col=c("gray60","#74add1"),las=2)
  mtext("PCP", side = 2,line=3, cex=.8) 
  abline(h=0, col = "black", lty = 5)
}
dev.off()




#A.8: Summary of In-Sample Posterior Predictive Checks when Omitting Leader Military Experience Indicator
load('appendix/irt_models/pcp_output_k7.RData')
NAMES<-c("Military Entry","CGV Military Regime","Military Leader",
         "GWF Military Regime","Prior Military Regime","ARD Military Regime",
         "Military Cabinet Ministers",
         "Mil. Involvement in Politics","Militarism Index")
pdf("plots/appendix/a8.pdf", width=9,height=8)
par(mfrow=c(1,1),mar=c(4.5,15,3,3))
boxplot(d_postpred[1,]-s_postpred[1,],
        d_postpred[2,]-s_postpred[2,],
        d_postpred[3,]-s_postpred[3,],
        d_postpred[4,]-s_postpred[4,],
        d_postpred[5,]-s_postpred[5,],
        d_postpred[6,]-s_postpred[6,],
        d_postpred[7,]-s_postpred[7,],
        d_postpred[8,]-s_postpred[8,],
        d_postpred[9,]-s_postpred[9,],
        col=c(rep("#74add1",7),"#74add1",rep("#74add1",2)),
        main="Summary of Posterior Predictions \n(In Sample)",horizontal=T,
        xlim=c(0.4,9.5),ylim=c(-.075,0.13),cex.axis=1.2
)
axis(2,labels=NAMES,at=seq(1,9,1),las=2,cex.axis=1.2)
abline(v=0, col = "orangered", lty = 5)
mtext(side = 1, "Difference in Proportion Correctly Predicted",las=0,line=2.5,cex=1.4) 
mtext(side = 1, expression((PCP[Drift] - PCP[Static])),las=0,line=3.5,cex=1) 
text(0.005,9.6,"Static Model Superior   ",pos=2,col="gray40")
text(.12,9.6," Drift Model Superior ",pos=2,col="#313695")
arrows(-.072,9.62,-0.08,9.62, xpd = TRUE, lwd=1.5, length=.05,col="gray40")
arrows(0.12,9.62,.133,9.62, xpd = TRUE, lwd=1.5,length=.05,col="#313695" )
box() 
dev.off()



#A.9-A15: Cross-Section Plots
setwd('/Users/kenwick/Dropbox/ctrl_rep_final/plots/appendix')
years = c(1946,seq(1950,2010,10))
for (ii in years){
  pdf(print(as.character(ii)),height=7,width=10)
  subset.d <- subset(data, year==ii)  
  n = nrow(subset.d)
  n.5 = round(n/2)
  INDEX <- 1:n
  INDEX
  INDEX <- INDEX[order(subset.d$theta_d)]
  INDEX
  # take the means of the posterior distributions, based on the rank order of the means
  POSTERIORS <- subset.d$theta_d[INDEX]
  POSTERIORS1 <- POSTERIORS[1:n.5]
  POSTERIORS2 <- POSTERIORS[(n.5+1):n]
  POSTERIORS_s <- subset.d$theta_s[INDEX]
  POSTERIORS1_s <- POSTERIORS_s[1:n.5]
  POSTERIORS2_s <- POSTERIORS_s[(n.5+1):n]
  LABELS = as.character(subset.d$gwf_country)[INDEX]
  LABELS1 = LABELS[1:n.5]
  LABELS2 = LABELS[(n.5+1):n]
  par(mfrow=c(1,2),mar=c(3,2,1,5.5), font=2, font.lab=1, cex=1, cex.axis=.5, oma=c(1,0,2,0))
  plot(POSTERIORS1, (1:n.5)+.25, xlim=c(-5,5),cex=.5, ylab="Country", xlab=" ", yaxt="n", main="")
  mtext(side = 1, "Civilian Control", line=2, cex=.5)# add title
  points(POSTERIORS1_s, (1:n.5)-.25,cex=.5)
  abline(h=1:n.5, col="gray80",lty=5, lwd=.5)
  lb <- subset.d$theta_d_up[INDEX]
  lb1<- lb[1:n.5]
  lb2<- lb[(n.5+1):n]
  lb_s <- subset.d$theta_s_up[INDEX]
  lb1_s<- lb_s[1:n.5]
  lb2_s<- lb_s[(n.5+1):n]
  ub <- subset.d$theta_d_lw[INDEX]
  ub1<- ub[1:n.5]
  ub2<- ub[(n.5+1):n]
  ub_s <- subset.d$theta_s_lw[INDEX]
  ub1_s<- ub_s[1:n.5]
  ub2_s<- ub_s[(n.5+1):n]
  for(i in 1:n){
    lines(c(lb1[i], ub1[i]), c(i+.25,i+.25), col="#313695", lwd=2)
    lines(c(lb1_s[i], ub1_s[i]), c(i-.25,i-.25), col="gray60", lwd=2)
  }
  points(POSTERIORS1, (1:n.5)+.25, bg="#74add1", col="#313695", cex=.5, pch=21)
  points(POSTERIORS1_s, (1:n.5)-.25, bg="gray90", col="gray60", cex=.5, pch=21)
  text(-5.5,length(POSTERIORS1),as.character(ii),pos=4,col="gray30",cex=1.5)
  axis(side=4, at=1:n.5, labels=LABELS1, las=2, cex=1)
  plot(POSTERIORS2, 1:length(POSTERIORS2)+.25, xlim=c(-5,5),cex=.5, ylab=" ", xlab=" ", yaxt="n", main="")
  mtext(side = 1, "Civilian Control", line=2, cex=.5)# add title
  points(POSTERIORS2_s, 1:length(POSTERIORS2)-.25,cex=.5)
  abline(h=1:length(POSTERIORS2), col="gray80",lty=5, lwd=.5)
  
  for(i in 1:length(POSTERIORS2)){
    lines(c(lb2[i], ub2[i]), c(i+.25,i+.25), col="#313695", lwd=2, cex=.5)
    lines(c(lb2_s[i], ub2_s[i]), c(i-.25,i-.25), col="gray60", lwd=2, cex=.5)
  }
  points(POSTERIORS2, (1:length(POSTERIORS2))+.25, bg="#74add1", col="#313695", cex=.5, pch=21)
  points(POSTERIORS2_s, (1:length(POSTERIORS2))-.25, bg="gray90", col="gray60", cex=.5, pch=21)
  axis(side=4, at=1:length(POSTERIORS2), labels=LABELS2, las=2)
  text(-5.5,length(POSTERIORS2)-2,"Static Estimates",pos=4,col="gray60",cex=.75)
  text(-5.5,length(POSTERIORS2),"Drift Estimates",pos=4,col="#313695", cex=.75)
  dev.off()
}
setwd('/Users/kenwick/Dropbox/ctrl_rep_final/')


#Figure A.16: Distribution of Civilian Control Estimates Across Regime Type
gwf<-read.dta('appendix/GWF_AllPoliticalRegimes.dta')
data2<-merge(data,gwf,by.x=c('ccode','year'),by.y=c('cowcode','year'),all.x=T,all.y=F)
data2$gwf_democ<-0
data2$gwf_democ[data2$gwf_nonautocracy=="democracy"] <-1
col<-brewer.pal(5,"Set1")
data2$col<-"Grey80"
data2$col[data2$gwf_monarchy==1]<-col[4]
data2$col[data2$gwf_party==1]<-col[1]
data2$col[data2$gwf_personal==1]<-col[5]
data2$col[data2$gwf_mil==1]<-col[3]
data2$col[data2$gwf_democ==1]<-col[2]

pdf("plots/appendix/a16.pdf", width=8,height=8)
par(mfrow=c(4,1),mar=c(5,5,3,3))
plot(NULL,
     xlim = c(-4,4), 
     ylim = c(0,1.5), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(data2$theta_d[data2$gwf_mil==1],bw=.2),col=alpha(col[3],.75),border=NA)  
polygon(density(data2$theta_s[data2$gwf_mil==1],bw=.2),col=alpha(col[3],.45),border=NA)  
axis(side=1, at=seq(-4,4,2), las=1, cex.axis=1.2,mgp=c(3,.4,0))
axis(side=2, at=seq(0,1,.5), las=2, cex.axis=1.2)
title("Military Regimes", cex.main=1.4)
mtext("Density    ", side = 2,line=3.5,cex=1.2) 
text(4,1,"Drift Estimates",pos=2,col=alpha(col[3],.75),cex=1.2)
text(4,.7,"Static Estimates",pos=2,col=alpha(col[3],.45),cex=1.2)
mtext("Civilian Control", side = 1, line=1.5,cex=1.2) 
box()

plot(NULL,
     xlim = c(-4,4), 
     ylim = c(0,1.5), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(data2$theta_d[data2$gwf_personal==1 & !is.na(data2$gwf_personal)],bw=.2),col=alpha(col[5],.75),border=NA)  
polygon(density(data2$theta_s[data2$gwf_personal==1 & !is.na(data2$gwf_personal)],bw=.2),col=alpha(col[5],.45),border=NA)  
axis(side=1, at=seq(-4,4,2), las=1, cex.axis=1.2,mgp=c(3,.4,0))
axis(side=2, at=seq(0,1,.5), las=2, cex.axis=1.2)
title("Personalist Regimes", cex.main=1.4)
mtext("Density    ", side = 2,line=3.5,cex=1.2)
text(4,1,"Drift Estimates",pos=2,col=alpha(col[5],.75),cex=1.2)
text(4,.7,"Static Estimates",pos=2,col=alpha(col[5],.45),cex=1.2)
mtext("Civilian Control", side = 1, line=1.5,cex=1.2) 
box()

plot(NULL,
     xlim = c(-4,4), 
     ylim = c(0,1.5), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(data2$theta_d[data2$gwf_party==1 & !is.na(data2$gwf_party)],bw=.2),col=alpha(col[1],.75),border=NA)  
polygon(density(data2$theta_s[data2$gwf_party ==1 & !is.na(data2$gwf_party)],bw=.2),col=alpha(col[1],.45),border=NA)  
axis(side=1, at=seq(-4,4,2), las=1, cex.axis=1.2,mgp=c(3,.4,0))
axis(side=2, at=seq(0,1,.5), las=2, cex.axis=1.2)
title("Party Regimes", cex.main=1.4)
mtext("Density    ", side = 2,line=3.5,cex=1.2) # label bottom axis
text(4,1,"Drift Estimates",pos=2,col=alpha(col[1],.75),cex=1.2)
text(4,.7,"Static Estimates",pos=2,col=alpha(col[1],.45),cex=1.2)
mtext("Civilian Control", side = 1, line=1.5,cex=1.2) # label bottom axis
box()

plot(NULL,
     xlim = c(-4,4), 
     ylim = c(0,1.5), 
     axes = F, xlab = NA, ylab = NA) 
polygon(density(data2$theta_d[data2$gwf_democ==1 & !is.na(data2$gwf_democ)],bw=.2),col=alpha(col[2],.75),border=NA)  
polygon(density(data2$theta_s[data2$gwf_democ==1 & !is.na(data2$gwf_democ)],bw=.2),col=alpha(col[2],.45),border=NA)  
axis(side=1, at=seq(-4,4,2), las=1, cex.axis=1.2,mgp=c(3,.4,0))
axis(side=2, at=seq(0,1,.5), las=2, cex.axis=1.2)
title("Democracies", cex.main=1.4)
mtext("Density    ", side = 2,line=3.5,cex=1.2)
text(4,1,"Drift Estimates",pos=2,col=alpha(col[2],.75),cex=1.2)
text(4,.7,"Static Estimates",pos=2,col=alpha(col[2],.45),cex=1.2)
mtext("Civilian Control", side = 1, line=1.5,cex=1.2) 
box()
dev.off()


#Figure A.17: Drift Parameter Across Age of Civilianized Regimes
#re-load data
load("output/drift_output.RData")
drfn<-output$drift
drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}
drift_hi2 <- apply(drfn2, 2, quantile, 0.975 )
drift_lo2 <- apply(drfn2, 2, quantile, 0.025 )
drift_mean2<-apply(drfn2, 2, mean )
age<-seq(1:length(drift_hi2))
rm(output)
load('appendix/irt_models/drift_alt1_output.RData')
drfn<-output$drift
drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}
drift_mean_alt1<-apply(drfn2, 2, mean )
rm(output)
load('appendix/irt_models/drift_alt2_output.RData')
drfn<-output$drift
drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}
drift_mean_alt2<-apply(drfn2, 2, mean )
rm(output)

pdf('plots/appendix/a17.pdf',width=8,height=8)
par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,5,4,2),
  mfrow= c(1,1)
)
plot(NULL,# create empty plot
     xlim = c(0,65), 
     ylim = c(-.5,2), 
     axes = F, xlab = NA, ylab = NA)  
abline(h=0, col="gray80",lty=1, lwd=1)
for (ii in 1:length(age)){
  lines(c(age[ii],age[ii]),c(drift_hi2[ii], drift_lo2[ii]),col="black", lwd=1.25)
  points(age[ii],drift_mean2[ii],bg=c("grey70"), col="black",lwd=1,cex=.5, pch=21 )
}
lines(1:65,drift_mean_alt1,col="#74c476", lwd=1.75)
lines(1:65,drift_mean_alt2,col="#3182bd", lwd=1.75)
axis(1)
axis(2)
mtext(side=2,"Cumulative Drift",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()
dev.off()



#Figure A.18: Mean Estimates of Civilian Control for Civilianized Observations, 1992-2010
load('appendix/irt_models/data_prepped_postcw.RData')
load('appendix/irt_models/static_output_postcw.RData')
data$theta_s<-apply(output$x,2,mean)
theta_s_pcw_draws<-t(output$x)
load('appendix/irt_models/drift_output_postcw.RData')
data$theta_d<-apply(output$x,2,mean)
theta_d_pcw_draws<-t(output$x)
theta_d_pcw_draws<-apply(theta_d_pcw_draws,2,scale)
id<-seq(1:nrow(data))
age<-drift_yrs
means_s<-matrix(ncol=max(drift_yrs),nrow=ncol(theta_s_pcw_draws))
for(ii in 1:ncol(theta_s_pcw_draws)){
  for(jj in 1:max(drift_yrs)){
    means_s[ii,jj] <- mean(theta_s_pcw_draws[id[drift_yrs==jj & drift_ind==1],ii])
  }
}
age<-seq(1:max(drift_yrs))
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
means<-matrix(ncol=max(drift_yrs),nrow=ncol(theta_s_pcw_draws))
for(ii in 1:ncol(theta_d_pcw_draws)){
  for(jj in 1:max(drift_yrs)){
    means[ii,jj] <- mean(theta_d_pcw_draws[id[drift_yrs==jj & drift_ind==1],ii])
  }
}
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25

pdf(file="plots/appendix/a18.pdf",height=6,width=6)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4,2,2),
  mfrow= c(1,1)
)
plot(NULL,# create empty plot
     xlim = c(1,19), # set xlim by guessing
     ylim = c(0.1,.8), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels  
reg_s<-lm(mean_s~age, data=age_data)
reg<-lm(mean~age,data=age_data)
for (ii in 0:18){
  lines(c(age_data$age[ii]-.25,age_data$age[ii]-.25),c(age_data$mean_up_s[ii], age_data$mean_lw_s[ii]),col="gray60", lwd=1.75)
  points(age_data$age[ii]-.25,age_data$mean_s[ii],bg=c("gray90"), col="gray60",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:18){
  lines(c(age_data$age[ii]+.25,age_data$age[ii]+.25),c(age_data$mean_up[ii], age_data$mean_lw[ii]),col="#313695", lwd=1.75)
  points(age_data$age[ii]+.25,age_data$mean[ii],bg=c("#74add1"), col="#313695",  cex=.65, pch=21, lwd=1.5)
}
lab=seq(1,20,by=2)
axis(1, at=lab, labels=lab,cex.axis=1)
axis(4,at=NULL, labels=FALSE,tick=FALSE,col="white",cex.axis=1)
axis(side=2,  las=1,cex.axis=1)
mtext(side = 1, "Years Civilianized", line =2.5,cex=1.25) # label bottom axis
mtext(side = 2, "Mean Civilian Control", line=2.5, cex=1.25)# add title
text(0.5,.8,"Static",pos=4,col="gray70",cex=1.5)
text(0.5,.75,"Drift",pos=4,col="#313695", cex=1.5)
box()
dev.off()




#Figure A.19: Drift Parameter Across Duration Civilianized or Non-Civilianized, Dual-Drift Model
rm(list=ls())
load("appendix/irt_models/drift_mil_output.RData")

drfnm<-output$drift2 
drift_him <- apply(drfnm, 2, quantile, 0.975 )
drift_lom <- apply(drfnm, 2, quantile, 0.025 )
drift_meanm<-apply(drfnm, 2, mean )
agem<-seq(1:length(drift_him))

drfn2m<-matrix(nrow=nrow(drfnm),ncol=ncol(drfnm),NA)
drfn2m[,1]<-drfnm[,1]
for (ii in 2:ncol(drfnm)){
  drfn2m[,ii]<-apply(drfnm[,1:ii],1,sum)
}
drift_hi2m <- apply(drfn2m, 2, quantile, 0.975 )
drift_lo2m <- apply(drfn2m, 2, quantile, 0.025 )
drift_mean2m<-apply(drfn2m, 2, mean )

drfn<-output$drift
drift_hi <- apply(drfn, 2, quantile, 0.975 )
drift_lo <- apply(drfn, 2, quantile, 0.025 )
drift_mean<-apply(drfn, 2, mean )
age<-seq(1:length(drift_hi))
drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}

drift_hi2 <- apply(drfn2, 2, quantile, 0.975 )
drift_lo2 <- apply(drfn2, 2, quantile, 0.025 )
drift_mean2<-apply(drfn2, 2, mean )

pdf('plots/appendix/a19.pdf',width=9,height=6)
par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,5,4,2),
  mfrow= c(1,2)
)
plot(NULL,# create empty plot
     xlim = c(0,64), 
     ylim = c(-.15,.25), 
     axes = F, xlab = NA, ylab = NA) 
abline(h=0, col="gray80", lwd=1)
for (ii in 1:length(age)){
  lines(c(age[ii],age[ii]),c(drift_hi[ii], drift_lo[ii]),col="black", lwd=1.25)
  points(age[ii],drift_mean[ii],bg=c("gray80"), col="black",lwd=1,cex=.5, pch=21 )
  lines(c(agem[ii]+.5,agem[ii]+.5),c(drift_him[ii], drift_lom[ii]),col="#238b45", lwd=1.25)
  points(agem[ii]+.5,drift_meanm[ii],bg=c("#e5f5f9"), col="#238b45",lwd=1,cex=.5, pch=21 )
}
text(0,.25,"Civilianized Drift",pos=4,col=alpha("black",1),cex=1.25)
text(0,.225,"Non-Civilianized Drift",pos=4,col="#238b45",cex=1.25)
axis(1)
axis(2)
mtext(side=2,"Drift Parameter Value",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()

plot(NULL,# create empty plot
     xlim = c(0,65), 
     ylim = c(-.5,2.75), 
     axes = F, xlab = NA, ylab = NA)  
abline(h=0, col="gray80",lty=1, lwd=1)
for (ii in 1:length(age)){
  lines(c(age[ii],age[ii]),c(drift_hi2[ii], drift_lo2[ii]),col="black", lwd=1.25)
  points(age[ii],drift_mean2[ii],bg=c("gray80"), col="black",lwd=1,cex=.5, pch=21 )
  lines(c(agem[ii]+.5,agem[ii]+.5),c(drift_hi2m[ii], drift_lo2m[ii]),col="#238b45", lwd=1.25)
  points(agem[ii]+.5,drift_mean2m[ii],bg=c("#e5f5f9"), col="#238b45",lwd=1,cex=.5, pch=21 )
}
text(0,2.75,"Civilianized Drift",pos=4,col=alpha("black",1),cex=1.25)
text(0,2.55,"Non-Civilianized Drift",pos=4,col="#238b45",cex=1.25)
axis(1)
axis(2)
mtext(side=2,"Cumulative Drift",line=2)
mtext(side=1,"Years Civilianized/Non-Civilianized",line=2)
box()
dev.off()




#Figure A.20: Mean Estimates of Civilian Control for Civilianized and Non-Civilianized Observations, Dual Drift Model 
rm(list=ls())
library(ggplot2)
load("output/static_output.RData")
output_s<-output
load("appendix/irt_models/drift_mil_output.RData")
load("irt_modeling/data_prepped.RData")
driftmil_draws<-output$x
driftmil_draws<-t(apply(driftmil_draws,1,scale))
static_draws<-output_s$x
id<-seq(1:nrow(data))
age<-drift_yrs
means_s<-matrix(ncol=max(observed_drift_yrs),nrow=ncol(static_draws))
for(ii in 1:ncol(static_draws)){
  for(jj in 1:max(observed_drift_yrs)){
    means_s[ii,jj] <- mean(static_draws[ii,id[observed_drift_yrs==jj & drift_ind==1]])
  }
}
age<-seq(1:max(observed_drift_yrs))
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
means<-matrix(ncol=max(observed_drift_yrs),nrow=ncol(driftmil_draws))
for(ii in 1:ncol(driftmil_draws)){
  for(jj in 1:max(observed_drift_yrs)){
    means[ii,jj] <- mean(driftmil_draws[ii,id[observed_drift_yrs==jj & drift_ind==1]])
  }
}
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25
age_m<-drift_yrs_m
means_s_m<-matrix(ncol=max(drift_yrs_m),nrow=ncol(static_draws))
for(ii in 1:ncol(static_draws)){
  for(jj in 1:max(drift_yrs_m)){
    means_s_m[ii,jj] <- mean(static_draws[ii,id[drift_yrs_m==jj & drift_ind_m==1]])
  }
}
age_m<-seq(1:max(drift_yrs_m))
age_data_m<-as.data.frame(age_m)
age_data_m$mean_s_m <-  apply(means_s_m, 2, mean)
age_data_m$mean_up_s_m <- apply(means_s_m, 2, quantile, 0.975 )
age_data_m$mean_lw_s_m <- apply(means_s_m, 2, quantile, 0.025 )
means_m<-matrix(ncol=max(drift_yrs_m),nrow=ncol(driftmil_draws))
for(ii in 1:ncol(driftmil_draws)){
  for(jj in 1:max(drift_yrs_m)){
    means_m[ii,jj] <- mean(driftmil_draws[ii,id[drift_yrs_m==jj & drift_ind_m==1]])
  }
}
age_data_m$mean_m <-  apply(means_m, 2, mean)
age_data_m$mean_up_m <- apply(means_m, 2, quantile, 0.975 )
age_data_m$mean_lw_m <- apply(means_m, 2, quantile, 0.025 )
age_data_m$age_s_m = age_data_m$age_m-.25
age_data_m$age_d_m = age_data_m$age_m+.25

pdf(file="plots/appendix/a20.pdf",height=7,width=7)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4,2,2),
  mfrow= c(1,1)
)
plot(NULL,# create empty plot
     xlim = c(1,65), # set xlim by guessing
     ylim = c(-2,3), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels  

reg_s<-lm(mean_s~age, data=age_data)
reg<-lm(mean~age,data=age_data)
for (ii in 0:65){
  lines(c(age_data$age[ii]-.25,age_data$age[ii]-.25),c(age_data$mean_up_s[ii], age_data$mean_lw_s[ii]),col="gray60", lwd=1.75)
  points(age_data$age[ii]-.25,age_data$mean_s[ii],bg=c("gray90"), col="gray60",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:65){
  lines(c(age_data_m$age_m[ii]-.25,age_data_m$age_m[ii]-.25),c(age_data_m$mean_up_s_m[ii], age_data_m$mean_lw_s_m[ii]),col="gray60", lwd=1.75)
  points(age_data_m$age_m[ii]-.25,age_data_m$mean_s_m[ii],bg=c("gray90"), col="gray60",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:65){
  lines(c(age_data$age[ii]+.25,age_data$age[ii]+.25),c(age_data$mean_up[ii], age_data$mean_lw[ii]),col="#313695", lwd=1.75)
  points(age_data$age[ii]+.25,age_data$mean[ii],bg=c("#74add1"), col="#313695",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:65){
  lines(c(age_data_m$age_m[ii]+.25,age_data_m$age_m[ii]+.25),c(age_data_m$mean_up_m[ii], age_data_m$mean_lw_m[ii]),col="#313695", lwd=1.75)
  points(age_data_m$age_m[ii]+.25,age_data_m$mean_m[ii],bg=c("#74add1"), col="#313695",  cex=.65, pch=21, lwd=1.5)
}
lab=c(0,(seq(5,65,by=5)))
axis(1, at=lab, labels=lab,cex.axis=1)
axis(4,at=NULL, labels=FALSE,tick=FALSE,col="white",cex.axis=1)
axis(side=2,  las=1,cex.axis=1)
mtext(side = 1, "Years Civilianized/Non-Civilianized", line =2.5,cex=1.25) # label bottom axis
mtext(side = 2, "Mean Civilian Control", line=2, cex=1.25)# add title
text(0,2.75,"Static Estimates",pos=4,col="gray70",cex=1.5)
text(0,2.4,"Drift Estimates",pos=4,col="#313695", cex=1.5)
box()
text(0,1,"Civilianized Observations",cex=1,pos=4)
text(0,-.5,"Non-Civilianized Observations",cex=1,pos=4)
dev.off()




#Figure A.21: Civilian Control and Years Since Last Coup
rm(list=ls())
library(foreign)
library(ggplot2)
library(dplyr)
coups<-read.dta("irt_modeling/pt_coup.dta")
panel = coups %>%
  group_by(ccode) %>%
  mutate(coup1 = lag(coup1, order_by=ccode))
panel$coup1[is.na(panel$coup1)]<-0
#This mini-function will count the number of drift years
PANEL = data.table(panel)
PANEL[, c("coup_yrs") := {
  rr <- sequence(rle(coup1)$lengths)
}, by=ccode]
panel = as.data.frame(PANEL)
coups = dplyr::arrange(panel, ccode, year)
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")
load("irt_modeling/data_prepped.RData")

#Extract model estiamtes and append to data object
data$theta_s<-apply(output_s$x,2,mean)
theta_d_draws<-t(apply(output$x,1,scale))
data$theta_d<-apply(theta_d_draws,2,mean)
data<-merge(data, coups, by=c('ccode','year'),all.x=T,all.y=F)
round(cor(data$coup_yrs,data$theta_s,method="spearman",use="pairwise.complete.obs"),3)
round(cor(data$coup_yrs,data$theta_d,method="spearman",use="pairwise.complete.obs"),3)

pdf(file="plots/appendix/a21.pdf",height=12,width=6)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4,4,2),
  mfrow= c(2,1)
)
plot(NULL,# create empty plot
     xlim = c(1,65), # set xlim by guessing
     ylim = c(-2.5,1), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels  
points(data$coup_yrs,data$theta_s,bg=alpha("gray90",.5), col=alpha("gray60",.5),  cex=.65, pch=21, lwd=1.5)
reg_s<-lm(data$theta_s~data$coup_yrs)
#abline(reg_s, col="green" )
axis(2,at=c(-2,-1,0,1),cex.axis=1)
axis(1,cex.axis=1)
mtext(side = 1, "Years Since Last Coup", line =2.5,cex=1.25) 
mtext(side = 2, "Civilian Control", line =2.5,cex=1.25) 
title("Static Model Estimates")
plot(NULL,# create empty plot
     xlim = c(1,60), # set xlim by guessing
     ylim = c(-3.5,3.5), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels 
points(data$coup_yrs,data$theta_d,bg=alpha("#74add1",.5), col=alpha("#313695",.5),  cex=.65, pch=21, lwd=1.5)
reg_d<-lm(data$theta_d~data$coup_yrs)
axis(2,cex.axis=1)
axis(1,cex.axis=1)
mtext(side = 1, "Years Since Last Coup", line =2.5,cex=1.25) 
mtext(side = 2, "Civilian Control", line =2.5,cex=1.25) 
title("Drift Model Estimates")
dev.off()




#Figure A.22: Civilian Control and Coup-Proofing
rm(list=ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(foreign)
set.seed(1989)
#Load and prep data
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")
load('irt_modeling/data_prepped.RData')
data$theta_s<-apply(output_s$x,2,mean)
data$theta_s_sd<-apply(output_s$x,2,sd)
theta_s_draws<-output_s$x
theta_d_draws<-t(apply(output$x,1,scale))
data$theta_d<-apply(theta_d_draws,2,mean)
data$theta_d_sd<-apply(theta_d_draws,2,sd)
debruin<-read.dta('appendix/debruin.dta')
pb<-read.dta('appendix/Coup-Proofing1970-2015.dta')
data<-merge(data, debruin, by=c('ccode','year'), all.x=T, all.y=F)
data<-merge(data, pb, by=c('ccode','year'), all.x=T, all.y=F)
data$db_miss<-ifelse(is.na(data$indepcount),1,0)
data$ef_miss<-ifelse(is.na(data$effective_number),1,0)

pdf("plots/appendix/a22.pdf", width=9,height=7)
par(mfrow=c(2,2),mar=c(3.75,3.5,2,2))
plot(NULL,
     ylim = c(0,9), 
     xlim = c(-2.5,1.5), 
     axes = F, xlab = NA, ylab = NA) 
points(data$theta_s, data$indepcount,col=alpha("black",.2),bg=alpha("black",.05), cex=.75, pch=21 )
axis(side=2, at=seq(0,9,1 ), las=1, lwd=.25, mgp=c(3,.5,0), cex.axis=.75)
axis(side=1, at=seq(-3.5,3.5,.5), las=1, lwd=.25,mgp=c(3,.5,0), cex.axis=.75)
box()
mtext("Military Counterweights\n(De Bruin)", side = 2, line=1.5, cex=.9)
mtext(expression(paste("Static Civilian Control Estimates")), side = 1, line=2.2, cex=.9)
title("", cex.main=.9)
abline(v= mean(na.omit(data$theta_s)),col="#f4a582",lwd=2,lty=2)
abline(h= mean(na.omit(data$indepcount)),col="#f4a582",lwd=2,lty=2)
abline(lm(indepcount~theta_s, data=data),col="#b2182b",lty=1,lwd=2.5)
text(-2.5,8,"Bivariate Regression",pos=4,cex=1,col="#b2182b")
text(-2.5,7.5,"Mean",pos=4,cex=1,col="#f4a582")

plot(NULL,
     ylim = c(0,9), 
     xlim = c(-3.5,3.5), 
     axes = F, xlab = NA, ylab = NA) 
points(data$theta_d, data$indepcount,col=alpha("#313695",.2),bg=alpha("#74add1",.05), cex=.75, pch=21 )
axis(side=2, at=seq(0,9,1 ), las=1, lwd=.25, mgp=c(3,.5,0), cex.axis=.75)
axis(side=1, at=seq(-3.5,3.5,.5), las=1, lwd=.25,mgp=c(3,.5,0), cex.axis=.75)
box()
mtext("Military Counterweights\n(DeBruin)", side = 2, line=1.5, cex=.9)
mtext(expression(paste("Drift Civilian Control Estimates")), side = 1, line=2.2, cex=.9)
title("", cex.main=.9)
abline(v= mean(na.omit(data$theta_d)),col="#f4a582",lwd=2,lty=2)
abline(h= mean(na.omit(data$indepcount)),col="#f4a582",lwd=2,lty=2)
abline(lm(indepcount~theta_d, data=data),col="#b2182b",lty=1,lwd=2.5)
text(-3.5,8,"Bivariate Regression",pos=4,cex=1,col="#b2182b")
text(-3.5,7.5,"Mean",pos=4,cex=1,col="#f4a582")

plot(NULL,
     ylim = c(1,4.5), 
     xlim = c(-2.5,1), 
     axes = F, xlab = NA, ylab = NA) 
for (ii in 0:9){
  abline(h=ii, col="gray90",lty=1, lwd=.75)
  abline(h=ii+.5, col="gray90",lty=1, lwd=.25)
}
points(data$theta_s, data$effective_number,col=alpha("black",.2),bg=alpha("black",.05), cex=.5, pch=21 )
axis(side=2, at=seq(0,9,1 ), las=1, lwd=.25, mgp=c(3,.5,0), cex.axis=.75)
axis(side=1, at=seq(-3.5,3.5,.5), las=1, lwd=.25,mgp=c(3,.5,0), cex.axis=.75)
box()
mtext("Effective Number\n(Pilster and Bohmelt)", side = 2, line=1.5, cex=.9)
mtext(expression(paste("Static Civilian Control Estimates")), side = 1, line=2.2, cex=.9)
title("", cex.main=.9)
abline(v= mean(na.omit(data$theta_s)),col="#f4a582",lwd=2,lty=2)
abline(h= mean(na.omit(data$effective_number)),col="#f4a582",lwd=2,lty=2)
abline(lm(effective_number~theta_s, data=data),col=alpha("#b2182b",1),lty=1,lwd=2.5)
text(-2.5,4.25,"Bivariate Regression",pos=4,cex=1,col="#b2182b")
text(-2.5,4.05,"Mean",pos=4,cex=1,col="#f4a582")

plot(NULL,
     ylim = c(1,4.5), 
     xlim = c(-3.5,3.5), 
     axes = F, xlab = NA, ylab = NA) 
for (ii in 0:9){
  abline(h=ii, col="gray90",lty=1, lwd=.75)
  abline(h=ii+.5, col="gray90",lty=1, lwd=.25)
}
points(data$theta_d, data$effective_number,col=alpha("#313695",.2),bg=alpha("#74add1",.05), cex=.5, pch=21 )
axis(side=2, at=seq(0,9,1 ), las=1, lwd=.25, mgp=c(3,.5,0), cex.axis=.75)
axis(side=1, at=seq(-3.5,3.5,.5), las=1, lwd=.25,mgp=c(3,.5,0), cex.axis=.75)
box()
mtext("Effective Number\n(Pilster and Bohmelt)", side = 2, line=1.5, cex=.9)
mtext(expression(paste("Drift Civilian Control Estimates")), side = 1, line=2.2, cex=.9)
title("", cex.main=.9)
abline(v= mean(na.omit(data$theta_d)),col="#f4a582",lwd=2,lty=2)
abline(h= mean(na.omit(data$effective_number)),col="#f4a582",lwd=2,lty=2)
abline(lm(effective_number~theta_d, data=data),col=alpha("#b2182b",1),lty=1,lwd=2.5)
text(-3.5,4.25,"Bivariate Regression",pos=4,cex=1,col="#b2182b")
text(-3.5,4.05,"Mean",pos=4,cex=1,col="#f4a582")
dev.off()




#Table A.3: Evaluating the Obsolescence Hypothesis
rm(list=ls())
set.seed(1989)
#Load and prep data
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")
load('irt_modeling/data_prepped.RData')
#Extract model estiamtes and append to data object
data$theta_s<-apply(output_s$x,2,mean)
theta_s_draws<-output_s$x
theta_d_draws<-t(apply(output$x,1,scale))
data$theta_d<-apply(theta_d_draws,2,mean)
data$observed_drift_yrs<-observed_drift_yrs
data$drift_yrs<-drift_yrs
data$drift_ind<-drift_ind
nmc<-read.csv('appendix/NMC_v4_0.csv')
data<-merge(data,nmc,by=c('ccode','year'),all.x=T,all.y=F)
data$milpop<-(data$milper/data$tpop)*100
#models with milpop as DV
fit1<-lm(milpop~observed_drift_yrs+year,data=data)
data$diff<-data$theta_d-data$theta_s
fit2<-lm(diff~observed_drift_yrs+year,data=subset(data,drift_ind==1))
summary(fit1)
summary(fit2)




###Figure A.23: Mean Estimates of Civilian Control for Civilianized Regimes, Dropping Left-Censored Regimes
#Script Generates figures and runs analyses reported in the main text
rm(list=ls())
library(dplyr)
set.seed(1989)
#Load and prep data
load('appendix/irt_models/data_prepped_drop_censored.RData')
load("appendix/irt_models/static_output_drop_censored.RData")
theta_s_draws<-output$x
load("appendix/irt_models/drift_output_drop_censored.RData")
theta_d_draws<-t(apply(output$x,1,scale))

id<-seq(1:nrow(data))
means_s<-matrix(ncol=max(drift_yrs_uncapped),nrow=nrow(theta_s_draws))
for(jj in 1:max(drift_yrs_uncapped)){
  x<-theta_s_draws[,id[drift_yrs_uncapped==jj & drift_ind==1]]
  means_s[,jj]<-apply(x,1,mean)
}
means<-matrix(ncol=max(drift_yrs_uncapped),nrow=nrow(theta_d_draws))
for(jj in 1:max(drift_yrs_uncapped)){
  x<-theta_d_draws[,id[drift_yrs_uncapped==jj & drift_ind==1]]
  means[,jj]<-apply(x,1,mean)
}
age<-seq(1:max(drift_yrs_uncapped))
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25

pdf(file="plots/appendix/a23.pdf",height=6,width=7)
par(
  family = "sans",
  oma = c(0,0,0,0),
  mar = c(4,4,2,2),
  mfrow= c(1,1)
)
plot(NULL,# create empty plot
     xlim = c(1,65), # set xlim by guessing
     ylim = c(-1,4), # set ylim by the number of variables
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
text(0,2.75,"Static",pos=4,col="gray60",cex=1.5)
text(0,2.5,"Dynamic Drift",pos=4,col="#313695", cex=1.5)
box()
dev.off()



###Figure A.24: Drift Parameter Across Age of Civilianized Regimes, Dropping Left-Censored Regimes
drfn<-output$drift
drfn<-cbind(drfn,matrix(rep(drfn[,50],14),ncol=14) )
drift_hi <- apply(drfn, 2, quantile, 0.975 )
drift_lo <- apply(drfn, 2, quantile, 0.025 )
drift_mean<-apply(drfn, 2, mean )
age<-seq(1:length(drift_hi))

drfn2<-matrix(nrow=nrow(drfn),ncol=ncol(drfn),NA)
drfn2[,1]<-drfn[,1]
for (ii in 2:ncol(drfn)){
  drfn2[,ii]<-apply(drfn[,1:ii],1,sum)
}

drift_hi2 <- apply(drfn2, 2, quantile, 0.975 )
drift_lo2 <- apply(drfn2, 2, quantile, 0.025 )
drift_mean2<-apply(drfn2, 2, mean )


pdf('plots/appendix/a24.pdf',width=9,height=6)
par(
  family = "serif",
  oma = c(0,0,0,0),
  mar = c(5,5,4,2),
  mfrow= c(1,2)
)
plot(NULL,# create empty plot
     xlim = c(0,64), 
     ylim = c(-.15,.29), 
     axes = F, xlab = NA, ylab = NA) 
abline(h=0, col="gray80", lwd=1)
for (ii in 1:50){
  lines(c(age[ii],age[ii]),c(drift_hi[ii], drift_lo[ii]),col="#313695", lwd=1.25)
  points(age[ii],drift_mean[ii],bg=c("#74add1"), col="#313695",lwd=1,cex=.5, pch=21 )
}
axis(1)
axis(2)
mtext(side=2,"Drift Parameter Value",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()

plot(NULL,# create empty plot
     xlim = c(0,65), 
     ylim = c(-.5,3.2), 
     axes = F, xlab = NA, ylab = NA)  
abline(h=0, col="gray80",lty=1, lwd=1)
for (ii in 1:length(age)){
  lines(c(age[ii],age[ii]),c(drift_hi2[ii], drift_lo2[ii]),col="#313695", lwd=1.25)
  points(age[ii],drift_mean2[ii],bg=c("#74add1"), col="#313695",lwd=1,cex=.5, pch=21 )
}

axis(1)
axis(2)
mtext(side=2,"Cumulative Drift",line=2)
mtext(side=1,"Years Civilianized",line=2)
box()
dev.off()




###Figure A.25:Mean Estimates of Civilian Control for Civilianized Regimes
rm(list=ls())
library(dplyr)
library(ggplot2)
set.seed(1989)
load("output/static_output.RData")
output_s<-output
load("output/drift_output.RData")
load("irt_modeling/data_prepped.RData")
theta_s_draws<-output_s$x
theta_d_draws<-t(apply(output$x,1,scale))
id<-seq(1:nrow(data))
means_s<-matrix(ncol=100,nrow=nrow(theta_s_draws))
for(jj in 1:100){
  x<-theta_s_draws[,id[drift_yrs_uncapped==jj & drift_ind==1]]
  means_s[,jj]<-apply(x,1,mean)
}
means<-matrix(ncol=100,nrow=nrow(theta_d_draws))
for(jj in 1:100){
  x<-theta_d_draws[,id[drift_yrs_uncapped==jj & drift_ind==1]]
  means[,jj]<-apply(x,1,mean)
}
age<-seq(1:100)
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25

pdf(file="plots/a25.pdf",height=7,width=6)
par(mfrow=c(1,1))
plot(NULL,# create empty plot
     xlim = c(1,100), # set xlim by guessing
     ylim = c(-1,2), # set ylim by the number of variables
     axes = F, xlab = NA, ylab = NA)  # turn off axes and labels  
grid()
abline(lm(age_data$mean~age_data$age),col="#74add1")
abline(lm(age_data$mean_s~age_data$age),col="gray60")
for (ii in 0:100){
  lines(c(age_data$age[ii]-.25,age_data$age[ii]-.25),c(age_data$mean_up_s[ii], age_data$mean_lw_s[ii]),col="gray60", lwd=1.75)
  points(age_data$age[ii]-.25,age_data$mean_s[ii],bg=c("gray80"), col="gray60",  cex=.65, pch=21, lwd=1.5)
}
for (ii in 0:100){
  lines(c(age_data$age[ii]+.25,age_data$age[ii]+.25),c(age_data$mean_up[ii], age_data$mean_lw[ii]),col="#313695", lwd=1.75)
  points(age_data$age[ii]+.25,age_data$mean[ii],bg=c("#74add1"), col="#313695",  cex=.65, pch=21, lwd=1.5)
}
lab=c(0,(seq(5,100,by=5)))
axis(1, at=lab, labels=lab,cex.axis=1)
axis(4,at=NULL, labels=FALSE,tick=FALSE,col="white",cex.axis=1)
axis(side=2,  las=1,cex.axis=1)
mtext(side = 1, "Civilianized Regime Age", line =2.5,cex=1.25) # label bottom axis
mtext(side = 2, "Mean Civilian Control", line=2.25, cex=1.25)# add title
text(0,2.75,"Static",pos=4,col="gray60",cex=1.5)
text(0,2.5,"Dynamic Drift",pos=4,col="#313695", cex=1.5)
box()
dev.off()



###Figure A.26:Comparison of Static, Dynamic, and Drift Models
rm(list=ls())
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stats4)
library(MASS)
set.seed(1989)
#Set the working directory to the top-level of the replication folder
#Load and prep data
load("irt_modeling/data_prepped.RData")
load('appendix/irt_models/dynamic_output.RData')
output_dy<-output
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
theta_dy_draws<-t(apply(output_dy$x,1,scale))
data$theta_dy<-apply(theta_dy_draws,2,mean)
data$theta_dy_sd<-apply(theta_dy_draws,2,sd)

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
means_dy<-matrix(ncol=max(observed_drift_yrs),nrow=nrow(theta_dy_draws))
for(jj in 1:max(observed_drift_yrs)){
  x<-theta_dy_draws[,id[observed_drift_yrs==jj & drift_ind==1]]
  means_dy[,jj]<-apply(x,1,mean)
}
age<-seq(1:max(observed_drift_yrs))
age_data<-as.data.frame(age)
age_data$mean_s <-  apply(means_s, 2, mean)
age_data$mean_up_s <- apply(means_s, 2, quantile, 0.975 )
age_data$mean_lw_s <- apply(means_s, 2, quantile, 0.025 )
age_data$mean <-  apply(means, 2, mean)
age_data$mean_up <- apply(means, 2, quantile, 0.975 )
age_data$mean_lw <- apply(means, 2, quantile, 0.025 )
age_data$mean_dy <-  apply(means_dy, 2, mean)
age_data$mean_up_dy <- apply(means_dy, 2, quantile, 0.975 )
age_data$mean_lw_dy <- apply(means_dy, 2, quantile, 0.025 )
age_data$age_s = age_data$age-.25
age_data$age_d = age_data$age+.25

pdf(file="plots/appendix/a26.pdf",height=6,width=7)
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
  lines(c(age_data$age[ii],age_data$age[ii]),c(age_data$mean_up_dy[ii], age_data$mean_lw_dy[ii]),col="#e31a1c", lwd=1.75)
  points(age_data$age[ii],age_data$mean_dy[ii],bg=c("#fed976"), col="#e31a1c",  cex=.65, pch=21, lwd=1.5)
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
text(0,2,"Static",pos=4,col="gray60",cex=1.5)
text(0,1.75,"Dynamic",pos=4,col="#e31a1c", cex=1.5)
text(0,1.5,"Dynamic Drift",pos=4,col="#313695", cex=1.5)
box()
dev.off()


