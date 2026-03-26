#Error simulations
##Note: This takes a while to run
rm(list=ls())

library(tidyverse)
library(plotrix)
library(lfe)
library(stargazer)
library(psych)
library(ClusterBootstrap)
library(DescTools)
library(FactoMineR)
library(factoextra)
library(reticulate)
library(ggplot2)
library(ggpubr)
library(cocor)
library(car)
library(simpleboot)
library(boot)

#Set working directory
setwd("YourDirectory\\replication_package_final")
set.seed(1226251)
#truth vs. noise
df <- data.frame(exp_hsd=numeric(),
                 exp_1sd=numeric(), 
                 exp_2sd=numeric(), 
                 exp_3sd=numeric()) 

##Use mean and sd for log consumption, but honestly shouldn't matter

pb = txtProgressBar(min = 0, max = 5000, initial = 0)

for (i in c(1:5000)) {
  setTxtProgressBar(pb,i)
  exp_samp<-rnorm(300, mean = 11.223, sd = 0.777)  
  sampdf<-data.frame(exp_samp)
  sampdf$exp_samp1sd<-sampdf$exp_samp+runif(300, min=-0.777, max=0.777)
  sampdf$exp_samp2sd<-sampdf$exp_samp+runif(300, min=-1.554, max=1.554)
  sampdf$exp_samp3sd<-sampdf$exp_samp+runif(300, min=-2.331, max=2.331)
  sampdf$exp_samphsd<-sampdf$exp_samp+runif(300, min=-0.3885, max=0.3885)
  
  v<-data.frame(rep(1:300, each=10))
  colnames(v)[1]<-"hhid"
  
  a<-sample(c(1:30), 300, replace = TRUE)
  b<-sample(c(31:60), 300, replace = TRUE)
  c<-sample(c(61:90), 300, replace = TRUE)
  d<-sample(c(91:120), 300, replace = TRUE)
  e<-sample(c(121:150), 300, replace = TRUE)
  f<-sample(c(151:180), 300, replace = TRUE)
  g<-sample(c(181:210), 300, replace = TRUE)
  h<-sample(c(211:240), 300, replace = TRUE)
  i<-sample(c(241:270), 300, replace = TRUE)
  j<-sample(c(271:300), 300, replace = TRUE)
  
  temp<-data.frame(rbind(sampdf[c(a),], sampdf[c(b),], sampdf[c(c),], sampdf[c(d),], sampdf[c(e),], sampdf[c(f),], sampdf[c(g),], sampdf[c(h),], sampdf[c(i),], sampdf[c(j),])) 
  vec<-data.frame(cbind(v, temp))
  vec$exp_rank_t<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_t<-rank(vec[vec$hhid==i,]$exp_samp)
  }
  
  
  vec$exp_rank_hsd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_hsd<-rank(vec[vec$hhid==i,]$exp_samphsd)
  }
  vec$exp_rank_1sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_1sd<-rank(vec[vec$hhid==i,]$exp_samp1sd)
  }
  vec$exp_rank_2sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_2sd<-rank(vec[vec$hhid==i,]$exp_samp2sd)
  }
  
  vec$exp_rank_3sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_3sd<-rank(vec[vec$hhid==i,]$exp_samp3sd)
  }
  
  
  #####get df
  exp1sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$exp_rank_t, vec[vec$hhid==i,]$exp_rank_1sd, method = "spearman", use="pairwise.complete.obs"))
    exp1sd<-rbind(exp1sd,ck)
  }
  colnames(exp1sd)<-c("hhid", "corr_expa")
  
  exp2sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$exp_rank_t, vec[vec$hhid==i,]$exp_rank_2sd, method = "spearman", use="pairwise.complete.obs"))
    exp2sd<-rbind(exp2sd,ck)
  }
  colnames(exp2sd)<-c("hhid", "corr_expa")
  
  exp3sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$exp_rank_t, vec[vec$hhid==i,]$exp_rank_3sd, method = "spearman", use="pairwise.complete.obs"))
    exp3sd<-rbind(exp3sd,ck)
  }
  colnames(exp3sd)<-c("hhid", "corr_expa")
  
  
  exphsd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$exp_rank_t, vec[vec$hhid==i,]$exp_rank_hsd, method = "spearman", use="pairwise.complete.obs"))
    exphsd<-rbind(exphsd,ck)
  }
  colnames(exphsd)<-c("hhid", "corr_expa")
  
  vector<-c(mean(exphsd$corr_expa), mean(exp1sd$corr_expa), mean(exp2sd$corr_expa),  mean(exp3sd$corr_expa))
  df<-rbind(df, vector)
  
  
}
close (pb)
colnames(df)<-c("exphsd", "exp1sd", "exp2sd", "exp3sd")


#######noise vs. noise

df4 <- data.frame(ph_h=numeric(),ph_1=numeric(), ph_2=numeric(), ph_3=numeric(), p1_h=numeric(),p1_1=numeric(), p1_2=numeric(), p1_3=numeric(), p2_h=numeric(),p2_1=numeric(), p2_2=numeric(), p2_3=numeric(), p3_h=numeric(),p3_1=numeric(), p3_2=numeric(), p3_3=numeric()) 
pb = txtProgressBar(min = 0, max = 5000, initial = 0)
for (i in c(1:5000)) {
  setTxtProgressBar(pb,i)
  exp_samp<-rnorm(300, mean = 0, sd = 2.78)  
  sampdf4<-data.frame(exp_samp)
  sampdf4$exp_samp1sd<-sampdf4$exp_samp+runif(300, min=-2.78, max=2.78)
  sampdf4$pexp_samp1sd<-sampdf4$exp_samp+runif(300, min=-2.78, max=2.78)
  sampdf4$exp_samp2sd<-sampdf4$exp_samp+runif(300, min=-5.56, max=5.56)
  sampdf4$pexp_samp2sd<-sampdf4$exp_samp+runif(300, min=-5.56, max=5.56)
  sampdf4$exp_samp3sd<-sampdf4$exp_samp+runif(300, min=-8.34, max=8.34)
  sampdf4$pexp_samp3sd<-sampdf4$exp_samp+runif(300, min=-8.34, max=8.34)
  sampdf4$exp_samphsd<-sampdf4$exp_samp+runif(300, min=-1.39, max=1.39)
  sampdf4$pexp_samphsd<-sampdf4$exp_samp+runif(300, min=-1.39, max=1.39)
  
  v<-data.frame(rep(1:300, each=10))
  colnames(v)[1]<-"hhid"
  
  a<-sample(c(1:30), 300, replace = TRUE)
  b<-sample(c(31:60), 300, replace = TRUE)
  c<-sample(c(61:90), 300, replace = TRUE)
  d<-sample(c(91:120), 300, replace = TRUE)
  e<-sample(c(121:150), 300, replace = TRUE)
  f<-sample(c(151:180), 300, replace = TRUE)
  g<-sample(c(181:210), 300, replace = TRUE)
  h<-sample(c(211:240), 300, replace = TRUE)
  i<-sample(c(241:270), 300, replace = TRUE)
  j<-sample(c(271:300), 300, replace = TRUE)
  
  temp<-data.frame(rbind(sampdf4[c(a),], sampdf4[c(b),], sampdf4[c(c),], sampdf4[c(d),], sampdf4[c(e),], sampdf4[c(f),], sampdf4[c(g),], sampdf4[c(h),], sampdf4[c(i),], sampdf4[c(j),])) 
  vec<-data.frame(cbind(v, temp))
  vec$exp_rank_t<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_t<-rank(vec[vec$hhid==i,]$exp_samp)
  }
  
  
  vec$exp_rank_hsd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_hsd<-rank(vec[vec$hhid==i,]$exp_samphsd)
  }
  
  vec$pexp_rank_hsd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$pexp_rank_hsd<-rank(vec[vec$hhid==i,]$pexp_samphsd)
  }
  vec$exp_rank_1sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_1sd<-rank(vec[vec$hhid==i,]$exp_samp1sd)
  }
  vec$pexp_rank_1sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$pexp_rank_1sd<-rank(vec[vec$hhid==i,]$pexp_samp1sd)
  }
  vec$exp_rank_2sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_2sd<-rank(vec[vec$hhid==i,]$exp_samp2sd)
  }
  
  vec$pexp_rank_2sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$pexp_rank_2sd<-rank(vec[vec$hhid==i,]$pexp_samp2sd)
  }
  
  vec$exp_rank_3sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$exp_rank_3sd<-rank(vec[vec$hhid==i,]$exp_samp3sd)
  }
  
  vec$pexp_rank_3sd<-NA
  for (i in unique(vec$hhid)){
    vec[vec$hhid==i,]$pexp_rank_3sd<-rank(vec[vec$hhid==i,]$pexp_samp3sd)
  }
  
  
  #####participant 0.5
  phexphsd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_hsd, vec[vec$hhid==i,]$exp_rank_hsd, method = "spearman", use="pairwise.complete.obs"))
    phexphsd<-rbind(phexphsd,ck)
  }
  colnames(phexphsd)<-c("hhid", "corr_expa")
  
  phexp1sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_hsd, vec[vec$hhid==i,]$exp_rank_1sd, method = "spearman", use="pairwise.complete.obs"))
    phexp1sd<-rbind(phexp1sd,ck)
  }
  colnames(phexp1sd)<-c("hhid", "corr_expa")
  
  phexp2sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_hsd, vec[vec$hhid==i,]$exp_rank_2sd, method = "spearman", use="pairwise.complete.obs"))
    phexp2sd<-rbind(phexp2sd,ck)
  }
  colnames(phexp2sd)<-c("hhid", "corr_expa")
  
  phexp3sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_hsd, vec[vec$hhid==i,]$exp_rank_3sd, method = "spearman", use="pairwise.complete.obs"))
    phexp3sd<-rbind(phexp3sd,ck)
  }
  colnames(phexp3sd)<-c("hhid", "corr_expa")
  
  #####participant 1
  p1exphsd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_1sd, vec[vec$hhid==i,]$exp_rank_hsd, method = "spearman", use="pairwise.complete.obs"))
    p1exphsd<-rbind(p1exphsd,ck)
  }
  colnames(p1exphsd)<-c("hhid", "corr_expa")
  
  p1exp1sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_1sd, vec[vec$hhid==i,]$exp_rank_1sd, method = "spearman", use="pairwise.complete.obs"))
    p1exp1sd<-rbind(p1exp1sd,ck)
  }
  colnames(p1exp1sd)<-c("hhid", "corr_expa")
  
  p1exp2sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_1sd, vec[vec$hhid==i,]$exp_rank_2sd, method = "spearman", use="pairwise.complete.obs"))
    p1exp2sd<-rbind(p1exp2sd,ck)
  }
  colnames(p1exp2sd)<-c("hhid", "corr_expa")
  
  p1exp3sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_1sd, vec[vec$hhid==i,]$exp_rank_3sd, method = "spearman", use="pairwise.complete.obs"))
    p1exp3sd<-rbind(p1exp3sd,ck)
  }
  colnames(p1exp3sd)<-c("hhid", "corr_expa")
  
  #####participant 2
  p2exphsd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_2sd, vec[vec$hhid==i,]$exp_rank_hsd, method = "spearman", use="pairwise.complete.obs"))
    p2exphsd<-rbind(p2exphsd,ck)
  }
  colnames(p2exphsd)<-c("hhid", "corr_expa")
  
  p2exp1sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_2sd, vec[vec$hhid==i,]$exp_rank_1sd, method = "spearman", use="pairwise.complete.obs"))
    p2exp1sd<-rbind(p2exp1sd,ck)
  }
  colnames(p2exp1sd)<-c("hhid", "corr_expa")
  
  p2exp2sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_2sd, vec[vec$hhid==i,]$exp_rank_2sd, method = "spearman", use="pairwise.complete.obs"))
    p2exp2sd<-rbind(p2exp2sd,ck)
  }
  colnames(p2exp2sd)<-c("hhid", "corr_expa")
  
  p2exp3sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_2sd, vec[vec$hhid==i,]$exp_rank_3sd, method = "spearman", use="pairwise.complete.obs"))
    p2exp3sd<-rbind(p2exp3sd,ck)
  }
  colnames(p2exp3sd)<-c("hhid", "corr_expa")
  
  #####participant 3
  p3exphsd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_3sd, vec[vec$hhid==i,]$exp_rank_hsd, method = "spearman", use="pairwise.complete.obs"))
    p3exphsd<-rbind(p3exphsd,ck)
  }
  colnames(p3exphsd)<-c("hhid", "corr_expa")
  
  p3exp1sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_3sd, vec[vec$hhid==i,]$exp_rank_1sd, method = "spearman", use="pairwise.complete.obs"))
    p3exp1sd<-rbind(p3exp1sd,ck)
  }
  colnames(p3exp1sd)<-c("hhid", "corr_expa")
  
  p3exp2sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_3sd, vec[vec$hhid==i,]$exp_rank_2sd, method = "spearman", use="pairwise.complete.obs"))
    p3exp2sd<-rbind(p3exp2sd,ck)
  }
  colnames(p3exp2sd)<-c("hhid", "corr_expa")
  
  p3exp3sd<-data.frame(hhid=numeric(), corr_expa=numeric())
  for (i in unique(vec$hhid)) {
    ck<-c(i,cor(vec[vec$hhid==i,]$pexp_rank_3sd, vec[vec$hhid==i,]$exp_rank_3sd, method = "spearman", use="pairwise.complete.obs"))
    p3exp3sd<-rbind(p3exp3sd,ck)
  }
  
colnames(p3exp3sd)<-c("hhid", "corr_expa")

vector<-c(mean(phexphsd$corr_expa), mean(phexp1sd$corr_expa), mean(phexp2sd$corr_expa),  mean(phexp3sd$corr_expa), mean(p1exphsd$corr_expa), mean(p1exp1sd$corr_expa), mean(p1exp2sd$corr_expa),  mean(p1exp3sd$corr_expa), mean(p2exphsd$corr_expa), mean(p2exp1sd$corr_expa), mean(p2exp2sd$corr_expa),  mean(p2exp3sd$corr_expa), mean(p3exphsd$corr_expa), mean(p3exp1sd$corr_expa), mean(p3exp2sd$corr_expa),  mean(p3exp3sd$corr_expa))
df4<-rbind(df4, vector)



}

close(pb)
colnames(df4)<-c("ph_h", "ph_1", "ph_2", "ph_3", "p1_h", "p1_1", "p1_2", "p1_3", "p2_h", "p2_1", "p2_2", "p2_3", "p3_h", "p3_1", "p3_2", "p3_3")

####Combine both parts
sim<-cbind(df, df4)


#######Create Simulation Table

r1<-c("truth", round(mean(sim$exphsd), digits=3), round(mean(sim$exp1sd), digits=3), round(mean(sim$exp2sd), digits=3), round(mean(sim$exp3sd), digits=3))
r2<-c("",paste0("[", round(quantile(sim$exphsd, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$exphsd, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$exp1sd, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$exp1sd, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$exp2sd, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$exp2sd, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$exp3sd, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$exp3sd, probs = c(0.975)), digits=3), "]"))
r3<-c("0.5SD",round(mean(sim$ph_h), digits=3), round(mean(sim$ph_1), digits=3), round(mean(sim$ph_2), digits=3), round(mean(sim$ph_3), digits=3))
r4<-c("",paste0("[", round(quantile(sim$ph_h, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$ph_h, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$ph_1, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$ph_1, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$ph_2, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$ph_2, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$ph_3, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$ph_3, probs = c(0.975)), digits=3), "]"))
r5<-c("1SD",round(mean(sim$p1_h), digits=3), round(mean(sim$p1_1), digits=3), round(mean(sim$p1_2), digits=3), round(mean(sim$p1_3), digits=3))
r6<-c("",paste0("[", round(quantile(sim$p1_h, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p1_h, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p1_1, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p1_1, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p1_2, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p1_2, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p1_3, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p1_3, probs = c(0.975)), digits=3), "]"))
r7<-c("2SD",round(mean(sim$p2_h), digits=3), round(mean(sim$p2_1), digits=3), round(mean(sim$p2_2), digits=3), round(mean(sim$p2_3), digits=3))
r8<-c("",paste0("[", round(quantile(sim$p2_h, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p2_h, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p2_1, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p2_1, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p2_2, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p2_2, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p2_3, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p2_3, probs = c(0.975)), digits=3), "]"))
r9<-c("3SD",round(mean(sim$p3_h), digits=3), round(mean(sim$p3_1), digits=3), round(mean(sim$p3_2), digits=3), round(mean(sim$p3_3), digits=3))
r10<-c("",paste0("[", round(quantile(sim$p3_h, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p3_h, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p3_1, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p3_1, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p3_2, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p3_2, probs = c(0.975)), digits=3), "]"), paste0("[", round(quantile(sim$p3_3, probs = c(0.025)), digits = 3), ", ", round(quantile(sim$p3_3, probs = c(0.975)), digits=3), "]"))

rows<-data.frame(rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10))
colnames(rows)<-c("Community Noise","0.5SD", "1SD", "2SD", "3SD")
rownames(rows)<-NULL
stargazer(rows, summary=F)

write.csv(sim,"Processed Data\\error_sim_results.csv") 
