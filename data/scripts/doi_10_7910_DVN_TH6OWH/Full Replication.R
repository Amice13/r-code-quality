###Justin Kirkland
###Beth Simas
###Scott Clifford
##########################

#Clear Memory
rm(list=ls())
set.seed(1111)

#Load libraries
library(foreign)
library(arm)
library(MASS)
library(psych)
library(GPArotation)
library(stargazer)

#Load data
#R has done something stupid, deprecating all duplicated factors. Now I have to reprogram all the work I did before.
data<-read.dta("/Users/jhk9y/Dropbox/Party Fit/Data/UHOU0001_OUTPUT.dta", convert.factors=FALSE)
names(data)

#begin recoding data and cleaning data
names(data)[which(names(data)=="Q17_29")]<-"check1"
data$check1<-ifelse(data$check1==1, 1, 0)

#Drop satisficers?
#data1<-data
data1<-data[which(data$check1==1),]

#Recode IRI

#Code Empathy Scales

#Code Demographics
data1$nopid<-ifelse(data1$pid7==8, 1, 0)
data1$pid7_2<-ifelse(data1$pid7==8, 4, as.numeric(data1$pid7))
data1$pidext<-abs(data1$pid7_2-4)
data1$male<-ifelse(data1$gender==1, 1, 0)
data1$black<-ifelse(data1$race==2, 1, 0)
data1$hisp<-ifelse(data1$race==3, 1, 0)
data1$otherrace<-ifelse(data1$race!=1 & data1$race!=2 & data1$race!=3, 1, 0)
data1$educ_num<-as.numeric(data1$educ)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)<4, 0, NA)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=4 & as.numeric(data1$faminc)<6, 1, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=6 & as.numeric(data1$faminc)<10, 2, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=10 & as.numeric(data1$faminc)<18, 3, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)==18, 4, data1$inc4miss)
data1$ideo5_num<-as.numeric(data1$ideo5)
data1$ideo5_num<-ifelse(data1$ideo5_num==6, NA, data1$ideo5_num)
data1$age<-2016-data1$birthyr
data1$married<-ifelse(data1$marstat!=1, 0, 1)
data1$full_employ<-ifelse(data1$employ==1, 1, 0)
data1$newsint_rev<-abs(as.numeric(data1$newsint)-5)

#Code Ambition
data1$run<-ifelse(data1$Q1==1, 1, 0)
data1$held<-ifelse(data1$Q3==1, 1, 0)
data1$considered<-ifelse(data1$Q5==1 | data1$run==1 | data1$held==1, 1, 0)
data1$considered_sum<-ifelse(data1$considered==1, 1, 0)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_1)==TRUE | data1$Q6_1==2, 0, 1)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_2)==TRUE | data1$Q6_2==2, 0, 1)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_3)==TRUE | data1$Q6_3==2, 0, 1)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_4)==TRUE | data1$Q6_4==2, 0, 1)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_5)==TRUE | data1$Q6_5==2, 0, 1)
data1$considered_sum<-data1$considered_sum+ifelse(is.na(data1$Q6_6)==TRUE | data1$Q6_6==2, 0, 1)
data1$person<-abs(as.numeric(data1$Q10_1)-7)+1
data1$qualified<-abs(as.numeric(data1$Q10_2)-7)+1
data1$ambschool<-as.numeric(data1$Q8_1)
data1$ambcity<-as.numeric(data1$Q8_2)
data1$ambmayor<-as.numeric(data1$Q8_3)
data1$ambstate<-as.numeric(data1$Q8_4)
data1$ambgov<-as.numeric(data1$Q8_5)
data1$ambhouse<-as.numeric(data1$Q8_6)
data1$ambsen<-as.numeric(data1$Q8_7)
data1$ambsum<-data1$ambschool+data1$ambcity+data1$ambmayor+data1$ambstate+data1$ambgov+data1$ambhouse+data1$ambsen
data1$ambsum2<-data1$ambcity+data1$ambmayor+data1$ambstate+data1$ambgov+data1$ambhouse+data1$ambsen

data1$ambsum<-data1$ambsum-7

#Suggestions to Run
data1$suggparty<-ifelse(data1$Q7_1==1, 1, 0)
data1$suggofficial<-ifelse(data1$Q7_2==1, 1, 0)
data1$suggfriend<-ifelse(data1$Q7_3==1, 1, 0)
data1$suggfamily<-ifelse(data1$Q7_4==1, 1, 0)
data1$suggactivist<-ifelse(data1$Q7_5==1, 1, 0)
data1$suggother<-ifelse(data1$Q7_6==1, 1, 0)
data1$suggsum<-data1$suggparty+data1$suggofficial+data1$suggfriend+data1$suggfamily+data1$suggactivist+data1$suggother

data1$suggsum1<-data1$suggparty+data1$suggofficial+data1$suggactivist
data1$suggsum2<-data1$suggfriend+data1$suggfamily+data1$suggother

#Code ideology
data1$ideo7<-ifelse(as.numeric(data1$Q21)<8, as.numeric(data1$Q21), NA)
data1$ideoext<-abs(4-data1$ideo7)

#Code Party Fit
data1$Perception_Dems<-as.numeric(data1$Q25_B)
data1$Perception_Reps<-as.numeric(data1$Q25_A)

data1$dist_dems<-abs(as.numeric(data1$Perception_Dems)-as.numeric(data1$ideo7))
data1$dist_reps<-abs(as.numeric(data1$Perception_Reps)-as.numeric(data1$ideo7))
data1$small_dist<-ifelse(data1$dist_dems<data1$dist_reps, data1$dist_dems, data1$dist_reps) 


data1$party_dist<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7[i])<=3){data1$party_dist[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7[i])>=5 & as.numeric(data1$pid7[i])<8){data1$party_dist[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7[i])==4){data1$party_dist[i]<-data1$small_dist[i]}
}

data1$party_dist_out<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7[i])>=5 & as.numeric(data1$pid7[i])<8){data1$party_dist_out[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7[i])<=3){data1$party_dist_out[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7[i])==4){data1$party_dist_out[i]<-data1$small_dist[i]}
}

data1$ext_indicator<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7[i])<=3){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])<as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])>as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
  
  if(as.numeric(data1$pid7[i])>=5 & as.numeric(data1$pid7[i])<8){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])>as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])<as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
}


#Table 1; Column 1
#Just the ambition index
summary(mod11<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1[which(data1$oversample==2), ], family=poisson))

########################
#Figure 2###############
############################################
#Create some graphics#
#############################################


library(mvtnorm)
library(arm)
sims<-1000
dist<-rmvnorm(sims, coef(mod11), as.matrix(vcov(mod11)))
calc<-300
z<-seq(min(data1$party_dist, na.rm=T), max(data1$party_dist, na.rm=T), length=calc)
z2<-seq(min(data1$ideoext, na.rm=T), max(data1$ideoext, na.rm=T), length=calc)
lo1<-numeric(calc)
hi1<-numeric(calc)
pe1<-numeric(calc)
lo2<-numeric(calc)
hi2<-numeric(calc)
pe2<-numeric(calc)
for(i in 1:calc){
  pp<-numeric(sims)
  pp2<-numeric(sims)
  
  for(j in 1:sims){
    #Ambition; distance
    pp[j]<-dist[j,1]+dist[j,2]*mean(data1$ideoext, na.rm=T)+dist[j,3]*mean(data1$educ_num, na.rm=T)+dist[j,4]*0+dist[j,5]*0+dist[j,6]*0+dist[j,7]*0+dist[j,8]*1+dist[j,9]*0+dist[j,10]*0+dist[j,11]*0+dist[j,12]*mean(data1$age, na.rm=T)+dist[j,13]*0+dist[j,14]*1+dist[j,15]*z[i]+dist[j,16]*mean(data1$party_dist_out, na.rm=T)
    #Ambition; extremity
    pp2[j]<-dist[j,1]+dist[j,2]*z2[i]+dist[j,3]*mean(data1$educ_num, na.rm=T)+dist[j,4]*0+dist[j,5]*0+dist[j,6]*0+dist[j,7]*0+dist[j,8]*1+dist[j,9]*0+dist[j,10]*0+dist[j,11]*0+dist[j,12]*mean(data1$age, na.rm=T)+dist[j,13]*0+dist[j,14]*1+dist[j,15]*mean(data1$party_dist, na.rm=T)+dist[j,16]*mean(data1$party_dist_out, na.rm=T)
  }
  
  pe1[i]<-exp(mean(pp))
  lo1[i]<-exp(quantile(pp, 0.025))
  hi1[i]<-exp(quantile(pp, 0.975))
  
  pe2[i]<-exp(mean(pp2))
  lo2[i]<-exp(quantile(pp2, 0.025))
  hi2[i]<-exp(quantile(pp2, 0.975))
  
  print(i)
}

par(mar=c(4,4,2,2))
#pdf("DistAmb.pdf")
plot(z, pe1, type="l", lwd=3, col="black", xlab="Perceived Party Distance", ylab="Sum of Office Ambitions", ylim=c(0, 10), xaxt="n")
grid()
rug(jitter(data1$party_dist))
lines(z, lo1, col="black", lwd=3, lty=2)
lines(z, hi1, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3, 4, 5, 6))
axis(1, at=c(0, 6), labels=c("Close to Party", "Far From Party"), line=1.25, cex.axis=0.75, tick=F)
legend("topleft", inset=0.05, c("Point Estimate", "95% Confidence Interval"), lty=c(1, 2), cex=0.85, lwd=c(3,3))
#dev.off()

par(mar=c(4,4,2,2))
#pdf("ExtAmb.pdf")
plot(z2, pe2, type="l", lwd=3, col="black", xlab="Ideological Extremity", ylab="Sum of Office Ambitions", ylim=c(0, 10), xaxt="n")
grid()
rug(jitter(data1$ideoext))
lines(z2, lo2, col="black", lwd=3, lty=2)
lines(z2, hi2, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3))
axis(1, at=c(0, 3), labels=c("Moderate", "Extreme"), line=1.25, cex.axis=0.75, tick=F)
#dev.off()


(pe1[300]-pe1[1])/sd(data1$ambsum, na.rm=T)
(pe2[300]-pe2[1])/sd(data1$ambsum, na.rm=T)

############################
####Table 2 columns 1 and 2
###########################################
#Vary by party?############################
#Democrats
summary(mod11.d<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1[which(data1$oversample==2 & as.numeric(data1$pid7)<=3), ], family=poisson))
#Republicans
summary(mod11.r<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1[which(data1$oversample==2 & as.numeric(data1$pid7)>=5 & as.numeric(data1$pid7)<8), ], family=poisson))


###############################
#Figure 3#######################

############################################
#Create some graphics#
#############################################


library(mvtnorm)
library(arm)
sims<-1000
dist<-rmvnorm(sims, coef(mod11.d), as.matrix(vcov(mod11.d)))
dist2<-rmvnorm(sims, coef(mod11.r), as.matrix(vcov(mod11.r)))
calc<-300
z<-seq(min(data1$party_dist, na.rm=T), max(data1$party_dist, na.rm=T), length=calc)
lo1<-numeric(calc)
hi1<-numeric(calc)
pe1<-numeric(calc)
lo2<-numeric(calc)
hi2<-numeric(calc)
pe2<-numeric(calc)
for(i in 1:calc){
  pp<-numeric(sims)
  pp2<-numeric(sims)
  
  for(j in 1:sims){
    #Ambition; distance
    pp[j]<-dist[j,1]+dist[j,2]*mean(data1$ideoext, na.rm=T)+dist[j,3]*mean(data1$educ_num, na.rm=T)+dist[j,4]*0+dist[j,5]*0+dist[j,6]*0+dist[j,7]*0+dist[j,8]*1+dist[j,9]*0+dist[j,10]*0+dist[j,11]*0+dist[j,12]*mean(data1$age, na.rm=T)+dist[j,13]*0+dist[j,14]*1+dist[j,15]*z[i]+dist[j,16]*mean(data1$party_dist_out, na.rm=T)
    #Ambition; extremity
    pp2[j]<-dist2[j,1]+dist2[j,2]*mean(data1$ideoext, na.rm=T)+dist2[j,3]*mean(data1$educ_num, na.rm=T)+dist2[j,4]*0+dist2[j,5]*0+dist2[j,6]*0+dist2[j,7]*0+dist2[j,8]*1+dist2[j,9]*0+dist2[j,10]*0+dist2[j,11]*0+dist2[j,12]*mean(data1$age, na.rm=T)+dist2[j,13]*0+dist2[j,14]*1+dist2[j,15]*z[i]+dist2[j,16]*mean(data1$party_dist_out, na.rm=T)
  }
  
  pe1[i]<-exp(mean(pp))
  lo1[i]<-exp(quantile(pp, 0.025))
  hi1[i]<-exp(quantile(pp, 0.975))
  
  pe2[i]<-exp(mean(pp2))
  lo2[i]<-exp(quantile(pp2, 0.025))
  hi2[i]<-exp(quantile(pp2, 0.975))
  
  print(i)
}

par(mar=c(4,4,2,2))
#pdf("DistAmbD.pdf")
plot(z, pe1, type="l", lwd=3, col="black", xlab="Perceived Party Distance", ylab="Sum of Office Ambitions", ylim=c(0, 15), xaxt="n")
grid()
rug(jitter(data1$party_dist))
lines(z, lo1, col="black", lwd=3, lty=2)
lines(z, hi1, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3, 4, 5, 6))
axis(1, at=c(0, 6), labels=c("Close to Party", "Far From Party"), line=1.25, cex.axis=0.75, tick=F)
legend("topleft", inset=0.05, c("Point Estimate", "95% Confidence Interval"), lty=c(1, 2), cex=0.85, lwd=c(3,3))
#dev.off()

par(mar=c(4,4,2,2))
#pdf("DistAmbR.pdf")
plot(z2, pe2, type="l", lwd=3, col="black", xlab="Perceived Party Distance", ylab="Sum of Office Ambitions", ylim=c(0, 15), xaxt="n")
grid()
rug(jitter(data1$ideoext))
lines(z2, lo2, col="black", lwd=3, lty=2)
lines(z2, hi2, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3, 4, 5, 6))
axis(1, at=c(0, 6), labels=c("Close to Party", "Far From Party"), line=1.25, cex.axis=0.75, tick=F)
#dev.off()


(pe1[300]-pe1[1])/sd(data1$ambsum, na.rm=T)
(pe2[300]-pe2[1])/sd(data1$ambsum, na.rm=T)


##############################################
#2018 Data####################################

#Clear Memory
rm(list=ls())
set.seed(1111)

#Load libraries
library(foreign)
library(arm)
library(MASS)
library(psych)
library(GPArotation)
library(stargazer)
library(readstata13)

#Load data
#R has done something stupid, deprecating all duplicated factors. Now I have to reprogram all the work I did before.
data1<-read.dta("/Users/jhk9y/Dropbox/Party Fit/Data/CCES18_UVH_OUTPUT.dta", convert.factors=FALSE)
names(data1)

#Code Demographics
data1$nopid<-ifelse(data1$pid7>=8, 1, 0)
data1<-data1[-which(data1$nopid==1),]
data1$pid7_2<-ifelse(data1$pid7==8, 4, as.numeric(data1$pid7))
data1$pidext<-abs(data1$pid7_2-4)
data1$male<-ifelse(data1$gender==1, 1, 0)
data1$black<-ifelse(data1$race==2, 1, 0)
data1$hisp<-ifelse(data1$race==3, 1, 0)
data1$otherrace<-ifelse(data1$race!=1 & data1$race!=2 & data1$race!=3, 1, 0)
data1$educ_num<-as.numeric(data1$educ)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)<4, 0, NA)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=4 & as.numeric(data1$faminc)<6, 1, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=6 & as.numeric(data1$faminc)<10, 2, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)>=10 & as.numeric(data1$faminc)<18, 3, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc)==18, 4, data1$inc4miss)
data1$ideo5_num<-as.numeric(data1$ideo5)
data1$ideo5_num<-ifelse(data1$ideo5_num==6, NA, data1$ideo5_num)
data1$age<-2018-data1$birthyr
data1$married<-ifelse(data1$marstat!=1, 0, 1)
data1$full_employ<-ifelse(data1$employ==1, 1, 0)
data1$newsint_rev<-abs(as.numeric(data1$newsint)-5)

#Code Ambition
data1$run<-ifelse(data1$UVH300==1, 1, 0)
data1$held<-ifelse(data1$UVH302==1, 1, 0)
data1$considered<-ifelse(data1$UVH304==1 | data1$run==1 | data1$held==1, 1, 0)

#Steps to run
data1$steps.ballot<-ifelse(data1$UVH305_1==1, 1, 0)
data1$steps.donor<-ifelse(data1$UVH305_2==1, 1, 0)
data1$steps.leader<-ifelse(data1$UVH305_3==1, 1, 0)
data1$steps.comm<-ifelse(data1$UVH305_4==1, 1, 0)
data1$steps.friends<-ifelse(data1$UVH305_5==1,1, 0)
data1$steps.sum<-data1$steps.ballot+data1$steps.donor+data1$steps.leader+data1$steps.comm+data1$steps.friends+ifelse(data1$considered==1, 1, 0)


data1$person<-ifelse(data1$UVH316<8, abs(as.numeric(data1$UVH316)-7)+1, NA)
data1$qualified<-ifelse(data1$UVH317<8, abs(as.numeric(data1$UVH317)-7)+1, NA)


data1$ambschool<-as.numeric(data1$UVH307)
data1$ambcity<-ifelse(data1$UVH308<8, as.numeric(data1$UVH308), 0)
data1$ambmayor<-as.numeric(data1$UVH309)
data1$ambstate<-as.numeric(data1$UVH310)
data1$ambgov<-as.numeric(data1$UVH311)
data1$ambhouse<-as.numeric(data1$UVH312)
data1$ambsen<-ifelse(data1$UVH313<8, as.numeric(data1$UVH313), 0)
data1$ambsum<-data1$ambschool+data1$ambcity+data1$ambmayor+data1$ambstate+data1$ambgov+data1$ambhouse+data1$ambsen
data1$ambsum2<-data1$ambcity+data1$ambmayor+data1$ambstate+data1$ambgov+data1$ambhouse+data1$ambsen

data1$ambsum<-data1$ambsum-7

#Suggestions to Run
data1$suggparty<-ifelse(data1$UVH306_1==1, 1, 0)
data1$suggofficial<-ifelse(data1$UVH306_2==1, 1, 0)
data1$suggfriend<-ifelse(data1$UVH306_3==1, 1, 0)
data1$suggfamily<-ifelse(data1$UVH306_4==1, 1, 0)
data1$suggactivist<-ifelse(data1$UVH306_5==1, 1, 0)
data1$suggother<-ifelse(data1$UVH306_6==1, 1, 0)
data1$suggsum<-data1$suggparty+data1$suggofficial+data1$suggfriend+data1$suggfamily+data1$suggactivist+data1$suggother

data1$suggsum1<-data1$suggparty+data1$suggofficial+data1$suggactivist
data1$suggsum2<-data1$suggfriend+data1$suggfamily+data1$suggother

#Code ideology
data1$ideo7<-ifelse(as.numeric(data1$CC18_334A)<8, as.numeric(data1$CC18_334A), NA)
data1$ideoext<-abs(4-data1$ideo7)

#Code Party Fit
data1$Perception_Dems<-ifelse(as.numeric(data1$CC18_334D)<8, as.numeric(data1$CC18_334D), NA)
data1$Perception_Reps<-ifelse(as.numeric(data1$CC18_334E)<8, as.numeric(data1$CC18_334E), NA)

data1$dist_dems<-abs(as.numeric(data1$Perception_Dems)-as.numeric(data1$ideo7))
data1$dist_reps<-abs(as.numeric(data1$Perception_Reps)-as.numeric(data1$ideo7))
data1$small_dist<-ifelse(data1$dist_dems<data1$dist_reps, data1$dist_dems, data1$dist_reps) 

data1$party_dist<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])<=3){data1$party_dist[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){data1$party_dist[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7_2[i])==4){data1$party_dist[i]<-data1$small_dist[i]}
}

data1$party_dist_out<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){data1$party_dist_out[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7_2[i])<=3){data1$party_dist_out[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7_2[i])==4){data1$party_dist_out[i]<-data1$small_dist[i]}
}

data1$ext_indicator<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])<=3){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])<as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])>as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
  
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])>as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])<as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
}


########################
#Table 1; Column 2
#AmbitionSum
summary(mod10<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1, family=poisson))

##########################
#Figure 2
############################################
#Create some graphics#
#############################################


library(mvtnorm)
library(arm)
sims<-1000
dist<-rmvnorm(sims, coef(mod10), as.matrix(vcov(mod10)))
calc<-300
z<-seq(min(data1$party_dist, na.rm=T), max(data1$party_dist, na.rm=T), length=calc)
z2<-seq(min(data1$ideoext, na.rm=T), max(data1$ideoext, na.rm=T), length=calc)
lo1<-numeric(calc)
hi1<-numeric(calc)
pe1<-numeric(calc)
lo2<-numeric(calc)
hi2<-numeric(calc)
pe2<-numeric(calc)
for(i in 1:calc){
  pp<-numeric(sims)
  pp2<-numeric(sims)
  
  for(j in 1:sims){
    #Ambition; distance
    pp[j]<-dist[j,1]+dist[j,2]*mean(data1$ideoext, na.rm=T)+dist[j,3]*mean(data1$educ_num, na.rm=T)+dist[j,4]*0+dist[j,5]*0+dist[j,6]*0+dist[j,7]*0+dist[j,8]*1+dist[j,9]*0+dist[j,10]*0+dist[j,11]*0+dist[j,12]*mean(data1$age, na.rm=T)+dist[j,13]*0+dist[j,14]*1+dist[j,15]*z[i]+dist[j,16]*mean(data1$party_dist_out, na.rm=T)
    #Ambition; extremity
    pp2[j]<-dist[j,1]+dist[j,2]*z2[i]+dist[j,3]*mean(data1$educ_num, na.rm=T)+dist[j,4]*0+dist[j,5]*0+dist[j,6]*0+dist[j,7]*0+dist[j,8]*1+dist[j,9]*0+dist[j,10]*0+dist[j,11]*0+dist[j,12]*mean(data1$age, na.rm=T)+dist[j,13]*0+dist[j,14]*1+dist[j,15]*mean(data1$party_dist, na.rm=T)+dist[j,16]*mean(data1$party_dist_out, na.rm=T)
  }
  
  pe1[i]<-exp(mean(pp))
  lo1[i]<-exp(quantile(pp, 0.025))
  hi1[i]<-exp(quantile(pp, 0.975))
  
  pe2[i]<-exp(mean(pp2))
  lo2[i]<-exp(quantile(pp2, 0.025))
  hi2[i]<-exp(quantile(pp2, 0.975))
  
  print(i)
}

par(mar=c(4,4,2,2))
#pdf("DistAmb18.pdf")
plot(z, pe1, type="l", lwd=3, col="black", xlab="Perceived Party Distance", ylab="Sum of Office Ambitions", ylim=c(0, 10), xaxt="n")
grid()
rug(jitter(data1$party_dist))
lines(z, lo1, col="black", lwd=3, lty=2)
lines(z, hi1, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3, 4, 5, 6))
axis(1, at=c(0, 6), labels=c("Close to Party", "Far From Party"), line=1.25, cex.axis=0.75, tick=F)
legend("topleft", inset=0.05, c("Point Estimate", "95% Confidence Interval"), lty=c(1, 2), cex=0.85, lwd=c(3,3))
#dev.off()

par(mar=c(4,4,2,2))
#pdf("ExtAmb18.pdf")
plot(z2, pe2, type="l", lwd=3, col="black", xlab="Ideological Extremity", ylab="Sum of Office Ambitions", ylim=c(0, 10), xaxt="n")
grid()
rug(jitter(data1$ideoext))
lines(z2, lo2, col="black", lwd=3, lty=2)
lines(z2, hi2, col="black", lwd=3, lty=2)
axis(1, at=c(0, 1, 2, 3))
axis(1, at=c(0, 3), labels=c("Moderate", "Extreme"), line=1.25, cex.axis=0.75, tick=F)
#dev.off()


(pe1[300]-pe1[1])/sd(data1$ambsum, na.rm=T)
(pe2[300]-pe2[1])/sd(data1$ambsum, na.rm=T)


##################################
#Table 2; Columns 3 and 4
#Vary by party?############################
#Democrats
summary(mod11.d<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1[which(as.numeric(data1$pid7)<=3), ], family=poisson))
#Republicans
summary(mod11.r<-glm(ambsum~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1[which(as.numeric(data1$pid7)>=5 & as.numeric(data1$pid7)<8), ], family=poisson))


###############################
#Panel Data####################

#Clear Memory
rm(list=ls())
set.seed(1111)

#Load libraries
library(foreign)
library(arm)
library(MASS)
library(psych)
library(GPArotation)
library(stargazer)
library(readstata13)

#Load data
#R has done something stupid, deprecating all duplicated factors. Now I have to reprogram all the work I did before.
data1<-read.dta("/Users/jhk9y/Dropbox/Party Fit/CCES 2010-2014 Panel/CCES_Panel_Full3waves_VV_V3.dta", convert.factors=FALSE)
names(data1)

#Code Demographics
data1$nopid<-ifelse(data1$pid7_10>=8 | is.na(data1$pid7_10)==TRUE, 1, 0)
data1<-data1[-which(data1$nopid==1),]
data1$pid7_2<-ifelse(data1$pid7_10==8, 4, as.numeric(data1$pid7_10))
data1$pidext<-abs(data1$pid7_2-4)
data1$male<-ifelse(data1$gender_10==1, 1, 0)
data1$black<-ifelse(data1$race_10==2, 1, 0)
data1$hisp<-ifelse(data1$race_10==3, 1, 0)
data1$otherrace<-ifelse(data1$race_10!=1 & data1$race_10!=2 & data1$race_10!=3, 1, 0)
data1$educ_num<-as.numeric(data1$educ_10)
data1$inc4miss<-ifelse(as.numeric(data1$faminc_10)<4, 0, NA)
data1$inc4miss<-ifelse(as.numeric(data1$faminc_10)>=4 & as.numeric(data1$faminc_10)<6, 1, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc_10)>=6 & as.numeric(data1$faminc_10)<10, 2, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc_10)>=10 & as.numeric(data1$faminc_10)<18, 3, data1$inc4miss)
data1$inc4miss<-ifelse(as.numeric(data1$faminc_10)==18, 4, data1$inc4miss)
data1$ideo5_num<-as.numeric(data1$ideo5_10)
data1$ideo5_num<-ifelse(data1$ideo5_num==6, NA, data1$ideo5_num)
data1$age<-2010-data1$birthyr_10
data1$married<-ifelse(data1$marstat_10!=1, 0, 1)
data1$full_employ<-ifelse(data1$employ_10==1, 1, 0)
data1$newsint_rev<-abs(as.numeric(data1$newsint_10)-5)


#Code ideology
data1$ideo7<-ifelse(as.numeric(data1$CC10_341A)<8, as.numeric(data1$CC10_341A), NA)
data1$ideoext<-abs(4-data1$ideo7)

#Code Party Fit: 2010
data1$Perception_Dems<-ifelse(as.numeric(data1$CC10_334E)<8, as.numeric(data1$CC10_334E), NA)
data1$Perception_Reps<-ifelse(as.numeric(data1$CC10_341F)<8, as.numeric(data1$CC10_341F), NA)

data1$dist_dems<-abs(as.numeric(data1$Perception_Dems)-as.numeric(data1$ideo7))
data1$dist_reps<-abs(as.numeric(data1$Perception_Reps)-as.numeric(data1$ideo7))
data1$small_dist<-ifelse(data1$dist_dems<data1$dist_reps, data1$dist_dems, data1$dist_reps) 

data1$party_dist<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])<=3){data1$party_dist[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){data1$party_dist[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7_2[i])==4){data1$party_dist[i]<-data1$small_dist[i]}
}

data1$party_dist_out<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){data1$party_dist_out[i]<-data1$dist_dems[i]}
  if(as.numeric(data1$pid7_2[i])<=3){data1$party_dist_out[i]<-data1$dist_reps[i]}
  if(as.numeric(data1$pid7_2[i])==4){data1$party_dist_out[i]<-data1$small_dist[i]}
}

data1$ext_indicator<-numeric(nrow(data1))
for(i in 1:nrow(data1)){
  if(as.numeric(data1$pid7_2[i])<=3){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])<as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Dems[i])>as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
  
  if(as.numeric(data1$pid7_2[i])>=5 & as.numeric(data1$pid7_2[i])<8){
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])>as.numeric(data1$ideo7[i]), 1, 0)
    data1$ext_indicator[i]<-ifelse(as.numeric(data1$Perception_Reps[i])<as.numeric(data1$ideo7[i]), -1, data1$ext_indicator[i]) 
  }
}

############################
#Code up run between 2010 and 2014
data1$run<-0
data1$run<-ifelse(data1$CC10_418a==2 & data1$CC14_418a==1, 1, data1$run)

#############################
#Code up other extreme outcomes
data1$ER<-0
data1$ER<-ifelse(data1$CC12_387_2==2 & data1$CC14_387_2==1, 1, data1$ER)

data1$victim<-0
data1$victim<-ifelse(data1$CC12_387_9==2 & data1$CC14_387_9==1, 1, data1$ER)


######################
#Table 3##############
###############################
#Models
summary(mod1<-glm(run~ideoext+educ_num+as.factor(inc4miss)+male+black+hisp+otherrace+age+married+full_employ+party_dist+party_dist_out, data=data1, family=binomial(link=logit)))

############################
#Some odds stuff###########
exp(coef(mod1)[14])
#CI for the ratio
library(oddsratio)
or_glm(data=data1, model=mod1, incr=list(party_dist=1, ideoext=0, educ_num=0, party_dist_out=0), ci=0.95)
