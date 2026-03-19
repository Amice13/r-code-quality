### Replication File for 
### "Gender and Political Compliance under Authoritarian Rule", Comparative Political Studies.
### Yingjie Fan, Jennifer Pan, Tongtong Zhang

### This file: codes that replicate all tables and figures in the MAIN PAPER.

######################################################################################
## Section 3.2 Survey Experiment
######################################################################################
rm(list=ls()) 
## Install and load packages
#install.packages("dplyr")
#install.packages("ggplot2")
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

## Please set the working directory to the folder where the ReadMe.txt is located.
setwd("")

###############################
####### Figure 1 #############
###############################
# Read in the data: each row is a unique CI teacher respondent who answered at least one question in the survey
d<-read.csv("Data/CI_RespondentDescriptives.csv",header=TRUE, stringsAsFactors = FALSE,sep=",")
output<-data.frame(matrix(NA, nrow=355, ncol=0))
output$age<-d$age
output$age<-ifelse(output$age<22,NA,output$age)
output$age_cut <- cut(output$age, breaks = c(-Inf, 22, 30, 40, 50, 60, Inf), 
                      labels = c("<22", "23~30", "31~40", "41~50", "51~60", ">60"), right = FALSE)
age<-output %>%
  dplyr::group_by(age_cut)%>%
  dplyr::summarise(n=n())
age_p<-ggplot(output, aes(age_cut))+
  geom_bar()+
  scale_color_grey()+
  labs(
    x = "Age",
    y = "Number of CI teachers")+
  theme(axis.title = element_text(size=10),axis.text = element_text(size=10.5))
ggsave("Log/Figure1.png",age_p,height=4,width=5)


######################################################################################
## Section 5 Gendered Expressions of Political Conformity
######################################################################################
rm(list=ls()) 
# Install and load packages
#install.packages("sandwich")
#install.packages("plm")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("lmtest")
#install.packages("multiwayvcov")
#install.packages("mfx")
suppressMessages(library("sandwich"))
suppressMessages(library(plm))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(lmtest))
suppressMessages(library(multiwayvcov))
suppressMessages(library(mfx))

d<-read.csv("Data/CI_MainExperiment.csv",header=TRUE, stringsAsFactors = FALSE,sep=",") #Read in the data

###############################
####### Figure 3 #############
###############################
d<-d[!is.na(d$SelfCensor),];dd<-d[d$VigOrder==1,] #Use only the first vignette answered by each respondent
res<-matrix(NA,nrow=4, ncol=5) # Save the overall effects to be plotted
colnames(res)<-c("outcome","estimate","s.e.","lower","upper")
res[,1]<-c("self-censor", "one-side", "two-side", "open")
## Self-censor
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal + female + Grad + ProfTeacher + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq +colleague_everyday + current + JoinMotivations_material, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
res[1,2:3]<-coeftest(ols.nc1, vcov = vcovWhite.nc1)[2,1:2]
## One-sided position taking (one-sided hereafter)
ols.nc1 <- lm(PosTaking1 ~ PRC + interpersonal + ccp + Grad + female + ProfTeacher + JoinMotivations_material + Chn_news_freq + Local_news_freq + NorthAmerica + Ocenia  + ChnPol_news_freq  + current + TeachYearsBefore0, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
res[2,2:3]<-coeftest(ols.nc1, vcov = vcovWhite.nc1)[2,1:2]
## Two-sided position introduction (two-sided hereafter)
ols.nc1 <- lm(PosTaking2 ~ PRC + interpersonal + ccp +  Grad + female +TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher +colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material+ TrainingOverMonth, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
res[3,2:3]<-coeftest(ols.nc1, vcov = vcovWhite.nc1)[2,1:2]
## Open discussion
ols.nc1 <- lm(open ~ PRC + interpersonal + ccp +  Grad + female + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1")  
res[4,2:3]<-coeftest(ols.nc1, vcov = vcovWhite.nc1)[2,1:2]

# Plotting Figure 3
res<-as.data.frame(res)
res[,2:5]<-lapply(res[,2:5],function(x) as.numeric(as.character(x)))
res$lower<-res$estimate + qnorm(0.025) * res$s.e. #lower bound of 95% CI
res$upper<-res$estimate + qnorm(0.975) * res$s.e. #upper bound of 95% CI
min(res$lower);max(res$upper)
pdf("Log/Figure3.pdf",width=10)
par(mar=c(8, 5, 1, 2) + 0.1)
x<-1:4 
plot(x,res[1:4,2],ylim=c(-0.3,0.35),col = 1,xlim=c(0.5,4.5),main = "",
     ylab = "Overall effect of the Objectives Prime", xlab="",
     pch=16, cex.lab=1.5, xaxt="n",cex=2, cex.axis=1.5)
axis(1,1:4,labels=c("Self-censor","One-sided","Two-sided","Open discussion"),cex.axis=1.5,cex.lab=1.5,mgp=c(3,2,0))
#axis(2,c(-0.15,-0.1,-0.05,0,0.05,0.1),labels=c("-15%","-10%","-5%","0","5%","10%"),cex.axis=1.5,mgp=c(1,1,0))
abline(h=0,lty=2)
for (i in 1:4){
  segments(i,res[i,4],i,res[i,5],lwd=2, col=1, lty=1)
}
dev.off()


###############################
####### Figure 4 #############
###############################
Male<-dd[dd$female==0,]
Female<-dd[dd$female==1,]
## Save effects by gender to be plotted
res_effects<-matrix(NA,nrow=8, ncol=4)
colnames(res_effects)<-c("estimate","s.e.","lower","upper")
row.names(res_effects)<-c("male_selfcensor","male_oneside","male_twoside","male_open",
                          "female_selfcensor","female_oneside","female_twoside","female_open")
res_effects2<-matrix(NA,nrow=8, ncol=4)
colnames(res_effects2)<-c("estimate","s.e.","lower","upper")
row.names(res_effects2)<-c("male_selfcensor","male_oneside","male_twoside","male_open",
                           "female_selfcensor","female_oneside","female_twoside","female_open")
## Self-censor
# Male:
ols.slfcensor.male <- lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male[Male$VigOrder==1,]) 
vcovWhite.slfcensor.male<-vcovHC(ols.slfcensor.male, type="HC1") #robust s.e. 
res_effects[1,1:2]<-coeftest(ols.slfcensor.male, vcov = vcovWhite.slfcensor.male)[2,1:2]
res_effects2[1,1:2]<-coeftest(ols.slfcensor.male, vcov = vcovWhite.slfcensor.male)[3,1:2]
# Female:
ols.slfcensor.female <- lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + Chn_news_freq + Local_news_freq +ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female[Female$VigOrder==1,]) 
vcovWhite.slfcensor.female<-vcovHC(ols.slfcensor.female, type="HC1") 
res_effects[5,1:2]<-coeftest(ols.slfcensor.female, vcov = vcovWhite.slfcensor.female)[2,1:2]
res_effects2[5,1:2]<-coeftest(ols.slfcensor.female, vcov = vcovWhite.slfcensor.female)[3,1:2]
## One-sided
# Male:
ols.postaking1.male <- lm(PosTaking1 ~ PRC + interpersonal+ Grad + AgeOver30 + Local_news_freq + ChnPol_news_freq + CIYearsOverOne + current + JoinMotivations_material +TrainingOverMonth, data = Male[Male$VigOrder==1,]) 
vcovWhite.postaking1.male<-vcovHC(ols.postaking1.male, type="HC1") 
res_effects[2,1:2]<-coeftest(ols.postaking1.male, vcov = vcovWhite.postaking1.male)[2,1:2]
res_effects2[2,1:2]<-coeftest(ols.postaking1.male, vcov = vcovWhite.postaking1.male)[3,1:2]
# Female:
ols.postaking1.female <- lm(PosTaking1 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female[Female$VigOrder==1,]) 
vcovWhite.postaking1.female<-vcovHC(ols.postaking1.female, type="HC1") 
res_effects[6,1:2]<-coeftest(ols.postaking1.female, vcov = vcovWhite.postaking1.female)[2,1:2]
res_effects2[6,1:2]<-coeftest(ols.postaking1.female, vcov = vcovWhite.postaking1.female)[3,1:2]
## Two-sided
# Male:
ols.postaking2.male <- lm(PosTaking2 ~ PRC + interpersonal+ Grad + AgeOver30 + ccp + Local_news_freq + InClassVig + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male[Male$VigOrder==1,]) 
vcovWhite.postaking2.male<-vcovHC(ols.postaking2.male, type="HC1") 
res_effects[3,1:2]<-coeftest(ols.postaking2.male, vcov = vcovWhite.postaking2.male)[2,1:2]
res_effects2[3,1:2]<-coeftest(ols.postaking2.male, vcov = vcovWhite.postaking2.male)[3,1:2]
# Female:
ols.postaking2.female <- lm(PosTaking2 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh + Chn_news_freq + Local_news_freq  + ChnPol_news_freq + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female[Female$VigOrder==1,])
vcovWhite.postaking2.female<-vcovHC(ols.postaking2.female, type="HC1") 
res_effects[7,1:2]<-coeftest(ols.postaking2.female, vcov = vcovWhite.postaking2.female)[2,1:2]
res_effects2[7,1:2]<-coeftest(ols.postaking2.female, vcov = vcovWhite.postaking2.female)[3,1:2]
## Open discussion
# Male:
ols.open.male <- lm(open ~ PRC + interpersonal  + AgeOver30 + Grad  + misChinahigh + Local_news_freq  + ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male[(Male$VigOrder==1),]) 
vcovWhite.open.male<-vcovHC(ols.open.male, type="HC1") #robust s.e. 
res_effects[4,1:2]<-coeftest(ols.open.male, vcov = vcovWhite.open.male)[2,1:2]
res_effects2[4,1:2]<-coeftest(ols.open.male, vcov = vcovWhite.open.male)[3,1:2]
# Female:
ols.open.female <- lm(open ~ PRC + interpersonal  + AgeOver30 + ccp + Grad + misChinahigh + Chn_news_freq + Local_news_freq  + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth, data = Female[Female$VigOrder==1,]) 
vcovWhite.open.female<-vcovHC(ols.open.female, type="HC1") 
res_effects[8,1:2]<-coeftest(ols.open.female, vcov = vcovWhite.open.female)[2,1:2]
res_effects2[8,1:2]<-coeftest(ols.open.female, vcov = vcovWhite.open.female)[3,1:2]

## Plotting Figure 4
res_effects<-as.data.frame(res_effects)
res_effects$lower<-res_effects[,1] - qnorm(0.975)*res_effects[,2] #lower bound of 95% CI
res_effects$upper<-res_effects[,1] + qnorm(0.975)*res_effects[,2] #upper bound of 95% CI
min(res_effects$lower)
max(res_effects$upper)
png("Log/Figure4.png", 
    units="in", width=10, height=7, res=240)
par(mar=c(8, 5, 1, 2) + 0.1)
x<-1:4
plot(x-0.1,res_effects[1:4,1],ylim=c(-0.5,0.5),xlim=c(0.4,4.6),main = "",
     ylab = "Effect of the Objectives Prime",
     pch=16,cex.lab=1.5,cex.axis=1.5, xaxt="n",xlab="",cex=2)
points(x+0.1,res_effects[5:8,1],pch=6)
axis(1,1:4,labels=c("Self-censor","One-sided","Two-sided","Open discussion"),
     cex.axis=1.5,mgp=c(3,2,0))
abline(h=0,lty=2)
for(i in 1:4){
  segments(i-0.1,res_effects[i,3],i-0.1,res_effects[i,4],lwd=2)
  segments(i+0.1,res_effects[i+4,3],i+0.1,res_effects[i+4,4],lwd=2,lty=2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()


###############################
####### Figure 5 #############
###############################
## Save effects by gender to be plotted
res_effects2<-as.data.frame(res_effects2)
res_effects2$lower<-res_effects2[,1] - qnorm(0.975)*res_effects2[,2] #lower bound of 95% CI
res_effects2$upper<-res_effects2[,1] + qnorm(0.975)*res_effects2[,2] #upper bound of 95% CI
min(res_effects2$lower)
max(res_effects2$upper)
png("Log/Figure5.png", 
    units="in", width=10, height=7, res=240)
par(mar=c(8, 5, 1, 2) + 0.1)
x<-1:4
plot(x-0.1,res_effects2[1:4,1],ylim=c(-0.42,0.4),xlim=c(0.4,4.6),main = "",
     ylab = "Effect of the Social Prime",
     pch=16,cex.lab=1.5,cex.axis=1.5, xaxt="n",xlab="",cex=2)
points(x+0.1,res_effects2[5:8,1],pch=6)
axis(1,1:4,labels=c("Self-censor","One-sided","Two-sided","Open discussion"),
     cex.axis=1.5,mgp=c(3,2,0))
abline(h=0,lty=2)
for(i in 1:4){
  segments(i-0.1,res_effects2[i,3],i-0.1,res_effects2[i,4],lwd=2)
  segments(i+0.1,res_effects2[i+4,3],i+0.1,res_effects2[i+4,4],lwd=2,lty=2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()





