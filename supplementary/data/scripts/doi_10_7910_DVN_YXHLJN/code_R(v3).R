# This code allows reproducing all tables and figures shown in the manuscript entitled "Female embryos are more likely to die than males in a wild mammal"

# R version 4.1.2

# load the dataset #

setwd('C:/Users/Douhard/Documents/travail/sanglier-fetal sex ratio/soumission/american naturalist')## chemin Mathieu

df_Chato=read.csv('data_wildboar(v3).csv',header=T, sep=",")

## variable definitions ##
colnames(df_Chato)
# year: the definition of “year” is based on hunting seasons and the seasonality of mast production, thus starting in October 1st and ending in September 30th the next civil year. 
# Mast: four categories depending on the quantity of beechnuts and acorns found in the stomachs during the hunting period 
# Weight_Mother: Maternal body mass (kg)
# LS:the number of fetus counted within pregnant females when shot during the hunting season
# nb_males: number of male fetuses in the litter
# nb_females: number of female fetuses in the litter
# ME:Embryonic mortality

# mast production defined as a factor
df_Chato$Mast<-as.factor(df_Chato$Mast)

## view correlations between continuous explanatory variables  
cor(df_Chato[,c(3,4,7)])

#####################################################################################################################################
################### Analyses on the whole data set ##################################################################################
#####################################################################################################################################

## Here, we suppose that a higher number of fetuses than of corpora lutea is a consequence of the occurrence of identical twins
df_Chato$ME1<-ifelse(df_Chato$ME<0,0,df_Chato$ME)

### Results shown in table 1 ###

# For a GLM with a binomial family, the response is specified as a two-column integer matrix with function cbind: the first column gives the number of successes and the second the number of failures.
mod2<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother)*scale(ME1) + Mast*scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
mod3<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother)*scale(ME1) + Mast, data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod3,mod2,test="Chisq")

mod4<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother) + Mast, data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod4,mod3,test="Chisq")

mod5<-glm(cbind(nb_males,nb_females)~ scale(LS) + scale(ME1) + scale(Weight_Mother) + Mast, data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod5,mod4,test="Chisq")

mod6<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother), data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod6,mod4,test="Chisq")

mod7<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod7,mod6,test="Chisq")

## Results shown in table 2 ### 

summary(mod7)

## figure 1A ##

f1p<-data.frame(ME1=seq(min(df_Chato$ME1),max(df_Chato$ME1),length=1000))
f1p$LS<-mean(df_Chato$LS)
f1p$fit<-predict(mod7,f1p,type="response")
f1p$sefit<-predict(mod7,f1p,type="response",se.fit=T)$se.fit

pdf("Fig1A.pdf",useDingbats=FALSE)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=df_Chato,ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Whole dataset",family="sans",seg.col = "black",font.main = 1,font.lab=1)
lines(fit*100~ME1,f1p,lwd=2)
lines((fit+sefit)*100~ME1,f1p,lty=2)
lines((fit-sefit)*100~ME1,f1p,lty=2)
mtext("A)",side=3,adj=-0.10,line=1)
dev.off()

## figure 2A ###

int1<-data.frame(ME1=c(
seq(min(df_Chato$ME1[df_Chato$LS=="4"]),max(df_Chato$ME1[df_Chato$LS=="4"]),length=1000),
seq(min(df_Chato$ME1[df_Chato$LS=="7"]),max(df_Chato$ME1[df_Chato$LS=="7"]),length=1000)
))
int1$LS<-c(rep(4,1000),rep(7,1000))
int1$fit<-predict(mod7,int1,type="response")
int1$sefit<-predict(mod7,int1,type="response",se.fit=T)$se.fit

pdf("Fig2A.pdf",useDingbats=FALSE)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=subset(df_Chato,LS =="4"),col="dodgerblue",seg.col ="dodgerblue",ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Whole dataset",family="sans",font.main = 1,font.lab=1)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=subset(df_Chato,LS =="7"),col="orange",seg.col ="orange",ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Whole dataset",family="sans",font.main = 1,font.lab=1,add=T)
lines(fit*100~ME1,int1[int1$LS=="4",],lwd=2,col="dodgerblue")
lines((fit+sefit)*100~ME1,int1[int1$LS=="4",],col="dodgerblue",lty=2)
lines((fit-sefit)*100~ME1,int1[int1$LS=="4",],col="dodgerblue",lty=2)
lines(fit*100~ME1,int1[int1$LS=="7",],lwd=2,col="orange")
lines((fit+sefit)*100~ME1,int1[int1$LS=="7",],col="orange",lty=2)
lines((fit-sefit)*100~ME1,int1[int1$LS=="7",],col="orange",lty=2)
legend("topright", legend = c("Litter sizes of 4 (n=80)", "Litter sizes of 7 (n=89)"),lty=c(1,1),col=c("dodgerblue","orange"),pch=c(18,18))
mtext("A)",side=3,adj=-0.10,line=1)
dev.off()

### Table S2 in supplementary information ###

mod8<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
mod9<-glm(cbind(nb_males,nb_females)~ scale(LS) + scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod9,mod8,test="Chisq")

mod10<-glm(cbind(nb_males,nb_females)~ Mast *scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
mod11<-glm(cbind(nb_males,nb_females)~ Mast + scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod11,mod10,test="Chisq")

mod12<-glm(cbind(nb_males,nb_females)~ scale(Weight_Mother)*scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
mod13<-glm(cbind(nb_males,nb_females)~ scale(Weight_Mother)+ scale(ME1), data=df_Chato, family=binomial) ############################ THE MODEL
anova(mod13,mod12,test="Chisq")

########################################################################################################################
######### analysis on the restrictive dataset ##########################################################################
########################################################################################################################

## we remove cases where a higher number of fetuses than of corpora lutea was observerd (possibly due to errors). 
df_Chato2<-subset(df_Chato,ME>=0)

## Results from table 1##

mod222<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother)*scale(ME1) + Mast*scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
mod333<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother)*scale(ME1) + Mast, data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod333,mod222,test="Chisq")

mod444<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother) + Mast, data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod444,mod333,test="Chisq")

mod555<-glm(cbind(nb_males,nb_females)~ scale(LS) + scale(ME1) + scale(Weight_Mother) + Mast, data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod555,mod444,test="Chisq")

mod666<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1) + scale(Weight_Mother), data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod666,mod444,test="Chisq")

mod777<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod777,mod666,test="Chisq")

## Results from table 2##
summary(mod777)

## figure 1B ##

f14p<-data.frame(ME1=seq(min(df_Chato2$ME1),max(df_Chato2$ME1),length=1000))
f14p$LS<-mean(df_Chato2$LS)
f14p$fit<-predict(mod777,f14p,type="response")
f14p$sefit<-predict(mod777,f14p,type="response",se.fit=T)$se.fit

pdf("Fig1B.pdf",useDingbats=FALSE)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=df_Chato2,ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Restrictive dataset",family="sans",seg.col = "black",font.lab=1,font.main = 1)
lines(fit*100~ME1,f14p,lwd=2)
lines((fit+sefit)*100~ME1,f14p,lty=2)
lines((fit-sefit)*100~ME1,f14p,lty=2)
mtext("B)",side=3,adj=-0.10,line=1)
dev.off()

## figure 2b ###

into1<-data.frame(ME1=c(
seq(min(df_Chato2$ME1[df_Chato2$LS=="4"]),max(df_Chato2$ME1[df_Chato2$LS=="4"]),length=1000),
seq(min(df_Chato2$ME1[df_Chato2$LS=="7"]),max(df_Chato2$ME1[df_Chato2$LS=="7"]),length=1000)
))
into1$LS<-c(rep(4,1000),rep(7,1000))
into1$fit<-predict(mod777,into1,type="response")
into1$sefit<-predict(mod777,into1,type="response",se.fit=T)$se.fit

pdf("Fig2b.pdf",useDingbats=FALSE)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=subset(df_Chato2,LS =="4"),col="dodgerblue",seg.col ="dodgerblue",ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Restrictive dataset",family="sans",font.main = 1,font.lab=1)
sunflowerplot((nb_males)/(nb_males + nb_females)*100~ME1,data=subset(df_Chato2,LS =="7"),col="orange",seg.col ="orange",ylab=substitute(paste(italic('Percentage of male fetus'))),xlab=substitute(paste(italic('Percentage of embryonic mortality'))),main="Restrictive dataset",family="sans",font.main = 1,font.lab=1,add=T)
lines(fit*100~ME1,into1[into1$LS=="4",],lwd=2,col="dodgerblue")
lines((fit+sefit)*100~ME1,into1[into1$LS=="4",],col="dodgerblue",lty=2)
lines((fit-sefit)*100~ME1,into1[into1$LS=="4",],col="dodgerblue",lty=2)
lines(fit*100~ME1,into1[into1$LS=="7",],lwd=2,col="orange")
lines((fit+sefit)*100~ME1,into1[into1$LS=="7",],col="orange",lty=2)
lines((fit-sefit)*100~ME1,into1[into1$LS=="7",],col="orange",lty=2)
legend("topright", legend = c("Litter sizes of 4 (n=77)", "Litter sizes of 7 (n=81)"),lty=c(1,1),col=c("dodgerblue","orange"),pch=c(18,18))
mtext("B)",side=3,adj=-0.10,line=1)
dev.off()

#### Table S2 in supplementary information ###

mod88<-glm(cbind(nb_males,nb_females)~ scale(LS)*scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
mod99<-glm(cbind(nb_males,nb_females)~ scale(LS) + scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod99,mod88,test="Chisq")

mod100<-glm(cbind(nb_males,nb_females)~ Mast *scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
mod111<-glm(cbind(nb_males,nb_females)~ Mast + scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod111,mod100,test="Chisq")

mod120<-glm(cbind(nb_males,nb_females)~ scale(Weight_Mother)*scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
mod130<-glm(cbind(nb_males,nb_females)~ scale(Weight_Mother)+ scale(ME1), data=df_Chato2, family=binomial) ############################ THE MODEL
anova(mod130,mod120,test="Chisq")


