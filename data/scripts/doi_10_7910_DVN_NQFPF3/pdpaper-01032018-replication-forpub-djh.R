### Dan Hopkins, Cheryl Kaiser, Efren Perez et al.
### 1/7/2018
### Replication file for Does Perceiving Discrimination Influence Partisanship 
### among U.S. Immigrant Minorities?

### set working directory
setwd("/users/danhop/Dropbox/PDparty/replication/jexps-replication/")

### log file 
sink("logfile-hopkaiper-01072018.txt")

### load libraries
library(texreg)
library(xtable)
library(lme4)
library(dplyr)


### load modified experiment 1 data
load("lab-experiment-1-01032018.Rdata")

### descriptive statistics experiment 1 ----
### Table 3
vars1 <- c("OFFENSIVE","DISCRIMASIAN","PDGROUP","PID7R","PIDCHANGE","FTREPDEM","FTROMOBA")
rmat <- matrix(NA,length(vars1),4)
for(i in 1:length(vars1)){
  txt <- paste("holdvar <- dta1$",vars1[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(holdvar,na.rm=T)
  rmat[i,2] <- sd(holdvar,na.rm=T)
  rmat[i,3] <- min(holdvar,na.rm=T)
  rmat[i,4] <- max(holdvar,na.rm=T)
}
rownames(rmat) <- vars1
xtable(rmat,digits=c(0,3,3,3,3))

###### manipulation checks experiment 1 ----
##### Table 9
lout1mc <- lm(OFFENSIVE~ARTICLE,data=dta1)
lout2mc <- lm(DISCRIMASIAN~ARTICLE,data=dta1)
lout3mc <- lm(PDGROUP~ARTICLE,data=dta1)
texreg(list(lout1mc,lout2mc,lout3mc),custom.model.names=c("Offensive Info","Discriminatory","PD Against Group"),stars=0.05,digits=3)

#### outcome models experiment 1
#### Table 14
lout1pid <- lm(PID7R~ARTICLE,data=dta1)
lout1change <- lm(PIDCHANGE ~ ARTICLE,data=dta1)
lout1ft <- lm(FTREPDEM ~ ARTICLE,data=dta1)
lout1ftro <- lm(FTROMOBA ~ ARTICLE,data=dta1)

texreg(list(lout1pid,lout1change,lout1ft,lout1ftro),custom.model.names=c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"),stars=0.05,digits=3)

#### figure: experiment 1 results ----
#### subset of figure 1
models <- rev(c("lout1pid","lout1change","lout1ft","lout1ftro"))
lbs <- rev(c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"))

#pdf("study1mainresults09142017.pdf")
plot(1:length(models),type="n",ylab="",ylim=c(0.5,length(models)+.5),xlim=c(-.15,.15),yaxt="n",
     xlab="Pro Dems                          Pro GOP",
     main="Experiment 1: Asian Americans",cex.main=1.5,cex.lab=1.25)
for(i in 1:length(models)){
  txt <- paste("lout <- ",models[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i,pch=16)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i,i),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i,i),pch=2,lwd=2)	
  text(x=summary(lout)$coef[2,1],y=i+.25,lbs[i])
}
abline(v=0,lty=2)
#dev.off()


#### load experiment 2 ----
### load modified experiment 2 data
load("lab-experiment-2-01072018.Rdata")

vars2 <- c("OFFENSIVE","DISCRIMASIAN","PDGROUP","PDPERSON","PID7R","PIDCHANGE","FTREPDEM","FTROMOBA")
rmat <- matrix(NA,length(vars2),4)
for(i in 1:length(vars2)){
  txt <- paste("holdvar <- dta2$",vars2[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(holdvar,na.rm=T)
  rmat[i,2] <- sd(holdvar,na.rm=T)
  rmat[i,3] <- min(holdvar,na.rm=T)
  rmat[i,4] <- max(holdvar,na.rm=T)
}
rownames(rmat) <- vars2
xtable(rmat,digits=c(0,3,3,3,3))

#### experiment 2 manipulation checks ----
#### Table 10
lout1mc <- lm(OFFENSIVE~ARTICLE,data=dta2)
lout2mc <- lm(DISCRIMASIAN~ARTICLE,data=dta2)
lout3mc <- lm(PDGROUP~ARTICLE,data=dta2)
lout4mc <- lm(PDPERSON~ARTICLE,data=dta2)
texreg(list(lout1mc,lout2mc,lout3mc,lout4mc),custom.model.names=c("Offensive Info","Discriminatory","PD Against Group","PD Against Ind."),stars=0.05,digits=3)

#### experiment 2 outcomes ----
lout1pid <- lm(PID7R~ARTICLE,data=dta2)
lout1change <- lm(PIDCHANGE ~ ARTICLE,data=dta2)
lout1ft <- lm(FTREPDEM ~ ARTICLE,data=dta2)
lout1ftro <- lm(FTROMOBA ~ ARTICLE,data=dta2)

models <- rev(c("lout1pid","lout1change","lout1ft","lout1ftro"))
lbs <- rev(c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"))
texreg(list(lout1pid,lout1change,lout1ft,lout1ftro),custom.model.names=c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"),stars=0.05,digits=3)

### study 2 graphical presentation ----
#pdf("study2mainresults09142017.pdf")
plot(1:length(models),type="n",ylab="",ylim=c(0.5,length(models)+.5),xlim=c(-.075,.075),yaxt="n",
     xlab="Pro Dems                          Pro GOP",
     main="Experiment 2: Asian Americans",cex.main=1.5,cex.lab=1.25)
for(i in 1:length(models)){
  txt <- paste("lout <- ",models[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i,pch=16)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i,i),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i,i),pch=2,lwd=2)	
  text(x=summary(lout)$coef[2,1],y=i+.25,lbs[i])
}
abline(v=0,lty=2)
#dev.off()

#### load experiment 3 ----
### load modified experiment 1 data
load("lab-experiment-3-01072018.Rdata")

#### Table 5
vars3 <- c("OFFENSIVE","PREJUDICED","DISCRIMASIAN","PDGROUP","PDPERSON","PID7R","PIDCHANGE","FTREPDEM","FTROMOBA")
rmat <- matrix(NA,length(vars3),4)
for(i in 1:length(vars3)){
  txt <- paste("holdvar <- dta3$",vars3[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(holdvar,na.rm=T)
  rmat[i,2] <- sd(holdvar,na.rm=T)
  rmat[i,3] <- min(holdvar,na.rm=T)
  rmat[i,4] <- max(holdvar,na.rm=T)
}
rownames(rmat) <- vars3
xtable(rmat,digits=c(0,3,3,3,3))

#### experiment 3 manipulation checks ----
### Table 11
lout1mc <- lm(OFFENSIVE~ARTICLE,data=dta3)
lout2mc <- lm(PREJUDICED~ARTICLE,data=dta3)
lout3mc <- lm(DISCRIMASIAN~ARTICLE,data=dta3)
lout4mc <- lm(PDGROUP~ARTICLE,data=dta3)
lout5mc <- lm(PDPERSON~ARTICLE,data=dta3)
texreg(list(lout1mc,lout2mc,lout3mc,lout4mc,lout5mc),custom.model.names=c("Offensive Info","Prejudiced","Discriminatory","PD Against Group","PD Against Ind."),stars=0.05,digits=3)

#### experiment 3 outcomes ----
#### Table 16
lout1pid <- lm(PID7R~ARTICLE,data=dta3)
lout1change <- lm(PIDCHANGE ~ ARTICLE,data=dta3)
lout1ft <- lm(FTREPDEM ~ ARTICLE,data=dta3)
lout1ftro <- lm(FTROMOBA ~ ARTICLE,data=dta3)
models <- rev(c("lout1pid","lout1change","lout1ft","lout1ftro"))
lbs <- rev(c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"))

texreg(list(lout1pid,lout1change,lout1ft,lout1ftro),custom.model.names=c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"),stars=0.05,digits=3)

### experiment 3 graphical presentation ----
#pdf("study3mainresults09142017.pdf")
plot(1:length(models),type="n",ylab="",ylim=c(0.5,length(models)+.5),xlim=c(-.075,.075),yaxt="n",
     xlab="Pro Dems                          Pro GOP",
     main="Experiment 3: Asian Americans",cex.main=1.5,cex.lab=1.25)
for(i in 1:length(models)){
  txt <- paste("lout <- ",models[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i,pch=16)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i,i),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i,i),pch=2,lwd=2)	
  text(x=summary(lout)$coef[2,1],y=i+.25,lbs[i])
}
abline(v=0,lty=2)
#dev.off()

### load experiment 4 ----
load("lab-experiment-4-01072018.Rdata")

### experiment 4 descriptive stats ----
### Table 6
vars4 <- c("OFFENSIVE","DISCRIMLATINO","PDGROUP","PDPERSON","PID7R","PIDCHANGE","FTREPDEM","FTROMOBA")
rmat <- matrix(NA,length(vars4),4)
for(i in 1:length(vars4)){
  txt <- paste("holdvar <- dta4$",vars4[i],sep="")
  eval(parse(text=txt))
  rmat[i,1] <- mean(holdvar,na.rm=T)
  rmat[i,2] <- sd(holdvar,na.rm=T)
  rmat[i,3] <- min(holdvar,na.rm=T)
  rmat[i,4] <- max(holdvar,na.rm=T)
}
rownames(rmat) <- vars4
xtable(rmat,digits=c(0,3,3,3,3))

#### experiment 4 manipulation checks ----
#### Table 12
lout1mc <- lm(OFFENSIVE~ARTICLE,data=dta4)
lout2mc <- lm(DISCRIMLATINO~ARTICLE,data=dta4)
lout3mc <- lm(PDGROUP~ARTICLE,data=dta4)
lout4mc <- lm(PDPERSON~ARTICLE,data=dta4)
texreg(list(lout1mc,lout2mc,lout3mc,lout4mc),custom.model.names=c("Offensive Info","Discriminatory","PD Against Group","PD Against Ind."),stars=0.05,digits=3)

#### experiment 4 outcomes ----
#### Table 17
lout1pid <- lm(PID7R ~ TREATED,data=dta4)
lout1change <- lm(PIDCHANGE ~ TREATED,data=dta4)
lout1ft <- lm(FTREPDEM ~ TREATED,data=dta4)
lout1ftro <- lm(FTROMOBA ~ TREATED,data=dta4)

texreg(list(lout1pid,lout1change,lout1ft,lout1ftro),custom.model.names=c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"),stars=0.05,digits=3)

models <- rev(c("lout1pid","lout1change","lout1ft","lout1ftro"))
lbs <- rev(c("Party ID","Party Change","GOP - Dem Feelings","Romney - Obama Feelings"))

#pdf("study4-UTEP-mainresults09142017.pdf")
plot(1:length(models),type="n",ylab="",ylim=c(0.5,length(models)+.5),xlim=c(-.10,.10),yaxt="n",
     xlab="Pro Dems                          Pro GOP",
     main="Experiment 4: Latinos",cex.main=1.5,cex.lab=1.25)
for(i in 1:length(models)){
  txt <- paste("lout <- ",models[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i,pch=16)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i,i),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i,i),pch=2,lwd=2)	
  text(x=summary(lout)$coef[2,1],y=i+.25,lbs[i])
}
abline(v=0,lty=2)
#dev.off()

### joint analysis for power (Table 1) ----
### generates partial data for Table 1 (columns 1 and 2)
vars <- c("FTREPDEM","ARTICLE","EXPERIMENT","PIDCHANGE","PID7R","FTROMOBA")
dta1sub <- subset(dta1,select=vars)
dta2sub <- subset(dta2,select=vars)
dta3sub <- subset(dta3,select=vars)
dta4sub <- subset(dta4,select=vars)

dta.joint <- rbind(dta1sub,dta2sub,dta3sub,dta4sub)

lout1p <- lmer(PID7R ~ ARTICLE+(1|EXPERIMENT),data=dta.joint)
lout2p <- lmer(PIDCHANGE ~ ARTICLE+(1|EXPERIMENT),data=dta.joint)
lout3p <- lmer(FTREPDEM ~ ARTICLE+(1|EXPERIMENT),data=dta.joint)
lout4p <- lmer(FTROMOBA ~ ARTICLE+(1|EXPERIMENT),data=dta.joint)

summary(lout1p)
summary(lout2p)
summary(lout3p)
summary(lout4p)


#### load original GfK data
load("gfk-experiment-01042018.Rdata")
dta.gfk$FTTRUCLI <- (dta.gfk$FTTRUMP-dta.gfk$FTCLINTON+100)/200
dta.gfk$FTREPDEM <- (dta.gfk$FTREPS-dta.gfk$FTDEMS+100)/200

### DESCRIPTIVE STATISTICS, Gfk, Table 7 ----
dtahis <- dta.gfk[dta.gfk$ASIAN==0,]
dtaasn <- dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,]
dtaspn <- dta.gfk[dta.gfk$ASIAN==0 & dta.gfk$SPANISH==1,]
vars <- c("HASBA","EDYEARS","INCOME","PID","MEXICAN","CUBAN","PUERTORICAN",
          "BORNUS","CITIZEN","SPANISH","INDIAN","FILIPINO","CHINESE","JAPANESE",
          "GROUPDIS","PERSONDIS","FTREPDEM","FTTRUCLI")
rmat <- matrix(NA,length(vars),6)
for(i in 1:length(vars)){
  txt1 <- paste("vars1 <- dtaasn$",vars[i])
  eval(parse(text=txt1))
  
  txt2 <- paste("vars2 <- dtahis$",vars[i])
  eval(parse(text=txt2))
  
  rmat[i,1] <- mean(vars1,na.rm=T)
  rmat[i,2] <- sd(vars1,na.rm=T)
  rmat[i,4] <- mean(vars2,na.rm=T)
  rmat[i,5] <- sd(vars2,na.rm=T)
}
rownames(rmat) <- vars
colnames(rmat) <- c("Asian.Mean","Asian.SD","Asian.Census","Hispanic.Mean","Hispanic.SD","Hispanic.Census")
round(rmat,digits=3)
library(xtable)
xtable(rmat,digits=c(0,rep(2,6)))

#### Summary of correlates (Table 8) ----
louta1 <- lm(GROUPDIS ~ AGE+EDYEARS+CITIZEN+BORNUS+INCOME+CHINESE+FILIPINO+INDIAN+JAPANESE,data=dtaasn)
louth1 <- lm(GROUPDIS ~ AGE+EDYEARS+CITIZEN+BORNUS+INCOME+MEXICAN+CUBAN+PUERTORICAN+SPANISH,data=dtahis)
louta2 <- lm(PIDPRE ~ AGE+EDYEARS+CITIZEN+BORNUS+INCOME+CHINESE+FILIPINO+INDIAN+JAPANESE,data=dtaasn)
louth2 <- lm(PIDPRE ~ AGE+EDYEARS+CITIZEN+BORNUS+INCOME+MEXICAN+CUBAN+PUERTORICAN+SPANISH,data=dtahis)

texreg(list(louta1,louth1,louta2,louth2),digits=3,stars=0.05,custom.model.names=c("Asian Am.","Latino/as","Asian Am.","Latino/a"))

##### Gfk, Manipulation Checks ----
lout1allgrp <- lm(I(GROUPDIS) ~ ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.gfk[dta.gfk$OPTIN==0,])
lout1asiangrp <- lm(I(GROUPDIS) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+
                      BORNUS+as.factor(PPINCIMP),data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,])
lout1latinogrp <- lm(I(GROUPDIS) ~ ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+
                       as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.gfk[dta.gfk$ASIAN==0,])
lout1allper <- lm(I(PERSONDIS) ~ ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                  data=dta.gfk[dta.gfk$OPTIN==0,])
lout1asianper <- lm(I(PERSONDIS) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                    data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,])
lout1latinoper <- lm(I(PERSONDIS) ~ ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                     data=dta.gfk[dta.gfk$ASIAN==0,])

lbs <- rev(c("All","Asian Americans","Latinos"))
models <- rev(c("lout1allgrp","lout1asiangrp","lout1latinogrp"))
modelsper <- rev(c("lout1allper","lout1asianper","lout1latinoper"))

#### gfk experiments, manipulation checks figure ----
#### generate figure 4
#pdf("exp1mainresultsGROUPPERSONDIS07192017.pdf")
plot(1:length(models),type="n",ylab="",ylim=c(0.5,length(models)+.5),xlim=c(-.25,.85),yaxt="n",
     xlab="Change in Perceived Discrimination",
     main="Effect of Article",cex.main=1.5,cex.lab=1.25)
for(i in 1:length(models)){
  txt <- paste("lout <- ",models[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i,pch=16)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i,i),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i,i),pch=2,lwd=2)
  
  text(x=summary(lout)$coef[2,1],y=i+.25,lbs[i],cex=1.4)
  
  txt <- paste("lout <- ",modelsper[i])
  eval(parse(text=txt))
  points(x=summary(lout)$coef[2,1],y=i-.15,pch=17)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(i-.15,i-.15),pch=2)	
  lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(i-.15,i-.15),pch=2,lwd=2)
  
  text(x=.675,y=i,"Targeting Group")
  text(x=.675,y=i-.15,"Targeting Individual")
  
}
abline(v=0,lty=2)
#dev.off()

### Table 2
### gfk data; manipulation checks for group-targeted PD
texreg(list(lout1allgrp,lout1asiangrp,lout1latinogrp),stars=0.05)
### Table 13
### gfk data; manipulation checks for personally targeted PD
texreg(list(lout1allper,lout1asianper,lout1latinoper),stars=0.05)

### joint results; gfk experiment ----
#####
lout1alldr <- lm(I(FTREPDEM) ~ ARTICLE+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                 data=dta.gfk[dta.gfk$OPTIN==0,])
lout1asiandr <- lm(I(FTREPDEM) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                   data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,])
lout1optindr <- lm(I(FTREPDEM) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                   data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==1,])
lout1latinodr <- lm(I(FTREPDEM) ~ ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+
                      FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                    data=dta.gfk[dta.gfk$ASIAN==0,])
### Article Table 18
texreg(list(lout1alldr,lout1asiandr,lout1latinodr),digits=3,stars=0.05)

lout1allct <- lm(I(FTTRUCLI) ~ ARTICLE+OPTIN+SPANISH+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),data=dta.gfk[dta.gfk$OPTIN==0,])
lout1asianct <- lm(I(FTTRUCLI) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                   data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,])
lout1optinct <- lm(I(FTTRUCLI) ~ ARTICLE+OPTIN+JAPANESE+CHINESE+INDIAN+FILIPINO+EDYEARS+FEMALE+
                     as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                   data=dta.gfk[dta.gfk$ASIAN==1 & dta.gfk$OPTIN==0,])
lout1latinoct <- lm(I(FTTRUCLI) ~ ARTICLE+SPANISH+MEXICAN+CUBAN+PUERTORICAN+EDYEARS+
                      FEMALE+as.factor(ppagect4)+CITIZEN+BORNUS+as.factor(PPINCIMP),
                    data=dta.gfk[dta.gfk$ASIAN==0 & dta.gfk$OPTIN==0,])
#### article table 19
texreg(list(lout1allct,lout1asianct,lout1latinoct),digits=3,stars=0.05)

#### produces final element of Figure 1
models2 <- rev(c("lout1alldr","lout1asiandr","lout1latinodr"))
models1 <- rev(c("lout1allct","lout1asianct","lout1latinoct"))
outcomes <- rev(c("GOP - Dems","Trump - Clinton"))

pch.options <- c(16,17,18)
groups <- rev(c("All Respondents","Asian Americans","Latinos"))

#pdf("~/exp1mainresultsjoint08032018.pdf")
plot(1:10,type="n",ylab="",ylim=c(0.5,9.5),xlim=c(-0.06,0.06),yaxt="n",
     xlab="Pro Dems                         Pro GOP",
     main="Experiment 5: National Samples \n of Latinos and Asian Americans",cex.main=1.5,cex.lab=1.25)

k <- -1
### loop over 3 dependent variables
for(j in 1:2){
  k <- k+2
  txt10 <- paste("models <- models",j,sep="")
  eval(parse(text=txt10))
  for(i in 1:length(models)){
    txt <- paste("lout <- ",models[i])
    eval(parse(text=txt))
    points(x=summary(lout)$coef[2,1],y=k,pch=pch.options[i])	
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96,summary(lout)$coef[2,1]+summary(lout)$coef[2,2]*1.96),y=c(k,k),pch=2)	
    #text(x=summary(lout)$coef[2,1],y=k+.25,lbs[i])
    lines(x=c(summary(lout)$coef[2,1]-summary(lout)$coef[2,2],summary(lout)$coef[2,1]+summary(lout)$coef[2,2]),y=c(k,k),pch=2,lwd=2)	
    if(j==2){
      #text(groups[i],x=(summary(lout)$coef[2,1]-summary(lout)$coef[2,2]*1.96-2),y=k)
      text(groups[i],x=-0.05,y=k)
      
    }else{
      text(groups[i],x=.05,y=k)
    }
    k <- k+1
  }
}
abline(v=0,lty=2)
#dev.off()

text(y=3.95,x=0,outcomes[1],cex=1.4)
text(y=8.95,x=0,outcomes[2],cex=1.4)
#text(y=13.95,x=0,outcomes[3],cex=1.4)
#dev.off()

sink()