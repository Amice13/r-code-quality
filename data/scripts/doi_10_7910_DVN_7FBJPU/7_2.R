################Simulation for cluster-randomized experiments###################
######################Section 7.2 Simulation based on data######################

library(gee)
library(car)
library(geepack)
library(readstata13)
library(tidyverse)
library(ggpubr)


set.seed(3)
#Monte Carlo replication times
replicate=1000        
#Read data
dat=read.dta13("BD-SAN-FINAL.dta")
attach(dat)


#Identify the non-qualified household
notqualified=is.na(r4_hyg_access)|(dat$eligible=="Not eligible")|(treat_cat_1=="LPP Only")|(treat_cat_1=="Supply Only")|(vid==301)
N=length(notqualified)-sum(notqualified)
#Treatment indicator Z, outcome Y, and covariate x
Z=numeric(N)
Y=numeric(N)
x=numeric(N)
cluster=numeric(N)
N=0
for (i in 1:18254)
{
  if (notqualified[i]==FALSE)
  {
    N=N+1
    if ((treat_cat_1[i]=="LPP+Subsidy+Supply")|(treat_cat_1[i]=="LPP+Subsidy")) Z[N]=1
    else Z[N]=0
    if (vid[i]==301) Z[N]=0
    Y[N]=r4_hyg_access[i]
    x[N]=mean(bl_c_hyg_access[cid==cid[i]])
    cluster[N]=vid[i]
  }
}
cluster=as.factor(cluster)
cluster=as.integer(cluster)
Y=Y-1
x=x-mean(x)

#Impute the missing outcome
r=lm(Y~1+Z+x+Z*x)
summary(r)
Ymiss=numeric(N)
for (i in 1:N)
{
  Ymiss[i]=rbinom(1,1,r$coefficients[1]+r$coefficients[2]*(1-Z[i])+r$coefficients[3]*x[i]+r$coefficients[4]*x[i]*(1-Z[i]))
}
Y1=Z*Y+(1-Z)*Ymiss
Y0=(1-Z)*Y+Z*Ymiss

#Cluster treatment indicator and cluster size
C=max(cluster)
Zc=numeric(C)
nsize=numeric(C)
for (i in 1:N)
{  
  Zc[cluster[i]]=Z[i]
  nsize[cluster[i]]=nsize[cluster[i]]+1
}
nc=nsize-N/C
ec=sum(Zc)/C
#Prepare for cluster-total regressions
Ysum1=numeric(C)
Ysum0=numeric(C)
xsum=numeric(C)
for (c in 1:C)
{
  Ysum1[c]=sum(Y1[cluster==c])
  Ysum0[c]=sum(Y0[cluster==c])
  xsum[c]=sum(x[cluster==c])
}
causal = mean(Y1 - Y0)

#Set variable to store coverage probability for each each estimators and each regression variance estimation
#tauI is individual-level regression, Difference-in-means
#tauIR is individual-level regression adjustment with covariates x_{ij}
#tauIR2 is individual-level regression adjustment with average covariates \bar{x}_{i\cdot}
#tauT is cluster-total regression
#tauTR is cluter-total regression adjustment with cluster size n_{ij} as covariates
#tauTR2 is cluster-total regression adjustment with cluster size n_{ij} and sum of covariates \tilde{x}_{i\cdot}
#tauA is cluster-average regression
#tauA is cluster-average regression with average covariates \bar{x}_{i\cdot} 
#tauOLS is traditional ancova regression with covariates x_{ij}
tauIp=0;   tauIRp=0;   tauIR2p=0;   tauTp=0;   tauTRp=0;   tauTR2p=0;   tauAp=0;   tauARp=0;   tauOLSp=0

tauI=numeric(replicate)
tauIR=numeric(replicate)
tauIR2=numeric(replicate)
tauT=numeric(replicate)
tauTR=numeric(replicate)
tauTR2=numeric(replicate)
tauA=numeric(replicate)
tauAR=numeric(replicate)
tauOLS=numeric(replicate)

tauIev=numeric(replicate);     
tauIRev=numeric(replicate);   
tauIR2ev=numeric(replicate);   
tauTev=numeric(replicate);    
tauTRev=numeric(replicate);   
tauTR2ev=numeric(replicate);  
tauAev=numeric(replicate);   
tauARev=numeric(replicate);   
tauOLSev=numeric(replicate);  

for (i in 1:replicate)
{
  Zc=numeric(C)
  Z0=sample(C,C*ec,replace=FALSE)
  for (j in 1:(C*ec)) Zc[Z0[j]]=1
  for (j in 1:N) Z[j]=Zc[cluster[j]]
  Y=Z*Y1+(1-Z)*Y0
  Ysum=Zc*Ysum1+(1-Zc)*Ysum0
  
  #Individual level 
  r=lm(Y~1+Z)
  tauI[i]=r$coefficients[2]
  X=cbind(1+0*Z,Z)
  sandwich=matrix(nrow=2,ncol=2,0)
  dat=data.frame(Y,Z,cluster)
  #fit.gee=gee(Y~1+Z,id=cluster,corstr="independence",data=dat)
  for (c in 1:C)
  {
    e=r$residuals[cluster==c]
    Xc=cbind(1+0*Z[cluster==c],Z[cluster==c])
    sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
  }
  tauIev[i]=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]
  if ((tauI[i]-sqrt(tauIev[i])*qnorm(0.975)<causal)&(causal<tauI[i]+sqrt(tauIev[i])*qnorm(0.975))) tauIp=tauIp+1
  
  #Individual level regression
  r=lm(Y~1+Z+x+Z*x)
  tauIR[i]=r$coefficient[2] 
  X=cbind(1+0*Z,Z,x,Z*x)
  sandwich=matrix(nrow=4,ncol=4,0)
  for (c in 1:C)
  {
    e=r$residuals[cluster==c]
    Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],x[cluster==c],Z[cluster==c]*x[cluster==c])
    sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
  }
  tauIRev[i]=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]
  if ((tauIR[i]-sqrt(tauIRev[i])*qnorm(0.975)<causal)&(causal<tauIR[i]+sqrt(tauIRev[i])*qnorm(0.975))) tauIRp=tauIRp+1
  
  xm=xsum[cluster]/nsize[cluster]
  #Individual level regression
  r=lm(Y~1+Z+xm+Z*xm)
  tauIR2[i]=r$coefficient[2] 
  X=cbind(1+0*Z,Z,xm,Z*xm)
  sandwich=matrix(nrow=4,ncol=4,0)
  for (c in 1:C)
  {
    e=r$residuals[cluster==c]
    Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],xm[cluster==c],Z[cluster==c]*xm[cluster==c])
    sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
  }
  tauIR2ev[i]=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]
  if ((tauIR2[i]-sqrt(tauIR2ev[i])*qnorm(0.975)<causal)&(causal<tauIR2[i]+sqrt(tauIR2ev[i])*qnorm(0.975))) tauIR2p=tauIR2p+1

  #ancova
  r=lm(Y~1+Z+x)
  tauOLS[i]=r$coefficient[2] 
  X=cbind(1+0*Z,Z,x)
  sandwich=matrix(nrow=3,ncol=3,0)
  for (c in 1:C)
  {
    e=r$residuals[cluster==c]
    Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],x[cluster==c])
    sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
  }
  tauOLSev[i]=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]
  if ((tauOLS[i]-sqrt(tauOLSev[i])*qnorm(0.975)<causal)&(causal<tauOLS[i]+sqrt(tauOLSev[i])*qnorm(0.975))) tauOLSp=tauOLSp+1
  
  #Cluster total regression
  r=lm(Ysum/(N/C)~1+Zc)
  tauT[i]=r$coefficients[2]
  tauTev[i]=hccm(r,type="hc0")[2,2]          
  if ((tauT[i]-sqrt(tauTev[i])*qnorm(0.975)<causal)&(causal<tauT[i]+sqrt(tauTev[i])*qnorm(0.975))) tauTp=tauTp+1
  
  #Cluster total regression with cluster size
  r=lm(Ysum/(N/C)~1+Zc+nc+Zc*nc)        
  tauTR[i]=r$coefficients[2]
  tauTRev[i]=hccm(r,type="hc0")[2,2]          
  if ((tauTR[i]-sqrt(tauTRev[i])*qnorm(0.975)<causal)&(causal<tauTR[i]+sqrt(tauTRev[i])*qnorm(0.975))) tauTRp=tauTRp+1
  
  #Cluster total regression with cluster size and additional covariate
  r=lm(Ysum/(N/C)~1+Zc+nc+xsum+Zc*nc+Zc*xsum)   
  tauTR2[i]=r$coefficients[2]
  tauTR2ev[i]=hccm(r,type="hc0")[2,2] 
  if ((tauTR2[i]-sqrt(tauTR2ev[i])*qnorm(0.975)<causal)&(causal<tauTR2[i]+sqrt(tauTR2ev[i])*qnorm(0.975))) tauTR2p=tauTR2p+1
  
  #Average
  r=lm(Ysum/nsize~1+Zc)        
  tauA[i]=r$coefficients[2]
  tauAev[i]=hccm(r,type="hc0")[2,2]          
  if ((tauA[i]-sqrt(tauAev[i])*qnorm(0.975)<causal)&(causal<tauA[i]+sqrt(tauAev[i])*qnorm(0.975))) tauAp=tauAp+1
  
  #Average with additional covariate
  xmeanc=xsum/nsize-mean(xsum/nsize)
  r=lm(Ysum/nsize~1+Zc+xmeanc+Zc*xmeanc)   
  tauAR[i]=r$coefficients[2]
  tauARev[i]=hccm(r,type="hc0")[2,2] 
  if ((tauAR[i]-sqrt(tauARev[i])*qnorm(0.975)<causal)&(causal<tauAR[i]+sqrt(tauARev[i])*qnorm(0.975))) tauARp=tauARp+1
  
}
#Arrange the data into dataframe
datI=rbind(mean(tauI)-causal,sd(tauI),mean(sqrt(tauIev)),
           sqrt(mean((tauI-causal)^2)),tauIp)

datIR=rbind(mean(tauIR)-causal,sd(tauIR),mean(sqrt(tauIRev)),
            sqrt(mean((tauIR-causal)^2)),tauIRp)

datIR2=rbind(mean(tauIR2)-causal,sd(tauIR2),mean(sqrt(tauIR2ev)),
            sqrt(mean((tauIR2-causal)^2)),tauIR2p)

datOLS=rbind(mean(tauOLS)-causal,sd(tauOLS),mean(sqrt(tauOLSev)),
             sqrt(mean((tauOLS-causal)^2)),tauOLSp)

datT=rbind(mean(tauT)-causal,sd(tauT),mean(sqrt(tauTev)),
           sqrt(mean((tauT-causal)^2)),tauTp)

datTR=rbind(mean(tauTR)-causal,sd(tauTR),mean(sqrt(tauTRev)),
            sqrt(mean((tauTR-causal)^2)),tauTRp)

datTR2=rbind(mean(tauTR2)-causal,sd(tauTR2),mean(sqrt(tauTR2ev)),
             sqrt(mean((tauTR2-causal)^2)),tauTR2p)


datA=rbind(mean(tauA)-causal,sd(tauA),mean(sqrt(tauAev)),
           sqrt(mean((tauA-causal)^2)),tauAp)

datAR=rbind(mean(tauAR)-causal,sd(tauAR),mean(sqrt(tauARev)),
            sqrt(mean((tauAR-causal)^2)),tauARp)

boxdata = as.data.frame(cbind(tauI,tauIR,tauIR2,tauOLS,tauT,tauTR,tauTR2,tauA,tauAR))
boxdata <- boxdata %>% pivot_longer(tauI:tauAR) 

boxdata$name <- factor(boxdata$name, 
                       levels = c("tauI","tauIR",
                                  "tauIR2","tauOLS",
                                  "tauT","tauTR","tauTR2","tauA",
                                  "tauAR"))
#Boxplot
box <- ggplot(boxdata) + geom_boxplot(aes(x = name, y = value)) +
  geom_abline(slope = 0, intercept = causal, lty = 2) +
  labs(x = "", y = "Boxplot") + 
  scale_x_discrete(labels = c(expression(hat(tau)[I]),
                              expression(hat(tau)[I]^{adj}),
                              expression(hat(tau)[I]^{adj}~(bar(x)[i.])),
                              expression(hat(tau)[I]^{ancova}),
                              expression(hat(tau)[T]),
                              expression(hat(tau)[T]^{adj}~(n[i])),
                              expression({hat(tau)[T]^{adj}}(n[i],tilde(x)[i.])),
                              expression(hat(tau)[A]),
                              expression(hat(tau)[A]^{adj}~(bar(x)[i.])))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, size = 13))

#Build dataframe for table 
mul=100
dat=round(mul*cbind(datI,datIR,datIR2,datOLS,datT,datTR,datTR2,datA,datAR),2)
dat[5,]=dat[5,]/(mul*replicate)
dat
#Plot the coverage rates
dat = as.data.frame(dat) 
bar_data = as.data.frame(t(dat[5,]))
bar_data = bar_data %>% mutate(row_name = row.names(bar_data),
                               baseline = 0.95)

rate <- ggplot(data = bar_data, aes(x = row_name, y = tauIp)) + 
  geom_bar(stat = "identity", fill = "white", color = "black") + ylim(0, 1) + 
  labs(x = "", y = "Coverage rate") +
  geom_abline(slope = 0, intercept = 0.95, lty = 2) +
  scale_x_discrete(labels = c(expression(hat(tau)[I]),
                              expression(hat(tau)[I]^{adj}),
                              expression(hat(tau)[I]^{adj}~(bar(x)[i.])),
                              expression(hat(tau)[I]^{ancova}),
                              expression(hat(tau)[T]),
                              expression(hat(tau)[T]^{adj}~(n[i])),
                              expression({hat(tau)[T]^{adj}}(n[i],tilde(x)[i.])),
                              expression(hat(tau)[A]),
                              expression(hat(tau)[A]^{adj}~(bar(x)[i.])))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, size = 13))
#Combine two plots
p <- ggarrange(box, rate, nrow = 1, ncol = 2)  + theme(plot.margin = unit(c(0, 0, -1.6, 0), "cm"))
ggsave("Control22(One_page).pdf", plot = p, device = "pdf", width = 12, height = 1.8)


write.csv(dat,file="7_2.csv",row.names=F)