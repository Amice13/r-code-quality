################Simulation for cluster-randomized experiments###################
############Section 6.1, Comparing estimators and standard errors############### 
library(tidyverse)
library(gee)
library(car)
library(geepack)
library(ggpubr)

set.seed(1)

#Monte Carlo replication times 
replicate = 1000              

#Generating clusters
N0 = 3000
C = 160
nsize = round(runif(C,N0 / C * 0.6, N0 / C * 1.4))
N = sum(nsize)



#Propensity scores for clusters
ec = 0.3                                             
              
#Define cluster index and potential outcome 
cluster = numeric(N)      
c = 1
for (i in 1:N)
{
  if (i > sum(nsize[1:c])) c = c + 1 
  cluster[i] = c
}

#x : individual level covariate
#Y1 : treatment potential outcome
#Y0 : control potential outcome
x = (cluster / C) + runif(N, -1, 1)    
# Center x at its mean as required in the paper
x = x - mean(x)                                                                  
Y1 = x^3 + 2 * nsize[cluster] / (N / C)+rnorm(N, 0, 1)                                   
Y0 = (cluster / C) + x^2 + rnorm(N, 0, 1)                                              

#Calculate sum to prepare for cluster-total regressions
Ysum1 = numeric(C)
Ysum0 = numeric(C)
xsum = numeric(C)

for (c in 1:C)
{
  Ysum1[c] = sum(Y1[cluster == c])
  Ysum0[c] = sum(Y0[cluster == c])
  xsum[c] = sum(x[cluster == c])
}
#Calculate centered cluster size to prepare for cluster total regressions
nc = nsize - N / C

#Calculate causal effect
causal = mean(Y1-Y0)

#Set variable to store coverage probability for each each estimators and each regression variance estimation
#tauI is individual-level regression, Difference-in-means
#tauIR is individual-level regression adjustment with covariates x_{ij}
#tauIR2 is individual-level regression adjustment with average covariates \bar{x}_{i\cdot}
#tauT is cluster-total regression
#tauTR is cluter-total regression adjustment with cluster size n_{ij} as covariates
#tauTR2 is cluster-total regression adjustment with cluster size n_{ij} and sum of covariates \tilde{x}_{i\cdot}
#tauA is cluster-average regression
#tauA is cluster-average regression with average covariates \bar{x}_{i\cdot}
#tauW is cluster-average regression with weight n_i/N
#tauWR is cluster-average regression with weight n_i/N and average covariates \bar{x}_{i\cdot}
#tauOLS is traditional ANCOVA regression with covariates x_{ij}
#For variance estimation, ols and hw is OLS variance estimator and Huber-White variance estimator
#The first line without additional explanation is variance estimator with theoretical guarentee
#Which is Liang-Zeger for individual-level regression and ols, Huber-White for cluster-total regression
#cluster-average regression, and weighted cluster-average regression
tauIp=0;   tauIRp=0;   tauIR2p=0;   tauTp=0;   tauTRp=0;   tauTR2p=0;   tauAp=0;   tauARp=0;   tauWp=0;   tauWRp=0;   tauOLSp=0
tauIpols=0;tauIRpols=0;tauIR2pols=0;tauTpols=0;tauTRpols=0;tauTR2pols=0;tauApols=0;tauARpols=0;tauWpols=0;tauWRpols=0;tauOLSpols=0
tauIphw=0; tauIRphw=0; tauIR2phw=0;                                                                                   tauOLSphw=0

#Store the estimators for each Monte Carlo
tauI=numeric(replicate)
tauIR=numeric(replicate)
tauIR2=numeric(replicate)
tauT=numeric(replicate)
tauTR=numeric(replicate)
tauTR2=numeric(replicate)
tauA=numeric(replicate)
tauAR=numeric(replicate)
tauW=numeric(replicate)
tauWR=numeric(replicate)
tauOLS=numeric(replicate)

#Store the variance estimators for each Monte Carlo
tauIev=numeric(replicate);    tauIevols=numeric(replicate);   tauIevhw=numeric(replicate)
tauIRev=numeric(replicate);   tauIRevols=numeric(replicate);  tauIRevhw=numeric(replicate)
tauIR2ev=numeric(replicate);  tauIR2evols=numeric(replicate); tauIR2evhw=numeric(replicate)
tauTev=numeric(replicate);    tauTevols=numeric(replicate)
tauTRev=numeric(replicate);   tauTRevols=numeric(replicate)
tauTR2ev=numeric(replicate);  tauTR2evols=numeric(replicate)
tauAev=numeric(replicate);    tauAevols=numeric(replicate)
tauARev=numeric(replicate);   tauARevols=numeric(replicate)
tauWev=numeric(replicate);    tauWevols=numeric(replicate)
tauWRev=numeric(replicate);   tauWRevols=numeric(replicate)
tauOLSev=numeric(replicate);  tauOLSevols=numeric(replicate); tauOLSevhw=numeric(replicate)

Z=numeric(N)
for (i in 1:replicate)
{
  #Generate treatment assignment and observed data
  Zc=numeric(C)
  Z0=sample(C,C*ec,replace=FALSE)
  for (j in 1:(C*ec)) Zc[Z0[j]]=1
  for (j in 1:N) Z[j]=Zc[cluster[j]]
  Y=Z*Y1+(1-Z)*Y0
  Ysum=Zc*Ysum1+(1-Zc)*Ysum0
  
  #Individual level 
  dat=data.frame(Y,Z,cluster)
  r=geeglm(Y~1+Z,id=cluster,corstr="independence",data=dat)
  tauI[i]=r$coefficients[2]
  tauIev[i]=(summary(r)$coefficients[2,2])^2
  r=lm(Y~1+Z)
  tauIevols[i]=summary(r)$coefficients[2,2]^2  
  tauIevhw[i]=hccm(r,type="hc0")[2,2]  
  if ((tauI[i]-sqrt(tauIev[i])*qnorm(0.975)<causal)&(causal<tauI[i]+sqrt(tauIev[i])*qnorm(0.975))) tauIp=tauIp+1
  if ((tauI[i]-sqrt(tauIevols[i])*qnorm(0.975)<causal)&(causal<tauI[i]+sqrt(tauIevols[i])*qnorm(0.975))) tauIpols=tauIpols+1
  if ((tauI[i]-sqrt(tauIevhw[i])*qnorm(0.975)<causal)&(causal<tauI[i]+sqrt(tauIevhw[i])*qnorm(0.975))) tauIphw=tauIphw+1
  
  #Individual level regression with covariate x_{ij}
  dat=data.frame(Y,Z,x,cluster)
  r=geeglm(Y~1+Z+x+Z*x,id=cluster,corstr="independence",data=dat)
  tauIR[i]=r$coefficients[2]
  tauIRev[i]=(summary(r)$coefficients[2,2])^2
  r=lm(Y~1+Z+x+Z*x)
  tauIRevols[i]=summary(r)$coefficients[2,2]^2  
  tauIRevhw[i]=hccm(r,type="hc0")[2,2]  
  if ((tauIR[i]-sqrt(tauIRev[i])*qnorm(0.975)<causal)&(causal<tauIR[i]+sqrt(tauIRev[i])*qnorm(0.975))) tauIRp=tauIRp+1
  if ((tauIR[i]-sqrt(tauIRevols[i])*qnorm(0.975)<causal)&(causal<tauIR[i]+sqrt(tauIRevols[i])*qnorm(0.975))) tauIRpols=tauIRpols+1
  if ((tauIR[i]-sqrt(tauIRevhw[i])*qnorm(0.975)<causal)&(causal<tauIR[i]+sqrt(tauIRevhw[i])*qnorm(0.975))) tauIRphw=tauIRphw+1
  
  xm=xsum[cluster]/nsize[cluster]
  #Individual level regression with covariate \bar{x}_{i\cdot}
  dat=data.frame(Y,Z,xm,cluster)
  r=geeglm(Y~1+Z+xm+Z*xm,id=cluster,corstr="independence",data=dat)
  tauIR2[i]=r$coefficients[2]
  tauIR2ev[i]=summary(r)$coefficients[2,2]^2
  r=lm(Y~1+Z+xm+Z*xm)
  tauIR2evols[i]=summary(r)$coefficients[2,2]^2  
  tauIR2evhw[i]=hccm(r,type="hc0")[2,2]  
  if ((tauIR2[i]-sqrt(tauIR2ev[i])*qnorm(0.975)<causal)&(causal<tauIR2[i]+sqrt(tauIR2ev[i])*qnorm(0.975))) tauIR2p=tauIR2p+1
  if ((tauIR2[i]-sqrt(tauIR2evols[i])*qnorm(0.975)<causal)&(causal<tauIR2[i]+sqrt(tauIR2evols[i])*qnorm(0.975))) tauIR2pols=tauIR2pols+1
  if ((tauIR2[i]-sqrt(tauIR2evhw[i])*qnorm(0.975)<causal)&(causal<tauIR2[i]+sqrt(tauIR2evhw[i])*qnorm(0.975))) tauIR2phw=tauIR2phw+1
  
  #ancova
  dat=data.frame(Y,Z,x,cluster)
  r=geeglm(Y~1+Z+x,id=cluster,corstr="independence",data=dat)
  tauOLS[i]=r$coefficients[2]
  tauOLSev[i]=summary(r)$coefficients[2,2]^2
  r=lm(Y~1+Z+x)
  tauOLSevols[i]=summary(r)$coefficients[2,2]^2  
  tauOLSevhw[i]=hccm(r,type="hc0")[2,2]    
  
  if ((tauOLS[i]-sqrt(tauOLSev[i])*qnorm(0.975)<causal)&(causal<tauOLS[i]+sqrt(tauOLSev[i])*qnorm(0.975))) tauOLSp=tauOLSp+1
  if ((tauOLS[i]-sqrt(tauOLSevols[i])*qnorm(0.975)<causal)&(causal<tauOLS[i]+sqrt(tauOLSevols[i])*qnorm(0.975))) tauOLSpols=tauOLSpols+1
  if ((tauOLS[i]-sqrt(tauOLSevhw[i])*qnorm(0.975)<causal)&(causal<tauOLS[i]+sqrt(tauOLSevhw[i])*qnorm(0.975))) tauOLSphw=tauOLSphw+1
 
  #Cluster total regression
  r=lm(Ysum/(N/C)~1+Zc)
  tauT[i]=r$coefficients[2]
  tauTev[i]=hccm(r,type="hc0")[2,2]    
  tauTevols[i]=summary(r)$coefficients[2,2]^2  
  if ((tauT[i]-sqrt(tauTev[i])*qnorm(0.975)<causal)&(causal<tauT[i]+sqrt(tauTev[i])*qnorm(0.975))) tauTp=tauTp+1
  if ((tauT[i]-sqrt(tauTevols[i])*qnorm(0.975)<causal)&(causal<tauT[i]+sqrt(tauTevols[i])*qnorm(0.975))) tauTpols=tauTpols+1
  
  #Cluster total regression with cluster size
  r=lm(Ysum/(N/C)~1+Zc+nc+Zc*nc)        
  tauTR[i]=r$coefficients[2]
  tauTRev[i]=hccm(r,type="hc0")[2,2]          
  tauTRevols[i]=summary(r)$coefficients[2,2]^2  
  if ((tauTR[i]-sqrt(tauTRev[i])*qnorm(0.975)<causal)&(causal<tauTR[i]+sqrt(tauTRev[i])*qnorm(0.975))) tauTRp=tauTRp+1
  if ((tauTR[i]-sqrt(tauTRevols[i])*qnorm(0.975)<causal)&(causal<tauTR[i]+sqrt(tauTRevols[i])*qnorm(0.975))) tauTRpols=tauTRpols+1
  
  #Cluster total regression with cluster size and additional covariate
  r=lm(Ysum/(N/C)~1+Zc+nc+xsum+Zc*nc+Zc*xsum)   
  tauTR2[i]=r$coefficients[2]
  tauTR2ev[i]=hccm(r,type="hc0")[2,2]
  tauTR2evols[i]=summary(r)$coefficients[2,2]^2  
  if ((tauTR2[i]-sqrt(tauTR2ev[i])*qnorm(0.975)<causal)&(causal<tauTR2[i]+sqrt(tauTR2ev[i])*qnorm(0.975))) tauTR2p=tauTR2p+1
  if ((tauTR2[i]-sqrt(tauTR2evols[i])*qnorm(0.975)<causal)&(causal<tauTR2[i]+sqrt(tauTR2evols[i])*qnorm(0.975))) tauTR2pols=tauTR2pols+1
  
  #Average
  r=lm(Ysum/nsize~1+Zc)        
  tauA[i]=r$coefficients[2]
  tauAev[i]=hccm(r,type="hc0")[2,2]  
  tauAevols[i]=summary(r)$coefficients[2,2]^2
  if ((tauA[i]-sqrt(tauAev[i])*qnorm(0.975)<causal)&(causal<tauA[i]+sqrt(tauAev[i])*qnorm(0.975))) tauAp=tauAp+1
  if ((tauA[i]-sqrt(tauAevols[i])*qnorm(0.975)<causal)&(causal<tauA[i]+sqrt(tauAevols[i])*qnorm(0.975))) tauApols=tauApols+1
  
  #Average with additional covariate
  xmeanc=xsum/nsize-mean(xsum/nsize)
  r=lm(Ysum/nsize~1+Zc+xmeanc+Zc*xmeanc)   
  tauAR[i]=r$coefficients[2]
  tauARev[i]=hccm(r,type="hc0")[2,2] 
  tauARevols[i]=summary(r)$coefficients[2,2]^2
  if ((tauAR[i]-sqrt(tauARev[i])*qnorm(0.975)<causal)&(causal<tauAR[i]+sqrt(tauARev[i])*qnorm(0.975))) tauARp=tauARp+1
  if ((tauAR[i]-sqrt(tauARevols[i])*qnorm(0.975)<causal)&(causal<tauAR[i]+sqrt(tauARevols[i])*qnorm(0.975))) tauARpols=tauARpols+1
  
  #Weight
  a=sqrt(nsize)
  b=sqrt(nsize*Zc)
  r=lm(Ysum/sqrt(nsize)~0+a+b)        
  tauW[i]=r$coefficients[2]
  tauWev[i]=hccm(r,type="hc0")[2,2]      
  tauWevols[i]=summary(r)$coefficients[2,2]^2
  if ((tauW[i]-sqrt(tauWev[i])*qnorm(0.975)<causal)&(causal<tauW[i]+sqrt(tauWev[i])*qnorm(0.975))) tauWp=tauWp+1
  if ((tauW[i]-sqrt(tauWevols[i])*qnorm(0.975)<causal)&(causal<tauW[i]+sqrt(tauWevols[i])*qnorm(0.975))) tauWpols=tauWpols+1
  
  #Weight with additional covariate
  xmeanc=xsum/nsize
  c=sqrt(nsize)*xmeanc
  d=c*Zc
  r=lm(Ysum/sqrt(nsize)~0+a+b+c+d)   
  tauWR[i]=r$coefficients[2]
  tauWRev[i]=hccm(r,type="hc0")[2,2] 
  tauWRevols[i]=summary(r)$coefficients[2,2]^2
  if ((tauWR[i]-sqrt(tauWRev[i])*qnorm(0.975)<causal)&(causal<tauWR[i]+sqrt(tauWRev[i])*qnorm(0.975))) tauWRp=tauWRp+1
  if ((tauWR[i]-sqrt(tauWRevols[i])*qnorm(0.975)<causal)&(causal<tauWR[i]+sqrt(tauWRevols[i])*qnorm(0.975))) tauWRpols=tauWRpols+1
  
}

#Arrange the data into dataframe
datI=rbind(mean(tauI)-causal,sd(tauI),mean(sqrt(tauIevols)),mean(sqrt(tauIevhw)),mean(sqrt(tauIev)),
           sqrt(mean((tauI-causal)^2)),tauIpols,tauIphw,tauIp)

datIR=rbind(mean(tauIR)-causal,sd(tauIR),mean(sqrt(tauIRevols)),mean(sqrt(tauIRevhw)),mean(sqrt(tauIRev)),
            sqrt(mean((tauIR-causal)^2)),tauIRpols,tauIRphw,tauIRp)

datIR2=rbind(mean(tauIR2)-causal,sd(tauIR2),mean(sqrt(tauIR2evols)),mean(sqrt(tauIR2evhw)),mean(sqrt(tauIR2ev)),
            sqrt(mean((tauIR2-causal)^2)),tauIR2pols,tauIR2phw,tauIR2p)

datOLS=rbind(mean(tauOLS)-causal,sd(tauOLS),mean(sqrt(tauOLSevols)),mean(sqrt(tauOLSevhw)),mean(sqrt(tauOLSev)),
             sqrt(mean((tauOLS-causal)^2)),tauOLSpols,tauOLSphw,tauOLSp)

datT=rbind(mean(tauT)-causal,sd(tauT),mean(sqrt(tauTevols)),mean(sqrt(tauTev)),
            sqrt(mean((tauT-causal)^2)),tauTpols,tauTp)

datTR=rbind(mean(tauTR)-causal,sd(tauTR),mean(sqrt(tauTRevols)),mean(sqrt(tauTRev)),
            sqrt(mean((tauTR-causal)^2)),tauTRpols,tauTRp)

datTR2=rbind(mean(tauTR2)-causal,sd(tauTR2),mean(sqrt(tauTR2evols)),mean(sqrt(tauTR2ev)),
            sqrt(mean((tauTR2-causal)^2)),tauTR2pols,tauTR2p)

datA=rbind(mean(tauA)-causal,sd(tauA),mean(sqrt(tauAevols)),mean(sqrt(tauAev)),
            sqrt(mean((tauA-causal)^2)),tauApols,tauAp)

datAR=rbind(mean(tauAR)-causal,sd(tauAR),mean(sqrt(tauARevols)),mean(sqrt(tauARev)),
           sqrt(mean((tauAR-causal)^2)),tauARpols,tauARp)

datW=rbind(mean(tauW)-causal,sd(tauW),mean(sqrt(tauWevols)),mean(sqrt(tauWev)),
           sqrt(mean((tauW-causal)^2)),tauWpols,tauWp)

datWR=rbind(mean(tauWR)-causal,sd(tauWR),mean(sqrt(tauWRevols)),mean(sqrt(tauWRev)),
           sqrt(mean((tauWR-causal)^2)),tauWRpols,tauWRp)

#Draw boxplot
boxdata = as.data.frame(cbind(tauI, tauIR, tauIR2, tauOLS,
                              tauT, tauTR, tauTR2, tauA, tauAR, tauW, tauWR))
boxdata <- boxdata %>% pivot_longer(tauI:tauWR) 

boxdata$name <- factor(boxdata$name, 
                       levels = c("tauI","tauIR",
                                  "tauIR2","tauOLS",
                                  "tauT","tauTR","tauTR2","tauA",
                                  "tauAR", "tauW", "tauWR"))

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
                              expression(hat(tau)[A]^{adj}~(bar(x)[i.])),
                              expression(hat(tau)[symbol(Aw)]),
                              expression(hat(tau)[symbol(Aw)]^{adj}~(bar(x)[i.])))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, size = 13))

mul=100
#First row of output
dat1=round(mul*cbind(datI,datIR,datIR2,datOLS),2)
#Second row of output
dat2=round(mul*cbind(datT,datTR,datTR2,datA,datAR,datW,datWR),2)

#Scale the dataframe

dat1[7,]=dat1[7,]/(mul*replicate)
dat1[8,]=dat1[8,]/(mul*replicate)
dat1[9,]=dat1[9,]/(mul*replicate)
dat2[6,]=dat2[6,]/(mul*replicate)
dat2[7,]=dat2[7,]/(mul*replicate)

#Plot the coverage rates
bar_data1 = as.data.frame(t(dat1[7:9,1:4]))
names(bar_data1) <- c("OLS", "HW", "LZ")

bar_data2 = as.data.frame(t(dat2[6:7,1:7]))
bar_data2$LZ <- rep(0, 7)
names(bar_data2) <- c("OLS", "HW", "LZ")

bar_data <- rbind(bar_data1, bar_data2)

bar_data <- rownames_to_column(bar_data, var = "row_name")

bar_data <- bar_data %>% pivot_longer(OLS:LZ) 

bar_data$name <- factor(bar_data$name, 
                       levels = c("OLS","HW", "LZ"))

bar_data$row_name <- factor(bar_data$row_name, 
                            levels = c("1", "2", "3", "4", "5", "6", 
                                       "7", "8", "9", "10", "11"))

rate <- ggplot(data = bar_data, aes(x = row_name, y = value, fill = name)) + 
  geom_bar(position="dodge", stat = "identity") + ylim(0, 1) + 
  geom_abline(slope = 0, intercept = 0.95, lty = 2) +
  scale_x_discrete(labels = c(expression(hat(tau)[I]),
                              expression(hat(tau)[I]^{adj}),
                              expression(hat(tau)[I]^{adj}~(bar(x)[i.])),
                              expression(hat(tau)[I]^{ancova}),
                              expression(hat(tau)[T]),
                              expression(hat(tau)[T]^{adj}~(n[i])),
                              expression({hat(tau)[T]^{adj}}(n[i],tilde(x)[i.])),
                              expression(hat(tau)[A]),
                              expression(hat(tau)[A]^{adj}~(bar(x)[i.])),
                              expression(hat(tau)[symbol(Aw)]),
                              expression(hat(tau)[symbol(Aw)]^{adj}~(bar(x)[i.])))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, size = 13)) + 
  labs(x = "", y = "Coverage rate") +
  scale_fill_discrete(name = expression(hat(se))) 

rate

#Combine two plots into "p"
p <- ggarrange(box, rate, nrow = 1, ncol = 2)
p <- p + theme(plot.margin = unit(c(0, 0, -1.6, 0), "cm"))
ggsave("Clusterplot(One_page).pdf", plot = p, device = "pdf", width = 12, height = 1.8)

#Output dataframe
write.csv(dat1,file="6_1_firstrow.csv",row.names=F)
write.csv(dat2,file="6_1_secondrow.csv",row.names=F)