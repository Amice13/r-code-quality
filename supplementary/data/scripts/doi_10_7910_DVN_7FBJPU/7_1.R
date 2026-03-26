################Application for cluster-randomized experiments##################
###########################Section 7.1, Data analysis###########################

library(car)
library(readstata13)
library(tidyverse)
library(tidyr)
library(ggpubr)

#Read data
dat=read.dta13("BD-SAN-FINAL.dta")
attach(dat)

#Identify the non-qualified household
notqualified = is.na(r4_hyg_access)|(dat$eligible=="Not eligible")|(treat_cat_1=="LPP Only")|(treat_cat_1=="Supply Only")|(vid==301)
N = length(notqualified) - sum(notqualified)
#Treatment indicator Z, outcome Y, and covariate x
Z = numeric(N)
Y = numeric(N)
x = numeric(N)
cluster = numeric(N)
N = 0
for (i in 1:18254)
{
  if (notqualified[i] == FALSE)
  {
    N = N + 1
    if ((treat_cat_1[i] == "LPP+Subsidy+Supply")|(treat_cat_1[i] == "LPP+Subsidy")) Z[N] = 1
    else Z[N] = 0 
    if (vid[i] == 301) Z[N] = 0
    Y[N] = r4_hyg_access[i]
    x[N] = mean(bl_c_hyg_access[cid == cid[i]])  
    cluster[N] = vid[i]
  }
}
cluster = as.factor(cluster)
cluster = as.integer(cluster)
#Scale the Y into 0/1 indicator
Y = Y - 1
#Center the covariate
x = x - mean(x)
C = max(cluster)
#Cluster treatment indicator and cluster size
Zc = numeric(C)
nsize = numeric(C)
for (i in 1:N)
{  
  Zc[cluster[i]] = Z[i]
  nsize[cluster[i]] = nsize[cluster[i]] + 1
}
#Propensity scores for clusters
ec = sum(Zc) / C
#Prepare for cluster-total regressions
Ysum = numeric(C)
xsum = numeric(C)
for (c in 1:C)
{
  Ysum[c] = sum(Y[cluster==c])
  xsum[c] = sum(x[cluster==c])
}
nc = nsize - N/C

#Individual level 
r=lm(Y~1+Z)
tauI=r$coefficients[2]
X=cbind(1+0*Z,Z)
sandwich=matrix(nrow=2,ncol=2,0)
for (c in 1:C)
{
  e=r$residuals[cluster==c]
  Xc=cbind(1+0*Z[cluster==c],Z[cluster==c])
  sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
}
tauIev=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]

#Individual level regression with covariate x_{ij}
r=lm(Y~1+Z+x+Z*x)
tauIR=r$coefficient[2] 
X=cbind(1+0*Z,Z,x,Z*x)
sandwich=matrix(nrow=4,ncol=4,0)
for (c in 1:C)
{
  e=r$residuals[cluster==c]
  Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],x[cluster==c],Z[cluster==c]*x[cluster==c])
  sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
}
tauIRev=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]



xm=xsum[cluster]/nsize[cluster]
#Individual level regression with covariate \bar{x}_{i\cdot}
r=lm(Y~1+Z+xm+Z*xm)
tauIR2=r$coefficient[2] 
X=cbind(1+0*Z,Z,xm,Z*xm)
sandwich=matrix(nrow=4,ncol=4,0)
for (c in 1:C)
{
  e=r$residuals[cluster==c]
  Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],xm[cluster==c],Z[cluster==c]*xm[cluster==c])
  sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
}
tauIR2ev=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]


#ancova
r=lm(Y~1+Z+x)
tauOLS=r$coefficient[2] 
X=cbind(1+0*Z,Z,x)
sandwich=matrix(nrow=3,ncol=3,0)
for (c in 1:C)
{
  e=r$residuals[cluster==c]
  Xc=cbind(1+0*Z[cluster==c],Z[cluster==c],x[cluster==c])
  sandwich=sandwich+t(Xc)%*%as.matrix(e)%*%t(as.matrix(e))%*%Xc    #as.matrix(e) is column vector
}
tauOLSev=as.matrix(solve(t(X)%*%X)%*%sandwich%*%solve(t(X)%*%X))[2,2]


#Cluster-total regression
r=lm(Ysum/(N/C)~1+Zc)
tauT=r$coefficients[2]
tauTev=hccm(r,type="hc0")[2,2]

#Cluster-total regression regression with cluster size
r=lm(Ysum/(N/C)~1+Zc+nc+Zc*nc)        
tauTR=r$coefficients[2]
tauTRev=hccm(r,type="hc0")[2,2]          

#Cluster-total regression with cluster size and additional covariate
r=lm(Ysum/(N/C)~1+Zc+nc+xsum+Zc*nc+Zc*xsum)   
tauTR2=r$coefficients[2]
tauTR2ev=hccm(r,type="hc0")[2,2]

#Average
r=lm(Ysum/nsize~1+Zc)        
tauA=r$coefficients[2]
tauAev=hccm(r,type="hc0")[2,2]

#Average with additional covariate
xmeanc=xsum/nsize-mean(xsum/nsize)
r=lm(Ysum/nsize~1+Zc+xmeanc+Zc*xmeanc)   
tauAR=r$coefficients[2]
tauARev=hccm(r,type="hc0")[2,2] 

#Arrange the data into dataframe
datI=cbind(tauI,sqrt(tauIev),
           tauI-sqrt(tauIev)*qnorm(0.975),tauI+sqrt(tauIev)*qnorm(0.975))

datIR=cbind(tauIR,sqrt(tauIRev),
            tauIR-sqrt(tauIRev)*qnorm(0.975),tauIR+sqrt(tauIRev)*qnorm(0.975))

datIR2=cbind(tauIR2,sqrt(tauIR2ev),
             tauIR2-sqrt(tauIR2ev)*qnorm(0.975),tauIR2+sqrt(tauIR2ev)*qnorm(0.975))

datOLS=cbind(tauOLS,sqrt(tauOLSev),
             tauOLS-sqrt(tauOLSev)*qnorm(0.975),tauOLS+sqrt(tauOLSev)*qnorm(0.975))

datT=cbind(tauT,sqrt(tauTev),
           tauT-sqrt(tauTev)*qnorm(0.975),tauT+sqrt(tauTev)*qnorm(0.975))

datTR=cbind(tauTR,sqrt(tauTRev),
            tauTR-sqrt(tauTRev)*qnorm(0.975),tauTR+sqrt(tauTRev)*qnorm(0.975))

datTR2=cbind(tauTR2,sqrt(tauTR2ev),
             tauTR2-sqrt(tauTR2ev)*qnorm(0.975),tauTR2+sqrt(tauTR2ev)*qnorm(0.975))

datA=cbind(tauA,sqrt(tauAev),
           tauA-sqrt(tauAev)*qnorm(0.975),tauA+sqrt(tauAev)*qnorm(0.975))

datAR=cbind(tauAR,sqrt(tauARev),
            tauAR-sqrt(tauARev)*qnorm(0.975),tauAR+sqrt(tauARev)*qnorm(0.975))

dat1=round(rbind(datI,datIR,datIR2,datOLS),3)
dat2=round(rbind(datT,datTR,datTR2,datA,datAR),3)

dat_table <- as.data.frame(t(rbind(dat1, dat2)))
write.csv(dat_table,file="7_1.csv",row.names=F)

row.names(dat_table) <- c("est", "se", "lower_CI", "upper_CI")
names(dat_table) <- c("tauI","tauIR", "tauIR2","tauOLS",
                          "tauT","tauTR","tauTR2","tauA", "tauAR")

dat_table <- as.data.frame(t(dat_table))

dat_table <- rownames_to_column(dat_table, var = "row_name") 

dat_table$row_name <- factor(dat_table$row_name, 
                       levels = c("tauI","tauIR","tauIR2","tauOLS",
                                  "tauT","tauTR","tauTR2","tauA", "tauAR"))
#Plot for Est and CI
app <- ggplot(dat_table, aes(x = row_name, y = est)) + 
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.1) + geom_point() +
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
  theme(axis.text.x = element_text(angle = 60, size = 13)) + 
  labs(x = "", y = "Est and CI")
#Barplot of standard error  
box <- ggplot(dat_table, aes(x = row_name, y = se)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
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
  theme(axis.text.x = element_text(angle = 60, size = 13)) + 
  labs(x = "", y = "se")
#Combine two plots
p <- ggarrange(app, box, nrow = 1, ncol = 2) + theme(plot.margin = unit(c(0, 0, -1.6, 0), "cm")) 
ggsave("app.pdf", plot = p, device = "pdf", width = 12, height = 1.8)
