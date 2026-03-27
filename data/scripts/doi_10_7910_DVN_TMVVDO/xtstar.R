library(rjags)
library(runjags)

load("gerber_ds.RData")

y<-rh[,10:16]
y<-t(apply(y,1,as.numeric))
y<-y>3
y<-t(apply(y,1,as.numeric))


NBETA=15
NALPHA=length(unique(rh$oblast))
xenmod<-list(N=nrow(rh), NBETA=NBETA, NALPHA=NALPHA, oblast=rh$oblast,
                            b0=rep(0,NBETA), B0=diag(1,NBETA), y=y, 
                             male=rh$male, 
             young=as.numeric(rh$age_cat=="1"),
             old=as.numeric(rh$age_cat=="3"),
             lowed=as.numeric(rh$education=="1"),
             highed=as.numeric(rh$education=="3"),
             inc=rh$income,
             rural=as.numeric(rh$rural=="1"),
             threat=rh$threat,
             northodox=rh$northodox,
                             putin=rh$putin,
                             nrussian=rh$nRussian,
             unemployed=rh$unemployed, survey=as.factor(rh$survey), iota=rep(1,3), phi=rep(1,3))

xenmodel<-run.jags(model="xtstar.txt",
                     monitor=c("beta", "alpha", "mu.b", "mu.a", "ort", "rus", "une", "i1", "p1"), data=xenmod, n.chains=8, 
                   adapt=5000, burnin=7500, 
                   sample=500, thin=150, summarise=F,plots=F, modules=c("glm", "lecuyer"))
save(xenmodel, file="xtstar.RData")

##############Analysis
cds<-as.mcmc.list(xenmodel)
sum(gelman.diag(cds,multivariate = F)[[1]][,1]>1.1)

###combine chains
cds<-as.mcmc(xenmodel)


#######Graphics
library(ggplot2)

m50<-summary(cds)[[2]][,3]
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(1,seq(15,140,14),141,seq(155,210,14))],1, hpd[c(1,seq(15,140,14),141,seq(155,210,14)),1], 2,hpd[c(1,seq(15,140,14),141,seq(155,210,14)),2], 3,
           m50[c(2,seq(16,140,14),142,seq(156,210,14))],1, hpd[c(2,seq(16,140,14),142,seq(156,210,14)),1], 2,hpd[c(2,seq(16,140,14),142,seq(156,210,14)),2], 3,
           m50[c(7,seq(21,140,14),147,seq(161,210,14))],1, hpd[c(7,seq(21,140,14),147,seq(161,210,14)),1], 2,hpd[c(7,seq(21,140,14),147,seq(161,210,14)),2], 3,
           m50[c(8,seq(22,140,14),148,seq(162,210,14))],1, hpd[c(8,seq(22,140,14),148,seq(162,210,14)),1], 2,hpd[c(8,seq(22,140,14),148,seq(162,210,14)),2], 3,
           m50[c(13,seq(27,140,14),153,seq(167,210,14))],1, hpd[c(13,seq(27,140,14),153,seq(167,210,14)),1], 2,hpd[c(13,seq(27,140,14),153,seq(167,210,14)),2], 3,
           m50[c(14,seq(28,140,14),154,seq(168,210,14))],1, hpd[c(14,seq(28,140,14),154,seq(168,210,14)),1], 2,hpd[c(14,seq(28,140,14),154,seq(168,210,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural",
                 "Not Orthodox", "Not Russian", "Economic fear", "Very not confident in Putin",
                 "Very confident in Putin")


stargazer(hpd, digits=2)

######


hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(3,seq(17,140,14),143,seq(157,210,14))],1, hpd[c(3,seq(17,140,14),143,seq(157,210,14)),1], 2,hpd[c(3,seq(17,140,14),143,seq(157,210,14)),2], 3,
           m50[c(4,seq(18,140,14),144,seq(158,210,14))],1, hpd[c(4,seq(18,140,14),144,seq(158,210,14)),1], 2,hpd[c(4,seq(18,140,14),144,seq(158,210,14)),2], 3,
           m50[c(11,seq(25,140,14),151,seq(165,210,14))],1, hpd[c(11,seq(25,140,14),151,seq(165,210,14)),1], 2,hpd[c(11,seq(25,140,14),151,seq(165,210,14)),2], 3,
           m50[c(12,seq(26,140,14),152,seq(166,210,14))],1, hpd[c(12,seq(26,140,14),152,seq(166,210,14)),1], 2,hpd[c(12,seq(26,140,14),152,seq(166,210,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Not Orthodox","Not Russian", "Economic fear", "Very not confident in Putin",
                 "Very confident in Putin")
stargazer(hpd, digits=2)

######


hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(5,seq(19,140,14),145,seq(159,210,14))],1, hpd[c(5,seq(19,140,14),145,seq(159,210,14)),1], 2,hpd[c(5,seq(19,140,14),145,seq(159,210,14)),2], 3,
           m50[c(6,seq(20,140,14),146,seq(160,210,14))],1, hpd[c(6,seq(20,140,14),146,seq(160,210,14)),1], 2,hpd[c(6,seq(20,140,14),146,seq(160,210,14)),2], 3,
           m50[c(9,seq(23,140,14),149,seq(163,210,14))],1, hpd[c(9,seq(23,140,14),149,seq(163,210,14)),1], 2,hpd[c(9,seq(23,140,14),149,seq(163,210,14)),2], 3,
           m50[c(10,seq(24,140,14),150,seq(164,210,14))],1, hpd[c(10,seq(24,140,14),150,seq(164,210,14)),1], 2,hpd[c(10,seq(24,140,14),150,seq(164,210,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural",
                 "Not Orthodox", "Not Russian", "Economic fear", "Very not confident in Putin",
                 "Very confident in Putin")


stargazer(hpd, digits=2)
