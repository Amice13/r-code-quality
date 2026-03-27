library(rjags)
library(runjags)

load("gerber_ds.RData")

####dichotomize xenophobia outcomes
y<-rh[,10:16]
y<-t(apply(y,1,as.numeric))
y<-y>3
y<-t(apply(y,1,as.numeric))


NBETA=17
xenmod<-list(N=nrow(rh), NBETA=NBETA, 
                            b0=rep(0,NBETA), B0=diag(1,NBETA), y=y, 
                             male=rh$male, 
             young=as.numeric(rh$age_cat=="1"),
             old=as.numeric(rh$age_cat=="3"),
             lowed=as.numeric(rh$education=="1"),
             highed=as.numeric(rh$education=="3"),
             inc=rh$income,
             rural=as.numeric(rh$rural=="1"),
             moscow=rh$moscow,
             stp=rh$stp,
                          threat=rh$threat,
             northodox=rh$northodox,
                             putin=rh$putin,
                             nrussian=rh$nRussian,
             unemployed=rh$unemployed, survey=as.factor(rh$survey), iota=rep(1,3), phi=rep(1,3))

xenmodel<-run.jags(model="xentstar.txt",
                     monitor=c("beta", "mu.b",  "ort", "rus", "une", "i1", "p1"), data=xenmod, n.chains=8, 
                   adapt=5000, burnin=10000, 
                   sample=500, thin=200, summarise=F,plots=F, modules=c("glm", "lecuyer"))
save(xenmodel, file="xentstar.RData")


##############Analysis
cds<-as.mcmc.list(xenmodel)
sum(gelman.diag(cds,multivariate = F)[[1]][,1]>1.1)

###combine chains
cds<-as.mcmc(xenmodel)


####Graphics
library(ggplot2)

###Prediction function
ppred<-function(x,y){pnorm(x + y)}

############Figure 4, Jews
####2004
pp<-ppred(cds[,1],cbind(cds[,seq(85,126,14)],cds[,seq(169,238,14)],cds[,seq(127,168,14)]))
pp<-cbind(pnorm(cds[,1]),pp)

####2012
pp2<-ppred(cds[,2],cbind(cds[,seq(86,126,14)],cds[,seq(170,238,14)],cds[,seq(128,168,14)]))
pp2<-cbind(pnorm(cds[,2]),pp2)

####combine
pp<-cbind(pp,pp2)

###point estimate and hpd
ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


#####variable names
rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

####year indicator
Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))


#####create graphic
dodge=.5
pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1, show.legend=F,
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Jews", x="") + 
  theme(legend.position=c(.8,.2), text = element_text(size=15))  + coord_flip(ylim=c(0,1))

######Figure 4, Chechens
pp<-ppred(cds[,7],cbind(cds[,seq(91,126,14)],cds[,seq(175,238,14)],cds[,seq(133,168,14)]))
pp<-cbind(pnorm(cds[,7]),pp)

pp2<-ppred(cds[,8],cbind(cds[,seq(92,126,14)],cds[,seq(176,238,14)],cds[,seq(134,168,14)]))
pp2<-cbind(pnorm(cds[,8]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))



rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F,
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Chechens", x="") + 
  theme(legend.position="", text = element_text(size=15))  + coord_flip(ylim=c(0,1))


######Figure 4, Muslims
pp<-ppred(cds[,13],cbind(cds[,seq(97,126,14)],cds[,seq(181,238,14)],cds[,seq(139,168,14)]))
pp<-cbind(pnorm(cds[,13]),pp)

pp2<-ppred(cds[,14],cbind(cds[,seq(98,126,14)],cds[,seq(182,238,14)],cds[,seq(140,168,14)]))
pp2<-cbind(pnorm(cds[,14]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F,
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Muslims", x="") + 
  theme(legend.position="none", text = element_text(size=15))  + coord_flip(ylim=c(0,1))


######Figure 4, Roma
pp<-ppred(cds[,3],cbind(cds[,seq(87,126,14)],cds[,seq(171,238,14)],cds[,seq(129,168,14)]))
pp<-cbind(pnorm(cds[,3]),pp)

pp2<-ppred(cds[,4],cbind(cds[,seq(88,126,14)],cds[,seq(172,238,14)],cds[,seq(130,168,14)]))
pp2<-cbind(pnorm(cds[,4]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F, 
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Roma", x="") + 
  theme(legend.position="none", text = element_text(size=15))  + coord_flip(ylim=c(0,1))


###Figure 4, Americans
pp<-ppred(cds[,5],cbind(cds[,seq(89,126,14)],cds[,seq(173,238,14)],cds[,seq(131,168,14)]))
pp<-cbind(pnorm(cds[,5]),pp)

pp2<-ppred(cds[,6],cbind(cds[,seq(90,126,14)],cds[,seq(174,238,14)],cds[,seq(132,168,14)]))
pp2<-cbind(pnorm(cds[,6]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F,
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Americans", x="") + 
  theme(legend.position="none", text = element_text(size=15))  + coord_flip(ylim=c(0,1))

########Swedes (not in paper)
pp<-ppred(cds[,9],cbind(cds[,seq(93,126,14)],cds[,seq(177,238,14)],cds[,seq(135,168,14)]))
pp<-cbind(pnorm(cds[,9]),pp)

pp2<-ppred(cds[,10],cbind(cds[,seq(94,126,14)],cds[,seq(178,238,14)],cds[,seq(136,168,14)]))
pp2<-cbind(pnorm(cds[,10]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F, 
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Swedes", x="") + 
  theme(legend.position="none", text = element_text(size=15))  + coord_flip(ylim=c(0,1))

#####Figure 4, Azerbaijanis
pp<-ppred(cds[,11],cbind(cds[,seq(95,126,14)],cds[,seq(179,238,14)],cds[,seq(137,168,14)]))
pp<-cbind(pnorm(cds[,11]),pp)

pp2<-ppred(cds[,12],cbind(cds[,seq(96,126,14)],cds[,seq(180,238,14)],cds[,seq(138,168,14)]))
pp2<-cbind(pnorm(cds[,12]),pp2)

pp<-cbind(pp,pp2)

ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(rep(c("Reference","Unemployed",
                  "First quintile income", "Fifth quintile income",
                  "Not Orthodox", "Not Russian",
                  "No economic fear", "Very not confident in Putin", 
                  "Very confident in Putin",
                  "Rural", "St. Petersburg","Moscow"),2))
rpp<-factor(rpp,levels=c("Reference",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian", "No economic fear", 
                         "Very not confident in Putin","Very confident in Putin",
                         "Rural", "St. Petersburg", "Moscow"))

Year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

dodge=.5

pl.ob<-data.frame(rpp,Year,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm, color=Year))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper, color=Year), size=1,show.legend=F,
                position=position_dodge(width=0.5)) + scale_color_grey(end=.5) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5)) + 
  theme_bw() + labs(y="Azerbaijanis", x="") + 
  theme(legend.position="none", text = element_text(size=15))  + coord_flip(ylim=c(0,1))


##############Appendix D.1, Table 4
m50<-summary(cds)[[2]][,3]
hpd<-HPDinterval(cds, prob=.9)

####Jews, Chechens and Muslims
hpd<-cbind(m50[c(1,seq(15,140,14),155,141,seq(169,238,14))],1, hpd[c(1,seq(15,140,14),155,141,seq(169,238,14)),1], 2,hpd[c(1,seq(15,140,14),155,141,seq(169,238,14)),2], 3,
           m50[c(2,seq(16,140,14),156,142,seq(170,238,14))],1, hpd[c(2,seq(16,140,14),156,142,seq(170,238,14)),1], 2,hpd[c(2,seq(16,140,14),156,142,seq(170,238,14)),2], 3,
           m50[c(7,seq(21,140,14),161,147,seq(175,238,14))],1, hpd[c(7,seq(21,140,14),161,147,seq(175,238,14)),1], 2,hpd[c(7,seq(21,140,14),161,147,seq(175,238,14)),2], 3,
           m50[c(8,seq(22,140,14),162,148,seq(176,238,14))],1, hpd[c(8,seq(22,140,14),162,148,seq(176,238,14)),1], 2,hpd[c(8,seq(22,140,14),162,148,seq(176,238,14)),2], 3,
           m50[c(13,seq(27,140,14),167,153,seq(181,238,14))],1, hpd[c(13,seq(27,140,14),167,153,seq(181,238,14)),1], 2,hpd[c(13,seq(27,140,14),167,153,seq(181,238,14)),2], 3,
           m50[c(14,seq(28,140,14),168,154,seq(182,238,14))],1, hpd[c(14,seq(28,140,14),168,154,seq(182,238,14)),1], 2,hpd[c(14,seq(28,140,14),168,154,seq(182,238,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Moscow",
                 "St. Petersburg","Not Orthodox","Not Russian","Economic fear","Very not confident in Putin","Very confident in Putin")


stargazer(hpd, digits=2)
####Manually format LaTeX

######Roma and Azerbaijanis
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(3,seq(17,140,14),157,143,seq(171,238,14))],1, hpd[c(3,seq(17,140,14),157,143,seq(171,238,14)),1], 2,hpd[c(3,seq(17,140,14),157,143,seq(171,238,14)),2], 3,
           m50[c(4,seq(18,140,14),158,144,seq(172,238,14))],1, hpd[c(4,seq(18,140,14),158,144,seq(172,238,14)),1], 2,hpd[c(4,seq(18,140,14),158,144,seq(172,238,14)),2], 3,
           m50[c(11,seq(25,140,14),165,151,seq(179,238,14))],1, hpd[c(11,seq(25,140,14),165,151,seq(179,238,14)),1], 2,hpd[c(11,seq(25,140,14),165,151,seq(179,238,14)),2], 3,
           m50[c(12,seq(26,140,14),166,151,seq(180,238,14))],1, hpd[c(12,seq(26,140,14),166,151,seq(180,238,14)),1], 2,hpd[c(12,seq(26,140,14),166,151,seq(180,238,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Moscow",
                 "St. Petersburg","Not Orthodox","Not Russian","Economic fear","Very not confident in Putin","Very confident in Putin")
stargazer(hpd, digits=2)

######Americans and Swedes
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(5,seq(19,140,14),159,145,seq(173,238,14))],1, hpd[c(5,seq(19,140,14),159,145,seq(173,238,14)),1], 2,hpd[c(5,seq(19,140,14),159,145,seq(173,238,14)),2], 3,
           m50[c(6,seq(20,140,14),160,146,seq(174,238,14))],1, hpd[c(6,seq(20,140,14),160,146,seq(174,238,14)),1], 2,hpd[c(6,seq(20,140,14),160,146,seq(174,238,14)),2], 3,
           m50[c(9,seq(23,140,14),163,149,seq(177,238,14))],1, hpd[c(9,seq(23,140,14),163,149,seq(177,238,14)),1], 2,hpd[c(9,seq(23,140,14),163,149,seq(177,238,14)),2], 3,
           m50[c(10,seq(24,140,14),164,150,seq(178,238,14))],1, hpd[c(10,seq(24,140,14),164,150,seq(178,238,14)),1], 2,hpd[c(10,seq(24,140,14),164,150,seq(178,238,14)),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Moscow",
                 "St. Petersburg","Not Orthodox","Not Russian","Economic fear","Very not confident in Putin","Very confident in Putin")

stargazer(hpd, digits=2)