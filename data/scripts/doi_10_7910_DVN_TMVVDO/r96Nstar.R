library(rjags)
library(runjags)

load("r96.RData")

taustarO4<-c(qnorm(mean(r4$feelchec<2,na.rm=T)),qnorm(mean(r4$feelchec<3,na.rm=T)),qnorm(mean(r4$feelchec<4,na.rm=T)),qnorm(mean(r4$feelchec<5,na.rm=T)))


NBETA=12
NALPHA=length(unique(r4$oblast))
xenmod<-list(N=nrow(r4), NALPHA=NALPHA, NBETA=NBETA, g0=rep(0,4), G0=diag(1,4),
                            b0=rep(0,NBETA), B0=diag(1,NBETA), y=as.matrix(r4[,8:10]), 
                             male=r4$male, 
             age=r4$age_cat,
             ed=r4$education,
             inc=r4$income,
             rural=r4$rural,
                             northodox=r4$northodox,
                             nrussian=r4$nRussian,
             unemployed=r4$unemployed,
             oblast=r4$oblast, iota=rep(1,3), epsilon=rep(1,3), omnicron=rep(1,3))

xenmodel<-run.jags(model="xen96star.txt", inits=list(gammastar=taustarO4), 
                   monitor=c("beta", "alpha", "gamma", "ort", "rus", "une", "i1",
                             "e1", "a1"), data=xenmod, n.chains=8, adapt=5000, burnin=50000, 
                   sample=500, thin=1000, summarise=F,plots=F, modules=c("glm", "lecuyer"))


save(xenmodel, file="xen96Nstar.RData")


cds<-as.mcmc(xenmodel)

gamma<-cds[,136]
ppred<-function(x,y,gamma){1- pnorm(gamma- (x + y))}

#########Jews
pp<-ppred(cds[,1],cds[,seq(37,132,3)],gamma)

ppm<-apply(pp,2,median)
pph<-HPDinterval(as.mcmc(pp))

oblast<-c("St. Petersburg",
                     "Moscow (City)",
                     "Moscow (Oblast)",
                     "Komi",
                     "Saratov",
                     "Leningrad",
                     "Smolensk",
                     "Tver",
                     "Tula",
                     "Kaluga",
                     "Nizhny Novgorod",
                     "Chuvashia",
                     "Penzensk",
                     "Lipetsk",
                     "Tambov",
                     "Tatarstan",
                     "Kranodarsk",
                     "Chelyabinsk",
                     "Volgograd",
                     "Kabardino-Balkaria",
                     "Rostov",
                     "Altai (Krai)",
                     "Stavropol",
                     "Krasnoiarsk",
                     "Kurgan",
                     "Udmurtia",
                     "Orenburg",
                     "Perm",
                     "Tomsk",
                     "Khanti-Mansi",
                     "Primorsk",
                     "Amur")

pp<-data.frame(oblast,ppm,pph)

pp[,1]<-reorder(pp[,1],pp[,2])

pp$msp<-0
pp$msp[which(pp[,1]=="Moscow (City)")]<-1
pp$msp[which(pp[,1]=="St. Petersburg")]<-2


qplot(pp[,1],pp[,2], xlab="", ylab="", 
      color=as.factor(pp$msp), shape=as.factor(pp$msp), size=I(2)) + 
  geom_errorbar(ymin=pp[,3],ymax=pp[,4], width=I(.001))  + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) +
  theme_bw() + theme(legend.position="none") + coord_flip(ylim=c(0,1))

  

###########Chechens

pp<-ppred(cds[,2],cds[,seq(38,132,3)],gamma)

ppm<-apply(pp,2,median)
pph<-HPDinterval(as.mcmc(pp))

oblast<-c("St. Petersburg",
          "Moscow (City)",
          "Moscow (Oblast)",
          "Komi",
          "Saratov",
          "Leningrad",
          "Smolensk",
          "Tver",
          "Tula",
          "Kaluga",
          "Nizhny Novgorod",
          "Chuvashia",
          "Penzensk",
          "Lipetsk",
          "Tambov",
          "Tatarstan",
          "Kranodarsk",
          "Chelyabinsk",
          "Volgograd",
          "Kabardino-Balkaria",
          "Rostov",
          "Altai (Krai)",
          "Stavropol",
          "Krasnoiarsk",
          "Kurgan",
          "Udmurtia",
          "Orenburg",
          "Perm",
          "Tomsk",
          "Khanti-Mansi",
          "Primorsk",
          "Amur")

pp<-data.frame(oblast,ppm,pph)

pp[,1]<-reorder(pp[,1],pp[,2])

pp$msp<-0
pp$msp[which(pp[,1]=="Moscow (City)")]<-1
pp$msp[which(pp[,1]=="St. Petersburg")]<-2



qplot(pp[,1],pp[,2], xlab="", ylab="", 
      color=as.factor(pp$msp), shape=as.factor(pp$msp), size=I(2)) + 
  geom_errorbar(ymin=pp[,3],ymax=pp[,4], width=I(.001))  + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) +
  theme_bw() + theme(legend.position="none") + coord_flip(ylim=c(0,1))


###########Muslims

pp<-ppred(cds[,3],cds[,seq(39,132,3)],gamma)

ppm<-apply(pp,2,median)
pph<-HPDinterval(as.mcmc(pp))

oblast<-c("St. Petersburg",
          "Moscow (City)",
          "Moscow (Oblast)",
          "Komi",
          "Saratov",
          "Leningrad",
          "Smolensk",
          "Tver",
          "Tula",
          "Kaluga",
          "Nizhny Novgorod",
          "Chuvashia",
          "Penzensk",
          "Lipetsk",
          "Tambov",
          "Tatarstan",
          "Kranodarsk",
          "Chelyabinsk",
          "Volgograd",
          "Kabardino-Balkaria",
          "Rostov",
          "Altai (Krai)",
          "Stavropol",
          "Krasnoiarsk",
          "Kurgan",
          "Udmurtia",
          "Orenburg",
          "Perm",
          "Tomsk",
          "Khanti-Mansi",
          "Primorsk",
          "Amur")

pp<-data.frame(oblast,ppm,pph)

pp[,1]<-reorder(pp[,1],pp[,2])

pp$msp<-0
pp$msp[which(pp[,1]=="Moscow (City)")]<-1
pp$msp[which(pp[,1]=="St. Petersburg")]<-2



qplot(pp[,1],pp[,2], xlab="", ylab="", 
      color=as.factor(pp$msp), shape=as.factor(pp$msp), size=I(2)) + 
  geom_errorbar(ymin=pp[,3],ymax=pp[,4], width=I(.001))  + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) +
  theme_bw() + theme(legend.position="none") + coord_flip(ylim=c(0,1))

####

m50<-summary(cds)[[2]][,3]
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[seq(1,42,3)],1, hpd[seq(1,42,3),1], 2,hpd[seq(1,42,3),2], 3,
           m50[seq(2,42,3)],1, hpd[seq(2,42,3),1], 2,hpd[seq(2,42,3),2], 3,
           m50[seq(3,42,3)],1, hpd[seq(3,42,3),1], 2,hpd[seq(3,42,3),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Moscow",
                 "St. Petersburg","Not Orthodox", "Not ethnic Russian")


stargazer(hpd, digits=2)

