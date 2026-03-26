library(rjags)
library(runjags)

load("r96.RData")

taustarO4<-c(qnorm(mean(r4$feelchec<2,na.rm=T)),qnorm(mean(r4$feelchec<3,na.rm=T)),qnorm(mean(r4$feelchec<4,na.rm=T)),qnorm(mean(r4$feelchec<5,na.rm=T)))


NBETA=14
NALPHA=length(unique(r4$oblast))
xenmod<-list(N=nrow(r4), NALPHA=NALPHA, NBETA=NBETA, g0=rep(0,4), G0=diag(1,4),
                            b0=rep(0,NBETA), B0=diag(1,NBETA), y=as.matrix(r4[,8:10]), 
                             male=r4$male, 
             age=r4$age_cat,
             ed=r4$education,
             inc=r4$income,
             rural=r4$rural,
             moscow=r4$moscow,
             stp=r4$stp,
             northodox=r4$northodox,
             nrussian=r4$nRussian,
             unemployed=r4$unemployed,
             oblast=r4$oblast, iota=rep(1,3), epsilon=rep(1,3), omnicron=rep(1,3))

xenmodel<-run.jags(model="x96star.txt", inits=list(gammastar=taustarO4), 
                   monitor=c("beta", "gamma", "ort", "rus", "une", "i1",
                             "e1", "a1"), data=xenmod, n.chains=8, adapt=5000, burnin=50000, 
                   sample=500, thin=1000, summarise=F,plots=F, modules=c("glm", "lecuyer"))


save(xenmodel, file="x96Nstar.RData")

cds<-as.mcmc(xenmodel)


HPDinterval(cds[,43:46],prob=.9)


gamma<-cds[,46]
ppred<-function(x,y,gamma){1- pnorm(gamma- (x + y))}


################Reduced


####Jews
pp<-ppred(cds[,1],cds[,c(seq(19,27,3),seq(37,42,3),seq(28,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,1]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(c("Reference","Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", 
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper), size=1,show.legend = F, 
                position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="", text = element_text(size=15))  + coord_flip(ylim=c(0,1))



####Chechens
pp<-ppred(cds[,2],cds[,c(seq(20,27,3),seq(38,42,3),seq(29,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,2]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))



rpp<-factor(c("Reference","Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", 
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)


ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper), size=1,show.legend = F, 
                 position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="", text = element_text(size=15))  + coord_flip(ylim=c(0,1))



####Muslims
pp<-ppred(cds[,3],cds[,c(seq(21,27,3),seq(39,42,3),seq(30,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,3]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))



rpp<-factor(c("Reference","Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", 
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 2, position=position_dodge(width=0.5)) +
  geom_linerange(aes(ymin = lower, ymax = upper), size=1,show.legend = F, 
                 position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="", text = element_text(size=15))  + coord_flip(ylim=c(0,1))






###############Not reduced

####Jews
pp<-ppred(cds[,1],cds[,c(seq(4,27,3),seq(37,42,3),seq(28,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,1]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(c("Reference", "Male", "Age < 22", 
              "Age > 60", "Incomplete high school or lower", 
              "University or higher education", "Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", "Male", "Age < 22", "Age > 60", 
                         "Incomplete high school or lower", "University or higher education",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size=.5,width = .01, 
                position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5,11.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="")  + coord_flip(ylim=c(0,1))



####Chechens
pp<-ppred(cds[,2],cds[,c(seq(5,27,3),seq(38,42,3),seq(29,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,2]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(c("Reference", "Male", "Age < 22", 
              "Age > 60", "Incomplete high school or lower", 
              "University or higher education", "Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", "Male", "Age < 22", "Age > 60", 
                         "Incomplete high school or lower", "University or higher education",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size=.5,width = .01, 
                position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5,11.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="")  + coord_flip(ylim=c(0,1))



####Muslims
pp<-ppred(cds[,3],cds[,c(seq(6,27,3),seq(39,42,3),seq(30,36,3))],gamma)
pp<-cbind(1-pnorm(gamma-cds[,3]),pp)


ppm<-apply(pp,2,median)
pphpd<-HPDinterval(as.mcmc(pp))


rpp<-factor(c("Reference", "Male", "Age < 22", 
              "Age > 60", "Incomplete high school or lower", 
              "University or higher education", "Unemployed",
              "First quintile income", "Fifth quintile income",
              "Not Orthodox", "Not Russian",
              "Rural","Moscow", "St. Petersburg"))
rpp<-factor(rpp,levels=c("Reference", "Male", "Age < 22", "Age > 60", 
                         "Incomplete high school or lower", "University or higher education",
                         "Unemployed", "First quintile income", "Fifth quintile income",  
                         "Not Orthodox", "Not Russian",
                         "Rural", "St. Petersburg", "Moscow"))

pl.ob<-data.frame(rpp,ppm,pphpd)

ggplot(data = pl.ob, aes(rpp, ppm))  + 
  geom_point(size = 1, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), size=.5,width = .01, 
                position=position_dodge(width=0.5)) + 
  geom_vline(xintercept = c(1.5,2.5,4.5,6.5,7.5,9.5,11.5)) + 
  theme_bw() + labs(y="", x="") + 
  theme(legend.position="")  + coord_flip(ylim=c(0,1))


##################
pp<-ppred(cds[,1],cds[,40],gamma)
pp<-cbind(1-pnorm(gamma-cds[,1]),pp)


rpp<-factor(c(rep("Russian",4000), rep("Not Russian",4000)
))          
rpp<-factor(rpp,levels=c("Russian", "Not Russian"
))


qplot(rpp,as.vector(pp), alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Jews, 1996") + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

###############


pp<-ppred(cds[,2],cds[,41],gamma)
pp<-cbind(1-pnorm(gamma-cds[,2]),pp)


rpp<-factor(c(rep("Russian",4000), rep("Not Russian",4000)
))          
rpp<-factor(rpp,levels=c("Russian", "Not Russian"
))


qplot(rpp,as.vector(pp), alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Chechens, 1996") + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

###############


pp<-ppred(cds[,3],cds[,42],gamma)
pp<-cbind(1-pnorm(gamma-cds[,3]),pp)


rpp<-factor(c(rep("Russian",4000), rep("Not Russian",4000)
))          
rpp<-factor(rpp,levels=c("Russian", "Not Russian"
))


qplot(rpp,as.vector(pp), alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Muslims, 1996") + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

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

