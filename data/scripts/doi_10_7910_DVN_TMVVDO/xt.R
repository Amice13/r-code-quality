library(rjags)
library(runjags)

load("gerber_ds.RData")

####dichotomize xenophobia outcomes
y<-rh[,10:16]
y<-t(apply(y,1,as.numeric))
y<-y>3
y<-t(apply(y,1,as.numeric))


NBETA=12
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
             northodox=rh$northodox,
             nrussian=rh$nRussian,
             unemployed=rh$unemployed, survey=as.factor(rh$survey), iota=rep(1,3))

xenmodel<-run.jags(model="xt.txt",
                   monitor=c("beta", "alpha", "mu.b", "mu.a","ort", "rus", "une", "i1"), data=xenmod, n.chains=8, 
                   adapt=5000, burnin=1000, 
                   sample=500, thin=20, summarise=F,plots=F, modules=c("glm", "lecuyer"))
save(xenmodel, file="xt.RData")



##############Analysis
cds<-as.mcmc.list(xenmodel)
sum(gelman.diag(cds,multivariate = F)[[1]][,1]>1.1)

###combine chains
cds<-as.mcmc(xenmodel)


#######Graphics
library(ggplot2)
####Prediction function
ppred<-function(x,y){pnorm(x + y)}

####Jews
####Predict by region and survey
pp1<-ppred(cds[,1],cds[,seq(169,574,7)])
pp2<-ppred(cds[,2],cds[,seq(575,980,7)])
pp<-as.vector(cbind(pp1,pp2))

####Matrix of region names
blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

####year names
year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

####data frame
pp<-data.frame(rpp,year,pp)


pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

###Identify Moscow and StP
pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

####Remove regions not in a survey
pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


####Order datasets
pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)



####Graphics by year
qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Jews, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Jews, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))



####Figure 3, Jews
pp1<-ppred(cds[,1],cds[,seq(169,574,7)])
pp2<-ppred(cds[,2],cds[,seq(575,980,7)])
pp1m<-apply(pp1,2,median)
pp2m<-apply(pp2,2,median)

pp1h<-HPDinterval(as.mcmc(pp1))
pp2h<-HPDinterval(as.mcmc(pp2))

oblast<-c("Arkhangelsk", "Karelia", "Leningrad", "Vladimir", "Ivanovo",
          "Kaluga", "Kostroma", "Moscow (Oblast)", "Riazan", "Smolensk",
          "Nizhny Novgorod", "Kirov", "Chuvashia", "Voronezh", "Lipetsk",
          "Volgograd", "Samara", "Saratov", "Ulianovsk", "Tatarstan",            
          "Krasnodar", "Adygeia", "Stavropol", "Karachai-Cherkessia",
          "Rostov", "Kurgan", "Orenburg", "Perm", "Komi-Permiak", "Sverdlovsk",
          "Cheliabinsk", "Bashkortostan", "Udmurtia", "Altai (Krai)", "Altai (Republic)",
          "Kemerovo", "Novosibirsk", "Krasnoiarsk", "Khakhassia", "Irkutsk", 
          "Primorsk", "Khabarovsk", "Jewish AOb", "Kaliningrad", "Moscow (City)",
          "Saint Petersburg", "Dagestan", "Kursk", "Magadan", "Novgorod",
          "Omsk", "Oryol", "Penzensk", "Tula", "Tiumen", "Tver", "Khanti-Mansi",
          "Yaroslavl")
pp1<-data.frame(oblast,pp1m,pp1h)
pp2<-data.frame(oblast,pp2m,pp2h)


pp1$msp<-0
pp1$msp[which(pp1[,1]=="Moscow (City)")]<-1
pp1$msp[which(pp1[,1]=="Saint Petersburg")]<-2

pp2$msp<-0
pp2$msp[which(pp2[,1]=="Moscow (City)")]<-1
pp2$msp[which(pp2[,1]=="Saint Petersburg")]<-2


pp1<-pp1[-which(pp1[,1] == "Dagestan" | pp1[,1] == "Kursk" | pp1[,1] == "Magadan"
                | pp1[,1] == "Novgorod" | pp1[,1] ==  "Omsk" | pp1[,1] ==  "Oryol"
                | pp1[,1] == "Penzensk" | pp1[,1] == "Tula" | pp1[,1] ==  "Tiumen"
                | pp1[,1] == "Tver" | pp1[,1] == "Khanti-Mansi" | pp1[,1] == "Yaroslavl"),]


pp2<-pp2[-which(pp2[,1] == "Karelia" | pp2[,1] == "Ivanovo" | pp2[,1] == "Kaluga"
                | pp2[,1] == "Riazan" | pp2[,1] ==  "Smolensk" | pp2[,1] ==  "Kirov"
                | pp2[,1] == "Karachai-Cherkessia" | pp2[,1] == "Komi-Permiak" | pp2[,1] ==  "Udmurtia"
                | pp2[,1] == "Altai (Republic)" | pp2[,1] == "Irkutsk" | pp2[,1] == "Jewish AOb"),]


pp1[,1]<-reorder(pp1[,1],pp1[,2])
pp2[,1]<-reorder(pp2[,1],pp2[,2])

qplot(pp1[,1],pp1[,2], xlab="", ylab="", 
      color=as.factor(pp1$msp), shape=as.factor(pp1$msp), size=I(2)) + 
  geom_errorbar(ymin=pp1[,3],ymax=pp1[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))


qplot(pp2[,1],pp2[,2], xlab="", ylab="", 
      color=as.factor(pp2$msp), shape=as.factor(pp2$msp), size=I(2)) + 
  geom_errorbar(ymin=pp2[,3],ymax=pp2[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))


######Roma
pp1<-ppred(cds[,3],cds[,seq(170,574,7)])
pp2<-ppred(cds[,4],cds[,seq(576,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]
pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Roma, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Roma, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))


######Americans

pp1<-ppred(cds[,5],cds[,seq(171,574,7)])
pp2<-ppred(cds[,6],cds[,seq(577,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Americans, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Americans, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))


######Chechens

pp1<-ppred(cds[,7],cds[,seq(172,574,7)])
pp2<-ppred(cds[,8],cds[,seq(578,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Chechens, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Chechens, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))


####Figure 3, Chechens

pp1<-ppred(cds[,7],cds[,seq(172,574,7)])
pp2<-ppred(cds[,8],cds[,seq(578,980,7)])

pp1m<-apply(pp1,2,median)
pp2m<-apply(pp2,2,median)

pp1h<-HPDinterval(as.mcmc(pp1))
pp2h<-HPDinterval(as.mcmc(pp2))

oblast<-c("Arkhangelsk", "Karelia", "Leningrad", "Vladimir", "Ivanovo",
          "Kaluga", "Kostroma", "Moscow (Oblast)", "Riazan", "Smolensk",
          "Nizhny Novgorod", "Kirov", "Chuvashia", "Voronezh", "Lipetsk",
          "Volgograd", "Samara", "Saratov", "Ulianovsk", "Tatarstan",            
          "Krasnodar", "Adygeia", "Stavropol", "Karachai-Cherkessia",
          "Rostov", "Kurgan", "Orenburg", "Perm", "Komi-Permiak", "Sverdlovsk",
          "Cheliabinsk", "Bashkortostan", "Udmurtia", "Altai (Krai)", "Altai (Republic)",
          "Kemerovo", "Novosibirsk", "Krasnoiarsk", "Khakhassia", "Irkutsk", 
          "Primorsk", "Khabarovsk", "Jewish AOb", "Kaliningrad", "Moscow (City)",
          "Saint Petersburg", "Dagestan", "Kursk", "Magadan", "Novgorod",
          "Omsk", "Oryol", "Penzensk", "Tula", "Tiumen", "Tver", "Khanti-Mansi",
          "Yaroslavl")
pp1<-data.frame(oblast,pp1m,pp1h)
pp2<-data.frame(oblast,pp2m,pp2h)


pp1$msp<-0
pp1$msp[which(pp1[,1]=="Moscow (City)")]<-1
pp1$msp[which(pp1[,1]=="Saint Petersburg")]<-2

pp2$msp<-0
pp2$msp[which(pp2[,1]=="Moscow (City)")]<-1
pp2$msp[which(pp2[,1]=="Saint Petersburg")]<-2


pp1<-pp1[-which(pp1[,1] == "Dagestan" | pp1[,1] == "Kursk" | pp1[,1] == "Magadan"
                | pp1[,1] == "Novgorod" | pp1[,1] ==  "Omsk" | pp1[,1] ==  "Oryol"
                | pp1[,1] == "Penzensk" | pp1[,1] == "Tula" | pp1[,1] ==  "Tiumen"
                | pp1[,1] == "Tver" | pp1[,1] == "Khanti-Mansi" | pp1[,1] == "Yaroslavl"),]


pp2<-pp2[-which(pp2[,1] == "Karelia" | pp2[,1] == "Ivanovo" | pp2[,1] == "Kaluga"
                | pp2[,1] == "Riazan" | pp2[,1] ==  "Smolensk" | pp2[,1] ==  "Kirov"
                | pp2[,1] == "Karachai-Cherkessia" | pp2[,1] == "Komi-Permiak" | pp2[,1] ==  "Udmurtia"
                | pp2[,1] == "Altai (Republic)" | pp2[,1] == "Irkutsk" | pp2[,1] == "Jewish AOb"),]


pp1[,1]<-reorder(pp1[,1],pp1[,2])
pp2[,1]<-reorder(pp2[,1],pp2[,2])



qplot(pp1[,1],pp1[,2], xlab="", ylab="", 
      color=as.factor(pp1$msp), shape=as.factor(pp1$msp), size=I(2)) + 
  geom_errorbar(ymin=pp1[,3],ymax=pp1[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))


qplot(pp2[,1],pp2[,2], xlab="", ylab="", 
      color=as.factor(pp2$msp), shape=as.factor(pp2$msp), size=I(2)) + 
  geom_errorbar(ymin=pp2[,3],ymax=pp2[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))



######Swedes
pp1<-ppred(cds[,9],cds[,seq(173,574,7)])
pp2<-ppred(cds[,10],cds[,seq(579,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Swedes, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Swedes, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

######Azerbaijanis
pp1<-ppred(cds[,11],cds[,seq(174,574,7)])
pp2<-ppred(cds[,12],cds[,seq(580,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Azerbaijanis, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Azerbaijanis, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

##############Muslims
pp1<-ppred(cds[,13],cds[,seq(175,574,7)])
pp2<-ppred(cds[,14],cds[,seq(581,980,7)])
pp<-as.vector(cbind(pp1,pp2))

blah<-matrix(nrow=4000, ncol=length(levels(rh$oblast)))
for(i in 1:length(levels(rh$oblast))){
  blah[,i]<-rep(levels(rh$oblast)[i],4000)
}

rpp<-rep(as.vector(blah),2)

year<-c(rep("2004",length(rpp)/2),rep("2012",length(rpp)/2))

pp<-data.frame(rpp,year,pp)

pp1<-pp[pp$year=="2004",]
pp2<-pp[pp$year=="2012",]

pp1$msp<-0
pp2$msp<-0
pp1$msp[which(pp1[,1]=="moscow      city")]<-1
pp1$msp[which(pp1[,1]=="spb city")]<-2
pp2$msp[which(pp2[,1]=="moscow      city")]<-1
pp2$msp[which(pp2[,1]=="spb city")]<-2

pp1[which(pp1[,1] == "dagestan" | pp1[,1] == "kurskaia" | pp1[,1] == "magadanskaia"
          | pp1[,1] == "novgorod" | pp1[,1] ==  "omsk" | pp1[,1] ==  "orlovskaia"
          | pp1[,1] == "penzenskaia" | pp1[,1] == "tulskaia" | pp1[,1] ==  "tumenskaia"
          | pp1[,1] == "tverskaia" | pp1[,1] == "xanti-manskiiskii" | pp1[,1] == "yaroslavskaia"),3]<-NA
pp1<-pp1[!is.na(pp1[,3]),]


pp2[which(pp2[,1] == "karelia" | pp2[,1] == "ivanovskaia" | pp2[,1] == "kaluga"
          | pp2[,1] == "ryazan" | pp2[,1] ==  "smolenskaia" | pp2[,1] ==  "kirovskaia"
          | pp2[,1] == "karachaevo-cherkessiia" | pp2[,1] == "komi-permiakskaia" | pp2[,1] ==  "udmurtiia"
          | pp2[,1] == "altai rep" | pp2[,1] == "irkutskaia" | pp2[,1] == "evreiskaia"),3]<-NA
pp2<-pp2[!is.na(pp2[,3]),]


pp1[,1]<-reorder(pp1[,1],pp1[,3],median)
pp2[,1]<-reorder(pp2[,1],pp2[,3],median)

qplot(pp1[,1],pp1[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Muslims, 2004", color=as.factor(pp1$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))
qplot(pp2[,1],pp2[,3], alpha=I(.01), xlab="", ylab="Posterior probability of xenophobia toward Muslims, 2012", color=as.factor(pp2$msp)) + theme_bw()   + theme(legend.position="none")  + coord_flip(ylim=c(0,1))

###Figure 3, Muslims

pp1<-ppred(cds[,13],cds[,seq(175,574,7)])
pp2<-ppred(cds[,14],cds[,seq(581,980,7)])

pp1m<-apply(pp1,2,median)
pp2m<-apply(pp2,2,median)

pp1h<-HPDinterval(as.mcmc(pp1))
pp2h<-HPDinterval(as.mcmc(pp2))

oblast<-c("Arkhangelsk", "Karelia", "Leningrad", "Vladimir", "Ivanovo",
          "Kaluga", "Kostroma", "Moscow (Oblast)", "Riazan", "Smolensk",
          "Nizhny Novgorod", "Kirov", "Chuvashia", "Voronezh", "Lipetsk",
          "Volgograd", "Samara", "Saratov", "Ulianovsk", "Tatarstan",            
          "Krasnodar", "Adygeia", "Stavropol", "Karachai-Cherkessia",
          "Rostov", "Kurgan", "Orenburg", "Perm", "Komi-Permiak", "Sverdlovsk",
          "Cheliabinsk", "Bashkortostan", "Udmurtia", "Altai (Krai)", "Altai (Republic)",
          "Kemerovo", "Novosibirsk", "Krasnoiarsk", "Khakhassia", "Irkutsk", 
          "Primorsk", "Khabarovsk", "Jewish AOb", "Kaliningrad", "Moscow (City)",
          "Saint Petersburg", "Dagestan", "Kursk", "Magadan", "Novgorod",
          "Omsk", "Oryol", "Penzensk", "Tula", "Tiumen", "Tver", "Khanti-Mansi",
          "Yaroslavl")
pp1<-data.frame(oblast,pp1m,pp1h)
pp2<-data.frame(oblast,pp2m,pp2h)


pp1$msp<-0
pp1$msp[which(pp1[,1]=="Moscow (City)")]<-1
pp1$msp[which(pp1[,1]=="Saint Petersburg")]<-2

pp2$msp<-0
pp2$msp[which(pp2[,1]=="Moscow (City)")]<-1
pp2$msp[which(pp2[,1]=="Saint Petersburg")]<-2


pp1<-pp1[-which(pp1[,1] == "Dagestan" | pp1[,1] == "Kursk" | pp1[,1] == "Magadan"
                | pp1[,1] == "Novgorod" | pp1[,1] ==  "Omsk" | pp1[,1] ==  "Oryol"
                | pp1[,1] == "Penzensk" | pp1[,1] == "Tula" | pp1[,1] ==  "Tiumen"
                | pp1[,1] == "Tver" | pp1[,1] == "Khanti-Mansi" | pp1[,1] == "Yaroslavl"),]


pp2<-pp2[-which(pp2[,1] == "Karelia" | pp2[,1] == "Ivanovo" | pp2[,1] == "Kaluga"
                | pp2[,1] == "Riazan" | pp2[,1] ==  "Smolensk" | pp2[,1] ==  "Kirov"
                | pp2[,1] == "Karachai-Cherkessia" | pp2[,1] == "Komi-Permiak" | pp2[,1] ==  "Udmurtia"
                | pp2[,1] == "Altai (Republic)" | pp2[,1] == "Irkutsk" | pp2[,1] == "Jewish AOb"),]


pp1[,1]<-reorder(pp1[,1],pp1[,2])
pp2[,1]<-reorder(pp2[,1],pp2[,2])

qplot(pp1[,1],pp1[,2], xlab="", ylab="", 
      color=as.factor(pp1$msp), shape=as.factor(pp1$msp), size=I(2)) + 
  geom_errorbar(ymin=pp1[,3],ymax=pp1[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))


qplot(pp2[,1],pp2[,2], xlab="", ylab="", 
      color=as.factor(pp2$msp), shape=as.factor(pp2$msp), size=I(2)) + 
  geom_errorbar(ymin=pp2[,3],ymax=pp2[,4], width=I(.001)) + 
  theme_bw() + theme(legend.position="none") + 
  scale_color_manual(values=c("1"="black", "2"="black", "0"="grey")) + coord_flip(ylim=c(0,1))


#####################Appendix D.2, Table 6
m50<-summary(cds)[[2]][,3]
hpd<-HPDinterval(cds, prob=.9)

###Jews, Chechens and Muslims
hpd<-cbind(m50[c(1,seq(15,140,14),141,155)],1, hpd[c(1,seq(15,140,14),141,155),1], 2,hpd[c(1,seq(15,140,14),141,155),2], 3,
           m50[c(2,seq(16,140,14),142,156)],1, hpd[c(2,seq(16,140,14),142,156),1], 2,hpd[c(2,seq(16,140,14),142,156),2], 3,
           m50[c(7,seq(21,140,14),147,161)],1, hpd[c(7,seq(21,140,14),147,161),1], 2,hpd[c(7,seq(21,140,14),147,161),2], 3,
           m50[c(8,seq(22,140,14),148,162)],1, hpd[c(8,seq(22,140,14),148,162),1], 2,hpd[c(8,seq(22,140,14),148,162),2], 3,
           m50[c(13,seq(27,140,14),153,167)],1, hpd[c(13,seq(27,140,14),153,167),1], 2,hpd[c(13,seq(27,140,14),153,167),2], 3,
           m50[c(14,seq(28,140,14),154,168)],1, hpd[c(14,seq(28,140,14),154,168),1], 2,hpd[c(14,seq(28,140,14),154,168),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural",
                 "Not Orthodox", "Not Russian")


stargazer(hpd, digits=2)
###Manually convert to LaTeX

#####Roma and Azerbaijanis
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(3,seq(17,140,14),143,157)],1, hpd[c(3,seq(17,140,14),143,157),1], 2,hpd[c(3,seq(17,140,14),143,157),2], 3,
           m50[c(4,seq(18,140,14),144,158)],1, hpd[c(4,seq(18,140,14),144,158),1], 2,hpd[c(4,seq(18,140,14),144,158),2], 3,
           m50[c(11,seq(25,140,14),151,165)],1, hpd[c(11,seq(25,140,14),151,165),1], 2,hpd[c(11,seq(25,140,14),151,165),2], 3,
           m50[c(12,seq(26,140,14),152,166)],1, hpd[c(12,seq(26,140,14),152,166),1], 2,hpd[c(12,seq(26,140,14),152,166),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural", "Not Orthodox","Not Russian")
stargazer(hpd, digits=2)

######Americans and Swedes
hpd<-HPDinterval(cds, prob=.9)
hpd<-cbind(m50[c(5,seq(19,140,14),145,159)],1, hpd[c(5,seq(19,140,14),145,159),1], 2,hpd[c(5,seq(19,140,14),145,159),2], 3,
           m50[c(6,seq(20,140,14),146,160)],1, hpd[c(6,seq(20,140,14),146,160),1], 2,hpd[c(6,seq(20,140,14),146,160),2], 3,
           m50[c(9,seq(23,140,14),149,163)],1, hpd[c(9,seq(23,140,14),149,163),1], 2,hpd[c(9,seq(23,140,14),149,163),2], 3,
           m50[c(10,seq(24,140,14),150,164)],1, hpd[c(10,seq(24,140,14),150,164),1], 2,hpd[c(10,seq(24,140,14),150,164),2], 4)
rownames(hpd)<-c("Constant", "Male", "Age < 22", 
                 "Age > 60", "Incomplete high school or lower", "University or higher education", 
                 "Unemployed", "First quintile income", "Fifth quintile income","Rural",
                 "Not Orthodox", "Not Russian")


stargazer(hpd, digits=2)

