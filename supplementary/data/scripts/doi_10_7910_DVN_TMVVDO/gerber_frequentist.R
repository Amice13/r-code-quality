########################Frequentist robustness checks
load("gerber_ds.RData")

#####Edit 2012 data
#####dichotomize xenophobia variables
rh$feelchec<-as.numeric(as.numeric(rh$feelchec)>3)
rh$feeljew<-as.numeric(as.numeric(rh$feeljew)>3)
rh$feelmusl<-as.numeric(as.numeric(rh$feelmusl)>3)
rh$feelgyps<-as.numeric(as.numeric(rh$feelgyps)>3)
rh$feelazer<-as.numeric(as.numeric(rh$feelazer)>3)
rh$feelamer<-as.numeric(as.numeric(rh$feelamer)>3)
rh$feelswed<-as.numeric(as.numeric(rh$feelswed)>3)

####turn age variable into "young" and "old" variables
rh$young<-as.numeric(rh$age_cat=="1")
rh$old<-as.numeric(rh$age_cat=="3")

####turn age education into "low" and "high" variables
rh$lowed<-as.numeric(rh$education=="1")
rh$highed<-as.numeric(rh$education=="3")


####turn age income into "low" and "high" variables
rh$lowinc<-as.numeric(rh$income=="1")
rh$highinc<-as.numeric(rh$income=="3")

####set moscow as reference for oblast variable
rh$oblast<-relevel(rh$oblast,"moscow      city")

####turn putin into "low" and "high" variables
rh$lputin<-as.numeric(rh$putin=="1")
rh$hputin<-as.numeric(rh$putin=="3")

####Table 1, descriptive stats,2004
mean(rh$male[rh$survey=="2004"],na.rm=T)
mean(rh$age_cat[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$age_cat[rh$survey=="2004"]=="2",na.rm=T)
mean(rh$age_cat[rh$survey=="2004"]=="3",na.rm=T)
mean(rh$education[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$education[rh$survey=="2004"]=="2",na.rm=T)
mean(rh$education[rh$survey=="2004"]=="3",na.rm=T)
mean(rh$income[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$income[rh$survey=="2004"]=="2",na.rm=T)
mean(rh$income[rh$survey=="2004"]=="3",na.rm=T)
mean(rh$unemployed[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$threat[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$moscow[rh$survey=="2004"],na.rm=T)
mean(rh$stp[rh$survey=="2004"],na.rm=T)
mean(rh$rural[rh$survey=="2004"]=="1",na.rm=T)
mean(rh$northodox[rh$survey=="2004"],na.rm=T)
mean(rh$nRussian[rh$survey=="2004"],na.rm=T)
mean(rh$lputin[rh$survey=="2004"],na.rm=T)
mean(rh$hputin[rh$survey=="2004"],na.rm=T)
mean(rh$feelchec[rh$survey=="2004"],na.rm=T)
mean(rh$feeljew[rh$survey=="2004"],na.rm=T)
mean(rh$feelmusl[rh$survey=="2004"],na.rm=T)
mean(rh$feelgyps[rh$survey=="2004"],na.rm=T)
mean(rh$feelazer[rh$survey=="2004"],na.rm=T)
mean(rh$feelamer[rh$survey=="2004"],na.rm=T)
mean(rh$feelswed[rh$survey=="2004"],na.rm=T)

####Table 1, descriptive stats,2012
mean(rh$male[rh$survey=="2012"],na.rm=T)
mean(rh$age_cat[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$age_cat[rh$survey=="2012"]=="2",na.rm=T)
mean(rh$age_cat[rh$survey=="2012"]=="3",na.rm=T)
mean(rh$education[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$education[rh$survey=="2012"]=="2",na.rm=T)
mean(rh$education[rh$survey=="2012"]=="3",na.rm=T)
mean(rh$income[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$income[rh$survey=="2012"]=="2",na.rm=T)
mean(rh$income[rh$survey=="2012"]=="3",na.rm=T)
mean(rh$unemployed[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$threat[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$moscow[rh$survey=="2012"],na.rm=T)
mean(rh$stp[rh$survey=="2012"],na.rm=T)
mean(rh$rural[rh$survey=="2012"]=="1",na.rm=T)
mean(rh$northodox[rh$survey=="2012"],na.rm=T)
mean(rh$nRussian[rh$survey=="2012"],na.rm=T)
mean(rh$lputin[rh$survey=="2012"],na.rm=T)
mean(rh$hputin[rh$survey=="2012"],na.rm=T)
mean(rh$feelchec[rh$survey=="2012"],na.rm=T)
mean(rh$feeljew[rh$survey=="2012"],na.rm=T)
mean(rh$feelmusl[rh$survey=="2012"],na.rm=T)
mean(rh$feelgyps[rh$survey=="2012"],na.rm=T)
mean(rh$feelazer[rh$survey=="2012"],na.rm=T)
mean(rh$feelamer[rh$survey=="2012"],na.rm=T)
mean(rh$feelswed[rh$survey=="2012"],na.rm=T)

#############################Replication with comparable variables to 1996 data
#####"4" suffix = 2003/4 data, "12" suffix = 2011/2 data
#####no prefix = regional effects, "m" prefix = moscow and stp variables

chec4<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + oblast, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))

mchec4<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + moscow+stp, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))


chec12<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mchec12<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
             northodox + nRussian + moscow+stp, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

jew12<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
           northodox + nRussian + oblast, data=rh[rh$survey=="2012",],
           family = binomial(link="probit"))

mjew12<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + moscow+stp, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

jew4<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
          northodox + nRussian + oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mjew4<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + moscow+stp, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))

musl12<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + oblast, data=rh[rh$survey=="2012",],
           family = binomial(link="probit"))

mmusl12<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
             northodox + nRussian + moscow+stp, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

musl4<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
           northodox + nRussian + oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mmusl4<-glm(feelmusl~ male + young + old + lowed + highed + unemployed + lowinc + highinc + rural + 
            northodox + nRussian + moscow+stp, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))


library(stargazer)
####Appendix E.2.1
###Table 14
stargazer(mchec4,mchec12,chec4,chec12, digits=2, single.row=T)
###Table 15
stargazer(mjew4,mjew12,jew4,jew12, digits=2, single.row=T)
###Table 16
stargazer(mmusl4,mmusl12,musl4,musl12, digits=2, single.row=T)

#############################Replicate for all groups, all variables

chec4<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",], family = binomial(link="probit"))

mchec4<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + 
              lputin + hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))


chec12<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mchec12<-glm(feelchec ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

jew12<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2012",],
           family = binomial(link="probit"))

mjew12<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

jew4<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + 
            highinc + rural + northodox + nRussian + threat + lputin + hputin + 
            oblast, data=rh[rh$survey=="2004",],
          family = binomial(link="probit"))

mjew4<-glm(feeljew ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
             hputin, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

musl12<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mmusl12<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

musl4<-glm(feelmusl ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mmusl4<-glm(feelmusl~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))



gyps12<-glm(feelgyps ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mgyps12<-glm(feelgyps ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

gyps4<-glm(feelgyps ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mgyps4<-glm(feelgyps~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))




azer12<-glm(feelazer ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mazer12<-glm(feelazer ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

azer4<-glm(feelazer ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mazer4<-glm(feelazer~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))


amer12<-glm(feelamer ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mamer12<-glm(feelamer ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

amer4<-glm(feelamer ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mamer4<-glm(feelamer~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))


swed12<-glm(feelswed ~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + threat + lputin + hputin + 
              oblast, data=rh[rh$survey=="2012",],
            family = binomial(link="probit"))

mswed12<-glm(feelswed ~ male + young + old + lowed + highed + unemployed + lowinc + 
               highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
               hputin, data=rh[rh$survey=="2012",],
             family = binomial(link="probit"))

swed4<-glm(feelswed ~ male + young + old + lowed + highed + unemployed + lowinc + 
             highinc + rural + northodox + nRussian + threat + lputin + hputin + 
             oblast, data=rh[rh$survey=="2004",],
           family = binomial(link="probit"))

mswed4<-glm(feelswed~ male + young + old + lowed + highed + unemployed + lowinc + 
              highinc + rural + northodox + nRussian + moscow+stp + threat + lputin + 
              hputin, data=rh[rh$survey=="2004",],
            family = binomial(link="probit"))

####Appendix E.2.1
###Table 17
stargazer(mchec4,mchec12,chec4,chec12, digits=2, single.row=T)
###Table 18
stargazer(mjew4,mjew12,jew4,jew12, digits=2, single.row=T)
###Table 19
stargazer(mmusl4,mmusl12,musl4,musl12, digits=2, single.row=T)
###Table 20
stargazer(mgyps4,mgyps12,gyps4,gyps12, digits=2, single.row=T)
###Table 21
stargazer(mazer4,mazer12,azer4,azer12, digits=2, single.row=T)
###Table 22
stargazer(mamer4,mamer12,amer4,amer12, digits=2, single.row=T)
###Table 23
stargazer(mswed4,mswed12,swed4,swed12, digits=2, single.row=T)

