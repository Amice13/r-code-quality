#R code to replicate results in Democracy, Autocracy, and Everything in Between: How Domestic Institutions Affect Environmental Protection
# Forthcoming, British Journal of Political Science, Jana von Stein, jana.vonstein@anu.edu.au 
library(readstata13); library(ggeffects); library(lme4); library(arm); library(stargazer); library(xtable); library(devtools); library(sjlabelled);
library(sjmisc); library(sjstats); library(ggeffects); library(sjPlot); library(ggthemes); library(ggplot2); library(gridExtra); library(margins); 
library(see);library(sjPlot); library(sjmisc); library(ggeffects);library(performance); library(corrplot); library(dynpanel); library(pgmm)

setwd('~/Dropbox/environment-autocracies/final-version/final-data')
lmm.data<- read.dta13(file='final-data.dta', convert.underscore=TRUE)
##########################################################################TABLE 1A
ggtotal.cowyearregion<- lmer(lnggtotalcapnplus1 ~freefairelect+corecivil+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data)
so2.cowyearregion<- lmer(lnso2capnplus1 ~freefairelect+corecivil+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+(1|year)+(1|cowcode)+(1|region2), data = lmm.data) 
nox.cowyearregion<- lmer(lnnoxcapnplus1 ~freefairelect+corecivil+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+(1|year)+(1|cowcode)+(1|region2), data = lmm.data)  ; summary(so2.cowyearregion)
energyuse.cowyearregion<- lmer(lnenergyusenplus1 ~freefairelect+corecivil+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+lntradegdp+lnpopdens+(1|year)+(1|cowcode)+(1|region2), data = lmm.data) 
nonrenewables.cowyearregion<- lmer(lnnonrenewablesnplus1 ~freefairelect+corecivil+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+lntradegdp+lnpopdens+(1|year)+(1|cowcode)+(1|region2), data = lmm.data) 
nonprotected.cowyearregion<- lmer(lnnonprotectednplus1 ~freefairelect+corecivil10+polconiii+lngdpcapcurmean+I(lngdpcapcurmean^2)+lntradegdp+lnpopdens+(1|year)+(1|cowcode)+(1|region2), data = lmm.data) 
tab_model(ggtotal.cowyearregion, so2.cowyearregion, nox.cowyearregion, energyuse.cowyearregion, nonrenewables.cowyearregion, nonprotected.cowyearregion, 
  show.se=FALSE, show.ci=FALSE, digits=3)
##########################################################################FIGURE1A
ggtotal.plot<-plot_model(ggtotal.cowyearregion, title="Greenhouse Gas Emissions", axis.title="Coefficient",line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil","freefairelect"), 
  labels=c("freefairelect"="Elections", "corecivil"="Civil Lib/Society", "polconiii"="Polit Constraints", "lngdpcapcurmean"="GDP per cap", 
  "I(lngdpcapcurmean^2)"="GDP per cap-sq","lntradegdp"="Trade Openness", "lnpopdens"="Pop Density"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+ylim(-.25,.25)+
  theme(plot.title = element_text(hjust = 0.5))

so2.plot<-plot_model(so2.cowyearregion, title="Sulfur Dioxide Emissions", axis.title="Coefficient",line.size=.5,dot.size =1.5) +
  scale_x_discrete(limits=c("lnpopdens","lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil","freefairelect"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+ylim(-.5,1.25)+
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

nox.plot<-plot_model(nox.cowyearregion, title="Nitrogen Oxide Emissions", axis.title="Coefficient", line.size=.5,dot.size =1.5) +scale_x_discrete(limits=c("lnpopdens",
  "lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil","freefairelect"))+theme_bw() + theme(axis.line = element_line(colour = "black", size=.3), 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+ylim(-.5,1.25)+
  theme(plot.title = element_text(hjust = 0.5))+ylim(-.3,.25)+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

energyuse.plot<-plot_model(energyuse.cowyearregion, title="Energy Use", axis.title="Coefficient",line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil","freefairelect"), 
  labels=c("freefairelect"="Elections", "corecivil"="Civil Lib/Society", "polconiii"="Polit Constraints", "lngdpcapcurmean"="GDP per cap", 
  "I(lngdpcapcurmean^2)"="GDP per cap-sq","lntradegdp"="Trade Openness", "lnpopdens"="Popul Density"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+
  ylim(-.25,.4)+theme(plot.title = element_text(hjust = 0.5))

nonrenewables.plot<-plot_model(nonrenewables.cowyearregion, title="Nonrenewable Energy Use",axis.title="Coefficient", line.size=.5,dot.size =1.5) +
  scale_x_discrete(limits=c("lnpopdens","lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil","freefairelect"))+theme_bw() + 
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+ylim(-.2,.4)

nonprotected.plot<-plot_model(nonprotected.cowyearregion, title="Land Non-Protection", axis.title="Coefficient", line.size=.5,dot.size =1.5) +
  scale_x_discrete(limits=c("lnpopdens","lntradegdp","I(lngdpcapcurmean^2)", "lngdpcapcurmean","polconiii","corecivil10","freefairelect"))+theme_bw() + 
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+
  geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())+ylim(-.5,.35)

grid.arrange(ggtotal.plot, so2.plot, nox.plot, energyuse.plot, nonrenewables.plot, nonprotected.plot, nrow=2, widths=c(1.35,1,1))
##########################################################################TABLE 2A
ggtotal.cowyearregion<- lmer(lnggtotalcapnplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil*lnmanufgdpmean+polconiii+lnpopdens+lntradegdp+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) ; summary(ggtotal.cowyearregion)  
so2.cowyearregion<- lmer(lnso2capnplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil*lnmanufgdpmean+polconiii+lnpopdens+lntradegdp+(1|year)+(1|cowcode)+(1|region2), data = lmm.data)  ; summary(so2.cowyearregion)
nox.cowyearregion<- lmer(lnnoxcapnplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil*lnmanufgdpmean+polconiii+lnpopdens+lntradegdp+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data)  ; summary(nox.cowyearregion)
energyuse.cowyearregion<- lmer(lnenergyusenplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil*lnindustrymean+polconiii+lnpopdens+lntradegdp+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data)  ; summary(energyuse.cowyearregion)
nonrenewables.cowyearregion<- lmer(lnnonrenewablesnplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil*lnindustrymean+polconiii+lnpopdens+lntradegdp+ (1|year)+ (1|cowcode)+(1|region2), data = lmm.data)  ; summary(nonrenewables.cowyearregion)
nonprotected.cowyearregion<- lmer(lnnonprotectednplus1 ~freefairelect*lngdpcapcurmean+freefairelect*I(lngdpcapcurmean^2)+corecivil10*lnindustrymean+polconiii+lnpopdens+lntradegdp+ (1|year)+ (1|cowcode)+(1|region2), data = lmm.data)  ; summary(nonprotected.cowyearregion)
tab_model(ggtotal.cowyearregion, so2.cowyearregion, nox.cowyearregion, energyuse.cowyearregion, nonrenewables.cowyearregion, nonprotected.cowyearregion, show.se=FALSE, show.ci=FALSE, digits=3)
##########################################################################FIGURE 2
marginsfreefair.ggtotal.cowyearregion<-summary(margins(ggtotal.cowyearregion, at = list(lngdpcapcurmean =c(-3.97,-3.5,-3.25,-3,-2.75,-2.5,
 -2.25,-2,-1.75,-1.5,-1.25,-1,-.75,-.5,-.25,0,.25,.5,.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4,4.25,4.5,4.6)), variables = "freefairelect"))
ggtotalplotfreefair<-ggplot(marginsfreefair.ggtotal.cowyearregion, title="Greenhouse Gas Emissions", aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ylab("Marginal Effect, Electoral Accountability")+ xlab("GDP per Capita ")+ theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+geom_hline(yintercept=0, color="blue")+
  ggtitle("Greenhouse Gas Emissions")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220), 
  labels=c("100","500","2.5K", "10K","50K", "125K"))

marginsfreefair.so2.cowyearregion<-summary(margins(so2.cowyearregion, at = list(lngdpcapcurmean = c(-3.97,-3.5,-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,1.5, 2.5,3,3.5,
  4,4.5,4.6)), variables = "freefairelect"))
so2plotfreefair<-ggplot(marginsfreefair.so2.cowyearregion, aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), 
  color="grey30", linetype="dashed")+geom_line(aes(y=upper), color="grey30", linetype="dashed")+theme(axis.title.y=element_blank())+ xlab("GDP per Capita ")+ theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+geom_hline(yintercept=0, color="blue")+
  ggtitle("Sulfur Dioxide Emissions")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220), 
  labels=c("100","500","2.5K", "10K","50K", "125K"))

marginsfreefair.nox.cowyearregion<-summary(margins(nox.cowyearregion, at = list(lngdpcapcurmean = c(-3.97,-3.5,-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,1.5, 2.5,3,3.5,
  4,4.5,4.6)), variables = "freefairelect"))
noxplotfreefair<-ggplot(marginsfreefair.nox.cowyearregion, aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+theme(axis.title.y=element_blank())+ xlab("GDP per Capita ")+ theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+geom_hline(yintercept=0, color="blue")+
  ggtitle("Nitrogen Oxide Emissions")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220), 
  labels=c("100","500","2.5K", "10K","50K", "125K"))

marginsfreefair.energyuse.cowyearregion<-summary(margins(energyuse.cowyearregion, at = list(lngdpcapcurmean = c(-3.97,-3.5,-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,
  1.5, 2.5,3,3.5,4,4.5,4.6)), variables = "freefairelect"))
energyuseplotfreefair<-ggplot(marginsfreefair.energyuse.cowyearregion, aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", 
  linetype="dashed")+ geom_line(aes(y=upper), color="grey30", linetype="dashed")+ylab("Marginal Effect, Electoral Accountability")+ xlab("GDP per Capita ")+ theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+geom_hline(yintercept=0, color="blue")+
  ggtitle("Energy Use")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220), 
  labels=c("100","500","2.5K", "10K","50K", "125K"))

marginsfreefair.nonrenewables.cowyearregion<-summary(margins(nonrenewables.cowyearregion, at = list(lngdpcapcurmean = c(-3.97,-3.5,-3,-2.5,-2,-1.5,-1, -.75,
  -.5,-.25,0,.25,.5,.75,1,1.25,1.5,1.75,2, 2.25, 2.5,3,3.5,4,4.5,4.6)), variables = "freefairelect"))
nonrenewablesplotfreefair<-ggplot(marginsfreefair.nonrenewables.cowyearregion, aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+theme(axis.title.y=element_blank())+ xlab("GDP per Capita ")+ theme(panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+geom_hline(yintercept=0, color="blue")+
  ggtitle("Nonrenewables Use")+theme(plot.title = element_text(hjust = 0.5))+scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220),
  labels=c("100","500","2.5K", "10K","50K", "125K"))

marginsfreefair.nonprotected.cowyearregion<-summary(margins(nonprotected.cowyearregion, at = list(lngdpcapcurmean = c(-3.97,-3.5,-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,
  1.5, 2.5,3,3.5,4,4.5,4.6)), variables = "freefairelect"))
nonprotectedplotfreefair<-ggplot(marginsfreefair.nonprotected.cowyearregion, aes(x=lngdpcapcurmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), 
  color="grey30", linetype="dashed")+ geom_line(aes(y=upper), color="grey30", linetype="dashed")+theme(axis.title.y=element_blank())+ xlab("GDP per Capita ")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+
  geom_hline(yintercept=0, color="blue")+ ggtitle("Land Non-Protection")+theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=c(-2.911, -1.301, .308, 1.695, 3.304, 4.220), labels=c("100","500","2.5K", "10K","50K", "125K"))

grid.arrange(ggtotalplotfreefair,so2plotfreefair, noxplotfreefair, energyuseplotfreefair, nonrenewablesplotfreefair, nonprotectedplotfreefair, ncol = 3)
###################################################################################FIGURE 3
marginscore.ggtotal.cowyearregion<-summary(margins(ggtotal.cowyearregion, at = list(lnmanufgdpmean = c(-2,-1.467,-1,0,1,2, 2.112)), variables = "corecivil"))
ggtotalplotcore<-ggplot(marginscore.ggtotal.cowyearregion, aes(x=lnmanufgdpmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ylab("Marginal Effect, Civil Lib/Society Protections")+ xlab("Manufacturing as % of GDP")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+ggtitle("Greenhouse Gas Emissions")+
  theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=0, color="blue")+scale_x_continuous(breaks=c(-1.467,-.773, -.080,.613, 1.01, 1.306, 1.711, 1.999),
  labels=c("2.5","5","10","20","30","40","60", "80"))

marginscore.so2.cowyearregion<-summary(margins(so2.cowyearregion, at = list(lnmanufgdpmean = c(-2,-1.467,-1,0,1,2, 2.112)), variables = "corecivil"))
so2plotcore<-ggplot(marginscore.so2.cowyearregion, aes(x=lnmanufgdpmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ xlab("Manufacturing as % of GDP")+  theme(axis.title.y=element_blank())+ theme(panel.grid.major = element_blank(),          
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+ggtitle("Sulfur Dioxide Emissions")+
  theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=0, color="blue")+scale_x_continuous(breaks=c(-1.467,-.773, -.080,.613, 1.01, 1.306, 1.711, 1.999),
  labels=c("2.5","5","10","20","30","40","60", "80"))

marginscore.nox.cowyearregion<-summary(margins(nox.cowyearregion, at = list(lnmanufgdpmean = c(-2,-1.467,-1,-.75,-.5,-.25,0,.25,.5,.75,1,2, 2.112)), variables = "corecivil"))
noxplotcore<-ggplot(marginscore.nox.cowyearregion, aes(x=lnmanufgdpmean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ xlab("Manufacturing as % of GDP")+  theme(axis.title.y=element_blank())+ theme(panel.grid.major = element_blank(),                             
  panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+ggtitle("Nitrogen Oxide Emissions")+
  theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=0, color="blue")+scale_x_continuous(breaks=c(-1.467,-.773, -.080,.613, 1.01, 1.306, 1.711, 1.999),
  labels=c("2.5","5","10","20","30","40","60", "80"))

margins.energyusecore.cowyearregion<-summary(margins(energyuse.cowyearregion, at = list(lnindustrymean = c(-2.72,-2,-1.5,-1,-.5, 0,.5,1,1.32)), variables = "corecivil"))
energyuseplotcore<-ggplot(margins.energyusecore.cowyearregion, aes(x=lnindustrymean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ylab("Marginal Effect, Civil Lib/Society Protections")+ xlab("Manufacturing as % of GDP")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+ggtitle("Energy Use")+
  theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=0, color="blue")+scale_x_continuous(breaks=c(-2.272,-1.579, -.886, -.193,.213, .500,.906, 1.193),
  labels=c("2.5","5","10","20","30","40","60", "80"))

margins.nonrenewablescore.cowyearregion<-summary(margins(nonrenewables.cowyearregion, at = list(lnindustrymean = c(-2.72,-2,-1.5,-1,-.5, 0,.5,1,1.32)), variables = "corecivil"))
nonrenewablesplotcore<-ggplot(margins.nonrenewablescore.cowyearregion, aes(x=lnindustrymean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", 
  linetype="dashed")+geom_line(aes(y=upper), color="grey30", linetype="dashed")+ylab("Marginal Effect of Civil Society Protections")+ xlab("Manufacturing as % of GDP")+  
  theme(axis.title.y=element_blank())+ theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), panel.background = element_blank(), 
  axis.line = element_line(color = "grey50"))+ggtitle("Nonrenewable Energy Use")+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept=0, color="blue")+
  scale_x_continuous(breaks=c(-2.272,-1.579, -.886, -.193,.213, .500,.906, 1.193), labels=c("2.5","5","10","20","30","40","60", "80"))

margins.nonprotectedcore.cowyearregion<-summary(margins(nonprotected.cowyearregion, at = list(lnindustrymean = c(-2.72,-2,-1.5,-1,-.5, 0,.5,1,1.32)), 
  variables = "corecivil10"))
nonprotectedplotcore<-ggplot(margins.nonprotectedcore.cowyearregion, aes(x=lnindustrymean))+geom_line(aes(y =AME), color = "grey30")+ geom_line(aes(y=lower), color="grey30", linetype="dashed")+
  geom_line(aes(y=upper), color="grey30", linetype="dashed")+ xlab("Manufacturing as % of GDP")+  theme(axis.title.y=element_blank())+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "grey50"))+
  ggtitle("Land Non-Protection")+theme(plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0, color="blue")+scale_x_continuous(breaks=c(-2.272,-1.579, -.886, -.193,.213, .500,.906, 1.193),  
  labels=c("2.5","5","10","20","30","40","60", "80"))

grid.arrange(ggtotalplotcore,so2plotcore, noxplotcore, energyuseplotcore, nonrenewablesplotcore, nonprotectedplotcore, ncol = 3)
############################################################FIGURE4A
ggtotal.plot<-plot_model(ggtotal.cowyearregion, title="Greenhouse Gas Emissions", axis.title="Coefficient",line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil:lnmanufgdpmean","lnmanufgdpmean", "corecivil", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
  "lngdpcapcurmean","freefairelect"), labels=c("freefairelect"="Elections", "lngdpcapcurmean"="GDP per cap", "I(lngdpcapcurmean^2)"="GDP per cap-sq",
  "freefairelect:lngdpcapcurmean"="Elections*GDP per cap", "freefairelect:I(lngdpcapcurmean^2)"="Elections*GDP per cap-sq", "corecivil"="Civ Lib/Society",
  "corecivil:lnmanufgdpmean"="Civ Lib/Society*Manuf/GDP", "lnmanufgdpmean"="Manuf/GDP", "polconiii"="Polit Constraints", "lntradegdp"="Trade Openness", 
  "lnpopdens"="Pop Density"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+
  theme(panel.grid.major = element_blank())+theme(axis.ticks = element_blank())+theme(plot.title = element_text(hjust = 0.5))+ylim(-.25,.25)

so2.plot<-plot_model(so2.cowyearregion, title="Sulfur Dioxide Emissions", axis.title="Coefficient", line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil:lnmanufgdpmean","lnmanufgdpmean", "corecivil", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
  "lngdpcapcurmean","freefairelect"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+ylim(-.3,1)+
  theme(axis.text.y=element_blank())

nox.plot<-plot_model(nox.cowyearregion, title="Nitrogen Oxide Emissions", axis.title="Coefficient", line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil:lnmanufgdpmean","lnmanufgdpmean", "corecivil", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
  "lngdpcapcurmean","freefairelect"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+ylim(-.4,.3)+
  theme(axis.text.y=element_blank())

energyuse.plot<-plot_model(energyuse.cowyearregion, title="Energy Use", axis.title="Coefficient",line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil:lnindustrymean","lnindustrymean", "corecivil", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
"lngdpcapcurmean","freefairelect"), labels=c("freefairelect"="Elections", "lngdpcapcurmean"="GDP per cap", "I(lngdpcapcurmean^2)"="GDP per cap-sq",
 "freefairelect:lngdpcapcurmean"="Elections*GDP per cap", "freefairelect:I(lngdpcapcurmean^2)"="Elections*GDP per cap-sq", "corecivil"="Civ Lib/Society",
 "corecivil:lnindustrymean"="Civ Lib/Society*Manuf/GDP", "lnindustrymean"="Manuf/GDP", "polconiii"="Polit Constraints", "lntradegdp"="Trade Openness", 
 "lnpopdens"="Pop Density"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+ylim(-.6,.7)

nonrenewables.plot<-plot_model(nonrenewables.cowyearregion, title="Nonrenewable Energy Use", axis.title="Coefficient", line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil:lnindustrymean","lnindustrymean", "corecivil", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
 "lngdpcapcurmean","freefairelect"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+ylim(-.2,.4)+
  theme(axis.text.y=element_blank())

nonprotected.plot<-plot_model(nonprotected.cowyearregion, title="Land Non-Protection", axis.title="Coefficient", line.size=.5,dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnpopdens","lntradegdp", "polconiii","corecivil10:lnindustrymean","lnindustrymean", "corecivil10", "freefairelect:I(lngdpcapcurmean^2)", "freefairelect:lngdpcapcurmean","I(lngdpcapcurmean^2)", 
  "lngdpcapcurmean","freefairelect"))+theme_bw() +theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), panel.border = element_blank())+ geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+ylim(-.7,.8)+
  theme(axis.text.y=element_blank())

grid.arrange(ggtotal.plot, so2.plot, nox.plot, energyuse.plot, nonrenewables.plot, nonprotected.plot, nrow=2, widths=c(1.3,1,1))

#################################################TABLE 3A
ggtotalchange <- lmer(skewggtotalvolnplus1~polconiii+freefairelectvol + corecivilvol+ 
lngdpgrowvol+lnpopdensvol+lnggtotalcap+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

so2change <- lmer(skewso2volnplus1~polconiii+freefairelectvol + corecivilvol+ 
  lngdpgrowvol+lnpopdensvol+lnso2cap+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

noxchange <- lmer(skewnoxvolnplus1~polconiii+freefairelectvol + corecivilvol+ 
  lngdpgrowvol+lnpopdensvol+lnnoxcap+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

energyusechange <- lmer(skewenergyusevolnplus1~polconiii+freefairelectvol + corecivilvol+ 
  lngdpgrowvol+lnpopdensvol+lnenergyuse+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

nonrenewableschange <- lmer(skewnonrenewablesvolnplus1~polconiii+freefairelectvol + corecivilvol+ 
  lngdpgrowvol+lnpopdensvol+lnnonrenewables+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

nonprotectedchange <- lmer(lnnonprotectedvolnplus1~polconiii+freefairelectvol + corecivilvol+ 
  lngdpgrowvol+lnpopdensvol+lnnonprotected+(1|year)+ (1|cowcode)+(1|region2), data = lmm.data) 

tab_model(ggtotalchange, so2change, noxchange, energyusechange, nonrenewableschange, nonprotectedchange, 
          show.se=FALSE, show.ci=FALSE, digits=3)
############################################################FIGURE 3A
ggtotalchange.plot<-plot_model(ggtotalchange, title="Greenhouse Gas Emissions Change", axis.title="Coefficient",line.size=.5,
  dot.size =1.5)+ scale_x_discrete(limits=c("lnggtotalcap","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
  labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnggtotalcap"="GG Emissions per Capita"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-.75,.5)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

so2change.plot<-plot_model(so2change, title="Sulfur Dioxide Emissions Change", axis.title="Coefficient",line.size=.5, dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnso2cap","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
  labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnso2cap"="SO2 Emissions per Capita"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-.45,.4)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

noxchange.plot<-plot_model(noxchange, title="Nitrogen Oxides Emissions Change", axis.title="Coefficient",line.size=.5, dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnnoxcap","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
  labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnnoxcap"="NOx Emissions per Capita"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-1.05,.5)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

energyusechange.plot<-plot_model(energyusechange, title="Energy Use Change", axis.title="Coefficient",line.size=.5, dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnenergyuse","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
  labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnenergyuse"="Energy Use per Capita"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-.6,.8)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

nonrenewableschange.plot<-plot_model(nonrenewableschange, title="Non-Renewable Use Change", axis.title="Coefficient",line.size=.5, dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnnonrenewables","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
 labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnnonrenewables"="Non-Renewable Use"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-1,.6)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

nonprotectedchange.plot<-plot_model(nonprotectedchange, title="Land Non-Protection Change", axis.title="Coefficient",line.size=.5, dot.size =1.5)+ 
  scale_x_discrete(limits=c("lnnonprotected","lnpopdensvol", "lngdpgrowvol", "corecivilvol","freefairelectvol", "polconiii"), 
  labels=c("polconiii"="Political Constraints", "freefairelectvol"="Free/Fair Elections Change", "corecivilvol"="Civil Lib/Society Change",
  "lngdpgrowvol"="GDP Growth", "lnpopdensvol"="Population Density Change", "lnnonprotected"="Land Non-Protection"))+theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.3), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.border = element_blank())+geom_hline(yintercept = 0,  color = "black", size=.3,lty=2)+theme(panel.grid.major = element_blank())+
  theme(axis.ticks = element_blank())+ylim(-3,3.3)+theme(plot.title = element_text(hjust = 0.5))+theme(plot.title = element_text(size=11))

grid.arrange(ggtotalchange.plot, so2change.plot, noxchange.plot, energyusechange.plot, nonrenewableschange.plot, nonprotectedchange.plot, nrow=2)