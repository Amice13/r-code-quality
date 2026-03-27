#Replication Code for Brian Palmer-Rubin and Ruth Berins Collier, "Work and Demand Making: Productionist and Consumptionist Politics in Latin America," Comparative Political Studies

#Analysis conducted in R version 4.0.5

#install the following packages
library(ggplot2)
library(Hmisc)
library(stargazer)
library(cowplot)

#Set working directory:
#setwd(".../CPS Replication Materials")

load("repdatCPS.RData")

names(repdat)


########## Table 1: Frequency of Different Types of Demand Making by Country (percent of respondents)

prop.table(table(repdat$COUNTRY,repdat$wrkplcpsalldum1),1)
prop.table(table(repdat$COUNTRY,repdat$prodclm1),1)
prop.table(table(repdat$COUNTRY,repdat$consclm),1)
prop.table(table(repdat$COUNTRY,repdat$polsclm),1)

repdat$anyclm<-0
repdat$anyclm[repdat$prodclm1==1]<-1
repdat$anyclm[repdat$consclm==1]<-1
repdat$anyclm[repdat$polsclm==1]<-1

table(repdat$anyclm)

prop.table(table(repdat$COUNTRY,repdat$anyclm),1)


prop.table(table(repdat$wrkplcpsalldum1))
prop.table(table(repdat$prodclm1))
prop.table(table(repdat$consclm))
prop.table(table(repdat$polsclm))
prop.table(table(repdat$anyclm))



########## Table 2: Traits of Work and Workers, Country Averages

# Work Network
tapply(repdat$Work.Network,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Work.Network,na.rm=T)

# Union Access
tapply(repdat$Union.Access,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Union.Access,na.rm=T)

# Job Instability
tapply(repdat$Job.Instability,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Job.Instability,na.rm=T)

# Income Volatility
tapply(repdat$Income.Volatility,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Income.Volatility,na.rm=T)

# Contract
tapply(repdat$Contract,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Contract,na.rm=T)

# Social Security
tapply(repdat$Social.Sec,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Social.Sec,na.rm=T)

# Secondary Education

tapply(repdat$sec.comp,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$sec.comp,na.rm=T)

# Union Experience

tapply(repdat$Union.Experience,repdat$COUNTRY,mean, na.rm=T)
mean(repdat$Union.Experience,na.rm=T)

# N
table(repdat$COUNTRY)

########## Table 3: Correlation Matrix of Work Traits

corvars<-c("Work.Network","Union.Access","Job.Instability","Income.Volatility","Contract","Social.Sec","Education","Union.Experience")

cordat<-repdat[corvars]

rcorr(as.matrix(cordat))



########## Table 4: Multi-Variate Models of Demand Making, Logistic Regressions

reg1fullFE<-glm(wrkplcpsalldum1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

reg2fullFE<-glm(prodclm1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

reg3fullFE<-glm(consclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

reg4fullFE<-glm(polsclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

coef.names<-c("Union Access","Work Network (log)","Income Volatility (log)","Job Instability","Contract","Social Security","Union Experience","Education","Female","Years in Residence","Age","Age$^{2}$","Chile","Peru","Venezuela")

stargazer(reg1fullFE,reg2fullFE,reg3fullFE,reg4fullFE,no.space=T,covariate.labels=coef.names)


########## Figures 1, 2 and 3: Marginal Effects Plots


# Using a version of the dataset excluding cases with NA's to comply with ggplot

repdatX<-repdat[!is.na(repdat$wrkplcpsalldum1),]
repdatX<-repdatX[!is.na(repdatX$prodclm1),]
repdatX<-repdatX[!is.na(repdatX$consclm),]
repdatX<-repdatX[!is.na(repdatX$polsclm),]
repdatX<-repdatX[!is.na(repdatX$Union.Access),]
repdatX<-repdatX[!is.na(repdatX$Work.Network),]
repdatX<-repdatX[!is.na(repdatX$Income.Volatility),]
repdatX<-repdatX[!is.na(repdatX$Job.Instability),]
repdatX<-repdatX[!is.na(repdatX$Contract),]
repdatX<-repdatX[!is.na(repdatX$Social.Sec),]
repdatX<-repdatX[!is.na(repdatX$Education),]
repdatX<-repdatX[!is.na(repdatX$Union.Experience),]
repdatX<-repdatX[!is.na(repdatX$Female),]
repdatX<-repdatX[!is.na(repdatX$Years.in.Res),]
repdatX<-repdatX[!is.na(repdatX$Age),]

reg1fullFEX<-glm(wrkplcpsalldum1~Union.Experience+Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial,weights=WEIGHT,data=repdatX)

reg2fullFEX<-glm(prodclm1~Union.Experience+Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial,weights=WEIGHT,data=repdatX)

reg3fullFEX<-glm(consclm~Union.Experience+Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial,weights=WEIGHT,data=repdatX)

reg4fullFEX<-glm(polsclm~Union.Experience+Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Education+Female+Years.in.Res+Age+AGESQRED+COUNTRY,family=binomial,weights=WEIGHT,data=repdatX)


# Creating individual plots for each IV-DV combination

# Work Network

a1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Work.Network)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Work Arena") +xlab(label="Work Network")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(0, 30) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5)) 

b1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Work.Network)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Productionist") +xlab(label="Work Network")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(0, 30) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

c1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Work.Network)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Consumptionist") +xlab(label="Work Network")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(0, 30) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

d1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Work.Network)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Political") +xlab(label="Work Network")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(0, 30) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

# Inc Vol

e1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Income.Volatility)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Work Arena") +xlab(label="Income Volatility")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 10) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

f1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Income.Volatility)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Productionist") +xlab(label="Income Volatility")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 10) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

g1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Income.Volatility)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Consumptionist") +xlab(label="Income Volatility")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 10) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

h1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Income.Volatility)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Political") +xlab(label="Income Volatility")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 10) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))


# Job Instability

i1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Job.Instability)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Work Arena") +xlab(label="Job Instability")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 5) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

j1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Job.Instability)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Productionist") +xlab(label="Job Instability")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 5) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

k1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Job.Instability)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Consumptionist") +xlab(label="Job Instability")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 5) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

l1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Job.Instability)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b") + ylab(label="Political") +xlab(label="Job Instability")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + xlim(1, 5) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))


# Union Access

m1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Union.Access)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Work Arena") +xlab(label="Union Access")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))  

n1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Union.Access)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Productionist") +xlab(label="Union Access")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1))  + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5)) 

o1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Union.Access)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Consumptionist") +xlab(label="Union Access")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))
  
  
p1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Union.Access)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Political") +xlab(label="Union Access")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))


# Social Security

q1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Social.Sec)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Work Arena") +xlab(label="Social Security")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1))  + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

r1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Social.Sec)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Productionist") +xlab(label="Social Security")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

s1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Social.Sec)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Consumptionist") +xlab(label="Social Security")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

t1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Social.Sec)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Political") +xlab(label="Social Security")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))


# Contracts

u1<-ggplot(data=repdatX,aes(y=fitted.values(reg1fullFEX),x=Contract)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Work Arena") +xlab(label="Contract")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

v1<-ggplot(data=repdatX,aes(y=fitted.values(reg2fullFEX),x=Contract)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Productionist") +xlab(label="Contract")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

w1<-ggplot(data=repdatX,aes(y=fitted.values(reg3fullFEX),x=Contract)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Consumptionist") +xlab(label="Contract")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))

x1<-ggplot(data=repdatX,aes(y=fitted.values(reg4fullFEX),x=Contract)) + geom_smooth(aes(linetype=COUNTRY,color=COUNTRY),method="lm")+ geom_rug(sides="b")+  ylab(label="Political") +xlab(label="Contract")+theme (axis.title=element_text(size=20), legend.position="none", plot.margin = margin(6, 0, 6, 0),legend.key.width = unit(2, 'cm'),legend.title = element_text(size=16), legend.text = element_text(size=16), axis.title.y = element_text(vjust=-3.5), axis.title.x=element_blank()) +  scale_colour_manual(values = c("black", "gray33","gray58","gray70")) + scale_x_continuous(breaks=seq(0,1,1)) + scale_y_continuous(breaks=seq(0,.5,.5)) + coord_cartesian(ylim=c(0, .5))



# Extracting legend to stick in cowplots
legend_country <- get_legend(
  a1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

######## Figure 1: Work Resources


resGrid1<-plot_grid(m1,n1,o1,p1,align="hv",axis="b",nrow=1) 

resGrid2<-plot_grid(a1,b1,c1,d1,align="hv",axis="b",nrow=1) 



title1 <- ggdraw() + draw_label("Marginal Effect of Union Access",
                                fontface = 'bold',
                                x = 0,
                                hjust = -1,size = 20)

title2 <- ggdraw() + draw_label("Marginal Effect of Work Network",
                                fontface = 'bold',
                                x = 0,
                                hjust = -1, size = 20)

resGrid1a<-plot_grid(title1,resGrid1,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


resGrid2a<-plot_grid(title2,resGrid2,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


pdf(file="./combinedPlotsByCountryResources.pdf",width=12,height=8)

plot_grid(resGrid1a,resGrid2a, legend_country, ncol = 1, rel_heights = c(1,1,.15))

dev.off()

######## Figure 2: Income-Based Insecurity

incGrid1<-plot_grid(e1,f1,g1,h1,align="hv",axis="b",nrow=1)

incGrid2<-plot_grid(q1,r1,s1,t1,align="hv",axis="b",nrow=1)

title3 <- ggdraw() + draw_label("Marginal Effect of Income Volatility",
                                fontface = 'bold',
                                x = 0,
                                hjust = -.9,size = 20)

title4 <- ggdraw() + draw_label("Marginal Effect of Social Security",
                                fontface = 'bold',
                                x = 0,
                                hjust = -1, size = 20)

incGrid1a<-plot_grid(title3,incGrid1,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

incGrid2a<-plot_grid(title4,incGrid2,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


pdf(file="./combinedPlotsByCountryIncome.pdf",width=12,height=8)

plot_grid(incGrid1a,incGrid2a, legend_country, ncol = 1, rel_heights = c(1,1,.15))

dev.off()

######## Figure 3: Job-Based Insecurity

jobsGrid1<-plot_grid(i1,j1,k1,l1,align="hv",axis="b",nrow=1)

jobsGrid2<-plot_grid(u1,v1,w1,x1,align="hv",axis="b",nrow=1)

title5 <- ggdraw() + draw_label("Marginal Effect of Job Instability",
                                fontface = 'bold',
                                x = 0,
                                hjust = -1,size = 20)

title6 <- ggdraw() + draw_label("Marginal Effect of Contract",
                                fontface = 'bold',
                                x = 0,
                                hjust = -1.3,size = 20)

jobsGrid1a<-plot_grid(title5,jobsGrid1,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

jobsGrid2a<-plot_grid(title6,jobsGrid2,ncol=1,rel_heights=c(.1,1)) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


pdf(file="./combinedPlotsByCountryJobs.pdf",width=12,height=8)

plot_grid(jobsGrid1a,jobsGrid2a, legend_country, ncol = 1, rel_heights = c(1,1,.15))

dev.off()



################################################################ 
######## Appendix 1: Bivariate Analysis


######## Table 1: Demand Making at Work, Bivariate Logistic Regressions


reg1a<-glm(wrkplcpsalldum1~Union.Access,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1b<-glm(wrkplcpsalldum1~Work.Network.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1c<-glm(wrkplcpsalldum1~Income.Volatility.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1d<-glm(wrkplcpsalldum1~Job.Instability,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1e<-glm(wrkplcpsalldum1~Contract,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1f<-glm(wrkplcpsalldum1~Social.Sec,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1g<-glm(wrkplcpsalldum1~Union.Experience,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg1h<-glm(wrkplcpsalldum1~Education,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

stargazer(reg1a,reg1b,reg1c,reg1d,reg1e,reg1f,reg1g,reg1h,no.space=T,covariate.labels=coef.names)


######## Table 2: Productionist Demand Making in the Interest Arena, Bivariate Logistic Regressions

reg2a<-glm(prodclm1~Union.Access,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2b<-glm(prodclm1~Work.Network.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2c<-glm(prodclm1~Income.Volatility.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2d<-glm(prodclm1~Job.Instability,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2e<-glm(prodclm1~Contract,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2f<-glm(prodclm1~Social.Sec,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2g<-glm(prodclm1~Union.Experience,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg2h<-glm(prodclm1~Education,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

stargazer(reg2a,reg2b,reg2c,reg2d,reg2e,reg2f,reg2g,reg2h,no.space=T,covariate.labels=coef.names)

######## Table 3: Consumptionist Demand Making in the Interest Arena, Bivariate Logistic Regressions

reg3a<-glm(consclm~Union.Access,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3b<-glm(consclm~Work.Network.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3c<-glm(consclm~Income.Volatility.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3d<-glm(consclm~Job.Instability,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3e<-glm(consclm~Contract,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3f<-glm(consclm~Social.Sec,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3g<-glm(consclm~Union.Experience,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg3h<-glm(consclm~Education,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

stargazer(reg3a,reg3b,reg3c,reg3d,reg3e,reg3f,reg3g,reg3h,no.space=T,covariate.labels=coef.names)


######## Table 4: Political Demand Making in the Interest Arena, Bivariate Logistic Regressions

reg4a<-glm(polsclm~Union.Access,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4b<-glm(polsclm~Work.Network.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4c<-glm(polsclm~Income.Volatility.log,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4d<-glm(polsclm~Job.Instability,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4e<-glm(polsclm~Contract,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4f<-glm(polsclm~Social.Sec,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4g<-glm(polsclm~Union.Experience,family=binomial(link="logit"),weights=WEIGHT,data=repdat)
reg4h<-glm(polsclm~Education,family=binomial(link="logit"),weights=WEIGHT,data=repdat)

stargazer(reg4a,reg4b,reg4c,reg4d,reg4e,reg4f,reg4g,reg4h,no.space=T,covariate.labels=coef.names)


################################################################ 
######## Appendix 2: Main Analysis Broken Down by Country

argdat<-repdat[repdat$COUNTRY=="Argentina",]
chiledat<-repdat[repdat$COUNTRY=="Chile",]
perudat<-repdat[repdat$COUNTRY=="Peru",]
vendat<-repdat[repdat$COUNTRY=="Venezuela",]
 

######## Table 5: Multi-Variate Models of Demand Making, Logistic Regressions (Argentina)

reg1fullArg<-glm(wrkplcpsalldum1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=argdat)

reg2fullArg<-glm(prodclm1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=argdat)

reg3fullArg<-glm(consclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=argdat)

reg4fullArg<-glm(polsclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=argdat)

stargazer(reg1fullArg,reg2fullArg,reg3fullArg,reg4fullArg,no.space=T,covariate.labels=coef.names)



######## Table 6: Multi-Variate Models of Demand Making, Logistic Regressions (Chile)

reg1fullCh<-glm(wrkplcpsalldum1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=chiledat)

reg2fullCh<-glm(prodclm1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=chiledat)

reg3fullCh<-glm(consclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=chiledat)

reg4fullCh<-glm(polsclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=chiledat)

stargazer(reg1fullCh,reg2fullCh,reg3fullCh,reg4fullCh,no.space=T,covariate.labels=coef.names)



######## Table 7: Multi-Variate Models of Demand Making, Logistic Regressions (Peru)

reg1fullPeru<-glm(wrkplcpsalldum1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=perudat)

reg2fullPeru<-glm(prodclm1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=perudat)

reg3fullPeru<-glm(consclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=perudat)

reg4fullPeru<-glm(polsclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=perudat)

stargazer(reg1fullPeru,reg2fullPeru,reg3fullPeru,reg4fullPeru,no.space=T,covariate.labels=coef.names)



######## Table 8: Multi-Variate Models of Demand Making, Logistic Regressions (Venezuela)

reg1fullVen<-glm(wrkplcpsalldum1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=vendat)

reg2fullVen<-glm(prodclm1~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=vendat)

reg3fullVen<-glm(consclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=vendat)

reg4fullVen<-glm(polsclm~Union.Access+Work.Network.log+Income.Volatility.log+Job.Instability+Contract+Social.Sec+Union.Experience+Education+Female+Years.in.Res+Age+AGESQRED,family=binomial(link="logit"),weights=WEIGHT,data=vendat)

stargazer(reg1fullVen,reg2fullVen,reg3fullVen,reg4fullVen,no.space=T,covariate.labels=coef.names)



