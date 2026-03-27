setwd("/Users/dwhill/Documents/human rights/diss/pi concept")
library(foreign)
library(MASS)
library(mgcv)

the.data<-read.dta(file="ciri_polity_aclp_pwt_jop.dta")

### recode mising CIRI values
for (i in 5:9){
	the.data[,i]<-ifelse(the.data[,i]<0,NA,the.data[,i])
}

### get rid of weird parcomp 0 value
the.data$parcomp<-ifelse(the.data$parcomp==0,NA,the.data$parcomp)

### natural logs of GDP/capita and population

the.data$lngdp<-log(the.data$rgdpl)
the.data$lnpop<-log(the.data$POP)

### create binary version of components

the.data$polpris_bin<-as.numeric(the.data$polpris>1)
the.data$tort_bin<-as.numeric(the.data$tort>1)
the.data$kill_bin<-as.numeric(the.data$kill>1)
the.data$disap_bin<-as.numeric(the.data$disap>1)

### factors for ordered logit models

the.data$kill_ord<-as.factor(the.data$kill)
the.data$disap_ord<-as.factor(the.data$disap)
the.data$tort_ord<-as.factor(the.data$tort)
the.data$polpris_ord<-as.factor(the.data$polpris)

### standardized (0 to 1) polity variables 
the.data$xropen_std<-the.data$xropen/max(the.data$xropen,na.rm=T)
the.data$xconst_std<-the.data$xconst/max(the.data$xconst,na.rm=T)
the.data$xrcomp_std<-the.data$xrcomp/max(the.data$xrcomp,na.rm=T)
the.data$parcomp_std<-the.data$parcomp/max(the.data$parcomp,na.rm=T)

the.data.nona<-na.omit(the.data)
dim(the.data.nona)

### ordered logits

ciri.ol.mods<-list()
for (i in 1:4){
		ciri.ol.mods[[i]]<-polr(the.data.nona[,i+27]~democracy+lngdp+lnpop+injud,data=the.data.nona,method=c("logistic"))
}

ciri.ol.mods.2<-list()
for (i in 1:4){
		ciri.ol.mods.2[[i]]<-polr(the.data.nona[,i+27]~xropen_std+lngdp+lnpop+injud,data=the.data.nona,method=c("logistic"))
}

ciri.ol.mods.3<-list()
for (i in 1:4){
		ciri.ol.mods.3[[i]]<-polr(the.data.nona[,i+27]~xrcomp_std+lngdp+lnpop+injud,data=the.data.nona,method=c("logistic"))
}

ciri.ol.mods.4<-list()
for (i in 1:4){
		ciri.ol.mods.4[[i]]<-polr(the.data.nona[,i+27]~xconst_std+lngdp+lnpop+injud,data=the.data.nona,method=c("logistic"))
}

ciri.ol.mods.5<-list()
for (i in 1:4){
		ciri.ol.mods.5[[i]]<-polr(the.data.nona[,i+27]~parcomp_std+lngdp+lnpop+injud,data=the.data.nona,method=c("logistic"))
}

dude<-matrix(nrow=20,ncol=3)
for (i in 1:4){
	dude[i,]<-coef(summary(ciri.ol.mods[[i]]))[1,]
	dude[i+4,]<-coef(summary(ciri.ol.mods.2[[i]]))[1,]
	dude[i+8,]<-coef(summary(ciri.ol.mods.3[[i]]))[1,]
	dude[i+12,]<-coef(summary(ciri.ol.mods.4[[i]]))[1,]
	dude[i+16,]<-coef(summary(ciri.ol.mods.5[[i]]))[1,]
}

coef.ests<-cbind(dude[,1],dude[,1]-1.96*dude[,2],dude[,1]+1.96*dude[,2])

coefs<-matrix(nrow=29,ncol=3)
coefs[1:4,]<-coef.ests[1:4,]
coefs[7:10,]<-coef.ests[5:8,]
coefs[13:16,]<-coef.ests[9:12,]
coefs[19:22,]<-coef.ests[13:16,]
coefs[25:28,]<-coef.ests[17:20,]

pol.labs<-c("ACLP", "xropen","xrcomp","xconst","parcomp")
ciri.labs<-c("Killing","Disap.","Torture","Pol. Pris.")

pdf(width=7, height=8, file="polity_coefs.pdf")
#dev.new(width=7,height=8)
par(mar=c(2, 4.75, 2, 2))
plot(coefs[,1], seq(1:dim(coefs)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(-1,5), main="Ordered Logit Coefficients")
abline(v=c(-1, 1, 2, 3, 4, 5), lty=3, lwd=2, col="grey")
abline(v=0, lty=3, lwd=2)
points(coefs[,1], seq(1:dim(coefs)[1]), pch=16, cex=1.5)
for (i in 1:dim(coefs)[1]){
	lines(coefs[i,2:3], c(i, i), lwd=1.5)
	}
axis(2, labels=ciri.labs, at=1:4, las=1)
axis(2, labels=ciri.labs, at=7:10, las=1)
axis(2, labels=ciri.labs, at=13:16, las=1)
axis(2, labels=ciri.labs, at=19:22, las=1)
axis(2, labels=ciri.labs, at=25:28, las=1)
axis(2, labels=pol.labs, at=c(5, 11, 17, 23, 29), las=1, font=2, tick=F)
axis(1, at=seq(from=-1, to=5, by=1))
abline(h=6, lty=1, lwd=1, col="black")
box()
dev.off()

pol.col<-rgb(99,99,99, 150, maxColorValue=255)
dis.col<-rgb(217,217,217, 150, maxColorValue=255)

ciri_parcomp.table<-table(the.data.nona$physint,the.data.nona$parcomp)
prop.table(ciri_parcomp.table,2)
sum(prop.table(ciri_parcomp.table,2)[7:9,5])

### GAMs

### physint

ciri.gam<-gam(physint ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona)
democ<-0:10
ciri.preds<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

tor.col<-rgb(150,150,150, 150, maxColorValue=255)
kil.col<-rgb(189,189,189, 150, maxColorValue=255)

up.ci<-ciri.preds$fit+1.96*ciri.preds$se
lo.ci<-ciri.preds$fit-1.96*ciri.preds$se

pdf(file="ciri_democ_gam.pdf")
plot(democ, ciri.preds$fit, type="l", ylim=c(0,8), xlab="Democracy", ylab="Physical Integrity Index", lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci, rev(up.ci), lo.ci[1]), col=kil.col, border=NA)
abline(h=c(0, 2, 4, 6, 8), lty=3, lwd=2, col="grey")
dev.off()

### components

### models w/ democracy scale 

## linear models
ciri.gam<-gam(polpris ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.1<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(tort ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.2<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(disap ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.3<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(kill ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.4<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

up.ci.1<-ciri.preds.1$fit+1.96*ciri.preds.1$se
lo.ci.1<-ciri.preds.1$fit-1.96*ciri.preds.1$se

up.ci.2<-ciri.preds.2$fit+1.96*ciri.preds.1$se
lo.ci.2<-ciri.preds.2$fit-1.96*ciri.preds.1$se

up.ci.3<-ciri.preds.3$fit+1.96*ciri.preds.1$se
lo.ci.3<-ciri.preds.3$fit-1.96*ciri.preds.1$se

up.ci.4<-ciri.preds.4$fit+1.96*ciri.preds.1$se
lo.ci.4<-ciri.preds.4$fit-1.96*ciri.preds.1$se

democ<-0:10

pdf(file="components_democ_gam.pdf")
plot(democ, ciri.preds.1$fit, type="l", ylim=c(0,2.25), xlab="Democracy", ylab="Physical Integrity Components", lwd=2, axes=F)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.1, rev(up.ci.1), lo.ci.1[1]), col=pol.col, border=NA)

lines(democ, ciri.preds.2$fit, lty=2, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.2, rev(up.ci.2), lo.ci.2[1]), col=tor.col, border=NA)

lines(democ, ciri.preds.3$fit, lty=3, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.3, rev(up.ci.3), lo.ci.3[1]), col=kil.col, border=NA)

lines(democ, ciri.preds.4$fit, lty=4, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.4, rev(up.ci.4), lo.ci.4[1]), col=dis.col, border=NA)

abline(h=c(0,1,2),lty=3,lwd=2, col="grey")
axis(1, at=seq(from=0,to=10, by=2))
axis(2, at=c(0, 1, 2))
legend("topleft", c("Imprisonment", "Torture", "Killing", "Disappearance"), lty=c(1, 2, 3, 4), cex=0.9, bg="white")
box()
dev.off()

## binary models

ciri.gam<-gam(polpris_bin ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.1<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(tort_bin ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.2<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(disap_bin ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.3<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(kill_bin ~ s(democ, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.4<-predict(ciri.gam, newdata=data.frame(democ=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

up.ci.1<-ciri.preds.1$fit+1.96*ciri.preds.1$se
lo.ci.1<-ciri.preds.1$fit-1.96*ciri.preds.1$se

up.ci.2<-ciri.preds.2$fit+1.96*ciri.preds.1$se
lo.ci.2<-ciri.preds.2$fit-1.96*ciri.preds.1$se

up.ci.3<-ciri.preds.3$fit+1.96*ciri.preds.1$se
lo.ci.3<-ciri.preds.3$fit-1.96*ciri.preds.1$se

up.ci.4<-ciri.preds.4$fit+1.96*ciri.preds.1$se
lo.ci.4<-ciri.preds.4$fit-1.96*ciri.preds.1$se

pdf(file="components_democ_logit_gam.pdf")
plot(democ, ciri.preds.1$fit, type="l", ylim=c(-3,3), xlab="Democracy", ylab="Log Odds of No Abuse", lwd=2, axes=F)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.1, rev(up.ci.1), lo.ci.1[1]), col=pol.col, border=NA)

lines(democ, ciri.preds.2$fit, lty=2, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.2, rev(up.ci.2), lo.ci.2[1]), col=tor.col, border=NA)

lines(democ, ciri.preds.3$fit, lty=3, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.3, rev(up.ci.3), lo.ci.3[1]), col=kil.col, border=NA)

lines(democ, ciri.preds.4$fit, lty=4, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.4, rev(up.ci.4), lo.ci.4[1]), col=dis.col, border=NA)

abline(h=c(-2, 0, 2, 4), lty=3, lwd=2, col="grey")
axis(1, at=seq(from=0, to=10, by=2))
axis(2, at=seq(from=-2,to=4,by=2))
legend("topleft", c("Imprisonment", "Torture", "Killing", "Disappearance"), lty=c(1, 2, 3, 4), cex=0.9, bg="white")
box()
dev.off()

### models w/ parcomp

## linear models
ciri.gam<-gam(polpris ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona)
democ<-1:5
ciri.preds.1<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(tort ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.2<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(disap ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.3<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(kill ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona)
ciri.preds.4<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

up.ci.1<-ciri.preds.1$fit+1.96*ciri.preds.1$se
lo.ci.1<-ciri.preds.1$fit-1.96*ciri.preds.1$se

up.ci.2<-ciri.preds.2$fit+1.96*ciri.preds.1$se
lo.ci.2<-ciri.preds.2$fit-1.96*ciri.preds.1$se

up.ci.3<-ciri.preds.3$fit+1.96*ciri.preds.1$se
lo.ci.3<-ciri.preds.3$fit-1.96*ciri.preds.1$se

up.ci.4<-ciri.preds.4$fit+1.96*ciri.preds.1$se
lo.ci.4<-ciri.preds.4$fit-1.96*ciri.preds.1$se

pdf(file="components_parcomp_gam.pdf")
plot(democ, ciri.preds.1$fit, type="l", ylim=c(0,2.25), xlab="Parcomp", ylab="Physical Integrity Components", lwd=2, axes=F)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.1, rev(up.ci.1), lo.ci.1[1]), col=pol.col, border=NA)

lines(democ, ciri.preds.2$fit, lty=2, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.2, rev(up.ci.2), lo.ci.2[1]), col=tor.col, border=NA)

lines(democ, ciri.preds.3$fit, lty=3, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.3, rev(up.ci.3), lo.ci.3[1]), col=kil.col, border=NA)

lines(democ, ciri.preds.4$fit, lty=4, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.4, rev(up.ci.4), lo.ci.4[1]), col=dis.col, border=NA)

abline(h=c(0,1,2),lty=3,lwd=2,col="grey")
axis(1, at=c(1, 2, 3, 4, 5))
axis(2, at=c(0,1,2))
legend("topleft", c("Imprisonment", "Torture", "Killing", "Disappearance"), lty=c(1, 2, 3, 4), cex=0.9, bg="white")
box()
dev.off()

## binary models 
ciri.gam<-gam(polpris_bin ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.1<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(tort_bin ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.2<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(disap_bin ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.3<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

ciri.gam<-gam(kill_bin ~ s(parcomp, k=5) + lngdp + lnpop + injud, data=the.data.nona, family="binomial")
ciri.preds.4<-predict(ciri.gam, newdata=data.frame(parcomp=democ, lngdp=mean(the.data.nona$lngdp), lnpop=mean(the.data.nona$lnpop), injud=mean(the.data.nona$injud)), se.fit=T)

up.ci.1<-ciri.preds.1$fit+1.96*ciri.preds.1$se
lo.ci.1<-ciri.preds.1$fit-1.96*ciri.preds.1$se

up.ci.2<-ciri.preds.2$fit+1.96*ciri.preds.1$se
lo.ci.2<-ciri.preds.2$fit-1.96*ciri.preds.1$se

up.ci.3<-ciri.preds.3$fit+1.96*ciri.preds.1$se
lo.ci.3<-ciri.preds.3$fit-1.96*ciri.preds.1$se

up.ci.4<-ciri.preds.4$fit+1.96*ciri.preds.1$se
lo.ci.4<-ciri.preds.4$fit-1.96*ciri.preds.1$se

pdf(file="components_parcomp_logit_gam.pdf")
plot(democ, ciri.preds.1$fit, type="l", ylim=c(-3,3), xlab="Competition", ylab="Log Odds of No Abuse", lwd=2, axes=F)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.1, rev(up.ci.1), lo.ci.1[1]), col=pol.col, border=NA)

lines(democ, ciri.preds.2$fit, lty=2, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.2, rev(up.ci.2), lo.ci.2[1]), col=tor.col, border=NA)

lines(democ, ciri.preds.3$fit, lty=3, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.3, rev(up.ci.3), lo.ci.3[1]), col=kil.col, border=NA)

lines(democ, ciri.preds.4$fit, lty=4, lwd=2)
polygon(c(democ, rev(democ), democ[1]), c(lo.ci.4, rev(up.ci.4), lo.ci.4[1]), col=dis.col, border=NA)

abline(h=c(-2, 0, 2, 4), lty=3, lwd=2, col="grey")
axis(1, at=c(1,2,3,4,5))
axis(2, at=c(-2, 0, 2, 4))
legend("topleft", c("Imprisonment", "Torture", "Killing", "Disappearance"), lty=c(1, 2, 3, 4), cex=0.9, bg="white")
box()
dev.off()


