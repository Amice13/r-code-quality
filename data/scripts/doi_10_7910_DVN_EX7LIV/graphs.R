setwd("/Users/danielhill/Documents/torture/JPR R&R/empirics/replication")

library(countrycode)

tort.dat<-read.csv(file="estimation_sample.csv")
tort.dat<-data.frame(tort.dat[,1:13],tort.dat[,17:18])
tort.dat<-na.omit(tort.dat)

tort.dat$country<-countrycode(tort.dat$cowcode,origin="cown",destination="country.name")

##### stealth
meds.stealth<-tapply(tort.dat$stealth,tort.dat$cowcode,median)
lower.stealth<-tapply(tort.dat$stealth,tort.dat$cowcode,quantile,probs=c(0.25))
upper.stealth<-tapply(tort.dat$stealth,tort.dat$cowcode,quantile,probs=c(0.75))
bob<-data.frame(meds.stealth,lower.stealth,upper.stealth,unique(tort.dat$country))
colnames(bob)[4]<-c("country")
bob<-bob[order(bob[,1]),]

ylabels<-bob$country
bob1<-bob[1:56,]
bob2<-bob[57:112,]
ylabels1<-as.character(bob1$country)
ylabels2<-as.character(bob2$country)

ylabels1[25]<-"Central African Rep."
ylabels1[27]<-"Tanzania"
ylabels1[34]<-"Macedonia"

ylabels2[23]<-"Bolivia"
ylabels2[25]<-"DR Congo"
ylabels2[28]<-"Iran"
ylabels2[31]<-"Venezuela"
ylabels2[50]<-"Syria"

pdf(file="stealth_all.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 50), main="Top")
abline(v=c(0, 10, 20, 30, 40, 50), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=50, by=10))
box()

plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 5), main="Bottom")
abline(v=c(0, 1, 2, 3, 4, 5), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=5, by=1))
box()
dev.off()

##### scarring
meds.scar<-tapply(tort.dat$scar,tort.dat$cowcode,median)
lower.scar<-tapply(tort.dat$scar,tort.dat$cowcode,quantile,probs=c(0.25))
upper.scar<-tapply(tort.dat$scar,tort.dat$cowcode,quantile,probs=c(0.75))
bob<-data.frame(meds.scar,lower.scar,upper.scar,unique(tort.dat$country))
colnames(bob)[4]<-c("country")
bob<-bob[order(bob[,1]),]

ylabels<-bob$country
bob1<-bob[1:56,]
bob2<-bob[57:112,]
ylabels1<-as.character(bob1$country)
ylabels2<-as.character(bob2$country)

ylabels2[5]<-"Tanzania"
ylabels2[7]<-"Iran"
ylabels2[11]<-"Venezuela"
ylabels2[15]<-"Syria"
ylabels2[17]<-"Bolivia"
ylabels2[38]<-"Macedonia"
ylabels2[42]<-"Central African Rep."
ylabels2[46]<-"DR Congo"

pdf(file="scar_all.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 85), main="Top")
abline(v=c(0, 20, 40, 60, 80), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=80, by=20))
box()

plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 14), main="Bottom")
abline(v=c(0, 2, 4, 6, 8, 10, 12, 14), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=14, by=2), cex.axis=0.9)
box()
dev.off()

##### stealth torture descriptives by regime type
democs<-tort.dat[tort.dat$dem==1,]
autocs<-tort.dat[tort.dat$dem==0,]

meds.stealth<-tapply(democs$stealth,democs$cowcode,median)
lower.stealth<-tapply(democs$stealth,democs$cowcode,quantile,probs=c(0.25))
upper.stealth<-tapply(democs$stealth,democs$cowcode,quantile,probs=c(0.75))

bob1<-data.frame(meds.stealth,lower.stealth,upper.stealth,unique(democs$country))
colnames(bob1)[4]<-c("country")
bob1<-bob1[order(bob1[,1]),]
ylabels1<-as.character(bob1$country)

meds.stealth<-tapply(autocs$stealth,autocs$cowcode,median)
lower.stealth<-tapply(autocs$stealth,autocs$cowcode,quantile,probs=c(0.25))
upper.stealth<-tapply(autocs$stealth,autocs$cowcode,quantile,probs=c(0.75))

bob2<-data.frame(meds.stealth,lower.stealth,upper.stealth,unique(autocs$country))
colnames(bob2)[4]<-c("country")
bob2<-bob2[order(bob2[,1]),]
ylabels2<-as.character(bob2$country)

ylabels1[19]<-"Central African Rep."
ylabels1[22]<-"Macedonia"
ylabels1[55]<-"Bolivia"
ylabels1[56]<-"Venezuela"

ylabels2[9]<-"Central African Rep."
ylabels2[11]<-"Tanzania"
ylabels2[33]<-"DR Congo"
ylabels2[35]<-"Iran"
ylabels2[45]<-"Syria"

pdf(file="dem_aut_stealth.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 50), main="Competitive Elections")
abline(v=c(0, 10, 20, 30, 40, 50), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=50, by=10))
box()

plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 50), main="No Competitive Elections")
abline(v=c(0, 10, 20, 30, 40, 50), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=50, by=10))
box()
dev.off()

###### scarring descriptives by regime type

meds.scar<-tapply(democs$scar,democs$cowcode,median)
lower.scar<-tapply(democs$scar,democs$cowcode,quantile,probs=c(0.25))
upper.scar<-tapply(democs$scar,democs$cowcode,quantile,probs=c(0.75))

bob1<-data.frame(meds.scar,lower.scar,upper.scar,unique(democs$country))
colnames(bob1)[4]<-c("country")
bob1<-bob1[order(bob1[,1]),]
ylabels1<-as.character(bob1$country)

meds.scar<-tapply(autocs$scar,autocs$cowcode,median)
lower.scar<-tapply(autocs$scar,autocs$cowcode,quantile,probs=c(0.25))
upper.scar<-tapply(autocs$scar,autocs$cowcode,quantile,probs=c(0.75))

bob2<-data.frame(meds.scar,lower.scar,upper.scar,unique(autocs$country))
colnames(bob2)[4]<-c("country")
bob2<-bob2[order(bob2[,1]),]
ylabels2<-as.character(bob2$country)

ylabels1[45]<-"Venezuela"
ylabels1[48]<-"Bolivia"
ylabels1[60]<-"Macedonia"
ylabels1[61]<-"Central African Rep."

ylabels2[26]<-"Tanzania"
ylabels2[28]<-"Iran"
ylabels2[31]<-"Syria"
ylabels2[44]<-"Central African Rep."
ylabels2[46]<-"DR Congo"

pdf(file="dem_aut_scar.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 85), main="Competitive Elections")
abline(v=c(0, 20, 40, 60, 80), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=80, by=20))
box()

plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 85), main="No Competitive Elections")
abline(v=c(0, 20, 40, 60, 80), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=80, by=20))
box()
dev.off()

##### scarring descriptives by level of judicial independence. split sample at JI >=0.4743 (median of JI)
democs<-tort.dat[tort.dat$ji>=0.4743,]
autocs<-tort.dat[tort.dat$ji<0.4743,]

meds.stealth<-tapply(democs$stealth,democs$cowcode,median)
lower.stealth<-tapply(democs$stealth,democs$cowcode,quantile,probs=c(0.25))
upper.stealth<-tapply(democs$stealth,democs$cowcode,quantile,probs=c(0.75))

bob1<-data.frame(meds.stealth,lower.stealth,upper.stealth,unique(democs$country))
colnames(bob1)[4]<-c("country")
bob1<-bob1[order(bob1[,1]),]
ylabels1<-as.character(bob1$country)

meds.stealth<-tapply(autocs$stealth,autocs$cowcode,median)
lower.stealth<-tapply(autocs$stealth,autocs$cowcode,quantile,probs=c(0.25))
upper.stealth<-tapply(autocs$stealth,autocs$cowcode,quantile,probs=c(0.75))

bob2<-data.frame(meds.stealth,lower.stealth,upper.stealth,unique(autocs$country))
colnames(bob2)[4]<-c("country")
bob2<-bob2[order(bob2[,1]),]
ylabels2<-as.character(bob2$country)

ylabels1[22]<-"Macedonia"
ylabels1[46]<-"Bolivia"

ylabels2[11]<-"Central African Rep."
ylabels2[13]<-"Tanzania"
ylabels2[44]<-"DR Congo"
ylabels2[46]<-"Iran"
ylabels2[49]<-"Venezuela"
ylabels2[58]<-"Syria"

#pdf(file="dem_stealth.pdf",width=7,height=12)
#par(oma=c(1, 2, 1, 1))
pdf(file="ji_stealth.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 50), main="High Judicial Power")
abline(v=c(0, 10, 20, 30, 40, 50), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1,)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=50, by=10))
box()
#dev.off()

#pdf(file="aut_stealth.pdf",width=7,height=12)
#par(oma=c(1, 2, 1, 1))
plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 50), main="Low Judicial Power")
abline(v=c(0, 10, 20, 30, 40, 50), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=50, by=10))
box()
dev.off()

### scarring

meds.scar<-tapply(democs$scar,democs$cowcode,median)
lower.scar<-tapply(democs$scar,democs$cowcode,quantile,probs=c(0.25))
upper.scar<-tapply(democs$scar,democs$cowcode,quantile,probs=c(0.75))

bob1<-data.frame(meds.scar,lower.scar,upper.scar,unique(democs$country))
colnames(bob1)[4]<-c("country")
bob1<-bob1[order(bob1[,1]),]
ylabels1<-as.character(bob1$country)

meds.scar<-tapply(autocs$scar,autocs$cowcode,median)
lower.scar<-tapply(autocs$scar,autocs$cowcode,quantile,probs=c(0.25))
upper.scar<-tapply(autocs$scar,autocs$cowcode,quantile,probs=c(0.75))

bob2<-data.frame(meds.scar,lower.scar,upper.scar,unique(autocs$country))
colnames(bob2)[4]<-c("country")
bob2<-bob2[order(bob2[,1]),]
ylabels2<-as.character(bob2$country)

ylabels1[40]<-"Bolivia"
ylabels1[52]<-"Macedonia"

ylabels2[33]<-"Tanzania"
ylabels2[34]<-"Iran"
ylabels2[39]<-"Venezuela"
ylabels2[42]<-"Syria"
ylabels2[55]<-"Central African Rep."
ylabels2[58]<-"DR Congo"

#pdf(file="dem_scar.pdf",width=7,height=12)
#par(oma=c(1, 2, 1, 1))
pdf(file="ji_scar.pdf",width=8,height=10)
par(mfrow=c(1,2),mar=c(2, 6.25, 2, 2))
plot(bob1[,1], seq(1:dim(bob1)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(0, 85), main="High Judicial Power")
abline(v=c(0, 20, 40, 60, 80), lty=3, lwd=2, col="grey")
points(bob1[,1], seq(1:dim(bob1)[1]), pch=20, cex=1)
for (i in 1:dim(bob1)[1]){
	lines(bob1[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels1, at=seq(1:dim(bob1)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=80, by=20))
box()
#dev.off()

#pdf(file="aut_scar.pdf",width=7,height=12)
#par(oma=c(1, 2, 1, 1))
plot(bob2[,1], seq(1:dim(bob2)[1]), type="n", pch=20, cex=1, ylab="", xlab="", axes=F, xlim=c(0, 85), main="Low Judicial Power")
abline(v=c(0, 20, 40, 60, 80), lty=3, lwd=2, col="grey")
points(bob2[,1], seq(1:dim(bob2)[1]), pch=20, cex=1)
for (i in 1:dim(bob2)[1]){
	lines(bob2[i,2:3], c(i, i), lwd=1)
}
axis(2, labels=ylabels2, at=seq(1:dim(bob2)[1]), las=1, cex.axis=0.7)
axis(1, at=seq(from=0, to=80, by=20))
box()
dev.off()

#### coefficient plots

load("stealth_ests.rda")
load("scar_ests.rda")

stealth.ests<-t(do.call(rbind,stealth.ests))
row.names(stealth.ests)[9:14]<-c("(Intercept)","laghrscore","hros","gdpc","speech","alpha")

stealth.irr<-exp(stealth.ests[2:8,1])
stealth.zstat<-stealth.ests[2:8,1]/stealth.ests[2:8,2]
stealth.pval<-2*(1-pnorm(abs(stealth.zstat)))
print(cbind(stealth.irr,stealth.pval),digits=2)

scar.ests<-t(do.call(rbind,scar.ests))
row.names(scar.ests)[9:14]<-c("(Intercept)","laghrscore","hros","gdpc","speech","alpha")

scar.irr<-exp(scar.ests[2:8,1])
scar.zstat<-scar.ests[2:8,1]/scar.ests[2:8,2]
scar.pval<-2*(1-pnorm(abs(scar.zstat)))
print(cbind(scar.irr,scar.pval),digits=2)

## for count equation

coefs<-matrix(nrow=22,ncol=4)

### population
coefs[1,1:2]<-stealth.ests[7,1:2]
coefs[2,1:2]<-scar.ests[7,1:2]

### GDP/capita
coefs[4,1:2]<-stealth.ests[6,1:2]
coefs[5,1:2]<-scar.ests[6,1:2]

### Civil War
coefs[7,1:2]<-stealth.ests[8,1:2]
coefs[8,1:2]<-scar.ests[8,1:2]

### Unstated
coefs[10,1:2]<-stealth.ests[5,1:2]
coefs[11,1:2]<-scar.ests[5,1:2]

### Stealth
coefs[13,1:2]<-scar.ests[4,1:2]

### Scarring
coefs[15,1:2]<-stealth.ests[4,1:2]

### Judiciary
coefs[17,1:2]<-stealth.ests[3,1:2]
coefs[18,1:2]<-scar.ests[3,1:2]

### Elections
coefs[20,1:2]<-stealth.ests[2,1:2]
coefs[21,1:2]<-scar.ests[2,1:2]

coefs[,3]<-coefs[,1]-1.65*coefs[,2]
coefs[,4]<-coefs[,1]+1.65*coefs[,2]

var.labs<-c("Population  ","Country wealth  ","Civil war  ","Unstated torture  ","Scarring torture  ","Clean torture  ","Judicial power  ","Elections  ")
mod.labs<-c("Clean torture","Scarring torture")

pdf(width=7, height=8, file="count_coefs.pdf")
#dev.new(width=7,height=8)
par(mar=c(2,8.5,2,2))
plot(coefs[,1], seq(1:dim(coefs)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(-1.5,1.5), main="Count equation estimates (1995-2005)")
abline(v=0, lty=3, lwd=2, col="black")
abline(v=c(-1.5, -1, -0.5, 0.5, 1, 1.5), lty=3, lwd=2, col="grey")
points(coefs[,1], seq(1:dim(coefs)[1]), pch=16, cex=1.5)
for (i in 1:dim(coefs)[1]){
	lines(coefs[i,3:4], c(i, i), lwd=1.5)
	}
axis(1, at=seq(from=-1.5, to=1.5, by=0.5))
axis(2, labels=var.labs, at=c(3,6,9,12,14,16,19,22), las=1, font=2, tick=F)
axis(2, labels=mod.labs, at=1:2, las=1)
axis(2, labels=mod.labs, at=4:5, las=1)
axis(2, labels=mod.labs, at=7:8, las=1)
axis(2, labels=mod.labs, at=10:11, las=1)
axis(2, labels=mod.labs, at=17:18, las=1)
axis(2, labels=mod.labs, at=20:21, las=1)
axis(2, labels="Clean torture",at=13,las=1)
axis(2, labels="Scarring torture",at=15,las=1)
box()
dev.off()

## for detection equation

coefs<-matrix(nrow=12,ncol=4)

## gdp/capita
coefs[1,1:2]<-stealth.ests[12,1:2]
coefs[2,1:2]<-scar.ests[12,1:2]

## lagged hr
coefs[4,1:2]<-stealth.ests[10,1:2]
coefs[5,1:2]<-scar.ests[10,1:2]

## speech
coefs[7,1:2]<-stealth.ests[13,1:2]
coefs[8,1:2]<-scar.ests[13,1:2]

## hros
coefs[10,1:2]<-stealth.ests[11,1:2]
coefs[11,1:2]<-scar.ests[11,1:2]

coefs[,3]<-coefs[,1]-1.65*coefs[,2]
coefs[,4]<-coefs[,1]+1.65*coefs[,2]

var.labs<-c("Country wealth  ","Lagged HR score  ", "Free speech", "INGOs  ")

pdf(width=7, height=8, file="detect_coefs.pdf")
#dev.new(width=7, height=8)
par(mar=c(2,8.5,2,2))
plot(coefs[,1], seq(1:dim(coefs)[1]), type="n", ylab="", xlab="", axes=F, xlim=c(-3.5,2), main="Detection equation estimates (1995-2005)")
abline(v=0, lty=3, lwd=2, col="black")
abline(v=c(-3, -2, -1, 1, 2), lty=3, lwd=2, col="grey")
points(coefs[,1], seq(1:dim(coefs)[1]), pch=16, cex=1.5)
for (i in 1:dim(coefs)[1]){
	lines(coefs[i,3:4], c(i, i), lwd=1.5)
	}
axis(1, at=seq(from=-3, to=2, by=1))
axis(2, labels=var.labs, at=c(3,6,9,12), las=1, font=2, tick=F)
axis(2, labels=mod.labs, at=1:2, las=1)
axis(2, labels=mod.labs, at=4:5, las=1)
axis(2, labels=mod.labs, at=7:8, las=1)
axis(2, labels=mod.labs, at=10:11, las=1)
box()
dev.off()

### global trends over time

## scarring
year.median<-tapply(tort.dat$scar,tort.dat$year,median)
year.lo<-tapply(tort.dat$scar,tort.dat$year,quantile,probs=0.25)
year.hi<-tapply(tort.dat$scar,tort.dat$year,quantile,probs=0.75)
year.scar<-data.frame(year.median,year.lo,year.hi)
year.scar[,4]<-seq(1:dim(year.scar)[1])
reg.scar<-lm(year.scar[,1]~year.scar[,4])

### stealth
year.median<-tapply(tort.dat$stealth,tort.dat$year,median)
year.lo<-tapply(tort.dat$stealth,tort.dat$year,quantile,probs=0.25)
year.hi<-tapply(tort.dat$stealth,tort.dat$year,quantile,probs=0.75)
year.stealth<-data.frame(year.median,year.lo,year.hi)
year.stealth[,4]<-seq(1:dim(year.stealth)[1])
reg.stealth<-lm(year.stealth[,1]~year.stealth[,4])

### combined graph

pdf(file="trends.pdf",height=6,width=14)
#dev.new(height=6,width=14)
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(year.scar[,4],year.scar[,1],type="p",pch=16,cex=1.75,ylab="Count",xlab="Year",xlim=c(1,dim(year.scar)[1]),ylim=c(0,13),main="Scarring torture",axes=F)
abline(a=coef(reg.scar)[1],b=coef(reg.scar)[2],col="grey",lwd=2)
abline(v=seq(1:15),lty=3,col="grey50",lwd=2.5)
for (i in 1:dim(year.scar)[1]){
	lines(c(i, i), year.scar[i,2:3], lwd=2.5)	
	}
axis(2,at=seq(from=0,to=12,by=2))
axis(1,at=seq(from=1,to=11,by=1),labels=1995:2005)
box()

plot(year.stealth[,4],year.stealth[,1],type="p",pch=16,cex=1.75,ylab="",xlab="Year",xlim=c(1,dim(year.stealth)[1]),ylim=c(0,13),main="Clean torture",axes=F)
abline(a=coef(reg.stealth)[1],b=coef(reg.stealth)[2],col="grey",lwd=2)
abline(v=seq(1:15),lty=3,col="grey50",lwd=2.5)
for (i in 1:dim(year.stealth)[1]){
	lines(c(i, i), year.stealth[i,2:3], lwd=2.5)	
	}
axis(2,at=seq(from=0,to=12,by=2))
axis(1,at=seq(from=1,to=11,by=1),labels=1995:2005)
box()
dev.off()

## detection probabilities

load("stealth_detection_probs.rda")
load("scar_detection_probs.rda")

scar.col<-rgb(99,99,99, 150, maxColorValue=255)
stealth.col<-rgb(204,204,204, 150, maxColorValue=255)

pdf(file="detection_probs.pdf")
plot(density(scar.detection.probs$phat),main="Predicted detection probabilities",xlab="Probability",col=scar.col)
polygon(density(scar.detection.probs$phat),col=scar.col)
lines(density(stealth.detection.probs$phat),col=stealth.col)
polygon(density(stealth.detection.probs$phat),col=stealth.col)
legend("top",c("Clean torture","Scarring torture"),pch=15,pt.cex=2,col=c(stealth.col,scar.col))
dev.off()

### distribution of JI by democ/autoc

pdf(file="ji_elections.pdf")
plot(density(tort.dat$ji[tort.dat$dem==1]),main="",xlab="Judicial Power",col=scar.col,ylim=c(0,4),xlim=c(-0.1,1.2),axes=F)
polygon(density(tort.dat$ji[tort.dat$dem==1]),col=scar.col)
lines(density(tort.dat$ji[tort.dat$dem==0]),col=stealth.col)
polygon(density(tort.dat$ji[tort.dat$dem==0]),col=stealth.col)
legend("topleft",c("Elections","No Elections"),pch=15,pt.cex=2,col=c(scar.col,stealth.col))
axis(1,at=c(0,0.25,0.5,0.75,1),labels=c(0, 0.25, 0.5, 0.75, 1))
axis(2,at=c(0,1,2,3))
box()
dev.off()


