####
# This code conducts all analysis & produces all graphics 
# found in the main text and appendicies of
# Ahlquist, John S. and Layna Mosley.
# "Firm Participation in Voluntary Regulatory Initiatives: 
## the Accord, Alliance, and US garment importers from Bangladesh"
#	Review of International Organizations
# R version 3.3.1 for OSX
# Created by JSA August 2017
# Last modified: January 6 2019
####

rm(list=ls())
gc()
set.seed(4242013)
#wd<-"AhlquistMosleyReplication" #set filepath appropriately
setwd(wd)
library(foreign)
library(MASS)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(countrycode)
library(stringr)
library(ggplot2)
library(texreg)
library(arm)
library(logistf)
library(mlogit)
library(tab)


#helper fcns
inv.logit<-function(x){exp(x)/(exp(x)+1)}
source("extract_logistf.R")

#data
igd<-read.csv("IGconformingShipments.csv") #IG shipping data excluding nonconforming shipments
#igd<-read.csv("IGallShipments.csv") #includes all shipments

#setting dates
rp.date<-as.Date("4/24/2013", "%m/%d/%Y")  #rana plaza date
accord.date<- as.Date("5/15/2013", "%m/%d/%Y") #Accord announcement date
alliance.date<- as.Date("7/10/2013", "%m/%d/%Y") #Alliance announcement date


row<-read.csv("RoWapparrelShipmentsToUSA.csv", header=T)
row$month<-dym(paste("01-",row$month, sep =""))

bgd<-read.csv("BGDdescriptiveData.csv", header=T)
bgd$date<-as.Date(paste(bgd$year,"-12-31", sep=""))

#Figure 1
## 1a
pdf("Fig1a.pdf", width=5.75, height=4.85)
plot(year(bgd$date), bgd$rmgex/1000, 
	type="l", bty="n", xlab="", lwd=3, 
	ylab="$US millions",
	main = "Bangladeshi RMG exports", 
	xlim = c(1995,2017), ylim = c(0, 30000))
lines(year(bgd$date), bgd$rmgex.usa/1000, lty=2)
lines(year(bgd$date), bgd$rmgex.euro/1000, lty=3)
text(2016, 10000, "Eurozone", cex=.75)
text(2016, 3000, "USA", cex=.75)
text(2016, 25000, "World", cex=.75)

abline(v=2013, col="slateblue1", lty=2, lwd=2)
text(2011.75,27500, "Rana\n Plaza", col="slateblue1", cex=.75)
abline(v=2005, col="red", lty=2, lwd=2)
text(2003.75,27500, "MFA\n expires", col="red", cex=.75)
dev.off()

##1b
pdf("Fig1b.pdf", width=5.75, height=4.85)
with(bgd, plot(year(date), rmgexpct, 
	type="l", bty="n", xlab="", lwd=2, 
	ylab="percent of exports", ylim=c(0,90),
	xlim = c(1995,2017))
)
lines(year(bgd$date), bgd$rmgex.usa.pctex, lty=2)
lines(year(bgd$date), bgd$rmgex.euro.pctex, lty=3)
text(2016, 32, "Eurozone", cex=.75)
text(2016, 10, "USA", cex=.75)
text(2016, 78, "World", cex=.75)
abline(v=2013, col="slateblue1", lwd=2, lty=2)
text(2011.75,89, "Rana\n Plaza", col="slateblue1", cex=.75)
#text(2011,70, "Rana Plaza", col="slateblue1")
abline(v=2005, col="red", lwd=2, lty=2)
text(2003.75,89, "MFA\n expires", col="red", cex=.75)
dev.off()

## 1c
pdf("Fig1c.pdf", width=5.75, height=4.85)
with(bgd,plot(year(date), rmggdppct,
	type="l", bty="n", xlab="", lwd=2,
	ylab="percent of GDP", ylim=c(0,20))
)
lines(year(bgd$date), bgd$rmgex.usa.pctgdp, lty=2)
lines(year(bgd$date), bgd$rmgex.euro.pctgdp, lty=3)
text(2016, 4, "Eurozone", cex=.75)
text(2016, 1, "USA", cex=.75)
text(2016, 13, "World", cex=.75)
abline(v=2013, col="slateblue1", lwd=2, lty=2)
text(2011.75,19, "Rana\n Plaza", col="slateblue1", cex=.75)
abline(v=2005, col="red", lwd=2, lty=2)
text(2003.75,19, "MFA\n expires", col="red", cex=.75)
dev.off()


igd$arrivaldate<-as.Date(igd$arrivaldate, "%m/%d/%y")
igd$pre.rp60<-0
igd$pre.rp60[igd$arrivaldate<rp.date+60]<-1 #indicator for pre-RP shipments assuming a min 60 day delay
igd$arrivalyearmon<-as.yearmon(igd$arrivaldate)
igd$arrivalqtr<-quarter(igd$arrivaldate)
igd$arrivalmon<-month(igd$arrivaldate)
igd$yrlater<-0
igd$yrlater[igd$arrivaldate<rp.date+365+30]<-1


igd$plans<-NA
igd$plans[!is.na(igd$UOHQ) & igd$Accord == 1& igd$Alliance == 0]<-"accord"
igd$plans[!is.na(igd$UOHQ) & igd$Accord == 1& igd$Alliance == 1]<-"both"
igd$plans[!is.na(igd$UOHQ) & igd$Accord == 0& igd$Alliance == 1]<-"alliance"
igd$plans[is.na(igd$plans)]<-"neither" #neither/unverified/unknown
igd$plans<-as.factor(igd$plans)
igd$alliance.postrp<-igd$accord.postrp<-0
igd$alliance.postrp[igd$plans %in% c("alliance","both") & igd$arrivaldate>rp.date+90]<-1 #Shipments to Alliance signatories that were possibly ordered after RP
igd$accord.postrp[igd$plans %in% c("accord","both") & igd$arrivaldate>rp.date+90]<-1 #Shipments to Alliance signatories that were possibly ordered after RP

#### calculations supporting claims in section 4.3
ship.vol.byplan<-aggregate(igd$grossweightkg ~ igd$arrivalyearmon*igd$plans, FUN=sum)
ship.cnt.byplan<-aggregate(igd$grossweightkg ~ igd$arrivalyearmon*igd$plans, FUN=length)
names(ship.vol.byplan)<-names(ship.cnt.byplan)<-c("date", "plan", "shipping")
acc.shipvol<-ship.vol.byplan[ship.vol.byplan$plan=="accord",]
all.shipvol<-ship.vol.byplan[ship.vol.byplan$plan=="alliance",]
b.sv<-ship.vol.byplan[ship.vol.byplan$plan=="both",]
acc.shipvol[match(b.sv$date,acc.shipvol$date),"shipping"]<-acc.shipvol[match(b.sv$date,acc.shipvol$date),"shipping"] +b.sv[,"shipping"]
all.shipvol[match(b.sv$date,all.shipvol$date),"shipping"]<-all.shipvol[match(b.sv$date,all.shipvol$date),"shipping"] +b.sv[,"shipping"]


ship.vol<-aggregate(igd$grossweightkg ~igd$arrivalyearmon, FUN=sum)
ship.vol.bytype<-aggregate(igd$grossweightkg ~ 
	igd$arrivalyearmon*I(igd$Type%in%c("BRAND", "RETAILER","WHOLESALER","MANUFACTURER")),
	FUN=sum)

ship.cnt<-aggregate(igd$grossweightkg ~igd$arrivalyearmon, FUN=length)
names(ship.vol)<-names(ship.cnt)<-c("date", "shipping")
names(ship.vol.bytype)<-c("date", "std.type", "shipping")

ship.vol$date<-as.Date(paste("1 ",ship.vol$date), format = "%d %b %Y" )
ship.cnt$date<-as.Date(paste("1 ",ship.cnt$date), format = "%d %b %Y" )
ship.vol.byplan$date<-as.Date(paste("1 ",ship.vol.byplan$date), format = "%d %b %Y" )
ship.vol.bytype$date<-as.Date(paste("1 ",ship.vol.bytype$date), format = "%d %b %Y" )

ship.vol.bytype$rp<-0  #finding % of pre-RP shipping that goes to {DK, NA,logisitics} firms.
ship.vol.bytype$rp[ship.vol.bytype$date>rp.date+60]<-1
sum(ship.vol.bytype$shipping[ship.vol.bytype$std.type==FALSE & ship.vol.bytype$rp==0])/sum(ship.vol.bytype$shipping[ship.vol.bytype$rp==0])

ship.vol$month<-month(ship.vol$date)
ship.vol$qtr<-quarter(ship.vol$date)
ship.cnt$month<-month(ship.cnt$date)
ship.cnt$qtr<-quarter(ship.cnt$date)
ship.cnt$rp<-0
ship.cnt$rp[ship.cnt$date>rp.date+60]<-1
ship.vol$RoW<-ship.vol$kg.pct<-NA
ship.vol$RoW[match(row$month,ship.vol$date)]<-row$row.kg[na.omit(match(ship.vol$date,row$month))]

ship.vol$kg.pct<-100*ship.vol$shipping/ship.vol$RoW
ship.vol$rp<-0
ship.vol$rp[ship.vol$date>rp.date+60]<-1
diff.df<-NULL
for(i in 1:12){
	temp<-ship.vol[ship.vol$month==i,]
	temp$diff<-c(NA,diff(temp$shipping))
	temp$diff.pcf<-c(NA,diff(temp$kg.pct))
	diff.df<-rbind(diff.df,temp)
}
with(diff.df, lm(diff.pcf~rp*as.factor(qtr)))
with(diff.df, lm(diff.pcf~rp + as.factor(qtr)))
with(diff.df, lm(diff.pcf~rp))

# Appendix figure 1
pdf("descShip.pdf")
par(mfrow=c(2,2))
with(ship.vol, plot(date, shipping/1000000, bty="n", ylim=c(0,22), lwd=2, las=1,
	ylab="Kg (millions)", xlab="", main= "Weight" ))
abline(v=rp.date, lty=2, col="slateblue1")
text(x=rp.date, y=3, label="Rana Plaza", col="slateblue1", lwd=2, pos=4,) 
with(ship.vol, plot(date, kg.pct, bty="n", lwd=2, las=1,
	ylab="percent", xlab="", main = "Relative to RoW"))
abline(v=rp.date, lty=2, col="slateblue1")
text(x=rp.date, y=3, label="Rana Plaza", col="slateblue1", lwd=2) 
with(ship.cnt, plot(date, shipping, bty="n",lwd=2, las=1, ylim = c(0,3500),
	ylab="number of shipments", xlab="", main = "Shipments"))
abline(v=rp.date, lty=2, col="slateblue1")
text(x=rp.date, y=3, label="Rana Plaza", col="slateblue1", lwd=2) 
mtext("USA monthly apparel import shipments from Bangladesh", 
	line = -1, side=3,outer=TRUE)
dev.off()


igd$DOCode<-as.factor(igd$DOCode)
igd$UOCode<-as.factor(igd$UOCode)

##Regression analysis
prelim<-aggregate(igd$grossweightkg ~ igd$DOCode, FUN=sum)
names(prelim)<-c("recipientFirm", "grossweightkg")
prelim$DOName<-igd$DOName[match(prelim$recipientFirm, igd$DOCode)]
prelim$UOName<-igd$UOName[match(prelim$recipientFirm, igd$DOCode)]
prelim$firmtype<-igd$Type[match(prelim$recipientFirm, igd$DOCode)]
prelim$DOEqualsUO<-igd$DOEqualsUO[match(prelim$recipientFirm, igd$DOCode)]
prelim$independent<-igd$Independent[match(prelim$recipientFirm, igd$DOCode)]
prelim$DOHQ<-igd$DOHQ[match(prelim$recipientFirm, igd$DOCode)]
prelim$UOHQ<-igd$UOHQ[match(prelim$recipientFirm, igd$DOCode)]
prelim$UOPublic<-igd$UOPublic[match(prelim$recipientFirm, igd$DOCode)]
prelim$plans<-igd$plans[match(prelim$recipientFirm, igd$DOCode)]
prelim<-subset(prelim, !is.na(recipientFirm))
prelim$plans.simp<-prelim$plans
prelim$plans.simp[prelim$plans.simp=="both"]<-"accord"
prelim$plans.simp<-factor(prelim$plans.simp, exclude="both")
prelim$plans.simp<-relevel(prelim$plans.simp, ref="neither")
prelim$usa.uohq<-as.factor(prelim$UOHQ)=="USA"
prelim$consumer.facing<- as.factor(prelim$firmtype)=="RETAILER"|as.factor(prelim$firmtype)=="BRAND"

prelim.usa<-subset(prelim, usa.uohq==TRUE) # for appendix models
prelim.cf<-subset(prelim, consumer.facing==TRUE)

#Table 2 in appendix
tabmulti(prelim, xvarname="plans.simp", 
	yvarname = c("consumer.facing", "UOPublic", "usa.uohq"), 
	decimals=0, p.include=FALSE, print.html=TRUE, html.filename="Xtabs.html")


#MNL/Hausmann test for fn 30.
mnl.data<-mlogit.data(prelim, choice = "plans.simp", shape="wide")
mnl.1<-mlogit(plans.simp ~ 1 | log(grossweightkg) + usa.uohq , reflevel="neither",data = mnl.data)
mnl.2<-mlogit(plans.simp ~ 1 | log(grossweightkg) + usa.uohq + consumer.facing + UOPublic, reflevel="neither",data = mnl.data)
hmftest(mnl.1, c("accord", "alliance")) # p<<<<0.01


#Table 2: signed on to either plan
remed.choosers1<-with(prelim, 
	glm(plans!="neither" ~ log(grossweightkg) + usa.uohq, family = binomial)
	)

remed.choosers2<-with(prelim, 
	glm(plans!="neither" ~ log(grossweightkg) + usa.uohq +consumer.facing + UOPublic, family = binomial)
	)
rc2.check<-update(remed.choosers2, .~. - log(grossweightkg))
remed.choosers3<-with(prelim, 
	glm(plans!="neither" ~ log(grossweightkg) + usa.uohq +consumer.facing * UOPublic, family = binomial)
	)

remed.firth<-with(prelim, 
	logistf(plans!="neither" ~ log(grossweightkg) + usa.uohq +consumer.facing + UOPublic)
	)
htmlreg(list(remed.choosers1, remed.choosers2,remed.firth), 
	file = "Table2.htm", bold = 0.05, stars = numeric(0) ,
	custom.model.names = c("Logit", "Logit", "Firth"),
	caption = "Which importers to the USA signed the Accord and Alliance?<br> 
	Logistic regression parameter estimates.",
	custom.coef.names = c("constant", 
		"Import volume", 
		"USA HQ",
		"Consumer-facing firm",
		"Publicly traded"),
	caption.above=T,
	ci.force = F)

#Table 3: differentiating Accord from alliance
acc.1<-with(prelim, 
	glm(plans%in%c("accord","both") ~log(grossweightkg) + usa.uohq, family = binomial)
	)
acc.2<-update(acc.1, .~. + consumer.facing + UOPublic)
acc2.check<-update(acc.2, .~. - log(grossweightkg))
acc.3<-update(acc.1, .~. + consumer.facing * UOPublic)

all.1<-with(prelim, 
	glm(plans%in%c("alliance","both")~log(grossweightkg) + usa.uohq, family = binomial)
	)
all.2<-update(all.1, .~. + consumer.facing + UOPublic)
all2.check<-update(all.2, .~. - log(grossweightkg))
all.3<-update(all.1, .~. + consumer.facing * UOPublic)

acc.firth<-with(prelim,
	logistf(plans%in%c("accord","both") ~log(grossweightkg) + usa.uohq +consumer.facing + UOPublic))
all.firth<-with(prelim,
	logistf(plans%in%c("alliance","both") ~log(grossweightkg) + usa.uohq +consumer.facing + UOPublic))

htmlreg(list(acc.1, acc.2,acc.firth,all.1,all.2,all.firth), 
	file = "Table3.htm", bold = 0.05, stars = numeric(0) ,
	custom.model.names = c("Accord", "Accord","Acc-firth", "Alliance", "Alliance", "All-firth"),
	caption = "Which importers to the USA signed the Accord and Alliance?<br> 
	Logistic regression parameter estimates.",
	custom.coef.names = c("constant", 
		"Import volume", 
		"USA HQ",
		"Consumer-facing firm",
		"Publicly traded"),
	caption.above=T,
	ci.force = F)#,


#interpretation & figure 2
nd.gw<-cbind(1,quantile(log(prelim$grossweightkg),c(0.25,0.75)), 1,0,0)
Beta<-mvrnorm(1000,coef(acc.2), vcov(acc.2))
p.hat.gw<-inv.logit(Beta%*%t(nd.gw))
gw.rr<-p.hat.gw[,2]/p.hat.gw[,1]
gw.acc<-quantile(gw.rr, c(0.025,0.5,0.975))
Beta<-mvrnorm(1000,coef(all.2), vcov(all.2))
p.hat.gw<-inv.logit(Beta%*%t(nd.gw))
gw.rr<-p.hat.gw[,2]/p.hat.gw[,1]
gw.all<-quantile(gw.rr, c(0.025,0.5,0.975))

nd.usa<-cbind(1,quantile(log(prelim$grossweightkg),0.5), c(0,1),0,0)
Beta<-mvrnorm(1000,coef(acc.2), vcov(acc.2))
p.hat.usa<-inv.logit(Beta%*%t(nd.usa))
usa.rr<-p.hat.usa[,2]/p.hat.usa[,1]
usa.acc<-quantile(usa.rr, c(0.025,0.5,0.975))
Beta<-mvrnorm(1000,coef(all.2), vcov(all.2))
p.hat.usa<-inv.logit(Beta%*%t(nd.usa))
usa.rr<-p.hat.usa[,2]/p.hat.usa[,1]
usa.all<-quantile(usa.rr, c(0.025,0.5,0.975))

nd.cf<-cbind(1,quantile(log(prelim$grossweightkg),0.5), 1,c(0,1),0)
Beta<-mvrnorm(1000,coef(acc.2), vcov(acc.2))
p.hat.cf<-inv.logit(Beta%*%t(nd.cf))
cf.rr<-p.hat.cf[,2]/p.hat.cf[,1]
cf.acc<-quantile(cf.rr, c(0.025,0.5,0.975))
Beta<-mvrnorm(1000,coef(all.2), vcov(all.2))
p.hat.cf<-inv.logit(Beta%*%t(nd.cf))
cf.rr<-p.hat.cf[,2]/p.hat.cf[,1]
cf.all<-quantile(cf.rr, c(0.025,0.5,0.975))

nd.pt<-cbind(1,quantile(log(prelim$grossweightkg),0.5), 1,0,c(0,1))
Beta<-mvrnorm(1000,coef(acc.2), vcov(acc.2))
p.hat.pt<-inv.logit(Beta%*%t(nd.pt))
pt.rr<-p.hat.pt[,2]/p.hat.pt[,1]
pt.acc<-quantile(pt.rr, c(0.025,0.5,0.975))
Beta<-mvrnorm(1000,coef(all.2), vcov(all.2))
p.hat.pt<-inv.logit(Beta%*%t(nd.pt))
pt.rr<-p.hat.pt[,2]/p.hat.pt[,1]
pt.all<-quantile(pt.rr, c(0.025,0.5,0.975))


plot.data<-data.frame(
	var=c("Volume", "Volume", "US HQ", "US HQ", "Consumer", "Consumer", "Public", "Public"),
	plan = c("Accord", "Alliance", "Accord", "Alliance", "Accord", "Alliance", "Accord", "Alliance"),
	y = c(gw.acc[2], gw.all[2], usa.acc[2], usa.all[2], cf.acc[2], cf.all[2], pt.acc[2], pt.all[2]),
	lwr = c(gw.acc[1], gw.all[1], usa.acc[1], usa.all[1], cf.acc[1], cf.all[1], pt.acc[1], pt.all[1]),
	upr = c(gw.acc[3], gw.all[3], usa.acc[3], usa.all[3], cf.acc[3], cf.all[3], pt.acc[3], pt.all[3]),
	pch = c(16,17,16,2,16,17,16,17))
## figure 3
pdf("fig3.pdf")
par(mar=c(5.1, 8.1, 4.1, 2.1))
plot(0, 0, xlim = c(0, max(plot.data$y)), ylim = c(1, 4), 
        xlab = "relative risk",
        main = "Interpreting the models", ylab = "", 
        axes = FALSE, type = "n")

axis(side = 2, at = 1:(length(plot.data$var)/2),
	labels = plot.data$var[c(1,3,5,7)], 
	las = 2, tck = 0, lty = 0)
axis(side = 1, las = 0, lty = 1, lwd=2, line=.5)
segments(1, 0, 1, 9, lwd = 4, col = "grey75")
points(plot.data$y,c(1,1,2,2,3,3,4,4),pch=plot.data$pch,cex=1.5)
legend("bottomright", bty="n", legend = c("Accord", "Alliance"), pch=c(16,17))
dev.off()


# consumer-facing only firms for supplementary materials
prelim.cf<-subset(prelim, consumer.facing==TRUE)

both.cf<-with(prelim.cf, 
	glm(plans!="neither" ~ log(grossweightkg)+ usa.uohq + UOPublic, family = binomial)
	)

both.firth.cf<-with(prelim.cf, 
	logistf(plans!="neither" ~ log(grossweightkg)+ usa.uohq + UOPublic, family = binomial)
	)


acc.cf<-with(prelim.cf, 
	glm(plans%in%c("accord","both") ~ log(grossweightkg)+ usa.uohq + UOPublic, family = binomial)
	)

all.cf<-with(prelim.cf, 
	glm(plans%in%c("alliance","both")~log(grossweightkg)+ usa.uohq + UOPublic, family = binomial)
	)

acc.firth.cf<-with(prelim.cf,
	logistf(plans%in%c("accord","both") ~log(grossweightkg)+ usa.uohq + UOPublic))
all.firth.cf<-with(prelim.cf,
	logistf(plans%in%c("alliance","both") ~log(grossweightkg)+ usa.uohq + UOPublic))

htmlreg(list(both.cf, acc.cf, all.cf),  #Appendix table 4
	file = "CFonlyConformRR.htm", bold = 0.05, stars = numeric(0) ,
	#omit.coef = "province",
	custom.model.names = c("Either", "Accord", "Alliance"),
	caption = "Which importers to the USA signed the Accord and Alliance?<br> 
	Logistic regression parameter estimates.",
	custom.coef.names = c("constant", 
		"Import volume", 
		"USA HQ",
		#"Consumer-facing firm",
		"Publicly traded"),
	caption.above=T,
	ci.force = F)#,


#USA only firms for supplementary materials
prelim.usa<-subset(prelim, usa.uohq==TRUE)

acc.usa<-with(prelim.usa, 
	glm(plans%in%c("accord","both") ~ log(grossweightkg) + consumer.facing + UOPublic, family = binomial)
	)

all.usa<-with(prelim.usa, 
	glm(plans%in%c("alliance","both")~log(grossweightkg) + consumer.facing + UOPublic, family = binomial)
	)

acc.firth.usa<-with(prelim.usa,
	logistf(plans%in%c("accord","both") ~log(grossweightkg) + consumer.facing + UOPublic))
all.firth.usa<-with(prelim.usa,
	logistf(plans%in%c("alliance","both") ~log(grossweightkg) + consumer.facing + UOPublic))

htmlreg(list(acc.usa, acc.firth.usa, all.usa, all.firth.usa), #Appendix table 3
	file = "Table3app.htm", bold = 0.05, stars = numeric(0) ,
	custom.model.names = c("Logit", "Firth", "Logit", "Firth"),
	caption = "Which importers to the USA signed the Accord and Alliance?<br> 
	(USA-based firms only).",
	custom.coef.names = c("constant", 
		"Import volume", 
		"Consumer-facing firm",
		"Publicly traded"),
	caption.above=T,
	ci.force = F)

#End