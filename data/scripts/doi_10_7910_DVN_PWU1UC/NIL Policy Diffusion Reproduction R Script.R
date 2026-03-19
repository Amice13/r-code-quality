## Daniel J. Mallinson and Darrell Lovell
## Reproduction Code for "Cash Rules Everything Around Me: 
## The Expansion of NCAA Name, Image, and Likeness Policy Among the States"
## 2023-08-28

library(readxl)
library(survival)
library(survMisc)
library(simPH)

data <- read_excel("nil_analysis_data.xlsx", sheet=1)

######################### Figure 1 ####################################
## Cumulative Adoptions Plot

justadopts.int <- data[c("int.months", "int")]
justadopts.int <- aggregate(justadopts.int, by=list(justadopts.int$int.months), FUN=sum)
justadopts.int <- justadopts.int[c("Group.1", "int")]
names(justadopts.int)[1] <- "month"
justadopts.int$sum <- cumsum(justadopts.int$int)

justadopts.ad <- data[c("adopt.months", "adopt")]
justadopts.ad <- aggregate(justadopts.ad, by=list(justadopts.ad$adopt.months), FUN=sum)
justadopts.ad <- justadopts.ad[c("Group.1", "adopt")]
names(justadopts.ad)[1] <- "month"
justadopts.ad$sum <- cumsum(justadopts.ad$adopt)

png("figure1.png", height=6, width=7, res=600, units="in")
plot.new()
par(mar=c(2,4,0,0))
plot.window(xlim=c(0,40), ylim=c(0,50))
lines(justadopts.int$sum~justadopts.int$month, col="blue", lty=1, lwd=2)
lines(justadopts.ad$sum~justadopts.ad$month, col="red", lty=2, lwd=2)
axis(1, at=seq(0,40,6), labels=c("01/19", "07/19", "01/20", "07/20", "01/21", "07/21", "01/22"))
axis(2, at=seq(0,50,5), labels=seq(0,50,5), las=2)
title(ylab="Cumulative Count of States")
abline(v=29)
text(24, 45, "NCAA v. Alston")
legend(0,45,legend=c("Introduction", "Adoption"), col=c("blue", "red"), lty=c(1,2), bty="n", lwd=2)
dev.off()

################################# Speed Measure ###############################################

data$years <- 4
data$years[data$adopt.year==2019] <- 1
data$years[data$adopt.year==2020] <- 3
data$years[data$adopt.year==2021] <- 2

nilspeed <- survreg(Surv(time=data$years, event=data$adopt, type="right")~1, dist="weibull", data=data)

speeds <- read.csv("all_raw_speed_spid_2021-02-23.csv")

speeds <- speeds[,2:3]
mean(speeds$weib.speed)

nrow(speeds[speeds$weib.speed>as.numeric(coef(nilspeed)[1]),])/(nrow(speeds)+1)

################################# All Schools Survival Models #############################################

unidata <- read.csv("All_Data_Combined_2019.csv")
unidata <- unidata[c("UNITID", "Institution.Name", "st.abb", "Classification.Name", "Sanction.Name", "Total.Undergraduates", "Unduplicated.Count.Men.s.Participation",
                     "Unduplicated.Count.Women.s.Participation", "Football.Men.s.Team.Revenue", "Grand.Total.Revenue")]

unidata <- unidata[which(unidata$Sanction.Name=="NCAA"),]

unidata$total.participation <- unidata$Unduplicated.Count.Men.s.Participation + unidata$Unduplicated.Count.Men.s.Participation

unidata <- aggregate(cbind(Total.Undergraduates,total.participation, Football.Men.s.Team.Revenue,Grand.Total.Revenue)~st.abb, FUN=sum, data=unidata, na.rm=TRUE, na.action=NULL)

unidata$participation.percentage <- unidata$total.participation/unidata$Total.Undergraduates*100
unidata$revenue.percap.thousands <- unidata$Grand.Total.Revenue/unidata$Total.Undergraduates/1000
unidata$fbrevenue.percap.thousands <- unidata$Football.Men.s.Team.Revenue/unidata$Total.Undergraduates/1000

data <- merge(data, unidata, by="st.abb", all.x=TRUE, all.y=FALSE)

## Introduction

cox.int1 <- coxph(Surv(int.months, int)~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(cox.int1)
test.ph.int1 <- cox.zph(cox.int1)
test.ph.int1

aft.int1 <- survreg(Surv(int.months, int) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int1)
exp(coef(aft.int1))
exp(confint(aft.int1))

cox.int2 <- coxph(Surv(int.months, int)~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(cox.int2)
test.ph.int2 <- cox.zph(cox.int2)
test.ph.int2

aft.int2 <- survreg(Surv(int.months, int) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int2)
exp(coef(aft.int2))
exp(confint(aft.int2))

## Adoption

cox.adt1 <- coxph(Surv(adopt.months, adopt) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(cox.adt1)
test.ph.adt1 <- cox.zph(cox.adt1)
test.ph.adt1

aft.adt1 <- survreg(Surv(adopt.months, adopt) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt1)
exp(coef(aft.adt1))
exp(confint(aft.adt1))

cox.adt2 <- coxph(Surv(adopt.months, adopt)~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(cox.adt2)
test.ph.adt2 <- cox.zph(cox.adt2)
test.ph.adt2

aft.adt2 <- survreg(Surv(adopt.months, adopt) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt2)
exp(coef(aft.adt2))
exp(confint(aft.adt2))

png("figure4_jpna.png", height=6, width=6, units="in", res=600)
newx <- seq(min(data$fbs), max(data$fbs), by=1)
preds <- predict(aft.adt1, newdata=data.frame(fbs=newx, revenue.percap.thousands=mean(data$revenue.percap.thousands),
                           participation.percentage=mean(data$participation.percentage),
                           inst6017_nom=mean(data$inst6017_nom),
                           neighbor_adopt=mean(data$neighbor_adopt)), se.fit=TRUE,
                 type="lp")
yy <- cbind(newx, exp(preds$fit), exp(preds$fit - 1.96*preds$se.fit),
            exp(preds$fit + 1.96*preds$se.fit))
plot(yy[,1], yy[,2], type="n", ylim=c(10, 50), ylab="Survival Time (Months)", xlab="FBS Football Teams")
lines(yy[,1], yy[,2], lwd=2)
lines(yy[,1], yy[,3], lty=2)
lines(yy[,1], yy[,4], lty=2)
dev.off

png("figure5_jpna.png", height=6, width=6, units="in", res=600)
newx <- seq(min(data$neighbor_adopt), max(data$neighbor_adopt), by=.01)
preds <- predict(aft.adt1, newdata=data.frame(fbs=mean(data$fbs), revenue.percap.thousands=mean(data$revenue.percap.thousands),
                                              participation.percentage=mean(data$participation.percentage),
                                              inst6017_nom=mean(data$inst6017_nom),
                                              neighbor_adopt=newx), se.fit=TRUE, type="lp")

yy <- cbind(newx, exp(preds$fit), exp(preds$fit - 1.96*preds$se.fit),
            exp(preds$fit + 1.96*preds$se.fit))
plot(yy[,1], yy[,2], type="n", ylim=c(20, 70), ylab="Survival Time (Months)", xlab="Proportion of Neighbors Adopting")
lines(yy[,1], yy[,2], lwd=2)
lines(yy[,1], yy[,3], lty=2)
lines(yy[,1], yy[,4], lty=2)
dev.off()


png("figure6_jpna.png", height=6, width=6, units="in", res=600)
newx <- seq(min(data$inst6017_nom), max(data$inst6017_nom), by=1)
preds <- predict(aft.int1, newdata=data.frame(fbs=mean(data$fbs), revenue.percap.thousands=mean(data$revenue.percap.thousands),
                                              participation.percentage=mean(data$participation.percentage),
                                              inst6017_nom=newx,
                                              neighbor_int=mean(data$neighbor_int)), se.fit=TRUE, type="lp")

yy <- cbind(newx, exp(preds$fit), exp(preds$fit - 1.96*preds$se.fit),
            exp(preds$fit + 1.96*preds$se.fit))
plot(yy[,1], yy[,2], type="n", ylim=c(10, 60), ylab="Survival Time (Months)", xlab="Government Conservatism")
lines(yy[,1], yy[,2], lwd=2)
lines(yy[,1], yy[,3], lty=2)
lines(yy[,1], yy[,4], lty=2)
dev.off()

####################################### Robustness Checks ####################################
### Include "college basketball" and "college football" search activity from Google Trends

## Introduction
aft.int1.check1 <- survreg(Surv(int.months, int)~ revenue.percap.thousands + fbs + participation.percentage + inst6017_nom + int.gt.basketball + int.gt.football + neighbor_int, data=data)
summary(aft.int1.check1)

aft.int2.check1 <- survreg(Surv(int.months, int)~ fbrevenue.percap.thousands + fbs + participation.percentage + inst6017_nom + int.gt.basketball + int.gt.football  + neighbor_int, data=data)
summary(aft.int2.check1)

## Adoption

aft.adt1.check1 <- survreg(Surv(adopt.months, adopt)~ revenue.percap.thousands + fbs + participation.percentage + inst6017_nom + adopt.gt.basketball + adopt.gt.football + neighbor_adopt, data=data)
summary(aft.adt1.check1)

aft.adt2.check1 <- survreg(Surv(adopt.months, adopt)~ fbrevenue.percap.thousands + fbs + participation.percentage + inst6017_nom + adopt.gt.basketball + adopt.gt.football + neighbor_adopt, data=data)
summary(aft.adt2.check1)

### Include count of NCAA Division 1 basketball teams per state

## Introduction

aft.int1.check2 <- survreg(Surv(int.months, int)~ revenue.percap.thousands + fbs + basketball + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int1.check2)

aft.int2.check2 <- survreg(Surv(int.months, int)~ fbrevenue.percap.thousands + fbs + basketball + participation.percentage + inst6017_nom  + neighbor_int, data=data)
summary(aft.int2.check2)

## Adoption

aft.adt1.check2 <- survreg(Surv(adopt.months, adopt)~ revenue.percap.thousands + fbs + basketball  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt1.check2)

aft.adt2.check2 <- survreg(Surv(adopt.months, adopt)~ fbrevenue.percap.thousands + fbs +basketball + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt2.check2)

#Check correlation between fbs and basketball
cor.test(data$fbs, data$basketball) #0.80

################################### Football Schools Survival Models #################################
data <- read_excel("nil_analysis_data.xlsx", sheet=1)

unidata <- read.csv("All_Data_Combined_2019.csv")
unidata <- unidata[c("UNITID", "Institution.Name", "st.abb", "Classification.Name", "Sanction.Name", "Total.Undergraduates", "Unduplicated.Count.Men.s.Participation",
                     "Unduplicated.Count.Women.s.Participation", "Football.Men.s.Team.Revenue", "Grand.Total.Revenue")]

unidata <- unidata[which(unidata$Classification.Name %in% c("NCAA Division I-FCS", "NCAA Division I-FBS", "NCAA Division II with football", "NCAA Division III with football")),]

unidata$total.participation <- unidata$Unduplicated.Count.Men.s.Participation + unidata$Unduplicated.Count.Men.s.Participation

unidata <- aggregate(cbind(Total.Undergraduates,total.participation, Football.Men.s.Team.Revenue,Grand.Total.Revenue)~st.abb, FUN=sum, data=unidata, na.rm=TRUE, na.action=NULL)

unidata$participation.percentage <- unidata$total.participation/unidata$Total.Undergraduates*100
unidata$revenue.percap.thousands <- unidata$Grand.Total.Revenue/unidata$Total.Undergraduates/1000
unidata$fbrevenue.percap.thousands <- unidata$Football.Men.s.Team.Revenue/unidata$Total.Undergraduates/1000

data <- merge(data, unidata, by="st.abb", all.x=TRUE, all.y=FALSE)

## Introduction

aft.int3 <- survreg(Surv(int.months, int) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int3)
exp(coef(aft.int3))
exp(confint(aft.int3))

aft.int4 <- survreg(Surv(int.months, int) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int4)
exp(coef(aft.int4))
exp(confint(aft.int4))

## Adoption
aft.adt3 <- survreg(Surv(adopt.months, adopt) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt3)
exp(coef(aft.adt3))
exp(confint(aft.adt3))

aft.adt4 <- survreg(Surv(adopt.months, adopt) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt4)
exp(coef(aft.adt4))
exp(confint(aft.adt4))

################################### FBS and FCS Schools Survival Models ########################
data <- read_excel("nil_analysis_data.xlsx", sheet=1)

unidata <- read.csv("All_Data_Combined_2019.csv")
unidata <- unidata[c("UNITID", "Institution.Name", "st.abb", "Classification.Name", "Sanction.Name", "Total.Undergraduates", "Unduplicated.Count.Men.s.Participation",
                     "Unduplicated.Count.Women.s.Participation", "Football.Men.s.Team.Revenue", "Grand.Total.Revenue")]

unidata <- unidata[which(unidata$Classification.Name %in% c("NCAA Division I-FCS", "NCAA Division I-FBS")),]

unidata$total.participation <- unidata$Unduplicated.Count.Men.s.Participation + unidata$Unduplicated.Count.Men.s.Participation

unidata <- aggregate(cbind(Total.Undergraduates,total.participation, Football.Men.s.Team.Revenue,Grand.Total.Revenue)~st.abb, FUN=sum, data=unidata, na.rm=TRUE, na.action=NULL)

unidata$participation.percentage <- unidata$total.participation/unidata$Total.Undergraduates*100
unidata$revenue.percap.thousands <- unidata$Grand.Total.Revenue/unidata$Total.Undergraduates/1000
unidata$fbrevenue.percap.thousands <- unidata$Football.Men.s.Team.Revenue/unidata$Total.Undergraduates/1000

data <- merge(data, unidata, by="st.abb", all.x=TRUE, all.y=FALSE)

## Introduction

aft.int5 <- survreg(Surv(int.months, int) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int5)
exp(coef(aft.int5))
exp(confint(aft.int5))

aft.int6 <- survreg(Surv(int.months, int) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int6)
exp(coef(aft.int6))
exp(confint(aft.int6))

## Adoption

aft.adt5 <- survreg(Surv(adopt.months, adopt) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt5)
exp(coef(aft.adt5))
exp(confint(aft.adt5))

aft.adt6 <- survreg(Surv(adopt.months, adopt) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt6)
exp(coef(aft.adt6))
exp(confint(aft.adt6))

################################### FBS Schools Survival Models #############################
data <- read_excel("nil_analysis_data.xlsx", sheet=1)

unidata <- read.csv("All_Data_Combined_2019.csv")
unidata <- unidata[c("UNITID", "Institution.Name", "st.abb", "Classification.Name", "Sanction.Name", "Total.Undergraduates", "Unduplicated.Count.Men.s.Participation",
                     "Unduplicated.Count.Women.s.Participation", "Football.Men.s.Team.Revenue", "Grand.Total.Revenue")]

unidata <- unidata[which(unidata$Classification.Name == "NCAA Division I-FBS"),]

unidata$total.participation <- unidata$Unduplicated.Count.Men.s.Participation + unidata$Unduplicated.Count.Men.s.Participation

unidata <- aggregate(cbind(Total.Undergraduates,total.participation, Football.Men.s.Team.Revenue,Grand.Total.Revenue)~st.abb, FUN=sum, data=unidata, na.rm=TRUE, na.action=NULL)

unidata$participation.percentage <- unidata$total.participation/unidata$Total.Undergraduates*100
unidata$revenue.percap.thousands <- unidata$Grand.Total.Revenue/unidata$Total.Undergraduates/1000
unidata$fbrevenue.percap.thousands <- unidata$Football.Men.s.Team.Revenue/unidata$Total.Undergraduates/1000

data <- merge(data, unidata, by="st.abb", all.x=TRUE, all.y=FALSE)

#For FBS only analysis
data$participation.percentage[is.na(data$participation.percentage)==TRUE] <- 0
data$revenue.percap.thousands[is.na(data$revenue.percap.thousands)==TRUE] <- 0
data$fbrevenue.percap.thousands[is.na(data$fbrevenue.percap.thousands)==TRUE] <- 0

## Introduction

aft.int7 <- survreg(Surv(int.months, int) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int7)
exp(coef(aft.int7))
exp(confint(aft.int7))

aft.int8 <- survreg(Surv(int.months, int) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_int, data=data)
summary(aft.int8)
exp(coef(aft.int8))
exp(confint(aft.int8))

## Adoption

aft.adt7 <- survreg(Surv(adopt.months, adopt) ~ revenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt7)
exp(coef(aft.adt7))
exp(confint(aft.adt7))

aft.adt8 <- survreg(Surv(adopt.months, adopt) ~ fbrevenue.percap.thousands + fbs  + participation.percentage + inst6017_nom + neighbor_adopt, data=data)
summary(aft.adt8)
exp(coef(aft.adt8))
exp(confint(aft.adt8))
