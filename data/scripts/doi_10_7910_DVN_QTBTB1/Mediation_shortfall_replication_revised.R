# The Mediation Shortfall: Mapping and Explaining the Surprising Decline of International Mediation
# Research & Politics
# January 30, 2020


# Install packages, if necessary

install.packages("lmtest") 
install.packages("sandwich")
install.packages("stargazer") 

# Open libraries

library(lmtest)
library(sandwich)
library(stargazer)

# Set up workspace and read data

rm(list=ls())

data<-read.csv("~/Dropbox/mediationshortfalldata.csv")
iocap<-read.csv("~/Dropbox/iocap.csv", sep=",")
icbagg<-read.csv("~/Dropbox/icb_agg_data.csv")
osv<-read.csv("~/Dropbox/OSV.csv",sep=";")
colnames(osv)[1]<-"Year"
osv[,2]<-as.numeric(osv[,2])*100


# Figure 1 

data$count<-1
conflict<-aggregate(cbind(Med, Jihadist, jihadmed,count)~Year+ConflictId,data=data,FUN=sum)
conflict$count[conflict$count>1]<-1 # Dyads collapsed into conflicts 
conflict$Med[conflict$Med>1]<-1 
conflict$Jihadist[conflict$Jihadist>1]<-1 
conflict$jihadmed[conflict$jihadmed>1]<-1 
conflict<-aggregate(cbind(Med,Jihadist, jihadmed,count) ~Year, data=conflict, FUN=sum)

par(mfrow=c(1,1))
plot(1989:2013, conflict$count[1:25], ylim=c(0,55), type="l", main="Conflicts and mediated conflicts, 1989-2013", ylab="Count",xlab="Year", lwd=2, yaxs="i")
lines(1989:2013, conflict$Med[1:25], lty=3, lwd=2)
legend("topright",c("Conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.75,bty="n",y.intersp=.5)



# Figure 2 
par(mar = c(5, 4, 4, 4) + 0.3) 
plot(iocap[45:66,2],iocap[45:66,3]/14*100, type="l", lwd=2, main="", ylab="IO mediation capability (1980=100)",xlab="Year",ylim=c(125,240)) # plot aggregate capability as index; 1980 = 100  
par(new = TRUE)
plot(1989:2010, conflict$Med[1:22]/conflict$count[1:22]*100, ylim=c(0,55), type="l", lty=2, axes="F", ylab="",xlab="", lwd=2)
axis(side=4, at = pretty(range( conflict$Med/conflict$count*100)))
mtext("Mediated conflicts (%)", side=4, line=3)
legend("topleft",lty=c(1,2),lwd=2,legend=c("IO mediation capability","Mediated conflicts"),bty="n",cex=1,y.intersp=1)


# Figure 3
plot(1989:2015, conflict$Jihadist, ylim=c(0,30), type="l",main="Mediation of Islamist conflicts", ylab="Count",xlab="Year", lwd=2)
lines(1989:2013, conflict$jihadmed[1:25], lty=3, lwd=2)
legend("topleft",c("Islamist conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=1,bty="n")



# Figure 4 
terror<-aggregate(Terror_SideB~Year+ConflictId, FUN=sum,data)
terror$Terror_SideB[terror$Terror_SideB>1]<-1
terror<-aggregate(Terror_SideB~Year, FUN=sum,data)
terror$count<-conflict$count
plot(1989:2015, terror[,2]/terror[,3]*100, ylim=c(0,50), type="l", main="", ylab="Proportion (%)",xlab="Year", col="black", lwd=2)
lines(1989:2015,osv[,2],lwd=2,lty=3)
legend("topleft",lwd=2,lty=c(1,3),legend=c("Terror-listed","Perpetrators of OSV"),cex=1,bty="n")




# Online appendix

# Table A1


model1<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + RelIncomp +internationalized+factor(Region)+factor(Period), family = binomial("logit"),data = data)

model2<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + RelIncomp +internationalized+factor(Region)+factor(Period), family = binomial("logit"),data = data[data$Region!=3,])

model3<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + RelIncomp +internationalized+factor(Region)+post2001, family = binomial("logit"),data = data)

model4<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + Jihadist +internationalized+factor(Region)+post2001, family = binomial("logit"),data = data)

model5<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + Jihadist +internationalized+factor(Region)+post2001+Jihadist:post2001, family = binomial("logit"),data = data)

model6<-glm(formula = Med ~ medlag + as.factor(IntensityLevel) +logdur + as.factor(Incompatibility) + Terror_SideB +internationalized+factor(Region), family = binomial("logit"),data = data)



cov1        <- vcovHC(model1, type = "HC1",cluster=data$ConflictId)
robust1    <- sqrt(diag(cov1))
cov2       <- vcovHC(model2, type = "HC1",cluster=data$ConflictId)
robust2    <- sqrt(diag(cov2))
cov3        <- vcovHC(model3, type = "HC1",cluster=data$ConflictId)
robust3     <- sqrt(diag(cov3))
cov4       <- vcovHC(model4, type = "HC1",cluster=data$ConflictId)
robust4     <- sqrt(diag(cov4))
cov5        <- vcovHC(model5, type = "HC1",cluster=data$ConflictId)
robust5    <- sqrt(diag(cov5))
cov6        <- vcovHC(model6, type = "HC1",cluster=data$ConflictId)
robust6    <- sqrt(diag(cov6))

stargazer(model1, model2,model3,model4,model5,model6,se=list(robust1,robust2,robust3,robust4,robust5,robust6),digits=2,title="Determinants of mediation onset, 1989-2014", covariate.labels=c("Previous mediation","High intensity","Conflict duration (log)","Government compatibility","Religious claims","Jihadist","Terror-listed","Internationalized","Middle East","Asia","Africa","Americas","1994-1999","2000-2004","2005-2009","2010-2014","Post-2001"),df=F,type="text") 





# Figure A1

temp<-aggregate(cbind(count)~Year+ConflictId,data=data,FUN=sum)
temp<-aggregate(cbind(count)~Year,data=data,FUN=sum)
temp$conflict<-conflict$count

par(mfrow=c(1,1))
plot(1989:2013, temp$count[1:25]/temp$conflict[1:25], ylim=c(0,2), type="l", xlab="Year",ylab="Average dyads per conflict", main="Fragmentation of armed conflicts, 1989-2013",lwd=2)



# Figure A2 (a-b)

time<-aggregate(cbind(Med)~Duration,data=data,FUN=mean)
time2<-aggregate(cbind(count)~Duration,data=data,FUN=sum)
time<-cbind(time,time2[,2])
colnames(time)[3]<-"Conflicts"
time$med<-time$Med*time$Conflicts

par(mfrow=c(1,2))
plot(time$Duration,time$Conflicts, ylim=c(0,220), type="l",xlim=c(0,20),lwd=2, xlab="Conflict duration (Years)", ylab="Conflicts")
lines(time$Duration,time$med,lty=2, lwd=2)
legend("topright",c("All conflicts", "Mediated conflicts"), lty=c(1, 2), lwd=c(2,2),cex=.8,bty="n",y.intersp=1.5)

plot(time$Duration[1:21],time$Med[1:21]*100, ylim=c(0,50), type="l",xlim=c(0,20),lwd=2, xlab="Conflict duration (Years)", ylab="Proportion mediated (%)", main="")



# Figure A3 (a-b) 

data$inter_jihadist<-0
data$inter_jihadist[data$Jihadist==1 & data$internationalized==1]<-1
data$inter_jihadist_med<-0
data$inter_jihadist_med[data$Jihadist==1 & data$internationalized==1 &data$Med==1]<-1
temp<-aggregate(cbind(Med,internationalized, intermed,inter_jihadist,inter_jihadist_med)~Year+ConflictId,data=data,FUN=sum)
temp$count<-1
temp$Med[temp$Med>0]<-1 # All conflicts that received at least one mediation coded as mediated
temp$internationalized[temp$internationalized>0]<-1 
temp$intermed[temp$intermed>0]<-1 
temp$inter_jihadist[temp$inter_jihadist>0]<-1 
temp$inter_jihadist_med[temp$inter_jihadist_med>0]<-1 
temp<-aggregate(cbind(Med,internationalized, intermed,inter_jihadist,inter_jihadist_med,count) ~Year, data=temp, FUN=sum)
temp<-temp[1:25,]
par(mfrow=c(1,2))
plot(1989:2013, temp$internationalized, ylim=c(0,10), type="l", main="(a)", ylab="Count",xlab="Year", lwd=2, yaxs="i")
lines(1989:2013, temp$intermed, lty=3, lwd=2)
legend("topleft",c("Internationalized conflicts","Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.7,bty="n",y.intersp=1.5)
plot(1989:2013, temp$internationalized, ylim=c(0,10), type="l", main="(b)", ylab="Count",xlab="Year", lwd=2, yaxs="i")
lines(1989:2013, temp$inter_jihadist, lty=3, lwd=2)
legend("topleft",c("Internationalized conflicts","Of which Jihadist"), lty=c(1, 3), lwd=c(2,2),cex=.7,bty="n",y.intersp=1.5)


# Figure A4
dyad<-aggregate(cbind(Med,Jihadist, jihadmed,count) ~Year, data=data, FUN=sum)

par(mfrow=c(1,1))
plot(1989:2013, dyad$count[1:25], ylim=c(0,70), type="l", main="Conflict dyads and mediated conflict dyads, 1989-2013", ylab="Count",xlab="Year", lwd=2, yaxs="i")
lines(1989:2013, dyad$Med[1:25], lty=3, lwd=2)
legend("topright",c("Conflict dyads", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.75)



# Figure A5
par(mar = c(5, 4, 4, 4) + 0.3) 
plot(iocap[45:66,2],iocap[45:66,3]/14*100, type="l", lwd=2, main="", ylab="IO mediation capability (1980=100)",xlab="Year",ylim=c(125,240)) # plot aggregate capability as index; 1980 = 100  
par(new = TRUE)
plot(1989:2010, dyad$Med[1:22]/dyad$count[1:22]*100, ylim=c(0,55), type="l", lty=2, axes="F", ylab="",xlab="", lwd=2)
axis(side=4, at = pretty(range( dyad$Med/dyad$count*100)))
mtext("Mediated conflict dyads (%)", side=4, line=3)
legend("topleft",lty=c(1,2),lwd=2,legend=c("IO mediation capability","Mediated conflict dyads"),bty="n",cex=1,y.intersp=1)



# Figure A6 (a-d) 

africa<-data[data$continent=="Africa",]
africa<-aggregate(cbind(Med,count)~ConflictId+Year,data=africa,FUN=sum)
africa$count[africa$count>1]<-1 
africa$Med[africa$Med>1]<-1 
africa<-aggregate(cbind(Med,count) ~Year, data=africa, FUN=sum)
africa<-africa[1:25,]

asia<-data[data$continent=="Asia",]
asia<-aggregate(cbind(Med,count)~ConflictId+Year,data=asia,FUN=sum)
asia$count[asia$count>1]<-1 
asia$Med[asia$Med>1]<-1 
asia<-aggregate(cbind(Med,count) ~Year, data=asia, FUN=sum)
asia<-asia[1:25,]

americas<-data[data$continent=="Americas",]
americas<-aggregate(cbind(Med,count)~ConflictId+Year,data=americas,FUN=sum)
americas$count[americas$count>1]<-1 
americas$Med[americas$Med>1]<-1 
americas<-aggregate(cbind(Med,count) ~Year, data=americas, FUN=sum)
americas<-americas[1:25,]

europe<-data[data$continent=="Europe",]
europe<-aggregate(cbind(Med,count)~ConflictId+Year,data=europe,FUN=sum)
europe$count[europe$count>1]<-1 
europe$Med[europe$Med>1]<-1 
europe<-aggregate(cbind(Med,count) ~Year, data=europe, FUN=sum)
europe<-europe[1:24,]
europe<-rbind(europe, c(1997,0,0))

par(mfrow=c(2,2))
plot(1989:2013, africa$count, ylim=c(0,30), type="l", lwd=2,main="Africa", ylab="Count",xlab="Year" )
lines(1989:2013, africa$Med,lty=3, lwd=2)
legend("topleft",c("Conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.6,bty="n")

plot(1989:2013, asia$count, ylim=c(0,30), type="l", lwd=2,main="Asia", ylab="Count",xlab="Year" )
lines(1989:2013, asia$Med,lty=3, lwd=2)
legend("topleft",c("Conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.6,bty="n")

plot(1989:2013, americas$count, ylim=c(0,10), type="l", lwd=2,main="Americas", ylab="Count",xlab="Year" )
lines(1989:2013, americas$Med,lty=3, lwd=2)
legend("topleft",c("Conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.6,bty="n")

plot(c(1989:2013), europe$count, ylim=c(0,10), type="l", lwd=2,main="Europe", ylab="Count",xlab="Year" )
lines(c(1989:2013), europe$Med, ylim=c(0,10),lty=3, lwd=2)
legend("topleft",c("Conflicts", "Of which mediated"), lty=c(1, 3), lwd=c(2,2),cex=.6,bty="n")



# Figure A7

par(mfrow=c(1,1))
plot(icbagg$yrtrig, icbagg$med/icbagg$count, type="l", lwd=2, ylim=c(0,1.1), ylab="Proportion mediated",xlab="Year", main="Proportion of crises that receive mediation, 1989-2013")


# Figure A8

plot(icbagg$yrtrig, icbagg$offmed/icbagg$count, type="l", lwd=2, ylim=c(0,1), ylab="Proportion of crises",xlab="Year", main="Mediation offered, not accepted, 1989-2013")




# Figure A9

temp<-aggregate(cbind(IntensityLevel,Med,count)~Year+ConflictId,data=data,FUN=sum)
temp$count[temp$count>1]<-1 # Dyads collapsed into conflicts 
temp$IntensityLevel[temp$IntensityLevel>1]<-2 # Coding all conflicts with dyads of differnet intensity level (i.e., a mean above 1) as 2s
temp$Med[temp$Med>0]<-1
temp<-temp[temp$IntensityLevel==2,]
temp<-aggregate(cbind(Med,count) ~Year, data=temp, FUN=sum)
temp<-temp[1:25,]

par(mfrow=c(1,2))
plot(1989:2013, (temp$Med/temp$count)*100, ylim=c(0,100), type="l",  ylab="Proportion (%)",xlab="Year", lwd=2,main="High-intensity conflicts")

# Low-intensity only
temp<-aggregate(cbind(IntensityLevel,Med,count)~Year+ConflictId,data=data,FUN=sum)
temp$count[temp$count>1]<-1 # Dyads collapsed into conflicts 
temp$IntensityLevel[temp$IntensityLevel>1]<-2 # Coding all conflicts with dyads of differnet intensity level (i.e., a mean above 1) as 2s
temp$Med[temp$Med>0]<-1
temp<-temp[temp$IntensityLevel==1,]

temp<-aggregate(cbind(Med,count) ~Year, data=temp, FUN=sum)
temp<-temp[1:25,]

plot(1989:2013, (temp$Med/temp$count)*100, ylim=c(0,100), type="l",  ylab="Proportion (%)",xlab="Year", lwd=2,main="Low-intensity conflicts")


