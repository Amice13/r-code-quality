############## ESTIMATING MODELS #########################
#clean up and options
rm(list=ls())
options(scipen=10,digits=10)

#set working directory, set to YOUR directory
#setwd('/Volumes/MONOGAN/polluterPlacement/data')
#setwd('/Users/jamie/Desktop/replication/')
#setwd('/Users/jamie/Documents/polluterPlacement/data')

#load libraries
library(spatstat)
library(maptools)
library(rgdal)
library(maps)
library(foreign)
library(mgcv)
#library(rgeos)
library(xtable)
library(akima) #for drawing image/surface plots
library(scatterplot3d)
library(lattice)
library(ggplot2)
library(MASS)
library(car)
library(spdep)

#Load data
full<-read.csv('majorAirAllDist.csv')

#subset to data under study
base<-subset(full,air==1 | lqg==1)

#census region variable
base$region<-NA
base$region[base$loc_state%in%c('DE','MD','DC','VA','WV','KY','NC','TN','SC','GA','FL','AL','MS','LA','AR','OK','TX')]<-"south"
base$region[base$loc_state%in%c('ME','NH','VT','MA','RI','CT','NY','NJ','PA')]<-"northeast"
base$region[base$loc_state%in%c('OH','MI','IN','IL','WI','MN','IA','MO','ND','SD','NE','KS')]<-"midwest"
base$region[base$loc_state%in%c('MT','WY','CO','NM','AZ','UT','ID','WA','OR','NV','CA')]<-"west"

#subset cases and controls separately
aironly<-subset(base, air==1)
lqgonly<-subset(base, lqg==1)

#How much TRI?
100*table(as.numeric(base$airtotal>0))/sum(table(as.numeric(base$airtotal>0)))
100*table(as.numeric(base$airtotal[base$air==1]>0))/sum(table(as.numeric(base$airtotal[base$air==1]>0)))

####BASE MODELS WITH ROBUSTNESS CHECKS###
#Standardized Downwind Distance
s.down.mod<-gam(air~sDown+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(s.down.mod); s.down.mod$aic

#GAM model: Non-Standardized Downwind Distance
gam.air<-gam(air~down+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(gam.air); gam.air$aic

#Standardized Upwind Distance
s.up.mod<-gam(air~sUp+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(s.up.mod); s.up.mod$aic

#Standardized Eastern Distance
s.east.mod<-gam(air~sEast+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(s.east.mod); s.east.mod$aic

#Standardized Western Distance
s.west.mod<-gam(air~sWest+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(s.west.mod); s.west.mod$aic

#pdf('smoothed.pdf')
vis.gam(s.down.mod,view=c("longitude","latitude"),color='gray',plot.type='contour',type='link',cond=list(distance=0),main="", xlab="Longitude", ylab="Latitude")
points(x=full$longitude, y=full$latitude, pch='.')
#dev.off()

####REVIEWER-SUGGESTED###
#Spatial correlation in residuals
us.states<-map("state", fill=TRUE, plot=TRUE, resolution=0) #too cluttered, projection=???
IDs <- sapply(strsplit(us.states$names, ":"), function(x) x[1])
us.map.0<-map2SpatialPolygons(us.states, IDs=IDs)
us.map<-us.map.0[-8]
state.nb<-poly2nb(us.map)
state.coords<-coordinates(us.map)
plot(state.nb, coords=state.coords)
moran.test(as.vector(by(s.down.mod$residuals,INDICES=base$loc_state,FUN=mean)),nb2listw(state.nb,style='W'))
geary.test(as.vector(by(s.down.mod$residuals,INDICES=base$loc_state,FUN=mean)),nb2listw(state.nb,style='W'))

#Spatial correlation in raw averages
moran.test(log(as.vector(by(aironly$sDown,INDICES=aironly$loc_state,FUN=mean))/as.vector(by(lqgonly$sDown,INDICES=lqgonly$loc_state,FUN=mean))),nb2listw(state.nb,style='W'))
geary.test(log(as.vector(by(aironly$sDown,INDICES=aironly$loc_state,FUN=mean))/as.vector(by(lqgonly$sDown,INDICES=lqgonly$loc_state,FUN=mean))),nb2listw(state.nb,style='W'))

#Spatial correlation in green index
moran.test(as.vector(by(base$gi_net,INDICES=base$loc_state,FUN=head,1)),nb2listw(state.nb,style='W'))
geary.test(as.vector(by(base$gi_net,INDICES=base$loc_state,FUN=head,1)),nb2listw(state.nb,style='W'))

#fixed effects, with population density
fe.pop.mod<-gam(air~sDown+I(popDens/1000)+as.factor(loc_state)+s(longitude,latitude), data=base, family=binomial(link='logit'));summary(fe.pop.mod);fe.pop.mod$aic 

#LaTeX code for output table
fe.pop.coef<-fe.pop.mod$coef[1:3]
fe.pop.se<-sqrt(diag(fe.pop.mod$Vp[1:3,1:3]))
fe.pop.z<-fe.pop.coef/fe.pop.se
fe.pop.p<-2*(1-pnorm(abs(fe.pop.z)))
fe.pop.output<-cbind(fe.pop.coef,fe.pop.se,fe.pop.z,fe.pop.p)
rownames(fe.pop.output)<-c("Intercept","Scaled distance from leeward border","Population density (1000/square mile)")
colnames(fe.pop.output)<-c("Coefficient","Std. Err.","z-ratio","p-value")
xtable(fe.pop.output,digits=4,caption="Model of Spatial Variation in Risk of Major Air Polluter Placement, Including Fixed Effects for States (Generalized Additive Model, Logit Link)",label="fe.mod",align="lrrrr")

#convert wind speed to kilomters per hour
base$windSpeed<-base$windSpeed*1.60934

#regional model
region.mod<-gam(air~sDown*as.factor(region)+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(region.mod); region.mod$aic 

#regional effects
region.results<-c(NA,4)
region.se<-rep(NA,4)
region.names<-c('Midwest','Northeast','South','West')

region.results[1]<-region.mod$coef[2] #midwest baseline
region.results[2]<-region.mod$coef[2]+ region.mod$coef[6] #northeast
region.results[3]<-region.mod$coef[2]+ region.mod$coef[7] #south
region.results[4]<-region.mod$coef[2]+ region.mod$coef[8] #west

region.se[1]<-sqrt(region.mod$Vp[2,2])
region.se[2]<-sqrt(region.mod$Vp[2,2]+2*region.mod$Vp[2,6]+region.mod$Vp[6,6])
region.se[3]<-sqrt(region.mod$Vp[2,2]+2*region.mod$Vp[2,7]+region.mod$Vp[7,7])
region.se[4]<-sqrt(region.mod$Vp[2,2]+2*region.mod$Vp[2,8]+region.mod$Vp[8,8])

region.lower<-region.results-region.se*1.645
region.lower.alt<- -10 #arbitrarily low number to go off the scale
region.upper<-region.results+region.se*1.645

#pdf('regionConditioning.pdf')
#pdf('regionConditioningAlt.pdf')
par(mar=c(5,6,4,2))
plot(x=region.results,y=c(1:4),xlim=c(min(region.lower),max(region.upper)),axes=F,xlab="Coefficient",ylab="",col='red',lwd=2)
axis(1)
axis(2,at=c(1:4),labels=region.names,las=2)
arrows(x0=region.lower,x1=region.upper,y0=c(1:4),length=0,lty=2,col='red',lwd=2)
#arrows(x0=region.lower.alt,x1=region.upper,y0=c(1:4),length=0,lty=2,col='red',lwd=2)
abline(v=0,col='gray60')
box()
#dev.off()

#wind speed model
wind.mod<-gam(air~down*windSpeed+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(wind.mod); wind.mod$aic 

#effect of distance by wind
summary(base$windSpeed)
quantile(base$windSpeed,c(.3,.31))
potential.wind<-seq(10.5,26.3,.01)
dist.effect<-wind.mod$coef[2]+wind.mod$coef[4]*potential.wind
dist.se<-sqrt(wind.mod$Vp[2,2]+2*potential.wind*wind.mod$Vp[2,4]+(potential.wind^2)*wind.mod$Vp[4,4])
dist.lower<-dist.effect-1.645*dist.se
dist.upper<-dist.effect+1.645*dist.se
limiter<-c(dist.lower,dist.upper)

#draw graph of effect
#pdf('windConditioning.pdf')
plot(x=potential.wind,y=dist.effect,xlab="Average Wind Speed (km/h)",ylab="Effect of Distance in Kilometers",type='l',ylim=c(min(limiter),max(limiter)),lwd=2)
abline(h=0,col='gray60')
lines(x=potential.wind,y=dist.upper,col='red',lty=3,lwd=2)
#lines(x=potential.wind,y=dist.lower,col='red',lty=3)
points(x=12,y=0,pch=20)
text(x=13,y=.00005,"12 km/h")#,pos=4)
#dev.off()

#threshold model
thresholds<-seq(.05,.5,by=.05)
thresh.aic<-rep(NA,length(thresholds))
thresh.coef<-matrix(NA,ncol=3,nrow=length(thresholds))
potential.distances<-seq(0,1,.01)
thresh.util<-matrix(NA,ncol=length(potential.distances),nrow=length(thresholds))
for (i in 1:length(thresholds)){
	hold<-gam(air~sDown+as.numeric(sDown>thresholds[i]):I(sDown-thresholds[i])+s(longitude,latitude), data=base, family=binomial(link='logit'))
	thresh.aic[i]<-hold$aic
	thresh.coef[i,]<-hold$coef[1:3]
	thresh.util[i,]<-thresh.coef[i,1]+thresh.coef[i,2]*potential.distances+thresh.coef[i,3]*(potential.distances-thresholds[i])*as.numeric(potential.distances>thresholds[i])
}
which.min(thresh.aic)
thresh.mod<-gam(air~sDown+as.numeric(sDown>.1):I(sDown-.1)+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(thresh.mod); thresh.mod$aic

#joint effect after knot point
spline.eff<-thresh.mod$coef[2]+thresh.mod$coef[3]; spline.eff
spline.se<-sqrt(thresh.mod$Vp[2,2]+2*thresh.mod$Vp[2,3]+thresh.mod$Vp[3,3]); spline.se
spline.eff/spline.se
1-pnorm(abs(spline.eff/spline.se))

#relevant descriptive statistics
summary(base$sDown)#median=.2875556
quantile(base$sDown,c(.18,.19,.2))#18%

#produce output of threshold model for LaTeX
coef.thresh<-thresh.mod$coef[1:3]
se.thresh<-sqrt(diag(thresh.mod$Vp))[1:3]
z.thresh<-thresh.mod$coef[1:3]/sqrt(diag(thresh.mod$Vp))[1:3]
p.thresh<-2*(1-pnorm(abs(z.thresh)))
thresh.output<-cbind(coef.thresh,se.thresh,z.thresh,p.thresh)
rownames(thresh.output)<-c("Intercept","Scaled distance from leeward border","Switch in effect for scaled distance greater than 0.1")
colnames(thresh.output)<-c("Coefficient","Std. Err.","z-ratio","p-value")
xtable(thresh.output,digits=4,caption="Model of Spatial Variation in Risk of Major Air Polluter Placement with Spline Effect for Scaled Distance (Generalized Additive Model, Logit Link)",label="thresh.mod",align="lrrrr")

#plot spline effect
potential.distances<-seq(0,1,.01)
spline.distances<-(potential.distances-.1)*as.numeric(potential.distances>.1)
util<-0.11383885-4.36463447*potential.distances+4.32444301*spline.distances
#pdf('splineFunction.pdf')
plot(y=plogis(util),x=potential.distances,xlab="Scaled Distance to Downwind Border",ylab="Probability of Major Air Polluter",type='n',ylim=c(.35,.6))
for(i in 1:length(thresholds)){
	lines(y=plogis(thresh.util[i,]),x=potential.distances,col='gray60')
	}
lines(y=plogis(util),x=potential.distances,col='red',lwd=3)
#dev.off()

#Green index against proportion of air pollutants
#pdf("greenAir.pdf")
green.air<-cbind(as.matrix(by(base$air,INDICES=base$loc_state,FUN=mean)),as.matrix(by(base$gi_net,INDICES=base$loc_state,FUN=head,1)))
plot(x=green.air[,2],y=green.air[,1],xlab="Green Index",ylab="Proportion of Air Polluters",type="n")
abline(lm(green.air[,1]~green.air[,2]))
text(rownames(green.air),x=green.air[,2],y=green.air[,1])
#dev.off()

####PUBLIC V. PRIVATE###
#Public
s.down.public<-gam(air~sDown+s(longitude,latitude), data=base, subset=public==1|air==0, family=binomial(link='logit')); summary(s.down.public); s.down.public$aic

#Private
s.down.private<-gam(air~sDown+s(longitude,latitude), data=base, subset=public==0, family=binomial(link='logit')); summary(s.down.private); s.down.private$aic

####MANUFACTURING MODEL###
base$sicGen<-trunc(base$sic1/100)
base$allManuf<-as.numeric(base$sicGen>=29 & base$sicGen<=39)
manuf.mod<-gam(air~sDown+s(longitude,latitude), data=base, subset=allManuf==1, family=binomial(link='logit')); summary(manuf.mod); manuf.mod$aic

####MODEL WITH STATE INTERACTIONS####
s.down.inter<-gam(air~sDown*gi_net+sDown*ed_locational+sDown*env_ig+s(longitude,latitude), data=base, family=binomial(link='logit')); summary(s.down.inter); s.down.inter$aic
vcov<-summary(s.down.inter)$cov.scaled[1:8,1:8]
#vis.gam(s.down.inter,view=c("longitude","latitude"),color='gray',plot.type='contour',type='link',cond=list(distance=0),main="", xlab="Longitude", ylab="Latitude")
#points(x=full$longitude, y=full$latitude, pch='.')

mean.effect<-.21088689+(.014806179*21.74488)+(-.343244064*1.4202632)+(-.012749319*17.86226)
mean.se<-sqrt(vcov[2,2]+(21.74488^2)*vcov[6,6]+(1.4202632^2)*vcov[7,7]+(17.86226^2)*vcov[8,8]+2*21.74488*vcov[2,6]+2*1.4202632*vcov[2,7]+2*17.86226*vcov[2,8]+2*21.74488*1.4202632*vcov[6,7]+2*21.74488*17.86226*vcov[6,8]+2*1.4202632* 17.86226*vcov[7,8])

low.green.effect<-.21088689+(.014806179*4)+(-.343244064*1.4202632)+(-.012749319*17.86226)
low.green.se<-sqrt(vcov[2,2]+(4^2)*vcov[6,6]+(1.4202632^2)*vcov[7,7]+(17.86226^2)*vcov[8,8]+2*4*vcov[2,6]+2*1.4202632*vcov[2,7]+2*17.86226*vcov[2,8]+2*4*1.4202632*vcov[6,7]+2*4*17.86226*vcov[6,8]+2*1.4202632* 17.86226*vcov[7,8])

high.green.effect<-.21088689+(.014806179*34)+(-.343244064*1.4202632)+(-.012749319*17.86226)
high.green.se<-sqrt(vcov[2,2]+(34^2)*vcov[6,6]+(1.4202632^2)*vcov[7,7]+(17.86226^2)*vcov[8,8]+2*34*vcov[2,6]+2*1.4202632*vcov[2,7]+2*17.86226*vcov[2,8]+2*34*1.4202632*vcov[6,7]+2*34*17.86226*vcov[6,8]+2*1.4202632* 17.86226*vcov[7,8])

low.loc.effect<-.21088689+(.014806179*21.74488)+(-.343244064*0.9333333)+(-.012749319*17.86226)
low.loc.se<-sqrt(vcov[2,2]+(21.74488^2)*vcov[6,6]+(0.9333333^2)*vcov[7,7]+(17.86226^2)*vcov[8,8]+2*21.74488*vcov[2,6]+2*0.9333333*vcov[2,7]+2*17.86226*vcov[2,8]+2*21.74488*0.9333333*vcov[6,7]+2*21.74488*17.86226*vcov[6,8]+2*0.9333333* 17.86226*vcov[7,8])

high.loc.effect<-.21088689+(.014806179*21.74488)+(-.343244064* 2.2933333)+(-.012749319*17.86226)
high.loc.se<-sqrt(vcov[2,2]+(21.74488^2)*vcov[6,6]+(2.2933333 ^2)*vcov[7,7]+(17.86226^2)*vcov[8,8]+2*21.74488*vcov[2,6]+2* 2.2933333*vcov[2,7]+2*17.86226*vcov[2,8]+2*21.74488* 2.2933333*vcov[6,7]+2*21.74488*17.86226*vcov[6,8]+2* 2.2933333* 17.86226*vcov[7,8])

low.int.effect<-.21088689+(.014806179*21.74488)+(-.343244064*1.4202632)+(-.012749319* 2.50000)
low.int.se<-sqrt(vcov[2,2]+(21.74488^2)*vcov[6,6]+(1.4202632^2)*vcov[7,7]+(2.50000 ^2)*vcov[8,8]+2*21.74488*vcov[2,6]+2*1.4202632*vcov[2,7]+2* 2.50000*vcov[2,8]+2*21.74488*1.4202632*vcov[6,7]+2*21.74488* 2.50000*vcov[6,8]+2*1.4202632* 2.50000*vcov[7,8])

high.int.effect<-.21088689+(.014806179*21.74488)+(-.343244064*1.4202632)+(-.012749319*52.5)
high.int.se<-sqrt(vcov[2,2]+(21.74488^2)*vcov[6,6]+(1.4202632^2)*vcov[7,7]+(52.5^2)*vcov[8,8]+2*21.74488*vcov[2,6]+2*1.4202632*vcov[2,7]+2*52.5*vcov[2,8]+2*21.74488*1.4202632*vcov[6,7]+2*21.74488*52.5*vcov[6,8]+2*1.4202632* 52.5*vcov[7,8])

effects<-c(low.green.effect,mean.effect,high.green.effect,low.loc.effect,mean.effect,high.loc.effect,low.int.effect,mean.effect,high.int.effect)
std.errors<-c(low.green.se,mean.se,high.green.se,low.loc.se,mean.se,high.loc.se,low.int.se,mean.se,high.int.se)
lowers<-effects-1.645*std.errors
uppers<-effects+1.645*std.errors
locations<-c(1:3,6:8,11:13)
colors<-c('red','black','blue','red','black','blue','red','black','blue')
characters<-c(1:3,1:3,1:3)
spread<-c(min(lowers),max(uppers))

#pdf('forestInteraction.pdf')
par(omi=c(.2,1.1,.1,.01))
plot(x=effects,y=locations,xlim=spread,ylim=c(0,14), xlab="Conditional Coefficient for Leeward Distance", ylab="",axes=F,col=colors,pch=characters,cex=1.5,lwd=2)
axis(1)
axis(2, at=locations, labels=c('Low Green Index','Mean','High Green Index','Low Loc. Development','Mean','High Loc. Development','Low Interest Groups','Mean','High Interest Groups'),las=1)
box()
segments(x0=lowers,x1=uppers,y0=locations,y1=locations, lwd=2,lty=characters,col=colors)
#arrows(x0=dist.low.80,x1=dist.high.80,y0=c(1:length(dist.coef)),y1=c(1:length(dist.coef)), lwd=3,col='red',angle=90,length=.05,code=3)
abline(v=0,col='gray60')
#dev.off()

#pdf('forestInteractionOneTail.pdf')
lowers<- -10
par(omi=c(.2,1.1,.1,.01))
plot(x=effects,y=locations,xlim=spread,ylim=c(0,14), xlab="Conditional Coefficient for Leeward Distance", ylab="",axes=F,col=colors,pch=characters,cex=1.5,lwd=2)
axis(1)
axis(2, at=locations, labels=c('Low Green Index','Mean','High Green Index','Low Loc. Development','Mean','High Loc. Development','Low Interest Groups','Mean','High Interest Groups'),las=1)
box()
segments(x0=lowers,x1=uppers,y0=locations,y1=locations, lwd=2,lty=characters,col=colors)
#arrows(x0=dist.low.80,x1=dist.high.80,y0=c(1:length(dist.coef)),y1=c(1:length(dist.coef)), lwd=3,col='red',angle=90,length=.05,code=3)
abline(v=0,col='gray60')
#dev.off()

####ROBUSTNESS CHECK WITHOUT POWER PLANTS####
s.down.mod.nopower<-gam(air~sDown+s(longitude,latitude), data=base, subset=power==0, family=binomial(link='logit')); summary(s.down.mod.nopower); s.down.mod.nopower$aic

####ROBUSTNESS CHECK WITH POWER PLANTS ONLY####
s.down.mod.power<-gam(air~sDown+s(longitude,latitude), data=base, subset=power==1|air==0, family=binomial(link='logit')); summary(s.down.mod.power); s.down.mod.power$aic

####MAPS OF SELECTED STATES####
ca<-subset(base,loc_state=="CA")
ca.map<-map("state","california",resolution=0)
#pdf('california.pdf')
plot(ca.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ca.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='CA'], y=base$latitude[base$air==0&base$loc_state=='CA'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='CA'], y=base$latitude[base$air==1&base$loc_state=='CA'], pch='+',col='black')
#dev.off()

#pdf('california0.pdf')
plot(ca.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ca.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='CA'], y=base$latitude[base$air==0&base$loc_state=='CA'], pch=20,col='white')
#dev.off()

ind<-subset(base,loc_state=="IN")
ind.map<-map("state","indiana",resolution=0)
#pdf('indiana.pdf')
plot(ind.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ind.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='IN'], y=base$latitude[base$air==0&base$loc_state=='IN'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='IN'], y=base$latitude[base$air==1&base$loc_state=='IN'], pch='+',col='black')
#dev.off()

#pdf('indiana0.pdf')
plot(ind.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ind.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='IN'], y=base$latitude[base$air==0&base$loc_state=='IN'], pch=20,col='white')
#dev.off()

mi<-subset(base,loc_state=="MI")
mi.map<-map("state","michigan",resolution=0)
#pdf('michigan.pdf')
plot(mi.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(mi.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='MI'], y=base$latitude[base$air==0&base$loc_state=='MI'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='MI'], y=base$latitude[base$air==1&base$loc_state=='MI'], pch='+',col='black')
#dev.off()

#pdf('michigan0.pdf')
plot(mi.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(mi.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='MI'], y=base$latitude[base$air==0&base$loc_state=='MI'], pch=20,col='white')
#dev.off()

ia<-subset(base,loc_state=="IA")
ia.map<-map("state","iowa",resolution=0)
#pdf('iowa.pdf')
plot(ia.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ia.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='IA'], y=base$latitude[base$air==0&base$loc_state=='IA'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='IA'], y=base$latitude[base$air==1&base$loc_state=='IA'], pch='+',col='black')
#dev.off()

#pdf('iowa0.pdf')
plot(ia.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ia.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='IA'], y=base$latitude[base$air==0&base$loc_state=='IA'], pch=20,col='white')
#dev.off()

tx<-subset(base,loc_state=="TX")
tx.map<-map("state","texas",resolution=0)
#pdf('texas.pdf')
plot(tx.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(tx.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='TX'], y=base$latitude[base$air==0&base$loc_state=='TX'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='TX'], y=base$latitude[base$air==1&base$loc_state=='TX'], pch='+',col='black')
#dev.off()

#pdf('texas0.pdf')
plot(tx.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(tx.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='TX'], y=base$latitude[base$air==0&base$loc_state=='TX'], pch=20,col='white')
#dev.off()

ga<-subset(base,loc_state=="GA")
ga.map<-map("state","georgia",resolution=0)
#pdf('georgia.pdf')
#jpeg('georgiaPollution.jpg',width=870,height=300,quality=100)
#jpeg('georgiaPollution.jpg',width=300,height=300)
#plot(ga.map,type='n',xlab="",ylab="",axes=F,asp=1)
plot(ga.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ga.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='GA'], y=base$latitude[base$air==0&base$loc_state=='GA'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='GA'], y=base$latitude[base$air==1&base$loc_state=='GA'], pch='+',col='black')
#dev.off()

#pdf('georgia0.pdf')
plot(ga.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(ga.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='GA'], y=base$latitude[base$air==0&base$loc_state=='GA'], pch=20,col='white')
#dev.off()

mn<-subset(base,loc_state=="MN")
mn.map<-map("state","minnesota",resolution=0)
#pdf('minnesota.pdf')
plot(mn.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(mn.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='MN'], y=base$latitude[base$air==0&base$loc_state=='MN'], pch=20,col='white')
points(x=base$longitude[base$air==1&base$loc_state=='MN'], y=base$latitude[base$air==1&base$loc_state=='MN'], pch='+',col='black')
#dev.off()

#pdf('minnesota0.pdf')
plot(mn.map,type='n',xlab="Longitude", ylab="Latitude",asp=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "gray60")
lines(mn.map,col='yellow',lwd=3)
points(x=base$longitude[base$air==0&base$loc_state=='MN'], y=base$latitude[base$air==0&base$loc_state=='MN'], pch=20,col='white')
#dev.off()

####TOXICITY ANALYSIS####
median(base$airtotal[base$air==1 & base$airtotal>0])

#create subsets
no.tox<-subset(base,airtotal==0)
all.tox<-subset(base,air==0 | airtotal!=0)
low.tox<-subset(base,air==0 | airtotal<18372.59961)
high.tox<-subset(base,air==0 | airtotal>=18372.59961)

#GAM models
gam.no<-gam(air~sDown+s(longitude,latitude), data=no.tox, family=binomial(link='logit')); summary(gam.no)
gam.low<-gam(air~ sDown +s(longitude,latitude), data=low.tox, family=binomial(link='logit')); summary(gam.low)
gam.all<-gam(air~ sDown +s(longitude,latitude), data=all.tox, family=binomial(link='logit')); summary(gam.all)
gam.high<-gam(air~ sDown +s(longitude,latitude), data=high.tox, family=binomial(link='logit')); summary(gam.high)

##FOREST PLOT##
dist.coef<-c(-0.21889425,-0.19814016, -0.25295336, -0.31799832,-0.45864086)
dist.se<-c(0.05250080,0.04934241,  0.04721229,0.06949821,0.09259778)
dist.low<-(dist.coef-1.645*dist.se)
dist.high<-(dist.coef+1.645*dist.se)
dist.low.80<-(dist.coef-1.282*dist.se)
dist.high.80<-(dist.coef+1.282*dist.se)

#pdf('forest.pdf')
par(omi=c(.2,1.1,.1,.01))
plot(x=dist.coef,y=c(1:length(dist.coef)),xlim=c(-.65,.1),ylim=c(1,length(dist.coef)), xlab="Coefficients for Leeward Distance", ylab="",axes=F,col='red',pch=16,cex=1.5)
axis(1)
axis(2, at=c(1:length(dist.coef)), labels=c('No toxic','Low toxic','Full data','Any toxic','High toxic'),las=1)
box()
segments(x0=dist.low,x1=dist.high,y0=c(1:length(dist.coef)),y1=c(1:length(dist.coef)), lwd=2,col='red')
#arrows(x0=dist.low.80,x1=dist.high.80,y0=c(1:length(dist.coef)),y1=c(1:length(dist.coef)), lwd=3,col='red',angle=90,length=.05,code=3)
abline(v=0,col='gray60')
#dev.off()

####NO CONTROL GROUP: INHOMOGENOUS POISSON PROCESS###
air.ppp<-ppp(x=aironly$longitude,y=aironly$latitude,window=owin(xrange=c(min(base$longitude),max(base$longitude)),yrange=c(min(base$latitude),max(base$latitude))))
sdown.ppp<-ppp(x=base$longitude,y=base$latitude,marks=base$sDown,window=owin(xrange=c(min(base$longitude),max(base$longitude)),yrange=c(min(base$latitude),max(base$latitude))))
Z<-idw(sdown.ppp)
air.ppm.gam<-ppm(Q=air.ppp,trend=~std.down+s(x,y),covariates=list(std.down=Z),use.gam=TRUE); summary(air.ppm.gam); AIC(air.ppm.gam)

lqg.ppp<-ppp(x=lqgonly$longitude,y=lqgonly$latitude,window=owin(xrange=c(min(base$longitude),max(base$longitude)),yrange=c(min(base$latitude),max(base$latitude))))
sdown.ppp.lqg<-ppp(x=base$longitude,y=base$latitude,marks=base$sDown,window=owin(xrange=c(min(base$longitude),max(base$longitude)),yrange=c(min(base$latitude),max(base$latitude))))
Z.lqg<-idw(sdown.ppp.lqg)
lqg.ppm.gam<-ppm(Q=lqg.ppp,trend=~std.down+s(x,y),covariates=list(std.down=Z.lqg),use.gam=TRUE); summary(lqg.ppm.gam); AIC(lqg.ppm.gam)
