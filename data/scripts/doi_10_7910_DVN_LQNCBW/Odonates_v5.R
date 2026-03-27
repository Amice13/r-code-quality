### Analyzing odonate orientation data ###
require(circular)
require(plotrix)
require(oce)

source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/read.focal.data.v1.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/read.focal.data.v2.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/read.orientation.data.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/SpecMeanTestRes.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/SpecMeanTestBoot.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/rFLCorrCoeff.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/rFLIndTestRand.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/rhoFLCIBoot.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/JSTestRand.R')
source('~/Desktop/Coursework/FloridaCourse/R/AuxillaryRScripts/sunPosition.R')

### Site lat and lon ###
lat<-27.13490
lon<-(-81.36139)

### Read in data ###
o_data<-list.files("~/Desktop/Coursework/FloridaCourse/Data/Orientation",full.names=T)
f_data<-list.files("~/Desktop/Coursework/FloridaCourse/Data/Focal",full.names=T)

### Parse f_data into new and old format ###
f_data_v1<-f_data[grep("Mar",f_data)]
f_data_v2<-f_data[(!1:length(f_data) %in% grep("Mar",f_data))]

### Process orientation data ###
o_list<-list()
for(i in 1:length(o_data)){
	o_list[[i]]<-read.orientation.data(o_data[i])
}

### Process focal data ###
f_list_v1<-list()
for(i in 1:length(f_data_v1)){
	f_list_v1[[i]]<-read.focal.data.v1(f_data_v1[[i]])
}
f_list_v2<-list()
for(i in 1:length(f_data_v2)){
	f_list_v2[[i]]<-read.focal.data.v2(f_data_v2[[i]])
}

f_list<-c(f_list_v1,f_list_v2) #Combine v1 and v2 data sets
f_data<-c(f_data_v1,f_data_v2)

#### ACTIVITY LEVEL ANALYSES ###
### Looking at foraging data ###
f_forage<-sapply(f_list,function(x) x$forage)
f_territorial<-sapply(f_list,function(x) x$territorial)
f_fly<-sapply(f_list,function(x) x$fly)
f_nflight<-sapply(f_list,function(x) x$nflight)
f_windspeed<-sapply(f_list,function(x) x$wind.speed)
f_windspeed<-(f_windspeed*1000/3600)
f_temp<-sapply(f_list,function(x) x$temp)
f_temp <-(f_temp-32) *5/9 #Convert to Celcius

model3<-glm(log(f_fly+1)~log(f_windspeed+1)+f_temp)
summary(model3)
hist(residuals(model3))

model4<-glm(f_nflight~log(f_windspeed+1)+f_temp,family="poisson")
summary(model4)
hist(residuals(model4))

model5<-glm(f_forage~log(f_windspeed+1)+f_temp,family="poisson")
summary(model5)
hist(residuals(model5))

model6<-glm(f_territorial~log(f_windspeed+1)+f_temp,family="poisson")
summary(model6)
hist(residuals(model6))

pdf(width=6.5,height=6.5/4,file="~/Desktop/Coursework/FloridaCourse/Figures/Fig2_ForagingVsWinddir_v2.pdf")
par(mfrow=c(1,4))
par(mar=c(2.25,2.25,0.5,0.5))

plot(log(f_fly+1)~log(f_windspeed+0.01),axes=F,ylim=range(log(f_fly+1)*1.1))
axis(1,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.6)
axis(2,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("log(flight time) (sec)",side=2,line=1.25,cex=0.6)
box()
corner.label("A",font=2,cex=1)
corner.label(bquote(beta~" = "~.(format(round(summary(model3)$coefficients[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(model3)$coefficients[2,4],digits=3))),x=1,y=1,cex=0.6)

plot(f_nflight~log(f_windspeed+0.01),axes=F,ylim=range(f_nflight)*1.1)
axis(1,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.6)
axis(2,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("number of flights",side=2,line=1.25,cex=0.6)
box()
corner.label("B",font=2,cex=1)
corner.label(bquote(beta~" = "~.(format(round(summary(model4)$coefficients[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(model4)$coefficients[2,4],digits=3))),x=1,y=1,cex=0.6)

plot(f_forage~log(f_windspeed+0.01),axes=F,ylim=range(f_forage)*1.1)
axis(1,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.6)
axis(2,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("foraging attempts",side=2,line=1.25,cex=0.6)
box()
corner.label("C",font=2,cex=1)
corner.label(bquote(beta~" = "~.(format(round(summary(model5)$coefficients[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(model5)$coefficients[2,4],digits=3))),x=1,y=1,cex=0.6)

plot(f_territorial~log(f_windspeed+0.01),axes=F,ylim=range(f_territorial)*1.1)
axis(1,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.6)
axis(2,mgp=c(0.1,0.2,0),cex.axis=0.7,tck=-0.025)
mtext("territorial interactions",side=2,line=1.25,cex=0.6)
box()
corner.label("D",font=2,cex=1)
corner.label(bquote(beta~" = "~.(format(round(summary(model6)$coefficients[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(model6)$coefficients[2,4],digits=3))),x=1,y=1,cex=0.6)
dev.off()

### Name observations by date and time ###
names(o_data)<-sapply(strsplit(o_data,"/"),function(x) x[length(x)])
names(f_data)<-sapply(strsplit(f_data,"/"),function(x) x[length(x)])

###
names(o_data)

### Process orientation data ###
o_circ<-lapply(o_list,function(x) x$data)
o_var<-sapply(o_circ,angular.variance)
o_mean<-circular(sapply(o_circ,mean),zero=pi/2)
o_wind<-sapply(o_list,function(x) x$wind.speed)
o_wind<-(o_wind*1000/3600) #convert from kilometers per hour to m/s
o_gust<-sapply(o_list,function(x) x$wind.gust)
o_winddir<-circular(sapply(o_list,function(x) x$orientation),zero=pi/2)
o_temp<-sapply(o_list,function(x) x$temp)
o_temp<-(o_temp-32) *5/9 #Convert to Celcius
o_time<-sapply(o_list,function(x) x$time)
o_time<-gsub("numeric[(]0[)]","",paste(lapply(o_time,function(x) rep(0,4-nchar(x))),o_time,sep=""))
o_hour<-sapply(lapply(strsplit(o_time,""),function(x) x[1:2]),function(x) paste(x,collapse=""))
o_min<-sapply(lapply(strsplit(o_time,""),function(x) x[3:4]),function(x) paste(x,collapse=""))
o_time<-paste(o_hour,o_min,"00",sep=":")
o_date<-sapply(o_list,function(x) x$date)
o_date<-gsub("Mar","03",o_date)
o_date<-gsub("Apr","04",o_date)

citation("oce")
### Gather azimuthal data ###
o_POSIXct<-paste(paste(paste("20",sapply(strsplit(o_date,"-"),function(x) x[3]),sep=""),sapply(strsplit(o_date,"-"),function(x) x[2]),sapply(strsplit(o_date,"-"),function(x) x[1]),sep="-"), o_time, "EST", sep=" ")

o_POSIXct_vec<-vector()
sunAngle_vec<-list()
for(i in 1:length(o_POSIXct)){
	o_POSIXct_vec[i]<-as.POSIXct(as.character(o_POSIXct[i]))
	sunAngle_vec[[i]]<-sunAngle(o_POSIXct_vec[i],longitude=lon,latitude=lat)
}

sun_azimuth<-sapply(sunAngle_vec,function(x) x$azimuth)
sun_altitude<-sapply(sunAngle_vec,function(x) x$altitude)

sun_azimuth<-abs(conversion.circular(circular(as.numeric(sun_azimuth),type="angles",units="degrees",template="geographics",zero=0),type="angles",units="radians"))

### Create data frame with all relevant orientation data ###
o_df<-data.frame(heading=o_mean,winddir=o_winddir,azimuth=sun_azimuth,altitude=sun_altitude)

### Correlation between mean odonate orientation and wind orientation
#rFLCorrCoeff(o_mean,o_winddir)
#rFLIndTestRand(o_mean,o_winddir,9999)
#rhoFLCIBoot(o_mean,o_winddir,95,9999)
head_winddir<-cor.circular(o_df[,1],o_df[,2],test=T)#Presented in paper
head_winddir

head_azimuth<-cor.circular(o_df[,1],o_df[,3],test=T)#Presented in paper
head_azimuth

### Visualize correlations ###
pdf(width=6.5,height=((6.5/3)*2),file="~/Desktop/Coursework/FloridaCourse/Figures/Fig3_OrientationVsWinddirVsAzimuth_v2.pdf")

#quartz(width=6.5,height=((6.5/3)*2))
par(mar=c(1.65,0.5,1.75,0.5))
par(mfrow=c(2,3))
plot.circular(o_mean,zero=pi/2,axes=F,pch=NA,tck=F)
par(xpd=NA)
axis.circular(at=circular(seq(pi/2,9*pi/4,pi/4)), labels = c('0','-45','-90','-135', '180','135','90','45'),zero=pi, rotation='counter', cex=0.75,tcl.text=0.5,tcl=0.05)

o_dangles<-atan2(sin(as.numeric(o_mean)-as.numeric(o_winddir)), cos(as.numeric(o_mean)-as.numeric(o_winddir)))
par(xpd=NA)

points(conversion.circular(o_dangles,units="radians"),zero=pi/2,stack=T,sep=0.025,bins=100,pch=21,bg="black",cex=0.75)

corner.label("A",font=2,figcorner=T)
par(xpd=NA)
text(x=0,y=1.28,label=bquote(italic(rho)~" = "~.(round(head_winddir$cor,2))~"; P = "~.(formatC(head_winddir$p.value,digits=3))),font=3,cex=1)
text(x=0,y=-1.2,label=bquote(italic(theta[wind])-italic(bar(theta)[damselfly])),font=3,cex=1)

par(mar=c(2.75,2.75,0.5,0.5))

## cos of heading and winddir
plot(cos(as.numeric(o_df[,2])),cos(as.numeric(o_df[,1])),axes=F,xlab="",ylab="")
box()
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("cos(",italic(theta[wind]),")")),side=1,line=1.75,cex=0.75)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("cos(",italic(bar(theta)[damselfly]),")")),side=2,line=1.25,cex=0.75)

corner.label("B",font=2,figcorner=T)

## sin of heading and winddir
plot(sin(as.numeric(o_df[,2])),sin(as.numeric(o_df[,1])),axes=F,xlab="",ylab="")
box()
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("sin(",italic(theta[wind]),")")),side=1,line=1.75,cex=0.75)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("sin(",italic(bar(theta)[damselfly]),")")),side=2,line=1.25,cex=0.75)

corner.label("C",font=2,figcorner=T)

### NOW FOR SUN AZIMUTH ###
par(mar=c(1.65,0.5,1.75,0.5))
plot(o_mean,zero=pi/2,axes=F,pch=NA,tck=F)
par(xpd=NA)
axis.circular(at=circular(seq(pi/2,9*pi/4,pi/4)), labels = c('0','-45','-90','-135', '180','135','90','45'),zero=pi, rotation='counter', cex=0.75,tcl.text=0.5,tcl=0.05)

o_d_sunangles<-atan2(sin(as.numeric(o_mean)-as.numeric(sun_azimuth)), cos(as.numeric(o_mean)-as.numeric(sun_azimuth)))

points(conversion.circular(o_d_sunangles,units="radians"),zero=pi/2,stack=T,sep=0.025,bins=100,pch=21,bg="black",cex=0.75)

corner.label("D",font=2,figcorner=T)
par(xpd=NA)
text(x=0,y=1.25,label=bquote(italic(rho)~" = "~.(round(head_azimuth$cor,2))~"; P = "~.(round(head_azimuth$p.value,digits=3))),font=3,cex=1)
text(x=0,y=-1.2,label=bquote(italic(theta[wind])-italic(bar(theta)[solar~azimuth])),font=3,cex=1)

par(mar=c(2.75,2.75,0.5,0.5))

## cos of heading and winddir
plot(cos(as.numeric(o_df[,3])),cos(as.numeric(o_df[,1])),axes=F,xlab="",ylab="")
box()
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("cos(",italic(theta[solar~azimuth]),")")),side=1,line=1.75,cex=0.75)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("cos(",italic(bar(theta)[damselfly]),")")),side=2,line=1.25,cex=0.75)

corner.label("E",font=2,figcorner=T)

## sin of heading and winddir
plot(sin(as.numeric(o_df[,3])),sin(as.numeric(o_df[,1])),axes=F,xlab="",ylab="")
box()
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("sin(",italic(theta[solar~azimuth]),")")),side=1,line=1.75,cex=0.75)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.7)
mtext(expression(paste("sin(",italic(bar(theta)[damselfly]),")")),side=2,line=1.25,cex=0.75)

corner.label("F",font=2,figcorner=T)

dev.off()

### ANGULAR VARIANCE AND WIND SPEED ###
### Angular variance as a function of wind speed and ambient temperature
o_var_lm<-lm(log(o_var)~log(o_wind)+o_temp)
summary(o_var_lm)
hist(residuals(o_var_lm))

### Look for correlation between difference in mean-winddir and windspeed
o_dangles<-atan2(sin(as.numeric(o_mean)-as.numeric(o_winddir)), cos(as.numeric(o_mean)-as.numeric(o_winddir)))
o_dangles_lm<-lm(log(abs(o_dangles))~log(o_wind))
summary(o_dangles_lm)

o_d_sunangles<-atan2(sin(as.numeric(o_mean)-as.numeric(sun_azimuth)), cos(as.numeric(o_mean)-as.numeric(sun_azimuth)))

o_d_sunangles_lm<-lm(abs(o_d_sunangles)~o_temp)
summary(o_d_sunangles_lm)

### Create figure of correlations ###
pdf(width=6.5,height=(6.5/4),file="~/Desktop/Coursework/FloridaCourse/Figures/Fig4_OrientationVsWindspeedVsTemp_v1.pdf")

#quartz(width=6.5,height=(6.5/4))
par(mfrow=c(1,4))
par(mar=c(2.25,2.25,0.5,0.5))

plot(log(o_var)~log(o_wind),ylim=c(range(log(o_var))[1],range(log(o_var))[2]*2),xlim=c(range(log(o_wind))[1]*1.1,range(log(o_wind))[2]),axes=F)
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.6)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext("log(angular variance)",side=2,line=1.25,cex=0.6)
box()
corner.label("A",font=2)
corner.label(bquote(italic(beta)~" = "~.(format(round(summary(o_var_lm)$coeff[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(o_var_lm)$coeff[2,4],digits=3))),x=1,y=1,cex=0.65)

plot(log(abs(o_dangles))~log(o_wind),ylim=c(range(log(abs(o_dangles)))[1],range(log(o_var))[2]*2.3),xlim=c(range(log(o_wind))[1]*1.1,range(log(o_wind))[2]),axes=F)
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext("log(wind speed) (m/s)",side=1,line=1.25,cex=0.55)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext(expression("log(|"~italic(theta[wind])-italic(bar(theta)[damselfly])~"|)"),side=2,line=1.25,cex=0.55)
box()
corner.label("B",font=2)
corner.label(bquote(italic(beta)~" = "~.(format(round(summary(o_dangles_lm)$coeff[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(o_dangles_lm)$coeff[2,4],digits=3))),x=1,y=1,cex=0.65)

plot(log(o_var)~o_temp,ylim=c(range(log(o_var))[1],range(log(o_var))[2]*1.2),xlim=c(range(o_temp)[1]*0.97,range(o_temp)[2]),axes=F)
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext(bquote("ambient temperature ("*degree*C*")"),side=1,line=1.4,cex=0.55)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext("log(angular variance)",side=2,line=1.25,cex=0.55)
box()
corner.label("C",font=2)
corner.label(bquote(italic(beta)~" = "~.(format(round(summary(o_var_lm)$coeff[3,1],2),nsmall=2))~"; P = "~.(formatC(summary(o_var_lm)$coeff[3,4],digits=3))),x=1,y=1,cex=0.65)

plot(abs(o_d_sunangles)~o_temp,ylim=c(range(abs(o_d_sunangles))[1],range(o_d_sunangles)[2]*1.2),xlim=c(range(o_temp)[1]*0.97,range(o_temp)[2]),axes=F)
axis(1,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext(bquote("ambient temperature ("*degree*C*")"),side=1,line=1.4,cex=0.55)
axis(2,mgp=c(0.1,0.5,0),cex.axis=0.55)
mtext(expression("|"~theta[solar~azimuth]-theta[damselfly]~"|"),side=2,line=1.25,cex=0.55)
box()
corner.label("D",font=2)
corner.label(bquote(italic(beta)~" = "~.(format(round(summary(o_d_sunangles_lm)$coeff[2,1],2),nsmall=2))~"; P = "~.(formatC(summary(o_d_sunangles_lm)$coeff[2,4],digits=3))),x=1,y=1,cex=0.65)

dev.off()

### Random stuff for paper ###
length(o_list) #Number of orientation trials
sum(sapply(o_list,function(x) length(x$data))) #Total number of landings / perch orientations recorded

length(f_list) #Number of focal trials
mean(f_fly)
sd(f_fly)

mean(f_forage)
sd(f_forage)

mean(f_territorial)
sd(f_territorial)

mean(c(f_windspeed,o_windspeed))
sd(c(f_windspeed,o_windspeed))

mean((c(f_temp,o_temp) - 32) * 5/9)
sd((c(f_temp,o_temp) - 32) * 5/9)

packageVersion("circular")
citation("circular")