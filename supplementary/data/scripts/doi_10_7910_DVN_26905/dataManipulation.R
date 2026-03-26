#clean up
rm(list=ls())

#set working directory, set to YOUR directory
#setwd('/Volumes/MONOGAN/polluterPlacement/data/')
#setwd('/Users/jamie/Desktop/replication/')

#load libraries
library(spatstat)
library(maptools)
library(rgdal)
library(maps)
library(foreign)
library(mgcv)
library(rgeos)
library(spdep)
library(ggmap)
library(car)
library(CircStats)

#Need to locally install CircSpatial first time through. Be sure to install 'fields', 'CircStats', 'geoR', 'RandomFields', and 'spam' as well.
#install.packages("/Volumes/MONOGAN/CircSpatialV3.tar.gz",repos=NULL, type="source")
library(CircSpatial)
#revision to debug code from CircSpatial:
source("altKrigCRF.R")
checkCRSArgs <- function(uprojargs) {.Call("checkCRSArgs", uprojargs, PACKAGE="rgdal")}

#options
options(scipen=8)

#Necessary function: define the haversine formula (Banerjee, Carlin & Gelfand 2004, 17-19)
haversine<-function(lon1,lon2,lat1,lat2){
	R<-6371
	x1<-(lon1*pi)/180
	x2<-(lon2*pi)/180
	y1<-(lat1*pi)/180
	y2<-(lat2*pi)/180
	dist<-R*acos((sin(y1)*sin(y2))+(cos(y1)*cos(y2)*cos(x1-x2)))
	return(dist)
	}
	
############STEP 1: MERGE POLLUTER DATA WITH POLLUTANT INFORMATION AND CONTROL GROUPS###################
#load TRI emission data
tri.0<-read.dta('tri_emissions2010.dta') #N=16374

#load major air polluter data
data.0<-read.csv('Air_Majors.csv') #N=16490

#merge air polluters with TRI information
data.0$AFSalt<-as.character(data.0$AFS1)
data.1<-merge(x=tri.0,y=data.0,by.x="afsid",by.y="AFSalt",all.x=FALSE,all.y=TRUE) #N=16490
data.1$airtotal[is.na(data.1$airtotal)]<-0

#subset major air data
data.2<-subset(data.1,subset=LOC_STATE!="GU"&LOC_STATE!="PR"&LOC_STATE!="VI"&LOC_STATE!="DC"&LOC_STATE!="AK"&LOC_STATE!="HI",select=c(FAC_NAME,LATITUDE,LONGITUDE,LOC_STATE,REG_ID,airtotal,afsid)) #Lower 48 only. N=15942. Without the TRI merge, N=16217. 
data.2$air<-1
data.2$tsdf<-0
data.2$lqg<-0
dimnames(data.2)[[2]]<-c('fac_name','latitude','longitude','loc_state','reg_id','airtotal','AFSalt','air','tsdf','lqg')#AFSalt is a late add. Not essential, though.

#load and subset waste treatment site control data
control.0<-read.dta('tsdfs.dta')
control.0$airtotal<-0
control.0$AFSalt<-NA 
control.0$air<-0
control.1<-subset(control.0,subset=loc_state!="GU"&loc_state!="PR"&loc_state!="VI"&loc_state!="DC"&loc_state!="AK"&loc_state!="HI",select=c(fac_name,latitude,longitude,loc_state,reg_id,airtotal,AFSalt, air,tsdf,lqg)) #Lower 48 only.

#append data sets together
full.0<-rbind(control.1,data.2)

############STEP 2: DATA CLEANING###################
#eliminate miscodes for latitude and longitude not being in the same state
#code for finding out-of-bounds observations
#print(attr(OH.ppp,"rejects")$x,digits=22)
#print(attr(OH.ppp,"rejects")$y,digits=22)
full.0$reject<-1
full.0$reject[full.0$longitude==-83.89930725097656250000 & full.0$latitude==36.58665084838867187500 & full.0$loc_state=="KY"]<-0
full.0$reject[full.0$longitude==-87.46333312988281250000  & full.0$latitude==36.61305618286132812500 & full.0$loc_state=="KY"]<-0
full.0$reject[full.0$longitude==-87.46333300000000576802 & full.0$latitude==36.61305600000000026739 & full.0$loc_state=="KY"]<-0
full.0$reject[full.0$longitude==-83.89930999999999983174 & full.0$latitude==36.58664999999999878355 & full.0$loc_state=="KY"]<-0
full.0$reject[full.0$longitude==-76.4148406982421875 & full.0$latitude==39.721248626708984375 & full.0$loc_state=="MD"]<-0
full.0$reject[full.0$longitude==-94.60826873779296875 & full.0$latitude==38.97463226318359375 & full.0$loc_state=="MO"]<-0
full.0$reject[full.0$longitude==-84.81978607177734375 & full.0$latitude==39.265655517578125 & full.0$loc_state=="OH"]<-0
full.0$reject[full.0$longitude==-105.0549926757812500000 & full.0$latitude==39.57403945922851562500 & full.0$loc_state=="SD"]<-0
full.0$reject[full.0$longitude==-105.0549950000000052341  & full.0$latitude==39.57404100000000113369 & full.0$loc_state=="SD"]<-0
full.0$reject[full.0$longitude==-80.0550384521484375000 & full.0$latitude==40.34703063964843750000 & full.0$loc_state=="WV"]<-0
full.0$reject[full.0$longitude==-80.0550400000000053069  & full.0$latitude==40.34702999999999661895 & full.0$loc_state=="WV"]<-0
full.0$reject[full.0$longitude==-97.33065032958984375000 & full.0$latitude==32.75219345092773437500 & full.0$loc_state=="WY"]<-0
full.0$reject[full.0$longitude==-105.13520050048828125000  & full.0$latitude==39.72220611572265625000 & full.0$loc_state=="WY"]<-0
full.0$reject[full.0$longitude==-105.13520099999999501961 & full.0$latitude==39.72220599999999990359 & full.0$loc_state=="WY"]<-0
full.0$reject[full.0$longitude==-97.33065200000000061209 & full.0$latitude==32.75219299999999833517 & full.0$loc_state=="WY"]<-0
full.1<-subset(full.0, reject==1, select=-reject)

#Cut controls observations that are also major air polluters
full.1<-full.1[order(full.1$loc_state,full.1$reg_id,-full.1$air),]
full.1$duplicate<-0
full.length<-dim(full.1)[1]
for (k in 2:full.length){
	if (full.1$reg_id[k]==full.1$reg_id[k-1]){full.1$duplicate[k]<-1}
}
full<-subset(full.1,duplicate==0) #N=36972

#sort the data
full<-full[order(full$loc_state,-full$longitude,full$air,full$latitude),]

############STEP 3: KRIGING WIND DIRECTION###################
#load revised wind data
wind.0<-read.table('windAngle.txt',header=TRUE,sep="\t")
wind.hold<-wind.0
#View(wind)

#Code radians so that "East" is 0. Essential for the sine and cosine functions.
wind.0$degreesA<- 450-wind.0$degrees
wind.0$oldRadians<-wind.0$radians
wind.0$radians<-pi*wind.0$degreesA/180

#plot the raw data
#pdf('wind3096.pdf')
us.states<-map("state",fill=FALSE, plot=TRUE, resolution=0)
#points(y=wind$latitude, x=wind$longitude,col='blue',pch=20)
arrow.end.lat<-wind.0$latitude+sin(wind.0$radians+pi)
arrow.end.long<-wind.0$longitude+cos(wind.0$radians+pi)
arrows(x0=wind.0$longitude, x1=arrow.end.long, y0=wind.0$latitude, y1=arrow.end.lat,length=.025,col='blue')
#dev.off()

#Create eastings and northings
coordinates(wind.0)<- ~longitude+latitude
proj4string(wind.0)<-CRS("+proj=longlat +datum=NAD83")
wind<-spTransform(wind.0,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#simplify to vectors
x1<-coordinates(wind)[,1]
y1<-coordinates(wind)[,2]
model.direction1<-wind$radians+pi

## Fit An Appropriate Model
FitHoriz1 <- lm(cos(model.direction1) ~ (x1 + y1 + I(x1*y1) + I(x1^2) + I(y1^2))) #2nd order trend
FitVert1 <- lm(sin(model.direction1)  ~ (x1 + y1 + I(x1*y1) + I(x1^2) + I(y1^2))) #2nd order trend
fitted.direction1 <- atan2(FitVert1$fitted.values, FitHoriz1$fitted.values)

## Plot Fitted Model
us.states<-map("state",fill=FALSE, plot=TRUE, resolution=0)
plot(x1, y1, type="n", asp=1, xlab="", ylab="",axes=F)
arrow.plot(x1,y1,u=cos(wind$radians+pi),v=sin(wind$radians+pi),col='blue',  arrow.ex=0.05, xpd=TRUE, true.angle=TRUE, length=.05)
arrow.plot(x1, y1, u=cos(fitted.direction1), v=sin(fitted.direction1),  arrow.ex=0.05, xpd=TRUE, true.angle=TRUE, length=.05)

##plot the raw and smoothed data
#pdf('wind3096fitted.pdf')
us.states<-map("state",fill=FALSE, plot=TRUE, resolution=0)
arrow.end.lat<-wind.hold$latitude+sin(fitted.direction1)
arrow.end.long<-wind.hold$longitude+cos(fitted.direction1)
arrows(x0=wind.hold$longitude, x1=arrow.end.long, y0=wind.hold$latitude, y1=arrow.end.lat,length=.025,col='blue')
#dev.off()

## Compute Residuals
resids1 <- CircResidual(X=x1, Y=y1, Raw=model.direction1,Trend=fitted.direction1, Plot=F)

## Cosineogram 
cosineogram.out<-CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=FALSE)
abline(h=.24,col=2); abline(v=1000000,col=2)

## Fit of exponential with range=1000000 and sill=.24
#pdf('cosinePlots.pdf')
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=TRUE, nugget=0.01,Range=1000000,  sill=0.24, x.legend=.5, y.legend=0.8)
#dev.off()

############STEP 4: INTERPOLATING WIND DIRECTION FOR POLLUTERS###################
air.0<-full

#Create eastings and northings
coordinates(air.0)<- ~longitude+latitude
proj4string(air.0)<-CRS("+proj=longlat +datum=NAD83")
air<-spTransform(air.0,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#simplify to vectors
x2<-coordinates(air)[,1]
y2<-coordinates(air)[,2]

## Krig to residuals using cosine model. Using previous range and sill estimate.
krig2 <- altKrigCRF(krig.x=x2, krig.y=y2, resid.x=resids1$x, resid.y=resids1$y, resid.direction=resids1$direction, Model = "exponential", Nugget=0.01, Range=1000000, sill=0.24, Smooth=FALSE, Plot=FALSE)

## Interpolate Fitted Model
range(x1); range(x2)
range(y1); range(y2)
horiz2<-predict.lm(FitHoriz1,newdata=data.frame(x1=x2,y1=y2))
vert2<-predict.lm(FitVert1,newdata=data.frame(x1=x2,y1=y2))
fitted2<-atan2(vert2,horiz2) #forecasted values

## Plot Estimate Of Direction And Overplot Sample.
points<-length(x2)
map.set<-sample(1:points,1000)
estimate2=fitted2 + krig2$direction
#pdf('arrowPlot.pdf')
plot(x2, y2, type="n", xlab="", ylab="", asp=1)
arrow.plot(x2[map.set], y2[map.set], u=cos(estimate2[map.set]), v= sin(estimate2[map.set]),arrow.ex=0.05, xpd=FALSE, true.angle=TRUE, length=.05, col="tan")
arrow.plot(x1, y1, u=cos(model.direction1), v=sin(model.direction1), arrow.ex=0.05, xpd=FALSE, true.angle=TRUE, length=.05, col=1)
#dev.off()

#write out data
eastings<-x2
northings<-y2
longitude<-full$longitude
latitude<-full$latitude
angle.full<-estimate2
angle.mod<-fitted2
angle.error<-krig2$direction
air.direction<-cbind(air@data,longitude,latitude,eastings,northings,angle.full,angle.mod,angle.error)

write.csv(air.direction, "majorAirDirection.csv",row.names=FALSE)

############STEP 5.A: DIRECTION TO LEEWARD BORDER###################
#relabel
#full<-read.csv('majorAirDirection.csv')
full<-air.direction

#load map polygon data
map.states<-readShapePoly(fn="tl_2011_us_state.shp")

#set up input and output objects
states<-c('AL','AR','AZ','CA','CO','CT','DE','FL','GA','IA','ID','IL','IN','KS','KY','LA','MA','MD','ME','MI','MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM','NV','NY','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VA','VT','WA','WI','WV','WY')
numbers<-c(41,53,6,5,1,15,11,44,46,49,14,16,29,24,25,52,51,47,45,28,8,54,9,36,22,40,3,43,56,33,7,13,21,30,27,34,32,35,48,42,2,50,20,12,37,38,55,4)
factories<-rep(NA,48)
in.polygon<-rep(NA,48)
no.polygon<-rep(NA,48)
no.segments<-rep(NA,48)
deviation.long<-rep(NA,48)
deviation.lat<-rep(NA,48)
distances<-c(0)

#loop to create ppp objects and create distance measures
for(i in 1:48){
	assign(states[i],SpatialPolygons(Srl=list(map.states@polygons[[numbers[i]]])))
	data.for.state<-subset(full,loc_state==states[i])
	factories[i]<-dim(data.for.state)[1]
	
	#necessary information for window creation
	no.polygon[i]<-length(get(states[i])@polygons[[1]]@Polygons)
	full.coords<-list()
	loc.min.x<-rep(NA,no.polygon[i]); loc.max.x<-rep(NA,no.polygon[i]); loc.min.y<-rep(NA,no.polygon[i]); loc.max.y<-rep(NA,no.polygon[i])
		
		#create window features	
		if (no.polygon[i]==1) {
		state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[1]]@coords[-1,])
		state.bound<-owin(xrange=c(min(state.trim[,1]),max(state.trim[,1])),yrange=c(min(state.trim[,2]),max(state.trim[,2])),poly=list(x=rev(state.trim[,1]),y=rev(state.trim[,2])))
		} else {
			for(j in 1:no.polygon[i]){
				state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[j]]@coords[-1,])
				loc.min.x[j]<-min(state.trim[,1]); loc.max.x[j]<-max(state.trim[,1]); loc.min.y[j]<-min(state.trim[,2]); loc.max.y[j]<-max(state.trim[,2])
				full.coords[[j]]<-list(x=rev(state.trim[,1]),y=rev(state.trim[,2]))
				}
		min.x<-min(loc.min.x); max.x<-max(loc.max.x); min.y<-min(loc.min.y); max.y<-max(loc.max.y)
		state.bound<-owin(xrange=c(min.x,max.x),yrange=c(min.y,max.y),poly=full.coords)
				}

	#combine window feature into ppp object
	state.ppp<-ppp(x=data.for.state$longitude, y=data.for.state$latitude, window=state.bound, marks=as.factor(data.for.state$air))
	assign(paste(states[i],'ppp',sep='.'), state.ppp)
	in.polygon[i]<-state.ppp$n

	#generate lines for distance measure, create psp object
	reach.x<-state.ppp$x+10000*cos(data.for.state$angle.full)
	reach.y<-state.ppp$y+10000*sin(data.for.state$angle.full)
	initial<-psp(x0=state.ppp$x, y0=state.ppp$y, x1=reach.x, y1=reach.y, window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
	assign(paste(states[i],'psp',sep='.'), initial[,state.bound])
	no.segments[i]<-dim(get(paste(states[i],'psp',sep='.'))$ends)[1]
	angles<-angles.psp(get(paste(states[i],'psp',sep='.')))
	
	#drop duplicate lines from the PSP for the same point
	#count how many times a segment overlaps with the state border
	keep<-rep(1,no.segments[i])
	splice.count<-rep(NA,length(reach.x))
	for(j in 1:length(reach.x)){
		temp.psp<-psp(x0=state.ppp$x[j], y0=state.ppp$y[j], x1=reach.x[j], y1=reach.y[j], window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
		splice.count[j]<-nsegments(temp.psp[,state.bound])
	}
	mult.segs<-which(splice.count>1)
	overshoot<-c(0)
	add.overshoot<-	splice.count[which(splice.count>1)]-1
	if(max(splice.count)>1 & length(mult.segs)==1){ 
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	if(max(splice.count)>1 & length(mult.segs)>1){ 
		for(k in 2:length(mult.segs)){overshoot[k]<-overshoot[k-1]+add.overshoot[k-1]}
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	
	#compute the haversine distance
	segs<-get(paste(states[i],'psp',sep='.'))$ends[keep==1,]
	distances<-append(distances,haversine(segs$x0,segs$x1,segs$y0,segs$y1))

	#data checking
	no.segments[i]<-dim(segs)[1]
	deviation.long[i]<-sum(data.for.state$longitude-segs$x0)
	deviation.lat[i]<-sum(data.for.state$latitude-segs$y0)
}

###checking  that sample sizes align and that segments properly correspond to points###
#table(factories[]-in.polygon[])
#table(no.segments[]-in.polygon[])
#table(deviation.long); table(deviation.lat)
#length(distances)
#dim(full)

#add downwind distances to main data
full$down<-distances[-1]

#standardized downwind
full$sDown<-full$down/ave(full$down,full$loc_state,FUN=max)

############STEP 5.B: DIRECTION TO WINDWARD BORDER##################
#RESET input and output objects
#'states' and 'numbers' defined from 5.A
factories<-rep(NA,48)
in.polygon<-rep(NA,48)
no.polygon<-rep(NA,48)
no.segments<-rep(NA,48)
deviation.long<-rep(NA,48)
deviation.lat<-rep(NA,48)
distances<-c(0)

#loop to create ppp objects and create distance measures
for(i in 1:48){
	assign(states[i],SpatialPolygons(Srl=list(map.states@polygons[[numbers[i]]])))
	data.for.state<-subset(full,loc_state==states[i])
	factories[i]<-dim(data.for.state)[1]
	
	#necessary information for window creation
	no.polygon[i]<-length(get(states[i])@polygons[[1]]@Polygons)
	full.coords<-list()
	loc.min.x<-rep(NA,no.polygon[i]); loc.max.x<-rep(NA,no.polygon[i]); loc.min.y<-rep(NA,no.polygon[i]); loc.max.y<-rep(NA,no.polygon[i])
		
		#create window features	
		if (no.polygon[i]==1) {
		state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[1]]@coords[-1,])
		state.bound<-owin(xrange=c(min(state.trim[,1]),max(state.trim[,1])),yrange=c(min(state.trim[,2]),max(state.trim[,2])),poly=list(x=rev(state.trim[,1]),y=rev(state.trim[,2])))
		} else {
			for(j in 1:no.polygon[i]){
				state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[j]]@coords[-1,])
				loc.min.x[j]<-min(state.trim[,1]); loc.max.x[j]<-max(state.trim[,1]); loc.min.y[j]<-min(state.trim[,2]); loc.max.y[j]<-max(state.trim[,2])
				full.coords[[j]]<-list(x=rev(state.trim[,1]),y=rev(state.trim[,2]))
				}
		min.x<-min(loc.min.x); max.x<-max(loc.max.x); min.y<-min(loc.min.y); max.y<-max(loc.max.y)
		state.bound<-owin(xrange=c(min.x,max.x),yrange=c(min.y,max.y),poly=full.coords)
				}

	#combine window feature into ppp object
	state.ppp<-ppp(x=data.for.state$longitude, y=data.for.state$latitude, window=state.bound, marks=as.factor(data.for.state$air))
	assign(paste(states[i],'ppp',sep='.'), state.ppp)
	in.polygon[i]<-state.ppp$n

	#generate lines for distance measure, create psp object
	reach.x<-state.ppp$x+10000*cos(data.for.state$angle.full+pi)
	reach.y<-state.ppp$y+10000*sin(data.for.state$angle.full+pi)
	initial<-psp(x0=state.ppp$x, y0=state.ppp$y, x1=reach.x, y1=reach.y, window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
	assign(paste(states[i],'psp',sep='.'), initial[,state.bound])
	no.segments[i]<-dim(get(paste(states[i],'psp',sep='.'))$ends)[1]
	angles<-angles.psp(get(paste(states[i],'psp',sep='.')))
	
	#drop duplicate lines from the PSP for the same point
	#count how many times a segment overlaps with the state border
	keep<-rep(1,no.segments[i])
	splice.count<-rep(NA,length(reach.x))
	for(j in 1:length(reach.x)){
		temp.psp<-psp(x0=state.ppp$x[j], y0=state.ppp$y[j], x1=reach.x[j], y1=reach.y[j], window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
		splice.count[j]<-nsegments(temp.psp[,state.bound])
	}
	mult.segs<-which(splice.count>1)
	overshoot<-c(0)
	add.overshoot<-	splice.count[which(splice.count>1)]-1
	if(max(splice.count)>1 & length(mult.segs)==1){ 
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	if(max(splice.count)>1 & length(mult.segs)>1){ 
		for(k in 2:length(mult.segs)){overshoot[k]<-overshoot[k-1]+add.overshoot[k-1]}
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	
	#compute the haversine distance
	segs<-get(paste(states[i],'psp',sep='.'))$ends[keep==1,]
	distances<-append(distances,haversine(segs$x0,segs$x1,segs$y0,segs$y1))

	#data checking
	no.segments[i]<-dim(segs)[1]
	deviation.long[i]<-sum(data.for.state$longitude-segs$x0)
	deviation.lat[i]<-sum(data.for.state$latitude-segs$y0)
}

###checking  that sample sizes align and that segments properly correspond to points###
#table(factories[]-in.polygon[])
#table(no.segments[]-in.polygon[])
#table(deviation.long); table(deviation.lat)
#length(distances)
#dim(full)

#add upwind distances to main data
full$up<-distances[-1]

#standardized upwind
full$sUp<-full$up/ave(full$up,full$loc_state,FUN=max)

############STEP 5.C: DIRECTION TO EASTERN BORDER###################
#RESET input and output objects
#'states' and 'numbers' defined from 5.A
factories<-rep(NA,48)
in.polygon<-rep(NA,48)
no.polygon<-rep(NA,48)
no.segments<-rep(NA,48)
deviation.long<-rep(NA,48)
deviation.lat<-rep(NA,48)
distances<-c(0)

#loop to create ppp objects and create distance measures
for(i in 1:48){
	assign(states[i],SpatialPolygons(Srl=list(map.states@polygons[[numbers[i]]])))
	data.for.state<-subset(full,loc_state==states[i])
	factories[i]<-dim(data.for.state)[1]
	
	#necessary information for window creation
	no.polygon[i]<-length(get(states[i])@polygons[[1]]@Polygons)
	full.coords<-list()
	loc.min.x<-rep(NA,no.polygon[i]); loc.max.x<-rep(NA,no.polygon[i]); loc.min.y<-rep(NA,no.polygon[i]); loc.max.y<-rep(NA,no.polygon[i])
		
		#create window features	
		if (no.polygon[i]==1) {
		state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[1]]@coords[-1,])
		state.bound<-owin(xrange=c(min(state.trim[,1]),max(state.trim[,1])),yrange=c(min(state.trim[,2]),max(state.trim[,2])),poly=list(x=rev(state.trim[,1]),y=rev(state.trim[,2])))
		} else {
			for(j in 1:no.polygon[i]){
				state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[j]]@coords[-1,])
				loc.min.x[j]<-min(state.trim[,1]); loc.max.x[j]<-max(state.trim[,1]); loc.min.y[j]<-min(state.trim[,2]); loc.max.y[j]<-max(state.trim[,2])
				full.coords[[j]]<-list(x=rev(state.trim[,1]),y=rev(state.trim[,2]))
				}
		min.x<-min(loc.min.x); max.x<-max(loc.max.x); min.y<-min(loc.min.y); max.y<-max(loc.max.y)
		state.bound<-owin(xrange=c(min.x,max.x),yrange=c(min.y,max.y),poly=full.coords)
				}

	#combine window feature into ppp object
	state.ppp<-ppp(x=data.for.state$longitude, y=data.for.state$latitude, window=state.bound, marks=as.factor(data.for.state$air))
	assign(paste(states[i],'ppp',sep='.'), state.ppp)
	in.polygon[i]<-state.ppp$n

	#generate lines for distance measure, create psp object
	reach.y<-state.ppp$y
	reach.x<-rep(state.ppp$window$xrange[2],length(reach.y))
	initial<-psp(x0=state.ppp$x, y0=state.ppp$y, x1=reach.x, y1=reach.y, window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
	assign(paste(states[i],'psp',sep='.'), initial[,state.bound])
	no.segments[i]<-dim(get(paste(states[i],'psp',sep='.'))$ends)[1]
	angles<-angles.psp(get(paste(states[i],'psp',sep='.')))
	
	#drop duplicate lines from the PSP for the same point
	#count how many times a segment overlaps with the state border
	keep<-rep(1,no.segments[i])
	splice.count<-rep(NA,length(reach.x))
	for(j in 1:length(reach.x)){
		temp.psp<-psp(x0=state.ppp$x[j], y0=state.ppp$y[j], x1=reach.x[j], y1=reach.y[j], window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
		splice.count[j]<-nsegments(temp.psp[,state.bound])
	}
	mult.segs<-which(splice.count>1)
	overshoot<-c(0)
	add.overshoot<-	splice.count[which(splice.count>1)]-1
	if(max(splice.count)>1 & length(mult.segs)==1){ 
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	if(max(splice.count)>1 & length(mult.segs)>1){ 
		for(k in 2:length(mult.segs)){overshoot[k]<-overshoot[k-1]+add.overshoot[k-1]}
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	
	#compute the haversine distance
	segs<-get(paste(states[i],'psp',sep='.'))$ends[keep==1,]
	distances<-append(distances,haversine(segs$x0,segs$x1,segs$y0,segs$y1))

	#data checking
	no.segments[i]<-dim(segs)[1]
	deviation.long[i]<-sum(data.for.state$longitude-segs$x0)
	deviation.lat[i]<-sum(data.for.state$latitude-segs$y0)
}

###checking  that sample sizes align and that segments properly correspond to points###
#table(factories[]-in.polygon[])
#table(no.segments[]-in.polygon[])
#table(deviation.long); table(deviation.lat)
#length(distances)
#dim(full)

#add eastbound distances to main data
full$east<-distances[-1]

#standardized eastbound
full$sEast<-full$east/ave(full$east,full$loc_state,FUN=max)


############STEP 5.D: DIRECTION TO WESTERN BORDER###################
#RESET input and output objects
#'states' and 'numbers' defined from 5.A
factories<-rep(NA,48)
in.polygon<-rep(NA,48)
no.polygon<-rep(NA,48)
no.segments<-rep(NA,48)
deviation.long<-rep(NA,48)
deviation.lat<-rep(NA,48)
distances<-c(0)

#loop to create ppp objects and create distance measures
for(i in 1:48){
	assign(states[i],SpatialPolygons(Srl=list(map.states@polygons[[numbers[i]]])))
	data.for.state<-subset(full,loc_state==states[i])
	factories[i]<-dim(data.for.state)[1]
	
	#necessary information for window creation
	no.polygon[i]<-length(get(states[i])@polygons[[1]]@Polygons)
	full.coords<-list()
	loc.min.x<-rep(NA,no.polygon[i]); loc.max.x<-rep(NA,no.polygon[i]); loc.min.y<-rep(NA,no.polygon[i]); loc.max.y<-rep(NA,no.polygon[i])
		
		#create window features	
		if (no.polygon[i]==1) {
		state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[1]]@coords[-1,])
		state.bound<-owin(xrange=c(min(state.trim[,1]),max(state.trim[,1])),yrange=c(min(state.trim[,2]),max(state.trim[,2])),poly=list(x=rev(state.trim[,1]),y=rev(state.trim[,2])))
		} else {
			for(j in 1:no.polygon[i]){
				state.trim<-as.matrix(get(states[i])@polygons[[1]]@Polygons[[j]]@coords[-1,])
				loc.min.x[j]<-min(state.trim[,1]); loc.max.x[j]<-max(state.trim[,1]); loc.min.y[j]<-min(state.trim[,2]); loc.max.y[j]<-max(state.trim[,2])
				full.coords[[j]]<-list(x=rev(state.trim[,1]),y=rev(state.trim[,2]))
				}
		min.x<-min(loc.min.x); max.x<-max(loc.max.x); min.y<-min(loc.min.y); max.y<-max(loc.max.y)
		state.bound<-owin(xrange=c(min.x,max.x),yrange=c(min.y,max.y),poly=full.coords)
				}

	#combine window feature into ppp object
	state.ppp<-ppp(x=data.for.state$longitude, y=data.for.state$latitude, window=state.bound, marks=as.factor(data.for.state$air))
	assign(paste(states[i],'ppp',sep='.'), state.ppp)
	in.polygon[i]<-state.ppp$n

	#generate lines for distance measure, create psp object
	reach.y<-state.ppp$y
	reach.x<-rep(state.ppp$window$xrange[1],length(reach.y))
	initial<-psp(x0=state.ppp$x, y0=state.ppp$y, x1=reach.x, y1=reach.y, window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
	assign(paste(states[i],'psp',sep='.'), initial[,state.bound])
	no.segments[i]<-dim(get(paste(states[i],'psp',sep='.'))$ends)[1]
	angles<-angles.psp(get(paste(states[i],'psp',sep='.')))
	
	#drop duplicate lines from the PSP for the same point
	#count how many times a segment overlaps with the state border
	keep<-rep(1,no.segments[i])
	splice.count<-rep(NA,length(reach.x))
	for(j in 1:length(reach.x)){
		temp.psp<-psp(x0=state.ppp$x[j], y0=state.ppp$y[j], x1=reach.x[j], y1=reach.y[j], window=owin(xrange=state.ppp$window$xrange, yrange=state.ppp$window$yrange),check=FALSE)
		splice.count[j]<-nsegments(temp.psp[,state.bound])
	}
	mult.segs<-which(splice.count>1)
	overshoot<-c(0)
	add.overshoot<-	splice.count[which(splice.count>1)]-1
	if(max(splice.count)>1 & length(mult.segs)==1){ 
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	if(max(splice.count)>1 & length(mult.segs)>1){ 
		for(k in 2:length(mult.segs)){overshoot[k]<-overshoot[k-1]+add.overshoot[k-1]}
		starts<-mult.segs+overshoot+1
		stops<-starts+add.overshoot-1
			for(k in 1:length(starts)){keep[(starts[k]):(stops[k])]<-0}
	}
	
	#compute the haversine distance
	segs<-get(paste(states[i],'psp',sep='.'))$ends[keep==1,]
	distances<-append(distances,haversine(segs$x0,segs$x1,segs$y0,segs$y1))

	#data checking
	no.segments[i]<-dim(segs)[1]
	deviation.long[i]<-sum(data.for.state$longitude-segs$x0)
	deviation.lat[i]<-sum(data.for.state$latitude-segs$y0)
}

###checking  that sample sizes align and that segments properly correspond to points###
#table(factories[]-in.polygon[])
#table(no.segments[]-in.polygon[])
#table(deviation.long); table(deviation.lat)
#length(distances)
#dim(full)

#add westbound distances to main data
full$west<-distances[-1]

#standardized westbound
full$sWest<-full$west/ave(full$west,full$loc_state,FUN=max)


############STEP 6: MERGE SIC CODES IN###################
sic.merger<-read.csv('sicMerger.csv')
full.3<-merge(x=full,y=sic.merger,by.x='reg_id',by.y='REGISTRY_ID',all.x=TRUE,all.y=FALSE)
full.3$SIC_CODES<-as.character(full.3$SIC_CODES)

#extract SIC codes from text
lf3<-dim(full.3)[1]
for(i in 1:lf3){
	full.3$sic1[i]<-as.numeric(strsplit(full.3$SIC_CODES[i],split=',')[[1]][1])
}
full.alt<-full.3[is.na(full.3$sic1) & full.3$air==1,]

#create power plant dummy
full.3$power<-as.numeric(full.3$sic1>=4900 & full.3$sic1<5000)
full.3$power[is.na(full.3$power)]<-0
full.3$power[full.3$reg_id %in% c(110015980722, 110017209938, 110023013718, 110024286075, 110037400522, 110037740380, 110038046851, 110038159701, 110038324382, 110038440176, 110038456088, 110038950310, 110039622405, 110039687443, 110040110536, 110040366065, 110040484641, 110040486033, 110040486364, 110040487960, 110040488004, 110040488282, 110040488683, 110040489209, 110040489502, 110040489717, 110040489771, 110040489780, 110040489860, 110040489888, 110040491955, 110040492972, 110040493196, 110040493551, 110040493560, 110040493597, 110040495407, 110040496148, 110040496380, 110040497030, 110040497147, 110040504200, 110040506529, 110040507421, 110040507458, 110040509296, 110040512059, 110040512638, 110040513110, 110040513165, 110040567017, 110040573466, 110040754350, 110040755910, 110040826176, 110040833426, 110040834229, 110041354441, 110041604350, 110041711877, 110041889873, 110041891450, 110042148155, 110042262262, 110042262636, 110042331009, 110043217453, 110043280927, 110043287813, 110043290408, 110043298552, 110043321955, 110043432693, 110043575486, 110043577572, 110043685811, 110043703114, 110043704275, 110043974090, 110043977783, 110044301975)]<-1

############STEP 7: MERGE STATE-LEVEL COVARIATES###################
st.cov<-read.csv('statePolluterCovs.csv')
full.4<-merge(x=full.3,y=st.cov,by='loc_state',all=TRUE)

############STEP 8: MERGE ADDITIONAL SITE COVARIATES###################
site.cov<-read.dta('new_public.dta')
full.5<-merge(x=full.4,y=site.cov,by.x='AFSalt',by.y='afsid',all.x=TRUE,all.y=FALSE)
full.5$new[is.na(full.5$new)]<-0
full.5$public[is.na(full.5$public)]<-0

############STEP 9: DUMP MISSING DATA WHEN COORDINATES ARE MISSING, CONVERT STATE ABBREVIATION TO CHARACTER###################
full.5<-subset(full.5,!is.na(longitude) & !is.na(latitude))
full.5$loc_state<-as.character(full.5$loc_state)

############STEP 10: MERGE AVERAGE WIND SPEED###################
#wind.0<-read.table('windAngle.txt',header=TRUE,sep="\t")
#wind.hold<-wind.0
places<-as.character(wind.hold$location)
places[123]<-"Blue Hills Reservation (Milton), MA"
places[159]<-"Eppley Airfield (Omaha), NE"
places[185]<-"Central Park (New York City), NY"
places[187]<-"LaGuardia Airport (Queens), NY"
places[217]<-"Clinton-Sherman Airport Tower (Burns Flat), OK"
places[52]<-"Washington National Airport, DC"
places[53]<-"Washington Dulles International Airport, DC"
wind.hold$state<-substring(sapply(strsplit(places,split=","), function(x) x[2]),2)
speeds<-as.data.frame(as.matrix(by(wind.hold$speed,wind.hold$state,mean,na.rm=T)))
colnames(speeds)<-"windSpeed"
speeds$loc_state<-row.names(speeds)
full.6<-merge(x=full.5,y=speeds,by="loc_state",all.x=TRUE,all.y=FALSE)

############STEP 11: LINK LOCATION TO COUNTY POPULATION DENSITY###################
#set up nationwide county map to link sites with
us.counties<-map("county",fill=FALSE, plot=TRUE, resolution=0)
IDs.2 <- us.counties$names
us.counties.1<-map(database="county",region=IDs.2, fill=TRUE, plot=TRUE, interior=TRUE, exact=TRUE, resolution=0)
us.counties.2<-map2SpatialPolygons(us.counties.1, IDs=IDs.2, proj4string=CRS("+proj=longlat +datum=NAD83"))
#us.map.1<-spTransform(a.1,CRS("+init=esri:102003"))
#us.counties.2<-spTransform(a.2,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs"))

#lookup info for counties and states
state.id<-rep(NA,3082)
for(i in 1:3082){state.id[i]<-strsplit(names(us.counties.2),split=",")[[i]][1]}
state.id<-recode(state.id,'"alabama"="AL";"arizona"="AZ";"arkansas"="AR";"california"="CA";"colorado"="CO";"connecticut"="CT";"delaware"="DE";"district of columbia"="DC";"florida"="FL";"georgia"="GA";"idaho"="ID";"illinois"="IL";"indiana"="IN";"iowa"="IA";"kansas"="KS";"kentucky"="KY";"louisiana"="LA";"maine"="ME";"maryland"="MD";"massachusetts"="MA";"michigan"="MI";"minnesota"="MN";"mississippi"="MS";"missouri"="MO";"montana"="MT";"nebraska"="NE";"nevada"="NV";"new hampshire"="NH";"new jersey"="NJ";"new mexico"="NM";"new york"="NY";"north carolina"="NC";"north dakota"="ND";"ohio"="OH";"oklahoma"="OK";"oregon"="OR";"pennsylvania"="PA";"rhode island"="RI";"south carolina"="SC";"south dakota"="SD";"tennessee"="TN";"texas"="TX";"utah"="UT";"vermont"="VT";"virginia"="VA";"washington"="WA";"west virginia"="WV";"wisconsin"="WI";"wyoming"="WY"')
county.id.0<-county.id<-rep(NA,3082)
for(i in 1:3082){county.id[i]<-strsplit(names(us.counties.2),split=",")[[i]][2]}
county.id[county.id=="dade" & state.id=="FL"]<-"miami-dade"
county.id<-sub(":main","",county.id)
county.id<-sub(":penrose","",county.id)
county.id<-sub(":lopez island","",county.id)
county.id<-sub(":orcas island","",county.id)
county.id<-sub(":san juan island","",county.id)
county.id<-sub(":chincoteague","",county.id)
county.id<-sub(":spit","",county.id)
county.id<-sub(":knotts","",county.id)
county.id<-sub(":north","",county.id)
county.id<-sub(":south","",county.id)
county.id[county.id=="obrien" & state.id=="IA"]<-"o'brien"
county.id[county.id=="prince georges" & state.id=="MD"]<-"prince george's"
county.id[county.id=="queen annes" & state.id=="MD"]<-"queen anne's"
county.id[county.id=="st marys" & state.id=="MD"]<-"st mary's"
county.id[county.id=="washington" & state.id=="DC"]<-"district of columbia"

#site locations
positions<-as.data.frame(cbind(full.6$longitude,full.6$latitude))

#corrections to account for map imprecision on county lookup
adjust<-c(81, 380, 381, 6133, 7109, 8816, 8831, 8832, 8864,9356, 9390, 9435, 9480, 9523, 9565, 9567, 9573, 9638,9677, 9734, 9847, 9870, 9897, 10073, 10117, 10430, 10436,10721, 10954, 11517, 11583, 11629, 11710, 11979, 12058,12250, 12386, 12528, 12601, 12649, 12810, 13131, 13425,13679, 14636, 14741, 14749, 14806, 14811, 15070, 15621,15633, 15658, 15961, 15988, 16083, 16814, 17010, 17178,17189, 17514, 17790, 17795, 17905, 18063, 18085, 18086,18172, 18506, 19424, 19567, 20084, 20121, 20147, 20168,20244, 20280, 20341, 20356, 20431, 20442, 20516, 20544,20561, 20584, 20595, 20605, 20618, 20620, 20665, 20666,20672, 20674, 20701, 20717, 20723, 20780, 20813, 20835,20861, 20899, 20931, 20964, 20985, 21030, 21031, 21065,21088, 21161, 21175, 21235, 21248, 21258, 21285, 21340,21390, 21435, 21450, 21500, 21503, 21531, 21532, 21539,21613, 21794, 22055, 22580, 22984, 23998, 24564, 25130,25147, 25299, 25672, 25675, 25734, 25774, 25987, 26026,26064, 26071, 26080, 26089, 26165, 26184, 26196, 26261,26322, 26351, 26437, 26477, 26486, 26524, 26646, 26717,26721, 26751, 26962, 26985, 26988, 27018, 27061, 27064,27078, 27114, 27186, 27199, 27247, 27310, 27369, 27457,27472, 27475, 27509, 27548, 27581, 27615, 27621, 27640,27662, 27718, 27727, 27736, 27968, 28150, 28156, 28162,28167, 28171, 28354, 28428, 28433, 28434, 28885, 29159,29344, 29389, 29393, 29508, 29833, 29858, 29991, 30228,30237, 30324, 31275, 31296, 31376, 31492, 31561, 31621,31708, 31740, 31808, 31845, 31859, 31912, 31935, 32054,32409, 33095, 33705, 34510, 34537, 34649, 34675, 34714,34975, 35665, 35820, 36075, 36166, 36167, 36222, 36225,36664, 36740, 36742, 36743, 36779, 36819, 36820, 36835,36851, 285, 289, 294, 297, 300, 302, 305, 307, 313, 323, 325, 328, 490, 546, 587, 713, 1340, 1506, 1536, 1544, 1562, 1592, 1604, 1616, 1628, 1636, 1664, 1686, 1687, 1693, 1700, 1705, 1706, 1708, 1718, 1722, 1725, 1726, 1750, 1760, 1770, 1773, 1774, 1786, 1792, 1799, 1817, 1826, 1856, 1877, 1910, 1920, 1922, 1942, 1960, 1973, 1978, 1995, 2000, 2009, 2014, 2032, 2033, 2040, 2041, 2059, 2070, 2076, 2091, 2092, 2104, 2127, 2131, 2137, 2140, 2186, 2191, 2200, 2272, 2292, 2304, 2306, 2309, 2310, 2347, 2349, 2357, 2361, 2363, 2378, 2383, 2391, 2417, 2425, 2436, 2441, 2458, 2471, 2490, 2491, 2504, 2510, 2522, 2543, 2548, 2554, 2565, 2568, 2569, 2588, 2589, 2601, 2609, 2615, 2618, 2627, 2656, 2663, 2667, 2670, 2695, 2707, 2710, 2723, 2724, 2725, 2728, 2733, 2736, 2742, 2768, 2771, 2781, 2801, 2812, 2817, 2829, 2833, 2839, 2845, 2862, 2870, 2883, 2886, 2906, 2917, 2920, 2929, 2960, 2992, 3025, 3038, 3080, 3129, 3138, 3145, 3161, 3165, 3171, 3205, 3207, 3219, 3229, 3230, 3231, 3240, 3248, 3251, 3266, 3270, 3271, 3285, 3296, 3313, 3331, 3335, 3342, 3345, 3371, 3390, 3423, 3435, 3441, 3467, 3479, 3507, 3511, 3521, 3543, 3554, 3560, 3566, 3570, 3600, 3605, 3608, 3614, 3615, 3633, 3634, 3644, 3645, 3654, 3684, 3691, 3696, 3703, 3734, 3751, 3793, 3798, 3799, 3811, 3840, 3858, 3862, 3864, 3865, 3874, 3882, 3888, 3929, 3933, 3942, 3951, 3962, 3969, 3982, 4000, 4007, 4008, 4024, 4026, 4045, 4075, 4084, 4100, 4117, 4126, 4135, 4162, 4170, 4175, 4184, 4228, 4245, 4249, 4252, 4259, 4277, 4280, 4296, 4302, 4307, 4324, 4376, 4402, 4433, 4436, 4447, 4454, 4477, 4478, 4493, 4506, 4509, 4526, 4530, 4538, 4539, 4555, 4556, 4574, 4579, 4612, 4631, 4670, 4678, 4682, 4691, 4720, 4739, 4747, 4751, 4762, 4790, 4811, 4812, 4829, 4837, 4845, 4853, 4877, 4884, 4888, 4889, 4893, 4910, 4938, 4940, 4964, 4972, 4985, 4986, 5050, 5064, 5076, 5077, 5085, 5086, 5144, 5147, 5178, 5192, 5205, 5228, 5238, 5268, 5298, 5305, 5306, 5310, 5327, 5354, 5361, 5375, 5381, 5382, 5408, 5419, 5430, 5440, 5468, 5471, 5493, 5516, 5520, 5523, 5527, 5532, 5547, 5551, 5580, 5582, 5643, 5651, 5654, 5656, 5662, 5692, 5725, 5772, 5777, 5793, 5794, 5806, 5809, 5841, 5845, 5855, 5856, 5875, 5880, 5895, 5924, 5942, 5948, 5950, 5956, 5958, 5978, 6011, 6012, 6027, 6037, 6056, 6079, 6092, 6095, 6103, 6115, 6125, 6149, 6177, 6178, 6197, 6206, 6217, 6222, 6240, 6244, 6264, 6277, 6279, 6283, 6287, 6305, 6367, 6383, 6385, 6388, 6390, 6392, 6410, 6412, 6436, 6447, 6450, 6478, 6516, 6525, 6527, 6535, 6549, 6561, 6569, 6615, 6619, 6636, 6652, 6658, 6659, 6674, 6678, 6682, 6684, 6697, 6707, 6716, 6727, 7112, 7243, 7317, 7378, 7396, 7413, 7460, 7690, 7702, 7708, 7717, 7726, 7738, 7758, 7763, 7771, 7795, 7799, 7803, 7805, 7817, 7825, 7831, 7836, 7837, 7840, 7855, 7865, 7866, 7867, 7870, 7882, 7887, 7888, 7904, 7930, 7948, 7949, 7954, 7958, 7961, 7974, 8001, 8006, 8007, 8019, 8042, 8060, 8068, 8074, 8077, 8088, 8091, 8129, 8156, 8161, 8173, 8184, 8186, 8209, 8213, 8216, 8217, 8220, 8222, 8223, 8252, 8254, 8255, 8263, 8313, 8339, 8343, 8411, 8429, 8456, 8463, 8473, 8490, 8494, 8509, 9044, 9077, 10816, 13481, 13568, 13631, 13635, 13688, 13706, 13827, 13939, 13965, 14184, 14192, 14256, 14272, 14274, 14323, 14409, 14463, 14540, 14562, 14572, 14578, 14628, 14643, 14647, 14690, 14692, 14698, 14746, 14777, 14788, 14802, 14811, 14845, 14875, 14878, 14954, 14983, 14984, 15033, 15061, 15081, 15156, 15171, 15176, 15179, 15184, 15191, 15196, 15197, 15236, 15278, 15291, 15356, 15373, 15437, 15456, 15467, 15469, 15482, 15487, 15495, 15510, 15535, 15563, 15600, 15604, 15621, 15632, 15690, 15692, 15741, 15762, 15771, 15838, 15882, 15886, 15894, 15903, 15914, 15915, 15946, 15982, 16026, 16068, 16133, 16138, 16144, 16195, 16208, 16297, 16304, 16315, 16344, 16381, 16391, 16400, 16432, 16440, 16479, 16481, 16562, 16591, 16603, 16679, 16700, 16713, 16717, 16718, 16729, 16817, 16846, 17131, 17159, 18165, 18167, 18178, 18182, 18192, 18203, 18255, 18265, 18268, 18321, 18348, 18357, 18374, 18379, 18460, 18463, 18734, 18761, 18898, 18940, 19136, 19433, 19444, 19519, 19523, 19721, 19780, 19797, 20238, 20270, 20285, 20304, 20332, 20345, 20361, 20404, 20427, 20429, 20460, 20480, 20485, 20535, 20567, 20588, 20590, 20682, 20692, 20755, 20785, 20791, 20803, 20821, 20838, 20844, 20878, 20880, 20895, 20912, 20998, 21036, 21050, 21053, 21067, 21069, 21164, 21166, 21215, 21221, 21249, 21254, 21257, 21264, 21282, 21313, 21338, 21348, 21360, 21367, 21370, 21378, 21389, 21394, 21422, 21433, 21447, 21458, 21488, 21497, 21499, 21515, 21525, 21563, 21620, 21623, 21639, 21991, 22001, 22003, 22012, 22038, 22040, 22042, 22056, 22057, 22071, 22073, 22100, 22106, 22110, 22113, 22115, 22116, 22129, 22133, 22137, 22160, 22183, 22193, 22194, 22232, 22271, 22285, 22296, 22305, 22309, 22320, 22340, 22362, 22380, 22383, 22403, 22408, 22416, 22456, 22473, 22486, 22488, 22490, 22507, 22514, 22525, 22537, 22539, 22559, 22570, 22582, 22586, 22587, 22588, 22616, 22630, 22633, 22635, 22637, 22641, 22642, 22651, 22653, 22657, 22660, 22668, 22687, 22706, 22726, 22760, 22764, 22773, 22801, 22810, 22841, 22868, 22892, 22928, 22935, 22941, 22958, 22975, 22988, 23013, 23016, 23048, 23051, 23072, 23073, 23075, 23085, 23088, 23095, 23103, 23108, 23121, 23143, 23149, 23168, 23169, 23177, 23180, 23184, 23189, 23193, 23202, 23208, 23235, 23241, 23252, 23256, 23270, 23308, 23314, 23324, 23327, 23338, 23353, 23357, 23368, 23369, 23373, 23389, 23395, 23407, 23418, 23429, 23446, 23450, 23483, 23491, 23505, 23512, 23531, 23536, 23538, 23543, 23578, 23586, 23592, 23595, 23607, 23617, 23621, 23629, 23630, 23632, 23636, 23639, 23642, 23645, 23659, 23668, 23670, 23678, 23687, 23689, 23691, 23708, 23719, 23725, 23732, 23733, 23736, 23741, 23755, 23756, 23772, 23792, 23811, 23818, 23819, 23820, 23839, 23847, 23850, 23863, 23878, 23885, 23893, 23899, 23906, 23909, 23912, 23923, 23924, 23929, 23932, 23937, 23973, 23977, 23979, 23981, 23985, 24009, 24016, 24036, 24043, 24049, 24057, 24071, 24093, 24101, 24104, 24109, 24111, 24123, 24127, 24128, 24143, 24153, 24155, 24177, 24182, 24183, 24187, 24189, 24200, 24203, 24206, 24215, 24223, 24229, 24237, 24243, 24244, 24261, 24263, 24264, 24311, 24313, 24327, 24332, 24333, 24335, 24344, 24349, 24350, 24376, 24379, 24384, 24385, 24409, 24411, 24430, 24441, 24456, 24475, 24478, 24490, 24499, 24511, 24529, 24534, 24543, 24551, 24555, 24565, 24576, 24582, 24584, 24589, 24598, 24607, 24616, 24653, 24663, 24690, 24702, 24704, 24705, 24712, 24720, 24724, 24727, 24732, 24748, 24776, 24778, 24786, 24791, 24796, 24815, 24820, 24826, 24836, 24861, 24863, 24872, 24906, 24907, 24908, 24914, 24917, 24933, 24936, 24944, 24946, 24949, 24950, 24985, 24987, 24997, 25002, 25005, 25007, 25012, 25016, 25048, 25052, 25056, 25063, 25067, 25083, 25090, 25121, 25132, 25138, 25139, 25155, 25158, 25164, 25165, 25166, 25176, 25183, 25184, 25186, 25199, 25204, 25227, 25229, 25243, 25264, 25265, 25266, 25267, 25268, 25281, 25282, 25284, 25291, 25321, 25330, 25334, 25335, 25340, 25372, 25373, 25405, 25408, 25413, 25416, 25433, 25441, 25442, 25456, 25464, 25471, 25477, 25505, 25514, 25518, 25522, 25525, 25535, 25544, 25549, 25552, 25557, 25561, 25564, 25590, 25592, 25595, 25597, 25615, 25616, 27126, 27455, 28150, 28156, 28219, 28221, 29292, 29654, 29655, 29673, 30324, 30370, 30372, 30377, 30380, 30385, 30399, 30420, 30429, 30435, 31966, 32000, 32021, 32067, 32081, 32102, 32106, 32121, 32154, 32159, 32160, 32215, 32307, 32350, 32351, 32354, 32428, 32492, 32576, 32581, 32582, 32592, 32720, 32728, 33029, 33033, 33073, 33078, 33204, 33229, 33238, 33261, 33307, 33387, 33443, 33511, 33566, 33587, 33598, 33623, 33635, 33713, 33719, 33805, 33817, 33821, 33931, 33932, 33980, 34016, 34083, 34161, 34163, 34194, 34227, 34244, 34476, 34486, 34525, 34655, 34657, 34669, 34709, 34729, 34787, 34824, 34848, 35014, 35023, 35027, 35043, 35044, 35050, 35055, 35058, 35059, 35063, 35077, 35078, 35079, 35091, 35096, 35100, 35116, 35117, 35122, 35132, 35133, 35135, 35137, 35142, 35156, 35178, 35182, 35185, 35190, 35198, 35205, 35210, 35213, 35220, 35232, 35235, 35238, 35239, 35240, 35249, 35260, 35268, 35276, 35278, 35281, 35295, 35304, 35307, 35315, 35333, 35349, 35350, 35376, 35379, 35393, 35409, 35413, 35422, 35425, 35430, 35436, 35439, 35441, 35442, 35445, 35446, 35454, 35461, 35462, 35470, 35473, 35474, 35487, 35497, 35515, 35516, 35523, 35528, 35534, 35544, 35549, 35555, 35560, 35562, 35569, 35574, 35588, 35620, 35669, 36031, 36035, 36037, 36052, 36142, 36143, 36147, 36148, 36218, 36534, 36855, 36857, 36859, 36864, 36865, 36868, 36869, 36870, 36871, 36873, 36874, 36875, 36876, 36879, 36880, 36882)
adjust.2 <-c(81, 8832, 9356, 9435, 9480, 9847, 9870, 9897, 10436, 10954, 12528, 13131, 13425, 13679, 14636, 14741, 14749, 14806, 14811, 15070, 15633, 15658, 16083, 17010, 17514, 18506, 19424, 19567, 20084, 20121, 20147, 20168, 20280, 20431, 20516, 20544, 20595, 20620, 20666, 20674, 20931, 20985, 21161, 21175, 21248, 21258, 21390, 21503, 21531, 22580, 23998, 24564, 25130, 25299, 26064, 26524, 26721, 26751, 26985, 27018, 27114, 27186, 27199, 27247, 27310, 27369, 27472, 27475, 27581, 27718, 27968, 29344, 29393, 29833, 30237, 30324, 31275, 31296, 31376, 31492, 31561, 31621, 31708, 31740, 31808, 31845, 31859, 31912, 31935, 32054, 32409, 35665, 35820, 36075, 36166, 36167, 36222, 36225, 36664, 36740, 36742, 36743, 36779, 36819, 36820, 36851, 289, 313, 1340, 1506, 1536, 1544, 1562, 1592, 1604, 1616, 1628, 1636, 1664, 1686, 1687, 1693, 1700, 1705, 1706, 1708, 1718, 1722, 1725, 1726, 1750, 1760, 1770, 1773, 1786, 1792, 1799, 1817, 1826, 1856, 1877, 1910, 1920, 1922, 1942, 1960, 1973, 1978, 1995, 2000, 2009, 2014, 2032, 2033, 2040, 2059, 2070, 2076, 2091, 2104, 2127, 2131, 2137, 2140, 2186, 2191, 2200, 2272, 2292, 2304, 2306, 2309, 2310, 2347, 2349, 2357, 2361, 2363, 2378, 2383, 2391, 2417, 2425, 2436, 2441, 2458, 2471, 2490, 2491, 2504, 2510, 2522, 2543, 2548, 2554, 2565, 2568, 2569, 2588, 2589, 2601, 2609, 2615, 2618, 2627, 2656, 2663, 2667, 2670, 2695, 2707, 2710, 2723, 2724, 2728, 2733, 2736, 2742, 2768, 2771, 2781, 2801, 2812, 2817, 2829, 2833, 2839, 2845, 2862, 2870, 2883, 2886, 2906, 2917, 2920, 2929, 2960, 2992, 3025, 3038, 3080, 3129, 3138, 3145, 3161, 3165, 3171, 3205, 3207, 3219, 3229, 3230, 3231, 3240, 3248, 3251, 3266, 3270, 3271, 3285, 3296, 3313, 3331, 3335, 3342, 3345, 3371, 3390, 3423, 3435, 3441, 3479, 3507, 3511, 3521, 3543, 3554, 3560, 3566, 3570, 3600, 3605, 3608, 3614, 3615, 3633, 3634, 3644, 3645, 3654, 3684, 3691, 3696, 3703, 3734, 3751, 3793, 3798, 3799, 3811, 3840, 3858, 3862, 3864, 3865, 3874, 3882, 3888, 3929, 3933, 3942, 3951, 3962, 3969, 3982, 4000, 4007, 4008, 4024, 4026, 4045, 4075, 4084, 4100, 4117, 4126, 4135, 4162, 4170, 4175, 4184, 4228, 4245, 4249, 4259, 4277, 4280, 4296, 4302, 4307, 4324, 4376, 4402, 4433, 4436, 4447, 4454, 4477, 4478, 4493, 4506, 4509, 4526, 4530, 4538, 4539, 4556, 4574, 4579, 4612, 4631, 4670, 4678, 4682, 4691, 4720, 4739, 4747, 4751, 4762, 4790, 4811, 4812, 4829, 4837, 4845, 4853, 4877, 4884, 4888, 4889, 4893, 4910, 4938, 4940, 4964, 4972, 4985, 4986, 5050, 5064, 5076, 5077, 5085, 5086, 5144, 5147, 5178, 5192, 5205, 5228, 5238, 5268, 5298, 5305, 5310, 5354, 5361, 5375, 5381, 5382, 5408, 5419, 5430, 5440, 5468, 5471, 5493, 5516, 5520, 5523, 5527, 5532, 5547, 5551, 5580, 5582, 5643, 5651, 5654, 5656, 5662, 5692, 5725, 5772, 5777, 5793, 5794, 5806, 5841, 5845, 5855, 5856, 5875, 5880, 5895, 5924, 5942, 5948, 5950, 5956, 5958, 5978, 6011, 6012, 6027, 6037, 6056, 6079, 6092, 6095, 6103, 6115, 6125, 6149, 6177, 6178, 6197, 6206, 6217, 6222, 6244, 6264, 6277, 6279, 6283, 6287, 6305, 6367, 6383, 6385, 6388, 6390, 6392, 6410, 6412, 6436, 6447, 6450, 6478, 6516, 6525, 6527, 6535, 6549, 6561, 6569, 6615, 6619, 6636, 6652, 6658, 6659, 6674, 6678, 6682, 6684, 6697, 6707, 6716, 6727, 7702, 7708, 7726, 7758, 7771, 7799, 7803, 7805, 7825, 7831, 7837, 7840, 7866, 7867, 7870, 7887, 7888, 7948, 7949, 7958, 8001, 8019, 8042, 8068, 8074, 8088, 8173, 8184, 8186, 8213, 8217, 8220, 8222, 8223, 8252, 8263, 8313, 8343, 8463, 8473, 8490, 13635, 13688, 13939, 14184, 14628, 14811, 15621, 15838, 15914, 15915, 16133, 16562, 16700, 18167, 18178, 18182, 18192, 18203, 18255, 18379, 18463, 18734, 19523, 20345, 20844, 20895, 21050, 21338, 22038, 22056, 22110, 22403, 22456, 22801, 23338, 23756, 24490, 24724, 24872, 24985, 25048, 25229, 25408, 25413, 25433, 27455, 28150, 28156, 28219, 28221, 29292, 29654, 29655, 29673, 30324, 30372, 31966, 32307, 32492, 33587, 33713, 34161, 34227, 34244, 35014, 35044, 35050, 35078, 35100, 35117, 35133, 35205, 35213, 35238, 35239, 35278, 35281, 35295, 35349, 35350, 35393, 35409, 35430, 35442, 35445, 35446, 35454, 35461, 35470, 35487, 35497, 35523, 35534, 35555, 36859, 36868, 36870, 36871, 36873, 36876, 36880)
adjust.3 <-c(8832, 9435, 9480, 9847, 9870, 9897, 10954, 13131, 13425, 13679, 14811, 15633, 15658, 17010, 17514, 18506, 20084, 20121, 20147, 20168, 20280, 20431, 20516, 20544, 20595, 20620, 20666, 20674, 20931, 20985, 21161, 21175, 21248, 21258, 21390, 21503, 21531, 26064, 27018, 27247, 27310, 27718, 29344, 29393, 29833, 30324, 31275, 31296, 31376, 31492, 31561, 31621, 31708, 31740, 31808, 31845, 31859, 31912, 31935, 32054, 32409, 36166, 36167, 36664, 36740, 36742, 36743, 36779, 36819, 36820, 36851, 1562, 1604, 1628, 1664, 1686, 1706, 1760, 1770, 1817, 1856, 1995, 2076, 2140, 2306, 2310, 2436, 2458, 2510, 2554, 2589, 2618, 2724, 2742, 2845, 2870, 2883, 2906, 2920, 2992, 3038, 3080, 3129, 3138, 3171, 3205, 3219, 3231, 3248, 3251, 3285, 3331, 3335, 3479, 3554, 3566, 3633, 3634, 3751, 3858, 3862, 3951, 4126, 4162, 4228, 4277, 4280, 4296, 4376, 4477, 4506, 4574, 4579, 4670, 4682, 4691, 4720, 4762, 4940, 5050, 5192, 5268, 5361, 5382, 5440, 5468, 5532, 5551, 5582, 5662, 5692, 5725, 5924, 5942, 5950, 5956, 5978, 6011, 6012, 6027, 6037, 6206, 6222, 6264, 6277, 6367, 6388, 6412, 6478, 6516, 6525, 6636, 6658, 6659, 6674, 6684, 6716, 6727, 7702, 7708, 7726, 7758, 7771, 7805, 7831, 7837, 7840, 7867, 7887, 7888, 7948, 7949, 7958, 8001, 8019, 8068, 8074, 8088, 8184, 8186, 8213, 8217, 8220, 8222, 8223, 8263, 8343, 8463, 8490, 14811, 15838, 15914, 15915, 16133, 16562, 16700, 18734, 22038, 22110, 22403, 22456, 22801, 23338, 23756, 24490, 24724, 24872, 24985, 25048, 25229, 25408, 25413, 25433, 27455, 28219, 28221, 29292, 29654, 29655, 29673, 30324, 35014, 35044, 35050, 35078, 35100, 35117, 35133, 35205, 35213, 35238, 35239, 35278, 35281, 35295, 35349, 35350, 35393, 35409, 35430, 35442, 35445, 35461, 35470, 35487, 35497, 35523, 35534)
adjust.4<-c(13131, 13425, 14811, 18506, 32054, 32409, 36740, 36742, 36819, 2306, 8186, 8220, 8222, 8343, 14811, 16562, 16700, 18734, 22110, 22403, 22801, 23756, 24872, 24985, 25048, 25229, 25408, 25413, 27455, 29292, 29654, 29655, 29673, 30324, 35044, 35350, 35393, 35430, 35470, 35487, 35497, 35523)
adjust.5<-c(36740,36742,36819,2306, 8186, 8220, 8222, 8343, 14811, 18734, 30324)
adjust.6<-c(285, 289, 294, 297, 300, 302, 305, 307, 313, 323, 325, 328, 490, 546, 587, 713, 4579, 7243, 7317, 7460, 7763, 7904, 7954, 8254, 13631, 13706, 13827, 14192, 14274, 14323, 14409, 14463, 14540, 14562, 14698, 15356, 15510, 15621, 18321, 21050, 21991, 22001, 22003, 22012, 22040, 22042, 22056, 22057, 22071, 22073, 22100, 22106, 22113, 22115, 22116, 22129, 22133, 22137, 22160, 22183, 22193, 22194, 22232, 22271, 22285, 22296, 22305, 22309, 22320, 22340, 22362, 22380, 22383, 22408, 22473, 22486, 22490, 22507, 22514, 22525, 22537, 22539, 22559, 22570, 22582, 22586, 22587, 22588, 22616, 22630, 22633, 22635, 22637, 22641, 22642, 22651, 22653, 22657, 22660, 22668, 22687, 22706, 22726, 22760, 22764, 22773, 22810, 22841, 22868, 22892, 22928, 22935, 22941, 22958, 22975, 22988, 23013, 23016, 23048, 23051, 23072, 23073, 23075, 23085, 23088, 23095, 23103, 23108, 23121, 23143, 23149, 23168, 23169, 23177, 23180, 23184, 23189, 23193, 23202, 23208, 23235, 23241, 23252, 23256, 23270, 23308, 23314, 23324, 23327, 23353, 23357, 23368, 23369, 23373, 23389, 23395, 23407, 23418, 23429, 23446, 23450, 23483, 23491, 23505, 23512, 23531, 23536, 23538, 23543, 23578, 23586, 23592, 23595, 23607, 23617, 23621, 23629, 23630, 23632, 23636, 23639, 23642, 23645, 23659, 23668, 23670, 23678, 23687, 23689, 23691, 23708, 23719, 23725, 23732, 23733, 23736, 23741, 23755, 23772, 23792, 23811, 23818, 23819, 23820, 23839, 23847, 23850, 23863, 23878, 23885, 23893, 23899, 23906, 23909, 23912, 23923, 23924, 23929, 23932, 23937, 23973, 23977, 23979, 23981, 23985, 24009, 24016, 24036, 24043, 24049, 24057, 24071, 24093, 24101, 24104, 24109, 24111, 24123, 24127, 24128, 24143, 24153, 24155, 24177, 24183, 24187, 24189, 24200, 24203, 24206, 24215, 24223, 24229, 24237, 24243, 24244, 24261, 24264, 24311, 24313, 24327, 24332, 24333, 24335, 24344, 24349, 24350, 24376, 24379, 24384, 24385, 24409, 24411, 24430, 24441, 24456, 24475, 24478, 24499, 24511, 24529, 24534, 24543, 24551, 24555, 24565, 24576, 24582, 24584, 24589, 24598, 24607, 24616, 24653, 24663, 24690, 24702, 24704, 24705, 24712, 24720, 24727, 24732, 24748, 24776, 24778, 24786, 24791, 24796, 24815, 24820, 24826, 24836, 24861, 24863, 24906, 24907, 24908, 24914, 24917, 24933, 24944, 24946, 24949, 24950, 24987, 24997, 25002, 25005, 25007, 25012, 25016, 25052, 25056, 25063, 25067, 25083, 25090, 25121, 25132, 25138, 25139, 25155, 25158, 25164, 25165, 25166, 25176, 25183, 25184, 25186, 25199, 25204, 25227, 25243, 25264, 25265, 25266, 25267, 25268, 25281, 25282, 25284, 25291, 25321, 25330, 25334, 25335, 25340, 25372, 25373, 25405, 25416, 25441, 25442, 25456, 25464, 25471, 25477, 25505, 25514, 25518, 25522, 25525, 25535, 25544, 25552, 25557, 25561, 25564, 25590, 25592, 25595, 25597, 25615, 25616, 28150, 28156, 30370, 30372, 30377, 30380, 30385, 30399, 30420, 30429, 30435, 35562, 36142, 36143, 36147, 36148, 36855, 36857, 36864, 36865, 36869, 36879, 36882)
adjust.7<-c(285, 289, 294, 297, 300, 302, 305, 307, 313, 323, 325, 328, 490, 546, 587, 713, 4579, 7243, 7317, 7460, 7763, 7904, 7954, 8254, 13631, 13706, 13827, 14192, 14274, 14323, 14409, 14463, 14540, 14562, 15356, 15621, 18321, 21050, 22056, 22115, 22285, 22296, 22588, 22635, 22988, 23073, 23169, 23177, 23180, 23202, 23235, 23324, 23429, 23586, 23621, 23629, 23732, 23850, 23924, 23973, 24111, 24153, 24177, 24189, 24215, 24244, 24511, 24529, 24551, 24576, 24690, 24826, 24907, 24908, 25016, 25056, 25138, 25184, 25267, 25456, 25514, 25552, 28150, 28156, 30370, 30372, 30377, 30380, 30385, 30399, 30420, 30429, 30435, 36142, 36143, 36147, 36148, 36855, 36857, 36864, 36865, 36869, 36879, 36882)
adjust.8 <- c(4579, 7904, 7954, 15621, 21050, 22056, 28150, 28156, 30370, 30372, 30377, 30380, 30385, 30399, 30420, 30429, 30435, 36142, 36143, 36147, 36148)
adjust.9 <-c(4579, 15621, 21050, 22056, 28150, 28156, 36142, 36143, 36147, 36148)

positions[adjust,1]<-positions[adjust,1]-.5
positions[adjust.2,2]<-positions[adjust.2,2]+.5
positions[adjust.3,1]<-positions[adjust.3,1]+1
positions[adjust.4,2]<-positions[adjust.4,2]-1
positions[adjust.5,1]<-positions[adjust.5,1]-.5
positions[adjust.5,2]<-positions[adjust.5,2]+.4
positions[adjust.6,2]<-positions[adjust.6,2]+.6
positions[adjust.7,1]<-positions[adjust.7,1]+.5
positions[adjust.8,2]<-positions[adjust.8,2]-.5
positions[adjust.9,2]<-positions[adjust.9,2]-.5
positions[c(8186,8220,8222,8343),1]<--80.933333
positions[c(8186,8220,8222,8343),2]<-25.5
positions[2306,1]<--119.15
positions[2306,2]<-34.36
positions[18734,1]<--75.917222
positions[18734,2]<-35.8425
positions[30420,1]<--71.26
positions[30420,2]<-41.51 
positions[30385,1]<--71.62
positions[30385,2]<-41.39 
positions[21050,1]<--75.36
positions[21050,2]<-39.58 
positions[22056,1]<--72.68
positions[22056,2]<-40.94
positions[15621,1]<--76.1
positions[15621,2]<-39.23
positions[c(28150,28156),1]<--123.08
positions[c(28150,28156),2]<-45.95

colnames(positions)<-c('longitude','latitude')
coordinates(positions)<-c('longitude','latitude')
proj4string(positions)<-CRS("+proj=longlat +datum=NAD83")

#find corresponding county
county.no<-rep(NA,length(full.6$longitude))
#for(i in 1:length(full.6$longitude)){
county.no<-positions%over%us.counties.2
full.6$county<-county.id[county.no]
full.6$abbr2<-state.id[county.no]
full.6$combined<-names(us.counties.2)[county.no]

#merge county fips codes in
data(county.fips)
full.7<-merge(x=full.6,y=county.fips,by.x="combined",by.y="polyname",all.x=TRUE,all.y=FALSE)

#merge county population density in
density<-read.csv('countyDensity.csv')
full.8<-merge(x=full.7,y=density,by="fips",all.x=TRUE,all.y=FALSE)
full.8<-subset(full.8,select=-c(combined, abbr2))
full.8<-subset(full.8,select=-c(combined, abbr2))

############WRITE OUT THE FILE###################
write.csv(full.8, 'majorAirAllDist.csv', row.names=FALSE)


