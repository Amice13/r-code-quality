#R
#pb210 accum rate analysis

#from rplume analysis on andean cores

library(ggplot2)
library(gridExtra)
library(terra)

#

pcdir<-"D:/Dropbox/papers/peat 210 pb/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats"

setwd(pcdir)

###############
#carbon depth
carbon<-read.csv("CORESASH.csv",header=TRUE)
bd<-read.csv("BKDENS 2.csv",header=TRUE)

cores_clim<-read.csv("cores_clim.csv",header=TRUE)


carbon$carbon.g.g<- 1-(carbon$CRUC.ASH.W.g-carbon$CRUC.W.g)/(carbon$CRUC.SAM.W.g-carbon$CRUC.W.g)

carbon2<- aggregate(carbon.g.g ~ depth2+Core ,FUN=mean,data=carbon)
bd$corecode<-paste(bd$Core,bd$depth2,sep="")

bd$bd<-bd$weight/(pi*((2.54*2)^2)*bd$depth.1*(1/2))

bd2<-aggregate(bd ~ depth2+Core ,FUN=mean,data=bd)

carbon2$corecode<-paste(carbon2$Core,carbon2$depth2,sep="")
bd2$corecode<-paste(bd2$Core,bd2$depth2,sep="")


carbon3<-merge(carbon2,bd2,by="corecode",all=TRUE)
plot(carbon3$bd,log (carbon3$carbon.g.g))
abline(a<-lm(log(carbon.g.g) ~ bd,data=carbon3))
c<-lm(log(bd) ~ carbon.g.g,data=carbon3)
carbon3$carbon.p<-exp(predict(a,carbon3))
carbon3$bd.p<-exp(predict(c,carbon3))

carbon3$carbon.g.g2<-ifelse(is.na(carbon3$carbon.g.g),carbon3$carbon.p,carbon3$carbon.g.g)
carbon3$bd2<-ifelse(is.na(carbon3$bd),carbon3$bd.p,carbon3$bd)
carbon3$core<-ifelse(is.na(carbon3$Core.x),carbon3$Core.y,carbon3$Core.x)
carbon3$depth<-ifelse(is.na(carbon3$depth2.x),carbon3$depth2.y,carbon3$depth2.x)


carbon4<-data.frame(core=carbon3$core,depth=carbon3$depth,bd=carbon3$bd2,carbon.g.g=carbon3$carbon.g.g2)


carbon4$carbon<-carbon4$bd*carbon4$carbon.g.g *100 #mg C per ha

carbon4$code<- paste(carbon4$core,carbon4$depth,sep="")

#load dates from plum 210pb analysis

dates<-read.csv("all_ages.csv",header=TRUE)
dates$agediff<-1
dates$time

cores<-unique(dates$core)

dates2<-dates[-c(1:dim(dates)[1]),]


for(i in seq(1,length(cores))){

	a<-subset(dates,core==cores[i])
	a$agediff<-c(0,diff(a$mean))
	a$time<-cumsum(a$agediff)
	dates2<-rbind(dates2,a)
}

plot_depths<-ggplot(data=dates2,aes(x=depth,y=time,group=core))
plot_depths+geom_point(size=0.1)+geom_line(aes(colour=core))
#create new data frame

dates3<-dates2[,-c(3:6)]

dates3$code<-paste(dates3$core,dates3$depth,sep="-")
carbon4$code<-paste(carbon4$core,carbon4$depth,sep="-")

dates4<-merge(dates3,carbon4,by="code",all=TRUE)

dates4$core<-ifelse(is.na(dates4$core.x),dates4$core.y,dates4$core.x)
dates4$depth<-ifelse(is.na(dates4$depth.x),dates4$depth.y,dates4$depth.x)
dates5<-dates4[-c(1:length(dates4$depth)),]

cores<-unique(dates3$core)
for ( i in seq (1:length(cores))){
	a<-subset(dates4,core==cores[i])
	b<-subset(a,!is.na(bd))
	c<-subset(b,depth==max(depth))
	a$bd<-ifelse(is.na(a$bd),c$bd,a$bd)
	a$carbon.g.g<-ifelse(is.na(a$carbon.g.g),c$carbon.g.g,a$carbon.g.g)
	a$carbon<-ifelse(is.na(a$carbon),c$carbon,a$carbon)
	dates5<-rbind(dates5,a)
	print(dim(a))
}

dates6<-subset(dates5[,-c(1,2,3,7,8)],depth!=0)

dates6$c.rates<-dates6$carbon/dates6$agediff
plot_rates<-ggplot(data=dates6,aes(x=depth,y=c.rates,group=core))
plot_rates+geom_point(size=0.1)+geom_line(aes(colour=core))+facet_wrap(vars(core),nrow=4)


##################################################
####################################################################################################
#climate

#last 25 years

dates6$interval<-ifelse(dates6$year>2000,"10Y",NA)
dates6$interval<-ifelse(dates6$year>1985 & dates6$year<2000,"10-25Y",dates6$interval)
dates6$interval<-ifelse(dates6$year>1960 & dates6$year<1985,"25-50Y",dates6$interval)
dates6$interval<-ifelse(dates6$year>1910 & dates6$year<1960,"50-100Y",dates6$interval)
dates6$interval<-ifelse(dates6$year<1910,">100Y",dates6$interval)


dates7<-merge(dates6,cores_clim,by="core")

cores<-unique(dates7[,c(1,17,18)])


#load points
core_points <- vect("core_points.shp")


pcdir<-"F:/Espeletia"

setwd(pcdir)


#study area
#vect loads vector using terra
studyarea <- vect("area/1000m polygonv3.shp")

plot(studyarea)
plot(core_points,add=TRUE,col="red")

