#R
#2023
#climate prep for espeletia modelling

macdir<-NA
pcdir<-"F:/Espeletia"

library(terra)


#files in f disk at office
setwd(pcdir)


#study area
#vect loads vector using terra
studyarea <- vect("area/1000m polygonv3.shp")

#load points
sample_points <- vect("randompoints.shp")

#load watersheds
watersheds <- vect("Cuencas 1000m_v3/cuencas_clip2.shp")


#currentclimate

#precip
prec<-rast(c("wc2.1_30s_prec/wc2.1_30s_prec_01.tif","wc2.1_30s_prec/wc2.1_30s_prec_02.tif",
	"wc2.1_30s_prec/wc2.1_30s_prec_03.tif","wc2.1_30s_prec/wc2.1_30s_prec_04.tif",
	"wc2.1_30s_prec/wc2.1_30s_prec_05.tif","wc2.1_30s_prec/wc2.1_30s_prec_06.tif",
	"wc2.1_30s_prec/wc2.1_30s_prec_07.tif","wc2.1_30s_prec/wc2.1_30s_prec_08.tif",
	"wc2.1_30s_prec/wc2.1_30s_prec_09.tif","wc2.1_30s_prec/wc2.1_30s_prec_10.tif",
	"wc2.1_30s_prec/wc2.1_30s_prec_11.tif","wc2.1_30s_prec/wc2.1_30s_prec_12.tif"))

Prec_clip<-crop(prec,studyarea)

Prec_s<-sum(Prec_clip)


#tmin
Tmin<-rast(c("wc2.1_30s_tmin/wc2.1_30s_tmin_01.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_02.tif",
	"wc2.1_30s_tmin/wc2.1_30s_tmin_03.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_04.tif",
	"wc2.1_30s_tmin/wc2.1_30s_tmin_05.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_06.tif",
	"wc2.1_30s_tmin/wc2.1_30s_tmin_07.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_08.tif",
	"wc2.1_30s_tmin/wc2.1_30s_tmin_09.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_10.tif",
	"wc2.1_30s_tmin/wc2.1_30s_tmin_11.tif","wc2.1_30s_tmin/wc2.1_30s_tmin_12.tif"))

Tmin_clip<-crop(Tmin,studyarea)

Tmin_m<-mean(Tmin_clip)

#tmax
tavg<-rast(c("wc2.1_30s_tavg/wc2.1_30s_tavg_01.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_02.tif",
	"wc2.1_30s_tavg/wc2.1_30s_tavg_03.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_04.tif",
	"wc2.1_30s_tavg/wc2.1_30s_tavg_05.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_06.tif",
	"wc2.1_30s_tavg/wc2.1_30s_tavg_07.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_08.tif",
	"wc2.1_30s_tavg/wc2.1_30s_tavg_09.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_10.tif",
	"wc2.1_30s_tavg/wc2.1_30s_tavg_11.tif","wc2.1_30s_tavg/wc2.1_30s_tavg_12.tif"))

tavg_clip<-crop(tavg,studyarea)

tavg_m<-mean(tavg_clip)


#bioc

#futureclimate

#clip with area

tmin_ssp245_2040_clip<-crop(tmin_ssp245_2040,studyarea)

#using terra mean value from the stack 12 months
tmin_ssp245_2040_m1<-mean(tmin_ssp245_2040_clip)

#extract environmental values on sampling points

points_value<-extract(Tmin_m,sample_points)
points_value2<-geom(sample_points)
points_value3<-vect(points_value2,"points")

locations <-  xyFromCell(tmin_ssp245_2040_m1, which(tmin_ssp245_2040_m1[]==points_value[2][[1]][1]))
locations2<-vect(locations,"points")

distance1<-pointDistance(geom(points_value3)[1,c(3,4)],geom(locations))

distance1<-pointDistance(locations,locations)

locations<-vect(locations,"points")


colnames(locations2)<-c('x', 'y','ID')

locations3<-vect(locations2,"points")

locations2$ID<-points_value$ID

plot(tmin_ssp245_2040_m)
plot(locations2[locations2$ID==1],add=TRUE)

plot(points_value[1],add=TRUE)











