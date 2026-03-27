# Data preparation 
# Aim: extract and clean covariates and terrorism (GTD) data
# In addition, this code generate a discretized study area

# save figure in following path
figpath<-paste0("results/figs")

# packages to be installed if not already installed 
list.of.packages <- c("raster","lattice","ggplot2","foreign","plyr","maptools","rgdal","rgeos","sp","gridExtra","viridisLite","maps",
                      "latticeExtra", "countrycode","lubridate","deldir","prettymapr","rasterVis","prioritizr", "GISTools","dplyr",
                      "ggthemes","cowplot","grid","sf", "nngeo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
#load packages
lapply(list.of.packages, library, character.only = TRUE)

myregion<-world[world$regnb==mm,]
backcountry<-myregion
gtpt <- SpatialPoints(gtdt)
proj4string(gtpt) <- proj4string(backcountry) 
myregion <- backcountry[!is.na(sp::over(backcountry,gtpt)),]
myregion <-rgeos::gBuffer(myregion, byid=TRUE, width=0)

regnodata <- myregion
regnodata@data[,2:ncol(regnodata@data)] <- NULL
PRIOreg <- raster::intersect(regnodata,PRIO)
colnames(PRIOreg@data)[colnames(PRIOreg@data) == "iso_a3.1"] <- "iso_a3"
PRIOreg$iso_a3.2  <-NULL
rm(PRIO,regnodata)

#keep GTD events inside PRIOGRID of the region
GTDpt <- gtdt[!is.na(sp::over(gtdt,as(PRIOreg,"SpatialPolygons"))),]

#check plots
#plot(myregion,col="red");points(GTDpt,cex=1,pch='*',col="green");plot(backcountry,add=TRUE,border='grey',lwd=2)   

#keep original PRIO for mapping only (to check if NA are present)
PRIOback <- PRIOreg
PRIOback$id <- 1:nrow(PRIOback@data)
# add static covariate data into PRIO polygon
PRIOreg@data = data.frame(PRIOreg@data, prios[match(PRIOreg@data[,"gid"], prios[,"gid"]),])
PRIOreg$gid.1  <-NULL

#add non-PRIOreg covariates to PRIOreg polygon
# alt<-raster::raster(alt)
# acc<-raster::raster(acc)
# gr<-raster::raster(gr)
alt<-raster::crop(alt,myregion)
acc<-raster::crop(acc,myregion)
gr<-raster::crop(gr,myregion)
altitude<-prioritizr::fast_extract(alt, PRIOreg,fun="mean",na.rm=TRUE)
access<-prioritizr::fast_extract(acc, PRIOreg,fun="mean",na.rm=TRUE)
grip<-prioritizr::fast_extract(gr, PRIOreg,fun="mean",na.rm=TRUE)

# Join mean values to polygon data
PRIOreg@data <- cbind(PRIOreg@data, altitude=altitude)
PRIOreg@data <- cbind(PRIOreg@data, access=access)
PRIOreg@data <- cbind(PRIOreg@data, grip=grip)

rm(alt,acc,gr,altitude,access,grip)

#save study area (without covariates)
voronpoly <- PRIOreg
voronpoly@data$x <- voronpoly$xcoord
voronpoly@data$y <- voronpoly$ycoord
voronpoly@data$gid <- voronpoly$gid
voronpoly@data$iso_a3 <- voronpoly$iso_a3
voronpoly@data$id <- 1:nrow(voronpoly@data)
voronpoly@data <- voronpoly@data[c("x","y","gid","iso_a3", "id")] 
# save study area (country) for mapping
save(myregion, file = paste0("results/studyarea/studyarea",mm,".Rdata"))
# save PRIOGRID by region for mapping
#remove potential duplicates 
voronpoly  <- voronpoly[!duplicated(voronpoly@data[,c('id')]),]
voronpoly@data$id <- as.character(1:nrow(voronpoly@data))
save(voronpoly, file = paste0("results/voronoi/voronpoly",mm,".Rdata"))

# Add spatiotemporal covariates into PRIOregGRID polygons
#repeat the static PRIOregGRID dataframe for each year
PRIOregdf <- list()
for (i in 1:nyears){
  PRIOregdf[[i]] <-data.frame(PRIOreg)
  PRIOregdf[[i]]$year <- min(priot$year,na.rm=TRUE)+i-1
}
PRIOregdf <-  do.call(rbind,PRIOregdf)
#merge static with temporal dataframe by year and gid
priot$gid<-as.character(priot$gid)
#summary(PRIOregdf);summary(priot)
PRIOregdf<- merge(PRIOregdf,priot,by=c("gid","year"))

#add all data into PRIGRID polygon
voronpolyt <-list()
for (i in 1:nyears){
  voronpolyt[[i]] <- voronpoly 
  vorondata <- PRIOregdf[PRIOregdf$year==(min(priot$year,na.rm=TRUE)+i-1),]
  voronpolyt[[i]]@data = data.frame(voronpolyt[[i]]@data, vorondata[match(voronpolyt[[i]]@data[,"gid"], vorondata[,"gid"]),])
  voronpolyt[[i]]$gid.1  <-NULL
}

#keep only one variable of interest LDI from vdem polygons
#add vdemdf into Vdem polygon, then
#join Vdem with  PRIOGRID polygon
for (i in 1:nyears){
  mydf <- vdemdf[vdemdf$year==(min(priot$year,na.rm=TRUE)+i-1),]
  vdem[[i]]@data <- data.frame(LDI=mydf$LDI)
  voronpolyt[[i]] <- raster::intersect(voronpolyt[[i]],vdem[[i]])
  voronpolyt[[i]]$iso_a3.1  <-NULL
  #remove unused covariates
  voronpolyt[[i]]@data$landarea <- NULL
  voronpolyt[[i]]@data$type <- NULL
  voronpolyt[[i]]@data$homepart <- NULL
  voronpolyt[[i]]@data$area <- NULL
  voronpolyt[[i]]@data$xcoord <- NULL
  voronpolyt[[i]]@data$ycoord <- NULL
}

#Add per capital GDP (worldbank) into voronpoly
load(file = paste0("subcode/data/gdpoly.Rdata"))
#add variable
for (i in 1:nyears){
 gddf <-gdpoly[[i]]@data
  voronpolyt[[i]]@data = data.frame(voronpolyt[[i]]@data, gddf[match(voronpolyt[[i]]@data[,"iso_a3"], gddf[,"iso_a3"]),])
   voronpolyt[[i]]$iso_a3.1  <-NULL
}


#Add weekly occurrence of terror at grid-cell level
# prepare sampling data (presence of terrorism)
sampdf <- data.frame(
  x      = GTDpt$lon,
  y      = GTDpt$lat,
  Rdate  = GTDpt$Rdate,
  terror = 1)
sampling <- sampdf
sampling$year  <-  lubridate::year(sampling$Rdate)
sampling$month  <-  lubridate::month(sampling$Rdate)
sampling$week_year  <-  lubridate::week(sampling$Rdate)
sampling$week  <-  lubridate::week(sampling$Rdate)
sampling$week  <-  sampling$week + 53*(sampling$year-min(sampling$year))#give number of week without restarting at new year
sampling$day  <-  lubridate::day(sampling$Rdate)
sampling$weekday  <-  lubridate::wday(sampling$Rdate)
samplingpt <- sampling
sp::coordinates(samplingpt)  <-  c("x","y")
sp::proj4string(samplingpt)  <-  sp::CRS(WGS84)

# count nb. of terrorist events within a fine grid-cell for each week
time_info <- data.frame(time = seq(as.Date("2002-01-01"), as.Date("2017-12-31"), by = 1))
# time_info$time <- as.POSIXct(time_info$time, origin = "1970-01-01")
time_info <- dplyr::mutate(time_info,
                           year      = lubridate::year(time),
                           month     = lubridate::month(time),
                           week_year = lubridate::week(time),
                           week      = week_year + 53 * (year - min(year)))
time_info <- unique(dplyr::select(time_info, -time, -month))
#terror years (min and max)
GTDtimemin <- min(time_info$year,na.rm=TRUE)
GTDtimemax <- max(time_info$year,na.rm=TRUE)
GTDyears <- GTDtimemax -  GTDtimemin + 1

#PRIOregGRID
for (i in 1:length(voronpolyt))
{
  #lag covariates to avoid endogeneity######################
  voronpolyt[[i]]@data$year <- voronpolyt[[i]]@data$year+1
}
##########################################################  
#voronpolyt is a list of 16 spdf, one per year from 2001 to 2019 (lagged)
#voronpolyt[[1]]$year#2001 
#voronpolyt[[18]]$year#2018 
#keep from 2002 to 2017 to match GTD
############################manual operation (to be checked if time period change)#################################
#remove elements of list: element 1 (2001), element 18 (2018)
voronpolyt <- voronpolyt[ -c(1,18)]
############################manual operation (to be checked if time period change)#################################

#remove potential duplicates 
for (i in 1:length(voronpolyt)) {
  voronpolyt[[i]]  <- voronpolyt[[i]][!duplicated(voronpolyt[[i]]@data[,c('id')]),]
  voronpolyt[[i]]@data$id <- as.character(voronpolyt[[i]]@data$id)
}

#match polygon year with week (create a correspondence dataframe to use the correct priogrid polygon for each week)
priotime <- data.frame(year = time_info$year, week = time_info$week)
priow <- data.frame(year=rep(GTDtimemin:GTDtimemax,times=1, each=53),priolist=rep(1:(GTDyears),times=1, each=53),week=1:(53*(GTDyears)))
#process the data
terrordata <- list()
pb = txtProgressBar(min = 1, max = max(samplingpt@data$week), initial = 0, style = 3)
for (i in 1:max(samplingpt@data$week)) {
setTxtProgressBar(pb, i)
#select terrorist events only
  terrorpt <- samplingpt[(samplingpt@data$terror == 1 & samplingpt@data$week == i),]
  time_df <- time_info[time_info$week == i,]
  #index PRIOregGRID
  prioindexdf <- priow[priow$week==i,]
  prioindex <- prioindexdf$priolist
  # count terrorist events within each cell
  voronweek <- voronpolyt[[prioindex]]
  if (!length(terrorpt)>0){
    voronweek@data$terror <- 0
    vorondata <- data.frame(voronweek)
    vorondata$year <- time_df$year
    vorondata$week_year <- time_df$week_year
    terrordata[[i]] <- droplevels(vorondata)
  } else {
    pts = sf::st_as_sf(terrorpt)
    voronw = sf::st_as_sf(voronweek)
    # Spatial join (with small spatial tolerance due to potential spatial issues)
    library(sf)
    library(nngeo)
    p1 = sf::st_join(pts, voronw, join = nngeo::st_nn)
    st_geometry(p1) <- NULL
    p1 <- p1[c("gid","terror")]
    terrorcount <-data.frame(p1)
    names(terrorcount)<-c("gid","terror")
    terrorcount$gid <- as.character(terrorcount$gid)
    terrorcount$terror <- as.numeric(terrorcount$terror)
    voronweek@data = data.frame(voronweek@data, terrorcount[match(voronweek@data[,"gid"], terrorcount[,"gid"]),])
    rm(terrorcount)
    voronweek@data$terror[is.na(voronweek@data$terror)]  <-  0
    vorondata <- data.frame(voronweek)
    vorondata$year <- time_df$year
    vorondata$week_year <- time_df$week_year
    vorondata$gid.1 <- NULL
    terrordata[[i]] <- droplevels(vorondata)
  }
}
save(terrordata, file = paste0("results/terrordata/terrordata",mm,".Rdata"))


rm(priotime,priow,PRIOglobal,PRIOreg,PRIOregdf,
   backcountry,backcountryf,GTDpt,mydata,myregion,p1,pop00,PRIOback,PRIObackf,prios,priot,sampdf,
   sampling,terrordata,time_df,vdem,vdemdf,voron,vorondata,vorondf,voronpolyt)
#End script
