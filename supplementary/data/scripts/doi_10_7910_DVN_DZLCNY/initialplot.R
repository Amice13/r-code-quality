## Code to initialize the data and reproduce initial plots of the data

# packages to be installed if not already installed 
lspack<- c("raster","lattice","ggplot2","foreign","plyr","maptools","rgdal","rgeos","sp","gridExtra","viridisLite","maps",
                      "latticeExtra", "countrycode","lubridate","deldir","prettymapr","rasterVis","prioritizr", "GISTools","dplyr",
                      "ggthemes","grid")
new.packages <- lspack[!(lspack %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(lspack, library, character.only = TRUE)

#create objects for each region 
mypts <- sp::SpatialPoints(cbind(GTD$longitude,GTD$latitude))
sp::proj4string(mypts) <- sp::proj4string(allworld)
world<-allworld
world = raster::intersect(world,mypts)
world <-rgeos::gBuffer(world, byid=TRUE, width=0)
#check countries removed
# removedc <- rgeos::gDifference(allworld,world)
# plot(allworld)
# plot(removedc,add=TRUE,col="green")
#keep PRIOGRID cells inside the study area
PRIO <- raster::intersect(world,PRIOglobal)
#remove areas where terrorism cannot occur based on population density
pop00 <- prios[c("pop","gid")]
pop00$pop[is.na(pop00$pop)] <- 0
names(pop00)<-c("pop00","gid")
PRIO@data = data.frame(PRIO@data, pop00[match(PRIO@data[,"gid"], pop00[,"gid"]),])
larea <- prios[c("landarea","gid")]
PRIO@data = data.frame(PRIO@data, larea[match(PRIO@data[,"gid"], larea[,"gid"]),])
#compute population density per land area in grid-cell [km2]
PRIO$popdens <- PRIO$pop00/PRIO$landarea
PRIO$popdens[is.na(PRIO$popdens)] <- 0
#summary(PRIO$popdens)
#keep areas if more than 1 people per km2
PRIO <- raster::subset(PRIO, popdens > 5)
PRIO$gid.1 <- PRIO$gid.2 <-NULL
PRIO$landarea <- PRIO$pop <-  NULL
PRIO$pop00 <-  NULL
#remove too small polygons
PRIO$area <- rgeos::gArea(PRIO,byid = T)
#summary(PRIO$area)
PRIO <- PRIO[PRIO$area > 0.025,]#0.25 is area of cell

#transform data for  mapping
allcountry <- ggplot2::fortify(world, region = "iso_a3")  # country border 
allregion <- ggplot2::fortify(world, region = "myregion")  # region border 
allPRIO <- ggplot2::fortify(PRIO, region = "gid") #background plot / polygon shape
gtdt <- sp::SpatialPointsDataFrame(mypts,data.frame(lon=GTD$longitude,lat=GTD$latitude,Rdate=GTD$Rdate))
#keep terrorist events inside PRIOGRID (GTD events removed if spatially inaccurate)
gtdt <- gtdt[!is.na(sp::over(gtdt,as(PRIO,"SpatialPolygons"))),]
#add internal border of countries creating regions with negative buffer
worldunion <- rgeos::gUnaryUnion(world, id = world@data$regnb,checkValidity=2)
intborder <- rmapshaper::ms_filter_islands(worldunion, min_area = 500000)
intborder <- sp::SpatialPolygonsDataFrame(intborder,data=data.frame(id=unique(world@data$regnb)))
intborder <-rgeos::gBuffer(intborder, byid=TRUE, width= -0.15)
#add region names to the internal border polygon
intreg <- unique(data.frame(Regions=world$myregion,id=world$regnb))
intborder@data <- merge(intborder@data,intreg, by= "id")
intborder <- ggplot2::fortify(intborder,region = "Regions")
rm(worldunion,intreg)


#plot with colors for the regions
pworld<- 
  ggplot(data = allregion) +
  geom_polygon(data = allPRIO, aes(long, lat, group = group), fill = NA ,colour="grey75",size=0.05)   + #"grey75" 
  geom_point(data = gtdt@data , aes(x=lon, y=lat),size=0.85, shape = 16, alpha=0.7, colour = "darkred") +
  geom_polygon(data = allcountry, aes(long, lat, group = group), fill = NA,colour="grey55",size=0.5) +
  geom_polygon(data = allregion,aes(long, lat, group = group),color = "black",fill=NA, alpha=0.1,size=1.1) + 
  geom_polygon(data = intborder,aes(long, lat, group = group,color = id),fill=NA, alpha=0.0,size=0.9) + 
    scale_color_manual(name="Regions",values=regcol)+ ggplot2::theme_void()+ 
    theme(legend.position = "none")
plot.new()
cairo_pdf(file=paste0("results/figs/","studyarea_color.pdf"),width=25,height=14,
          family="serif",)
par(mar = c(0.1, 0.5, 2.5, 0.1))#bottom,left,top,right
# sets the bottom, left, top and right margins respectively of the plot region in number of lines of tex
print(pworld)
while (!is.null(dev.list()))  dev.off()
rm(pworld)

#END
rm(allcountry,allPRIO,allregion, mypts,pworld,larea)

