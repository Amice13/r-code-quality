### input year and compute
######## need aug$AS, slavepaths, tmat, Cities, krg
library(fields) #for krig, quilt plot, etc.
library(MASS) # for kde2d
#library(pals) #plotting palettes
library(rgdal) #QGIS Map properties
library(raster) # for crop function and plotting
library(dplyr)




path <- "~/Documents/LISAprojects/Fall18Oyo/Docker/"
path <- "/home/oyo/"
source(paste0(path, 'AllOyoFunctions2.R'))
load(paste0(path, 'forShiny.rda'))
load(paste0(path, 'col.pals.rda'))
aug <- object.list$aug
# for(yr in 1817:1836){
#   load(paste0(path, 'shinydata/forShiny;',yr,'.rda'))
#   attach(object.list)
#   attach(data.list)
#   list2env(data.list, envir = .GlobalEnv)
#   list2env(object.list, envir = .GlobalEnv)
# }



zoom <- data.list$zoom
rivers <- data.list$rivers
lakes <- data.list$lakes
ocean <- data.list$ocean

#load(paste0(path, 'newton.data.rda'))
#load(paste0(path, 'slavepaths0.rda'))
#conf <- read.csv(paste0(path,"Conflict.csv"), header=TRUE)
#Cities <- read.csv(paste0(path,"BigCities.csv"), header=TRUE)

### for each year, need 
# tmat and AS -> aug [Note: need to do tmat again] (aug$TR.xy defines locations and aug$aug.mat gives connections)
# slavepaths (sim.locs and nsims)
# rewards


#Cities[,2:3] <- LLproj(Cities[,2:3])
#conf[,6:7] <- LLproj(conf[,6:7])
#rivers <- readOGR(dsn=path.expand(paste0(path,"./Map/largerivers.shp")), layer="largerivers")
#zoom <- readOGR(dsn=path.expand(paste0(path,"./Map/Zoom1.shp")), layer="Zoom1")
#lakes <- readOGR(dsn=path.expand(paste0(path,"./Map/Lakes_Lagoons.shp")), layer="Lakes_Lagoons")
#ocean <-readOGR(dsn=paste0(path,"./Map"),layer="ocean2")

borders <- shapefile(paste0(path,"./Map/borders/Borders/Borders.shp" ))
brd.cls <- alphab
unq <- unique(borders@data$Cat)
#alphabet(n= length( unq <- unique(borders@data$Cat, alpha=0.2) ) )
brd.cls <- adjustcolor(brd.cls, alpha.f=0.46)
borders@data$cols <- brd.cls[match(borders@data$Cat, unq)]




conf <- read.csv(paste0(path,"Conflict.csv"), header=TRUE)
conf$X <- NULL

# 
# if(!exists('rivers')){
#   rivers <- readOGR(dsn=path.expand(paste0(path,"./Map/largerivers.shp")), layer="largerivers")
# }
# if(!exists('zoom')){
#   zoom <- readOGR(dsn=path.expand(paste0(path,"./Map/Zoom1.shp")), layer="Zoom1")
#   
# }
# if(!exists('lakes')){
#   lakes <- readOGR(dsn=path.expand(paste0(path,"./Map/Lakes_Lagoons.shp")), layer="Lakes_Lagoons")
#   
# }
# if(!exists('ocean')){
#   ocean <-readOGR(dsn=paste0(path,"./Map"),layer="ocean2")
# }
# if(!exists('borders')){
#   borders <- shapefile(paste0(path,"./Map/borders/Borders/Borders.shp" ))
#   brd.cls <- alphabet(n= length( unq <- unique(borders@data$Cat, alpha=0.2) ) )
#   brd.cls <- adjustcolor(brd.cls, alpha.f=0.46)
#   borders@data$cols <- brd.cls[match(borders@data$Cat, unq)]
#   
# }

#xlim.krg<-c(1.27445785988605, 6.77468064779873) + c(-0.6,0.6); ylim.krg<-c(6.15110493503802, 9.69916382463531)+ c(-0.6,0.6)
#slavepaths<-sim.plot(nsim, year, sim.locs, aug, nodechains, Cities, tmat, coltab)
#dat <- slavepaths#[,1:2]
#index.shift <- dim(tmat)[1]