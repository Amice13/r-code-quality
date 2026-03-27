#maps of colombian armed group origins

date()

#load data
library(haven)
library(car)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plyr)


types2 <- read_stata("violence_t_export.dta")
all_muni <- read_stata("code_muni_cede.dta")


#format grouped yearly data

types2 <- types2[c("muni_code","time","action_farc2","action_paras2")]
types2_9801 <- subset(types2, time==1) 
types2_0205 <- subset(types2, time==2)

colnames(types2_9801) <- c("muni_code","year","action_farct1","action_parast1")
colnames(types2_0205) <- c("muni_code","year","action_farct2","action_parast2")

types2_9801 <- types2_9801[,-2]
types2_0205 <- types2_0205[,-2]

types2_9801$muni_code <- as.integer(types2_9801$muni_code)
types2_0205$muni_code <- as.integer(types2_0205$muni_code)

types2_9801$action_farct1 <- as.factor(types2_9801$action_farct1)
types2_9801$action_parast1 <- as.factor(types2_9801$action_parast1)

types2_0205$action_farct2 <- as.factor(types2_0205$action_farct2)
types2_0205$action_parast2 <- as.factor(types2_0205$action_parast2)

levels(types2_9801$action_farct1) <- c("No Victimization","Selective","Non-Selective")
levels(types2_9801$action_parast1) <- c("No Victimization","Selective","Non-Selective")

levels(types2_0205$action_farct2) <- c("No Victimization","Selective","Non-Selective")
levels(types2_0205$action_parast2) <- c("No Victimization","Selective","Non-Selective")

#add a binary victimization/no victimization coding

types2_9801$action_farct1_dummy <- as.factor(ifelse(types2_9801$action_farct1 %in% c("Selective","Non-Selective"),1,0))
types2_9801$action_parast1_dummy <- as.factor(ifelse(types2_9801$action_parast1 %in% c("Selective","Non-Selective"),1,0))

types2_0205$action_farct2_dummy <- as.factor(ifelse(types2_0205$action_farct2 %in% c("Selective","Non-Selective"),1,0))
types2_0205$action_parast2_dummy <- as.factor(ifelse(types2_0205$action_parast2 %in% c("Selective","Non-Selective"),1,0))

levels(types2_9801$action_farct1_dummy) <- c("No Victimization","Victimization")
levels(types2_9801$action_parast1_dummy) <- c("No Victimization","Victimization")

levels(types2_0205$action_farct2_dummy) <- c("No Victimization","Victimization")
levels(types2_0205$action_parast2_dummy) <- c("No Victimization","Victimization")



#3 maps: grouped yearly data####

#a prepare data####

#load shape file and fortify
colom.shape3 <- readOGR(dsn="mpio.shp", layer =  "mpio")
colom.map.data2 <- fortify(colom.shape3, region="MPIOS")

colom.shape4 <- colom.shape3@data

#get latlong for centroid of each municipality
#(https://stackoverflow.com/questions/16462290/obtaining-latitude-and-longitude-with-from-spatial-objects-in-r)

centroids <- as.data.frame(coordinates(colom.shape3))
colnames(centroids) <- c("cent_long","cent_lat")

colom.shape4 <- cbind(colom.shape4, centroids)

#format for merging
colnames(colom.map.data2)[colnames(colom.map.data2) == "id"] <- "muni_code"
colom.map.data2$muni_code <- as.integer(colom.map.data2$muni_code)

colnames(colom.shape4)[colnames(colom.shape4) == "MPIOS"] <- "muni_code"
colom.shape4$muni_code <- as.integer(as.character(colom.shape4$muni_code))
colom.shape4 <- list(colom.shape4, all_muni, types2_9801, types2_0205) %>% reduce(left_join, by = "muni_code")

#merge with shapefile
map.data2 <- merge(colom.map.data2, colom.shape4, by = "muni_code", all.x = TRUE)

#remove islands
map.data2 <- subset(map.data2, !(map.data2$NOMBRE_DPT %in% c("VICHADA","VAUPES","GUAINIA","AMAZONAS","ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA")))

map.data2$test <- as.factor(1)

#b countrywide maps####


#period 2 (2002-2005)


#(b) dichotomous action coding

map11b <- ggplot() +  
  geom_polygon(data = map.data2, aes(x=long,y=lat,group=group,fill=test),alpha = 0.4, colour="grey75")
map11b <- map11b +  geom_point(data = subset(map.data2, !is.na(action_parast2_dummy)), 
                               aes(x=cent_long, y=cent_lat, shape=action_parast2_dummy,color="Paramilitaries"),size=2)
map11b <- map11b +  geom_point(data = subset(map.data2, !is.na(action_farct2_dummy)), 
                               aes(x=cent_long, y=cent_lat, shape=action_farct2_dummy,color="FARC"),size=2,
                               position=position_nudge(x = 13000, y = 0)) + 
  scale_shape_manual(values=c(3, 16)) +
  scale_color_manual(values=c("black","grey60"))
map11b <- map11b + coord_equal()
map11b <- map11b + theme(axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         axis.line.x = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         plot.background = element_blank())
map11b <- map11b + scale_fill_manual(values=c("white")) + theme(legend.position="bottom", legend.title=element_blank(),
                                                                 legend.text=element_text(size=18)) + guides(fill="none")
ggsave("figurea1-b.png", plot = map11b, width = 10, height = 9)

#c antioquia only####

map.data_ant2 <- subset(map.data2, map.data2$NOMBRE_DPT == "ANTIOQUIA")


#period 2 (2002-2005)

#(b) dichotomous action coding


map.ant8b <- ggplot() +  
  geom_polygon(data = map.data_ant2, aes(x=long,y=lat,group=group,fill=test),alpha = 0.4, colour="grey75")
map.ant8b <- map.ant8b +  geom_point(data = subset(map.data_ant2, !is.na(action_parast2_dummy)), 
                                     aes(x=cent_long, y=cent_lat, shape=action_parast2_dummy,color="Paramilitaries"),size=2)
map.ant8b <- map.ant8b +  geom_point(data = subset(map.data_ant2, !is.na(action_farct2_dummy)), 
                                     aes(x=cent_long, y=cent_lat, shape=action_farct2_dummy,color="FARC"),size=2,
                                     position=position_nudge(x = 6000, y = 0)) + 
  scale_shape_manual(values=c(3, 16, 20)) +
  scale_color_manual(values=c("black","grey55"))
map.ant8b <- map.ant8b + coord_equal()
map.ant8b <- map.ant8b + theme(axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank(),
                               axis.title.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.line.x = element_blank(),
                               panel.background = element_blank(),
                               panel.border = element_blank(),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               plot.background = element_blank())
map.ant8b <- map.ant8b + scale_fill_manual(values=c("white")) + theme(legend.position="bottom", legend.title=element_blank(),
                                                                       legend.text=element_text(size=18)) + guides(fill="none")
ggsave("figurea1-a.png", plot = map.ant8b, width = 10, height = 9)


sessionInfo()
date()
