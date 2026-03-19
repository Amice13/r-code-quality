rm(list = ls())
library(data.table)
library(ggmap)
library(googleway)
library(ggplot2)
library(ggthemes)
library(here)
library(gridExtra)

homicides <- readRDS(here("data","map_data_mogi.rds"))
homicides = data.table(homicides)

make_circles <- function(centers, radius, nPoints = 100){
  # centers: the data frame of centers with ID
  # radius: radius measured in kilometer
  #
  meanLat <- mean(centers$latitude)
  # length per longitude changes with latitude, so need correction
  radiusLon <- radius /111 / cos(meanLat/57.3) 
  radiusLat <- radius / 111
  circleDF <- data.frame(ID = rep(centers$ID, each = nPoints))
  angle <- seq(0,2*pi,length.out = nPoints)
  
  circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
  circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
  return(circleDF)
}

#these are the polling stations i picked as examples

datapoints <- data.frame(ID = c('Faustino','Jundiapeba'), 
                   longitude = c(-46.17144,-46.25664),
                   latitude = c(-23.52267,-23.54658))  

myCircles <- make_circles(datapoints, 1)

register_google('[ENTER YOUR KEY HERE]')

mogi = get_map("Vila Rubens, MogiDasCruzes, SP, Brazil", zoom=13, maptype = 'toner')

map_mogi_after = ggmap(mogi) + 
  geom_point(data=homicides[ANO_BO>=2013 & ANO_BO<=2015], aes(x=lon, y=lat), color="red", size=1, alpha=1,shape=4) +
  geom_point(data=datapoints, aes(y=latitude, x=longitude, group=ID), color="blue", size=1, alpha=.75) +
  theme_tufte() +
  geom_polygon(data = myCircles, aes(lon, lat, group = ID), color = "orange", alpha = .25) +
  ggtitle('B: 2013 - 2015 period') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size = 10))

map_mogi_before = ggmap(mogi) + 
  geom_point(data=homicides[ANO_BO>=2009 & ANO_BO<=2011], aes(x=lon, y=lat), color="red", size=1, alpha=1,shape=4) +
  geom_point(data=datapoints, aes(y=latitude, x=longitude,group=ID), color="blue", size=1, alpha=.75) +
  theme_tufte() +
  geom_polygon(data = myCircles, aes(lon, lat, group = ID), color = "orange", alpha = .25) +
  ggtitle('A: 2009 - 2011 period') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size = 10))


#grid.arrange(map_mogi_before, map_mogi_after,nrow=1,top = "Murders and voting stations in Mogi das Cruzes")
g <- arrangeGrob(map_mogi_before, map_mogi_after, nrow=1)
ggsave(here("writing","img","fig_8.pdf"), g)

