library(ggplot2)
library(dplyr)
library(maps)
library(usmap)
library(stringr)
library(readr)
states_lookup = us_map(region = "states")

,breaks = c(0.2,0.4,0.6,0.8)


## average electricity price by state in 1976
st_prices <- read.csv("C:/Users/User/Dropbox/work_in_progress/carbon_lockin/dataR/state_price_data.csv", header = TRUE, stringsAsFactors = FALSE)
st_map<-inner_join(states_lookup,st_prices,by="abbr")

ggplot(st_map, aes(x=x, y=y, group = group)) +
  geom_polygon(data = st_map, mapping = aes(x, y, group = group),
               fill = NA) +
  geom_polygon( aes(fill=elec_price)) +
  scale_fill_gradient(name = "Avg Price", low = "#9ecae1", high = "#13134D") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )


## average shares in 1976
energy_sh <- read.csv("C:/Users/User/Dropbox/work_in_progress/carbon_lockin/dataSTATA/seds/shares1976.csv", header = TRUE, stringsAsFactors = FALSE)
energy_st<-inner_join(states_lookup,energy_sh,by="abbr")

## Coal
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
 geom_polygon( aes(fill=L0elecShareB_cl)) +
# scale_fill_gradient(name = "Coal Share", low = "#B0C4DE", high = "#003366",breaks = c(0.2,0.4,0.6,0.8)) + 
  scale_fill_gradient(name = "Coal Share", low = "#ffffbf", high = "#2c7bb6",breaks = c(0.2,0.4,0.6,0.8)) + 
  
# scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +
# scale_fill_gradientn(name = "Coal Share",colors= c("#6baed6", "#4292c6", "#2171b5", "navy"))+
# ,
#    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
#  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )

## Natural Gas Share
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
  geom_polygon( aes(fill=L0elecShareB_ng)) +
  # scale_fill_gradient(name = "Natural Gas\nShare", low = "light salmon", high = "dark red", breaks = c(0.2,0.4,0.6,0.8)) + 
  scale_fill_gradient(name = "Natural\nGas Share", low = "#ffffbf", high = "#d7191c", breaks = c(0.2,0.4,0.6,0.8)) + 
  
  # scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +
  # scale_fill_gradientn(name = "Coal Share",colors= c("#FFE5CC","light salmon", "darksalmon", "indianred","dark red"))+
  # ,
  #    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
  #  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.94, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )

## Petroleum Oil Share
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
  geom_polygon( aes(fill=L0elecShareB_pa)) +
  #scale_fill_gradient(name = "Petroleum Oil\nShare", low = "yellow green", high = "dark green",breaks = c(0.2,0.4,0.6,0.8)) + 
  scale_fill_gradient(name = "Petroleum\nOil Share", low = "#ffffbf", high = "#1a9641",breaks = c(0.2,0.4,0.6,0.8)) + 
  
  # scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +

  #scale_fill_gradientn(name = "Petroleum Oil\nShare",colors= c("#FFFF66","green yellow", "yellow green", "forest green","dark green"),breaks = c(0.2,0.4,0.6,0.8))+
  # ,
  #    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
  #  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )


## change relative to 1976
energy_delta <- read.csv("C:/Users/KATHERENE/Dropbox/work_in_progress/carbon_lockin/dataSTATA/seds/changeinshares.csv", header = TRUE, stringsAsFactors = FALSE)
energy_st<-inner_join(states_lookup,energy_delta,by="abbr")

## Coal
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
  geom_polygon( aes(fill=delta_coal)) +
  scale_fill_gradient2(name = "Change in\nCoal Share", midpoint = 0, mid = "#ffffbf", low = "#2c7666", high = "#d7191c",breaks=c(-0.50,-0.25,0, 0.25,0.50)) + 
  # scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +
  # scale_fill_gradientn(name = "Coal Share",colors= c("#6baed6", "#4292c6", "#2171b5", "navy"))+
  # ,
  #    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
  #  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )

## Natural Gas Share
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
  geom_polygon( aes(fill=delta_ng)) +
  scale_fill_gradient2(name = "Change in\nNatural\nGas Share", midpoint = 0, mid = "#ffffbf", low = "#2c7666", high = "#d7191c",breaks=c(-0.50,-0.25,0, 0.25,0.50,0.75)) + 
  # scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +
  # scale_fill_gradientn(name = "Coal Share",colors= c("#6baed6", "#4292c6", "#2171b5", "navy"))+
  # ,
  #    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
  #  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )

## Petroleum Oil Share
ggplot(energy_st, aes(x=long, y=lat, group = group)) +
  geom_polygon(data = energy_st, mapping = aes(long, lat, group = group),
               fill = NA) +
  geom_polygon( aes(fill=delta_pa)) +
  scale_fill_gradient2(name = "Change in\nPetroleum\nOil Share", midpoint = 0, mid = "#ffffbf", low = "#2c7666", high = "#d7191c",breaks=c(-0.75,-0.50,-0.25,0, 0.25,0.50,0.75)) + 
  # scale_fill_gradient(name = "Coal Share", low="#6baed6", high= "#084594") +
  # scale_fill_gradientn(name = "Coal Share",colors= c("#6baed6", "#4292c6", "#2171b5", "navy"))+
  # ,
  #    values = c("#6baed6", "#4292c6", "#2171b5", "#084594"),
  #  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        #legend.position = "right" #enabling this will put this to the bottom
        legend.position = c(0.93, 0.2),
        #legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.8, 'cm'), #change legend key height
        #legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.title = element_text(size=16), #change legend title font size
        legend.text = element_text(size=12) #change legend text font size
  )