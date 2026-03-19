
rm(list=ls())
#install.packages(c("rgeos", "rggmap"))
library(tidyverse); library(sf); library(rgeos); library(ggthemes)
library(maptools); library(raster); library(plyr); library(rgdal);library(janitor)
library(ggmap); library(scales); library(viridis);library(tmap); library(devtools)


Kenya<-getData("GADM", country="KE", level=0)
Kenya1<-getData("GADM", country="KE", level=1)


Kenya1_UTM<-spTransform(Kenya1, CRS("+init=EPSG:32737"))  
NAME_1<-Kenya1_UTM@data$NAME_1
count<-read.csv("~/Dropbox/COVID-19/SECONDWAVE/sourcecode/Data/county_cases.csv", header=TRUE, sep=",", stringsAsFactors = T)
count_df<-data.frame(count)
Kenya1_UTM@data$id <- rownames(Kenya1_UTM@data)
Kenya1_UTM@data <- join(Kenya1_UTM@data, count_df, by="NAME_1")
Kenya1_df <- fortify(Kenya1_UTM)
Kenya1_df <- join(Kenya1_df,Kenya1_UTM@data, by="id")

plot1 <- ggplot(Kenya1_df)+ 
  geom_polygon(aes(x = long, y = lat, group = group, fill=count), color = "black", size = 0.25) +
  theme_void()+
  #coord_quickmap()+
  scale_fill_distiller(palette = "RdYlBu", direction = -1,trans = "log",na.value = "white",breaks = c(100,500,1000, 2000, 5000,10000,20000,40000), labels = c(100,500,1000, 2000, 5000,10000,20000,40000)) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10, face="bold"),
        legend.position = c(0.18, 0.18),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.spacing.y = unit(0.25, 'cm'),
        legend.text = element_text(size = 8),
        legend.title =element_text(size = 9),
        legend.box.background = element_blank())+
  guides(fill=guide_legend(ncol=1, title = "Cases"), size=T)
pdf("~/Dropbox/COVID-19/SECONDWAVE/sourcecode/Figures/Fig.1/Fig.1A.pdf", width = 3.5, height = 4.0, family = "Helvetica")
print(plot1)
dev.off()
