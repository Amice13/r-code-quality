rm(list=ls())

install.packages(c("terra", "tmap", "tmaptools", "sf", "leaflet", "haven", "sp", "raster"))

library(terra)
library(tmap)
library(tmaptools)
library(sf)
library(leaflet)
library(haven)
library(sp)
library(raster)

wbluescale <- c("#ffffff", "#08519c")
wgrayscale <- c("#ffffff", "#AAAAAA")
wpurpscale <- c("#ffffff", "#9e9ac8")
wredscale <- c("#ffffff", "#de2d26")
wgreenscale <- c("#ffffff", "#2e8b57")
wbluescale6 <- c("#ffffff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

setwd("/Users/cb2257/Desktop/AJPS Replication/")
#setwd("/Users/christopherblair/Desktop/AJPS Replication/")

afg <- st_read("shapefile/afghan_districts/afghan_districts.shp")

st_is_valid(afg, reason = TRUE)
afg <- st_make_valid(afg)

district <- read_dta(file="data/final/map_descriptives.dta")

joined <- merge(afg, district, by.x="DISTID", by.y="distid")

# Map for Trafficking Dynamics

smuggling<-joined[ which(joined$qtyr== 199), ]

smuggling <- tm_shape(smuggling) +
  tm_borders()+
  tm_fill("drugmarket", title = "Trafficking Node", palette=wgreenscale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Historical Trafficking Hubs", title.size=1, frame = F)

smuggling
tmap_save(smuggling, "figures/FigureC4.png")

# Map for Q4 2009

district09Q4<-joined[ which(joined$qtyr== 199), ]

forts09Q4 <- tm_shape(district09Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2009)", title.size=1, frame = F)

forts09Q4
tmap_save(forts09Q4, "figures/Figure1_PanelA.png")

# Map for Q4 2010

district10Q4<-joined[ which(joined$qtyr== 203), ]

forts10Q4 <- tm_shape(district10Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2010)", title.size=1, frame = F)

forts10Q4
tmap_save(forts10Q4, "figures/Figure1_PanelB.png")

# Map for Q4 2011

district11Q4<-joined[ which(joined$qtyr== 207), ]

forts11Q4 <- tm_shape(district11Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2011)", title.size=1, frame = F)

forts11Q4
tmap_save(forts11Q4, "figures/Figure1_PanelC.png")

# Map for Q4 2012

district12Q4<-joined[ which(joined$qtyr== 211), ]

forts12Q4 <- tm_shape(district12Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2012)", title.size=1, frame = F)

forts12Q4
tmap_save(forts12Q4, "figures/Figure1_PanelD.png")

# Map for Q4 2013

district13Q4<-joined[ which(joined$qtyr== 215), ]

forts13Q4 <- tm_shape(district13Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2013)", title.size=1, frame = F)

forts13Q4
tmap_save(forts13Q4, "figures/Figure1_PanelE.png")

# Map for Q4 2014

district14Q4<-joined[ which(joined$qtyr== 215), ]

forts14Q4 <- tm_shape(district14Q4) +
  tm_borders()+
  tm_fill("borderfort55", title = "Border Fortification", palette=wbluescale, style="fixed",
          breaks=c(0, 1, 2), interval.closure="left", 
          labels=c("No", "Yes"))+
  tm_layout("Border Fortification \n(2014)", title.size=1, frame = F)

forts14Q4
tmap_save(forts14Q4, "figures/Figure1_PanelF.png")

numforts14Q4 <- tm_shape(district14Q4) +
  tm_borders()+
  tm_fill("num_borderfort55", title = "# of Border Forts", palette=wbluescale6, style="fixed",
          breaks=c(0, 1, 2, 3, 4, 5, 9), interval.closure="left", 
          labels=c("0", "1", "2", "3", "4", "5-8"))+
  tm_layout("# of Border Forts \n(2014)", title.size=1, frame = F)

numforts14Q4
tmap_save(numforts14Q4, "figures/FigureC5.png")

empstrategy <- tm_shape(district14Q4) +
  tm_borders()+
  tm_fill("empdesign", title = "Empirical Strategy", palette=wbluescale6, style="fixed",
          breaks=c(0, 1, 2, 3), interval.closure="left", 
          labels=c("N/A", "Control", "Treated"))+
  tm_layout("Treatment Status", title.size=1, frame = F)

empstrategy
tmap_save(empstrategy, "figures/empstrategy.png")

totsigacts <- tm_shape(district14Q4) +
  tm_borders()+
  tm_fill("totalsigacts", title = "# of SIGACTs", palette=wbluescale6, style="fixed",
          breaks=c(0, 75, 174, 564, 1402, 3008, 30000), interval.closure="left", 
          labels=c("0-74", "75-173", "174-563", "564-1401", "1402-3007", "3008-28745"))+
  tm_layout("Significant Activities (SIGACTs)", title.size=1, frame = F)

totsigacts
tmap_save(totsigacts, "figures/FigureA2_LeftPanel.png")

totresp <- tm_shape(district14Q4) +
  tm_borders()+
  tm_fill("totalresp", title = "# of Respondents", palette=wredscale, style="fixed",
          breaks=c(0, 178, 361, 621, 1090, 1886, 36000), interval.closure="left", 
          labels=c("16-177", "178-360", "361-620", "621-1089", "1090-1885", "1886-35923"))+
  tm_layout("ANQAR Respondents", title.size=1, frame = F)

totresp
tmap_save(totresp, "figures/FigureA2_RightPanel.png")
