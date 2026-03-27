### Replication code for Arriola, Choi, Davis, Phillips, and Rakner:
### Policymakers' Abortion Preferences: Understanding the Intersection of Gender and Wealth
### Accepted for Publication at Comparative Political Studies

### The following code reproduces Figure 1.
### To run the code, save the four files below under a folder labeled "shapefile"
### in the workding directory and execute the code.
### world-administrative-boundaries.dbf
### world-administrative-boundaries.prj
### world-administrative-boundaries.shp
### world-administrative-boundaries.shx

rm(list=ls())                    # Clears the memory.

library(tidyverse)
library(broom)
library(rjson)
library(rgdal)
library(haven)
library(readxl)
library(sf)
library(terra)
library(plyr)


ogrListLayers("shapefile/world-administrative-boundaries.shp") #will show you available layers for the above dataset
abortion <- readOGR("shapefile/world-administrative-boundaries.shp", layer="world-administrative-boundaries") #will load the shapefile to your dataset.
proj4string(abortion)
abortionmap <- abortion
abortionmap@data$id <- rownames(abortion@data)
abortion.points <- fortify(abortionmap, region="id")
abortionmap.df <- join(abortion.points, abortionmap@data, by="id")

map <- ggplot() +
  geom_polygon(data = abortionmap.df,
               aes(x = long, y = lat, group = group),
               color = 'grey', fill = 'white', size = 0.2, alpha=0.5) +
  theme_minimal()


# read in abortion data
crrdat <- read_excel("Abortion_Global.xlsx")

crrdat$catcodealt <- NA
crrdat$catcodealt <- ifelse(crrdat$catcode==1, 1, crrdat$catcodealt)
crrdat$catcodealt <- ifelse(crrdat$catcode>=2 & crrdat$catcode<=4, 2, crrdat$catcodealt)
crrdat$catcodealt <- ifelse(crrdat$catcode==5, 3, crrdat$catcodealt)
crrdat$catcodealt <- ifelse(crrdat$catcode==6, 2, crrdat$catcodealt)


abortionmap.df <- dplyr::left_join(abortionmap.df, crrdat, by=c("name"="country"))

ggplot() +
  geom_polygon(data = abortionmap.df,
               aes(x = long, y = lat, group = group, fill=factor(catcodealt)),
               color = 'grey', size = 0.1, alpha=0.8) +
  scale_fill_manual(name="Categories of Abortion Laws",
                    values=c("red", "yellow", "dodgerblue3",
                    "black"),
                    labels=c("Prohibited",
                             "Various Restrictions",
                             "On request",
                             "No data")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow=2, byrow=T)) +
  labs(title="The World's Abortion Laws",
       subtitle="Source: Center for Reproductive Rights (2022)")

ggsave("abortionmap_alt.pdf", height=6, width=10)
