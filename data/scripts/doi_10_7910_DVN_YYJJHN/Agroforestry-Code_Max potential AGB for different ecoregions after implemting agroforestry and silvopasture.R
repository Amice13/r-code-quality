## This code plots the maximum potential AGB for the ecoregions after implementing agroforestry or silvopasture
## To run this code please change the file path on you local computer
## All the files are available in the repository

setwd("/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry")

library(rnaturalearth)
library(raster)
#library(rgdal)
library(sf)
library(reticulate)
library(sf)
library(raster)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(patchwork)

#---------------------------------------------------------------------------------------------------------------------#

Senegal_shp <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_0.shp"

Senegal <- st_read(Senegal_shp)
Senegal  <- st_make_valid(Senegal)

#----------------------------------------------IMPORT ECOREGION--------------------------------------------------------#

ecoregions_path <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture/carbon_zone_file.tif"
ecoregions <- raster(ecoregions_path)

ecoregions <- crop(ecoregions, extent(Senegal))
ecoregions<- mask(ecoregions, Senegal)

ecoregions_resampled <- resample(ecoregions, AGBSenegal, method="ngb")
print(ecoregions_resampled)

ecoregions_df <- as.data.frame(ecoregions_resampled, xy = TRUE)
names(ecoregions_df)[3] <- "Ecoregion"
ecoregions_df <- drop_na(ecoregions_df, Ecoregion)

#----------------------------------------------IMPORT MAX AGB POTENTIAL ----------------------------------------------#

max_AGB_path <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/max_potential_AGB_Senegal.tif"
max_AGB <- raster(max_AGB_path)


max_AGB_df <- as.data.frame(max_AGB, xy = TRUE)

#-------------------------------------------------IMPORT AGB IN SENEGAL----------------------------------------------#

AGB_path <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/aboveground_biomass_carbon_2010.tif"
AGB <- raster(AGB_path)

AGBSenegal <- crop(AGB, extent(Senegal))
AGBSenegal <- mask(AGBSenegal, Senegal)

AGBSenegal_df <- as.data.frame(AGBSenegal, xy = TRUE)


#-----------------

Senegal_shp_level3 <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_3.shp"

Senegal_level3 <- st_read(Senegal_shp_level3)
Senegal_level3  <- st_make_valid(Senegal_level3)

#-----------------

Senegal_shp_level1 <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_1.shp"

Senegal_level1 <- st_read(Senegal_shp_level1)
Senegal_level1  <- st_make_valid(Senegal_level1)

#--------------------------------------Merge acutal AGB and MAX AGB data----------------------------------------------#

resampled_max_AGB <- resample(max_AGB, AGBSenegal, method="ngb")
print(resampled_max_AGB)

resampled_max_AGB_df = as.data.frame(resampled_max_AGB, xy=TRUE)

comp_AGB_max_actual <- merge(resampled_max_AGB_df, AGBSenegal_df, by  = c("x","y") )


#-------------------------------------------------MAX AGROFORESTRY POTENTIAL------------------------------------------#


max_AGB_df$max_agro <- max_AGB_df$max_potential_AGB_Senegal*0.5 

max_AGB_df$Category_agro <- cut(max_AGB_df$max_agro, breaks = c(0, 2, 10, 45, 85), labels = c("<2", "2-10", "10-40", ">40"))
category_colors <- c("<2" = "#B2AC88", "2-10" = "lightgreen", "10-40"="#9ACD32",">40" ="darkgreen")

max_AGB_df <- drop_na(max_AGB_df, Category_agro)


ggplot() +
  geom_tile(data = max_AGB_df, aes(x=x, y=y, fill = Category_agro), alpha = 0.7) +
  scale_fill_manual(values = category_colors, name = "AGB (tons/hectare)") +
  geom_sf(data = Senegal_level3, fill = NA, color = "gray40", linewidth = 0.3, linetype = "longdash") +
  geom_sf(data = Senegal, fill = NA, color = "black", linewidth = 0.2) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4)) +
  ggtitle("Maximum potential AGB - agroforestry") +
  xlab("Longitude") + ylab("Latitdue")


#-------------------------------------------------MAX SILVOPASTURE POTENTIAL------------------------------------------#


max_AGB_df$max_agro <- max_AGB_df$max_potential_AGB_Senegal*0.3 

max_AGB_df$Category_agro <- cut(max_AGB_df$max_agro, breaks = c(0, 2, 10, 45, 85), labels = c("<2", "2-10", "10-40", ">40"))
category_colors <- c("<2" = "#B2AC88", "2-10" = "lightgreen", "10-40"="#9ACD32",">40" ="darkgreen")

max_AGB_df <- drop_na(max_AGB_df, Category_agro)


ggplot() +
  geom_tile(data = max_AGB_df, aes(x=x, y=y, fill = Category_agro), alpha = 0.7) +
  scale_fill_manual(values = category_colors, name = "AGB (tons/hectare)") +
  geom_sf(data = Senegal_level3, fill = NA, color = "gray40", linewidth = 0.3, linetype = "longdash") +
  geom_sf(data = Senegal, fill = NA, color = "black", linewidth = 0.2) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4)) +
  ggtitle("Maximum potential AGB - silvopasture") +
  xlab("Longitude") + ylab("Latitdue")


##---------------------------------------------------------------------------------------------------------------------#