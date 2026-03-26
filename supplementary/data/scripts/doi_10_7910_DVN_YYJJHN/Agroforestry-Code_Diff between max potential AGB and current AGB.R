###################### READ ME ######################################

## To run this code please change the file path on you local computer

## This code plots the difference in the max potential AGB and current AGB for the different ecoregions in Senegal

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

#-----------------shapefile level 3 ----------------#

Senegal_shp_level3 <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_3.shp"

Senegal_level3 <- st_read(Senegal_shp_level3)
Senegal_level3  <- st_make_valid(Senegal_level3)

#-----------------shapefile level 1 ----------------#

Senegal_shp_level1 <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_1.shp"

Senegal_level1 <- st_read(Senegal_shp_level1)
Senegal_level1  <- st_make_valid(Senegal_level1)

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


#--------------------------------------Merge actual AGB and MAX AGB rasters----------------------------------------------#

resampled_max_AGB <- resample(max_AGB, AGBSenegal, method="ngb")
print(resampled_max_AGB)

resampled_max_AGB_df = as.data.frame(resampled_max_AGB, xy=TRUE)

comp_AGB_max_actual <- merge(resampled_max_AGB_df, AGBSenegal_df, by  = c("x","y") )


#----------------------------------calculate the difference between max AGB and current AGB ----------------#


comp_AGB_max_actual$benefit_agro = comp_AGB_max_actual$max_potential_AGB_Senegal - comp_AGB_max_actual$aboveground_biomass_carbon_2010/10

comp_AGB_max_actual <- drop_na(comp_AGB_max_actual, max_potential_AGB_Senegal) ##drop cells with na

#----------------------------------plot the map ----------------#

comp_AGB_max_actual$Category_benefit_agro<- cut(comp_AGB_max_actual$benefit_agro, breaks = c(-100.000, -10.000, -5.000, -2.5, 2.5, 5.000, 10.0000, 20.000, 90.000), 
                                                labels = c("<-10", "-10 - -5", "-5 - -2.5", "-2.5 - 2.5","2.5 - 5", "5 - 10", "10 - 20", ">20"))
category_colors <- c("<-10" = "#AE4639", "-10 - -5" = "#f18d6f", "-5 - -2.5" ="#ead4c8","-2.5 - 2.5" ="gray97",
                     "2.5 - 5" = "#77ccff" , "5 - 10" = "#3388ff", "10 - 20" = "#0066ff" , ">20" = "#0000ff")

ggplot() +
  geom_tile(data = comp_AGB_max_actual, aes(x=x, y=y, fill = Category_benefit_agro), alpha = 0.9) +
  scale_fill_manual(values = category_colors, name = "AGB (tons/ha)") +
  geom_sf(data = Senegal_level3, fill = NA, color = "gray40", linewidth = 0.3, linetype = "longdash") +
  geom_sf(data = Senegal, fill = NA, color = "black", linewidth = 0.2) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4)) +
  ggtitle("Difference in the maximum potential AGB and current AGB") +
  xlab("Longitude") + ylab("Latitude")

#----------------------------------#
