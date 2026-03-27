
######################. READ ME ######################################
##This code has three component.
## To run this code please change the file path on you local computer

## First, it summarizes the diverse land use types in Senegal in to 6 broad categories ("Cropland", "Grazing", "Forestry",
## "Natural-forest","Natural-shrubs", "Mixed").

## Second, the code plots the 6 broad categories using ggplot.

## Third, it exports the categories as csv and tiff file

setwd("/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture")

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

range_path <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture/current_lulc.tif"
range_cover <- raster(range_path)

##load Senegal shape file

Senegal_shp <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_0.shp"

Senegal <- st_read(Senegal_shp)
Senegal  <- st_make_valid(Senegal)


Senegal_shp_level3 <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_3.shp"

Senegal_level3 <- st_read(Senegal_shp_level3)
Senegal_level3  <- st_make_valid(Senegal_level3)



range_cover <- crop(range_cover, extent(Senegal))
range_cover <- mask(range_cover, Senegal)

plot(range_cover)

#range_cover <- projectRaster(range_cover, crs = "+proj=longlat +datum=WGS84")

range_cover_df <- as.data.frame(range_cover, xy = TRUE) ## convert to dataframe

sort(unique(range_cover_df $current_lulc))

###let's get the agriculture land


range_cover_df <- range_cover_df  %>%
  mutate(land_type = case_when(
    current_lulc %in% c(10,11,12,15,16,20,25,26,30,35,40) ~ 1,
    current_lulc %in% c(44,104,114,124,125,126,134,144,154,156, 157,184, 204, 205, 206) ~ 2,
    current_lulc %in% c(55,65,66,67,75,76,77,85,86,87,95,105,115,165,175) ~ 3,
    current_lulc %in% c(50,61,62,70,71,72,80,81,82,90,100,110,160,170) ~4,
    current_lulc %in% c(120,121,122,130,150,152,153, 180) ~5,
    current_lulc %in% c(34,35,39,49,109,119) ~ 6,
    TRUE ~ NA_integer_))

range_cover_df <- na.omit(range_cover_df)
head(range_cover_df)
#################################plot land type

range_cover_df$Category_landtype <- cut(range_cover_df$land_type, 
                                          breaks = c(0, 1, 2, 3,4,5,6), 
                                          labels = c("Cropland", "Grazing", "Forestry","Natural-forest",
                                                     "Natural-shrubs", "Mixed"))
                               
category_colors <- c("Cropland" = "#BEE5b0", "Forestry" = "#1b4332",  "Natural-forest" = "#2d6a4f",
                     "Grazing" = "#d3b683","Natural-shrubs" = "wheat",
                     "Mixed"= "yellow")


ggplot() +
  geom_tile(data = range_cover_df, aes(x=x, y=y, fill = Category_landtype)) +
  scale_fill_manual(values = category_colors, name = "Land type") +
  geom_sf(data = Senegal, fill = NA, color = "black", linetype = "dashed") +
  geom_sf(data = Senegal_level3, fill = NA, color = "gray40", linewidth = 0.2, linetype = "longdash") +
  #geom_sf(data = shp_Senegallevel1_sf, fill = NA, color = "black", linewidth = 0.4) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4)) +
  ggtitle("Land type") +
  xlab("Longitude") + ylab("Latitude")

###calculate the proportion of the different land types

land_type_summary <- range_cover_df %>%
  group_by(Category_landtype) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Print the summary
print(land_type_summary)

##Export range cover dataframe

write.csv(range_cover_df, 'landtype_df.csv')

#######Export the land cover type : where different land use types are summarised in a few higher level land type categories

range_cover_df_export_land_type <- range_cover_df %>%
  select(x, y, land_type)
raster_range_cover_land_type <- raster::rasterFromXYZ(range_cover_df_export_land_type)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #### projection
proj4string(raster_range_cover_land_type) = wgs.84
raster_range_cover_land_type

writeRaster(raster_range_cover_land_type, filename = "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture/land_type_Senegal.tif", 
            format = "GTiff", overwrite = T)


####Export the raw lulc type


range_cover_df_export_lulc <- range_cover_df %>%
  select(x, y, current_lulc)
raster_range_cover_lulc  <- raster::rasterFromXYZ(range_cover_df_export_lulc)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #### projection
proj4string(raster_range_cover_lulc ) = wgs.84
raster_range_cover_lulc  

writeRaster(raster_range_cover_lulc , filename = "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture/lulc_Senegal.tif", 
            format = "GTiff", overwrite = T)
