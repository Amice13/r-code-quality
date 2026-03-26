## This code plots the maximum potential AGB for the different ecoregions in Senegal
## To run this code please change the file path on you local computer
## All the files are available in the repository


setwd("/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry")

#-----------------------------------------------------------------------------------------------------------------------#

Senegal_shp <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_agroforestry/Shapefile_Senegal/gadm1_SEN_shp/gadm41_SEN_0.shp"

Senegal <- st_read(Senegal_shp)
Senegal  <- st_make_valid(Senegal)

#-----------------------------------------------------------------------------------------------------------------------#

ecoregions_path <- "/Users/yusufjameel/Dropbox/ProjectDrawdown/Agroforestry/GIS_silvopasture/carbon_zone_file.tif"
ecoregions <- raster(ecoregions_path)

ecoregions <- crop(ecoregions, extent(Senegal))
ecoregions<- mask(ecoregions, Senegal)

ecoregions_df <- as.data.frame(ecoregions, xy = TRUE)
names(ecoregions_df)[3] <- "Ecoregion"
ecoregions_df <- drop_na(ecoregions_df, Ecoregion)

#----------------------------------add a new column based on maximum carbon content------------------------------------------#


# Create a copy of the dataset to preserve the original
ecoregions_df <- ecoregions_df

# Add the new column 'Carbon_max' with default NA values
ecoregions_df$Carbon_max <- NA

# Use conditional statements to assign values to 'Carbon_max' based on 'Ecoregion'
ecoregions_df$Carbon_max[ecoregions_df$Ecoregion == 44] <- 40.407123
ecoregions_df$Carbon_max[ecoregions_df$Ecoregion == 53] <- 3.0400
ecoregions_df$Carbon_max[ecoregions_df$Ecoregion == 62] <- 15.1744
ecoregions_df$Carbon_max[ecoregions_df$Ecoregion == 113] <- 80.2464640

# Display the updated dataset
print(ecoregions_df)

#-----------------------------------------------------------------------------------------------------------------------#


ecoregions_df$Category <- cut(ecoregions_df$Carbon_max, breaks = c(0, 5, 20, 45, 85), labels = c("3", "15", "40", "80"))
category_colors <- c("3" = "#B2AC88", "15" = "lightgreen", "40"="#9ACD32","80" ="darkgreen")

ecoregions_df <- drop_na(ecoregions_df, Category)


ggplot() +
  geom_tile(data = ecoregions_df, aes(x=x, y=y, fill = Category), alpha = 0.7) +
  scale_fill_manual(values = category_colors, name = "AGB (tons/hectare)") +
  geom_sf(data = Senegal_level3, fill = NA, color = "gray40", linewidth = 0.3, linetype = "longdash") +
  geom_sf(data = Senegal, fill = NA, color = "black", linewidth = 0.2) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.4)) +
  ggtitle("Maximum potential aboveground biomass in Senegal ") +
  xlab("Longitude") + ylab("Latitdue")



#--------------------------------------------export the map as tiff -------------------------------------------------------------------#

ecoregions_df_export <- ecoregions_df %>%
  select(x, y, Carbon_max)

raster_ecoregions_df_export <- raster::rasterFromXYZ(ecoregions_df_export)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #### projection
proj4string(raster_ecoregions_df_export) = wgs.84
raster_ecoregions_df_export 

writeRaster(raster_ecoregions_df_export, filename = "max_potential_AGB_Senegal.tif", format = "GTiff", overwrite =T)

#-----------------------------------------------------------------------------------------------------------------------#



