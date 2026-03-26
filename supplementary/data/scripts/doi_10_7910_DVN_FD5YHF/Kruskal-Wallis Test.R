
#====NOTE: ALL DIRECTORIES, FILE NAMES AND VARIABLES SHOULD BE CHANGED TO MATCH YOUR FILES


#============== Load required libraries
library(terra)       # for raster and spatial data
library(sf)          # for shapefiles and spatial vector data
library(dplyr)       # for data manipulation
library(FSA)         # for post-hoc tests like Dunn's
library(ggplot2)     # for plotting

#============== Set file paths

diversity_shapefile_path <- "C:/Users/USER/Desktop/END-Soil data analysis/Shapefiles/New_data_mlr_projected.shp"

#================ Read the raster and vector data
soil_raster <- rast(soil_raster_path)  # raster from terra
diversity_sf <- st_read(diversity_shapefile_path)  # shapefile as sf object

#======== Extract soil class (raster values) to the points/polygons
# If diversity_sf is points:
diversity_sf$soil_class <- terra::extract(soil_raster, vect(diversity_sf))[,2]
diversity_sf
print(diversity_sf)

#================= Clean: remove NAs if any
diversity_clean <- diversity_sf %>%
  filter(!is.na(soil_class)) %>%
  st_drop_geometry()  # remove geometry for stats

#================= View data structure
head(diversity_clean)
print(diversity_clean)


#================ Perform Kruskal-Wallis test - CANOPY COVER
kruskal_result <- kruskal.test(Canopy_Cov ~ as.factor(soil_class), data = diversity_clean)
print(kruskal_result)


#================= Convert numeric soil_class to factor with labels
diversity_clean$soil_class <- factor(diversity_clean$soil_class,
                                     levels = c(1, 2, 3, 4, 5),         # adjust to your actual class codes
                                     labels = c("Very High", "High", "Moderate", "Low", "Very Low"))  # custom names


############################################################################
windowsFonts("Times New Roman" = windowsFont("TT Times New Roman"))

#============kruskal_result <- kruskal.test(Shade_Tree ~ soil_class, data = diversity_clean)
p_val <- format.pval(kruskal_result$p.value, digits = 2, eps = .01)

#========  PLOT
p1 <- ggplot(diversity_clean, aes(x = soil_class, y =  Canopy_Cov, fill = soil_class)) +
  geom_boxplot(
    width = 0.4,
    color = "black"
  ) +
  scale_fill_manual(values = c(
    "Very High" = "#D3D3D3",
    "High" = "#008080",
    "Moderate" = "#99FF99",
    "Low" = "#FFC0CB",
    "Very Low" = "#87CEEB"
  )) +
  scale_x_discrete(expand = c(0.1, 0)) +
  labs(
    x = "SQI Class",
    y = "Canopy Cover (%)"
    # Removed caption
  ) +
  # Add p-value annotation inside plot
  annotate("text",
           x = 0.7,
           y = max(diversity_clean$Canopy_Cov, na.rm = TRUE),
           label = paste0("Kruskal-Wallis p = ", p_val),
           hjust = 0,
           vjust = 0.05,
           family = "Times New Roman",
           size = 4) +
  theme_minimal(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

# Save the final plot
ggsave("C:/Users/USER/Desktop/The final analysis/Maps/boxplot-cc3.png", width = 4, height = 4, dpi = 2000)
#########################################################################

colnames(diversity_clean)

# If significant, perform post-hoc Dunn's test
#dunn_result <- dunnTest(Canopy_Cov ~ as.factor(soil_class), data = diversity_clean, method = "bonferroni")
#print(dunn_result)


###################################################################################
#========== Kruskal Wallis Test -  Shade tree diversity

# Load required libraries
library(terra)       # for raster and spatial data
library(sf)          # for shapefiles and spatial vector data
library(dplyr)       # for data manipulation
library(FSA)         # for post-hoc tests like Dunn's
library(ggplot2)     # for plotting

#=========== Set file paths
soil_raster_path <- "C:/Users/USER/Desktop/The final analysis/Transformed Rasters/Final_SQI_Map.tif"
#===========soil_raster_path <- "C:/Users/USER/Desktop/Soil data analysis/Top/Top_rasters/SQI_top.tif"
diversity_shapefile_path <- "C:/Users/USER/Desktop/END-Soil data analysis/Shapefiles/New_data_mlr_projected.shp"

#=========== Read the raster and vector data
soil_raster <- rast(soil_raster_path)  # raster from terra
diversity_sf <- st_read(diversity_shapefile_path)  # shapefile as sf object

# Extract soil class (raster values) to the points/polygons
# If diversity_sf is points:
diversity_sf$soil_class <- terra::extract(soil_raster, vect(diversity_sf))[,2]

print(diversity_sf)

# Clean: remove NAs if any
diversity_clean <- diversity_sf %>%
  filter(!is.na(soil_class)) %>%
  st_drop_geometry()  # remove geometry for stats

# View data structure
head(diversity_clean)
print(diversity_clean)


# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(Shade_Tree ~ as.factor(soil_class), data = diversity_clean)
print(kruskal_result)

########################################################################################
# Example: suppose the numeric classes are 1, 2, and 3
# and you want to name them "Low", "Medium", and "High"

# Convert numeric soil_class to factor with labels
diversity_clean$soil_class <- factor(diversity_clean$soil_class,
                                     levels = c(1, 2, 3, 4, 5),         # adjust to your actual class codes
                                     labels = c("Very High", "High", "Moderate", "Low", "Very Low"))  # custom names


####################################################################################
####################################################################################
windowsFonts("Times New Roman" = windowsFont("TT Times New Roman"))

#kruskal_result <- kruskal.test(Shade_Tree ~ soil_class, data = diversity_clean)
p_val <- format.pval(kruskal_result$p.value, digits = 2, eps = .01)


p2 <- ggplot(diversity_clean, aes(x = soil_class, y =  Shade_Tree, fill = soil_class)) +
    geom_boxplot(
    width = 0.4,
    color = "black"
  ) +
  scale_fill_manual(values = c(
    "Very High" = "#D3D3D3",
    "High" = "#008080",
    "Moderate" = "#99FF99",
    "Low" = "#FFC0CB",
    "Very Low" = "#87CEEB"
  )) +
  scale_x_discrete(expand = c(0.1, 0)) +
  labs(
    x = "SQI Class",
    y = "Shade Tree Diversity"
    # Removed caption
  ) +
  # Add p-value annotation inside plot
  annotate("text",
           x = 0.7,
           y = max(diversity_clean$Shade_Tree, na.rm = TRUE),
           label = paste0("Kruskal-Wallis p = ", p_val),
           hjust = 0,
           vjust = 0.05,
           family = "Times New Roman",
           size = 4) +
  theme_minimal(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

# Save the final plot
ggsave("C:/Users/USER/Desktop/The final analysis/Maps/boxplot-sd.png", width = 4, height = 4, dpi = 3000)
#########################################################################



############## DOT PLOT
# --- Install necessary packages (if not already installed) ---
#install.packages(c("readxl", "ggplot2", "dplyr"))

# --- Load packages ---
library(readxl)
library(ggplot2)
library(dplyr)

# --- Read Excel file ---
# Replace with your actual file name and path
data <- read_excel("C:/Users/USER/Desktop/The final analysis/dot plot data.xlsx")

# --- Treat SoilClass as a factor (unordered) ---
# If you want a custom order, modify the levels below

# Example: c("Very Low", "Low", "Medium", "High", "Very High")
data$`SQI Class` <- factor(data$`SQI Class`)
#data$SoilClass <- factor(data$SoilClass)

# --- OPTIONAL: custom display order (if you care about axis layout) ---
data$`SQI Class` <- factor(data$`SQI Class`, levels = c("Very High", "High", "Moderate", "Low", "Very Low"))


# ========== 1. Dot Plot of Individual Values ==========
#ggplot(data, aes(x = `SQI Class`, y = `Species Richness`)) +
#  geom_jitter(width = 0.2, size = 2.5, alpha = 0.7, color = "#0072B2") +
#    x = "Soil Class",
#    y = "Species Richness") +
#  theme_minimal()

colnames(data)
############################################################################
#####################################################################

windowsFonts("Times New Roman" = windowsFont("TT Times New Roman"))

# Ensure Plot Number is a character (optional now)
data$`Plot Number` <- as.character(data$`Plot Number`)

# Create the customized dot plot (no plot number labels)
p3 <- ggplot(data, aes(x = `SQI Class`, y = `Total Abundance`)) +
  
  # Points with custom color
  geom_jitter(width = 0, size = 3, alpha = 0.8, color = "darkgreen") +
  
  # Titles and axis labels
  labs(
    x = "SQI Class",
    y = "Total Abundance"
  ) +
  
  # Minimal theme with Times New Roman and visible gridlines/axes
  theme_minimal(base_family = "Times New Roman") +
  theme(
    # Font styling (not bold)
    plot.title = element_text(size = 14, face = "plain", color = "black", hjust = 0.5),
    axis.title = element_text(size = 12, face = "plain", color = "black"),
    axis.text = element_text(size = 10, face = "plain", color = "black"),
    
    # Axis lines (visible)
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    
    # Restore major grid lines
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    
    # Keep plot border off
    panel.border = element_blank()
  )

################################################################################
#=============  Combining plots

# Load patchwork (only once at the top of your script)
library(patchwork)

# Combine the three plots vertically
final_plot <- p1 / p2 / p3

# One below
final_plot <- (p1 | p2) / p3final_plot <- (p1 | p2) / p3

# Show the combined plot
print(final_plot)



#######################################################
#========  OTHER ORIENTATIONS FOR THE PLOTS ARE AS FOLLOWS


final_plot <- (p1 | p2) / (p3 + plot_spacer()) + 
  plot_layout(heights = c(1, 1), widths = c(1, 1))

#=======================
final_plot <- (p1 | p2) / (plot_spacer() | p3) + 
  plot_layout(heights = c(1, 1), widths = c(1, 1))
#=======================

final_plot <- (p1 | p2) / (p3 | plot_spacer()) + 
  plot_layout(heights = c(1, 1), widths = c(1, 1))
#=======================
final_plot <- (p1 | p2) / (plot_spacer() | p3 | plot_spacer()) + 
  plot_layout(heights = c(1, 1), widths = c(1, 1, 1))
#=======================
final_plot <- (p1 | p2) / (plot_spacer() | p3 | plot_spacer()) + 
  plot_layout(heights = c(1, 1), widths = c(0.1, 0.8, 0.1))
#=======================
final_plot <- (p1 | p2) / ((plot_spacer() + p3) | plot_spacer()) + 
  plot_layout(heights = c(1, 1), widths = c(0.2, 0.7))

print(final_plot)


# Save the combined plot as a high-res image
ggsave("C:/Users/USER/Desktop/The final analysis/Maps/combined-plot13.png",
       plot = final_plot, width = 12, height = 8, dpi = 600)


