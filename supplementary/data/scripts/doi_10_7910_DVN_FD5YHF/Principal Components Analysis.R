#====NOTE: ALL DIRECTORIES, FILE NAMES AND VARIABLES SHOULD BE CHANGED TO MATCH YOUR FILES


#================ Load required libraries
library(readxl)
library(terra)
library(vegan)
library(dplyr)
library(nnet)
library(reshape2)
library(ggplot2)
library(factoextra)  # For PCA plots

#=============== Load your Excel file
raw_data <- read_excel("C:/Users/USER/Desktop/The final analysis/abundance and var -2.xlsx")
raw_data <- as.data.frame(raw_data)
rownames(raw_data) <- raw_data$`Grid No`

#============== Extract species data (adjust column names as needed)
species_cols <- which(names(raw_data) == "P. angolensis"):which(names(raw_data) == "T. ivorensis")
species_data <- raw_data[, species_cols]
env_data <- raw_data[, -species_cols]

#============== Load SQI raster layers
sqi_top <- rast("C:/Users/USER/Desktop/The final analysis/Transformed Rasters/Final_SQI_Map.tif")


#============== Convert to spatial vector and reproject
plot_points <- vect(env_data, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")
plot_points_utm <- project(plot_points, crs(sqi_top))

#============== Extract SQI values
env_data$SQI_top <- terra::extract(sqi_top, plot_points_utm)[, 2]

#============== Remove rows with missing values in any SQI
complete_rows <- complete.cases(env_data[, c("SQI_top")])
env_data <- env_data[complete_rows, ]
species_data <- species_data[complete_rows, ]

#============== Convert SQI columns to factors for categorical grouping and rename levels
env_data$SQI_top <- factor(env_data$SQI_top, 
                           levels = c("1", "2", "3", "4", "5"), 
                           labels = c("Very High", "High", "Moderate", "Low", "Very Low"))


#============== PCA for Top Soil Layer
pca_top <- prcomp(species_data, scale. = TRUE)
summary(pca_top)


windowsFonts(Times = windowsFont("Times New Roman"))

p <- fviz_pca_biplot(
  pca_top, 
  habillage = env_data$SQI_top, 
  addEllipses = FALSE,
  label = "var",
  repel = TRUE,
  title = NULL,
  labelsize = 9
) +
  geom_point(aes(color = env_data$SQI_top, shape = env_data$SQI_top), size = 5) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) +
  scale_color_manual(values = c("red", "green", "gold", "purple", "blue")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA),
    legend.title = element_text(size = 28, face = "bold", family = "Times"),
    legend.text = element_text(size = 28, family = "Times"),
    axis.title = element_text(size = 28, face = "bold", family = "Times"),
    axis.text = element_text(size = 28, family = "Times"),
    legend.position = c(0.2, 0),
    legend.justification = c(1, 0)
  ) +
  labs(
    color = "SQI Class",
    shape = "SQI Class",
    x = "PC1 (15.7%)",
    y = "PC2 (15.4%)"
  )

# Set variable labels to italic
for (i in seq_along(p$layers)) {
  if (inherits(p$layers[[i]]$geom, "GeomTextRepel")) {
    p$layers[[i]]$aes_params$fontface <- "italic"
    p$layers[[i]]$aes_params$family <- "Times"     # set font family
  }
}

print(p)

