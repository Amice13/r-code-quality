#====NOTE: ALL DIRECTORIES, FILE NAMES AND VARIABLES SHOULD BE CHANGED TO MATCH YOUR FILES


# ========================================
# LOAD LIBRARIES
# =====================================
library(raster)   # For raster processing
library(terra)    # Optional if you switch to terra later
library(psych)
library(sp)
library(ggplot2)

# ========================================
# SET WORKING DIRECTORY AND LOAD RASTERS
# ========================================
setwd("C:/Users/USER/Desktop/The final analysis/Rasters")

# Load and stack all raster layers
raster_stack <- stack(
  raster("Al.tif"),     # Al
  raster("B.tif"),      # B
  raster("Ca.tif"),     # Ca
  raster("CEC.tif"),    # CEC
  raster("Fe.tif"),     # Fe
  raster("K.tif"),      # K
  raster("Mg.tif"),     # Mg
  raster("Mn.tif"),     # Mn
  raster("P.tif"),      # P
  raster("pH.tif"),     # pH
  raster("SOC.tif"),    # SOC
  raster("TN.tif")      # TN
)

# Name the raster layers
names(raster_stack) <- c("Al", "B", "Ca", "CEC", "Fe", "K", "Mg", "Mn", "P", "pH", "SOC", "TN")
print(names(raster_stack))

# ========================================
# DEFINE CATEGORIES AND THRESHOLDS
# ========================================
more_is_better <- c("SOC", "Ca", "K", "CEC", "P", "Mg")
less_is_better <- c("Al", "Fe", "Mn")
optimum <- c("pH", "B", "TN")

optimum_thresholds <- list(
  pH = c(5.5, 6.5),
  B = c(0.5, 2.0),
  TN = c(0.2, 0.4)
)

# ========================================
# TRANSFORMATION FUNCTION
# ========================================
transform_raster <- function(r, category, threshold = NULL) {
  if (category == "more_is_better") {
    min_val <- cellStats(r, stat = "min", na.rm = TRUE)
    max_val <- cellStats(r, stat = "max", na.rm = TRUE)
    return((r - min_val) / (max_val - min_val))
  }
  
  else if (category == "less_is_better") {
    min_val <- cellStats(r, stat = "min", na.rm = TRUE)
    max_val <- cellStats(r, stat = "max", na.rm = TRUE)
    return(1 - ((r - min_val) / (max_val - min_val)))
  }
  
  else if (category == "optimum" && !is.null(threshold)) {
    lower <- threshold[1]
    upper <- threshold[2]
    min_val <- cellStats(r, stat = "min", na.rm = TRUE)
    max_val <- cellStats(r, stat = "max", na.rm = TRUE)
    
    r_vals <- values(r)
    score <- rep(NA, length(r_vals))
    
    # Inside optimum range
    inside <- which(r_vals >= lower & r_vals <= upper)
    score[inside] <- 1
    
    # Above optimum: less is better
    above <- which(r_vals > upper)
    score[above] <- 1 - ((r_vals[above] - upper) / (max_val - upper))
    
    # Below optimum: more is better
    below <- which(r_vals < lower)
    score[below] <- (r_vals[below] - min_val) / (lower - min_val)
    
    # Ensure all scores are between 0 and 1
    score <- pmax(pmin(score, 1), 0)
    
    r_out <- raster(r)
    values(r_out) <- score
    return(r_out)
  }
  
  else {
    stop("Invalid category or missing threshold.")
  }
}

# ========================================
# APPLY TRANSFORMATIONS TO EACH LAYER
# ========================================
transformed_layers <- list()

for (param in names(raster_stack)) {
  r <- raster_stack[[param]]
  
  if (param %in% more_is_better) {
    transformed_layers[[param]] <- transform_raster(r, "more_is_better")
  } else if (param %in% less_is_better) {
    transformed_layers[[param]] <- transform_raster(r, "less_is_better")
  } else if (param %in% optimum) {
    transformed_layers[[param]] <- transform_raster(r, "optimum", threshold = optimum_thresholds[[param]])
  }
}

# ========================================
# STACK, PLOT, AND SAVE TRANSFORMED RASTERS
# ========================================
transformed_stack <- stack(transformed_layers)
plot(transformed_stack)
writeRaster(transformed_stack, "transformed_rasters.tif", overwrite = TRUE)

#============================================
# WEIGHTED SUM
#===========================================

# ========================================
# 1. Define AHP Weights
# ========================================

ahp_weights <- c(
  Al  = 0.0273,
  B   = 0.0323,
  Ca  = 0.0606,
  CEC = 0.1019,
  Fe  = 0.0188,
  K   = 0.1146,
  Mg  = 0.0575,
  Mn  = 0.0250,
  P   = 0.1074,
  pH  = 0.1927,
  SOC = 0.1521,
  TN  = 0.1099
)

# Make sure the weights sum to 1
if (abs(sum(ahp_weights) - 1) > 0.001) {
  stop("AHP weights must sum to 1.")
}

# ========================================
# 2. Ensure raster names match weight names
# ========================================

if (!all(names(transformed_stack) %in% names(ahp_weights))) {
  stop("Some raster names do not match the names in ahp_weights.")
}

# ========================================
# 3. Multiply each raster layer by its weight
# ========================================

weighted_layers <- list()

for (param in names(transformed_stack)) {
  r <- transformed_stack[[param]]
  weight <- ahp_weights[[param]]
  weighted_layers[[param]] <- r * weight
}

# ========================================
# 4. Create weighted sum (Soil Quality Index)
# ========================================

weighted_stack <- stack(weighted_layers)
sqi <- calc(weighted_stack, sum)

# ========================================
# 5. Plot and Save
# ========================================

plot(sqi, main = "Soil Quality Index (AHP Weighted Sum)")
writeRaster(sqi, "C:/Users/USER/Desktop/The final analysis/Rasters/Soil_Quality_Index.tif", overwrite = TRUE)
