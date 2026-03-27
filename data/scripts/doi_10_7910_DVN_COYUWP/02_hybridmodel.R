# purpose     : population estimation for each month (random forest + area-to-point kriging)
# date        : 2020-04-25
# prepared by : Zhifeng Cheng
# --------------------------------------------------------------------------------

# --------------------------------------------------------------------------------
# Initial
# --------------------------------------------------------------------------------
library(velox)
library(randomForest)
library(raster)
library(tidyverse)
library(sf)
library(doParallel)
options("encoding" = "UTF-8")
options(scipen = 200)

# --------------------------------------------------------------------------------
# Extract mobile phone positioning count for each town for each month
# --------------------------------------------------------------------------------
# *** Attention! ***
# The town level boundary data is secret in China, we cannot share this
# All the intermediate results are in our shared town level point layer "CensusTownPoint_MP_Pop_monthly_freq.rds"

census <- st_read('data/CensusTown.shp')
MPmonth01 <- raster('data/04_monthly_China/MP_monthsum_201501.tif')
MPmonth02 <- raster('data/04_monthly_China/MP_monthsum_201502.tif')
MPmonth03 <- raster('data/04_monthly_China/MP_monthsum_201503.tif')
MPmonth04 <- raster('data/04_monthly_China/MP_monthsum_201504.tif')
MPmonth05 <- raster('data/04_monthly_China/MP_monthsum_201505.tif')
MPmonth06 <- raster('data/04_monthly_China/MP_monthsum_201506.tif')
MPmonth07 <- raster('data/04_monthly_China/MP_monthsum_201507.tif')
MPmonth08 <- raster('data/04_monthly_China/MP_monthsum_201508.tif')
MPmonth09 <- raster('data/04_monthly_China/MP_monthsum_201509.tif')
MPmonth10 <- raster('data/04_monthly_China/MP_monthsum_201510.tif')
MPmonth11 <- raster('data/04_monthly_China/MP_monthsum_201511.tif')
MPmonth12 <- raster('data/04_monthly_China/MP_monthsum_201512.tif')
stack <- stack(MPmonth01, MPmonth02, MPmonth03, MPmonth04, MPmonth05, MPmonth06, 
               MPmonth07, MPmonth08, MPmonth09, MPmonth10, MPmonth11, MPmonth12)

vx.mp <- velox(stack)
extract.mp <- vx.mp$extract(sp = census, fun = sum, df = T)

census <- cbind.data.frame(census, extract.mp[, -1])
st_write(census, 'data/10_monthly_mapping/01_Census_MP_monthly.shp')


# --------------------------------------------------------------------------------
# Adjust census data for each month
# --------------------------------------------------------------------------------
# results are stored in shared town level point layer "CensusTownPoint_MP_Pop_monthly_freq.rds"

#' 
#' AdjustPop
#' 
#' Adjust census data for each month.
#' 1) for a certain town, if there are more than three months whose mobile phone positioning counts fewer than 100, 
#' we will keep its population for 12 months unchanged.
#' 2) if there are less than three months whose mobile phone positioning counts fewer than 100, 
#' the population in the month with fewer positioning will keep unadjusted.
#' 
#' @param month
#' @return 
 
AdjustPop <- function(month) {
  census <- st_read('data/10_monthly_mapping/01_Census_MP_monthly.shp')
  census$geometry <- NULL
  for (i in 1:nrow(census)) {
    if (length(as.numeric(census[i, 8:19])[as.numeric(census[i, 8:19]) <= 100]) > 3) {
      census$PopAdjusted[i] <- census$Pop[i]
      census$Tag[i] <- 'C'
      next()
    }
    if (length(as.numeric(census[i, 8:19])[as.numeric(census[i, 8:19]) <= 100]) > 0 & census[i, month+6] <= 100) {
      census$PopAdjusted[i] <- census$Pop[i]
      census$Tag[i] <- 'B'
      next()
    }
    census$PopAdjusted[i] <- as.integer((census[i, month+6] / sum(census[, month+6])) / (census$Yearmean[i] / sum(census$Yearmean)) * census$Pop[i])
    census$Tag[i] <- 'A'
  }
  pop.adjusted <- as.data.frame(census$PopAdjusted)
  colnames(pop.adjusted)[1] <- gsub('MP', 'Pop', colnames(census)[month+6])
  return(pop.adjusted)
}

census <- st_read('data/10_monthly_mapping/01_Census_MP_monthly.shp')

pop.adjusted <- foreach(i = 1:12) %do% AdjustPop(i)
census.popadj <- bind_cols(foreach(i =1:12) %do% pop.adjusted[[i]])
census.popadj <- bind_cols(census, census.popadj)
st_write(census.popadj, 'data/10_monthly_mapping/02_Census_MP_Pop_monthly.shp')


# --------------------------------------------------------------------------------
# Random forest model for one month (January as example)
# --------------------------------------------------------------------------------
# Covariates preparation
census <- st_read('data/10_monthly_mapping/02_Census_MP_Pop_monthly.shp')

dem <- raster('data/01_dem/CNdem_fixed.tif')
slope <- raster('data/02_slope/CNslope_fixed.tif')
cropland <- raster('data/03_landuse/landuse_10_Cropland.tif')
forest <- raster('data/03_landuse/landuse_20_Forest.tif')
grassland <- raster('data/03_landuse/landuse_30_Grassland.tif')
waters <- raster('data/03_landuse/landuse_40_Waters.tif')
urban <- raster('data/03_landuse/landuse_51_Urban.tif')
rural <- raster('data/03_landuse/landuse_52_Rural.tif')
otherbuilding <- raster('data/03_landuse/landuse_53_OtherBuilding.tif')
unutilized <- raster('data/03_landuse/landuse_60_Unutilized.tif')
nightlight <- raster('data/05_nightlight/VIIRS_2015_CHINA.tif')
MP <- raster('data/04_monthly_China/MP_monthsum_201501.tif')

stack <- stack(dem, slope, cropland, forest, grassland, waters, urban, 
               rural, otherbuilding, unutilized, nightlight, MP)
vx.stack <- velox(stack)
extract <- vx.stack$extract(sp = census, fun = mean, df = T)

colnames(extract)[-1] <- c('CNdem_fixed','CNslope_fixed','landuse_10_Cropland','landuse_20_Forest',
                           'landuse_30_Grassland','landuse_40_Waters','landuse_51_Urban','landuse_52_Rural',
                           'landuse_53_OtherBuilding','landuse_60_Unutilized','VIIRS_2015_CHINA', 'MP_monthsum_201501')

# Excute random forest model
## list towns whose population were not adjusted 
list.nodynamic <- NULL
for (i in 1:nrow(census)) {
  if (length(as.numeric(census[i, 8:19])[as.numeric(census[i, 8:19]) <= 100]) > 3) {
    list.nodynamic <- c(list.nodynamic, i)
  }
}

## calculate popultion density as response variable
pop.column <- census[, c('Pop201501', 'Area')]
pop.column$per <- as.numeric(pop.column$Pop201501 / pop.column$Area)
pop.column$geometry <- NULL

## combine population density and covariates
cov <- cbind.data.frame(pop.column, extract[, -1])

## filter towns whose covariates have NULL value or population is 0 (farm, etc.)
cov <- cov[-list.nodynamic, ]
cov <- cov[!is.na(cov$CNslope_fixed) & 
           !is.na(cov$landuse_10_Cropland) & 
           !is.na(cov$VIIRS_2015_CHINA) & 
           !is.na(cov$MP_monthsum_201501) & 
           cov$per != 0, ]

## fit random forest model
tune <- tuneRF(y = log(cov$per), 
               x = cov[, -1])
rf <- randomForest::randomForest(log(per) ~ ., 
                                 data = cov, 
                                 importance = T, 
                                 ntree = 500, 
                                 mtry = 4)

## predict population for each pixel using established random forest model 
predict <- predict(stack, rf)
predict.pop <- exp(1) ^ predict
saveRDS(rf, 'data/10_Monthly_mapping/01_RF_RDS/RF_201501.RDS')
writeRaster(predict.pop, 'data/10_Monthly_mapping/02_RF_Pop/Pop_RF_201501.tif', format = 'GTiff')



# --------------------------------------------------------------------------------
# Area-to-point kriging for one month (January as example)
# --------------------------------------------------------------------------------
# calculate number of pixels each town covered (execute this once is enough)
# results are stored in shared town level point layer "CensusTownPoint_MP_Pop_monthly_freq.rds"
census <- st_read('data/10_monthly_mapping/02_Census_MP_Pop_monthly.shp')
sp.census <- as(census, 'Spatial')
rf <- raster('data/10_monthly_mapping/02_RF_Pop/Pop_RF_201501.tif')
sp.point <- rasterToPoints(rf, spatial = T)
over <- over(sp.point, sp.census)
over.narm <- over[!is.na(over$MergeTown), ]
for (i in 1:nrow()) {
  Freq <- nrow(over.narm[over.narm$MergeTown == census.part$MergeTown[i], ])
  census$Freq[i] <- Freq
}
st_write(census, 'data/10_monthly_mapping/03_Census_MP_Pop_monthly_freq.shp')

# area-to-point kriging
## calculate residual for each town
## results are stored in shared town level point layer "CensusTownPoint_MP_Pop_monthly_freq.rds"
census <- st_read('data/10_monthly_mapping/03_Census_MP_Pop_monthly_freq.shp')

vx <- velox(rf)
extract.rf <- vx.mp$extract(sp = census, fun = sum, df = T)
census <- cbind.data.frame(census, extract.rf[, -1])

for (i in 1:nrow(census)) {
  cat('Processing ->', i, '\n')
  if(census$Freq[i] == 0) {
    census$residual[i] <- as.numeric(census$Pop201501[i] - census$RF201501[i])
    next()
  }
  census$residual[i] <- as.numeric((census$Pop201501[i] - census$RF201501[i]) / census$Freq[i])
}
census$residual_sum <- census$Pop201501 - census$RF201501
census$Area <- as.numeric(st_area(census$geometry) / 1000000)
census <- census[census$Freq != 0, ]
sp.census <- as(census, 'Spatial')

## extract centriod of each town to fit variogram
resi.point <- st_centroid(census)
sp.resi.point <- as(resi.point, 'Spatial')

## Variogram fitting
vgm <- automap::autofitVariogram(residual~1,
                                 sp.resi.point,
                                 model = c("Mat"),
                                 kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10))

vgmarea <- vgmArea(sp.census, vgm = vgm$var_model, ndiscr = 96, verbose = T)

rf <- raster('data/10_monthly_mapping/02_RF_Pop/Pop_RF_201501.tif')
sp.point <- rasterToPoints(rf, spatial = T)

## execute area-to-point kriging 
## "ndiscr" is set to 96, this will achieve a higher accuracy, but is very computationally intensive
## set it lower, and you may achieve faster calculation speed by sacrificing some accuracy
a2p <- krige0(residual~1, 
              sp.census, 
              sp.point,  
              vgmArea, ndiscr = 96,
              vgm = vgm$var_model, 
              verbose = TRUE)
a2p <- raster(SpatialPixelsDataFrame(sp.point, data.frame(pred = a2p)))

