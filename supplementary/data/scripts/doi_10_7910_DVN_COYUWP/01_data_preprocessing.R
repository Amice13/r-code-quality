# purpose     : Data preprocessing (nightlight, landuse, MP)
# Produced by : Jianghao Wang & Zhifeng Cheng
# Date        : 2020-04-25

# ---------------------------------------------------------------
# Initial
# ---------------------------------------------------------------
library(rgdal)
library(raster)
library(sf)
library(tidyverse)

# ---------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------
## boundary
boundary <- st_read('data/00_boundary/Boundary.shp')

## slope
slope <- raster('data/02_slope/CNslope.tif')
tocrs <- proj4string(slope)


## nightlight
nightlight <- raster('data/05_nightlight/SVDNB_npp_20150101-20151231_CHINA.tif') %>%
  resample(slope, method = "bilinear")
writeRaster(nightlight, 'data/05_nightlight/VIIRS_2015_CHINA.tif', format = 'GTiff')


## landuse
landuse <- raster('data/03_landuse/ld2015/hdr.adf')
for (i in c(10, 20, 30, 40, 50, 60, 70, 80, 90)) {
  cat(as.character(Sys.time()), i)
  ld <- landuse
  if(i %% 10 == 0) ld <- (ld > i) & (ld < i + 10)
  if(i %% 10 != 0) ld <- (ld == i)
  ld.agg <- aggregate(ld, fact = c(10, 10), fun = sum)
  ld.resam <- resample(ld.agg, slope, method="bilinear")
  writeRaster(ld.resam, paste0('data/03_landuse/landuse_', i, '_.tif'), format="GTiff")
}


## MP
boundary.latlon <- st_transform(boundary, "+proj=longlat +datum=WGS84")
bbox.pre <- extent(boundary.latlon)
temp <- raster(bbox.pre, resolution = c(0.01, 0.01), crs = CRS("+proj=longlat +datum=WGS84"))

list <- list.files('data/04_monthly_China/origin', full.names = T)
for (i in 1:length(list)) {
  cat(as.character(Sys.time()), 'processing -> ', i, '\n')
  load(list[i])
  fddf <- fddf %>%
    dplyr::mutate(x = x / 100) %>%
    dplyr::mutate(y = y / 100) %>%
    dplyr::filter(x >= bbox.pre@xmin, x <= bbox.pre@xmax, y >= bbox.pre@ymin, y <= bbox.pre@ymax) %>% 
    st_as_sf(coords = c('x', 'y'), crs = "+proj=longlat +datum=WGS84")
  rs <- rasterize(x = monthsum, y = temp, field = "month_sum", background = 0, fun = "sum", na.rm = TRUE) %>%
    projectRaster(crs = tocrs) %>%
    resample(slope, method = 'ngb')
  writeRaster(rs, filename = gsub('.Rdata', '.tif', gsub('origin', 'rasterization', list[i])), 
              format = 'GTiff', datatype = 'INT4U', overwrite = T)
}


