# process data from the ERA5 (copernicus)
extractCOPERNICUS <- function(file, distance = 25000){
    browser()
    grib <- readGDAL(paste0(spatial_input, "era5/", file))
    
    day <- str_sub(file, start = 1, end = 8) # extract day from filename 
    
    r <- raster::brick(grib) # convert grib to raster brick, each layer is an hourly observation

    r <- projectRaster(r, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # reproject to match shapefile    
    
    buffer <- st_buffer(embassy, dist = distance) # use a 25 km radius around 
    
    bufferWind <- raster::mask(r, buffer) # extract area around US embassy
    
    naVal <- r@file@nodatavalue
    
    wind_df <- rasterToPolygons(bufferWind) %>%
        st_as_sf() 
    st_geometry(wind_df) <- NULL
    wind_df[wind_df == naVal] <- NA
    
    wind_df <- colMeans(wind_df, na.rm = T) # take the mean of all grid-cells for masked raster
    
    U <- wind_df[seq(1,120,5)] # subset just u component (24 hours worth)
    V <- wind_df[seq(2,120,5)] # subset just v component (24 hours worth)
    dew <- wind_df[seq(3,120,5)]
    temp <- wind_df[seq(4,120,5)]
    rain <- wind_df[seq(5,120,5)]
    
    meanU <- mean(U, na.rm = T) # take the average across the entire day
    meanV <- mean(V, na.rm = T)# take the average across the entire day
    meanDew <- mean(dew, na.rm = T)
    meanTemp <- mean(temp, na.rm = T)
    sumRain <- sum(rain, na.rm = T)
    
    ws <- sqrt(meanU^2 + meanV^2) # convert to wind speed
    wd <- (atan2(meanU, meanV)*360/2/pi) + 180 #convert to wind direction in degrees
    
    df <- data.frame(ws = ws, wd = wd, u = meanU, v = meanV, day = day,
                     dewpoint = meanDew, temp = meanTemp, rain = sumRain) # add all values to a 1 row data frame
    
    # define breaks for wind direction
    dir_breaks16 <- c(0, 360/32, (1/32 + (1:15 / 16)) * 360, 360)
    dir_breaks8 <- c(0, 360/16, (1/16 + (1:7 / 8)) * 360, 360)
    
    # define labels for wind direction
    dir_labs16 <- c(
        "N", "NNE", "NE", "EENE",
        "E", "ESE", "SE", "SSE",
        "S", "SSW", "SW", "WSW",
        "W", "WNW", "NW", "NNW",
        "N"
    )
    
    
    dir_labs8 <- c(
        "N", "NE", 
        "E",  "SE", 
        "S",  "SW", 
        "W",  "NW", 
        "N"
    )
    
    df <- df %>%
        mutate(dir8 = cut(wd,
                          breaks = dir_breaks8,
                          labels = dir_labs8,
                          right = FALSE,
                          include.lowest = TRUE),
               dir16 = cut(wd,
                           breaks = dir_breaks16,
                           labels = dir_labs16,
                           right = FALSE,
                           include.lowest = TRUE))
    return(df)
    
}
