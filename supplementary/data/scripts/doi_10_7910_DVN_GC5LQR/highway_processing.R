library(tidyverse)
library(sf)
library(geosphere)
library(lwgeom)
library(units)

rootdir <- dirname(getwd())
setwd(paste(rootdir, "/Data/Traffic and Roadways/", sep = ""))

### load data - put zip codes in same projection as roads
shp.tract <- st_read(paste(rootdir, "/Data/Census Tracts shapefiles/", sep = ""), 
                               layer = "geo_export_79d01148-4a56-4331-a800-1e4f76bed5aa") %>%
        st_transform(., crs = 4269)
shp.tract$tract_index <- c(1:nrow(shp.tract))

njroads.sf <- st_read("tl_2015_36_prisecroads/", layer = "tl_2015_34_prisecroads")
nyroads.sf <- st_read("tl_2015_36_prisecroads/", layer = "tl_2015_36_prisecroads")
roads.sf <- rbind(njroads.sf, nyroads.sf)
roads.sfo <- roads.sf %>%
        group_by(FULLNAME) %>%
        summarize(geometry = st_combine(geometry)) %>%
        ungroup() %>%
        mutate(index = c(1:4285))

roads.sf <- distinct(roads.sf)
roads.sf$road_index <- c(1:nrow(roads.sf))

### plot nj and ny roads together
ggplot(roads.sf) +
        geom_sf()

### create approx 5km buffer (.05 degree) around nyc and include all intersected road segments
tracts <- st_buffer(st_union(shp.tract), .05)
nycroads <- data.frame(st_intersects(roads.sf, tracts))

### plot only NYC roads
roads.sf <- filter(roads.sf, road_index %in% nycroads$row.id)
roads.sf <- st_combine(roads.sf)

ggplot() +
        geom_sf(data = shp.tract) + 
        geom_sf(data = roads.sf, color = "red")

saveRDS(roads.sf, file = "nycrds.RDS")

### calculate tract centroids
centroids <- st_centroid(shp.tract)
nearestrds <- data.frame(tract = c(1:2165),
                         closesthwy = st_nearest_feature(centroids, roads.sfo))

nearestrds <- merge(nearestrds, roads.sfo, by.x = "closesthwy", by.y = "index", all.x = TRUE)
nearestrds$geometry <- NULL

### make grid to define road segments (.01 degree cell = approx 1km)
grid.sf <- st_make_grid(shp.tract, cellsize = .01)
roadsegs.sf <- st_split(roads.sf, grid.sf)
roadsegs.sf <- st_sf(geometry = st_collection_extract(roadsegs.sf, "LINESTRING"))
roadsegs.sf$index  <- c(1:nrow(roadsegs.sf))

# calculate distances from centroids to all road segments, for each tract, only keep segments that are within 5km
distance_matrix <- matrix(as.numeric(st_distance(centroids, roadsegs.sf)), nrow = nrow(shp.tract), ncol = nrow(roadsegs.sf))
indices <- data.frame(which(distance_matrix < 10000, arr.ind = TRUE))

### for each centroid find distance, bearing (to nearest point on the segment), and index of all roads <1km
sppoints <- as(centroids, "Spatial")
df.tracts <- data.frame("tract_index" = c(),
                      "road_index" = c(),
                      "dist" = c(),
                      "bearing" = c())

for (i in 1:nrow(centroids)) {
        ind <- filter(indices, row==i)
        if (nrow(ind)==0) {
                next
        }
        c <- ind$col
        ind$dist <- distance_matrix[i, c]
        ind$bearing <- NA
        xsf <- centroids[i,]
        x <- sppoints@coords[i,]
        rds <- filter(roadsegs.sf, index %in% c)
        closepts <- st_nearest_points(xsf, rds)
        closepts <- st_cast(closepts, "POINT")
        closepts <- closepts[seq(2, length(closepts), 2)]
        for (j in 1:nrow(rds)) {
                y <- as(closepts[j], "Spatial")
                ind$bearing[j] <- bearing(x, y)
        }
        colnames(ind) <- c("tract_index", "road_index", "dist", "bearing")
        df.tracts <- rbind(df.tracts, ind)
}


df.tracts_min <- mutate(df.tracts, dir = if_else(bearing > 0 & bearing<= 45, "NNE",
                                             if_else(bearing > 45 & bearing <= 90, "ENE",
                                                     if_else(bearing > 90 & bearing <= 135, "ESE",
                                                             if_else(bearing > 135 & bearing <= 180, "SSE",
                                                                     if_else(bearing <  0 & bearing >= -45, "NNW",
                                                                             if_else(bearing < -45 & bearing >= -90, "WNW",
                                                                                     if_else(bearing< -90 & bearing>= -135, "WSW", "SSW")))))))) %>%
        group_by(tract_index, dir) %>%
        summarize(dist = min(dist)) %>%
        pivot_wider(names_from = dir, values_from = dist)

df.tracts_min[is.na(df.tracts_min)] <- 10000
        
df.tracts <- merge(df.tracts, shp.tract[,c("boro_code", "boro_ct201", "ntaname", "tract_index")], by = "tract_index")
df.tracts$geometry <- NULL

write_csv(df.tracts, "tract_roads_distance_bearing.csv")

df.tracts_min <- merge(df.tracts_min, shp.tract[,c("boro_code", "boro_ct201", "ntaname", "tract_index")], by = "tract_index")
df.tracts_min$geometry <- NULL

df.tracts_min <- df.tracts %>%
        arrange(dist) %>%
        group_by(boro_ct201) %>%
        slice(20) %>%
        select(boro_ct201, dist) %>%
        #merge(df.tracts_min, by = "boro_ct201") %>%
        rename(near_dist20 = dist)

df.tracts_min <- merge(df.tracts_min, nearestrds, by.x = "tract_index", by.y = "tract")

### interesected tracts
tractrdi <- st_intersects(shp.tract, roads.sf)
tractrdi <- data.frame(tractrdi)

df.tracts_min <- mutate(df.tracts_min, rd_intersect = if_else(tract_index %in% tractrdi$row.id, 1, 0))

write_csv(df.tracts_min, "tract_roads_min_dist.csv")


#### how many tracts are intersected by roads

shp.tract <- merge(shp.tract, tractrdi, by.x = "tract_index", by.y = "row.id", all.x = TRUE)
shp.tract$col.id[is.na(shp.tract$col.id)] <- 0

ggplot() + geom_sf(data = shp.tract, aes(fill = as.factor(col.id)))
