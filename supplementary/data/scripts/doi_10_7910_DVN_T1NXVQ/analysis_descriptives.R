#############################
# DESCRIPTIVE ANALYSIS
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#############################

# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}
load_data(reload = F)

## Make cluster
if(!exists("cl")){
  cl <- make_cluster(ncore)
  stop.cl <- T
} else {
  stop.cl <- F
}



# DATA ######################

# Historical Cshapes
cshapes.shp <- readOGR("data/geography/cshapes_2_shapefile/cshapes_2_cow.geojson",
                       paste0("cshapes_2_cow"))
cshapes.shp$startdate <- as.Date(cshapes.shp$start)
cshapes.shp$enddate <- as.Date(cshapes.shp$end)
cshapes.shp <- raster::crop(cshapes.shp, plot.europe.shp)

# Extent of ethnic data detail plot

## Make
ethmap.ext <- as(extent(c(32, 34, 54, 56)), "SpatialPolygons")
ethmap.ext.diff <- gDifference(gBuffer(ethmap.ext, width = 5), ethmap.ext, byid = T)


## Save as overview map
png(file.path(fig.path, "ethmap_example_1.png"), width = 4, height = 4, res = 400, units = "in")
par(mar = c(0,0,0,0))
plot(plot.europe.shp)
plot(ethmap.ext, add = T, border = "red", col = "red")
plot(cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(1886, "-01-01")) & 
                   cshapes.shp$enddate >= as.Date(paste0(1886, "-01-01")), ], 
     add = T, border = "darkgrey")
plot(plot.europe.shp, add = T)
dev.off()








# ETHNIC BORDERS CORRELATIONS ###

## Compute correlation
years <- seq(1886, 2011, by = 25)
corr.df <- do.call(rbind, lapply(years, function(i){
  do.call(rbind, lapply(years, function(j){
    data.frame(i = i, j = j, 
              correlation = cor(edge_attr(graph, paste0("cuteth", i)),
        edge_attr(graph, paste0("cuteth", j)),
        use = "complete.obs"))
  }))
}))

## Prepare plot
plot.df <- corr.df

## Plot
g <- ggplot(plot.df, aes(x = i, y = j)) +
  geom_raster(aes(fill = correlation)) +
  scale_fill_viridis_c(name = "Coefficient\nof\ncorrelation",
                       direction = -1) +
  scale_y_continuous(breaks = years, labels = paste0(years - 51, "-\n",years-1), 
                     name = NULL) +
  scale_x_continuous(breaks = years, labels = paste0(years - 51, "-\n",years-1),
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

png(file.path(fig.path, "eth_edge_corr.png"), width = 4.2, height = 3, res = 400, units = "in")
print(g)
dev.off()



# GRAPH ######

## Some baseraster for legends. 
base.rs <- raster(extent(plot.europe.shp), res = 0.05, crs = "+proj=longlat +datum=WGS84")
base.rs <- setValues(base.rs, rep(0, ncell(base.rs)))

## Plots

### Base lattice
png(file.path(fig.path, "map_base.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,2,0))
plot(plot.europe.shp, border = "lightgrey",
     lwd = .4, asp = 1)
plot_spatial_graph(graph, vertex.size = 0.025, 
                   edge.color = "grey", 
                   edge.width = .75,
                   add = T)
dev.off()

### Main outcome plot
png(file.path(fig.path, "map_partitioning.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,2,0))
plot(plot.europe.shp, border = "lightgrey",
     lwd = .4, asp = 1)
set.seed(4)
plot_spatial_graph(graph, vertex.size = 0.1, 
                   vertex.color = sample(inferno(256, direction = -1), 256, replace = F)[as.numeric(as.factor(V(graph)$cshp_1886))],
                   edge.color = ifelse(E(graph)$cshp_1886 == 1, grey(.05), "lightgrey"), 
                   edge.width = .75,
                   add = T)
dev.off()

### Main outcome plot -- for every 25years
for(i in seq(1886, 2011, by = 25)){
  png(file.path(fig.path, paste0("map_part_",i,".png")), 
      width = 6, height = 3, res = 400, units = "in")
  par(mar = c(0,0,2,0))
  plot(plot.europe.shp, border = "lightgrey",
       lwd = .4, asp = 1, main = i)
  set.seed(4)
  plot_spatial_graph(graph, vertex.size = 0.1, 
                     vertex.color = sample(inferno(256, direction = -1), 256, replace = F)[
                       as.numeric(as.factor(vertex_attr(graph, paste0("cshp_", i))))
                       ],
                     edge.color = ifelse(edge_attr(graph, paste0("cshp_", i)) == 1, 
                                         grey(.05), "lightgrey"), 
                     edge.width = .75,
                     add = T)
  dev.off()
}


## Plot Ethnic boundaries
png(file.path(fig.path, "map_ethnicborders.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,2,0))
plot(plot.europe.shp, border = "lightgrey", 
     lwd = .4, asp = 1)
plot_spatial_graph(graph, vertex.size = 0.025, 
                   edge.color = viridis(256, direction = -1)[1 + ceiling(E(graph)$cuteth1886 * 255)], 
                   edge.width = .75,
                   add = T)
raster::plot(base.rs, zlim = c(0,1), col = viridis(256, direction = -1),
             legend.only = T, add = T)
dev.off()



# ETHNIC MAPS ####

# Example Polygons

## Load data

### Meta
eth.meta.df <- read.csv("data/geography/HEG/heg_meta.csv")

### Language match of groups to standardize
eth.match.df <- read.csv("data/geography/HEG/heg_langmatch.csv")

### Shapefiles
heg.shp <- readOGR("data/geography/HEG/heg_full.geojson",
                   paste0("heg_full"))

### Extents
heg.ext.shp <- readOGR("data/geography/HEG/heg_extent.geojson",
                   paste0("heg_extent"))
heg.ext.shp <- as(heg.ext.shp,"SpatialLinesDataFrame")



## Subset to pre-1885
ex.eth.spdf <- heg.shp[heg.shp$file_name %in% 
                         eth.meta.df$file_name[eth.meta.df$year_data_end <= 1885],] 


### Get language match
ex.eth.spdf@data <- join(ex.eth.spdf@data, 
                         eth.match.df,
                         by = c("file_name", "group"),
                         type = "left", match = "first")


## Plot Hungarians

### Define group and extent
g = "hungarian"
ext = c(15.46, 27.3, 45,49)

### Print
png(file.path(fig.path, "ex_hungarians.png"), width = 4, height = 1.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
plot(as(extent(ext),"SpatialPolygons"), border = "white")

plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = rgb(1,0,0,.1), add = T)
plot(cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(1886, "-01-01")) & 
                   cshapes.shp$enddate >= as.Date(paste0(1886, "-01-01")), ], 
     add = T, border = "darkgrey", col = "white")
plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = "white", add = T)
plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = rgb(1,0,0,.1), add= T)
plot_spatial_graph(graph, vertex.size = .5, 
                   edge.color = "lightgrey", 
                   edge.width = 1, edge.lty = 1,
                   add = T)
dev.off()

### Write out number of maps cited in paper
writeLines(as.character(length(unique(ex.eth.spdf$file_name[grepl(g, ex.eth.spdf$main_match)]))),
           file.path(num.path, "num_ex_hungarians.tex"))


## Plot Slovenes

### Define group and extent
g = "slovene"
ext = c(13.15, 16.55, 45.18,47.5)

### Print
png(file.path(fig.path, "ex_slovenes.png"), width = 3, height = 1.5, res = 400, units = "in")
par(mar = c(0,0,0,0))
plot(as(extent(ext),"SpatialPolygons"), border = "white")

plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = rgb(1,0,0,.1), add = T)
plot(cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(1886, "-01-01")) & 
                   cshapes.shp$enddate >= as.Date(paste0(1886, "-01-01")), ], 
     add = T, border = "darkgrey", col = "white")
plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = "white", add = T)
plot(ex.eth.spdf[grepl(g, ex.eth.spdf$main_match),],
     col = rgb(1,0,0,.1), add= T)
plot_spatial_graph(graph, vertex.size = .5, 
                   edge.color = "lightgrey", 
                   edge.width = 1, edge.lty = 1,
                   add = T)
dev.off()

### Write out number of maps cited in paper
writeLines(as.character(length(unique(ex.eth.spdf$file_name[grepl(g, ex.eth.spdf$main_match)]))),
           file.path(num.path, "num_ex_slovenes.tex"))


# PLOT ALL MAPS ##########

# Make base raster
base.rs <- raster(extent(plot.europe.shp), res = 0.05, crs = "+proj=longlat +datum=WGS84")
base.rs <- setValues(base.rs, rep(0, ncell(base.rs)))

# Negative of Europe
this.europe.diff.shp <- gDifference(as(extent(base.rs) + c(-5,5,-5,5), "SpatialPolygons"),
                                    plot.europe.shp)

# Bargraph: Baps per year (5-year periods)
g <- ggplot(eth.meta.df, aes(x = floor(year_data_end/5)*5+2.5)) +
  geom_bar() + 
  scale_y_continuous(breaks = seq(0,10, by = 2),
                     name = "Number of maps") +
  scale_x_continuous(name = "Year (5-year periods)") +
  theme_minimal() 
png(file.path(fig.path, "maps_by_year.png"), width = 3, height = 3, res = 400, units = "in")
print(g)
dev.off()

# Raster: Number of maps

## Rasterize
count.rs <- fasterize(st_as_sf(heg.shp), base.rs, 
                      fun = "count", background = NA)

## Make nice plot
png(file.path(fig.path, "ethmap_count.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,2,0))
plot(plot.europe.shp, border = "lightgrey", # main = "Count of maps by area",
     lwd = .4, asp = 1)
plot(count.rs, col = inferno(256, direction = -1), add = T)
plot(plot.europe.shp, border = "lightgrey",
     col = "transparent", add = T,
     lwd = .4, asp = 1)
plot(this.europe.diff.shp, border = "transparent", 
     lwd = .4, col = "white", asp = 1,  add = T)
plot(count.rs,  col = inferno(256, direction = -1), add = T, legend.only = T)
dev.off()



## Show one raw map
if("imager" %in% installed.packages()[,"Package"]){
  library(imager)
  
  ### Load
  raw.map <- raster::stack("data/analysis_data/uch_cam_12_georef.tif")
  
  ### Set background to white
  im <- load.image("data/analysis_data/uch_cam_12_georef.tif") %>% grayscale #%>% resize(size_x = -10, size_y = -10)
  labels <- imager::label(im, tolerance = 0)
  label.ids <- unique(c(labels[1, 1,1,1], labels[1, ncol(labels),1,1],labels[nrow(labels), 1,1,1],
                        labels[round(nrow(labels)/2), 1,1,1], labels[round(nrow(labels)/2), ncol(labels),1,1],
                        labels[nrow(labels), 1,1,1],
                        labels[nrow(labels), ncol(labels),1,1]))
  im[!labels %in% label.ids] <- NA
  im[labels %in% label.ids] <- 1
  mask.rs <- raster(t(as.matrix(im)))
  extent(mask.rs) <- extent(raw.map)
  
  
  ### Plot whole map
  png(file.path(fig.path, "ethmap_example_1.png"), width = 4, height = 4, res = 200, units = "in")
  par(mar = c(0,0,0,0))
  plotRGB(raw.map, maxpixels = (200*4)^2, bgalpha = 0, asp = 1)
  plot(mask.rs, maxpixels = (200*4)^2, bgalpha = 0, add = T, asp = 1, 
       breaks = c( .5, 100), col = c("white", "white"),
       legend = F)
  plot(as(extent(mask.rs), "SpatialPolygons"), border = "white", add = T)
  plot(cshapes.shp[cshapes.shp$startdate <= as.Date(paste0(1886, "-01-01")) & 
                     cshapes.shp$enddate >= as.Date(paste0(1886, "-01-01")), ], 
       add = T, border = "grey")
  plot(ethmap.ext, add = T, border = "black", col = "red")
  plot(plot.europe.shp, border = "black", add = T)
  dev.off()
  
  
  ### Plot detail
  raw.map.det <- crop(raw.map, extent(ethmap.ext))
  png(file.path(fig.path, "ethmap_example_2.png"), width = 4, height = 4, res = 100, units = "in")
  par(mar = c(0,0,0,0))
  plot(ethmap.ext)
  plotRGB(raw.map.det, add = T, maxpixels = (200*4)^2)
  dev.off()
  
  
  # Example: borders, pre 1886
  
  ## Prepare
  map.groups <- heg.shp[heg.shp$file_name %in% 
                          eth.meta.df$file_name[eth.meta.df$year_data_end <= 1885],] 
  map.groups <- map.groups[gIntersects(map.groups, ethmap.ext, byid = T)[1,],]
  
  png(file.path(fig.path, "ethmap_example_3.png"), width = 4, height = 4, res = 400, units = "in")
  par(mar = c(0,0,0,0))
  plot(ethmap.ext, border = "white")
  plot(map.groups, add = T, lwd = 2)
  
  ## Correct map borders
  borders <- heg.ext.shp[heg.ext.shp$file_name %in% map.groups$file_name,]
  b.int <- gIntersects(borders, ethmap.ext, byid = T)[1,]
  plot(borders[b.int,], add = T,
       col = "white", lwd = 3, border = "white")
  plot(map.groups[map.groups$file_name %in% borders$file_name[!b.int],], add = T,
       lwd = 2)
  
  ## Add Graph
  plot_spatial_graph(graph, vertex.size = 2, 
                     edge.color = "grey", 
                     edge.width = 2.5, edge.lty = 2,
                     add = T)
  plot(ethmap.ext.diff, border = "white", col = "white", add = T)
  
  ## Add labels
  text(SpatialPoints(cbind(x = c(32.75, 33.67), y = c(54.8, 55.85))),
       labels = c("Belarussians", "Russians"), cex = 1.8)
  
  dev.off()
  
  
  
  ## Final graph for subplot
  png(file.path(fig.path, "ethmap_example_4.png"), width = 4, height = 4, res = 400, units = "in")
  par(mar = c(0,0,0,0))
  plot(ethmap.ext, border = "white")
  
  plot(map.groups, add = T, border = "grey", lwd = 2)
  
  ## Correct map borders
  plot(borders[b.int,], add = T,
       col = "white", lwd = 3, border = "white")
  plot(map.groups[map.groups$file_name %in% borders$file_name[!b.int],], 
       add = T, border = "grey", lwd = 2)
  
  
  ## Add labels
  text(SpatialPoints(cbind(x = c(32.75, 33.67), y = c(54.8, 55.85))),
       labels = c("Belarussians", "Russians"), cex = 1.8,col = "grey")
  
  plot(plot.europe.shp, border = "lightgrey", 
       lwd = .4, asp = 1, add = T)
  plot_spatial_graph(graph, vertex.size = 2, 
                     edge.color = viridis(256, direction = -1)[1 + ceiling(E(graph)$cuteth1886 * 255)], 
                     edge.width = 5,
                     add = T)
  plot(ethmap.ext.diff, col = "white", border = "white", add = T)
  
  # Legend
  rect(33.6, 54.4, 34, 55.6, col = rgb(1,1,1, 1), border = rgb(1,1,1, 1) )
  raster::plot(base.rs, zlim = c(0,1), 
               col = viridis(256, direction = -1),
               legend.only = T, add = T)
  dev.off()
  
}



## Prepare Lines

### Get Lines
plot.lines <- eth.meta.df$file_name[eth.meta.df$file_name %in% heg.ext.shp$file_name]
clusterExport_fast(cl, c("heg.shp", "heg.ext.shp", "base.rs"))
eth.lines.ls <- foreach(f = plot.lines, .noexport = c("heg.shp", "heg.ext.shp")) %dopar% {
  
                          ## Load lines and extent
                          shp <- heg.shp[heg.shp$file_name == f,]
                          shp <- gBuffer(shp, width = 0, byid = T)
                          shp.sl <- as(shp,"SpatialLinesDataFrame")
                          
                          ext <- heg.ext.shp[heg.ext.shp$file_name == f,]
                          ext.sl <- as(ext,"SpatialLinesDataFrame")

                          
                          ## Rasterize extent
                          ext.sl$count <- 1
                          rs_ext <- rasterize(ext.sl, base.rs, field = "count", fun = "first")
                          
                          ## Rasterize group borders
                          shp.sl$count <- 1
                          rs <- rasterize(shp.sl, base.rs, field = "count", fun = "first")
                          rs[rs_ext == 1] <- 0
                          
                          ## Rasterize count
                          shp$count <- 1
                          shp <- gBuffer(shp, width = .1, byid = T)
                          cr <- rasterize(shp, base.rs, field = "count", fun = "first")
                          
                          ## Return
                          list(rs, cr)
                        }

### Sum up lines
line.rs <- stack(lapply(eth.lines.ls, function(i){i[[1]]}))
line.rs <- sum(line.rs, na.rm = T)
line.rs <- line.rs /
  (sum(stack(lapply(eth.lines.ls, function(i){i[[2]]})), na.rm = T))

## Make nice plot
png(file.path(fig.path, "ethmap_all.png"), width = 6, height = 3, res = 400, units = "in")
par(mar = c(0,0,2,0))
plot(plot.europe.shp, border = "lightgrey", # main = "All borders (as fraction of maps in area)",
     lwd = .6, asp = 1)
raster::plot(line.rs, col = inferno(256, direction = -1), add = T, maxpixels = 1e7)
plot(plot.europe.shp, border = "lightgrey",
     col = "transparent", add = T,
     lwd = .4, asp = 1)
plot(this.europe.diff.shp, border = "transparent", 
     lwd = .4, col = "white", asp = 1,  add = T)
plot(line.rs, col = inferno(256, direction = -1), add = T, legend.only = T)
dev.off()

# Stop cluster if necessary 
if(stop.cl){
  stopCluster(cl)
  rm(cl)
}
