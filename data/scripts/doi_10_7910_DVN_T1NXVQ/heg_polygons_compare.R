#############################
# HEG MAP COMPARISONS
# 
# This file compares polygons that show the same language group on different maps. 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/data_prep/prepare_all.R.R
#
#############################
rm(list = ls())

library(fasterize)
library(plyr)
library(tidyverse)
library(sp)
library(RPostgres)
library(scales)
library(mapview)
library(sf)
library(dplyr)
library(raster)
library(parallel)
library(rpostgis)
library(cshapes)
library(data.table)
library(foreach)
library(doParallel)

################################################################################
## Setup, load data
################################################################################


## Globals
heg.final.dir <- "data/analysis_data" ## save comparison table there


## Set number of cores 
NCORE = 40


## Functions
path <- "scripts/functions"
for(f in list.files(path, recursive = T)){
  source(file.path(path, f))
}


## DATA ########################
## Main dataset extent
ext.poly <- st_read(file.path("data", "geography/continents"),
                    "continents")
ext.poly <- ext.poly[ext.poly$continent == "Europe",]
ext.poly$id <-1

## Make Cluster
cl <- makeCluster(getOption("cl.cores", NCORE))
registerDoParallel(cl)
clusterExport(cl, c("ext.poly"))

## Make Connection


### Get Metadata
eth.meta.df <- read.csv("data/geography/HEG/heg_meta.csv")


### Load all polygons
eth.map.spdf <- readOGR("data/geography/HEG/heg_full.geojson",
                        paste0("heg_full"))

# LEDA

## Load // creates LEDA object
leda <- readRDS("data/geography/HEG/leda.rds")

## Get group links
grp.lang.df <- leda$get_raw_ethnolinks(list(type = "NASTAC"))

## Clean Group links
grp.lang.df$link <- sapply(grp.lang.df$link, function(l){
  paste(sort(trimws(unlist(strsplit(l, "|", fixed = T)))), collapse = " | ")
})
grp.lang.df$file_name <- grp.lang.df$map

## Join
eth.map.spdf@data <- plyr::join(eth.map.spdf@data,
                          grp.lang.df[, c("group","file_name", "link")],
                          by = c("file_name","group"), 
                          type = "left", match = "first")

plot(eth.map.spdf[eth.map.spdf$file_name %in% c("brl_rob_21"), ])

plot(eth.map.spdf[eth.map.spdf$file_name %in% c("brl_aya_25"), ], add = T, col = "grey")

plot(eth.map.spdf[eth.map.spdf$link == "Greek [L2]" &
                    eth.map.spdf$file_name %in% c("brl_aya_25"), ],
     col = "red")
plot(eth.map.spdf[eth.map.spdf$link == "Greek [L2]" &
                    eth.map.spdf$file_name %in% c("brl_rob_21"), ],
     add = T, col = "blue")
plot(eth.map.spdf[eth.map.spdf$file_name %in% c("brl_rob_21"), ], add = T, col = "grey")



## To sf
eth.map.sf <- st_as_sf(eth.map.spdf)



################################################################################
## Functions
################################################################################

# Polygon comparison
compare_polys <- function(ras, pop.ras, group.polys.df, grp.name){
  ## all.polys: df containing all ethnic group polygons
  ## grp.rgx:   regex for group in question 
  ## ras:       raster used in rasterization
  

  ## Get group polygons
  gp.group.polys.df <- group.polys.df %>% 
    filter(link == grp.name &
             !is.na(grp.name)) %>%
    st_cast("MULTIPOLYGON")
  
  ## Process by map
  rs.ls <- lapply(na.omit(unique(gp.group.polys.df$file_name)), function(m){
    print(m)
    ## Subset data into 3 groups
    ## 1: polygons that exactly match ethnologue group G (1:1)
    grp.polys.main <- gp.group.polys.df %>%
      filter(file_name == m)
    
    ## Copy raster and count
    pos.count <- ras
    pos.count <- setValues(pos.count, 0)
    
    ## Full map without related parent groups
    this.map.all <- group.polys.df %>% 
      filter(file_name == m & 
               !(grepl(grp.name, link, fixed = T) & grepl("|", link, fixed = T))) 
    
    ### Rasterize
    all.count <- fasterize(this.map.all, 
                           ras, 
                           fun = "count", 
                           background = NA)
    coverage <- fasterize(this.map.all, 
                          ras, 
                          fun = "any", 
                          background = NA)
    ## Setup empty results 
    result <- list(main = NULL, 
                   coverage = coverage, 
                   count = all.count)
    
    ## Main polygons (1:1)
    ##  Logic: Manipulate count of *positive* hits ; +1 for each hit
    if(nrow(grp.polys.main) > 0){
      result$main <- fasterize(grp.polys.main, pos.count, 
                                         field = NULL, 
                                         fun = "sum",
                                         background = 0)/all.count
    }
    
    ## Return
    return(result)
  })
  names(rs.ls) <- unique(gp.group.polys.df$file_name)
  
  # Double comparison loop
  comp.ls <- unlist(lapply(names(rs.ls), function(x){
    lapply(names(rs.ls), function(y){
      ## Setup 
      xl <- rs.ls[[x]]
      yl <- rs.ls[[y]]
      
      ## Return NULL if empty
      if(is.null(xl$main) | is.null(yl$main)){
        return(NULL)
      }
      
      ## Join  coverage
      cov <- xl$coverage * yl$coverage
      sum.cov <- sum(as.matrix(cov), na.rm = T)
      if(sum.cov == 0){
        return(NULL)
      }
      
      ## Territory
      x.terr <-  xl$main * cov
      y.terr <-  yl$main * cov
      y.terr.bin <- (y.terr > 0)
      
      ## Population
      x.pop <- pop.ras * x.terr
      y.pop <- pop.ras * y.terr
      
      ## Totals
      x.terr.tot = sum(as.matrix(x.terr), na.rm = T)
      x.pop.tot = sum(as.matrix(x.pop), na.rm = T)
      
      y.terr.tot = sum(as.matrix(y.terr), na.rm = T)
      y.pop.tot = sum(as.matrix(y.pop), na.rm = T)
      
      ## Matrix
      diff <- as.matrix(x.terr - y.terr)
      diff[!is.na(diff) & diff < 0] <- 0
      x.in.y.terr <- 1 - (sum(diff, na.rm = T) / x.terr.tot)
      diff <- as.matrix(x.pop - y.pop)
      diff[!is.na(diff) & diff < 0] <- 0
      x.in.y.pop <- 1 - (sum(as.matrix(diff), na.rm = T) / x.pop.tot)
      
      ## Result
      data.frame(x = x, y = y, 
                 x.year = gp.group.polys.df$year[gp.group.polys.df$file_name == x][1],
                 y.year = gp.group.polys.df$year[gp.group.polys.df$file_name == y][1],
                 x.in.y.terr = x.in.y.terr,
                 x.in.y.pop = x.in.y.pop, 
                 group = grp.name,
                 x.terr.tot = x.terr.tot,
                 x.pop.tot = x.pop.tot,
                 y.terr.tot = y.terr.tot,
                 y.pop.tot = y.pop.tot,
                 cov.perc.x = sum.cov / sum(as.matrix(xl$coverage), na.rm = T),
                 cov.perc.y = sum.cov / sum(as.matrix(yl$coverage), na.rm = T),
                 stringsAsFactors = F)
    })
  }), recursive = F)
  comp.df <- do.call(rbind, comp.ls)
  
  # Return
  return(comp.df)
}


################################################################################
################################################################################
## Generate Rasters
################################################################################
################################################################################


# Make main raster

## Use HYDE Population Raster for easy combination later
pop.ras <- raster("~/Data/geodata/pop_density_hist/popc_1880AD.asc")
main.ras <- raster("~/Data/geodata/pop_density_hist/popc_1880AD.asc")

## Crop
pop.ras <- raster::crop(pop.ras, extent(ext.poly))
main.ras <- raster::crop(main.ras, extent(ext.poly))

## Rasterize extent
main.ras <- fasterize(ext.poly, main.ras,
                 field = "id", 
                 fun = "first",
                 background = NA)
main.ras[main.ras == 1] <- 0

# Export to cluster
exp.vec <- c("eth.map.sf", "main.ras", "pop.ras")
clusterExport_fast(cl, exp.vec)

# Comparisons of all maps

## All groups
all.groups <-  names(sort(table(eth.map.sf$link), decreasing = T))
all.groups <- all.groups[!all.groups %in% c("", "NA", NA)]

## Drop all parent groups / conglomerates
all.groups <- all.groups[!grepl("|", all.groups, fixed = T)]

## Comparison
print("COMPARE ALL")
all.comp.ls <- foreach(group = all.groups,
                    .packages = c("raster", "fasterize", "sf", "sp", "rgdal", "rgeos", "dplyr"),
                    .noexport = exp.vec,
                    .options.multicore = list(preschedule = FALSE)) %dopar% {
                      print(group)
                      compare_polys(ras = main.ras, 
                                    pop.ras = pop.ras, 
                                    group.polys.df = eth.map.sf,
                                    grp.name = group)
                    }

all.comp.df <- do.call(rbind, all.comp.ls)

# Save
saveRDS(all.comp.df, file.path(heg.final.dir, "compare_heg_df.rds"))

## Stop cluster
stopCluster(cl)
rm(cl)
