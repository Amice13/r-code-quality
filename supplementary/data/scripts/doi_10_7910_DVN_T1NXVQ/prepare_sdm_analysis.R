###############################
# PREPARE SDM ANALYSIS DATA
###############################

prep_sdm_analysis <- function(points.df){
  # GeoSDM
  gsdm <- readRDS("data/geography/geosdm_subset/gsdm.rds")
  sdm.claims <- read.csv("data/geography/geosdm_subset/sdm_claims.csv")
  
 
  # Countries in cut_yrs
  all.cshp <- cshp(useGW = FALSE)
  all.cshp <- as(all.cshp, "Spatial")

  # Ethnic groups from GREG for SEs
  greg.shp <- readOGR(file.path("data/geography/GREG"), "GREG")
  points.df$greg.id <- unlist(apply(gContains(greg.shp,
                                              SpatialPoints(points.df[, c("x","y")]), byid = T), 1, 
                                    function(x){which(x)[1]}))
  
  # Raster Data
  
  ## Population
  pop.yrs <- seq(1910, 2000,by = 10)
  pop.vx <- velox(stack(file.path("data", "geography", "pop_density_hist", paste0("popc_", pop.yrs, "AD.asc"))))
  pts.pop <- pop.vx$extract_points(SpatialPoints(points.df[, c("x","y")]))
  colnames(pts.pop) <- paste0("pop",pop.yrs )
  points.df <- cbind(points.df, pts.pop)
  rm(pop.vx)
  
  ## Terrain
  fao.keys <- load_fao_keys()
  fao.crops <- c(fao.keys$crop[fao.keys$type == "terrainresources"], "temperaturemean", "precipitation")
  fao.vx <- velox(load_fao_misc(crop = fao.crops))
  pts.fao <- fao.vx$extract_points(SpatialPoints(points.df[, c("x","y")]))
  colnames(pts.fao) <- fao.crops
  points.df <- cbind(points.df, pts.fao)
  rm(fao.vx)
  
  # Point years
  years <- 1946:2016
  sdm.years <- years[years %in% 1946:2012]
  points.yrs.df <- cbind(points.df[rep(seq_len(nrow(points.df)), each = length(years)),],
                         year = rep(years, nrow(points.df)))
  
  
  # Intersect
  
  ## Countries
  
  ### Interset
  int.mat <- gContains(all.cshp,
                       SpatialPoints(points.df[, c("x","y")]),
                       byid = T)
  int.ls <- apply(int.mat, 1, which)
  
  ### Distance to border
  dist.bord.mat <- do.call(cbind, lapply(seq_along(all.cshp), function(x){
    dist <- rep(NA, nrow(points.df))
    if(any(int.mat[, x])){
      cow.line <- as(all.cshp[x,], "SpatialLines")
      dist[int.mat[, x]] <- dist_km(SpatialPoints(points.df[int.mat[, x], c("x","y")]), cow.line)
    } 
    dist
  }))
  dist.bord.ls <- lapply(seq_along(int.ls), function(x){dist.bord.mat[x, int.ls[[x]]]})
  
  ### Distance to capital
  dist.cap.mat <- do.call(cbind, lapply(seq_along(all.cshp), function(x){
    dist <- rep(NA, nrow(points.df))
    if(any(int.mat[, x])){
      cap <- SpatialPoints(all.cshp@data[x, c("caplong", "caplat"), drop = F])
      dist[int.mat[, x]] <- dist_km(SpatialPoints(points.df[int.mat[, x], c("x","y")]), cap)
    }
    dist
  }))
  dist.cap.ls <- lapply(seq_along(int.ls), function(x){dist.cap.mat[x, int.ls[[x]]]})
  
  
  
  ## # Disentangle
  pts.cow <- do.call(rbind, lapply(seq_along(int.ls), function(i){
    ids = int.ls[[i]]
    these.years <- lapply(ids, function(j){
      s = ifelse(grepl("-01-01", as.character(all.cshp$start[j])), 
                 as.numeric(substr(as.character(all.cshp$start[j]), 1, 4)), 
                 as.numeric(substr(as.character(all.cshp$start[j]), 1, 4)) + 1)
      e = min(max(years), as.numeric(substr(as.character(all.cshp$end[j]), 1, 4)))
      if(s > e){
        NULL
      } else {
        s:e
      }
    })
    data.frame(pts.id = rep(i, length(unlist(these.years))),
               year = unlist(these.years),
               cshp.id = rep(ids, sapply(these.years, length)),
               cap.dist = rep(dist.cap.ls[[i]], sapply(these.years, length)),
               border.dist = rep(dist.bord.ls[[i]], sapply(these.years, length)),
               all.cshp@data[rep(ids, sapply(these.years, length)),],
               stringsAsFactors = F)
  }))
  stopifnot(all(!duplicated(pts.cow[, c("pts.id", "year")])))
  points.yrs.df <- join(points.yrs.df, pts.cow,
                        by = c("pts.id","year"), type = "left", match = "first")
  
  ### Crossing river?
  
  #### Make Lines between points -- Capitals
  lines <- lapply(seq_along(all.cshp), function(x){
    if(any(int.mat[, x])){
      # print(x)
      cap <- all.cshp@data[x, c("caplong", "caplat")]
      do.call(rbind, lapply(which(int.mat[, x]), function(p){
        SpatialLinesDataFrame(SpatialLines(list(Lines(list(Line(matrix(as.numeric(c(cap, points.df[p, c("x","y")])), nrow = 2, byrow = T))), 
                                                      ID = as.character(paste0(p, ".", x))))),
                              data.frame(pts.id = points.df$pts.id[p],
                                         cshp.id = x),
                              match.ID = F)
      }))
    } else {
      NULL
    }
  })
  lines[sapply(lines, is.null)] <- NULL
  lines <- do.call(rbind, lines)
  
  #### Intersect with rivers
  diff_river <- line2rivers(sl = lines, min.river.size = 1, 
                            predictor.data.path = "data")
  
  ### Elevation along line
  
  #### Compute elevation profile
  edge.elev.ls <- line2elevprofile(sl = lines, 
                                   elev_rast_path = file.path("data", "geography/elevation/globe_30sec.tif"),
                                   sample_res = 2 * .00833333 ## 2 x resolution of elevation raster
  )
  
  #### Encode 0s instead of missings (in sea)
  edge.elev.ls <- lapply(edge.elev.ls, function(x){
    x[is.na(x)] <- 0
    x
  })
  
  #### Calculate reasonable measure: sum of absolute differences, setting missings (i.e. water) to 0
  sum_abs_diff <- function(x){
    sum(abs(diff(x)))
  }
  lines$elevdiff <- unlist(lapply(edge.elev.ls, sum_abs_diff)) / 1000
  lines$elevmean <- unlist(lapply(edge.elev.ls, mean, na.rm = T)) / 1000
  lines$elevsd <- unlist(lapply(edge.elev.ls, sd, na.rm = T)) / 1000
  lines$elevmax <- unlist(lapply(edge.elev.ls, max, na.rm = T)) / 1000
  lines$elevmin <- unlist(lapply(edge.elev.ls, min, na.rm = T)) / 1000
  
  ### Join and save
  points.yrs.df <- join(points.yrs.df, 
                        data.frame(lines@data, 
                                   diff.river = diff_river, 
                                   any_river = as.numeric(diff_river > 1) ),
                        by = c("pts.id","cshp.id"), type = "left", match = "first")
  

  ## SDM
  int.ls <- apply(gContains(gsdm,
                            SpatialPoints(points.df[, c("x","y")]),
                            byid = T), 1, which)
  pts.sdm <- do.call(rbind, lapply(seq_along(int.ls), function(i){
    ids = int.ls[[i]]
    these.sdm <- gsdm@data[ids, , drop = F]
    these.sdm <- join(sdm.claims, these.sdm, 
                      by = c("ccode","groupid"), type = "inner", match = "all")
    sdm.mat <- do.call(cbind, lapply(1:4, function(c){
      as.numeric(sdm.years %in% these.sdm$year[these.sdm$claim == c])
    }))
    colnames(sdm.mat) <- paste0("sdmclaim.", 1:4)
    data.frame(pts.id = i,
               year = sdm.years,
               sdm.mat,
               all.claims = rowSums(sdm.mat),
               stringsAsFactors = F)
  }))
  points.yrs.df <- join(points.yrs.df, pts.sdm,
                        by = c("pts.id","year"), type = "left", match = "first")
  
  
  
  ## GeoEPR Secessionist conflict
  
  # Load
  epr.sec.spdf <- readOGR(dsn = "data/geography/EPR/epr_sec.geojson", 
                          layer = "epr_sec")
  epr.sec.df <- read.csv("data/geography/EPR/epr_sec.csv")
  
  ### Intersect and build time series
  int.ls <- apply(gContains(epr.sec.spdf,
                            SpatialPoints(points.df[, c("x","y")]),
                            byid = T), 1, which)
  pts.epr <- do.call(rbind, lapply(seq_along(int.ls), function(i){
    ids = int.ls[[i]]
    these.epr <- do.call(rbind, lapply(ids, function(x){
      these.epr <- epr.sec.spdf@data[x, , drop = F]
      epr.sec.df[epr.sec.df$cowgroupid  == these.epr$groups_cowgroupid &
                   as.Date(paste0(epr.sec.df$year, "-01-01")) >= as.Date(these.epr$startdate) &
                   as.Date(paste0(epr.sec.df$year, "-01-01")) <= as.Date(these.epr$enddate), , drop = F]
    }))
    
    data.frame(pts.id = i,
               year = years,
               terrconflict = as.numeric(years %in% these.epr$year),
               stringsAsFactors = F)
  }))
  
  pts.epr <- aggregate.data.frame(pts.epr[, "terrconflict", drop = F],
                                  pts.epr[, c("pts.id", "year")], FUN = max)
  
  points.yrs.df <- join(points.yrs.df, pts.epr,
                        by = c("pts.id","year"), type = "left", match = "first")


  ## Deep Lag w/ Abramson
  
  ### Set Abramson Meta
  abram.yrs <- c(seq(1100, 1700, by = 100), 1790)
  abram.path <- "/nastac/data/abramsonmax"
  
  ### Same Abramson State as capital
  same.histstate <- do.call(cbind, lapply(abram.yrs, function(y){
    print(y)
    
    # Load
    if(file.exists(file.path(abram.path, paste0(y, "_poly.shp")))){
      shp <- readOGR(abram.path,
                     paste0(y, "_poly"))
    } else {
      shp <- readOGR(abram.path,
                     paste0("poly_", y))
    }
    
    # Intersections
    
    ## points
    e.pts <- unlist(apply(gContains(shp,
                                    SpatialPoints(points.df[, c("x","y")]),
                                    byid = T), MARGIN = 1, function(x){
                                      x <- which(x)
                                      if(length(x) == 0){ NA } else {x[1]}
                                    }))
    
    
    ## Capitals
    e.cap <- unlist(apply(gContains(shp,
                                    SpatialPoints(all.cshp@data[, c("caplong","caplat")]),
                                    byid = T), MARGIN = 1, function(x){
                                      x <- which(x)
                                      if(length(x) == 0){ NA } else {x[1]}
                                    }))
    
    
    # Return data
    as.numeric(rep(e.pts, each = length(years)) ==
                 e.cap[points.yrs.df$cshp.id])
  }))
  
  
  ### Summarize
  points.yrs.df$abramstate.hist <- rowSums(same.histstate, na.rm = T) / 
    apply(same.histstate, 1, function(x){sum(!is.na(x))})
  
  
  ## Same Watershed
  
  ### Load
  basin.shp <- load_hydroshed_polys(type = "standard", continent = "eu", 
                                    level = "07", 
                                    hydro.path = file.path("data", "geography/hydrosheds"))
  
  ### points
  e.pts <- unlist(apply(gContains(basin.shp,
                                  SpatialPoints(points.df[, c("x","y")]),
                                  byid = T), MARGIN = 1, function(x){
                                    x <- which(x)
                                    if(length(x) == 0){ NA } else {x[1]}
                                  }))
  
  
  ### Capitals
  e.cap <- unlist(apply(gContains(basin.shp,
                                  SpatialPoints(all.cshp@data[, c("caplong","caplat")]),
                                  byid = T), MARGIN = 1, function(x){
                                    x <- which(x)
                                    if(length(x) == 0){ NA } else {x[1]}
                                  }))
  
  ### Difference
  points.yrs.df$watershed.diff <- ifelse(!is.na(e.pts), 0 , NA)
  for(l in as.numeric(7):1){
    pid <- substr(basin.shp$PFAF_ID[e.pts], 1, l)
    cid <- substr(basin.shp$PFAF_ID[e.pts], 1, l)
    points.yrs.df$watershed.diff <- ifelse(pid != cid[points.yrs.df$cshp.id],
                                           points.yrs.df$watershed.diff + 1, 
                                           points.yrs.df$watershed.diff)
  }
  
  
  ## Ethnicity
  
  ### Load data
  eth.meta.df <- read.csv("data/geography/HEG/heg_meta.csv")
  heg.shp <- readOGR("data/geography/HEG/heg_full.geojson",
                     paste0("heg_full"))
  
  ### Same ethnicity as capital
  diff.ethcap <- do.call(cbind, lapply(seq_len(nrow(eth.meta.df)), function(e){
    print(paste(e , "of", nrow(eth.meta.df)))
    # This map
    this.map <- eth.meta.df[e, , drop = F]
    
    # Get map
    shp <- heg.shp[heg.shp$file_name == this.map$file_name, ] 
    shp <- gBuffer(shp, width = 0, byid = T)
    
    # Intersections
    
    ## points
    e.pts <- lapply(asplit(gContains(shp,
                                    SpatialPoints(points.df[, c("x","y")]),
                                    byid = T),1), function(x){
                                      x <- unique(shp$group[which(x)])
                                    })
    
    
    ## Capitals
    e.cap <- lapply(asplit(gContains(shp,
                                    SpatialPoints(all.cshp@data[, c("caplong","caplat")]),
                                    byid = T),1), function(x){
                                      x <- unique(shp$group[which(x)])
                                    })
    
    
    # Return data
    e.pts.yrs <- rep(e.pts, each = length(years))
    sapply(seq_along(e.pts.yrs), function(i){
      v1 = e.pts.yrs[[i]]
      v2 = e.cap[[points.yrs.df$cshp.id[i]]]
      if(is.null(v1) | is.null(v2)){
        NA
      } else {
        max(c(mean(!v1 %in% v2), mean(!v2 %in% v1)))
      }
    })
  }))
  
  
  ### Finalize
  
  #### Time variant version // always include GREG if post-1963
  points.yrs.df$diff.capeth.tv <- rowMeans(do.call(cbind, lapply(seq_len(nrow(eth.meta.df)), function(e){
    ifelse(points.yrs.df$year >= eth.meta.df$year_data_end[e] & 
             points.yrs.df$year < eth.meta.df$year_data_end[e] + 50, 
           
           diff.ethcap[,e], NA)
  })), na.rm = T)
  
  #### pre-1946 map variant version
  points.yrs.df$diff.capeth.1946 <- rowMeans(do.call(cbind, lapply(seq_len(nrow(eth.meta.df)), function(e){
    if(eth.meta.df$year_data_end[e] < 1946 & eth.meta.df$year_data_end[e] > 1946-50){
      diff.ethcap[,e]
    } else {
      rep(NA, nrow(diff.ethcap))
    }
  })), na.rm = T)
  
  #### pre-1886 map variant version
  points.yrs.df$diff.capeth.1886 <- rowMeans(do.call(cbind, lapply(seq_len(nrow(eth.meta.df)), function(e){
    if(eth.meta.df$year_data_end[e] < 1886 & eth.meta.df$year_data_end[e] > 1886-50){
      diff.ethcap[,e]
    } else {
      rep(NA, nrow(diff.ethcap))
    }
  })), na.rm = T)
  

  #### Status in 1946
  
  ## IDs
  points.yrs.df$cow.yrs <- as.numeric(as.factor(paste0(points.yrs.df$cowcode, ".", points.yrs.df$year)))
  points.yrs.df$grp.yrs <- as.numeric(as.factor(paste0(points.yrs.df$greg.id, ".", points.yrs.df$year)))
  
  
  ## Population reshape
  points.yrs.df$pop <- NA
  for(y in seq(1910, 2000, by = 10)){
    points.yrs.df$pop[points.yrs.df$year >= y] <-
      points.yrs.df[points.yrs.df$year >= y, paste0("pop", y)]
  }
  
  ## Colnames rename
  colnames(points.yrs.df) <- gsub(" ", ".", colnames(points.yrs.df))
  
  
  ## Any claim
  points.yrs.df$any.claim <- ifelse(points.yrs.df$all.claims > 0 , 1, 0)
  
  ## International claim
  points.yrs.df$int.claim <- ifelse(points.yrs.df$sdmclaim.3 == 1 | points.yrs.df$sdmclaim.4 == 1 , 1, 0)
  
  # ## Ethnic difference
  # points.yrs.df$same.capeth.1946 <- 1-points.yrs.df$diff.capeth.1946
  # points.yrs.df$same.capeth.tv <- 1-points.yrs.df$diff.capeth.tv
  
  
  # Survival model setup
  ## Drop if country is missing
  points.yrs.df<- points.yrs.df[!is.na(points.yrs.df$cowcode),]
  
  ## Country-point spells
  points.yrs.df <- points.yrs.df[order(points.yrs.df$pts.id, points.yrs.df$year),]
  points.yrs.df$spell.id <- NA
  points.yrs.df$yrs.since.start <- NA
  points.yrs.df$sec.onset <- NA
  points.yrs.df$irr.onset <- NA
  points.yrs.df$tco.onset <- NA
  
  spell.id = 0
  flag.onset <- paste0(c("sec", "irr", "tco"),".onset")
  flag.incidence <- c("sdmclaim.3", "sdmclaim.4", "terrconflict")
  for(i in seq_len(nrow(points.yrs.df))){
    if(i == 1){
      yr.count = 0
      spell.id = spell.id + 1
      had.onset = c(F, F, F)
    } else if(points.yrs.df$cowcode[i] != points.yrs.df$cowcode[i-1] |
              points.yrs.df$pts.id[i] != points.yrs.df$pts.id[i-1]) {
      yr.count = 0
      spell.id = spell.id + 1
      had.onset = c(F, F, F)
    } else {
      yr.count = yr.count + 1
    }
    points.yrs.df$spell.id[i] <- spell.id
    points.yrs.df$yrs.since.start[i] <- yr.count
    
    for(c in 1:3){
      if(!had.onset[c]){
        if(is.na(points.yrs.df[i, flag.incidence[c]])){
          had.onset[c] <- T
          points.yrs.df[i, flag.onset[c]] <- NA
        } else if(points.yrs.df[i, flag.incidence[c]] == 1){
          had.onset[c] <- T
          points.yrs.df[i, flag.onset[c]] <- 1
        } else {
          points.yrs.df[i, flag.onset[c]] <- 0
        }
      }
    }
  }
  
  ## Onset of any SDM
  points.yrs.df$sdm.onset <- ifelse(points.yrs.df$sec.onset == 1 | points.yrs.df$irr.onset == 1, 
                                    1, ifelse(!is.na(points.yrs.df$sec.onset) & !is.na(points.yrs.df$irr.onset),
                                              0, NA))
  
  
  ## Breaks from country
  
  ### End of spell = border change
  spell.max.yr <- aggregate.data.frame(list(spell.max = points.yrs.df$year),
                                       points.yrs.df[, "spell.id", drop = F], FUN = max)
  points.yrs.df <- join(points.yrs.df, spell.max.yr,
                        by = "spell.id", type = "left", match = "first")
  points.yrs.df$border.change <- ifelse(points.yrs.df$year == points.yrs.df$spell.max &
                                        points.yrs.df$year != max(points.yrs.df$year, na.rm = T),
                                      1, 0)
  
  ### Break away / secession = borderchange with new Cowcode
  points.yrs.df$breaks.away <- points.yrs.df$border.change
  points.yrs.df <- join(points.yrs.df[, colnames(points.yrs.df) != "new.cowcode"], 
                        data.frame(pts.id = points.yrs.df$pts.id,
                                   year = points.yrs.df$year - 1,
                                   new.cowcode = points.yrs.df$cowcode),
                        type = "left", match = "first", by = c("pts.id","year"))
  for(y in unique(points.yrs.df$year[points.yrs.df$year != min(points.yrs.df$year)])){
    points.yrs.df$breaks.away[points.yrs.df$year == y] <- 
      ifelse(points.yrs.df$new.cowcode[points.yrs.df$year == y] %in% points.yrs.df$cowcode[points.yrs.df$year == y], 0,
             points.yrs.df$breaks.away[points.yrs.df$year == y])
  }
  
  ### Correct Unification of Germany in 1990 -- misleading cowcodes
  points.yrs.df$breaks.away[points.yrs.df$year == 1990 &
                              points.yrs.df$cowcode %in% c(260, 265)] <- 0
  
  ## Stable clusters
  clust <- aggregate.data.frame(list(cluster.id = points.yrs.df$cowcode),
                                points.yrs.df[, "pts.id", drop = F], 
                                FUN = paste, collapse = ".")
  clust$cluster.id <- as.numeric(as.factor(clust$cluster.id))
  points.yrs.df <- join(points.yrs.df, clust,
                        by = "pts.id", type = "left", match = "first")
  
  
  # Survival preparation
  
  ## End of the year since start by country-period & point
  # points.yrs.df$cshp.yrs.since <- as.numeric(factor(paste0(points.yrs.df$cshp.id, ".", points.yrs.df$yrs.since.start)))
  # points.yrs.df$pts.yrs.since <- as.numeric(factor(paste0(points.yrs.df$pts.id, ".", points.yrs.df$yrs.since.start)))
  points.yrs.df$yrs.since.end <- points.yrs.df$yrs.since.start + 1
  
  # Drop variables to save space
  drop.vars <- c( "sdmclaim.1" ,  "sdmclaim.2" , "sdmclaim.3","sdmclaim.4","all.claims"  , "terrconflict" ,
                  "any.claim" , "int.claim" ,  "sec.onset"  ,      "irr.onset"  ,  "b_def" ,  "fid" ,
                  paste0("pop", seq(1880, 2000, by = 10)),
                  "owner" , "country_name"  ,   "start"    ,        "end"       ,       "status", "spell.max",  "border.change" )
  points.yrs.df <- points.yrs.df[, !colnames(points.yrs.df) %in% drop.vars]
  
  # Return
  return(points.yrs.df)
}
