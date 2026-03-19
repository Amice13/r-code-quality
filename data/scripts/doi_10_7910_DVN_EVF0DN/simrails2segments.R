# load spatial libraries
library(raster)
library(sf)
library(lwgeom)
library(sp)
library(countrycode)
library(RPostgres)
library(dplyr)
library(geojsonio)
library(exactextractr)
library(fasterize)
library(igraph)
library(tictoc)

# load parallelization
library(foreach)
library(doParallel)
library(parallel)
ncores <- detectCores()-2

library(here)
sf_use_s2(FALSE)

### load r_shapes data
rails <- st_read(here("input_data","lines_sim.json"))


# Years
rail.years <- 1834:1922
yrs <- 1816:2017

t0 <- Sys.time()
cl <- parallel::makeForkCluster(ncores)
registerDoParallel(cl)


# Load data in parallel (faster)
res <- clusterEvalQ(cl, {
  segments.ls <- readRDS(here("analysis_data","segments_sf_ls_tv.rds"))
  rails <- st_read(here("analysis_data","lines_sim.json"))
  NULL
})

# Run loop
rails.yearly <- foreach(yr=yrs,.combine=rbind,
                        .noexport = c("rails", "segments.ls")) %dopar% {
                          ## Select segments
                          segments.yr <- segments.ls[[paste(yr)]]
                          segments.yr <- st_make_valid(segments.yr)
                          
                          ## Select rails
                          rail.year <- rail.years[which.min(abs(rail.years - yr))]
                          rails.yr <- rails[rails$railyear<rail.year,]
                          
                          ## Intersect
                          inters <- st_intersection(segments.yr,rails.yr)
                          
                          ## Compute length by intersection
                          inters$rail_length_km <- as.numeric(st_length(inters))/1000
                          
                          ## Aggregate to segment w/rail
                          out <- st_drop_geometry(inters) %>% group_by(seg_id,year) %>% summarise(
                            rail_length_km = sum(rail_length_km)
                          ) %>% ungroup()
                          out$rail_any_yn <- 1
                          
                          ## Add to all segments
                          out.add <- unique(st_drop_geometry(segments.yr[,c("seg_id","year")]))
                          out.add <- out.add[!out.add$seg_id%in%out$seg_id,]
                          out.add$rail_length_km <- 0
                          out.add$rail_any_yn <- 0
                          out.df <- rbind(as.data.frame(out),out.add)
                          
                          ## Return
                          return(out.df)
                        }

stopImplicitCluster()

parallel::stopCluster(cl)
rm(cl)

t1 <- Sys.time()
t1-t0 
names(rails.yearly)[3:4] <- c("rail_length_km_sim","rail_any_yn_sim")

saveRDS(rails.yearly,here("analysis_data","simrails2segments_tv.rds"))


