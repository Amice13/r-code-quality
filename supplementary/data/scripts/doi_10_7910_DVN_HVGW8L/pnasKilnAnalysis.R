################################################################################
# Purpose: Conduct spatial assessment of brick kilns
# Input files: kiln locations, spatial data on schools, health facilities, 
#              railways, and protected areas, satellite PM data (pre-processed),
#              population data (pre-processed)
################################################################################

################################################################################
##############################        Set Up            ########################
################################################################################

# load required libraries
library(raster) # to read in raster data
library(sf) # main spatial analysis package
library(rgdal) # necessary underlying spatial package
library(rgeos) # necessary underlying spatial package
library(RColorBrewer) # for color palettes
library(colorspace) # more color palettes
library(ggsci) # for color palletes
library(ggpubr) # to arrange plots
library(xtable) # for exporting tables to latex
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(geosphere) # for bearing
library(readxl)
library(units)
library(grid)
library(ggrepel) # for labeling
library(leaflet) # for dynamic mapping
library(viridis)
library(stargazer) # for exporting regression results
library(broom) # for making tidy dataframes of regression results
library(dotwhisker) # for plotting regression coefficients
library(data.table)
library(scales) # for different scales in plotting
library(sandwich) # for robust SE
library(clubSandwich) # for cluster robust SE
library(sjPlot)
library(ggeffects)
library(ggspatial) # for annotation scale bar in maps
library(patchwork) # for combining plots
library(imputeTS) # for time series imputation
library(lubridate)
library(tidyverse) 

# Set root folder
if (Sys.info()[["user"]] == "ninabrooks"){
    rootFolder <- "~/Dropbox/pnas-kiln/spatial-analysis/"
    lib.loc <- .libPaths()
}

# set directories
raw <- paste0(rootFolder, "data/raw/")
clean <- paste0(rootFolder, "data/clean/")
functions <- paste0(rootFolder, "scripts/functions/")
output <- paste0(rootFolder, "output/")


################################################################################
###############################      Load  Functions     #######################
################################################################################
source(paste0(functions, "closestKiln.R"))
source(paste0(functions, "closestObj.R"))
source(paste0(functions, "checkViolationDist.R"))


# define not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

################################################################################
###############################   Load Data     ################################
################################################################################
# reproject everything to EPSG: 32646 for distance calculations
# coordinates from model
kilns <- read_csv(paste0(clean, "model_kilns.csv"))  %>%
    mutate(
        model_kiln_ID = row_number()
    ) %>%
    sf::st_as_sf(coords = c("long", "lat"), 
                 crs = 4326) %>%
    sf::st_transform(crs = 32646) 

kilns$X <- st_coordinates(kilns)[,1]
kilns$Y <- st_coordinates(kilns)[,2]
kilnMat <- as.matrix(data.frame("X" = kilns$X, "Y" = kilns$Y)) # do we need this?

# load in other spatial data and re-project
railways <- st_read(paste0(spatial_input, 
                           "railways/bgd_trs_railways_lged.shp")) %>%
    mutate(id = 1:n()) %>%
    st_transform(32646)

healthsites <- st_read(paste0(spatial_input,
                              "health_facilities/bgd_poi_healthfacilities_lged.shp")) %>%
    mutate(id = 1:n()) %>%
    st_transform(32646)

edufac <- st_read(paste0(spatial_input,
                         "edu_facilities/bgd_poi_educationfacilities_lged.shp")) %>%
    mutate(id = 1:n()) %>%
    st_transform(32646)

forests <- st_read(paste0(spatial_input,
                          "forests/bgd_phy_forestnaturalparks_lged.shp")) %>%
    mutate(id = 1:n()) %>%
    st_transform(32646)

pa <- st_read(paste0(spatial_input,
                     "PAs/WDPA_Nov2018_BGD-shapefile-polygons.shp")) %>%
    mutate(id = 1:n()) %>%
    st_transform(32646) %>%
    filter(DESIG %!in% c("Marine Park", "Marine Reserve")) %>% # removing PAs in the bay of bengal
    filter(STATUS == "Designated") # keep only officially designated PAs

# district shapefiles of Bangladesh
bd2 <- st_read(paste0(BD_shp, "bgd_admbnda_adm2_bbs_20180410.shp")) %>%
    st_transform(32646) %>%
    dplyr::select(Shape_Leng, Shape_Area, ADM2_EN, ADM2_PCODE, ADM1_EN) %>%
    mutate(district = as.character(ADM2_EN))

# DOE data on brick kilns
doe <- read_excel(paste0(input, "Brick Last Update.xlsx"), 
                  sheet = "January 2019") %>%
    filter(!is.na(DISTRICT)) %>%
    dplyr::select(-Sl.) %>%
    rename(district = DISTRICT,
           totalDOE = `NO OF BRICK KILN`,
           hasClearance = `Clearance: Yes`,
           noClearance = `Clearance: No`,
           fck = `Fixed (80-120' ft)Chimney`,
           zigzag = Zigzag,
           hhk = `Hybrid Hoffman`,
           tunnel = `Automatic/ Tunnel Kiln`,
           alt = `Alternative Tech`,
           totalEnvFriendly = `Total Environment Friendly Technology`,
           pctEnvFriendly = Percentage) %>%
    mutate(district = str_to_title(district), # making sure district names match with the shape file
           district = ifelse(district == "Dhaka Metro", "Dhaka", district),
           district = ifelse(district == "Bandarbon", "Bandarban", district),
           district = ifelse(district == "Brahmanbaria", "Brahamanbaria", district),
           district = ifelse(district == "Cox'sbazar", "Cox's Bazar", district),
           district = ifelse(district == "Jhalakatir", "Jhalokati", district),
           district = ifelse(district == "Moulavibazar", "Maulvibazar", district),
           district = ifelse(district == "Gopalgonj", "Gopalganj", district),
           district = ifelse(district == "Hobiganj", "Habiganj", district),
           district = ifelse(district == "Jhalakati", "Jhalokati", district),
           district = ifelse(district == "Netrokona", "Netrakona", district),
           district = ifelse(district == "Sunamgonj", "Sunamganj", district)) %>%
    group_by(district) %>%
    summarise_all(sum) %>% # collapsing the 2 dhaka entries
    ungroup() %>%
    mutate(totalTrad = fck + zigzag) 

# calculate total number of kilns
doe %>% dplyr::select(fck:alt) %>% rowSums(na.rm=TRUE) -> doe$totalKilns

# merge with district shapefile
doe <- left_join(doe, bd2, by = "district") %>% 
    st_as_sf()

# kiln coords from CASE report
caseKilns <- read_excel(paste0(input, 
                               "GPS coordinates_Brick Kilns_Bangladesh.xlsx")) %>%
    filter(TYPE == "Brick Kiln") %>%
    filter(STATUS == "Active") %>%
    mutate(
        gov_kiln_ID = row_numer()
    )
    sf::st_as_sf(coords = c("X_Coordina", "Y_Coordina"), 
                 crs = 4326) %>%
    st_transform(32646)



# population exposure datasets (output from constructKilnPopData.R)
popKilnDF <- readRDS(paste0(clean, "popKilnDF.Rds"))
pregKilnDF <- readRDS(paste0(clean, "pregKilnDF.Rds"))

# PM data set (output from constructKilnPMData.R)
dhakaEmbassyDF <- readRDS(paste0(clean, "dhakaEmbassyDF.Rds"))


################################################################################
#######################  1.  Comparison to DOE  ################################
################################################################################
kiln_count <- st_intersects(doe$geometry, kilns$geometry)
doe$coord_predictions <- sapply(1:length(kiln_count), 
                                function(x) length(kiln_count[[x]])) 

# doe total traditional kilns
doe %>%
    summarise(total = sum(totalTrad))

# count district-level discrepancies b/e DoE & model
doe <- doe %>%
    dplyr::select(ADM1_EN, district, fck, zigzag,
                  totalTrad, coord_predictions) %>%
    rename(division = ADM1_EN) %>%
    mutate(
        diff = coord_predictions - totalTrad
        ) 

# check for kilns in banned districts
kilns_in_restricted_districts <- doe %>%
    dplyr::select(district, coord_predictions, totalTrad) %>%
    filter(district %in% c("Bandarban", "Khagrachhari", "Rangamati")) %>%
    rename(District = district,
           "Model Predictions" = coord_predictions,
           DOE = totalTrad)


# merge model predictions to district boundaries
districtKilns <- st_intersects(bd2, kilns)
districtKilnCount <- sapply(1:nrow(bd2), 
                            function(x) length(districtKilns[[x]]))
bd2$kilnCount <- districtKilnCount

bd2 <- bd2 %>% 
    st_transform(4326)

# figure 4a: density of kilns by district
districtMap <- ggplot(data = bd2) + 
    geom_sf(aes(fill = kilnCount), color = NA) +
    scale_fill_distiller(palette = "Reds", direction = 1) +
    labs(fill = "Kilns per District",
         title = "Kilns per District") + 
    guides(fill = guide_colourbar(barwidth = 8, barheight = 0.5)) +
    theme(strip.background = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.key = element_blank(), 
          legend.background=element_blank(),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 8),
          legend.position = "bottom") 
ggsave(districtMap, filename = paste0(figures, "districtKilnMap.pdf"), 
       dpi = 320,
       width = 4, height = 4)

# figure 4b: difference between doe  model by district
doe <- doe %>%
    mutate(
        restricted = ifelse(district %in% c("Bandarban", 
                                            "Khagrachhari",
                                           "Rangamati"),
                            "Kilns Banned","Kilns Allowed"),
        restricted = factor(restricted, 
                            levels = c("Kilns Banned","Kilns Allowed"), 
                            labels = c("Kilns Banned","Kilns Allowed"))
        ) %>%
    st_transform(4326) # reproject for plotting


diffMap <- ggplot(data = doe) + geom_sf(aes(fill = diff, 
                                            color = restricted, 
                                            linetype = restricted), 
                                        size = 0.55) +
    scale_fill_distiller(palette = "RdBu", 
                         limits = c(-1,1)*max(abs(doe$diff)),
                         direction = -1) +
    labs(fill = "Discrepancies with GoB Data",
         title = "Discrepancies with Government Data (by District)",
         lty = "") +
    scale_color_manual(values = c("black", NA)) +
    scale_linetype_manual(breaks = c("Kilns Banned"),
                          values = c("dashed", "solid")) +
    guides(fill = guide_colourbar(barwidth = 8, barheight = 0.5),
           lty = guide_legend(override.aes = list(lty = "dashed", 
                                                  fill = NA)),
           color = F) +
    theme(strip.background = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 8),
          legend.key = element_blank(),
          legend.background=element_blank(),
          legend.position = "bottom", 
          legend.box="vertical", 
          legend.margin=margin()) 
ggsave(diffMap, filename = paste0(figures, "discrepanciesMap.pdf"), 
       dpi = 320,
       width = 4, height = 4)


# Compare DOE data on kiln type (shape) to model predictions -------------------
# DOE
st_geometry(doe) <- NULL
doeKilnType <- doe %>%
    summarise(zigzagT = sum(zigzag),
              fckT = sum(fck),
              allkilns = zigzagT + fckT,
              zigzag_pct = zigzagT/allkilns,
              fck_pct = fckT/allkilns)

# Hand-validated shape predictions (shown in Fig 1B)
zzT <- 944 + 96 
fckT <- 208 + 375
totalModel <- zzT + fckT

zzT/totalModel       
fckT/totalModel


# Model predictions vs. CASE kilns  --------------------------------------------
# pairwise distances
kilnCASE_dist <- st_distance(kilns, caseKilns)
closestCaseKiln <- apply(kilnCASE_dist, 1, min) # closest model kiln to each CASE kiln
closestCaseKilnID <-  apply(kilnCASE_dist, 1,which.min) 
modelCASE_DF <- data.frame("gov_kiln_ID" = closestCaseKilnID,
                           "dist" = unclass(closestCaseKiln)) %>%
    mutate(model_kiln_ID = 1:n())


# identify closest single matches within 2 km
singleMatches <- modelCASE_DF %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n == 1) %>%
    left_join(modelCASE_DF, by = "gov_kiln_ID") %>%
    filter(dist <= 2000) %>%
    dplyr::select(-n) %>%
    arrange(model_kiln_ID)

# identify closest matches within 2 km when there were multiple matches
closestMultiMatch <- modelCASE_DF %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n >1) %>%
    left_join(modelCASE_DF, by = "gov_kiln_ID") %>%
    group_by(gov_kiln_ID) %>%
    arrange(dist) %>%
    mutate(id = 1:n()) %>%
    arrange(gov_kiln_ID) %>%
    filter(id == 1) %>%
    ungroup() %>%
    dplyr::select(-id, -n) %>%
    filter(dist <= 2000)

bestMatches <- bind_rows(singleMatches, closestMultiMatch)

# identify remaining kilns in each set
modelKilnsRemaining <- kilns[kilns$model_kiln_ID %!in% bestMatches$model_kiln_ID,] 
caseKilnsRemaining <- caseKilns[caseKilns$gov_kiln_ID %!in% bestMatches$gov_kiln_ID,] 

# match again 
secondRoundDist <- st_distance(modelKilnsRemaining, caseKilnsRemaining)
closestCaseKiln2 <- apply(secondRoundDist, 1, min) # closest model kiln to CASE kiln
closestCaseKiln2Loc <-  apply(secondRoundDist, 1, which.min) 
closestCaseKiln2ID <- caseKilnsRemaining[closestCaseKiln2Loc,]$gov_kiln_ID
modelCASE_DF2 <- data.frame("gov_kiln_ID" = closestCaseKiln2ID,
                            "dist" = unclass(closestCaseKiln2)) %>%
    mutate(model_kiln_ID = modelKilnsRemaining$model_kiln_ID)

# identify closest single matches within 2 km
singleMatches2 <- modelCASE_DF2 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n == 1) %>%
    left_join(modelCASE_DF2, by = "gov_kiln_ID") %>%
    filter(dist <= 2000) %>%
    dplyr::select(-n)

# identify closest matches within 2 km when there were multiple matches
closestMultiMatch2 <- modelCASE_DF2 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n >1) %>%
    left_join(modelCASE_DF2, by = "gov_kiln_ID") %>%
    group_by(gov_kiln_ID) %>%
    arrange(dist) %>%
    mutate(id = 1:n()) %>%
    arrange(gov_kiln_ID) %>%
    filter(id == 1) %>%
    ungroup() %>%
    dplyr::select(-id, -n) %>%
    filter(dist <= 2000)

bestMatches <- bind_rows(bestMatches, singleMatches2, closestMultiMatch2)

# identify remaining kilns in each set (3rd round)
modelKilnsRemaining2 <- modelKilnsRemaining[modelKilnsRemaining$model_kiln_ID %!in% 
                                                bestMatches$model_kiln_ID,] 
caseKilnsRemaining2 <- caseKilnsRemaining[caseKilnsRemaining$gov_kiln_ID %!in% 
                                              bestMatches$gov_kiln_ID,] 

# third round of matching
thirdRoundDist <- st_distance(modelKilnsRemaining2, caseKilnsRemaining2)
closestCaseKiln3 <- apply(thirdRoundDist, 1, min) # closest model kiln to each CASE kiln
closestCaseKiln3Loc <-  apply(thirdRoundDist, 1, which.min) 
closestCaseKiln3ID <- caseKilnsRemaining2[closestCaseKiln3Loc,]$gov_kiln_ID
modelCASE_DF3 <- data.frame("gov_kiln_ID" = closestCaseKiln3ID,
                            "dist" = unclass(closestCaseKiln3)) %>%
    mutate(model_kiln_ID = modelKilnsRemaining2$model_kiln_ID)


# identify closest single matches within 2 km
singleMatches3 <- modelCASE_DF3 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n == 1) %>%
    left_join(modelCASE_DF3, by = "gov_kiln_ID") %>%
    filter(dist <= 2000) %>%
    dplyr::select(-n)

# identify closest matches within 2 km when there were multiple matches
closestMultiMatch3 <- modelCASE_DF3 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n >1) %>%
    left_join(modelCASE_DF3, by = "gov_kiln_ID") %>%
    group_by(gov_kiln_ID) %>%
    arrange(dist) %>%
    mutate(id = 1:n()) %>% 
    arrange(gov_kiln_ID) %>%
    filter(id == 1) %>%
    ungroup() %>%
    dplyr::select(-id, -n) %>%
    filter(dist <= 2000)

bestMatches <- bind_rows(bestMatches, singleMatches3, closestMultiMatch3)

# identify remaining kilns in each set (4th round)
modelKilnsRemaining3 <- modelKilnsRemaining2[modelKilnsRemaining2$model_kiln_ID %!in% 
                                                 bestMatches$model_kiln_ID,] 
caseKilnsRemaining3 <- caseKilnsRemaining2[caseKilnsRemaining2$gov_kiln_ID %!in% 
                                               bestMatches$gov_kiln_ID,] 

# fourth round of matching
fourthRoundDist <- st_distance(modelKilnsRemaining3, caseKilnsRemaining3)
closestCaseKiln4 <- apply(fourthRoundDist, 1, min) # closest model kiln to each ground-truth kiln
closestCaseKiln4Loc <-  apply(fourthRoundDist, 1, which.min) 
closestCaseKiln4ID <- caseKilnsRemaining3[closestCaseKiln4Loc,]$gov_kiln_ID
modelCASE_DF4 <- data.frame("gov_kiln_ID" = closestCaseKiln4ID,
                            "dist" = unclass(closestCaseKiln4)) %>%
    mutate(model_kiln_ID = modelKilnsRemaining3$model_kiln_ID)


# identify closest single matches within 2 km
singleMatches4 <- modelCASE_DF4 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n == 1) %>%
    left_join(modelCASE_DF4, by = "gov_kiln_ID") %>%
    filter(dist <= 2000) %>%
    dplyr::select(-n)

# identify closest matches within 2 km when there were multiple matches
closestMultiMatch4 <- modelCASE_DF4 %>%
    group_by(gov_kiln_ID) %>%
    tally() %>%
    filter(n >1) %>%
    left_join(modelCASE_DF4, by = "gov_kiln_ID") %>%
    group_by(gov_kiln_ID) %>%
    arrange(dist) %>%
    mutate(id = 1:n()) %>% 
    arrange(gov_kiln_ID) %>%
    filter(id == 1) %>%
    ungroup() %>%
    dplyr::select(-id, -n) %>%
    filter(dist <= 2000)

bestMatches <- bind_rows(bestMatches, singleMatches4, closestMultiMatch4)

# Fig 3: Venn Diagram Numbers
unmatchedB <- nrow(kilns) - nrow(bestMatches)
unmatchedA <- nrow(caseKilns) - nrow(bestMatches)

summary(bestMatches$dist)
sd(bestMatches$dist)


# identify what type of kiln the unmatchedB are
allKilnID <- kilns$model_kiln_ID
bestMatchesID <- bestMatches$model_kiln_ID
unmatchedB_typeID <- allKilnID %in% bestMatchesID
unmatchedB_type <- kilns[!unmatchedB_typeID,]
table(unmatchedB_type$prediction)
nrow(unmatchedB_type)


################################################################################
####################### 2. Assess Regulations & Population Exposure  ###########
################################################################################
# re-transform
kilns <- st_transform(kilns, crs = 32646) # see if it's needed

# Apply over all objects to get distances
allObjects <- list(list(healthsites, "Health Facilities"), 
                   list(edufac, "Schools"),
                   list(railways, "Railways"),
                   list(forests, "Forests"),
                   list(pa, "Protected Areas"))

distances <- lapply(allObjects, closestKiln) # distance to closest kiln
distance_df <- rbindlist(distances) # do i need this?

# summary statistics of kiln-object distances
meanDist <- sapply(distances, function(x) mean(x[[1]], na.rm = T))
sdDist <- sapply(distances, function(x) sd(x[[1]], na.rm = T))
medDist <- sapply(distances, function(x) median(x[[1]], na.rm = T))
minDist <- sapply(distances, function(x) min(x[[1]], na.rm = T))*1000
maxDist <- sapply(distances, function(x) max(x[[1]], na.rm = T))

# calculate how many entities have kiln within 1, 2, 5, & 10 km
violations <- lapply(distances, function(x)
    lapply(c(1, 2, 5, 10), 
           function(y) checkViolationDist(df = x, dist = y)))

violationsDF <- t(sapply(1:length(distances), 
                         function(x) sapply(1:length(c(1, 2, 5, 10)), 
                                            function(y) violations[[x]][[y]])))
colnames(violationsDF) <- c("1km", "2km", 
                            "5km", "10km")
totals <- data.frame(total = c(nrow(allObjects[[1]][[1]]),
                               nrow(allObjects[[2]][[1]]),
                               nrow(allObjects[[3]][[1]]),
                               nrow(allObjects[[4]][[1]]),
                               nrow(allObjects[[5]][[1]])))

violationsPCT <- data.frame(violationsDF) %>%
    mutate(X1km = unlist(X1km),
           X2km = unlist(X2km),
           X5km = unlist(X5km),
           X10km = unlist(X10km)) %>%
    bind_cols(totals) %>%
    mutate(pct1 = round(100*(X1km / total), 0), 
           pct2 = round(100*(X2km / total), 0),
           pct5 = round(100*(X5km / total), 0),
           pct10 = round(100*(X10km / total), 0)) %>%
    dplyr::select(starts_with("pct"))


# for railways calculate the % of the rail line that is within 1, 5, 10 km of a kiln
# 1. create 1 km buffer around all kilns

kiln1km <- st_buffer(kilns, dist = 1000)
kiln5km <- st_buffer(kilns, dist = 5000)
kiln10km <- st_buffer(kilns, dist = 10000)

# 2. disolve buffers into unified buffer
kiln1km_union <- st_union(kiln1km)
kiln5km_union <- st_union(kiln5km)
kiln10km_union <- st_union(kiln10km)

# 3. intersect rail line with unified buffer
kiln_railway1km <- st_intersection(railways, kiln1km_union)
kiln_railway5km <- st_intersection(railways, kiln5km_union)
kiln_railway10km <- st_intersection(railways, kiln10km_union)


# 4. calculate length of the intersect
kiln_railway_ln1km <- st_length(kiln_railway1km)
kiln_railway_ln5km <- st_length(kiln_railway5km)
kiln_railway_ln10km <- st_length(kiln_railway10km)

# 5. divide length of intersect by total length of rail
railwayPCT_1km <- kiln_railway_ln1km/st_length(railways[railways$id %in% kiln_railway1km$id,])
railwayPCT_1km_mean <- unclass(mean(railwayPCT_1km))

railwayPCT_5km <- kiln_railway_ln5km/st_length(railways[railways$id %in% kiln_railway5km$id,])
railwayPCT_5km_mean <- unclass(mean(railwayPCT_5km))

railwayPCT_10km <- kiln_railway_ln10km/st_length(railways[railways$id %in% kiln_railway10km$id,])
railwayPCT_10km_mean <- unclass(mean(railwayPCT_10km))

railwayPCT <- data.frame(pct1 = railwayPCT_1km_mean*100,
                         pct5 = railwayPCT_5km_mean*100, 
                         pct10 = railwayPCT_10km_mean*100)

# population exposure
popStatsPct <- popKilnDF %>%
    summarise(pct1 = round(100*sum(pop[kilnCount1km >0])/sum(pop), 0),
              pct5 = round(100*sum(pop[kilnCount5km >0])/sum(pop), 0),
              pct10 = round(100*sum(pop[kilnCount10km >0])/sum(pop), 0))

pregStatsPct <- pregKilnDF %>%
    summarise(pct1 = round(100*sum(pop[kilnCount1km >0])/sum(pop), 0),
              pct5 = round(100*sum(pop[kilnCount5km >0])/sum(pop), 0),
              pct10 = round(100*sum(pop[kilnCount10km >0])/sum(pop), 0))


popTable <- bind_rows(popStatsPct, pregStatsPct) 

# Figure 5A: Regulated entity proximity to kiln 
combinedPctExposed <- violationsPCT[, c(1, 3, 4)] %>% # 1, 5, 10 km
    bind_rows(popTable) %>%
    mutate(entity = c("Health Facilities", "Schools", "Railways", "Forests",
                      "Protected Areas", "Total Population", "Pregnancies")) 

# add % of the rail line that is in proximity to kilns
combinedPctExposed[combinedPctExposed$entity == "Railways", 1:3] <- railwayPCT

combinedPctExposed <- combinedPctExposed %>%  
    mutate(entity = factor(entity, 
                           levels = c("Health Facilities", "Schools", 
                                      "Railways", "Forests",
                                      "Protected Areas", "Total Population", 
                                      "Pregnancies"),
                           labels = c("Health Facilities", "Schools", 
                                      "Railways", "Forests",
                                      "Protected Areas", "Total Population",
                                      "Pregnancies"),
                           ordered = T)) %>%
    mutate(pct10 = pct10 - pct5,
           pct5 = pct5 - pct1) %>%
    pivot_longer(cols = c(pct1, pct5, pct10),
                 names_to = "distance") %>%
    mutate(
        distance = ifelse(distance == "pct1", "1 km",
                          ifelse(distance == "pct5", "5 km", "10 km")),
        distance = factor(distance, levels = c("1 km", "5 km", "10 km"), 
                          ordered = T),
        distance = fct_rev(distance),
        entity = fct_rev(entity),
        line = ifelse(entity == "Health Facilities",  
              value[entity == "Health Facilities" &  distance == "1 km"], 
                 ifelse(entity == "Schools", 
                        value[entity == "Schools" & distance == "1 km"],
                        ifelse(entity == "Railways",
                               value[entity == "Railways" & distance == "1 km"],
                               ifelse(entity == "Forests", 8, 
                                      ifelse(entity == "Protected Areas", 
                                             value[entity == "Protected Areas" & 
                                                       distance == "1 km"], NA))))),
        line = ifelse(distance ==  "1 km", line, NA)
        )


# Figure 5A
combinedKilnPctBarPlot <- combinedPctExposed %>%
    ggplot(aes(y = entity, x = value, group = entity)) +
    geom_bar(aes(fill =  distance), 
             position = position_stack(), 
             stat = "identity") +
    geom_errorbarh(aes(xmax = line, xmin = line, group = entity), 
                   linetype = "dashed") +
    scale_fill_brewer(palette = "Reds",
                      guide = guide_legend(reverse=TRUE)) + 
    labs(x = "%  with kiln in given distance",
         y = "",
         fill = "",
         title = "Regulated Entity Proximity to Kilns") +
    theme(plot.background = element_blank(),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 11),
          panel.background = element_blank(),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank()) 
ggsave(plot = combinedKilnPctBarPlot,
       filename = paste0(figures, "combinedKilnPctBarPlot.pdf"), 
       dpi = 320,
       width = 8, height = 4)


# Assess Kiln proximity to regulated entities 
distancetoEntities <- lapply(allObjects, closestObj) # distance to closest kiln
distanceEntityDF <- rbindlist(distancetoEntities) %>%
    mutate(id = rep(1:nrow(kilns), 5))

kilnViolations <- lapply(distancetoEntities, function(x)
    lapply(c(1, 2, 5, 10), function(y) checkViolationDist(df = x, dist = y)))

kilnViolationsDF <- t(sapply(1:length(distancetoEntities), 
                             function(x) sapply(1:length(c(1, 2, 5, 10)), 
                                                function(y) kilnViolations[[x]][[y]])))

kilnViolationsDF <- data.frame(kilnViolationsDF) %>%
    mutate_all(list(~unlist(.))) %>%
    mutate_all(list(~(./nrow(kilns))*100)) %>%
    rename(pct1 = n,
           pct2 = n.1,
           pct5 = n.2,
           pct10 = n.3)

kilnViolationsDF$entity <- c("Health Facilities", "Schools", 
                             "Railways", "Forests", "Protected Areas")

kilnViolationsPCT <- kilnViolationsDF %>%
    dplyr::select(-pct2) %>%
    mutate(pct10 = pct10 - pct5,
           pct5 = pct5 - pct1) %>%
    pivot_longer(cols = c(pct1, pct5, pct10),
                 names_to = "distance") %>%
    mutate(distance = ifelse(distance == "pct1", "1 km",
                             ifelse(distance == "pct5", "5 km", "10 km")),
           distance = factor(distance, levels = c("1 km", "5 km", "10 km"), 
                             ordered = T),
           distance = fct_rev(distance),
           entity = fct_rev(factor(entity, 
                                   levels = c("Health Facilities", "Schools", 
                                              "Railways", "Forests", "Protected Areas"),
                                   labels = c("Health Facilities", "Schools", 
                                              "Railways", "Forests", "Protected Areas"),
                                   ordered = T)),
           line = ifelse(entity == "Health Facilities",  value[entity == "Health Facilities" & 
                                                                   distance == "1 km"], 
                         ifelse(entity == "Schools", value[entity == "Schools" & distance == "1 km"],
                                ifelse(entity == "Railways", value[entity == "Railways" & 
                                                                       distance == "1 km"],
                                       ifelse(entity == "Forests", 5.5, 
                                              ifelse(entity == "Protected Areas", 
                                                     value[entity == "Protected Areas" & 
                                                               distance == "1 km"], NA))))),
           line = ifelse(distance ==  "1 km", line, NA)) %>%
    arrange(entity)

# Fig 5B: Kiln proximity to regulated entities
kilnViolationPctBarPlot <- kilnViolationsPCT %>%
    ggplot(aes(y = entity, x = value)) +
    geom_bar(aes(fill =  distance), 
             position = position_stack(), 
             stat = "identity") +
    scale_fill_brewer(palette = "Blues",
                      guide = guide_legend(reverse=TRUE)) + 
    geom_errorbarh(aes(xmax = line, xmin = line, group = entity), linetype = "dashed") +
    labs(x = "% of kilns within a given distance",
         y = "",
         fill = "",
         title = "Kiln Proximity to Regulated Entities") +
    theme(plot.background = element_blank(),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 8),
          plot.title = element_text(size = 11),
          panel.background = element_blank(),
          legend.background = element_blank(),
          legend.position = "bottom",
          legend.key = element_blank()) 
ggsave(plot = kilnViolationPctBarPlot,
       filename = paste0(figures, "kilnViolationPctBarPlot.pdf"), 
       dpi = 320,
       width = 8, height = 4)




################################################################################
###################### 3.  Assess PM2.5 in Dhaka from kilns  ###################
################################################################################
upWindDiD15km <- lm(pm ~ upwindKilns15km*season +  poly(rain, 2) + 
                        poly(temp, 2) + poly(ws,2) + year,
                    data = dhakaEmbassyDF)
upWindDiD15km_cov1 <- vcovHAC(upWindDiD15km)
upWindDiD15km_robust_se <- sqrt(diag(upWindDiD15km_cov1))

plot_model(upWindDiD15km, type = "pred",
           terms = c("upwindKilns15km", "season"),
           vcov.fun = "vcovHC",
           vcov.type = "HC1") +
    scale_color_nejm(labels = c("Kilns Off", "Kilns On")) + 
    guides(color = guide_legend(override.aes = list(shape = 1))) +
    scale_fill_nejm(labels = c("Kilns Off", "Kilns On")) + 
    scale_y_continuous(limits = c(0,145),
                       breaks = pretty_breaks(5)) +
    labs(x = "# of kilns",
         y = "PM2.5 (ug/m^3)",
         title = " ",
         color = "") +
    theme_minimal() + theme(legend.position = "bottom",
                            axis.text = element_text(size = 12),
                            legend.text = element_text(size = 12))
ggsave(filename = paste0(output, "upwind15km_45deg.pdf"), 
       dpi = 320,
       width = 5, height = 4)



