# Quick setup instructions to deploy VM on GCP and set up cron job
# sign up for free trial on GCP and  create ubuntu 18.04 minimal VM instance (Iowa or Oregon)
# SSH into instance 
#
# need to insert passwords below
# mkdir -p Desktop/Worldfish/peskAAS
# echo atilley:******** > Desktop/Worldfish/peskAAS/KoBo-auth.txt
# echo wildrlab_shaun:*************** > Desktop/Worldfish/peskAAS/peskaDB-auth.txt
# echo shaunpwilkinson@gmail.com:********** > Desktop/Worldfish/peskAAS/PDS-auth.txt

# ## Install R 
# sudo sh -c 'echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list'
# sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
# sudo apt update
# sudo apt install r-base
# cd /usr/local/lib/R
# sudo chmod o+w site-library
# ls -l
# cd ~
# ## libraries needed for openssl, curl, xml2 and RMySQL
# sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev libmariadb-client-lgpl-dev mysql-server
## install required R packages
# R
# install.packages(c("DBI", "RMySQL", "httr", "readxl", "readr", "RANN"))
# q()

## modify next chunk to suit specific environment
## note - need full file path (no tilde shortcut)
## target directory should contain KoBo-auth.txt, peskaDB-auth.txt and PDS-auth.txt files
## following line appended to etc/crontab runs the peskaparse.R script on a daily basis
## at 10:30 am
## (no bash script needed)
## 30 10   * * *   root    Rscript -e 'source("https://www.dropbox.com/s/iwzuw0z0q7skytu/peskaPARSE.R?dl=1")'
## to append run 
# sudo apt update
# sudo apt install nano
# sudo nano /etc/crontab
## paste the line in and write out (ctrl-O, enter, ctrl-X)


path_to_koboauth <- "/home/shaun/Desktop/Worldfish/peskAAS/KoBo-auth.txt"
path_to_peskaDBauth <- "/home/shaun/Desktop/Worldfish/peskAAS/peskaDB-auth.txt"
path_to_PDSauth <- "/home/shaun/Desktop/Worldfish/peskAAS/PDS-auth.txt"


kobo_id <-  344563 # new landings data


if(!("DBI" %in% list.files(.libPaths()))) install.packages("DBI", repos = "https://cloud.r-project.org/")
if(!("RMySQL" %in% list.files(.libPaths()))) install.packages("RMySQL", repos = "https://cloud.r-project.org/")
if(!("httr" %in% list.files(.libPaths()))) install.packages("httr", repos = "https://cloud.r-project.org/")
if(!("readxl" %in% list.files(.libPaths()))) install.packages("readxl", repos = "https://cloud.r-project.org/")
if(!("readr" %in% list.files(.libPaths()))) install.packages("readr", repos = "https://cloud.r-project.org/")
if(!("RANN" %in% list.files(.libPaths()))) install.packages("RANN", repos = "https://cloud.r-project.org/")


print(Sys.Date())

## read in credentials
kobocreds <- scan(path_to_koboauth, what = "", sep = "\n")
kobocreds <- kobocreds[!grepl("^#", kobocreds)]
kobocreds <- kobocreds[grep(":", kobocreds)[1]]
stopifnot(length(kobocreds) == 1L)
kobocreds <- gsub(" ", "", kobocreds)
kobocreds <- strsplit(kobocreds, split = ":")[[1]][1:2]
stopifnot(length(kobocreds) == 2L)
names(kobocreds) <- c("user", "pass")

peskacreds <- scan(path_to_peskaDBauth, what = "", sep = "\n")
peskacreds <- peskacreds[!grepl("^#", peskacreds)]
peskacreds <- peskacreds[grep(":", peskacreds)[1]]
stopifnot(length(peskacreds) == 1L)
peskacreds <- gsub(" ", "", peskacreds)
peskacreds <- strsplit(peskacreds, split = ":")[[1]][1:2]
stopifnot(length(peskacreds) == 2L)
names(peskacreds) <- c("user", "pass")

PDScreds <- scan(path_to_PDSauth, what = "", sep = "\n")
PDScreds <- PDScreds[!grepl("^#", PDScreds)]
PDScreds <- PDScreds[grep(":", PDScreds)[1]]
stopifnot(length(PDScreds) == 1L)
PDScreds <- gsub(" ", "", PDScreds)
PDScreds <- strsplit(PDScreds, split = ":")[[1]][1:2]
stopifnot(length(PDScreds) == 2L)
names(PDScreds) <- c("user", "pass")


message("Retrieving landings data from KoBo")
URL <- paste0("https://kc.humanitarianresponse.info/api/v1/data/", kobo_id, ".xlsx")
z <- httr::GET(URL, httr::authenticate(kobocreds["user"], kobocreds["pass"]))
httr::stop_for_status(z)
tmpz <- tempfile(fileext = ".xlsx")
filetag <- file(tmpz, open = "wb")
writeBin(httr::content(z, type = "raw"), filetag)
close(filetag)
message("Reading Kobo tables")
cty <- c("text", "text", "date", "text", "date", rep("text", 39))
trips <- readxl::read_excel(tmpz, sheet = 1, col_types = cty)
colnames(trips) <- gsub(".+/", "", colnames(trips))
colnames(trips) <- gsub("^_+", "", colnames(trips))
trips$index <- as.integer(trips$index)
spp <- readxl::read_excel(tmpz, sheet = 2, col_types = "text")
colnames(spp) <- gsub(".+/", "", colnames(spp))
colnames(spp) <- gsub("^_+", "", colnames(spp))
spp$parent_index <- as.integer(spp$parent_index)
spp <- spp[order(spp$parent_index), ] ## should be ordered anyway but just in case
spp$trip_id <- trips$id[spp$parent_index] # make sure no trips have been removed prior to this
tmp <- split(spp$trip_id, f = factor(spp$trip_id, levels = unique(spp$trip_id)))
addnos <- function(v) paste0(v, formatC(seq_along(v), width = 2, format = "d", flag = "0"))
tmp <- lapply(tmp, addnos)
spp$rec_id <- unlist(tmp, use.names = FALSE)
## remove non-boat-based trips
trips$has_boat[is.na(trips$has_boat)] <- "FALSE"
trips$has_boat[trips$has_boat != "TRUE"] <- "FALSE" ## may not need this next time
trips$has_boat <- as.logical(trips$has_boat)
trips <- trips[trips$has_boat,]
###trips <- trips[!trips$id %in% peskador$trip_id, ] ############# replace rows in peskaDAT.sqlite?? 
spp <- spp[spp$parent_index %in% trips$index, ]
dups <- duplicated(spp[, -match("index", colnames(spp))]) 
if(any(dups)){
  message("Duplicates found for trip(s) ", unique(spp[["parent_index"]][dups]))
  spp <- spp[!dups, ]
}
message("Filtering tables & tidying columns")
trips$rel_effort <- NA_integer_ ### remember to calculate = men + women + children
trips$trip_effort <- NA_real_ ### remember to calculate = rel_effort * trip_hours
trips$flag_code <- 0L
trips$boat_id <- NA_integer_
trips$mesh_size[is.na(trips$mesh_size)] <- trips$mesh_size_other[is.na(trips$mesh_size)]
trips <- trips[c("id", "date", "landing_site_name", "habitat_boat", "boat_type", "has_PDS", "IMEI", "boat_id", "boat_reg_no", "boat_owner_name", 
                 "gear_type", "mesh_size",  "duration", "no_men_fishers", "no_women_fishers", "no_child_fishers",
                 "rel_effort", "trip_effort", "total_catch_value",  "happiness_rating", "no_boats", "how_many_gleaners_today", 
                    "flag_code", "notes")]
colnames(trips) <- c("trip_id", "date", "station_code", "habitat_code", "boat_code",  "has_PDS", "IMEI",  "boat_id", "boat_reg_no", "owner", "gear_code", 
                     "mesh", "trip_hours", "men", "women", "children", "rel_effort", "trip_effort", "catch_value", "rank_code", 
                     "all_boats", "all_gleaners", "flag_code", "note")
trips$station_code[!trips$station_code %in% paste(1:30)] <- NA
trips$station_code <- as.integer(trips$station_code)
if(mode(trips$station_code) == "character") warning("Site variable failed conversion to integer")
trips$habitat_code[!trips$habitat_code %in% paste(1:7)] <- NA 
trips$habitat_code <- as.integer(trips$habitat_code)
if(mode(trips$habitat_code) == "character") warning("Habitat variable failed conversion to integer")
## convert other cols to integers/numeric
trips$gear_code[!trips$gear_code %in% c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP")] <- NA_character_
trips$gear_code <- match(trips$gear_code, c("GN", "HL", "LL", "SG", "CN", "MC", "BS", "SN", "TP"))
#trips$mesh[!trips$mesh %in% c("1", "1.25", "1.5", "2", "2.5", "3")] <- NA_character_
suppressWarnings(trips$mesh <- as.numeric(trips$mesh)) #warning ok
trips$men <- as.integer(trips$men)
trips$women <- as.integer(trips$women)
trips$children <- as.integer(trips$children)
trips$rank_code[!trips$rank_code %in% paste(1:5)] <- NA_character_
trips$rank_code <- as.integer(trips$rank_code)
trips$boat_code[!trips$boat_code %in% paste(1:2)] <- NA_character_ ## note no shoreys included here
trips$boat_code <- as.integer(trips$boat_code)
trips$rel_effort <- trips$men + trips$women + trips$children
trips$flag_code[trips$rel_effort < 1] <- 10L
trips$trip_hours <- as.numeric(trips$trip_hours)
trips$trip_hours[is.na(trips$trip_hours)] <- 0
trips$flag_code[trips$trip_hours < 1] <- 9L
trips$flag_code[trips$trip_hours > 72] <- 9L
trips$trip_hours <- round(trips$trip_hours, 1)
trips$trip_effort <- trips$rel_effort * trips$trip_hours

trips$IMEI[is.na(trips$IMEI)] <- "0"
ncimei <- nchar(trips$IMEI)
trips$IMEI[ncimei > 7] <- sub(".+(.{7})$", "\\1", trips$IMEI[ncimei > 7])
ncimei <- nchar(trips$IMEI)
nzta <- 8 - ncimei # would be 7 - ncemi but add 1 for R indexing style
zta <- c("", "0", "00", "000", "0000", "00000", "000000", "0000000")[nzta]
trips$IMEI <- paste0(zta, trips$IMEI)
trips$date_numeric <- as.numeric(as.Date(trips$date))
trips <- trips[order(trips$date_numeric, trips$trip_id), ]
trips$has_PDS <- as.integer(as.logical(trips$has_PDS))
trips$date <- as.character(trips$date)
trips$catch_value <- as.integer(trips$catch_value) 
trips$all_boats <- as.integer(trips$all_boats) 
trips$all_gleaners <- as.integer(trips$all_gleaners) 

suppressWarnings(spp$species <- as.integer(spp$species)) # warning ok
spp$species[is.na(spp$species)] <- 999 # unidentified species code
fcc <- grep("no_individuals_5_10", colnames(spp))
lcc <- grep("no_individuals_55_60", colnames(spp))
tmp <- spp[, seq(fcc, lcc)]
midpoints <- c(7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5)
spp$length <- midpoints[apply(tmp, 1, function(r) match(FALSE, is.na(r)))]
spp$nfish <- as.integer(apply(tmp, 1, function(r) r[match(FALSE, is.na(r))]))
bigfish <- is.na(spp$length)
spp$length[bigfish] <- as.numeric(spp$fish_length_over60[bigfish]) # will be NA for zero catch
spp$nfish[bigfish] <- as.integer(spp$no_individuals_over60[bigfish]) # will be NA for zero catch
spp$nfish[is.na(spp$length) | spp$species == 0] <- 0L
spp$flag_code <- 0L
spp$flag_code[is.na(spp$nfish)] <- 7L # shouldn't be any NA in nfish
spp$weight_g <- NA_real_ 
spp <- spp[c("rec_id", "trip_id", "species", "length", "nfish", "weight_g", "food_or_sale", "flag_code", "notes")]
colnames(spp) <- c("rec_id", "trip_id", "species_code",  "length", "nfish", "weight_g", "food_sale", "flag_code", "note")
spp$species_code <- as.integer(spp$species_code)
spp <- spp[-1] ## no longer use rec_ids from 20190905

## connect to database
peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user=peskacreds["user"], password=peskacreds["pass"], 
                         dbname='wildrlab_peskaDB', host='johnny.heliohost.org', port=3306)
## Get old trips table and check for duplicates
trips1 <- RMySQL::dbReadTable(peskaDAT, "trips")
discard_trips <- trips$trip_id %in% trips1$trip_id
discard_spps <- spp$trip_id %in% trips$trip_id[discard_trips]
trips <- trips[!discard_trips, ]
spp <- spp[!discard_spps, ]


# Append new records
if(nrow(trips) > 0){
  ## Apply flag codes
  sp_pars <- RMySQL::dbReadTable(peskaDAT, "species")
  sp_rows <- match(spp$species_code, sp_pars$species_code)
  sp_rows[is.na(sp_rows)] <- match(999L, sp_pars$species_code)
  ## Flag suspect entries
  tooshort <- spp$length > 0 & spp$length < sp_pars$minlength[sp_rows]
  tooshort[is.na(tooshort)] <- FALSE
  spp$flag_code[tooshort] <- 1L
  toolong <- spp$length > sp_pars$maxlength[sp_rows]
  toolong[is.na(toolong)] <- FALSE
  spp$flag_code[toolong] <- 2L
  get_weight <- function(a, b, l) a * l^b
  unitwgt <- mapply(get_weight, sp_pars$a[sp_rows], sp_pars$b[sp_rows],  spp$length)
  tooheavy <- unitwgt > sp_pars$maxweight[sp_rows] * 1.5
  tooheavy[is.na(tooheavy)] <- FALSE
  spp$flag_code[tooheavy & !toolong] <- 3L
  unitwgt[spp$species_code == 45L] <- 50 # crab
  unitwgt[spp$species_code == 24L] <- 1000 # octopus
  unitwgt[spp$species_code == 50L] <- 6 # octopus
  spp$weight_g <- round(spp$nfish * unitwgt) # note: throws error if no spps
  spp$weight_g[spp$species_code == 0L] <- 0
  invalidcombo <- trips$habitat_code %in% c(2, 5) & trips$gear_code == 7
  trips$flag_code[invalidcombo] <- 8L
  trips$trip_id <- as.integer(trips$trip_id)
  message("Appending new records")
  RMySQL::dbWriteTable(peskaDAT, "trips", trips, append = TRUE, row.names=FALSE)
  message("Successfully appended ", nrow(trips),  " new trip records")
  trips1 <- rbind(trips1, trips) ## still need to link PDS records below
  if(nrow(spp) > 0){
    spp$trip_id <- as.integer(spp$trip_id)
    RMySQL::dbWriteTable(peskaDAT, "landings", spp, append = TRUE, row.names=FALSE)
    message("Successfully appended ", nrow(spp),  " new landings records")
  }
}else{
  message("No new records to append")
}


## import existing PDS data
boats <- RMySQL::dbReadTable(peskaDAT, "boats")
stations <- RMySQL::dbReadTable(peskaDAT, "stations")
linked_records <- DBI::dbGetQuery(peskaDAT, "SELECT * FROM PDS_trips WHERE NOT trip_id='NA'")
last_PDS_record <- DBI::dbGetQuery(peskaDAT, 'SELECT * FROM PDS_trips ORDER BY date_numeric DESC LIMIT 1') 
last_PDS_upload_date <- last_PDS_record$date_numeric
RMySQL::dbDisconnect(peskaDAT)

boats <- boats[!is.na(boats$IMEI) & !is.na(boats$boat_id) & !is.na(boats$boat_code), ]
if(as.integer(Sys.Date()) == last_PDS_record$date_numeric) return(NULL) ## append new PDS records

startdate <- as.character(as.Date(last_PDS_upload_date, origin = "1970-01-01")) # note there may be some duplicates
## filter by uniques at append stage 
enddate <- as.character(Sys.Date()) ## pds api will only provide records up to yesterday
URL <- paste0("https://api.pelagicdata.com/api/VLxEDznakTkiJtu34Lq2/v1/points/", startdate, "/", enddate)
tmp <- httr::GET(URL, httr::authenticate(PDScreds["user"], PDScreds["pass"]))
geos <- readr::read_csv(httr::content(tmp, "raw"), col_types = "Tdddddddcc")
if(nrow(geos) == 0) return(NULL)
geos <- geos[!is.na(geos$Trip), ]
if(nrow(geos) == 0) return(NULL)
geos <- geos[, 1:5]
geos$Time <- as.Date(geos$Time)
geos$Lat <- round(geos$Lat, 3)
geos$Lng <- round(geos$Lng, 3)
geos <- geos[!duplicated(geos[c("Trip", "Lat", "Lng")]), ]
if(nrow(geos) == 0) return(NULL)
geos$Lat <- geos$Lat * 1000
geos$Lng <- geos$Lng * 1000
geos$Lat <- 2 * round(geos$Lat/2)
geos$Lng <- 2 * round(geos$Lng/2)
geos$Lat <- geos$Lat/1000
geos$Lng <- geos$Lng/1000
geos <- geos[!duplicated(geos[c("Trip", "Lat", "Lng")]), ]
if(nrow(geos) == 0) return(NULL)
geos$latlng <- paste0(geos$Lat, ",", geos$Lng)
colnames(geos) <- c("date", "boat_id", "PDS_trip", "lat", "lng", "latlng")
geos$date <- as.character(geos$date)
PDS_trips <- geos[c("PDS_trip", "date", "boat_id")]
PDS_trips <- PDS_trips[!duplicated(PDS_trips$PDS_trip),] #30572
PDS_points <- geos[c("PDS_trip", "latlng")]
PDS_trips$date_numeric <- as.numeric(as.Date(PDS_trips$date))
  
indices <- match(PDS_trips$boat_id, boats$boat_id)
PDS_trips$IMEI <- boats$IMEI[indices]
PDS_trips$boat_code <- boats$boat_code[indices]#  1 = canoe, 2 = motor
PDS_trips$station_code <- stations$station_code[match(boats$municipality_name[indices], stations$municipality_name)]

discards <- is.na(PDS_trips$IMEI) | is.na(PDS_trips$boat_code) | is.na(PDS_trips$station_code)
discard_trips <- PDS_trips$PDS_trip[discards]
PDS_trips <- PDS_trips[!discards, ] 
if(nrow(PDS_trips) == 0) return(NULL)
discard_points <- PDS_points$PDS_trip %in% discard_trips
PDS_points <- PDS_points[!discard_points, ]

## Link records where possible
PDSTIMEI7 <- sub(".+(.{7})$", "\\1", PDS_trips$IMEI)
PDSTIMEI7 <- paste0(PDSTIMEI7, "-",PDS_trips$date)

PDS_trips$trip_id <- trips1$trip_id[match(PDSTIMEI7, paste0(trips1$IMEI, "-", trips1$date))]
## unlink KOBO trips that point to multiple PDS trips 
PDS_trips$trip_id[PDS_trips$trip_id %in% PDS_trips$trip_id[duplicated(PDS_trips$trip_id)]] <- NA

## assign 10 evenly spaced points to each pds trip
tmp <- strsplit(PDS_points$latlng, split = ",")
PDS_points_full <- PDS_points
PDS_points_full$lat <- as.numeric(sapply(tmp, "[", 1))
PDS_points_full$lng <- as.numeric(sapply(tmp, "[", 2))
f <- factor(PDS_points_full$PDS_trip, levels = unique(PDS_points_full$PDS_trip))
lats <- split(PDS_points_full$lat, f = f)
lngs <- split(PDS_points_full$lng, f = f)
myfun <- function(v) v[round(seq(1, length(v), length.out = 10))]
newlats <- lapply(lats, myfun)
newlats <- do.call("rbind", newlats)
colnames(newlats) <- paste0("lat", 1:10)
newlngs <- lapply(lngs, myfun)
newlngs <- do.call("rbind", newlngs)
colnames(newlngs) <- paste0("lng", 1:10)
newcoords <- cbind(newlats, newlngs)

## which ones need predicting
nas <- is.na(PDS_trips$trip_id)
PDS_trips$gear_code <- NA
PDS_trips$habitat_code <- NA 
PDS_trips$donor_tripid <- NA
PDS_trips$donor_distance <- NA

## insert known gears and habitats into pds trip table
if(any(!nas)){
  linked_tripids <- PDS_trips$trip_id[!nas] #kobo ids
  indices <- match(linked_tripids, trips1$trip_id) #row in kobo trip table
  PDS_trips$gear_code[!nas] <- trips1$gear_code[indices]
  PDS_trips$habitat_code[!nas] <- trips1$habitat_code[indices] #stations and btypes already done earlier
  PDS_trips$donor_tripid[!nas] <- PDS_trips$trip_id[!nas]
  PDS_trips$donor_distance[!nas] <- 0 
}


if(any(nas)){ ## find nearest neighbors
  oldcoords <- linked_records[, seq(match("lat1", colnames(linked_records)), ncol(linked_records))]
  nnobj <- RANN::nn2(oldcoords, newcoords[nas,], k = 1)
  ## insert unknown gears and habitats into pds trip table
  donors <- linked_records$trip_id[nnobj$nn.idx] # kobo trip ids
  PDS_trips$gear_code[nas] <- linked_records$gear_code[nnobj$nn.idx] # note name-match indexing on kobo trip id
  PDS_trips$habitat_code[nas] <- linked_records$habitat_code[nnobj$nn.idx]
  PDS_trips$station_code[nas] <- linked_records$station_code[nnobj$nn.idx]
  ## not necessary to predict boat type
  PDS_trips$donor_tripid[nas] <- donors
  PDS_trips$donor_distance[nas] <- nnobj$nn.dists[, 1]
}


PDS_trips <- cbind(PDS_trips, newcoords)

## remove anomolous PDS trips
discards <- PDS_trips$donor_distance > 0.2
if(all(discards)) return(NULL)
discard_trips <- PDS_trips$PDS_trip[discards]
PDS_trips <- PDS_trips[!discards, ]
discard_points <- PDS_points$PDS_trip %in% discard_trips
PDS_points <- PDS_points[!discard_points, ]

## reconnect to DB and write data
peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user=peskacreds["user"], password=peskacreds["pass"], 
                             dbname='wildrlab_peskaDB', host='johnny.heliohost.org', port=3306)

RMySQL::dbWriteTable(peskaDAT, "PDS_trips", PDS_trips, append = TRUE, row.names=FALSE)
message("Successfully appended ", nrow(PDS_trips),  " new PDS trip records")
RMySQL::dbWriteTable(peskaDAT, "PDS_points", PDS_points, append = TRUE, row.names=FALSE)
message("Successfully appended ", nrow(PDS_points),  " new PDS point records")
RMySQL::dbDisconnect(peskaDAT)
####################### END ##########################


