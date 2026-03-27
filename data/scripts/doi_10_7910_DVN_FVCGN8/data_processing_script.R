############################################
# Set Working Directory and Load Libraries #
############################################

rm(list=ls())

# Sets the working directory and user-specific path for file access

path <- "" # your path to directory here

# Libraries

libraries <- c("geosphere", "rgdal", "rgeos", "raster", "maptools", "lfe", "plyr",
               "stargazer", "foreign", "DiagrammeR", "xtable", 
               "RColorBrewer", "rsvg", "DiagrammeRsvg", "magrittr", "fixest", "data.table")
lapply(libraries, library, character.only = TRUE)


#######################################################
# Create list of decadal county shapefiles, 1900-1990 #
#######################################################

# Loads NHGIS county shapefiles for decades 1900-1990
# Subsets to Great Plains states and stores in a list

# Load county shapefiles
years <- c("1900", "1910", "1920", "1930", "1940", "1950", "1960", "1970", "1980", "1990")
county_list <- lapply(years, function(yr) {
  shp_path <- paste0(path, "input_files/nhgis0001_shape/nhgis0001_shapefile_tl2008_us_county_", yr, "/US_county_", yr, "_conflated.shp")
  readOGR(shp_path)
})

# Create county panel
great_plains_states <- c("Colorado", "North Dakota", "South Dakota", "Wyoming", "Nebraska", 
                         "Montana", "Oklahoma", "Kansas", "New Mexico", "Texas")

process_counties <- function(county) {
  subset(county, county@data$STATENAM %in% great_plains_states) |>
    spTransform(CRS("+init=epsg:4326"))
}

county_list <- lapply(county_list, process_counties)

# Save processed data
save(county_list, file=paste0(path, "output_files/county_list.RData"))

##################################################################################
# Process shapfiles to compute county-level aquifer overlap and distance metrics #
##################################################################################

# Loads list of shapefiles
# For each decade, computes county-level metrics
# Including overlap with Ogallala aquifer (based on boundary shapefile)

# Load county shapefiles

load(paste0(path, "output_files/county_list.Rdata"))

# Load and process aquifer data

aquifer <- rgdal::readOGR(paste0(path, "input_files/aquifer/hp_bound2010.shp"))
aquifer <- sp::spTransform(aquifer, CRS("+init=epsg:4326")) ## transform to lat/long CRS
save(aquifer, file=paste0(path, "output_files/aquifer.Rdata"))

# Create a version where internal holes are removed

hole_free_aquifer <- rgeos::gBuffer(aquifer, width = 0) ## Zero buffer to eliminate internal holes
spdf <- data.frame(x=NA)
rownames(spdf) <- "buffer"

# Create a spatial lines version of aquifer 

aquifer_line <- sp::SpatialPolygonsDataFrame(hole_free_aquifer, spdf) 
aquifer_line  <- as(aquifer_line , "SpatialLinesDataFrame")
save(aquifer_line, file=paste0(path, "output_files/aquifer_line.Rdata"))

# Create a state boundary variable

states <- unionSpatialPolygons(county_list[[10]], county_list[[10]]@data$STATENAM) # use 1990 boundaries
save(states, file=paste0(path, "output_files/states.Rdata"))

## Compute metrics

# Compute aquifer overlap

aquifer_intersection <- function(x){ ## gIntersection method
for (i in 1:nrow(x@data)){
	temp <- rgeos::gIntersection(aquifer, x[i,], byid=T)
	if(length(temp)>=1){
	   	x@data$overlap[i] <- round(sum(geosphere::areaPolygon(temp))/sum(geosphere::areaPolygon(x[i,])), digits=3)
	}else(x@data$overlap[i] <- 0)
print(i)
}
return(x)
}

county_list <- lapply(county_list, aquifer_intersection)

## Compute a distance to boundary variable and related metrics

boundary_distance <- function(x){

	for (i in 1:nrow(x)){
		xy <- rgeos::gCentroid(x[i,]) ## county centroid
		x@data$longitude[i] <- coordinates(xy)[1] ## long of county centroid
		x@data$latitude[i] <- coordinates(xy)[2] ## lat of county centroid

		x@data$in_state[i] <- names(states)[sp::over(xy,states)] ## name of state in which county located
		state <- states[sp::over(xy,states),]
		state_line <- rgeos::intersect(aquifer_line, state) ## get within-state boundary segment

		if(length(state_line)>0){
		dist <- geosphere::dist2Line(xy, state_line, distfun=distGeo) ## geodesic distance to closest point on within-state boundary segment
		x@data$dist[i] <- dist[,1]/1000 ## in kilometers
		x@data$boundary_lon[i] <- dist[,2] ## longitude of closest point on boundary
		x@data$boundary_lat[i] <- dist[,3] ## latitute of closest point on boundary
		mydf <- data.frame(longitude=dist[,2], latitude=dist[,3])
		coordinates(mydf) <- cbind(mydf$longitude , mydf$latitude)
		proj4string(mydf) = CRS("+init=epsg:4326")
		x@data$boundary_state[i] <- names(states)[sp::over(mydf,states)] ## name of state in which closest point on boundary located
		x@data$inside_aquifer[i] <- ifelse(is.na(sp::over(xy, aquifer)[1,]), 0, 1) ## is county centroid inside aquifer					
		}

		if(length(state_line)==0){
		x@data$dist[i] <- NA
		x@data$boundary_lon[i] <- NA
		x@data$boundary_lat[i] <- NA
		x@data$boundary_state[i] <- NA
		x@data$inside_aquifer[i] <- NA	
		}

	print(i)
	}
	return(x)
}

county_list <- lapply(county_list, boundary_distance)

## Create a panel dataset

names <- Reduce(intersect,
	list(colnames(county_list[[1]]@data),
		colnames(county_list[[2]]@data), colnames(county_list[[3]]@data),
			colnames(county_list[[4]]@data), colnames(county_list[[5]]@data),
				colnames(county_list[[6]]@data), colnames(county_list[[7]]@data),
					colnames(county_list[[8]]@data), colnames(county_list[[9]]@data),
						colnames(county_list[[10]]@data)))
subset_vars <- function(x){
	subset(x, select=names)
}

## Export saved dataset

county_panel_list <- lapply(county_list, subset_vars)
county_panel <- do.call("rbind", county_panel_list)

## Create a FIPS identifier to link county_panel dataset to electoral outcomes

county_panel@data$fips <- paste0(substr(county_panel@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(county_panel@data$NHGISCTY, 1, 3))
county_panel@data$id_no <- 1:nrow(county_panel)

save(county_panel, file=paste0(path, "output_files/county_panel.RData"))

################################################################################
# Merge county data with eletoral data (prsidential, senatorial, gubernatorial #
################################################################################

# Loads county panel and electoral datasets
# Assigns FIPS codes for county identification and merges with electoral data
# Creates and processes data for presidential, senatorial, and gubernatorial elections
# Saves intermediate and merged datasets

## Load panel shapefile of county boundaries and intersection with aquifer

load(paste0(path, "output_files/county_panel.RData"))

## Load election dataset 

load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_presidential_county_returns_1868_2020.RData"))
load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_gubernatorial_county_returns_1865_2020.RData"))
load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_us_senate_county_returns_1908_2020.RData"))

## Create a decade variable

pres_elections_release$DECADE <- ifelse(pres_elections_release$election_year%in%1900:1909, 1900,
	ifelse(pres_elections_release$election_year%in%1910:1919, 1910,
		ifelse(pres_elections_release$election_year%in%1920:1929, 1920,
			ifelse(pres_elections_release$election_year%in%1930:1939, 1930,
				ifelse(pres_elections_release$election_year%in%1940:1949, 1940,
	ifelse(pres_elections_release$election_year%in%1950:1959, 1950,
		ifelse(pres_elections_release$election_year%in%1960:1969, 1960,
			ifelse(pres_elections_release$election_year%in%1970:1979, 1970,
	ifelse(pres_elections_release$election_year%in%1980:1989, 1980,
		ifelse(pres_elections_release$election_year%in%1990:2000, 1990, NA))))))))))

senate_elections_release$DECADE <- ifelse(senate_elections_release$election_year%in%1900:1909, 1900,
	ifelse(senate_elections_release$election_year%in%1910:1919, 1910,
		ifelse(senate_elections_release$election_year%in%1920:1929, 1920,
			ifelse(senate_elections_release$election_year%in%1930:1939, 1930,
				ifelse(senate_elections_release$election_year%in%1940:1949, 1940,
	ifelse(senate_elections_release$election_year%in%1950:1959, 1950,
		ifelse(senate_elections_release$election_year%in%1960:1969, 1960,
			ifelse(senate_elections_release$election_year%in%1970:1979, 1970,
	ifelse(senate_elections_release$election_year%in%1980:1989, 1980,
		ifelse(senate_elections_release$election_year%in%1990:2000, 1990, NA))))))))))

gov_elections_release$DECADE <- ifelse(gov_elections_release$election_year%in%1900:1909, 1900,
	ifelse(gov_elections_release$election_year%in%1910:1919, 1910,
		ifelse(gov_elections_release$election_year%in%1920:1929, 1920,
			ifelse(gov_elections_release$election_year%in%1930:1939, 1930,
				ifelse(gov_elections_release$election_year%in%1940:1949, 1940,
	ifelse(gov_elections_release$election_year%in%1950:1959, 1950,
		ifelse(gov_elections_release$election_year%in%1960:1969, 1960,
			ifelse(gov_elections_release$election_year%in%1970:1979, 1970,
	ifelse(gov_elections_release$election_year%in%1980:1989, 1980,
		ifelse(gov_elections_release$election_year%in%1990:2000, 1990, NA))))))))))

## Subset to relevant time periods and states

pres <- subset(pres_elections_release, pres_elections_release$election_year%in%1910:2000 &
	pres_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX"))

sen <- subset(senate_elections_release, senate_elections_release$election_year%in%1910:2000 &
	senate_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX"))

gov <- subset(gov_elections_release, gov_elections_release$election_year%in%1910:2000 &
	gov_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX") &
		is.na(gov_elections_release$raw_county_vote_totals)==F)

## Link electoral data to GIS data for presidential elections

fail <- numeric(nrow(pres))
pres$id_no <- NA  
for (i in 1:nrow(pres)){
    id_no <- NA  
    id_no <- which(county_panel@data$fips == pres$fips[i] & county_panel@data$DECADE == pres$DECADE[i])
    if(length(id_no) == 0){
        id_no <- which(county_panel@data$fips == pres$fips[i] & county_panel@data$DECADE == (pres$DECADE[i] + 10))
    }
    tryCatch(pres$id_no[i] <- id_no,  
             warning = function(w) {
                 print(w)
                 fail[i] <<- 1
             })
print(i)
}
pres_data <- merge(pres, county_panel, by="id_no", all.x=T) 

## Link electoral data to GIS data for senatorial elections

fail <- numeric(nrow(sen))
sen$id_no <- NA
for (i in 1:nrow(sen)){
	id_no <- NA
	id_no <- which(county_panel@data$fips==sen$fips[i] & county_panel@data$DECADE==sen$DECADE[i])
	if(length(id_no)==0){
	id_no <- which(county_panel@data$fips==sen$fips[i] & county_panel@data$DECADE==(sen$DECADE[i]+10))
	}
	tryCatch(sen$id_no[i] <- id_no,
             warning = function(w) {
                 print(w)
                 fail[i] <<- 1
             })
	sen$id_no[i] <- id_no
print(i)
}
sen_data <- merge(sen, county_panel, by="id_no", all.x=T)

## Link electoral data to GIS data for gubernatorial elections

fail <- numeric(nrow(gov))
gov$id_no <- NA
for (i in 1:nrow(gov)){
	id_no <- NA
	id_no <- which(county_panel@data$fips==gov$fips[i] & county_panel@data$DECADE==gov$DECADE[i])
	if(length(id_no)==0){
	id_no <- which(county_panel@data$fips==gov$fips[i] & county_panel@data$DECADE==(gov$DECADE[i]+10))
	}
	tryCatch(gov$id_no[i] <- id_no,
             warning = function(w) {
                 print(w)
                 fail[i] <<- 1
             })
 	 gov$id_no[i] <- id_no
print(i)
}

gov_data <- merge(gov, county_panel, by="id_no", all.x=T)

## Note that Schnasse and Sterling are two short-lived counties (elimintated in 1911 in SD) that share FIPS codes with Sanborn and Stanley respectively. They generate warnings but the match is correct.

# Define key DV

pres_data$conservative <- pres_data$republican_raw_votes/pres_data$pres_raw_county_vote_totals_two_party
sen_data$conservative <- sen_data$republican_raw_votes/sen_data$senate_raw_county_vote_totals_two_party
gov_data$conservative <- gov_data$republican_raw_votes/gov_data$gov_raw_county_vote_totals_two_party

# define election id variable

pres_data$election_id <- paste0(pres_data$STATENAM, pres_data$election_year)
sen_data$election_id <- paste0(sen_data$STATENAM, sen_data$election_year)
gov_data$election_id <- paste0(gov_data$STATENAM, gov_data$election_year)

# define decadal 'buckets'

pres_data$buckets <- ifelse(pres_data$election_year%in%c(1900:1909), 1900,
	ifelse(pres_data$election_year%in%c(1910:1919), 1910,
	ifelse(pres_data$election_year%in%c(1920:1929), 1920,
	ifelse(pres_data$election_year%in%c(1930:1939), 1930,
	ifelse(pres_data$election_year%in%c(1940:1949), 1940,
	ifelse(pres_data$election_year%in%c(1950:1959), 1950,
	ifelse(pres_data$election_year%in%c(1960:1969), 1960,
	ifelse(pres_data$election_year%in%c(1970:1979), 1970,
	ifelse(pres_data$election_year%in%c(1980:1989), 1980,
	ifelse(pres_data$election_year%in%c(1990:2000), 1990,NA))))))))))

sen_data$buckets <- ifelse(sen_data$election_year%in%c(1900:1909), 1900,
	ifelse(sen_data$election_year%in%c(1910:1919), 1910,
	ifelse(sen_data$election_year%in%c(1920:1929), 1920,
	ifelse(sen_data$election_year%in%c(1930:1939), 1930,
	ifelse(sen_data$election_year%in%c(1940:1949), 1940,
	ifelse(sen_data$election_year%in%c(1950:1959), 1950,
	ifelse(sen_data$election_year%in%c(1960:1969), 1960,
	ifelse(sen_data$election_year%in%c(1970:1979), 1970,
	ifelse(sen_data$election_year%in%c(1980:1989), 1980,
	ifelse(sen_data$election_year%in%c(1990:2000), 1990,NA))))))))))

gov_data$buckets <- ifelse(gov_data$election_year%in%c(1900:1909), 1900,
	ifelse(gov_data$election_year%in%c(1910:1919), 1910,
	ifelse(gov_data$election_year%in%c(1920:1929), 1920,
	ifelse(gov_data$election_year%in%c(1930:1939), 1930,
	ifelse(gov_data$election_year%in%c(1940:1949), 1940,
	ifelse(gov_data$election_year%in%c(1950:1959), 1950,
	ifelse(gov_data$election_year%in%c(1960:1969), 1960,
	ifelse(gov_data$election_year%in%c(1970:1979), 1970,
	ifelse(gov_data$election_year%in%c(1980:1989), 1980,
	ifelse(gov_data$election_year%in%c(1990:2000), 1990,NA))))))))))

# save files

save(pres_data, file=paste0(path,"output_files/pres_data.RData"))
save(sen_data, file=paste0(path,"output_files/sen_data.RData"))
save(gov_data, file=paste0(path,"output_files/gov_data.RData"))

###############################
# Create agricultural dataset #
###############################

# Loads and processes agricultural datasets from various years
# Computes key agricultural metrics like irrigation extent and farm machinery value
# Merges agricultural data with county panel data
# Saves the merged dataset for further analysis

# Load data

load(paste0(path,"output_files/county_panel.Rdata"))

ag1910 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0005/04254-0005-Data.dta"))
ag1920 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0006/04254-0006-Data.dta"))
ag1925 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0007/04254-0007-Data.dta"))
ag1930 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0008/04254-0008-Data.dta"))
ag1935 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0009/04254-0009-Data.dta"))
ag1940 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0010/04254-0010-Data.dta"))
ag1945 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0011/04254-0011-Data.dta"))
ag1950 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0012/04254-0012-Data.dta"))
ag1954 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0013/04254-0013-Data.dta"))
ag1959 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0014/04254-0014-Data.dta"))
ag1964 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0015/04254-0015-Data.dta"))
ag1969 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0016/04254-0016-Data.dta"))
ag1969 <- subset(ag1969, ag1969$TYPE69=="All")
ag1974 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0017/04254-0017-Data.dta"))
ag1974 <- subset(ag1974, ag1974$TYPE74=="All")
ag1978 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0018/04254-0018-Data.dta"))
ag1982 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0019/04254-0019-Data.dta"))
ag1987 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0020/04254-0020-Data.dta"))
ag1992 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0021/04254-0021-Data.dta"))
ag1997 <- read.dta(paste0(path,"input_files/ICPSR_04254/DS0022/04254-0022-Data.dta"))

holder <- list(ag1910,ag1920,ag1925,ag1930,ag1935,ag1940,ag1945,ag1950,ag1954,ag1959,  
	ag1964,ag1969,ag1974,ag1978,ag1982,ag1987,ag1992,ag1997)
holder1 <- list()
for (i in 1:length(holder)){
	holder1[[i]] <- holder[[i]]
	holder1[[i]]$YEAR <- c(1910,1920,1925,1930,1935,1940,1945,1950,1954, 1959, 
	1964,1969,1974,1978,1982,1987,1992,1997)[i]
print(i)
}
agdat <- do.call("rbind.fill",holder1)

## Decadal buckets

agdat$buckets <- ifelse(agdat$YEAR%in%c(1900:1909), 1900,
	ifelse(agdat$YEAR%in%c(1910:1919), 1910,
	ifelse(agdat$YEAR%in%c(1920:1929), 1920,
	ifelse(agdat$YEAR%in%c(1930:1939), 1930,
	ifelse(agdat$YEAR%in%c(1940:1949), 1940,
	ifelse(agdat$YEAR%in%c(1950:1959), 1950,
	ifelse(agdat$YEAR%in%c(1960:1969), 1960,
	ifelse(agdat$YEAR%in%c(1970:1979), 1970,
	ifelse(agdat$YEAR%in%c(1980:1989), 1980,
	ifelse(agdat$YEAR%in%c(1990:2000), 1990,NA))))))))))

## Create a matching identifier

agdat$state_code <- substr(agdat$UNFIPS,1,2)
agdat$STATENAM <- ifelse(agdat$state_code=="08", "Colorado",
	ifelse(agdat$state_code=="19", "Iowa",
	ifelse(agdat$state_code=="20", "Kansas",
	ifelse(agdat$state_code=="27", "Minnesota",
	ifelse(agdat$state_code=="30", "Montana",
	ifelse(agdat$state_code=="31", "Nebraska",
	ifelse(agdat$state_code=="35", "New Mexico",
	ifelse(agdat$state_code=="38", "North Dakota",
	ifelse(agdat$state_code=="40", "Oklahoma",
	ifelse(agdat$state_code=="46", "South Dakota",
	ifelse(agdat$state_code=="48", "Texas",
	ifelse(agdat$state_code=="56", "Wyoming",NA))))))))))))
agdat <- subset(agdat, agdat$STATENAM%in%c("Colorado","Kansas","Wyoming","Nebraska","New Mexico",
	"Oklahoma","South Dakota","Texas"))

## Code irrigation variable

agdat$FML_IX_A <- ifelse(agdat$FML_IX_A<0, NA, agdat$FML_IX_A)
agdat$ARE_XX_A <- ifelse(agdat$ARE_XX_A <0, NA, agdat$ARE_XX_A)
agdat$FML_XX_A <- ifelse(agdat$FML_XX_A <0, NA, agdat$FML_XX_A)
agdat$irr <- 100*(agdat$FML_IX_A/agdat$FML_XX_A)
agdat$irr2 <- ifelse(is.na(agdat$irr), 0, agdat$irr) ## See 1920 census -- they did not report for counties with minimal irrigation

## Decade

agdat$DECADE <- ifelse(agdat$YEAR%in%1900:1909, 1900,
	ifelse(agdat$YEAR%in%1910:1919, 1910,
		ifelse(agdat$YEAR%in%1920:1929, 1920,
			ifelse(agdat$YEAR%in%1930:1939, 1930,
				ifelse(agdat$YEAR%in%1940:1949, 1940,
	ifelse(agdat$YEAR%in%1950:1959, 1950,
		ifelse(agdat$YEAR%in%1960:1969, 1960,
			ifelse(agdat$YEAR%in%1970:1979, 1970,
	ifelse(agdat$YEAR%in%1980:1989, 1980,
		ifelse(agdat$YEAR%in%1990:2000, 1990, NA))))))))))

## Create a id_no variable to link datasets

agdat$id_no <- NA
for (i in 1:nrow(agdat)){
	id_no <- which(county_panel@data$fips==agdat$UNFIPS[i] & county_panel@data$DECADE==agdat$DECADE[i])
	if(length(id_no)==0){
	id_no <- which(county_panel@data$fips==agdat$UNFIPS[i] & county_panel@data$DECADE==(agdat$DECADE[i]+10))
	}
	agdat$id_no[i] <- ifelse(length(id_no)>0, id_no, NA)
print(i)
}

## Tenure

agdat$tenancy <- NA
agdat$tenancy[agdat$YEAR==1910] <- (agdat$FRM_TX_Q[agdat$YEAR==1910])/(agdat$FRM_XX_Q[agdat$YEAR==1910])
agdat$tenancy[agdat$YEAR==1920] <- agdat$FRM_TX_Q[agdat$YEAR==1920]/agdat$FRM_XX_[agdat$YEAR==1920]
agdat$tenancy[agdat$YEAR==1925] <- agdat$FRM_TX_Q[agdat$YEAR==1925]/agdat$FRM_XX_[agdat$YEAR==1925]
agdat$tenancy[agdat$YEAR==1930] <- agdat$FRM_TX_Q[agdat$YEAR==1930]/agdat$FRM_XX_[agdat$YEAR==1930]
agdat$tenancy[agdat$YEAR==1935] <- agdat$FRM_TX_Q[agdat$YEAR==1935]/agdat$FRM_XX_[agdat$YEAR==1935]
agdat$tenancy[agdat$YEAR==1940] <- agdat$FRM_TX_Q[agdat$YEAR==1940]/agdat$FRM_XX_[agdat$YEAR==1940]
agdat$tenancy[agdat$YEAR==1945] <- agdat$FRM_TX_Q[agdat$YEAR==1945]/agdat$FRM_XX_[agdat$YEAR==1945]
agdat$tenancy[agdat$YEAR==1950] <- agdat$FRM_TX_Q[agdat$YEAR==1950]/agdat$FRM_XX_[agdat$YEAR==1950] ## value of machinery missing
agdat$tenancy[agdat$YEAR==1954] <- agdat$FRM_TX_Q[agdat$YEAR==1954]/agdat$FRM_XX_[agdat$YEAR==1954]
agdat$tenancy[agdat$YEAR==1959] <- agdat$FRM_TX_Q[agdat$YEAR==1959]/agdat$FRM_XX_[agdat$YEAR==1959]
agdat$tenancy[agdat$YEAR==1964] <- agdat$FRM_TX_Q[agdat$YEAR==1964]/agdat$FRM_XX_[agdat$YEAR==1964]
agdat$tenancy[agdat$YEAR==1969] <- agdat$FRM_TX_Q[agdat$YEAR==1969]/agdat$FRM_XX_[agdat$YEAR==1969]
agdat$tenancy[agdat$YEAR==1974] <- agdat$FRM_TX_Q[agdat$YEAR==1974]/agdat$FRM_XX_[agdat$YEAR==1974] 
agdat$tenancy[agdat$YEAR==1978] <- agdat$FRM_TX_Q[agdat$YEAR==1978]/agdat$FRM_XX_[agdat$YEAR==1978] 
agdat$tenancy[agdat$YEAR==1982] <-  agdat$FRM_TX_Q[agdat$YEAR==1982]/agdat$FRM_XX_[agdat$YEAR==1982] 
agdat$tenancy[agdat$YEAR==1987] <-  agdat$FRM_TX_Q[agdat$YEAR==1987]/agdat$FRM_XX_[agdat$YEAR==1987]
agdat$tenancy[agdat$YEAR==1992] <- agdat$FRM_TX_Q[agdat$YEAR==1992]/agdat$FRM_XX_[agdat$YEAR==1992]
agdat$tenancy[agdat$YEAR==1997] <- agdat$FRM_TX_Q[agdat$YEAR==1997]/agdat$FRM_XX_[agdat$YEAR==1997]

## Average value of machinery per farm

agdat$machinery <- NA
agdat$machinery[agdat$YEAR==1910] <- (agdat$IMP_XX_V[agdat$YEAR==1910])/.040
agdat$machinery[agdat$YEAR==1920] <- (agdat$IMP_XX_V[agdat$YEAR==1920])/.083
agdat$machinery[agdat$YEAR==1925] <- agdat$IMP_XX_V[agdat$YEAR==1925]/.073
agdat$machinery[agdat$YEAR==1930] <- (agdat$IMP_XX_V[agdat$YEAR==1930])/.070
agdat$machinery[agdat$YEAR==1935] <- NA ## no machinery data
agdat$machinery[agdat$YEAR==1940] <- (agdat$IMP_OF_V[agdat$YEAR==1940] + agdat$IMP_OF_V[agdat$YEAR==1940] + 
	agdat$IMP_MX_V[agdat$YEAR==1940] + agdat$IMP_TX_V[agdat$YEAR==1940])/.058
agdat$machinery[agdat$YEAR==1945] <- (agdat$IMP_XX_V[agdat$YEAR==1945])/.075
agdat$machinery[agdat$YEAR==1950] <- NA/.110 ## value of machinery missing
agdat$machinery[agdat$YEAR==1954] <- NA/.112  ## value of machinery missing
agdat$machinery[agdat$YEAR==1959] <- NA/.120 ## value of machinery missing
agdat$machinery[agdat$YEAR==1964] <- NA/.129 ## value of machinery missing
agdat$machinery[agdat$YEAR==1969] <- (agdat$IMP_XX_V[agdat$YEAR==1969])/.153
agdat$machinery[agdat$YEAR==1974] <- (agdat$IMP_AX_V[agdat$YEAR==1974]*agdat$FRM_XX_Q[agdat$YEAR==1974])/.205
agdat$machinery[agdat$YEAR==1978] <- (agdat$IMP_AX_V[agdat$YEAR==1978]*agdat$FRM_XX_Q[agdat$YEAR==1978])/.272
agdat$machinery[agdat$YEAR==1982] <- (agdat$IMP_AX_V[agdat$YEAR==1982]*agdat$FRM_XX_Q[agdat$YEAR==1982])/.402 
agdat$machinery[agdat$YEAR==1987] <- (agdat$IMP_AX_V[agdat$YEAR==1987]*agdat$FRM_XX_Q[agdat$YEAR==1987])/.473
agdat$machinery[agdat$YEAR==1992] <- (agdat$IMP_XX_V[agdat$YEAR==1992])/.585
agdat$machinery[agdat$YEAR==1997] <- (agdat$IMP_XX_V[agdat$YEAR==1997])/.669
agdat$machinery <- ifelse(agdat$machinery < 0, NA, agdat$machinery)
agdat$farm_number <- ifelse(agdat$FRM_XX_Q <0 , NA, agdat$FRM_XX_Q)
agdat$machinery_per_farm <- agdat$machinery/agdat$farm_number

## Agricultural land value estimates

agdat$output <- NA
agdat$output[agdat$YEAR==1910] <- (agdat$AFP_TX_V[agdat$YEAR==1910])/.040
agdat$output[agdat$YEAR==1920] <- (agdat$AFP_TX_V[agdat$YEAR==1920])/.083
agdat$output[agdat$YEAR==1925] <- agdat$AFP_TX_V[agdat$YEAR==1925]/.073
agdat$output[agdat$YEAR==1930] <- (agdat$AFP_TX_V[agdat$YEAR==1930])/.070
agdat$output[agdat$YEAR==1935] <- (agdat$AFP_TX_V[agdat$YEAR==1935])/.070## no machinery data
agdat$output[agdat$YEAR==1940] <- agdat$AFP_TX_V[agdat$YEAR==1940]/.058
agdat$output[agdat$YEAR==1945] <- (agdat$AFP_TX_V[agdat$YEAR==1945])/.075
agdat$output[agdat$YEAR==1950] <- agdat$AFP_TX_V[agdat$YEAR==1950]/.110 ## value of machinery missing
agdat$output[agdat$YEAR==1954] <- agdat$AFP_TX_V[agdat$YEAR==1954]/.112  ## value of machinery missing
agdat$output[agdat$YEAR==1959] <- agdat$AFP_TX_V[agdat$YEAR==1959]/.120 ## value of machinery missing
agdat$output[agdat$YEAR==1964] <- agdat$AFP_TX_V[agdat$YEAR==1964]/.129 ## value of machinery missing
agdat$output[agdat$YEAR==1969] <- agdat$AFP_TX_V[agdat$YEAR==1969]/.153
agdat$output[agdat$YEAR==1974] <- agdat$AFP_TX_V[agdat$YEAR==1974]/.205
agdat$output[agdat$YEAR==1978] <- agdat$AFP_TX_V[agdat$YEAR==1978]/.272
agdat$output[agdat$YEAR==1982] <- agdat$AFP_TX_V[agdat$YEAR==1982]/.402 
agdat$output[agdat$YEAR==1987] <- agdat$AFP_TX_V[agdat$YEAR==1987]/.473
agdat$output[agdat$YEAR==1992] <- agdat$AFP_TX_V[agdat$YEAR==1992]/.585
agdat$output[agdat$YEAR==1997] <- agdat$AFP_TX_V[agdat$YEAR==1997]/.669
agdat$output <- ifelse(agdat$output < 0, NA, agdat$output)
agdat$output_per_farm <- agdat$output/agdat$farm_number


## Agricultural output estimates

agdat$output <- NA
agdat$output[agdat$YEAR==1910] <- (agdat$AFP_TX_V[agdat$YEAR==1910])/.040
agdat$output[agdat$YEAR==1920] <- (agdat$AFP_TX_V[agdat$YEAR==1920])/.083
agdat$output[agdat$YEAR==1925] <- agdat$AFP_TX_V[agdat$YEAR==1925]/.073
agdat$output[agdat$YEAR==1930] <- (agdat$AFP_TX_V[agdat$YEAR==1930])/.070
agdat$output[agdat$YEAR==1935] <- (agdat$AFP_TX_V[agdat$YEAR==1935])/.070## no machinery data
agdat$output[agdat$YEAR==1940] <- agdat$AFP_TX_V[agdat$YEAR==1940]/.058
agdat$output[agdat$YEAR==1945] <- (agdat$AFP_TX_V[agdat$YEAR==1945])/.075
agdat$output[agdat$YEAR==1950] <- agdat$AFP_TX_V[agdat$YEAR==1950]/.110 ## value of machinery missing
agdat$output[agdat$YEAR==1954] <- agdat$AFP_TX_V[agdat$YEAR==1954]/.112  ## value of machinery missing
agdat$output[agdat$YEAR==1959] <- agdat$AFP_TX_V[agdat$YEAR==1959]/.120 ## value of machinery missing
agdat$output[agdat$YEAR==1964] <- agdat$AFP_TX_V[agdat$YEAR==1964]/.129 ## value of machinery missing
agdat$output[agdat$YEAR==1969] <- agdat$AFP_TX_V[agdat$YEAR==1969]/.153
agdat$output[agdat$YEAR==1974] <- agdat$AFP_TX_V[agdat$YEAR==1974]/.205
agdat$output[agdat$YEAR==1978] <- agdat$AFP_TX_V[agdat$YEAR==1978]/.272
agdat$output[agdat$YEAR==1982] <- agdat$AFP_TX_V[agdat$YEAR==1982]/.402 
agdat$output[agdat$YEAR==1987] <- agdat$AFP_TX_V[agdat$YEAR==1987]/.473
agdat$output[agdat$YEAR==1992] <- agdat$AFP_TX_V[agdat$YEAR==1992]/.585
agdat$output[agdat$YEAR==1997] <- agdat$AFP_TX_V[agdat$YEAR==1997]/.669
agdat$output <- ifelse(agdat$output < 0, NA, agdat$output)
agdat$output_per_farm <- agdat$output/agdat$farm_number

## Agricultural crops

agdat$crop_output <- NA
agdat$crop_output[agdat$YEAR==1910] <- agdat$CRO_XX_V[agdat$YEAR==1910]/.040
agdat$crop_output[agdat$YEAR==1920] <- agdat$CRO_XX_V[agdat$YEAR==1920]/.083
agdat$crop_output[agdat$YEAR==1925] <- agdat$CRO_XX_V[agdat$YEAR==1925]/.073
agdat$crop_output[agdat$YEAR==1930] <- agdat$CRO_XX_V[agdat$YEAR==1930]/.070
agdat$crop_output[agdat$YEAR==1935] <- agdat$CRO_XX_V[agdat$YEAR==1935]/.070
agdat$crop_output[agdat$YEAR==1940] <- agdat$CRO_XX_V[agdat$YEAR==1940]/.058
agdat$crop_output[agdat$YEAR==1945] <- agdat$CRO_XX_V[agdat$YEAR==1945]/.075
agdat$crop_output[agdat$YEAR==1950] <- agdat$CRO_TX_V[agdat$YEAR==1950]/.110 
agdat$crop_output[agdat$YEAR==1954] <- agdat$CRO_TX_V[agdat$YEAR==1954]/.112  
agdat$crop_output[agdat$YEAR==1959] <- agdat$CRO_TX_V[agdat$YEAR==1959]/.120 
agdat$crop_output[agdat$YEAR==1964] <- agdat$CRO_TX_V[agdat$YEAR==1964]/.129 
agdat$crop_output[agdat$YEAR==1969] <- agdat$CRO_TX_V[agdat$YEAR==1969]/.153
agdat$crop_output[agdat$YEAR==1974] <- agdat$CRO_TX_V[agdat$YEAR==1974]/.205
agdat$crop_output[agdat$YEAR==1978] <- agdat$CRO_TX_V[agdat$YEAR==1978]/.272
agdat$crop_output[agdat$YEAR==1982] <- agdat$CRO_TX_V[agdat$YEAR==1982]/.402 
agdat$crop_output[agdat$YEAR==1987] <- agdat$CRO_TX_V[agdat$YEAR==1987]/.473
agdat$crop_output[agdat$YEAR==1992] <- agdat$CRO_TX_V[agdat$YEAR==1992]/.585
agdat$crop_output[agdat$YEAR==1997] <- agdat$CRO_TX_V[agdat$YEAR==1997]/.669
agdat$crop_output <- ifelse(agdat$crop_output < 0, NA, agdat$crop_output)
agdat$crop_output_per_farm <- agdat$crop_output/agdat$farm_number

## Average value of land and buildings per farm

agdat$value <- NA
agdat$value[agdat$YEAR==1910] <- (agdat$PRP_OF_V[agdat$YEAR==1910] + agdat$PRP_TX_V[agdat$YEAR==1910] + agdat$PRP_MX_V[agdat$YEAR==1910])/.040
agdat$value[agdat$YEAR==1920] <- (agdat$FML_XX_V[agdat$YEAR==1920] + agdat$BLD_XX_V[agdat$YEAR==1920])/.083
agdat$value[agdat$YEAR==1925] <- NA ## no building data
agdat$value[agdat$YEAR==1930] <- (agdat$PRP_OF_V[agdat$YEAR==1930]+agdat$PRP_OP_V[agdat$YEAR==1930]+agdat$PRP_MX_V[agdat$YEAR==1930]+agdat$PRP_TX_V[agdat$YEAR==1930])/.070
agdat$value[agdat$YEAR==1935] <- NA ## no building data
agdat$value[agdat$YEAR==1940] <- (agdat$PRP_OF_V[agdat$YEAR==1940]+agdat$PRP_OP_V[agdat$YEAR==1940]+agdat$PRP_MX_V[agdat$YEAR==1940]+agdat$PRP_TX_V[agdat$YEAR==1940])/.058
agdat$value[agdat$YEAR==1945] <- NA/.075 ## Value of buildings is missing
agdat$value[agdat$YEAR==1950] <- (agdat$PRP_AX_V[agdat$YEAR==1950]*agdat$FRM_XX_Q[agdat$YEAR==1950])/.100
agdat$value[agdat$YEAR==1954] <- NA/.112   ## No building data
agdat$value[agdat$YEAR==1959] <- (agdat$FML_BX_V[agdat$YEAR==1959]*agdat$FRM_XX_Q[agdat$YEAR==1959])/.120
agdat$value[agdat$YEAR==1964] <- (agdat$FML_BX_V[agdat$YEAR==1964]*agdat$FRM_XX_Q[agdat$YEAR==1964])/.129 
agdat$value[agdat$YEAR==1969] <- (agdat$PRP_XX_V[agdat$YEAR==1969])/.153
agdat$value[agdat$YEAR==1974] <- (agdat$PRP_XX_V[agdat$YEAR==1974]+agdat$IMP_AX_V[agdat$YEAR==1974]*agdat$FRM_XX_Q[agdat$YEAR==1974])/.205
agdat$value[agdat$YEAR==1978] <- (agdat$PRP_AX_V[agdat$YEAR==1978]*agdat$FRM_XX_Q[agdat$YEAR==1978])/.272
agdat$value[agdat$YEAR==1982] <- (agdat$FML_BX_V[agdat$YEAR==1982]*agdat$FRM_XX_Q[agdat$YEAR==1982])/.402 
agdat$value[agdat$YEAR==1987] <- (agdat$FML_BX_V[agdat$YEAR==1987]*agdat$FRM_XX_Q[agdat$YEAR==1987])/.473
agdat$value[agdat$YEAR==1992] <- (agdat$FML_BX_V[agdat$YEAR==1992]*agdat$FRM_XX_Q[agdat$YEAR==1992])/.585
agdat$value[agdat$YEAR==1997] <- (agdat$FML_BX_V[agdat$YEAR==1997]*agdat$FRM_XX_Q[agdat$YEAR==1997])/.669
agdat$value <- ifelse(agdat$value < 0, NA, agdat$value)
agdat$value_per_farm <- agdat$value/agdat$farm_number
agdat$capitalized_value <- I((agdat$value_per_farm+agdat$machinery_per_farm)/1000) 

## Number of farms per 1000 acres

agdat$county_acres <- ifelse(agdat$ARE_XX_A <0 , NA, agdat$ARE_XX_A)
agdat$farms_per_acre <- (1000*agdat$farm_number/agdat$county_acres)
agdat$value_per_acre <- agdat$value/agdat$FML_XX_A

## Number of livestock per farm

agdat$cattle <- ifelse(agdat$CTL_XX_Q<0, NA, agdat$CTL_XX_Q)
agdat$swine <- ifelse(agdat$SWN_XX_Q<0, NA, agdat$SWN_XX_Q)
agdat$livestock_per_farm <- (agdat$cattle+agdat$swine)/agdat$farm_number

## Merge

agdat <- merge(agdat, county_panel, by="id_no", all.x=T)
agdat$stateyear <- paste(agdat$STATENAM.x, agdat$YEAR)
save(agdat, file=paste0(path,"output_files/agdat.Rdata"))

##########################################
# Create census, religion, etc. datasets #
##########################################

load(paste0(path,"output_files/county_panel.Rdata"))

## Load population data

pop1900 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0004/04296-0004-Data.dta"))
pop1910 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0005/04296-0005-Data.dta"))
pop1920 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0006/04296-0006-Data.dta"))
pop1930 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0007/04296-0007-Data.dta"))
pop1940 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0008/04296-0008-Data.dta"))
pop1950 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0009/04296-0009-Data.dta"))
pop1960 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0010/04296-0010-Data.dta"))
pop1970 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0011/04296-0011-Data.dta"))
pop1980 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0012/04296-0012-Data.dta"))
pop1990 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0013/04296-0013-Data.dta"))
pop2000 <- read.dta(paste0(path,"input_files/ICPSR_04296/DS0014/04296-0014-Data.dta"))

pop <- list(pop1910,pop1920,pop1930,pop1940,pop1950,pop1960,pop1970,pop1980,pop1990,pop2000)

for (i in 1:length(pop)){
	pop[[i]] <- subset(pop[[i]])
print(i)
}
popdat <- do.call("rbind.fill",pop)

## Agricultural employment

popdat$adult_population <- ifelse(popdat$FT15_19<0, NA,popdat$FT15_19) +
	ifelse(popdat$FT20_24<0, NA,popdat$FT20_24) +
		ifelse(popdat$FT25_29<0, NA,popdat$FT25_29) +
			ifelse(popdat$FT30_34<0, NA,popdat$FT30_34) +
	ifelse(popdat$FT35_44<0, NA,popdat$FT35_44) +
		ifelse(popdat$FT45_54<0, NA,popdat$FT45_54) +
			ifelse(popdat$FT55_64<0, NA,popdat$FT55_64) +
	ifelse(popdat$MT15_19<0, NA,popdat$MT15_19) +
		ifelse(popdat$MT20_24<0, NA,popdat$MT20_24) +
			ifelse(popdat$MT25_29<0, NA,popdat$MT25_29) +
	ifelse(popdat$MT30_34<0, NA,popdat$MT30_34) +
		ifelse(popdat$MT35_44<0, NA,popdat$MT35_44) +
			ifelse(popdat$MT45_54<0, NA,popdat$MT45_54) +
	ifelse(popdat$MT55_64<0, NA,popdat$MT55_64)
popdat$emp <- ifelse(popdat$EMP >0, popdat$EMP, NA)
popdat$ag <- ifelse(popdat$AG >=0, popdat$AG, NA)
popdat$ag_percent <- 100*(popdat$ag/popdat$emp)

## Population density

popdat$total <- ifelse(popdat$TTOT >=0, popdat$TTOT, NA)
popdat$size <- ifelse(popdat$ARE_XX_A >=0, popdat$ARE_XX_A, NA)
popdat$pop_density <- popdat$total/(popdat$size)

## Urbanization

popdat$urban <- ifelse(popdat$URB >=0, popdat$URB, NA)
popdat$urban_percent <- 100*(popdat$urban/popdat$total)

## Age ratio

popdat$young_population <- ifelse(popdat$FT0_4<0, NA,popdat$FT0_4) +
	ifelse(popdat$FT5_9<0, NA,popdat$FT5_9) +
		ifelse(popdat$FT10_14<0, NA,popdat$FT10_14) +
	ifelse(popdat$FT15_19<0, NA,popdat$FT15_19) +
		ifelse(popdat$FT20_24<0, NA,popdat$FT20_24) + 
	ifelse(popdat$MT0_4<0, NA,popdat$MT0_4) +
		ifelse(popdat$MT5_9<0, NA,popdat$MT5_9) +
	ifelse(popdat$MT10_14<0, NA,popdat$MT10_14) +
		ifelse(popdat$MT15_19<0, NA,popdat$MT15_19) +
	ifelse(popdat$MT20_24<0, NA,popdat$MT20_24)

popdat$old_population <- 	ifelse(popdat$FT65_74<0, NA,popdat$FT65_74) +
	ifelse(popdat$FT75_ <0, NA, popdat$FT75_) +
	 	ifelse(popdat$MT65_74<0, NA, popdat$MT65_74) +
	ifelse(popdat$MT75_ <0, NA, popdat$MT75_) 

popdat$age_ratio <- popdat$old_population/popdat$young_population

## Create a state code variable that matches across datasets

popdat$state_code <- substr(popdat$UNFIPS,1,2)

## Decade

popdat$DECADE <- ifelse(popdat$YEAR%in%1900:1909, 1900,
	ifelse(popdat$YEAR%in%1910:1919, 1910,
		ifelse(popdat$YEAR%in%1920:1929, 1920,
			ifelse(popdat$YEAR%in%1930:1939, 1930,
				ifelse(popdat$YEAR%in%1940:1949, 1940,
	ifelse(popdat$YEAR%in%1950:1959, 1950,
		ifelse(popdat$YEAR%in%1960:1969, 1960,
			ifelse(popdat$YEAR%in%1970:1979, 1970,
	ifelse(popdat$YEAR%in%1980:1989, 1980,
		ifelse(popdat$YEAR%in%1990:2000, 1990, NA))))))))))

## Create a id_no variable to link datasets

popdat$id_no <- NA
for (i in 1:nrow(popdat)){
	id_no <- which(county_panel@data$fips==popdat$UNFIPS[i] & county_panel@data$DECADE==popdat$DECADE[i])
	if(length(id_no)==0){
	id_no <- which(county_panel@data$fips==popdat$UNFIPS[i] & county_panel@data$DECADE==(popdat$DECADE[i]+10))
	}
	popdat$id_no[i] <- ifelse(length(id_no)>0, id_no, NA)
print(i)
}

## Merge

popdat <- merge(popdat, county_panel, by="id_no", all.x=T)
popdat$stateyear <- paste(popdat$STATENAM.x, popdat$YEAR)

## Buckets

popdat$buckets <- ifelse(popdat$YEAR%in%c(1900:1909), 1900,
	ifelse(popdat$YEAR%in%c(1910:1919), 1910,
	ifelse(popdat$YEAR%in%c(1920:1929), 1920,
	ifelse(popdat$YEAR%in%c(1930:1939), 1930,
	ifelse(popdat$YEAR%in%c(1940:1949), 1940,
	ifelse(popdat$YEAR%in%c(1950:1959), 1950,
	ifelse(popdat$YEAR%in%c(1960:1969), 1960,
	ifelse(popdat$YEAR%in%c(1970:1979), 1970,
	ifelse(popdat$YEAR%in%c(1980:1989), 1980,
	ifelse(popdat$YEAR%in%c(1990:2000), 1990,NA))))))))))

## Religosity (church membership)

religion_1926 <- read.dta(paste0(path,"input_files/religion_data/United States Census of Religious Bodies, County File, 1926.DTA"))
religion_1926$YEAR <- 1926
religion_1926$tot_members <- apply(religion_1926[,5:87],1,sum) 
religion_1936 <- read.dta(paste0(path,"input_files/religion_data/United States Census of Religious Bodies, County File, 1936.DTA"))
religion_1936$YEAR <- 1936
religion_1936$tot_members <- apply(religion_1936[,5:78],1,sum) 
religion_1952 <- read.dta(paste0(path,"input_files/religion_data/Churches and Church Membership in the United States, 1952 (Counties).DTA"))
religion_1952$YEAR <- 1952
religion_1952$tot_members <- religion_1952$totmemb
religion_1971 <- read.dta(paste0(path,"input_files/religion_data/Churches and Church Membership in the United States, 1971 (Counties).DTA"))
religion_1971$YEAR <- 1971
religion_1971$tot_members <- religion_1971$totmem
religion_1980 <- read.dta(paste0(path,"input_files/religion_data/Churches and Church Membership in the United States, 1980 (Counties).DTA"))
religion_1980$YEAR <- 1980
religion_1980$tot_members <- religion_1980$MEMTOT80
religion_1990 <- read.dta(paste0(path,"input_files/religion_data/Churches and Church Membership in the United States, 1990 (Counties).DTA"))
religion_1990$YEAR <- 1990
religion_1990$tot_members <- religion_1990$totmem

religion_1952$cntyname <- toupper(gsub(",.*$", "", religion_1952$cname))
religion_1952$state <- toupper(religion_1952$stcode)
religion_1971$cntyname <- toupper(gsub(",.*$", "", religion_1971$name))
religion_1971$state <- toupper(religion_1971$state)

religion_1980$cntyname <- toupper(gsub(",.*$", "", religion_1980$name))
religion_1980$state <- ifelse(religion_1980$state==8, "COLORADO",
	ifelse(religion_1980$state==20, "KANSAS",
	ifelse(religion_1980$state==40, "OKLAHOMA",
	ifelse(religion_1980$state==48, "TEXAS",
	ifelse(religion_1980$state==35, "NEW MEXICO",
	ifelse(religion_1980$state==46, "SOUTH DAKOTA",
	ifelse(religion_1980$state==38, "NORTH DAKOTA",
	ifelse(religion_1980$state==30, "MONTANA",
	ifelse(religion_1980$state==56, "WYOMING",
	ifelse(religion_1980$state==31, "NEBRASKA",NA))))))))))

religion_1990$cntyname <- toupper(gsub(",.*$", "", religion_1990$name))
religion_1990$state <- ifelse(religion_1990$state==8, "COLORADO",
	ifelse(religion_1990$state==20, "KANSAS",
	ifelse(religion_1990$state==40, "OKLAHOMA",
	ifelse(religion_1990$state==48, "TEXAS",
	ifelse(religion_1990$state==35, "NEW MEXICO",
	ifelse(religion_1990$state==46, "SOUTH DAKOTA",
	ifelse(religion_1990$state==38, "NORTH DAKOTA",
	ifelse(religion_1990$state==30, "MONTANA",
	ifelse(religion_1990$state==56, "WYOMING",
	ifelse(religion_1990$state==31, "NEBRASKA",NA))))))))))
	
religion_list <- list(religion_1926, religion_1936, religion_1952,
	religion_1971, religion_1980, religion_1990)
religion_dat <- do.call("rbind.fill", religion_list)
religion_dat$state <- ifelse(religion_dat$state=="NEW MEX", "NEW MEXICO",
	ifelse(religion_dat$state=="NORTH DAK", "NORTH DAKOTA",
		ifelse(religion_dat$state=="SOUTH DAK", "SOUTH DAKOTA",religion_dat$state)))
religion_dat <- subset(religion_dat, religion_dat$state%in%toupper(c("Nebraska","New Mexico","Oklahoma","South Dakota","Texas","Montana","North Dakota","Colorado","Wyoming","Kansas")))

link_table <- read.csv(paste0(path,"input_files/religion_data/religion.csv"))
link_table <- subset(link_table, select=c("cntyname","state","fips"))

religion_dat <- merge(religion_dat, link_table, by=c("cntyname","state"), all.x=T)
religion_dat$fips <- ifelse(nchar(religion_dat$fips.y)==4, paste0(0,religion_dat$fips.y), religion_dat$fips.y)
religion_dat$DECADE <- ifelse(religion_dat$YEAR%in%c(1920:1929), 1920,
	ifelse(religion_dat$YEAR%in%c(1930:1939), 1930,
		ifelse(religion_dat$YEAR%in%c(1940:1949), 1940,
			ifelse(religion_dat$YEAR%in%c(1950:1959), 1950,
	ifelse(religion_dat$YEAR%in%c(1960:1969), 1960,
		ifelse(religion_dat$YEAR%in%c(1970:1979), 1970,
			ifelse(religion_dat$YEAR%in%c(1980:1989), 1980,
	ifelse(religion_dat$YEAR%in%c(1990:2000), 1990,NA))))))))

religion_dat <- merge(religion_dat, county_panel, by.x=c("fips","DECADE"), by.y=c("fips","DECADE"), all.x=T)

## Buckets

religion_dat$buckets <- ifelse(religion_dat$YEAR%in%c(1900:1909), 1900,
	ifelse(religion_dat$YEAR%in%c(1910:1919), 1910,
	ifelse(religion_dat$YEAR%in%c(1920:1929), 1920,
	ifelse(religion_dat$YEAR%in%c(1930:1939), 1930,
	ifelse(religion_dat$YEAR%in%c(1940:1949), 1940,
	ifelse(religion_dat$YEAR%in%c(1950:1959), 1950,
	ifelse(religion_dat$YEAR%in%c(1960:1969), 1960,
	ifelse(religion_dat$YEAR%in%c(1970:1979), 1970,
	ifelse(religion_dat$YEAR%in%c(1980:1989), 1980,
	ifelse(religion_dat$YEAR%in%c(1990:2000), 1990,NA))))))))))

## stateyear

religion_dat$stateyear <- paste(religion_dat$state, religion_dat$YEAR)

## population

for (i in 1:nrow(religion_dat)){
	pop <- popdat$total[popdat$fips==religion_dat$fips[i] & popdat$YEAR==(religion_dat$buckets[i]+10)]
	religion_dat$pop[i] <- ifelse(length(pop) > 0, pop, NA)
print(i)
}

## Church membership

religion_dat$membership <- ((religion_dat$tot_members)/religion_dat$pop)
religion_dat$membership <- ifelse(religion_dat$membership >1, 1, religion_dat$membership)

## Code racial composition variable
###################################

## Load population data

race1930 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds52_1930_county.csv"))
race1930$white <- race1930$BCZ001/(race1930$BCZ001+race1930$BCZ002)
race1930$YEAR <- 1930

race1940 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds78_1940_county.csv"))
race1940$white <- (race1940$BYA001+race1940$BYA002)/(race1940$BYA001+race1940$BYA002+race1940$BYA003+race1940$BYA004)
race1940$YEAR <- 1940

race1950 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds83_1950_county.csv"))
race1950$white <- race1950$B1T001/(race1950$B1T001 +race1950$B1T002 )
race1950$YEAR <- 1950

race1960 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds89_1960_county.csv"))
race1960$white <- race1960$B48001/(race1960$B48001 +race1960$B48002 )
race1960$YEAR <- 1960

race1970 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds94_1970_county.csv"))
race1970$white <- race1970$CBW001/(race1970$CBW001 +race1970$CBW002 +race1970$CBW002 + race1970$CBW003+ race1970$CBW004 + race1970$CBW005)
race1970$YEAR <- 1970

race1980 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds104_1980_county.csv"))
race1980$white <- race1980$C9D001/(race1980$C9D001 + race1980$C9D002 + race1980$C9D003 +race1980$C9D004 +
	race1980$C9D005 + race1980$C9D006 + race1980$C9D007 + race1980$C9D008 + race1980$C9D009 + race1980$C9D010 +
		race1980$C9D011 + race1980$C9D012 + race1980$C9D013 + race1980$C9D014 + race1980$C9D015)
race1980$YEAR <- 1980

race1990 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds120_1990_county.csv"))
race1990$white <- race1990$EUY001/(race1990$EUY001 + race1990$EUY002 + race1990$EUY003 + race1990$EUY004 + race1990$EUY005)
race1990$YEAR <- 1990

race2000 <- read.csv(paste0(path,"input_files/race/nhgis0004_ds151_2000_county.csv"))
race2000$white <- race2000$GQO001/(race2000$GQO001 + race2000$GQO002 + race2000$GQO003 + race2000$GQO004 + race2000$GQO005 +
	race2000$GQO006 + race2000$GQO007)
race2000$YEAR <- 2000

race <- rbind.fill(race1930,race1940,
	race1950,race1960,race1970,race1980,
		race1990, race2000)
## Decade

race$DECADE <- ifelse(race$YEAR%in%1900:1909, 1900,
	ifelse(race$YEAR%in%1910:1919, 1910,
		ifelse(race$YEAR%in%1920:1929, 1920,
			ifelse(race$YEAR%in%1930:1939, 1930,
				ifelse(race$YEAR%in%1940:1949, 1940,
	ifelse(race$YEAR%in%1950:1959, 1950,
		ifelse(race$YEAR%in%1960:1969, 1960,
			ifelse(race$YEAR%in%1970:1979, 1970,
	ifelse(race$YEAR%in%1980:1989, 1980,
		ifelse(race$YEAR%in%1990:2000, 1990, NA))))))))))


## Buckets

race$buckets <- ifelse(race$YEAR%in%1900:1909, 1900,
	ifelse(race$YEAR%in%1910:1919, 1910,
		ifelse(race$YEAR%in%1920:1929, 1920,
			ifelse(race$YEAR%in%1930:1939, 1930,
				ifelse(race$YEAR%in%1940:1949, 1940,
	ifelse(race$YEAR%in%1950:1959, 1950,
		ifelse(race$YEAR%in%1960:1969, 1960,
			ifelse(race$YEAR%in%1970:1979, 1970,
	ifelse(race$YEAR%in%1980:1989, 1980,
		ifelse(race$YEAR%in%1990:2000, 1990, NA))))))))))

## Create a id_no variable to link datasets

race$id_no <- NA
for (i in 1:nrow(race)){
	id_no <- which(county_panel@data$GISJOIN==race$GISJOIN[i] & county_panel@data$DECADE==race$DECADE[i])
	if(length(id_no)==0){
	id_no <- which(county_panel@data$GISJOIN==race$GISJOIN[i] & county_panel@data$DECADE==(race$DECADE[i]+10))
	}
	race$id_no[i] <- ifelse(length(id_no)>0, id_no, NA)
print(i)
}


## Merge

race <- merge(race, county_panel, by="id_no")
race$stateyear <- paste(race$STATENAM, race$YEAR)

save(agdat, file=paste0(path,"output_files/agdat.Rdata"))
save(popdat, file=paste0(path,"output_files/popdat.Rdata"))
save(religion_dat, file=paste0(path,"output_files/religion_dat.Rdata"))
save(race, file=paste0(path,"output_files/race.Rdata"))

############################################
# Identify counties with stable boundaries #
############################################

# Analyzes counties over various decades for stability of boundaries

load(paste0(path,"output_files/county_panel.Rdata"))

## Does county exist over the entire relevant time period?

counties <- unique(county_panel$fips)
county_frame <- data.frame(fips=counties)

for (i in 1:nrow(county_frame)){
	county_frame$exists90[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1990])
	county_frame$exists80[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1980])
	county_frame$exists70[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1930])
	county_frame$exists60[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1930])
	county_frame$exists50[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1930])
	county_frame$exists40[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1930])
	county_frame$exists30[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1930])
	county_frame$exists20[i] <- county_frame$fips[i]%in%unique(county_panel$fips[county_panel@data$DECADE==1920])
print(i)
}

mean(county_frame$exists90 & county_frame$exists80 & county_frame$exists30 & county_frame$exists20)

## Is area 99% similar from beginning to end? Check in terms of overlap of union and intersection 

county_frame$stable <- NA

for (i in 1:nrow(county_frame)){
	if(county_frame$exists90[i] & county_frame$exists80[i] & county_frame$exists30[i] & county_frame$exists20[i]){
	
	intersection <- gIntersection(county_panel[county_panel$fips==county_frame$fips[i] & county_panel@data$DECADE==1920,], ## create intersection and divide by original area
		county_panel[county_panel$fips==county_frame$fips[i] & county_panel@data$DECADE==1990,])

	if(class(intersection)=="SpatialCollections"){

	intersection <- intersection@polyobj

	}

	union <- gUnion(county_panel[county_panel$fips==county_frame$fips[i] & county_panel@data$DECADE==1920,], ## create intersection and divide by original area
		county_panel[county_panel$fips==county_frame$fips[i] & county_panel@data$DECADE==1990,])

	county_frame$stable[i] <- sum(geosphere::areaPolygon(intersection))/geosphere::areaPolygon(union) >=.99

	}else(county_frame$stable[i] <- FALSE)
print(i)
}

mean(county_frame$stable)

save(county_frame, file=paste0(path,"output_files/county_frame.Rdata"))

############################
# Create two-panel dataset #
############################

## Load necessary files

load(paste0(path,"output_files/county_frame.Rdata"))
load(paste0(path,"output_files/pres_data.Rdata"))
load(paste0(path,"output_files/gov_data.Rdata"))
load(paste0(path,"output_files/sen_data.Rdata"))
load(paste0(path,"output_files/agdat.Rdata"))
load(paste0(path,"input_files/covariates.Rdata"))
load(paste0(path,"input_files/deep_learning/big_map2022-09-12.RData"))

## Create a FIPS identifier to link county_panel dataset to electoral outcomes

county_panel@data$fips <- paste0(substr(county_panel@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(county_panel@data$NHGISCTY, 1, 3))
county_panel@data$id_no <- 1:nrow(county_panel)

## Create a two-period panel of stable counties

dat <- data.frame(ID=rep(unique(county_frame$fips[county_frame$stable==1]), times=2),
	post=c(rep(0,times=length(unique(county_frame$fips[county_frame$stable==1]))), rep(1,times=length(unique(county_frame$fips[county_frame$stable==1])))))

for (i in 1:nrow(dat)){
	if(dat$post[i]==0){
		dat$pres[i] <- mean(pres_data$conservative[pres_data$election_year%in%c(1920:1940) & pres_data$fips.x==dat$ID[i]], na.rm=T)
		dat$sen[i] <- mean(sen_data$conservative[sen_data$election_year%in%c(1920:1940) & sen_data$fips.x==dat$ID[i]], na.rm=T)
		dat$gov[i] <- mean(gov_data$conservative[gov_data$election_year%in%c(1920:1940) & gov_data$fips.x==dat$ID[i]], na.rm=T)

		dat$longitude[i] <- mean(county_panel$longitude[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1920","1930")], na.rm=T)
		dat$latitude[i] <- mean(county_panel$latitude[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1920","1930")], na.rm=T)
		dat$dist[i] <- mean(county_panel$dist[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1920","1930")], na.rm=T)
		dat$overlap[i] <- mean(county_panel$overlap[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1920","1930")], na.rm=T)

	}

	if(dat$post[i]==1){
		dat$pres[i] <- mean(pres_data$conservative[pres_data$election_year%in%c(1980:2000) & pres_data$fips.x==dat$ID[i]], na.rm=T)
		dat$sen[i] <- mean(sen_data$conservative[sen_data$election_year%in%c(1980:2000) & sen_data$fips.x==dat$ID[i]], na.rm=T)
		dat$gov[i] <- mean(gov_data$conservative[gov_data$election_year%in%c(1980:2000) & gov_data$fips.x==dat$ID[i]], na.rm=T)

		dat$longitude[i] <- mean(county_panel$longitude[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1980","1990")], na.rm=T)
		dat$latitude[i] <- mean(county_panel$latitude[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1980","1990")], na.rm=T)
		dat$dist[i] <- mean(county_panel$dist[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1980","1990")], na.rm=T)
		dat$overlap[i] <- mean(county_panel$overlap[county_panel@data$fips==dat$ID[i] & county_panel@data$DECADE%in%c("1980","1990")], na.rm=T)
	}

	dat$STATENAM[i] <- unique(county_panel@data$STATENAM[county_panel@data$fips==dat$ID[i]])

print(i)
}

## Subset sample 

dat <- subset(dat, !(dat$STATENAM%in%c("Montana","North Dakota")))
dat$stateyear <- paste(dat$post, dat$STATENAM)
dat$overlap <- round(dat$overlap, digits=3)
dat$fips <- ifelse(nchar(dat$ID)==4, paste0(0,as.character(dat$ID)), as.character(dat$ID))
dat$ID <- as.numeric(dat$ID)

## Attach covariates

dat <- merge(dat, covariates, by=c("ID","post"), all.x=T)

## Attach irrigation and center-pivot irrigation adoption

for (i in 1:nrow(dat)){

	ID <- dat$fips[i]

	if(dat$post[i]==0){
		dat$irr2[i] <- mean(agdat$irr2[agdat$fips==ID & agdat$YEAR%in%c(1920:1940)], na.rm=T)

	}

	if(dat$post[i]==1){
		dat$irr2[i] <- mean(agdat$irr2[agdat$fips==ID & agdat$YEAR%in%c(1980:2000)], na.rm=T)
	}
print(i)
}

## Code center-pivot irrigation variable

big_map@data$fips <- paste0(substr(big_map@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(big_map@data$NHGISCTY, 1, 3))

dat$center_pivot <- NA
for (i in 1:nrow(dat)){

	ID <- ifelse(nchar(dat$ID[i])==4, paste0("0",dat$ID[i]), as.character(dat$ID[i]))

	if(dat$post[i]==0){
		dat$center_pivot[i] <- 0
	}

	if(dat$post[i]==1){
		count <- big_map@data[big_map@data$fips==dat$ID[i],c("year1","year2",
			"year3","year4","year5","year6","year7","year8","year9",
				"year10","year11","year12","year13","year14","year15","year16")]
		dat$center_pivot[i] <- ifelse(length(count)>0, apply(count, 1, mean, na.rm=T), NA)
	}
print(i)
}

# Compute weights

for (i in 1:nrow(dat)){
	dat$w[i] <- mean((dat$overlap[dat$STATENAM==dat$STATENAM[i]]-mean(dat$overlap[dat$STATENAM==dat$STATENAM[i]]))^2)
print(i)
}

dat200 <- subset(dat, dat$dist <= 200)
for (i in 1:nrow(dat200)){
	dat200$w[i] <- mean((dat200$overlap[dat200$STATENAM==dat200$STATENAM[i]]-mean(dat200$overlap[dat200$STATENAM==dat200$STATENAM[i]]))^2)
print(i)
}

dat100 <- subset(dat, dat$dist <= 100)
for (i in 1:nrow(dat100)){
	dat100$w[i] <- mean((dat100$overlap[dat100$STATENAM==dat100$STATENAM[i]]-mean(dat100$overlap[dat100$STATENAM==dat100$STATENAM[i]]))^2)
print(i)
}

# save file

save(dat, file=paste0(path,"output_files/dat.Rdata"))
save(dat200, file=paste0(path,"output_files/dat200.Rdata"))
save(dat100, file=paste0(path,"output_files/dat100.Rdata"))

#####################################
# Individual policy preference data #
#####################################

# Loads and processes CCES (Cooperative Congressional Election Study) survey data
# Focuses on Great Plains states and merges policy preference data
# Analyzes zip code-level data for spatial analysis
# Computes aquifer intersection and distance metrics for each zip code
# Merges these metrics with CCES data for survey analysis
# Saves datasets for figure generation

## Load policy preferences 

load(paste0(path,"input_files/CCES_data/cumulative_cces_policy_preferences.RData"))

## Load CCES data, 2006-2020

cces <- readRDS(paste0(path,"input_files/CCES_data/cumulative_2006-2020.Rds"))

## Subset CCES data to great plains states

cces <- subset(cces, cces$state%in%c("Colorado","Texas",
	"Kansas","Montana","Oklahoma","South Dakota","North Dakota",
		"New Mexico","Nebraska","Wyoming"))

## Merge policy preference with cumulative

cces <- merge(cces, table, by=c("case_id","year"), all.x=T)
save(cces, file=paste0(path,"output_files/cces.Rdata"))

## Load shapefile of zipcodes with the right data

zip <- readOGR(paste0(path,"input_files/zip_code/cb_2018_us_zcta510_500k.shp"))
zip <- spTransform(zip, CRS("+init=epsg:4326"))
zip_sample <- zip[zip@data$ZCTA5CE10%in%cces$zipcode,]

## Compute aquifer intersection with zip code and a distance to boundary variable

zip_sample@data$aquifer <- NA
zip_sample@data$in_state <- NA
zip_sample@data$longitude <- NA
zip_sample@data$latitude <- NA
zip_sample@data$dist <- NA
zip_sample@data$boundary_lon <- NA
zip_sample@data$boundary_lat <- NA
zip_sample@data$boundary_state <- NA
zip_sample@data$inside_aquifer <- NA

for (i in 1:nrow(zip_sample@data)){
		intersections <- gIntersection(zip_sample[i,], aquifer)
		zip_sample@data$aquifer[i] <- ifelse(length(intersections)==0, 0,
			sum(geosphere::areaPolygon(intersections))/geosphere::areaPolygon(zip_sample[i,]))

		xy <- gCentroid(zip_sample[i,]) ## zip centroid
		zip_sample@data$longitude[i] <- coordinates(xy)[1] ## long of county centroid
		zip_sample@data$latitude[i] <- coordinates(xy)[2] ## lat of county centroid
		zip_sample@data$in_state[i] <- names(states)[sp::over(xy,states)] ## name of state in which county located
		
		if(is.na(zip_sample@data$in_state[i])==F){
		state <- states[sp::over(xy,states),]
		state_line <- rgeos::intersect(aquifer_line, state) ## get within-state boundary segment
		}

		if(is.na(zip_sample@data$in_state[i])){
		state <- NA
		state_line <- NULL ## get within-state boundary segment
		}
	
		if(length(state_line)>0){
		dist <- geosphere::dist2Line(xy, state_line, distfun=distGeo) ## geodesic distance to closest point on within-state boundary segment
		zip_sample@data$dist[i] <- dist[,1]/1000 ## in kilometers
		zip_sample@data$boundary_lon[i] <- dist[,2] ## longitude of closest point on boundary
		zip_sample@data$boundary_lat[i] <- dist[,3] ## latitute of closest point on boundary
		mydf <- data.frame(longitude=dist[,2], latitude=dist[,3])
		coordinates(mydf) <- cbind(mydf$longitude , mydf$latitude)
		proj4string(mydf) = CRS("+init=epsg:4326")
		zip_sample@data$boundary_state[i] <- names(states)[sp::over(mydf,states)] ## name of state in which closest point on boundary located
		zip_sample@data$inside_aquifer[i] <- ifelse(is.na(sp::over(xy, aquifer)[1,]), 0, 1) ## is county centroid inside aquifer					
		}
		
		if(length(state_line)==0){
		zip_sample@data$dist[i] <- NA
		zip_sample@data$boundary_lon[i] <- NA
		zip_sample@data$boundary_lat[i] <- NA
		zip_sample@data$boundary_state[i] <- NA
		zip_sample@data$inside_aquifer[i] <- NA	
		}

	print(i)
}

## Merge aquifer-county spatial data back to cumulative policy preference file

save(zip_sample, file=paste0(path, "output_files/zip_zample.RData"))
zip_sample@data$aquifer <- round(zip_sample@data$aquifer, digits=3)
temp3 <- merge(cces, zip_sample@data, by.x="zipcode", by.y="ZCTA5CE10", all.x=T)
save(temp3, file=paste0(path,"output_files/tab5.Rdata"))

#####################
## ONLINE APPENDIX ##
#####################

## FIGURE A1-A5 
###############

# Not based on statistical analysis


## FIGURE A6: Scatter plot of Accuracy of computer vision estimates
###################################################




## Figure A7: Map of Great Plains counties shaded by center-pivot irrigation quartile
####################################################




## Table A1: Descriptive statistics
###################################




## Table A2: Main results full table (full sample)
####################################

# code produced above


## Table A3: Main results full table (200 km sample)
####################################

# code produced above


## Table A4: Main results full table (100 km sample)
####################################

# code produced above


## Table A5. Pre- and Post-technological Shock Trends
##################################################


# code produced above


## Table A6: Potential Channels (Table Version of Figure 7 inMain Paper)
######################################################

# code produced above 



## Table A7: Impact of Technological Shock on Conservative Voting (County Fixed Effects)
#####################################################


# code produced above


## Table A8: Impact of Technological Shock on Conservative Voting (2000-2020 Endline)
######################################################

## Load panel shapefile of county boundaries and intersection with aquifer

load(paste0(path,"output_files/county_panel.RData"))
load(paste0(path,"output_files/dat.RData"))
load(paste0(path,"output_files/dat200.RData"))
load(paste0(path,"output_files/dat100.RData"))

## Load election dataset 

load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_presidential_county_returns_1868_2020.RData"))
load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_gubernatorial_county_returns_1865_2020.RData"))
load(paste0(path,"input_files/County_Level_US_Elections_Data/dataverse_shareable_us_senate_county_returns_1908_2020.RData"))

## Merge and format presidential elections dataset with aquifer data

county_panel@data$fips <- paste0(substr(county_panel@data$NHGISST, 1, 2),    ## Assign each county a fips code
	substr(county_panel@data$NHGISCTY, 1, 3))
county_panel@data$seed <- 1:nrow(county_panel)

## Create a decade variable

pres_elections_release$DECADE <- ifelse(pres_elections_release$election_year%in%1900:1909, 1900,
	ifelse(pres_elections_release$election_year%in%1910:1919, 1910,
		ifelse(pres_elections_release$election_year%in%1920:1929, 1920,
			ifelse(pres_elections_release$election_year%in%1930:1939, 1930,
				ifelse(pres_elections_release$election_year%in%1940:1949, 1940,
	ifelse(pres_elections_release$election_year%in%1950:1959, 1950,
		ifelse(pres_elections_release$election_year%in%1960:1969, 1960,
			ifelse(pres_elections_release$election_year%in%1970:1979, 1970,
	ifelse(pres_elections_release$election_year%in%1980:1989, 1980,
		ifelse(pres_elections_release$election_year%in%1990:1999, 1990,
	ifelse(pres_elections_release$election_year%in%2000:2009, 2000, 
			ifelse(pres_elections_release$election_year%in%2010:2020, 2000, NA))))))))))))

senate_elections_release$DECADE <- ifelse(senate_elections_release$election_year%in%1900:1909, 1900,
	ifelse(senate_elections_release$election_year%in%1910:1919, 1910,
		ifelse(senate_elections_release$election_year%in%1920:1929, 1920,
			ifelse(senate_elections_release$election_year%in%1930:1939, 1930,
				ifelse(senate_elections_release$election_year%in%1940:1949, 1940,
	ifelse(senate_elections_release$election_year%in%1950:1959, 1950,
		ifelse(senate_elections_release$election_year%in%1960:1969, 1960,
			ifelse(senate_elections_release$election_year%in%1970:1979, 1970,
	ifelse(senate_elections_release$election_year%in%1980:1989, 1980,
		ifelse(senate_elections_release$election_year%in%1990:1999, 1990, 
	ifelse(senate_elections_release$election_year%in%2000:2009, 2000,
		ifelse(senate_elections_release$election_year%in%2010:2020, 2010, NA))))))))))))

gov_elections_release$DECADE <- ifelse(gov_elections_release$election_year%in%1900:1909, 1900,
	ifelse(gov_elections_release$election_year%in%1910:1919, 1910,
		ifelse(gov_elections_release$election_year%in%1920:1929, 1920,
			ifelse(gov_elections_release$election_year%in%1930:1939, 1930,
				ifelse(gov_elections_release$election_year%in%1940:1949, 1940,
	ifelse(gov_elections_release$election_year%in%1950:1959, 1950,
		ifelse(gov_elections_release$election_year%in%1960:1969, 1960,
			ifelse(gov_elections_release$election_year%in%1970:1979, 1970,
	ifelse(gov_elections_release$election_year%in%1980:1989, 1980,
		ifelse(gov_elections_release$election_year%in%1990:1999, 1990,
			ifelse(gov_elections_release$election_year%in%2000:2009, 2000,
		ifelse(gov_elections_release$election_year%in%2010:2020, 2010, NA))))))))))))

## Subset to relevant time periods and states

pres <- subset(pres_elections_release, pres_elections_release$election_year%in%1910:2020 &
	pres_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX"))

sen <- subset(senate_elections_release, senate_elections_release$election_year%in%1910:2020 &
	senate_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX"))

gov <- subset(gov_elections_release, gov_elections_release$election_year%in%1910:2020 &
	gov_elections_release$state%in%c("MT","ND","SD","WY","NE","CO","KS","OK","NM","TX"))

dat$pres_alt <- NA
dat$sen_alt <- NA
dat$gov_alt <- NA
for (i in 1:nrow(dat)){
	if(dat$post[i]==0){
	dat$pres_alt[i] <- dat$pres[i]
	dat$sen_alt[i] <- dat$sen[i]
	dat$gov_alt[i] <- dat$gov[i]
	}

	if(dat$post[i]==1){
	seed <- which(pres$fips==dat$fips[i] & pres$DECADE%in%c(2000,2010))
	dat$pres_alt[i] <- ifelse(length(seed)>0, mean(pres$republican_raw_votes[seed]/pres$pres_raw_county_vote_totals_two_party[seed]), NA)

	seed1 <- which(sen$fips==dat$fips[i] & sen$DECADE%in%c(2000,2010))
	dat$sen_alt[i] <- ifelse(length(seed1)>0, mean(sen$republican_raw_votes[seed1]/sen$senate_raw_county_vote_totals_two_party[seed1]), NA)

	seed2 <- which(gov$fips==dat$fips[i] & gov$DECADE%in%c(2000,2010))
	dat$gov_alt[i] <- ifelse(length(seed2)>0, mean(gov$republican_raw_votes[seed2]/gov$gov_raw_county_vote_totals_two_party[seed2]), NA)
	}

print(i)

}

alt_dat <- dat

save(alt_dat, file=paste0(path, "output_files/alt_dat.Rdata"))

dat200$pres_alt <- NA
dat200$sen_alt <- NA
dat200$gov_alt <- NA
for (i in 1:nrow(dat200)){
	if(dat$post[i]==0){
	dat200$pres_alt[i] <- dat200$pres[i]
	dat200$sen_alt[i] <- dat200$sen[i]
	dat200$gov_alt[i] <- dat200$gov[i]
	}

	if(dat200$post[i]==1){
	seed <- which(pres$fips==dat200$fips[i] & pres$DECADE%in%c(2000,2010))
	dat200$pres_alt[i] <- ifelse(length(seed)>0, mean(pres$republican_raw_votes[seed]/pres$pres_raw_county_vote_totals_two_party[seed]), NA)

	seed1 <- which(sen$fips==dat200$fips[i] & sen$DECADE%in%c(2000,2010))
	dat200$sen_alt[i] <- ifelse(length(seed1)>0, mean(sen$republican_raw_votes[seed1]/sen$senate_raw_county_vote_totals_two_party[seed1]), NA)

	seed2 <- which(gov$fips==dat200$fips[i] & gov$DECADE%in%c(2000,2010))
	dat200$gov_alt[i] <- ifelse(length(seed2)>0, mean(gov$republican_raw_votes[seed2]/gov$gov_raw_county_vote_totals_two_party[seed2]), NA)
	}

print(i)

}

alt_dat200 <- dat200
save(alt_dat200, file=paste0(path, "output_files/alt_dat200.Rdata"))

dat100$pres_alt <- NA
dat100$sen_alt <- NA
dat100$gov_alt <- NA
for (i in 1:nrow(dat100)){
	if(dat$post[i]==0){
	dat100$pres_alt[i] <- dat100$pres[i]
	dat100$sen_alt[i] <- dat100$sen[i]
	dat100$gov_alt[i] <- dat100$gov[i]
	}

	if(dat100$post[i]==1){
	seed <- which(pres$fips==dat100$fips[i] & pres$DECADE%in%c(2000,2010))
	dat100$pres_alt[i] <- ifelse(length(seed)>0, mean(pres$republican_raw_votes[seed]/pres$pres_raw_county_vote_totals_two_party[seed]), NA)

	seed1 <- which(sen$fips==dat100$fips[i] & sen$DECADE%in%c(2000,2010))
	dat100$sen_alt[i] <- ifelse(length(seed1)>0, mean(sen$republican_raw_votes[seed1]/sen$senate_raw_county_vote_totals_two_party[seed1]), NA)

	seed2 <- which(gov$fips==dat100$fips[i] & gov$DECADE%in%c(2000,2010))
	dat100$gov_alt[i] <- ifelse(length(seed2)>0, mean(gov$republican_raw_votes[seed2]/gov$gov_raw_county_vote_totals_two_party[seed2]), NA)
	}

print(i)

}

alt_dat100 <- dat100
save(alt_dat100, file=paste0(path, "output_files/alt_dat100.Rdata"))

## Figure A9: Pre- and Post-shock Trends Controlling for County-specific Linear Trends
#######################################################


# code produced above


## Table A9: Partisan Bias in Campaign Contributions
####################################################

# Load DIME data

load(paste0(path,"input_files/dime/dime_contributors_1979_2014.RData"))

# Subset data to contributors in relevant states

great_plains_states <- c("CO", "SD", "NE", "KS", "OK", "TX", "NM", "WY", "ND", "MT")
dime <- subset(contribs,contribs$most.recent.contributor.state %in%great_plains_states)

# Link each contributor to a zipcode

cleaned_string <- gsub("\\s+", "", dime$most.recent.contributor.zipcode)
zipcode <- substr(cleaned_string, 1, 5)

# link to shapefile

zip <- readOGR(paste0(path,"input_files/zip_code/cb_2018_us_zcta510_500k.shp"))
zip <- spTransform(zip, CRS("+init=epsg:4326"))
zip_GP <- zip[zip@data$ZCTA5CE10%in%unique(zipcode),]
zip_centroids <- gCentroid(zip_GP, byid=T)

load(paste0(path, "output_files/county_panel.RData"))
counties <- county_panel[county_panel@data$DECADE==1990,]
assigned_counties <- over(zip_centroids, counties)

zip_GP <- zip_GP[is.na(assigned_counties$STATENAM)==F,]
assigned_counties <- assigned_counties[is.na(assigned_counties$STATENAM)==F,]
plot(zip_GP)

# compute CF scores by county

for (i in 1:nrow(counties)){
	zips <- which(assigned_counties$STATENAM==counties@data$STATENAM[i] &
		assigned_counties$NHGISNAM==counties@data$NHGISNAM[i])
	contrib_data <- dime[zipcode%in%zip_GP@data$ZCTA5CE10[zips],]
	dollar_weight <- (contrib_data$amount_1980+contrib_data$amount_1982+contrib_data$amount_1984+
	contrib_data$amount_1986+contrib_data$amount_1988+contrib_data$amount_1990+
		contrib_data$amount_1992+contrib_data$amount_1994+contrib_data$amount_1996+
	contrib_data$amount_1998+contrib_data$amount_2000)
	counties@data$contributor.cfscore[i] <- mean(contrib_data$contributor.cfscor, na.rm=T)
	counties@data$weighted_contributor.cfscore[i]<- weighted.mean(contrib_data$contributor.cfscor, dollar_weight, na.rm = TRUE)
print(i)
}

save(counties, file=paste0(path,"output_files/cf_scores.RData"))


