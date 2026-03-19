rm(list = ls())  # clear the working directory
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringi, strex, stringi, XML, xml2, readr)


# set path to directory with kmz files
path_to_kmz <- "~/Documents/shapefiles/kmz/"

# list and unzip all kmz files in a given folder path
KMZs <- list.files(path=path_to_kmz, pattern="*.kmz", full.names=FALSE)

# unzip each KMZ file and store in KML 
sapply(KMZs,function(x)unzip(zipfile = paste0(path_to_kmz, x),exdir =paste0(path_to_kmz, "KML/",x)))


# retrive all .kml files from directory 
kml_files <- list.files(pattern = ".kml$", recursive = TRUE)

# import .kml file
n = 1 # select which .kml file (by index) you want to convert from kml_files
data <- xmlParse(kml_files[n])
xml_data <- xmlToList(data) # this may take a few minutes

# make empty df to store output  
emptydf = data.frame()

# run loop for as many cells there are in .kml file 
n_cellz = length(xml_data$Document$Folder)

# extract data from .xml file and bind to empty df  (takes several minutes for ~60,000 points)
for (i in 2:n_cellz) {
  
  # Extract all items for one cell
  cellx = xml_data$Document$Folder[i]
  
  nm = cellx$Placemark$name #cell name
  ws = cellx$Placemark$ExtendedData[1]$Data$value # windspeed
  dir = cellx$Placemark$ExtendedData[2][1]$Data$value  # angle
  coord = cellx$Placemark$LineString$coordinates # coordinates
  
  lon = str_nth_number(coord, n = 1, decimals = TRUE, negs = TRUE)
  lat = str_nth_number(coord, n = 2, decimals = TRUE, negs = TRUE)
  
  
  df = as.data.frame(cbind(nm, lon, lat, ws, dir)) %>%
    mutate_at(vars(lon, lat, ws, dir), as.numeric)
  
  emptydf <- bind_rows(emptydf, df)
  
  
}


## save as .csv
# rebuild name string 
old_name <- kml_files[n]
x = strsplit(old_name, '[-_]')[[1]]
new_name <- paste0("Maui_point_", x[3], "-", x[4], "-", x[5], "_", x[12])

# create directory to store .csv
dir.create(file.path(paste0(path_to_kmz, "csv/")))
write_csv(emptydf, paste0(path_to_kmz, "csv/", new_name, ".csv"))   #save



