########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS IS THE CODE (#5) THAT REPLICATES THE MAP FOUND IN FIG A2


########### NOTE: THIS CODE WILL TAKE SOME TIME TO RUN


#########################
#####     Figure A2   ###
#########################

# this figure is copied from Gould and Urpelainen (2018), as cited
# the code is repeated here

####  create state-level statistics

states <- data.frame(unique(RawDataHH$m1_q8_state))
states["n"] <- 0
states["lpg"] <- NA

for (i in 1:6) {
  states[i,2] <- sum(RawDataHH$Interviewed & RawDataHH$m1_q8_state == states[i,1], na.rm = TRUE) #n
  states[i,3] <- sum(RawDataHH$m4_q103_lpg & RawDataHH$m1_q8_state == states[i,1], na.rm = TRUE) #gas uptake
}

states["% lpg"] <- states$lpg / states$n

##### rename states

states["ST_NAME"] <- properCase(as.character(unique(states$unique.RawDataHH.m1_q8_state.)))
states[states=="West bengal"] <- "West Bengal"
states[states=="Uttar pradesh"] <- "Uttar Pradesh"
states[states=="Madhya pradesh"] <- "Madhya Pradesh"
states[states=="Odisha"] <- "Orissa" # Orissa was re-named Odisha in 2011 (but shapefile retains old nomenclature)

colnames(states) <- c("State", "n", "lpg", "Percent LPG Adoption", "ST_NAME")

####  create district-level statistics

districts <- data.frame(unique(RawDataHH$m1_q9_district))
districts["n"] <- 0
districts["lpg"] <- NA

for (i in 1:nrow(districts)) {
  districts[i,2] <- sum(RawDataHH$Interviewed & RawDataHH$m1_q9_district == districts[i,1]) #n
  districts[i,3] <- sum(RawDataHH$m4_q103_lpg & RawDataHH$m1_q9_district == districts[i,1]) #gas uptake
}

districts["% lpg"] <- districts$lpg / districts$n

#### rename districts

districts["District_Name"] <- properCase(as.character(unique(RawDataHH$m1_q9_district)))
districts[districts=="North twenty four parganas"] <- "North 24 Parganas"
districts[districts=="Khandwa (east nimar)"] <- "East Nimar"
districts[districts=="Paschim mednipur"] <- "Pashchim Medinipur"
districts[districts=="Bijnour"] <- "Bijnor"
districts[districts=="Bulandshahar"] <- "Bulandshahr"
districts[districts=="Siddharthnagar"] <- "Siddharth Nagar"
districts[districts=="Purwa champaran"] <- "Purba Champaran"

colnames(districts) <- c("District name", "n", "lpg", "Percent LPG Adoption", "DISTRICT")

##### shape file data

# state shapefile
india_shape <- readOGR(dsn = "~/Dataverse_Files/", layer = "INDIA") #### ** NOTE: SET YOUR FILE PATH ***

# district shapefile
districts_shape <- readOGR(dsn = "~/Dataverse_Files/", layer = "2011_Dist") #### ** NOTE: SET YOUR FILE PATH ***

# data join ACCESS to shapefile
districts_shape@data <- left_join(districts_shape@data, districts, by = "DISTRICT", all = TRUE)
india_shape@data <- left_join(india_shape@data, states, by = "ST_NAME", all = TRUE)

# remove NA districts (non ACCESS districts)
districts_shape_partial <- districts_shape[!is.na(districts_shape@data$`Percent LPG Adoption`), ]
india_shape_partial <- india_shape[!is.na(india_shape@data$`Percent LPG Adoption`),]

######## TWO MAPS TOGETHER: STATES AND DISTRICTS

country_map <- tm_shape(india_shape) + 
  tm_polygons(col = "white") +
  tm_borders(col = "black") +
  tm_shape(districts_shape_partial) +
  tm_polygons("Percent LPG Adoption", palette = "Blues") + 
  tm_borders(col = "gray50", lwd = 0.5) + 
  tm_legend(scale = 1.5, aes.color = c(borders = "black"), legend.position = c("right", "top"))

country_map     # may take several minutes

districts_map <- tm_shape(india_shape) + 
  tm_polygons(col="white") +
  tm_borders(col = "black") +
  tm_shape(india_shape_partial) + 
  tm_polygons("Percent LPG Adoption", palette = "Blues") +
  tm_borders(col = "gray50", lwd = 0.5) +
  tm_legend(scale = 1.5, aes.color = c(borders = "black"), legend.position = c("right", "top"))

districts_map     # may take several minutes

combined_map <- tmap_arrange(districts_map, country_map, ncol = 2) # combining maps

