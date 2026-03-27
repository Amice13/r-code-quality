library(data.table)
library(readxl)

# set working directory to appropriate location on your computer
# setwd("")

# read in election results for 1913 and 1917 at the county and town level (with 
# beach/non-beach indicators), then calculate the share of the vote Wilson 
# received and the total number of votes, then subset to only the variables we
# will use later
counties_1913vote <- read_excel("1913ElectionResults_County.xlsx")
counties_1913vote$wilson1912 <- counties_1913vote$DemVotes / (counties_1913vote$GOPVotes + 
                                                                counties_1913vote$ProgVotes + 
                                                                counties_1913vote$DemVotes)
counties_1913vote <- subset(counties_1913vote, select = c(County, beach, 
                                                          machine_ab, machine_mayhew, 
                                                          attack, coastal, wilson1912))

towns_1913vote <- read_excel("1913ElectionResults_Town.xlsx")
towns_1913vote$wilson1912 <- towns_1913vote$DemVotes / (towns_1913vote$GOPVotes + 
                                                          towns_1913vote$ProgVotes + 
                                                          towns_1913vote$DemVotes)
towns_1913vote$total1912 <- towns_1913vote$GOPVotes + towns_1913vote$ProgVotes + towns_1913vote$DemVotes
towns_1913vote <- subset(towns_1913vote, select = c(County, City, wilson1912, total1912,
                                                    GOPVotes, ProgVotes, DemVotes,
                                                    Beach, NearBeach,
                                                    Both, Beach_FH, NearBeach_FH,
                                                    Both_FH, BoundaryChange_FH))
colnames(towns_1913vote)[5:7] <- c("GOPVotes1912", "ProgVotes1912", "DemVotes1912")

counties_1917vote <- read_excel("1917ElectionResults_County.xlsx")
counties_1917vote$wilson1916 <- counties_1917vote$DemVotes / (counties_1917vote$GOPVotes + counties_1917vote$DemVotes)
counties_1917vote$total1916 <- counties_1917vote$GOPVotes + counties_1917vote$DemVotes
counties_1917vote <- subset(counties_1917vote, select = c(County, wilson1916, total1916))

towns_1917vote <- read_excel("1917ElectionResults_Town.xlsx")
towns_1917vote$wilson1916 <- towns_1917vote$DemVotes / (towns_1917vote$GOPVotes + towns_1917vote$DemVotes)
towns_1917vote$total1916 <- towns_1917vote$GOPVotes + towns_1917vote$DemVotes
towns_1917vote <- subset(towns_1917vote, select = c(County, City, wilson1916, total1916,
                                                    GOPVotes, DemVotes))
colnames(towns_1917vote)[5:6] <- c("GOPVotes1916", "DemVotes1916")

# merge together vote totals across years for county and town level
counties_vote <- merge(counties_1913vote, counties_1917vote, by = "County")
towns_vote <- merge(towns_1913vote, towns_1917vote, by = c("County", "City"), all = TRUE)

# for some towns, we need to merge them together due to boundary changes or
# AHD coding of hotels (see supplemental information for more details); for each
# merge, we create a new row that is the sum/combination of the towns and then
# delete the rows for those individual towns

# merge together Atlantic Highlands, Highlands, Middletown, and Raritan
ahhmr <- towns_vote[which(towns_vote$City %in%c("Atlantic Highlands", "Highlands", "Middletown", "Raritan")),]
ahhmr_row <- c("Monmouth", "AHHMR", sum(ahhmr$DemVotes1912)/(sum(ahhmr$GOPVotes1912) +
                                                               sum(ahhmr$ProgVotes1912) +
                                                               sum(ahhmr$DemVotes1912)),
               sum(ahhmr$total1912), sum(ahhmr$GOPVotes1912),
               sum(ahhmr$ProgVotes1912), sum(ahhmr$DemVotes1912), 0, 0,
               1, 0, 1, 0, 0, sum(ahhmr$DemVotes1916)/(sum(ahhmr$GOPVotes1916) +
                                                         sum(ahhmr$DemVotes1916)),
               sum(ahhmr$total1916), sum(ahhmr$GOPVotes1916), sum(ahhmr$DemVotes1916))
towns_vote <- rbind(towns_vote, ahhmr_row)
towns_vote <- towns_vote[!(towns_vote$City %in%c("Atlantic Highlands", "Highlands", "Middletown", "Raritan")),]

# merge Middle and Stone Harbor
msh <- towns_vote[which(towns_vote$City %in%c("Middle", "Stone Harbor")),]
msh_row <- c("Cape May", "MSH", sum(as.numeric(msh$DemVotes1912), na.rm = TRUE)/
               (sum(as.numeric(msh$GOPVotes1912), na.rm = TRUE) +
                  sum(as.numeric(msh$ProgVotes1912), na.rm = TRUE) +
                  sum(as.numeric(msh$DemVotes1912), na.rm = TRUE)),
             sum(as.numeric(msh$total1912), na.rm = TRUE), sum(as.numeric(msh$GOPVotes1912), na.rm = TRUE),
             sum(as.numeric(msh$ProgVotes1912), na.rm = TRUE), sum(as.numeric(msh$DemVotes1912), na.rm = TRUE), 
             0, 0, 1, NA, NA, NA, 0, 
             sum(as.numeric(msh$DemVotes1916), na.rm = TRUE)/(sum(as.numeric(msh$GOPVotes1916), na.rm = TRUE) +
                                                                               sum(as.numeric(msh$DemVotes1916), 
                                                                                   na.rm = TRUE)),
             sum(as.numeric(msh$total1916), na.rm = TRUE), sum(as.numeric(msh$GOPVotes1916), na.rm = TRUE), 
             sum(as.numeric(msh$DemVotes1916), na.rm = TRUE))
towns_vote <- rbind(towns_vote, msh_row)
towns_vote <- towns_vote[!(towns_vote$City %in%c("Middle", "Stone Harbor")),]

# merge Brigantine City and East Atlantic City
bcea <- towns_vote[which(towns_vote$City %in%c("Brigantine City", "East Atlantic City")),]
bcea_row <- c("Atlantic", "BCEA", sum(as.numeric(bcea$DemVotes1912), na.rm = TRUE)/
                (sum(as.numeric(bcea$GOPVotes1912), na.rm = TRUE) +
                   sum(as.numeric(bcea$ProgVotes1912), na.rm = TRUE) +
                   sum(as.numeric(bcea$DemVotes1912), na.rm = TRUE)),
              sum(as.numeric(bcea$total1912), na.rm = TRUE), sum(as.numeric(bcea$GOPVotes1912), na.rm = TRUE),
              sum(as.numeric(bcea$ProgVotes1912), na.rm = TRUE), sum(as.numeric(bcea$DemVotes1912), na.rm = TRUE), 
              0, 0, 1, 1, 0, 0, 0, 
              sum(as.numeric(bcea$DemVotes1916), na.rm = TRUE)/(sum(as.numeric(bcea$GOPVotes1916), na.rm = TRUE) +
                                                                              sum(as.numeric(bcea$DemVotes1916), 
                                                                                  na.rm = TRUE)),
              sum(as.numeric(bcea$total1916), na.rm = TRUE), sum(as.numeric(bcea$GOPVotes1916), na.rm = TRUE), 
              sum(as.numeric(bcea$DemVotes1916), na.rm = TRUE))
towns_vote <- rbind(towns_vote, bcea_row)
towns_vote <- towns_vote[!(towns_vote$City %in%c("Brigantine City", "East Atlantic City")),]

# some town-level analyses also utilize change in vote totals and population

towns_vote$abspopchange <- abs((as.numeric(towns_vote$total1916) - 
                                  as.numeric(towns_vote$total1912))/as.numeric(towns_vote$total1912))
towns_vote$votechange <- as.numeric(towns_vote$wilson1916) - as.numeric(towns_vote$wilson1912)

# read in hotels data and a key file that links county/town names as specified
# in AHD and county/town names as specified by election returns
hotels <- read_excel("HotelData.xlsx")
hotels_key <- read_excel("Elections-Hotels Key.xlsx")

# aggregating total number of hotels by county
hotels_county <- aggregate(hotels$AHD, by = list(hotels$County), FUN = sum, na.rm = TRUE)
colnames(hotels_county) <- c("County_Hotels", "Num_hotels")

# aggregating total number of hotel rooms by county
hotelrooms_county <- aggregate(as.numeric(hotels$`Rooms-AHD`), by = list(hotels$County), FUN = sum, na.rm = TRUE)
colnames(hotelrooms_county) <- c("County_Hotels", "Num_hotelrooms")

# merging hotels and hotel rooms by county
hotels_county <- merge(hotels_county, hotelrooms_county, by = "County_Hotels")
colnames(hotels_county)[1] <- "County"

# aggregating total number of hotels by town as specified by AHD, then merging
# with key file so town names are consistent with election returns town names
hotels_town <- aggregate(hotels$AHD, by = list(hotels$County, hotels$Town), FUN = sum, na.rm = TRUE)
colnames(hotels_town) <- c("County_Hotels", "City_Hotels", "Num_hotels")
hotels_town <- merge(hotels_town, hotels_key, by = c("County_Hotels", "City_Hotels"), all.y = TRUE)
hotels_town$Num_hotels <- ifelse(is.na(hotels_town$Num_hotels), 0, hotels_town$Num_hotels)
hotels_town <- aggregate(hotels_town$Num_hotels, by = list(hotels_town$County, hotels_town$City), FUN = sum, na.rm = TRUE)
colnames(hotels_town) <- c("County", "City", "Num_hotels")

# aggregating total number of hotel rooms by town as specified by AHD, then merging
# with key file so town names are consistent with election returns town names
hotelrooms_town <- aggregate(as.numeric(hotels$`Rooms-AHD`), by = list(hotels$County, hotels$Town), FUN = sum, na.rm = TRUE)
colnames(hotelrooms_town) <- c("County_Hotels", "City_Hotels", "Num_hotelrooms")
hotelrooms_town <- merge(hotelrooms_town, hotels_key, by = c("County_Hotels", "City_Hotels"), all.y = TRUE)
hotelrooms_town$Num_hotelrooms <- ifelse(is.na(hotelrooms_town$Num_hotelrooms), 0, hotelrooms_town$Num_hotelrooms)
hotelrooms_town <- aggregate(as.numeric(hotelrooms_town$Num_hotelrooms), by = list(hotelrooms_town$County, hotelrooms_town$City), FUN = sum, na.rm = TRUE)
colnames(hotelrooms_town) <- c("County", "City", "Num_hotelrooms")

# merging hotels and hotel rooms for towns
hotels_town <- merge(hotels_town, hotelrooms_town, by = c("County", "City"))

# consolidating hotel data for same towns that required consolidation in the 
# elections data above

# Atlantic Highlands, Highlands, Middletown, and Raritan
ahhmr <- hotels_town[which(hotels_town$City %in%c("Atlantic Highlands", "Highlands", "Middletown", "Raritan", "Keansburg")),]
ahhmr_row <- c("Monmouth", "AHHMR", sum(ahhmr$Num_hotels), sum(ahhmr$Num_hotelrooms))
hotels_town <- rbind(hotels_town, ahhmr_row)
hotels_town <- hotels_town[!(hotels_town$City %in%c("Atlantic Highlands", "Highlands", "Middletown", "Raritan", "Keansburg")),]

# Middle and Stone Harbor
msh <- hotels_town[which(hotels_town$City %in%c("Middle", "Stone Harbor")),]
msh_row <- c("Cape May", "MSH", sum(as.numeric(msh$Num_hotels)), sum(as.numeric(msh$Num_hotelrooms)))
hotels_town <- rbind(hotels_town, msh_row)
hotels_town <- hotels_town[!(hotels_town$City %in%c("Middle", "Stone Harbor")),]

# Brigantine City and East Atlantic
bcea <- hotels_town[which(hotels_town$City %in%c("Brigantine City", "East Atlantic City")),]
bcea_row <- c("Atlantic", "BCEA", sum(as.numeric(bcea$Num_hotels)), sum(as.numeric(bcea$Num_hotelrooms)))
hotels_town <- rbind(hotels_town, bcea_row)
hotels_town <- hotels_town[!(hotels_town$City %in%c("Brigantine City", "East Atlantic City")),]

# merge elections data with hotels data, then make sure variables are of the
# appropriate class
full_data_county <- merge(counties_vote, hotels_county, by = "County")
full_data_town <- merge(towns_vote, hotels_town, by = c("County", "City"))
full_data_town$Num_hotels <- as.numeric(full_data_town$Num_hotels)
full_data_town$Num_hotelrooms <- as.numeric(full_data_town$Num_hotelrooms)
full_data_town$total1916 <- as.numeric(full_data_town$total1916)
full_data_town$County <- as.factor(full_data_town$County)

# the main analyses use measures of hotel industry presence that are per capita
# (per 1000 for counties and 100 for towns) and rescaled to range from 0 to 1; we
# also need to perform this rescaling for each unique subset in our models (e.g.,
# for counties we need to both include and exclude Essex)

# first merely creating the per capita measures
full_data_county$Num_hotels_per1000 <- full_data_county$Num_hotels / (full_data_county$total1916 / 1000)
full_data_county$Num_hotelrooms_per1000 <- full_data_county$Num_hotelrooms / (full_data_county$total1916 / 1000)
full_data_town$Num_hotels_per100_rescaled <- full_data_town$Num_hotels / (full_data_town$total1916 / 100)
full_data_town$Num_hotelrooms_per100_rescaled <- full_data_town$Num_hotelrooms / (full_data_town$total1916 / 100)

# now for counties, with and without essex
full_data_county$Num_hotels_per1000_rescaled_noessex[which(full_data_county$County!="Essex")] <- (full_data_county$Num_hotels_per1000[which(full_data_county$County!="Essex")] - min(full_data_county$Num_hotels_per1000[which(full_data_county$County!="Essex")]))/ (max(full_data_county$Num_hotels_per1000[which(full_data_county$County!="Essex")]) - min(full_data_county$Num_hotels_per1000[which(full_data_county$County!="Essex")]))
full_data_county$Num_hotelrooms_per1000_rescaled_noessex[which(full_data_county$County!="Essex")] <- (full_data_county$Num_hotelrooms_per1000[which(full_data_county$County!="Essex")] - min(full_data_county$Num_hotelrooms_per1000[which(full_data_county$County!="Essex")])) / (max(full_data_county$Num_hotelrooms_per1000[which(full_data_county$County!="Essex")]) - min(full_data_county$Num_hotelrooms_per1000[which(full_data_county$County!="Essex")]))

full_data_county$Num_hotels_per1000_rescaled_wessex <- (full_data_county$Num_hotels_per1000 - min(full_data_county$Num_hotels_per1000))/ (max(full_data_county$Num_hotels_per1000) - min(full_data_county$Num_hotels_per1000))
full_data_county$Num_hotelrooms_per1000_rescaled_wessex <- (full_data_county$Num_hotelrooms_per1000 - min(full_data_county$Num_hotelrooms_per1000)) / (max(full_data_county$Num_hotelrooms_per1000) - min(full_data_county$Num_hotelrooms_per1000))

# now for towns, creating the rescaled measures for each unique subset of towns in 
# models 1-6 (variables numbers to corresponding model)

full_data_town$Num_hotels_per100_rescaled1[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] <- (full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")])) / (max(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]) - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]))
full_data_town$Num_hotelrooms_per100_rescaled1[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] <- (full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")])) / (max(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]) - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]))

full_data_town$Num_hotels_per100_rescaled2[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] <- (full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))])) / (max(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]) - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]))
full_data_town$Num_hotelrooms_per100_rescaled2[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] <- (full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))])) / (max(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]) - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]))

full_data_town$Num_hotels_per100_rescaled3[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] <- (full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)])) / (max(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]) - min(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]))
full_data_town$Num_hotelrooms_per100_rescaled3[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] <- (full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)])) / (max(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]) - min(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]))

# town-level models 4-6 consolidate Long Beach and Beach Haven, so we create
# a new data frame with those two towns consolidated to one and then create
# 0-1 rescaled variable
full_data_town_LBBH <- rbind(full_data_town, data.frame("County" = "Ocean",
                                                        "City" = "LBBH",
                                                        "wilson1912" = sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))/(sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "total1912" = (sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "GOPVotes1912" = sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "ProgVotes1912" = sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "DemVotes1912" = sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Beach" = 1,
                                                        "NearBeach" = 0,
                                                        "Both" = 0,
                                                        "Beach_FH" = 1,
                                                        "NearBeach_FH" = 0,
                                                        "Both_FH" = 0,
                                                        "BoundaryChange_FH" = 0,
                                                        "wilson1916" = sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))/(sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "total1916" = (sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "GOPVotes1916" = sum(as.numeric(full_data_town$GOPVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "DemVotes1916" = sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "abspopchange" = ((sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))) - (sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))))/(sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "votechange" = sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))/(sum(as.numeric(full_data_town$DemVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1916[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))) - sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))/(sum(as.numeric(full_data_town$DemVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$GOPVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])) + sum(as.numeric(full_data_town$ProgVotes1912[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))),
                                                        "Num_hotels" = sum(as.numeric(full_data_town$Num_hotels[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotelrooms" = sum(as.numeric(full_data_town$Num_hotelrooms[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotels_per100_rescaled" = sum(as.numeric(full_data_town$Num_hotels_per100_rescaled[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotelrooms_per100_rescaled" = sum(as.numeric(full_data_town$Num_hotelrooms_per100_rescaled[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotels_per100_rescaled1" = sum(as.numeric(full_data_town$Num_hotels_per100_rescaled1[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotelrooms_per100_rescaled1" = sum(as.numeric(full_data_town$Num_hotelrooms_per100_rescaled1[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotels_per100_rescaled2" = sum(as.numeric(full_data_town$Num_hotels_per100_rescaled2[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotelrooms_per100_rescaled2" = sum(as.numeric(full_data_town$Num_hotelrooms_per100_rescaled2[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotels_per100_rescaled3" = sum(as.numeric(full_data_town$Num_hotels_per100_rescaled3[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")])),
                                                        "Num_hotelrooms_per100_rescaled3" = sum(as.numeric(full_data_town$Num_hotelrooms_per100_rescaled3[which(full_data_town$City=="Beach Haven" | full_data_town$City=="Long Beach")]))
))
full_data_town_LBBH <- full_data_town_LBBH[which(full_data_town_LBBH$City!="Beach Haven" & full_data_town_LBBH$City!="Long Beach"),]

full_data_town_LBBH$Num_hotels_per100_rescaled4[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_per100_rescaled4[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

full_data_town_LBBH$Num_hotels_per100_rescaled5[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_per100_rescaled5[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

full_data_town_LBBH$Num_hotels_per100_rescaled6[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_per100_rescaled6[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

# for some of our robustness checks, we also create binary measures for the presence
# of *any* hotels at the town level
full_data_town$any_hotels <- ifelse(full_data_town$Num_hotels>0, 1, 0)
full_data_town_LBBH$any_hotels <- ifelse(full_data_town_LBBH$Num_hotels>0, 1, 0)

# writing out final datasets used for analysis

fwrite(full_data_county, "full_data_county.csv", na = NA, quote = "auto")
fwrite(full_data_town, "full_data_town.csv", na = NA, quote = "auto")
fwrite(full_data_town_LBBH, "full_data_town_LBBH.csv", na = NA, quote = "auto")
