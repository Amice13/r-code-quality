library(readxl)

hotels <-read_xlsx("HotelData.xlsx")

# fill NAs with zeroes
hotels$RedBook <- ifelse(is.na(hotels$RedBook), 0, hotels$RedBook)
hotels$Willys <- ifelse(is.na(hotels$Willys), 0, hotels$Willys)
hotels$AHD <- ifelse(is.na(hotels$AHD), 0, hotels$AHD)

# how many hotels are listed in 1, 2, or 3 guides?
table(rowSums(hotels[,4:6]))

# how well does coverage compare across guides?
table(hotels$RedBook, hotels$AHD)
table(hotels$Willys, hotels$AHD)

# at the county level
county_names <- unique(hotels$County)
county_coverage <- data.frame("County" = character(),
                              "AHD_count" = numeric(),
                              "Willys_count" = numeric(),
                              "RedBook_count" = numeric(),
                              "AHD_Willys" = numeric(),
                              "AHD_RedBook" = numeric())

for(i in 1:21){
  county_hotels <- hotels[which(hotels$County==county_names[i]),]
  AHD_count <- sum(county_hotels$AHD)
  Willys_count <- sum(county_hotels$Willys)
  RedBook_count <- sum(county_hotels$RedBook)
  AHD_Willys <- sum(county_hotels$AHD==1 & county_hotels$Willys==1)
  AHD_RedBook <- sum(county_hotels$AHD==1 & county_hotels$RedBook==1)
  county_coverage <-  rbind(county_coverage, data.frame("County" = county_names[i],
                                                        "AHD_count" = AHD_count,
                                                        "Willys_count" = Willys_count,
                                                        "RedBook_count" = RedBook_count,
                                                        "AHD_Willys" = AHD_Willys,
                                                        "AHD_RedBook" = AHD_RedBook))
}

# how well are guides correlated at the county level?
cor(county_coverage$AHD_count, county_coverage$Willys_count)
cor(county_coverage$AHD_count, county_coverage$RedBook_count)

# at the town level for the four counties we are interested in
town_names <- unique(hotels$Town)
town_coverage <- data.frame("county" = character(),
                            "town" = character(),
                            "AHD_count" = numeric(),
                            "Willys_count" = numeric(),
                            "RedBook_count" = numeric(),
                            "AHD_Willys" = numeric(),
                            "AHD_RedBook" = numeric())
for(i in 1:221){
  town_hotels <- hotels[which(hotels$Town==town_names[i]),]
  AHD_count <- sum(town_hotels$AHD)
  Willys_count <- sum(town_hotels$Willys)
  RedBook_count <- sum(town_hotels$RedBook)
  AHD_Willys <- sum(town_hotels$AHD==1 & town_hotels$Willys==1)
  AHD_RedBook <- sum(town_hotels$AHD==1 & town_hotels$RedBook==1)
  town_coverage <-  rbind(town_coverage, data.frame("county" = town_hotels$County[1],
                                                    "town" = town_names[i],
                                                    "AHD_count" = AHD_count,
                                                    "Willys_count" = Willys_count,
                                                    "RedBook_count" = RedBook_count,
                                                    "AHD_Willys" = AHD_Willys,
                                                    "AHD_RedBook" = AHD_RedBook))
}

# aggregate town units to match with those identified in the elections data
key <- read_excel("Elections-Hotels Key.xlsx")

town_coverage <- merge(town_coverage, key, by.x = c("county", "town"), by.y = c("County", "City"),
                       all = TRUE)
town_coverage$RedBook_count <- ifelse(is.na(town_coverage$RedBook_count), 0, town_coverage$RedBook_count)
town_coverage$Willys_count <- ifelse(is.na(town_coverage$Willys_count), 0, town_coverage$Willys_count)
town_coverage$AHD_count <- ifelse(is.na(town_coverage$AHD_count), 0, town_coverage$AHD_count)

# how well are guides correlated at the county level?
cor(town_coverage$AHD_count[which(town_coverage$county %in% c("Atlantic", "Cape May", "Monmouth", "Ocean"))], 
    town_coverage$Willys_count[which(town_coverage$county %in% c("Atlantic", "Cape May", "Monmouth", "Ocean"))])
cor(town_coverage$AHD_count[which(town_coverage$county %in% c("Atlantic", "Cape May", "Monmouth", "Ocean"))], 
    town_coverage$RedBook_count[which(town_coverage$county %in% c("Atlantic", "Cape May", "Monmouth", "Ocean"))])
