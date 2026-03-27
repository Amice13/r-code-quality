######Import Bormann and Golder Data#####

des <- import("Data/Democratic Electoral Systems/es_data-v4_0.dta")

#####select and clean relevant variables#####

des <- des %>%
  filter(region2 %in% c(9, 10)) %>% #Keep only West and East European countries
  select(country, date, regime, enep, ccode, tier1_avemag, region2, legislative_type)

des$country[des$country == 'West Germany'] <- 'Germany' #keep the Germanies together
 
des$ccode[des$country == 'Germany'] <- 255 #Fix country code difference

#Break up the date variable into day, month, and year#

des$Date <- mdy(des$date)

des$year <- year(des$Date)
des$month <- month(des$Date)
des$day <- day(des$Date)

#clean ENEP

des$enep <- ifelse(des$enep == -99, NA, des$enep)


#clean regime variable#

des$exec <- ifelse(des$regime == 0, 'Parliamentary',
                   ifelse(des$regime == 1, 'Semi-Presidential', 'Presidential'))



des$countryname <- des$country

#clean legislative type variable#

des$type <- ifelse(des$legislative_type == 1, 'Majoritarian',
                   ifelse(des$legislative_type == 2, 'Proportional',
                          ifelse(des$legislative_type == 3, 'Mixed', NA)))

#Fix Denmark 2015 election date

des$month[des$country == 'Denmark' & des$year == 2015] <- 06
des$day[des$country == 'Denmark' & des$year == 2015] <- 18

#Adjust region numbers to names#

des$region <- ifelse(des$region2 == 9, 'East', 'West')

#Drop unnecessary variables#

des <- des %>%
  select(-date, -regime, -region2) %>%
  filter(!is.na(enep)) %>%
  filter(countryname %in% sample)

test <- des%>%
  group_by(countryname) %>%
  dplyr::summarize(earliest = min(year),
            latest = max(year))
