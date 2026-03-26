library(data.table)
library(texreg)

# set working directory to appropriate location on your computer
# setwd("")

full_data_county <- fread("full_data_county.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_town <- fread("full_data_town.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_town_LBBH <- fread("full_data_town_LBBH.csv", header = TRUE, stringsAsFactors = FALSE)

# Table 3 County Analysis

tab3mod1a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1a)

tab3mod2a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2a)

tab3mod3a <- lm(wilson1916 ~ attack*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3a)

tab3mod4a <- lm(wilson1916 ~ coastal*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4a)

tab3mod5a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5a)

tab3mod6a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6a)

tab3mod1b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1b)

tab3mod2b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2b)

tab3mod3b <- lm(wilson1916 ~ attack*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3b)

tab3mod4b <- lm(wilson1916 ~ coastal*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4b)

tab3mod5b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5b)

tab3mod6b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6b)

wordreg(l = list(tab3mod1a, tab3mod1b, tab3mod2a, tab3mod2b, tab3mod3a, tab3mod3b,
                 tab3mod4a, tab3mod4b, tab3mod5a, tab3mod5b, tab3mod6a, tab3mod6b),
        file = "Table3.doc",
        custom.coef.map = list("beach" = "Beach County",
                               "attack" = "Attack County",
                               "coastal" = "Coastal County",
                               "Num_hotels_per1000_rescaled_noessex" = "Num. Hotels",
                               "beach:Num_hotels_per1000_rescaled_noessex" = "Beach:Num. Hotels",
                               "Num_hotels_per1000_rescaled_wessex" = "Num. Hotels",
                               "beach:Num_hotels_per1000_rescaled_wessex" = "Beach:Num. Hotels",
                               "attack:Num_hotels_per1000_rescaled_noessex" = "Attack:Num. Hotels",
                               "attack:Num_hotels_per1000_rescaled_wessex" = "Attack:Num. Hotels",
                               "coastal:Num_hotels_per1000_rescaled_noessex" = "Coastal:Num. Hotels",
                               "coastal:Num_hotels_per1000_rescaled_wessex" = "Coastal:Num. Hotels",
                               "Num_hotelrooms_per1000_rescaled_wessex" = "Num. Hotel Rooms",
                               "beach:Num_hotelrooms_per1000_rescaled_wessex" = "Beach:Num. Hotel Rooms",
                               "Num_hotelrooms_per1000_rescaled_noessex" = "Num. Hotel Rooms",
                               "beach:Num_hotelrooms_per1000_rescaled_noessex" = "Beach:Num. Hotel Rooms",
                               "attack:Num_hotelrooms_per1000_rescaled_wessex" = "Attack:Num. Hotel Rooms",
                               "attack:Num_hotelrooms_per1000_rescaled_noessex" = "Attack:Num. Hotel Rooms",
                               "coastal:Num_hotelrooms_per1000_rescaled_wessex" = "Coastal:Num. Hotel Rooms",
                               "coastal:Num_hotelrooms_per1000_rescaled_noessex" = "Coastal:Num. Hotel Rooms",
                               "machine_ab" = "Machine (Achen and Bartels)",
                               "machine_mayhew" = "Machine (Mayhew)",
                               "wilson1912" = "Wilson 1912 Vote Share",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Number of Hotel Rooms) and Exposure to Shark Attacks (County-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)

################################################################################

# Table 4 Town Level Analysis

tab4mod1a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1a)

tab4mod2a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2a)

tab4mod3a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3a)

tab4mod1b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1b)

tab4mod2b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2b)

tab4mod3b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3b)

tab4mod4a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4a)

tab4mod5a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5a)

tab4mod6a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6a)

tab4mod4b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4b)

tab4mod5b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5b)

tab4mod6b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6b)



wordreg(l = list(tab4mod1a, tab4mod1b, tab4mod2a, tab4mod2b, tab4mod3a, tab4mod3b,
                 tab4mod4a, tab4mod4b, tab4mod5a, tab4mod5b, tab4mod6a, tab4mod6b),
        file = "Table4.doc",
        custom.coef.map = list("Beach" = "Beach Town",
                               "Num_hotels_per100_rescaled1" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled1" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled1" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled1" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_per100_rescaled2" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled2" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled2" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled2" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_per100_rescaled3" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled3" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled3" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled3" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_per100_rescaled4" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled4" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled4" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled4" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_per100_rescaled5" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled5" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled5" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled5" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_per100_rescaled6" = "Num. Hotels",
                               "Beach:Num_hotels_per100_rescaled6" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_per100_rescaled6" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_per100_rescaled6" = "Beach Town:Num. Hotel Rooms",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Number of Hotel Rooms) and Exposure to Shark Attacks (Town-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)
