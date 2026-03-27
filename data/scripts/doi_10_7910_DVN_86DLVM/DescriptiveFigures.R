library(data.table)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(taylor)
library(maps)
library(mapdata)
library(readxl)

# set working directory to appropriate location on your computer
# setwd("")

full_data_county <- fread("full_data_county.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_town_LBBH <- fread("full_data_town_LBBH.csv", header = TRUE, stringsAsFactors = FALSE)

# Figure 1/SI.1

states <- map_data("state")
njdf <- subset(states, region == "new jersey")

counties <- map_data("county")
njcounty <- subset(counties, region == "new jersey")

njbase <- ggplot(data = njdf, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
njbase + theme_nothing()

njbase + theme_nothing() + 
  geom_polygon(data = njcounty, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

njhotels <-data.frame("subregion" = tolower(full_data_county$County),
                      "hotels" = full_data_county$Num_hotels)

njcounty <- merge(njcounty, njhotels, by = "subregion")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

elbow_room1 <- njbase + 
  geom_polygon(data = njcounty, aes(fill = hotels), color = "black") +
  scale_fill_taylor_c(album = "folklore", direction = -1) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes + labs(fill='Hotels') 
elbow_room1

njhotelrooms <-data.frame("subregion" = tolower(full_data_county$County),
                      "hotelrooms" = full_data_county$Num_hotelrooms)

njcountyrooms <- merge(njcounty, njhotelrooms, by = "subregion")

elbow_room2 <- njbase + 
  geom_polygon(data = njcountyrooms, aes(fill = hotelrooms), color = "black") +
  scale_fill_taylor_c(album = "folklore", direction = -1) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes + labs(fill='Rooms') 
elbow_room2

grid.arrange(elbow_room1, elbow_room2, nrow = 1)
ggsave("state_hotels.png", grid.arrange(elbow_room1, elbow_room2, nrow = 1))

# Figure SI.2

# subsetting to the 71 observations in the most expansive model, Model 6
# in the town-level analyses

model6_data <- full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),]

model6_data$Proximity <- ifelse(model6_data$Beach==1, "Beach", NA)
model6_data$Proximity <- ifelse(model6_data$NearBeach==1, "Near-Beach", model6_data$Proximity)
model6_data$Proximity <- ifelse(model6_data$Both==1, "Both", model6_data$Proximity)

town_hotel_hist <- ggplot(model6_data, aes(x = as.numeric(Num_hotels), fill = Proximity)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ County) +
  labs(x = "Hotels",
       y = "Count of Towns") + 
  scale_fill_manual(values=c('grey20', 'grey50', 'grey80')) +
  theme_bw() +
  theme(legend.position = "none")

town_hotelrooms_hist <-  ggplot(model6_data, aes(x = as.numeric(Num_hotelrooms), fill = Proximity)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ County) +
  labs(x = "Hotel Rooms",
       y = "Count of Towns") +
  scale_fill_manual(values=c('grey20', 'grey50', 'grey80')) +
  theme_bw()

grid.arrange(town_hotel_hist, town_hotelrooms_hist, nrow = 1)
ggsave("town_hotels.png", grid.arrange(town_hotel_hist, town_hotelrooms_hist, nrow = 1, widths = c(0.45,0.55)), width = 10, height = 8)

town_hotel_hist_consol <- ggplot(model6_data, aes(x = as.numeric(Num_hotels), fill = Proximity)) +
  geom_histogram(bins = 30) +
  labs(x = "Hotels",
       y = "Count of Towns") + 
  scale_fill_manual(values=c('grey20', 'grey50', 'grey80')) +
  theme_bw() +
  theme(legend.position = "none")

town_hotelrooms_hist_consol <-  ggplot(model6_data, aes(x = as.numeric(Num_hotelrooms), fill = Proximity)) +
  geom_histogram(bins = 30) +
  labs(x = "Hotel Rooms",
       y = "Count of Towns") +
  scale_fill_manual(values=c('grey20', 'grey50', 'grey80')) +
  theme_bw()

ggsave("town_hotels_consolidated.png", grid.arrange(town_hotel_hist_consol, town_hotelrooms_hist_consol, nrow = 1, widths = c(0.45,0.55)), width = 10, height = 5)
