############################################################################################################
#### Replication for: Prevalence and predictors of wind energy opposition in North America (Figures) #######
############################################################################################################

# Set Working Directory #

# Load Packages #
library(ggplot2)
library(dplyr)
library(rgdal)
library(sf)
library(grid)

# Write Fig #
writefig<-function(name,w,h,...){
  png(file=paste0("figures/",name,".png"),
      res=300,width=w,height=h,units="in",
      ...)
}

# Set Options #

options(max.print = 100000)
`%notin%` <- Negate(`%in%`)  

#### Load Data ####  

wind_data_usa <- read.csv("wind_data_usa.csv")
wind_data_usa$country <- "USA"
wind_data_canada <- read.csv("wind_data_canada.csv")
wind_data_canada$country <- "Canada"

wind_data_tot <- wind_data_usa %>% bind_rows(wind_data_canada)
wind_data_tot$year_plot <- as.numeric(wind_data_tot$year)
wind_data_tot$year_plot[which(wind_data_tot$country == "Canada")] <- as.numeric(wind_data_tot$year[which(wind_data_tot$country == "Canada")])

wind_data_tot$anti_wind_plot <- as.numeric(wind_data_tot$is_anti_wind)
wind_data_tot$anti_wind_plot[which(wind_data_tot$country == "Canada")] <- as.numeric(wind_data_tot$is_anti_wind[which(wind_data_tot$country == "Canada")])

#### Figures ####
#### Opposition over time ####
bar_samp <- wind_data_tot %>%  
  group_by(country, year_plot) %>% 
  dplyr::summarise(n_plants = n()) 

bar_samp_oppo <- wind_data_tot %>% 
  group_by(country, year_plot, anti_wind_plot) %>% 
  dplyr::summarise(n_oppo = n()) 

samp_plot <- bar_samp %>% 
  left_join(bar_samp_oppo)

samp_plot$anti_wind_plot <- ordered(samp_plot$anti_wind_plot, levels = c(0,1), labels = c("No opposition", "Opposition"))

perc_plot <- samp_plot %>% 
  filter(anti_wind_plot == "Opposition")

no_oppo_years <- data.frame(country = c("Canada", "Canada", "Canada", "Canada", "Canada", "Canada", "USA"),
                            year_plot = c(2001, 2003, 2004, 2005, 2007, 2008, 2003),
                            n_oppo = c(0,0,0,0,0,0,0),
                            n_plants = c(8, 2, 4, 6, 8, 10, 38),
                            anti_wind_plot = "Opposition" )

perc_plot <- perc_plot %>% 
  bind_rows(no_oppo_years) %>%
  mutate(oppo_perc = n_oppo/n_plants * 100)

samp_plot$country <- ordered(samp_plot$country, levels = c("USA", "Canada"), labels = c("USA", "Canada"))
perc_plot$country <- ordered(perc_plot$country, levels = c("USA", "Canada"), labels = c("USA", "Canada"))


# Bar count
bar <- ggplot(samp_plot, aes(x=year_plot, y= n_oppo, fill = anti_wind_plot)) +
  geom_bar(position=position_stack(vjust=0.5),stat="identity") +
  facet_wrap(~ country) + 
  labs(x = "Year",
       y = "Number of Plants",
       title = "Wind Plants and Opposition Over Time") +  
  scale_fill_manual(values = c("No opposition" = "blue", "Opposition" = "red"))+ 
  theme_bw()+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        legend.direction = "horizontal") 
bar

writefig("wind_oppo_bar", 11/2.54, 11/2.54)
bar
dev.off()

# Scatter percent
scatter <- ggplot(perc_plot, aes(x=year_plot, y= oppo_perc)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  facet_wrap(~ country) + 
  theme_bw() +
  labs(x = "Year",
       y = "Percentage of Plants with Opposition",
       title = "Wind Opposition Over Time") 
scatter

writefig("wind_oppo_scatter",  11/2.54, 11/2.54)
scatter
dev.off()

#### Map ####
# Necessary Shapefiles for Replication:
  # Mexico and Greenland (read in as mexico_greenland)
  # USA and Canada (read in as us_can)
  # Separate shapefile for Hawaii (read in as hawaii)

us_can_lcc <- st_transform(us_can, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-97 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
hawaii_lcc <- st_transform(hawaii, CRS("+proj=lcc +lat_1=18 +lat_2=23 +lat_0=15 +lon_0=-165 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
mexico_greenland_lcc <- st_transform(mexico_greenland, CRS("+proj=lcc +lat_1=18 +lat_2=23 +lat_0=15 +lon_0=-165 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

wind_data_lcc <- wind_data_tot %>% filter(state != "HI" | is.na(state))
wind_data_lcc <-  st_as_sf(wind_data_lcc , coords = c("longitude", "latitude"), crs = 4326)
wind_data_lcc <- st_transform(wind_data_lcc, CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-97 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

wind_data_lcc <- wind_data_lcc %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

wind_data_hawaii <- wind_data_tot %>% filter(state == "HI")
wind_data_hawaii <-  st_as_sf(wind_data_hawaii , coords = c("longitude", "latitude"), crs = 4326)
wind_data_hawaii_lcc <- st_transform(wind_data_hawaii, CRS("+proj=lcc +lat_1=18 +lat_2=23 +lat_0=15 +lon_0=-165 +x_0=6200000 +y_0=3000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
wind_data_hawaii_lcc <- wind_data_hawaii_lcc %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

map_xlim=list(c(2440000, 9040000))
map_ylim=list(c(-1300000,4900000))

map <- ggplot() + 
  geom_sf(data = us_can_lcc,
          fill = "white") + 
  geom_sf(data = mexico_greenland_lcc, 
          fill = "#D3D3D3") +
  geom_point(data = wind_data_lcc, 
             aes(x = lon, y = lat, 
                 colour = as.factor(anti_wind_plot)),
             shape = 20,
             size = 2.5,
             alpha = .275) +
  scale_colour_manual(values = c("black", "firebrick1"),
                      labels = c("No Opposition", "Opposition"),
                      name = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(.5,"line"),
        legend.text = element_text(size = 10),
        legend.position = c(0.9, 0.18),
        legend.background = element_rect(color = NA),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(size = 15, 
                                  face = "bold"),
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))+ 
  coord_sf(xlim=map_xlim[[1]], ylim=map_ylim[[1]]) + 
  theme(aspect.ratio = 1)


map.hawaii <- ggplot() + 
  geom_sf(data = hawaii_lcc,
          fill = "white") + 
  geom_point(data = wind_data_hawaii_lcc, 
             aes(x = lon, y = lat, 
                 colour = as.factor(anti_wind_plot)),
             shape = 20,
             size = 2.5,
             alpha = .275) +
  scale_colour_manual(values = c("black", "firebrick1"),
                      labels = c("No Opposition", "Opposition"),
                      name = NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none", 
        legend.key = element_blank(), 
        text=element_text(family="Helvetica"))+ 
  coord_sf(xlim=c(6500000,7500000), ylim=c(3150000, 4150000)) + 
  theme(aspect.ratio = 1) + 
  theme(plot.margin=unit(c(0,0,0,0),"cm"))    


v1 <- viewport(width=.95,height=.95,x=.5,y=.5)
v2 <- viewport(width=.15, height=.5,x=.16,y=.22)

png(file = paste("~/figures/", "wind_map_2",".png", sep = ""), width = 18/2.54, height = 22/2.54, units = "in", res = 300)
print(map, vp = v1)
print(map.hawaii, vp = v2)
dev.off()