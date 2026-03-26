#	-----------------------------------------------------------------------------------------
#   File Name:      politymap.R                                                    
#   Author:         Omar García-Ponce and Leonard Wantchekon                                
#   Date:           April 20, 2023                                                 
#   Purpose:        Generate Figure 1 from “Critical Junctures...”                                                                                                                 
#   Machine:        Omar's MacBook Pro with macOS Monterey version 12.6                                                 
#	  Software:       R with RStudio 2022.07.2
#	-----------------------------------------------------------------------------------------

### Install the following packages 
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("rgdal")
#install.packages("broom")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("RColorBrewer")
#install.packages("gridExtra")
#install.packages("grid")

### Load libraries
library(dplyr)
library(tidyverse)
library(rgdal)
library(broom)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(gridExtra)
library(grid)

### Set working directory here

### NOTE: It is important to correctly specify the working directory and make 
### sure that the polity4_2010.dta file and the World Shapefile folder (which 
### contains the shape files World_Countries) are stored in the same path. 

### Import shapefile. Note that the folder containing the shapefile is called 
### "World Shapefile" and the name of shapefile is World_Countries.shp

world <- readOGR(dsn = "World Shapefile" , 
                 layer = "World_Countries")
glimpse(world)

### Convert spatial object to a ggplot ready data frame
world_tidy <- tidy(world)
ggplot(world_tidy, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "lightgrey" , size = 0.1, fill = "lightgrey") + 
  coord_equal() +
  theme_minimal()

### Make sure the shapefile attribute table has an id column
world$id <- row.names(world)
world_tidy <- left_join(world_tidy, world@data)

### Replace country isocode values to match with code in stata data
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Bosnia and Herzegovina", "Bosnia")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Congo", "Congo Kinshasa")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Democratic Republic of the Congo", "Congo Brazzaville")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Congo Brazzaville Kinshasa", "Congo Brazzaville")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Dominican Republic", "Dominican Rep")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "North Korea", "Korea North")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "South Korea", "Korea South")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Myanmar", "Myanmar (Burma)")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Slovakia", "Slovak Republic")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "Trinidad and Tobago", "Trinidad")
world_tidy$COUNTRY <- str_replace(world_tidy$COUNTRY, "United Arab Emirates", "UAE")

### Import stata file that contains polity variable for each country into a new data polity
library(haven)
polity2010 <- read_dta("polity4_2010.dta")

### Rename country name to math with country name in the shapefile
polity2010$COUNTRY <- polity2010$country
polity2010$Polity4 <- polity2010$polity2

### Joining polity data with world shapefile
world_tidy <- left_join(world_tidy, polity2010, by = "COUNTRY")

### Plot
p <- ggplot(world_tidy, aes(x = long, y = lat, group = group, fill = Polity4)) + 
  geom_polygon() + xlab("") + 
  ylab("") +
  coord_equal() + 
  theme_void() + 
  scale_fill_gradient(low="black", high = "lightgray", na.value = "white") +
  theme(legend.position = c(.1, .4))  

p 

### Save plot
ggsave("worldmap.pdf")

