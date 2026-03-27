#	-----------------------------------------------------------------------------------------
#   File Name:      ruralinsmap.R                                                    
#   Author:         Omar García-Ponce and Leonard Wantchekon                                
#   Date:           April 20, 2023                                                 
#   Purpose:        Generate Figure 2 from “Critical Junctures...”                                                                                                                 
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
### sure that  the rural_insurgency_coding.dta file and the Africa folder (which 
### contains the shape files afr_g2014_2013_0) are stored in the same path. 

### Import shapefile. Note that the folder containing the shapefile is called 
### "Africa" and the name of the shapefile is afr_g2014_2013_0.shp 

africa <- readOGR(dsn = "Africa" , 
                  layer = "afr_g2014_2013_0")
glimpse(africa)

### Convert spatial object to a ggplot ready data frame
africa_tidy <- tidy(africa)
ggplot(africa_tidy, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "lightgrey" , size = 0.1, fill = "lightgrey") + 
  coord_equal() +
  theme_minimal()

### Make sure the shapefile attribute table has an id column
africa$id <- row.names(africa)
africa_tidy <- left_join(africa_tidy, africa@data)

### Replace country isocode values to match with code in stata data
africa_tidy$ISO3 <- str_replace(africa_tidy$ISO3, "COD", "ZAR")
### Note that western africa is not included in Morocco

### Import stata file that contains polity variables for each country into a new data polity
library(haven)
my_data <- read_dta("rural_insurgency_coding.dta")

### Rename code as ISO3
my_data$ISO3 <-my_data$code

### Label ruralins variable
my_data$ruralins <- factor(my_data$ruralins,
                           levels = c(1,0),
                           labels = c("Rural insurgency", "Urban protest"))


### Joining stata data with africa shapefile using ISO3
africa_tidy <- left_join(africa_tidy, my_data, by = "ISO3")

### Plotting map
my_colors <- c("#666666", "#E6E6E6")  
p <- ggplot(africa_tidy, aes(x = long, y = lat, group = group,  fill = ruralins)) + 
  geom_polygon(color = "black" , size = 0.1) + xlab("") +
  ylab("") +
  scale_fill_manual(labels = c("Rural insurgency", "Urban protest", "No data "), na.value = "#ffffff", values = my_colors) +
  coord_equal() +
  theme_void()  +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.2, .3))  

p 

### Save plot
ggsave("africamap.pdf")



