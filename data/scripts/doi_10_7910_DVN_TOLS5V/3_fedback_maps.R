# --------------------------------------------------------------------- # 
# Federalism and Democratic Backsliding from a Comparative Perspective 
# Kaufman, Kelemen, Kolcak 
# Creating Maps for Partisan Control of States 
# --------------------------------------------------------------------- # 

rm(list = ls())

## set working directory, e.g. 
#setwd("~/Dropbox/fedbacksliding replication")

library(dplyr)
library(ggplot2)
library(data.table)
library(sf)
library(sp)
library(spData)
library(spDataLarge)
library(rgdal)
library(rgeos)
library(lwgeom)
library(data.table)
library(tigris)
library(tmap)
library(tmaptools)
library(maps)
library(countrycode)
library(grid)
library(gridExtra)
library(cowplot)
library(ggmap)
library(scales)
library(ggpubr)

######################################
## US - Fig. 2
# Created based on Appendix Table A9
######################################

us_states <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  shift_geometry() %>%
  filter(NAME != "Puerto Rico" & NAME != "District of Columbia")

subvote_2020 <- read.csv("us_subcontrol_2020.csv", strip.white = TRUE)
subvote_2020 <- subvote_2020 %>% filter(state != "District of Columbia")

us_states_joined1 <- us_states %>%
  left_join(subvote_2020, by = c("NAME" = "state"))

us_states_joined1$state.control
us_states_joined1$state.control <- as.factor(us_states_joined1$state.control)
levels(us_states_joined1$state.control)
us_states_joined1$state.control <- factor(us_states_joined1$state.control ,
                                          levels = c("Democratic", "Republican", "Divided"))

ggplot(us_states_joined1, aes(fill = state.control)) +
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("blue", "red", "orange")) + 
  theme_void() +
  labs(fill = "Party") +
  theme(
    legend.margin = margin(t = 10, r = 10, b = 10, l = 10))

#####################################
## Brazil - Fig. 3 
# Created based on Appendix Table A10
######################################

brazil <- st_read("gadm40_BRA_1.shp")
brazil <- rmapshaper::ms_simplify(brazil, keep = 0.1,
                                  keep_shapes = TRUE) # simplify admin 

brazil_meta <- read.csv("brazil-gubernatorial.csv")

brazil_states_joined1 <- brazil %>%
  left_join(brazil_meta, by = c("ID_1" = "ID_1"))

brazil_states_joined1$State.Control <- as.factor(brazil_states_joined1$State.Control)
table(brazil_states_joined1$State.Control)

brazil_states_joined1$State.Control <- factor(brazil_states_joined1$State.Control,levels=c(
  'Governing Party','Opposition Controlled','Neutral/Undefined'))

brazil_states_joined1$State.Control2 <- ifelse(brazil_states_joined1$State.Control == 'Governing Party', 'Aligned w/ Bolsonaro',
                                               ifelse(brazil_states_joined1$State.Control =='Opposition Controlled', 'Opposition',
                                                      ifelse(brazil_states_joined1$State.Control == 'Neutral/Undefined', 'Neutral', NA)))

brazil_states_joined1$State.Control2 <- as.factor(brazil_states_joined1$State.Control2)

brazil_states_joined1$State.Control2 <- factor(brazil_states_joined1$State.Control2,levels=c(
  'Aligned w/ Bolsonaro','Opposition','Neutral'))

brazil_states_joined1[brazil_states_joined1$NAME_1 == "Ceará",] <- "Opposition"

ggplot(brazil_states_joined1, aes(fill = State.Control2)) + 
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("darkgreen","firebrick1","darkgrey")) + 
  theme_void() + 
  labs(fill = "Party") + 
  theme(legend.margin = margin(t = 10, r = 10, b = 10, l = 10))

######################################
## Venezuela - Fig. 4 
# Created based on Appendix Table A11
######################################

venezuela <- st_read("gadm40_VEN_1.shp")
venezuela <- rmapshaper::ms_simplify(venezuela, keep = 0.1,
                                     keep_shapes = TRUE) # simplify admin 

venezuela_id <- venezuela %>% dplyr::select(NAME_1, ID_1)
venezuela_id$geometry <- NULL

venezuela <- venezuela %>% filter(ID_1 != "VEN.11_1" & ID_1 != "VEN.10_1")

## load venezuela meta
venezuela_meta <- read.csv("venezuela-regional.csv")

venezuela_meta$State.Control <- as.factor(venezuela_meta$State.Control)

## Merge with venezuela, 2000 
venezuela_meta_2000 <- venezuela_meta %>% filter(YEAR == 2000)

venezuela_states_joined_2000 <- venezuela %>%
  left_join(venezuela_meta_2000, by = c("ID_1" = "ID_1"))

vez_2000 <- ggplot(venezuela_states_joined_2000, aes(fill = State.Control)) + 
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("red","darkorange")) + 
  theme_void() + 
  labs(fill = "Party") + ggtitle("2000 Regional Elections") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

## Merge with venezuela, 2004
venezuela_meta_2004 <- venezuela_meta %>% filter(YEAR == 2004)

venezuela_states_joined_2004 <- venezuela %>%
  left_join(venezuela_meta_2004, by = c("ID_1" = "ID_1"))

vez_2004 <- ggplot(venezuela_states_joined_2004, aes(fill = State.Control)) + 
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("red","darkorange")) + 
  theme_void() + 
  labs(fill = "Party") + ggtitle("2004 Regional Elections") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

## Merge with venezuela, 2008
venezuela_meta_2008 <- venezuela_meta %>% filter(YEAR == 2008)

venezuela_states_joined_2008 <- venezuela %>%
  left_join(venezuela_meta_2008, by = c("ID_1" = "ID_1"))

vez_2008 <- ggplot(venezuela_states_joined_2008, aes(fill = State.Control)) + 
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("red","darkorange")) + 
  theme_void() + 
  labs(fill = "Party") + ggtitle("2008 Regional Elections") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

## Merge with venezuela, 2012
venezuela_meta_2012  <- venezuela_meta %>% filter(YEAR == 2012)

venezuela_states_joined_2012 <- venezuela %>%
  left_join(venezuela_meta_2012, by = c("ID_1" = "ID_1"))

vez_2012 <- ggplot(venezuela_states_joined_2012, aes(fill = State.Control)) + 
  geom_sf(color = "white", lwd = 0.2) + 
  scale_fill_manual(values = c("red","darkorange")) + 
  theme_void() + 
  labs(fill = "Party") + ggtitle("2012 Regional Elections")  +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

ggpubr::ggarrange(vez_2000, vez_2004, vez_2008, vez_2012,  ncol=2, nrow=2, common.legend = TRUE, legend="bottom") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")) 

######################################
## India  - Fig - 5 
# Created based on Appendix Table A12
######################################

india <- st_read("Admin2.shp")
india <- rmapshaper::ms_simplify(india, keep = 0.1,
                                  keep_shapes = TRUE) # simplify admin 

india_meta <- read.csv("india-vaishnav.csv")
india_meta$year2014 <- india_meta$X2014.label
india_meta$year2014 <- as.factor(india_meta$year2014)
india_meta$year2014_no <- india_meta$X2014.numeric
india_meta$year2019 <- india_meta$X2019.label
india_meta$year2019 <- as.factor(india_meta$year2019)
india_meta$year2019_no <- india_meta$X2019.numeric
# india_meta <- india_meta %>% filter(!is.na(year2019_no))
india_meta$ST_NM2 <- india_meta$ST_NM
india_meta$ST_NM <- NULL 
names(india_meta)
india_meta$year2014 <- as.factor(india_meta$year2014)

india <- india %>%  arrange(ST_NM)
india_meta_2014 <- india_meta %>% dplyr::select(ST_NM2 , year2014_no, year2014) %>% arrange(ST_NM2)
india_meta_2019 <- india_meta %>% dplyr::select(ST_NM2 , year2019_no, year2019) %>% arrange(ST_NM2)

india_states_joined2_2014 <- cbind(india, india_meta_2014)
india_states_joined2_2019 <- cbind(india, india_meta_2019)

india_states_joined2_2019$year2019 <- factor(india_states_joined2_2019$year2019, 
                                             levels = c("BJP" , "BJP allies","INC",  "INC allies", "Other",
                                                        ""))


india_states_joined2_2014$year2014 <- factor(india_states_joined2_2014$year2014, 
                                             levels = c("BJP","BJP allies","INC","INC allies", "Other",
                                                        "Indeterminate", ""))

india_14 <- india_states_joined2_2014 %>%
  filter(year2014 != "") %>%
  ggplot(aes(fill = year2014)) + 
  geom_sf(color = "black", lwd = 0.2) + 
  scale_fill_manual(values = c("orangered2", "orange", "navyblue", "turquoise4",
                               "mediumpurple2", "white"),
                    name = "2014 State Control") + 
  theme_void() +
  theme(
    legend.key=element_rect(colour="black"), 
    legend.position = "bottom",           
    legend.box = "horizontal",             
    legend.spacing.y = unit(0.3, "cm"),   
    legend.direction = "horizontal",     
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 10, r = 0, b = 30, l = 0),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(
    title.position = "top", 
    title.hjust = 0.5,        
    title.theme = element_text(face = "bold", size = 8),  
    keywidth = unit(0.5, "cm"),  
    keyheight = unit(0.4, "cm")  
  )) + ggtitle("2014 State Control")


india_18 <- india_states_joined2_2019 %>%
  filter(year2019 != "") %>%
  ggplot(aes(fill = year2019)) + 
  geom_sf(color = "black", lwd = 0.2) + 
  scale_fill_manual(values = c("orangered2", "orange", "navyblue",
                               "mediumpurple2"),
                    name = "2018 State Control") + 
  theme_void() +
  theme(
    legend.key=element_rect(colour="black"), 
    legend.position = "bottom",           
    legend.box = "horizontal",             
    legend.spacing.y = unit(0.2, "cm"),  
    legend.direction = "horizontal",      
    legend.text = element_text(size = 8), 
    plot.margin = margin(t = 10, r = 0, b = 30, l = 0),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
  ) +
  guides(fill = guide_legend(
    title.position = "top",   
    title.hjust = 0.5,       
    title.theme = element_text(face = "bold", size = 8), 
    keywidth = unit(0.5, "cm"),  
    keyheight = unit(0.4, "cm") 
  )) + ggtitle("2018 State Control")


ggpubr::ggarrange(india_14, india_18, nrow = 1, common.legend = F, legend="bottom") +
  theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")) 
