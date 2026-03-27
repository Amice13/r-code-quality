###########################################################################################
### Script to create Figure A7: Definition of North and South West German labor markets ###
###########################################################################################

# check for packages needed and compare to list of installed packages, install if necessary
list.of.packages <- c("haven", "dplyr", "sf", "stringr", "ggplot2", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(haven)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)



# set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()


### Load data
df.main <- read_dta("../processed/workingdataset.dta")


### Load shapefile of Local Labor Markets
shp.LLM <- st_read("../data/shapefiles/Labor_Market_1966_Saar_for_mapping.shp")
shp.LLM$labor_market_id <- shp.LLM$Sht1__L
shp.LLM <- shp.LLM[shp.LLM$labor_market_id != 103,] # Drop the Saarland


### Join data with shapefile
shp.LLM <- left_join(shp.LLM,df.main[df.main$year==1926,c("labor_market_id","north1","north2")], 
                     by = "labor_market_id")

### Get German outer border
shp.bb <- shp.LLM %>% 
  st_union() %>% 
  st_cast("MULTILINESTRING")

### Get border of North, narrow definition
shp.n1 <- shp.LLM %>% 
  group_by(north1) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_cast("MULTILINESTRING") %>% 
  st_difference(shp.bb)

### Get border of North, broad definition
shp.n2 <- shp.LLM %>% 
  group_by(north2) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_cast("MULTILINESTRING") %>% 
  st_difference(shp.bb)

ggplot(shp.n2) +
  geom_sf()




### IV plot
(m.NorthSouth <- ggplot() + 
    # Local Labor Markets
    geom_sf(data = shp.LLM, color = "grey65", fill = "grey95") +
    # Narrow classification
    geom_sf(data = shp.n1, aes(color = "N1"), 
            fill = NA,
            size = 5,
            show.legend = "line",
            inherit.aes = FALSE) +
    # Broad classification
    geom_sf(data = shp.n2, aes(color = "N2" ), 
            fill = NA, 
            size = 5,
            show.legend = "line",
            inherit.aes = FALSE) + 
    # Color definition
    scale_color_manual(values = c("N1" = "red",
                                  "N2" = "green"),
                       label = c("Narrow classification",
                                 "Broad classification"),
                       name = "North-South borders", 
                       guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), 
                                                               shape = NA))) +
    # Theme
    theme_void() + 
    theme(legend.position = c(0.95,0.55), 
          legend.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(face = "bold"))
)


### Save and export the map
ggsave("../results/figures/figA7.eps", m.NorthSouth, dpi = 300, limitsize = TRUE,
       width = 10.0, height = 6.0, units = c("in"))

