#############################################################################################################
### Script to create Figure 4: Weighted least-cost distance to European coalfields                        ###
#############################################################################################################

# check for packages needed and compare to list of installed packages, install if necessary
list.of.packages <- c("haven", "dplyr", "sf", "stringr", "ggplot2", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(haven)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)
library(ggpubr)


# set working directory to current file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()


### Load data
df.main <- read_dta("../processed/workingdataset.dta")


### Load shapefile of Local Labor Markets
shp.LLM <- st_read("../data/shapefiles/Labor_Market_1966_Saar_for_mapping.shp")
shp.LLM$labor_market_id <- shp.LLM$Sht1__L
shp.LLM <- st_transform(shp.LLM, crs = "EPSG:4326")

### Load Coastline
shp.coast <- st_read("../data/shapefiles/ne_10m_coastline.shp")
shp.coast <- st_transform(shp.coast, crs = st_crs(shp.LLM))
shp.coast <- shp.coast %>% 
  st_make_valid()


### Load Coalfields
shp.coal <- st_read("../data/shapefiles/coalshapeunion.shp")
shp.coal <- st_transform(shp.coal, crs = st_crs(shp.LLM))

### Load Major Rivers
shp.rivers <- st_read("../data/shapefiles/ne_10m_rivers_lake_centerlines.shp")
shp.rivers <- st_transform(shp.rivers, crs = st_crs(shp.LLM))



### Prepare data
df.map <- df.main[df.main$year==1926,c("labor_market_id","log_coal_access1")] 



### Calculate quantiles
qq.l <- quantile(df.map$log_coal_access1, probs = seq(0, 1, 1/7), na.rm = TRUE) 

qq.l.r <- qq.l %>% 
  round(digits = 4) %>% 
  as.character() %>% 
  str_pad(width = 6, side = c("right"), pad = "0")

df.map <- df.map %>% 
  mutate(
    q.IV = cut(log_coal_access1, breaks = qq.l, 
               include.lowest = TRUE, labels = FALSE),
    q.lab = case_when(
      q.IV == 1 ~ paste0(qq.l.r[1], " - ", qq.l.r[2]),
      q.IV == 2 ~ paste0(qq.l.r[2], " - ", qq.l.r[3]),
      q.IV == 3 ~ paste0(qq.l.r[3], " - ", qq.l.r[4]),
      q.IV == 4 ~ paste0(qq.l.r[4], " - ", qq.l.r[5]),
      q.IV == 5 ~ paste0(qq.l.r[5], " - ", qq.l.r[6]),
      q.IV == 6 ~ paste0(qq.l.r[6], " - ", qq.l.r[7]),
      q.IV == 7 ~ paste0(qq.l.r[7], " - ", qq.l.r[8]),
      .default = NA
    )
  )


### Join data with shapefile
shp.LLM <- left_join(shp.LLM,df.map, by = "labor_market_id")




### IV plot
(m.IV <- ggplot() + 
    # IV for Local Labor Markets
    geom_sf(data = shp.LLM[!is.na(shp.LLM$q.IV),], aes(fill = as.factor(q.IV)), color = NA) +
    geom_sf(data = shp.LLM[!is.na(shp.LLM$q.IV) & shp.LLM$q.IV < 7,], aes(), fill = NA, color = "grey5") +
    geom_sf(data = shp.LLM[!is.na(shp.LLM$q.IV) & shp.LLM$q.IV == 7,], aes(), fill = NA, color = "grey30") +
    # Coastline
    geom_sf(data = shp.coast, aes(color = "CL"), 
            fill = NA,
            size = 2,
            show.legend = "line",
            inherit.aes = FALSE) +
    # Coal Fields
    geom_sf(data = shp.coal, aes(fill = "MCF"), 
            color = NA, 
            alpha = 0.75,
            show.legend = TRUE,
            inherit.aes = FALSE) + 
    # Rivers
    geom_sf(data = shp.rivers, aes(color = "MR"), 
            fill = NA, 
            show.legend = "line",
            inherit.aes = FALSE) +
    # Spatial extend
    coord_sf(xlim = c(-5,15), ylim = c(47.5,55)) +
    # Fill definition
    scale_fill_manual(values = c("grey97",
                                 "grey85",
                                 "grey75",
                                 "grey60",
                                 "grey45",
                                 "grey30",
                                 "grey0",
                                 "brown"),
                      label = c(paste0(qq.l.r[1], " - ", qq.l.r[2]),
                              paste0(qq.l.r[2], " - ", qq.l.r[3]),
                              paste0(qq.l.r[3], " - ", qq.l.r[4]),
                              paste0(qq.l.r[4], " - ", qq.l.r[5]),
                              paste0(qq.l.r[5], " - ", qq.l.r[6]),
                              paste0(qq.l.r[6], " - ", qq.l.r[7]),
                              paste0(qq.l.r[7], " - ", qq.l.r[8]),
                              "Major coal fields"
                      ),
                      name = "Transp.-cost weighted coal access",
                      guide = guide_legend(override.aes = list(linetype = "blank", shape = NA))) +
    scale_color_manual(values = c("CL" = "blue4",
                                  "MR" = "blue"),
                       label = c("Coast line",
                                 "Major rivers"),
                       name = NULL, 
                       guide = guide_legend(override.aes = list(linetype = c("solid", "solid"), fill = NA, shape = NA))) +
    # Theme
    theme_void() + 
    theme(legend.position = c(0.13,0.25), 
          legend.background = element_rect(fill = "white", color = NA),
          legend.title = element_text(face = "bold"))
)


### Save and export the map
ggsave("../results/figures/fig4.jpeg", m.IV, dpi = 600, limitsize = TRUE,
       width = 10.0, height = 6.0, units = c("in"))

