#############################################################################################################
### Script to create Figure 3: Per capita income rank of West German labor markets, 1926-2019 (quartiles) ###
#############################################################################################################

# check for packages needed and compare to list of installed packages, install if necessary
list.of.packages <- c("haven", "dplyr", "sf", "ggplot2", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(haven)
library(dplyr)
library(sf)
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


### Prepare data
df.map <- df.main[df.main$year==1926,c("labor_market_id","rank_perc")] %>% 
  left_join(df.main[df.main$year==1957,c("labor_market_id","rank_perc")], by = "labor_market_id") %>% 
  left_join(df.main[df.main$year==2019,c("labor_market_id","rank_perc")], by = "labor_market_id")

names(df.map) <- c("labor_market_id","rank_perc1926","rank_perc1957","rank_perc2019")

### Calculate quantiles
df.map <- df.map %>% 
  mutate(
    q.26 = cut(rank_perc1926, breaks = quantile(rank_perc1926, probs = seq(0, 1, 0.25), na.rm = TRUE), 
               include.lowest = TRUE, labels = FALSE),
    q.57 = cut(rank_perc1957, breaks = quantile(rank_perc1957, probs = seq(0, 1, 0.25), na.rm = TRUE), 
               include.lowest = TRUE, labels = FALSE),
    q.19 = cut(rank_perc2019, breaks = quantile(rank_perc2019, probs = seq(0, 1, 0.25), na.rm = TRUE), 
               include.lowest = TRUE, labels = FALSE)
  )


### Join data with shapefile
shp.LLM <- left_join(shp.LLM,df.map, by = "labor_market_id")




### 1926
(m.26 <- ggplot(shp.LLM[!is.na(shp.LLM$rank_perc1926),], aes(fill = as.factor(q.26))) +
    scale_fill_manual(values = c("white",
                                 "lightblue",
                                 "steelblue",
                                 "navyblue"),
                      lab = c("Bottom Quartile", 
                              "Second Quartile",
                              "Third Quartile", 
                              "Top Quartile"),
                      name = "") +
    geom_sf() +
    theme_void() + 
    theme(plot.margin = margin(-50,0,-60,0))
    )


### 1957
(m.57 <- ggplot(shp.LLM[!is.na(shp.LLM$rank_perc1957),], aes(fill = as.factor(q.57))) +
    scale_fill_manual(values = c("white",
                                 "lightblue",
                                 "steelblue",
                                 "navyblue"),
                      lab = c("Bottom Quartile", 
                              "Second Quartile",
                              "Third Quartile", 
                              "Top Quartile"),
                      name = "") +
    geom_sf() +
    theme_void() + 
    theme(plot.margin = margin(-50,0,-60,0))
)


### 2019
(m.19 <- ggplot(shp.LLM[!is.na(shp.LLM$rank_perc2019),], aes(fill = as.factor(q.19))) +
    scale_fill_manual(values = c("white",
                                 "lightblue",
                                 "steelblue",
                                 "navyblue"),
                      lab = c("Bottom Quartile", 
                              "Second Quartile",
                              "Third Quartile", 
                              "Top Quartile"),
                      name = "") +
    geom_sf() +
    theme_void() + 
    theme(plot.margin = margin(-50,0,-60,0))
)



### Combine plots
(m.final <- ggarrange(
  m.26, m.57, m.19, 
  labels = c("(a) 1926", "(b) 1957", "(c) 2019"),
  font.label = list(size = 10, color = "black", face = "plain", family = NULL),
  hjust = -1,
  ncol = 3,
  common.legend = TRUE, 
  legend = "bottom"
)
)


### Save and export the map
ggsave("../results/figures/fig3.eps", m.final, dpi = 300, limitsize = TRUE,
       width = 5.48, height = 3.2, units = c("in"))

