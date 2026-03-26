### ----------------------------------------------------------------------------
### CLIMATE CHANGE, ARIDITY, AND INTERNAL MIGRATION 
### ----------------------------------------------------------------------------


### 
### EXTENDED MODELS: MAPPING PROJECTED IMPACTS OF CHANGES IN AI ---------------- 
###  

rm(list=ls())

##
## PACKAGES --------------------------------------------------------------------
## 

library(tidyverse)
library(countrycode)
library(fixest)
library(ggpubr)
library(sf)
library(rgdal)
library(colorspace)
library(RColorBrewer)

citation("tidyverse")
citation("countrycode")
citation("fixest")
citation("ggpubr")
citation("sf")
citation("rgdal")
citation("colorspace")
citation("RColorBrewer")

##
## LOAD DATA -------------------------------------------------------------------
## 

load(file="full migration data.RData")
load(file="projected changes in aridity index.RData")

#> creating raw dataset to collect estimates 
margins <- project %>% group_by(worldregion, aridity_cat) %>% count() %>%
  select(worldregion, aridity_cat) %>% filter(!is.na(worldregion)&!is.na(aridity_cat))

table(margins$worldregion)

##
## EXTENDED - MARGINAL EFFECTS BY WORLD REGION AND ARIDITY  --------------------
## 

m1 <-  fepois(flow_out_rate_annual ~ 
                 orig_ai_mean10:worldregion+
                 orig_ai_mean10:aridity_cat +
                dest_ai_mean10_stan1+
                worldregion*as.factor(year_cat10)+
                log(dist) + contig+
                as.factor(mig_interval),
               fixef = c("orig_dest_dyad"),
               data=d) 
summary(m1, se = "cluster", cluster=c(d$orig))

m2 <-  fepois(flow_out_rate_annual ~ 
                orig_pdsi_mean10:worldregion+
                orig_pdsi_mean10:aridity_cat +
                dest_pdsi_mean10+
                worldregion*as.factor(year_cat10)+
                log(dist) + contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m2, se = "cluster")


m3 <-  fepois(flow_out_rate_annual ~ 
                orig_spei03_mean10:worldregion+
                orig_spei03_mean10:aridity_cat +
                dest_spei03_mean10+
                worldregion*as.factor(year_cat10)+
                log(dist) + contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m3, se = "cluster")


m4 <-  fepois(flow_out_rate_annual ~ 
                orig_spei12_mean10:worldregion+
                orig_spei12_mean10:aridity_cat +
                dest_spei12_mean10+
                worldregion*as.factor(year_cat10)+
                log(dist) + contig+
                as.factor(mig_interval),
              fixef = c("orig_dest_dyad"),
              data=d) 
summary(m4, se = "cluster")


models <- etable(m1, m2,m3,m4,
                 se="cluster",
                 signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
                 coefstat = "se")
models


write.csv(models, "table_extended models_heterogeneity by regions and aridity2.csv")


##
## DERIVING PREDICTIONS FOR CHANGES IN ARIDITY INDEX ---------------------------
## 

#> focus on estimates for aridity index
m.sum1 <- summary(m1, se = "cluster")
m.coef1 <- as.data.frame(m.sum1$coeftable)
m.coef1 <- rownames_to_column(m.coef1, "name")
m.coef1 <- m.coef1[grep("orig_ai_mean10", m.coef1$name), ]
m.coef1$name <- gsub("orig_ai_mean10", "", m.coef1$name)

table(margins$aridity_cat)

# obtain predicted marginal effects for different world regions and baseline aridity levels
margins <- margins %>% 
  mutate(predict = case_when(worldregion == "Africa & Middle East" & aridity_cat == "Humid" ~  m.coef1[1,2] , 
                             worldregion == "Africa & Middle East" & aridity_cat == "Hyper-arid/arid" ~ m.coef1[1,2] + m.coef1[9,2],
                             worldregion == "Africa & Middle East" & aridity_cat == "Semi-arid" ~ m.coef1[1,2] + m.coef1[10,2],
                             worldregion == "Africa & Middle East" & aridity_cat == "Sub-humid" ~ m.coef1[1,2] ,
                             
                             worldregion == "Central America & Caribbean" & aridity_cat == "Humid"  ~ m.coef1[2,2] ,
                             worldregion == "Central America & Caribbean" & aridity_cat == "Hyper-arid/arid" ~ m.coef1[2,2] + m.coef1[9,2] ,
                             worldregion == "Central America & Caribbean" & aridity_cat == "Semi-arid" ~ m.coef1[2,2] + m.coef1[10,2],
                             worldregion == "Central America & Caribbean" & aridity_cat == "Sub-humid"  ~ m.coef1[2,2] ,
                             
                             worldregion == "East Asia & Pacific" & aridity_cat == "Humid"  ~  m.coef1[3,2],
                             worldregion == "East Asia & Pacific" & aridity_cat == "Hyper-arid/arid" ~  m.coef1[3,2] + m.coef1[9,2],
                             worldregion == "East Asia & Pacific" & aridity_cat == "Semi-arid" ~  m.coef1[3,2] + m.coef1[10,2],
                             worldregion == "East Asia & Pacific" & aridity_cat == "Sub-humid"  ~  m.coef1[3,2],
                             
                             worldregion == "North America" & aridity_cat == "Humid"  ~  0,
                             worldregion == "North America" & aridity_cat == "Hyper-arid/arid" ~  0  + m.coef1[9,2],
                             worldregion == "North America" & aridity_cat == "Semi-arid" ~  0 + m.coef1[10,2],
                             worldregion == "North America" & aridity_cat == "Sub-humid"  ~  0,
                             
                             worldregion == "Northeastern Europe & Central Asia" & aridity_cat == "Humid"  ~  m.coef1[5,2],
                             worldregion == "Northeastern Europe & Central Asia" & aridity_cat == "Hyper-arid/arid" ~  m.coef1[5,2]  + m.coef1[9,2],
                             worldregion == "Northeastern Europe & Central Asia" & aridity_cat == "Semi-arid" ~  m.coef1[5,2] + m.coef1[10,2],
                             worldregion == "Northeastern Europe & Central Asia" & aridity_cat == "Sub-humid"  ~  m.coef1[5,2],
                             
                             worldregion == "South America" & aridity_cat == "Humid" ~ m.coef1[6,2],
                             worldregion == "South America" & aridity_cat == "Hyper-arid/arid" ~  m.coef1[6,2]  + m.coef1[9,2],
                             worldregion == "South America" & aridity_cat == "Semi-arid" ~ m.coef1[6,2] + m.coef1[10,2],
                             worldregion == "South America" & aridity_cat == "Sub-humid" ~ m.coef1[6,2],
                             
                             worldregion == "South Asia" & aridity_cat == "Humid"  ~  m.coef1[7,2],
                             worldregion == "South Asia" & aridity_cat == "Hyper-arid/arid" ~  m.coef1[7,2]  + m.coef1[9,2],
                             worldregion == "South Asia" & aridity_cat == "Semi-arid" ~ m.coef1[7,2] + m.coef1[10,2] ,
                             worldregion == "South Asia" & aridity_cat == "Sub-humid"  ~  m.coef1[7,2],
                             
                             worldregion == "Southern Europe" & aridity_cat == "Humid"  ~ m.coef1[8,2],
                             worldregion == "Southern Europe" & aridity_cat == "Hyper-arid/arid" ~  m.coef1[8,2]  + m.coef1[9,2],
                             worldregion == "Southern Europe" & aridity_cat == "Semi-arid" ~m.coef1[8,2]  + m.coef1[10,2],
                             worldregion == "Southern Europe" & aridity_cat == "Sub-humid"  ~ m.coef1[8,2]))
  

##
## COMBINING PREDICTED MARGINAL EFFECTS AND PROJECTIONS ------------------------
## 

project <- project %>% 
  left_join(margins)

check <- project %>% filter(is.na(predict))

#> calculating projected changes in migration under 2° and 4°C scenarios

summary(project$ai4_changes)

project <- project %>% 
  mutate( ai4_changes = ai4_changes*-1, # rescaling original aridity index variable
          ai4_changes = ifelse(ai4_changes > -0.01 & ai4_changes < 0.01, 0, ai4_changes),
          project4c = ai4_changes*predict)

summary(project$project4c)
summary(project$predict)
summary(project$ai4_changes)

#> categorizing projected changes in migration under 4 and 2 degree warming scenario
project <- project %>% 
  mutate(project4c_cat = cut(project4c, breaks=c(-Inf,-0.01,0.01,0.025, 0.05, 0.1,0.15,0.2,0.25,0.5, Inf)), #-0.2,-0.15,-0.1,-0.05, -0.01

         ai4_change_cat = cut(ai4_changes, breaks=c(-Inf,-0.01, 0.01, 0.05, 0.1,0.15, 0.2,0.25,0.3,0.35,0.4,0.45,0.5,Inf)))


table(project$project4c_cat, useNA = "always")
table(project$project2c_cat, useNA = "always")

table(project$ai4_change_cat, useNA = "always")

class(project$ai4_change_cat)
class(project$project4c_cat)

levels(project$project4c_cat)

##
## PREPARING GEOSPATIAL DATA FOR MAPPING ---------------------------------------
## 

#> Downloading world map
map.world <- map_data('world') %>% 
  rename("Country"="region")  

#> Loading IPUMS regions shapefile
load("shapefile all countries.RData")

geo1_shp <- shapefile

class(geo1_shp)

#> translating st to sf object
geo1_shp_sf <- st_as_sf(geo1_shp)
class(geo1_shp_sf)

#> checking countries included
table(geo1_shp_sf$CNTRY_NAME)  

geo1_shp_sf <- geo1_shp_sf %>% 
  mutate(geolevel1 = ifelse(is.na(geolevel1),
                            gadm_code,
                            geolevel1))

#> making geo identifier numeric
#geo1_shp_sf <- geo1_shp_sf %>% 
#  mutate(GEOLEVEL1 = as.numeric(GEOLEVEL1))

#> combining projection and geospatial data

# project <- project %>% mutate(geolevel1 = as.numeric(geolevel1)) 
geo1_shp_project <- inner_join(geo1_shp_sf, project, by=c("country"="cntry_name", "geolevel1"="geolevel1"))

#> checking countries included
table(geo1_shp_project$CNTRY_NAME)

#> defining color space
mycolors <-
  c("#f6cbf7", # #ffe6ff
    "white",
     brewer.pal(8, "Greens")
    )


##
## PRODUCING MAPS WITH PREDICTED CHANGES IN MIGRATION -------------------------- 
## 

# 1 PREDICTED CHANGES IN MIGRATION UNDER 4 DEGREE WARMING

geo1_shp_project <- geo1_shp_project %>% filter(country!="Fiji")

g1 <- ggplot() +
  geom_sf(data=geo1_shp_project, 
          aes(fill=project4c_cat), 
          size=NA,
          color="#9da0a6",
          linewidth=0.025,
          alpha=0.85)+
  geom_polygon(data=map.world, 
               aes( x = long, y = lat, group = group), 
               color="#2b2b2b",
               linewidth = 0.05,
               fill=NA)+
  theme_light()+
  coord_sf(ylim = c(-55,80),
           xlim = c(-160,170))+
 scale_fill_manual(name="Modeled out-migration changes under 4°C global warming scenario",values = mycolors,  
                   na.value = "grey",
                   na.translate = F,
                   labels=c("<-1%", "(-1%,1%]", "(1%,2.5%]","(2.5%,5%]","(5%,10%]",
                            "(10%,15%]","(15%,20%]","(20%,25%]","(25%,50%]",">50%"))+
 # scale_fill_gradientn(colours = c("#800080","white","#5dd97a"), 
 #                      values= scales::rescale(-1,0,1),
 #                       limits=c(-6,6))+
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5,
                             nrow=1, 
                             keywidth = 3,
                             label.position = "bottom",
                             label.theme = element_text(size=11),
                             title.theme = element_text(size=13)))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="bottom")
#g1

ggsave(g1, file="figure_map_predicted migration changes under 4C scenario.png", width=10, height = 5)
save(g1, file="figure_predicted migration under 4 degree scenarios.RData")


# 2 PROJECTED CHANGES IN AI UNDER 4 DEGREE OF WARMING 

table(geo1_shp_project$ai4_change_cat)
mycolors <-
  c("#f6cbf7", # #ffe6ff
    "white",
    colorRampPalette(brewer.pal(9, "Greens"))(11)
  )

g2 <- ggplot() +
  geom_polygon(data=map.world, 
               aes( x = long, y = lat, group = group), 
               fill="#f5f5f2")+
  geom_sf(data=geo1_shp_project, 
          aes(fill=ai4_change_cat), 
          size=NA,
          color="#9da0a6",
          linewidth=0.025,
          alpha=0.85)+
  geom_polygon(data=map.world, 
               aes( x = long, y = lat, group = group), 
               color="#2b2b2b", 
               linewidth = 0.05,
               fill=NA)+
  theme_light()+
  coord_sf(ylim = c(-55,80),
           xlim = c(-152,165))+
  scale_fill_manual(name="Projected changes in aridity index until the end of the century under a 4°C (RCP 8.5) scenario", values = mycolors)+
  #  scale_fill_gradient2(midpoint = 0,  limits=c(-2, 2), low="#5dd97a", high="#46b3d4", mid="white",
  #                       breaks=c(-2, -1.5,-1,-0.5,0,0.5,1,1.5, 2),
  #                       name="Standardized migration")+
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5,
                             nrow=1, 
                             keywidth = 3,
                             label.position = "bottom",
                             label.theme = element_text(size=10),
                             title.theme = element_text(size=13)))+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position="bottom")

ggsave(g2, file="figure_supplement_projected changes in aridity index under 4 degree.png", width=10, height = 5)

 
