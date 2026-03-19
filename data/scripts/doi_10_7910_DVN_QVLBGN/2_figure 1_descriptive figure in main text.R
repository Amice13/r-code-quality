### ****************************************************************************
### CLIMATE CHANGE, ARIDITY, MIGRATION 
### ****************************************************************************

###
### DESCRIPTIVE FIGURES FOR MIGRATION OUTCOMES ---------------------------------
### 

rm(list = ls())

##
## PACKAGES --------------------------------------------------------------------
## 

library(tidyverse)
library(fixest)
library(ggpubr)
library(RColorBrewer)
library(countrycode)
library(sf)
library(rgdal)
library(colorspace)

citation("tidyverse")
citation("fixest")
citation("ggpubr")
citation("RColorBrewer")
citation("countrycode")
citation("sf")
citation("rgdal")
citation("colorspace")

##
## LOAD DATA -------------------------------------------------------------------
## 

load(file="full migration data.RData")



##
## FIGURE - MAP: DISTRIBUTION OF INTERNAL MIGRATION BASED ON LAST CENSUS -------
## 

#> loading world map
map.world <- map_data('world') %>% 
  rename("Country"="region")  

#> loading IPUMS subnational shapefile
geo1_shp <- readOGR("world_geolev1_2021.shp")
class(geo1_shp)

#> translating from st to sf object
geo1_shp_sf <- st_as_sf(geo1_shp)
class(geo1_shp_sf)

#> recoding problematic country names
geo1_shp_sample <- geo1_shp_sf %>%
  mutate(CNTRY_NAME = recode(CNTRY_NAME,
                             "United States" = "USA", 
                             "United Kingdom" = "UK", 
                             "Kyrghzstan" ="Kyrgyzstan",  
                             "Lao People's Democratic Republic" = "Laos" , 
                             "Trinidad and Tobago" = "Trinidad"))  

#> checking countries included 
table(geo1_shp_sample$CNTRY_NAME) 

#> saving geospatial identifier as numeric variable 
geo1_shp_sample <- geo1_shp_sample %>% 
  mutate(GEOLEVEL1 = as.numeric(GEOLEVEL1))

#> Focus on last census wave 
d.aggregated <- d.aggregated %>% 
  group_by(country_name, alpha3, worldregion) %>% 
  mutate(year_max = max(year)) %>% 
  filter(year == year_max)

#> joining shapefile and data on standardized migration
geo1_shp_migr <- inner_join(geo1_shp_sample, d.aggregated, by=c("GEOLEVEL1"="orig"))

#> checking countries included 
table(geo1_shp_migr$CNTRY_NAME)

mycolors <- colorRampPalette(brewer.pal(9, "PRGn"))(14)

#> producing map with standardized migration patterns
g1 <- ggplot() +
  geom_polygon(data=map.world, 
               aes( x = long, y = lat, group = group), 
               fill="#f5f5f2")+
  geom_sf(data=geo1_shp_migr, 
          aes(fill=flow_out_rate_annual_stan_cat), 
              size=NA,
              color="#8d8f94",
              linewidth=0.02)+
  geom_polygon(data=map.world, 
               aes( x = long, y = lat, group = group), 
               color="#2b2b2b",
               linewidth = 0.3,
               fill=NA)+
  theme_light()+
  coord_sf(ylim = c(-55,80),
           xlim = c(-152,165))+
  scale_fill_manual(name="Standardized deviation in out-migration from country mean", values = mycolors)+
#  scale_fill_gradient2(midpoint = 0,  limits=c(-2, 2), low="#5dd97a", high="#46b3d4", mid="white",
#                       breaks=c(-2, -1.5,-1,-0.5,0,0.5,1,1.5, 2),
#                       name="Standardized migration")+
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
          legend.position="bottom")+
  ggtitle("")
  
##
## FIGURE - BOX PLOT: MIGRATION BY AGE AND SEX ------------------------
## 

#> combining main data file and auxiliary files with information non migration by 
#> age, sex and education

d <- d %>% left_join(d.sexage) %>% left_join(d.sexedu)

aux2 <- d %>% 
  select(alpha3, year, orig,     
         "female015"="flow_out_rate_annual_female015"  ,      
         "female1520"="flow_out_rate_annual_female1520" ,      
         "female2025" ="flow_out_rate_annual_female2025"  ,     
         "female2530" = "flow_out_rate_annual_female2530"  ,     
         "female3045" ="flow_out_rate_annual_female3045"  ,     
         "female4560" ="flow_out_rate_annual_female4560"   ,    
         "female60" = "flow_out_rate_annual_female60"    ,     
         "femalelessprimary" ="flow_out_rate_annual_femalelessprimary",
         "femaleprimary" ="flow_out_rate_annual_femaleprimary"  ,  
         "femalesecondary" ="flow_out_rate_annual_femalesecondary" , 
         "femaletertiary" ="flow_out_rate_annual_femaletertiary",   
         "male015" ="flow_out_rate_annual_male015"  ,        
         "male1520"= "flow_out_rate_annual_male1520"  ,       
         "male2025" ="flow_out_rate_annual_male2025"  ,       
         "male2530"= "flow_out_rate_annual_male2530"  ,       
         "male3045"=  "flow_out_rate_annual_male3045"  ,       
         "male4560" ="flow_out_rate_annual_male4560"  ,       
         "male60"="flow_out_rate_annual_male60"    ,       
         "malelessprimary"="flow_out_rate_annual_malelessprimary" , 
         "maleprimary"="flow_out_rate_annual_maleprimary"  ,    
         "malesecondary"="flow_out_rate_annual_malesecondary" ,   
         "maletertiary"="flow_out_rate_annual_maletertiary")

aux3 <- aux2 %>% 
  gather("female015":"maletertiary", key="group", "value"="migration") %>% 
  group_by(alpha3, orig) %>% 
  mutate(year_max = max(year)) %>% 
  filter(year == year_max)

aux3 <- aux3 %>% 
  mutate(education = 0, 
         education = ifelse(group %in% 
                              c("maletertiary", "malesecondary",
                                "maleprimary", "malelessprimary",
                                "femaletertiary", "femalesecondary",
                                "femaleprimary", "femalelessprimary"),
                            1, 0),
         sex = ifelse(group %in%
                              c("female015", "female1520", "female2025", 
                                "female2530", "female3045", "female4560",
                                "female60", "femaletertiary", "femalesecondary",
                                "femaleprimary", "femalelessprimary"),
                         "women", "men"))
                        
table(aux3$sex, useNA = "always")
table(aux3$education, useNA = "always")

g2 <- aux3 %>% 
  filter(education == 0) %>% 
  mutate(group = recode(group, 
                        "female015"="<15"  ,      
                         "female1520"="15-20" ,      
                         "female2025" ="21-25"  ,     
                         "female2530" = "26-30"  ,     
                                 "female3045" ="31-45"  ,     
                                 "female4560" ="46-60"   ,    
                                 "female60" = ">60"    ,     
                                 "male015" ="<15"  ,        
                                 "male1520"= "15-20"  ,       
                                 "male2025" ="21-25"  ,       
                                 "male2530"= "26-30"  ,       
                                 "male3045"=  "31-45"  ,       
                                 "male4560" ="46-60"  ,       
                                 "male60"=">60"),
         order = recode(group,
                        "<15" = 1 ,      
                        "15-20" = 2,      
                        "21-25" = 3 ,     
                        "26-30" = 4 ,     
                        "31-45" = 5 ,     
                        "46-60" = 6  ,    
                        ">60" = 7 ),
         order=as.numeric(order)) %>% 
  ggplot()+
  geom_boxplot(mapping=aes(y=migration, 
                           x=fct_reorder(group,order),
                           fill=sex),
               outlier.shape = NA)+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#daede3", "#1aa156"))+
  coord_cartesian(ylim = c(0,0.004))+
  xlab("Age group")+ylab("Out-migration rate (bilateral)")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        legend.background = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10))


##
## FIGURE - FACETED BAR CHART: MIGRATION BY EDUCATION AND SEX ------------------
## 


g3 <- aux3 %>% 
  filter(education == 1) %>% 
  mutate(group = recode(group, 
                        "maletertiary" = "tertiary",
                        "malesecondary"= "secondary",
                        "maleprimary"= "primary",
                        "malelessprimary" = "< primary",
                        "femaletertiary"= "tertiary", 
                        "femalesecondary"= "secondary",
                        "femaleprimary"= "primary", 
                        "femalelessprimary"= "< primary"),
         order = recode(group,
                        "< primary" = 1 ,      
                        "primary" = 2,      
                        "secondary" = 3 ,     
                        "tertiary" = 4 ),
         order=as.numeric(order)) %>% 
  ggplot()+
  geom_boxplot(mapping=aes(y=migration, 
                           x=fct_reorder(group,order),
                           fill=sex),
               outlier.shape = NA)+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_manual(values = c("#daede3", "#1aa156"))+
  coord_cartesian(ylim = c(0,0.004))+
  xlab("Education")+ylab("Out-migration rate (bilateral)")+
  ggtitle("")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size=12),
        axis.text = element_text(size=10))

g3

#> combining panels in one figure
g123 <- ggarrange(g1,
                  labels=c("A"),
                  ggarrange(g2, g2, ncol=2,  labels=c("B", "C"),
                            align=c("hv"),
                            common.legend = TRUE, 
                            legend="bottom",
                            font.label=list(size=18)),
                  nrow=2,
                  heights=c(1.4,1),
                  widths=c(1,1),
                  font.label=list(size=18))

#> save final figure
ggsave(g123, 
       filename="figure 1.png", 
       width=12, height = 10)

ggsave(g123, 
       filename="figure 1.pdf", 
       width=12, height = 10)
 
