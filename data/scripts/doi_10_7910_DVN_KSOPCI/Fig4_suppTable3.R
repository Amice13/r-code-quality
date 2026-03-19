
setwd("")

#libraries
require(pacman)
pacman::p_load(tidyverse, rgdal, sf, rmapshaper, ggforce, cowplot, patchwork, rnaturalearth,
               ggtext, showtext, rnaturalearthdata, ggmap, ggalluvial, ggrepel, viridis)

#read file with merged transmission links for all HRV types
merged<- read.csv("data/phylogeography_links.csv")

# Figure 4 #####
## Figure 4A #####

# ggplot
sankey<- ggplot(data = merged,
                aes(axis1 = LOCATION1, axis2 = LOCATION2)) +
  geom_alluvium(width = 1/20, aes(fill=LOCATION1), alpha=0.65) +
  geom_stratum(width = 1/10, fill='white', color='black') +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)),
             color = "black",size = 3, label.size = 0.1, nudge_x = -0.1)+
  scale_x_discrete(limits = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank()) +
  labs(y="Number of Events") 


select_df <- merged %>%
  dplyr::select(LOCATION1, LOCATION2)

matrix_all <-xtabs(~LOCATION1+LOCATION2, select_df)
matrix_all[4,4] <- 0 # remove local transmission pathways

## Figure 4B #####

font_add_google("Pacifico", "pacifico")
font_add_google("Source Sans Pro", "source")
showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)


## shp files
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="mapShapefiles.zip")
#system("unzip world_shape_file.zip")

globe_shp <- st_read(dsn="./data/mapShapefiles/", layer="TM_WORLD_BORDERS_SIMPL-0.3")
globe <- st_transform(globe_shp, crs=4326) 
#globe <- ms_simplify(globe) # Simplify as otherwise is massive

selectRegions <- globe %>% 
  dplyr::filter(NAME == "Kenya")

no_borders<- globe %>% 
  dplyr::filter(! NAME == "Kenya")

globe$REGION <- as.character(globe$REGION)

# Create a data frame for segments.
lat_lon <- data.frame(REGION = c("Kenya", "Africa", "Asia", "Europe", "North America", "South America", "Oceania"), 
                      lat = c(0.0236, 10, 50, 49.5, 45, -14, -20), 
                      lon = c(37.9062, 12.1, 100.1, 14.1, -95, -60, 128.98))

tmp_df <- as.data.frame(matrix_all) %>%
  dplyr::filter(Freq>0)


df2 <- merge(tmp_df, lat_lon, by.x='LOCATION1', by.y='REGION', all.x=T) %>%
  merge(., lat_lon, by.x='LOCATION2', by.y='REGION', all.x=T) %>%
  dplyr::rename("lat_from" = "lat.x",
                "lat_to"="lat.y",
                "lon_from" = "lon.x",
                "lon_to" = "lon.y",
                "Frequency" = "Freq")

# use average BF value
head(merged)

global_links <- merged %>%
  dplyr::filter(! LOCATION1 == LOCATION2) %>% 
  dplyr::select(-c(HRV_type)) %>% 
  group_by(LOCATION1, LOCATION2)  %>% 
  summarise_at(vars(BAYES_FACTOR),      
               list(mean_BF = mean))

df3 <- merge(global_links, lat_lon, by.x='LOCATION1', by.y='REGION', all.x=T) %>%
  merge(., lat_lon, by.x='LOCATION2', by.y='REGION', all.x=T) %>%
  dplyr::rename("lat_from" = "lat.x",
                "lat_to"="lat.y",
                "lon_from" = "lon.x",
                "lon_to" = "lon.y",
                "Average_BF" = "mean_BF")


# plot map
legendBrks = c(100, 250, 400, 550, 700, 850)

(map <- ggplot() +
    #geom_sf(data = globe, fill = NA) +
    #geom_sf(data = globe, aes(fill = REGION)) +
    geom_sf(data = no_borders, color = "#FAFAFA", alpha= 1) +
    geom_sf(data=selectRegions, fill = "coral1")+
    xlab("Longitude") +
    ylab("Latitude") + 
    #xlim(-135, 135)+
    ylim(-55, 80) +
    geom_point(data = lat_lon, aes(x = lon, y = lat), color = "hotpink2", alpha = 0.5, size = 15)+
    geom_text_repel(data = lat_lon, 
                    aes(x = lon, y = lat, label = REGION,  family = "serif"))+
    geom_curve(data = df3,
               aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to,  
                   color = Average_BF),
               linewidth = 0.8, alpha = 0.8, lineend = "round",
               curvature = -0.3, show.legend = TRUE) +
    scale_color_viridis(breaks = legendBrks) +
    #scale_color_gradientn(colours = c("lightblue","darkblue", "orange"))+
    theme_bw() +
    theme(plot.background = element_blank(),
          panel.background = element_rect(fill = "lightblue1", colour = "powderblue"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank())
)



# Figure 4
fig4 <- sankey/map #export to pdf for better rendering



# Supplementary Table 3 #####
pacman::p_load(plyr, data.table, flextable)

all_merged <- read.csv("./data/supp_table3_data.csv")

# Rename all local to Kenya
cont_df1 <- all_merged %>%
  dplyr::rename("Continent"= "continent")%>%
  mutate_at("Continent", recode,
            "country"= "Kenya",
            "hh" = "Kenya",
            "hdss" = "Kenya",
            "school" = "Kenya",
            "Other_Kenya" = "Kenya",
            "Germany" = "Europe") %>%
  ddply(~Continent+HRV_type,summarise,
        Total=length(HRV_type))

# order table
preferred_order <- c("Kenya", "Africa", "Asia", "Europe",
                     "N.America", "S.America", "Oceania")

my_table <- reshape2::dcast(cont_df1, Continent ~ HRV_type, value.var = "Total")

# find sum across rows
my_table <- my_table %>%
  mutate(Total = dplyr::select(.,A12:C35) %>% 
           rowSums(na.rm=TRUE))

my_table <- my_table[match(preferred_order, my_table$Continent),] 
my_table[is.na(my_table)] <- 0

supp_table_3<-flextable(my_table) %>% vline(part = "all")  %>% hline(part = "all") 

#save_as_html(my_table2, path = "../../../../figures/cleaned_up/Supplementary_Table.html")

