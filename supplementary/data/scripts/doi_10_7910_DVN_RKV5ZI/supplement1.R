####Figure S1####
sau_geo_full5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo_full5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)
boundaries_2018_china <- subset(sau_geo_full5, cowcode == 710 & t_pl_pr == 2018)
boundaries_2018_china$subordinate[grepl("CH_M", boundaries_2018_china$fips)] <- 1
boundaries_2018_china$subordinate <- ifelse(is.na(boundaries_2018_china$subordinate),0,boundaries_2018_china$subordinate)
boundaries_2018_china_box <- st_bbox(boundaries_2018_china)
figures1 <- ggplot() + 
  geom_sf(data = boundaries_2018_china, aes(fill = factor(subordinate)), colour = 'black', size = 0.25) + 
  geom_sf(data = subset(cshapes_augmented_2018,cowcode ==710), fill=NA, colour = 'black', size =1)+# + ggtitle('2018') + 
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018_china_box$xmin),max(boundaries_2018_china_box$xmax)), ylim=c(min(boundaries_2018_china_box$ymin),max(boundaries_2018_china_box$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_manual(values = c("white","lightgrey"), name='subordinate')
ggsave(figures1, file='../figures/figures1.pdf', width = 17)

####Table S1####
#import underlying data
sau_geo_full5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo_full5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)#all units in sample
sau1.0 <- sf::st_read("./../original_data/administrative_boundaries/sau1.0_simplified_clean/sau1.0_simplified_clean_specialregion.shp",options = "ENCODING=UTF-8",promote_to_multi=T)
sau1.0 <- subset(sau1.0, type == "Subordinate unit")#all subordinate SAU units
countries <- read.csv("./../external_data/epr_augmented/epr_augmented_gyear.csv")
countries <- unique(countries[c("cowcode","year","nopluralitysum")])
countries <- subset(countries, year == 2018)
countries$year <- NULL
#join
sau1.0$geometry <- NULL
sau1.0 <- data.frame(sau1.0)
tables1 <- left_join(sau_geo_full5, unique(sau1.0[c("CNTRY_NAME","fips","type","starty","endy")]))
tables1 <- left_join(tables1, unique(countries[c("cowcode","nopluralitysum")]))
tables1 <- subset(tables1, type == "Subordinate unit" & nopluralitysum >= 0.05)
tables1 <- unique(tables1[c("cowcode","CNTRY_NAME","ADMIN_N","starty","endy")])
tables1$geometry <- NULL
tables1 <- data.frame(tables1)
write.csv(tables1, "../tables/tables1.csv")

####Figure S2####
##import data
sf::sf_use_s2(FALSE)
#administrative boundaries
sau_geo_full5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo_full5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)
#country borders
cshapes_augmented <- sf::st_read("./../external_data/CShapes_augmented/cshapes_augmented.shp")
cshapes_augmented <- st_simplify(cshapes_augmented, dTolerance =0.2)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018)
##Preparing data
#subsets
boundaries_nigeria <- subset(sau_geo_full5, cowcode == 475)
boundaries_nigeria1996 <- subset(boundaries_nigeria, t_pl_pr >= 1996 & frm_pl_ <= 1996)
boundaries_nigeria1991 <- subset(boundaries_nigeria, t_pl_pr >= 1991 & frm_pl_ <= 1991)
boundaries_nigeria1987 <- subset(boundaries_nigeria, t_pl_pr >= 1987 & frm_pl_ <= 1987)
#highlight states
boundaries_nigeria1996$highlight <- ifelse(boundaries_nigeria1996$ADMIN_N == "Bayelsa" | boundaries_nigeria1996$ADMIN_N == "Rivers", "red", "white")
boundaries_nigeria1991$highlight <- ifelse(boundaries_nigeria1991$ADMIN_N == "Bayelsa" | boundaries_nigeria1991$ADMIN_N == "Rivers", "red", "white")
boundaries_nigeria1991$highlight <- ifelse(boundaries_nigeria1991$ADMIN_N == "Delta" | boundaries_nigeria1991$ADMIN_N == "Edo" | boundaries_nigeria1991$ADMIN_N == "Bendel", "blue", boundaries_nigeria1991$highlight)
boundaries_nigeria1987$highlight <- ifelse(boundaries_nigeria1987$ADMIN_N == "Delta" | boundaries_nigeria1987$ADMIN_N == "Edo" | boundaries_nigeria1987$ADMIN_N == "Bendel", "blue", "white")
boundaries_nigeria_box <- st_bbox(boundaries_nigeria1996)
##Figure S2
map_1996_nigeria <- ggplot() + 
  geom_sf(data = boundaries_nigeria1996, colour = 'black', fill = boundaries_nigeria1996$highlight, size = 0.25) + 
  geom_sf(data = subset(cshapes_augmented_2018,cowcode ==475), fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 12), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_nigeria_box$xmin),max(boundaries_nigeria_box$xmax)), ylim=c(min(boundaries_nigeria_box$ymin),max(boundaries_nigeria_box$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL)
map_1991_nigeria <- ggplot() + 
  geom_sf(data = boundaries_nigeria1991, colour = 'black', fill = boundaries_nigeria1991$highlight, size = 0.25) + 
  geom_sf(data = subset(cshapes_augmented_2018,cowcode ==475), fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 12), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_nigeria_box$xmin),max(boundaries_nigeria_box$xmax)), ylim=c(min(boundaries_nigeria_box$ymin),max(boundaries_nigeria_box$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL)
map_1987_nigeria <- ggplot() +
  geom_sf(data = boundaries_nigeria1987, colour = 'black', fill = boundaries_nigeria1987$highlight, size = 0.25) +
  geom_sf(data = subset(cshapes_augmented_2018,cowcode ==475), fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 12), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_nigeria_box$xmin),max(boundaries_nigeria_box$xmax)), ylim=c(min(boundaries_nigeria_box$ymin),max(boundaries_nigeria_box$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL)
figures2 <- grid.arrange(map_1996_nigeria + ggtitle("1996-2018"), map_1991_nigeria + ggtitle("1991-1996"), map_1987_nigeria + ggtitle("1987-1991"), nrow = 1, ncol =3)
ggsave(figures2, file='../figures/figures2.pdf', width = 17,dpi=600)

####Figure S3####
#figure S3 directly created in QGis during data coding process

####Figure S4####
##importing data
sf::sf_use_s2(FALSE)
#administrative boundaries
sau_geo_full5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo_full5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)
sau_geo_full5_2018 <- subset(sau_geo_full5, t_pl_pr == 2018)
#country borders
cshapes_augmented <- sf::st_read("./../external_data/CShapes_augmented/cshapes_augmented.shp")
cshapes_augmented <- st_simplify(cshapes_augmented, dTolerance =0.001)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018)
#settlement patterns
geoepr <- sf::st_read("./../external_data/geoEPR_augmented/geoepr_augmented_simplified_clean.shp")
geoepr2018 <- subset(geoepr,from <= 2018 & to >= 2018)
geoepr2018 <- st_simplify(geoepr2018, dTolerance = 0.001)
#population density
popdens <- raster("./../external_data/hyde_population_density/2017AD_pop/popd_2017AD.asc")
popdens15_475 <- crop(popdens, as(subset(sau_geo_full5_2018,cowcode==475), 'Spatial'))
popdens15_475_df <- as(popdens15_475, "SpatialPixelsDataFrame")
popdens15_475_df <- as.data.frame(popdens15_475_df)
##Figure S4
figures4 <- ggplot() + 
  geom_tile(data=popdens15_475_df, aes(x=x, y=y, fill=popd_2017AD), alpha=0.8, show.legend = TRUE) +
  scale_fill_gradientn(colors = c("lightgrey",   "grey30"), name = "Population density") +
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Hausa-Fulani and Muslim Middle Belt"), aes(colour=factor(group)),fill="#7fc97f", alpha=0.5, size =0.000001) + 
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Igbo"), aes(colour=factor(group)),fill="#beaed4", alpha=0.5, size =0.000001) + 
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Ijaw"), aes(colour=factor(group)),fill="#fdc086", alpha=0.5, size =0.000001) + 
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Ogoni"), aes(colour=factor(group)),fill="#ffff99", alpha=0.5, size =0.000001) + 
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Tiv"), aes(colour=factor(group)),fill="#386cb0", alpha=0.5, size =0.000001) + 
  geom_sf(data = subset(geoepr2018,cowcode==475 & group == "Yoruba"), aes(colour=factor(group)),fill="#f0027f", alpha=0.5, size =0.000001) + 
  scale_colour_manual("Group", values=c("black","black","black","black","black","black"), guide=guide_legend(override.aes = list(fill=c("#7fc97f", "#beaed4","#fdc086","#ffff99","#386cb0","#f0027f"))))+
  geom_sf(data = subset(sau_geo_full5_2018,cowcode==475), fill = NA, colour = 'black', size = 0.5) + 
  theme_bw() + theme(text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL)
ggsave(figures4, file='../figures/figures4.pdf', width = 8, height = 10,dpi=600)

####Table S2####
cv_group6_full <- read.csv("./../data_set_up_script/03_dataframes_variables/a_group_unit_year/group_unit_year_basis_full.csv")
admin_names <- read.csv("./../original_data/territorial_autonomy/sa_admin_unit_level_all_indicators.csv")
admin_names <- admin_names[c("cowcode","fips","rump","year","ADMIN_NAME")]
cv_group6_full <- left_join(cv_group6_full, admin_names, by=c("cowcode","fips","rump","year"))
cv_group6_full <- cv_group6_full %>% group_by(cowcode,fips,rump,gwgroupid) %>% mutate(int_grp_rel_largest_g_ever = max(int_grp_rel_largest_g))
tables2 <- subset(cv_group6_full, ethnic_t_g2 == 1 & int_grp_rel_largest_g_ever == 0)
tables2 <- unique(tables2[c("cowcode","ADMIN_NAME","group")])
tables2 <- tables2[order(tables2$cowcode, tables2$ADMIN_NAME),]
write.csv(tables2, file="../tables/tables2.csv")
