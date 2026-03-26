####Importing underlying data####
sf::sf_use_s2(FALSE)
#world
world <- ne_countries(scale = "medium", returnclass = "sf")
#country sample
countries <- read.csv("./../external_data/epr_augmented/epr_augmented_gyear.csv")
countries <- unique(countries[c("cowcode","year","nopluralitysum")])
countries <- subset(countries, year == 2018)
countries$year <- NULL
#admin units
sau_geo_full5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo_full5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)
sau_geo_full5 <- left_join(sau_geo_full5, countries, by="cowcode")
sau_geo_full5 <- subset(sau_geo_full5, nopluralitysum >= 0.05)
sau_geo_full5$adm_area <- round(as.numeric(st_area(sau_geo_full5)) / 1000 / 1000, digits=2)
sau_geo5_large <- subset(sau_geo_full5, adm_area > 500000)
sau_geo5_large <- st_simplify(sau_geo5_large, dTolerance =0.5)
sau_geo5_large <- st_cast(sau_geo5_large, "MULTIPOLYGON")
sau_geo5_small <- subset(sau_geo_full5, adm_area <= 500000 & adm_area > 100000)
sau_geo5_small <- st_simplify(sau_geo5_small, dTolerance =0.01)
sau_geo5_small <- st_cast(sau_geo5_small, "MULTIPOLYGON")
sau_geo5_supersmall <- subset(sau_geo_full5, adm_area <= 100000)
sau_geo5_supersmall <- st_cast(sau_geo5_supersmall, "MULTIPOLYGON")
sau_geo5_2 <- rbind(sau_geo5_supersmall, sau_geo5_small,sau_geo5_large)
sau_geo5_2 <- st_simplify(sau_geo_full5, dTolerance =0.2)
sau_geo_full5_2 <- subset(sau_geo5_2, frm_pl_ <= 2018 & t_pl_pr >= 2018)
#country borders
cshapes_augmented <- sf::st_read("./../external_data/CShapes_augmented/cshapes_augmented.shp")
cshapes_augmented <- subset(cshapes_augmented)
cshapes_augmented <- left_join(cshapes_augmented, countries, by="cowcode")
cshapes_augmented <- st_simplify(cshapes_augmented, dTolerance =0.2)
#autonomy
territory_tiers_sa <- read.csv("./../original_data/territorial_autonomy/sa_admin_unit_level_all_indicators.csv")
territory_tiers_sa <- unique(territory_tiers_sa[c("cowcode","year","fips","rump","sa_territory_t")])

####Figure S8 (Ethiopia, 2018)####
sau_et_2018 <- subset(sau_geo_full5, frm_pl_ <= 2018 & t_pl_pr >= 2018 & cowcode == 530)
sau_et_2018 <- left_join(sau_et_2018, subset(territory_tiers_sa, year == 2018 & cowcode == 530), by=c("cowcode","fips","rump"))
sau_et_2018$sa_territory_t <- ifelse(is.na(sau_et_2018$sa_territory_t),0,sau_et_2018$sa_territory_t)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018 & cowcode == 530)
boundaries_2018 <- st_bbox(sau_et_2018)
figures8 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_et_2018, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2018, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018$xmin),max(boundaries_2018$xmax)), ylim=c(min(boundaries_2018$ymin),max(boundaries_2018$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures8, file='../figures/figures8.pdf', width = 17)

####Figure S9 (Nigeria, 2018)####
sau_ni_2018 <- subset(sau_geo_full5, frm_pl_ <= 2018 & t_pl_pr >= 2018 & cowcode == 475)
sau_ni_2018 <- left_join(sau_ni_2018, subset(territory_tiers_sa, year == 2018 & cowcode == 475), by=c("cowcode","fips","rump"))
sau_ni_2018$sa_territory_t <- ifelse(is.na(sau_ni_2018$sa_territory_t),0,sau_ni_2018$sa_territory_t)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018 & cowcode == 475)
boundaries_2018 <- st_bbox(sau_ni_2018)
figures9 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_ni_2018, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2018, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018$xmin),max(boundaries_2018$xmax)), ylim=c(min(boundaries_2018$ymin),max(boundaries_2018$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures9, file='../figures/figures9.pdf', width = 17)

####Figure S10 (Indonesia, 2018)####
sau_in_2018 <- subset(sau_geo_full5, frm_pl_ <= 2018 & t_pl_pr >= 2018 & cowcode == 850)
sau_in_2018 <- left_join(sau_in_2018, subset(territory_tiers_sa, year == 2018 & cowcode == 850), by=c("cowcode","fips","rump"))
sau_in_2018$sa_territory_t <- ifelse(is.na(sau_in_2018$sa_territory_t),0,sau_in_2018$sa_territory_t)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018 & cowcode == 850)
boundaries_2018 <- st_bbox(sau_in_2018)
figures10 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_in_2018, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2018, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018$xmin),max(boundaries_2018$xmax)), ylim=c(min(boundaries_2018$ymin),max(boundaries_2018$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures10, file='../figures/figures10.pdf', width = 17)

####Figure S11 (Uganda, 2004)####
sau_ug_2004 <- subset(sau_geo_full5, frm_pl_ <= 2004 & t_pl_pr >= 2004 & cowcode == 500)
sau_ug_2004 <- left_join(sau_ug_2004, subset(territory_tiers_sa, year == 2004 & cowcode == 500), by=c("cowcode","fips","rump"))
sau_ug_2004$sa_territory_t <- ifelse(is.na(sau_ug_2004$sa_territory_t),0,sau_ug_2004$sa_territory_t)
cshapes_augmented_2004 <- subset(cshapes_augmented, GWSYEAR <= 2004 & GWEYEAR >= 2004 & cowcode == 500)
boundaries_2004 <- st_bbox(sau_ug_2004)
figures11 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_ug_2004, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2004, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2004$xmin),max(boundaries_2004$xmax)), ylim=c(min(boundaries_2004$ymin),max(boundaries_2004$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures11, file='../figures/figures11.pdf', width = 17)

####Figure S12 (Pakistan, 1998)####
sau_pk_1998 <- subset(sau_geo_full5, frm_pl_ <= 1998 & t_pl_pr >= 1998 & cowcode == 770)
sau_pk_1998 <- left_join(sau_pk_1998, subset(territory_tiers_sa, year == 1998 & cowcode == 770), by=c("cowcode","fips","rump"))
sau_pk_1998$sa_territory_t <- ifelse(is.na(sau_pk_1998$sa_territory_t),0,sau_pk_1998$sa_territory_t)
cshapes_augmented_1998 <- subset(cshapes_augmented, GWSYEAR <= 1998 & GWEYEAR >= 1998 & cowcode == 770)
boundaries_1998 <- st_bbox(sau_pk_1998)
figures12 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_pk_1998, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_1998, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_1998$xmin),max(boundaries_1998$xmax)), ylim=c(min(boundaries_1998$ymin),max(boundaries_1998$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures12, file='../figures/figures12.pdf', width = 17)

####Figure S13 (Sudan, 2010)####
sau_sd_2010 <- subset(sau_geo_full5, frm_pl_ <= 2010 & t_pl_pr >= 2010 & cowcode == 625)
sau_sd_2010 <- left_join(sau_sd_2010, subset(territory_tiers_sa, year == 2010 & cowcode == 625), by=c("cowcode","fips","rump"))
sau_sd_2010$sa_territory_t <- ifelse(is.na(sau_sd_2010$sa_territory_t),0,sau_sd_2010$sa_territory_t)
cshapes_augmented_2010 <- subset(cshapes_augmented, GWSYEAR <= 2010 & GWEYEAR >= 2010 & cowcode == 625)
boundaries_2010 <- st_bbox(sau_sd_2010)
figures13 <- ggplot() + 
  geom_sf(data = world, fill ='grey90') + 
  geom_sf(data = sau_sd_2010, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2010, fill=NA, colour = 'black', size =1)+
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2010$xmin),max(boundaries_2010$xmax)), ylim=c(min(boundaries_2010$ymin),max(boundaries_2010$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence"))
ggsave(figures13, file='../figures/figures13.pdf', width = 17)