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
#communal violence
ged <- read.csv("./../external_data/ucdp/ucdp_ged/ged201.csv")
ged <- subset(ged,type_of_violence == 2)
ged_geo <- st_as_sf(ged, coords = c("longitude","latitude"))
ged_geo$year <- ged_geo$year -1
st_crs(ged_geo) <- "+proj=longlat +datum=WGS84 +no_defs"
#ethnic attribution communal violence
ged_attributed <- read.csv("./../data_set_up_script/02_components/group_geo_violence_link/ged_attributed.csv")
ged_attributed2 <- unique(ged_attributed[c("gwgroupid1","gwgroupid2","conflict_new_id","year")])
ged_attributed2$conflict_new_id <- as.integer(paste(ged_attributed2$conflict_new_id))
ged_geo2 <- left_join(ged_geo, ged_attributed2, by=c("conflict_new_id","year"),relationship = "many-to-many")
ged_geo2$other1 <- ifelse(grepl("X", ged_geo2$gwgroupid1) == TRUE, 1, 0)
ged_geo2$other2 <- ifelse(grepl("X", ged_geo2$gwgroupid2) == TRUE, 1, 0)
ged_geo2 <- subset(ged_geo2, other1 == 0 & other2 == 0 & !is.na(gwgroupid1) &!is.na(gwgroupid2))
ged_geo2[c("gwgroupid1","gwgroupid2","other1","other2")] <- NULL
ged_geo2 <- unique(ged_geo2)
ged_geo2$cowcode <- ged_geo2$country_id
ged_geo2$region <- NULL
ged_geo2 <- left_join(ged_geo2, countries, by="cowcode")
ged_geo2 <- subset(ged_geo2, nopluralitysum >= 0.05)
ged_geo2$type <- "communal"
#civil violence
civil_war_geo <- read.csv("./../external_data/ucdp/ucdp_ged/ged201.csv")
civil_war_geo <- subset(civil_war_geo,type_of_violence ==1)#only state
civil_war_geo <- st_as_sf(civil_war_geo, coords = c("longitude","latitude"))
civil_war_geo$year <- civil_war_geo$year -1
civil_war_geo$cowcode <- civil_war_geo$country_id
civil_war_geo$region <- NULL
st_crs(civil_war_geo) <- "+proj=longlat +datum=WGS84 +no_defs"
#ethnic attribution civil violence
acd2epr2 <- read.csv("./../external_data/EPR_augmented/ACD2EPR-2021.csv")
acd2epr2$dyad_new_id <- as.character(paste(acd2epr2$dyadid))
acd2epr2 <- subset(acd2epr2, !is.na(gwgroupid) & (claim == 1 | claim == 2) & (recruitment == 1 | recruitment == 2))
acd2epr2$dyad_new_id <- as.integer(acd2epr2$dyad_new_id)
civil_war_geo2 <- left_join(civil_war_geo, acd2epr2[c("dyad_new_id","gwgroupid")], by=c("dyad_new_id"),relationship = "many-to-many")
civil_war_geo2$gwgroupid <- NULL
civil_war_geo2 <- unique(civil_war_geo2)
civil_war_geo2$cowcode <- civil_war_geo2$country_id
civil_war_geo2$region <- NULL
civil_war_geo2 <- left_join(civil_war_geo2, countries, by="cowcode")
civil_war_geo2 <- subset(civil_war_geo2, nopluralitysum >= 0.05)
civil_war_geo2$type <- "civil"
cv_cw_geo <- rbind(ged_geo2[c("type","year")],civil_war_geo2[c("type","year")])
#country borders
cshapes_augmented <- sf::st_read("./../external_data/CShapes_augmented/cshapes_augmented.shp")
cshapes_augmented <- subset(cshapes_augmented)
cshapes_augmented <- left_join(cshapes_augmented, countries, by="cowcode")
cshapes_augmented <- st_simplify(cshapes_augmented, dTolerance =0.2)
#autonomy
territory_tiers_sa <- read.csv("./../original_data/territorial_autonomy/sa_admin_unit_level_all_indicators.csv")
territory_tiers_sa <- unique(territory_tiers_sa[c("cowcode","year","fips","rump","sa_territory_t")])

####Figure 3 (2019)####
#analogously to data structure in article: administrative boundaries at December 31 of t (2018) and violence any day in t+1 (2019)
sau_geo_full5_2018 <- subset(sau_geo_full5_2, frm_pl_ <= 2018 & t_pl_pr >= 2018)
sau_geo_full5_2018 <- left_join(sau_geo_full5_2018, subset(territory_tiers_sa, year == 2018), by=c("cowcode","fips","rump"))
sau_geo_full5_2018$sa_territory_t <- ifelse(is.na(sau_geo_full5_2018$sa_territory_t),0,sau_geo_full5_2018$sa_territory_t)
cv_cw_geo_2018 <- subset(cv_cw_geo, year >= 2018 & year <= 2018)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018)
boundaries_2018 <- st_bbox(sau_geo_full5_2018)
#part 1: full size map
map_2018 <- ggplot() + 
  geom_sf(data = world, fill ='darkgrey', aes(linetype = "Ethnically homogeneous")) + 
  geom_sf(data = sau_geo_full5_2018, aes(fill = sa_territory_t), colour = 'black', size = 0.125) + 
  geom_sf(data = cshapes_augmented_2018, fill=NA, colour = 'black', size =0.5)+
  geom_sf(data = cv_cw_geo_2018,size=0.1, aes(colour = factor(type)), alpha = 0.5) +
  geom_rect(aes(xmin = -20, xmax = 107, ymin = -38, ymax = 45), linetype=2, color = "black", fill = NA) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), title = element_text(size = 12), legend.title = element_text(size = 10), legend.text = element_text(size = 8), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018$xmin),max(boundaries_2018$xmax)), ylim=c(min(boundaries_2018$ymin),max(boundaries_2018$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_colour_manual(values=c("red","orange"), name="violence") +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence")) +
  scale_linetype_manual(values = c("Ethnically homogeneous" = 0)
                        , name = ""
                        , guide = guide_legend(override.aes = list(fill = c("darkgrey"))))
#part 2: inset map
map_2018_inset <- ggplot() + 
  geom_sf(data = world, fill ='darkgrey', aes(linetype = "Ethnically homogeneous")) + 
  geom_sf(data = sau_geo_full5_2018, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_2018, fill=NA, colour = 'black', size =1)+
  geom_sf(data = cv_cw_geo_2018,size=0.2, aes(colour = factor(type)), alpha = 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), title = element_text(size = 12), legend.title = element_text(size = 10), legend.text = element_text(size = 8), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(-16,105), ylim=c(-32,40)) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_colour_manual(values=c("red","orange"), name="violence") +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence")) +
  scale_linetype_manual(values = c("Ethnically homogeneous" = 0), name = "", guide = guide_legend(override.aes = list(fill = c("darkgrey"))))
#combined
mylegend <- g_legend(map_2018)
figure3 <- grid.arrange(arrangeGrob(map_2018 + theme(legend.position="none"), map_2018_inset + theme(legend.position="none"), nrow=2, heights = c(0.4,0.6)), mylegend, nrow=2,heights=c(9,1))
ggsave(figure3, file='../figures/figure3.pdf', width = 17, height = 20, units="cm",dpi=600)


####Figure S7 (1989)####
#analogously to data structure in article: administrative boundaries at December 31 of t (1988) and violence any day in t+1 (1989)
sau_geo_full5_1988 <- subset(sau_geo5_2, frm_pl_ <= 1988 & t_pl_pr >= 1988)
sau_geo_full5_1988 <- left_join(sau_geo_full5_1988, subset(territory_tiers_sa, year == 1988), by=c("cowcode","fips","rump"))
sau_geo_full5_1988$sa_territory_t <- ifelse(is.na(sau_geo_full5_1988$sa_territory_t),0,sau_geo_full5_1988$sa_territory_t)
cv_cw_geo_1988 <- subset(cv_cw_geo, year >= 1988 & year <= 1988)
cshapes_augmented_1988 <- subset(cshapes_augmented, GWSYEAR <= 1988 & GWEYEAR >= 1988)
boundaries_1988 <- st_bbox(sau_geo_full5_1988)
#part 1: full size map
map_1988 <- ggplot() + 
  geom_sf(data = world, fill ='darkgrey', aes(linetype = "Ethnically homogeneous")) + 
  geom_sf(data = sau_geo_full5_1988, aes(fill = sa_territory_t), colour = 'black', size = 0.125) + 
  geom_sf(data = cshapes_augmented_1988, fill=NA, colour = 'black', size =0.5)+
  geom_sf(data = cv_cw_geo_1988,size=0.1, aes(colour = factor(type)), alpha = 0.5) +
  geom_rect(aes(xmin = -20, xmax = 107, ymin = -38, ymax = 45), linetype=2, color = "black", fill = NA) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), title = element_text(size = 12), legend.title = element_text(size = 10), legend.text = element_text(size = 8), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_1988$xmin),max(boundaries_1988$xmax)), ylim=c(min(boundaries_1988$ymin),max(boundaries_1988$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_colour_manual(values=c("red","orange"), name="violence") +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence")) +
  scale_linetype_manual(values = c("Ethnically homogeneous" = 0)
                        , name = ""
                        , guide = guide_legend(override.aes = list(fill = c("darkgrey"))))
#part 2: inset map
map_1988_inset <- ggplot() + 
  geom_sf(data = world, fill ='darkgrey', aes(linetype = "Ethnically homogeneous")) + 
  geom_sf(data = sau_geo_full5_1988, aes(fill = sa_territory_t), colour = 'black', size = 0.25) + 
  geom_sf(data = cshapes_augmented_1988, fill=NA, colour = 'black', size =1)+ 
  geom_sf(data = cv_cw_geo_1988,size=0.2, aes(colour = factor(type)), alpha = 0.5) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), title = element_text(size = 12), legend.title = element_text(size = 10), legend.text = element_text(size = 8), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(-16,105), ylim=c(-32,40)) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL) +
  scale_colour_manual(values=c("red","orange"), name="violence") +
  scale_fill_gradient(low = 'white', high = "#2439a5", limits = c(0, 1), name='territorial autonomy', breaks =c(0,1)) +
  guides(colour = guide_legend(nrow=1,title = "violence")) + guides(colour = guide_legend(override.aes = list(size = 3), title = "violence")) +
  scale_linetype_manual(values = c("Ethnically homogeneous" = 0), name = "", guide = guide_legend(override.aes = list(fill = c("darkgrey"))))
#combined
mylegend <- g_legend(map_1988)
figures7 <- grid.arrange(arrangeGrob(map_1988 + theme(legend.position="none"), map_1988_inset + theme(legend.position="none"), nrow=2, heights = c(0.4,0.6)), mylegend, nrow=2,heights=c(9,1))
ggsave(figures7, file='../figures/figures7.pdf', width = 17, height = 20, units="cm",dpi=600)
