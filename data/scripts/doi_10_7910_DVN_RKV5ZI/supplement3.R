####figure S17####
communal_conflicts_all <- read.csv("./../data_set_up_script/03_dataframes_variables/a_group_unit_year/group_unit_year_basis_full.csv")
cv_time_description <- subset(communal_conflicts_all, other == 0 & nopluralitysum >= 0.05)
cv_time_description <- cv_time_description %>% group_by(cowcode,year) %>% mutate(cv_event_g = max(cv_event_g,na.rm=T))
cv_time_description <- unique(cv_time_description[c("cowcode","year","cv_event_g")])
cv_time_description <- cv_time_description %>% group_by(year) %>% mutate(cv_event_g = mean(cv_event_g,na.rm=T))
cv_time_description <- unique(cv_time_description[c("year","cv_event_g")])
figures17 <- ggplot(cv_time_description, aes(x = year, y = cv_event_g)) + geom_line() +
  xlab("year") + ylab("fraction of countries\nexperiencing communal violence") + theme_bw() + theme(text=element_text(family="Times"), legend.position="bottom", legend.title=element_text(size=10), axis.title=element_text(size=10)) + scale_x_continuous(breaks = c(1988, 1993, 1998, 2003, 2008, 2013, 2018))
ggsave(figures17, file='../figures/figures17.pdf', width = 17, height = 6, units="cm")

####figure S18####
sf::sf_use_s2(FALSE)
##import underlying data
#world
world <- ne_countries(scale = "medium", returnclass = "sf")
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
ged_geo2$type <- "communal"
#country borders
cshapes_augmented <- sf::st_read("./../external_data/CShapes_augmented/cshapes_augmented.shp")
cshapes_augmented <- subset(cshapes_augmented)
cshapes_augmented <- st_simplify(cshapes_augmented, dTolerance =0.2)
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018 & cowcode == 530)
##figure S18
cv_geo_all <- subset(ged_geo2[c("type","year")])
cshapes_augmented_2018 <- subset(cshapes_augmented, GWSYEAR <= 2018 & GWEYEAR >= 2018)
boundaries_2018 <- st_bbox(cshapes_augmented_2018)
figures18 <- ggplot() + 
  geom_sf(data = world, fill ='white') + 
  geom_sf(data = cv_geo_all,size=1, colour = "orange", alpha = 0.5) +
  theme_bw() + theme(title = element_text(size = 35), legend.title = element_text(size = 25), legend.text = element_text(size = 20), text=element_text(family='Times'), legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
  coord_sf(xlim = c(min(boundaries_2018$xmin),max(boundaries_2018$xmax)), ylim=c(min(boundaries_2018$ymin),max(boundaries_2018$ymax))) + scale_x_continuous(breaks = NULL, labels =NULL, guide = NULL)
ggsave(figures18, file='../figures/figures18.pdf', width = 17, height = 10)

####table S7####
#(communal violence involving groups outside of core area)
#note: in the data, this corresponds to groups whose relative share of an administrative unit's/grid cell's population (int_grp_rel / rel_grp_gid) is 0 (see appendix 3.7.2).
communal_conflicts_all <- read.csv("./../data_set_up_script/03_dataframes_variables/a_group_unit_year/group_unit_year_basis_full.csv")
communal_conflicts_all <- subset(communal_conflicts_all, other == 0 & nopluralitysum >= 0.05)#select sample: no non-EPR ("other") groups and only multiethnic countries (at least 5% minority population)
communal_conflicts_all <- subset(communal_conflicts_all, cv_event_g == 1 & int_grp_rel == 0)#all communal violence events (cv_event_g == 1) of groups outside of core area/with regional population share 0 (int_grp_rel == 0)
communal_conflicts_all <- unique(communal_conflicts_all[c("cowcode","gwgroupid","group","fips","year")])
adm_name <- read.csv("./../original_data/territorial_autonomy/sa_admin_unit_level_all_indicators.csv")
adm_name <- unique(adm_name[c("fips","ADMIN_NAME","year")])
adm_name <- subset(adm_name, !is.na(ADMIN_NAME))
tables7 <- left_join(communal_conflicts_all, adm_name, by=c("fips","year"))
write.csv(tables7, file='../tables/tables7.csv')