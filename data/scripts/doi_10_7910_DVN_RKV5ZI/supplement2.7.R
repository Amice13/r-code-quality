######Import underlying data####
#all admin units in sample
sau_geo5 <- sf::st_read("./../data_set_up_script/02_components/relevant_admin_units/sau_geo5.shp",options = "ENCODING=UTF-8",promote_to_multi=T)#NEW now simplified because of geo problems
sau_geo5$ADMIN_NAME <- sau_geo5$ADMIN_N
sau_geo5$from_pol_period <- sau_geo5$frm_pl_
sau_geo5$to_pol_period <- sau_geo5$t_pl_pr
sau_geo5 <- sau_geo5[c("cowcode","ADMIN_NAME","fips","rump","from_pol_period","to_pol_period")]
sau_geo5 <- subset(sau_geo5, to_pol_period >= 1988)
units_included <- unique(sau_geo5[c("cowcode","fips","rump")])
units_included$unit_included <- 1
units_included$geometry <- NULL
units_included <- unique(units_included)
units_included <- data.frame(units_included)
#autonomy score and link to autonomy tier
unit_autonomy <- read.csv("./../original_data/territorial_autonomy/sa_admin_unit_level_all_indicators.csv")
unit_autonomy <- unit_autonomy[c("cowcode","fips","rump","year","sa_territory_t","autid_code")]
#list of tiers
sa_tiers <- read.csv("./../original_data/territorial_autonomy/sa_tiers_indicators.csv")
sa_tiers <- sa_tiers[c("cowcode","country","from","to","autid_code","tier")]
#combine
unit_autonomy2 <- subset(unit_autonomy, sa_territory_t > 0)
unit_autonomy3 <- left_join(unit_autonomy2, unique(units_included), by=c("cowcode","fips","rump"))
unit_autonomy3 <- subset(unit_autonomy3, unit_included == 1)
unit_autonomy4 <- left_join(unit_autonomy3, sa_tiers, by=c("cowcode","autid_code"), relationship = "many-to-many")
unit_autonomy4 <- subset(unit_autonomy4, year >= from & year <= to)
unit_autonomy4 <- unique(unit_autonomy4)
included_tiers <- unique(unit_autonomy4[c("autid_code","from","to")])
unit_autonomy5 <- unique(unit_autonomy4[c("cowcode","country","tier","year","sa_territory_t","from","to")])
unit_autonomy5$tier_number <- 1
unit_autonomy5 <- unit_autonomy5 %>% group_by(cowcode,country,tier,year)%>% arrange(cowcode,country,tier,year,sa_territory_t) %>% mutate(tier_number = cumsum(tier_number))
unit_autonomy5 <- unique(unit_autonomy5[c("country","tier","tier_number","from","to","sa_territory_t")])
unit_autonomy5$from <- ifelse(unit_autonomy5$from < 1988, 1988, unit_autonomy5$from)
unit_autonomy5$description <- ifelse(unit_autonomy5$from != unit_autonomy5$to, paste(unit_autonomy5$tier, " (",unit_autonomy5$from,"-",unit_autonomy5$to,")",sep=""), paste(unit_autonomy5$tier, " (",unit_autonomy5$from,")",sep=""))
unit_autonomy5$sa_territory_t_rounded <- round(unit_autonomy5$sa_territory_t, 2)
unit_autonomy5$description2 <- ifelse(unit_autonomy5$from != unit_autonomy5$to, paste(unit_autonomy5$tier, " [", unit_autonomy5$sa_territory_t_rounded, "] ", " (",unit_autonomy5$from,"-",unit_autonomy5$to,")",sep=""), paste(unit_autonomy5$tier, " [", unit_autonomy5$sa_territory_t_rounded, "] ", " (",unit_autonomy5$from,")",sep=""))
unit_autonomy5$description2 <- ifelse(is.na(unit_autonomy5$from), unit_autonomy5$description, unit_autonomy5$description2)
#basis for table S6
unit_autonomy5 <- unit_autonomy5[order(unit_autonomy5$country,unit_autonomy5$from,unit_autonomy5$tier),]
tables6 <- unit_autonomy5[c("country","description2")]#Note: some modifications were manually applied after exporting this automatically-generated table (e.g. combining temporally adjacent tiers with same autonomy scores)
write.csv(tables6, file="../tables/tables6.csv")
