desc_analysis_communal <- subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)
desc_analysis_communal$one <- 1
##a) all
desc_analysis_communal_all <- desc_analysis_communal %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_communal_all <- desc_analysis_communal_all %>% group_by(one) %>% mutate(number_even = sum(one * (1-included_excluded),na.rm=T))
desc_analysis_communal_all <- desc_analysis_communal_all %>% group_by(one) %>% mutate(number_uneven = sum(one * included_excluded,na.rm=T))
desc_analysis_communal_all <- desc_analysis_communal_all %>% group_by(one) %>% mutate(number_newincidence = sum(one * cv_event,na.rm=T))
desc_analysis_communal_all <- desc_analysis_communal_all %>% group_by(one) %>% mutate(number_newincidence_even = sum(one * cv_event * (1-included_excluded),na.rm=T))
desc_analysis_communal_all <- desc_analysis_communal_all %>% group_by(one) %>% mutate(number_newincidence_uneven = sum(one * cv_event * included_excluded,na.rm=T))
desc_analysis_communal_all$type <- "all"
desc_analysis_communal_all <- unique(desc_analysis_communal_all[c("type","number","number_even","number_uneven","number_newincidence","number_newincidence_even","number_newincidence_uneven")])
##b) autonomy = 0
desc_analysis_communal_nonaut <- subset(desc_analysis_communal, sa_territory_t == 0)
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number_even = sum(one * (1-included_excluded),na.rm=T))
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number_uneven = sum(one * included_excluded,na.rm=T))
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number_newincidence = sum(one * cv_event,na.rm=T))
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number_newincidence_even = sum(one * cv_event * (1-included_excluded),na.rm=T))
desc_analysis_communal_nonaut <- desc_analysis_communal_nonaut %>% group_by(one) %>% mutate(number_newincidence_uneven = sum(one * cv_event * included_excluded,na.rm=T))
desc_analysis_communal_nonaut$type <- "territorial autonomy = 0"
desc_analysis_communal_nonaut <- unique(desc_analysis_communal_nonaut[c("type","number","number_even","number_uneven","number_newincidence","number_newincidence_even","number_newincidence_uneven")])
##c) autonomy > 0
desc_analysis_communal_aut <- subset(desc_analysis_communal, sa_territory_t > 0)
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number_even = sum(one * (1-included_excluded),na.rm=T))
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number_uneven = sum(one * included_excluded,na.rm=T))
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number_newincidence = sum(one * cv_event,na.rm=T))
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number_newincidence_even = sum(one * cv_event * (1-included_excluded),na.rm=T))
desc_analysis_communal_aut <- desc_analysis_communal_aut %>% group_by(one) %>% mutate(number_newincidence_uneven = sum(one * cv_event * included_excluded,na.rm=T))
desc_analysis_communal_aut$type <- "territorial autonomy > 0"
desc_analysis_communal_aut <- unique(desc_analysis_communal_aut[c("type","number","number_even","number_uneven","number_newincidence","number_newincidence_even","number_newincidence_uneven")])
##d) combined
tablea4 <- rbind(desc_analysis_communal_all, desc_analysis_communal_nonaut, desc_analysis_communal_aut)
tablea4$group_units <- paste(tablea4$type, " (", tablea4$number, ")", sep="")
tablea4$newincidence_all <- paste(tablea4$number_newincidence, " (", round(tablea4$number_newincidence/tablea4$number, 5), ")", sep="")
tablea4$newincidence_even <- paste(tablea4$number_newincidence_even, " (", round(tablea4$number_newincidence_even/tablea4$number_even, 5), ")", sep="")
tablea4$newincidence_uneven <- paste(tablea4$number_newincidence_uneven, " (", round(tablea4$number_newincidence_uneven/tablea4$number_uneven, 5), ")", sep="")
tablea4 <- tablea4[c("group_units","newincidence_all","newincidence_even","newincidence_uneven")]
write.csv(tablea4, file = "../tables/tablea4.csv")