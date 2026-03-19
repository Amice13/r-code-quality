desc_analysis_civil <- subset(main_group, subset_analysis == 1)
desc_analysis_civil$one <- 1
##a) all
desc_analysis_civil_all <- desc_analysis_civil %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_civil_all <- desc_analysis_civil_all %>% group_by(one) %>% mutate(number_maj = sum(one * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_all <- desc_analysis_civil_all %>% group_by(one) %>% mutate(number_min = sum(one * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_all <- desc_analysis_civil_all %>% group_by(one) %>% mutate(number_newincidence = sum(one * cw_event_g,na.rm=T))
desc_analysis_civil_all <- desc_analysis_civil_all %>% group_by(one) %>% mutate(number_newincidence_maj = sum(one * cw_event_g * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_all <- desc_analysis_civil_all %>% group_by(one) %>% mutate(number_newincidence_min = sum(one * cw_event_g * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_all$type <- "all"
desc_analysis_civil_all <- unique(desc_analysis_civil_all[c("type","number","number_maj","number_min","number_newincidence","number_newincidence_maj","number_newincidence_min")])
##b) autonomy < 0
desc_analysis_civil_nonaut <- subset(desc_analysis_civil, sa_territory_t == 0)
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number_maj = sum(one * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number_min = sum(one * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number_newincidence = sum(one * cw_event_g,na.rm=T))
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number_newincidence_maj = sum(one * cw_event_g * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_nonaut <- desc_analysis_civil_nonaut %>% group_by(one) %>% mutate(number_newincidence_min = sum(one * cw_event_g * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_nonaut$type <- "territorial autonomy = 0"
desc_analysis_civil_nonaut <- unique(desc_analysis_civil_nonaut[c("type","number","number_maj","number_min","number_newincidence","number_newincidence_maj","number_newincidence_min")])
##c) autonomy >= 0
desc_analysis_civil_aut <- subset(desc_analysis_civil, sa_territory_t > 0)
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number = sum(one,na.rm=T))
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number_maj = sum(one * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number_min = sum(one * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number_newincidence = sum(one * cw_event_g,na.rm=T))
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number_newincidence_maj = sum(one * cw_event_g * int_grp_rel_dominant_g,na.rm=T))
desc_analysis_civil_aut <- desc_analysis_civil_aut %>% group_by(one) %>% mutate(number_newincidence_min = sum(one * cw_event_g * (1-int_grp_rel_dominant_g),na.rm=T))
desc_analysis_civil_aut$type <- "territorial autonomy > 0"
desc_analysis_civil_aut <- unique(desc_analysis_civil_aut[c("type","number","number_maj","number_min","number_newincidence","number_newincidence_maj","number_newincidence_min")])
##d) combined
tablea3 <- rbind(desc_analysis_civil_all, desc_analysis_civil_nonaut, desc_analysis_civil_aut)
tablea3$group_units <- paste(tablea3$type, " (", tablea3$number, ")", sep="")
tablea3$newincidence_all <- paste(tablea3$number_newincidence, " (", round(tablea3$number_newincidence/tablea3$number, 5), ")", sep="")
tablea3$newincidence_maj <- paste(tablea3$number_newincidence_maj, " (", round(tablea3$number_newincidence_maj/tablea3$number_maj, 5), ")", sep="")
tablea3$newincidence_min <- paste(tablea3$number_newincidence_min, " (", round(tablea3$number_newincidence_min/tablea3$number_min, 5), ")", sep="")
tablea3 <- tablea3[c("group_units","newincidence_all","newincidence_maj","newincidence_min")]
write.csv(tablea3, file = "../tables/tablea3.csv")