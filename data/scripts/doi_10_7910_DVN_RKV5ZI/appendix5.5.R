####Predict probability of communal violence during period####
main_dyad_prediction <- subset(main_dyad, int_grp_rel_dominant == 1)# & subset_analysis == 1
main_dyad_prediction$pred_main_m2d_t1_sb <- predict(main_m2d_t1_sb, newdata = main_dyad_prediction, type ="response")
main_dyad_prediction_empty <- main_dyad_prediction
main_dyad_prediction_empty$sa_territory_t <- 0
main_dyad_prediction$pred_main_m2d_t1_sb_empty <- predict(main_m2d_t1_sb, newdata = main_dyad_prediction_empty, type ="response")
main_dyad_prediction <- main_dyad_prediction[c("cowcode","group1","group2","year","fips","ADMIN_NAME", "pred_main_m2d_t1_sb","pred_main_m2d_t1_sb_empty","cv_event","tot_events","tot_best_fat", "sa_territory_t", "included_excluded","includedd_excludednd","includednd_excludedd", "int_grp_rel1", "int_grp_rel2")]
main_dyad_prediction <- main_dyad_prediction[complete.cases(main_dyad_prediction),]
main_dyad_prediction <- unique(main_dyad_prediction)
main_dyad_prediction_lag <- main_dyad_prediction[c("cowcode","group1","group2","year","fips","sa_territory_t","included_excluded")]
colnames(main_dyad_prediction_lag)[6:ncol(main_dyad_prediction_lag)] <- paste(colnames(main_dyad_prediction_lag)[6:ncol(main_dyad_prediction_lag)], "l1", sep = "_")
main_dyad_prediction_lag$year <- main_dyad_prediction_lag$year + 1
main_dyad_prediction <- left_join(main_dyad_prediction, main_dyad_prediction_lag, by=c("cowcode","group1","group2","year","fips"))
main_dyad_prediction$included_excluded_l1 <- ifelse(is.na(main_dyad_prediction$included_excluded_l1), -999, main_dyad_prediction$included_excluded_l1)
main_dyad_prediction$sa_territory_t_l1 <- ifelse(is.na(main_dyad_prediction$sa_territory_t_l1), -999, main_dyad_prediction$sa_territory_t_l1)
main_dyad_prediction$dfg_period <- ifelse(main_dyad_prediction$included_excluded != main_dyad_prediction$included_excluded_l1 | abs(main_dyad_prediction$sa_territory_t - main_dyad_prediction$sa_territory_t_l1) > 0.1, 1, 0)
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips) %>% arrange(cowcode,group1,group2,fips,year) %>% mutate(dfg_period = cumsum(dfg_period) + 1)
main_dyad_prediction$one <- 1
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(dfg_period_pred_main_m2d_t1_sb = 1-prod((1-pred_main_m2d_t1_sb)))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(dfg_period_pred_main_m2d_t1_sb_empty = 1-prod((1-pred_main_m2d_t1_sb_empty)))
main_dyad_prediction$dfg_period_pred_main_m2d_t1_sb_diff <- main_dyad_prediction$dfg_period_pred_main_m2d_t1_sb - main_dyad_prediction$dfg_period_pred_main_m2d_t1_sb_empty
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(from = min(year))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(to = max(year))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(dfg_period_tot_events = sum(tot_events))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(dfg_period_tot_best_fat = sum(tot_best_fat))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(includedd_excludednd = max(includedd_excludednd))
main_dyad_prediction <- main_dyad_prediction %>% group_by(cowcode,group1,group2,fips,dfg_period) %>% mutate(includednd_excludedd = max(includednd_excludedd))
main_dyad_prediction <- unique(main_dyad_prediction[c("cowcode","fips","ADMIN_NAME","group1","group2","from","to","dfg_period_tot_events","dfg_period_tot_best_fat","dfg_period_pred_main_m2d_t1_sb","dfg_period_pred_main_m2d_t1_sb_empty","dfg_period_pred_main_m2d_t1_sb_diff","included_excluded","includedd_excludednd","includednd_excludedd", "sa_territory_t")])
main_dyad_prediction$any_event <- ifelse(main_dyad_prediction$dfg_period_tot_events > 0, 1, 0)

####Define labels for selected cases####
main_dyad_prediction$ADMIN_NAME <- ifelse(main_dyad_prediction$ADMIN_NAME == "Jungoli", "Jonglei", main_dyad_prediction$ADMIN_NAME)#alternative transliteration of the same name; Jonglei is more common
main_dyad_prediction$label <- 0
main_dyad_prediction$label <- ifelse(main_dyad_prediction$ADMIN_NAME == "Sind" & main_dyad_prediction$group1 =="Mohajirs" & main_dyad_prediction$group2 =="Sindhi" & main_dyad_prediction$from == 1988,1,0)
main_dyad_prediction$label <- ifelse(main_dyad_prediction$ADMIN_NAME == "Jonglei" & main_dyad_prediction$group1 =="Dinka" & main_dyad_prediction$group2 =="Nuer" & main_dyad_prediction$from == 2005,1,main_dyad_prediction$label)
main_dyad_prediction$label <- ifelse(main_dyad_prediction$ADMIN_NAME == "Ondo" & main_dyad_prediction$group1 =="Ijaw" & main_dyad_prediction$group2 =="Yoruba" & main_dyad_prediction$from == 1999,1,main_dyad_prediction$label)

####Figure A26####
figurea26 <- ggplot(main_dyad_prediction) + geom_point(alpha = 0.4, aes(x = sa_territory_t, y = dfg_period_pred_main_m2d_t1_sb_diff, size = dfg_period_tot_best_fat, colour = factor(any_event), shape = factor(included_excluded))) +
  geom_abline(intercept = 0, slope = 0,linetype="dotted") +
  geom_label_repel(min.segment.length=0.001, nudge_y = 0.1, data = subset(main_dyad_prediction, label==1), fill = alpha(c("white"),0.5), aes(x = sa_territory_t, y = dfg_period_pred_main_m2d_t1_sb_diff, family="Times", label = as.character(paste(group1,"-",group2, "\n(",ADMIN_NAME,", ",from,"-",to,")", sep=""))),box.padding   = 0.35, point.padding = 0.5,size = 3,segment.color = 'grey50') +
  theme_bw() + xlab("territorial autonomy") + ylab("cumulative effect of territorial\nautonomy on communal violence") +
  theme(legend.position = "bottom", text=element_text(family="Times")) +
  scale_size(breaks = c(0,2000,4000,6000), limits=c(0,10254)) +
  guides(size = guide_legend(title = "#of casualties",nrow=2), colour = guide_legend(title = "actual communal\nviolence",nrow=2), shape = guide_legend(title = "included-excluded",nrow=2)) + scale_colour_manual(values = c("black", "red"), labels=c("no","yes"))
ggsave(figurea26, file='../figures/figurea26.pdf', width = 17, height = 12, units="cm",dpi=500)