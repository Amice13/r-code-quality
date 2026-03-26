####Model results: Controlling for the other type of violence (table X4, models A12-A15)####
r1c_m1g_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
r1c_m1g_t1_sc_cse <- data.frame(cluster.se(r1c_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
r1c_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r1c_m1g_t1_sf_cse <- data.frame(cluster.se(r1c_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
r1c_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_m1d_t1_sb_cse <- data.frame(cluster.se(r1c_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1c_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_m2d_t1_sb_cse <- data.frame(cluster.se(r1c_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r1c_m1g_t1_sc, r1c_m1g_t1_sf, r1c_m1d_t1_sb, r1c_m2d_t1_sb, se=c(r1c_m1g_t1_sc_cse, r1c_m1g_t1_sf_cse, r1c_m1d_t1_sb_cse, r1c_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A12)", "Min. (model A13)","Maj./min. dyad (model A14)","Maj./min. dyad (model A15)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X4. Additional results: controlling for the (respectively) other type of violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Recent communal violence event", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag", "Recent civil violence event"), out="../tables/additional_results_tables_appendices/tablex4.txt")
stargazer(r1c_m1g_t1_sc, r1c_m1g_t1_sf, r1c_m1d_t1_sb, r1c_m2d_t1_sb, se=c(r1c_m1g_t1_sc_cse, r1c_m1g_t1_sf_cse, r1c_m1d_t1_sb_cse, r1c_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A12)", "Min. (model A13)","Maj./min. dyad (model A14)","Maj./min. dyad (model A15)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X4. Additional results: controlling for the (respectively) other type of violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Recent communal violence event", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag", "Recent civil violence event"), out="../tables/additional_results_tables_appendices/tablex4.html")

####Model results: Communal violence, while controlling for civil violence and distinguishing between formally and informally organized communal violence (table X5, models A16-A19)####
r1c_org_m1d_t1_sb <- glm(as.formula(paste("org_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_org_m1d_t1_sb_cse <- data.frame(cluster.se(r1c_org_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1c_org_m2d_t1_sb <- glm(as.formula(paste("org_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_org_m2d_t1_sb_cse <- data.frame(cluster.se(r1c_org_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1c_unorg_m1d_t1_sb <- glm(as.formula(paste("unorg_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_unorg_m1d_t1_sb_cse <- data.frame(cluster.se(r1c_unorg_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1c_unorg_m2d_t1_sb <- glm(as.formula(paste("unorg_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_unorg_m2d_t1_sb_cse <- data.frame(cluster.se(r1c_unorg_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r1c_org_m1d_t1_sb, r1c_org_m2d_t1_sb, r1c_unorg_m1d_t1_sb, r1c_unorg_m2d_t1_sb, se=c(r1c_org_m1d_t1_sb_cse, r1c_org_m2d_t1_sb_cse, r1c_unorg_m1d_t1_sb_cse, r1c_unorg_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence (formal)","Communal violence (formal)","Communal violence (informal)","Communal violence (informal)"), model.numbers =F, column.labels = c("Maj./min. dyad (model A16)","Maj./min. dyad (model A17)","Maj./min. dyad (model A18)","Maj./min. dyad (model A19)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X5. Additional results: distinguishing between formally and informally-organized communal violence while controlling for recent civil violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence of the respective type involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included/excluded","Excluded/excluded","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Formally-org. communal violence peace years", "Formally-org. communal violence peace years 2", "Formally-org. communal violence peace years 3","Informally-org. communal violence peace years", "Informally-org. communal violence peace years 2", "Informally-org. communal violence peace years 3","Communal violence spatial lag","Recent civil violence"), out="../tables/additional_results_tables_appendices/tablex5.txt")
stargazer(r1c_org_m1d_t1_sb, r1c_org_m2d_t1_sb, r1c_unorg_m1d_t1_sb, r1c_unorg_m2d_t1_sb, se=c(r1c_org_m1d_t1_sb_cse, r1c_org_m2d_t1_sb_cse, r1c_unorg_m1d_t1_sb_cse, r1c_unorg_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence (formal)","Communal violence (formal)","Communal violence (informal)","Communal violence (informal)"), model.numbers =F, column.labels = c("Maj./min. dyad (model A16)","Maj./min. dyad (model A17)","Maj./min. dyad (model A18)","Maj./min. dyad (model A19)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X5. Additional results: distinguishing between formally and informally-organized communal violence while controlling for recent civil violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence of the respective type involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included/excluded","Excluded/excluded","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Formally-org. communal violence peace years", "Formally-org. communal violence peace years 2", "Formally-org. communal violence peace years 3","Informally-org. communal violence peace years", "Informally-org. communal violence peace years 2", "Informally-org. communal violence peace years 3","Communal violence spatial lag","Recent civil violence"), out="../tables/additional_results_tables_appendices/tablex5.html")

####Figure A4####
##a) civil violence
me_r1c_m1g_t1_sc <- margins_summary(r1c_m1g_t1_sc, variables = "cv_event_g_l0_l1_anyw", vcov = cluster.vcov(r1c_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_r1c_m1g_t1_sc$term <- "second-order maj."
me_r1c_m1g_t1_sf <- margins_summary(r1c_m1g_t1_sf, variables = "cv_event_g_l0_l1_anyw", vcov = cluster.vcov(r1c_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_r1c_m1g_t1_sf$term <- "second-order min."
me_r1c_m1g <- rbind.fill(me_r1c_m1g_t1_sc, me_r1c_m1g_t1_sf)
me_r1c_m1g$term <- as.factor(me_r1c_m1g$term)
me_r1c_m1g$term = factor(me_r1c_m1g$term,levels(me_r1c_m1g$term)[c(2,1)])
me_r1c_m1g$model <- "all"
me_r1c_m1g$estimate <- me_r1c_m1g$AME
me_r1c_m1g$conf.low <- me_r1c_m1g$lower
me_r1c_m1g$conf.high <- me_r1c_m1g$upper
me_r1c_m1g_plot <- dwplot(me_r1c_m1g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##b) communal violence (all/formally organized/informally organized)
me_r1c_m1d_t1_sb <- margins_summary(r1c_m1d_t1_sb, variables = "cw_event_either_anyw_l0_l1", vcov = cluster.vcov(r1c_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_m1d_t1_sb$term <- "all"
me_r1c_m1d_t1_sb$model <- "all"
me_r1c_m2d_t1_sb <- margins_summary(r1c_m2d_t1_sb, variables = "cw_event_either_anyw_l0_l1", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1c_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_m2d_t1_sb$term <- ifelse(me_r1c_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_r1c_m2d_t1_sb$model <- "all"
me_r1c_org_m1d_t1_sb <- margins_summary(r1c_org_m1d_t1_sb, variables = "cw_event_either_anyw_l0_l1", vcov = cluster.vcov(r1c_org_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_org_m1d_t1_sb$term <- "all"
me_r1c_org_m1d_t1_sb$model <- "formally org."
me_r1c_org_m2d_t1_sb <- margins_summary(r1c_org_m2d_t1_sb, variables = "cw_event_either_anyw_l0_l1", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1c_org_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_org_m2d_t1_sb$term <- ifelse(me_r1c_org_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_r1c_org_m2d_t1_sb$model <- "formally org."
me_r1c_unorg_m1d_t1_sb <- margins_summary(r1c_unorg_m1d_t1_sb, variables = "cw_event_either_anyw_l0_l1", vcov = cluster.vcov(r1c_unorg_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_unorg_m1d_t1_sb$term <- "all"
me_r1c_unorg_m1d_t1_sb$model <- "informally org."
me_r1c_unorg_m2d_t1_sb <- margins_summary(r1c_unorg_m2d_t1_sb, variables = "cw_event_either_anyw_l0_l1", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1c_unorg_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1c_unorg_m2d_t1_sb$term <- ifelse(me_r1c_unorg_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_r1c_unorg_m2d_t1_sb$model <- "informally org."
me_r1c_m13d <- rbind.fill(me_r1c_m1d_t1_sb, me_r1c_org_m1d_t1_sb, me_r1c_unorg_m1d_t1_sb)
me_r1c_m13d <- subset(me_r1c_m13d, term != "other")
me_r1c_m13d$term <- as.factor(me_r1c_m13d$term)
me_r1c_m13d$estimate <- me_r1c_m13d$AME
me_r1c_m13d$conf.low <- me_r1c_m13d$lower
me_r1c_m13d$conf.high <- me_r1c_m13d$upper
me_r1c_m13d_plot <- dwplot(me_r1c_m13d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)),  whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type") +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(0, 0.001, 0.002)) +
  guides(shape = guide_legend(nrow=1,"type of communal violence",reverse=TRUE), colour = guide_legend(nrow=1,"type of communal violence",reverse=TRUE),linetype = guide_legend(nrow=1,"type of communal violence",reverse=TRUE))
##c) combined
mylegend <- g_legend(me_r1c_m13d_plot)
figurea4 <- grid.arrange(arrangeGrob(me_r1c_m1g_plot + theme(legend.position="none"), me_r1c_m13d_plot + theme(legend.position="none"), nrow=1), mylegend, nrow=2,heights=c(5, 1))
ggsave(figurea4, file='../figures/figurea4.pdf', width = 14, height = 4.5, units="cm",dpi=1000)


####Figure A5####
##a) mediation communal -> civil
r1c_m1g_t1_sc_med <- glm(as.formula(paste("cv_event_g_l0_l1_anyw", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
r1c_m1g_t1_sc_cse_med <- data.frame(cluster.se(r1c_m1g_t1_sc_med, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
r1c_m1g_t1_sf_med <- glm(as.formula(paste("cv_event_g_l0_l1_anyw", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r1c_m1g_t1_sf_cse_med <- data.frame(cluster.se(r1c_m1g_t1_sf_med, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
main_dyad <- data.frame(main_dyad)
r1c_m1g_t1_sb_mediation <- mediate(r1c_m1g_t1_sc_med, r1c_m1g_t1_sc, treat = "sa_territory_t", mediator = "cv_event_g_l0_l1_anyw",sims=100, cluster=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)
r1c_m1g_t1_sf_mediation <- mediate(r1c_m1g_t1_sf_med, r1c_m1g_t1_sf, treat = "sa_territory_t", mediator = "cv_event_g_l0_l1_anyw",sims=100, cluster=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)
r1c_m1g_t1_sb_mediation_df <- data.frame(extract_mediation_summary(summary(r1c_m1g_t1_sb_mediation)))
r1c_m1g_t1_sb_mediation_df$property <- rownames(r1c_m1g_t1_sb_mediation_df)
r1c_m1g_t1_sf_mediation_df <- data.frame(extract_mediation_summary(summary(r1c_m1g_t1_sf_mediation)))
r1c_m1g_t1_sf_mediation_df$property <- rownames(r1c_m1g_t1_sf_mediation_df)
##b) sequential g analysis communal -> civil
r1c_m1g_t1_sc_sample <- subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)
r1c_m1g_t1_sc_stage1 <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sc_sample)
r1c_m1g_t1_sc_stage1_cse <- data.frame(cluster.se(r1c_m1g_t1_sc_stage1, as.factor(r1c_m1g_t1_sc_sample$cowcode))[,2])
r1c_m1g_t1_sc_stage2 <- lm(as.formula(paste("I(cw_event_g - coef(r1c_m1g_t1_sc_stage1)['cv_event_g_l0_l1_anyw']*cv_event_g_l0_l1_anyw)", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, "lpop","fractionalization1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sc_sample)
boots <- 100
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  r1c_m1g_t1_sc_sample.star <- r1c_m1g_t1_sc_sample[sample(1:nrow(r1c_m1g_t1_sc_sample),
                                                           replace = TRUE),]
  boot.first <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sc_sample.star)
  boot.direct <- lm(as.formula(paste("I(cw_event_g - coef(boot.first)['cv_event_g_l0_l1_anyw']*cv_event_g_l0_l1_anyw)", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, "lpop","fractionalization1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sc_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
r1c_m1g_t1_sc_seqg_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_m1g_t1_sc_stage1)['sa_territory_t'], coef(r1c_m1g_t1_sc_stage2)['sa_territory_t']), sd = c(r1c_m1g_t1_sc_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_m1g_t1_sf_sample <- subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)
r1c_m1g_t1_sf_stage1 <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sf_sample)
r1c_m1g_t1_sf_stage1_cse <- data.frame(cluster.se(r1c_m1g_t1_sf_stage1, as.factor(r1c_m1g_t1_sf_sample$cowcode))[,2])
r1c_m1g_t1_sf_stage2 <- lm(as.formula(paste("I(cw_event_g - coef(r1c_m1g_t1_sf_stage1)['cv_event_g_l0_l1_anyw']*cv_event_g_l0_l1_anyw)", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, "lpop","fractionalization1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sf_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  r1c_m1g_t1_sf_sample.star <- r1c_m1g_t1_sf_sample[sample(1:nrow(r1c_m1g_t1_sf_sample),
                                                           replace = TRUE),]
  boot.first <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sf_sample.star)
  boot.direct <- lm(as.formula(paste("I(cw_event_g - coef(boot.first)['cv_event_g_l0_l1_anyw']*cv_event_g_l0_l1_anyw)", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, "lpop","fractionalization1","cv_event_g_l0_l1_anyw"), collapse = " + "), sep = " ~ ")), data=r1c_m1g_t1_sf_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
r1c_m1g_t1_sf_seqg_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_m1g_t1_sf_stage1)['sa_territory_t'], coef(r1c_m1g_t1_sf_stage2)['sa_territory_t']), sd = c(r1c_m1g_t1_sf_stage1_cse['sa_territory_t',], sd(fl.boots)))
##c) mediation civil -> communal (all / formally organized / informally organized) (all / included-excluded)
r1c_m1d_t1_sb_med <- glm(as.formula(paste("cw_event_either_anyw_l0_l1", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_m1d_t1_sb_cse_med <- data.frame(cluster.se(r1c_m1d_t1_sb_med, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1c_m2d_t1_sb_med <- glm(as.formula(paste("cw_event_either_anyw_l0_l1", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1c_m2d_t1_sb_cse_med <- data.frame(cluster.se(r1c_m2d_t1_sb_med, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
main_dyad <- data.frame(main_dyad)
r1c_m1d_t1_sb_mediation <- mediate(r1c_m1d_t1_sb_med, r1c_m1d_t1_sb, treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_m2d_t1_sb_mediation_inc_exc0 <- mediate(r1c_m2d_t1_sb_med, r1c_m2d_t1_sb, covariates = list(included_excluded = 0), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_m2d_t1_sb_mediation_inc_exc1 <- mediate(r1c_m2d_t1_sb_med, r1c_m2d_t1_sb, covariates = list(included_excluded = 1), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_org_m1d_t1_sb_mediation <- mediate(r1c_m1d_t1_sb_med, r1c_org_m1d_t1_sb, treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_org_m2d_t1_sb_mediation_inc_exc0 <- mediate(r1c_m2d_t1_sb_med, r1c_org_m2d_t1_sb, covariates = list(included_excluded = 0), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_org_m2d_t1_sb_mediation_inc_exc1 <- mediate(r1c_m2d_t1_sb_med, r1c_org_m2d_t1_sb, covariates = list(included_excluded = 1), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_unorg_m1d_t1_sb_mediation <- mediate(r1c_m1d_t1_sb_med, r1c_unorg_m1d_t1_sb, treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_unorg_m2d_t1_sb_mediation_inc_exc0 <- mediate(r1c_m2d_t1_sb_med, r1c_unorg_m2d_t1_sb, covariates = list(included_excluded = 0), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_unorg_m2d_t1_sb_mediation_inc_exc1 <- mediate(r1c_m2d_t1_sb_med, r1c_unorg_m2d_t1_sb, covariates = list(included_excluded = 1), treat = "sa_territory_t", mediator = "cw_event_either_anyw_l0_l1",sims=100, cluster=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)
r1c_m1d_t1_sb_mediation_df <- data.frame(extract_mediation_summary(summary(r1c_m1d_t1_sb_mediation)))
r1c_m1d_t1_sb_mediation_df$property <- rownames(r1c_m1d_t1_sb_mediation_df)
r1c_m2d_t1_sb_mediation_inc_exc0_df <- data.frame(extract_mediation_summary(summary(r1c_m2d_t1_sb_mediation_inc_exc0)))
r1c_m2d_t1_sb_mediation_inc_exc0_df$property <- rownames(r1c_m2d_t1_sb_mediation_inc_exc0_df)
r1c_m2d_t1_sb_mediation_inc_exc1_df <- data.frame(extract_mediation_summary(summary(r1c_m2d_t1_sb_mediation_inc_exc1)))
r1c_m2d_t1_sb_mediation_inc_exc1_df$property <- rownames(r1c_m2d_t1_sb_mediation_inc_exc1_df)
r1c_org_m1d_t1_sb_mediation_df <- data.frame(extract_mediation_summary(summary(r1c_org_m1d_t1_sb_mediation)))
r1c_org_m1d_t1_sb_mediation_df$property <- rownames(r1c_org_m1d_t1_sb_mediation_df)
r1c_org_m2d_t1_sb_mediation_inc_exc0_df <- data.frame(extract_mediation_summary(summary(r1c_org_m2d_t1_sb_mediation_inc_exc0)))
r1c_org_m2d_t1_sb_mediation_inc_exc0_df$property <- rownames(r1c_org_m2d_t1_sb_mediation_inc_exc0_df)
r1c_org_m2d_t1_sb_mediation_inc_exc1_df <- data.frame(extract_mediation_summary(summary(r1c_org_m2d_t1_sb_mediation_inc_exc1)))
r1c_org_m2d_t1_sb_mediation_inc_exc1_df$property <- rownames(r1c_org_m2d_t1_sb_mediation_inc_exc1_df)
r1c_unorg_m1d_t1_sb_mediation_df <- data.frame(extract_mediation_summary(summary(r1c_unorg_m1d_t1_sb_mediation)))
r1c_unorg_m1d_t1_sb_mediation_df$property <- rownames(r1c_unorg_m1d_t1_sb_mediation_df)
r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df <- data.frame(extract_mediation_summary(summary(r1c_unorg_m2d_t1_sb_mediation_inc_exc0)))
r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$property <- rownames(r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df)
r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df <- data.frame(extract_mediation_summary(summary(r1c_unorg_m2d_t1_sb_mediation_inc_exc1)))
r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$property <- rownames(r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df)
##d) sequential g analysis civil -> communal (all / formally organized / informally organized)
main_dyad_seqg_sample <- subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)
r1c_m1d_t1_sb_stage1 <- lm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_m1d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_m1d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_m1d_t1_sb_stage2 <- lm(as.formula(paste("I(cv_event - coef(r1c_m1d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
boots <- 100
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
r1c_m1d_t1_sb_seqg_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_m1d_t1_sb_stage1)['sa_territory_t'], coef(r1c_m1d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_m1d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_m2d_t1_sb_stage1 <- lm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(cv_event - coef(r1c_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
r1c_m2d_t1_sb_seqg_inc_exc0_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
main_dyad_seqg_sample$included_excluded0 <- 1-main_dyad_seqg_sample$included_excluded
m2d_t1_alt <- c("sa_territory_t*included_excluded0","excluded_excluded")
r1c_m2d_t1_sb_stage1 <- lm(as.formula(paste("cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(cv_event - coef(r1c_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_m2d_t1_sb_seqg_inc_exc1_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_org_m1d_t1_sb_stage1 <- lm(as.formula(paste("org_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_org_m1d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_org_m1d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_org_m1d_t1_sb_stage2 <- lm(as.formula(paste("I(org_cv_event - coef(r1c_org_m1d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("org_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(org_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_org_m1d_t1_sb_seqg_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_org_m1d_t1_sb_stage1)['sa_territory_t'], coef(r1c_org_m1d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_org_m1d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_org_m2d_t1_sb_stage1 <- lm(as.formula(paste("org_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_org_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_org_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_org_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(org_cv_event - coef(r1c_org_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("org_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(org_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_org_m2d_t1_sb_seqg_inc_exc0_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_org_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_org_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_org_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_org_m2d_t1_sb_stage1 <- lm(as.formula(paste("org_cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_org_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_org_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_org_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(org_cv_event - coef(r1c_org_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("org_cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"org_cv_event_peaceyears_l1","I(org_cv_event_peaceyears_l1^2)","I(org_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(org_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
r1c_org_m2d_t1_sb_seqg_inc_exc1_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_org_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_org_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_org_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_unorg_m1d_t1_sb_stage1 <- lm(as.formula(paste("unorg_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_unorg_m1d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_unorg_m1d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_unorg_m1d_t1_sb_stage2 <- lm(as.formula(paste("I(unorg_cv_event - coef(r1c_unorg_m1d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("unorg_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(unorg_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_unorg_m1d_t1_sb_seqg_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_unorg_m1d_t1_sb_stage1)['sa_territory_t'], coef(r1c_unorg_m1d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_unorg_m1d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_unorg_m2d_t1_sb_stage1 <- lm(as.formula(paste("unorg_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_unorg_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_unorg_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_unorg_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(unorg_cv_event - coef(r1c_unorg_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("unorg_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(unorg_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_unorg_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_unorg_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_unorg_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
r1c_unorg_m2d_t1_sb_stage1 <- lm(as.formula(paste("unorg_cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
r1c_unorg_m2d_t1_sb_stage1_cse <- data.frame(cluster.se(r1c_unorg_m2d_t1_sb_stage1, as.factor(main_dyad_seqg_sample$cowcode))[,2])
r1c_unorg_m2d_t1_sb_stage2 <- lm(as.formula(paste("I(unorg_cv_event - coef(r1c_unorg_m2d_t1_sb_stage1)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample)
fl.boots <- rep(NA, times = boots)
for (b in 1:boots){
  main_dyad_seqg_sample.star <- main_dyad_seqg_sample[sample(1:nrow(main_dyad_seqg_sample),
                                                             replace = TRUE),]
  boot.first <- lm(as.formula(paste("unorg_cv_event", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"unorg_cv_event_peaceyears_l1","I(unorg_cv_event_peaceyears_l1^2)","I(unorg_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  boot.direct <- lm(as.formula(paste("I(unorg_cv_event - coef(boot.first)['cw_event_either_anyw_l0_l1']*cw_event_either_anyw_l0_l1)", paste(c(m2d_t1_alt, dyad_nat_vars, dyad_unit_vars, unit_vars, "lpop","fractionalization1","cw_event_either_anyw_l0_l1"), collapse = " + "), sep = " ~ ")), data=main_dyad_seqg_sample.star)
  fl.boots[b] <- coef(boot.direct)["sa_territory_t"]
}
sd(fl.boots)
gc()
r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df <- data.frame(property = c("Total Effect", "ACDE"), estimate = c(coef(r1c_unorg_m2d_t1_sb_stage1)['sa_territory_t'], coef(r1c_unorg_m2d_t1_sb_stage2)['sa_territory_t']), sd = c(r1c_unorg_m2d_t1_sb_stage1_cse['sa_territory_t',], sd(fl.boots)))
##e) mediation graph: communal -> civil
r1c_m1g_t1_sb_mediation_df$term <- "second-order maj."
r1c_m1g_t1_sb_mediation_df$conf.low <- r1c_m1g_t1_sb_mediation_df$lower
r1c_m1g_t1_sb_mediation_df$conf.high <- r1c_m1g_t1_sb_mediation_df$upper
r1c_m1g_t1_sf_mediation_df$term <- "second-order min."
r1c_m1g_t1_sf_mediation_df$conf.low <- r1c_m1g_t1_sf_mediation_df$lower
r1c_m1g_t1_sf_mediation_df$conf.high <- r1c_m1g_t1_sf_mediation_df$upper
#
r1c_m1g_t1_sc_seqg_df$term <- "second-order maj."
r1c_m1g_t1_sc_seqg_df$conf.low <- r1c_m1g_t1_sc_seqg_df$estimate - 1.96 * r1c_m1g_t1_sc_seqg_df$sd
r1c_m1g_t1_sc_seqg_df$conf.high <- r1c_m1g_t1_sc_seqg_df$estimate + 1.96 * r1c_m1g_t1_sc_seqg_df$sd
r1c_m1g_t1_sf_seqg_df$term <- "second-order min."
r1c_m1g_t1_sf_seqg_df$conf.low <- r1c_m1g_t1_sf_seqg_df$estimate - 1.96 * r1c_m1g_t1_sf_seqg_df$sd
r1c_m1g_t1_sf_seqg_df$conf.high <- r1c_m1g_t1_sf_seqg_df$estimate + 1.96 * r1c_m1g_t1_sf_seqg_df$sd
cw_cv_mediation <- rbind.fill(r1c_m1g_t1_sb_mediation_df,r1c_m1g_t1_sf_mediation_df,r1c_m1g_t1_sc_seqg_df,r1c_m1g_t1_sf_seqg_df)
cw_cv_mediation <- subset(cw_cv_mediation, property == "ACME (average)" | property == "ADE (average)" | property == "ACDE")
cw_cv_mediation$model <- cw_cv_mediation$property
cw_cv_mediation_plot <- dwplot(cw_cv_mediation, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=1,"effect",reverse=TRUE), colour = guide_legend(nrow=1,"effect",reverse=TRUE),linetype = guide_legend(nrow=1,"effect",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(-0.015, -0.0075, 0))
##f) mediation graph: civil -> communal
r1c_m1d_t1_sb_mediation_df$term <- "all"
r1c_m1d_t1_sb_mediation_df$conf.low <- r1c_m1d_t1_sb_mediation_df$lower
r1c_m1d_t1_sb_mediation_df$conf.high <- r1c_m1d_t1_sb_mediation_df$upper
r1c_m2d_t1_sb_mediation_inc_exc1_df$term <- "included/excluded"
r1c_m2d_t1_sb_mediation_inc_exc1_df$conf.low <- r1c_m2d_t1_sb_mediation_inc_exc1_df$lower
r1c_m2d_t1_sb_mediation_inc_exc1_df$conf.high <- r1c_m2d_t1_sb_mediation_inc_exc1_df$upper
r1c_m2d_t1_sb_mediation_inc_exc0_df$term <- "evenly in-/excluded"
r1c_m2d_t1_sb_mediation_inc_exc0_df$conf.low <- r1c_m2d_t1_sb_mediation_inc_exc0_df$lower
r1c_m2d_t1_sb_mediation_inc_exc0_df$conf.high <- r1c_m2d_t1_sb_mediation_inc_exc0_df$upper
r1c_m1d_t1_sb_seqg_df$term <- "all"
r1c_m1d_t1_sb_seqg_df$conf.low <- r1c_m1d_t1_sb_seqg_df$estimate - 1.96 * r1c_m1d_t1_sb_seqg_df$sd
r1c_m1d_t1_sb_seqg_df$conf.high <- r1c_m1d_t1_sb_seqg_df$estimate + 1.96 * r1c_m1d_t1_sb_seqg_df$sd
r1c_m2d_t1_sb_seqg_inc_exc1_df$term <- "included/excluded"
r1c_m2d_t1_sb_seqg_inc_exc1_df$conf.low <- r1c_m2d_t1_sb_seqg_inc_exc1_df$estimate - 1.96 * r1c_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_m2d_t1_sb_seqg_inc_exc1_df$conf.high <- r1c_m2d_t1_sb_seqg_inc_exc1_df$estimate + 1.96 * r1c_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_m2d_t1_sb_seqg_inc_exc0_df$term <- "evenly in-/excluded"
r1c_m2d_t1_sb_seqg_inc_exc0_df$conf.low <- r1c_m2d_t1_sb_seqg_inc_exc0_df$estimate - 1.96 * r1c_m2d_t1_sb_seqg_inc_exc0_df$sd
r1c_m2d_t1_sb_seqg_inc_exc0_df$conf.high <- r1c_m2d_t1_sb_seqg_inc_exc0_df$estimate + 1.96 * r1c_m2d_t1_sb_seqg_inc_exc0_df$sd
cv_cw_mediation <- rbind.fill(r1c_m1d_t1_sb_mediation_df,r1c_m2d_t1_sb_mediation_inc_exc1_df,r1c_m2d_t1_sb_mediation_inc_exc0_df,r1c_m1d_t1_sb_seqg_df,r1c_m2d_t1_sb_seqg_inc_exc1_df,r1c_m2d_t1_sb_seqg_inc_exc0_df)
cv_cw_mediation <- subset(cv_cw_mediation, property == "ACME (average)" | property == "ADE (average)" | property == "ACDE")
cv_cw_mediation$model <- cv_cw_mediation$property
cv_cw_mediation_plot <- dwplot(cv_cw_mediation, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ #+
  guides(shape = guide_legend(nrow=1,"effect",reverse=TRUE), colour = guide_legend(nrow=1,"effect",reverse=TRUE),linetype = guide_legend(nrow=1,"effect",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(0,0.005,0.010))
##g) mediation graph: civil -> communal (formally organized)
r1c_org_m1d_t1_sb_mediation_df$term <- "all"
r1c_org_m1d_t1_sb_mediation_df$conf.low <- r1c_org_m1d_t1_sb_mediation_df$lower
r1c_org_m1d_t1_sb_mediation_df$conf.high <- r1c_org_m1d_t1_sb_mediation_df$upper
r1c_org_m2d_t1_sb_mediation_inc_exc1_df$term <- "included/excluded"
r1c_org_m2d_t1_sb_mediation_inc_exc1_df$conf.low <- r1c_org_m2d_t1_sb_mediation_inc_exc1_df$lower
r1c_org_m2d_t1_sb_mediation_inc_exc1_df$conf.high <- r1c_org_m2d_t1_sb_mediation_inc_exc1_df$upper
r1c_org_m2d_t1_sb_mediation_inc_exc0_df$term <- "evenly in-/excluded"
r1c_org_m2d_t1_sb_mediation_inc_exc0_df$conf.low <- r1c_org_m2d_t1_sb_mediation_inc_exc0_df$lower
r1c_org_m2d_t1_sb_mediation_inc_exc0_df$conf.high <- r1c_org_m2d_t1_sb_mediation_inc_exc0_df$upper
r1c_org_m1d_t1_sb_seqg_df$term <- "all"
r1c_org_m1d_t1_sb_seqg_df$conf.low <- r1c_org_m1d_t1_sb_seqg_df$estimate - 1.96 * r1c_org_m1d_t1_sb_seqg_df$sd
r1c_org_m1d_t1_sb_seqg_df$conf.high <- r1c_org_m1d_t1_sb_seqg_df$estimate + 1.96 * r1c_org_m1d_t1_sb_seqg_df$sd
r1c_org_m2d_t1_sb_seqg_inc_exc1_df$term <- "included/excluded"
r1c_org_m2d_t1_sb_seqg_inc_exc1_df$conf.low <- r1c_org_m2d_t1_sb_seqg_inc_exc1_df$estimate - 1.96 * r1c_org_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_org_m2d_t1_sb_seqg_inc_exc1_df$conf.high <- r1c_org_m2d_t1_sb_seqg_inc_exc1_df$estimate + 1.96 * r1c_org_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_org_m2d_t1_sb_seqg_inc_exc0_df$term <- "evenly in-/excluded"
r1c_org_m2d_t1_sb_seqg_inc_exc0_df$conf.low <- r1c_org_m2d_t1_sb_seqg_inc_exc0_df$estimate - 1.96 * r1c_org_m2d_t1_sb_seqg_inc_exc0_df$sd
r1c_org_m2d_t1_sb_seqg_inc_exc0_df$conf.high <- r1c_org_m2d_t1_sb_seqg_inc_exc0_df$estimate + 1.96 * r1c_org_m2d_t1_sb_seqg_inc_exc0_df$sd
cv_org_cw_mediation <- rbind.fill(r1c_org_m1d_t1_sb_mediation_df,r1c_org_m2d_t1_sb_mediation_inc_exc1_df,r1c_org_m2d_t1_sb_mediation_inc_exc0_df,r1c_org_m1d_t1_sb_seqg_df,r1c_org_m2d_t1_sb_seqg_inc_exc1_df,r1c_org_m2d_t1_sb_seqg_inc_exc0_df)
cv_org_cw_mediation$property <- ifelse(cv_org_cw_mediation$property == "ACME", "ACME (average)", cv_org_cw_mediation$property)
cv_org_cw_mediation$property <- ifelse(cv_org_cw_mediation$property == "ADE", "ADE (average)", cv_org_cw_mediation$property)
cv_org_cw_mediation <- subset(cv_org_cw_mediation, property == "ACME (average)" | property == "ADE (average)" | property == "ACDE")
cv_org_cw_mediation$model <- cv_org_cw_mediation$property
cv_org_cw_mediation_plot <- dwplot(cv_org_cw_mediation, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("c) formally org. communal violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ #+
  guides(shape = guide_legend(nrow=1,"effect",reverse=TRUE), colour = guide_legend(nrow=1,"effect",reverse=TRUE),linetype = guide_legend(nrow=1,"effect",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(-0.005,0,0.005,0.01))
##h) mediation graph: civil -> communal (informally organized)
r1c_unorg_m1d_t1_sb_mediation_df$term <- "all"
r1c_unorg_m1d_t1_sb_mediation_df$conf.low <- r1c_unorg_m1d_t1_sb_mediation_df$lower
r1c_unorg_m1d_t1_sb_mediation_df$conf.high <- r1c_unorg_m1d_t1_sb_mediation_df$upper
r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$term <- "included/excluded"
r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$conf.low <- r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$lower
r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$conf.high <- r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df$upper
r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$term <- "evenly in-/excluded"
r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$conf.low <- r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$lower
r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$conf.high <- r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df$upper
r1c_unorg_m1d_t1_sb_seqg_df$term <- "all"
r1c_unorg_m1d_t1_sb_seqg_df$conf.low <- r1c_unorg_m1d_t1_sb_seqg_df$estimate - 1.96 * r1c_unorg_m1d_t1_sb_seqg_df$sd
r1c_unorg_m1d_t1_sb_seqg_df$conf.high <- r1c_unorg_m1d_t1_sb_seqg_df$estimate + 1.96 * r1c_unorg_m1d_t1_sb_seqg_df$sd
r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$term <- "included/excluded"
r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$conf.low <- r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$estimate - 1.96 * r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$conf.high <- r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$estimate + 1.96 * r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df$sd
r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$term <- "evenly in-/excluded"
r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$conf.low <- r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$estimate - 1.96 * r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$sd
r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$conf.high <- r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$estimate + 1.96 * r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df$sd
cv_unorg_cw_mediation <- rbind.fill(r1c_unorg_m1d_t1_sb_mediation_df,r1c_unorg_m2d_t1_sb_mediation_inc_exc1_df,r1c_unorg_m2d_t1_sb_mediation_inc_exc0_df,r1c_unorg_m1d_t1_sb_seqg_df,r1c_unorg_m2d_t1_sb_seqg_inc_exc1_df,r1c_unorg_m2d_t1_sb_seqg_inc_exc0_df)
cv_unorg_cw_mediation$property <- ifelse(cv_unorg_cw_mediation$property == "ACME", "ACME (average)", cv_unorg_cw_mediation$property)
cv_unorg_cw_mediation$property <- ifelse(cv_unorg_cw_mediation$property == "ADE", "ADE (average)", cv_unorg_cw_mediation$property)
cv_unorg_cw_mediation <- subset(cv_unorg_cw_mediation, property == "ACME (average)" | property == "ADE (average)" | property == "ACDE")
cv_unorg_cw_mediation$model <- cv_unorg_cw_mediation$property
cv_unorg_cw_mediation_plot <- dwplot(cv_unorg_cw_mediation, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("d) informally org. communal violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ #+
  guides(shape = guide_legend(nrow=1,"effect",reverse=TRUE), colour = guide_legend(nrow=1,"effect",reverse=TRUE),linetype = guide_legend(nrow=1,"effect",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(-0.005,0,0.005,0.010,0.015))
##i) combined
figurea5 <- grid_arrange_shared_legend(cw_cv_mediation_plot, cv_cw_mediation_plot, cv_org_cw_mediation_plot, cv_unorg_cw_mediation_plot, ncol=2, nrow=2)
ggsave(figurea5, file='../figures/figurea5.pdf', width = 14, height = 10, units="cm",dpi=1000)