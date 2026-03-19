####Model results####
##a) LPM  (table X11, models A30-A33)
lm_main_m1g_t1_sc <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
lm_main_m1g_t1_sc_cse <- data.frame(cluster.se(lm_main_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
lm_main_m1g_t1_sf <- lm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
lm_main_m1g_t1_sf_cse <- data.frame(cluster.se(lm_main_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
lm_main_m1d_t1_sb <- lm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
lm_main_m1d_t1_sb_cse <- data.frame(cluster.se(lm_main_m1d_t1_sb, as.factor(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
lm_main_m2d_t1_sb <- lm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
lm_main_m2d_t1_sb_cse <- data.frame(cluster.se(lm_main_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(lm_main_m1g_t1_sc, lm_main_m1g_t1_sf, lm_main_m1d_t1_sb, lm_main_m2d_t1_sb, se=c(lm_main_m1g_t1_sc_cse, lm_main_m1g_t1_sf_cse, lm_main_m1d_t1_sb_cse, lm_main_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A30)", "Min. (model A31)","Maj./min. dyad (model A32)","Maj./min. dyad (model A33)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X11. Additional results: linear probability model.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex11.txt")
stargazer(lm_main_m1g_t1_sc, lm_main_m1g_t1_sf, lm_main_m1d_t1_sb, lm_main_m2d_t1_sb, se=c(lm_main_m1g_t1_sc_cse, lm_main_m1g_t1_sf_cse, lm_main_m1d_t1_sb_cse, lm_main_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A30)", "Min. (model A31)","Maj./min. dyad (model A32)","Maj./min. dyad (model A33)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X11. Additional results: linear probability model.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex11.html")
##b) no FE (table X12, models A34-A37)
nofe_main_m1g_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
nofe_main_m1g_t1_sc_cse <- data.frame(cluster.se(nofe_main_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
nofe_main_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars,"cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
nofe_main_m1g_t1_sf_cse <- data.frame(cluster.se(nofe_main_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
nofe_main_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
nofe_main_m1d_t1_sb_cse <- data.frame(cluster.se(nofe_main_m1d_t1_sb, as.factor(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
nofe_main_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
nofe_main_m2d_t1_sb_cse <- data.frame(cluster.se(nofe_main_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(nofe_main_m1g_t1_sc, nofe_main_m1g_t1_sf, nofe_main_m1d_t1_sb, nofe_main_m2d_t1_sb, se=c(nofe_main_m1g_t1_sc_cse, nofe_main_m1g_t1_sf_cse, nofe_main_m1d_t1_sb_cse, nofe_main_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A34)", "Min. (model A35)","Maj./min. dyad (model A36)","Maj./min. dyad (model A37)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X12. Additional results: omitting region- and year-fixed effects.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex12.txt")
stargazer(nofe_main_m1g_t1_sc, nofe_main_m1g_t1_sf, nofe_main_m1d_t1_sb, nofe_main_m2d_t1_sb, se=c(nofe_main_m1g_t1_sc_cse, nofe_main_m1g_t1_sf_cse, nofe_main_m1d_t1_sb_cse, nofe_main_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A34)", "Min. (model A35)","Maj./min. dyad (model A36)","Maj./min. dyad (model A37)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X12. Additional results: omitting region- and year-fixed effects.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex12.html")

####Figure A10####
##a) civil violence (LPM)
me_lm_main_m1g_t1_sc <- margins_summary(lm_main_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(lm_main_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_lm_main_m1g_t1_sc$term <- "second-order maj."
me_lm_main_m1g_t1_sf <- margins_summary(lm_main_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(lm_main_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_lm_main_m1g_t1_sf$term <- "second-order min."
me_lm_main_m1g <- rbind.fill(me_lm_main_m1g_t1_sc, me_lm_main_m1g_t1_sf)
me_lm_main_m1g$term <- as.factor(me_lm_main_m1g$term)
me_lm_main_m1g$term = factor(me_lm_main_m1g$term,levels(me_lm_main_m1g$term)[c(2,1)])
me_lm_main_m1g$model <- "linear specification"
me_lm_main_m1g$estimate <- me_lm_main_m1g$AME
me_lm_main_m1g$conf.low <- me_lm_main_m1g$lower
me_lm_main_m1g$conf.high <- me_lm_main_m1g$upper
##b) communal violence (LPM)
me_lm_main_m1d_t1_sb <- margins_summary(lm_main_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(lm_main_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_lm_main_m1d_t1_sb$term <- "all"
me_lm_main_m1d_t1_sb$model <- "linear specification"
me_lm_main_m2d_t1_sb <- margins_summary(lm_main_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(lm_main_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_lm_main_m2d_t1_sb$term <- ifelse(me_lm_main_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_lm_main_m2d_t1_sb$model <- "linear specification"
me_lm_main_m13d <- rbind.fill(me_lm_main_m1d_t1_sb, me_lm_main_m2d_t1_sb)
me_lm_main_m13d <- subset(me_lm_main_m13d, term != "other")
me_lm_main_m13d$term <- as.factor(me_lm_main_m13d$term)
me_lm_main_m13d$estimate <- me_lm_main_m13d$AME
me_lm_main_m13d$conf.low <- me_lm_main_m13d$lower
me_lm_main_m13d$conf.high <- me_lm_main_m13d$upper
##c) civil violence (no FE)
me_nofe_main_m1g_t1_sc <- margins_summary(nofe_main_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(nofe_main_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_nofe_main_m1g_t1_sc$term <- "second-order maj."
me_nofe_main_m1g_t1_sf <- margins_summary(nofe_main_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(nofe_main_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_nofe_main_m1g_t1_sf$term <- "second-order min."
me_nofe_main_m1g <- rbind.fill(me_nofe_main_m1g_t1_sc, me_nofe_main_m1g_t1_sf)
me_nofe_main_m1g$term <- as.factor(me_nofe_main_m1g$term)
me_nofe_main_m1g$term = factor(me_nofe_main_m1g$term,levels(me_nofe_main_m1g$term)[c(2,1)])
me_nofe_main_m1g$model <- "no fixed effects"
me_nofe_main_m1g$estimate <- me_nofe_main_m1g$AME
me_nofe_main_m1g$conf.low <- me_nofe_main_m1g$lower
me_nofe_main_m1g$conf.high <- me_nofe_main_m1g$upper
##d) communal violence (no FE)
me_nofe_main_m1d_t1_sb <- margins_summary(nofe_main_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(nofe_main_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_nofe_main_m1d_t1_sb$term <- "all"
me_nofe_main_m1d_t1_sb$model <- "no fixed effects"
me_nofe_main_m2d_t1_sb <- margins_summary(nofe_main_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(nofe_main_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_nofe_main_m2d_t1_sb$term <- ifelse(me_nofe_main_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_nofe_main_m2d_t1_sb$model <- "no fixed effects"
me_nofe_main_m13d <- rbind.fill(me_nofe_main_m1d_t1_sb, me_nofe_main_m2d_t1_sb)
me_nofe_main_m13d <- subset(me_nofe_main_m13d, term != "other")
me_nofe_main_m13d$term <- as.factor(me_nofe_main_m13d$term)
me_nofe_main_m13d$estimate <- me_nofe_main_m13d$AME
me_nofe_main_m13d$conf.low <- me_nofe_main_m13d$lower
me_nofe_main_m13d$conf.high <- me_nofe_main_m13d$upper
##e) combined, civil violence
figurea10_plot_data_g <- rbind.fill(me_lm_main_m1g,me_nofe_main_m1g)
figurea10_plot_data_g$model <- as.factor(figurea10_plot_data_g$model)
figurea10_plot_data_g$term <- as.factor(figurea10_plot_data_g$term)
figurea10_plot_data_g$term = factor(figurea10_plot_data_g$term,levels(figurea10_plot_data_g$term)[c(2,1)])
figurea10_plot_g <- dwplot(figurea10_plot_data_g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ 
  guides(shape = guide_legend(nrow=1,"model",reverse=TRUE), colour = guide_legend(nrow=1,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks =c(-0.02, -0.01, 0))
##f) combined, communal violence
figurea10_plot_data_d <- rbind.fill(me_lm_main_m13d,me_nofe_main_m13d)
figurea10_plot_data_d$model <- as.factor(figurea10_plot_data_d$model)
figurea10_plot_d <- dwplot(figurea10_plot_data_d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)),  whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+
  guides(shape = guide_legend(nrow=1,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=1,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##g) combined
figurea10 <- grid_arrange_shared_legend(figurea10_plot_g, figurea10_plot_d, ncol=2, nrow=1)
ggsave(figurea10, file='../figures/figurea10.pdf', width = 14, height = 5, units="cm",dpi=1000)