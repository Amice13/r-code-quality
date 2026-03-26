####Model results (table X13, models A38-A41)####
main_m1g_gid_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_gid_vars, gid_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_unit_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
main_m1g_gid_t1_sc_cse <- data.frame(cluster.se(main_m1g_gid_t1_sc, as.factor(subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
main_m1g_gid_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_gid_vars, gid_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_unit_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
main_m1g_gid_t1_sf_cse <- data.frame(cluster.se(main_m1g_gid_t1_sf, as.factor(subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
main_m1d_gid_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_gid_vars, gid_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_unit_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
main_m1d_gid_t1_sb_cse <- data.frame(cluster.se(main_m1d_gid_t1_sb, as.factor(subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
main_m2d_gid_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_gid_vars, gid_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_unit_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
main_m2d_gid_t1_sb_cse <- data.frame(cluster.se(main_m2d_gid_t1_sb, as.factor(subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(main_m1g_gid_t1_sc, main_m1g_gid_t1_sf, main_m1d_gid_t1_sb, main_m2d_gid_t1_sb, se=c(main_m1g_gid_t1_sc_cse, main_m1g_gid_t1_sf_cse, main_m1d_gid_t1_sb_cse, main_m2d_gid_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A38)", "Min. (model A39)","Maj./min. dyad (model A40)","Maj./min. dyad (model A41)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X13. Additional results: group/dyad grid cell-level specifications.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given grid cell.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Asymmetry","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Relative size (grid cell)","Relative size (grid cell, mean)","Relative size (grid cell, diff.)", "Population (grid cell, logged)", "Area (grid cell, logged)", "Avg. ruggedness (grid cell)", "Oil in grid cell", "Distance capital (grid cell, logged)", "Distance border (grid cell, logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex13.txt")
stargazer(main_m1g_gid_t1_sc, main_m1g_gid_t1_sf, main_m1d_gid_t1_sb, main_m2d_gid_t1_sb, se=c(main_m1g_gid_t1_sc_cse, main_m1g_gid_t1_sf_cse, main_m1d_gid_t1_sb_cse, main_m2d_gid_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A38)", "Min. (model A39)","Maj./min. dyad (model A40)","Maj./min. dyad (model A41)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X13. Additional results: group/dyad grid cell-level specifications.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given grid cell.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Asymmetry","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Relative size (grid cell)","Relative size (grid cell, mean)","Relative size (grid cell, diff.)", "Population (grid cell, logged)", "Area (grid cell, logged)", "Avg. ruggedness (grid cell)", "Oil in grid cell", "Distance capital (grid cell, logged)", "Distance border (grid cell, logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex13.html")

####Figure A11####
##a) civil
me_main_m1g_gid_t1_sc <- margins_summary(main_m1g_gid_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(main_m1g_gid_t1_sc, as.integer(subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_main_m1g_gid_t1_sc$term <- "second-order maj."
me_main_m1g_gid_t1_sf <- margins_summary(main_m1g_gid_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(main_m1g_gid_t1_sf, as.integer(subset(grid_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_main_m1g_gid_t1_sf$term <- "second-order min."
me_main_m1g_gid <- rbind.fill(me_main_m1g_gid_t1_sc, me_main_m1g_gid_t1_sf)
me_main_m1g_gid$term <- as.factor(me_main_m1g_gid$term)
me_main_m1g_gid$term = factor(me_main_m1g_gid$term,levels(me_main_m1g_gid$term)[c(2,1)])
me_main_m1g_gid$model <- 1
me_main_m1g_gid$estimate <- me_main_m1g_gid$AME
me_main_m1g_gid$conf.low <- me_main_m1g_gid$lower
me_main_m1g_gid$conf.high <- me_main_m1g_gid$upper
me_main_m1g_gid_plot <- dwplot(me_main_m1g_gid, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##b) communal
me_main_m1d_gid_t1_sb <- margins_summary(main_m1d_gid_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(main_m1d_gid_t1_sb, as.integer(subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m1d_gid_t1_sb$term <- "all"
me_main_m1d_gid_t1_sb$model <- 1
me_main_m2d_gid_t1_sb <- margins_summary(main_m2d_gid_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(main_m2d_gid_t1_sb, as.integer(subset(grid_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m2d_gid_t1_sb$term <- ifelse(me_main_m2d_gid_t1_sb$included_excluded == 1, "included/excluded", "other")
me_main_m2d_gid_t1_sb$model <- 1
me_main_gid_m13d <- rbind.fill(me_main_m1d_gid_t1_sb, me_main_m2d_gid_t1_sb)
me_main_gid_m13d <- subset(me_main_gid_m13d, term != "other")
me_main_gid_m13d$term <- as.factor(me_main_gid_m13d$term)
me_main_gid_m13d$estimate <- me_main_gid_m13d$AME
me_main_gid_m13d$conf.low <- me_main_gid_m13d$lower
me_main_gid_m13d$conf.high <- me_main_gid_m13d$upper
me_main_gid_m13d_plot <- dwplot(me_main_gid_m13d,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\nof communal violence") + ylab("dyad type") +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(0, 0.0005, 0.0010, 0.0020))
##c) combined
figurea11 <- grid.arrange(me_main_m1g_gid_plot, me_main_gid_m13d_plot, ncol=2, nrow=1)
ggsave(figurea11, file='../figures/figurea11.pdf', width = 14, height = 3.5, units="cm",dpi=1000)