####Model results: Controlling for average territorial autonomy (table X1, models A1-A4)####
r1_m1g_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, "sa_territory_t_d_mean", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
r1_m1g_t1_sc_cse <- data.frame(cluster.se(r1_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
r1_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, "sa_territory_t_d_mean", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r1_m1g_t1_sf_cse <- data.frame(cluster.se(r1_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
r1_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, "sa_territory_t_d_mean_nd", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1_m1d_t1_sb_cse <- data.frame(cluster.se(r1_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, "sa_territory_t_d_mean_nd", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1_m2d_t1_sb_cse <- data.frame(cluster.se(r1_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r1_m1g_t1_sc, r1_m1g_t1_sf, r1_m1d_t1_sb, r1_m2d_t1_sb, se=c(r1_m1g_t1_sc_cse, r1_m1g_t1_sf_cse, r1_m1d_t1_sb_cse, r1_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A1)", "Min. (model A2)","Maj./min. dyad (model A3)","Maj./min. dyad (model A4)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X1. Additional results: controlling for average territorial autonomy.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy (mean)","Territorial autonomy (mean, of min.)","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex1.txt")
stargazer(r1_m1g_t1_sc, r1_m1g_t1_sf, r1_m1d_t1_sb, r1_m2d_t1_sb, se=c(r1_m1g_t1_sc_cse, r1_m1g_t1_sf_cse, r1_m1d_t1_sb_cse, r1_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A1)", "Min. (model A2)","Maj./min. dyad (model A3)","Maj./min. dyad (model A4)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X1. Additional results: controlling for average territorial autonomy.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy (mean)","Territorial autonomy (mean, of min.)","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex1.html")

####Model results: excluding SO minorities with any type of de-facto autonomy (table X2, models A5-A7)####
r1b_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, autonomy == 0 & subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r1b_m1g_t1_sf_cse <- data.frame(cluster.se(r1b_m1g_t1_sf, as.factor(subset(main_group, autonomy == 0 & subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
r1b_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1))
r1b_m1d_t1_sb_cse <- data.frame(cluster.se(r1b_m1d_t1_sb, as.factor(subset(main_dyad, autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1b_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1))
r1b_m2d_t1_sb_cse <- data.frame(cluster.se(r1b_m2d_t1_sb, as.factor(subset(main_dyad, autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r1b_m1g_t1_sf, r1b_m1d_t1_sb, r1b_m2d_t1_sb, se=c(r1b_m1g_t1_sf_cse, r1b_m1d_t1_sb_cse, r1b_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Min. (model A5)","Maj./min. dyad (model A6)","Maj./min. dyad (model A7)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X2. Additional results: excluding second-order minorities with de-facto autonomy.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex2.txt")
stargazer(r1b_m1g_t1_sf, r1b_m1d_t1_sb, r1b_m2d_t1_sb, se=c(r1b_m1g_t1_sf_cse, r1b_m1d_t1_sb_cse, r1b_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Min. (model A5)","Maj./min. dyad (model A6)","Maj./min. dyad (model A7)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X2. Additional results: excluding second-order minorities with de-facto autonomy.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex2.html")

####Model results: controlling for maximum territorial autonomy (table X3, models A8-A11)####
r1a_m1g_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, "sa_territory_t_d_max_any_country", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
r1a_m1g_t1_sc_cse <- data.frame(cluster.se(r1a_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
r1a_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c(m1g_t1, "sa_territory_t_d_max_any_country", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r1a_m1g_t1_sf_cse <- data.frame(cluster.se(r1a_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
r1a_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, "sa_territory_t_d_max_any_country", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1a_m1d_t1_sb_cse <- data.frame(cluster.se(r1a_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
r1a_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, "sa_territory_t_d_max_any_country", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r1a_m2d_t1_sb_cse <- data.frame(cluster.se(r1a_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r1a_m1g_t1_sc, r1a_m1g_t1_sf, r1a_m1d_t1_sb, r1a_m2d_t1_sb, se=c(r1a_m1g_t1_sc_cse, r1a_m1g_t1_sf_cse, r1a_m1d_t1_sb_cse, r1a_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A8)", "Min. (model A9)","Maj./min. dyad (model A10)","Maj./min. dyad (model A11)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X3. Additional results: controlling for maximum territorial autonomy of any administrative unit.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy (max.)","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex3.txt")
stargazer(r1a_m1g_t1_sc, r1a_m1g_t1_sf, r1a_m1d_t1_sb, r1a_m2d_t1_sb, se=c(r1a_m1g_t1_sc_cse, r1a_m1g_t1_sf_cse, r1a_m1d_t1_sb_cse, r1a_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A8)", "Min. (model A9)","Maj./min. dyad (model A10)","Maj./min. dyad (model A11)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X3. Additional results: controlling for maximum territorial autonomy of any administrative unit.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy (max.)","Territorial autonomy x included/excluded","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex3.html")

####Figure A1####
##a) civil violence
me_r1_m1g_t1_sc <- margins_summary(r1_m1g_t1_sc, variables = "sa_territory_t_d_mean", vcov = cluster.vcov(r1_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_r1_m1g_t1_sc$term <- "second-order maj."
me_r1_m1g_t1_sf <- margins_summary(r1_m1g_t1_sf, variables = "sa_territory_t_d_mean", vcov = cluster.vcov(r1_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_r1_m1g_t1_sf$term <- "second-order min."
me_r1_m1g <- rbind.fill(me_r1_m1g_t1_sc, me_r1_m1g_t1_sf)
me_r1_m1g$term <- as.factor(me_r1_m1g$term)
me_r1_m1g$term = factor(me_r1_m1g$term,levels(me_r1_m1g$term)[c(2,1)])
me_r1_m1g$model <- "all"
me_r1_m1g$estimate <- me_r1_m1g$AME
me_r1_m1g$conf.low <- me_r1_m1g$lower
me_r1_m1g$conf.high <- me_r1_m1g$upper
me_r1_m1g_plot <- dwplot(me_r1_m1g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ 
  scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ 
  scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE), linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(0, 0.015, 0.03))
##b) communal violence
me_r1_m1d_t1_sb <- margins_summary(r1_m1d_t1_sb, variables = "sa_territory_t_d_mean_nd", vcov = cluster.vcov(r1_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1_m1d_t1_sb$term <- "all"
me_r1_m1d_t1_sb$model <- "all"
me_r1_m13d <- me_r1_m1d_t1_sb
me_r1_m13d <- subset(me_r1_m13d, term != "other")
me_r1_m13d$term <- as.factor(me_r1_m13d$term)
me_r1_m13d$estimate <- me_r1_m13d$AME
me_r1_m13d$conf.low <- me_r1_m13d$lower
me_r1_m13d$conf.high <- me_r1_m13d$upper
me_r1_m13d_plot <- dwplot(me_r1_m13d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") + theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type") + scale_x_continuous(labels=scales::percent_format()) +
  guides(shape = guide_legend(nrow=1,"type of communal violence",reverse=TRUE), colour = guide_legend(nrow=1,"type of communal violence",reverse=TRUE),linetype = guide_legend(nrow=1,"type of communal violence",reverse=TRUE))
##c) combined
figurea1 <- grid.arrange(me_r1_m1g_plot,me_r1_m13d_plot,ncol=2,nrow=1)
ggsave(figurea1, file='../figures/figurea1.pdf', width = 14, height = 3.5, units="cm",dpi=1000)

####Figure A2####
##a) civil violence
me_r1a_m1g_t1_sc <- margins_summary(r1a_m1g_t1_sc, variables = "sa_territory_t_d_max_any_country", vcov = cluster.vcov(r1a_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_r1a_m1g_t1_sc$term <- "second-order maj."
me_r1a_m1g_t1_sf <- margins_summary(r1a_m1g_t1_sf, variables = "sa_territory_t_d_max_any_country", vcov = cluster.vcov(r1a_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_r1a_m1g_t1_sf$term <- "second-order min."
me_r1a_m1g <- rbind.fill(me_r1a_m1g_t1_sc, me_r1a_m1g_t1_sf)
me_r1a_m1g$term <- as.factor(me_r1a_m1g$term)
me_r1a_m1g$term = factor(me_r1a_m1g$term,levels(me_r1a_m1g$term)[c(2,1)])
me_r1a_m1g$model <- "all"
me_r1a_m1g$estimate <- me_r1a_m1g$AME
me_r1a_m1g$conf.low <- me_r1a_m1g$lower
me_r1a_m1g$conf.high <- me_r1a_m1g$upper
me_r1a_m1g_plot <- dwplot(me_r1a_m1g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") + scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3")) + scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1)) + scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type") + 
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE), linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##b) communal violence
me_r1a_m1d_t1_sb <- margins_summary(r1a_m1d_t1_sb, variables = "sa_territory_t_d_max_any_country", vcov = cluster.vcov(r1a_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r1a_m1d_t1_sb$term <- "all"
me_r1a_m1d_t1_sb$model <- "all"
me_r1a_m13d <- me_r1a_m1d_t1_sb
me_r1a_m13d <- subset(me_r1a_m13d, term != "other")
me_r1a_m13d$term <- as.factor(me_r1a_m13d$term)
me_r1a_m13d$estimate <- me_r1a_m13d$AME
me_r1a_m13d$conf.low <- me_r1a_m13d$lower
me_r1a_m13d$conf.high <- me_r1a_m13d$upper
me_r1a_m13d_plot <- dwplot(me_r1a_m13d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type") +
  scale_x_continuous(labels=scales::percent_format()) +
  guides(shape = guide_legend(nrow=1,"type of communal violence",reverse=TRUE), colour = guide_legend(nrow=1,"type of communal violence",reverse=TRUE),linetype = guide_legend(nrow=1,"type of communal violence",reverse=TRUE))
##c) combined
figurea2 <- grid.arrange(me_r1a_m1g_plot,me_r1a_m13d_plot,ncol=2,nrow=1)
ggsave(figurea2, file='../figures/figurea2.pdf', width = 14, height = 3.5, units="cm",dpi=1000)

####Figure A3####
##a) Controlling for average territorial autonomy, civil violence
me2_r1_m1g_t1_sc <- margins_summary(r1_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(r1_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me2_r1_m1g_t1_sc$term <- "second-order maj."
me2_r1_m1g_t1_sf <- margins_summary(r1_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(r1_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me2_r1_m1g_t1_sf$term <- "second-order min."
me2_r1_m1g <- rbind.fill(me2_r1_m1g_t1_sc, me2_r1_m1g_t1_sf)
me2_r1_m1g$term <- as.factor(me2_r1_m1g$term)
me2_r1_m1g$term = factor(me2_r1_m1g$term,levels(me2_r1_m1g$term)[c(2,1)])
me2_r1_m1g$model <- "additional control for avg. territorial autonomy"
me2_r1_m1g$estimate <- me2_r1_m1g$AME
me2_r1_m1g$conf.low <- me2_r1_m1g$lower
me2_r1_m1g$conf.high <- me2_r1_m1g$upper
##b) Controlling for average territorial autonomy, communal violence
me2_r1_m1d_t1_sb <- margins_summary(r1_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(r1_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1_m1d_t1_sb$term <- "all"
me2_r1_m1d_t1_sb$model <- "additional control for avg. territorial autonomy"
me2_r1_m2d_t1_sb <- margins_summary(r1_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1_m2d_t1_sb$term <- ifelse(me2_r1_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me2_r1_m2d_t1_sb$model <- "additional control for avg. territorial autonomy"
me2_r1_m13d <- rbind.fill(me2_r1_m1d_t1_sb, me2_r1_m2d_t1_sb)
me2_r1_m13d <- subset(me2_r1_m13d, term != "other")
me2_r1_m13d$term <- as.factor(me2_r1_m13d$term)
me2_r1_m13d$estimate <- me2_r1_m13d$AME
me2_r1_m13d$conf.low <- me2_r1_m13d$lower
me2_r1_m13d$conf.high <- me2_r1_m13d$upper
##c) excluding SO minorities with any type of de-facto autonomy, civil violence
me2_r1b_m1g_t1_sf <- margins_summary(r1b_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(r1b_m1g_t1_sf, as.integer(subset(main_group, autonomy == 0 & subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me2_r1b_m1g_t1_sf$term <- "second-order min."
me2_r1b_m1g_t1_sf$term <- as.factor(me2_r1b_m1g_t1_sf$term)
me2_r1b_m1g_t1_sf$model <- "excluding second-order minorities\nwith df. autonomy"
me2_r1b_m1g_t1_sf$estimate <- me2_r1b_m1g_t1_sf$AME
me2_r1b_m1g_t1_sf$conf.low <- me2_r1b_m1g_t1_sf$lower
me2_r1b_m1g_t1_sf$conf.high <- me2_r1b_m1g_t1_sf$upper
##d) excluding SO minorities with any type of de-facto autonomy, communal violence
me2_r1b_m1d_t1_sb <- margins_summary(r1b_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(r1b_m1d_t1_sb, as.integer(subset(main_dyad,  autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1b_m1d_t1_sb$term <- "all"
me2_r1b_m1d_t1_sb$model <- "excluding second-order minorities\nwith df. autonomy"
me2_r1b_m2d_t1_sb <- margins_summary(r1b_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1b_m2d_t1_sb, as.integer(subset(main_dyad,  autonomy_nd == 0 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1b_m2d_t1_sb$term <- ifelse(me2_r1b_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me2_r1b_m2d_t1_sb$model <- "excluding second-order minorities\nwith df. autonomy"
me2_r1b_m13d <- rbind.fill(me2_r1b_m1d_t1_sb, me2_r1b_m2d_t1_sb)
me2_r1b_m13d <- subset(me2_r1b_m13d, term != "other")
me2_r1b_m13d$term <- as.factor(me2_r1b_m13d$term)
me2_r1b_m13d$estimate <- me2_r1b_m13d$AME
me2_r1b_m13d$conf.low <- me2_r1b_m13d$lower
me2_r1b_m13d$conf.high <- me2_r1b_m13d$upper
##e) controlling for maximum territorial autonomy, civil violence
me2_r1a_m1g_t1_sc <- margins_summary(r1a_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(r1a_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me2_r1a_m1g_t1_sc$term <- "second-order maj."
me2_r1a_m1g_t1_sf <- margins_summary(r1a_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(r1a_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me2_r1a_m1g_t1_sf$term <- "second-order min."
me2_r1a_m1g <- rbind.fill(me2_r1a_m1g_t1_sc, me2_r1a_m1g_t1_sf)
me2_r1a_m1g$term <- as.factor(me2_r1a_m1g$term)
me2_r1a_m1g$term = factor(me2_r1a_m1g$term,levels(me2_r1a_m1g$term)[c(2,1)])
me2_r1a_m1g$model <- "additional control for max.\nterritorial autonomy"
me2_r1a_m1g$estimate <- me2_r1a_m1g$AME
me2_r1a_m1g$conf.low <- me2_r1a_m1g$lower
me2_r1a_m1g$conf.high <- me2_r1a_m1g$upper
##f) controlling for maximum territorial autonomy, communal violence
me2_r1a_m1d_t1_sb <- margins_summary(r1a_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(r1a_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1a_m1d_t1_sb$term <- "all"
me2_r1a_m1d_t1_sb$model <- "additional control for max.\nterritorial autonomy"
me2_r1a_m2d_t1_sb <- margins_summary(r1a_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(r1a_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me2_r1a_m2d_t1_sb$term <- ifelse(me2_r1a_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me2_r1a_m2d_t1_sb$model <- "additional control for max.\nterritorial autonomy"
me2_r1a_m13d <- rbind.fill(me2_r1a_m1d_t1_sb, me2_r1a_m2d_t1_sb)
me2_r1a_m13d <- subset(me2_r1a_m13d, term != "other")
me2_r1a_m13d$term <- as.factor(me2_r1a_m13d$term)
me2_r1a_m13d$estimate <- me2_r1a_m13d$AME
me2_r1a_m13d$conf.low <- me2_r1a_m13d$lower
me2_r1a_m13d$conf.high <- me2_r1a_m13d$upper
##g) civil violence, combined
figure3a_data_g <- rbind.fill(me2_r1_m1g, me2_r1b_m1g_t1_sf, me2_r1a_m1g)
figure3a_data_g$term = factor(figure3a_data_g$term,levels(figure3a_data_g$term)[c(2,1)])
figure3a_data_g$model <- as.factor(figure3a_data_g$model)
figure3a_g <- dwplot(figure3a_data_g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3")) + scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1)) + scale_shape_manual(name = "Coefficient for:", values = c(17,18,20)) + 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type") +
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks = c(-0.04,-0.02, 0))
##h) communal violence, combined
figure3a_data_d <- rbind.fill(me2_r1_m13d, me2_r1b_m13d, me2_r1a_m13d)
figure3a_data_d$model <- as.factor(figure3a_data_d$model)
figure3a_data_d$model = factor(figure3a_data_d$model,levels(figure3a_data_d$model)[c(2,3,1)])
figure3a_d <- dwplot(figure3a_data_d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) + ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3")) + scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1)) + scale_shape_manual(name = "Coefficient for:", values = c(17,18,20)) + 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks = c(0, 0.003, 0.006))
##i) combined
figurea3 <- grid_arrange_shared_legend(figure3a_g, figure3a_d, ncol=2, nrow=1)
ggsave(figurea3, file='../figures/figurea3.pdf', width = 14, height = 6.5, units="cm",dpi=1000)