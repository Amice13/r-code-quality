####Model results####
##a) territorial/governmental civil war (table X37, models A131-A134)
i1a_m1g_t1_sc <- glm(as.formula(paste("cw_territory_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_territory_event_g_peaceyears_l1","I(cw_territory_event_g_peaceyears_l1^2)","I(cw_territory_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
i1a_m1g_t1_sc_cse <- data.frame(cluster.se(i1a_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
i1a_m1g_t1_sf <- glm(as.formula(paste("cw_territory_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_territory_event_g_peaceyears_l1","I(cw_territory_event_g_peaceyears_l1^2)","I(cw_territory_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
i1a_m1g_t1_sf_cse <- data.frame(cluster.se(i1a_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
i1b_m1g_t1_sc <- glm(as.formula(paste("cw_gov_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_gov_event_g_peaceyears_l1","I(cw_gov_event_g_peaceyears_l1^2)","I(cw_gov_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
i1b_m1g_t1_sc_cse <- data.frame(cluster.se(i1b_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
i1b_m1g_t1_sf <- glm(as.formula(paste("cw_gov_event_g", paste(c(m1g_t1, group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_gov_event_g_peaceyears_l1","I(cw_gov_event_g_peaceyears_l1^2)","I(cw_gov_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
i1b_m1g_t1_sf_cse <- data.frame(cluster.se(i1b_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
stargazer(i1a_m1g_t1_sc, i1a_m1g_t1_sf, i1b_m1g_t1_sc, i1b_m1g_t1_sf, se=c(i1a_m1g_t1_sc_cse, i1a_m1g_t1_sf_cse, i1b_m1g_t1_sc_cse, i1b_m1g_t1_sf_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group, territorial)","Civil violence (group, governmental)"), model.numbers =F, column.labels = c("Maj. (model A131)", "Min. (model A132)","Maj. (model A133)", "Min. (model A134)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X37. Additional results: disaggregated dependent variables (territorial vs. governmental civil violence).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil violence of the respective type involving a group in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Included","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Terr. civil violence peace years","Terr. civil violence peace years 2","Terr. civil violence peace years 3","Gov. civil violence peace years","Gov. civil violence peace years 2","Gov. civil violence peace years 3","Civil violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex37.txt")
stargazer(i1a_m1g_t1_sc, i1a_m1g_t1_sf, i1b_m1g_t1_sc, i1b_m1g_t1_sf, se=c(i1a_m1g_t1_sc_cse, i1a_m1g_t1_sf_cse, i1b_m1g_t1_sc_cse, i1b_m1g_t1_sf_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group, territorial)","Civil violence (group, governmental)"), model.numbers =F, column.labels = c("Maj. (model A131)", "Min. (model A132)","Maj. (model A133)", "Min. (model A134)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X37. Additional results: disaggregated dependent variables (territorial vs. governmental civil violence).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil violence of the respective type involving a group in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Included","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Terr. civil violence peace years","Terr. civil violence peace years 2","Terr. civil violence peace years 3","Gov. civil violence peace years","Gov. civil violence peace years 2","Gov. civil violence peace years 3","Civil violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex37.html")
##b) territorial/authority/SDM CV (table X38, models A135-A140)
i4b_m1d_t1_sb <- glm(as.formula(paste("territory_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","territory_cv_event_peaceyears_l1","I(territory_cv_event_peaceyears_l1^2)","I(territory_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i4b_m1d_t1_sb_cse <- data.frame(cluster.se(i4b_m1d_t1_sb, as.factor(subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
i4b_m2d_t1_sb <- glm(as.formula(paste("territory_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","territory_cv_event_peaceyears_l1","I(territory_cv_event_peaceyears_l1^2)","I(territory_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i4b_m2d_t1_sb_cse <- data.frame(cluster.se(i4b_m2d_t1_sb, as.factor(subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
i4c_m1d_t1_sb <- glm(as.formula(paste("aut_cv_event", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","aut_cv_event_peaceyears_l1","I(aut_cv_event_peaceyears_l1^2)","I(aut_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i4c_m1d_t1_sb_cse <- data.frame(cluster.se(i4c_m1d_t1_sb, as.factor(subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
i4c_m2d_t1_sb <- glm(as.formula(paste("aut_cv_event", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","aut_cv_event_peaceyears_l1","I(aut_cv_event_peaceyears_l1^2)","I(aut_cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i4c_m2d_t1_sb_cse <- data.frame(cluster.se(i4c_m2d_t1_sb, as.factor(subset(main_dyad, issues_sample == 1 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
i5_m1d_t1_sb <- glm(as.formula(paste("I(sdm_any_either * cv_event)", paste(c(m1d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i5_m1d_t1_sb_cse <- data.frame(cluster.se(i5_m1d_t1_sb, as.factor(subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
i5_m2d_t1_sb <- glm(as.formula(paste("I(sdm_any_either * cv_event)", paste(c(m2d_t1, dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1))
i5_m2d_t1_sb_cse <- data.frame(cluster.se(i5_m2d_t1_sb, as.factor(subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(i4b_m1d_t1_sb, i4b_m2d_t1_sb, i4c_m1d_t1_sb, i4c_m2d_t1_sb, i5_m1d_t1_sb, i5_m2d_t1_sb, se=c(i4b_m1d_t1_sb_cse, i4b_m2d_t1_sb_cse, i4c_m1d_t1_sb_cse, i4c_m2d_t1_sb_cse, i5_m1d_t1_sb_cse, i5_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence (territorial)","Communal violence (territorial)","Communal violence (authority)","Communal violence (authority)","Communal violence (SDM)","Communal violence (SDM)"), model.numbers =F, column.labels = c("Maj./min. dyad (model A135)","Maj./min. dyad (model A136)","Maj./min. dyad (model A137)","Maj./min. dyad (model A138)","Maj./min. dyad (model A139)","Maj./min. dyad (model A140)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X38. Additional results: disaggregated dependent variables (territorial vs. authority vs. SDM-related communal violence).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence of the respective type involving a dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included/excluded","Excluded/excluded","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Terr. communal violence peace years", "Terr. communal violence peace years 2", "Terr. communal violence peace years 3","Aut. communal violence peace years", "Aut. communal violence peace years 2", "Aut. communal violence peace years 3","Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex38.txt")
stargazer(i4b_m1d_t1_sb, i4b_m2d_t1_sb, i4c_m1d_t1_sb, i4c_m2d_t1_sb, i5_m1d_t1_sb, i5_m2d_t1_sb, se=c(i4b_m1d_t1_sb_cse, i4b_m2d_t1_sb_cse, i4c_m1d_t1_sb_cse, i4c_m2d_t1_sb_cse, i5_m1d_t1_sb_cse, i5_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence (territorial)","Communal violence (territorial)","Communal violence (authority)","Communal violence (authority)","Communal violence (SDM)","Communal violence (SDM)"), model.numbers =F, column.labels = c("Maj./min. dyad (model A135)","Maj./min. dyad (model A136)","Maj./min. dyad (model A137)","Maj./min. dyad (model A138)","Maj./min. dyad (model A139)","Maj./min. dyad (model A140)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X38. Additional results: disaggregated dependent variables (territorial vs. authority vs. SDM-related communal violence).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence of the respective type involving a dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included/excluded","Excluded/excluded","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Terr. communal violence peace years", "Terr. communal violence peace years 2", "Terr. communal violence peace years 3","Aut. communal violence peace years", "Aut. communal violence peace years 2", "Aut. communal violence peace years 3","Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex38.html")

####Figure A20####
##a) territorial civil war
me_i1a_m1g_t1_sc <- margins_summary(i1a_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(i1a_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_i1a_m1g_t1_sc$term <- "second-order maj."
me_i1a_m1g_t1_sf <- margins_summary(i1a_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(i1a_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_i1a_m1g_t1_sf$term <- "second-order min."
me_i1a_m1g <- rbind.fill(me_i1a_m1g_t1_sc, me_i1a_m1g_t1_sf)
me_i1a_m1g$term <- as.factor(me_i1a_m1g$term)
me_i1a_m1g$model <- "territorial conflict"
me_i1a_m1g$estimate <- me_i1a_m1g$AME
me_i1a_m1g$conf.low <- me_i1a_m1g$lower
me_i1a_m1g$conf.high <- me_i1a_m1g$upper
##b) governmental civil war
me_i1b_m1g_t1_sc <- margins_summary(i1b_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(i1b_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_i1b_m1g_t1_sc$term <- "second-order maj."
me_i1b_m1g_t1_sf <- margins_summary(i1b_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(i1b_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_i1b_m1g_t1_sf$term <- "second-order min."
me_i1b_m1g <- rbind.fill(me_i1b_m1g_t1_sc, me_i1b_m1g_t1_sf)
me_i1b_m1g$term <- as.factor(me_i1b_m1g$term)
me_i1b_m1g$term = factor(me_i1b_m1g$term,levels(me_i1b_m1g$term)[c(2,1)])
me_i1b_m1g$model <- "governmental/authority conflict"
me_i1b_m1g$estimate <- me_i1b_m1g$AME
me_i1b_m1g$conf.low <- me_i1b_m1g$lower
me_i1b_m1g$conf.high <- me_i1b_m1g$upper
##c) communal violence (territorial)
me_i4b_m1d_t1_sb <- margins_summary(i4b_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(i4b_m1d_t1_sb, as.integer(subset(main_dyad, issues_sample == 1 & subset_analysis == 1  & int_grp_rel_dominant == 1)$cowcode)))
me_i4b_m1d_t1_sb$term <- "all"
me_i4b_m1d_t1_sb$model <- "territorial conflict"
me_i4b_m2d_t1_sb <- margins_summary(i4b_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(i4b_m2d_t1_sb, as.integer(subset(main_dyad, issues_sample == 1 & subset_analysis == 1  & int_grp_rel_dominant == 1)$cowcode)))
me_i4b_m2d_t1_sb$term <- ifelse(me_i4b_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_i4b_m2d_t1_sb$model <- "territorial conflict"
me_i4b_m13d <- rbind.fill(me_i4b_m1d_t1_sb, me_i4b_m2d_t1_sb)
me_i4b_m13d <- subset(me_i4b_m13d, term != "other")
me_i4b_m13d$term <- as.factor(me_i4b_m13d$term)
me_i4b_m13d$estimate <- me_i4b_m13d$AME
me_i4b_m13d$conf.low <- me_i4b_m13d$lower
me_i4b_m13d$conf.high <- me_i4b_m13d$upper
##d) communal violence (authority)
me_i4c_m1d_t1_sb <- margins_summary(i4c_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(i4c_m1d_t1_sb, as.integer(subset(main_dyad, issues_sample == 1 & subset_analysis == 1  & int_grp_rel_dominant == 1)$cowcode)))
me_i4c_m1d_t1_sb$term <- "all"
me_i4c_m1d_t1_sb$model <- "governmental/authority conflict"
me_i4c_m2d_t1_sb <- margins_summary(i4c_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(i4c_m2d_t1_sb, as.integer(subset(main_dyad, issues_sample == 1 & subset_analysis == 1  & int_grp_rel_dominant == 1)$cowcode)))
me_i4c_m2d_t1_sb$term <- ifelse(me_i4c_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_i4c_m2d_t1_sb$model <- "governmental/authority conflict"
me_i4c_m13d <- rbind.fill(me_i4c_m1d_t1_sb, me_i4c_m2d_t1_sb)
me_i4c_m13d <- subset(me_i4c_m13d, term != "other")
me_i4c_m13d$term <- as.factor(me_i4c_m13d$term)
me_i4c_m13d$estimate <- me_i4c_m13d$AME
me_i4c_m13d$conf.low <- me_i4c_m13d$lower
me_i4c_m13d$conf.high <- me_i4c_m13d$upper
##e) communal violence (SDM)
me_i5_m1d_t1_sb <- margins_summary(i5_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(i5_m1d_t1_sb, as.integer(subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_i5_m1d_t1_sb$term <- "all"
me_i5_m1d_t1_sb$model <- "self-determination conflict"
me_i5_m2d_t1_sb <- margins_summary(i5_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(i5_m2d_t1_sb, as.integer(subset(main_dyad, year <= 2017 & subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_i5_m2d_t1_sb$term <- ifelse(me_i5_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_i5_m2d_t1_sb$model <- "self-determination conflict"
me_i5_m13d <- rbind.fill(me_i5_m1d_t1_sb, me_i5_m2d_t1_sb)
me_i5_m13d <- subset(me_i5_m13d, term != "other")
me_i5_m13d$term <- as.factor(me_i5_m13d$term)
me_i5_m13d$estimate <- me_i5_m13d$AME
me_i5_m13d$conf.low <- me_i5_m13d$lower
me_i5_m13d$conf.high <- me_i5_m13d$upper
##f) combined (civil)
figurea20_plot_data_g <- rbind.fill(me_i1a_m1g,me_i1b_m1g)
figurea20_plot_data_g$model <- factor(figurea20_plot_data_g$model, levels=c("territorial conflict","governmental/authority conflict","self-determination conflict"))
figurea20_plot_g <- dwplot(figurea20_plot_data_g,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##g) combined (communal)
figurea20_plot_data_d <- rbind.fill(me_i4b_m13d,me_i4c_m13d,me_i5_m13d)
figurea20_plot_d <- dwplot(figurea20_plot_data_d,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks=c(0, 0.0075, 0.015))
##h) combined
mylegend<-g_legend(figurea20_plot_d)
figurea20 <- grid.arrange(arrangeGrob(figurea20_plot_g + theme(legend.position="none"),figurea20_plot_d + theme(legend.position="none"),nrow=1),mylegend, nrow=2,heights=c(5, 1))
ggsave(figurea20, file='../figures/figurea20.pdf', width = 14, height = 6.5, units="cm",dpi=1000)