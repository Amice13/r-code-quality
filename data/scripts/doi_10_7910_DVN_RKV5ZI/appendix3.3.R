####Model results (table X6, models A20-A21)####
r7_m1d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m1d_t1, "int_grp_rel_dominant*sa_territory_t", "int_grp_rel_dominant*included_excluded", "int_grp_rel_dominant*excluded_excluded", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1))
r7_m1d_t1_sb_cse <- data.frame(cluster.se(r7_m1d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1)$cowcode))[, 2])
r7_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c(m2d_t1, "int_grp_rel_dominant*sa_territory_t*included_excluded", "int_grp_rel_dominant*excluded_excluded", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1))
r7_m2d_t1_sb_cse <- data.frame(cluster.se(r7_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1)$cowcode))[, 2])
stargazer(r7_m1d_t1_sb, r7_m2d_t1_sb, se=c(r7_m1d_t1_sb_cse, r7_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence","Communal violence"), model.numbers =F, column.labels = c("Dyad (model A20)","Dyad (model A21)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X6. Additional results: nation-wide inter-group rivalries (including min./min. dyads and interacting territorial autonomy and included/excluded with maj./min.-dyad dummy).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Territorial autonomy x maj./min. dyad","Territorial autonomy x included/excluded x maj./min. dyad","Included/excluded","Included/excluded x maj./min. dyad","Excluded/excluded","Excluded/excluded x maj./min. dyad","Relative size (state, mean)","Relative size (state, diff.)","Maj./min. dyad","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex6.txt")
stargazer(r7_m1d_t1_sb, r7_m2d_t1_sb, se=c(r7_m1d_t1_sb_cse, r7_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Communal violence","Communal violence"), model.numbers =F, column.labels = c("Dyad (model A20)","Dyad (model A21)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X6. Additional results: nation-wide inter-group rivalries (including min./min. dyads and interacting territorial autonomy and included/excluded with maj./min.-dyad dummy).", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Territorial autonomy x maj./min. dyad","Territorial autonomy x included/excluded x maj./min. dyad","Included/excluded","Included/excluded x maj./min. dyad","Excluded/excluded","Excluded/excluded x maj./min. dyad","Relative size (state, mean)","Relative size (state, diff.)","Maj./min. dyad","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex6.html")

####Figure A6####
##a) maj./min. dyad
me_r7_m1d_t1_sb <- margins_summary(r7_m1d_t1_sb, variables = "sa_territory_t", at = list(int_grp_rel_dominant = 1), vcov = cluster.vcov(r7_m1d_t1_sb, as.integer(subset(main_dyad, subset_analysis == 1)$cowcode)))
me_r7_m1d_t1_sb$term <- "all"
me_r7_m1d_t1_sb$model <- "maj./min. dyads"
me_r7_m2d_t1_sb <- margins_summary(r7_m2d_t1_sb, variables = "sa_territory_t", at = list(int_grp_rel_dominant = 1, included_excluded = 1), vcov = cluster.vcov(r7_m2d_t1_sb, as.integer(subset(main_dyad, subset_analysis == 1)$cowcode)))
me_r7_m2d_t1_sb$term <- ifelse(me_r7_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_r7_m2d_t1_sb$model <- "maj./min. dyads"
me_r7_m13d <- rbind.fill(me_r7_m1d_t1_sb, me_r7_m2d_t1_sb)
me_r7_m13d <- subset(me_r7_m13d, term != "other")
me_r7_m13d$term <- as.factor(me_r7_m13d$term)
me_r7_m13d$estimate <- me_r7_m13d$AME
me_r7_m13d$conf.low <- me_r7_m13d$lower
me_r7_m13d$conf.high <- me_r7_m13d$upper
##b) results for min./min. dyad
me_r7b_m1d_t1_sb <- margins_summary(r7_m1d_t1_sb, variables = "sa_territory_t", at = list(int_grp_rel_dominant = 0), vcov = cluster.vcov(r7_m1d_t1_sb, as.integer(subset(main_dyad, subset_analysis == 1)$cowcode)))
me_r7b_m1d_t1_sb$term <- "all"
me_r7b_m1d_t1_sb$model <- "min./min. dyads"
me_r7b_m2d_t1_sb <- margins_summary(r7_m2d_t1_sb, variables = "sa_territory_t", at = list(int_grp_rel_dominant = 0, included_excluded = 1), vcov = cluster.vcov(r7_m2d_t1_sb, as.integer(subset(main_dyad, subset_analysis == 1)$cowcode)))
me_r7b_m2d_t1_sb$term <- ifelse(me_r7b_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_r7b_m2d_t1_sb$model <- "min./min. dyads"
me_r7b_m13d <- rbind.fill(me_r7b_m1d_t1_sb, me_r7b_m2d_t1_sb)
me_r7b_m13d <- subset(me_r7b_m13d, term != "other")
me_r7b_m13d$term <- as.factor(me_r7b_m13d$term)
me_r7b_m13d$estimate <- me_r7b_m13d$AME
me_r7b_m13d$conf.low <- me_r7b_m13d$lower
me_r7b_m13d$conf.high <- me_r7b_m13d$upper
##c) combined
figurea6_plot_data_d <- rbind.fill(me_r7_m13d, me_r7b_m13d)
figurea6_plot_data_d$model <- as.factor(figurea6_plot_data_d$model)
figurea6_plot_data_d$model = factor(figurea6_plot_data_d$model,levels(figurea6_plot_data_d$model)[c(2,3,1)])
figurea6 <- dwplot(figurea6_plot_data_d, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob. of communal violence") + ylab("dyad type")+
  guides(shape = guide_legend(nrow=1,"model",reverse=TRUE), colour = guide_legend(nrow=1,"model",reverse=TRUE),linetype = guide_legend(nrow=1,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
ggsave(figurea6, file='../figures/figurea6.pdf', width = 14, height = 4, units="cm",dpi=1000)