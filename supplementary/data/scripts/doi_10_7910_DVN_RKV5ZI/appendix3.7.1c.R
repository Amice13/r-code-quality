####Model results (table X14, models A42-A44)####
main_m1g_t1_s_all_g <- glm(as.formula(paste("cw_event_g", paste(c("sa_territory_t_avg","included", group_nat_vars, group_unit_vars_avg, nat_vars, unit_vars_avg2_g, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(country_group, subset_analysis == 1))
main_m1g_t1_s_all_g_cse <- data.frame(cluster.se(main_m1g_t1_s_all_g, as.factor(subset(country_group, subset_analysis == 1)$cowcode))[,2])
main_m1d_t1_sb_g <- glm(as.formula(paste("cv_event", paste(c("sa_territory_t_avg", "included_excluded", "excluded_excluded", dyad_nat_vars, dyad_unit_vars_avg, unit_vars_avg2_d, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(country_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
main_m1d_t1_sb_g_cse <- data.frame(cluster.se(main_m1d_t1_sb_g, as.factor(subset(country_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
main_m2d_t1_sb_g <- glm(as.formula(paste("cv_event", paste(c("sa_territory_t_avg*included_excluded", "excluded_excluded", dyad_nat_vars, dyad_unit_vars_avg, unit_vars_avg2_d, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(country_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
main_m2d_t1_sb_g_cse <- data.frame(cluster.se(main_m2d_t1_sb_g, as.factor(subset(country_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(main_m1g_t1_s_all_g, main_m1d_t1_sb_g, main_m2d_t1_sb_g, se=c(main_m1g_t1_s_all_g_cse, main_m1d_t1_sb_g_cse, main_m2d_t1_sb_g_cse), dep.var.labels.include = F, omit=c("cowcode|region|factor"), type="text", order=vars.order_g, title="Table X14. Additional results: group-/dyad-level specifications.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses;","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad.","Region- and year-fixed effects included but not reported."), model.numbers =F, column.labels = c("group (model A42)", "dyad (model A43)", "dyad (model A44)"), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Group population (mean, logged)","Group population (logged)","Relative size (unit, avg.)","Relative size (unit, avg., mean)","Relative size (unit, avg., diff.)", "Avg. ruggedness (unit, avg.)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Excluded/excluded", "Group area (logged, mean)", "Group area (logged)", "Oil in group area", "Distance capital (group, logged)", "Distance border (group, logged)", "Oil in group area (mean)", "Distance capital (group, mean, logged)", "Distance border (group, mean, logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3"), out="../tables/additional_results_tables_appendices/tablex14.txt")                 
stargazer(main_m1g_t1_s_all_g, main_m1d_t1_sb_g, main_m2d_t1_sb_g, se=c(main_m1g_t1_s_all_g_cse, main_m1d_t1_sb_g_cse, main_m2d_t1_sb_g_cse), dep.var.labels.include = F, omit=c("cowcode|region|factor"), type="html", order=vars.order_g, title="Table X14. Additional results: group-/dyad-level specifications.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses;","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad.","Region- and year-fixed effects included but not reported."), model.numbers =F, column.labels = c("group (model A42)", "dyad (model A43)", "dyad (model A44)"), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Included","Included/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Group population (mean, logged)","Group population (logged)","Relative size (unit, avg.)","Relative size (unit, avg., mean)","Relative size (unit, avg., diff.)", "Avg. ruggedness (unit, avg.)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Excluded/excluded", "Group area (logged, mean)", "Group area (logged)", "Oil in group area", "Distance capital (group, logged)", "Distance border (group, logged)", "Oil in group area (mean)", "Distance capital (group, mean, logged)", "Distance border (group, mean, logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3"), out="../tables/additional_results_tables_appendices/tablex14.html")                 

####Figure A12####
##a) civil
me_main_m1g_t1_s_all_g <- margins_summary(main_m1g_t1_s_all_g, variables = "sa_territory_t_avg", vcov = cluster.vcov(main_m1g_t1_s_all_g, as.integer(subset(country_group, subset_analysis == 1)$cowcode)))
me_main_m1g_t1_s_all_g$term <- "all"
me_main_m1g_t1_s_all_g$model <- "group-level specification"
me_main_m1g_t1_s_all_g$estimate <- me_main_m1g_t1_s_all_g$AME
me_main_m1g_t1_s_all_g$conf.low <- me_main_m1g_t1_s_all_g$lower
me_main_m1g_t1_s_all_g$conf.high <- me_main_m1g_t1_s_all_g$upper
groupspec_plot_g <- dwplot(me_main_m1g_t1_s_all_g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks =c(-0.02, -0.01, 0))
##b) communal
me_main_m1d_t1_sb_g <- margins_summary(main_m1d_t1_sb_g, variables = "sa_territory_t_avg", vcov = cluster.vcov(main_m1d_t1_sb_g, as.integer(subset(country_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m1d_t1_sb_g$term <- "all"
me_main_m1d_t1_sb_g$model <- "dyad-level specification"
me_main_m1d_t1_sb_g$estimate <- me_main_m1d_t1_sb_g$AME
me_main_m1d_t1_sb_g$conf.low <- me_main_m1d_t1_sb_g$lower
me_main_m1d_t1_sb_g$conf.high <- me_main_m1d_t1_sb_g$upper
me_main_m2d_t1_sb_g <- margins_summary(main_m2d_t1_sb_g, variables = "sa_territory_t_avg", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(main_m2d_t1_sb_g, as.integer(subset(country_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m2d_t1_sb_g$term <- ifelse(me_main_m2d_t1_sb_g$included_excluded == 1, "included/excluded", "other")
me_main_m2d_t1_sb_g$model <- "dyad-level specification"
me_main_m2d_t1_sb_g$estimate <- me_main_m2d_t1_sb_g$AME
me_main_m2d_t1_sb_g$conf.low <- me_main_m2d_t1_sb_g$lower
me_main_m2d_t1_sb_g$conf.high <- me_main_m2d_t1_sb_g$upper
me_main_m2d_t1_sb_g <- subset(me_main_m2d_t1_sb_g, term != "other")
me_main_m12d_t1_sb_g <- rbind.fill(me_main_m1d_t1_sb_g,me_main_m2d_t1_sb_g)
me_main_m12d_t1_sb_g$term <- as.factor(me_main_m12d_t1_sb_g$term)
dyadspec_plot_d <- dwplot(me_main_m12d_t1_sb_g,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type")+ 
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
###main plot combined
figurea12 <- grid.arrange(groupspec_plot_g, dyadspec_plot_d, ncol=2, nrow=1)
ggsave(figurea12, file='../figures/figurea12.pdf', width = 14, height = 3.5, units="cm",dpi=1000)