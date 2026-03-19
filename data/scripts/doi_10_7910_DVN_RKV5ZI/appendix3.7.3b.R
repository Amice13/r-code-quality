####Model results (table X28, models A96-A98)####
r3h_m1g_t1_sc <- glm(as.formula(paste("cw_event_g", paste(c("sa_territory_t * lage_both","included", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1))
r3h_m1g_t1_sc_cse <- data.frame(cluster.se(r3h_m1g_t1_sc, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode))[,2])
r3h_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c("sa_territory_t * lage_both","included", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
r3h_m1g_t1_sf_cse <- data.frame(cluster.se(r3h_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
r3h_m2d_t1_sb <- glm(as.formula(paste("cv_event", paste(c("sa_territory_t*included_excluded","excluded_excluded","sa_territory_t*lage_both", dyad_nat_vars, dyad_unit_vars, unit_vars, nat_vars,"as.factor(region)","as.factor(year)","cv_event_peaceyears_l1","I(cv_event_peaceyears_l1^2)","I(cv_event_peaceyears_l1^3)","cv_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1))
r3h_m2d_t1_sb_cse <- data.frame(cluster.se(r3h_m2d_t1_sb, as.factor(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode))[, 2])
stargazer(r3h_m1g_t1_sc, r3h_m1g_t1_sf, r3h_m2d_t1_sb, se=c(r3h_m1g_t1_sc_cse, r3h_m1g_t1_sf_cse, r3h_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A96)", "Min. (model A97)","Maj./min. dyad (model A98)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X28. Additional results: dynamic changes in territorial autonomy arrangements.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Territorial autonomy x age of autonomy arrangement (logged)","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Age of autonomy arrangement (logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex28.txt")
stargazer(r3h_m1g_t1_sc, r3h_m1g_t1_sf, r3h_m2d_t1_sb, se=c(r3h_m1g_t1_sc_cse, r3h_m1g_t1_sf_cse, r3h_m2d_t1_sb_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)","Communal violence"), model.numbers =F, column.labels = c("Maj. (model A96)", "Min. (model A97)","Maj./min. dyad (model A98)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X28. Additional results: dynamic changes in territorial autonomy arrangements.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x included/excluded","Territorial autonomy x age of autonomy arrangement (logged)","Included","Included/excluded","Excluded/excluded","Relative size (state)","Relative size (state, mean)","Relative size (state, diff.)","Relative size (unit)","Relative size (unit, mean)","Relative size (unit, diff.)","Asymmetry", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Age of autonomy arrangement (logged)","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag", "Communal violence peace years", "Communal violence peace years 2", "Communal violence peace years 3","Communal violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex28.html")

####Figure A16####
##a) civil (maj.)
age_unit <- seq(0, 50, by=1)
lage_unit <- log(age_unit+1)
me_r3h_m1g_t1_sc <- margins_summary(r3h_m1g_t1_sc, variables = "sa_territory_t", at = list(lage_both = c(lage_unit)), vcov = cluster.vcov(r3h_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_r3h_m1g_t1_sc$ageboth <- exp(me_r3h_m1g_t1_sc$lage_both) - 1
me_r3h_m1g_t1_sc_plot <- ggplot(data = me_r3h_m1g_t1_sc) + geom_line(aes(x=ageboth, y=AME)) + 
  geom_ribbon(aes(x=ageboth, ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_histogram(data = subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1), aes(x = age_both, y = ..count.. / 150000), position="identity", linetype=1,
                 fill="black", alpha=0.5, binwidth = 1) +
  scale_y_continuous(name ='change in predicted prob.\n of civil violence',labels = percent, sec.axis = sec_axis(~.*150000,name="#observations", breaks=c(0,500,1000,1500))) +
  theme_bw() + theme(text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=6)) +
  xlab('age of administrative unit') + ggtitle("a) civil violence\n(second-order maj.)\n ") +
  coord_cartesian(xlim = c(0,50))
##b) civil (min.)
me_r3h_m1g_t1_sf <- margins_summary(r3h_m1g_t1_sf, variables = "sa_territory_t", at = list(lage_both = c(lage_unit)), vcov = cluster.vcov(r3h_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_r3h_m1g_t1_sf$ageboth <- exp(me_r3h_m1g_t1_sf$lage_both) - 1
me_r3h_m1g_t1_sf_plot <- ggplot(data = me_r3h_m1g_t1_sf) + geom_line(aes(x=ageboth, y=AME)) + 
  geom_ribbon(aes(x=ageboth, ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_histogram(data = subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1), aes(x = age_both, y = ..count.. / 500000), position="identity", linetype=1,
                 fill="black", alpha=0.5, binwidth = 1) +
  scale_y_continuous(name ='change in predicted prob.\n of civil violence',labels = percent, sec.axis = sec_axis(~.*500000,name="#observations", breaks=c(0,500,1000,1500))) +
  theme_bw() + theme(text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=6)) +
  xlab('age of administrative unit') + ggtitle("b) civil violence,\n(second-order min.)\n ") +
  coord_cartesian(xlim = c(0,50))
##c) communal (maj./min.)
me_r3h_m2d_t1_sb_incexc1 <- margins_summary(r3h_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = 1, lage_both = c(lage_unit)), vcov = cluster.vcov(r3h_m2d_t1_sb, as.integer(subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_r3h_m2d_t1_sb_incexc1$ageboth <- exp(me_r3h_m2d_t1_sb_incexc1$lage_both) - 1
me_r3h_m2d_t1_sb_incexc1_plot <- ggplot(data = me_r3h_m2d_t1_sb_incexc1) + geom_line(aes(x=ageboth, y=AME)) + 
  geom_ribbon(aes(x=ageboth, ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_histogram(data = subset(main_dyad, subset_analysis == 1 & int_grp_rel_dominant == 1), aes(x = age_both, y = ..count.. / 2000000), position="identity", linetype=1,
                 fill="black", alpha=0.5, binwidth = 1) +
  scale_y_continuous(name ='change in predicted prob.\n of civil violence',labels = percent, sec.axis = sec_axis(~.*2000000,name="#observations", breaks=c(0,5000,10000,15000))) +
  theme_bw() + theme(text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=6)) +
  xlab('age of administrative unit') + ggtitle("c) communal vio-\nlence (min.-maj.,\nincl./excl.)") +
  coord_cartesian(xlim = c(0,50))
##e) combined
figurea16 <- grid.arrange(me_r3h_m1g_t1_sc_plot,me_r3h_m1g_t1_sf_plot,me_r3h_m2d_t1_sb_incexc1_plot,nrow=1,ncol=3)
ggsave(figurea16, file='../figures/figurea16.pdf', width = 14, height = 6, units="cm",dpi=1000)