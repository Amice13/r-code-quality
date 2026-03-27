####Model result (table X9, model A28)####
nuisance1_m1g_t1_sf <- glm(as.formula(paste("cw_event_g", paste(c("sa_territory_t", "included", "includedd_excludedg", "excludedd_includedg", group_nat_vars, group_unit_vars, unit_vars, nat_vars, "as.factor(region)","as.factor(year)","cw_event_g_peaceyears_l1","I(cw_event_g_peaceyears_l1^2)","I(cw_event_g_peaceyears_l1^3)","cw_event_any_slag_l1"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),control = list(maxit = 50), data=subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0))
nuisance1_m1g_t1_sf_cse <- data.frame(cluster.se(nuisance1_m1g_t1_sf, as.factor(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode))[,2])
stargazer(nuisance1_m1g_t1_sf, se=c(nuisance1_m1g_t1_sf_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)"), model.numbers =F, column.labels = c("Min. (model A28)"), omit=c("cowcode|region|factor"), type="text", order=vars.order_gd, title="Table X9. Additional results: impact of uneven government inclusion on civil violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Included","included (maj.)/excluded (min.)","included (min.)/excluded (maj.)","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex9.txt")
stargazer(nuisance1_m1g_t1_sf, se=c(nuisance1_m1g_t1_sf_cse), dep.var.labels.include = T, dep.var.labels = c("Civil violence (group)"), model.numbers =F, column.labels = c("Min. (model A28)"), omit=c("cowcode|region|factor"), type="html", order=vars.order_gd, title="Table X9. Additional results: impact of uneven government inclusion on civil violence.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a binary variable equal to one if there is at least one instance of civil/communal violence involving a group/dyad in a given unit.","Region- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Included","included (maj.)/excluded (min.)","included (min.)/excluded (maj.)","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Civil violence peace years","Civil violence peace years 2","Civil violence peace years 3","Civil violence spatial lag"), out="../tables/additional_results_tables_appendices/tablex9.html")

####Figure A8####
me_nuisance1_m1g_t1_sf1 <- margins_summary(nuisance1_m1g_t1_sf, variables = "includedd_excludedg", vcov = cluster.vcov(nuisance1_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_nuisance1_m1g_t1_sf1$term <- "maj. incl./min. excl."
me_nuisance1_m1g_t1_sf2 <- margins_summary(nuisance1_m1g_t1_sf, variables = "excludedd_includedg", vcov = cluster.vcov(nuisance1_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_nuisance1_m1g_t1_sf2$term <- "min. incl./maj. excl."
me_lm_nuisance_m1g <- rbind.fill(me_nuisance1_m1g_t1_sf1, me_nuisance1_m1g_t1_sf2)
me_lm_nuisance_m1g$term <- as.factor(me_lm_nuisance_m1g$term)
me_lm_nuisance_m1g$term = factor(me_lm_nuisance_m1g$term,levels(me_lm_nuisance_m1g$term)[c(2,1)])
me_lm_nuisance_m1g$model <- 1
me_lm_nuisance_m1g$estimate <- me_lm_nuisance_m1g$AME
me_lm_nuisance_m1g$conf.low <- me_lm_nuisance_m1g$lower
me_lm_nuisance_m1g$conf.high <- me_lm_nuisance_m1g$upper
figurea8 <- dwplot(me_lm_nuisance_m1g,
                   vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                   dot_args = list(shape = 16, colour = "black"), 
                   whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ 
  scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1))+ 
  scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob. of civil violence") + ylab("group type")+ #+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE),linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
ggsave(figurea8, file='../figures/figurea8.pdf', width = 14, height = 4.5, units="cm",dpi=1000)