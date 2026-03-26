####Table A8####
main_group_nl <- subset(main_group, int_grp_rel > 0 & geo_conc == 1 & igrd_geo_conc == 1 & year >= 1992 & !is.na(adm_grp_nl))
main_group_nl <- main_group_nl %>% group_by(cowcode,fips,year) %>% mutate(adm_grp_nl_avg_ay = mean(adm_grp_nl,na.rm=T))
nl_model1 <- lm(as.formula(paste("I(adm_grp_nl-adm_grp_nl_avg_ay)", paste(c("sa_territory_t * int_grp_rel_dominant_g","included", group_unit_vars, unit_vars, "lgdppc","lpop","politya","election_year", "as.factor(gwgroupid)","as.factor(year)"), collapse = " + "), sep = " ~ ")), data=main_group_nl)
nl_model1_cse <- data.frame(cluster.se(nl_model1, as.factor(main_group_nl$cowcode))[,2])
stargazer(nl_model1, se=c(nl_model1_cse), dep.var.labels.include = F, single.row = T, omit=c("cowcode|region|factor|years|fips"), type="text", order=vars.order_g, model.numbers =F, column.labels = c("model A161"), title="Table A8. Territorial autonomy and stable nightlight emissions.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE’s in parentheses; maj. = second-order majority; min. = second-order minority.","The dependent variable is a continuous variable, measuring the stable nightlight emissions in a group's settlement area within a given administrative unit,", "normalized by subtracting the average nightlight emissions across all groups in a given administrative unit year.","Group- and year-fixed effects included but not reported."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Territorial autonomy x second-order majority","Included","Second-order majority","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Election year"), out="../tables/tablea8.txt")                   

####Figure 6, panel a
#note: figure 6 is the combination of figure 6, panels a, b, and c
me_nl_model1 <- margins_summary(nl_model1, variables = c("sa_territory_t"), at =list(int_grp_rel_dominant_g =c(0,1)), vcov = cluster.vcov(nl_model1, as.integer(main_group_nl$cowcode)))
me_nl_model1$term <- ifelse(me_nl_model1$int_grp_rel_dominant_g == 1, "maj.", "min.")
me_nl_model1$model <- 2
me_nl_model1$term <- as.factor(me_nl_model1$term)
me_nl_model1$estimate <- me_nl_model1$AME
me_nl_model1$conf.low <- me_nl_model1$lower
me_nl_model1$conf.high <- me_nl_model1$upper
me_nl_model1$term <- as.factor(me_nl_model1$term)
me_nl_model1$term = factor(me_nl_model1$term,levels(me_nl_model1$term)[c(2,1)])
me_nl_model1 <- me_nl_model1%>% arrange(desc(term))
figure6a <- dwplot(me_nl_model1, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) AME of territorial autonomy on stable nightlights in group settlement areas") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted stable nightlights") + ylab("group type") +
  guides(shape = guide_legend(nrow=1,"outcome",reverse=TRUE), colour = guide_legend(nrow=1,"outcome",reverse=TRUE))
ggsave(figure6a, file='../figures/figure6a.pdf', width = 14, height = 4, units="cm",dpi=1000)