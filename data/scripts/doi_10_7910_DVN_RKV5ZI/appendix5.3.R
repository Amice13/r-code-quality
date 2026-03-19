####Table A11####
##a) subset to usable sample
#criteria: 1. both SO majority and SO minority in sample; 2. information on territorial autonomy; 3. information on grievances (excluding don't knows); 4. ISSP-N, Afrobarometer, and New Europe Barometer measures not for national-level majorities
main_individual <- data.frame(subset(main_individual, dominant_fips_yn == 1 & nondominant_fips_yn == 1 & !is.na(sa_territory_t) & !is.na(disc_grp) & equal_grp != 0 & !(survey == "ISSP_N" & state_control == 1) & !(survey == "AsianB" & state_control == 1) & !(survey == "NEWEB" & state_control == 1)))
##b) models
m1g_t1_disc_grp_sb <- glmer(as.formula(paste("disc_grp", paste(c("1", "(1|cowcode)", "(1|cowcode_year)", "(1|gwgroupid)", "(1|gwgroupid_year)", "(1|fips)", "survey","sa_territory_t","int_grp_rel_dominant_g","included",group_nat_vars,group_unit_vars, unit_vars, nat_vars), collapse = " + "), sep = " ~ ")), data=main_individual, family=binomial(link="logit"),nAGQ = 0)
m2g_t1_disc_grp_sb <- glmer(as.formula(paste("disc_grp", paste(c("1", "(1|cowcode)", "(1|cowcode_year)", "(1|gwgroupid)", "(1|gwgroupid_year)", "(1|fips)", "survey","sa_territory_t*int_grp_rel_dominant_g","included",group_nat_vars,group_unit_vars, unit_vars, nat_vars), collapse = " + "), sep = " ~ ")), data=main_individual, family=binomial(link="logit"),nAGQ = 0)
stargazer(m1g_t1_disc_grp_sb, m2g_t1_disc_grp_sb, dep.var.labels.include = F, model.numbers =F, column.labels = c("model A165","model A166"), title="Table A11. Territorial autonomy and grievances of second-order majorities and second-order minorities.", omit=c("cowcode|region|year"), notes.append = FALSE, type="text", order=vars.order_g, style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; random intercepts included at levels of: country, group, country-year, and group-year.", "The dependent variable is a binary variable equal to one if there is a respondent stated they belong to a group that is discriminated against or treated unfairly, and 0 otherwise."), covariate.labels = c("Territorial autonomy","Second-order majority x territorial autonomy","Second-order majority","Included","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Asian Barometer","European Social Survey","ISSP-N","Latinobarometro","New Baltic Barometer"), out="../tables/tablea11.txt")
stargazer(m1g_t1_disc_grp_sb, m2g_t1_disc_grp_sb, dep.var.labels.include = F, model.numbers =F, column.labels = c("model A165","model A166"), title="Table A11. Territorial autonomy and grievances of second-order majorities and second-order minorities.", omit=c("cowcode|region|year"), notes.append = FALSE, type="html", order=vars.order_g, style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; random intercepts included at levels of: country, group, country-year, and group-year.", "The dependent variable is a binary variable equal to one if there is a respondent stated they belong to a group that is discriminated against or treated unfairly, and 0 otherwise."), covariate.labels = c("Territorial autonomy","Second-order majority x territorial autonomy","Second-order majority","Included","Relative size (state)","Relative size (unit)", "Population (unit, logged)", "Area (unit, logged)", "Avg. ruggedness (unit)", "Oil in unit", "Distance capital (unit, logged)", "Distance border (unit, logged)","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Asian Barometer","European Social Survey","ISSP-N","Latinobarometro","New Baltic Barometer"), out="../tables/tablea11.html")

####Figure 6c####
#note: figure 6 is the combination of figure 6, panels a, b, and c
me_m1g_t1_disc_grp_sb <- margins_summary(m1g_t1_disc_grp_sb, variables = "sa_territory_t", data=main_individual)
me_m1g_t1_disc_grp_sb$term <- "all"
me_m1g_t1_disc_grp_sb$model <- "feeling discriminated"
me_m2g_t1_disc_grp_sb <- margins_summary(m2g_t1_disc_grp_sb, variables = "sa_territory_t", at = list(int_grp_rel_dominant_g = c(0,1)), data=main_individual)
me_m2g_t1_disc_grp_sb$term <- ifelse(me_m2g_t1_disc_grp_sb$int_grp_rel_dominant_g == 1, "maj.", "min.")
me_m2g_t1_disc_grp_sb$model <- "feeling discriminated"
me_m12g_t1_disc_grp_sb <- rbind.fill(me_m1g_t1_disc_grp_sb, me_m2g_t1_disc_grp_sb)
me_m12g_t1_disc_grp_sb$estimate <- me_m12g_t1_disc_grp_sb$AME
me_m12g_t1_disc_grp_sb$conf.low <- me_m12g_t1_disc_grp_sb$lower
me_m12g_t1_disc_grp_sb$conf.high <- me_m12g_t1_disc_grp_sb$upper
me_m12g_t1_disc_grp_sb <- subset(me_m12g_t1_disc_grp_sb, term != "all")
me_m12g_t1_disc_grp_sb$term <- as.factor(me_m12g_t1_disc_grp_sb$term)
me_m12g_t1_disc_grp_sb$term = factor(me_m12g_t1_disc_grp_sb$term,levels(me_m12g_t1_disc_grp_sb$term)[c(2,1)])
me_m12g_t1_disc_grp_sb <- me_m12g_t1_disc_grp_sb%>% arrange(desc(term))
figure6c <- dwplot(me_m12g_t1_disc_grp_sb,
                   vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                   dot_args = list(shape = 16, colour = "black"), 
                   whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("c) AME of territorial autonomy on grievances") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of feeling discriminated") + ylab("group type") +
  scale_x_continuous(labels=scales::percent_format())
ggsave(figure6c, file='../figures/figure6c.pdf', width = 14, height = 3.5, units="cm",dpi=1000)