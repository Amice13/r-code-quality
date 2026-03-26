####Table A12####
negotiations_any2 <- glm(as.formula(paste("negotiations_any", paste(c("sa_territory_t","includedd_excludednd2","includednd_excludedd2","excluded_excluded_any", "lgdppc","lpop","politya","fractionalization1","election_year", "logintensity","ethnic_cv","prev_negotiations_any","as.factor(year)"), collapse = " + "), sep = " ~ ")), data=conf_pp, family=binomial(link="logit"),control = list(maxit = 50))
negotiations_any2_cse <- data.frame(cluster.se(negotiations_any2, conf_pp$cowcode)[,2])
intervention2 <- glm(as.formula(paste("intervention", paste(c("sa_territory_t","includedd_excludednd2","includednd_excludedd2","excluded_excluded_any", "lgdppc","lpop","politya","fractionalization1","election_year", "logintensity","ethnic_cv","prev_int","as.factor(year)"), collapse = " + "), sep = " ~ ")), data=conf_int, family=binomial(link="logit"),control = list(maxit = 50))
intervention2_cse <- data.frame(cluster.se(intervention2, conf_int$cowcode)[,2])
gov_mediation2 <- glm(as.formula(paste("gov_mediation", paste(c("sa_territory_t","includedd_excludednd2","includednd_excludedd2","excluded_excluded_any", "lgdppc","lpop","politya","fractionalization1","election_year", "logintensity","ethnic_cv","prev_gov_mediation","as.factor(year)"), collapse = " + "), sep = " ~ ")), data=conf_pp, family=binomial(link="logit"),control = list(maxit = 50))
gov_mediation2_cse <- data.frame(cluster.se(gov_mediation2, conf_pp$cowcode)[,2])
agreement2 <- glm(as.formula(paste("agreement", paste(c("sa_territory_t","includedd_excludednd2","includednd_excludedd2","excluded_excluded_any", "lgdppc","lpop","politya","fractionalization1","election_year", "logintensity","ethnic_cv","prev_agreement","as.factor(year)"), collapse = " + "), sep = " ~ ")), data=conf_pp, family=binomial(link="logit"),control = list(maxit = 50))
agreement2_cse <- data.frame(cluster.se(agreement2, conf_pp$cowcode)[,2])
stargazer(gov_mediation2,intervention2,negotiations_any2,agreement2, se=c(gov_mediation2_cse,intervention2_cse,negotiations_any2_cse,agreement2_cse), model.numbers =F, column.labels = c("Gov. mediation (model A167)","Gov. intervention (model A168)","Negotiations (model A169)","Agreement (model A170)"), dep.var.labels.include = F, omit=c("cowcode|region|factor"), type="text", order=vars.order_d, title="Table A12. Uneven government inclusion, central government responses to ongoing communal violence, and negotiated agreements.", style = "apsr", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses; year-fixed effects included but not reported.", "The dependent variable are binary variables equal to one if there is an instance of government mediation (model A169),","government intervention (model A170), negotiations between involved non-state actors (model A171),","or negotiated agreement between involved non-state actors (model A172) in the given non-state conflict year."), notes.append=FALSE, covariate.labels = c("Territorial autonomy","Included (maj.)/excluded (min.)","Included (min.)/excluded (maj.)","Excluded-excluded","GDP pc. (logged)", "Population (state, logged)","Democracy","Ethnic fractionalization","Election year","Intensity (logged)","Ethnic violence","Previous negotiations","Previous gov. mediation","Previous gov. intervention","Previous agreement"), out="../tables/tablea12.txt")

####Figure A25####
me_gov_mediation2 <- margins_summary(gov_mediation2, variables = c("includedd_excludednd2","includednd_excludedd2"), vcov = cluster.vcov(gov_mediation2, as.integer(conf_pp$cowcode)))
me_gov_mediation2$term <- "government mediation"
me_gov_mediation2$model <- ifelse(me_gov_mediation2$factor == "includedd_excludednd2", "included (maj.)/excluded (min.)", "included (min.)/excluded (maj.)")
me_intervention2 <- margins_summary(intervention2, variables = c("includedd_excludednd2","includednd_excludedd2"), vcov = cluster.vcov(intervention2, as.integer(conf_int$cowcode)))
me_intervention2$term <- "government intervention"
me_intervention2$model <- ifelse(me_intervention2$factor == "includedd_excludednd2", "included (maj.)/excluded (min.)", "included (min.)/excluded (maj.)")
me_negotiations_any2 <- margins_summary(negotiations_any2, variables = c("includedd_excludednd2","includednd_excludedd2"), vcov = cluster.vcov(negotiations_any2, as.integer(conf_pp$cowcode)))
me_negotiations_any2$term <- "negotiations"
me_negotiations_any2$model <- ifelse(me_negotiations_any2$factor == "includedd_excludednd2", "included (maj.)/excluded (min.)", "included (min.)/excluded (maj.)")
me_agreement2 <- margins_summary(agreement2, variables = c("includedd_excludednd2","includednd_excludedd2"), vcov = cluster.vcov(agreement2, as.integer(conf_pp$cowcode)))
me_agreement2$term <- "agreement"
me_agreement2$model <- ifelse(me_agreement2$factor == "includedd_excludednd2", "included (maj.)/excluded (min.)", "included (min.)/excluded (maj.)")
me_gov_response <- rbind.fill(me_gov_mediation2, me_intervention2, me_negotiations_any2, me_agreement2)
me_gov_response$estimate <- me_gov_response$AME
me_gov_response$conf.low <- me_gov_response$lower
me_gov_response$conf.high <- me_gov_response$upper
me_gov_response$model <- as.factor(me_gov_response$model)
me_gov_response$model= factor(me_gov_response$model,levels(me_gov_response$model)[c(2,1)])
me_gov_response <- me_gov_response[order(me_gov_response$model),]
figurea25 <- dwplot(me_gov_response,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(shape = factor(model), color = factor(model))), whisker_args = list(linetype = 1, aes(color = factor(model)))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("d) AME of included/excluded on government mediation,\nmilitary intervention, negotiations, and agreement") +
  scale_color_manual(name = "Coefficient for:", values = c("black","darkgrey"))+ 
  scale_shape_manual(name = "Coefficient for:", values = c(17,18,20))+ 
  theme(legend.spacing.y = unit(0.2, 'cm'), plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of outcome") + ylab("outcome") +
  scale_x_continuous(labels=scales::percent_format())+
  guides(shape = guide_legend(nrow=2,"dyad type",reverse=TRUE), colour = guide_legend(nrow=2,"dyad type",reverse=TRUE))
ggsave(figurea25 + theme(plot.title = element_blank()), file='../figures/figurea25.pdf', width = 14, height = 5.5, units="cm",dpi=1000)