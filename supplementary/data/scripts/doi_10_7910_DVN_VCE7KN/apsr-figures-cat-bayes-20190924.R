

#Coefficient plots for categorical bayesian models

# Troops model
coef.1.catbayes <- `m1.cat.bayes` %>% 
  gather_draws(b_mupos_contact_persDontknowDDeclinetoanswer,
               b_mupos_contact_persYes,
               b_mupos_contact_nonpersDontknowDDeclinetoanswer,
               b_mupos_contact_nonpersYes,
               b_mupos_benefit_persDontknowDDeclinetoanswer,
               b_mupos_benefit_persYes,
               b_mupos_benefit_nonpersDontknowDDeclinetoanswer,
               b_mupos_benefit_nonpersYes,
               b_muneg_contact_persDontknowDDeclinetoanswer,
               b_muneg_contact_persYes,
               b_muneg_contact_nonpersDontknowDDeclinetoanswer,
               b_muneg_contact_nonpersYes,
               b_muneg_benefit_persDontknowDDeclinetoanswer,
               b_muneg_benefit_persYes,
               b_muneg_benefit_nonpersDontknowDDeclinetoanswer,
               b_muneg_benefit_nonpersYes,
               b_mudk_contact_persDontknowDDeclinetoanswer,
               b_mudk_contact_persYes,
               b_mudk_contact_nonpersDontknowDDeclinetoanswer,
               b_mudk_contact_nonpersYes,
               b_mudk_benefit_persDontknowDDeclinetoanswer,
               b_mudk_benefit_persYes,
               b_mudk_benefit_nonpersDontknowDDeclinetoanswer,
               b_mudk_benefit_nonpersYes) %>%
  group_by(.variable) %>% 
  mutate(rope.bound = ifelse(.value > 0 | .value < 0, 1, 0),
         median.value = median(.value),
         rope.out = ifelse(median.value > 0.0 & .value > 0.0, 1, ifelse(median.value < 0.0 & .value < 0.0, 1, NA)),
         perc.out = sum(rope.out, na.rm = TRUE)/28000) %>% 
  median_qi(.width = c(.95, .9), na.rm = TRUE) %>% 
  mutate(dv = "US Troops",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))

# Government model
coef.2.catbayes <- `m2.cat.bayes` %>% 
  gather_draws(b_mupos_contact_persDontknowDDeclinetoanswer,
               b_mupos_contact_persYes,
               b_mupos_contact_nonpersDontknowDDeclinetoanswer,
               b_mupos_contact_nonpersYes,
               b_mupos_benefit_persDontknowDDeclinetoanswer,
               b_mupos_benefit_persYes,
               b_mupos_benefit_nonpersDontknowDDeclinetoanswer,
               b_mupos_benefit_nonpersYes,
               b_muneg_contact_persDontknowDDeclinetoanswer,
               b_muneg_contact_persYes,
               b_muneg_contact_nonpersDontknowDDeclinetoanswer,
               b_muneg_contact_nonpersYes,
               b_muneg_benefit_persDontknowDDeclinetoanswer,
               b_muneg_benefit_persYes,
               b_muneg_benefit_nonpersDontknowDDeclinetoanswer,
               b_muneg_benefit_nonpersYes,
               b_mudk_contact_persDontknowDDeclinetoanswer,
               b_mudk_contact_persYes,
               b_mudk_contact_nonpersDontknowDDeclinetoanswer,
               b_mudk_contact_nonpersYes,
               b_mudk_benefit_persDontknowDDeclinetoanswer,
               b_mudk_benefit_persYes,
               b_mudk_benefit_nonpersDontknowDDeclinetoanswer,
               b_mudk_benefit_nonpersYes) %>% 
  group_by(.variable) %>% 
  mutate(rope.bound = ifelse(.value > 0 | .value < 0, 1, 0),
         median.value = median(.value),
         rope.out = ifelse(median.value > 0.0 & .value > 0.0, 1, ifelse(median.value < 0.0 & .value < 0.0, 1, NA)),
         perc.out = sum(rope.out, na.rm = TRUE)/28000) %>% 
  median_qi(.width = c(.95, .9), na.rm = TRUE) %>% 
  mutate(dv = "US Government",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))

# People model
coef.3.catbayes <- `m1.cat.bayes` %>% 
  gather_draws(b_mupos_contact_persDontknowDDeclinetoanswer,
               b_mupos_contact_persYes,
               b_mupos_contact_nonpersDontknowDDeclinetoanswer,
               b_mupos_contact_nonpersYes,
               b_mupos_benefit_persDontknowDDeclinetoanswer,
               b_mupos_benefit_persYes,
               b_mupos_benefit_nonpersDontknowDDeclinetoanswer,
               b_mupos_benefit_nonpersYes,
               b_muneg_contact_persDontknowDDeclinetoanswer,
               b_muneg_contact_persYes,
               b_muneg_contact_nonpersDontknowDDeclinetoanswer,
               b_muneg_contact_nonpersYes,
               b_muneg_benefit_persDontknowDDeclinetoanswer,
               b_muneg_benefit_persYes,
               b_muneg_benefit_nonpersDontknowDDeclinetoanswer,
               b_muneg_benefit_nonpersYes,
               b_mudk_contact_persDontknowDDeclinetoanswer,
               b_mudk_contact_persYes,
               b_mudk_contact_nonpersDontknowDDeclinetoanswer,
               b_mudk_contact_nonpersYes,
               b_mudk_benefit_persDontknowDDeclinetoanswer,
               b_mudk_benefit_persYes,
               b_mudk_benefit_nonpersDontknowDDeclinetoanswer,
               b_mudk_benefit_nonpersYes) %>% 
  group_by(.variable) %>% 
  mutate(rope.bound = ifelse(.value > 0 | .value < 0, 1, 0),
         median.value = median(.value),
         rope.out = ifelse(median.value > 0.0 & .value > 0.0, 1, ifelse(median.value < 0.0 & .value < 0.0, 1, NA)),
         perc.out = sum(rope.out, na.rm = TRUE)/28000) %>% 
  median_qi(.width = c(.95, .9), na.rm = TRUE) %>% 
  mutate(dv = "US People",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))


coef.catbayes.com <- rbind(coef.1.catbayes, coef.2.catbayes, coef.3.catbayes) %>% 
  mutate(iv = ifelse(grepl(".*contact_pers.*", .variable), "Personal Contact",
                ifelse(grepl(".*contact_nonpers.*", .variable), "Network Contact",
                ifelse(grepl(".*benefit_pers.*", .variable), "Personal Benefit",
                ifelse(grepl(".*benefit_nonpers.*", .variable), "Network Benefit", NA)))),
         iv.response = ifelse(grepl(".*Declinetoanswer.*", .variable), "Don't know/Decline", 
                       ifelse(grepl(".*Yes.*", .variable), "Yes",
                       ifelse(grepl(".*No.*", .variable), "No", NA))),
         dv = factor(dv, levels = c("US Troops", "US Government", "US People")),
         iv = factor(iv, levels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit")),
         dv.response = factor(dv.response, levels = c("Don't know/Decline", "Negative", "Positive")),
         iv.response = factor(iv.response, levels = c("Don't know/Decline", "No", "Yes"))) 


# Full coefficient table
ggplot(data = coef.catbayes.com, aes(x = .value, y = iv.response, color = dv.response, shape = dv.response)) +
  geom_text(aes(x = .value, y = iv.response, label = percent(perc.out, digits = 2)), position = position_dodgev(height = 1.0), size = 3.0, vjust = 0, show.legend = FALSE) +
  geom_pointintervalh(aes(xmin = .value.lower, xmax = .value.upper), position = position_dodgev(height = .5), alpha = .7) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(iv ~ dv) +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  labs(x = "Coefficient Estimate",
       y = "Independent Variable Response",
       color = "Dependent Variable Response",
       shape = "Dependent Variable Response")


ggsave(here("Figures", "apsr-figure-coefplot-cat-bayes-20190924.pdf"))



# Intercept values
# US Troops
intercept.catbayes.1 <- `m1.cat.bayes` %>% 
  spread_draws(b_mupos_Intercept,
               b_muneg_Intercept,
               b_mudk_Intercept,
               r_country__mupos[country],
               r_country__muneg[country],
               r_country__mudk[country]) %>% 
  group_by(country) %>% 
  mutate(int.pos = b_mupos_Intercept + r_country__mupos,
            int.neg = b_muneg_Intercept + r_country__muneg,
            int.dk = b_mudk_Intercept + r_country__mudk) %>% 
  gather_variables() %>% 
  median_qi(.width = c(0.90, 0.95)) %>% 
  mutate(DV = "US Troops")


# US Government
intercept.catbayes.2 <- `m2.cat.bayes` %>% 
  spread_draws(b_mupos_Intercept,
               b_muneg_Intercept,
               b_mudk_Intercept,
               r_country__mupos[country],
               r_country__muneg[country],
               r_country__mudk[country]) %>% 
  group_by(country) %>% 
  mutate(int.pos = b_mupos_Intercept + r_country__mupos,
         int.neg = b_muneg_Intercept + r_country__muneg,
         int.dk = b_mudk_Intercept + r_country__mudk) %>% 
  gather_variables() %>% 
  median_qi(.width = c(0.90, 0.95)) %>% 
  mutate(DV = "US Government")


# US People
intercept.catbayes.3 <- `m3.cat.bayes` %>% 
  spread_draws(b_mupos_Intercept,
               b_muneg_Intercept,
               b_mudk_Intercept,
               r_country__mupos[country],
               r_country__muneg[country],
               r_country__mudk[country]) %>% 
  group_by(country) %>% 
  mutate(int.pos = b_mupos_Intercept + r_country__mupos,
         int.neg = b_muneg_Intercept + r_country__muneg,
         int.dk = b_mudk_Intercept + r_country__mudk) %>% 
  gather_variables() %>% 
  median_qi(.width = c(0.90, 0.95)) %>% 
  mutate(DV = "US People")

intercept.catbayes.com <- rbind(intercept.catbayes.1, intercept.catbayes.2, intercept.catbayes.3) %>% 
  mutate(.variable = ifelse(grepl('.*b_mupos.*', .variable), "Population Intercept (Positive)",
                     ifelse(grepl('.*b_muneg.*', .variable), "Population Intercept (Negative)",
                     ifelse(grepl('.*b_mudk.*', .variable), "Population Intercept (Don't know/Decline)", 
                     ifelse(grepl('int.pos', .variable), "Country Intercept (Positive)",
                     ifelse(grepl('int.neg', .variable), "Country Intercept (Negative)",
                     ifelse(grepl('int.dk', .variable), "Country Intercept (Don't know/Decline)",
                     ifelse(grepl('.*__mupos.*', .variable), "Country Error (Positive)",
                     ifelse(grepl('.*__muneg.*', .variable), "Country Error (Negative)",
                     ifelse(grepl('.*__mudk.*', .variable), "Country Error (Don't know/Decline)",.variable))))))))),
         equation = ifelse(grepl('.*(Positive).*', .variable), "Positive",
                             ifelse(grepl('.*(Negative).*', .variable), "Negative",
                                    ifelse(grepl(".*(Don't know/Decline).*", .variable), "Don't know/Decline", .variable))))


# Color palette used in summary figure
#pal.6.cat[1] <- "gray60" 
pal.4.cat <- c("#cccccc", "#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")
pal.5.cat <- c("#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")
pal.6.cat <- c("#cccccc", "#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4")

# Population intercept values
cat.1 <- ggplot(data = intercept.catbayes.com %>% filter(.variable == "Population Intercept (Positive)" |
                                              .variable == "Population Intercept (Negative)" |
                                              .variable == "Population Intercept (Don't know/Decline)"), aes(x = .value, y = DV, color = equation, shape = equation)) +
  geom_pointintervalh(position = position_dodgev(height = .5), alpha = .6) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-20, 20, 4)) +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  labs(x = "Intercept",
       y = "", 
       color = "", 
       shape = "")

# Country error intercept values
cat.2 <- ggplot(data = intercept.catbayes.com %>% filter(.variable == "Country Error (Positive)" |
                                                  .variable == "Country Error (Negative)" |
                                                  .variable == "Country Error (Don't know/Decline)"), aes(x = .value, y = country, color = equation, shape = equation)) +
  geom_pointintervalh(position = position_dodgev(height = .5), alpha = .6) +
  geom_point(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  scale_x_continuous() +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  facet_grid(. ~ DV) +
  labs(x = "Deviation",
       y = "",
       color = "Response Equation",
       shape = "Response Equation")

ggarrange(
  cat.1, cat.2, 
  labels = c("A", "B"),
  ncol = 2,
  common.legend = TRUE, 
  legend = "bottom"
)

ggsave(here("Figures", "apsr-figure-intercepts-cat-bayes-20190924.pdf"), width = 11, height = 7.5, units = "in")





# Predicted Probabilities 
conditions.cat.bayes <- `m1.cat.bayes`$data %>% 
  group_by(country) %>% 
  summarise(american_inf_1 = modal(levels(american_inf_1)),
            american_inf_2 = modal(levels(american_inf_2)),
            demgov = modal(levels(demgov)),
            gender = modal(levels(gender)),
            ed = mean(ed, na.rm = TRUE),
            age = modal(levels(age)),
            income = modal(levels(income)),
            ideology = mean(ideology, na.rm = TRUE),
            relig = modal(levels(relig)),
            minority = modal(levels(minority)),
            spend_toa_combined_w_log = mean(spend_toa_combined_w_log, na.rm = TRUE),
            baseinprovince = modal(baseinprovince),
            defense = modal(defense),
            polity2 = mean(polity2, na.rm = TRUE),
            gdp_constant_log = mean(gdp_constant_log, na.rm = TRUE),
            log_trade_total_2017 = mean(log_trade_total_2017, na.rm = TRUE),
            log_students = mean(log_students, na.rm = TRUE),
            log_troops_2017 = mean(log_troops_2017, na.rm = TRUE),
            log_threat_environment = mean(log_threat_environment, na.rm = TRUE))


# Troops Prediction Data
me.cat.1 <- brms::marginal_effects(m1.cat.bayes,
                                  effects = c("contact_pers", "contact_nonpers", "benefit_pers", "benefit_nonpers"),
                                  conditions = conditions.cat.bayes,
                                  re_formula = ~(1 | country),
                                  probs = c(0.025, 0.975),
                                  method = c("fitted"),
                                  spaghetti = FALSE,
                                  categorical = TRUE)


# Troops Model: Personal Contact Predicted Probability
me.cat.1.df.1 <- as.data.frame(me.cat.1[[1]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_pers = factor(contact_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.1.df.1, aes(x = contact_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Military Personnel",
       subtitle = "Personal Contact Varies. All other variables held at country mean, median, or mode.")
  
ggsave(here("Figures", "apsr-figure-predicted-troops-contact-pers.png"))

  
# Troops Model: Network Contact Predicted Probability
me.cat.1.df.2 <- as.data.frame(me.cat.1[[2]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_nonpers = factor(contact_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.1.df.2, aes(x = contact_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Military Personnel",
       subtitle = "Network Contact Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-troops-contact-nonpers.png"))


# Troops Model: Personal Benefit Predicted Probability
me.cat.1.df.3 <- as.data.frame(me.cat.1[[3]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_pers = factor(benefit_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.1.df.3, aes(x = benefit_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Military Personnel",
       subtitle = "Personal Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-troops-benefit-pers.png"))


# Troops Model: Network Benefit Predicted Probability
me.cat.1.df.4 <- as.data.frame(me.cat.1[[4]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_nonpers = factor(benefit_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.1.df.4, aes(x = benefit_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Military Personnel",
       subtitle = "Network Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-troops-benefit-nonpers.png"))





# Government Prediction Data
me.cat.2 <- brms::marginal_effects(m2.cat.bayes,
                                   effects = c("contact_pers", "contact_nonpers", "benefit_pers", "benefit_nonpers"),
                                   conditions = conditions.cat.bayes,
                                   re_formula = ~(1 | country),
                                   probs = c(0.025, 0.975),
                                   method = c("fitted"),
                                   spaghetti = FALSE,
                                   categorical = TRUE)


# Government Model: Personal Contact Predicted Probability
me.cat.2.df.1 <- as.data.frame(me.cat.2[[1]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_pers = factor(contact_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.2.df.1, aes(x = contact_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Government",
       subtitle = "Personal Contact Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-gov-contact-pers.png"))


# Government Model: Network Contact Predicted Probability
me.cat.2.df.2 <- as.data.frame(me.cat.2[[2]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_nonpers = factor(contact_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.2.df.2, aes(x = contact_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Government",
       subtitle = "Network Contact Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-gov-contact-nonpers.png"))


# Government Model: Personal Benefit Predicted Probability
me.cat.2.df.3 <- as.data.frame(me.cat.2[[3]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_pers = factor(benefit_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.2.df.3, aes(x = benefit_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Government",
       subtitle = "Personal Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-gov-benefit-pers.png"))


# Government Model: Network Benefit Predicted Probability
me.cat.2.df.4 <- as.data.frame(me.cat.2[[4]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_nonpers = factor(benefit_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.2.df.4, aes(x = benefit_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards US Government",
       subtitle = "Network Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-gov-benefit-nonpers.png"))




# People Prediction Data
me.cat.3 <- brms::marginal_effects(m3.cat.bayes,
                                   effects = c("contact_pers", "contact_nonpers", "benefit_pers", "benefit_nonpers"),
                                   conditions = conditions.cat.bayes,
                                   re_formula = ~(1 | country),
                                   probs = c(0.025, 0.975),
                                   method = c("fitted"),
                                   spaghetti = FALSE,
                                   categorical = TRUE)


# People Model: Personal Contact Predicted Probability
me.cat.3.df.1 <- as.data.frame(me.cat.3[[1]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_pers = factor(contact_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.3.df.1, aes(x = contact_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards American People",
       subtitle = "Personal Contact Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-people-contact-pers.png"))


# People Model: Network Contact Predicted Probability
me.cat.3.df.2 <- as.data.frame(me.cat.3[[2]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(contact_nonpers = factor(contact_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.3.df.2, aes(x = contact_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Contact",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards American People",
       subtitle = "Network Contact Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-people-contact-nonpers.png"))


# People Model: Personal Benefit Predicted Probability
me.cat.3.df.3 <- as.data.frame(me.cat.3[[3]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_pers = factor(benefit_pers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.3.df.3, aes(x = benefit_pers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Personal Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards American People",
       subtitle = "Personal Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-people-benefit-pers.png"))


# People Model: Network Benefit Predicted Probability
me.cat.3.df.4 <- as.data.frame(me.cat.3[[4]]) %>% 
  arrange(country, cats__, effect1__) %>% 
  mutate(benefit_nonpers = factor(benefit_nonpers, levels = c("No", "Yes", "Don't know/Decline to answer")),
         cats__ = factor(cats__, labels = c("Don't know", "Negative", "Neutral", "Positive")))


ggplot(me.cat.3.df.4, aes(x = benefit_nonpers, y = estimate__, ymin = lower__, ymax = upper__ , color = cats__)) +
  geom_pointrange(position = position_dodge(width = 0.65)) +
  facet_wrap(country ~ ., ncol = 4) +
  theme_bw() +
  scale_x_discrete(labels = wrap_format(15)) +
  scale_color_manual(values = c("gray60", "#D73027", "purple", "#4575B4")) +
  labs(x = "Network Benefit",
       y = "Predicted Probability",
       color = "Respondent Attitude",
       title = "Predicted Probability of Respondent Attitude Towards American People",
       subtitle = "Network Benefit Varies. All other variables held at country mean, median, or mode.")

ggsave(here("Figures", "apsr-figure-predicted-people-benefit-nonpers.png"))

















# Save for later
# Posterior predictions 
posterior.catbayes <- `m1.cat.bayes`$data %>% 
  data_grid(contact_pers,
            contact_nonpers = modal(levels(contact_pers)),
            benefit_pers = modal(levels(benefit_pers)),
            benefit_nonpers = modal(levels(benefit_nonpers)),
            american_inf_1 = modal(levels(american_inf_1)),
            american_inf_2 = modal(levels(american_inf_2)),
            demgov = modal(levels(demgov)),
            gender = modal(levels(gender)),
            ed = mean(ed, na.rm = TRUE),
            age = modal(levels(age)),
            income = modal(levels(income)),
            ideology = mean(ideology, na.rm = TRUE),
            relig = modal(levels(relig)),
            minority = modal(levels(minority)),
            spend_toa_combined_w_log = mean(spend_toa_combined_w_log, na.rm = TRUE),
            baseinprovince = modal(baseinprovince),
            defense = 1,
            polity2 = mean(polity2, na.rm = TRUE),
            gdp_constant_log = mean(gdp_constant_log, na.rm = TRUE),
            log_trade_total_2017 = mean(log_trade_total_2017, na.rm = TRUE),
            log_students = mean(log_students, na.rm = TRUE),
            log_troops_2017 = mean(log_troops_2017, na.rm = TRUE),
            log_threat_environment = mean(log_threat_environment, na.rm = TRUE),
            country) %>% 
  add_fitted_draws(`m1.cat.bayes`,
                   prediction = "pos",
                   category = ".category",
                   re_formula = ~(1 | country),
                   allow_new_levels = TRUE,
                   n = 100,
                   seed = 66502,
                   model = `m1.cat.bayes`)

