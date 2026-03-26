

#Coefficient plots for categorical bayesian models

# Troops model
coef.1.catbayes.contact <- `m1.cat.bayes.contact` %>% 
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
  median_qi(.width = c(.95, .9)) %>% 
  mutate(dv = "US Troops",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))

# Government model
coef.2.catbayes.contact <- `m2.cat.bayes.contact` %>% 
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
  median_qi(.width = c(.95, .9)) %>% 
  mutate(dv = "US Government",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))

# People model
coef.3.catbayes.contact <- `m1.cat.bayes.contact` %>% 
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
  median_qi(.width = c(.95, .9)) %>% 
  mutate(dv = "US People",
         dv.response = rep(c("Don't know/Decline", "Negative", "Positive"), each = 8, times = 2))


coef.catbayes.contact.com <- rbind(coef.1.catbayes.contact, coef.2.catbayes.contact, coef.3.catbayes.contact) %>% 
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
ggplot(data = coef.catbayes.contact.com, aes(x = .value, y = iv.response, color = dv.response)) +
  geom_pointintervalh(position = position_dodgev(height = .5)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_grid(iv ~ dv) +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  labs(x = "Coefficient Estimate",
       y = "Independent Variable Response",
       color = "Dependent Variable Response")

ggsave(here("Figures", "apsr-figure-coefplot-cat-bayes-contact-only-20190924.pdf"))



# Intercept values
# US Troops
intercept.catbayes.contact.1 <- `m1.cat.bayes.contact` %>% 
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
intercept.catbayes.contact.2 <- `m2.cat.bayes.contact` %>% 
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
intercept.catbayes.contact.3 <- `m3.cat.bayes.contact` %>% 
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

intercept.catbayes.contact.com <- rbind(intercept.catbayes.contact.1, intercept.catbayes.contact.2, intercept.catbayes.contact.3) %>% 
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
cat.1 <- ggplot(data = intercept.catbayes.contact.com %>% filter(.variable == "Population Intercept (Positive)" |
                                                           .variable == "Population Intercept (Negative)" |
                                                           .variable == "Population Intercept (Don't know/Decline)"), aes(x = .value, y = DV, color = equation)) +
  geom_pointintervalh(position = position_dodgev(height = .5), alpha = .6) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-20, 20, 4)) +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  labs(x = "Intercept",
       y = "", 
       color = "")

# Country error intercept values
cat.2 <- ggplot(data = intercept.catbayes.contact.com %>% filter(.variable == "Country Error (Positive)" |
                                                           .variable == "Country Error (Negative)" |
                                                           .variable == "Country Error (Don't know/Decline)"), aes(x = .value, y = country, color = equation)) +
  geom_pointintervalh(position = position_dodgev(height = .5), alpha = .6) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  scale_x_continuous() +
  scale_color_manual(values = c("gray60", "#D73027", "#4575B4")) +
  facet_grid(. ~ DV) +
  labs(x = "Deviation",
       y = "",
       color = "")

ggarrange(
  cat.1, cat.2, 
  labels = c("A", "B"),
  ncol = 2,
  common.legend = TRUE, 
  legend = "bottom"
)

ggsave(here("Figures", "apsr-figure-intercepts-cat-bayes-contact-only-20190924.pdf"), width = 11, height = 7.5, units = "in")








# Save for later
# Posterior predictions 
posterior.catbayes.contact <- `m1.cat.bayes.contact`$data %>% 
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
            country) %>% 
  add_fitted_draws(`m1.cat.bayes.contact`,
                   prediction = "pos",
                   category = ".category",
                   re_formula = ~(1 | country),
                   allow_new_levels = TRUE,
                   n = 100,
                   seed = 5786,
                   model = `m1.cat.bayes.contact`)

