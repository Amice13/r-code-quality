

# Predictions for Bayesian Logit models

# Coefficient Plots

# Troops model
coef.1.bayes <- `m1.bayes` %>% 
  gather_draws(b_contact_persDontknowDDeclinetoanswer,
               b_contact_persYes,
               b_contact_nonpersDontknowDDeclinetoanswer,
               b_contact_nonpersYes,
               b_benefit_persDontknowDDeclinetoanswer,
               b_benefit_persYes,
               b_benefit_nonpersDontknowDDeclinetoanswer,
               b_benefit_nonpersYes) %>% 
  median_qi(.width = c(.95, .9)) %>% 
  mutate(model = "US Troops")

# Government model
coef.2.bayes <- `m2.bayes` %>% 
  gather_draws(b_contact_persDontknowDDeclinetoanswer,
               b_contact_persYes,
               b_contact_nonpersDontknowDDeclinetoanswer,
               b_contact_nonpersYes,
               b_benefit_persDontknowDDeclinetoanswer,
               b_benefit_persYes,
               b_benefit_nonpersDontknowDDeclinetoanswer,
               b_benefit_nonpersYes) %>% 
  median_qi(.width = c(.95, .9)) %>% 
  mutate(model = "US Government")

# People model
coef.3.bayes <- `m3.bayes` %>% 
  gather_draws(b_contact_persDontknowDDeclinetoanswer,
               b_contact_persYes,
               b_contact_nonpersDontknowDDeclinetoanswer,
               b_contact_nonpersYes,
               b_benefit_persDontknowDDeclinetoanswer,
               b_benefit_persYes,
               b_benefit_nonpersDontknowDDeclinetoanswer,
               b_benefit_nonpersYes) %>% 
  median_qi(.width = c(.95, .9)) %>% 
  mutate(model = "US People")

coef.com.bayes <- rbind(coef.1.bayes, coef.2.bayes, coef.3.bayes) %>% 
  mutate(var = ifelse(.variable == "b_contact_persDontknowDDeclinetoanswer" | .variable == "b_contact_persYes", "Personal Contact", NA),
         var = ifelse(.variable == "b_contact_nonpersDontknowDDeclinetoanswer" | .variable == "b_contact_nonpersYes", "Network Contact", var),
         var = ifelse(.variable == "b_benefit_persDontknowDDeclinetoanswer" | .variable == "b_benefit_persYes", "Personal Benefit", var),
         var = ifelse(.variable == "b_benefit_nonpersDontknowDDeclinetoanswer" | .variable == "b_benefit_nonpersYes", "Network Benefit", var),
         response = ifelse(.variable == "b_contact_persDontknowDDeclinetoanswer" |
                           .variable == "b_contact_nonpersDontknowDDeclinetoanswer" |
                           .variable == "b_benefit_persDontknowDDeclinetoanswer" |
                             .variable == "b_benefit_nonpersDontknowDDeclinetoanswer", "Don't know/Decline to Answer", NA),
         response = ifelse(.variable == "b_contact_persYes" |
                             .variable == "b_contact_nonpersYes" |
                             .variable == "b_benefit_persYes" |
                             .variable == "b_benefit_nonpersYes", "Yes", response),
         model = factor(model, levels = c("US Troops", "US Government", "US People")),
         var = factor(var, levels = c("Personal Contact", "Network Contact", "Personal Benefit", "Network Benefit")))

  ggplot(data = coef.com.bayes, aes(x = .value, y = response, color = model)) +
    geom_pointintervalh(position = position_dodgev(height = .5)) +
    geom_vline(xintercept = 0) +
    facet_grid(var ~ ., scale = "free") +
    theme_bw() +
    scale_color_grey() +
    labs(x = "Coefficient Estimate",
         y = "",
         color = "model")
  
  ggsave(here("Figures", "apsr-figure-coefplot-logit-bayes-20191001.pdf"))
  
  
  