load("experiment_data.RData")

library(tidyverse)

# ExperimentalCondition==2 indicates respondents who were randomized into the consequences experiment
# Formal t-tests, for numbers written in text
# Positive ("Both Important") versus Negative ("Neither Important")
neither.t.test <- t.test(filter(turk,ExperimentalCondition==2,cueStrength=="Both Important")[,"outcome"],
                         filter(turk,ExperimentalCondition==2,cueStrength=="Neither Important")[,"outcome"])
# Positive ("Both Important") versus Mixed ("Mixed Importance")
mixed.t.test <- t.test(filter(turk,ExperimentalCondition==2,cueStrength=="Both Important")[,"outcome"],
                       filter(turk,ExperimentalCondition==2,cueStrength=="Mixed Importance")[,"outcome"])
# Positive ("Both Important") versus nothing ("No Justification Given")
nothing.t.test <- t.test(filter(turk,ExperimentalCondition==2,cueStrength=="Both Important")[,"outcome"],
                         filter(turk,ExperimentalCondition==2,cueStrength=="No Justification Given")[,"outcome"])

############
# Figure 2 #
############
# Replicating the above t-tests but in a regression framework; outcome variable is support, predictors are treatment conditions, omitted level is "Both Important"
fit_all <- lm(outcome~cueStrength, data = filter(turk, ExperimentalCondition == 2))
summary(fit_all)

# Plot of differences when moving from both to one of the other conditions
# Size of all the differences
coefs_from_both <- c(fit_all$coefficients["cueStrengthNeither Important"],
                     fit_all$coefficients["cueStrengthMixed Importance"],
                     fit_all$coefficients["cueStrengthNo Justification Given"])
# Confidence intervals around the differences, these come from the formal t-tests above
ci95_high <- c(neither.t.test$conf.int[2]*-1,mixed.t.test$conf.int[2]*-1,nothing.t.test$conf.int[2]*-1)
ci95_low <- c(neither.t.test$conf.int[1]*-1,mixed.t.test$conf.int[1]*-1,nothing.t.test$conf.int[1]*-1)

# Making the plot
pdf("figure-2.pdf")
tibble(Estimate = coefs_from_both,
       ci_high = ci95_high,
       ci_low = ci95_low,
       Group = c("Neither Important","Mixed Importance","No Justification Given")) %>%
  mutate(Group = factor(Group, levels = c("Neither Important","Mixed Importance","No Justification Given"))) %>%
  ggplot(aes(x = Estimate,
             y = Group,
             xmin = ci95_low,
             xmax = ci95_high,
             color = Group)) +
  geom_point() +
  geom_errorbar(position = position_dodge(width = 0.9),
                width = 0.2) +
  #  coord_flip() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  ylab("") +
  xlab("Change in Support when Compared to Both Important") +
  labs(color = "Experimental Condition") +
  theme_classic() +
  theme(legend.position = "none") +
  geom_vline(xintercept=c(0), linetype="dotted")
dev.off()
