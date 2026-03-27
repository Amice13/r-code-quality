# Replication code for Butler, Sevenans et al. How politicians (mis)perceive policy salience

# ------------------------------------------------------------------------------------------

# Stage 1: Load and clean data

library(haven)
library(dplyr)
library(survey)
library(naniar)
library(tidyverse)
library(psych)

elites_long <- read.csv("PA_of_salience.csv")
str(elites_long)

#---------------------STAGE II: SET UP & DESCRIBE KEY VARIABLES--------------------------------------

# Calculate dependent variable I: absolute error in estimates of salience

# Remove missing values
elites_long <- elites_long %>% replace_with_na(replace = list(Q11 = -99))
elites_long <- elites_long %>% replace_with_na(replace = list(Q11_dis = -99))
elites_long <- elites_long %>% replace_with_na(replace = list(Q15 = -99))

# Combine Swiss data on cantonal salience with other countries' data on 
# national salience into single variables
elites_long$salience_lower_ci <- ifelse(elites_long$V2 == "Switzerland", elites_long$lower_ci_cantonal_salience,
                                        elites_long$lower_ci_salience)
elites_long$salience_upper_ci <- ifelse(elites_long$V2 == "Switzerland", elites_long$upper_ci_cantonal_salience,
                                        elites_long$upper_ci_salience)

elites_long$salience_estimate <- ifelse(elites_long$V2 == "Switzerland", elites_long$Q11_dis,
                                        elites_long$Q11)
elites_long$actual_salience <- ifelse(elites_long$V2 == "Switzerland", elites_long$dis_und_cal,
                                      elites_long$cou_und_cal)

# Calculate absolute distance of estimates from confidence intervals
elites_long$salience_lower_ci_diff <- (elites_long$salience_lower_ci-elites_long$salience_estimate)
elites_long$salience_upper_ci_diff <- (elites_long$salience_estimate-elites_long$salience_upper_ci)
elites_long <- elites_long %>%
  mutate(
    abs_distance_ci = ifelse(
      abs(salience_lower_ci_diff) <= abs(salience_upper_ci_diff),
      salience_lower_ci_diff,
      salience_upper_ci_diff
    )
  )

# Recode any estimates within confidence intervals to 0
elites_long$abs_distance <- ifelse(elites_long$abs_distance_ci > 0, elites_long$abs_distance_ci, 0)

# Plot for sense check
par(mar = c(4, 4, 2, 1))
plot(elites_long$abs_distance, elites_long$abs_distance_ci)

# Generate descriptive stats for this variable,
# removing data on Swiss cantons with small samples
elites_long <- elites_long %>% mutate(dis_small = ifelse(is.na(dis_small), 0, dis_small))
summary(elites_long$abs_distance[elites_long$dis_small != 1])
describe(elites_long$abs_distance[elites_long$dis_small != 1])

# Calculate proportion of estimates that fall within confidence intervals
prop_accurate <- mean(elites_long$abs_distance_ci[elites_long$dis_small != 1] < 0, na.rm = TRUE)

# Calculate proportion of estimates that are relatively small errors (less than 10%)?
prop_small_error <- mean(elites_long$abs_distance[elites_long$dis_small != 1] < 10.01, na.rm = TRUE)
prop_small_error-prop_accurate

# Repeat for estimates of party support (excluding small party electorates)

elites_long$support_lower_ci_diff <- (elites_long$lower_ci_party_salience-elites_long$Q15)

elites_long$support_upper_ci_diff <- (elites_long$Q15-elites_long$upper_ci_party_salience)

elites_long <- elites_long %>%
  mutate(
    abs_distance_ci_support = ifelse(
      abs(support_lower_ci_diff) <= abs(support_upper_ci_diff),
      support_lower_ci_diff,
      support_upper_ci_diff
    )
  )

elites_long$abs_distance_support <- ifelse(elites_long$abs_distance_ci_support > 0, elites_long$abs_distance_ci_support, 0)

plot(elites_long$abs_distance_support, elites_long$abs_distance_ci_support)

describe(elites_long$abs_distance_support[elites_long$ele_small != 1])

prop_accurate_party <- mean(elites_long$abs_distance_ci_support[elites_long$ele_small != 1] < 0, na.rm = TRUE)

# Calculate dependent variable II: simple error of estimations

# Recalculate distance from upper CI so that over-estimates of don't knows become negative values
elites_long$salience_upper_ci_diff <- (elites_long$salience_upper_ci-elites_long$salience_estimate)

# Calculate simple error
elites_long <- elites_long %>%
  mutate(
    distance_ci = ifelse(
      abs(salience_lower_ci_diff) <= abs(salience_upper_ci_diff),
      salience_lower_ci_diff,
      salience_upper_ci_diff
    )
  )


# Recode any estimates within confidence intervals to 0
elites_long$simple_distance <- ifelse(elites_long$abs_distance_ci > 0, elites_long$distance_ci, 0)

# Sense check
plot(elites_long$abs_distance, elites_long$simple_distance)

# Descriptive statistics for simple error variable
describe(elites_long$simple_distance[elites_long$dis_small != 1])

# Repeat for estimates among party supporters
elites_long$support_upper_ci_diff <- (elites_long$upper_ci_party_salience-elites_long$Q15)
elites_long <- elites_long %>%
  mutate(
    distance_support_ci = ifelse(
      abs(support_lower_ci_diff) <= abs(support_upper_ci_diff),
      support_lower_ci_diff,
      support_upper_ci_diff
    )
  )
elites_long$simple_distance_support <- ifelse(elites_long$abs_distance_ci_support > 0, elites_long$distance_support_ci, 0)

plot(elites_long$abs_distance_support, elites_long$simple_distance_support)

describe(elites_long$simple_distance_support[elites_long$ele_small != 1])

# Sense check. Why are some estimates so far outside confidence intervals? How many are there?

sense_check <- elites_long |> 
  select("Q15", "lower_ci_party_salience", "upper_ci_party_salience", "simple_distance_support")
sense_check %>% slice_min(simple_distance_support, n = 10)
prop_anamolies <- mean(elites_long$salience_estimate[elites_long$dis_small != 1] > 90, na.rm = TRUE)
prop_anamolies_supporters <- mean(elites_long$Q15[elites_long$ele_small != 1] > 90, na.rm = TRUE)

# Calculate PERCEIVED congruence with public opinion

# Remove missing values
elites_long <- elites_long %>% replace_with_na(replace = list(Q10 = -99))
elites_long <- elites_long %>% replace_with_na(replace = list(Q12 = -99))
elites_long <- elites_long %>% replace_with_na(replace = list(Q12_dis = -99))
elites_long <- elites_long %>% replace_with_na(replace = list(Q16 = -99))

# Exclude observations where politician has no opinion
elites_long <- elites_long %>% replace_with_na(replace = list(Q10 = 5))

# Create binary variable for congruence if politician agrees & over 50% of public do or vice-versa
elites_long$national_congruence <- ifelse((elites_long$Q10 > 2 & elites_long$Q12 > 50) | 
                                            (elites_long$Q10 < 3 & elites_long$Q12 < 50), 1, 0)
# Repeat for Swiss data
elites_long$district_congruence <- ifelse((elites_long$Q10 > 2 & elites_long$Q12_dis > 50) | 
                                            (elites_long$Q10 < 3 & elites_long$Q12_dis < 50), 1, 0)
# Combine variables
elites_long$perceived_voter_congruence <- ifelse(elites_long$V2 %in% c("Flanders", "Wallonia_or_Brussels", 
                                                                       "Germany", "Canada"), elites_long$national_congruence,
                                                 elites_long$district_congruence)

# Descriptive statistics
describe(elites_long$perceived_voter_congruence)

# Repeat for perceived congruence with party supporters
elites_long$perceived_party_congruence <- ifelse((elites_long$Q10 > 2 & elites_long$Q16 > 50) | 
                                                   (elites_long$Q10 < 3 & elites_long$Q16 < 50), 1, 0)
describe(elites_long$perceived_party_congruence)

# Descriptive stats for other variables of interest

# Invert salience variables from proportion of no opinion
# to proportion with opinion
elites_long$salience_estimate <- 100-elites_long$salience_estimate
describe(elites_long$salience_estimate)

elites_long$supporters_salience_estimate <- 100-elites_long$Q15
describe(elites_long$supporters_salience_estimate)

elites_long <- elites_long %>% replace_with_na(replace = list(Q14 = -99))
describe(elites_long$Q14)

elites_long$actual_salience <- 100-elites_long$actual_salience
describe(elites_long$actual_salience)

elites_long$ele_und_cal <- 100-elites_long$ele_und_cal
describe(elites_long$ele_und_cal)

#---------------- STAGE 3: RQ1 How accurate are estimations?---------------------------------

# Create dataframe of absolute error in estimates of salience
library(ggdist)
df1 <- elites_long %>%
  filter(is.na(dis_small) | dis_small != 1) %>%
  select(value = abs_distance) %>%
  mutate(group = "Estimates of citizen salience")

# Create dataframe of absolute error in estimates of salience among supporters
df2 <- elites_long %>%
  filter(is.na(ele_small) | ele_small != 1) %>%
  select(value = abs_distance_support) %>%
  mutate(group = "Estimates of supporter salience")

# Combine data
violin_data <- bind_rows(df1, df2)

jpeg(file="Figure I.jpeg", width=3000, height=3000, res=300)
ggplot(violin_data, aes(x = group, y = value, fill = group)) +
  ggdist::stat_halfeye(adjust = 0.5, width = 0.7, .width = 0.95, point_interval = "median_qi") +
  stat_slabinterval(side = "both") + 
  labs(
    title = "Distribution of absolute distance of estimates from confidence interval",
    x = "",
    y = "Distance to Closest CI Bound"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()

# Create issue type variable
elites_long$issue_type <-fct_collapse(elites_long$SID,
                                      Social = c("be_5", "be_8", "ch_A3", "ch_A4", "ch_A5", "ch_A9", "ch_B2",
                                                 "ch_B4", "ch_B5", "ch_B9", "de_A2", "de_A6", "de_A7", "de_B2",
                                                 "de_B6", "de_B7", "ca_4", "ca_6", "ca_8", "be_3", "de_A5", "ca_3"),
                                      Economic = c("be_4", "ch_A2", "ch_A8", "ch_B3", "ch_B8", "de_A3", "de_B3", 
                                                   "ca_2", "ca_7", "de_B5", "ca_5"),
                                      Political_Reform = c("be_2", "be_7", "ch_A7", "de_A4", "de_B4"),
                                      Defence_International = c("be_1", "ch_A1", "ch_B1", "de_A1", "de_B1"),
                                      Migration = c("be_6", "ch_A6", "ch_B6", "ch_B7", "de_A8", "de_B8", "ca_1"))

# Calculate average absolute error by issue, first for all citizens then for party supporters
group_summary <- elites_long %>%
  group_by(issue_type) %>%
  summarise(
    mean_sal = mean(abs_distance, na.rm = TRUE),
    sd_sal = sd(abs_distance, na.rm = TRUE),
    n = sum(!is.na(abs_distance)),
    se = sd_sal / sqrt(n),
    ci_low = mean_sal - 1.96 * se,
    ci_high = mean_sal + 1.96 * se
  )
group_summary$estimate <- "All citizens"

group_summary2 <- elites_long %>%
  group_by(issue_type) %>%
  summarise(
    mean_sal = mean(abs_distance_support, na.rm = TRUE),
    sd_sal = sd(abs_distance_support, na.rm = TRUE),
    n = sum(!is.na(abs_distance_support)),
    se = sd_sal / sqrt(n),
    ci_low = mean_sal - 1.96 * se,
    ci_high = mean_sal + 1.96 * se
  )
group_summary2$estimate <- "Party Supporters"

average_by_issue <- rbind(group_summary, group_summary2)

# Plot average absolute error by issue type
jpeg(file="Figure A8.1.jpeg", width=3000, height=2000, res=300)
ggplot(average_by_issue, aes(x = reorder(issue_type, -mean_sal), group = estimate, color = estimate, y = mean_sal)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  scale_x_discrete(labels = c("Defence & International", "Political Reform", "Social", "Economic", "Migration")) +
  labs(
    title = "Mean Absolute Perceived-Actual Salience Gap by Issue Type",
    x = "Issue Type",
    y = "Mean Absolute Error in Salience Estimate",
    color = " "
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

#---------------- RQ2 Are estimations usually over- or under-estimates?---------

# Create dataframe of simple error in estimates of salience
df3 <- elites_long %>%
  filter(is.na(dis_small) | dis_small != 1) %>%
  select(value = simple_distance) %>%
  mutate(group = "Estimates of citizen salience")

# Create dataframe of simple error in estimates of salience of supporters
df4 <- elites_long %>%
  filter(is.na(ele_small) | ele_small != 1) %>%
  select(value = simple_distance_support) %>%
  mutate(group = "Estimates of supporter salience")

# Combine data and plot
violin_data_support <- bind_rows(df3, df4)

jpeg(file="Figure 2.jpeg", width=3000, height=3000, res=300)
ggplot(violin_data_support, aes(x = group, y = value, fill = group)) +
  ggdist::stat_halfeye(adjust = 0.5, width = 0.7, .width = 0.95, point_interval = "median_qi") +
  stat_slabinterval(side = "both") + 
  labs(
    title = "Distribution of simple distance of estimates from confidence interval",
    x = "",
    y = "Distance to Closest CI Bound"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()

# What percentage of errors are over-estimates of salience?
sum(elites_long[elites_long$dis_small != 1, ]$simple_distance > 0, na.rm = T)
# 453
sum(elites_long[elites_long$dis_small != 1, ]$simple_distance < 0, na.rm = T)
# 5228
sum(elites_long[elites_long$ele_small != 1, ]$simple_distance_support > 0, na.rm = T)
# 515
sum(elites_long[elites_long$ele_small != 1, ]$simple_distance_support < 0, na.rm = T)
# 4792
453/(453+5228)
515/(515+4792)

rm(country_cis, df1, df2, df3, df4, party_cis, swiss_cis, violin_data, violin_data_support, sense_check)

#-------------------------RQ3 What predicts individual estimates?-----------------------------

# check if variables need cleaning
table(elites_long$perceived_voter_congruence)
table(elites_long$Q14)
table(elites_long$com_spec)
table(elites_long$self_spec)
table(elites_long$sex)

# Create seniority variable

elites_long$seniority <- ifelse((elites_long$partyleader > 0) | (elites_long$minister > 0) | 
                                  (elites_long$statesecretary > 0) | (elites_long$ppgleader > 0) | 
                                  (elites_long$ppgleader_exp > 0) | (elites_long$minister_expnat > 0) | 
                                  (elites_long$minister_expreg > 0) |(elites_long$statesecretary_exp > 0), 1, 0)

table(elites_long$seniority, elites_long$V2, useNA = "ifany")

# Create specialist variable

elites_long$specialist <- ifelse(elites_long$com_spec > 0 | elites_long$self_spec > 1, 1, 0)
table(elites_long$specialist, useNA = "ifany")

# Run multi-level models

library(lme4)
pa_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + seniority + 
                   V2 + (1|V3) + (1|SID), data = elites_long[elites_long$dis_small != 1,],
                 na.action = na.omit)

pa_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                   seniority + V2 + (1|V3) + (1|SID) + (1|V4), 
                    data = elites_long[elites_long$ele_small != 1,])
library(texreg)
screenreg(list(pa_model, pa_party))
htmlreg(list(pa_model, pa_party), file = "Table II.doc")

empty_model <- lm(salience_estimate ~ NULL, data = elites_long)
summary(empty_model)
AIC(empty_model)
BIC(empty_model)

empty_model_party <- lm(supporters_salience_estimate ~ NULL, data = elites_long)
AIC(empty_model_party)
BIC(empty_model_party)

library(MuMIn)
r.squaredGLMM(pa_model)
r.squaredGLMM(pa_party)

# Plot predicted probabilities

nonlevelmodel <- lm(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + seniority, 
                    data = elites_long)

mean(elites_long$actual_salience, na.rm = TRUE)
mean(elites_long$seniority, na.rm = TRUE)

fittedmin <- predict(nonlevelmodel, newdata = data.frame(Q14 = 0, perceived_voter_congruence = 0, 
                                                         actual_salience = 88.6,seniority = 0), 
                     type = "response")
fittedmax <- predict(nonlevelmodel, newdata = data.frame(Q14 = 10, perceived_voter_congruence = 1, 
                                                         actual_salience = 88.6,seniority = 0), 
                     type = "response")

# Plot personal importance only
library(ggeffects)
library(sjPlot)
library(effects)
predictions_general <- ggpredict(pa_model, terms = "Q14")
predictions_party <- ggpredict(pa_party, terms = "Q14")
predictions_general$group <- "General public opinion"
predictions_party$group <- "Party supporters"
combined_predictions <- rbind(predictions_general, predictions_party)

# Plot the combined data with ggplot2
jpeg(file="Figure 3.jpeg", width=3000, height=2250, res=300)
ggplot(combined_predictions, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  labs(title = "Salience Estimate by Personal Importance",
       x = "Personal Importance",
       y = "Predicted Salience",
       color = "Estimation",
       fill = "Estimation") +
  theme_minimal()
dev.off()

# Plot perceived congruence only
predictions_general <- ggpredict(pa_model, terms = "perceived_voter_congruence [0,1]")
predictions_party <- ggpredict(pa_party, terms = "perceived_party_congruence [0,1]")
predictions_general$group <- "General public opinion"
predictions_party$group <- "Party supporters"
combined_predictions <- rbind(predictions_general, predictions_party)
combined_predictions$x <- factor(combined_predictions$x, levels = c("0","1"))

jpeg(file="Figure 4.jpeg", width=3000, height=2000, res=300)
ggplot(combined_predictions, aes(x = x, y = predicted, color = group)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(
    ymin = predicted - std.error,
    ymax = predicted + std.error
  ),
  width = 0.1,
  position = position_dodge(width = 0.4)
  ) +
  scale_x_discrete(
    labels = c(`0` = "Perceived Incongruence", `1` = "Perceived Congruence")
  ) +
  labs(title = "Salience Estimate by Perceived Congruence",
       x = "Perceived Congruence",
       y = "Predicted Salience",
       color = "Estimation",
       fill = "Estimation") +
  theme_minimal()
dev.off()

# Run models with interaction
library(lmerTest)
interaction_model <- lmer(salience_estimate ~ Q14*perceived_voter_congruence + actual_salience + seniority + 
                            V2 + (1|V3) + (1|SID), data = elites_long[elites_long$dis_small != 1,],
                          na.action = na.omit)

interaction_party <- lmer(supporters_salience_estimate ~ Q14*perceived_party_congruence + ele_und_cal +
                            seniority + V2 + (1|V3) + (1|SID) + (1|V4), 
                          data = elites_long[elites_long$ele_small != 1,])

summary(interaction_model)
summary(interaction_party)
r.squaredGLMM(interaction_model)
r.squaredGLMM(interaction_party)
AIC(interaction_model)
BIC(interaction_model)
AIC(interaction_party)
BIC(interaction_party)

jpeg(file="Figure V.jpeg", width=3300, height=3000, res=300)
plot_model(interaction_model, type = "int", terms = c("Q14", "perceived_voter_congruence"), 
           axis.title = c("Personal Importance", "Estimation of salience"),
           legend.title = "Perceived Positional Congruence")
dev.off()

jpeg(file="Figure VI.jpeg", width=3300, height=3000, res=300)
plot_model(interaction_party, type = "int", terms = c("Q14", "perceived_party_congruence"),
           axis.title = c("Personal Importance", "Estimation of salience"),
           legend.title = "Perceived Positional Congruence")
dev.off()

# Reshape data to run stacked model
elites_longer <- elites_long %>%
  pivot_longer(cols = c(salience_estimate, supporters_salience_estimate),
               names_to = "estimate_source",
               values_to = "estimate_salience") %>%
  mutate(estimate = if_else(estimate_source == "salience_estimate", 1, 2)) %>%
  mutate(perceived_congruence = if_else(estimate_source == "salience_estimate",
                                        perceived_voter_congruence,
                                        perceived_party_congruence),
         real_salience = if_else(estimate_source == "salience_estimate",
                                 actual_salience,
                                 ele_und_cal)) %>%
  select(-c(perceived_voter_congruence, perceived_party_congruence, actual_salience, ele_und_cal))

# Double check data; do mean estimates & mean salience match up to previous statistics?
aggregate(elites_longer$estimate_salience, list(elites_longer$estimate), FUN=mean, na.rm = T)
aggregate(elites_longer$real_salience, list(elites_longer$estimate), FUN=mean, na.rm = T)
# They match!

# Now run model...
stacked_model <- lmer(estimate_salience ~ Q14*estimate + perceived_congruence*estimate + real_salience + seniority + 
                        V2 + (1|V3) + (1|SID), data = elites_longer)
screenreg(stacked_model)
htmlreg(stacked_model, file = "Table A9i Stacked Model results.doc")

rm(abs_dv_model, abs_dv_party, party_support_min_max, party_undecided_min_max, scatter_plot,
   scatter_plot2, simple_dv_model, simple_dv_party, stacked_model, correlation, correlation2,
   fittedmax, fittedmin, i, nonlevelmodel, combined_predictions, predictions_general, predictions_party)

#-------------------------Dyadic analysis-----------------------------

# Create variable for question set
elites_long <- elites_long %>%
  mutate(question_set = case_when(
    SID %in% c("be_1", "be_2", "be_3", "be_4", "be_5", "be_6", "be_7", "be_8") & V2 == "Flanders" ~ "Flanders",
    SID %in% c("be_1", "be_2", "be_3", "be_4", "be_5", "be_6", "be_7", "be_8") & V2 == "Wallonia_or_Brussels" ~ "Wallonia",
    SID %in% c("ca_1", "ca_2", "ca_3", "ca_4", "ca_5", "ca_6", "ca_7", "ca_8") ~ "Canada",
    SID %in% c("de_A1", "de_A2", "de_A3", "de_A4", "de_A5", "de_A6", "de_A7", "de_A8") ~ "Germany1",
    SID %in% c("de_B1", "de_B2", "de_B3", "de_B4", "de_B5", "de_B6", "de_B7", "de_B8") ~ "Germany2",
    SID %in% c("ch_A1", "ch_A2", "ch_A3", "ch_A4", "ch_A5", "ch_A6", "ch_A7", "ch_A8", "ch_A9") ~ "Switz1",
    SID %in% c("ch_B1", "ch_B2", "ch_B3", "ch_B4", "ch_B5", "ch_B6", "ch_B7", "ch_B8", "ch_B9") ~ "Switz2",
    TRUE ~ NA_character_
  ))

# Isolate observations and variables of interest
elites_sub <- elites_long %>%
  filter(dis_small != 1)
dyad_base <- elites_sub %>%
  select(
    politician = V3,
    issue      = SID,
    sal_est    = salience_estimate,
    sal_low    = salience_lower_ci,
    sal_high   = salience_upper_ci,
    imp        = Q14,
    cong       = perceived_voter_congruence,
    act_sal    = actual_salience,
    question   = question_set,
    seniority,
    country    = V2
  )

# Create relative/ dyadic variables
dyads <- dyad_base %>%
  inner_join(dyad_base,
             by = "politician",
             suffix = c(".A", ".B")) %>%
  filter(
    issue.A < issue.B,
    # Only include dyads where CIs do NOT overlap
    sal_high.A < sal_low.B | sal_high.B < sal_low.A
  ) %>%
  mutate(
    sal_higher     = as.integer(sal_est.A > sal_est.B),
    PersonalSalienceA = as.integer(imp.A > imp.B),
    ActualSalienceA   = as.integer(act_sal.A > act_sal.B),
    cong_pattern = case_when(
      cong.A == 1 & cong.B == 0 ~ "A_only",
      cong.A == 0 & cong.B == 1 ~ "B_only",
      (cong.A == 1 & cong.B == 1) | (cong.A == 0 & cong.B == 0) ~ "both_or_neither"
    )
  )

# Make both or neither the base factor in the perceived positional congruence variable
dyads <- dyads %>%
  mutate(cong_pattern = factor(cong_pattern,
                               levels = c("both_or_neither","A_only","B_only")))

# Descriptive stats

prop.table(table(dyads$sal_higher, dyads$ActualSalienceA))
0.62814659+0.09390001

# Fit a mixed‐effects logistic model predicting P(A > B)
m_dyadic <- glmer(
  sal_higher ~ PersonalSalienceA + cong_pattern + ActualSalienceA +
    (1 | politician) + (1 | question.A),
  data   = dyads,
  family = binomial,
  na.action = na.omit
)

summary(m_dyadic)

# Repeat for party predictions
dyad_party_base <- elites_long %>%
  select(
    politician = V3,
    issue      = SID,
    sal_est    = supporters_salience_estimate,
    sal_low    = lower_ci_party_salience,
    sal_high   = upper_ci_party_salience,
    imp        = Q14,
    cong       = perceived_party_congruence,
    act_sal    = ele_und_cal,
    question   = question_set)

# Self‐join to all pairs (A vs B), keeping only one ordering (A < B)
dyads_party <- dyad_party_base %>%
  inner_join(dyad_party_base,
             by     = "politician",
             suffix = c(".A", ".B")) %>%
  filter(
    issue.A < issue.B,
    # Only include dyads where CIs do NOT overlap
    sal_high.A < sal_low.B | sal_high.B < sal_low.A
  ) %>%
  mutate(sal_higher = as.integer(sal_est.A > sal_est.B),
         PersonalSalienceA = as.integer(imp.A > imp.B),
         ActualSalienceA  = as.integer(act_sal.A > act_sal.B),
         cong_pattern = case_when(
           cong.A == 1 & cong.B == 0 ~ "A_only",
           cong.A == 0 & cong.B == 1 ~ "B_only",
           (cong.A == 1 & cong.B == 1) | (cong.A == 0 & cong.B == 0) ~ "both_or_neither"))

dyads_party <- dyads_party %>%
  mutate(cong_pattern = factor(cong_pattern,
                               levels = c("both_or_neither","A_only","B_only")))
prop.table(table(dyads_party$cong_pattern))

# Descriptive stats

prop.table(table(dyads_party$sal_higher, dyads_party$ActualSalienceA))
0.5472385+0.1429500

# Fit a mixed‐effects logistic model predicting P(A > B)
party_dyadic <- glmer(sal_higher ~ PersonalSalienceA + cong_pattern + ActualSalienceA +
    (1 | politician) + (1 | question.A), data   = dyads_party, family = binomial, na.action = na.omit)

summary(party_dyadic)

# New visualisation

# Extract fixed effects
effects_m <- summary(m_dyadic)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "term") %>%
  rename(
    estimate = `Estimate`,
    std_error = `Std. Error`
  ) %>%
  mutate(model = "All Citizens")

effects_party <- summary(party_dyadic)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "term") %>%
  rename(
    estimate = `Estimate`,
    std_error = `Std. Error`
  ) %>%
  mutate(model = "Party Supporters")

# Compute approximate probability change
logit_to_prob <- function(beta, baseline_prob = 0.5) {
  # delta P ≈ plogis(logit + beta) - plogis(logit)
  plogis(qlogis(baseline_prob) + beta) - baseline_prob
}

# Apply to each coefficient
effects_m <- effects_m %>%
  mutate(
    prob_estimate = logit_to_prob(estimate),
    prob_conf.low = logit_to_prob(estimate - 1.96 * std_error),
    prob_conf.high = logit_to_prob(estimate + 1.96 * std_error)
  )

effects_party <- effects_party %>%
  mutate(
    prob_estimate = logit_to_prob(estimate),
    prob_conf.low = logit_to_prob(estimate - 1.96 * std_error),
    prob_conf.high = logit_to_prob(estimate + 1.96 * std_error)
  )

# Combined data
effects_combined <- bind_rows(effects_m, effects_party)
effects_combined <- effects_combined %>%
  mutate(
    label = case_when(
      term == "PersonalSalienceA" ~ "Personal Importance A",
      term == "ActualSalienceA" ~ "Actual Salience A",
      term == "cong_patternA_only" ~ "Perceived Congruence: A_only",
      term == "cong_patternB_only" ~ "Perceived Congruence: B_only",
      TRUE ~ term
    )
  )
jpeg(file="Figure VII.jpeg", width=3300, height=3000, res=300)
ggplot(effects_combined, aes(x = prob_estimate, y = label, color = model)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = prob_conf.low, xmax = prob_conf.high),
                 position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(
    title = "Approximate Marginal Effects on Probability Scale",
    x = "Change in Predicted Probability",
    y = NULL,
    color = "Model"
  ) +
  theme_minimal(base_size = 14)
dev.off()

screenreg(list(m_dyadic, party_dyadic))

rm(dyad_base, dyad_party_base, dyads, dyads_party, effects_combined, 
   effects_m, effects_party, logit_to_prob)

#-----------------------Robustness checks--------------------------------------

# Run models with additional controls for issue ownership and specialism
pa_model_rb <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + seniority + 
                   party_io + specialist + V2 + (1|V3) + (1|SID), data = elites_long)

pa_party_rb <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                   seniority + party_io + specialist + V2 + (1|V3) + (1|SID) + (1|V4), 
                 data = elites_long)
screenreg(list(pa_model, pa_model_rb, pa_party, pa_party_rb))
htmlreg(list(pa_model, pa_model_rb, pa_party, pa_party_rb), file = "Table A11.1.doc")

# Run models by country

# Split data by country
elites_long <- elites_long[order(elites_long$V2),]

country_list <- unique(elites_long$V2)
country_split <- split(elites_long, elites_long$V2)

for (i in seq_along(country_list)) {
  assign(paste0(country_list[i]), country_split[[i]])}

# Run models for individual countries
pa_model_can <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|SID), data = Canada)
pa_model_fla <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|SID), data = Flanders)
pa_model_ger <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|SID), data = Germany)
pa_model_swz <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|SID), data = Switzerland[Switzerland$dis_small != 1,])
pa_model_wal <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|SID), data = Wallonia_or_Brussels)
screenreg(list(pa_model_can, pa_model_fla, pa_model_ger, pa_model_swz, pa_model_wal))
htmlreg(list(pa_model_can, pa_model_fla, pa_model_ger, pa_model_swz, pa_model_wal), file = "Table A11.2 country models.doc")

pa_party_can <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                       seniority + (1|V3) + (1|SID) + (1|V4), data = Canada[Canada$ele_small != 1,])
pa_party_fla <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                       seniority + (1|V3) + (1|SID) + (1|V4), data = Flanders[Flanders$ele_small != 1,])
pa_party_ger <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                       seniority + (1|V3) + (1|SID) + (1|V4), data = Germany[Germany$ele_small != 1,])
pa_party_swz <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                       seniority + (1|V3) + (1|SID) + (1|V4), data = Switzerland[Switzerland$ele_small != 1,])
pa_party_wal <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal +
                       seniority + (1|V3) + (1|SID) + (1|V4), data = Wallonia_or_Brussels[Wallonia_or_Brussels$ele_small != 1,])

r.squaredGLMM(pa_model_can)
r.squaredGLMM(pa_model_fla)
r.squaredGLMM(pa_model_ger)
r.squaredGLMM(pa_model_swz)
r.squaredGLMM(pa_model_wal)

r.squaredGLMM(pa_party_can)
r.squaredGLMM(pa_party_fla)
r.squaredGLMM(pa_party_ger)
r.squaredGLMM(pa_party_swz)
r.squaredGLMM(pa_party_wal)

screenreg(list(pa_party_can, pa_party_fla, pa_party_ger, pa_party_swz, pa_party_wal))
htmlreg(list(pa_party_can, pa_party_fla, pa_party_ger, pa_party_swz, pa_party_wal), file = "Table A11.3 Supporters models by country.doc")

rm(Canada, country_split, Flanders, Germany, pa_model_can, pa_model_fla, pa_model_ger,
   pa_model_rb, pa_model_swz, pa_model_wal, pa_party_can, pa_party_fla, pa_party_ger,
   pa_party_rb, pa_party_swz, pa_party_wal, Switzerland, Wallonia_or_Brussels, country_list)

# Now run models by issue type

# Split data by issue type
elites_long <- elites_long[order(elites_long$issue_type),]
issue_list <- unique(elites_long$issue_type)
issue_split <- split(elites_long, elites_long$issue_type)

for (i in seq_along(issue_list)) {
  assign(paste0(issue_list[i]), issue_split[[i]])}

# Run models by issue type
econ_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|V2), data = Economic[Economic$dis_small != 1,])
defence_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V2), data = Defence_International[Defence_International$dis_small != 1,])
migration_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|V2), data = Migration[Migration$dis_small != 1,])
social_issues_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|V2), data = Social[Social$dis_small != 1,])
pol_reform_model <- lmer(salience_estimate ~ Q14 + perceived_voter_congruence + actual_salience + 
                       seniority + (1|V3) + (1|V2), data = Political_Reform[Political_Reform$dis_small != 1,])
screenreg(list(econ_model, defence_model, migration_model, social_issues_model, pol_reform_model))
htmlreg(list(econ_model, defence_model, migration_model, social_issues_model, 
             pol_reform_model), file = "Table A11.4 models by issue type.doc")

econ_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal + 
                     seniority + (1|V3) + (1|V2), data = Economic[Economic$ele_small != 1,])
defence_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal + 
                        seniority + (1|V2), data = Defence_International[Defence_International$ele_small != 1,])
migration_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal + 
                          seniority + (1|V3) + (1|V2), data = Migration[Migration$ele_small != 1,])
social_issues_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + 
                              ele_und_cal + seniority + (1|V3) + (1|V2), data = Social[Social$ele_small != 1,])
pol_reform_party <- lmer(supporters_salience_estimate ~ Q14 + perceived_party_congruence + ele_und_cal  + 
                           seniority + (1|V3) + (1|V2), data = Political_Reform[Political_Reform$ele_small != 1,])
screenreg(list(econ_party, defence_party, migration_party, social_issues_party, pol_reform_party))
htmlreg(list(econ_party, defence_party, migration_party, social_issues_party, 
             pol_reform_party), file = "Table A11.5 party models by issue type.doc")

r.squaredGLMM(econ_model)
r.squaredGLMM(defence_model)
r.squaredGLMM(migration_model)
r.squaredGLMM(social_issues_model)
r.squaredGLMM(pol_reform_model)

r.squaredGLMM(econ_party)
r.squaredGLMM(defence_party)
r.squaredGLMM(migration_party)
r.squaredGLMM(social_issues_party)
r.squaredGLMM(pol_reform_party)

rm(econ_party, defence_party, migration_party, social_issues_party, pol_reform_party,
   econ_model, defence_model, migration_model, social_issues_model, pol_reform_model,
   Defence_International, Economic, issue_split, Migration, modelA5i, Political_Reform,
   Social, i, issue_list)

# Now model predicted effects of variables of interest on magnitude of misperceptions

# Invert variables CHECK THESE
elites_long$salience <- 100-elites_long$actual_salience
elites_long$salience_support <- 100-elites_long$ele_und_cal

magnitude_model <- lmer(simple_distance ~ Q14 + perceived_voter_congruence + salience + seniority + 
                      V2 + (1|V3) + (1|SID), data = elites_long)

magnitude_model_support <- lmer(simple_distance_support ~ Q14 + perceived_party_congruence + salience_support +
                      seniority + V2 + (1|V3) + (1|SID) + (1|V4), 
                    data = elites_long)

screenreg(list(magnitude_model, magnitude_model_support))

r.squaredGLMM(magnitude_model)
r.squaredGLMM(magnitude_model_support)

empty_model_mag <- lm(simple_distance ~ NULL, data = elites_long)
AIC(empty_model_mag)
BIC(empty_model_mag)

empty_support_mag <- lm(simple_distance_support ~ NULL, data = elites_long)
AIC(empty_support_mag)
BIC(empty_support_mag)

htmlreg(list(magnitude_model, magnitude_model_support), 
        file = "Table A11.6 magnitude mispercetions as DV models.doc")

#----------------------OTHER FIGURES FOR APPENDICES-----------------------------

# Figure on min & max salience among party electorates for Table A2.1
party_undecided_min_max <- elites_long %>%                                       
  group_by(V2, SID) %>%
  summarise_at(vars(ele_und_cal),
               list(min = ~min(., na.rm = TRUE),
                    max = ~max(., na.rm = TRUE)))
party_undecided_min_max$real_min <- 100-party_undecided_min_max$min
party_undecided_min_max$real_max <- 100-party_undecided_min_max$max

party_support_min_max <- elites_long %>%                                       
  group_by(V2, SID) %>%
  summarise_at(vars(ele_agr_cal),
               list(min = ~min(., na.rm = TRUE),
                    max = ~max(., na.rm = TRUE)))

# Visualise variation in salience by policy statement

undecided_by_country <- elites_long[!is.na(elites_long$actual_salience),] %>%
  group_by(question_set, SID) %>%
  summarise_at(vars(actual_salience), list(salience = mean))

undecided_by_country$salience <- 100-undecided_by_country$salience

undecided_by_country <- undecided_by_country[order(undecided_by_country$question_set, 
                                                   undecided_by_country$salience),]

undecided_by_country <- undecided_by_country %>%
  arrange(question_set, desc(salience)) %>%
  mutate(SID_unique = paste(question_set, SID, sep = "_"),
         SID_unique = factor(SID_unique, levels = SID_unique))

jpeg(file="Figure A6i.jpeg", width=3000, height=3000, res=300)
ggplot(undecided_by_country, aes(x = SID_unique, y = salience, fill = question_set)) +
  geom_bar(stat = "identity", color = "black") +
  labs(fill = "Country & Question set") + 
  theme_classic() +
  xlab("Policy statement (unique SID)") +
  ylab("Proportion of undecideds") +
  theme(axis.text.x = element_blank()) 
dev.off()

# Test correlation personal importance and strength of opinion

elites_long$strong_opinion <- ifelse((elites_long$Q10 == 1)|
                                       (elites_long$Q10 == 4), 1, 0)
elites_long <- elites_long %>% replace_with_na(replace = list(Q14 = -99))

t.test(Q14 ~ strong_opinion, data = elites_long, var.equal = TRUE)

elite_salience_robust <- elites_long %>%
  group_by(strong_opinion) %>%
  summarise(
    mean_Q14 = mean(Q14, na.rm = TRUE),
    se_Q14 = sd(Q14, na.rm = TRUE) / sqrt(n())
  )

jpeg(file="Figure A7i.jpeg", width=3000, height=3000, res=300)
ggplot(elite_salience_robust, aes(x = factor(strong_opinion), y = mean_Q14)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = mean_Q14 - 2.576 * se_Q14, ymax = mean_Q14 + 2.576 * se_Q14), width = 0.2) +
  labs(
    title = "Mean Personal Salience to elites of issue with 99% CI",
    x = "Strong Opinion (0 = No, 1 = Yes)",
    y = "Mean Personal Salience"
  ) +
  ylim(2, 8) +
  theme_light()
dev.off()