#### ---- 

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(broom)
library(knitr)
library(magrittr)
library(gridExtra)

setwd("/Users/srowa/Dropbox/Projects/Oil and gas cap/replication")

#### Load data ####

## Analysis data
full_field_analysis <- read_csv("input/full_field_analysis.csv")


#### Carbon intensity #### 

c0 <- full_field_analysis %>% 
  lm(bdn_shutdown ~ bdn_baseline + carbonintensity_treatment, data = .)

c1 <- full_field_analysis %>% 
  filter(bdn_treatment != "Distributive politics") %>%
  lm(bdn_shutdown ~ bdn_baseline + carbonintensity_treatment, data = .)

c2 <- full_field_analysis %>% 
  mutate(province_bucket = fct_relevel(province_bucket, "General population", after = 0L)) %>% 
  lm(bdn_shutdown ~ bdn_baseline + carbonintensity_treatment*province_bucket, data = .)

c3 <- full_field_analysis %>% 
  filter(bdn_treatment != "Distributive politics") %>%
  mutate(province_bucket = fct_relevel(province_bucket, "General population", after = 0L)) %>% 
  lm(bdn_shutdown ~ bdn_baseline + carbonintensity_treatment*province_bucket, data = .)


coefficients_renamed = c(
  "carbonintensity_treatmentLifecycle × province_bucketAlberta" = "Lifecycle x Alberta",
  "carbonintensity_treatmentLifecycle × province_bucketNewfoundland" = "Lifecycle x Newfoundland",
  "carbonintensity_treatmentProduction × province_bucketAlberta" = "Production x Alberta",
  "carbonintensity_treatmentProduction × province_bucketNewfoundland" = "Production x Newfoundland",
  "carbonintensity_treatmentLifecycle" = "Lifecycle",
  "carbonintensity_treatmentProduction" = "Production",
  "province_bucketAlberta" = "Alberta",
  "province_bucketGeneral population" = "General population",
  "province_bucketNewfoundland" = "Newfoundland",
  "bdn_baseline" = "Bay du Nord baseline",
  "(Intercept)" = "(Intercept)")

gm <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "r.squared", "R2", 2,
  "nobs", "Observations", 0)

modelsummary(models = list(c0, c2, c1, c3),
             output = "output/carbon_intensity.docx",
             fmt = 2,
             coef_rename = coefficients_renamed,
             # coef_omit = c(""),
             gof_map = gm, 
             notes = list("Outcome is support for shutting down oil production outside NL (0-10)",
                          "Reference category is combustion emissions for the vignette",
                          "Reference category is general population for regions",
                          "Standard errors in parentheses"),
             stars = c("*" = 0.05, "**" = 0.01))
#' Interpretation
#' The carbon-intensity of *production* vignette increases support for shutting down oil production
#' The carbon-intensity of *lifecycle* vignette also increases support for shutting down oil production, but less
#' There are large baseline differences between AB and the rest of Canada on this
#' But the effect of the vignettes is not different (moderated) by AB, 
#' -- as given by not statistically significant coefficients on the interaction terms
#' The effects are the same when we drop respondents who are 'treated twice' with the stranded assets vignette
  




#### Environment v. economy ####

## Baseline model
s0 <- full_field_analysis %>% 
  filter(province == "AB") %>%
  filter(bdn_treatment %in% c(unique(full_field_analysis$bdn_treatment)[c(1,2)])) %>%
  lm(bdn_baseline ~ climate_support, data = .) 

## Update model
s1 <- full_field_analysis %>% 
  filter(province == "AB") %>%
  filter(bdn_treatment %in% c(unique(full_field_analysis$bdn_treatment)[c(1:2)])) %>%
  lm(bdn_update ~ bdn_baseline + bdn_treatment*climate_support + 0, data = .) 

## Get fitted values
s0_fitted <- predictions(model = s0,
                         newdata = datagrid(
                           climate_support = seq(0, 10, 1))) %>% 
  as.data.frame() %>% 
  select(climate_support, estimate, conf.low, conf.high) %>% 
  mutate(model = "Baseline")

s1_fitted <- predictions(model = s1,
                         newdata = datagrid(
                           bdn_treatment = unique(s1$xlevels)[[1]],
                           climate_support = seq(0, 10, 1))) %>% 
  as.data.frame() %>% 
  select(model = bdn_treatment, climate_support, estimate, conf.low, conf.high)  

## Combine and plot
ab_by_vignette <- bind_rows(s0_fitted, s1_fitted) %>% 
  mutate(model = ifelse(model == "Distributive politics", "Strand AB oil", model),
         model = replace(model, model == "Environmental stringency", "Policy stringency")) %>% 
  ggplot(., aes(climate_support, estimate, fill = model)) +
  geom_ribbon(aes(x = climate_support, 
                  ymin = conf.low,
                  ymax = conf.high),
              alpha = 0.25, color = "white") +
  geom_line(aes(color = model)) +
  scale_fill_manual(values = c("black", "#008744", "#d62d20")) +
  scale_color_manual(values = c("black", "#008744", "#d62d20")) +
  scale_y_continuous(breaks = seq(4, 9, 1),
                     limits = c(4, 9)) +
  labs(y = "Support for Bay du Nord", 
       x = "Support for climate policy",
       fill = "Bay du Nord vignette",
       color = "Bay du Nord vignette") +
  theme_classic() +
  # guides(fill = guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "bottom", legend.margin=margin())
ggsave(plot = ab_by_vignette,
       filename = "output/ab_by_vignette.png", 
       dpi = 320, height = 1200, width = 2400, units = "px")

#' : In Alberta
#' Support for Bay du Nord falls in the environmental stringency condition
#' -- as people get more supportive of climate policy
#' Support for Bay du Nord is flat in the distributive politics condition
#' -- across levels of support for climate policy
#' -- --> Suggests that climate concerns don't outweigh economic material concerns
#' -- --> If climate concerns ranked ahead of economic ones, 
#' -- -- --> then we'd see the same negative slope





#### Negative affect toward Alberta #### 

full_field_analysis %>% 
  filter(province != "AB") %>%
  mutate(negative_ab = therm_ownprov - therm_ab) %>%
  select(province, negative_ab, therm_ownprov, therm_ab)
# Higher values: Like own province much more than AB, dislike AB

## Check there are observations across full range
# full_field_analysis %>% 
#   filter(province != "AB") %>%
#   mutate(negative_ab = therm_ownprov - therm_ab) %>%
#   mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
#                                    "Distributive", "Other")) %>%
#   ggplot(., aes(negative_ab, fill = bdn_distributive)) +
#   scale_fill_manual(values = c("#d62d20", "#008744")) +
#   geom_histogram(bins = 21) +
#   labs(x = "Negative affect toward Alberta",
#        y = "Count",
#        color = "Bay du Nord vignette",
#        # caption = "Higher x-values are more negative sentiment toward AB than own province",
#        fill = "Bay du Nord vignette") +
#   theme_classic() +
#   theme(legend.position = "bottom")


a0 <- full_field_analysis %>% 
  filter(province != "AB") %>%
  mutate(negative_ab = therm_ownprov - therm_ab) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Strand AB oil", "Other")) %>%
  lm(bdn_baseline ~ bdn_distributive*negative_ab, data = .)
# summary(a0)

a0_fitted <- predictions(model = a0,
            newdata = datagrid(negative_ab = seq(-10, 10, 1))) %>% 
  as.data.frame() %>% 
  select(negative_ab, estimate, starts_with("conf")) %>% 
  mutate(model = "Baseline") 

a1 <- full_field_analysis %>% 
  filter(province != "AB") %>%
  mutate(negative_ab = therm_ownprov - therm_ab) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Strand AB oil", "Other")) %>%
  filter(bdn_treatment %in% c("Distributive politics", "Environmental stringency")) %>% 
  lm(bdn_update ~ bdn_baseline + bdn_distributive*negative_ab, data = .)

a1_fitted <- predictions(model = a1,
                         newdata = datagrid(bdn_distributive = c("Strand AB oil", "Other"),
                                            negative_ab = seq(-10, 10, 1))) %>% 
  as.data.frame() %>% 
  select(model = bdn_distributive, negative_ab, estimate, starts_with("conf"))

negative_ab <- bind_rows(a0_fitted, a1_fitted) %>% 
  ggplot(., aes(negative_ab, estimate, color = model, fill = model)) +
  geom_ribbon(aes(x = negative_ab, 
                  ymin = conf.low, ymax = conf.high), 
              color = "white", alpha = 0.25) +
  geom_line() +
  scale_fill_manual(values = c("black", "#008744", "#d62d20")) +
  scale_color_manual(values = c("black", "#008744", "#d62d20")) + 
  labs(x = "Negative affect toward Alberta",
       y = "Support for Bay du Nord",
       color = "Bay du Nord vignette",
       # title = "Non-AB, regional grievance",
       # caption = "Higher x-values are more negative sentiment toward AB than own province",
       fill = "Bay du Nord vignette") +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(plot = negative_ab,
       filename = "output/negative_ab.png", 
       dpi = 320, height = 1200, width = 2400, units = "px")

## Effect size

#' Calculate the difference of the stranded assets vignette compared to baseline at quantity of interest
bind_rows(a0_fitted, a1_fitted) %>% 
  # Roughly 90th percentile
  filter(negative_ab == 5) %>% 
  select(model, estimate) %>% 
  pivot_wider(names_from = model, values_from = estimate) %>% 
  mutate(effect = `Strand AB oil` - Baseline) %>% 
  pull(effect)
# 1.14

#' Get the standard deviation of the residuals in the baseline model and update model

# Baseline model
a0$call
full_field_analysis %>% 
  filter(province != "AB") %>%
  mutate(negative_ab = therm_ownprov - therm_ab) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Distributive", "Other")) %>%
  # from a0
  mutate(resid_bdn_baseline = resid(lm(bdn_baseline ~ negative_ab*bdn_distributive, data = .))) %>% 
  summarize(sd_resid = sd(resid_bdn_baseline)) %>% 
  pull(sd_resid)
# 2.577062

# Update model
a1$call
full_field_analysis %>% 
  filter(province != "AB") %>%
  mutate(negative_ab = therm_ownprov - therm_ab) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Distributive", "Other")) %>%
  # from a0
  mutate(resid_bdn_update = resid(lm(bdn_update ~ bdn_baseline + negative_ab*bdn_distributive, data = .))) %>% 
  summarize(sd_resid = sd(resid_bdn_update)) %>% 
  pull(sd_resid)
# 1.720411

#' Now get these together in a standardized effect size
#' mean2 - mean1 / sd(pooled)
1.148482 / sqrt((1.720411^2) + (2.577062^2))
# 0.3706505
#' There's your standardized effect size



#### Same again within Alberta ####

# full_field_analysis %>% 
#   # filter(province == "AB") %>%
#   mutate(negative_ab = therm_ownprov - therm_ab,
#          negative_in_ab = therm_mb - therm_ownprov) %>%
#   select(province, therm_ownprov, therm_ab, therm_mb, negative_ab, negative_in_ab) %>% 
#   View()
# Higher values: Like own province much more than AB, dislike AB

## Check there are observations across full range
# full_field_analysis %>%
#   filter(province == "AB") %>%
#   mutate(negative_in_ab = therm_mb - therm_ownprov) %>%
#   mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
#                                    "Distributive", "Other")) %>%
#   ggplot(., aes(negative_in_ab, fill = bdn_distributive)) +
#   scale_fill_manual(values = c("#d62d20", "#008744")) +
#   geom_histogram(bins = 21, color = "black") +
#   labs(x = "Negative affect toward Alberta (among Albertans)",
#        y = "Count",
#        color = "Bay du Nord vignette",
#        # caption = "Higher x-values are more negative sentiment toward AB than own province",
#        fill = "Bay du Nord vignette") +
#   theme_classic() +
#   theme(legend.position = "bottom")


b0 <- full_field_analysis %>% 
  # filter(province != "AB") %>%
  mutate(negative_in_ab = therm_mb - therm_ownprov) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Strand AB oil", "Other")) %>%
  lm(bdn_baseline ~ bdn_distributive*negative_in_ab, data = .)


b0_fitted <- predictions(model = b0,
                         newdata = datagrid(negative_in_ab = seq(-10, 10, 1))) %>% 
  as.data.frame() %>% 
  select(negative_in_ab, estimate, starts_with("conf")) %>% 
  mutate(model = "Baseline") 

b1 <- full_field_analysis %>% 
  mutate(negative_in_ab = therm_mb - therm_ownprov) %>%
  mutate(bdn_distributive = ifelse(str_detect(bdn_treatment, "Distributive"),
                                   "Strand AB oil", "Other")) %>%
  filter(bdn_treatment %in% c("Distributive politics", "Environmental stringency")) %>% 
  lm(bdn_update ~ bdn_baseline + bdn_distributive*negative_in_ab, data = .)

b1_fitted <- predictions(model = b1,
                         newdata = datagrid(bdn_distributive = c("Strand AB oil", "Other"),
                                            negative_in_ab = seq(-10, 10, 1))) %>% 
  as.data.frame() %>% 
  select(model = bdn_distributive, negative_in_ab, estimate, starts_with("conf"))

## Plot
negative_ab_in_ab <- bind_rows(b0_fitted, b1_fitted) %>% 
  # filter(str_detect(model, "Other")==F) %>%
  ggplot(., aes(negative_in_ab, estimate, color = model, fill = model)) +
  geom_ribbon(aes(x = negative_in_ab, 
                  ymin = conf.low, ymax = conf.high), 
              color = "white", alpha = 0.25) +
  geom_line() +
  scale_fill_manual(values = c("black", "#008744", "#d62d20")) +
  scale_color_manual(values = c("black", "#008744", "#d62d20")) + 
  labs(x = "Negative affect toward Alberta",
       y = "Support for Bay du Nord",
       color = "Bay du Nord vignette",
       # title = "In AB, regional grievance",
       # caption = "Higher x-values are more negative sentiment toward AB than own province",
       fill = "Bay du Nord vignette") +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(plot = negative_ab_in_ab,
       filename = "output/negative_ab_in_ab.png",
       dpi = 320, height = 1200, width = 2400, units = "px")


#### Fairness #### 

## Check there are observations across full range
# full_field_analysis %>% 
#   mutate(redistribution_over_dividends = redistribution - dividends) %>% 
#   ggplot(., aes(redistribution_over_dividends, fill = province_bucket)) +
#   scale_fill_manual(values = c("#d62d20", "#0057e7", "#008744")) +
#   geom_histogram(bins = 21) +
#   labs(x = "Redistribution over dividends",
#        y = "Count",
#        color = "Region",
#        fill = "Region") +
#   theme_classic() +
#   theme(legend.position = "bottom")


## Separate models, robustness
t1 <- full_field_analysis %>% 
  lm(bdn_shutdown ~ province_bucket + redistribution, data = .)

t2 <- full_field_analysis %>% 
  mutate(redistribution_over_dividends = redistribution - dividends) %>% 
  lm(bdn_shutdown ~ redistribution_over_dividends + province_bucket , data = .)

t3 <- full_field_analysis %>% 
  mutate(redistribution_over_retraining = redistribution - retraining) %>% 
  lm(bdn_shutdown ~ redistribution_over_retraining + province_bucket, data = .)

t4 <- full_field_analysis %>% 
  mutate(redistribution_over_diversification = redistribution - diversification) %>% 
  lm(bdn_shutdown ~ redistribution_over_diversification + province_bucket , data = .)

t5 <- full_field_analysis %>% 
  mutate(redistribution_over_ret_div = (redistribution+diversification)/2) %>% 
  lm(bdn_shutdown ~ redistribution_over_ret_div + province_bucket , data = .)

## Rename terms
coefficients_renamed <- c(
  "redistribution" = "Redistribution",
  "redistribution_over_dividends" = "Redistribution - Dividends",
  "redistribution_over_retraining" = "Redistribution - Retraining",
  "redistribution_over_diversification" = "Redistribution - Diversification",
  "redistribution_over_ret_div" = "Redistribution - (Retrain + Divers.)/2",
  "province_bucketGeneral population" = "General population",
  "province_bucketAlberta" = "Alberta",
  "province_bucketNewfoundland" = "Newfoundland")

## Define table settings
gm <- tibble::tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "Observations", 0,
  "r.squared", "R2", 2)


## Combine in table
modelsummary(models = list(t2, t1, t3, t4, t5), 
             output = "output/fairness.docx",
             fmt = 2,
             coef_rename = coefficients_renamed,
             # coef_omit = c("Alberta|Newfoundland|General population"),
             gof_map = gm, 
             notes = list("Outcome is support for shutting down oil production outside NL",
                          "Reference category is general population for regions",
                          "Standard errors in parentheses"),
             stars = c("*" = 0.05, "**" = 0.01))





## Model predictions and plot
# predictions(model = t2, 
#             newdata = datagrid(
#               redistribution_over_dividends = seq(-100, 100, 10),
#               province_bucket = c("Alberta", "Newfoundland", "General population"))) %>% 
#   select(redistribution_over_dividends, province_bucket, estimate, conf.low, conf.high) %>% 
#   as.data.frame() %>% 
#   ggplot(., aes(redistribution_over_dividends, estimate, 
#                 color = province_bucket,
#                 fill = province_bucket)) +
#   geom_ribbon(aes(x = redistribution_over_dividends,
#                   ymin = conf.low, ymax = conf.high),
#               alpha = 0.25, color = "white") +
#   scale_fill_manual(values = c("#d62d20", "#0057e7", "#008744")) +
#   scale_color_manual(values = c("#d62d20", "#0057e7", "#008744")) +
#   scale_y_continuous(limits = c(1,8.5)) +
#   geom_line(aes(color = province_bucket)) +
#   labs(x = "Preference for redistribution over dividends",
#        y = "Support for shutting down oil production\noutside Newfoundland",
#        color = "Region",
#        fill = "Region") +
#   theme_classic() +
#   theme(legend.position = "bottom")
# ggsave(filename = "output/redistribution_over_dividends.png", dpi = 320, height = 1200, width = 2400, units = "px")



#### Internal migration #### 

full_field_analysis %>% 
  mutate(ab_from_elsewhere = ifelse(province == "AB" & raised_province != "AB", 1, 0),
         ab_from_nl = ifelse(province == "AB" & raised_province == "NL", 1, 0),
         ab_from_atlantic = ifelse(province == "AB" & 
                                     raised_province %in% c("NL", "NS", "NB", "PEI"), 1, 0)) %>% 
  filter(province == "AB") %>%
  # lm(bdn_shutdown ~ carbonintensity_treatment + ab_from_elsewhere, data = .) 
  summarize(n_from_elsewhere = sum(ab_from_elsewhere),
            n_from_nl = sum(ab_from_nl),
            n_from_atlantic = sum(ab_from_atlantic))
# There are not many of these people ... 222 elsewhere; 4 NL; 12 Atlantic ...




