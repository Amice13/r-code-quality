#### ---- Models to predict baseline levels of support for Bay du Nord 

library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(broom)
library(knitr)
library(magrittr)

setwd("/Users/srowa/Dropbox/Projects/Oil and gas cap/replication")

#### Load data ####

## Analysis data
full_field_analysis <- read_csv("input/full_field_analysis.csv")



#### Vignette models ####

# - First get each province*treatment baseline mean
m0 <- full_field_analysis %>% 
  lm(bdn_baseline ~ province_bucket*bdn_treatment, data = .)

# - Then estimate each province*treatment treatment effect, controlling for baseline
m1 <- full_field_analysis %>% 
  lm(bdn_update ~ province_bucket*bdn_treatment + bdn_baseline, data = .)

# - Get fitted values from baseline model
m0_pred <- predictions(model = m0,
                       newdata = datagrid(province_bucket = c("General population",
                                                              "Alberta",
                                                              "Newfoundland"),
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(province_bucket, bdn_treatment, 
         beta_baseline = estimate, 
         se_baseline = std.error, 
         lower_baseline = conf.low, 
         upper_baseline = conf.high) 

# - Get fitted values from vignette model
m1_pred <- predictions(model = m1,
                       newdata = datagrid(province_bucket = c("General population",
                                                              "Alberta",
                                                              "Newfoundland"),
                                          # Use each province*treatment baseline mean
                                          bdn_baseline = m0_pred$beta_baseline,
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(province_bucket, bdn_treatment, 
         bdn_baseline,
         beta_update = estimate, 
         se_update = std.error, 
         lower_update = conf.low, 
         upper_update = conf.high) 



## Combine and plot 
# - Retain only the matched province*treatment*baseline_mean triple from m1_pred
bdn_vignette_cate <- right_join(m1_pred, m0_pred, by = c("province_bucket", "bdn_treatment", "bdn_baseline" = "beta_baseline")) %>% 
  pivot_longer(names_to = "parameter", values_to = "value", bdn_baseline:upper_baseline) %>% 
  filter(str_detect(parameter, "lower")==F,
         str_detect(parameter, "upper")==F) %>% 
  mutate(timing = ifelse(str_detect(parameter, "baseline"), "Pre", "Post"),
         stat = ifelse(str_detect(parameter, "se_"), "se", "beta")) %>% 
  select(-parameter) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # Create plot index
  mutate(index = case_when(
    timing == "Pre" & province_bucket == "Alberta" ~ 1 ,
    timing == "Pre" & province_bucket == "Newfoundland" ~ 1 - 0.05,
    timing == "Pre" & province_bucket == "General population" ~ 1 + 0.05,
    timing == "Post" & province_bucket == "Alberta" ~ 2 ,
    timing == "Post" & province_bucket == "Newfoundland" ~ 2 - 0.05,
    timing == "Post" & province_bucket == "General population" ~ 2 + 0.05)) %>% 
  mutate(bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Distributive politics", "Strand AB oil"),
         bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Environmental stringency", "Stringent policy"),
         bdn_treatment = fct_relevel(bdn_treatment, "Strand AB oil", after = 0L),
         bdn_treatment = fct_relevel(bdn_treatment, "Stringent policy", after = 1L),
         province_bucket = fct_relevel(province_bucket, "General population", after = 2L)) %>% 
  # Begin plot
  ggplot(., aes(index, beta, color = province_bucket)) +
  facet_grid(cols = vars(bdn_treatment)) +
  geom_line(linetype = 2, linewidth = 0.25) +
  geom_pointrange(aes(xmin = index, xmax = index, 
                      ymin = beta - 1.96*se,
                      ymax = beta + 1.96*se,
                      shape = timing)) +
  scale_shape_manual(values = c(19, 1), guide = "none") +
  scale_color_manual(values = c("#d62d20", "#0057e7", "#008744")) +
  labs(x = NULL,
       y = "Average support for Bay du Nord",
       color = "Region") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Pre", "Post")) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(plot = bdn_vignette_cate, 
       filename = "output/bdn_vignette_cate.png", 
       dpi = 320, height = 1200, width = 2400, units = "px")



#### Standardized effects sizes ####

## Load pilot data
pilot_analysis <- read_csv("input/pilot_analysis.csv")

## Get effects within each province and condition in pilot
pilot_analysis %>% 
  group_by(province) %>%
  summarize(mean_bdn = mean(bdn, na.rm = T),
            mean_iea = mean(iea, na.rm = T),
            mean_cap = mean(cap, na.rm = T),
            sd_bdn = sd(bdn, na.rm = T),
            # sd_iea = sd(iea, na.rm = T),
            # sd_cap = sd(cap, na.rm = T),
            mean_diff1 = mean_iea - mean_bdn,
            mean_diff2 = mean_cap - mean_iea,
            # n = n(),
            cohen1 = mean_diff1/sd_bdn,
            cohen2 = mean_diff2/sd_bdn)
## -- For Bay du Nord baseline to IEA text:
#' Pooled (don't group), BdN to IEA: -0.212**
#' By province (group), BdN to IEA: -0.150, -0.215, -0.249, -0.251
## -- For IEA to emissions cap text:
#' Pooled (don't group), IEA to cap: -0.08
#' By province (group), IEA to cap: 0.159 (QC), 0.012, -0.00, -0.524** (AB)

## Get effects within each province and condition in full field
full_field_analysis %>% 
  # group_by(province_bucket, bdn_treatment) %>%
  group_by(bdn_treatment) %>%
  summarize(mean_baseline = mean(bdn_baseline, na.rm = T),
            mean_update = mean(bdn_update, na.rm = T),
            sd_baseline = sd(bdn_baseline, na.rm = T),
            # sd_update = sd(bdn_update, na.rm = T),
            mean_diff1 = mean_update - mean_baseline,
            # n = n(),
            cohen = mean_diff1/sd_baseline)

# Equivalent AB effect is now: -.238 (not -.524)
# And the generic effect of future prices/demand is -0.09 or -0.14 (not -0.212)




