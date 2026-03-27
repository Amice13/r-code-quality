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


#### With Atlantic Canada ####

# - First get each province*treatment baseline mean
n0 <- full_field_analysis %>% 
  mutate(province_bucket_atlantic = ifelse(province %in% c("NL", "NB", "NS", "PEI"), 
                                           "Atlantic", province_bucket)) %>% 
  lm(bdn_baseline ~ province_bucket_atlantic*bdn_treatment, data = .)

# - Then estimate each province*treatment treatment effect, controlling for baseline
n1 <- full_field_analysis %>% 
  mutate(province_bucket_atlantic = ifelse(province %in% c("NL", "NB", "NS", "PEI"), 
                                           "Atlantic", province_bucket)) %>% 
  lm(bdn_update ~ province_bucket_atlantic*bdn_treatment + bdn_baseline, data = .)

# - Get fitted values from baseline model
n0_pred <- predictions(model = n0,
                       newdata = datagrid(province_bucket_atlantic = c("General population",
                                                              "Alberta",
                                                              "Atlantic"),
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(province_bucket_atlantic, bdn_treatment, 
         beta_baseline = estimate, 
         se_baseline = std.error, 
         lower_baseline = conf.low, 
         upper_baseline = conf.high) 

# - Get fitted values from vignette model
n1_pred <- predictions(model = n1,
                       newdata = datagrid(province_bucket_atlantic = c("General population",
                                                              "Alberta",
                                                              "Atlantic"),
                                          # Use each province*treatment baseline mean
                                          bdn_baseline = n0_pred$beta_baseline,
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(province_bucket_atlantic, bdn_treatment, 
         bdn_baseline,
         beta_update = estimate, 
         se_update = std.error, 
         lower_update = conf.low, 
         upper_update = conf.high) 



## Combine and plot 
# - Retain only the matched province*treatment*baseline_mean triple from n1_pred
bdn_vignette_atlantic <- right_join(n1_pred, n0_pred, by = c("province_bucket_atlantic",
                                    "bdn_treatment",
                                    "bdn_baseline" = "beta_baseline")) %>% 
  pivot_longer(names_to = "parameter", values_to = "value", 
               bdn_baseline:upper_baseline) %>% 
  filter(str_detect(parameter, "lower")==F,
         str_detect(parameter, "upper")==F) %>% 
  mutate(timing = ifelse(str_detect(parameter, "baseline"), "Pre", "Post"),
         stat = ifelse(str_detect(parameter, "se_"), "se", "beta")) %>% 
  select(-parameter) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # Create plot index
  mutate(index = case_when(
    timing == "Pre" & province_bucket_atlantic == "Alberta" ~ 1 ,
    timing == "Pre" & province_bucket_atlantic == "Atlantic" ~ 1 - 0.05,
    timing == "Pre" & province_bucket_atlantic == "General population" ~ 1 + 0.05,
    timing == "Post" & province_bucket_atlantic == "Alberta" ~ 2 ,
    timing == "Post" & province_bucket_atlantic == "Atlantic" ~ 2 - 0.05,
    timing == "Post" & province_bucket_atlantic == "General population" ~ 2 + 0.05)) %>% 
  mutate(bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Distributive politics", "Strand AB oil"),
         bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Environmental stringency", "Stringent policy"),
         bdn_treatment = fct_relevel(bdn_treatment, "Strand AB oil", after = 0L),
         bdn_treatment = fct_relevel(bdn_treatment, "Stringent policy", after = 1L),
         province_bucket_atlantic = fct_relevel(province_bucket_atlantic, "General population", after = 2L)) %>% 
  # Begin plot
  ggplot(., aes(index, beta, color = province_bucket_atlantic)) +
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
ggsave(plot = bdn_vignette_atlantic, 
       filename = "output/bdn_vignette_atlantic.png", 
       dpi = 320, height = 1200, width = 2400, units = "px")



#### With distance to extraction ####

# - First get each distance*treatment baseline mean
o0 <- full_field_analysis %>% 
  lm(bdn_baseline ~ distance_bucket*bdn_treatment, data = .)

# - Then estimate each distance*treatment treatment effect, controlling for baseline
o1 <- full_field_analysis %>% 
  lm(bdn_update ~ distance_bucket*bdn_treatment + bdn_baseline, data = .)

# - Get fitted values from baseline model
o0_pred <- predictions(model = o0,
                       newdata = datagrid(distance_bucket = c("<100km",
                                                              "<350km",
                                                              ">350km"),
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(distance_bucket, bdn_treatment, 
         beta_baseline = estimate, 
         se_baseline = std.error, 
         lower_baseline = conf.low, 
         upper_baseline = conf.high) 

# - Get fitted values from vignette model
o1_pred <- predictions(model = o1,
                       newdata = datagrid(distance_bucket = c("<100km",
                                                              "<350km",
                                                              ">350km"),
                                          # Use each distance*treatment baseline mean
                                          bdn_baseline = o0_pred$beta_baseline,
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(distance_bucket, bdn_treatment, 
         bdn_baseline,
         beta_update = estimate, 
         se_update = std.error, 
         lower_update = conf.low, 
         upper_update = conf.high) 



## Combine and plot 
# - Retain only the matched distance*treatment*baseline_mean triple from o1_pred
bdn_vignette_distance <- right_join(o1_pred, o0_pred, by = c("distance_bucket",
                                    "bdn_treatment",
                                    "bdn_baseline" = "beta_baseline")) %>% 
  pivot_longer(names_to = "parameter", values_to = "value", 
               bdn_baseline:upper_baseline) %>% 
  filter(str_detect(parameter, "lower")==F,
         str_detect(parameter, "upper")==F) %>% 
  mutate(timing = ifelse(str_detect(parameter, "baseline"), "Pre", "Post"),
         stat = ifelse(str_detect(parameter, "se_"), "se", "beta")) %>% 
  select(-parameter) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # Create plot index
  mutate(index = case_when(
    timing == "Pre" & distance_bucket == "<100km" ~ 1 ,
    timing == "Pre" & distance_bucket == "<350km" ~ 1 - 0.05,
    timing == "Pre" & distance_bucket == ">350km" ~ 1 + 0.05,
    timing == "Post" & distance_bucket == "<100km" ~ 2 ,
    timing == "Post" & distance_bucket == "<350km" ~ 2 - 0.05,
    timing == "Post" & distance_bucket == ">350km" ~ 2 + 0.05)) %>% 
  mutate(bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Distributive politics", "Strand AB oil"),
         bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Environmental stringency", "Stringent policy"),
         bdn_treatment = fct_relevel(bdn_treatment, "Strand AB oil", after = 0L),
         bdn_treatment = fct_relevel(bdn_treatment, "Stringent policy", after = 1L),
         distance_bucket = fct_relevel(distance_bucket, ">350km", after = 2L)) %>% 
  # Begin plot
  ggplot(., aes(index, beta, color = distance_bucket)) +
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
       color = "Distance from extraction") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Pre", "Post")) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(plot = bdn_vignette_distance, 
       filename = "output/bdn_vignette_distance.png",
       dpi = 320, height = 1200, width = 2400, units = "px")


#### ** Now do family and friends 

# - First get each ffff*treatment baseline mean
p0 <- full_field_analysis %>% 
  lm(bdn_baseline ~ ffff_bucket*bdn_treatment, data = .)

# - Then estimate each ffff*treatment treatment effect, controlling for baseline
p1 <- full_field_analysis %>% 
  lm(bdn_update ~ ffff_bucket*bdn_treatment + bdn_baseline, data = .)

# - Get fitted values from baseline model
p0_pred <- predictions(model = p0,
                       newdata = datagrid(ffff_bucket = c("0",
                                                          "1-4",
                                                          "5+"),
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(ffff_bucket, bdn_treatment, 
         beta_baseline = estimate, 
         se_baseline = std.error, 
         lower_baseline = conf.low, 
         upper_baseline = conf.high) 

# - Get fitted values from vignette model
p1_pred <- predictions(model = p1,
                       newdata = datagrid(ffff_bucket = c("0",
                                                          "1-4",
                                                          "5+"),
                                          # Use each ffff*treatment baseline mean
                                          bdn_baseline = p0_pred$beta_baseline,
                                          bdn_treatment = c("Distributive politics",
                                                            "Environmental stringency",
                                                            "Future demand",
                                                            "Future prices"))) %>% 
  as.data.frame() %>% 
  select(ffff_bucket, bdn_treatment, 
         bdn_baseline,
         beta_update = estimate, 
         se_update = std.error, 
         lower_update = conf.low, 
         upper_update = conf.high) 


# - Retain only the matched distance*treatment*baseline_mean triple from o1_pred
bdn_vignette_ffff <- right_join(p1_pred, p0_pred, by = c("ffff_bucket",
                                    "bdn_treatment",
                                    "bdn_baseline" = "beta_baseline")) %>% 
  pivot_longer(names_to = "parameter", values_to = "value", 
               bdn_baseline:upper_baseline) %>% 
  filter(str_detect(parameter, "lower")==F,
         str_detect(parameter, "upper")==F) %>% 
  mutate(timing = ifelse(str_detect(parameter, "baseline"), "Pre", "Post"),
         stat = ifelse(str_detect(parameter, "se_"), "se", "beta")) %>% 
  select(-parameter) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # Create plot index
  mutate(index = case_when(
    timing == "Pre" & ffff_bucket == "0" ~ 1 + 0.05,
    timing == "Pre" & ffff_bucket == "1-4" ~ 1 ,
    timing == "Pre" & ffff_bucket == "5+" ~ 1 - 0.05,
    timing == "Post" & ffff_bucket == "0" ~ 2 + 0.05,
    timing == "Post" & ffff_bucket == "1-4" ~ 2 ,
    timing == "Post" & ffff_bucket == "5+" ~ 2 - 0.05)) %>% 
  mutate(bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Distributive politics", "Strand AB oil"),
         bdn_treatment = replace(bdn_treatment, 
                                 bdn_treatment=="Environmental stringency", "Stringent policy"),
         bdn_treatment = fct_relevel(bdn_treatment, "Strand AB oil", after = 0L),
         bdn_treatment = fct_relevel(bdn_treatment, "Stringent policy", after = 1L),
         ffff_bucket = fct_relevel(ffff_bucket, "5+", after = 2L)) %>% 
  # Begin plot
  ggplot(., aes(index, beta, color = ffff_bucket)) +
  facet_grid(cols = vars(bdn_treatment)) +
  geom_line(linetype = 2, linewidth = 0.25) +
  geom_pointrange(aes(xmin = index, xmax = index, 
                      ymin = beta - 1.96*se,
                      ymax = beta + 1.96*se,
                      shape = timing)) +
  scale_shape_manual(values = c(19, 1), guide = "none") +
  scale_color_manual(values = c("#008744", "#0057e7", "#d62d20")) +
  labs(x = NULL,
       y = "Average support for Bay du Nord",
       color = "FF family and friends") +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Pre", "Post")) +
  theme_classic() +
  theme(legend.position = "bottom")
ggsave(plot = bdn_vignette_ffff,
       filename = "output/bdn_vignette_ffff.png", 
       dpi = 320, height = 1200, width = 2400, units = "px")


#### Pilot ####

pilot_analysis <- read_csv("input/pilot_analysis.csv")

# Plot
pilot_vignettes <- pilot_analysis  %>% 
  group_by(province) %>% 
  summarize(mean_bdn = mean(bdn, na.rm = T),
            mean_iea = mean(iea, na.rm = T),
            mean_cap = mean(cap, na.rm = T),
            n_prov = n(),
            se_bdn = sd(bdn, na.rm = T)/sqrt(n_prov),
            se_iea = sd(iea, na.rm = T)/sqrt(n_prov),
            se_cap = sd(cap, na.rm = T)/sqrt(n_prov)) %>% 
  pivot_longer(names_to = "stat", values_to = "value", mean_bdn:se_cap) %>% 
  filter(stat != "n_prov") %>% 
  separate(stat, into = c("stat", "question")) %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(index = case_when(question == "bdn" ~ 1,
                           question == "iea" ~ 2,
                           question == "cap" ~ 3)) %>% 
  mutate(index = case_when(province == "BC" ~ index-.05,
                           province == "AB" ~ index-.025,
                           province == "ON" ~ index+.025,
                           province == "QC" ~ index+.05)) %>% 
  ggplot(., aes(index, mean, 
                color = province, shape = province, linetype = province)) +
  scale_x_continuous(breaks = seq(1,3,1),
                     labels = c("Q1.\nBay du Nord", 
                                "Q2.\nIEA vignette", 
                                "Q3.\nDomestic\npolitics\nvignette"), 
                     name = NULL) +
  # scale_y_continuous(limits = c(4, 7.5)) +
  scale_color_manual(values = c("blue", "#3BB143", "#4F7942", "#50C878")) +
  scale_shape_manual(values = c(19, 5, 2, 1)) +
  scale_linetype_manual(values = c(1, 5, 5, 5)) +
  geom_segment(aes(x = index, xend = index,
                   y = mean-1.96*se, yend = mean+1.96*se),
               linetype = 1) +
  labs(color = "Province", shape = "Province", linetype = "Province",
       x = NULL,
       y = "Average support for\nBay du Nord project") +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = "bottom") +
  geom_line() +
  geom_point(size = 3, color = "white", shape = 19) +
  geom_point(size = 1.5) 
ggsave(plot = pilot_vignettes, 
       filename = "output/vignette_pilot.png", 
       dpi = 300, width = 6, height = 4.16)
