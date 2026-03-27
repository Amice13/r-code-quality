#### Aggregate analysis SM figures ############

### This code produces background figures and plots from the supplementary materials (Figure SM1-8)
### In places, this uses pre-loaded results that can be found in aggregate_analysis_PT.R respectively.
### The relevant source file is noted where relevant in the code below.

## Clear environment

rm(list=ls())

# Load packages (note: code to install packages in file aggregate_analysis_TWFE.R)

library(ggplot2)
library(dplyr)
library(purrr)
library(fixest)
library(stringr)
library(tidyr)
library(broom)
library(farver)


#### Background figures (supplementary materials section A)

## SM1 (validating with self-reported coverage use)

rm(list=ls())

mobile_use_data_rural <- readRDS("mobile_use_data_rural.rds")

rural_base1 <- feols(mobile_bin ~ inside + ever_inside | country_name + year,
                     cluster = ~ ea_code,
                     data=mobile_use_data_rural)

rural_base2 <- feols(mobile_use_std ~ inside + ever_inside | country_name + year, 
                     cluster = ~ ea_code,
                     data=mobile_use_data_rural)

rural_covs1 <- feols(mobile_bin ~ inside + ever_inside + 
                       news_radio + news_tv + news_paper + 
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year, 
                     cluster = ~ ea_code,
                     data=mobile_use_data_rural)

rural_covs2 <- feols(mobile_use_std ~ inside + ever_inside + 
                       news_radio + news_tv + news_paper + 
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year, 
                     cluster = ~ ea_code,
                     data=mobile_use_data_rural)

mob_list <- list(rural_base1, rural_base2, 
                 rural_covs1, rural_covs2)

mob_df <- map_df(mob_list, broom::tidy, .id="model") %>%
  filter(term == "inside") %>% 
  mutate(conf.low = estimate - 1.96*std.error, conf.high = estimate + 1.96*std.error,
         model = c("Baseline", "Baseline", "Covariates", "Covariates"),
         outcome = c(rep(c("\nEver use\n(% probability)", 
                           "\nFrequency of use \n(% standard deviation)"), 2)))

fig_SM1 <- ggplot(mob_df, aes(y=outcome, x=estimate, col=model)) +
  geom_point(position = position_dodge(width=0.5)) +
  geom_vline(xintercept=0, linetype=2) +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high, height=0, width = 0),
                 position = position_dodge(width=0.5)) +
  theme_bw() +
  scale_colour_manual(values = c("darkgrey", "darkred")) +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip() +
  xlab("\nEffect on self-reported use") +
  scale_x_continuous(limits = c(0, 0.24), 
                     breaks = c(0, 0.1, 0.2), 
                     labels = c("0%", "10%", "20%")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM1 ## Print figure


## SM2 (distribution of trust outcomes)

rm(list=ls())

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

data_long <- ABcombined_rural %>%
  dplyr::select(trust_pres, trust_police, trust_ec, trust_courts) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  mutate(variable = factor(variable, levels = c("trust_pres", "trust_police", 
                                                "trust_ec", "trust_courts"),
                           labels = c("a) President\n", "b) Police\n", 
                                      "c) Electoral Commission\n", "d) Courts\n")))

data_long_prop <- data_long %>%
  group_by(variable, response) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(proportion = (count / sum(count)*100))

mean_values <- data_long %>%
  group_by(variable) %>%
  summarize(mean_response = mean(response, na.rm = TRUE), .groups = 'drop')

fig_SM2 <- ggplot(data_long_prop, aes(x = response, y = proportion)) +
  geom_col(color = "black", fill = "lightgrey", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  geom_vline(data = mean_values, aes(xintercept = mean_response), color = "darkred") +
  labs(x = "Response\n",
       y = "Proportion (%)\n") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM2 ## Print figure

## SM3 (distribution of trust indices)

rm(list=ls())

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

data_long <- ABcombined_rural %>%
  dplyr::select(new_mean_trust, new_mean_trust_2, new_mean_trust_bin) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
  mutate(variable = factor(variable, levels = c("new_mean_trust", 
                                                "new_mean_trust_2", 
                                                "new_mean_trust_bin"),
                           labels = c("a) Mean trust\n", 
                                      "b) Mean trust\n(excl. President)\n", 
                                      "c) Mean trust (binary)\n")))

data_long_prop <- data_long %>%
  group_by(variable, response) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(variable) %>%
  mutate(proportion = (count / sum(count))*100)

mean_values <- data_long %>%
  group_by(variable) %>%
  summarize(mean_response = mean(response, na.rm = TRUE), .groups = 'drop')

fig_SM3 <- ggplot(data_long_prop, aes(x = response, y = proportion)) +
  geom_col(color = "black", fill = "lightgrey", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  geom_vline(data = mean_values, aes(xintercept = mean_response), color = "darkred") +
  labs(y = "Proportion (%)\n") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12), 
        axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM3 ## Print figure

## SM4 (distribution of relative coverage years)

rm(list=ls())

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

fig_SM4 <- ABcombined_rural %>% mutate(ever_treated = ifelse(time_to_inside == -10000, 
                                                  "Never treated", "Ever treated")) %>%
  filter(ever_treated == "Ever treated") %>%
  ggplot(aes(x=time_to_inside)) +
  geom_bar(col="black", fill = "darkgrey") +
  theme_bw() +
  xlab("Time to recieving coverage treatment (years)") +
  ylab("Count") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM4 ## Print figure


## SM5 and SM6 (pre-trends)
## Source file for saved results: aggregate_analysis_PT.R

rm(list=ls())
load("pt_results.RData") ## as made and saved in code above

results_pt_full <- results_pt_full %>%
  mutate(outcome = case_when(
    outcome == "Mean trust" ~ "a) Mean trust",
    outcome == "Mean trust (ex Pres)" ~ "b) Mean trust (ex Pres)",
    outcome == "Mean trust (binary)" ~ "c) Mean trust (binary)",
    outcome == "President" ~ "d) President",
    outcome == "President (binary)" ~ "e) President (binary)",
    outcome == "Police" ~ "f) Police",
    outcome == "Police (binary)" ~ "g) Police (binary)",
    outcome == "Election\nCommission" ~ "h) Election\nCommission",
    outcome == "Election\nCommission (binary)" ~ "i) Election\nCommission (binary)",
    outcome == "Courts" ~ "j) Courts",
    outcome == "Courts (binary)" ~ "k) Courts (binary)"
  ))

results_pt_bin <- results_pt_bin %>%
  mutate(outcome = case_when(
    outcome == "Mean trust" ~ "a) Mean trust",
    outcome == "Mean trust (ex Pres)" ~ "b) Mean trust (ex Pres)",
    outcome == "Mean trust (binary)" ~ "c) Mean trust (binary)",
    outcome == "President" ~ "d) President",
    outcome == "President (binary)" ~ "e) President (binary)",
    outcome == "Police" ~ "f) Police",
    outcome == "Police (binary)" ~ "g) Police (binary)",
    outcome == "Election\nCommission" ~ "h) Election\nCommission",
    outcome == "Election\nCommission (binary)" ~ "i) Election\nCommission (binary)",
    outcome == "Courts" ~ "j) Courts",
    outcome == "Courts (binary)" ~ "k) Courts (binary)"
  ))

fig_SM5 <- ggplot(results_pt_full, aes(x = term_numeric, y = estimate, 
                                        ymin = conf.low, ymax = conf.high, 
                                        col = factor(spec, levels = 
                                                       c("Baseline","+ Country trends",
                                                         "+ Conley 50km")), 
                                        shape = factor(spec, levels = 
                                                         c("Baseline","+ Country trends",
                                                           "+ Conley 50km")))) +
  facet_wrap(~factor(outcome, 
                     levels = c("a) Mean trust", "b) Mean trust (ex Pres)", 
                                "c) Mean trust (binary)", 
                                "d) President", "e) President (binary)",
                                "f) Police", "g) Police (binary)",
                                "h) Election\nCommission", "i) Election\nCommission (binary)", 
                                "j) Courts", "k) Courts (binary)")), 
             scales = "free", 
             nrow=4) +
  geom_pointrange(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept=0) +
  geom_point(position = position_dodge(width=0.7)) +
  theme_minimal() +
  scale_colour_manual(values = c("darkblue", "darkgrey", "black")) +
  ylab("Average difference") +
  xlab("\nRelative time periods until treatment (years)") +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM5 ## Print figure

fig_SM6 <- ggplot(results_pt_bin, aes(x = term_numeric, y = estimate, 
                                       ymin = conf.low, ymax = conf.high, 
                                       col = factor(spec, levels = 
                                                      c("Baseline","+ Country trends",
                                                        "+ Conley 50km")), 
                                       shape = factor(spec, levels = 
                                                        c("Baseline","+ Country trends",
                                                          "+ Conley 50km")))) +
  facet_wrap(~factor(outcome, 
                     levels = c("a) Mean trust", "b) Mean trust (ex Pres)", 
                                "c) Mean trust (binary)", 
                                "d) President", "e) President (binary)",
                                "f) Police", "g) Police (binary)",
                                "h) Election\nCommission", "i) Election\nCommission (binary)", 
                                "j) Courts", "k) Courts (binary)")), 
             scales = "free", 
             nrow=4) +
  geom_pointrange(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept=0) +
  geom_point(position = position_dodge(width=0.7)) +
  theme_minimal() +
  scale_colour_manual(values = c("darkblue", "darkgrey", "black")) +
  ylab("Average difference") +
  xlab("\nRelative time periods until treatment (years)") +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM6 ## Print figure


## SM7 (distribution of geocodes)

rm(list=ls())

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

fig_SM7 <- ggplot(ABcombined_rural, aes(x = precision_code)) +
  geom_bar(aes(y = ..prop.., group = 1), fill = "darkgrey", col = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("\nPrecision code (lower = more precise)") +
  ylab("Proportion") +
  scale_y_continuous(limits = c(0, 0.53), 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5), 
                     labels = c("0%", "10%", "20%", "30%", "40%", "50%")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM7 ## Print figure

## SM8 (Main results broken by country)

rm(list=ls())

ABcombined_rural <- readRDS("ABcombined.rds") %>%
  filter(urbrur == "Rural")

results_base <- ABcombined_rural %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model = map(data, ~ feols(new_mean_trust ~ inside + ever_inside +
                                     news_radio + news_tv + news_paper + 
                                     cash_income + food + water + medical + age + gender +
                                     education | year, 
                                   data=.x, 
                                   cluster = ~ea_code)),
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied)

coefficients <- results_base %>%
  filter(term %in% c("inside")) %>%
  dplyr::select(country_name, term, estimate, std.error, statistic)


fig_SM8 <- ggplot(coefficients, aes(x = reorder(country_name, -estimate), y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_hline(aes(yintercept = -0.084952, linetype = "Overall average effect reported in main text"), 
             color = "darkred", size = 1, show.legend = TRUE) +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2) +
  labs(x = "Country",
       y = "\nEstimate",
       color = "Variable") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.y = element_blank()) +
  theme(axis.line.x = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        axis.line.y = element_line(linewidth = 0.5, 
                                   linetype = "solid", 
                                   colour = "#000000"),
        panel.spacing = unit(2, "lines"),
        axis.title = element_text(colour = "#000000"),
        axis.text = element_text(colour = "#000000"), 
        plot.title = element_text(colour = "#000000", face = "bold"))

fig_SM8 ## Print figure









