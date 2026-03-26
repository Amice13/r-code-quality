
library(dplyr)
library(ggplot2)
library(tidyr)
library(rio)
library(ggpubr)
library(egg)
library(nnet)
library(marginaleffects)
library(stargazer)
library(knitr)
library(naniar)
library(kableExtra)
library(cjoint)
library(knitr)
library(kableExtra)
library(gridExtra)
library(magrittr)
library(logr)
library("rcartocolor")
library(ggforce)
library(Hmisc)

# visualization-related packages
library(ggtext)          
library(glue)            
library(scales)         
library(patchwork)       
library(ggrepel)         
library(MetBrewer)      

# for formatting numbers
nice_number <- label_number(style_negative = "minus", accuracy = 0.01)
nice_p <- label_pvalue(prefix = c("p < ", "p = ", "p > "))

# ggplot theme to make pretty plots
theme_mfx <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold"),
          #axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold"),
          #legend.title = element_text(face = "bold"))
          strip.background = element_rect(fill = "grey80", color = NA))
}

# Johnson color palette
clrs <- met.brewer("Johnson",25)[12:22]

## load data

AI <- read.csv("AI.csv") # cleaned data

#====================================================================================#
#
# Figure 1 
#
#====================================================================================#

# weighted linear model
mod <- lm(risks_AI_avg ~ gender_bin + age_group + education + country + manipulation_check2, 
          data = AI, weights = weight) 

# weighted predictions with counterfactual data grid
pred <- avg_predictions(mod, 
                        newdata = datagrid(gender_bin = c("Men", "Women"), 
                                           grid_type = "counterfactual"), 
                        by = c("gender_bin"), 
                        vcov = ~ weight) %>%  
  dplyr::mutate(nice_pred = nice_number(estimate)) 

# plot
ggplot(pred, aes(x = estimate, y = gender_bin, color=gender_bin)) +
  geom_vline(xintercept = 5, linewidth = 0.5, linetype = "24", color = clrs[6]) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5)) +
  geom_text(aes(label = nice_pred), position = position_nudge(y = 0.1)) +
  labs(x = "Predicted Risks Outweigh Benefits", y = "") +  
  scale_color_manual(values = c(clrs[1], clrs[9]), 
                     labels = c("Men", "Women")) +  
  theme_mfx() +
  xlim(3, 6) +
  theme(legend.position = "none")
ggsave("Figures/Figure1.png", height=3, width=5.5, dpi = 350)

# stargazer
stargazer(mod,
          type = "latex", 
          covariate.labels = c("Women (Ref: Men)", 
                               "Age 30-44 (Ref: Under 30)", 
                               "Age 45-64", 
                               "Age 65+", 
                               "Postgraduate (Ref: HS or Less)", 
                               "Some College", 
                               "University",
                               "U.S. (Ref: Canada)",
                               "Passed Manipulation Check"),
          dep.var.labels = "Risks of AI outweigh benefits",
          title = "",
          single.row=TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01))



#====================================================================================#
#
# Figure 2
#
#====================================================================================#

# ========== trait risk by gender =========== #

counts <- AI %>%
  group_by(gender_bin, trait_risk) %>%
  dplyr::summarise(count = sum(weight, na.rm = TRUE), .groups = 'drop') %>%  # Use weighted counts
  drop_na()

# total weighted number of entries per gender
totals <- AI %>%
  group_by(gender_bin) %>%
  dplyr::summarise(total = sum(weight, na.rm = TRUE), .groups = 'drop')  # Use weighted totals


# plot
a <- counts %>%
  left_join(totals, by = "gender_bin") %>%
  mutate(proportion = count / total, 
         ci_lower = proportion - 1.96 * sqrt(proportion * (1 - proportion) / total),
         ci_upper = proportion + 1.96 * sqrt(proportion * (1 - proportion) / total)) %>%
  ggplot(aes(x = trait_risk, y = proportion, fill = gender_bin)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha=0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(width = 0.9), width = 0.08) + 
  labs(fill = "",
       y = "Proportion",
       x = "Risk Orientation",
       title = "Risk Orientation") +
  scale_fill_manual(values = c(clrs[1], clrs[9]), 
                    labels = c("Men", "Women")) +  # Change legend labels and assign colors
  scale_y_continuous(labels = scales::percent_format()) +
  theme_mfx() +
  theme(legend.position="none")

# ========== objective threat, edu by gender =========== #

b <- AI %>%
  dplyr::select(gender_bin, objective_threat, educ_short) %>%
  drop_na() %>%
  mutate(educ_lab = ifelse(educ_short == 1, "University", "No University")) %>%
  ggplot(aes(x = objective_threat, fill = gender_bin)) +
  geom_density(color= NA, position = position_dodge(0.01), alpha=0.7) +
  facet_wrap(~educ_lab, 2) +
  labs(x = "Risk Exposure",
       y = "Density",
       fill = "",
       title = "Technological Exposure") +
  scale_fill_manual(values = c(clrs[1], clrs[9]), 
                    labels = c("Men", "Women")) +
  theme_mfx() + theme(legend.position = "right")

ggpubr::ggarrange(a, b, widths = c(0.7, 1))
ggsave("Figures/Figure2.png", height=4.6, width=7, dpi = 350)


#====================================================================================#
#
# Figure 3
#
#====================================================================================#

# weighted linear model
mod <- lm(risks_AI_avg ~ educ_short * gender_bin * trait_risk + age_group + country + manipulation_check2, 
          data = AI, weights = weight)  # Include weights

# weighted predictions with counterfactual data grid
pred <- avg_predictions(mod, 
                        newdata = datagrid(trait_risk = c("certain", "risky"), 
                                           gender_bin = c("Men", "Women"), 
                                           educ_short = c(0, 1),
                                           grid_type = "counterfactual"), 
                        by = c("gender_bin", "trait_risk", "educ_short"), 
                        vcov = ~ weight) %>%  # Ensure weights are considered in predictions
  dplyr::mutate(nice_pred = nice_number(estimate)) %>%
  dplyr::mutate(trait_labs = ifelse(trait_risk == "certain", "Prefers Certainty", "Takes Risks")) %>%
  dplyr::mutate(educ_short_labs = ifelse(educ_short == 1, "University", "No University")) %>%
  dplyr::mutate(educ_short_labs = factor(educ_short_labs, levels = c("No University", "University")))

# plot
ggplot(pred, aes(x=educ_short_labs, y = estimate, color=gender_bin)) + 
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.2)) +
  geom_text(aes(label = nice_pred), position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c(clrs[1], clrs[9]), 
                     labels = c("Men", "Women")) +  # Change legend labels and assign colors
  theme_mfx() +
  coord_flip() +
  ylim(2, 6) +
  facet_wrap(~trait_labs, nrow=2) +
  labs(title = "", 
       x ="", y = "Predicted Risks Outweigh Benefits", color="Gender")+
  theme(legend.position = "bottom")
ggsave("Figures/Figure3.png", height=5.5, width=4.5, dpi = 350)

# stargazer
stargazer(mod,
          type = "latex", 
          covariate.labels = c("University (Ref: No University)",
                               "Women (Ref: Men)", 
                               "Risk-taking Risk Orientation (Ref: Certain Risk Orientation)", 
                               "Age 30-44 (Ref: Under 30)", 
                               "Age 45-64", 
                               "Age 65+", 
                               "U.S. (Ref: Canada)",
                               "Passed Manipulation Check",
                               "University x Women", 
                               "University x Risk-taking Risk Orientation", 
                               "Women x Risk-taking Risk Orientation",
                               "University x Women x Risk-taking Risk Orientation"),
          dep.var.labels = "Risks of AI outweigh benefits",
          title = "",
          single.row=TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01))


#====================================================================================#
#
# Figure 4
#
#====================================================================================#

# regression
mod <- lm(support_company ~ gender_bin*percent_job_gain + age_group + education + country + manipulation_check2, data=AI)
pred <- avg_predictions(mod, by = c("percent_job_gain","gender_bin"), 
                        variables=c("percent_job_gain","gender_bin"))

# tidied results
pred <- pred %>%
  dplyr::mutate(nice_pred = nice_number(estimate)) %>%
  dplyr::mutate(percent_job_gain = factor(percent_job_gain, levels=c("30%", "50%", 
                                                                     "70%", "100%")))

# plot
ggplot(pred, aes(x = estimate, y = percent_job_gain, color=gender_bin)) +
  geom_vline(xintercept = 3, linewidth = 0.5, linetype = "24", color = clrs[6]) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.25)) +
  geom_text(aes(label = nice_pred), position = position_dodge2(width = 1)) +
  labs(x = "Marginal effect on support for company adopting AI", y = "Certainty of Job Gains",
       color = "Gender") +  # Change legend title
  scale_color_manual(values = c(clrs[1], clrs[9]), 
                     labels = c("Men", "Women")) +  # Change legend labels and assign colors
  theme_mfx() +
  theme(legend.position = "bottom")
ggsave("Figures/Figure4.png", height=3.5, width=6.5, dpi = 350)

# stargazer
stargazer(mod,
          type = "latex", 
          covariate.labels = c("Women (Ref: Men)", 
                               "50\\% Job Gain (Ref: 30\\%)",
                               "70\\% Job Gain",
                               "100\\% Job Gain",
                               "Age 30-44 (Ref: Under 30)", 
                               "Age 45-64", 
                               "Age 65+", 
                               "Postgraduate (Ref: HS or Less)", 
                               "Some College", 
                               "University",
                               "U.S. (Ref: Canada)",
                               "Passed Manipulation Check",
                               "Women x 50\\% Job Gain",
                               "Women x 70\\% Job Gain",
                               "Women x 100\\% Job Gain"
          ),
          dep.var.labels = "Support for a Company Adopting AI",
          title = "",
          single.row=TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01))




