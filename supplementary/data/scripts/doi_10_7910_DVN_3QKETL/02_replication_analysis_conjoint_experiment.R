##### CONJOINT CHOICE EXPERIMENT
rm(list = ls())

# packages
library(cjoint)
library(readxl)
library(plyr) 
library(dplyr)
library(tidyr)
library(readr)
library(sandwich)
library(tibble)
library(ggpubr)
library(stringi)
library(cregg)
library(ggsci)
library(knitr)
library(kableExtra)
library(tidyverse)

'%ni%' <- Negate("%in%")

#if (!require("remotes")) {
# install.packages("remotes")
#}

'%ni%' <- Negate("%in%")
################################################################################
# set path to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
################################################################################
data <- read.csv("conjoint_data_replication.csv")
#########################################################
### Data cleaning
data[2:7] <- lapply(data[2:7], factor)

# Define reference categories
data$attrib3_lab <- relevel(data$attrib3_lab, ref = "Reduce government debt")
data$attrib4_lab <- relevel(data$attrib4_lab, ref = "No labor market program")
data$attrib5_lab <- relevel(data$attrib5_lab, ref = "100% of domestic workers in in CO2-intensive jobs")
data$attrib6_lab <- relevel(data$attrib6_lab, ref = "The political left and right")

# Binary support outcome
data$support_binary <- ifelse(data$rate %in% c("Somewhat in favor", "Strongly in favor"), 1, 0)

# Rating outcome
data$rate <- as.factor(data$rate)
data <- data %>% mutate(rate = forcats::fct_relevel(rate, c("Strongly against", "Somewhat against", "Neither in favor nor against", "Somewhat in favor", "Strongly in favor")))
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Rename attribute levels
data$attrib5_lab <- plyr::revalue(data$attrib5_lab, c("No labor market program" = "No program"))

data$attrib5_lab <- plyr::revalue(data$attrib5_lab, c("25% of domestic workers in CO2-intensive jobs" = "25% of domestic workers",
                                                      "50% of domestic workers in CO2-intensive jobs" = "50% of domestic workers",
                                                      "75% of domestic workers in CO2-intensive jobs" = "75% of domestic workers",
                                                      "100% of domestic workers in in CO2-intensive jobs" = "100% of domestic workers"))

data$attrib2_lab <- plyr::revalue(data$attrib2_lab, c("Increase by 10 USD per metric ton CO2 <br> (157 USD per year for average consumer)" = "10 USD/ton CO2 (157 USD p.a.)",
                                                      "Increase by 20 USD per metric ton CO2 <br> (314 USD per year for average consumer)" = "20 USD/ton CO2 (314 USD p.a.)",
                                                      "Increase by 30 USD per metric ton CO2 <br> (471 USD per year for average consumer)" = "30 USD/ton CO2 (471 USD p.a.)",
                                                      "Increase by 40 USD per metric ton CO2 <br> (628 USD per year for average consumer)" = "40 USD/ton CO2 (628 USD p.a.)",
                                                      "Increase by 50 USD per metric ton CO2 <br> (785 USD per year for average consumer)" = "50 USD/ton CO2 (785 USD p.a.)",
                                                      "Increase by 60 USD per metric ton CO2 <br> (942 USD per year for average consumer)" = "60 USD/ton CO2 (942 USD p.a.)",   
                                                      "Increase by 70 USD per metric ton CO2 <br> (1099 USD per year for average consumer)" = "70 USD/ton CO2 (1099 USD p.a.)"))

data$attrib3_lab <- plyr::revalue(data$attrib3_lab, c("Fund a labor market program for workers in CO2-intensive jobs" = "Fund a labor market program",
                                                      "Tax rebate <br> paid to everyone" = "Tax rebate for everyone"))

data$attrib4_lab <- plyr::revalue(data$attrib4_lab, c("Income premium <br> after job change" = "Income premium",
                                                      "Income tax relief <br> after job change" = "Income tax relief"))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Design the restrictions for CJOINT package, using makeDesign (from RDocumentation of CJOINT package by Hainmuller etal)
## Manually constructs conjoint design in R

# List of attributes and levels
attribute_list <- list()
attribute_list[["attrib1_lab"]] <- levels(data$attrib1_lab)
attribute_list[["attrib2_lab"]] <- levels(data$attrib2_lab)
attribute_list[["attrib3_lab"]] <- levels(data$attrib3_lab)
attribute_list[["attrib4_lab"]] <- levels(data$attrib4_lab)
attribute_list[["attrib5_lab"]] <- levels(data$attrib5_lab)
attribute_list[["attrib6_lab"]] <- levels(data$attrib6_lab)

# Randomization constraints in the conjoint design
constraint_list <- list()

# Constraints on Revenue use and Labor market program and Coverage
# Cannot have "fund labor market program" with "no labor market program" LMP
constraint_list[[1]] <- list()

constraint_list[[1]][["attrib3_lab"]] <- c("Fund a labor market program")
constraint_list[[1]][["attrib4_lab"]] <- c("No labor market program")

# Cannot have "fund labor market program" with "no labor market program" Coverage
constraint_list[[2]] <- list()
constraint_list[[2]][["attrib3_lab"]] <- c("Fund a labor market program")
constraint_list[[2]][["attrib5_lab"]] <- c("No program")

# Constraints on Labor market program and Coverage
# Cannot have "No labor market program" LMP with positive coverage (25,50,75,100) Coverage
constraint_list[[3]] <- list()
constraint_list[[3]][["attrib4_lab"]] <- c("No labor market program")
constraint_list[[3]][["attrib5_lab"]] <- c("25% of domestic workers", "50% of domestic workers", "75% of domestic workers", "100% of domestic workers")
##################################################################################################################################################################
# Create the conjoint design function including the constraints
carbontaxdesign <- makeDesign(type='constraints', attribute.levels=attribute_list,
                                constraints=constraint_list)
##################################################################################################################################################################
#  RESULTS SECTION 4.3
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# OVERALL RESULTS for FIGURE 6 and table E1 appendix
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Forced choice
results1 <- cjoint::amce(choice ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab  + attrib6_lab, data=data,
                cluster=T, respondent.id="id", design = carbontaxdesign)
# Rating outcome
results2 <- cjoint::amce(support_binary ~ attrib1_lab + attrib2_lab + attrib3_lab + attrib4_lab + attrib5_lab  + attrib6_lab, data=data,
                         cluster=T, respondent.id="id", design = carbontaxdesign)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract labels, levels, estimates, errors and p-values for Table and FIGURE
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Forced choice outcome
summary.results <- summary.amce(results1)
amce_df <- as.data.frame(summary.results$amce)
amce_df$bl <- 0

baselines <- as.data.frame(summary.results$baselines_amce)
baselines$Estimate <- 0
baselines$`z value` <- NA
baselines$`Std. Err` <- 0
baselines$`Pr(>|z|)` <- NA
baselines$` ` <- NA
baselines$bl <- 1

amce_df <- amce_df %>% rbind(baselines) %>% filter(!is.na(Estimate))

# Confidence intervals 95%
amce_df$`Lower CI` <- amce_df$Estimate - 1.96 * amce_df$`Std. Err`
amce_df$`Upper CI` <- amce_df$Estimate + 1.96 * amce_df$`Std. Err`

# Unified label
amce_df <- amce_df %>%
  dplyr::mutate(
    feature_lab = "",
    feature_lab = ifelse(Attribute == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(Attribute == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(Attribute == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(Attribute == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(Attribute == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(Attribute == "attrib6_lab", "Sponsor", feature_lab)) %>% arrange(feature_lab, desc(bl), Level)

amce_df$sort <- c(1:14, 18,15,16,17,19:nrow(amce_df))
amce_df$outcome <- "choice"
amce_all <- amce_df
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Rating outcome
summary.results <- summary.amce(results2)
amce_df <- as.data.frame(summary.results$amce)
amce_df$bl <- 0

baselines <- as.data.frame(summary.results$baselines_amce)
baselines$Estimate <- 0
baselines$`z value` <- NA
baselines$`Std. Err` <- 0
baselines$`Pr(>|z|)` <- NA
baselines$` ` <- NA
baselines$bl <- 1

amce_df <- amce_df %>% rbind(baselines) %>% filter(!is.na(Estimate))

# Confidence intervals 95%
amce_df$`Lower CI` <- amce_df$Estimate - 1.96 * amce_df$`Std. Err`
amce_df$`Upper CI` <- amce_df$Estimate + 1.96 * amce_df$`Std. Err`

# Unified label
amce_df <- amce_df %>%
  dplyr::mutate(
    feature_lab = "",
    feature_lab = ifelse(Attribute == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(Attribute == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(Attribute == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(Attribute == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(Attribute == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(Attribute == "attrib6_lab", "Sponsor", feature_lab)) %>% arrange(feature_lab, desc(bl), Level)

amce_df$sort <-  c(1:14, 18,15,16,17,19:nrow(amce_df))
amce_df$outcome <- "rate"
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
amce_all <- amce_all %>% rbind(amce_df)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create latex table for TABLE E1
latex_df_all <- amce_all %>%
  dplyr::select(Outcome = outcome, Feature = feature_lab, Level, Estimate, `Std. Err`, `Pr(>|z|)`) %>%
  dplyr::rename(`Std. Error` = `Std. Err`, `p-value` = `Pr(>|z|)`) %>%
  pivot_longer(cols = c("Estimate", "Std. Error", "p-value"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Outcome_stat", Outcome, stat) %>%
  pivot_wider(names_from = Outcome_stat, values_from = value)

kbl(latex_df_all, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create FIGURE 6
# 
p1 <- ggplot(amce_all %>% filter(outcome =="choice"), aes(x = Estimate, y = forcats::fct_reorder(Level, sort), color=feature_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(aes(xmin = `Lower CI`, xmax = `Upper CI`)) +
  theme_classic(base_size = 11) +
  labs(x = "AMCE Estimate", y = NULL, title = "(a) Average marginal component effects: Forced choice") +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") + theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  scale_color_viridis_d(option="D", end = 0.85)  + theme(panel.grid = element_blank(),
    strip.text.y = element_blank(),
    #strip.background = element_rect(fill ="lightgrey"),
    #strip.text = element_text(colour = "black", size =7.5))
    ) + xlim(-0.2,0.2)

p2 <- ggplot(amce_all %>% filter(outcome =="rate"), aes(x = Estimate, y = forcats::fct_reorder(Level, sort), color=feature_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(aes(xmin = `Lower CI`, xmax = `Upper CI`)) +
  theme_classic(base_size = 11) +
  labs(x = "AMCE Estimate", y = NULL, title = "(b) Average marginal component effects: Suppport rate") +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") + theme_classic(base_size = 12) +
  theme(legend.position = "none") +
  scale_color_viridis_d(option="D", end = 0.85) + theme(#panel.grid = element_blank(),
    #strip.text.y = element_blank(),
    strip.background = element_rect(fill ="lightgrey"),
    strip.text = element_text(colour = "black", size =7.5)) + theme(axis.text.y = element_blank(),axis.title.y = element_blank()) + xlim(-0.2,0.2)

#################################################################################################################################################################
##################################################################################################################################################################
# Extract AMCEs by group
run_amce_by_group <- function(data, group_value, group_var, 
                              conjoint_design, respondent_id = "id",
                              formula = choice ~ attrib1_lab + attrib2_lab + attrib3_lab + 
                                attrib4_lab + attrib5_lab + attrib6_lab) {
  
  # Subset data by group
  group_data <- data %>% dplyr::filter(.data[[group_var]] == group_value)
  
  # Run amce – FIXED formula argument passed explicitly
  amce_model <- cjoint::amce(formula = formula, data = group_data,
                     cluster = TRUE,
                     respondent.id = respondent_id,
                     design = conjoint_design)
  
  # Extract summary
  s <- summary(amce_model)
  
  # Print number of observations
  nobs <- s$samplesize_estimates
  nresp <- s$respondents
  message("Group: ", group_value, " | N Respondents: ", nobs, 
          " | N Profiles: ", nresp)
  
  
  # AMCE estimates
  amce_df <- as.data.frame(s$amce)
  amce_df$bl <- 0
  
  # baselines 
  baselines <- as.data.frame(s$baselines_amce)
  baselines$Estimate <- 0
  baselines$`z value` <- NA
  baselines$`Std. Err` <- 0
  baselines$`Pr(>|z|)` <- NA
  baselines$` ` <- NA
  baselines$bl <- 1
  
  # bind baseline values to df
  amce_df <- amce_df %>% rbind(baselines) %>% filter(!is.na(Estimate))
  
  # Confidence intervals sort, and out
  out <- amce_df %>%
    dplyr::mutate(
      `Lower CI` = Estimate - 1.96 * `Std. Err`,
      `Upper CI` = Estimate + 1.96 * `Std. Err`,
      !!group_var := group_value
    )  %>%
     dplyr::mutate(
      feature_lab = "",
      feature_lab = ifelse(Attribute == "attrib1_lab", "Target sector", feature_lab),
      feature_lab = ifelse(Attribute == "attrib2_lab", "Costs of the tax", feature_lab),
      feature_lab = ifelse(Attribute == "attrib3_lab", "Revenue use", feature_lab),
      feature_lab = ifelse(Attribute == "attrib4_lab", "Labor market program", feature_lab),
      feature_lab = ifelse(Attribute == "attrib5_lab", "LMP coverage", feature_lab),
      feature_lab = ifelse(Attribute == "attrib6_lab", "Sponsor", feature_lab)) %>% 
    dplyr::arrange(group_var, feature_lab, desc(bl), Level) %>%
    dplyr::mutate(sort=c(1:7,11,8,9,10,12:nrow(amce_df))) %>% dplyr::arrange(group_var,sort)
  
  return(out)
 }

###########################################################################################################################################
# RESULTS BY COUNTRY (FIGURE 7)
countries <- c("Germany", "Sweden", "Switzerland", "United States of America")

amce_by_country <- lapply(countries, function(cntry) {
  run_amce_by_group(data = data,
                    group_value = cntry,
                    group_var = "resident",
                    conjoint_design = carbontaxdesign)
})
amce_all_resident <- do.call(rbind, amce_by_country)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create latex table wide by Country TABLE E2 APPENDICES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
latex_df_res <- amce_all_resident %>%
  dplyr::select(Country = resident, Feature = feature_lab, Level, Estimate, `Std. Err`, `Pr(>|z|)`) %>%
  dplyr::rename(`Std. Error` = `Std. Err`, `p-value` = `Pr(>|z|)`) %>%
  pivot_longer(cols = c("Estimate", "Std. Error", "p-value"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Country_stat", Country, stat) %>%
  pivot_wider(names_from = Country_stat, values_from = value)

kbl(latex_df_res, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create plot for FIGURE 7a
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
amce_all_resident$resident[amce_all_resident$resident=="United States of America"] <- "United States"
amce_all_resident$Level <- as.factor(amce_all_resident$Level)

p3 <- ggplot(amce_all_resident %>% dplyr::filter(feature_lab %ni% c("Target sector", "Costs of the tax")), aes(x = Estimate, y =  forcats::fct_reorder(Level, sort), color=resident)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = `Lower CI`, xmax = `Upper CI`), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  theme_classic(base_size = 11) +
  labs(x = "AMCE Estimate", y = NULL, title = "(a) AMCEs by country") +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") + theme_classic(base_size = 12) +
  scale_color_viridis_d(option="D", end = 0.85, name = "Country") +
  theme(legend.position = c(0.83,0.15), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  + theme(panel.grid = element_blank(),
                                                                    strip.text.y = element_blank(),
                                                                    #strip.background = element_rect(fill ="lightgrey"),
                                                                    #strip.text = element_text(colour = "black", size =7.5)
                                         ) +xlim(-0.16,0.25)
###########################################################################################################################################
# RESULTS BY POLITICAL LEANING (FIGURE 7)
leaning <- c("A_left", "B_center", "C_right")

amce_by_leaning <- lapply(leaning, function(lean) {
  run_amce_by_group(data = data,
                    group_value = lean,
                    group_var = "pol.lean.three",
                    conjoint_design = carbontaxdesign)
})

amce_all_pol <- do.call(rbind, amce_by_leaning)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create latex table wide by Political leaning TABLE E3 APPENDICES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
latex_df_pol <- amce_all_pol %>%
  dplyr::select(Leaning = pol.lean.three, Feature = feature_lab, Level, Estimate, `Std. Err`, `Pr(>|z|)`) %>%
  dplyr::rename(`Std. Error` = `Std. Err`, `p-value` = `Pr(>|z|)`) %>%
  pivot_longer(cols = c("Estimate", "Std. Error", "p-value"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Leaning_stat",Leaning, stat) %>%
  pivot_wider(names_from = Leaning_stat, values_from = value)

kbl(latex_df_pol, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create plot for FIGURE 7b
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
p4 <- ggplot(amce_all_pol%>% filter(feature_lab %ni% c("Target sector", "Costs of the tax")), aes(x = Estimate, y =  forcats::fct_reorder(Level, sort), color=pol.lean.three)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(aes(xmin = `Lower CI`, xmax = `Upper CI`), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  theme_classic(base_size = 11) +
  labs(x = "AMCE Estimate", y = NULL, title = "(b) AMCEs by political leaning") +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") + theme_classic(base_size = 12) +
  scale_color_viridis_d(option="D", end = 0.85, name = "Pol. leaning", labels=c("Left", "Center", "Right")) +
  theme(legend.position = c(0.83,0.15), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  + theme(panel.grid = element_blank(),
                                           strip.text.y = element_blank(),
                                           #strip.background = element_rect(fill ="lightgrey"),
                                           #strip.text = element_text(colour = "black", size =7.5)
                                           ) + theme(axis.text.y = element_blank()) +xlim(-0.16,0.25)

###########################################################################################################################################
# RESULTS BY JOB RISK (FIGURE 7)
risk <- c("High risk", "Low risk")

data$highrisk <- ifelse(data$index.score > mean(data$index.score,na.rm = T),"High risk", "Low risk")

amce_by_risk <- lapply(risk, function(risky) {
  run_amce_by_group(data = data,
                    group_value = risky,
                    group_var = "highrisk",
                    conjoint_design = carbontaxdesign)
})

amce_all_risk <- do.call(rbind, amce_by_risk)
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create latex table wide by job risk TABLE E4 APPENDICES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
latex_df_rk <- amce_all_risk %>%
  dplyr::select(Risk = highrisk, Feature = feature_lab, Level, Estimate, `Std. Err`, `Pr(>|z|)`) %>%
  dplyr::rename(`Std. Error` = `Std. Err`, `p-value` = `Pr(>|z|)`) %>%
  pivot_longer(cols = c("Estimate", "Std. Error", "p-value"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Risk_stat",Risk, stat) %>%
  pivot_wider(names_from = Risk_stat, values_from = value)

kbl(latex_df_rk, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create plot for FIGURE 7c
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------
p5 <- ggplot(amce_all_risk %>% filter(feature_lab %ni% c("Target sector", "Costs of the tax")), aes(x = Estimate, y =  forcats::fct_reorder(Level, sort), color=highrisk)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  
  geom_pointrange(aes(xmin = `Lower CI`, xmax = `Upper CI`), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  theme_classic(base_size = 11) +
  labs(x = "AMCE Estimate", y = NULL, title = "(c) AMCEs by job risk") +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") + theme_classic(base_size = 12) +
  scale_color_viridis_d(option="D", end = 0.85, name = "Job risk") +
  theme(legend.position = c(0.83,0.15), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  + theme(#panel.grid = element_blank(),
                                           #strip.text.y = element_blank(),
                                           strip.background = element_rect(fill ="lightgrey"),
                                           strip.text = element_text(colour = "black", size =7.5)) + theme(axis.text.y = element_blank()) +
  xlim(-0.16,0.25)
###########################################################################################################################################
# CREATE FIGURES 6 and 7
#------------------------------------------------------------------------------------------------------------------------------------------
# Figure overall (6)
#------------------------------------------------------------------------------------------------------------------------------------------
p6 <- gridExtra::grid.arrange(p1,p2, nrow=1, widths=c(7,5))

ggsave(plot = p6,
       "cj2.pdf",
       width = 12,
       height = 8,
       dpi = 800
)
#------------------------------------------------------------------------------------------------------------------------------------------
# Figure by subgroup (7)
#------------------------------------------------------------------------------------------------------------------------------------------
p7 <- gridExtra::grid.arrange(p3,p4,p5, nrow=1, widths=c(7.5,4.75,4.85))

ggsave(plot = p7,
       "cj3.pdf",
       width = 12,
       height = 8,
       dpi = 800
)
###########################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------
# MORE APPENDICES RESULTS
#------------------------------------------------------------------------------------------------------------------------------------------
###########################################################################################################################################
# Marginal means results (APPENDIX SECTIONS E5 to E9)
#------------------------------------------------------------------------------------------------------------------------------------------
# Overall marginal mean results

mm1 <- cj(data = data, choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id=~id, estimate = "mm")
#------------------------------------------------------------------------------------------------------------------------------------------
# Marginal means plot OVERALL (FIGURE E1 (a), Section E5)
#------------------------------------------------------------------------------------------------------------------------------------------
p_baseline2 <- mm1 %>%
  dplyr::mutate(
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Sponsor", feature_lab))

p11 <-  ggplot(p_baseline2,aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, color=feature_lab)) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") +
  theme_classic() +
  theme(panel.grid = element_blank(),
    strip.text.y = element_blank(),
    strip.background = element_rect(fill ="lightgrey"),
    strip.text = element_text(colour = "black", size =7.5)) +
  coord_flip() +
  labs(y ="MM", x = "", subtitle = "(a) Overall results: Marginal means") +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50") +
    scale_color_viridis_d(option="D", end = 0.9) +theme(legend.position = "none") + ylim(0.36,0.64) 
#------------------------------------------------------------------------------------------------------------------------------------------
# Overall marginal means table (Table E5, Section E6)
#------------------------------------------------------------------------------------------------------------------------------------------
latex_df_mm <- p_baseline2 %>%
  dplyr::select(Feature = feature_lab, level, estimate, std.error) %>%
  dplyr::rename(`Std. Error` = std.error, Level = level, Estimate = estimate) %>%
  arrange(Feature, Level)

kbl(latex_df_mm, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))
###########################################################################################################################################
# Country marginal mean results
#------------------------------------------------------------------------------------------------------------------------------------------
mm4 <- cj(data = data %>% filter(resident=="United States of America" ), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm4$group <- "United States"

mm5 <- cj(data = data %>% filter(resident=="Germany" ), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab +  attrib5_lab +  attrib6_lab , id = ~id, estimate = "mm")
mm5$group <- "Germany"

mm6 <- cj(data = data %>% filter(resident=="Switzerland" ), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab +  attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm6$group <- "Switzerland"

mm7 <- cj(data = data %>% filter(resident=="Sweden" ), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab +  attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm7$group <- "Sweden"

mm4 <- mm4 %>% rbind(mm5,mm6,mm7) 
#------------------------------------------------------------------------------------------------------------------------------------------
# Marginal means plot COUNTRY (FIGURE E1 (b), Section E5)
#------------------------------------------------------------------------------------------------------------------------------------------
p_country <- mm4 %>%
  mutate(
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Sponsor", feature_lab))

p12 <-  ggplot(p_country ,aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, color=group), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") +
  theme_classic() + 
  coord_flip() +
  labs(y ="MM", x = "", subtitle = "(b) MMs by country") +
  
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50")  +
  scale_color_viridis_d(name = "Country", option = "D", end = 0.9) +
  theme(#panel.grid = element_blank(),
    #strip.text.y = element_blank(),
    strip.background = element_rect(fill = "lightgrey"),
    strip.text = element_text(colour = "black", size =9)) +
  theme(legend.position = c(0.85,0.8), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  +
  
  theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
  ylim(0.36,0.64)
#------------------------------------------------------------------------------------------------------------------------------------------
# COUNTRY marginal means table (Table E6, Section E7)
#------------------------------------------------------------------------------------------------------------------------------------------
latex_df_mm_country <- p_country %>%
  dplyr::select(Country = group, Feature = feature_lab, level, estimate, std.error) %>%
  dplyr::rename(`Std. Error` = std.error, Level = level, Estimate = estimate) %>% arrange(Country, Feature, Level) %>%
  pivot_longer(cols = c("Estimate", "Std. Error"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Country_stat",Country, stat) %>%
  pivot_wider(names_from = Country_stat, values_from = value)
  

kbl(latex_df_mm_country, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))
##################################################################################################################################################################
## Political leaning marginal means results
#------------------------------------------------------------------------------------------------------------------------------------------
mm8 <- cj(data = data %>% filter(pol.lean.three == "A_left"), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm8$group <- "A_Left"

mm9 <- cj(data = data %>% filter(pol.lean.three == "B_center"), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm9$group <- "B_Center"

mm10 <- cj(data = data %>% filter(pol.lean.three == "C_right"), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm10$group <- "C_Right"

mm8 <- mm8 %>% rbind(mm9, mm10) 
#------------------------------------------------------------------------------------------------------------------------------------------
# Marginal means plot POLITICAL LEANING (FIGURE E1 (c), Section E5)
#------------------------------------------------------------------------------------------------------------------------------------------
p_politmm <- mm8 %>%
  mutate(
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Sponsor", feature_lab))


p13 <-  ggplot(p_politmm ,aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, color=group), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") +
  theme_classic() + 
  coord_flip() +
  labs(y ="MM", x = "", subtitle = "(c) MMs by political leaning") +
  theme(panel.grid = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50")  +
  theme(legend.position = c(0.85,0.8), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  +  scale_color_viridis_d(name = "Political leaning", labels=c("Left", "Center", "Right"), option = "D", end = 0.85) +
 ylim(0.36,0.64)
#------------------------------------------------------------------------------------------------------------------------------------------
# COUNTRY marginal means table (Table E7, Section E8)
#------------------------------------------------------------------------------------------------------------------------------------------
latex_df_mm_lean <- p_politmm %>%
  dplyr::select(Leaning = group, Feature = feature_lab, level, estimate, std.error) %>%
  dplyr::rename(`Std. Error` = std.error, Level = level, Estimate = estimate) %>% arrange(Leaning, Feature, Level) %>%
  pivot_longer(cols = c("Estimate", "Std. Error"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Leaning_stat",Leaning, stat) %>%
  pivot_wider(names_from = Leaning_stat, values_from = value)


kbl(latex_df_mm_lean, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))

##################################################################################################################################################################
## Job risk marginal mean results
#------------------------------------------------------------------------------------------------------------------------------------------
mm11 <- cj(data = data %>% filter(highrisk == "High risk"), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm11$group <- "High risk"

mm12 <- cj(data = data %>% filter(highrisk == "Low risk"), choice ~ attrib1_lab + attrib2_lab + attrib3_lab * attrib4_lab + attrib5_lab + attrib6_lab , id = ~id, estimate = "mm")
mm12$group <- "Low risk"

mm11 <- mm11 %>% rbind(mm12) 
#------------------------------------------------------------------------------------------------------------------------------------------
# Marginal means plot JOB RISK (FIGURE E1 (d), Section E5)
#------------------------------------------------------------------------------------------------------------------------------------------
p_riskmm <- mm11 %>%
  mutate(
    feature_lab = "",
    feature_lab = ifelse(feature == "attrib1_lab", "Target sector", feature_lab),
    feature_lab = ifelse(feature == "attrib2_lab", "Costs of the tax", feature_lab),
    feature_lab = ifelse(feature == "attrib3_lab", "Revenue use", feature_lab),
    feature_lab = ifelse(feature == "attrib4_lab", "Labor market program", feature_lab),
    feature_lab = ifelse(feature == "attrib5_lab", "LMP coverage", feature_lab),
    feature_lab = ifelse(feature == "attrib6_lab", "Sponsor", feature_lab))

p14 <-  ggplot(p_riskmm ,aes(level, estimate)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper, color=group), position = position_dodge(width=0.6), size=0.25, linewidth=0.2) +
  scale_x_discrete(limits=rev) +
  facet_grid(feature_lab~., scales = "free_y", space = "free_y") +
  theme_classic() + 
  coord_flip() +
  labs(y ="MM", x = "", subtitle = "(d) MMs by job risk") +
  theme(#panel.grid = element_blank(),
        #strip.text.y = element_blank(),
        strip.background = element_rect(fill = "lightgrey"),
        strip.text = element_text(colour = "black", size =9)) +
  geom_hline(yintercept = 0.5, lty = 2, alpha = 0.7, color = "gray50")  +
  theme(legend.position = c(0.85,0.8), legend.text = element_text(size=8), legend.title = element_text(size=8, face = "bold"),
        legend.background = element_rect(fill="white",
                                         linewidth =0.5, linetype="solid", 
                                         colour ="black"))  +  scale_color_viridis_d(name = "Job risk", option = "D", end = 0.8) +
  ylim(0.36,0.64) +   theme(axis.title.y = element_blank(), axis.text.y = element_blank()) 
#------------------------------------------------------------------------------------------------------------------------------------------
# JOB RISK marginal means table (Table E8, Section E9)
#------------------------------------------------------------------------------------------------------------------------------------------
latex_df_mm_risk <- p_riskmm %>%
  dplyr::select(Risk = group, Feature = feature_lab, level, estimate, std.error) %>%
  dplyr::rename(`Std. Error` = std.error, Level = level, Estimate = estimate) %>% arrange(Risk, Feature, Level) %>%
  pivot_longer(cols = c("Estimate", "Std. Error"),
               names_to = "stat", values_to = "value") %>%
  unite(col = "Risk_stat",Risk, stat) %>%
  pivot_wider(names_from = Risk_stat, values_from = value)


kbl(latex_df_mm_risk, format = "latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))



##################################################################################################################################################################
# COMBINE PANELS FOR Marginal means plot (FIGURE E1)
p15 <- gridExtra::grid.arrange(p11,p12,p13,p14,nrow=2, widths=c(6.7,5.3))

ggsave(plot = p15,
       "cj4.pdf",
       width = 12,
       height = 14,
       dpi = 800
)
##################################################################################################################################################################
