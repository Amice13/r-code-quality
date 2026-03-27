# --------------------------------------------
#
# Author: Shaka. Y.J. Li
# Copyright (c) 
# Email: yl24m@fsu.edu
#
# Date: 2025-12-20
#
# Script Name: Replication code 
#
# Script Description: 
# Replication code for the article:
# "Killing Many to Targeting Few: Economic Growth and Selective Repression in Taiwan’s White Terror Period"
#
# Notes: 
# Replicates tables and figures from the main text, robustness checks, and appendix. 
#
# --------------------------------------------


# LOAD REQUIRED PACKAGES --------------------------------------------------

library(ggplot2)
library(stargazer)
library(tidyverse)
library(dplyr)
library(ivreg)
library(dynlm)
library(nlme)
library(broom)
library(sjPlot)
library(effects)
library(gridExtra)
library(patchwork)
library(modelsummary)

# LOAD DATA--------------------------------------------------
cleaned_repress <- read_csv("clean_data.csv")
dta_death_sentence <- read.csv("dta_death_sentence.csv")
dta_sentence_outcome <- read.csv("dta_sentence_outcome.csv")

# ==========================================================
# VARIABLE PREPARATION
# ----------------------------------------------------------
# Create transformed variables (e.g., log forms) and 
# generate lagged values (t-1) for key predictors.
# These variables will be used in regression and IV models.
# ==========================================================

# Create log-transformed variables--------------------------------
# Apply log(x+1) transformation to avoid log(0)
cleaned_repress$ln_GDP_per_capita = log(cleaned_repress$GDP_per_capita + 1)
cleaned_repress$ln_population_size = log(cleaned_repress$Population_size +1)
cleaned_repress$ln_military_expenditrue = log(cleaned_repress$military_expenditrue+1)
cleaned_repress$GDP_lag = lag(cleaned_repress$ln_GDP_per_capita)
cleaned_repress$selective = lag(cleaned_repress$political_civil_liberty_index) 
cleaned_repress$ln_trade_openness = log(cleaned_repress$Trade_openness + 1)
cleaned_repress$ln_econ_rate = log(cleaned_repress$economic_growth_rate +1)
cleaned_repress$ln_trade_openness <- log(cleaned_repress$Trade_openness + 1)
cleaned_repress$ln_aid_constant <- log(cleaned_repress$constant_amount +1)
cleaned_repress$Aid_log_lag1 <- lag(cleaned_repress$ln_aid_constant, 1)
cleaned_repress$ln_political_pois <- log(cleaned_repress$political_crime + 1)
cleaned_repress$ln_political_pois_lag1 <- lag(cleaned_repress$ln_political_pois, 1)

# Create lagged variables (t-1)--------------------------------
# Generate lagged versions for time-series analysis
cleaned_repress <- cleaned_repress %>%
  mutate(
    ln_GDP_per_capita_lag1 = lag(ln_GDP_per_capita, 1),
    Anti_system_movements_lag1 = lag(Anti_system_movements, 1),
    economic_growth_rate_lag1 = lag(economic_growth_rate, 1),
    ln_population_size_lag1 = lag(ln_population_size, 1),
    political_crime_lag1 = lag(political_crime, 1),
    political_openness_lag1 = lag(political_openness, 1),
    aid_constant_lag1 = lag(constant_amount, 1),
    aid_current_lag1 = lag(current_amount, 1),
    trade_openness_lag1 = lag(ln_trade_openness, 1)
  )

# ==========================================================
# Statistical Results
# ==========================================================

# ==========================================================
# TABLE 2 
# Baseline Regression Results: GDP per capita × Anti-system Movements
# ----------------------------------------------------------
# Notes:
# Regression estimates for three subsamples:
# (1) Before 1970
# (2) After 1970
# (3) Full sample (1959–1988)
# ==========================================================

# Before 1970 --------------------------------------------------
# Model: moderator_before_1970

moderator_before_1970 <- dynlm(
  political_civil_liberty_index ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress[cleaned_repress$Year < 1970, ]
)

# After 1970 ---------------------------------------------------
# Model: moderator_after_1970

moderator_after_1970 <- dynlm(
  political_civil_liberty_index ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress[cleaned_repress$Year  >= 1970, ]
)

# Full Sample --------------------------------------------------
# Model: moderator_1

moderator_1 <- dynlm(
  political_civil_liberty_index ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress
)

# Output regression results to LaTeX file

stargazer(moderator_before_1970, moderator_after_1970, moderator_1, 
          type = "text", 
          out = "tabs/tab2_baseline.tex",
          dep.var.caption = "Selective Repression:",
          dep.var.labels = "",
          column.labels= c("Before 1970", "After 1970", "Overall"),
          covariate.labels = c("GDP per capita (logged, t-1)", 
                               "Anti-system Movements (logged, t-1)", 
                               "GDP per capita X Anti-system Movements (logged, t-1)",
                               "Economic Growth Rate (logged, t-1)",
                               "Population Size (logged, t-1)", 
                               "Political Prisoner (logged, t-1)", 
                               "Aid Constant (logged, t-1)"),
          order = c("ln_GDP_per_capita_lag1", 
                    "Anti_system_movements_lag1", 
                    "ln_GDP_per_capita_lag1 * Anti_system_movements_lag1",
                    "economic_growth_rate_lag1",
                    "ln_population_size_lag1", 
                    "political_crime_lag1", 
                    "aid_constant_lag1"), 
          model.names = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01))  

# ==========================================================
# FIGURE 5
# Coefficient Plot: Interaction Effects from Table 2
# ----------------------------------------------------------
# Notes:
# Visual representation of regression estimates from Table 2.
# Displays interaction effect between GDP per capita and 
# Anti-system Movements across different time periods.
# ==========================================================

# Extract tidy summaries (coefficients and standard errors) for each model
mod_1_tidy <- tidy(moderator_before_1970)
mod_2_tidy <- tidy(moderator_after_1970)
mod_3_tidy <- tidy(moderator_1)

# Combine model results into a single data frame and label each model
all_models <- bind_rows(
  mod_1_tidy %>% mutate(model = "Before 1970"),
  mod_2_tidy %>% mutate(model = "After 1970"),
  mod_3_tidy %>% mutate(model = "Moderator 1")
)

# Exclude the intercept term
all_models_no_intercept <- all_models %>%
  filter(term != "(Intercept)")

# Select key interaction and main effect terms for plotting
selected_terms <- c("ln_GDP_per_capita_lag1", "Anti_system_movements_lag1",
                    "ln_GDP_per_capita_lag1:Anti_system_movements_lag1")
filtered_models <- all_models_no_intercept %>%
  filter(term %in% selected_terms) %>%
  mutate(term = factor(term, levels = c(
    "ln_GDP_per_capita_lag1:Anti_system_movements_lag1", 
    "Anti_system_movements_lag1", 
    "ln_GDP_per_capita_lag1"
  )))

# Create coefficient plot with 95% confidence intervals
marginal <- ggplot(filtered_models, aes(x = estimate, y = term, color = model)) + 
              geom_point(size = 3) +
              geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                                 xmax = estimate + 1.96 * std.error), height = 0.2) +
              labs(
                x = "Coefficient Estimate (All covariates are included)",
                y = "Predictor Variables",
                color = "Model"
              ) +
              theme_minimal() +
              theme(legend.position = "bottom") +
              scale_color_manual(
                values = c(
                  "Before 1970" = "#2ECFCF",  
                  "After 1970"  = "#FF6F6F",   
                  "Moderator 1" = "#4682B4"   
                ),
                labels = c("Before 1970", "After 1970", "Overall")
              ) +
              scale_y_discrete(labels = c(
                "ln_GDP_per_capita_lag1" = "GDP per capita (t-1)", 
                "Anti_system_movements_lag1" = "Anti-system Movements (t-1)",
                "ln_GDP_per_capita_lag1:Anti_system_movements_lag1" = "GDP per capita × Anti-system Movements"
              ))

# Save the plot to figs/fig5.png
ggsave("figs/fig5.png", plot = marginal, width = 10, bg = "white", height = 6, dpi = 300)

# ==========================================================
# FIGURE 6
# Marginal Effect Plot: Interaction Effects from Table 2
# ----------------------------------------------------------
# Notes:
# ==========================================================

interaction <- plot_model(moderator_1, type = "pred", 
                          terms = c("ln_GDP_per_capita_lag1",
                                    "Anti_system_movements_lag1 [0, 2]"),
                          legend.title = "Anti-system Movement")+ 
  theme_minimal() +
  scale_color_manual(labels = c("Little amounts of movements", "Large amounts of movements"),
                     values = c("#F16B7F", "#4C9DDB")) +
  labs(
    title = "",  
    x = "Log of GDP per Capita (ln(GDP))",  
    y = "Predicted Probability of Selective Repression"  
  )

ggsave("figs/fig6_marginal.png", plot = interaction, width = 10, bg = "white", height = 6, dpi = 300)

# ==========================================================
# Robustness Check
# ==========================================================

# ==========================================================
# FIGURE 7
# Coefficient Plot
# ----------------------------------------------------------
# Expnanatory variable: Economic Grwoth Rate
# Dependent variable: Numbers of Death Sentence
# Interaction: Anti-system movement
# ==========================================================

moderator_total_charge <- dynlm(
  total_charge_death ~ economic_growth_rate_lag1 * Anti_system_movements_lag1 + 
    ln_GDP_per_capita_lag1 + ln_population_size_lag1 + Aid_log_lag1,
  data = cleaned_repress
)
m_list_Model <- list(moderator_total_charge)
c_order <- c("economic_growth_rate_lag1" = "Economic Growth Rate (logged, t-1)",
             "Anti_system_movements_lag1" = "Anti-system Movements (logged, t-1)",
             "economic_growth_rate_lag1:Anti_system_movements_lag1" = "Economic Growth X Anti-system Movements",
             "ln_GDP_per_capita_lag1" = "GDP per capita (logged, t-1)",
             "ln_population_size_lag1" = "Population Size (logged, t-1)",
             "Aid_log_lag1" = "Foreign Aid (logged, t-1)")
c_order_reversed <- rev(c_order)
sentence <- modelplot(m_list_Model, 
                      coef_omit = "Intercept", 
                      coef_map = c_order_reversed)
ggsave("figs/fig7_robustness.png", plot = sentence, width = 10, bg = "white", height = 6, dpi = 300)


# ==========================================================
# FIGURE 8
# Marginal Effcts Plot for figure 7
# ----------------------------------------------------------
# Expnanatory variable: Economic Grwoth Rate
# Dependent variable: Numbers of Death Sentence
# Interaction: Anti-system movement
# ==========================================================
charge <- plot_model(moderator_total_charge, type = "pred", 
                     terms = c("economic_growth_rate_lag1", 
                               "Anti_system_movements_lag1 [0, 2]"),
                     legend.title = "Anti-system Movement",
                     ci_color = "grey") + 
  theme_minimal() +
  scale_color_manual(labels = c("Little amounts of movements", "Large amounts of movements"),
                     values = c("#F16B7F", "#4C9DDB")) +
  labs(
    title = "",  
    x = "Log of Economic Grwoth Rate",  
    y = "Predicted Probability of Death Charge"
  )+
  theme(
    legend.position = "bottom" 
  )
ggsave("figs/fig8_margins.png", plot = charge, width = 10, bg = "white", height = 6, dpi = 300)

# ==========================================================
# FIGURE 9
# Coefficient plot: IV V.S. OLS 
# ----------------------------------------------------------
# Expnanatory variable: GDP_per_capita
# Dependent variable: Selective Repression
# ==========================================================

# REMOVE NA
cleaned_repress_no_na <- na.omit(cleaned_repress)
cleaned_repress_no_na$selective_rep <- ifelse(cleaned_repress_no_na$Year >= 1970, 1, 0)

# RUN REGRESSION FOR OLS AND IV ESTIMATION
ols <- lm(selective_rep ~ poly(GDP_per_capita, 2) + Anti_system_movements+ln_econ_rate+
            ln_population_size+ln_political_pois+ln_aid_constant,
          data = cleaned_repress_no_na)
iv_model <- ivreg(selective_rep ~ poly(GDP_per_capita, 2) + Anti_system_movements+ ln_econ_rate +
                    ln_population_size+ln_political_pois+ln_aid_constant| 
                    ln_military_expenditrue + oil_price + Anti_system_movements+ln_econ_rate+
                    ln_population_size+ln_political_pois+ln_aid_constant,
                  data = cleaned_repress_no_na)

# CREATE COEFFICIENT PLOT 
m_list <- list(OLS = ols, IV = iv_model)
msummary(m_list)
c_order <- c("ln_aid_constant" = "Fireign Aid (logged, t-1)",
             "ln_political_pois" = "Political Prisoner (logged, t-1)",
             "ln_population_size" = "Population Size (logged, t-1)",
             "ln_econ_rate" = "Economic Growth Rate (logged)",
             "Anti_system_movements" = "Anti-system Movements (logged)",
             "poly(GDP_per_capita, 2)2" = "GDP per capita (second-order)",
             "poly(GDP_per_capita, 2)1" = "GDP per capita (first-order)")
iv_ols <- modelplot(m_list, 
                    coef_omit = "Intercept", coef_map = c_order)

# OUTPUT COEFFICIENT PLOT 
ggsave("figs/fig9_ols_iv.png", plot = iv_ols, width = 10, height = 6, bg = "white", dpi = 300)


# ==========================================================
# Additional Figures in Main Text
# ==========================================================

# ==========================================================
# FIGURE 1
# Comparison between Chiang Kai-shek and Chiang Ching-kuo
# ----------------------------------------------------------
# Notes:
# This figure presents smoothed trends in GDP per capita (Panel A)
# and Anti-system Movements (Panel B) in Taiwan from 1950 to 1990.
# The vertical red dashed line indicates the leadership transition 
# in 1970 from Chiang Kai-shek to Chiang Ching-kuo.
# ==========================================================

# Load the cleaned dataset and create a new variable indicating leadership period based on year
# ----------------------------------------------------------
repress_1 <- cleaned_repress %>%
  mutate(period = factor(ifelse(Year <= 1970, "Chiang Kai-shek", "Chiang Ching-kuo"), 
                         levels = c("Chiang Kai-shek", "Chiang Ching-kuo")))
# Panel B: Anti-system Movements over Time (by Leadership Period)
# ----------------------------------------------------------
plot2 <- ggplot(repress_1, aes(x = Year, y = Anti_system_movements)) +
  geom_smooth(aes(colour = period), method = "loess", se = FALSE) + 
  scale_colour_manual(values = c("Chiang Kai-shek" = "grey30", "Chiang Ching-kuo" = "grey60")) +
  geom_vline(xintercept = 1970, linetype = "dashed", color = "red") + 
  labs(x = "Year", y = "Anti-system Movements") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("B")

# Panel A: GDP per Capita over Time (by Leadership Period)
# ----------------------------------------------------------
plot1 <- ggplot( repress_1, aes(x= Year, y = GDP_per_capita)) +
  geom_smooth(aes(colour = period), method = "loess", se = FALSE) +  
  scale_colour_manual(values = c("Chiang Kai-shek" = "grey30", "Chiang Ching-kuo" = "grey60")) +
  geom_vline(xintercept = 1970, linetype = "dashed", color = "red") + 
  labs( x = "Year", y = "GDP per capita", colour = "Leadership") +
  theme_minimal() +
  ggtitle("A")

# Combine Panel A and Panel B into one figure and place the legend at the bottom
# ----------------------------------------------------------
combined_plot <- plot1 + 
  theme(legend.position = "bottom") + 
  plot2

# Save the figure as 'fig1.png'
# ----------------------------------------------------------
ggsave("figs/fig1.png", plot = combined_plot, width = 10, height = 6, dpi = 300)

# ==========================================================
# FIGURE 2
# Comparison between Chiang Kai-shek and Chiang-Ching-kuo
# ----------------------------------------------------------
# Numbers of Political Prisoners by Leadership Period
# ==========================================================

# Library dataset for Figure 2
dta_death_sentence <- read.csv("Data/dta_death_sentence.csv")

# Output the plot
ggplot(dta_death_sentence, aes(x = sentence_year, y = total_charge_death, fill = period)) +
  geom_bar(stat = "identity", position = "stack") +  
  labs(x = "Year", y = "Total Death Sentences", fill = "Leader Period") + 
  theme_minimal() + 
  scale_fill_manual(values = c("Chiang Kai-shek" = "grey30", "Chiang Ching-kuo" = "grey70")) +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

# ==========================================================
# Appendix
# ==========================================================

# =======================================================================
# TABLE A1
# Alternative DV: Physical Violence Index
# -----------------------------------------------------------------------
# Regression examining the interactive effect between economic growth rate
# and anti-system movements on the levels of Physical Violence Index.
# =======================================================================

# Before 1970 --------------------------------------------------
# Model: robust_violence_pre1970
robust_violence_pre1970 <- dynlm(
  violence ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress[cleaned_repress$Year < 1970, ]
)

# After 1970--------------------------------------------------
# Model: robust_violence_after1970
robust_violence_after1970 <- dynlm(
  violence ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress[cleaned_repress$Year  >= 1970, ]
)

# Full Sample --------------------------------------------------
# Model: robust_violence_all
robust_violence_all <- dynlm(
  violence ~ ln_GDP_per_capita_lag1 * Anti_system_movements_lag1 + 
    economic_growth_rate_lag1 + ln_population_size_lag1 + 
    ln_political_pois_lag1 + Aid_log_lag1,
  data = cleaned_repress
)

# Output regression results to LaTeX file
stargazer(robust_violence_pre1970, robust_violence_after1970, robust_violence_all,
          type = "text",  
          out = "tabs/tab_a1_appendix.tex",
          dep.var.caption = "Physical Violence Index:",
          dep.var.labels = "",
          column.labels = c("Before 1970", "After 1970", "Overall"),
          covariate.labels = c("GDP per capita (logged, t-1)", 
                               "Anti-system Movements (logged, t-1)", 
                               "GDP per capita × Anti-system Movements (t-1)",
                               "Economic Growth Rate (t-1)",
                               "Population Size (logged, t-1)", 
                               "Political Prisoner (logged, t-1)", 
                               "Aid Constant (logged, t-1)"),
          order = c("ln_GDP_per_capita_lag1", 
                    "Anti_system_movements_lag1", 
                    "ln_GDP_per_capita_lag1:Anti_system_movements_lag1", 
                    "economic_growth_rate_lag1",
                    "ln_population_size_lag1", 
                    "ln_political_pois_lag1", 
                    "Aid_log_lag1"),
          model.names = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01))


# =======================================================================
# TABLE A2
# Alternative DV: Death Sentences 
# -----------------------------------------------------------------------
# Regression examining the interactive effect between economic growth rate
# and anti-system movements on the number of death sentences.
# =======================================================================

# Run dynamic linear model with death sentences as the dependent variable
moderator_total_charge <- dynlm(
  total_charge_death ~ economic_growth_rate_lag1 * Anti_system_movements_lag1 + 
    ln_GDP_per_capita_lag1 + ln_population_size_lag1 + Aid_log_lag1,
  data = cleaned_repress
)

# Output regression results
stargazer(moderator_total_charge, 
          type = "text", 
          out = "tabs/tab_a2_appendix.tex",
          dep.var.labels = "",
          column.labels= "Death Sentence",
          covariate.labels = c("Economic Growth Rate (logged, t-1)",
                               "Economic Growth Rate X Anti-system Movements (logged, t-1)",
                               "Anti-system Movements (logged, t-1)",
                               "GDP per capita (logged, t-1)",
                               "Population Size (logged, t-1)", 
                               "Foreign Aid (logged, t-1)"),
          order =  c("economic_growth_rate_lag1",
                     "economic_growth_rate_lag1:Anti_system_movements_lag1",
                     "Anti_system_movements_lag1",
                     "ln_GDP_per_capita_lag1",
                     "ln_population_size_lag1",
                     "Aid_log_lag1"), 
          model.names = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01))  

# ==========================================================
# TABLE A3
# Robustness Check: First Stage Regression for IV Estimation
# ----------------------------------------------------------
# Notes:
# Examines the relevance of instruments (military expenditure and oil price)
# for predicting GDP per capita in the first-stage regression.
# ==========================================================

# First regression: GDP per capita on military expenditure
f1 <- lm(GDP_per_capita ~ ln_military_expenditrue, data = cleaned_repress_no_na)

# Second regression: GDP per capita on oil price
f2 <- lm(GDP_per_capita ~ oil_price, data = cleaned_repress_no_na)

# Output regression results using stargazer
stargazer(f1, f2, 
          type = "text",
          out = "tabs/tab_a3_appendix_first_stage.tex",  
          dep.var.caption = "GDP per capita (without log)",
          dep.var.labels = "",
          covariate.labels = c("Military Expenditure",
                               "Oil Price"),
          order = c("ln_military_expenditrue", 
                    "oil_price"),
          digits = 2, 
          star.cutoffs = c(0.1, 0.05, 0.01), 
          style = "default") 

# ==========================================================
# TABLE A4
# Robustness Check: OLS and IV Regression
# ----------------------------------------------------------
# Models the relationship between economic development and selective repression
# Includes polynomial terms for GDP and uses IV regression to address endogeneity
# ==========================================================

# Remove rows with missing values for analysis
cleaned_repress_no_na <- na.omit(cleaned_repress)

# Create binary dependent variable: 1 if after 1970 (Chiang Ching-kuo), 0 otherwise
cleaned_repress_no_na$selective_rep <- ifelse(cleaned_repress_no_na$Year >= 1970, 1, 0)

# OLS regression with second-order polynomial for GDP per capita
ols <- lm(selective_rep ~ poly(GDP_per_capita, 2) + Anti_system_movements+ln_econ_rate+
            ln_population_size+ln_political_pois+ln_aid_constant,
          data = cleaned_repress_no_na)

# Instrumental Variable (IV) regression
# Instruments: military expenditure and oil price (for GDP)
iv_model <- ivreg(selective_rep ~ poly(GDP_per_capita, 2) + Anti_system_movements+ ln_econ_rate +
                    ln_population_size+ln_political_pois+ln_aid_constant| 
                    ln_military_expenditrue + oil_price + Anti_system_movements+ln_econ_rate+
                    ln_population_size+ln_political_pois+ln_aid_constant,
                  data = cleaned_repress_no_na)

# Output regression results using stargazer
# Exports both OLS and IV estimates to LaTeX
stargazer(ols, iv_model, 
          type = "text",
          out = "tabs/tab_a4_appendix.tex",   
          dep.var.caption = "Selective Repression",
          dep.var.labels = "",
          covariate.labels = c("GDP per capita (first-order)", 
                               "GDP per capita (second-order)",
                               "Anti-system Movements (logged, t-1)",
                               "Economic Growth Rate (logged, t-1)",
                               "Population Size (logged, t-1)", 
                               "Political Prisoner (logged, t-1)", 
                               "Aid Constant (logged, t-1)"),
          order = c("ln_aid_constant", 
                    "ln_political_pois", 
                    "ln_population_size", 
                    "ln_econ_rate", 
                    "Anti_system_movements", 
                    "poly(GDP_per_capita, 2)2", 
                    "poly(GDP_per_capita, 2)1"), 
          model.names = TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 3)

# ==========================================================
# FIGURE A1
# Political Prisoner Sentences by Year and Leader
# ----------------------------------------------------------
# Stacked bar plot of sentence types (1949–1988), 
# colored by leadership period and faceted by sentence outcome.
# ==========================================================

# Load required data
dta_sentence_outcome <- read.csv("Data/dta_sentence_outcome.csv")

# Output the plot
ggplot(dta_sentence_outcome, 
       aes(x = sentence_year, y = count, fill = leader_period)) +
  geom_col(width = 0.8) + 
  facet_wrap(~ type, scales = "free_y") +
  labs(
    title = "",
    x = "Year",
    y = "Count",
    fill = "Leader Period"
  ) +
  scale_fill_manual(values = c(
    "Chiang Kai-shek"  = "grey30",
    "Chiang Ching-kuo" = "grey70"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12)
  )

# ==========================================================
# FIGURE A2
# Predicted Probability of Selective Repression by GDP per capita
# ----------------------------------------------------------
# Visualizes predicted probabilities from IV model with polynomial terms
# Panel A: Linear fit
# Panel B: Quadratic fit
# ==========================================================

# Compute predicted values from the IV model (logit transformation)
pred_values_no_na <- predict(iv_model, type = "response")
pred_values_logit <- 1 / (1 + exp(-pred_values_no_na))

# Create a dataframe for plotting predicted values against GDP per capita
plot_datas_logit <- data.frame(
  GDP_per_capita = cleaned_repress_no_na$GDP_per_capita,
  pred_values = pred_values_logit 
)

# Panel A: Linear prediction line of selective repression probability
l <- ggplot(plot_datas_logit, aes(x = GDP_per_capita, y = pred_values_logit)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), color = "darkgrey", se = FALSE, linetype = "solid") + 
  labs(title = "",
       x = "GDP per capita",
       y = "Predicted Probability of Selective Repression") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) 

# Panel B: Quadratic prediction line of selective repression probability
q <-ggplot(plot_datas_logit, aes(x = GDP_per_capita, y = pred_values_logit)) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "lightgrey", se = FALSE, linetype = "solid") +  
  labs(title = "",
       x = "GDP per capita (squared)",
       y = "Predicted Probability of Selective Repression") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) 
all_plot <- grid.arrange(l, q, ncol = 2)

# Save the combined figure as "fig_appen_A2.png"
ggsave("figs/fig_appen_A2.png", plot = all_plot , width = 10, height = 6, dpi = 300)

# ==========================================================
# FIGURE A3
# Partial Residual Plot for Military Expenditure Instrument
# ----------------------------------------------------------
# Visualizes the relationship between the residualized instrument
# (military expenditure) and the residualized treatment variable
# (selective repression), with covariates partialled out.
# ==========================================================

# Regress the instrument (military expenditure) on all covariates to obtain residuals
iv_mil_res <- lm(ln_military_expenditrue ~ GDP_per_capita + Anti_system_movements+ ln_econ_rate +
                   ln_population_size+ln_political_pois+ln_aid_constant, data = cleaned_repress_no_na)$residuals

# Regress the treatment (selective repression) on all covariates to obtain residuals
dv_mil_res <- lm(selective_rep ~ GDP_per_capita + Anti_system_movements+ ln_econ_rate +
                   ln_population_size+ln_political_pois+ln_aid_constant, data = cleaned_repress_no_na)$residuals

# Set up PNG device
png("figs/fig_appen_A3_partial_residual.png", width = 800, height = 600, res = 300)

# Create a partial residual plot to show the relationship after removing covariate effects
plot(iv_mil_res, dv_mil_res, col = "#777777", cex = 0.5, 
     main = "Covariates Partialled Out (military expenditure)", 
     xlab = "Residualized Instrument", 
     ylab = "Residualized Treatment")
abline(lm(dv_mil_res ~ iv_mil_res), col = 2, lwd = 2, lty = 2)

# Close the device to save the file
dev.off()

# ==========================================================
# FIGURE A4
# Partial Residual Plot for Oil Price Instrument
# ----------------------------------------------------------
# Visualizes the relationship between the residualized instrument
# (oil price) and the residualized treatment variable
# (selective repression), with covariates partialled out.
# ==========================================================

# Regress the instrument (oil price) on all covariates to obtain residuals
iv_oil_res <- lm(oil_price ~ GDP_per_capita + Anti_system_movements+ ln_econ_rate +
                   ln_population_size+ln_political_pois+ln_aid_constant, data = cleaned_repress_no_na)$residuals

# Regress the treatment (selective repression) on all covariates to obtain residuals
dv_oil_res <- lm(selective_rep ~ GDP_per_capita + Anti_system_movements+ ln_econ_rate +
                   ln_population_size+ln_political_pois+ln_aid_constant, data = cleaned_repress_no_na)$residuals

# Set up PNG device
png("figs/fig_appen_A4_partial_residual.png", width = 800, height = 600, res = 300)

# Create a partial residual plot to show the relationship after removing covariate effects
plot(iv_oil_res, dv_oil_res, col = "#777777", cex = 0.5, 
     main = "Covariates Partialled Out (oil price)", 
     xlab = "Residualized Instrument", 
     ylab = "Residualized Treatment")
abline(lm(dv_oil_res ~ iv_oil_res), col = 2, lwd = 2, lty = 2)

# Close the device to save the file
dev.off()



