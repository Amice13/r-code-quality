# ============================================================================
# Replication Script
# "Federal Override or Force Multiplier? State Capacity and the
#  Heterogeneous Effects of Medicaid Expansion"
#
# Author: [Redacted for review]
# Journal: Journal of Public Administration Research and Theory
# Last updated: 2026-02-25
#
# INPUT:  master_panel.csv (500 obs: 50 states × 10 years, 2010–2019)
# OUTPUT: All tables and figures reported in the manuscript
# ============================================================================

# --- 1. Setup ----------------------------------------------------------------

library(tidyverse)
library(did)          # Callaway & Sant'Anna (2021)
library(HonestDiD)    # Rambachan & Roth (2023)
library(plm)          # Two-way fixed effects
library(lmtest)       # Coefficient testing
library(sandwich)     # Robust standard errors

set.seed(42)  # Reproducibility for bootstrap inference

# --- 2. Load Data ------------------------------------------------------------

master_panel <- read_csv("master_panel.csv")

# Treatment cohort variable: states expanding after 2019 are coded as
# never-treated (g = 0) because no post-treatment data exist in-sample.
master_panel <- master_panel %>%
  mutate(
    g = case_when(
      expansion_year == 0    ~ 0,
      expansion_year > 2019  ~ 0,
      TRUE                   ~ expansion_year
    ),
    stateno     = as.integer(as.factor(NAME)),
    high_squire = ifelse(squire >= median(squire), 1, 0)
  )

cat("Panel dimensions:", nrow(master_panel), "obs,",
    n_distinct(master_panel$NAME), "states,",
    n_distinct(master_panel$year), "years\n")

# Cohort composition
master_panel %>%
  group_by(g) %>%
  summarise(n_states = n_distinct(NAME), .groups = "drop") %>%
  print()

# --- 3. Table 2, Column 1–2: TWFE (Appendix Specification) ------------------

model_twfe <- plm(
  public_cov_pct ~ squire + treated_panel + povrate + gini + median_income,
  data   = master_panel,
  index  = c("NAME", "year"),
  model  = "within",
  effect = "twoways"
)

cat("\n=== Table 2, Col 1: TWFE with OLS SE ===\n")
summary(model_twfe)

cat("\n=== Table 2, Col 2: TWFE with HC3 Clustered SE ===\n")
coeftest(model_twfe, vcov = vcovHC(model_twfe, type = "HC3", cluster = "group"))

# --- 4. Table 2, Column 3: CS DiD (Primary Specification) -------------------

cs_model <- att_gt(
  yname         = "public_cov_pct",
  tname         = "year",
  idname        = "stateno",
  gname         = "g",
  xformla       = ~ squire + povrate + gini + median_income,
  data          = master_panel,
  control_group = "notyettreated",
  bstrap        = TRUE,
  biters        = 1000,
  cband         = TRUE
)

cat("\n=== Group-Time ATT(g,t) ===\n")
summary(cs_model)

# Group aggregation → Overall ATT (headline result)
cs_group <- aggte(cs_model, type = "group")
cat("\n=== Table 2, Col 3: CS DiD Group Aggregation (Overall ATT) ===\n")
summary(cs_group)

# --- 5. Figure 2 & Table 3: Dynamic Event Study -----------------------------

cs_es <- aggte(cs_model, type = "dynamic")
cat("\n=== Table 3 & Figure 2: Dynamic Event Study ===\n")
summary(cs_es)

# Event study plot (Figure 2)
# Note: Figure numbers belong in captions, not plot titles (JPART standard)
es_data <- tibble(
  event_time = cs_es$egt,
  att        = cs_es$att.egt,
  se         = cs_es$se.egt,
  ci_lower   = att - cs_es$crit.val.egt * se,
  ci_upper   = att + cs_es$crit.val.egt * se,
  period     = ifelse(event_time < 0, "Pre-treatment", "Post-treatment")
)

ggplot(es_data, aes(x = event_time, y = att)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), alpha = 0.2) +
  geom_point(aes(color = period), size = 3) +
  geom_line(aes(color = period, group = 1), linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Pre-treatment" = "#2166AC", "Post-treatment" = "#B2182B")) +
  scale_fill_manual(values = c("Pre-treatment" = "#2166AC", "Post-treatment" = "#B2182B")) +
  scale_x_continuous(breaks = es_data$event_time) +
  labs(
    x     = "Event Time (Years Relative to Expansion)",
    y     = "ATT (Percentage Points)",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.15, 0.85))

ggsave("figure2_event_study.png", width = 8, height = 5, dpi = 300)

# --- 6. Table 4: HonestDiD Sensitivity Bounds -------------------------------

# Variance–covariance matrix via influence functions
inffunc <- cs_es$inf.function$dynamic.inf.func.e
sigma   <- cov(inffunc)
betahat <- cs_es$att.egt

sensitivity <- createSensitivityResults_relativeMagnitudes(
  betahat        = betahat,
  sigma          = sigma,
  numPrePeriods  = sum(cs_es$egt < 0),
  numPostPeriods = sum(cs_es$egt >= 0),
  Mbarvec        = seq(0.5, 2, by = 0.5)
)

cat("\n=== Table 4: HonestDiD Sensitivity Bounds ===\n")
print(sensitivity)

# --- 7. Robustness: Step-Function Squire Specification -----------------------

# Squire Index published for 2009 and 2015 only.
# Primary spec: linear interpolation. Robustness: step function.
# squire_2009 and squire_2015 columns must exist in master_panel.
# If not present, join from squire_raw (see data construction code).

if (!("squire_step" %in% names(master_panel))) {
  # Attempt to construct from squire_2009 / squire_2015 if available
  if (all(c("squire_2009", "squire_2015") %in% names(master_panel))) {
    master_panel <- master_panel %>%
      mutate(squire_step = ifelse(year <= 2014, squire_2009, squire_2015))
  } else {
    cat("WARNING: squire_2009/squire_2015 not in dataset. Skipping step-function check.\n")
  }
}

if ("squire_step" %in% names(master_panel)) {
  cs_model_step <- att_gt(
    yname         = "public_cov_pct",
    tname         = "year",
    idname        = "stateno",
    gname         = "g",
    xformla       = ~ squire_step + povrate + gini + median_income,
    data          = master_panel,
    control_group = "notyettreated",
    bstrap        = TRUE,
    biters        = 1000,
    cband         = TRUE
  )

  cat("\n=== Robustness: Step-Function Squire ===\n")
  summary(aggte(cs_model_step, type = "group"))
}

# --- 8. Heterogeneity: High vs Low Capacity ---------------------------------

# Split at median Squire Index
cs_high <- att_gt(
  yname         = "public_cov_pct",
  tname         = "year",
  idname        = "stateno",
  gname         = "g",
  xformla       = ~ povrate + gini + median_income,
  data          = master_panel %>%
                    filter(high_squire == 1) %>%
                    mutate(stateno = as.integer(as.factor(NAME))),
  control_group = "notyettreated",
  bstrap        = TRUE,
  biters        = 1000,
  cband         = TRUE
)

cs_low <- att_gt(
  yname         = "public_cov_pct",
  tname         = "year",
  idname        = "stateno",
  gname         = "g",
  xformla       = ~ povrate + gini + median_income,
  data          = master_panel %>%
                    filter(high_squire == 0) %>%
                    mutate(stateno = as.integer(as.factor(NAME))),
  control_group = "notyettreated",
  bstrap        = TRUE,
  biters        = 1000,
  cband         = TRUE
)

cat("\n=== Heterogeneity: High-Capacity States ===\n")
summary(aggte(cs_high, type = "group"))

cat("\n=== Heterogeneity: Low-Capacity States ===\n")
summary(aggte(cs_low, type = "group"))

# --- 9. Summary of Key Results -----------------------------------------------

cat("\n============================================================\n")
cat("REPLICATION SUMMARY\n")
cat("============================================================\n")
cat("Overall ATT (CS DiD, not-yet-treated):  ", round(cs_group$overall.att, 4), "pp\n")
cat("Overall SE:                              ", round(cs_group$overall.se, 4), "\n")
cat("95% CI:                                 [",
    round(cs_group$overall.att - 1.96 * cs_group$overall.se, 4), ",",
    round(cs_group$overall.att + 1.96 * cs_group$overall.se, 4), "]\n")
cat("High-capacity ATT:                       ", round(aggte(cs_high, type = "group")$overall.att, 2), "pp\n")
cat("Low-capacity ATT:                        ", round(aggte(cs_low, type = "group")$overall.att, 2), "pp\n")
if ("squire_step" %in% names(master_panel)) {
  step_att <- aggte(cs_model_step, type = "group")$overall.att
  cat("Step-function ATT:                       ", round(step_att, 4), "pp\n")
}
cat("============================================================\n")

# END OF REPLICATION SCRIPT
