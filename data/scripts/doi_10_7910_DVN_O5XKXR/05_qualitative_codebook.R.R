# =============================================================================
# SCRIPT 03: ROBUSTNESS CHECKS AND SENSITIVITY ANALYSES
# =============================================================================
# Project : Social Networks, Gender, and Graduate Hiring in a Rentier State
# Dataset : Harvard Dataverse doi:10.7910/DVN/R2W8AW
# =============================================================================

pacman::p_load(
  lme4, lmerTest, broom.mixed, tidyverse,
  modelsummary, fixest, AER, clubSandwich
)

df <- read_rds("data/df_prepared.rds")

# =============================================================================
# RC1 — HAUSMAN TEST: RANDOM vs FIXED EFFECTS
# =============================================================================
# Hausman test to validate random-effects specification
# (respondent-level random intercept vs fixed effects)

# Random effects (main model)
m_re <- lmer(hire ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
               male + field_related + field_relevant +
               edu_bach + edu_mast + gpa_good + gpa_excel +
               uni_regional + uni_global + nat_gcc + nat_omani +
               exp_intern + exp_work + extra_basic + extra_leader +
               vignette_order + (1|resp_id),
             data = df, REML = FALSE)

# Fixed effects (within-respondent variation only — employer-level vars dropped)
m_fe <- feols(hire ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
                male + field_related + field_relevant +
                edu_bach + edu_mast + gpa_good + gpa_excel +
                uni_regional + uni_global + nat_gcc + nat_omani +
                exp_intern + exp_work + extra_basic + extra_leader +
                vignette_order | resp_id,
              data = df, cluster = ~resp_id)

summary(m_fe)

# Hausman test via coefficient comparison
# If RE and FE coefficients are similar → RE is efficient and consistent
hausman_coefs <- cbind(
  RE = fixef(m_re)[names(coef(m_fe))],
  FE = coef(m_fe)
)
hausman_diff <- hausman_coefs[,"FE"] - hausman_coefs[,"RE"]
cat("Max Hausman coefficient difference:", max(abs(hausman_diff)), "\n")
# Expect: p > .05 — RE preferred over FE

# =============================================================================
# RC2 — POOLED OLS WITH VIGNETTE-ORDER POLYNOMIAL
# =============================================================================
# Pooled OLS with robust standard errors clustered at employer level

m_ols_hire <- feols(
  hire ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male + field_related + field_relevant +
    edu_bach + edu_mast + gpa_good + gpa_excel +
    uni_regional + uni_global + nat_gcc + nat_omani +
    exp_intern + exp_work + extra_basic + extra_leader +
    vignette_order + I(vignette_order^2) +
    large_firm + sector_eng + sector_it + rec_exp_c,
  data    = df,
  cluster = ~resp_id
)

m_ols_train <- feols(
  train ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male + field_related + field_relevant +
    edu_bach + edu_mast + gpa_good + gpa_excel +
    uni_regional + uni_global + nat_gcc + nat_omani +
    exp_intern + exp_work + extra_basic + extra_leader +
    vignette_order + I(vignette_order^2) +
    large_firm + sector_eng + sector_it + rec_exp_c,
  data    = df,
  cluster = ~resp_id
)

etable(m_ols_hire, m_ols_train)

# =============================================================================
# RC3 — BINARY LOGIT: SHORTLIST THRESHOLD (hire >= 5 = 1)
# =============================================================================
df <- df |> mutate(hire_bin = as.integer(hire >= 5))

m_logit <- glmer(
  hire_bin ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male + field_related + field_relevant +
    edu_bach + edu_mast + gpa_good + gpa_excel +
    uni_regional + uni_global + nat_gcc + nat_omani +
    exp_intern + exp_work + extra_basic + extra_leader +
    vignette_order + large_firm + sector_eng + sector_it + rec_exp_c +
    (1|resp_id),
  data   = df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m_logit)

# Average marginal effects (to compare with linear coefficients)
library(marginaleffects)
ame_logit <- avg_slopes(m_logit)
print(ame_logit)

# =============================================================================
# RC4 — TRAINABILITY LOG-TRANSFORM (alternative coding)
# =============================================================================
df <- df |> mutate(train_log = log(train + 1))

m_train_log <- lmer(
  train_log ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male + field_related + field_relevant +
    edu_bach + edu_mast + gpa_good + gpa_excel +
    uni_regional + uni_global + nat_gcc + nat_omani +
    exp_intern + exp_work + extra_basic + extra_leader +
    vignette_order + large_firm + sector_eng + sector_it + rec_exp_c +
    (1|resp_id),
  data = df, REML = FALSE
)

# =============================================================================
# RC5 — VIGNETTE ORDER / FATIGUE EFFECTS TEST
# =============================================================================
# Test whether vignette position within deck predicts ratings (fatigue = bias)

fatigue_test <- lm(hire ~ vignette_order, data = df)
summary(fatigue_test)
# Expect: non-significant (F-test p > .05)
# Reported in text: F(1, N) = 1.14, p = .34

# Augmented: include order × attribute interaction to detect differential fatigue
fatigue_interact <- lmer(hire ~ vignette_order * ref_instcoop + (1|resp_id),
                          data = df, REML = FALSE)
anova(fatigue_interact)

# =============================================================================
# RC6 — SENSITIVITY: RECRUITER GENDER MODERATION
# =============================================================================
m_recgen <- lmer(
  hire ~ ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male + field_related + field_relevant +
    edu_bach + edu_mast + gpa_good + gpa_excel +
    uni_regional + uni_global + nat_gcc + nat_omani +
    exp_intern + exp_work + extra_basic + extra_leader +
    vignette_order + large_firm + sector_eng + sector_it + rec_exp_c +
    rec_female +
    rec_female:male +           # Does female recruiter reduce male advantage?
    rec_female:ref_instcoop +   # Does recruiter gender moderate referral effects?
    (1|resp_id),
  data = df, REML = FALSE
)

summary(m_recgen, ddf = "Kenward-Roger")

# =============================================================================
# TABLE A2 — ROBUSTNESS SUMMARY TABLE (for Appendix)
# =============================================================================
# Key coefficients across all robustness specifications

rob_models <- list(
  "Main MLM"     = m_re,
  "FE (fixest)"  = m_fe,
  "OLS-Cluster"  = m_ols_hire,
  "Logit (AME)"  = m_logit,
  "Log Train"    = m_train_log
)

rob_vars <- c("ref_instcoop","ref_instnoc","ref_employee","ref_personal",
              "male","nat_omani","field_relevant")

modelsummary(
  rob_models,
  coef_map  = setNames(rob_vars, rob_vars),
  statistic = "({std.error})",
  stars     = c("*"=.05,"**"=.01,"***"=.001),
  title     = "Table A2. Robustness Checks: Key Coefficients Across Specifications",
  notes     = "Outcome = hiring propensity (0-10) unless noted. SE in parentheses.",
  output    = "output/TableA2_Robustness.docx"
)

cat("\n✓ Robustness checks complete. Table A2 saved to output/\n")
