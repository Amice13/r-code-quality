# =============================================================================
# SCRIPT 02: MULTILEVEL LINEAR REGRESSION — MAIN MODELS
# =============================================================================
# Project : Social Networks, Gender, and Graduate Hiring in a Rentier State
# Dataset : Harvard Dataverse doi:10.7910/DVN/R2W8AW
# Models  : M0 (null), M1 (main effects), M2 (interactions)
# Software: lme4, lmerTest, broom.mixed
# =============================================================================

pacman::p_load(
  lme4, lmerTest, broom.mixed, performance,
  tidyverse, modelsummary, flextable, officer,
  emmeans, marginaleffects, ggplot2, ggeffects
)

df <- read_rds("data/df_prepared.rds")

# =============================================================================
# MODEL 0 — NULL (unconditional means model)
# =============================================================================
# Purpose: Establish baseline ICC and justify multilevel approach

m0_hire  <- lmer(hire  ~ 1 + (1|resp_id), data = df, REML = TRUE)
m0_train <- lmer(train ~ 1 + (1|resp_id), data = df, REML = TRUE)

performance::icc(m0_hire)   # ICC hiring propensity
performance::icc(m0_train)  # ICC trainability

# Hausman-equivalent test: compare random vs fixed effects
# lmerTest handles this via likelihood ratio test
anova(m0_hire, update(m0_hire, REML = FALSE))

# =============================================================================
# MODEL 1 — MAIN EFFECTS
# =============================================================================
# All nine vignette attributes as main effects + employer-level controls
# Random intercept for employer (resp_id)
# Vignette order included as covariate to rule out fatigue effects

m1_hire <- lmer(
  hire ~
    # ── KEY IVs: Referral (ref: None) ──────────────────────────────────────
    ref_instcoop + ref_instnoc + ref_employee + ref_personal +

    # ── KEY IV: Gender (ref: Female) ───────────────────────────────────────
    male +

    # ── Educational credentials ────────────────────────────────────────────
    field_related + field_relevant +
    edu_bach + edu_mast +
    gpa_good + gpa_excel +
    uni_regional + uni_global +

    # ── Other vignette attributes ──────────────────────────────────────────
    nat_gcc + nat_omani +
    exp_intern + exp_work +
    extra_basic + extra_leader +

    # ── Order control ──────────────────────────────────────────────────────
    vignette_order +

    # ── Employer-level controls ────────────────────────────────────────────
    large_firm + sector_eng + sector_it + rec_exp_c +

    # ── Random intercept ───────────────────────────────────────────────────
    (1|resp_id),
  data = df, REML = FALSE   # ML for model comparison
)

m1_train <- lmer(
  train ~
    ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male +
    field_related + field_relevant +
    edu_bach + edu_mast +
    gpa_good + gpa_excel +
    uni_regional + uni_global +
    nat_gcc + nat_omani +
    exp_intern + exp_work +
    extra_basic + extra_leader +
    vignette_order +
    large_firm + sector_eng + sector_it + rec_exp_c +
    (1|resp_id),
  data = df, REML = FALSE
)

# ── Summary ───────────────────────────────────────────────────────────────────
summary(m1_hire,  ddf = "Kenward-Roger")
summary(m1_train, ddf = "Kenward-Roger")

# ── Pseudo R² (Nakagawa & Schielzeth, 2013) ───────────────────────────────────
performance::r2(m1_hire)
performance::r2(m1_train)

# =============================================================================
# MODEL 2 — INTERACTIONS (H3a, H3b, H3c)
# =============================================================================
# H3a: Gender × Institutional Coop Referral
# H3b: Occupation × Referral / Occupation × Gender
# H3c: Firm Size × Employee Referral

m2_hire <- lmer(
  hire ~
    ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male +
    field_related + field_relevant +
    edu_bach + edu_mast +
    gpa_good + gpa_excel +
    uni_regional + uni_global +
    nat_gcc + nat_omani +
    exp_intern + exp_work +
    extra_basic + extra_leader +
    vignette_order +
    large_firm + sector_eng + sector_it + rec_exp_c +

    # ── H3a: Gender × Referral ──────────────────────────────────────────────
    male:ref_instcoop +          # gender × coop institutional
    male:ref_employee +          # gender × employee (also test)

    # ── H3b: Occupation × Referral ─────────────────────────────────────────
    sector_eng:ref_instcoop +    # engineering amplifies institutional
    sector_eng:male +            # engineering gender penalty

    # ── H3c: Firm Size × Employee Referral ─────────────────────────────────
    large_firm:ref_instcoop +    # large firm × coop
    large_firm:ref_employee +    # large firm × employee (null expected in SME)

    (1|resp_id),
  data = df, REML = FALSE
)

m2_train <- lmer(
  train ~
    ref_instcoop + ref_instnoc + ref_employee + ref_personal +
    male +
    field_related + field_relevant +
    edu_bach + edu_mast +
    gpa_good + gpa_excel +
    uni_regional + uni_global +
    nat_gcc + nat_omani +
    exp_intern + exp_work +
    extra_basic + extra_leader +
    vignette_order +
    large_firm + sector_eng + sector_it + rec_exp_c +
    male:ref_instcoop +
    male:ref_employee +
    sector_eng:ref_instcoop +
    sector_eng:male +
    large_firm:ref_instcoop +
    large_firm:ref_employee +
    (1|resp_id),
  data = df, REML = FALSE
)

summary(m2_hire,  ddf = "Kenward-Roger")
summary(m2_train, ddf = "Kenward-Roger")

# ── Model comparison (LRT) ───────────────────────────────────────────────────
anova(m1_hire, m2_hire)
anova(m1_train, m2_train)

# =============================================================================
# TABLE 2 — REGRESSION COEFFICIENT TABLE (publication-ready)
# =============================================================================
# Using modelsummary for a clean combined table

models_hire  <- list("Hiring (M1)"  = m1_hire,  "Hiring (M2)"  = m2_hire)
models_train <- list("Train. (M1)" = m1_train, "Train. (M2)" = m2_train)
all_models   <- c(models_hire, models_train)

# Clean coefficient names
coef_map <- c(
  "(Intercept)"          = "Intercept",
  "ref_instcoop"         = "Referral: Inst. Coop (ref: None)",
  "ref_instnoc"          = "Referral: Inst. Non-Coop",
  "ref_employee"         = "Referral: Employee",
  "ref_personal"         = "Referral: Personal",
  "male"                 = "Male Gender (ref: Female)",
  "nat_omani"            = "Omani Nationality (ref: Expat)",
  "nat_gcc"              = "GCC Nationality",
  "field_relevant"       = "Field: Relevant (ref: Unrelated)",
  "field_related"        = "Field: Related",
  "edu_bach"             = "Education: Bachelor's (ref: Diploma)",
  "edu_mast"             = "Education: Master's",
  "gpa_excel"            = "GPA: 3.8 (ref: 2.0)",
  "gpa_good"             = "GPA: 3.2",
  "uni_global"           = "University: Global (ref: Local)",
  "uni_regional"         = "University: Regional",
  "exp_intern"           = "Experience: 6-mo Internship (ref: None)",
  "exp_work"             = "Experience: 1-yr Work",
  "extra_leader"         = "Extracurric.: Leadership (ref: None)",
  "extra_basic"          = "Extracurric.: Basic",
  "vignette_order"       = "Vignette Order",
  "large_firm"           = "Large Firm (ref: SME)",
  "sector_eng"           = "Sector: Engineering (ref: Accounting)",
  "sector_it"            = "Sector: IT",
  "rec_exp_c"            = "Recruiter Experience (centred)",
  "male:ref_instcoop"    = "  × Male × Inst. Coop",
  "male:ref_employee"    = "  × Male × Employee Ref.",
  "sector_eng:ref_instcoop" = "  × Engineering × Inst. Coop",
  "sector_eng:male"      = "  × Engineering × Male",
  "large_firm:ref_instcoop" = "  × Large Firm × Inst. Coop",
  "large_firm:ref_employee" = "  × Large Firm × Employee Ref."
)

tbl <- modelsummary(
  all_models,
  coef_map    = coef_map,
  statistic   = "({std.error})",
  stars       = c("*"=.05, "**"=.01, "***"=.001),
  gof_map     = c("nobs","r2.marginal","r2.conditional","icc","rmse"),
  title       = "Table 2. Multilevel Linear Regression: Referral and Gender Effects",
  notes       = "Standard errors in parentheses. * p<.05, ** p<.01, *** p<.001. All models include random intercepts for employer.",
  output      = "flextable"
)

save_as_docx(tbl, path = "output/Table2_MLM_Results.docx")

# =============================================================================
# FIGURE 1 — FOREST PLOT: MAIN EFFECTS
# =============================================================================
tidy_m1 <- tidy(m1_hire, conf.int = TRUE, effects = "fixed") |>
  filter(term != "(Intercept)", !str_detect(term, "vignette_order|rec_exp")) |>
  mutate(
    label    = coef_map[term],
    category = case_when(
      str_detect(term, "ref_") ~ "Referral Type",
      term == "male"           ~ "Ascriptive: Gender",
      term %in% c("nat_omani","nat_gcc") ~ "Ascriptive: Nationality",
      str_detect(term, "field|edu|gpa|uni") ~ "Educational Credential",
      str_detect(term, "exp_|extra") ~ "Human Capital (other)",
      TRUE ~ "Employer Context"
    ),
    label = factor(label, levels = rev(label))
  )

p1 <- ggplot(tidy_m1, aes(x = estimate, xmin = conf.low, xmax = conf.high,
                           y = label, colour = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(height = 0.3, linewidth = 0.6) +
  geom_point(size = 2.5) +
  scale_colour_brewer(palette = "Dark2", name = "Attribute Category") +
  labs(
    title  = "Figure 1. Standardised Main Effects on Hiring Propensity",
    subtitle = "95% Confidence Intervals. Reference categories: None (referral), Female (gender),\nExpat (nationality), Diploma (education), 2.0 GPA, Local Oman university, No experience.",
    x      = "Unstandardised Coefficient (b)",
    y      = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave("output/Figure1_MainEffects.pdf", p1, width = 8, height = 7)
ggsave("output/Figure1_MainEffects.png", p1, width = 8, height = 7, dpi = 300)

# =============================================================================
# FIGURE 2 — INTERACTION: GENDER × REFERRAL
# =============================================================================
# Predicted hiring propensity by referral type × gender (H3a)

preds_gender_ref <- ggpredict(m2_hire, terms = c("ref_instcoop","male"))

p2 <- plot(preds_gender_ref) +
  labs(
    title    = "Figure 2. Predicted Hiring Propensity: Gender × Referral Type",
    subtitle = "Marginal effects at means. Error bars = 95% CI.",
    x        = "Cooperating University Referral",
    y        = "Predicted Hiring Propensity (0–10)",
    colour   = "Gender"
  ) +
  scale_colour_manual(values = c("0" = "#E41A1C", "1" = "#377EB8"),
                      labels = c("Female","Male")) +
  theme_bw(base_size = 11)

ggsave("output/Figure2_GenderReferralInteraction.pdf", p2, width = 6, height = 5)
ggsave("output/Figure2_GenderReferralInteraction.png", p2, width = 6, height = 5, dpi = 300)

# =============================================================================
# FIGURE 3 — INTERACTION: FIRM SIZE × EMPLOYEE REFERRAL (H3c)
# =============================================================================
preds_firm_ref <- ggpredict(m2_hire,
                             terms = c("ref_employee","large_firm"))

p3 <- plot(preds_firm_ref) +
  labs(
    title  = "Figure 3. Predicted Hiring Propensity: Firm Size × Employee Referral",
    x      = "Employee Referral",
    y      = "Predicted Hiring Propensity (0–10)",
    colour = "Firm Size"
  ) +
  scale_colour_manual(values = c("0" = "#FF7F00", "1" = "#4DAF4A"),
                      labels = c("SME (<100)","Large (100+)")) +
  theme_bw(base_size = 11)

ggsave("output/Figure3_FirmSizeReferral.pdf", p3, width = 6, height = 5)
ggsave("output/Figure3_FirmSizeReferral.png", p3, width = 6, height = 5, dpi = 300)

# =============================================================================
# MARGINAL MEANS: REFERRAL TYPE HIERARCHY
# =============================================================================
# Estimated marginal means by referral type (H1: inst coop > employee)
emm_ref <- emmeans(m1_hire, ~ referral_f, data = df |>
                    mutate(referral_f = factor(referral,
                    levels = c("none","inst_nocoop","inst_coop","employee","personal"))))

pairs(emm_ref, adjust = "bonferroni")
# Key contrast: inst_coop vs employee — expect significant positive difference

cat("\n✓ Main model analysis complete. Tables and figures saved to output/\n")
