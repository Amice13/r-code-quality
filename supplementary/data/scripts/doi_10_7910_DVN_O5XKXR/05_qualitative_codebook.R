# =============================================================================
# SCRIPT 01: DATA PREPARATION AND VARIABLE CODING
# =============================================================================
# Project : Social Networks, Gender, and Graduate Hiring in a Rentier State:
#           Disentangling Referral Types and Ascriptive Signals in Omani
#           Labour Markets
# Author  : [Blinded for review]
# Dataset : Harvard Dataverse doi:10.7910/DVN/R2W8AW
# Date    : March 2026
# R ver.  : 4.3.x
# =============================================================================

# ── PACKAGES ──────────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,   # data wrangling
  haven,       # read Stata/SPSS files
  labelled,    # variable labels
  janitor,     # clean_names
  skimr,       # data summary
  naniar,      # missing data
  psych        # ICC / alpha
)

# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────
# The dataset is deposited at Harvard Dataverse:
# doi:10.7910/DVN/R2W8AW
# Files: vignette_data.csv, employer_data.csv, interview_codebook.xlsx

# Option A: read CSV directly from Dataverse
base_url <- "https://dataverse.harvard.edu/api/access/datafile/"

# vignette-level dataset (long format: one row per vignette rating)
vig <- read_csv("data/vignette_data.csv")   # or replace with Dataverse URL

# employer-level dataset (one row per recruiter)
emp <- read_csv("data/employer_data.csv")

# ── 2. INSPECT ────────────────────────────────────────────────────────────────
glimpse(vig)
glimpse(emp)
skim(vig)

# ── 3. VIGNETTE VARIABLE CODING ───────────────────────────────────────────────
# All vignette attributes use effects coding (deviation coding) so that
# intercepts represent grand means, facilitating comparison of effect sizes.
# Reference categories for dummy coding are stated in each section.

vig <- vig |>
  # ── 3a. Referral (key IV; 5 levels)
  # Reference: None (no referral mentioned)
  mutate(
    ref_instcoop   = as.integer(referral == "inst_coop"),    # cooperating univ
    ref_instnoc    = as.integer(referral == "inst_nocoop"),  # non-coop univ
    ref_employee   = as.integer(referral == "employee"),     # current employee
    ref_personal   = as.integer(referral == "personal"),     # personal/family
    referral_f     = factor(referral,
                            levels = c("none","inst_nocoop","inst_coop",
                                       "employee","personal"),
                            labels = c("None","Univ (No Coop)","Univ (Coop)",
                                       "Employee","Personal"))
  ) |>

  # ── 3b. Gender (key IV; binary)
  # Reference: Female
  mutate(
    male           = as.integer(gender == "male"),
    gender_f       = factor(gender, levels = c("female","male"))
  ) |>

  # ── 3c. Field of Study (3 levels)
  # Reference: Unrelated
  mutate(
    field_related  = as.integer(field == "related"),
    field_relevant = as.integer(field == "relevant"),
    field_f        = factor(field,
                            levels = c("unrelated","related","relevant"),
                            labels = c("Unrelated","Related","Relevant"))
  ) |>

  # ── 3d. Education Level (3 levels)
  # Reference: Diploma
  mutate(
    edu_bach       = as.integer(education == "bachelors"),
    edu_mast       = as.integer(education == "masters"),
    edu_f          = factor(education,
                            levels = c("diploma","bachelors","masters"),
                            labels = c("Diploma","Bachelor's","Master's"))
  ) |>

  # ── 3e. GPA (3 levels)
  # Reference: 2.0 (Pass)
  mutate(
    gpa_good       = as.integer(gpa == "3.2"),
    gpa_excel      = as.integer(gpa == "3.8"),
    gpa_f          = factor(gpa,
                            levels = c("2.0","3.2","3.8"),
                            labels = c("2.0 (Pass)","3.2 (Good)","3.8 (Excellent)"))
  ) |>

  # ── 3f. University Rank (3 levels)
  # Reference: Local Oman
  mutate(
    uni_regional   = as.integer(uni_rank == "regional"),
    uni_global     = as.integer(uni_rank == "global"),
    uni_f          = factor(uni_rank,
                            levels = c("local","regional","global"),
                            labels = c("Local Oman","Regional GCC","Top Global"))
  ) |>

  # ── 3g. Work Experience (3 levels)
  # Reference: None
  mutate(
    exp_intern     = as.integer(experience == "internship"),
    exp_work       = as.integer(experience == "work"),
    exp_f          = factor(experience,
                            levels = c("none","internship","work"),
                            labels = c("None","6-mo Internship","1-yr Work"))
  ) |>

  # ── 3h. Nationality (3 levels)
  # Reference: Expatriate (Indian)
  mutate(
    nat_gcc        = as.integer(nationality == "gcc"),
    nat_omani      = as.integer(nationality == "omani"),
    nat_f          = factor(nationality,
                            levels = c("expat","gcc","omani"),
                            labels = c("Expat (Indian)","GCC (UAE)","Omani"))
  ) |>

  # ── 3i. Extracurriculars (3 levels)
  # Reference: None
  mutate(
    extra_basic    = as.integer(extracurric == "basic"),
    extra_leader   = as.integer(extracurric == "leadership"),
    extra_f        = factor(extracurric,
                            levels = c("none","basic","leadership"),
                            labels = c("None","Basic (clubs)","Leadership"))
  ) |>

  # ── 3j. Vignette order (for order-effects check)
  mutate(
    vignette_order = as.integer(vignette_position),
    order_sq       = vignette_order^2
  )

# ── 4. EMPLOYER-LEVEL VARIABLE CODING ─────────────────────────────────────────
emp <- emp |>
  mutate(
    # Firm size: binary (ref: SME)
    large_firm     = as.integer(firm_size == "large"),
    firm_size_f    = factor(firm_size, levels = c("sme","large"),
                            labels = c("SME (<100)","Large (100+)")),

    # Sector (ref: Accounting)
    sector_eng     = as.integer(sector == "engineering"),
    sector_it      = as.integer(sector == "it"),
    sector_f       = factor(sector,
                            levels = c("accounting","it","engineering"),
                            labels = c("Accounting","IT","Engineering")),

    # Recruiter experience (continuous, centred at mean)
    rec_exp_c      = scale(recruiter_exp, center = TRUE, scale = FALSE)[,1],

    # Recruiter gender (ref: male)
    rec_female     = as.integer(recruiter_gender == "female")
  )

# ── 5. MERGE VIGNETTE + EMPLOYER DATA ────────────────────────────────────────
df <- vig |>
  left_join(emp, by = "resp_id")

cat("Merged dataset dimensions:", nrow(df), "rows ×", ncol(df), "cols\n")
cat("Unique employers:", n_distinct(df$resp_id), "\n")
cat("Unique vignettes:", n_distinct(df$vignette_id), "\n")

# ── 6. OUTCOME VARIABLES ─────────────────────────────────────────────────────
# hire  : hiring propensity 0-10 ("How likely to shortlist for interview?")
# train : trainability 0-10 ("Expected trainability in 6 months?")
# Both are used as continuous outcomes in MLM (validated by histograms below)

summary(df$hire)
summary(df$train)

hist(df$hire,  main = "Hiring Propensity Distribution",  xlab = "Score (0-10)")
hist(df$train, main = "Trainability Distribution",       xlab = "Score (0-10)")

# ── 7. INTRA-CLASS CORRELATION (EMPLOYER CLUSTERING) ────────────────────────
# Calculate ICC to justify multilevel approach
library(lme4)
null_hire  <- lmer(hire  ~ 1 + (1|resp_id), data = df, REML = TRUE)
null_train <- lmer(train ~ 1 + (1|resp_id), data = df, REML = TRUE)

icc_hire  <- performance::icc(null_hire)
icc_train <- performance::icc(null_train)

cat("ICC Hiring Propensity :", round(icc_hire$ICC_adjusted,  3), "\n")
cat("ICC Trainability      :", round(icc_train$ICC_adjusted, 3), "\n")
# Expected: ~0.87 and ~0.84 — justifies random intercept at employer level

# ── 8. VIGNETTE ORDER / FATIGUE CHECK ────────────────────────────────────────
order_check <- lm(hire ~ vignette_order, data = df)
summary(order_check)
# Expect: non-significant coefficient on vignette_order (F-test p > .05)

# ── 9. SAVE PREPARED DATASET ─────────────────────────────────────────────────
write_csv(df,  "data/df_prepared.csv")
write_rds(df,  "data/df_prepared.rds")
write_csv(emp, "data/emp_prepared.csv")

cat("\n✓ Data preparation complete. Prepared datasets saved to data/\n")
