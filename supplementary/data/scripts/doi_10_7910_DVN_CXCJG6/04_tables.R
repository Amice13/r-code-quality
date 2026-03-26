############################################
#                 04_TABLES                #
############################################

## Libraries
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(mice)
library(ordinal)
library(kableExtra)
library(broom)
library(purrr)
library(readr)

select <- dplyr::select
filter <- dplyr::filter

###############################
#         FILE PATHS          #
###############################

tables_path  <- here("tables")
derived_path <- here("data_derived")

###############################
#   LOADING ANALYSIS OBJECTS  #
###############################

objs <- readRDS(file.path(derived_path, "analysis_objects.rds"))

## Extracting objects
class_data_full     <- objs$class_data_full
adoption_long_1     <- objs$adoption_long_1
imputed_with_class  <- objs$imputed_with_class   # N=107 analytical sample
results_A           <- objs$results_A
results_B           <- objs$results_B
results_C           <- objs$results_C
results_D           <- objs$results_D
pooled_A            <- objs$pooled_A
pooled_B            <- objs$pooled_B
pooled_C            <- objs$pooled_C
pooled_D            <- objs$pooled_D
m1_demo             <- objs$m1_demo
m2_farm             <- objs$m2_farm
m3_comb             <- objs$m3_comb
pooled_demographics <- objs$pooled_demographics
pooled_farm         <- objs$pooled_farm
pooled_combined     <- objs$pooled_combined
n_lca_sample        <- objs$n_lca_sample
sample_flow         <- objs$sample_flow

###############################
#            TABLE 3          #
###############################
## Demographics for the analytical sample (N=107 LCA-eligible respondents)
demo_data <- complete(imputed_with_class, 1)

summarize_cat_levels <- function(var_label, var, level_map, data) {
  x     <- data[[var]]
  total <- sum(!is.na(x))
  
  rows <- map_dfr(names(level_map), function(lbl) {
    vals <- level_map[[lbl]]
    n    <- sum(x %in% vals, na.rm = TRUE)
    pct  <- paste0(round(100 * n / total, 1), "%")
    tibble(Levels = lbl, N = n, Percent = pct)
  })
  
  rows$Variable <- c(var_label, rep("", nrow(rows) - 1))
  rows %>% select(Variable, Levels, N, Percent)
}

section_header <- function(label) {
  tibble(Variable = label, Levels = "", N = NA_integer_, Percent = "")
}

## Demographics
age_rows <- summarize_cat_levels("Age", "age", list(
  "25-34 years old"   = 1,
  "35-44 years old"   = 2,
  "45-54 years old"   = 3,
  "55-64 years old"   = 4,
  "65+ years old"     = 5,
  "Prefer not to say" = 6
), demo_data)

edu_rows <- summarize_cat_levels("Education", "education", list(
  "No formal education"           = 0,
  "High school diploma"           = 2,
  "Vocational training"           = 3,
  "Associate degree"              = 4,
  "Bachelor's degree"             = 5,
  "Master's degree"               = 6,
  "Doctoral degree or equivalent" = 7,
  "Other"                         = 8
), demo_data)

inc_rows <- summarize_cat_levels("Household Income", "household_income", list(
  "< $25k"    = 0,
  "$25-50k"   = 1,
  "$50-75k"   = 2,
  "$75-100k"  = 3,
  "$100-125k" = 4,
  "$125-150k" = 5,
  "$150-175k" = 6,
  "$175-200k" = 7,
  "$200-225k" = 9,
  "> $225k"   = 10
), demo_data)

gen_rows <- summarize_cat_levels("Gender", "gender", list(
  "Female"           = 0,
  "Male"             = 1,
  "Other"            = 2
), demo_data)

race_rows <- summarize_cat_levels("Race", "race_white", list(
  "Non-White" = 0,
  "White"     = 1
), demo_data)

## Farm-Level Characteristics
own_rows <- summarize_cat_levels("Ownership", "ownership", list(
  "Own"          = 0,
  "Rent"         = 1,
  "Own and Rent" = 2
), demo_data)

farm_rows <- summarize_cat_levels("Farm Size", "farm_acres", list(
  "< 11 acres"    = 0,
  "11-50 acres"   = 1,
  "51-100 acres"  = 2,
  "101-250 acres" = 3,
  "251-500 acres" = 4,
  "> 500 acres"   = 5
), demo_data)

prod_rows <- summarize_cat_levels("Production Type", "main_product_livestock", list(
  "Livestock" = 1,
  "Other"     = 0
), demo_data)

prof_rows <- summarize_cat_levels("Professionalism", "farmer_type", list(
  "Full-time farmer"     = 1,
  "Part-time farmer"     = 2,
  "Hobby farmer"         = 3,
  "Other / Not a farmer" = c(0, 4)
), demo_data)

## Assemble with N footnote reflecting analytical sample
table3_final <- bind_rows(
  section_header(paste0("Demographics (N = ", n_lca_sample, ")")),
  age_rows, edu_rows, inc_rows, gen_rows, race_rows,
  section_header("Farm-Level Characteristics"),
  own_rows, farm_rows, prod_rows, prof_rows
)

write_csv(table3_final, file.path(tables_path, "Table_3.csv"))

###############################
#            TABLE 4          #
###############################

row_labels_t4 <- c(
  "age_numeric"        = "Age",
  "education_numeric"  = "Education",
  "income_numeric"     = "Household Income",
  "male"               = "Gender (Male)",
  "white"              = "Race (White)",
  "owner"              = "Ownership (Owner)",
  "farm_acres_numeric" = "Farm Size",
  "livestock"          = "Product Type (Livestock)",
  "farmer"             = "Professionalism (Full-Time)"
)

row_order_t4 <- names(row_labels_t4)

## Format odds ratios for pooled logistic regression (class membership models)
format_pooled_or <- function(pooled_obj, model_name) {
  summary(pooled_obj, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(!str_detect(term, "Intercept|intercept")) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "†",
        TRUE            ~ ""
      ),
      entry = paste0(
        sprintf("%.3f", estimate),
        " (", sprintf("%.3f", `2.5 %`),
        ", ", sprintf("%.3f", `97.5 %`), ")", sig
      ),
      Model = model_name
    ) %>%
    select(term, entry, Model)
}

table4_long <- bind_rows(
  format_pooled_or(pooled_demographics, "(1) Demographics"),
  format_pooled_or(pooled_farm,         "(2) Farm-Level Characteristics"),
  format_pooled_or(pooled_combined,     "(3) Combined")
)

table4_wide <- table4_long %>%
  mutate(term = factor(term, levels = row_order_t4)) %>%
  filter(!is.na(term)) %>%
  arrange(term) %>%
  pivot_wider(names_from = Model, values_from = entry, values_fill = "—") %>%
  select(
    term,
    "(1) Demographics",
    "(2) Farm-Level Characteristics",
    "(3) Combined"
  ) %>%
  mutate(term = row_labels_t4[as.character(term)]) %>%
  rename(Covariate = term)

## Fit statistics from first imputation models
get_fit_stats_t4 <- function(model_obj, model_name) {
  aic    <- round(AIC(model_obj), 1)
  bic    <- round(BIC(model_obj), 1)
  n_resp <- nobs(model_obj)
  m_null <- update(model_obj, . ~ 1)
  r2_mcf <- round(
    1 - as.numeric(logLik(model_obj)) / as.numeric(logLik(m_null)),
    3
  )
  tibble(
    Covariate = c("N (Respondents)", "AIC", "BIC", "McFadden R²"),
    entry     = c(
      as.character(n_resp),
      as.character(aic),
      as.character(bic),
      as.character(r2_mcf)
    ),
    Model = model_name
  )
}

fit_long_t4 <- bind_rows(
  get_fit_stats_t4(m1_demo, "(1) Demographics"),
  get_fit_stats_t4(m2_farm, "(2) Farm-Level Characteristics"),
  get_fit_stats_t4(m3_comb, "(3) Combined")
)

fit_wide_t4 <- fit_long_t4 %>%
  pivot_wider(names_from = Model, values_from = entry, values_fill = "—") %>%
  select(
    Covariate,
    "(1) Demographics",
    "(2) Farm-Level Characteristics",
    "(3) Combined"
  )

table4_final <- bind_rows(table4_wide, fit_wide_t4)

write_csv(table4_final, file.path(tables_path, "Table_4.csv"))

###############################
#            TABLE 5          #
###############################

## Format log-odds coefficients with 95% CIs for pooled clmm results
format_clmm_coef <- function(pooled_df, model_name) {
  pooled_df %>%
    filter(!str_detect(term, "\\|")) %>%
    filter(!str_detect(term, "^solar_type")) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.10  ~ "†",
        TRUE            ~ ""
      ),
      entry = case_when(
        ## Quasi-complete separation: conf.low and conf.high are NA
        is.na(conf.low) | is.na(conf.high) ~
          paste0(sprintf("%.3f", estimate), " (-, -)\u2020\u2020"),
        TRUE ~
          paste0(
            sprintf("%.3f", estimate),
            " (", sprintf("%.3f", conf.low),
            ", ", sprintf("%.3f", conf.high), ")", sig
          )
      ),
      Model = model_name
    ) %>%
    select(term, entry, Model)
}

row_order <- c(
  "evaluation_styleComprehensive",  # fix the term name
  "age_numeric",
  "education_numeric",
  "income_numeric",
  "farm_acres_numeric",
  "owner",
  "male",
  "white",
  "livestock",
  "farmer",
  "familiarity_matched",
  "new_developments"
)

row_labels <- c(
  "evaluation_styleComprehensive" = "Comprehensive Evaluator (ref: Cost-Focused)",
  "age_numeric"               = "Age",
  "education_numeric"         = "Education",
  "income_numeric"            = "Household Income",
  "farm_acres_numeric"        = "Farm Acres (log)",
  "owner"                     = "Owner-Operator",
  "male"                      = "Male",
  "white"                     = "White",
  "livestock"                 = "Livestock Producer",
  "farmer"                    = "Full-Time Farmer",
  "familiarity_matched"       = "Familiarity (matched solar type)",
  "new_developments"          = "Openness to New Developments"
)

table5_wide <- table5_long %>%
  mutate(term = factor(term, levels = row_order)) %>%
  filter(!is.na(term)) %>%
  arrange(term) %>%
  pivot_wider(names_from = Model, values_from = entry, values_fill = "—") %>%
  select(
    term,
    "(1) Class Only",
    "(2) Class + Demographics + Farm-Level",
    "(3) Class + Familiarity + Tech Adoption",
    "(4) Full Model"
  ) %>%
  mutate(term = row_labels[as.character(term)]) %>%
  rename(Covariate = term)

table5_final <- bind_rows(table5_wide, fit_wide)
write_csv(table5_final, file.path(tables_path, "Table_5.csv"))