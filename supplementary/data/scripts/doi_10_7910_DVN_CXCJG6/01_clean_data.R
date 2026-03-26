############################################
#                01_CLEAN_DATA             #
############################################

## Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(mice)
library(here)
library(ggplot2)

###############################
#          FILE PATHS         #
###############################

derived_path  <- here("data_derived")
appendix_path <- here("appendix")

###############################
#         LOADING DATA        #
###############################

survey_raw <- read.csv(here("data_raw", "survey_data_raw.csv"))

## Solar types used throughout pipeline
solar_types <- c("distributed", "utility", "shared", "agrivoltaics")

## Track sample attrition throughout cleaning
n_raw <- nrow(survey_raw)
print(n_raw)

###############################
#        DROPPING ROWS        #
###############################

## Respondents who did not reach install_ items have no outcome data and cannot contribute to any phase of analysis
survey_raw <- survey_raw %>% filter(finished == 1)
n_finished <- nrow(survey_raw)
print(n_finished)

###############################
#         MISSINGNESS         #
###############################

## Characterizing missing values
na_counts <- survey_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0)

print(na_counts)

## Missingness pattern
survey_raw_numeric <- survey_raw %>%
  select(where(~ !is.character(.)))
md.pattern(survey_raw_numeric)
dev.off()

## Missingness by analysis variable
analysis_vars <- c(
  "age", "education", "household_income", "gender", "race_white",
  "farm_acres", "ownership", "farmer_type", "main_product_livestock",
  paste0("familiarity_", solar_types),
  paste0("install_",     solar_types)
)
analysis_vars <- analysis_vars[analysis_vars %in% names(survey_raw)]

missingness <- survey_raw %>%
  select(all_of(analysis_vars)) %>%
  summarise(across(everything(), ~ round(100 * mean(is.na(.)), 1))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Pct_Missing") %>%
  arrange(desc(Pct_Missing)) %>%
  mutate(
    Category = case_when(
      str_detect(Variable, "install_")                              ~ "Adoption Intention",
      str_detect(Variable, "familiarity_")                          ~ "Familiarity",
      Variable %in% c("farm_acres", "ownership",
                      "farmer_type", "main_product_livestock")      ~ "Farm Characteristics",
      Variable %in% c("age", "education", "household_income",
                      "gender", "race_white")                       ~ "Demographic",
      TRUE                                                          ~ "Other"
    )
  )

p_miss <- missingness %>%
  filter(Pct_Missing > 0) %>%
  ggplot(aes(x = reorder(Variable, Pct_Missing), y = Pct_Missing,
             fill = Category)) +
  geom_col() +
  coord_flip() +
  labs(
    subtitle = "Proportion of missing values across survey items prior to multiple imputation.\nOnly variables with any missingness are shown.",
    x        = NULL,
    y        = "% Missing",
    fill     = "Type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "grey30",
                                    margin = margin(b = 8)),
    strip.background = element_rect(fill = "black"),
    strip.text       = element_text(color = "white", face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 9),
    axis.text        = element_text(size = 9),
    axis.title.x     = element_text(size = 9, margin = margin(t = 6)),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Adoption Intention"   = "#d95f02",
    "Familiarity"          = "#1b9e77",
    "Farm Characteristics" = "#e7298a",
    "Demographic"          = "#7570b3"
  ))

ggsave(file.path(appendix_path, "FigureA1_Missingness_Plot.png"), p_miss,
       width = 7, height = 5, dpi = 300)

###############################
#           CLEANING          #
###############################

## Drop "other" free-text fields
survey_raw <- survey_raw %>%
  select(-ends_with("_other"), -ends_with("_other_specify"))

###############################
#   RECODE: STRUCTURAL ZEROS  #
###############################
## NAs on belief items explained by skip logic (checked "none" sentinel) are genuine zeros, not missing data

for (st in solar_types) {
  benefit_none  <- paste0(st, "_benefit_none")
  cost_none     <- paste0(st, "_cost_none")
  mit_noconcern <- paste0(st, "_mitigation_noconcern")
  
  benefit_cols <- grep(paste0("^", st, "_benefit_(?!none)"),
                       names(survey_raw), value = TRUE, perl = TRUE)
  cost_cols    <- grep(paste0("^", st, "_cost_(?!none)"),
                       names(survey_raw), value = TRUE, perl = TRUE)
  mit_cols     <- grep(paste0("^", st, "_mitigation_(?!noconcern|remain)"),
                       names(survey_raw), value = TRUE, perl = TRUE)
  
  if (benefit_none %in% names(survey_raw))
    survey_raw <- survey_raw %>%
    mutate(across(all_of(benefit_cols),
                  ~ ifelse(is.na(.) & .data[[benefit_none]] == 1, 0, .)))
  
  if (cost_none %in% names(survey_raw))
    survey_raw <- survey_raw %>%
    mutate(across(all_of(cost_cols),
                  ~ ifelse(is.na(.) & .data[[cost_none]] == 1, 0, .)))
  
  if (mit_noconcern %in% names(survey_raw))
    survey_raw <- survey_raw %>%
    mutate(across(all_of(mit_cols),
                  ~ ifelse(is.na(.) & .data[[mit_noconcern]] == 1, 0, .)))
}

###############################
#  VERIFY: STRUCTURAL ZEROS   #
###############################
## Residual missingness on eval items reflects genuine partial non-response
eval_item_vars <- c(
  grep("distributed_benefit_",    names(survey_raw), value = TRUE),
  grep("distributed_cost_",       names(survey_raw), value = TRUE),
  grep("distributed_mitigation_", names(survey_raw), value = TRUE),
  grep("utility_benefit_",        names(survey_raw), value = TRUE),
  grep("utility_cost_",           names(survey_raw), value = TRUE),
  grep("utility_mitigation_",     names(survey_raw), value = TRUE),
  grep("shared_benefit_",         names(survey_raw), value = TRUE),
  grep("shared_cost_",            names(survey_raw), value = TRUE),
  grep("shared_mitigation_",      names(survey_raw), value = TRUE),
  grep("agrivoltaics_benefit_",   names(survey_raw), value = TRUE),
  grep("agrivoltaics_cost_",      names(survey_raw), value = TRUE),
  grep("agrivoltaics_mitigation", names(survey_raw), value = TRUE)
)

## Per-respondent count of still-missing belief items (post structural zeros)
belief_missingness_per_respondent <- survey_raw %>%
  select(respondent_id, all_of(eval_item_vars)) %>%
  mutate(n_missing_belief = rowSums(is.na(across(all_of(eval_item_vars)))),
         any_missing_belief = n_missing_belief > 0)

n_any_belief_missing <- sum(belief_missingness_per_respondent$any_missing_belief)
print(n_any_belief_missing)
n_complete_belief    <- sum(!belief_missingness_per_respondent$any_missing_belief)
print(n_complete_belief)

## Per-solar-type breakdown for reporting
for (st in solar_types) {
  st_vars <- grep(paste0("^", st, "_(benefit|cost|mitigation)"),
                  names(survey_raw), value = TRUE)
  n_miss_type <- survey_raw %>%
    summarise(n = sum(rowSums(is.na(across(all_of(st_vars)))) > 0)) %>%
    pull(n)
}

## Total still-missing eval item cells (for appendix footnote)
n_still_missing_cells <- survey_raw %>%
  summarise(across(all_of(eval_item_vars), ~ sum(is.na(.)))) %>%
  unlist() %>%
  sum()
print(n_still_missing_cells)

###############################
#   SAMPLE FLOW: FINAL STATE  #
###############################
n_final_clean <- nrow(survey_raw)
print(n_final_clean)

## Save cleaned raw data, authoritative pre-split dataset
saveRDS(survey_raw, file.path(derived_path, "survey_raw_clean.rds"))

###############################
#          IMPUTATION         #
###############################

## Evaluation items and install_ outcomes are not imputed

# MICE operates only on covariates, on the full finished == 1 sample
model_vars <- c(
  "age", "education", "household_income", "farm_acres",
  "ownership", "gender", "race_white", "farmer_type",
  "main_product_livestock",
  "familiarity_distributed", "familiarity_utility",
  "familiarity_shared", "familiarity_agrivoltaics",
  "new_developments", "respondent_id"
)

impute_data <- survey_raw %>%
  dplyr::select(any_of(model_vars))

# Define imputation methods per variable
method <- sapply(impute_data, function(col) {
  if (is.numeric(col)) {
    if (all(!is.na(col))) "" else "pmm"
  } else if (is.factor(col)) {
    if (all(!is.na(col))) "" else {
      n_levels <- nlevels(col)
      if (n_levels == 2) "logreg" else if (n_levels > 2) "polyreg" else ""
    }
  } else {
    ""
  }
})

method["respondent_id"] <- ""

## Predictor matrix, only use variables with |r| > 0.1 as predictors
pred_matrix <- quickpred(
  impute_data,
  mincor  = 0.1,
  exclude = "respondent_id"
)

# Zero out rows for variables with no missingness
no_impute <- names(method)[method == ""]
pred_matrix[no_impute, ] <- 0

## Run MICE
set.seed(123)
imputed_data <- mice(
  impute_data,
  m               = 20,
  maxit           = 10,
  method          = method,
  predictorMatrix = pred_matrix,
  seed            = 123,
  printFlag       = TRUE
)

###############################
#         SAVING OUTPUT       #
###############################

## Full MICE object 
saveRDS(imputed_data,
        file.path(derived_path, "mice_imputation_object.rds"))

## Sample flow summary for reporting
sample_flow <- tibble::tibble(
  Stage = c(
    "Raw survey responses",
    "After dropping finished == 0",
    "LCA-eligible (complete belief items, resolved in 02_main_analysis.R)",
    "Residual belief missingness (dropped by poLCA)"
  ),
  N = c(
    n_raw,
    n_finished,
    n_complete_belief,
    n_any_belief_missing
  )
)

print(sample_flow)
saveRDS(sample_flow, file.path(derived_path, "sample_flow.rds"))