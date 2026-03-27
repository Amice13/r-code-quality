############################################
#                 05_APPENDIX              #
############################################

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(ggplot2)
library(patchwork)
library(mice)
library(poLCA)
library(ordinal)
library(mgcv)
library(lme4)

select <- dplyr::select
filter <- dplyr::filter

appendix_path <- here("appendix")
derived_path  <- here("data_derived")

objs             <- readRDS(file.path(derived_path, "analysis_objects.rds"))
survey_raw_clean <- readRDS(file.path(derived_path, "survey_raw_clean.rds"))
coa_data         <- read_csv(here("data_raw", "coa_2022_va.csv"))

class_data_full     <- objs$class_data_full
adoption_long_1     <- objs$adoption_long_1
results_A           <- objs$results_A
results_B           <- objs$results_B
results_C           <- objs$results_C
results_D           <- objs$results_D
pooled_A            <- objs$pooled_A
pooled_B            <- objs$pooled_B
pooled_C            <- objs$pooled_C
pooled_D            <- objs$pooled_D
pooled_demographics <- objs$pooled_demographics
pooled_farm         <- objs$pooled_farm
pooled_combined     <- objs$pooled_combined
m1_demo             <- objs$m1_demo
m2_farm             <- objs$m2_farm
m3_comb             <- objs$m3_comb
prepare_adoption_long <- objs$prepare_adoption_long
pool_clmm_results     <- objs$pool_clmm_results
lca_data_clean        <- objs$lca_data_clean
final_model           <- objs$final_model
lca_models            <- objs$lca_models
fit_comparison        <- objs$fit_comparison
imputed_with_class    <- objs$imputed_with_class
class_assignments     <- objs$class_assignments
n_lca_sample          <- objs$n_lca_sample
sample_flow           <- objs$sample_flow

solar_types       <- c("distributed", "utility", "shared", "agrivoltaics")
solar_type_levels <- c("Distributed", "Utility-Scale", "Shared", "Agrivoltaics")
profile_colors    <- c("Comprehensive" = "#1b9e77", "Cost-Focused" = "#d95f02")

demo_data <- complete(imputed_with_class, 1)
n_total   <- nrow(demo_data)

lca_eligible_ids <- lca_data_clean$respondent_id
survey_raw_lca   <- survey_raw_clean %>% filter(respondent_id %in% lca_eligible_ids)
n_total_race     <- nrow(survey_raw_lca)

belief_items_app <- survey_raw_lca[, c(
  grep("distributed_benefit_",    names(survey_raw_lca), value = TRUE),
  grep("distributed_cost_",       names(survey_raw_lca), value = TRUE),
  grep("distributed_mitigation_", names(survey_raw_lca), value = TRUE),
  grep("utility_benefit_",        names(survey_raw_lca), value = TRUE),
  grep("utility_cost_",           names(survey_raw_lca), value = TRUE),
  grep("utility_mitigation_",     names(survey_raw_lca), value = TRUE),
  grep("shared_benefit_",         names(survey_raw_lca), value = TRUE),
  grep("shared_cost_",            names(survey_raw_lca), value = TRUE),
  grep("shared_mitigation_",      names(survey_raw_lca), value = TRUE),
  grep("agrivoltaics_benefit_",   names(survey_raw_lca), value = TRUE),
  grep("agrivoltaics_cost_",      names(survey_raw_lca), value = TRUE),
  grep("agrivoltaics_mitigation", names(survey_raw_lca), value = TRUE)
)]
belief_items_app$respondent_id <- survey_raw_lca$respondent_id

###############################
#            TABLE A1         #
###############################

coa <- coa_data[1, ] %>%
  mutate(across(5:ncol(coa_data[1, ]), ~ as.integer(gsub(",", "", .)))) %>%
  mutate(
    white_pct      = (white_producers    / total_producers) * 100,
    black_pct      = (black_producers    / total_producers) * 100,
    native_pct     = (native_producers   / total_producers) * 100,
    hawaiian_pct   = (hawaiian_producers / total_producers) * 100,
    asian_pct      = (asian_producers    / total_producers) * 100,
    hispanic_pct   = (hispanic_producers / total_producers) * 100,
    female_pct     = (female_producers   / total_producers) * 100,
    male_pct       = (male_producers     / total_producers) * 100,
    fulltime_pct   = (fulltime_producer  / total_producers) * 100,
    age_25_34_pct  = (age_25_34          / total_producers) * 100,
    age_35_44_pct  = (age_35_44          / total_producers) * 100,
    age_45_54_pct  = (age_45_54          / total_producers) * 100,
    age_55_64_pct  = (age_55_64          / total_producers) * 100,
    age_65plus_pct = (age_65_75 + age_ge75) / total_producers * 100
  )

population_pct <- coa %>%
  select(ends_with("_pct")) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Population_Pct") %>%
  mutate(variable = str_replace(variable, "_pct$", ""))

age_map <- c("1" = "age_25_34", "2" = "age_35_44", "3" = "age_45_54",
             "4" = "age_55_64", "5" = "age_65plus", "6" = NA_character_)

age_sample <- demo_data %>%
  count(age) %>%
  mutate(
    variable   = age_map[as.character(age)],
    Sample_Pct = round(n / n_total * 100, 1)
  ) %>%
  filter(!is.na(variable)) %>%
  select(variable, Sample_Pct)

gender_sample <- demo_data %>%
  summarise(
    male_pct   = sum(gender == 1, na.rm = TRUE) / n_total * 100,
    female_pct = sum(gender == 0, na.rm = TRUE) / n_total * 100
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Sample_Pct") %>%
  mutate(variable   = str_replace(variable, "_pct$", ""),
         Sample_Pct = round(Sample_Pct, 1))

race_sample <- survey_raw_lca %>%
  summarise(
    white_pct    = sum(race_white          == 1, na.rm = TRUE) / n_total_race * 100,
    black_pct    = sum(race_black          == 1, na.rm = TRUE) / n_total_race * 100,
    native_pct   = sum(race_nativeamerican == 1, na.rm = TRUE) / n_total_race * 100,
    hawaiian_pct = sum(race_nhpi           == 1, na.rm = TRUE) / n_total_race * 100,
    asian_pct    = sum(race_asian          == 1, na.rm = TRUE) / n_total_race * 100
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Sample_Pct") %>%
  mutate(variable   = str_replace(variable, "_pct$", ""),
         Sample_Pct = round(Sample_Pct, 1))

hispanic_sample <- survey_raw_lca %>%
  summarise(hispanic_pct = sum(hispanic == 1, na.rm = TRUE) / n_total_race * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Sample_Pct") %>%
  mutate(variable   = str_replace(variable, "_pct$", ""),
         Sample_Pct = round(Sample_Pct, 1))

fulltime_sample <- demo_data %>%
  summarise(fulltime_pct = sum(farmer_type == 1, na.rm = TRUE) / n_total * 100) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "Sample_Pct") %>%
  mutate(variable   = str_replace(variable, "_pct$", ""),
         Sample_Pct = round(Sample_Pct, 1))

sample_pct <- bind_rows(age_sample, gender_sample, race_sample,
                        hispanic_sample, fulltime_sample)

var_labels <- c(
  "age_25_34"  = "Age: 25-34",
  "age_35_44"  = "Age: 35-44",
  "age_45_54"  = "Age: 45-54",
  "age_55_64"  = "Age: 55-64",
  "age_65plus" = "Age: 65+",
  "male"       = "Gender: Male",
  "female"     = "Gender: Female",
  "white"      = "Race: White",
  "black"      = "Race: Black",
  "native"     = "Race: Native American",
  "hawaiian"   = "Race: Native Hawaiian/PI",
  "asian"      = "Race: Asian",
  "hispanic"   = "Hispanic/Latino",
  "fulltime"   = "Full-time farmer"
)

a1_table <- population_pct %>%
  left_join(sample_pct, by = "variable") %>%
  filter(!is.na(Sample_Pct)) %>%
  mutate(
    Characteristic = coalesce(var_labels[variable], variable),
    Population_Pct = round(Population_Pct, 1),
    Difference     = round(Sample_Pct - Population_Pct, 1)
  ) %>%
  select(Characteristic, Sample_Pct, Population_Pct, Difference) %>%
  rename(
    `Sample (%)`     = Sample_Pct,
    `Population (%)` = Population_Pct,
    `Difference`     = Difference
  )

write_csv(a1_table, file.path(appendix_path, "TableA1_Sample_Population_Comparison.csv"))

###############################
#          FIGURE A3          #
###############################

lca_fit_full <- map_dfr(seq_along(lca_models), function(k) {
  m <- lca_models[[k]]
  if (is.null(m)) return(tibble(K = k, Status = "Failed"))
  post      <- m$posterior
  entropy   <- round(
    1 - (-sum(post * log(post + 1e-10)) / (nrow(post) * log(max(ncol(post), 2)))),
    3
  )
  prev_bic  <- if (k > 1 && !is.null(lca_models[[k - 1]])) lca_models[[k - 1]]$bic else NA
  delta_bic <- if (!is.na(prev_bic)) round(prev_bic - m$bic, 1) else NA
  tibble(
    K           = k,
    Status      = "OK",
    Log_Lik     = round(m$llik, 2),
    N_Params    = m$npar,
    AIC         = round(m$aic, 2),
    BIC         = round(m$bic, 2),
    Delta_BIC   = delta_bic,
    Entropy     = entropy,
    Class_sizes = paste0(
      paste(paste0("Class ", seq_len(k), ": ", round(100 * m$P, 1), "%"),
            collapse = "; ")
    )
  )
})

opt_k <- fit_comparison$K[which.min(fit_comparison$BIC[!is.na(fit_comparison$BIC)])]

p_lca_fit <- fit_comparison %>%
  filter(!is.na(AIC)) %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Criterion", values_to = "Value") %>%
  ggplot(aes(x = K, y = Value, color = Criterion, group = Criterion)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_vline(xintercept = opt_k, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("AIC" = "#1b9e77", "BIC" = "#d95f02")) +
  scale_x_continuous(breaks = 1:6) +
  labs(x = "Number of Classes (K)", y = "Information Criterion", color = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

p_lca_entropy <- fit_comparison %>%
  filter(K > 1, !is.na(Entropy)) %>%
  ggplot(aes(x = K, y = Entropy)) +
  geom_line(linewidth = 0.8, color = "#7570b3") +
  geom_point(size = 3, color = "#7570b3") +
  geom_vline(xintercept = opt_k, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 2:6) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Number of Classes (K)", y = "Entropy") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

p_lca_deltabic <- lca_fit_full %>%
  filter(!is.na(Delta_BIC)) %>%
  ggplot(aes(x = K, y = Delta_BIC)) +
  geom_col(fill = "#d95f02", color = "black", linewidth = 0.3, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 2:6) +
  labs(x = "Number of Classes (K)", y = "\u0394 BIC (K-1 vs K)") +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

p_lca_selection <- (p_lca_fit + p_lca_entropy) / p_lca_deltabic +
  plot_layout(heights = c(1, 0.6))

ggsave(file.path(appendix_path, "FigureA3_LCA_Model_Selection.png"),
       p_lca_selection, width = 12, height = 8, dpi = 300)

###############################
#           TABLE A3          #
###############################

write_csv(lca_fit_full, file.path(appendix_path, "TableA3_LCA_Fit_Statistics.csv"))

###############################
#           TABLE A4          #
###############################

class_data_with_items <- belief_items_app %>%
  inner_join(
    class_assignments %>% select(respondent_id, deliberation_class),
    by = "respondent_id"
  ) %>%
  mutate(
    label = factor(
      case_when(deliberation_class == 2 ~ "Comprehensive",
                deliberation_class == 1 ~ "Cost-Focused",
                TRUE ~ NA_character_),
      levels = c("Cost-Focused", "Comprehensive")
    )
  )

chisq_by_class <- function(item_col, label_col) {
  tbl <- table(label_col, item_col)
  if (nrow(tbl) < 2 || ncol(tbl) < 2) return(list(stat = NA, p = NA, n = length(item_col)))
  ct  <- tryCatch(chisq.test(tbl, correct = FALSE), error = function(e) NULL)
  if (is.null(ct)) return(list(stat = NA, p = NA, n = sum(!is.na(item_col))))
  list(stat = round(ct$statistic, 2), p = round(ct$p.value, 4),
       n = sum(!is.na(item_col)))
}

content_groups <- list(
  list(solar = "Distributed", type = "Benefits",              category = "Financial/Economic",
       pattern = "^distributed_benefit_(bills|revenue|cost)$"),
  list(solar = "Distributed", type = "Benefits",              category = "Energy Independence",
       pattern = "^distributed_benefit_(sufficiency|reliability)$"),
  list(solar = "Distributed", type = "Benefits",              category = "Environmental/Climate",
       pattern = "^distributed_benefit_footprint$"),
  list(solar = "Distributed", type = "Benefits",              category = "Property Value",
       pattern = "^distributed_benefit_property$"),
  list(solar = "Distributed", type = "Costs",                 category = "Financial/Economic",
       pattern = "^distributed_cost_cost$"),
  list(solar = "Distributed", type = "Costs",                 category = "Technical/Operational",
       pattern = "^distributed_cost_(space|reliability|qualified)$"),
  list(solar = "Distributed", type = "Costs",                 category = "Regulatory/Legal",
       pattern = "^distributed_cost_regulatory$"),
  list(solar = "Distributed", type = "Costs",                 category = "Agricultural Compatibility",
       pattern = "^distributed_cost_transition$"),
  list(solar = "Distributed", type = "Costs",                 category = "Aesthetics",
       pattern = "^distributed_cost_aesthetics$"),
  list(solar = "Distributed", type = "Costs",                 category = "Environmental/Wildlife",
       pattern = "^distributed_cost_wildlife$"),
  list(solar = "Distributed", type = "Costs",                 category = "Property/Land Use",
       pattern = "^distributed_cost_property$"),
  list(solar = "Distributed", type = "Costs",                 category = "Personal Renewable Opposition",
       pattern = "^distributed_cost_personal$"),
  list(solar = "Distributed", type = "Costs",                 category = "Information/Understanding",
       pattern = "^distributed_cost_incentives$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Financial/Economic",
       pattern = "^distributed_mitigation_(incentives|cost)$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Information/Education",
       pattern = "^distributed_mitigation_information$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Regulatory Clarity",
       pattern = "^distributed_mitigation_regulatory$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Technical/Performance",
       pattern = "^distributed_mitigation_(performance|storage)$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Property Value Protection",
       pattern = "^distributed_mitigation_property$"),
  list(solar = "Distributed", type = "Mitigation Strategies", category = "Agricultural Compatibility",
       pattern = "^distributed_mitigation_minimal$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Financial/Economic",
       pattern = "^utility_benefit_revenue$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Energy Independence",
       pattern = "^utility_benefit_reliability$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Environmental/Climate",
       pattern = "^utility_benefit_emissions$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Property Value",
       pattern = "^utility_benefit_property$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Community Development",
       pattern = "^utility_benefit_development$"),
  list(solar = "Utility-Scale", type = "Benefits",              category = "Efficient Land Use",
       pattern = "^utility_benefit_use$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Financial/Economic",
       pattern = "^utility_cost_negligible$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Technical/Operational",
       pattern = "^utility_cost_complex$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Regulatory/Legal",
       pattern = "^utility_cost_(regulatory|breach)$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Agricultural Compatibility",
       pattern = "^utility_cost_(transition|agricultural)$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Environmental/Wildlife",
       pattern = "^utility_cost_(wildlife|erosion)$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Property/Land Use",
       pattern = "^utility_cost_property$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Personal Renewable Opposition",
       pattern = "^utility_cost_personal$"),
  list(solar = "Utility-Scale", type = "Costs",                 category = "Community Opposition",
       pattern = "^utility_cost_community$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Financial/Economic",
       pattern = "^utility_mitigation_(incentives|payment)$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Information/Education",
       pattern = "^utility_mitigation_information$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Technical/Performance",
       pattern = "^utility_mitigation_minimal$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Property Value Protection",
       pattern = "^utility_mitigation_property$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Environmental Assurances",
       pattern = "^utility_mitigation_environmental$"),
  list(solar = "Utility-Scale", type = "Mitigation Strategies", category = "Community Engagement",
       pattern = "^utility_mitigation_community$"),
  list(solar = "Shared", type = "Benefits",              category = "Financial/Economic",
       pattern = "^shared_benefit_(bills|upfront)$"),
  list(solar = "Shared", type = "Benefits",              category = "Environmental/Climate",
       pattern = "^shared_benefit_footprint$"),
  list(solar = "Shared", type = "Benefits",              category = "Renewable Access",
       pattern = "^shared_benefit_access$"),
  list(solar = "Shared", type = "Costs",                 category = "Financial/Economic",
       pattern = "^shared_cost_(uncertainty|rates)$"),
  list(solar = "Shared", type = "Costs",                 category = "Technical/Operational",
       pattern = "^shared_cost_(reliability|commitment)$"),
  list(solar = "Shared", type = "Costs",                 category = "Information/Understanding",
       pattern = "^shared_cost_(information|difficulty)$"),
  list(solar = "Shared", type = "Costs",                 category = "Prefer Other Renewables",
       pattern = "^shared_cost_otherforms$"),
  list(solar = "Shared", type = "Mitigation Strategies", category = "Financial/Economic",
       pattern = "^shared_mitigation_(cost|incentives)$"),
  list(solar = "Shared", type = "Mitigation Strategies", category = "Information/Education",
       pattern = "^shared_mitigation_testimonial$"),
  list(solar = "Shared", type = "Mitigation Strategies", category = "Technical/Performance",
       pattern = "^shared_mitigation_(assurance|support)$"),
  list(solar = "Shared", type = "Mitigation Strategies", category = "Contractual Flexibility",
       pattern = "^shared_mitigation_flexible$"),
  list(solar = "Agrivoltaics", type = "Benefits",              category = "Financial/Economic",
       pattern = "^agrivoltaics_benefit_(cost|revenue)$"),
  list(solar = "Agrivoltaics", type = "Benefits",              category = "Energy Independence",
       pattern = "^agrivoltaics_benefit_sufficiency$"),
  list(solar = "Agrivoltaics", type = "Benefits",              category = "Environmental/Climate",
       pattern = "^agrivoltaics_benefit_footprint$"),
  list(solar = "Agrivoltaics", type = "Benefits",              category = "Property Value",
       pattern = "^agrivoltaics_benefit_property$"),
  list(solar = "Agrivoltaics", type = "Benefits",              category = "Agricultural Benefits",
       pattern = "^agrivoltaics_benefit_(yield|water|agricultural)$"),
  list(solar = "Agrivoltaics", type = "Costs",                 category = "Financial/Economic",
       pattern = "^agrivoltaics_cost_(cost|construction|insurance)$"),
  list(solar = "Agrivoltaics", type = "Costs",                 category = "Regulatory/Legal",
       pattern = "^agrivoltaics_cost_knowledge$"),
  list(solar = "Agrivoltaics", type = "Costs",                 category = "Agricultural Compatibility",
       pattern = "^agrivoltaics_cost_(shift|yield|transition)$"),
  list(solar = "Agrivoltaics", type = "Costs",                 category = "Property/Land Use",
       pattern = "^agrivoltaics_cost_property$"),
  list(solar = "Agrivoltaics", type = "Costs",                 category = "Information/Understanding",
       pattern = "^agrivoltaics_cost_information$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Financial/Economic",
       pattern = "^agrivoltaics_mitigation_(benefits|incentives)$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Information/Education",
       pattern = "^agrivoltaics_mitigation_examples$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Regulatory Clarity",
       pattern = "^agrivoltaics_mitigation_regulatory$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Technical/Performance",
       pattern = "^agrivoltaics_mitigation_(support|reliable)$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Property Value Protection",
       pattern = "^agrivoltaics_mitigation_property$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Agricultural Compatibility",
       pattern = "^agrivoltaics_mitigation_minimal$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Environmental Assurances",
       pattern = "^agrivoltaics_mitigation_environmental$"),
  list(solar = "Agrivoltaics", type = "Mitigation Strategies", category = "Contractual Flexibility",
       pattern = "^agrivoltaics_mitigation_insurance$")
)

chisq_results <- map_dfr(content_groups, function(grp) {
  matched_cols <- grep(grp$pattern, names(class_data_with_items), value = TRUE)
  if (length(matched_cols) == 0) {
    return(tibble(Solar_Type = grp$solar, Category_Type = grp$type,
                  Category = grp$category,
                  N_Items = 0, Chi_sq = NA, p = NA,
                  Mean_Comprehensive = NA, Mean_Cost_Focused = NA, N = NA))
  }
  item_endorsed <- rowSums(class_data_with_items[, matched_cols, drop = FALSE],
                           na.rm = TRUE) > 0
  label_vec     <- class_data_with_items$label
  valid         <- !is.na(label_vec) & !is.na(item_endorsed)
  mean_comp     <- round(mean(item_endorsed[valid & label_vec == "Comprehensive"], na.rm = TRUE), 3)
  mean_cost     <- round(mean(item_endorsed[valid & label_vec == "Cost-Focused"],  na.rm = TRUE), 3)
  ct            <- chisq_by_class(as.integer(item_endorsed[valid]), label_vec[valid])
  tibble(
    Solar_Type         = grp$solar,
    Category_Type      = grp$type,
    Category           = grp$category,
    N_Items            = length(matched_cols),
    Mean_Comprehensive = mean_comp,
    Mean_Cost_Focused  = mean_cost,
    Difference         = round(mean_comp - mean_cost, 3),
    Chi_sq             = ct$stat,
    df                 = 1,
    p                  = ct$p,
    sig = case_when(
      is.na(ct$p)  ~ "",
      ct$p < 0.001 ~ "***",
      ct$p < 0.01  ~ "**",
      ct$p < 0.05  ~ "*",
      ct$p < 0.10  ~ "\u2020",
      TRUE         ~ ""
    ),
    N = ct$n
  )
})

chisq_results <- chisq_results %>%
  group_by(Solar_Type, Category_Type) %>%
  mutate(
    p_adj_bh = p.adjust(p, method = "BH"),
    sig_bh = case_when(
      is.na(p_adj_bh)  ~ "",
      p_adj_bh < 0.001 ~ "***",
      p_adj_bh < 0.01  ~ "**",
      p_adj_bh < 0.05  ~ "*",
      p_adj_bh < 0.10  ~ "\u2020",
      TRUE             ~ ""
    )
  ) %>%
  ungroup()

write_csv(chisq_results,
          file.path(appendix_path, "TableA4_Solar_Evaluation_Factor_Endorsements.csv"))

###############################
#           TABLE A5          #
###############################

compute_bvr <- function(lca_fit) {
  post    <- lca_fit$posterior
  y_mat   <- as.matrix(lca_fit$y)
  p_items <- ncol(y_mat)
  probs   <- lca_fit$probs
  nclass  <- ncol(post)
  bvr_mat <- matrix(NA, p_items, p_items,
                    dimnames = list(colnames(y_mat), colnames(y_mat)))
  for (i in seq_len(p_items)) {
    for (j in seq_len(p_items)) {
      if (i >= j) next
      exp_11 <- sum(sapply(seq_len(nclass), function(k)
        post[, k] * probs[[i]][k, 2] * probs[[j]][k, 2]))
      exp_10 <- sum(sapply(seq_len(nclass), function(k)
        post[, k] * probs[[i]][k, 2] * probs[[j]][k, 1]))
      exp_01 <- sum(sapply(seq_len(nclass), function(k)
        post[, k] * probs[[i]][k, 1] * probs[[j]][k, 2]))
      exp_00 <- sum(sapply(seq_len(nclass), function(k)
        post[, k] * probs[[i]][k, 1] * probs[[j]][k, 1]))
      if (any(c(exp_11, exp_10, exp_01, exp_00) < 5)) {
        bvr_mat[i, j] <- NA
        next
      }
      obs_11 <- sum(y_mat[, i] == 1 & y_mat[, j] == 1, na.rm = TRUE)
      obs_10 <- sum(y_mat[, i] == 1 & y_mat[, j] == 0, na.rm = TRUE)
      obs_01 <- sum(y_mat[, i] == 0 & y_mat[, j] == 1, na.rm = TRUE)
      obs_00 <- sum(y_mat[, i] == 0 & y_mat[, j] == 0, na.rm = TRUE)
      bvr_mat[i, j] <- (obs_11 - exp_11)^2 / exp_11 +
        (obs_10 - exp_10)^2 / exp_10 +
        (obs_01 - exp_01)^2 / exp_01 +
        (obs_00 - exp_00)^2 / exp_00
    }
  }
  bvr_mat
}

bvr <- tryCatch(compute_bvr(final_model), error = function(e) NULL)

if (!is.null(bvr)) {
  indicator_names <- colnames(final_model$y)
  bvr_df <- as.data.frame(bvr) %>%
    mutate(Item_i = indicator_names) %>%
    pivot_longer(-Item_i, names_to = "Item_j", values_to = "BVR") %>%
    filter(!is.na(BVR)) %>%
    arrange(desc(BVR))
  
  bvr_flagged <- bvr_df %>%
    filter(BVR > 3.84) %>%
    mutate(
      domain_i    = str_extract(Item_i, "benefit|cost|mitigation"),
      domain_j    = str_extract(Item_j, "benefit|cost|mitigation"),
      solar_i     = str_extract(Item_i, "^[^_]+"),
      solar_j     = str_extract(Item_j, "^[^_]+"),
      same_domain = domain_i == domain_j,
      same_solar  = solar_i  == solar_j,
      pair_type   = factor(
        case_when(
          same_solar &  same_domain ~ "Same solar, same domain",
          same_solar & !same_domain ~ "Same solar, diff domain",
          !same_solar &  same_domain ~ "Diff solar, same domain",
          TRUE                      ~ "Diff solar, diff domain"
        ),
        levels = c("Same solar, same domain", "Same solar, diff domain",
                   "Diff solar, same domain", "Diff solar, diff domain")
      )
    )
  
  bvr_summary <- bvr_flagged %>%
    group_by(pair_type) %>%
    summarise(
      N        = n(),
      Pct      = paste0(round(100 * n() / nrow(bvr_flagged), 1), "%"),
      Mean_BVR = round(mean(BVR), 1),
      Max_BVR  = round(max(BVR), 1),
      .groups  = "drop"
    ) %>%
    arrange(desc(N)) %>%
    rename(`Pair Type` = pair_type, `Mean BVR` = Mean_BVR, `Max BVR` = Max_BVR)
  
  write_csv(bvr_summary,
            file.path(appendix_path, "TableA5_BVRs_by_Item_Pair_Type.csv"))
}

###############################
#           TABLE A6          #
###############################

main_assignments_df <- data.frame(
  respondent_id = lca_data_clean$respondent_id,
  main_class    = final_model$predclass
)

lca_by_type <- map_dfr(solar_types, function(st) {
  vars  <- c(
    grep(paste0(st, "_benefit_"),   names(lca_data_clean), value = TRUE),
    grep(paste0(st, "_cost_"),      names(lca_data_clean), value = TRUE),
    grep(paste0(st, "_mitigation"), names(lca_data_clean), value = TRUE)
  )
  vars  <- vars[vars %in% names(lca_data_clean)]
  y_sub <- lca_data_clean[, vars, drop = FALSE]
  y_sub <- y_sub[, apply(y_sub, 2, function(x) var(x, na.rm = TRUE) > 0), drop = FALSE]
  if (ncol(y_sub) < 2) return(tibble())
  f <- as.formula(paste("cbind(", paste(names(y_sub), collapse = ","), ") ~ 1"))
  lca_fit <- tryCatch(
    poLCA(f, data = lca_data_clean, nclass = 2, maxiter = 1000,
          nrep = 10, verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(lca_fit)) return(tibble())
  solar_class   <- lca_fit$predclass
  main_class    <- main_assignments_df$main_class
  agree_direct  <- mean(solar_class == main_class)
  agree_flipped <- mean(solar_class != main_class)
  if (agree_flipped > agree_direct) solar_class <- ifelse(solar_class == 1, 2, 1)
  agreement    <- mean(solar_class == main_class)
  n_discordant <- sum(solar_class != main_class)
  tibble(
    Solar_type    = solar_type_levels[solar_types == st],
    AIC           = round(lca_fit$aic, 1),
    BIC           = round(lca_fit$bic, 1),
    Entropy       = round(1 - (-sum(lca_fit$posterior *
                                      log(lca_fit$posterior + 1e-10)) /
                                 (nrow(lca_fit$posterior) * log(2))), 3),
    Class1_pct    = paste0(round(100 * lca_fit$P[1], 1), "%"),
    Class2_pct    = paste0(round(100 * lca_fit$P[2], 1), "%"),
    Pct_Agreement = paste0(round(100 * agreement, 1), "%"),
    N_Discordant  = n_discordant
  )
})

write_csv(lca_by_type,
          file.path(appendix_path, "TableA6_LCA_Classification_Stability.csv"))

###############################
#           TABLE A7          #
###############################

run_overselection_spec <- function(spec_name, truncate_mail = FALSE,
                                   truncate_web = FALSE, exclude_mail = FALSE,
                                   max_items = 3) {
  dat <- complete(imputed_with_class, 1) %>%
    left_join(belief_items_app, by = "respondent_id") %>%
    left_join(survey_raw_lca %>% select(respondent_id, source), by = "respondent_id")
  
  solar_item_groups <- lapply(solar_types, function(st) {
    list(
      benefit    = grep(paste0("^", st, "_benefit_"),    names(dat), value = TRUE),
      cost       = grep(paste0("^", st, "_cost_"),       names(dat), value = TRUE),
      mitigation = grep(paste0("^", st, "_mitigation_"), names(dat), value = TRUE)
    )
  })
  names(solar_item_groups) <- solar_types
  dat_mod <- dat
  
  if (exclude_mail) {
    dat_mod <- dat_mod %>% filter(source != 1)
  } else {
    for (st in solar_types) {
      all_items <- unlist(solar_item_groups[[st]])
      all_items <- all_items[all_items %in% names(dat_mod)]
      if (truncate_mail) {
        for (row in which(dat_mod$source == 1)) {
          endorsed <- which(dat_mod[row, all_items] == 1)
          if (length(endorsed) > max_items)
            dat_mod[row, all_items[endorsed[(max_items + 1):length(endorsed)]]] <- 0
        }
      }
      if (truncate_web) {
        for (row in which(dat_mod$source == 0)) {
          endorsed <- which(dat_mod[row, all_items] == 1)
          if (length(endorsed) > max_items)
            dat_mod[row, all_items[endorsed[(max_items + 1):length(endorsed)]]] <- 0
        }
      }
    }
  }
  
  na_row <- tibble(
    Specification = spec_name, N = NA_integer_,
    AIC = NA_real_, BIC = NA_real_, Entropy = NA_real_,
    Class1_pct = NA_character_, Class2_pct = NA_character_,
    Pct_Agreement = NA_character_, Note = NA_character_
  )
  
  lca_vars <- grep("_benefit_|_cost_|_mitigation_", names(dat_mod), value = TRUE)
  lca_vars <- lca_vars[lca_vars %in% names(dat_mod)]
  lca_sub  <- dat_mod %>%
    dplyr::select(respondent_id, all_of(lca_vars)) %>%
    mutate(across(-respondent_id, ~ . + 1))
  
  variance_check <- lca_sub %>%
    dplyr::select(-respondent_id) %>%
    summarise(across(everything(), ~ var(., na.rm = TRUE)))
  
  endorse_counts <- lca_sub %>%
    dplyr::select(-respondent_id) %>%
    summarise(across(everything(), ~ sum(. == 2, na.rm = TRUE)))
  
  keep <- names(variance_check)[
    !is.na(variance_check) &
      variance_check > 0 &
      endorse_counts >= 5 &
      endorse_counts <= (nrow(lca_sub) - 5)
  ]
  lca_sub <- lca_sub %>%
    dplyr::select(respondent_id, all_of(keep)) %>%
    filter(complete.cases(.))
  
  if (length(keep) < 5 || nrow(lca_sub) < 20) {
    return(na_row %>% mutate(Note = paste0("Skipped: ", length(keep),
                                           " items, ", nrow(lca_sub), " obs")))
  }
  
  belief_vars <- setdiff(names(lca_sub), "respondent_id")
  f <- as.formula(paste("cbind(", paste(belief_vars, collapse = ","), ") ~ 1"))
  lca_err <- NULL
  set.seed(12345)
  lca_fit <- tryCatch(
    poLCA(f, data = lca_sub %>% dplyr::select(-respondent_id),
          nclass = 2, maxiter = 5000, nrep = 10, verbose = FALSE),
    error   = function(e) { lca_err <<- e$message; NULL },
    warning = function(w) {
      suppressWarnings(
        poLCA(f, data = lca_sub %>% dplyr::select(-respondent_id),
              nclass = 2, maxiter = 5000, nrep = 10, verbose = FALSE)
      )
    }
  )
  
  if (is.null(lca_fit))
    return(na_row %>% mutate(Note = paste0("poLCA failed: ", lca_err)))
  
  common_ids    <- intersect(lca_sub$respondent_id, lca_data_clean$respondent_id)
  main_cls      <- final_model$predclass[match(common_ids, lca_data_clean$respondent_id)]
  spec_cls      <- lca_fit$predclass[match(common_ids, lca_sub$respondent_id)]
  agree_direct  <- mean(spec_cls == main_cls, na.rm = TRUE)
  agree_flipped <- mean(spec_cls != main_cls, na.rm = TRUE)
  if (agree_flipped > agree_direct) spec_cls <- ifelse(spec_cls == 1, 2, 1)
  agreement <- mean(spec_cls == main_cls, na.rm = TRUE)
  
  tibble(
    Specification = spec_name,
    N             = nrow(lca_sub),
    AIC           = round(lca_fit$aic, 1),
    BIC           = round(lca_fit$bic, 1),
    Entropy       = round(1 - (-sum(lca_fit$posterior *
                                      log(lca_fit$posterior + 1e-10)) /
                                 (nrow(lca_fit$posterior) * log(2))), 3),
    Class1_pct    = paste0(round(100 * lca_fit$P[1], 1), "%"),
    Class2_pct    = paste0(round(100 * lca_fit$P[2], 1), "%"),
    Pct_Agreement = paste0(round(100 * agreement, 1), "%"),
    Note          = paste0(length(keep), " items retained")
  )
}

overselection_specs <- bind_rows(
  run_overselection_spec("Primary (no truncation)"),
  run_overselection_spec("Truncate mail (<=3 per type)",  truncate_mail = TRUE),
  run_overselection_spec("Truncate web (<=3 per type)",   truncate_web  = TRUE),
  run_overselection_spec("Truncate all (<=3 per type)",   truncate_mail = TRUE, truncate_web = TRUE)
)

write_csv(overselection_specs,
          file.path(appendix_path, "TableA7_LCA_Overselection_Specifications.csv"))

###############################
#           FIGURE A4         #
###############################

dat_imp1 <- complete(imputed_with_class, 1) %>%
  mutate(
    active_class       = as.integer(lca_class == 2),
    age_numeric        = as.numeric(age),
    education_numeric  = as.numeric(education),
    income_numeric     = as.numeric(household_income),
    farm_acres_numeric = as.numeric(farm_acres)
  )

gam_class <- tryCatch(
  mgcv::gam(
    active_class ~ s(age_numeric, k = 5) + s(education_numeric, k = 5) +
      s(income_numeric, k = 5) + s(farm_acres_numeric, k = 5),
    data = dat_imp1, family = binomial(), method = "REML"
  ),
  error = function(e) NULL
)

if (!is.null(gam_class)) {
  
  smooth_specs <- list(
    list(var = "age_numeric",        label = "Age"),
    list(var = "education_numeric",  label = "Education"),
    list(var = "income_numeric",     label = "Household Income"),
    list(var = "farm_acres_numeric", label = "Farm Size (acres)")
  )
  
  gam_class_plots <- map(smooth_specs, function(spec) {
    nd <- data.frame(
      age_numeric        = median(dat_imp1$age_numeric,        na.rm = TRUE),
      education_numeric  = median(dat_imp1$education_numeric,  na.rm = TRUE),
      income_numeric     = median(dat_imp1$income_numeric,     na.rm = TRUE),
      farm_acres_numeric = median(dat_imp1$farm_acres_numeric, na.rm = TRUE)
    )
    nd <- nd[rep(1, 200), ]
    nd[[spec$var]] <- seq(min(dat_imp1[[spec$var]], na.rm = TRUE),
                          max(dat_imp1[[spec$var]], na.rm = TRUE),
                          length.out = 200)
    rownames(nd) <- NULL
    pred   <- predict(gam_class, newdata = nd, type = "response", se.fit = TRUE)
    nd$fit <- pred$fit
    nd$lwr <- pmax(pred$fit - 1.96 * pred$se.fit, 0)
    nd$upr <- pmin(pred$fit + 1.96 * pred$se.fit, 1)
    ggplot(nd, aes_string(x = spec$var, y = "fit")) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "#1b9e77") +
      geom_line(color = "#1b9e77", linewidth = 0.8) +
      geom_rug(data = dat_imp1, aes_string(x = spec$var, y = NULL),
               alpha = 0.3, sides = "b") +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = spec$label, y = "Pr(Comprehensive Evaluator)") +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })
  
  p_gam_class <- wrap_plots(gam_class_plots, ncol = 2)
  
  ggsave(file.path(appendix_path, "FigureA4_Class_Model_GAM.png"),
         p_gam_class, width = 10, height = 8, dpi = 300)
}

###############################
#           FIGURE A5         #
###############################

po_fixed_predictors <- c(
  "evaluation_style",
  "age_numeric", "education_numeric", "income_numeric",
  "farm_acres_numeric", "owner", "male", "white",
  "livestock", "farmer", "familiarity_matched", "new_developments"
)

po_dat <- adoption_long_1 %>%
  mutate(
    adoption_int      = as.integer(adoption),
    evaluation_style  = as.integer(evaluation_style == "Comprehensive"),
    solar_type_f      = factor(solar_type)
  ) %>%
  filter(!is.na(adoption_int))

K     <- length(unique(po_dat$adoption_int))
n_cut <- K - 1

glmer_formula_base <- function(k_threshold) {
  as.formula(paste(
    "I(adoption_int >", k_threshold, ") ~",
    "evaluation_style +",
    "age_numeric + education_numeric + income_numeric +",
    "farm_acres_numeric + owner + male + white +",
    "livestock + farmer + familiarity_matched + new_developments +",
    "solar_type_f + (1 | respondent_id)"
  ))
}

cut_fits <- vector("list", n_cut)
for (k in seq_len(n_cut)) {
  cut_fits[[k]] <- tryCatch(
    lme4::glmer(
      glmer_formula_base(k), data = po_dat, family = binomial(),
      control = lme4::glmerControl(optimizer = "bobyqa",
                                   optCtrl   = list(maxfun = 2e5))
    ),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(tryCatch(
      lme4::glmer(
        glmer_formula_base(k), data = po_dat, family = binomial(),
        control = lme4::glmerControl(optimizer = "bobyqa",
                                     optCtrl   = list(maxfun = 2e5))
      ),
      error = function(e2) NULL
    ))
  )
}

extract_cut_coefs <- function(fit, k) {
  if (is.null(fit)) return(NULL)
  cf   <- summary(fit)$coefficients
  keep <- rownames(cf)[!grepl("^\\(Intercept\\)|^solar_type_f", rownames(cf))]
  if (length(keep) == 0) return(NULL)
  data.frame(
    cut       = k,
    predictor = keep,
    estimate  = cf[keep, "Estimate"],
    se        = cf[keep, "Std. Error"],
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

coef_long <- map_dfr(seq_len(n_cut), ~ extract_cut_coefs(cut_fits[[.x]], .x))

if (nrow(coef_long) > 0) {
  p_po <- coef_long %>%
    filter(predictor %in% c("evaluation_style", "familiarity_matched",
                            "age_numeric", "education_numeric")) %>%
    mutate(
      lwr       = estimate - 1.96 * se,
      upr       = estimate + 1.96 * se,
      predictor = recode(predictor,
                         "evaluation_style"   = "Evaluation Style (Comprehensive)",
                         "familiarity_matched" = "Familiarity",
                         "age_numeric"         = "Age",
                         "education_numeric"   = "Education"),
      cut = factor(cut, labels = paste0("y > ", seq_len(n_cut)))
    ) %>%
    ggplot(aes(x = cut, y = estimate, group = predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 0.7, color = "grey40") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15,
                  linewidth = 0.5, color = "grey40") +
    geom_point(size = 2.5, color = "black") +
    facet_wrap(~ predictor, scales = "free_y", ncol = 2) +
    labs(x = "Cumulative Cut Point", y = "Log-Odds Coefficient") +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          strip.background   = element_rect(fill = "grey92"),
          strip.text         = element_text(face = "bold", size = 9))
  
  ggsave(file.path(appendix_path, "FigureA5_Coefficient_Stability.png"),
         p_po, width = 10, height = 7, dpi = 300)
}

###############################
#           TABLE A9          #
###############################

fit_glmer_robust <- function(formula, data) {
  fit <- tryCatch(
    lme4::glmer(formula, data = data, family = binomial(),
                control = lme4::glmerControl(optimizer = "bobyqa",
                                             optCtrl   = list(maxfun = 2e5))),
    error   = function(e) NULL,
    warning = function(w) suppressWarnings(tryCatch(
      lme4::glmer(formula, data = data, family = binomial(),
                  control = lme4::glmerControl(optimizer = "bobyqa",
                                               optCtrl   = list(maxfun = 2e5))),
      error = function(e) NULL
    ))
  )
  if (is.null(fit) || lme4::isSingular(fit, tol = 1e-4)) {
    fit2 <- tryCatch(
      suppressWarnings(
        lme4::glmer(formula, data = data, family = binomial(),
                    control = lme4::glmerControl(optimizer  = "Nelder_Mead",
                                                 optCtrl   = list(maxfun = 2e5)))
      ),
      error = function(e) NULL
    )
    if (!is.null(fit2) && !lme4::isSingular(fit2, tol = 1e-4)) fit <- fit2
  }
  fit
}

pool_glmer_fixed <- function(results_list, model_name) {
  coef_list <- lapply(results_list, function(x) {
    if (is.null(x$model)) return(NULL)
    tryCatch({
      cf <- summary(x$model)$coefficients
      if (is.null(cf) || nrow(cf) == 0) return(NULL)
      data.frame(term = rownames(cf), estimate = cf[, "Estimate"],
                 std.error = cf[, "Std. Error"], stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  coef_list <- Filter(Negate(is.null), coef_list)
  if (length(coef_list) == 0) return(tibble())
  
  all_terms         <- unique(unlist(lapply(coef_list, `[[`, "term")))
  substantive_terms <- all_terms[!grepl("^\\(Intercept\\)", all_terms)]
  
  pooled <- lapply(substantive_terms, function(trm) {
    ests  <- sapply(coef_list, function(x) { idx <- which(x$term == trm); if (length(idx) > 0) x$estimate[idx[1]] else NA_real_ })
    ses   <- sapply(coef_list, function(x) { idx <- which(x$term == trm); if (length(idx) > 0) x$std.error[idx[1]] else NA_real_ })
    valid <- !is.na(ests) & !is.na(ses) & ses > 0
    if (sum(valid) == 0) return(NULL)
    ests_v <- ests[valid]; ses_v <- ses[valid]; m_ <- length(ests_v)
    qbar  <- mean(ests_v); ubar <- mean(ses_v^2)
    b     <- if (m_ > 1) var(ests_v) else 0
    t_var <- ubar + (1 + 1 / m_) * b
    se_p  <- sqrt(t_var)
    pval  <- 2 * pnorm(-abs(qbar / se_p))
    data.frame(term = trm, estimate = round(qbar, 4), std.error = round(se_p, 4),
               conf.low = round(qbar - 1.96 * se_p, 4),
               conf.high = round(qbar + 1.96 * se_p, 4),
               p.value = round(pval, 4), n_imps = m_, Model = model_name,
               stringsAsFactors = FALSE)
  })
  
  result <- do.call(rbind, Filter(Negate(is.null), pooled))
  if (is.null(result)) tibble() else as_tibble(result)
}

binary_model_specs <- list(
  A = list(
    name    = "Model A: Class Only",
    formula = adopt_binary ~ evaluation_style + solar_type + (1 | respondent_id)
  ),
  B = list(
    name    = "Model B: Class + Demographics + Farm-Level",
    formula = adopt_binary ~ evaluation_style +
      age_numeric + education_numeric + income_numeric +
      farm_acres_numeric + owner + male + white + livestock + farmer +
      solar_type + (1 | respondent_id)
  ),
  C = list(
    name    = "Model C: Class + Familiarity + Tech Adoption",
    formula = adopt_binary ~ evaluation_style +
      familiarity_matched + new_developments +
      solar_type + (1 | respondent_id)
  ),
  D = list(
    name    = "Model D: Full Model",
    formula = adopt_binary ~ evaluation_style +
      age_numeric + education_numeric + income_numeric +
      farm_acres_numeric + owner + male + white + livestock + farmer +
      familiarity_matched + new_developments +
      solar_type + (1 | respondent_id)
  )
)

results_binary_all <- lapply(binary_model_specs, function(spec) {
  res <- vector("list", imputed_with_class$m)
  for (i in seq_len(imputed_with_class$m)) {
    imp_data      <- complete(imputed_with_class, i)
    adoption_long <- prepare_adoption_long(imp_data) %>%
      mutate(adopt_binary = as.integer(as.numeric(adoption) >= 3))
    res[[i]] <- list(model = fit_glmer_robust(spec$formula, adoption_long),
                     data  = adoption_long)
  }
  pool_glmer_fixed(res, spec$name)
})

pooled_binary_all <- bind_rows(results_binary_all)

if (nrow(pooled_binary_all) > 0 && "term" %in% names(pooled_binary_all)) {
  pooled_binary_clean <- pooled_binary_all %>%
    filter(!str_detect(term, "^solar_type"),
           !str_detect(term, "^\\(Intercept\\)")) %>%
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
        p.value < 0.05  ~ "*",   p.value < 0.10  ~ "\u2020",
        TRUE ~ ""
      ),
      entry = paste0(sprintf("%.3f", estimate),
                     " (", sprintf("%.3f", conf.low),
                     ", ", sprintf("%.3f", conf.high), ")", sig)
    )
  
  binary_wide <- pooled_binary_clean %>%
    select(term, Model, entry) %>%
    pivot_wider(names_from = Model, values_from = entry) %>%
    rename(Covariate = term) %>%
    select(any_of(c("Covariate",
                    binary_model_specs$A$name, binary_model_specs$B$name,
                    binary_model_specs$C$name, binary_model_specs$D$name)))
  
  write_csv(binary_wide,
            file.path(appendix_path, "TableA9_Binary_Logistic_Regression.csv"))
} else {
  write_csv(tibble(Note = "All models failed"),
            file.path(appendix_path, "TableA9_Binary_Logistic_Regression.csv"))
}

###############################
#           FIGURE A6         #
###############################

adoption_long_gam <- prepare_adoption_long(dat_imp1) %>%
  mutate(adoption_num = as.numeric(adoption))

gam_adoption <- tryCatch(
  mgcv::gam(
    adoption_num ~ s(age_numeric, k = 5) + s(income_numeric, k = 5) +
      s(farm_acres_numeric, k = 5) + s(familiarity_matched, k = 5) +
      education_numeric + evaluation_style + solar_type,
    data = adoption_long_gam, method = "REML"
  ),
  error = function(e) NULL
)

if (!is.null(gam_adoption)) {
  
  smooth_specs_adopt <- list(
    list(var = "age_numeric",         label = "Age"),
    list(var = "income_numeric",      label = "Household Income"),
    list(var = "farm_acres_numeric",  label = "Farm Size (acres)"),
    list(var = "familiarity_matched", label = "Familiarity")
  )
  
  gam_adopt_plots <- map(smooth_specs_adopt, function(spec) {
    nd <- data.frame(
      age_numeric         = median(adoption_long_gam$age_numeric,         na.rm = TRUE),
      income_numeric      = median(adoption_long_gam$income_numeric,      na.rm = TRUE),
      farm_acres_numeric  = median(adoption_long_gam$farm_acres_numeric,  na.rm = TRUE),
      familiarity_matched = median(adoption_long_gam$familiarity_matched, na.rm = TRUE),
      education_numeric   = median(adoption_long_gam$education_numeric,   na.rm = TRUE),
      evaluation_style    = factor("Cost-Focused",
                                   levels = levels(adoption_long_gam$evaluation_style)),
      solar_type          = factor("distributed",
                                   levels = levels(adoption_long_gam$solar_type))
    )
    nd <- nd[rep(1, 200), ]
    nd[[spec$var]] <- seq(min(adoption_long_gam[[spec$var]], na.rm = TRUE),
                          max(adoption_long_gam[[spec$var]], na.rm = TRUE),
                          length.out = 200)
    rownames(nd) <- NULL
    pred   <- predict(gam_adoption, newdata = nd, type = "response", se.fit = TRUE)
    nd$fit <- pred$fit
    nd$lwr <- pred$fit - 1.96 * pred$se.fit
    nd$upr <- pred$fit + 1.96 * pred$se.fit
    ggplot(nd, aes_string(x = spec$var, y = "fit")) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "#7570b3") +
      geom_line(color = "#7570b3", linewidth = 0.8) +
      geom_rug(data = adoption_long_gam, aes_string(x = spec$var, y = NULL),
               alpha = 0.15, sides = "b") +
      labs(x = spec$label, y = "Predicted Adoption Intention") +
      theme_bw(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })
  
  p_gam_adopt <- wrap_plots(gam_adopt_plots, ncol = 2)
  
  ggsave(file.path(appendix_path, "FigureA6_Adoption_Intention_GAM.png"),
         p_gam_adopt, width = 10, height = 8, dpi = 300)
}

###############################
#           TABLE A10         #
###############################

results_noclass <- list()
for (i in seq_len(imputed_with_class$m)) {
  imp_data      <- complete(imputed_with_class, i)
  adoption_long <- prepare_adoption_long(imp_data)
  m_nc <- tryCatch(
    clmm(adoption ~ age_numeric + education_numeric + income_numeric +
           farm_acres_numeric + owner + male + white + livestock + farmer +
           familiarity_matched + new_developments +
           solar_type + (1 | respondent_id),
         data = adoption_long, link = "logit"),
    error = function(e) NULL
  )
  results_noclass[[i]] <- list(model = m_nc, data = adoption_long)
}

pooled_noclass <- pool_clmm_results(results_noclass, "No Class (Full Covariates)")

noclass_table <- pooled_noclass %>%
  filter(!str_detect(term, "\\|"),
         !str_detect(term, "^solar_type")) %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
      p.value < 0.05  ~ "*",   p.value < 0.10  ~ "\u2020",
      TRUE ~ ""
    ),
    entry = paste0(sprintf("%.3f", estimate),
                   " (", sprintf("%.3f", conf.low),
                   ", ", sprintf("%.3f", conf.high), ")", sig)
  ) %>%
  select(term, entry) %>%
  rename(Covariate = term)

write_csv(noclass_table,
          file.path(appendix_path, "TableA10_Adoption_Intention_without_Class.csv"))