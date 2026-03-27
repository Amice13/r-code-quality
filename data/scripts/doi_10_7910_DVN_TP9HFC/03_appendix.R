data<-readRDS("data/processed/data.rds")

library(dplyr)
library(tidyr)
library(stringr)
library(kableExtra)
library(knitr)
library(estimatr)
library(gt)
library(texreg)
library(modelsummary)
library(here)

#Create dummy variables for balance test and sample table
data <- data %>%
  mutate(
    college = as.integer(edu_lvl %in% c(
      "Superior completo")),
    evangelical = as.integer(religion == "Evangélico"),
    black = as.integer(race == "Preta"),
    income_num=ifelse(
      income=="From R$2,005 to R$8,640",1,0)
  ) %>%
  mutate(treatment = factor(treatment, levels = c("Control","Crude","Journalistic")))%>%
  rename(Q5_1 = `Q5#1`,
         Q5_2 = `Q5#2`,
         Q5_3 = `Q5#3`,
         Q5_4 = `Q5#4`,
         Q5_5 = `Q5#5`,
         Q5_6 = `Q5#6`,
         Q5_7 = `Q5#7`) %>% 
  mutate(
    social_media_consumer = if_else(Q5_4 == 1 | Q5_5 == 1, 1, 0)
  )


#Appendix A: Respondents’ Demographics and balance tables

#Table 1: Sample details versus target population
df_gender <- data %>%
  count(gender, name = "Sample (N)") %>%
  mutate(`Sample (%)` = round(100 * `Sample (N)` / sum(`Sample (N)`), 1)) %>%
  rename(Category = gender)


df_age <- data %>%
  count(age_cat, name = "Sample (N)") %>%
  mutate(`Sample (%)` = round(100 * `Sample (N)` / sum(`Sample (N)`), 1)) %>%
  rename(Category = age_cat)

df_region <- data %>%
  count(region, name = "Sample (N)") %>%
  mutate(`Sample (%)` = round(100 * `Sample (N)` / sum(`Sample (N)`), 1)) %>%
  rename(Category = region)


df_gender <- df_gender %>%
  mutate(`Population (%)` = c(Woman = 51.5, Man = 48.5))


df_age <- df_age %>%
  mutate(`Population (%)` = 
           c("16-24" = 18, "25-34" = 19, "35-44" = 20, "45-59" = 23, ">=60" = 20))

df_region <- df_region %>%
  mutate(`Population (%)` = 
           c("North" = 8.5, "North East" = 27, "South East" = 41.8, "South" = 14.7, "Center-West" = 8))

kable(df_gender, format = "latex", booktabs = TRUE, align = "lccc",
      col.names = c("Category", "Sample (%)", "Population (%)", "Sample (N)"),
      caption = "Gender Details") %>%
  kable_styling(latex_options = c("hold_position"))

kable(df_age, format = "latex", booktabs = TRUE, align = "lccc",
      col.names = c("Category", "Sample (%)", "Population (%)", "Sample (N)"),
      caption = "Age Details") %>%
  kable_styling(latex_options = c("hold_position"))

kable(df_region, format = "latex", booktabs = TRUE, align = "lccc",
      col.names = c("Category", "Sample (%)", "Population (%)", "Sample (N)"),
      caption = "Geographical Location Details") %>%
  kable_styling(latex_options = c("hold_position"))


# Table 2: T tests for pre-treatment covariates by partisanship
vars <- c(
  age         = "age",
  female      = "female",
  college     = "college",
  interest    = "intr_pol_dummy",
  evangelical = "evangelical",
  black       = "black",
  income      = "income_num"
)

run_block_tests_raw <- function(treat_df, control_df, vars_vec) {
  tests <- vector("list", length(vars_vec))
  for (i in seq_along(vars_vec)) {
    v <- vars_vec[i]
    tests[[i]] <- tryCatch(
      t.test(treat_df[[v]], control_df[[v]], pool.sd = FALSE, na.rm = TRUE),
      error = function(e) NULL
    )
  }
  out <- as.data.frame(sapply(tests, function(x) {
    if (is.null(x)) return(c(NA, NA, p.value = NA, ci.lower = NA, ci.upper = NA))
    c(x$estimate[1], x$estimate[2], p.value = x$p.value, ci.lower = x$conf.int[1], ci.upper = x$conf.int[2])
  }))
  out
}


# --- Petistas ---
treatment <- data %>% filter(ideo_group == "Petistas",       treatment == "Crude")
control   <- data %>% filter(ideo_group == "Petistas",       treatment == "Control")

t_petistas <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_petistas) <- paste0(names(vars), "_pet")  
t_petistas <- as.data.frame(t(t_petistas)) 

# --- Antipetistas ---
treatment <- data %>% filter(ideo_group == "Antipetistas",   treatment == "Crude")
control   <- data %>% filter(ideo_group == "Antipetistas",   treatment == "Control")

t_antipetistas <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_antipetistas) <- paste0(names(vars), "_anti")
t_antipetistas <- as.data.frame(t(t_antipetistas))

# --- Non-Partisans ---
treatment <- data %>% filter(ideo_group == "Non-partisans",  treatment == "Crude")
control   <- data %>% filter(ideo_group == "Non-partisans",  treatment == "Control")

t_np <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_np) <- paste0(names(vars), "_np")
t_np <- as.data.frame(t(t_np))

# Bind 
df_balance_crude <- bind_rows(t_petistas, t_antipetistas, t_np)
df_balance_crude <- tibble::rownames_to_column(df_balance_crude, var = "variable")

df_balance_crude <- df_balance_crude %>%
  separate(variable, c("variable", "block"), "_") %>%
  mutate(variable = str_to_title(variable),
         Block = case_when(
           block == "pet"  ~ "Petistas",
           block == "anti" ~ "Antipetistas",
           block == "np"   ~ "Non-Partisans",
           TRUE            ~ block
         )) %>%
  relocate(Block) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))



# ========== JOURNALISTIC vs CONTROL =============================================

# --- Petistas ---
treatment <- data %>% filter(ideo_group == "Petistas",       treatment == "Journalistic")
control   <- data %>% filter(ideo_group == "Petistas",       treatment == "Control")

t_petistas_j <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_petistas_j) <- paste0(names(vars), "_pet")
t_petistas_j <- as.data.frame(t(t_petistas_j))

# --- Antipetistas ---
treatment <- data %>% filter(ideo_group == "Antipetistas",   treatment == "Journalistic")
control   <- data %>% filter(ideo_group == "Antipetistas",   treatment == "Control")

t_antipetistas_j <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_antipetistas_j) <- paste0(names(vars), "_anti")
t_antipetistas_j <- as.data.frame(t(t_antipetistas_j))

# --- Non-Partisans ---
treatment <- data %>% filter(ideo_group == "Non-partisans",  treatment == "Journalistic")
control   <- data %>% filter(ideo_group == "Non-partisans",  treatment == "Control")

t_np_j <- run_block_tests_raw(treatment, control, unname(vars))
colnames(t_np_j) <- paste0(names(vars), "_np")
t_np_j <- as.data.frame(t(t_np_j))

# Bind 
df_balance_journalistic <- bind_rows(t_petistas_j, t_antipetistas_j, t_np_j)
df_balance_journalistic <- tibble::rownames_to_column(df_balance_journalistic, var = "variable")

df_balance_journalistic <- df_balance_journalistic %>%
  separate(variable, c("variable", "block"), "_") %>%
  mutate(variable = str_to_title(variable),
         Block = case_when(
           block == "pet"  ~ "Petistas",
           block == "anti" ~ "Antipetistas",
           block == "np"   ~ "Non-Partisans",
           TRUE            ~ block
         )) %>%
  relocate(Block) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))


normalize_balance_cols <- function(df) {
  # Ensure these exist; create p.value if missing
  if (!"p.value" %in% names(df)) df[["p.value"]] <- NA_real_
  
  df %>%
    rename(
      # your columns before binding:
      # "Block", "variable", "block", "mean of x", "mean of y"
      Block    = Block,
      Variable = variable,
      # keep the short block code too (e.g., pet/anti/np) if you still have it
      block    = block,
      mean_t   = `mean of x`,
      mean_c   = `mean of y`
      # p.value already present or set above
    ) %>%
    # Clean label case (Age, College, …)
    mutate(Variable = str_to_title(Variable))
}

df_crude_std <- normalize_balance_cols(df_balance_crude) %>%
  mutate(Treatment = "crude")

df_jour_std  <- normalize_balance_cols(df_balance_journalistic) %>%
  mutate(Treatment = "journalistic")

balance_combined <- bind_rows(df_crude_std, df_jour_std) %>%
  select(Treatment, Variable, Block, mean_t, mean_c, p.value) %>%
  mutate(
    Treatment = factor(Treatment, levels = c("crude","journalistic")),
    Variable  = factor(Variable,
                       levels = c("Age","College","Female","Interest","Evangelical","Black","Income")),
    Block = factor(Block, levels = c("Petistas","Antipetistas","Non-Partisans"))
  ) %>%
  arrange(Treatment, Variable)


balance_wide <- balance_combined %>%
  pivot_wider(
    names_from  = Block,
    values_from = c(mean_t, mean_c, p.value)
  )

balance_wide <- balance_wide %>%
  select(
    Treatment, Variable,
    mean_t_Petistas,      mean_c_Petistas,      p.value_Petistas,
    mean_t_Antipetistas,  mean_c_Antipetistas,  p.value_Antipetistas,
    `mean_t_Non-Partisans`, `mean_c_Non-Partisans`, `p.value_Non-Partisans`
  ) %>%
  # Round to 2 decimals like your example
  mutate(across(-c(Treatment, Variable), ~ round(.x, 2)))%>%
  arrange(Variable)



balance_wide %>%
  kable(
    format = "latex", booktabs = TRUE, escape = FALSE,
    align = "ll|rrr|rrr|rrr",
    caption = "T tests for pre-treatment covariates by partisanship",
    label = "balance_combined"
  ) |>
  add_header_above(c(
    " "               = 2,   # use a space, not ""
    "\\textbf{Petistas}"      = 3,
    "\\textbf{Antipetistas}"  = 3,
    "\\textbf{Non-partisans}" = 3
  ), escape = FALSE) |>
  kable_styling(latex_options = c("hold_position", "scale_down"))


#Appendix 
# Load table functions created to produce the appendix -----------------------------------------------------

source("table_functions.R")

# Read processed data ------------------------------------------------------
data_specific_prompts <- readRDS("data/processed/data_specific_prompts.rds")
data_general_rumors <- readRDS("data/processed/data_general_rumors.rds")
data_polarization_voters <- readRDS("data/processed/data_polarization_voters.rds")
data_polarization_politicians <- readRDS("data/processed/data_polarization_politicians.rds")
data_polarization_attitudes_1 <- readRDS("data/processed/data_polarization_attitudes_1.rds")
data_polarization_attitudes_2 <- readRDS("data/processed/data_polarization_attitudes_2.rds")
data_polarization_game <- readRDS("data/processed/data_polarization_game.rds")
data_polarization_game <- data_polarization_game %>% 
  mutate(
    dv = affective_polarization_game
  )
data_specific_prompts_items <- readRDS("data/processed/data_specific_prompts_items.rds")
data_general_rumors_items <- readRDS("data/processed/data_general_rumors_items.rds")


# Appendix D: Baseline results
#Create folder output/tables/baseline_results to be able to save it following the function

#Table 3: Belief in the Specific Claims Included in Prompts
prompts_items <- c("Q17", "Q19", "Q21", "Q23", "Q25", "Q27")
data_belief_prompts <- get_data(data, prompts_items)
get_baseline_results(data_belief_prompts, "prompts.tex", "Belief in the Specific Claims Included in Prompts")


#Table 4: Belief in General Narratives about Lula
rumors_items <- c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5", "Q28_6")
data_belief_rumors <- get_data(data, rumors_items)
get_baseline_results(data_belief_rumors, "rumors.tex", "Belief in General Narratives about Lula")


#Table 5: Attitudinal Measure: First Measure of Social Distance
data_sd <- data %>% 
  mutate(Q30_1=ifelse(Q30_1==1 | Q30_1==2,1,0),
         Q30_2=ifelse(Q30_2==1 | Q30_2==2,1,0),
         Q30_3=ifelse(Q30_3==1 | Q30_3==2,1,0),
         Q30_4=ifelse(Q30_4==1 | Q30_4==2,1,0),
         Q30_5=ifelse(Q30_5==1 | Q30_5==2,1,0))

social_distance_items <- c("Q30_1", "Q30_2", "Q30_3", "Q30_4", "Q30_5")

data_social_distance <- get_data(data_sd, social_distance_items)

get_baseline_results(data_social_distance, "polarization_attitudes_1.tex", "Attitudinal Measure: First Measure of Social Distance")


#Table 6: Attitudinal Measure: Second Measure of Social Distance
data_sd_2 <- data %>% 
  mutate(Q31_1=ifelse(Q31_1==4 | Q31_1==3,1,0),
         Q31_2=ifelse(Q31_2==4 | Q31_1==3 ,1,0),
         Q31_3=ifelse(Q31_3==1 | Q31_3==2 ,1,0),
         Q31_4=ifelse(Q31_4==4 | Q31_4==4 ,1,0))

social_distance_items <- c("Q31_1", "Q31_2", "Q31_3", "Q31_4")

data_social_distance_2 <- get_data(data_sd_2, social_distance_items)

get_baseline_results(data_social_distance_2, "polarization_attitudes_2.tex", "Attitudinal Measure: Second Measure of Social Distance")

#Table 7: Behavioral Measure: Game-based Measure of Affective Polarization
data_game <- data %>%
  mutate(dv=10-QJUEGO,
         affective_polarization_game=dv-QJUEGO)

get_baseline_results(data_game, "game.tex", "Behavioral Measure: Game-based Measure of Affective Polarization")


# Appendix E: Regression tables

#Table 8: Tabular results / Figure 3 (note: the figure focuses on the coefficient on the ”journalistic” style)

belief_filters <- list(
  NULL,
  'ideo_group=="Petistas"',
  'ideo_group=="Antipetistas"',
  'ideo_group=="Non-partisans"'
)

demographic_controls <- c("gender", 
                          "income", 
                          "age", 
                          "religion", 
                          "intr_pol_dummy")



data_belief_prompts_comparing_treatments <- set_treatment_level_reference(data_belief_prompts, "Crude")
prompts_models_comp <- estimate_models(data_belief_prompts_comparing_treatments,
                                       outcome = "scale(dv)",
                                       treatment = "treatment",
                                       controls = NULL,
                                       filters = belief_filters)



export_reg_table(
  prompts_models_comp, 
  file = "belief_prompts_reg_table.tex",
  title = "This table replicates Figure 3",
  header = list("Belief in specific claims about Lula" = 1:4),
  gof_names = c(NA, NA, "N", NA),
  coef_map=list("treatmentControl"="Control","treatmentJournalistic"="Journalistic"),
  label = "table:reg_prompts_comparison",
)


#Table 9: Tabular Results / Figure 4
data_belief_prompts_reg_table <- data_belief_prompts

prompts_models <- estimate_models(data_belief_prompts,
                                  outcome = "scale(dv)",
                                  treatment = "treatment",
                                  controls = NULL,
                                  filters = belief_filters)



export_reg_table(
  prompts_models, 
  file = "belief_prompts_reg_table_sig.tex",
  title = "This table replicates Figure 4",
  header = list("Belief in specific claims about Lula" = 1:4),
  gof_names = c(NA, NA, "N", NA),
  coef_map=list("treatmentJournalistic"="Journalistic","treatmentCrude"="Crude"),
  label = "table:reg_prompts",
)


# Table 10: Tabular results / Figure 5 (note: the figure focuses on the coefficient on the ”journalistic” style)

data_belief_rumors_comparing_treatments <- set_treatment_level_reference(data_belief_rumors, "Crude")
rumors_models_comp <- estimate_models(data_belief_rumors_comparing_treatments,
                                      outcome = "scale(dv)",
                                      treatment = "treatment",
                                      controls = NULL,
                                      filters = belief_filters)

export_reg_table(
  rumors_models_comp, 
  file = "belief_rumors_reg_table.tex",
  title = "This table replicates Figure 5",
  header = list("Belief in rumors about Lula" = 1:4),
  coef_map=list("treatmentControl"="Control","treatmentJournalistic"="Journalistic"),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_rumors_comparison",
)

# Table 11: Tabular Results / Figure 6

data_belief_rumors_reg_table <- data_belief_rumors

rumors_models <- estimate_models(data_belief_rumors,
                                 outcome = "scale(dv)",
                                 treatment = "treatment",
                                 controls = NULL,
                                 filters = belief_filters)



export_reg_table(
  rumors_models, 
  file = "belief_rumors_reg_table_sig.tex",
  title = "This table replicates Figure 6",
  header = list("Belief in specific claims about Lula" = 1:4),
  coef_map=list("treatmentJournalistic"="Journalistic",
                "treatmentCrude"="Crude"),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_rumors",
)

#Table 12: Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula (omitted category = control) for Petista
belief_prompts_petistas <- list(
  q17<-lm_robust(Q17~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight),
  q19<-lm_robust(Q19~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight),
  q21<-lm_robust(Q21~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight),
  q23<-lm_robust(Q23~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight),
  q25<-lm_robust(Q25~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight),
  q27<-lm_robust(Q27~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Petistas"), weights = weight)
)
export_reg_table_items(
  belief_prompts_petistas, 
  file = "belief_prompts_items_petistas_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula
(omitted category = control) for Petistas",
  header = list("Belief in specific claims about Lula (Petistas)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_prompts_items_petistas"
)


#Table 13: Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula (omitted category = control) for Antipetistas

belief_prompts_antipetistas <- list(
  q17<-lm_robust(Q17~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  q19<-lm_robust(Q19~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  q21<-lm_robust(Q21~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  q23<-lm_robust(Q23~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  q25<-lm_robust(Q25~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  q27<-lm_robust(Q27~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Antipetistas"), weights = weight)
)
export_reg_table_items(
  belief_prompts_antipetistas, 
  file = "belief_prompts_items_antipetistas_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula
(omitted category = control) for Antipetistas",
  header = list("Belief in specific claims about Lula (Antipetistas)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_prompts_items_antipetistas"
)


#Table 14: Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula (omitted category = control) for Non-partisans

belief_prompts_np <- list(
  q17<-lm_robust(Q17~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  q19<-lm_robust(Q19~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  q21<-lm_robust(Q21~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  q23<-lm_robust(Q23~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  q25<-lm_robust(Q25~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  q27<-lm_robust(Q27~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_specific_prompts_items%>%filter(ideo_group=="Non-partisans"), weights = weight)
)
export_reg_table_items(
  belief_prompts_np, 
  file = "belief_prompts_items_np_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula
(omitted category = control) for Non-partisans",
  header = list("Belief in specific claims about Lula (Non-partisans)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_prompts_items_np"
)


#Table 15: Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula (omitted category = control) for Petistas

belief_prompts_np <- list(
  Q28_1<-lm_robust(Q28_1~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight),
  Q28_2<-lm_robust(Q28_2~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight),
  Q28_3<-lm_robust(Q28_3~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight),
  Q28_4<-lm_robust(Q28_4~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight),
  Q28_5<-lm_robust(Q28_5~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight),
  Q28_6<-lm_robust(Q28_6~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Petistas"), weights = weight)
)
export_reg_table_items(
  belief_prompts_np, 
  file = "belief_rumors_items_petistas_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula
(omitted category = control) for Petistas",
  header = list("Belief in general rumors about Lula (Petistas)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_rumors_items_petistas"
)

## Table 16: Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula (omitted category = control) for Antipetistas

belief_prompts_np <- list(
  Q28_1<-lm_robust(Q28_1~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  Q28_2<-lm_robust(Q28_2~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  Q28_3<-lm_robust(Q28_3~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  Q28_4<-lm_robust(Q28_4~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  Q28_5<-lm_robust(Q28_5~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight),
  Q28_6<-lm_robust(Q28_6~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Antipetistas"), weights = weight)
)
export_reg_table_items(
  belief_prompts_np, 
  file = "belief_rumors_items_antipetistas_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula
(omitted category = control) for Antipetistas",
  header = list("Belief in general rumors about Lula (Antipetistas)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_rumors_items_antipetistas"
)


## Table 17: Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula (omitted category = control) for Non-partisans

belief_prompts_np <- list(
  Q28_1<-lm_robust(Q28_1~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  Q28_2<-lm_robust(Q28_2~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  Q28_3<-lm_robust(Q28_3~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  Q28_4<-lm_robust(Q28_4~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  Q28_5<-lm_robust(Q28_5~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight),
  Q28_6<-lm_robust(Q28_6~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_general_rumors_items%>%filter(ideo_group=="Non-partisans"), weights = weight)
)
export_reg_table_items(
  belief_prompts_np, 
  file = "belief_rumors_items_np_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula
(omitted category = control) for Non-partisans",
  header = list("Belief in general rumors about Lula (Non-partisans)" = 1:6),
  gof_names = c(NA, NA, "N", NA),
  label = "table:reg_rumors_items_np"
)


#Table 18: The Effect of Misinformation Style on Feeling Thermometer / Outgroup Voters (omitted = control)

polarization_voters <- list(
  pol_voters_all<-lm_robust(dv~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_polarization_voters, weights = weight),
  pol_voters_petistas<-lm_robust(dv~treatment, se_type = 'HC2', data = data_polarization_voters%>%filter(ideo_group=="Petistas")),
  pol_voters_antipetistas<-lm_robust(dv~treatment , se_type = 'HC2', data= data_polarization_voters%>%filter(ideo_group=="Antipetistas")),
  pol_voters_np<-lm_robust(dv~treatment , se_type = 'HC2', data= data_polarization_voters%>%filter(ideo_group=="Non-partisans"))
)

export_reg_table(
  polarization_voters,
  file = "pol_voters_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Feeling Thermometer / Outgroup Voters (omitted = control)",
  gof_names = c(NA, NA, "N", NA),
  coef_map=list("treatmentJournalistic"="Journalistic","treatmentCrude"="Crude","(Intercept)"="Intercept"),
  header =list("Feeling Thermometer / Outgroup Voters" = 1:4),
  label = "table:reg_polarization_voters"
)



#Table 19: The Effect of Misinformation Style on Feeling Thermometer / Outgroup Politicians (omitted =control)

polarization_politicians <- list(
  pol_politicians_all<-lm_robust(dv~treatment, fixed_effects = ~ ideo_group, se_type = 'HC2', data = data_polarization_politicians, weights = weight),
  pol_politicians_petistas<-lm_robust(dv~treatment,  se_type = 'HC2', data = data_polarization_politicians%>%filter(ideo_group=="Petistas")),
  pol_politicians_antipetistas<-lm_robust(dv~treatment,  se_type = 'HC2', data= data_polarization_politicians%>%filter(ideo_group=="Antipetistas")),
  pol_politicians_np<-lm_robust(dv~treatment,  se_type = 'HC2', data= data_polarization_politicians%>%filter(ideo_group=="Non-partisans"))
)

export_reg_table(
  polarization_politicians,
  file = "pol_politicians_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Feeling Thermometer / Outgroup Politicians (omitted = control)",
  gof_names = c(NA, NA, "N", NA),
  coef_map=list("treatmentJournalistic"="Journalistic","treatmentCrude"="Crude","(Intercept)"="Intercept"),
  header =list("Feeling Thermometer / Outgroup Politicians" = 1:4),
  label = "table:reg_polarization_politicians"
)


#Table 20: The Effect of Misinformation Style on Social Distance /Figure 7 (omitted = control)
social_distance<-readRDS("data/processed/data_polarization_attitudes_1.rds")


social_distance_table<- list(
  reg_antipetistas<-lm_robust(scale(dv)~treatment,fixed_effects = prompt, data=social_distance%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista")),
  
  reg_petistas<-lm_robust(scale(dv)~treatment,  fixed_effects = prompt,data=social_distance%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista")),
  
  
  reg_all<-lm_robust(scale(dv)~treatment , data=social_distance%>%filter(ideo_group!= "Non-partisans"),fixed_effects = ideo_group + prompt,weights = weight  ))



export_reg_table3(
  models = social_distance_table,
  file   = "social_distance_finaltable.tex",
  title  = "The Effect of Misinformation Style on Social Distance /Figure 7 (omitted = control)",
  label  = "table:reg_polarization_voters3"
)





#Table 21: The Effect of Misinformation Style on Coupons /Figure 6 (omitted = control)

game<-readRDS("data/processed/data_polarization_game.rds")


game_table<- list(
  reg_antipetistas<-lm_robust(scale(keep)~treatment, data=game%>%filter(ideo_group=="Antipetistas" & partido_jugador2=="Petista")),
  
  reg_petistas<-lm_robust(scale(keep)~treatment, data=game%>%filter(ideo_group=="Petistas" & partido_jugador2=="Antipetista" ))
  
  )



export_reg_table4(
  models = game_table,
  file   = "game_final_table.tex",
  title  = "The Effect of Misinformation Style on Coupons /Figure 6 (omitted = control)",
  label  = "table:reg_polarization_voters1"
)






#Table 22: Model Estimates of Style of Misinformation on Beliefs in Specific Claims About Lula (with demographic controls and interest in politics)
prompts_items <- c("Q17", "Q19", "Q21", "Q23", "Q25", "Q27")
data_belief_prompts <- get_data(data, prompts_items)



prompts_models <- estimate_models(data_belief_prompts,
                                  outcome = "dv",
                                  treatment = "treatment",
                                  controls = demographic_controls,
                                  filters = belief_filters)


export_reg_table(
  prompts_models, 
  file = "controls_belief_prompts_reg_table.tex",
  title = "This table reproduces the results for Figure ",
  header = list("Belief in specific claims about Lula" = 1:4),
  coef_map=list("treatmentJournalistic"="Journalistic","treatmentCrude"="Crude",
"genderMale"="Male",
"incomeFrom R$1,255.00 to R$2,004"="Income level 2",
"incomeFrom R$2,005 to R$8,640"="Income level 3",
"incomeFrom R$8,641 to R$11,261"="Income level 4",
"incomeMore than R$11,262"="Income level 5",
"age"="Age",
"religionEvangélico"="Evangelic",               
"religionCrente sem religião"="Believer - no religion",   "religionOutra religião"= "Other religion",          
"religionAgnóstico ou ateu"= "Agnostic or Atheist",       
"intr_pol_dummy"="Interested in politcs"
                ),
  omitted = "Intercept",
  label = "table:reg_prompts_controls",
)




#Table 23: Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula (with demographic controls and interest in politics)
rumors_items <- c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5", "Q28_6")
data_belief_rumors <- get_data(data, rumors_items)

rumors_models <- estimate_models(data_belief_rumors,
                                 outcome = "dv",
                                 treatment = "treatment",
                                 controls = demographic_controls,
                                 filters = belief_filters)

export_reg_table(
  rumors_models, 
  file = "controls_belief_rumors_reg_table.tex",
  title = "Model Estimates of Style of Misinformation on Beliefs in General Rumors About Lula (with demographic controls and interest in politics)",
  header = list("Belief in general rumors about Lula" = 1:4),
  coef_map=list("treatmentJournalistic"="Journalistic","treatmentCrude"="Crude",
                "genderMale"="Male",
                "incomeFrom R$1,255.00 to R$2,004"="Income level 2",
                "incomeFrom R$2,005 to R$8,640"="Income level 3",
                "incomeFrom R$8,641 to R$11,261"="Income level 4",
                "incomeMore than R$11,262"="Income level 5",
                "age"="Age",
                "religionEvangélico"="Evangelic",               
                "religionCrente sem religião"="Believer - no religion",   "religionOutra religião"= "Other religion",          
                "religionAgnóstico ou ateu"= "Agnostic or Atheist",       
                "intr_pol_dummy"="Interested in politcs"
  ),
  omitted = "Intercept",
  label = "table:reg_rumors_controls",
)



#Table 24: Model Estimates of Style of Misinformation on Belief / (people who access the news only though internet and social media)
prompts_items <- c("Q17", "Q19", "Q21", "Q23", "Q25", "Q27")
data_belief_prompts_sm <- get_data(data, prompts_items)



belief_prompts_social_media <- list(
  reg_antipetistas<-lm_robust(scale(dv)~treatment + social_media_consumer, data=data_belief_prompts_sm%>%filter(ideo_group=="Antipetistas")),
  
  reg_petistas<-lm_robust(scale(dv)~treatment + social_media_consumer, data=data_belief_prompts_sm%>%filter(ideo_group=="Petistas")),
  
  reg_np<-lm_robust(scale(dv)~treatment + social_media_consumer + np_pt + np_antipt, data=data_belief_prompts_sm%>%filter(ideo_group=="Non-partisans"))
  
)

export_reg_table2(
  models = belief_prompts_social_media,
  file   = "social_media_belief.tex",
  title  = "Model Estimates of Style of Misinformation on Belief / \n(people who access the news only though internet and social media)",
  label  = "table:reg_belief_social_media"
)




#Table 25: Model Estimates of Style of Misinformation on General Narratives / (people who access the news only though internet and social media)

rumors_items <- c("Q28_1", "Q28_2", "Q28_3", "Q28_4", "Q28_5", "Q28_6")
data_belief_sm <- get_data(data, rumors_items)



belief_item_social_media <- list(
  reg_antipetistas<-lm_robust(scale(dv)~treatment + social_media_consumer, data=data_belief_sm%>%filter(ideo_group=="Antipetistas")),
  
  reg_petistas<-lm_robust(scale(dv)~treatment + social_media_consumer, data=data_belief_sm%>%filter(ideo_group=="Petistas")),
  
  reg_np<-lm_robust(scale(dv)~treatment + social_media_consumer + np_pt + np_antipt, data=data_belief_sm%>%filter(ideo_group=="Non-partisans"))
  
)

export_reg_table2(
  models = belief_item_social_media,
  file   = "social_media_narrative.tex",
  title  = "Model Estimates of Style of Misinformation on Belief / \n(people who access the news only though internet and social media)",
  label  = "table:reg_narrative_social_media"
)





#Table 26: The Effect of Experimental Group on Time Spent on The Survey (omitted category = journalistic-style
data_time<-data%>%
  filter(duration<=3000) %>% 
  rename(dv = duration)

data_time$treatment
time_spent <- estimate_models(data_time,
                              outcome = "dv",
                              treatment = "treatment",
                              controls = NULL,
                              filters = belief_filters)

export_reg_table(
  time_spent, 
  file = "time_spent_comparison_reg_table.tex",
  title = "Model Estimates of difference of time spent in survey by type of treatment (omitted category = journalistic-style)",
  header = list("Difference of Time Spent in Survey" = 1:4),
  coef_map=list("treatmentJournalistic"="Journalistic",
                "treatmentCrude"="Crude"),
  omitted = "Intercept",
  label = "table:reg_time_spent_comparison",
)


#Table 27: The Effect of Experimental Group on Time Spent on The Survey (omitted category = control)

data_time_comparison <- set_treatment_level_reference(data_time, "Journalistic")

time_spent <- estimate_models(data_time_comparison,
                              outcome = "dv",
                              treatment = "treatment",
                              controls = NULL,
                              filters = belief_filters)

export_reg_table(
  time_spent, 
  file = "time_spent_control_ommited_reg_table.tex",
  title = "Model Estimates of difference of time spent in survey by type of treatment (omitted category = control)",
  header = list("Difference of Time Spent in Survey" = 1:4),
  coef_map=list("treatmentControl"="Control",
                "treatmentCrude"="Crude"),
  omitted = "Intercept",
  label = "table:reg_time_spent_control_omitted",
)


#Appendix F: Attention checks

#Figure 7: Distribution of attention levels across respondents

#The distribution of attention by specific prompts
plot<-data.frame(category=c("high_attention","mid_attention","low_attention","no_attention"),
                 N=c(data%>%filter(high_attention==1)%>%nrow()%>%as.numeric(),
                     data%>%filter(mid_attention==1)%>%nrow()%>%as.numeric(),
                     data%>%filter(low_attention==1)%>%nrow()%>%as.numeric(),
                     data%>%filter(no_attention==1)%>%nrow()%>%as.numeric()))


p_attention<-ggplot(plot, aes(x = reorder(category, -N), y = N)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Category",
    y = "Count") +
  theme_minimal()+
  #ggtitle("Attention Checks by quartiles") +
  #theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=12))



ggsave("output/figures/attention_distribution.pdf", p_attention)



##Figure 8: Effects of the Style of Misinformation on Belief in Specific Claims By Level of Attention
data_prompts<-readRDS("data/processed/data_specific_prompts.rds")


data_prompts$treat_anti<-data_prompts$treatment
data_prompts$treat_anti_high<-data_prompts$treatment
data_prompts$treat_pt<-data_prompts$treatment
data_prompts$treat_pt_high<-data_prompts$treatment
data_prompts$treat_np<-data_prompts$treatment
data_prompts$treat_np_high<-data_prompts$treatment
data_prompts$treat_all_high<-data_prompts$treatment


reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_prompts%>%filter(ideo_group=="Antipetistas"))

reg_antipetistas_high<-lm_robust(scale(dv)~treat_anti_high, data=data_prompts%>%filter(ideo_group=="Antipetistas" & attention_level=="High" ))


reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_prompts%>%filter(ideo_group=="Petistas"))

reg_petistas_high<-lm_robust(scale(dv)~treat_pt_high, data=data_prompts%>%filter(ideo_group=="Petistas" & attention_level=="High" ))

reg_np<-lm_robust(scale(dv)~treat_np + np_pt + np_antipt , data=data_prompts%>%filter(ideo_group=="Non-partisans"))

reg_np_high<-lm_robust(scale(dv)~treat_np_high +np_pt +np_antipt, data=data_prompts%>%filter(ideo_group=="Non-partisans" & attention_level=="High" ))

reg_all<-lm_robust(scale(dv)~treatment, data=data_prompts,weights = weight,fixed_effects =  ideo_group_leaning  )

reg_all_high<-lm_robust(scale(dv)~treat_all_high, data=data_prompts%>%filter(attention_level=="High"),weights = weight, fixed_effects =  ideo_group_leaning)


# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Antipetistas High" = reg_antipetistas_high,
  "Petistas" = reg_petistas,
  "Petistas High" = reg_petistas_high,
  "Non-Partisans" = reg_np,
  "Non-Partisans High" = reg_np_high,
  "All Groups" = reg_all,
  "All Groups High" = reg_all_high
)

# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)


# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%
  filter(
    str_detect(term, "treat")
  )%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic" # Triangle for "Journalistic"
  ))%>%
  mutate(type=c(rep("Antipetistas",4),rep("Petistas",4),rep("Non-Partisans",4),rep("All Groups",4)))%>%
  mutate(attention=c("All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention"))


# Define the specific order for the 'type' variable (facets)
plot_data$type <- factor(plot_data$type, levels = c("Antipetistas", "Petistas", "Non-Partisans", "All Groups"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))


# Plot with ordered facets and shape
color_map2 <- c(
  "All" = "blue",
  "High Attention" = "orange")

# Plot with ordered facets and shape
beliefs2<-ggplot(
  plot_data%>%mutate(group=paste(type,attention,shape)),
  aes(x = estimate, y = group, color = attention, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +
  scale_color_manual(values = color_map2) +  # Apply custom color mapping
  theme_minimal() +
  facet_wrap(
    ~ type,  # Facet by the 'type' variable
    ncol = 2,
    scales = "free_y"
  ) +
  
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text = element_text(size = 12),  # Facet label styling
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "Partisan Groups",
    color = "Attention",
    shape = "Treatment Type"
  ) +
  theme(axis.text=element_text(size=12))


ggsave("output/figures/specific_beliefs_attention.pdf", beliefs2)




##Figure 9: The Effect of Misinformation Style on General Narratives, by Attention Level

data_general<-readRDS("data/processed/data_general_rumors.rds")

data_general$treat_anti<-data_general$treatment
data_general$treat_anti_high<-data_general$treatment
data_general$treat_pt<-data_general$treatment
data_general$treat_pt_high<-data_general$treatment
data_general$treat_np<-data_general$treatment
data_general$treat_np_high<-data_general$treatment
data_general$treat_all_high<-data_general$treatment



reg_antipetistas<-lm_robust(scale(dv)~treat_anti, data=data_general%>%filter(ideo_group=="Antipetistas"))

reg_antipetistas_high<-lm_robust(scale(dv)~treat_anti_high, data=data_general%>%filter(ideo_group=="Antipetistas" & attention_level=="High" ))

reg_petistas<-lm_robust(scale(dv)~treat_pt, data=data_general%>%filter(ideo_group=="Petistas"))

reg_petistas_high<-lm_robust(scale(dv)~treat_pt_high, data=data_general%>%filter(ideo_group=="Petistas" & attention_level=="High" ))

reg_np<-lm_robust(scale(dv)~treat_np +np_pt +np_antipt , data=data_general%>%filter(ideo_group=="Non-partisans"))

reg_np_high<-lm_robust(scale(dv)~treat_np_high +np_pt +np_antipt, data=data_general%>%filter(ideo_group=="Non-partisans" & attention_level=="High" ))

reg_all<-lm_robust(scale(dv)~treatment, data=data_general,fixed_effects = ideo_group_leaning,weights = weight  )

reg_all_high<-lm_robust(scale(dv)~treat_all_high, data=data_general%>%filter(attention_level=="High"),weights = weight, fixed_effects =  ideo_group_leaning  )



# Define the models
models_main <- list(
  "Antipetistas" = reg_antipetistas,
  "Antipetistas High" = reg_antipetistas_high,
  "Petistas" = reg_petistas,
  "Petistas High" = reg_petistas_high,
  "Non-Partisans" = reg_np,
  "Non-Partisans High" = reg_np_high,
  "All Groups" = reg_all,
  "All Groups High" = reg_all_high
)


# Extract the data from modelplot
plot_data <- modelplot(models_main, draw = FALSE)


# Define shapes for Crude and Journalistic treatments
plot_data <- plot_data%>%
  filter(
    str_detect(term, "treat")
  )%>%
  mutate(shape = case_when(
    str_detect(term, "Crude") ~ "Crude",        # Circle for "Crude"
    str_detect(term, "Journalistic") ~ "Journalistic" # Triangle for "Journalistic"
  ))%>%
  mutate(type=c(rep("Antipetistas",4),rep("Petistas",4),rep("Non-Partisans",4),rep("All Groups",4)))%>%
  mutate(attention=c("All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention","All","All","High Attention","High Attention"))


# Define the specific order for the 'type' variable (facets)
plot_data$type <- factor(plot_data$type, levels = c("Antipetistas", "Petistas", "Non-Partisans", "All Groups"))

# Define the specific order for the 'shape' variable
plot_data$shape <- factor(plot_data$shape, levels = c("Journalistic", "Crude"))



color_map2 <- c(
  "All" = "blue",
  "High Attention" = "orange")

# Plot with ordered facets and shape
beliefs5<-ggplot(
  plot_data%>%mutate(group=paste(type,attention,shape)),
  aes(x = estimate, y = group, color = attention, shape = shape)  # Use the ordered shape factor
) +
  geom_point(size = 3) +  # Set point size
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.4) +  # Error bars without ticks
  geom_vline(xintercept = 0, linetype = 2, color = "red") +
  scale_color_manual(values = color_map2) +  # Apply custom color mapping
  theme_minimal() +
  facet_wrap(
    ~ type,  # Facet by the 'type' variable
    ncol = 2,
    scales = "free_y"
  ) +
  
  theme(
    text = element_text(family = "Courier"),
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    strip.text = element_text(size = 12),  # Facet label styling
    panel.spacing = unit(1, "lines")  # Adjust space between facets
  ) +
  labs(
    x = "Effect of Treatment",
    y = "Partisan Groups",
    color = "Attention",
    shape = "Treatment Type"
  ) +
  theme(axis.text=element_text(size=12))


ggsave("output/figures/general_beliefs_attention.pdf", beliefs5)