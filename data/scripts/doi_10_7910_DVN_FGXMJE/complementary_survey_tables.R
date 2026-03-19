library(tidyverse)
library(glue)
library(modelsummary)
library(kableExtra)
library(gt)
library(texreg)
library(gtsummary)
library(stargazer)
library(TeXCheckR)
library(fixest)
library(estimatr)

rm(list = ls())

### LOAD DATA
data_all <- read_csv("Data/complementary_survey_all.csv")

data_noinc = data_all %>% filter(income_fixed == 0 | is.na(income_fixed))


##################
### TABLE FORMATTING FUNCTIONS
##################

remove_escape_latex <- function(x) {
  enable_special_characters = function(x) {
    gsub("\\\\([&%$#_{}])", "\\1", x, fixed = FALSE, ignore.case = TRUE) 
  }
  enable_backslash = function(x) {
    gsub("\\\\textbackslash([[:space:]])?", "\\\\", x, fixed = FALSE, ignore.case = TRUE)
  }
  enable_tilde = function(x) {
    gsub("\\\\textasciitilde([[:space:]])?", "~", x, fixed = FALSE, ignore.case = TRUE)
  }
  enable_exponents = function(x) {
    gsub("\\\\textasciicircum ", "\\^", x, fixed = FALSE, ignore.case = TRUE)
  }
  
  enable_backslash(enable_special_characters(enable_tilde(enable_exponents(x))))
}

custom_latex_changes = function(x){
  insert_midrule = function(x){
    str_replace(x,"Observations", "\\\\midrule \n Observations")
  }
  longtable_to_tabular = function(x){
    str_replace_all(x,"longtable", "tabular")
  }
  custom_top = function(x){
    double_line = "hline \n \\\\hline \n"
    
    str_replace(x, "toprule", double_line)
  }
  
  x %>%
    insert_midrule() %>% 
    longtable_to_tabular() %>%
    custom_top()
  
}


add_table_labels = function(x, labels){
  N = length(labels)
  number_line = 1:N %>% str_c("(", ., ")") %>%
    str_c(collapse = " & ") %>%
    str_c(., "\\\\\\\\ \n")
  label_line = labels %>% 
    str_c(collapse = " & ") %>%
    str_c(" & ", ., "\\\\\\\\\ ")
  total_line = str_c(number_line, label_line)
  x %>% 
    str_replace("Model(.+)\\\\", total_line)
}


add_depvar = function(x, depvar_label, num_col){
  
  label_line = depvar_label %>%
    str_c("\n \\\\multicolumn\\{", num_col+1 , "}{c}{Dependent variable: ", ., "} \\\\\\\\\ \n", 
          "\\\\midrule")
  total_line = label_line
  x %>%
    str_replace("\\\\toprule", str_c("\\\\toprule", total_line))
}
################






###### Define labels

outcomes_selected = c("h_durable","h_electronics","h_credit_card","h_total_spend" )

outcomes_selected_w23 = c("h_durable","h_electronics","h_credit_card",
                          "h_total_spend","h_fin_literacy_asset" )


outcomes_names_selected = c("Dur.", "Trad. Dur.", "Debt", "Total")
outcomes_names_selected_w23 = c("Dur.", "Trad. Dur.", "Debt", "Total", "Hyp. asset")





coef_labels_noinc = c("infla_change" = "$ \\pi^{scenario}_{i} - \\pi^{prior}_{i} $", 
                      "depr_change" = "$d^{scenario}_{i} - d^{prior}_{i} $") 

coef_labels_noinc_joint = c("infla_change" = "$ \\Delta Belief $", 
                            "depr_change" = "$ \\Delta Belief $") 

coef_labels_withinc = c("infla_change" = "$ \\pi^{scenario}_{i} - \\pi^{prior}_{i} $", 
                        "depr_change" = "$d^{scenario}_{i} - d^{prior}_{i} $",
                        "int_infla_income" = "$  \\left(\\pi^{scenario}_{i} - \\pi^{prior}_{i}\\right) \\cdot T^{FI}_i$",
                        "int_depr_income" = "$ \\left(d^{scenario}_{i} - d^{prior}_{i}\\right) \\cdot T^{FI}_i $"
) 

coef_labels_withinc_joint = c("infla_change" = "$ \\Delta Belief $", 
                              "depr_change" = "$ \\Delta Belief $",
                              "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                              "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$"
) 


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Observations",             0,
  "r.squared", "$R^2$", 3)


stat_names = c("Outcome mean" = "value_mean", "Outcome SD" = "value_sd")

############







####################
### TABLE: Effects of Hypothetical Shocks on Planned Expenditures, 
### PANEL A: Baseline
####################


models_infla <- tibble(outcome_var = outcomes_selected, 
                       formula = glue("{outcome_var}_infla ~ infla_change + {outcome_var}_prior")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_forex <- tibble(outcome_var = outcomes_selected, 
                       formula = glue("{outcome_var}_forex ~ depr_change + {outcome_var}_prior")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))


mean_dep_noinc <- data_noinc %>% 
  select(one_of(c(str_c(outcomes_selected, "_infla"),str_c(outcomes_selected, "_forex")))) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise( across(.fns = list(mean = mean, median = median, sd = sd)))  %>% 
  pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
  pivot_wider() %>% 
  mutate(term = fct_recode(term, !!!stat_names))

models_noinc = bind_rows(models_infla, models_forex)
model_labels_noinc =c(outcomes_names_selected, outcomes_names_selected)


table_noinc <- models_noinc %>% 
  pull(model) %>%
  set_names(glue("Model {1:length(.)}")) %>%
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               coef_map = coef_labels_noinc_joint, 
               gof_map = gm,
               add_rows = mean_dep_noinc,
               escape = FALSE,
               estimate = "{estimate}{stars}",
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:5) %>%
  tab_spanner(label = 'Depreciation', columns = 6:9)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_table_labels(labels = model_labels_noinc) %>%
  custom_latex_changes()

table_noinc %>%
  write_lines("output/tables/hypotheticals_noinc.tex")
####################



####################
### TABLE: Effects of Hypothetical Shocks on Planned Expenditures, 
### PANEL B: By financial literacy
####################

models_infla_literate <- tibble(outcome_var = outcomes_selected, 
                                formula = glue("{outcome_var}_infla ~ infla_change + literate + {outcome_var}_prior +  int_literate_infla + {outcome_var}_prior:literate")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_forex_literate <- tibble(outcome_var = outcomes_selected, 
                                formula = glue("{outcome_var}_forex ~ depr_change  + literate + {outcome_var}_prior + int_literate_depr + {outcome_var}_prior:literate")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))


mean_dep_noinc_literacy <- data_noinc %>% 
  filter(!is.na(literate)) %>%
  select(one_of(c(str_c(outcomes_selected, "_infla"),str_c(outcomes_selected, "_forex")))) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise( across(.fns = list(mean = mean, median = median,  sd = sd)))  %>% 
  pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
  pivot_wider() %>% 
  mutate(term = fct_recode(term, !!!stat_names))

models_noinc_literate = bind_rows(models_infla_literate, models_forex_literate)

coef_labels_noinc_literate = c("infla_change" = "$\\Delta Belief $", 
                               "depr_change" = "$\\Delta Belief $",
                               "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                               "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 
model_labels_noinc =c(outcomes_names_selected, outcomes_names_selected)


table_noinc_literate <- models_noinc_literate %>% 
  pull(model) %>%
  set_names(glue("Model {1:length(.)}")) %>%
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               coef_map = coef_labels_noinc_literate, 
               gof_map = gm,
               add_rows = mean_dep_noinc_literacy,
               escape = FALSE,
               estimate = "{estimate}{stars}",
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:5) %>%
  tab_spanner(label = 'Depreciation', columns = 6:9)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_table_labels(labels = model_labels_noinc) %>%
  custom_latex_changes()
table_noinc_literate %>%
  write_lines("output/tables/hypotheticals_by_literacy_combined.tex")
####################







########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, median = median, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla","h_fin_literacy_asset_forex")

mean_dep_asset <- data_all %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("infla", "forex"), 
                        change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("infla", "forex"), 
                               change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_fixed + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_fixed")) 

asset_literate_formulas = tibble(scenario = c("infla", "forex"), 
                                 change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:literate")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_all = bind_rows(models_asset, 
                             #models_asset_income, 
                             models_asset_literate) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% 
  pull(model) %>% 
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               coef_map = coef_labels_asset_joint, 
               gof_map = gm,
               add_rows = mean_dep_asset,
               escape = FALSE,
               estimate = "{estimate}{stars}",
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:3) %>%
  tab_spanner(label = 'Depreciation', columns = 4:5)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy.tex")
###########


########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security (With Heterogeneity)
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla","h_fin_literacy_asset_forex")

mean_dep_asset <- data_all %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("infla", "forex"), 
                        change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("infla", "forex"), 
                               change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_clean + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_clean")) 

asset_literate_formulas = tibble(scenario = c("infla", "forex"), 
                                 change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:literate")) 

asset_marital_formulas = tibble(scenario = c("infla", "forex"), 
                                 change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ marital_clean + {change}_change + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:marital_clean")) 

asset_education_formulas = tibble(scenario = c("infla", "forex"), 
                                 change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ education_clean + {change}_change + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:education_clean")) 

asset_credit_formulas = tibble(scenario = c("infla", "forex"), 
                                  change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_credit_card + {change}_change + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:has_credit_card")) 

asset_bank_formulas = tibble(scenario = c("infla", "forex"), 
                             change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_bank_account + {change}_change + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:has_bank_account")) 

asset_employement_formulas = tibble(scenario = c("infla", "forex"), 
                             change = c("infla", "depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ employment_clean + {change}_change  + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:employment_clean")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_marital <- asset_marital_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_education <- asset_education_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_bank <- asset_bank_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_credit <- asset_credit_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_employement <- asset_employement_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))



models_asset_all = bind_rows(models_asset, 
                             models_asset_income, 
                             models_asset_literate, 
                             models_asset_marital, 
                             models_asset_education,
                             models_asset_bank, 
                             models_asset_credit, 
                             models_asset_employement) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% pull(model) %>% 
  set_names(glue("({1:16})")) %>%  
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:9) %>%
  tab_spanner(label = 'Depreciation', columns = 10:16)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy_het.tex")
###########



########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security (With Heterogeneity) INFLATION
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla")

mean_dep_asset <- data_all %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("infla"), 
                        change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("infla"), 
                               change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_fixed + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_clean")) 

asset_literate_formulas = tibble(scenario = c("infla"), 
                                 change = c("infla"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + infla_change:literate_clean")) 

asset_marital_formulas = tibble(scenario = c("infla"), 
                                change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ marital_clean + {change}_change + h_fin_literacy_asset_prior + infla_change:marital_clean")) 

asset_education_formulas = tibble(scenario = c("infla"), 
                                  change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ education_clean + {change}_change + h_fin_literacy_asset_prior + infla_change:education_clean")) 

asset_credit_formulas = tibble(scenario = c("infla"), 
                               change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_credit_card + {change}_change + h_fin_literacy_asset_prior + infla_change:has_credit_card")) 

asset_bank_formulas = tibble(scenario = c("infla"), 
                             change = c("infla"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_bank_account + {change}_change + h_fin_literacy_asset_prior + infla_change:has_bank_account")) 

asset_employement_formulas = tibble(scenario = c("infla"), 
                                    change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ employment_clean + {change}_change  + h_fin_literacy_asset_prior + infla_change:employment_clean")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_marital <- asset_marital_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_education <- asset_education_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_bank <- asset_bank_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_credit <- asset_credit_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_employement <- asset_employement_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))





models_asset_all = bind_rows(models_asset, 
                             models_asset_income, 
                             models_asset_literate, 
                             models_asset_marital, 
                             models_asset_education,
                             models_asset_bank, 
                             models_asset_credit, 
                             models_asset_employement) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% pull(model) %>% set_names(glue("({1:8})")) %>%  
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:9) %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy_infla_het_clean_other.tex")
###########

########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security (With Heterogeneity) DEPRECIATION
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla")

mean_dep_asset <- data_all %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("forex"), 
                        change = c("depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("forex"), 
                               change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_fixed + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_clean")) 

asset_literate_formulas = tibble(scenario = c("forex"), 
                                 change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + depr_change:literate")) 

asset_marital_formulas = tibble(scenario = c("forex"), 
                                change = c("depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ marital_clean + {change}_change + h_fin_literacy_asset_prior + depr_change:marital_clean")) 

asset_education_formulas = tibble(scenario = c("forex"), 
                                  change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ education_clean + {change}_change + h_fin_literacy_asset_prior + depr_change:education_clean")) 

asset_credit_formulas = tibble(scenario = c("forex"), 
                               change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_credit_card + {change}_change + h_fin_literacy_asset_prior + depr_change:has_credit_card")) 

asset_bank_formulas = tibble(scenario = c("forex"), 
                             change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_bank_account + {change}_change + h_fin_literacy_asset_prior + depr_change:has_bank_account")) 

asset_employement_formulas = tibble(scenario = c("forex"), 
                                    change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ employment_clean + {change}_change  + h_fin_literacy_asset_prior + depr_change:employment_clean")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_marital <- asset_marital_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_education <- asset_education_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_bank <- asset_bank_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_credit <- asset_credit_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_asset_employement <- asset_employement_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))


models_asset_all = bind_rows(models_asset,
                             models_asset_literate,
                             models_asset_income,
                             models_asset_marital, 
                             models_asset_education,
                             models_asset_bank, 
                             models_asset_credit, 
                             models_asset_employement) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% pull(model) %>% set_names(glue("({1:8})")) %>%  
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Depreciation', columns = 2:9) %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy_depr_het_clean_other.tex")
###########






####################
### TABLE: Effects of Hypothetical Shocks on Planned Expenditures,  By fixed income treatment
####################

models_infla_income <- tibble(outcome_var = outcomes_selected, 
                                formula = glue("{outcome_var}_infla ~ infla_change + income_fixed + {outcome_var}_prior +  int_infla_income + {outcome_var}_prior:income_fixed")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))

models_forex_income <- tibble(outcome_var = outcomes_selected, 
                                formula = glue("{outcome_var}_forex ~ depr_change  + income_fixed + {outcome_var}_prior + int_depr_income + {outcome_var}_prior:income_fixed")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_all, fixed_effects = ~wave)))


mean_dep_all <- data_all %>% 
  select(one_of(c(str_c(outcomes_selected, "_infla"),str_c(outcomes_selected, "_forex")))) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
  pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
  pivot_wider() %>% 
  mutate(term = fct_recode(term, !!!stat_names))

models_income = bind_rows(models_infla_income, models_forex_income)

coef_labels_withinc_joint = c("infla_change" = "$ \\Delta Belief $", 
                              "depr_change" = "$ \\Delta Belief $",
                              "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                              "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$"
                              ) 
model_labels_withinc =c(outcomes_names_selected, outcomes_names_selected)


table_income <- models_income %>% 
  pull(model) %>%
  set_names(glue("Model {1:length(.)}")) %>%
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               coef_map = coef_labels_withinc_joint, 
               gof_map = gm,
               add_rows = mean_dep_all,
               escape = FALSE,
               estimate = "{estimate}{stars}",
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:5) %>%
  tab_spanner(label = 'Depreciation', columns = 6:9)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_table_labels(labels = model_labels_withinc) %>%
  custom_latex_changes()
table_income %>%
  write_lines("output/tables/hypotheticals_by_fixed_income.tex")
####################



########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security (With Heterogeneity) INFLATION
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla")

mean_dep_asset <- data_noinc %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("infla"), 
                        change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("infla"), 
                               change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_fixed + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_clean")) 

asset_literate_formulas = tibble(scenario = c("infla"), 
                                 change = c("infla"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:literate")) 

asset_marital_formulas = tibble(scenario = c("infla"), 
                                change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ marital_clean + {change}_change + h_fin_literacy_asset_prior + infla_change:marital_clean")) 

asset_education_formulas = tibble(scenario = c("infla"), 
                                  change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ education_clean + {change}_change + h_fin_literacy_asset_prior + infla_change:education_clean")) 

asset_credit_formulas = tibble(scenario = c("infla"), 
                               change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_credit_card + {change}_change + h_fin_literacy_asset_prior + infla_change:has_credit_card")) 

asset_bank_formulas = tibble(scenario = c("infla"), 
                             change = c("infla"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_bank_account + {change}_change + h_fin_literacy_asset_prior + infla_change:has_bank_account")) 

asset_employement_formulas = tibble(scenario = c("infla"), 
                                    change = c("infla")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ employment_clean + {change}_change  + h_fin_literacy_asset_prior + infla_change:employment_clean")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_marital <- asset_marital_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_education <- asset_education_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_bank <- asset_bank_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_credit <- asset_credit_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_employement <- asset_employement_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))





models_asset_all = bind_rows(models_asset, 
                             models_asset_income, 
                             models_asset_literate, 
                             models_asset_marital, 
                             models_asset_education,
                             models_asset_bank, 
                             models_asset_credit, 
                             models_asset_employement) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% pull(model) %>% set_names(glue("({1:8})")) %>%  
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:9) %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy_infla_het_clean_noinc.tex")
###########

########
#### TABLE:  Effects of Hypothetical Shocks on Demand for Inflation Indexed Security (With Heterogeneity) DEPRECIATION
########

coef_labels_asset_joint = c("infla_change" = "$\\Delta Belief $", 
                            "depr_change" = "$\\Delta Belief $",
                            "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                            "int_literate_infla" = "$\\Delta Belief  \\cdot L_i $",
                            "int_literate_depr" =  "$\\Delta Belief  \\cdot L_i $") 

create_sumstat = function(data){
  data %>% pivot_longer(everything()) %>% 
    group_by(name) %>% 
    summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
    pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
    pivot_wider() %>% 
    mutate(term = fct_recode(term, !!!stat_names))
}

asset_vars = c("h_fin_literacy_asset_infla")

mean_dep_asset <- data_noinc %>% 
  filter(!is.na(h_fin_literacy_asset_infla)) %>% 
  select(one_of(c(asset_vars, "income_fixed", "literate"))) %>%
  nest(data = everything()) %>%
  mutate(data_noinc = map(data, ~ .x %>% filter(income_fixed == 0) %>% 
                            select(one_of(asset_vars)) %>% 
                            create_sumstat
                          %>% 
                            rename_with(.fn = ~ if_else(.x != "term", str_c(.x, "_noinc"), .x))), 
         # data_inc = map(data, ~ .x %>% 
         #                  select(one_of(asset_vars)) %>% 
         #                  create_sumstat%>% 
         #                  select(-term) %>%
         #                  rename_with(~str_c(.x, "_inc"))
         #                ), 
         data_lit = map(data, ~ .x %>% filter(literate == 0) %>% 
                          select(one_of(asset_vars)) %>% 
                          create_sumstat%>% 
                          select(-term) %>%
                          rename_with(~str_c(.x, "_lit"))
         )
  ) %>%
  select(-data) %>% transpose() %>% bind_cols() 


asset_formulas = tibble(scenario = c("forex"), 
                        change = c("depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ {change}_change + h_fin_literacy_asset_prior"))

asset_income_formulas = tibble(scenario = c("forex"), 
                               change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ income_fixed + {change}_change + int_{change}_income + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:income_clean")) 

asset_literate_formulas = tibble(scenario = c("forex"), 
                                 change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ literate + {change}_change + int_literate_{change} + h_fin_literacy_asset_prior + h_fin_literacy_asset_prior:literate")) 

asset_marital_formulas = tibble(scenario = c("forex"), 
                                change = c("depr")) %>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ marital_clean + {change}_change + h_fin_literacy_asset_prior + depr_change:marital_clean")) 

asset_education_formulas = tibble(scenario = c("forex"), 
                                  change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ education_clean + {change}_change + h_fin_literacy_asset_prior + depr_change:education_clean")) 

asset_credit_formulas = tibble(scenario = c("forex"), 
                               change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_credit_card + {change}_change + h_fin_literacy_asset_prior + depr_change:has_credit_card")) 

asset_bank_formulas = tibble(scenario = c("forex"), 
                             change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ has_bank_account + {change}_change + h_fin_literacy_asset_prior + depr_change:has_bank_account")) 

asset_employement_formulas = tibble(scenario = c("forex"), 
                                    change = c("depr"))%>%
  mutate(formula = glue("h_fin_literacy_asset_{scenario} ~ employment_clean + {change}_change  + h_fin_literacy_asset_prior + depr_change:employment_clean")) 

models_asset <- asset_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_income <- asset_income_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_literate <- asset_literate_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_marital <- asset_marital_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_education <- asset_education_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_bank <- asset_bank_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_credit <- asset_credit_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_asset_employement <- asset_employement_formulas %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))


models_asset_all = bind_rows(models_asset,
                             models_asset_literate,
                             models_asset_income,
                             models_asset_marital, 
                             models_asset_education,
                             models_asset_bank, 
                             models_asset_credit, 
                             models_asset_employement) %>%
  arrange(desc(scenario)) 

table_asset = 
  models_asset_all %>% pull(model) %>% set_names(glue("({1:8})")) %>%  
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Depreciation', columns = 2:9) %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_depvar("Chosen inflation protected asset", num_col = 4) %>%
  custom_latex_changes()
table_asset %>%
  write_lines("output/tables/hypotheticals_asset_literacy_depr_het_clean_noinc.tex")
###########






####################
### TABLE: Effects of Hypothetical Shocks on Planned Expenditures,  By fixed income treatment
####################

models_infla_income <- tibble(outcome_var = outcomes_selected, 
                              formula = glue("{outcome_var}_infla ~ infla_change + income_fixed + {outcome_var}_prior +  int_infla_income + {outcome_var}_prior:income_fixed")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))

models_forex_income <- tibble(outcome_var = outcomes_selected, 
                              formula = glue("{outcome_var}_forex ~ depr_change  + income_fixed + {outcome_var}_prior + int_depr_income + {outcome_var}_prior:income_fixed")) %>%
  mutate(model = map(formula, ~lm_robust(as.formula(.x), data = data_noinc, fixed_effects = ~wave)))


mean_dep_all <- data_noinc %>% 
  select(one_of(c(str_c(outcomes_selected, "_infla"),str_c(outcomes_selected, "_forex")))) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise( across(.fns = list(mean = mean, sd = sd)))  %>% 
  pivot_longer(cols = starts_with("value_"), names_to = "term", values_to = "value") %>%
  pivot_wider() %>% 
  mutate(term = fct_recode(term, !!!stat_names))

models_income = bind_rows(models_infla_income, models_forex_income)

coef_labels_withinc_joint = c("infla_change" = "$ \\Delta Belief $", 
                              "depr_change" = "$ \\Delta Belief $",
                              "int_infla_income" = "$ \\Delta Belief \\cdot T^{FI}_i$",
                              "int_depr_income" = "$ \\Delta Belief \\cdot T^{FI}_i$"
) 
model_labels_withinc =c(outcomes_names_selected, outcomes_names_selected)


table_income <- models_income %>% 
  pull(model) %>%
  set_names(glue("Model {1:length(.)}")) %>%
  modelsummary(stars = c('*' = .1, '**' = .05, '***' = .01), 
               coef_map = coef_labels_withinc_joint, 
               gof_map = gm,
               add_rows = mean_dep_all,
               escape = FALSE,
               estimate = "{estimate}{stars}",
               output = "gt") %>% 
  tab_spanner(label = 'Scenario:', columns = 1) %>%
  tab_spanner(label = 'Inflation', columns = 2:5) %>%
  tab_spanner(label = 'Depreciation', columns = 6:9)  %>%
  as_latex() %>% as.character() %>% remove_escape_latex() %>%
  add_table_labels(labels = model_labels_withinc) %>%
  custom_latex_changes()
table_income %>%
  write_lines("output/tables/hypotheticals_by_fixed_income.tex")
####################

