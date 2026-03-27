# R script for: producing the tables and graphs presented in the supplemental information
# This version dated to: 2025-10-08

# NOTES:
## The code sections are presented in the same way the corresponding outputs are shown in text;
## Set your current working directory to where this file is before running.

rm(list = ls())

# CONFIG: Paths/Directories ----

dir_input <- 'OUT'
if (!dir.exists(dir_input)) stop('Input folder not found!')

dir_out_figs <- 'FIGURES (SI)'
if (!dir.exists(dir_out_figs)) dir.create(dir_out_figs)

dir_out_tabs <- 'TABLES (SI)'
if (!dir.exists(dir_out_tabs)) dir.create(dir_out_tabs)

clean_existing_figures <- TRUE # Set to FALSE to keep existing figure outputs
clean_existing_tables <- TRUE # Set to FALSE to keep existing table outputs

if (clean_existing_figures) unlink(dir_out_figs)
if (clean_existing_tables) unlink(dir_out_tabs)

# CONFIG: Packages ----

{
  suppressPackageStartupMessages(if (!require('tidyverse')) install.packages('tidyverse'))
  suppressPackageStartupMessages(if (!require('metafor')) install.packages('metafor'))
  suppressPackageStartupMessages(if (!require('ggthemes')) install.packages('ggthemes'))
  suppressPackageStartupMessages(if (!require('patchwork')) install.packages('patchwork'))
  suppressPackageStartupMessages(if (!require('ggpubr')) install.packages('ggpubr'))
  suppressPackageStartupMessages(if (!require('ggh4x')) install.packages('ggh4x'))
  suppressPackageStartupMessages(if (!require('sf')) install.packages('sf'))
  suppressPackageStartupMessages(if (!require('rnaturalearth')) install.packages('rnaturalearth'))
  suppressPackageStartupMessages(if (!require('readxl')) install.packages('readxl'))
  suppressPackageStartupMessages(if (!require('legendry')) install.packages('legendry'))
  suppressPackageStartupMessages(if (!require('texreg')) install.packages('texreg'))
  suppressPackageStartupMessages(if (!require('kableExtra')) install.packages('kableExtra'))
}

# PRELIM: Helpers ----

# Helpers for making plots

prettify_plot_labels <- function(data){
  
  # drop levels with <5 studies
  data <- data |>
    dplyr::filter(count_of_studies >= 5)
  
  # add attribute var
  data$attribute = stringr::str_to_sentence(gsub("_", " ",gsub("attr_", "", data$feature)))
  
  # edit labels
  data = data |> 
    dplyr::mutate(category = dplyr::case_when(
      grepl("External", attribute) ~ "Reasons to move",
      grepl("Age", attribute) ~ "Socio-demographics",
      grepl("Marital", attribute) ~ "Socio-demographics",
      grepl("Gender",attribute) ~ "Socio-demographics",
      grepl("Children", attribute) ~ "Socio-demographics",
      grepl("Benefits", attribute) ~ "Economic self-reliance",
      grepl("Needs",attribute) ~ "Economic self-reliance",
      grepl("Occupation", attribute) ~ "Economic self-reliance",
      grepl("Hunger", attribute) ~ "Economic self-reliance",
      grepl("Residence", attribute) ~ "Economic self-reliance",
      grepl("Unemployed", attribute) ~ "Economic self-reliance",
      grepl("Skills training", attribute) ~ "Economic self-reliance",
      grepl("Employment plans", attribute) ~ "Economic self-reliance",
      grepl("Education", attribute) ~ "Education/language",
      grepl("Language", attribute) ~ "Education/language",
      grepl("Integration", attribute) ~ "Education/language",
      grepl("Religion", attribute) ~ "Religious/ethnic identity",
      grepl("Skin", attribute) ~ "Religious/ethnic identity",
      grepl("outgroup", attribute) ~ "Religious/ethnic identity",
      grepl("Country", attribute) ~ "Origin country/region",
      grepl("Continent", attribute) ~ "Origin country/region",
      grepl("Violence", attribute) ~ "Crime/violence",
      grepl("Previous visits", attribute) ~ "Reasons to move",
      grepl("Reason", attribute) ~ "Reasons to move",
      grepl("Length", attribute) ~ "Reasons to move",
      grepl("Relatives", attribute) ~ "Reasons to move",
      grepl("testimony", attribute) ~ "Crime/violence",
      grepl("Criminal", attribute) ~ "Crime/violence",
      grepl("Prevalence", attribute) ~ "Reasons to move",
      grepl("Politics", attribute) ~ "Political participation",
      grepl("Voting", attribute) ~ "Political participation",
      grepl("Income", attribute) ~ "Economic self-reliance",
      grepl("Competition", attribute) ~ "Economic self-reliance",
      grepl("Educ match", attribute) ~ "Education/language",
      grepl("Muslim", attribute) ~ "Religious/ethnic identity",
      grepl("Region match", attribute) ~ "Origin country/region",
      grepl("North", attribute) ~ "Origin country/region",
      grepl("distance",attribute) ~ "Religious/ethnic identity",
      grepl("Race", attribute) ~ "Religious/ethnic identity",
      grepl("Geo", attribute) ~ "Origin country/region",
      grepl("Common", attribute) ~ "Education/language",
      grepl("Colonial",attribute) ~ "Origin country/region",
      TRUE ~ NA_character_
    )
    ) 
  
  data = data |> 
    dplyr::mutate(
      level_unchanged = level,
      level = dplyr::case_when(
        nchar(as.character(level))>20 & grepl("Participation",level) ~ gsub("in ", "in \n", level),
        nchar(as.character(level))>20 & grepl("No conflict",level) ~ gsub("No conflict ", "No conflict \n", level),
        nchar(as.character(level))>20 & grepl("testimony",level) ~ gsub("testimony inconsistencies", "inconsistencies", level),
        grepl("aware of culture", level) ~ gsub("aware of culture and traditions", "aware of culture",level),
        level == "Has no connection to the labor market" ~ "No labor market connection",
        level == "Extensive training/experience (more than 3y)" ~ "Extensive training/experience",
        level == "No/limited training/experience (less than 3y)" ~ "Limited training//experience",
        TRUE ~ level)) |> 
    dplyr::mutate(model_full = ifelse(model=="FE","Fixed-Effect","Random-Effects")) |>
    dplyr::mutate(significant_95pct = ifelse((effect_cl < 0 & effect_cu < 0) | (effect_cl > 0 & effect_cu > 0), 1, 0))|>
    dplyr::arrange(category, attribute, effect_estimate) |> 
    dplyr::mutate(order = 1:nrow(data)) |> 
    dplyr::mutate(level = gsub("&","and",level))
  
}

make_effect_df = function(eff_meta = meta_study_std, eff_study = study_effects_std, 
                          eff_attr, lab_meta = c("MetaReg (Fixed-Effect)", "MetaReg (Random-Effects)")) {
  rbind.data.frame(
    eff_meta |>
      dplyr::filter(feature==eff_attr) |>
      dplyr::select(
        feature, level, study=model, # outcome_type=outcome,
        effect_estimate, effect_cl, effect_cu) |>
      dplyr::mutate(study = ifelse(study=="FE", "MetaReg (Fixed-Effect)", "MetaReg (Random-Effects)")),
    eff_study |>
      dplyr::filter(features==eff_attr) |>
      dplyr::select(
        feature=features, level, study, # outcome_type=outcome,
        effect_estimate=marginal_mean,  
        effect_cl = conf_low, effect_cu = conf_high
      ) |>
      dplyr::arrange(study)
  ) |>
    dplyr::mutate(
      study_type = ifelse(study %in% lab_meta, "Meta", "Individual Studies") |>
        factor(levels = c("Individual Studies","Meta"))
    ) |>
    dplyr::mutate(
      significant_95pct = ifelse((effect_cl<0 & effect_cu<0) | (effect_cl>0 & effect_cu>0), 1, 0)
    )
}

set_level_order = function(eff_df, level_var = level, level_ref, 
                           lab_meta = c("MetaReg (Fixed-Effect)", "MetaReg (Random-Effects)")) {
  level_var = enquo(level_var)
  levels_new = eff_df |>
    # dplyr::filter(outcome_type=="Standardized") |>
    dplyr::filter(!!level_var==level_ref) |>
    dplyr::arrange(effect_estimate) |>
    dplyr::pull(study) |>
    setdiff(lab_meta) |>
    c(lab_meta)
  eff_df$study = factor(eff_df$study, levels = levels_new)
  return(eff_df)
}

plot_effect_df = function(eff_df, txt_title, level_var=level) {
  level_var = enquo(level_var)
  ggplot(eff_df, aes(color = !!level_var, shape = !!level_var, 
                     alpha = factor(significant_95pct), size = factor(study_type))) +
    geom_pointrange(
      aes(x = effect_estimate, y = study, xmin = effect_cl, xmax = effect_cu),
      position = position_dodge(0.7)) +
    geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
    facet_grid(rows = vars(study_type), scales = "free", space = "free_y", switch="y") +
    scale_y_discrete(position = "right") +
    ggpubr::theme_pubclean(base_size = 14) +
    scale_color_grey() +
    scale_alpha_manual(breaks = c(1,0), values = c(1,0.5), guide = "none") +
    scale_size_manual(breaks = c("Single-Study","Meta"), values = c(.5,.7), guide = "none") +
    labs(x = NULL, y = NULL, 
         title = txt_title,
         color = NULL, shape = NULL) +
    theme(
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 15),
      legend.position = "bottom"
    )
}

display_funnel_plots <- function(df = metaeffs_std_ls_mods$`RE`, feature, title, axis_lab_fontsize = 1.3) {
  metafor::funnel(df[[feature]], main = title, cex.lab=axis_lab_fontsize)
}

# Helpers for making tables

make_regression_tables <- function(
    ls_mods = metaeffs_std_ls_mods, name_mod, 
    ls_coef_maps, 
    vec_gof_map = metareg_gof_map, 
    table_figure_match, table_label_key
) {
  
  # extract fitted meta-models
  mod_fe = ls_mods[["FE"]][[name_mod]]
  mod_re = ls_mods[["RE"]][[name_mod]]
  mod_re[["tau2"]] <- mod_re[["sigma2"]]*1e4 # swap in correct tau^2
  mod_ls = list(mod_fe,mod_re)
  
  # make regression table (LaTeX format)
  metareg_table = texreg::texreg(
    mod_ls,
    custom.model.names = c("FE", "RE"),
    custom.coef.map = ls_coef_maps,
    custom.gof.names = vec_gof_map,
    digits = 3,
    # include.aic = FALSE, include.aicc = FALSE, include.deviance = FALSE,
    custom.gof.rows = list("No. of studies" = rep(mod_re[['s.nlevels']], 2)),
    reorder.gof = c(13, 1, 2, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12),
    custom.note = "Note: %stars.",
    caption = sprintf("Results in Figure \\ref{fig:%s} in Tabular Format", table_figure_match),
    caption.above = TRUE,
    label =  sprintf("tab:%s", table_label_key),
    booktab = TRUE, dcolumn = TRUE, center = TRUE,
    use.packages = FALSE
  )
  
  # return table (LaTeX codes)
  return(metareg_table)
}

normalize_tau2 <- function(fit_re = metaeffs_std_ls_mods$RE, reg_attr) {
  study_dat = fit_re[[reg_attr]]
  study_vars = study_dat[["data"]][["sampling_var"]] # vector of study-level effect variances (sampling)
  study_varmean = mean(study_vars,na.rm = TRUE) # mean study-level effect variance
  meta_var = study_dat[["sigma2"]] # tau^2 estimate (note the misnomer label)
  pct_metavar = meta_var/study_varmean
  return(c(
    "feature" = reg_attr, 
    "tau2" = meta_var,
    "pct_tau2" = pct_metavar)
  ) # note this forces tau2's to be string-valued
}

create_table_panel <- function(df_sub) {
  df_sub |>
    dplyr::mutate(effect_ci = paste0(round(effect_estimate, 3), " [", round(effect_cl, 3), ",", round(effect_cu, 3), "]")) |>
    dplyr::select(category_relabelled, attribute, level, effect_ci, count_of_studies)
}

reverse_row_order <- function(df) {
  df[rev(1:nrow(df)),]
}

# PRELIM: Ctrl Parameters ----

level_meta = c("MetaReg (Fixed-Effect)","MetaReg (Random-Effects)")

metareg_gof_map = c(
  "tau.squared" = "$\\tau^2$ (10^{-4})",
  "cochran.qe" = "Cochran's $Q_E$",
  "p.value.cochran.qe" = "p-value for $Q_E$",
  "cochran.qm" = "Cochran's $Q_M$",
  "p.value.cochran.qm" = "p-value for $Q_M$",
  "DF Resid." = "Residual DF",
  "Log Likelihood" ="Log Likelihood",
  "Deviance" = "Deviance",
  "AIC" = "AIC", 
  "BIC"="BIC", 
  "AICc" = "AICc",    
  "nobs" = "No. of study effects"
)

# INPUT: Data & Estimates ----

# Study-level inputs
study_data <- read.csv('DAT_cleaned_study_data.csv')
study_effects_std <- read.csv(file.path(dir_input, 'study_effects_std.csv'))

# Meta inputs
meta_effects_std <- read.csv(file.path(dir_input, 'meta_effects_std.csv'))
metaeffs_std_ls_mods <- readr::read_rds(file.path(dir_input, 'meta_estimates_std.RDS'))

# Conditioned inputs
reason = readr::read_csv(file.path(dir_input, 'meta_effects_std_reason.csv'))
reason_meta <- read.csv(file.path(dir_input, 'study_effects_std_reason.csv'))

region = readr::read_csv(file.path(dir_input, 'meta_effects_std_region.csv'))

resp.relig <- read.csv(file.path(dir_input, 'study_effects_std_resprelig.csv'))
resp.relig_meta <- read.csv(file.path(dir_input, 'meta_effects_std_resprelig.csv'))

study_muslimt_X_muslimr <- read.csv(file.path(dir_input, 'study_effects_std_muslimt_X_muslimr.csv'), row.names = 1)
meta_muslimt_X_muslimr <- read.csv(file.path(dir_input, 'meta_effects_std_muslimt_X_muslimr.csv'), row.names = 1)

study_religt_X_occupt <- read.csv(file.path(dir_input, 'study_effects_std_religt_X_occupt.csv'), row.names = 1)
meta_religt_X_occupt <- read.csv(file.path(dir_input, 'meta_effects_std_religt_X_occupt.csv'), row.names = 1)

outcome = readr::read_csv(file.path(dir_input, 'meta_effects_std_outcome.csv'))

mm_experiment <- read.csv(file.path(dir_input, 'study_effects_std_exptype.csv'))
metaeffs_experiment <- read.csv(file.path(dir_input, 'meta_effects_std_exptype.csv'))

# Study-level & Meta inputs
study_info = study_data |>
  dplyr::select(study, year, title:origin_country,country_religion, resp_gender) |>
  dplyr::distinct()
study_tab <- study_effects_std |>
  dplyr::left_join(study_info |> dplyr::select(study, outcome, country_type) |>
                     dplyr::mutate(country_type = if_else(study == "Wimmer et al. (2024)","Both",country_type)) |> 
                     dplyr::distinct()) |> 
  dplyr::distinct() |>
  dplyr::rename(feature=features) |>
  dplyr::group_by(feature, level) |> 
  dplyr::summarise(
    count_of_studies = length(unique(study)),
    outcome_type_count = length(unique(outcome)),
    developing_studies = sum(country_type %in% c("Developing/Low income","Both")),
    developed_studies = sum(country_type %in%c("Developed/High income","Both"))
  ) |>
  dplyr::ungroup()

meta_study_std <- dplyr::left_join(meta_effects_std, study_tab, by = c("feature","level"))

# CODES FOR SECTION A ----

## TABLE A1 ----

tab3 = study_data |> 
  dplyr::group_by(study, title, journal, experiment, migrant_type, n) |> 
  dplyr::summarise(`Region of study` = paste(unique(region), collapse=", ")) |> 
  dplyr::rename(`Authors (year)` = study, Title = title, Journal = journal, 
         `Experiment type` = experiment, `Migrant population` = migrant_type,
         N = n)

kableExtra::kable(tab3, "latex", longtable = T, booktabs = T) |>
  writeLines(file.path(dir_out_tabs, 'full_study_list.tex'))

## TABLE A2 ----

# No analysis involved; hand-typed.

## FIGURE A1 ----

features <- study_data |> 
  dplyr::select(study, starts_with("attr_")) |> 
  dplyr::mutate(across(starts_with("attr_"), \(x) if_else(is.na(x)==T, 0, 1))) |> 
  tidyr::pivot_longer(cols=starts_with("attr_")) |> 
  dplyr::group_by(study,name) |> 
  dplyr::summarise(value = mean(value)) |>
  dplyr::ungroup() |> 
  dplyr::filter(value > 0) |> 
  dplyr::group_by(name) |> 
  dplyr::summarise(num_studies = length(unique(study))) |> 
  dplyr::ungroup() |>
  dplyr::mutate(name = stringr::str_to_sentence(gsub("_", " ",gsub("attr_", "", name))))

mapping <- study_data |> 
  dplyr::group_by(study) |> 
  dplyr::summarise(across(starts_with("attr_"), \(x) any(is.na(x)==F))) |> 
  tidyr::pivot_longer(cols = contains("attr_")) |> 
  dplyr::mutate(
    label = str_to_sentence(gsub("_"," ",gsub("attr_","",name))),
    value2 = dplyr::case_when(
      value == FALSE ~ "Not present",
      value == TRUE & name %in% c(
        "attr_income_match","attr_occupation",
        "attr_educ_match","attr_language",
        "attr_religion","attr_north_south",
        "attr_region_match","attr_gender",
        "attr_reason") ~ "Meta-analyzed in main text",
      value == TRUE &  name %in%c("attr_education","attr_income",
                                  "attr_skills_training","attr_country",
                                  "attr_continent") ~ "Used to construct meta-analyzed attribute",
      value == TRUE  ~ "Not analyzed in main text"
    )) |> 
  dplyr::group_by(label ) |> 
  dplyr::mutate(count = sum(value)) |> 
  dplyr::ungroup()

mapping |> 
  ggplot(aes(reorder(label , -count), study, 
             fill = factor(value2,
                           c("Meta-analyzed in main text",
                             "Used to construct meta-analyzed attribute",
                             "Not analyzed in main text",
                             "Not present"))))+
  scale_x_discrete(position = "bottom") +
  scale_fill_manual(values = c("gray10","gray40","gray75","gray95"))+
  geom_tile()+
  theme_minimal(base_size = 14) + 
  labs(x=NULL,y = NULL, fill = NULL)+
  guides(fill=guide_legend(nrow=2,byrow=F))+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1,  
                               size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom",
    panel.border = element_rect(colour = "gray40", fill=NA, size=1)
  )

ggsave(file.path(dir_out_figs, 'mapping_overview.png'), width=11, height=16, bg = "white")

# END SECTION A


# CODES FOR SECTION B ----

## TABLE A3 ----

rmatab_educ_match = make_regression_tables(
  name_mod = "attr_educ_match", 
  ls_coef_maps = list("factor(level)Education match" = "Education match", "factor(level)Education mismatch" = "Education mismatch"),
  table_figure_match = "egocentric", table_label_key = "egocentric"
)
pcttau2_educ_match = normalize_tau2(reg_attr = "attr_educ_match")

rmatab_income_match = make_regression_tables(
  name_mod = "attr_income_match",
  ls_coef_maps = list('factor(level)Skills/income match' = 'Skills/income match', 'factor(level)Skills/income mismatch' = 'Skills/income mismatch'),
  table_figure_match = 'egocentric-income', table_label_key = 'egocentric-income'
)
pcttau2_income_match = normalize_tau2(reg_attr = "attr_income_match")

writeLines(rmatab_educ_match, file.path(dir_out_tabs, 'attr_educ_match.tex'))
writeLines(rmatab_income_match, file.path(dir_out_tabs, 'attr_income_match.tex'))
# The two tables are then manually stacked together into a two-panel table in tex file

## TABLE A4 ----

rmatab_occupation = make_regression_tables(
  name_mod = "attr_occupation",
  ls_coef_maps = list(
    "factor(level)Professional occupation" = "Professional occupation",
    "factor(level)Worker/Farmer" = "Worker/Famer",
    "factor(level)Unemployed" = "Unemployed",
    "factor(level)Student/Pensioner" = "Student/Pensioner"),
  table_figure_match = 'sociotropic_occ', table_label_key = 'sociotropic_occ'
)
pcttau2_occupation = normalize_tau2(reg_attr = 'attr_occupation')

rmatab_language = make_regression_tables(
  name_mod = "attr_language",
  ls_coef_maps = list(
    "factor(level)Broken language" = "Broken language",
    "factor(level)Fluent language" = "Fluent language",
    "factor(level)Unable to speak language" = "Unable to speak language"),
  table_figure_match = 'sociotropic_lang', table_label_key = 'sociotropic_lang'
)
pcttau2_language = normalize_tau2(reg_attr = 'attr_language')

writeLines(rmatab_occupation, file.path(dir_out_tabs, 'attr_occupation.tex'))
writeLines(rmatab_language, file.path(dir_out_tabs, 'attr_language.tex'))
# The two tables are then manually stacked together into a two-panel table in tex file

## TABLE A5 ----

rmatab_north_south = make_regression_tables(
  name_mod = 'attr_north_south',
  ls_coef_maps = list(
    "factor(level)Developed/High income" = "Developed/High income",
    "factor(level)Developing/Low income" = "Developing/Low income"),
  table_figure_match = 'cultural_origin', table_label_key = 'cultural_origin'
)
pcttau2_north_south = normalize_tau2(reg_attr = 'attr_north_south')

rmatab_region_match = make_regression_tables(
  name_mod = 'attr_region_match',
  ls_coef_maps = list(
    "factor(level)Different world region" = "Different world region",
    "factor(level)Same world region" = "Same world region"),
  table_figure_match = 'culture-mismatch', table_label_key = 'culture-mismatch'
)
pcttau2_region_match = normalize_tau2(reg_attr = 'attr_region_match')

rmatab_religion = make_regression_tables(
  name_mod = 'attr_religion',
  ls_coef_maps = list(
    "factor(level)No religion" = "No religion",
    "factor(level)Christian" = "Christian",
    "factor(level)Muslim" = "Muslim",
    "factor(level)Jew " = "Jew",
    "factor(level)Hindu" = "Hindu"
  ),
  table_figure_match = 'cultural_religion', table_label_key = 'cultural_religion'
)
pcttau2_religion = normalize_tau2(reg_attr = 'attr_religion')

writeLines(rmatab_north_south, file.path(dir_out_tabs, 'attr_north_south.tex'))
writeLines(rmatab_region_match, file.path(dir_out_tabs, 'attr_region_match.tex'))
writeLines(rmatab_religion, file.path(dir_out_tabs, 'attr_religion.tex'))
# The three tables are then manually stacked together into a two-panel table in tex file

## TABLE A6 ----

rmatab_gender = make_regression_tables(
  name_mod = 'attr_gender',
  ls_coef_maps = list(
    "factor(level)Female" = "Female",
    "factor(level)Male" = "Male",
    "factor(level)Male and female" = "Male and female"
  ),
  table_figure_match = 'hum_gender', table_label_key = 'hum_gender'
)
pcttau2_gender = normalize_tau2(reg_attr = 'attr_gender')

rmatab_reason = make_regression_tables(
  name_mod = 'attr_reason',
  ls_coef_maps = list(
    "factor(level)Economic migrant" = "Economic migrant",
    "factor(level)Family reunification" = "Family reunification",
    "factor(level)Forced migrant" = "Forced migrant",
    "factor(level)Climate migrant" = "Climate migrant"),
  table_figure_match = 'hum_reason', table_label_key = 'hum_reason'
)
pcttau2_reason = normalize_tau2(reg_attr = 'attr_reason')

writeLines(rmatab_gender, file.path(dir_out_tabs, 'attr_gender.tex'))
writeLines(rmatab_reason, file.path(dir_out_tabs, 'attr_reason.tex'))
# The two tables are then manually stacked together into a two-panel table in tex file

## TABLE A7 ----

{
  reason = dplyr::left_join(reason, study_tab, by = c("level"))
  reason = prettify_plot_labels(reason)
  reason_study <- reason_meta |>
    dplyr::left_join(
      study_info |> 
        dplyr::select(study, outcome, country_type)
    ) |>
    dplyr::distinct() |> 
    dplyr::rename(feature=features) |>
    dplyr::group_by(feature, level) |> 
    dplyr::summarise(
      reason_econ = sum(migrant_type== "Economic migrant"),
      reason_forced = sum(migrant_type== "Forced migrant")
    ) |>
    dplyr::ungroup()
  reason = dplyr::left_join(reason, reason_study, by = c("feature","level"))
  reason_plotdf <- reason |>
    dplyr::filter(reason_econ >=5 & reason_forced >= 5) |>
    dplyr::filter(feature %in% c(
      "attr_educ_match", "attr_education",
      "attr_income_match","attr_income", "attr_skills_training",
      "attr_occupation",
      "attr_language",
      "attr_north_south",
      "attr_region_match", "attr_region",
      "attr_religion",
      "attr_gender",
      "attr_reason"
    )) |> 
    dplyr::mutate(
      category_relabelled = dplyr::case_when(
        category %in% c("Economic self-reliance","Education/language") ~ "Sociotropic concerns",
        category %in% c("Religious/ethnic identity", "Origin country/region") ~ "Cultural concerns",
        category == "Socio-demographics" ~ "Humanitarian concerns"
      ) |>
        factor(levels = c("Sociotropic concerns", "Cultural concerns", "Humanitarian concerns"))
    ) 
}

contrast_migranttype = reason_plotdf |>
  dplyr::select(model_full, category_relabelled, attribute, level, effect_estimate:effect_cu, migrant_type, count_of_studies) 
contrast_migranttype_econ_fe <- contrast_migranttype |>
  dplyr::filter(migrant_type=="Economic migrant", model_full == "Fixed-Effect") |>
  create_table_panel() |> reverse_row_order()
contrast_migranttype_econ_re <- contrast_migranttype |>
  dplyr::filter(migrant_type=="Economic migrant", model_full == "Random-Effects") |>
  create_table_panel() |> reverse_row_order()
contrast_migranttype_forced_fe <- contrast_migranttype |>
  dplyr::filter(migrant_type=="Forced migrant", model_full == "Fixed-Effect") |>
  create_table_panel() |> reverse_row_order()
contrast_migranttype_forced_re <- contrast_migranttype |>
  dplyr::filter(migrant_type=="Forced migrant", model_full == "Random-Effects") |>
  create_table_panel() |> reverse_row_order()

contrast_migranttype_panels <- Reduce(
  function(x,y) merge(x, y, by = c("category_relabelled", "attribute", "level")),
  list(contrast_migranttype_econ_fe, contrast_migranttype_econ_re, contrast_migranttype_forced_fe, contrast_migranttype_forced_re)
) |> 
  reverse_row_order() |> 
  `rownames<-`(NULL)

splittab_forced_econ <- kableExtra::kbl(
  contrast_migranttype_panels,
  booktabs = TRUE, format = "latex",
  col.names = c("Category", "Attribute", "Level", 
                rep(c("FE Estimate (95% CI)", "N", "RE Estimate (95% CI)", "N"), 2)),
  caption = "Meta-regression results for Figure \\ref{fig:econ-forc}. Showing results by type of migrant, meta-model, and treatment.", 
  label = "reg_forced_econ") |>
  kable_classic() |>
  kable_styling(latex_options="scale_down") |>
  column_spec(1, bold = T) |>
  column_spec(2, bold = T) |>
  collapse_rows(columns = 1:2, valign = "middle") |>
  add_header_above(c(" "=1, "Treatment" = 2, "Economic migrant" = 4, "Forced migrant" = 4))

writeLines(splittab_forced_econ,file.path(dir_out_tabs, 'tab_forced_X_econ.tex'))

## TABLE A8 ----

{
  region = dplyr::left_join(region, study_tab, by = c("level"))
  region = prettify_plot_labels(region)
  
  region_plotdf <- region |>
    dplyr::filter(developed_studies >=5 & developing_studies >= 5) |>
    dplyr::filter(feature %in% c(
      "attr_educ_match", "attr_education",
      "attr_income_match","attr_income", "attr_skills_training",
      "attr_occupation",
      "attr_language",
      "attr_north_south",
      "attr_region_match", "attr_region",
      "attr_religion",
      "attr_gender",
      "attr_reason"
    )) |> 
    dplyr::mutate(
      category_relabelled = dplyr::case_when(
        attribute == "Age" ~ "Sociotropic concerns",
        category=="Economic self-reliance"~"Sociotropic concerns",
        category=="Religious/ethnic identity"~"Cultural concerns",
        category=="Education/language"~"Sociotropic concerns",
        category=="Socio-demographics"~"Humanitarian concerns",
        category=="Reasons to move"~"Humanitarian concerns"
      )
    )  |>
    dplyr::mutate(
      category_relabelled = dplyr::case_when(
        attribute %in% c('North south', 'Region match') ~ 'Cultural concerns',
        TRUE ~ category_relabelled
      )
    ) |>
    dplyr::arrange(category_relabelled, attribute, effect_estimate) |> # need to recompute order index here
    dplyr::mutate(order= 1:n()) |>
    dplyr::ungroup()
}

contrast_devtype = region_plotdf |>
  dplyr::select(model_full, category_relabelled, attribute, level, effect_estimate:effect_cu, country_type, count_of_studies) 
contrast_devtype_deved_fe <- contrast_devtype |>
  dplyr::filter(country_type=="Developed country", model_full == "Fixed-Effect") |>
  create_table_panel() |> reverse_row_order()
contrast_devtype_deved_re <- contrast_devtype |>
  dplyr::filter(country_type=="Developed country", model_full == "Random-Effects") |>
  create_table_panel() |> reverse_row_order()
contrast_devtype_deving_fe <- contrast_devtype |>
  dplyr::filter(country_type=="Developing country", model_full == "Fixed-Effect") |>
  create_table_panel() |> reverse_row_order()
contrast_devtype_deving_re <- contrast_devtype |>
  dplyr::filter(country_type=="Developing country", model_full == "Random-Effects") |>
  create_table_panel() |> reverse_row_order()

contrast_devtype_panels <- Reduce(
  function(x,y) merge(x, y, by = c("category_relabelled", "attribute", "level")),
  list(contrast_devtype_deved_fe, contrast_devtype_deved_re, contrast_devtype_deving_fe, contrast_devtype_deving_re)
) |> 
  reverse_row_order() |> 
  `rownames<-`(NULL)

splittab_dev_deving <- kableExtra::kbl(
  contrast_devtype_panels,
  booktabs = TRUE, format = "latex",
  col.names = c("Category", "Attribute", "Level", 
                rep(c("FE Estimate (95% CI)", "N", "RE Estimate (95% CI)", "N"), 2)),
  caption = "Meta-regression results for Figure \\ref{fig:developing_developed}. Showing results by host country level of development, meta-model, and treatment.", 
  label = "reg_developed_developing") |>
  kable_classic() |>
  kable_styling(latex_options="scale_down") |>
  column_spec(1, bold = T) |>
  column_spec(2, bold = T) |>
  collapse_rows(columns = 1:2, valign = "middle") |>
  add_header_above(c(" "=1, "Treatment" = 2, "Developed country" = 4, "Developing country" = 4))

writeLines(splittab_dev_deving, file.path(dir_out_tabs, 'tab_developed_X_developing.tex'))

# END SECTION B


# CODES FOR SECTION C ----

## FIGURE A2 ----

{
  png(file.path(dir_out_figs, 'funnel.png'), width = 1500, height = 1200, res = 150)
  par(mfrow=c(3,3))
  display_funnel_plots(feature = 'attr_educ_match', title = "Egocentric - Education Match")
  display_funnel_plots(feature = 'attr_income_match', title = "Egocentric - Skills/Income Match")
  display_funnel_plots(feature = 'attr_occupation', title = "Sociotropic - Occupation")
  display_funnel_plots(feature = 'attr_language', title = "Sociotropic - Language Skills")
  display_funnel_plots(feature = "attr_north_south", title = "Cultural - Region of Origin")
  display_funnel_plots(feature = 'attr_region_match', title = "Cultural - Region Match")
  display_funnel_plots(feature = 'attr_religion', title = "Cultural - Religion")
  display_funnel_plots(feature = 'attr_gender', title = "Humanitarian - Gender")
  display_funnel_plots(feature = "attr_reason", title = "Humanitarian - Reason to Migrate")
  dev.off()
}

# END SECTION C


# CODES FOR SECTION D ----

# See companion script 'R_supp_pilot_machine_learning.R' for codes that produced the inputs for this section.

## FIGURE A3 ----

vi_unemp_df <- read.csv('CACHE_cf_vi_unemp.csv')

ggplot(vi_unemp_df, aes(x = reorder(variable2, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance (Treatment = Unemployed)",
    x = "Covariate",
    y = "Importance"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_out_figs, 'vi_gs.png'), width = 10, height = 12, bg = 'white')

## FIGURE A4 ----

vi_gs_df <- read.csv('CACHE_cf_vi_gs.csv')

ggplot(vi_gs_df, aes(x = reorder(variable2, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance (Treatment = Global South)",
    x = "Covariate",
    y = "Importance"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_out_figs, 'vi_gs.png'), width = 10, height = 12, bg = 'white')

## FIGURE A5 ----

atesub_unemp_plotdf <- read.csv('CACHE_cf_tauhats_sub_unemp.csv')
atesub_unemp_plotdf$level <- factor(atesub_unemp_plotdf$level)
atesub_unemp_plotdf <- atesub_unemp_plotdf |>
  dplyr::mutate(variable2 = forcats::fct_reorder(variable2, imp_max))  # low→high; add .desc=TRUE for high→low

ggplot(
  atesub_unemp_plotdf,
  aes(x = variable2, y = estimate, ymin = conf.low, ymax = conf.high, color = level, shape = level)
) +
  geom_pointrange(
    position = position_dodge(width = 0.6),
    fatten = 1, size = 1.1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Subgroup Treatment Effects by Covariate [95% CI]\n (Treatment = Unemployed)",
    subtitle = "Each row shows CATE within level = 0 vs 1; ordered by variable importance",
    x = "Covariate",
    y = "Estimated effect",
    color = "Level", shape = 'Level'
  ) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_shape_manual(values = c('0'=16, '1'=18)) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_out_figs, 'tauhats_sub_unemp.png'), width = 10, height = 8, bg = 'white')

## FIGURE A6 ----

atesub_gs_plotdf <- read.csv('CACHE_cf_tauhats_sub_gs.csv')
atesub_gs_plotdf$level <- factor(atesub_gs_plotdf$level)
atesub_gs_plotdf <- atesub_gs_plotdf |>
  dplyr::mutate(variable2 = forcats::fct_reorder(variable2, imp_max))  # low→high; add .desc=TRUE for high→low

ggplot(
  atesub_gs_plotdf,
  aes(x = variable2, y = estimate, ymin = conf.low, ymax = conf.high, color = level, shape = level)
) +
  geom_pointrange(
    position = position_dodge(width = 0.6),
    fatten = 1, size = 1.1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4, alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Subgroup Treatment Effects by Covariate [95% CI]\n (Treatment = Global South)",
    subtitle = "Each row shows CATE within level = 0 vs 1; ordered by variable importance",
    x = "Covariate",
    y = "Estimated effect",
    color = "Level", shape = 'Level'
  ) +
  scale_color_grey(start = 0.8, end = 0.2) +
  scale_shape_manual(values = c('0'=16, '1'=18)) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_out_figs, 'tauhats_sub_gs.png'), width = 10, height = 8, bg = 'white')

# END SECTION D 


# CODES FOR SECTION E ----

## FIGURE A7 ----

meta_study_overview_std = prettify_plot_labels(meta_study_std)

ggplot(
  data = meta_study_overview_std |> dplyr::filter(feature!="attr_country"), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(" ",attribute, ": ",level," (",count_of_studies,")","&",category),order),
    color = model_full, shape = model_full,  alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  guides(y = ggh4x::guide_axis_nested(delim = "&")) + 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c("grey20", "grey60"),
                     name = "Meta-Model") +
  scale_shape_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c(16,17), name = "Meta-Model") +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14)+
  labs(x = NULL, y = NULL ) + 
  theme(
    axis.text = element_text(size=14),
    legend.position = "bottom"
  ) +
  guides(linewidthc= "none") 

ggsave(file.path(dir_out_figs, 'all_attributes.png'), width = 12, height = 15, bg = "white")

## FIGURE A8 ----

effects_ego_training = make_effect_df(
  eff_attr = "attr_skills_training") |>
  set_level_order(level_ref = "Extensive training/experience (more than 3y)")

plot_effect_df(
  eff_df = effects_ego_training |>
    dplyr::mutate(level = factor(level, levels = c("Extensive training/experience (more than 3y)",  "No/limited training/experience (less than 3y)"))), 
  txt_title = NULL)  

ggsave(file.path(dir_out_figs, 'single_ego_training.png'), width = 12, height = 12, bg = "white")

## FIGURE A9 ----

metastudy_muslimt_X_muslimr <- rbind.data.frame(
  study_muslimt_X_muslimr |> dplyr::select(
    study, level, effect_estimate = marginal_mean, effect_se = std_error, effect_cl = conf_low, effect_cu = conf_high, country_religion
  ) |>
    dplyr::mutate(study_type = "Individual Studies"),
  meta_muslimt_X_muslimr |> dplyr::select(
    study=model, level, effect_estimate, effect_se, effect_cl, effect_cu, country_religion
  ) |>
    dplyr::mutate(study = ifelse(study=="FE", "MetaReg (Fixed-Effect)", "MetaReg (Random-Effects)")) |>
    dplyr::mutate(study_type = "Meta")
) |>
  dplyr::mutate(
    significant_95pct = ifelse((effect_cl<0 & effect_cu<0) | (effect_cl>0 & effect_cu>0), 1, 0),
    study_type = factor(study_type, levels = c("Individual Studies","Meta"))
  ) |>
  dplyr::mutate(
    country_religion = case_when(
      country_religion == "Country with religious diversity, secular majorities, or other dominating religions" ~ "Other cases",
      TRUE ~ country_religion)
  )

p_religionmatch_christian = ggplot(metastudy_muslimt_X_muslimr |> 
                                     dplyr::filter(country_religion=="Christian-majority country"), 
                                   aes(effect_estimate, reorder(study, effect_estimate),
                                       color = level, shape = level,
                                       alpha = factor(significant_95pct), size = factor(study_type))) +
  geom_pointrange(aes(xmin = effect_cl, xmax = effect_cu),
                  position = position_dodge(0.7)) +
  facet_grid(rows = vars(study_type), cols = vars(country_religion), 
             scales = "free_y", space = "free", switch = "y") +
  scale_y_discrete(position = "right") +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  theme_pubclean(base_size = 14) + 
  scale_color_grey() +
  scale_alpha_manual(breaks = c(1,0), values = c(1,0.5), guide = "none") +
  scale_size_manual(breaks = c("Individual Studies","Meta"), values = c(.5,.75), guide = "none") +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL)

p_religionmatch_muslim = ggplot(metastudy_muslimt_X_muslimr |> 
                                  dplyr::filter(country_religion=="Muslim-majority country"), 
                                aes(effect_estimate, reorder(study, effect_estimate),
                                    color = level, shape = level,
                                    alpha = factor(significant_95pct), size = factor(study_type))) +
  geom_pointrange(aes(xmin = effect_cl, xmax = effect_cu),
                  position = position_dodge(0.7)) +
  facet_grid(rows = vars(study_type), cols = vars(country_religion), 
             scales = "free_y", space = "free", switch = "y") +
  scale_y_discrete(position = "right") +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  theme_pubclean(base_size = 14) + 
  scale_color_grey() +
  scale_alpha_manual(breaks = c(1,0), values = c(1,0.5), guide = "none") +
  scale_size_manual(breaks = c("Individual Studies","Meta"), values = c(.5,.75), guide = "none") +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL)

p_religionmatch_others = ggplot(metastudy_muslimt_X_muslimr |> 
                                  dplyr::filter(country_religion=="Other cases"), 
                                aes(effect_estimate, reorder(study, effect_estimate),
                                    color = level, shape = level,
                                    alpha = factor(significant_95pct), size = factor(study_type))) +
  geom_pointrange(aes(xmin = effect_cl, xmax = effect_cu),
                  position = position_dodge(0.7)) +
  facet_grid(rows = vars(study_type), cols = vars(country_religion), 
             scales = "free_y", space = "free", switch = "y") +
  scale_y_discrete(position = "right") +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  theme_pubclean(base_size = 14) + 
  scale_color_grey() +
  scale_alpha_manual(breaks = c(1,0), values = c(1,0.5), guide = "none") +
  scale_size_manual(breaks = c("Individual Studies","Meta"), values = c(.5,.75), guide = "none") +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL)

(p_religionmatch_christian + (p_religionmatch_muslim / p_religionmatch_others) +
    plot_layout(guides = "collect")) +
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 14)
    )
  )

ggsave(file.path(dir_out_figs, 'cross_culture_religiontype.png'), width = 15, height = 15, bg = "white")

## FIGURE A10 ----

resp.relig_meta = dplyr::left_join(resp.relig_meta, study_tab, by = c("level"))
resp.relig_meta = prettify_plot_labels(resp.relig_meta)

resp.relig_meta$count_of_studies <- NULL
study_counts_relig <- resp.relig |> 
  dplyr::filter(features=="attr_religion") |> 
  dplyr::group_by(resp_religion, level) |> 
  dplyr::summarise(count_of_studies = length(unique(study)))
study_counts_relig <- study_counts_relig |> 
  dplyr::arrange(factor(resp_religion,c("Christians","Muslims","Other religions","No religion"))) |> 
  dplyr::group_by(level) |> 
  dplyr::summarise(count_of_studies = paste(count_of_studies,collapse = "-"))

resp.relig_meta <- dplyr::left_join(resp.relig_meta,study_counts_relig) |> 
  dplyr::filter(level %in% c("No religion","Christian","Muslim"))

ggplot(
  data = resp.relig_meta, 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0("",level," (",count_of_studies,")"),order),
    color = model_full, shape = model_full,  alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c("grey20", "grey60"),
                     name = "Meta-Model") +
  scale_shape_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c(16,17), name = "Meta-Model") +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  facet_grid(~paste0("Respondent religion:\n",resp_religion))+
  # scale_y_discrete(position = "right") +
  scale_x_continuous(breaks=c(-0.2,-0.1,0,0.1,0.2))+
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14)+
  labs(x = NULL, y = NULL ) + 
  theme(
    axis.text = element_text(size=14),
    legend.position = "bottom"
  ) +
  guides(linewidthc= "none") 

ggsave(file.path(dir_out_figs, 'addition_resprelig.png'), width = 11, height = 5, bg = "white")

## FIGURE A11 ----

meta_religt_X_occupt <- dplyr::left_join(meta_religt_X_occupt, study_tab, by = c("level"))
meta_religt_X_occupt = prettify_plot_labels(meta_religt_X_occupt)

meta_religt_X_occupt$count_of_studies <- NULL
study_counts_relig <- study_religt_X_occupt |> 
  dplyr::filter(features=="attr_religion") |> 
  dplyr::group_by(attr_occupation, level) |> 
  dplyr::summarise(count_of_studies = length(unique(study))) |>
  dplyr::ungroup()
study_counts_relig <- study_counts_relig |> 
  dplyr::group_by(level) |> 
  dplyr::summarise(count_of_studies = paste(count_of_studies,collapse = "-")) |>
  dplyr::ungroup()

meta_religt_X_occupt <- left_join(meta_religt_X_occupt,study_counts_relig) |> 
  filter(level %in% c("No religion","Christian","Muslim"))

ggplot(
  data = meta_religt_X_occupt, 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0("",level," (",count_of_studies,")"),order),
    color = model_full, shape = model_full,  alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c("grey20", "grey60"),
                     name = "Meta-Model") +
  scale_shape_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c(16,17), name = "Meta-Model") +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  facet_grid(~paste0("Target occupation:\n", attr_occupation))+
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14)+
  labs(x = NULL, y = NULL ) + 
  theme(
    axis.text = element_text(size=14),
    legend.position = "bottom"
  ) +
  guides(linewidthc= "none") 
ggsave(file.path(dir_out_figs, 'addition_targetreligocc.png'), width = 10, height = 5, bg = "white")

## FIGURE A12 ----

meta_study_overview_std = prettify_plot_labels(meta_study_std)

ggplot(
  data = meta_study_overview_std |> dplyr::filter(feature=="attr_country"), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(level," (",count_of_studies,")"),order),
    color = model_full, shape = model_full,  alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c("grey20", "grey60"),
                     name = "Meta-Model") +
  scale_shape_manual(breaks = c("Fixed-Effect","Random-Effects"), 
                     values = c(16,17), name = "Meta-Model") +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14)+
  labs(x = NULL, y = NULL ) + 
  theme(
    axis.text = element_text(size=14),
    legend.position = "bottom"
  ) +
  guides(linewidthc= "none") 

ggsave(file.path(dir_out_figs, 'country.png'), width = 8, height = 10, bg = "white")

## TABLE A9 ----

study_overview_2 <- study_data |> 
  select(study, outcome) |> 
  unique() |> 
  group_by(outcome) |> 
  summarise(count=n()) |> 
  mutate(prop = count/sum(count)) |> 
  arrange(-count)
# study_overview_2

study_overview_2 <- kableExtra::kable(
  study_overview_2 |> dplyr::mutate(prop= round(100*prop,2)),
  "latex",booktabs=T
  )
writeLines(study_overview_2, file.path(dir_out_tabs, 'outcomes_overview.tex'))

## FIGURE A13 ----

outcome = dplyr::left_join(outcome, study_tab, by = c("level"))
outcome = prettify_plot_labels(outcome)

outcome_plotdf <- outcome |>
  dplyr::mutate(
    category_relabelled = dplyr::case_when(
      category=="Economic self-reliance"~"Economic concerns",
      category=="Religious/ethnic identity"~"Cultural concerns",
      category=="Socio-demographics"~"Humanitarian concerns",
      category == "Education/language" ~ "Economic concerns",
      category == "Reasons to move" ~ "Humanitarian concerns",
      category == "Origin country/region" ~ "Cultural concerns"
    )
  ) |> 
  dplyr::mutate(category_relabelled = factor(
    category_relabelled, levels = c("Economic concerns", "Cultural concerns", "Humanitarian concerns"))) |>
  dplyr::arrange(category_relabelled,attribute, -effect_estimate) |> 
  dplyr::mutate(order = 1:max(order)) 

ggplot(
  data = outcome_plotdf |> dplyr::filter(model == "RE"), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(" ",attribute, ": ",level,"&",category_relabelled), -order),
    color = outcome, shape = outcome, 
    alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  guides(y = ggh4x::guide_axis_nested(delim = "&")) + 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Admission","Citizenship/Residency","Other outcomes"),
                     values = c("grey20","grey30","grey40")) +
  scale_shape_manual(breaks = c("Admission","Citizenship/Residency","Other outcomes"),
                     values = c(16,17,18)) +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 13) + 
  # facet_wrap(~paste(model_full, "Meta-Estimates"))+
  facet_grid(~stringr::str_to_title(outcome)) +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL) + 
  theme(
    plot.caption= element_text(size = 11),
    strip.text = element_text(size = 13),
    # legend.position = "bottom"
    legend.position = 'none'
  ) +
  guides(linewidth= "none") 

ggsave(file.path(dir_out_figs, 'outcome_breakdown.png'), width = 15, height = 8, bg = "white")

## FIGURE A14 ----

counts_of_studies <- mm_experiment |> 
  dplyr::group_by(experiment, features) |> 
  dplyr::summarize(count_studies = length(unique(study))) |> 
  dplyr::mutate(attribute = stringr::str_to_sentence(gsub("_", " ",gsub("attr_", "", features))))

x = prettify_plot_labels(dplyr::left_join(metaeffs_experiment, study_tab, by = c("level")))
x <- dplyr::left_join(x, counts_of_studies) 
# Only levels in both
x <- x |> 
  dplyr::group_by(level) |> 
  dplyr::mutate(count = n()) |> 
  dplyr::filter(count == 4) |> 
  dplyr::select(-count)
x <- x|> 
  dplyr::group_by(attribute) |> 
  dplyr::mutate(both_over_5 = if_else(all(count_studies>=5),"Both","Too little")) |> 
  dplyr::ungroup() |> 
  dplyr::filter(both_over_5=="Both")

ggplot(
  data = x |> filter(model=="RE") |> 
    filter(attribute %in% c("Educ match","Education",
                            "Language","Occupation",
                            "North south","Region match",
                            "Religion","Gender","Reason")), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(" ",attribute, ": ",level,"&",category),order),
    color = experiment, shape =  experiment
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  guides(y = ggh4x::guide_axis_nested(delim = "&"))+ 
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Conjoint experiments","Other types of experiments"), 
                     values = c("grey20", "grey60"),
                     name = "Experiment type")+
  scale_shape_manual(breaks = c("Conjoint experiments","Other types of experiments"), 
                     values = c(16,17),
                     name = "Experiment type")+
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14)+
  labs(x = NULL, y = NULL ) + 
  theme(
    axis.text = element_text(size=14),
    legend.position = "bottom"
  ) +
  guides(linewidthc= "none") 

ggsave(file.path(dir_out_figs, 'addition_experiment.png'), width = 9, height = 6, bg = "white")

## FIGURE A15 ----

effects_rival = make_effect_df(eff_attr = "attr_geo_rival") 
effects_colony = make_effect_df(eff_attr = "attr_colonial_heritage") 
effects_common = make_effect_df(eff_attr = "attr_common_language") 

effects = dplyr::bind_rows(effects_rival |> mutate(model="Geo-political rivalry"),
                    effects_colony |> mutate(model="Colonial ties"),
                    effects_common |> mutate(model="Language ties")) |> 
  dplyr::mutate(shape_form = if_else(level %in% c("Colonial ties","Rival","Common official language"),
                              "Colonial ties | Rival | Common official language",
                              "No colonial history | Non-rival | No joint official language")) |> 
  dplyr::arrange(shape_form, effect_estimate) 

ggplot(effects, 
       aes(color = shape_form, shape = shape_form, 
           alpha = factor(significant_95pct), size = factor(study_type))) +
  geom_pointrange(
    aes(x = effect_estimate, y = (study), xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7)) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  facet_grid(rows = vars(study_type), 
             cols = vars(model),
             scales = "free", space = "free_y", switch="y") +
  scale_y_discrete(position = "right") +
  ggpubr::theme_pubclean(base_size = 14) +
  scale_color_grey() +
  scale_alpha_manual(breaks = c(1,0), values = c(1,0.5), guide = "none") +
  scale_size_manual(breaks = c("Single-Study","Meta"), values = c(.5,.7), guide = "none") +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL) +
  theme(
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(dir_out_figs, 'addition_country_classification.png'), width = 18, height = 15, bg = "white")

# CLEANUP ----

rm(list = ls())
gc()

# END SCRIPT
