# R script for: producing the tables and graphs presented in the main text
# This version dated to: 2025-10-08

# NOTES:
## The code sections are presented in the same way the corresponding outputs are shown in text;
## Set your current working directory to where this file is before running.

rm(list = ls())

# CONFIG: Paths/Directories ----

dir_input <- 'OUT'
if (!dir.exists(dir_input)) stop('Input folder not found!')

dir_out_figs <- 'FIGURES (MAIN)'
if (!dir.exists(dir_out_figs)) dir.create(dir_out_figs)

dir_out_tabs <- 'TABLES (MAIN)'
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

# PRELIM: Ctrl Parameters ----

level_meta = c("MetaReg (Fixed-Effect)","MetaReg (Random-Effects)")

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

# literature review data
listing <- readxl::read_xlsx("DAT_SCOPUS_LISTING.xlsx")

# timeline data
ref_time <- readr::read_csv("DAT_refugees_annnual_population.csv") |>
  dplyr::mutate(
    population = `Refugees under UNHCR's mandate` + `Asylum-seekers`+ `IDPs of concern to UNHCR` + `Other people in need of international protection`)
mig_total <- readxl::read_xlsx("DAT_undesa_migrant_stock.xlsx", sheet = 2)

# country coverage data
ref <- readr::read_csv("DAT_refugee_population.csv")
mig <- readxl::read_xlsx("DAT_undesa_pd_2020_ims_stock_by_sex_and_destination.xlsx", sheet = 4)

# FIGURE 1 ----

time_breakdown <-  study_data |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(
    unique_studies = length(unique(study)),
    respondents = length(unique(resp_id)))

mig_time <- mig_total |> 
  tidyr::pivot_longer(cols = c(`1995`,`2005`,`2010`,`2015`,`2020`)) |> 
  dplyr::mutate(year = as.numeric(name),
                migrants = as.numeric(value))|> 
  dplyr::group_by(year) |> 
  dplyr::summarise(migrants_total = sum(migrants, na.rm=T)) |>
  dplyr::ungroup()

time <- dplyr::full_join(ref_time, time_breakdown, by = c("Year"="year")) |> 
  dplyr::left_join(mig_time,by = c("Year"="year")) |> 
  tidyr::replace_na(list(unique_studies = 0, respondents=0)) |> 
  dplyr::group_by(Year) |> 
  dplyr::mutate(label = dplyr::if_else(is.na(migrants_total)==F, paste0(Year),NA)) |> 
  dplyr::mutate(label = dplyr::if_else(Year == 2005,paste0("Global Migrant Stock (",label,")\n in 5 millions"),label)) |>
  dplyr::ungroup()

time2 <- time |>
  dplyr::filter(unique_studies>0) |>
  dplyr::arrange(Year) |>
  dplyr::mutate(is_down = as.integer(unique_studies<lag(unique_studies)|unique_studies%in%c(12,13,16))) |>
  dplyr::mutate(is_down = ifelse(Year==min(Year), 0, is_down))

ggplot(time) +
  geom_line(aes(x=Year,y=population/5000000), color = "grey50", lty = "dashed") +
  geom_col(aes(x=Year,y=population/5000000), color = "grey60", fill = "grey60", alpha = 0.4) +
  geom_line(aes(x=Year,y=unique_studies), color = "white", lwd = 3) +
  geom_point(data = time2, aes(x = Year, y = unique_studies), color = "white", size = 3) +
  geom_line(aes(x=Year, y=unique_studies))+
  geom_point(data = time2, aes(x = Year, y = unique_studies)) +
  geom_text(
    data = time2,
    aes(x = Year, y=unique_studies, label = paste0("+", unique_studies) ), 
    nudge_y = ifelse(time2$is_down, -0.5, 0.5)) +
  xlim(2000,2024) +
  scale_y_continuous(
    name = "Studies in meta-analysis",
    sec.axis = sec_axis(~., name = "Global refugees, asylumseekers \nand IDPs (in 5 millions)")
  ) + 
  theme_minimal(base_size = 13) +
  theme(axis.text.y.right = element_text(color = "grey50"),
        axis.title.y.right = element_text(color = "grey50"),
        plot.tag.position = "bottomleft"
  )

ggsave(file.path(dir_out_figs, 'timeline.png'), dpi= 600, width=10, height=5, bg = "white")

# TABLE 1 ----

tab1 = study_data |> 
  dplyr::select(study, starts_with("attr_")) |> 
  tidyr::pivot_longer(cols=starts_with("attr_")) |> 
  dplyr::filter(is.na(value)==F) |> 
  dplyr::filter(name %in% c("attr_educ_match","attr_income_match","attr_occupation",
                     "attr_language","attr_skills_training","attr_north_south",
                     "attr_religion","attr_gender","attr_reason","attr_region_match")) |> 
  dplyr::group_by(study, name) |> 
  dplyr::summarise(value = length(unique(value))) |> 
  dplyr::filter(value >1) |> 
  dplyr::select(-value) |> 
  unique() |> 
  dplyr::group_by(name) |> 
  dplyr::summarise(Studies = length(unique(study))) |>
  dplyr::ungroup() %>% 
  dplyr::left_join(
    data.frame(
      name = c(
        'attr_educ_match', 'attr_income_match',
        'attr_occupation', 'attr_language',
        'attr_north_south', 'attr_region_match', 
        'attr_religion', 'attr_reason'
      )
    ), .
  )

kableExtra::kable(tab1, "latex", longtable = T, booktabs = T) |>
  writeLines(file.path(dir_out_tabs, 'theory_explanations.tex'))
# Columns 1-4 & 6 are manually added and involve no data analysis.

# TABLE 3 ----

tab2 = dplyr::bind_rows(
  data.frame(
    "Approach"  = c("Universe of possible studies through key word search",
                    "Studies identified as relevant"),
    "Studies" = c(nrow(listing |> filter(Source == "Scopus")),
                  nrow(listing |> filter(Source == "Scopus"))-nrow(listing |> filter(Source == "Scopus")|> filter(Progress == "Not relevant"))),
    "Percentage" = c(100,round(100*(nrow(listing |> filter(Source == "Scopus"))-nrow(listing |> filter(Source == "Scopus")|> filter(Progress == "Not relevant")))/nrow(listing |> filter(Source == "Scopus"))))
  ),
  listing |> 
    dplyr::filter(Source == "Scopus" & Progress != "Not relevant") |> 
    dplyr::group_by(`Data status`) |> 
    dplyr::summarise(count=n()) |> 
    # Add the three cases in which one paper had 2 studies
    dplyr::mutate(count = case_when(
      `Data status` == "Data available" ~ count + 2,
      `Data status` == "Data provided upon request" ~ count + 1,
      TRUE ~ count)) |> 
    dplyr::mutate(Percentage = round(100*count/sum(count,na.rm=T))) |> 
    dplyr::rename(Approach = `Data status`,
           Studies = count),
  listing |> 
    dplyr::filter(Progress == "Included") |> 
    dplyr::filter(Source != "Scopus") |> 
    dplyr::mutate(Source = if_else(Year == "NA","Solicitation of unpublished working papers",Source)) |> 
    dplyr::group_by(Source) |> 
    dplyr::summarise(Studies = n()) |> 
    dplyr::rename(Approach = Source),
  data.frame("Approach" = "Total number of studies included in meta-analysis",
             Studies = length(unique(study_data$study)))
)

kableExtra::kable(tab2, "latex", longtable = T, booktabs = T) |>
  writeLines(file.path(dir_out_tabs, 'filter_information.tex'))

# FIGURE 2 ----

crseffects_edu = make_effect_df(eff_attr = "attr_educ_match") 

level_edumatch_matchhigh = crseffects_edu |>
  dplyr::group_by(study) |>
  dplyr::filter(level=="Education match") |>
  dplyr::ungroup() |>
  dplyr::arrange(effect_estimate) |>
  dplyr::pull(study) |>
  setdiff(level_meta)
level_edumatch_noneconomic = setdiff(setdiff(unique(crseffects_edu$study), level_meta), level_edumatch_matchhigh)
level_edumatch = c(level_edumatch_noneconomic, level_edumatch_matchhigh, level_meta)
crseffects_edu$study = factor(crseffects_edu$study, levels = level_edumatch)

plot_effect_df(
  eff_df = crseffects_edu |>
    dplyr::mutate(
      level = factor(
        level, levels = c("Education match","Education mismatch"))),
  txt_title = NULL
  ) +
  scale_color_grey(start = .1, end = .6)

ggsave(
  file.path(dir_out_figs, 'cross_ego_edumatch.png'),
  width = 12, height = 13, bg = "white", dpi= 600
  )

# FIGURE 3 ----

crseffects_income = make_effect_df(eff_attr = "attr_income_match") 

level_incmatch_matchhigh = crseffects_income |>
  dplyr::group_by(study) |>
  dplyr::filter(level=="Skills/income match") |>
  dplyr::ungroup() |>
  dplyr::arrange(effect_estimate) |>
  dplyr::pull(study) |>
  setdiff(level_meta)
level_incmatch_matchlow = setdiff(setdiff(unique(crseffects_income$study), level_meta), level_incmatch_matchhigh)
level_incmatch = c(level_incmatch_matchlow, level_incmatch_matchhigh, level_meta)
crseffects_income$study = factor(crseffects_income$study, levels = level_incmatch)

plot_effect_df(
  eff_df = crseffects_income |>
    dplyr::mutate(level = factor(level, levels = c("Skills/income match","Skills/income mismatch"))),
  txt_title = NULL
  ) +
  scale_color_grey(start = .1, end = .6)

ggsave(
  file.path(dir_out_figs, 'cross_ego_incmatch.png'), 
  width = 12, height = 13, bg = "white", dpi= 600
  )

# FIGURE 4 ----

effects_socio_occu = make_effect_df(eff_attr = "attr_occupation") |>
  dplyr::filter(!(level=="Student/Pensioner" & study_type=="Meta")) |>
  set_level_order(level_ref = "Professional occupation") 

plot_effect_df(
  eff_df = effects_socio_occu |>
    dplyr::mutate(level = factor(level, levels=c("Professional occupation", "Worker/Farmer", "Unemployed", "Student/Pensioner"))),
  txt_title = NULL
  ) 

ggsave(
  file.path(dir_out_figs, 'single_socio_occupation.png'),
  width = 12, height = 12, bg = "white",dpi= 600
  )

# FIGURE 5 ----

effects_socio_lan = make_effect_df(eff_attr = "attr_language") |>
  set_level_order(level_ref = "Fluent language")

plot_effect_df(
  eff_df = effects_socio_lan |>
    dplyr::mutate(level = factor(level, levels = c("Fluent language","Broken language", "Unable to speak language"))),
  txt_title = NULL
  ) 

ggsave(
  file.path(dir_out_figs, 'single_socio_language.png'), 
  width = 12, height = 12, dpi= 600, bg = "white"
  )

# FIGURE 6 ----

effects_culture_continent = make_effect_df(eff_attr = "attr_north_south") |>
  set_level_order(level_ref = "Developed/High income")

plot_effect_df(
  eff_df = effects_culture_continent |>
    dplyr::mutate(level = factor(level, levels = c("Developed/High income",  "Developing/Low income"))),
  txt_title = NULL
  )  +
  scale_color_grey(start = .2, end = .6)

ggsave(
  file.path(dir_out_figs, 'single_culture_northsouth.png'),
  dpi= 600, width = 10, height = 12, bg = "white"
  )

# FIGURE 7 ----

effects_region_match = make_effect_df(eff_attr = "attr_region_match") |>
  set_level_order(level_ref = "Same world region")

plot_effect_df(
  eff_df = effects_region_match |>
    dplyr::mutate(level = factor(level, levels = c("Same world region",  "Different world region"))), 
  txt_title = NULL
  )  +
  scale_color_grey(start = .2, end = .6)

ggsave(
  file.path(dir_out_figs, 'single_culture_regionmatch.png'),
  dpi= 600, width = 10, height = 12, bg = "white"
  )

# FIGURE 8 ----

effects_culture_relig = make_effect_df(eff_attr = "attr_religion") |>
  dplyr::filter(!(level=="Jew" & study_type=="Meta"))  |>
  dplyr::filter(!(level=="Buddhist" & study_type=="Meta"))

level_religion_christians = effects_culture_relig |>
  dplyr::group_by(study) |>
  dplyr::filter(level=="Christian") |>
  dplyr::ungroup() |>
  dplyr::arrange(effect_estimate) |>
  dplyr::pull(study) |>
  setdiff(level_meta)
level_religion_nonchristians = setdiff(setdiff(unique(effects_culture_relig$study), level_meta), level_religion_christians)
level_religion = c(level_religion_nonchristians, level_religion_christians, level_meta)
effects_culture_relig$study = factor(effects_culture_relig$study, levels = level_religion)

plot_effect_df(
  eff_df = effects_culture_relig |>
    dplyr::mutate(level = factor(level, levels = c("Christian", "Muslim", "Hindu", "Jew", "Buddhist","No religion"))),
  txt_title = NULL
  ) 

ggsave(
  file.path(dir_out_figs, 'single_culture_religion.png'),
  dpi= 600, width = 12, height = 15, bg = "white"
  )

# FIGURE 9 ----

effects_human_gender = make_effect_df(eff_attr = "attr_gender") |>
  set_level_order(level_ref = "Female")

plot_effect_df(eff_df = effects_human_gender, txt_title = NULL) 

ggsave(
  file.path(dir_out_figs, 'single_human_gender.png'),
  dpi= 600, width = 10, height = 12, bg = "white"
  )

# FIGURE 10 ----

effects_human_reason = make_effect_df(eff_attr = "attr_reason") 

level_reason_economic = effects_human_reason |>
  dplyr::group_by(study) |>
  dplyr::filter(level=="Economic migrant") |>
  dplyr::ungroup() |>
  dplyr::arrange(effect_estimate) |>
  dplyr::pull(study) |>
  setdiff(level_meta)
level_reason_noneconomic = setdiff(setdiff(unique(effects_human_reason$study), level_meta), level_reason_economic)
level_reason = c(level_reason_noneconomic, level_reason_economic, level_meta)
effects_human_reason$study = factor(effects_human_reason$study, levels = level_reason)

plot_effect_df(
  eff_df = effects_human_reason |>
    dplyr::mutate(level = factor(level, levels = c("Economic migrant", "Climate migrant", "Family reunification", "Forced migrant"))), 
  txt_title = NULL
  ) 

ggsave(
  file.path(dir_out_figs, 'single_human_reason.png'),
  dpi= 600,width = 12, height = 12, bg = "white"
  )

# FIGURE 11 ----

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

ggplot(
  data = dplyr::filter(reason_plotdf, model=="RE"), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(" ",attribute, ": ",level," (",reason_econ," - ",reason_forced,")","&",category_relabelled),-order),
    color = migrant_type, shape = migrant_type,
    alpha = factor(significant_95pct)
  )) +
  geom_point(size = 2, position = position_dodge(0.7)) + 
  guides(y = guide_axis_nested(key=key_range_auto(sep="&"))) +
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Economic migrant", "Forced migrant"),
                     values = c("grey20","grey60")) +
  scale_shape_manual(breaks = c("Economic migrant", "Forced migrant"),
                     values = c(16,17)) +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 14) + 
  facet_wrap(~paste(model_full, "Meta-Estimates")) +
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL
  ) + 
  theme(
    axis.text=element_text(size=14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 15),
    legend.position = "bottom"
  ) +
  guides(linewidth= "none")

ggsave(
  file.path(dir_out_figs, 'forced_econ.png'),
  dpi= 600,width = 12, height = 9, bg = "white"
  )

# FIGURE 12 ----

mig <- mig |> 
  tidyr::pivot_longer(cols = c(`1990`,`1995`,`2000`,`2005`,`2010`,`2015`,`2020`)) |> 
  dplyr::mutate(year = as.numeric(name),
                migrants = as.numeric(value))|> 
  dplyr::filter(year >= 2010) |> 
  dplyr::group_by(`Location code`, `Region, development group, country or area`) |> 
  dplyr::summarise(migrants = mean(migrants, na.rm=T)) |>
  dplyr::ungroup()

country_list <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> 
  dplyr::select(abbrev,name_long,iso_a3, iso_n3)|> 
  dplyr::filter(name_long != "Antarctica") |> 
  dplyr::filter(name_long != "Greenland") |> 
  dplyr::mutate(iso_n3 = as.numeric(iso_n3))
country_list$iso_a3[country_list$name_long=="France"] <- "FRA"
country_list$iso_a3[country_list$name_long=="Norway"] <- "NOR"
country_list$iso_n3[country_list$name_long=="France"] <- 250L
country_list$iso_n3[country_list$name_long=="Norway"] <- 578L

country_breakdown <- study_data |> 
  dplyr::group_by(country) |> 
  dplyr::summarise(
    unique_studies = length(unique(study)),
    respondents = length(unique(resp_id)))

country_list <- country_list |> 
  # add number of studies
  dplyr::left_join(
    country_breakdown,
    by = c("name_long" = "country")) |> 
  # add mean of forcibly displaced populations since 2010
  dplyr::left_join(
    ref |>
      dplyr::filter(Year >=2010) |>
      dplyr::mutate(fdps = `Refugees under UNHCR's mandate`+`Asylum-seekers`+`IDPs of concern to UNHCR`) |>
      dplyr::group_by(`Country of asylum (ISO)`) |>
      dplyr::summarise(fdps = mean(fdps,na.rm=T)) |>
      dplyr::ungroup(),
    by = c("iso_a3"= "Country of asylum (ISO)")) |> 
  # add rates of migrants since 2000
  dplyr::left_join(mig, by = c("iso_n3" = "Location code")) |> 
  dplyr::left_join(mig_total,by = c("iso_n3" = "location_code")) |> 
  tidyr::replace_na(list(unique_studies = 0, respondents = 0))

ggplot(
  country_list |> 
    dplyr::filter(abbrev != "A.C.Is.")|> 
    dplyr::filter(respondents > 0 | fdps > 2000000) |>
    dplyr:::mutate(name_long = ifelse(name_long=='Democratic Republic of the Congo', 'DR congo', name_long))
  ) +
  geom_bar(aes(
    y = reorder(name_long, fdps),
    x = respondents/100, fill = "Respondents"), 
    stat = "identity",alpha=0.8, width = 0.8) +
  geom_bar(aes(
    y = reorder(name_long, fdps),
    x = fdps/10000, fill = "Refugees"),
    stat = "identity", width = 0.8, alpha=0.5) +
  scale_fill_manual(values = c("Respondents" = "black", "Refugees" = "grey60")) +
  labs(fill = "") +
  theme_minimal(base_size = 13) +
  scale_x_continuous(
    name = "Respondents (in hundreds)",
    sec.axis = sec_axis(~., name = "Hosted forcibly displaced population (in ten thousands)"),
    expand = c(0,0))+
  xlab("") + ylab("")+ 
  theme(legend.position = 'none',
        axis.text.x.top = element_text(color = "grey40"),
        axis.title.x.top= element_text(color = "grey40"),
        plot.tag.position = "topleft",
        plot.caption = element_text(size=11,hjust=1),
  ) + # Add manual legend box in bottom right
  annotate("rect", xmin = 500, xmax = 690, ymin = 37-20, ymax = 42-20,
           fill = "white", color = "grey50") +
  annotate("rect", xmin = 510, xmax = 530, ymin = 41.2-20.5, ymax = 41.6-20.5,
           fill = "grey60", alpha = 0.5, color = NA) +
  annotate("text", x = 535, y = 41.4-20.5, label = "Hosted displaced (×10k)", hjust = 0, size = 4) +
  annotate("rect", xmin = 510, xmax = 530, ymin = 38.4-20, ymax = 38.8-20,
           fill = "black", alpha = 0.8, color = NA) +
  annotate("text", x = 535, y = 38.6-20, label = "Respondents (×100)", hjust = 0, size = 4)

ggsave(
  file.path(dir_out_figs, 'knowledge_barplot.png'),
  dpi=600, width=10,height=8, bg = "white"
  )

# FIGURE 13 ----

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

ggplot(
  data = region_plotdf |> 
    dplyr::filter(model == "RE"), 
  mapping = aes(
    x = effect_estimate, 
    y = reorder(paste0(" ",attribute, ": ",level," (",developed_studies," - ", developing_studies,")","&",category_relabelled), order),
    color = country_type, shape = country_type,
    alpha = factor(significant_95pct)
  )) +
  geom_point(size = 1.5, position = position_dodge(0.7)) + 
  guides(y = guide_axis_nested(key=key_range_auto(sep="&"))) +
  geom_errorbarh(aes(
    xmin = effect_cl, xmax = effect_cu),
    position = position_dodge(0.7), height=.01) + 
  scale_linewidth_continuous(range = c(0.5,1.1)) +
  scale_color_manual(breaks = c("Developed country","Developing country"), 
                     values = c("grey20","grey60")) +
  scale_shape_manual(breaks = c("Developed country","Developing country"), 
                     values = c(16,17)) +
  scale_alpha_manual(breaks = c(1,0), values = c(0.9,0.3), guide = "none"
  ) +
  geom_vline(xintercept = 0, lty = "dotdash", color = "red") +
  ggpubr::theme_pubclean(base_size = 13) + 
  facet_wrap(~paste(model_full, "Meta-Estimates"))+
  labs(x = NULL, y = NULL, 
       color = NULL, shape = NULL) + 
  theme(
    plot.caption= element_text(size = 11),
    strip.text = element_text(size = 13),
    legend.position = "bottom"
  ) +
  guides(linewidth= "none") 

ggsave(
  file.path(dir_out_figs, 'developing_developed.png'),
  dpi=600, width = 8, height = 6, bg = "white"
  )

# TABLE 4 ----

# MANUALLY TYPED UP; NO ANALYSIS INVOLVED.

# CLEANUP ----

rm(list = ls())
gc()

# END SCRIPT #

