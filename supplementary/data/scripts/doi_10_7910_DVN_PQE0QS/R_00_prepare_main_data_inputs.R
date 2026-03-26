# R script for: turning individual study-level data into meta-analytic estimates
# This version dated to: 2025-10-08

# NOTES:
## Set your current working directory to where this file is;
## Expect long running time for some codes in this script.

rm(list = ls())

# CONFIG: Set controls ----

erase_existing_estimates <- TRUE # Set to FALSE to save runtime

# CONFIG: Set directories ----

dir_out <- 'OUT'

if (!dir.exists(dir_out)) {
  dir.create(dir_out)
} 
if (erase_existing_estimates) unlink(dir_out)

# PRELIM: Load packages ----

{
  suppressPackageStartupMessages(if (!require('tidyverse')) install.packages('tidyverse'))
  suppressPackageStartupMessages(if (!require('data.table')) install.packages('data.table'))
  suppressPackageStartupMessages(if (!require('here')) install.packages('here'))
  suppressPackageStartupMessages(if (!require('readxl')) install.packages('readxl'))
  suppressPackageStartupMessages(if (!require('fastDummies')) install.packages('fastDummies'))
  suppressPackageStartupMessages(if (!require('cjoint')) install.packages('cjoint'))
  suppressPackageStartupMessages(if (!require('estimatr')) install.packages('estimatr'))
  suppressPackageStartupMessages(if (!require('metafor')) install.packages('metafor'))
  if (!require("remotes")) {
    install.packages("remotes")
  }
  remotes::install_github("leeper/cregg")
  suppressPackageStartupMessages(if (!require('cregg')) install.packages('cregg'))
}

# PRELIM: Load data ----

data <- readr::read_csv("DAT_cleaned_study_data.csv")
data_ls = split(data, data$study)

# PRELIM: Define helpers ----

estimate_marginal_means <- function(study, standardize_outcome = FALSE){
  
  # select required vars
  working_data = study |>
    dplyr::select(choice, starts_with("attr_"), resp_id)
  
  # drop degenerate vars
  working_data = working_data |> 
    dplyr::select_if(\(x) sd(as.numeric(factor(x)),na.rm=T)!=0) 
  
  # factorize features
  working_data = working_data |>
    dplyr::mutate(across(starts_with("attr_"), factor))
  
  # (if requested) standardize outcome
  if (standardize_outcome) working_data = working_data |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1])
  
  # set formula
  working_formula = as.formula(
    paste("choice ~ ", paste(names(dplyr::select(working_data, -resp_id,-choice)), collapse= "+"))
  )
  
  # fit model
  working_model = cregg::mm(data = working_data, formula = working_formula, id = ~resp_id)
  
  # extract results
  out = data.frame(
    features=working_model$feature,
    level=working_model$level,
    marginal_mean=working_model$estimate,
    std_error=working_model$std.error, 
    conf_low=working_model$lower,
    conf_high=working_model$upper
  ) |>
    dplyr::mutate(sampling_var = std_error**2)
  
  # return results
  return(out)
  
}

estimate_meta_effects = function(mm) { # 'mm' = df of MMs for single attribute with possibly multiple levels
  
  # fit fixed-effects model
  fe = metafor::rma.mv(
    yi = marginal_mean,
    V = sampling_var,
    mods = ~factor(level)-1,
    data = mm
  )
  fe_out = data.frame(
    model = "FE",
    level = gsub("factor(level)","",rownames(fe$beta), fixed = TRUE),
    effect_estimate = as.numeric(unname(fe$beta)),
    effect_se = as.numeric(fe$se),
    effect_cl = as.numeric(fe$ci.lb),
    effect_cu = as.numeric(fe$ci.ub)
  )
  
  # fit random-effects model 
  re = metafor::rma.mv(
    yi = marginal_mean,
    V = sampling_var,
    mods = ~factor(level)-1,
    random = ~1|study,
    data = mm
  )
  re_out = data.frame(
    model = "RE",
    level = gsub("factor(level)","",rownames(re$beta), fixed = TRUE),
    effect_estimate = as.numeric(unname(re$beta)),
    effect_se = as.numeric(re$se),
    effect_cl = as.numeric(re$ci.lb),
    effect_cu = as.numeric(re$ci.ub)
  )
  
  # prepare outputs
  mods = list("FE" = fe, "RE" = re)
  ests = rbind.data.frame(fe_out, re_out)
  outs = list("model_fits"=mods, "model_estimates"=ests)
  
  # return outputs
  return(outs)
  
}

extract_metareg_fits = function(metareg_ls) {
  # extract model fits 
  metareg_fit = purrr::map(metareg_ls, "model_fits")
  metareg_fit_fe = purrr::map(metareg_fit, "FE")
  metareg_fit_re = purrr::map(metareg_fit, "RE")
  metareg_fits = list("FE" = metareg_fit_fe, "RE" = metareg_fit_re)
  
  # extract effect estimates 
  metareg_ests = purrr::map(metareg_ls, "model_estimates")
  metareg_ests_df = dplyr::bind_rows(metareg_ests, .id = "feature") 
  
  # return extractions
  return(list("fit"=metareg_fits, "ests"=metareg_ests_df))
}

# RUN: Estimate marginal means ----

mm_std_ls = purrr::map(data_ls, ~estimate_marginal_means(.x, standardize_outcome = TRUE))

if (is.null(names(mm_std_ls))) names(mm_std_ls) <- names(data_ls)
print(all.equal(names(mm_std_ls), names(data_ls))) # required: TRUE

mms_std = dplyr::bind_rows(mm_std_ls, .id = "study")
mms_std = dplyr::select(mms_std, study, everything())

readr::write_csv(mms_std, file.path(dir_out, 'study_effects_std.csv'))
mms_std <- readr::read_csv(file.path(dir_out, 'study_effects_std.csv'))

# RUN: Estimate simple meta-effects ----

estimate_meta_effects_safely = purrr::safely(estimate_meta_effects, otherwise = NULL)

mms_std_ls = split(mms_std, mms_std$features)

metaeffs_std_ls = purrr::map(mms_std_ls, estimate_meta_effects_safely) |>
  purrr::map("result")

metaeffs_std_ls_out = extract_metareg_fits(metaeffs_std_ls)
metaeffs_std_ls_mods2 = metaeffs_std_ls_out$fit
metaeffs_std_dfs_ests = metaeffs_std_ls_out$ests

readr::write_rds(metaeffs_std_ls_mods2, file.path(dir_out, 'meta_estimates_std.RDS'))
readr::write_csv(metaeffs_std_dfs_ests, file.path(dir_out, 'meta_effects_std.csv'))

metaeffs_std_ls_mods2 <- readr::read_rds(file.path(dir_out, 'meta_estimates_std.RDS'))
metaeffs_std_dfs_ests <- readr::read_csv(file.path(dir_out, 'meta_effects_std.csv'))

# RUN: Estimate conditional meta-effects ----

# BY migration reason: Forced vs economic reasons

{
  data_forced = data |>
    dplyr::filter(attr_reason == "Forced migrant" | migrant_type %in% c("IDPs","Refugees")) |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  data_economic = data |>
    dplyr::filter(attr_reason == "Economic migrant" | migrant_type %in% c("Internal migrants","Migrants")) |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
}

{
  mm_forced = data_forced %>%
    dplyr::filter(!study  %in% c("Castellano, Dolsak & Prakash (2021)", 
                                 "Graf et al. (2023)","Hoewe (2018)",
                                 "Valsecchi et al. (2023)",
                                 "Henning, Steimanis & Vollan (2022)")) %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_economic = data_economic%>%
    dplyr::filter(!study  %in% c("Castellano, Dolsak & Prakash (2021)", 
                                 "Graf et al. (2023)","Hoewe (2018)",
                                 "Valsecchi et al. (2023)",
                                 "Henning, Steimanis & Vollan (2022)")) %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  
  mm_reason = rbind.data.frame(
    mm_economic |> dplyr::mutate(migrant_type ="Economic migrant"),
    mm_forced |> dplyr::mutate(migrant_type="Forced migrant")
  )
}

{
  metaeffs_reason_economic = mm_economic |>
    estimate_meta_effects()
  metaeffs_reason_forced = mm_forced |>
    estimate_meta_effects()
  metaeffs_reason= rbind.data.frame(
    metaeffs_reason_economic$model_estimates |> dplyr::mutate(migrant_type="Economic migrant"),
    metaeffs_reason_forced$model_estimates |> dplyr::mutate(migrant_type="Forced migrant")
  )
}

write.csv(mm_reason, file.path(dir_out, 'study_effects_std_reason.csv'))
write.csv(metaeffs_reason, file.path(dir_out, 'meta_effects_std_reason.csv'))

mm_reason <- read.csv(file.path(dir_out, 'study_effects_std_reason.csv'))
metaeffs_reason <- read.csv(file.path(dir_out, 'meta_effects_std_reason.csv'))

# BY migrant origin: Developed or developing country 

{
  wimmer_developing <- estimate_marginal_means(
    (data_ls$`Wimmer et al. (2024)`)|> 
      dplyr::filter(country_type == "Developing/Low income"), 
    standardize_outcome = T) |> 
    dplyr::mutate(study="Wimmer et al. (2024)")
  
  wimmer_developed <- estimate_marginal_means(
    (data_ls$`Wimmer et al. (2024)`)|> 
      dplyr::filter(country_type == "Developed/High income"), 
    standardize_outcome = T) |> 
    dplyr::mutate(study="Wimmer et al. (2024)")
  
  frame_developing <- bind_rows(
    mms_std |> 
      dplyr::filter(study != "Wimmer et al. (2024)"),
    wimmer_developing
  ) |> dplyr::left_join(
    data |> 
      dplyr::mutate(country_type = if_else(study == "Wimmer et al. (2024)",
                                           "Developing/Low income",country_type)) |> 
      dplyr::select(study,country_type) |> 
      unique()) |> 
    dplyr::filter(country_type == "Developing/Low income") |> 
    dplyr::select(-country_type) |> 
    dplyr::group_by(features) |> 
    dplyr::mutate(count = length(unique(study))) |> 
    dplyr::filter(count > 4)
  
  frame_developed <- bind_rows(
    mms_std |> 
      dplyr::filter(study != "Wimmer et al. (2024)"),
    wimmer_developed
  ) |> dplyr::left_join(
    data |> 
      dplyr::mutate(country_type = if_else(study == "Wimmer et al. (2024)",
                                           "Developed/High income",country_type)) |> 
      dplyr::select(study,country_type) |> 
      unique()) |> 
    dplyr::filter(country_type != "Developing/Low income") |> 
    dplyr::select(-country_type) |> 
    dplyr::group_by(features) |> 
    dplyr::mutate(count = length(unique(study))) |> 
    dplyr::filter(count > 4)
}

{
  metaeffs_developing = frame_developing |> 
    dplyr::filter(!features %in% c("attr_country","attr_continent")) |>
    estimate_meta_effects()
  
  metaeffs_developed = frame_developed |>
    estimate_meta_effects()
 
  metaeffs_region = rbind.data.frame(
    metaeffs_developing$model_estimates |> dplyr::mutate(country_type="Developing country"),
    metaeffs_developed$model_estimates |> dplyr::mutate(country_type="Developed country")
  )
}

write.csv(metaeffs_region, file.path(dir_out, 'meta_effects_std_region.csv'))
metaeffs_region <- read.csv(file.path(dir_out, 'meta_effects_std_region.csv'))

# BY migrant religion: Majority religion at origin

{
  data_christians <- data |> 
    dplyr::filter(resp_religion == "Christian") |> 
    dplyr::group_by(study) |>
    dplyr::mutate(count = n()) |> 
    dplyr::filter(count>=100) |> 
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  
  data_muslims <- data |> 
    dplyr::filter(resp_religion == "Muslim") |> 
    dplyr::group_by(study) |>
    dplyr::mutate(count = n()) |> 
    dplyr::filter(count>=100) |> 
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  
  data_other <- data |> 
    dplyr::filter(!resp_religion %in% c("Christian","Muslim","No faith")) |> 
    dplyr::filter(is.na(resp_religion)==F) |> 
    dplyr::group_by(study) |>
    dplyr::mutate(count = n()) |> 
    dplyr::filter(count>=100) |> 
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  
  data_none <- data |> 
    dplyr::filter(resp_religion=="No faith") |> 
    dplyr::filter(is.na(resp_religion)==F) |> 
    dplyr::group_by(study) |>
    dplyr::mutate(count = n()) |> 
    dplyr::filter(count>=100) |> 
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
}

{
  mm_christian = data_christians %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study") 
  mm_muslim = data_muslims %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_other = data_other %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_none = data_none %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_religresp = rbind.data.frame(
    mm_christian |> dplyr::mutate(resp_religion ="Christians"),
    mm_muslim|> dplyr::mutate(resp_religion="Muslims"),
    mm_other|> dplyr::mutate(resp_religion="Other religions"),
    mm_none|> dplyr::mutate(resp_religion="No religion")
  )
}

{
  metaeffs_christian = mm_christian|>
    dplyr::filter(features == "attr_religion") |> 
    estimate_meta_effects()
  metaeffs_muslim = mm_muslim |>
    dplyr::filter(features == "attr_religion") |> 
    estimate_meta_effects()
  metaeffs_other = mm_other |>
    dplyr::filter(features == "attr_religion") |> 
    estimate_meta_effects()
  metaeffs_none = mm_none |>
    dplyr::filter(features == "attr_religion") |> 
    estimate_meta_effects()
  
  metaeffs_relig = rbind.data.frame(
    metaeffs_christian$model_estimates |> 
      dplyr::mutate(resp_religion ="Christians"),
    metaeffs_muslim$model_estimates |> 
      dplyr::mutate(resp_religion ="Muslims"),
    metaeffs_other$model_estimates |> 
      dplyr::mutate(resp_religion ="Other religions"),
    metaeffs_none$model_estimates |> 
      dplyr::mutate(resp_religion ="No religion")
  )
}

write.csv(mm_religresp, file.path(dir_out, 'study_effects_std_resprelig.csv'))
write.csv(metaeffs_relig, file.path(dir_out, 'meta_effects_std_resprelig.csv'))

mm_resprelig <- read.csv(file.path(dir_out, 'study_effects_std_resprelig.csv'))
metaeffs_resprelig <- read.csv(file.path(dir_out, 'meta_effects_std_resprelig.csv'))

# BY religion match: Migrant's and respondent's religions

{
  data_muslimt_X_muslimr = data |>
    dplyr::filter(!is.na(attr_muslim), !is.na(country_religion)) |>
    dplyr::select(study, choice, attr_muslim, country_religion, resp_id) |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  data_muslimt_X_muslimr_christian = data_muslimt_X_muslimr |>
    dplyr::filter(country_religion=="Christian-majority country")
  data_muslimt_X_muslimr_muslim = data_muslimt_X_muslimr |>
    dplyr::filter(country_religion=="Muslim-majority country")
  data_muslimt_X_muslimr_others = data_muslimt_X_muslimr |>
    dplyr::filter(country_religion=="Country with religious diversity, secular majorities, or other dominating religions")
}

{
  mm_muslimt_X_muslimr_christian = data_muslimt_X_muslimr_christian |>
    dplyr::select(-country_religion) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_muslimt_X_muslimr_muslim = data_muslimt_X_muslimr_muslim |>
    dplyr::select(-country_religion) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_muslimt_X_muslimr_others = data_muslimt_X_muslimr_others |>
    dplyr::select(-country_religion) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_muslimt_X_muslimr = rbind.data.frame(
    mm_muslimt_X_muslimr_christian |> dplyr::mutate(country_religion="Christian-majority country"),
    mm_muslimt_X_muslimr_muslim |> dplyr::mutate(country_religion="Muslim-majority country"),
    mm_muslimt_X_muslimr_others |> dplyr::mutate(country_religion="Country with religious diversity, secular majorities, or other dominating religions")
  )
}

{
  metaeffs_muslimt_X_muslimr_christian = mm_muslimt_X_muslimr_christian |>
    estimate_meta_effects()
  metaeffs_muslimt_X_muslimr_muslim = mm_muslimt_X_muslimr_muslim |>
    estimate_meta_effects()
  metaeffs_muslimt_X_muslimr_others = mm_muslimt_X_muslimr_others |>
    estimate_meta_effects()
  metaeffs_muslimt_X_muslimr = rbind.data.frame(
    metaeffs_muslimt_X_muslimr_christian$model_estimates |> dplyr::mutate(country_religion="Christian-majority country"),
    metaeffs_muslimt_X_muslimr_muslim$model_estimates |> dplyr::mutate(country_religion="Muslim-majority country"),
    metaeffs_muslimt_X_muslimr_others$model_estimates |> dplyr::mutate(country_religion="Country with religious diversity, secular majorities, or other dominating religions")
  )
}

write.csv(mm_muslimt_X_muslimr, file.path(dir_out, 'study_effects_std_muslimt_X_muslimr.csv'))
write.csv(metaeffs_muslimt_X_muslimr, file.path(dir_out, 'meta_effects_std_muslimt_X_muslimr.csv'))

mm_muslimt_X_muslimr <- read.csv(file.path(dir_out, 'study_effects_std_muslimt_X_muslimr.csv'))
metaeffs_muslimt_X_muslimr <- read.csv(file.path(dir_out, 'meta_effects_std_muslimt_X_muslimr.csv'))

# BY migrant profile: Migrant's religion + occupation

{
  data_religt_X_occupt = data |>
    dplyr::filter(!is.na(attr_religion), !is.na(attr_occupation)) |>
    dplyr::select(study, choice, attr_religion, attr_occupation, resp_id) |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  
  data_religt_X_occupt_professional = data_religt_X_occupt |>
    dplyr::filter(attr_occupation=="Professional occupation")
  data_religt_X_occupt_workerfarmer = data_religt_X_occupt |>
    dplyr::filter(attr_occupation=="Worker/Farmer")
  data_religt_X_occupt_unemployed = data_religt_X_occupt |>
    dplyr::filter(attr_occupation=="Unemployed")
}

{
  mm_religt_X_occupt_professional = data_religt_X_occupt_professional |>
    dplyr::select(-attr_occupation) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_religt_X_occupt_workerfarmer = data_religt_X_occupt_workerfarmer |>
    dplyr::select(-attr_occupation) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_religt_X_occupt_unemployed = data_religt_X_occupt_unemployed |>
    dplyr::select(-attr_occupation) %>% 
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  
  mm_religt_X_occupt = rbind.data.frame(
    mm_religt_X_occupt_professional |> dplyr::mutate(attr_occupation="Professional occupation"),
    mm_religt_X_occupt_workerfarmer |> dplyr::mutate(attr_occupation="Worker/Farmer"),
    mm_religt_X_occupt_unemployed |> dplyr::mutate(attr_occupation="Unemployed")
  )
}

{
  metaeffs_religt_X_occupt_professional = mm_religt_X_occupt_professional |>
    estimate_meta_effects()
  metaeffs_religt_X_occupt_workerfarmer = mm_religt_X_occupt_workerfarmer |>
    estimate_meta_effects()
  metaeffs_religt_X_occupt_unemployed = mm_religt_X_occupt_unemployed |>
    estimate_meta_effects()
  metaeffs_religt_X_occupt = rbind.data.frame(
    metaeffs_religt_X_occupt_professional$model_estimates |> dplyr::mutate(attr_occupation="Professional occupation"),
    metaeffs_religt_X_occupt_workerfarmer$model_estimates |> dplyr::mutate(attr_occupation="Worker/Farmer"),
    metaeffs_religt_X_occupt_unemployed$model_estimates |> dplyr::mutate(attr_occupation="Unemployed")
  )
}

write.csv(mm_religt_X_occupt, file.path(dir_out, 'study_effects_std_religt_X_occupt.csv'))
write.csv(metaeffs_religt_X_occupt, file.path(dir_out, 'meta_effects_std_religt_X_occupt.csv'))

mm_religt_X_occupt <- read.csv(file.path(dir_out, 'study_effects_std_religt_X_occupt.csv'))
metaeffs_religt_X_occupt <- read.csv(file.path(dir_out, 'meta_effects_std_religt_X_occupt.csv'))

# BY outcome type: Admission, residency or others

{
  frame_admission <- 
    dplyr::left_join(
      mms_std, data |>
        dplyr::select(study,outcome) |> 
        unique()) |> 
    dplyr::filter(outcome == "Admission") |> 
    dplyr::select(-outcome) |> 
    dplyr::filter(
      features %in% c(
        "attr_occupation","attr_religion", "attr_reason","attr_language",
        "attr_region_match", "attr_north_south","attr_gender","attr_education","attr_educ_match"))
  frame_citizenship <- 
    dplyr::left_join(
      mms_std, data |>
        dplyr::select(study,outcome) |> 
        unique()) |> 
    dplyr::filter(outcome == "Citizenship"| outcome == "Permanent residence") |> 
    dplyr::select(-outcome) |> 
    dplyr::filter(
      features %in% c(
        "attr_occupation","attr_religion", "attr_reason","attr_language",
        "attr_region_match", "attr_north_south","attr_gender","attr_education","attr_educ_match"))
  
  frame_other <- dplyr::left_join(
    mms_std, data |> 
      dplyr::select(study,outcome) |> 
      unique()) |> 
    dplyr::filter(!outcome %in% c("Admission","Permanent residence","Citizenship")) |> 
    dplyr::select(-outcome)|> 
    dplyr::filter(
      features %in% c("attr_occupation","attr_region_match", "attr_north_south",
                      "attr_gender","attr_education","attr_educ_match",
                      "attr_reason","attr_language","attr_religion"))
}

{
  metaeffs_admission = frame_admission|>
    estimate_meta_effects()
  metaeffs_citizenship = frame_citizenship|>
    estimate_meta_effects()
  metaeffs_other = frame_other |>
    estimate_meta_effects()
  metaeffs_outcome = rbind.data.frame(
    metaeffs_admission$model_estimates |> dplyr::mutate(outcome="Admission"),
    metaeffs_citizenship$model_estimates |> dplyr::mutate(outcome="Citizenship/Residency"),
    metaeffs_other$model_estimates |> dplyr::mutate(outcome="Other outcomes")
  )
}

write.csv(metaeffs_outcome, file.path(dir_out, 'meta_effects_std_outcome.csv'))
metaeffs_outcome <- read.csv(file.path(dir_out, 'meta_effects_std_outcome.csv'))

# BY experiment type: Conjoint or others

{
  data_conjoint = data |>
    dplyr::filter(experiment == "Conjoint") |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
  data_noconjoint = data |>
    dplyr::filter(experiment != "Conjoint") |>
    dplyr::group_by(study) |>
    dplyr::mutate(choice = scale(choice, center = TRUE, scale = TRUE)[,1]) |>
    dplyr::ungroup()
}

{
  mm_conjoint = data_conjoint %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  mm_noconjoint = data_noconjoint %>%
    split(.$study) |>
    purrr::map(estimate_marginal_means) |>
    dplyr::bind_rows(.id = "study")
  
  mm_experiment = rbind.data.frame(
    mm_conjoint |> dplyr::mutate(experiment ="Conjoint experiments"),
    mm_noconjoint |> dplyr::mutate(experiment="Other types of experiments")
  )
}

{
  metaeffs_conjoint = mm_conjoint |>
    dplyr::filter(study!='Spilker et al. (2020)') |> 
    estimate_meta_effects()
  metaeffs_noconjoint = mm_noconjoint |>
    estimate_meta_effects()
  metaeffs_experiment = rbind.data.frame(
    metaeffs_conjoint$model_estimates |> dplyr::mutate(experiment ="Conjoint experiments"),
    metaeffs_noconjoint$model_estimates |> dplyr::mutate(experiment="Other types of experiments")
  )
}

write.csv(mm_experiment, file.path(dir_out, 'study_effects_std_exptype.csv'))
write.csv(metaeffs_experiment, file.path(dir_out, 'meta_effects_std_exptype.csv'))

mm_experiment <- read.csv(file.path(dir_out, 'study_effects_std_exptype.csv'))
metaeffs_experiment <- read.csv(file.path(dir_out, 'meta_effects_std_exptype.csv'))

# CLEANUP ----

rm(list = ls())
gc()

# END SCRIPT

