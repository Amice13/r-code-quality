# Preliminary ----

if (!require(pacman)) install.packages("pacman") # install pacman to install any required packages

pacman::p_load(tidyverse, haven, labelled, modelsummary, fixest, kableExtra, panelr, lubridate, here, metafor)

options(modelsummary_factory_default = 'kableExtra') 
options(modelsummary_factory_latex = 'kableExtra') 
options(modelsummary_factory_html = 'kableExtra')

setFixest_vcov(no_FE = "iid", one_FE = "iid", two_FE = "iid")

options(scipen = 100)
options(digits = 2) 

basedir <- here()

setwd(basedir)

dir.create("outputs", showWarnings = FALSE)

# Read data ----

panel <- readRDS("data/BESIP_clean.rds")

data1 <- readRDS("data/data1.rds")
data2 <- readRDS("data/data2.rds")
data3 <- readRDS("data/data3.rds")

# Analysis ----

## Main text

### Table 2 ----

# the following analysis steps are used to extract coefficients from multiple models and average together using the `metafor' package. 

coefficients_df <- data.frame(group_name = character(),
                              measure = character(),
                              outcome = character(),
                              yi = numeric(),
                              sei = numeric(),
                              stringsAsFactors = FALSE)

add_coefficients <- function(model, variable_name, group_name, measure, outcome, coefficients_df) {

  coefficients <- coef(model)
  se <- sqrt(diag(vcov(model)))
  
  variable_index <- grep(variable_name, names(coefficients))
  
  if (length(variable_index) > 0) {
    coefficient <- coefficients[variable_index]
    standard_error <- se[variable_index]
    
    result_row <- data.frame(group_name = group_name,
                             measure = measure,
                             outcome = outcome,
                             yi = coefficient,
                             sei = standard_error)
    
    coefficients_df <- rbind(coefficients_df, result_row)
  }
  
  return(coefficients_df)
}

#### Vote (group-level) ----
region_obj_vot <- panel |> 
  feols(ptvConW ~ self_region_std + ego_std + socio_std | id + wave, vcov = ~id)
variable_name <- "self_region_std"
group_name <- "region"
measure <- "objective"
outcome <- "vote"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

class_obj_vot <- panel |> 
  feols(ptvConW ~ self_class_std + ego_std + socio_std | id + wave, vcov = ~id)
variable_name <- "self_class_std"
group_name <- "class"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

#### Vote (individual-level) ----
region_subj_vot <- panel |> 
  feols(ptvConW ~ regionEconW_std + ego_std + socio_std | id + wave, vcov = ~id)
variable_name <- "regionEconW_std"
group_name <- "region"
measure <- "subjective"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)

local_subj_vot <- panel |> 
  feols(ptvConW ~ localEconW_std + ego_std + socio_std | id + wave, vcov = ~id)
variable_name <- "localEconW_std"
group_name <- "local"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)

class_subj_vot <- panel |> 
  feols(ptvConW ~ classEcon_std + ego_std + socio_std | id + wave, vcov = ~id)
variable_name <- "classEcon_std"
group_name <- "class"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)

#### Combining estimates ----
vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote"))
vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote"))
socio_vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote" & group_name=="nation"))
socio_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="nation"))
ego_vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote" & group_name=="self"))
ego_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="self"))

get_estimates(vote_obj)
get_estimates(vote_subj)
get_estimates(socio_vote_subj)

socioegolist <- list(
  "socio_vote_obj" = socio_vote_obj,
  "socio_vote_subj" = socio_vote_subj,
  "ego_vote_obj" = ego_vote_obj,
  "ego_vote_subj" = ego_vote_subj)
socioego <- modelsummary(socioegolist, output = "modelsummary_list")

socio_vote_obj_estimate <- socioego[["socio_vote_obj"]][["tidy"]][["estimate"]]
socio_vote_obj_se <- socioego[["socio_vote_obj"]][["tidy"]][["std.error"]]
socio_vote_obj_pval <- socioego[["socio_vote_obj"]][["tidy"]][["p.value"]]

socio_vote_subj_estimate <- socioego[["socio_vote_subj"]][["tidy"]][["estimate"]]
socio_vote_subj_se <- socioego[["socio_vote_subj"]][["tidy"]][["std.error"]]
socio_vote_subj_pval <- socioego[["socio_vote_subj"]][["tidy"]][["p.value"]]

ego_vote_obj_estimate <- socioego[["ego_vote_obj"]][["tidy"]][["estimate"]]
ego_vote_obj_se <- socioego[["ego_vote_obj"]][["tidy"]][["std.error"]]
ego_vote_obj_pval <- socioego[["ego_vote_obj"]][["tidy"]][["p.value"]]

ego_vote_subj_estimate <- socioego[["ego_vote_subj"]][["tidy"]][["estimate"]]
ego_vote_subj_se <- socioego[["ego_vote_subj"]][["tidy"]][["std.error"]]
ego_vote_subj_pval <- socioego[["ego_vote_subj"]][["tidy"]][["p.value"]]

#### Table 2 output ----

main_observational <- list(
  "(1)" = vote_obj,
  "(2)" = vote_subj,
  "(3)" = panel |>
    filter(subjclassW!=0) |>
    filter(is.na(ptvConW) == FALSE) |> 
    feols(concern ~ self_class_std + ego_std + socio_std | id + wave, vcov = ~id),
  "(4)" = panel |>
    filter(subjclassW!=0) |>
    filter(is.na(self_class_std) == FALSE) |> 
    feols(ptvConW ~ concern_std + ego_std + socio_std | id + wave, vcov = ~id)
)

total_n <- as.character(sapply(main_observational, function(x) x$nobs))
individual_fes <- as.character(sapply(main_observational, function(x) x$nparams))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS, 
                'TWFE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark',
                'Clustered SEs', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', 
                'Wave N', '18', '4', '6', '6', 
                'Individual FEs', '91125', '20406', '37493', '37493'
)

attr(rows, 'position') <- c(7,8,9,10)

main_observational_tab <- modelsummary(main_observational, output = "modelsummary_list")

main_observational_tab[["(1)"]][["tidy"]][["term"]] <- paste('obj')
main_observational_tab[["(2)"]][["tidy"]][["term"]] <- paste('subj')

main_observational_tab[["(1)"]][["glance"]][["nobs"]] <- paste('354304')
main_observational_tab[["(2)"]][["glance"]][["nobs"]] <- paste('22352')

additional_terms <- c("socio_std", "ego_std")
tidy_section <- main_observational_tab[["(1)"]][["tidy"]]
new_rows <- data.frame(
  term = rep(additional_terms, each = 1),
  type = rep("summary", length(additional_terms)),
  estimate = c(socio_vote_obj_estimate, ego_vote_obj_estimate),
  std.error = c(socio_vote_obj_se, ego_vote_obj_se),
  statistic = rep(NA, length(additional_terms)),
  p.value = c(socio_vote_obj_pval, ego_vote_obj_pval)
)

tidy_section <- select(tidy_section, -conf.low, -conf.high)
updated_tidy_section <- rbind(tidy_section, new_rows)
main_observational_tab[["(1)"]][["tidy"]] <- updated_tidy_section

tidy_section <- main_observational_tab[["(2)"]][["tidy"]]
new_rows <- data.frame(
  term = rep(additional_terms, each = 1),
  type = rep("summary", length(additional_terms)),
  estimate = c(socio_vote_subj_estimate, ego_vote_subj_estimate),
  std.error = c(socio_vote_subj_se, ego_vote_subj_se),
  statistic = rep(NA, length(additional_terms)),
  p.value = c(socio_vote_subj_pval, ego_vote_subj_pval)
)
tidy_section <- select(tidy_section, -conf.low, -conf.high)
updated_tidy_section <- rbind(tidy_section, new_rows)
main_observational_tab[["(2)"]][["tidy"]] <- updated_tidy_section

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Total N",         0)

modelsummary(main_observational_tab,
             output = "outputs/table_2.tex",
                             fmt = 2,
                             booktabs = TRUE,
                             estimate = "{estimate} ({std.error}){stars}",
                             stars = c("+" = 0.1, "**" = 0.01, "*" = 0.05),
                             statistic = NULL,
                             coef_map = c("obj" = "Group performance (grp.-level)",
                                          "subj" = "Group performance (ind.-level)",
                                          "self_class_std" = "Class performance (grp.-level)",
                                          "concern_std" = "Class concern",
                                          "ego_std" = "Egotropic evaluations",
                                          "socio_std" = "Sociotropic evaluations"),
                             gof_map = gm,
                             title = "Relationship Between Group Performance, Perceived Group Concern and Incumbent Vote Intention",
                             add_rows = rows) |> 
  add_header_above(c(" ", "Vote intention" = 2, "Class concern" = 1, "Vote intention" = 1)) |> 
  add_header_above(c(" ", "Group-based retrospection (pooled estimates)" = 2, "Test of the mechanism" = 2)) |> 
  footnote(general = "Note: Estimates from two-way fixed effects models of self-reported likelihood of voting for the incumbent party. All independent variables are standardized. Coefficients in columns 1 and 2 are pooled estimates from several underlying models, one per group type, using simple meta-regression models with equal weights (see Appendix A (p. 1) for unpooled models). For these meta-regressions in columns 1 and 2, the total N and individual FE statistics show the minimum numbers, as they vary slightly between underlying models. † p<.10; * p<.05; ** p<.01.", 
           threeparttable = TRUE) |>
  kable_styling(font_size = 10.5, full_width = FALSE)

### Sample sizes for Table 3 ----

nrow(data1)
nrow(data2)
nrow(data3)

### Table 4 ----

maintable <- list(
  "Econ. satisfaction" = data1 |> 
    feols(q8 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Gov't approval" = data1 |> 
    feols(q9 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Econ. satisfaction" = data2 |>
    feols(EXP2 ~ C*govtecon.pre.m + C*intensity.m + C*socio.pre.m + socio.pre.d + FV22govt | gruppe, subset = data2$byland!=99 & data2$unggam!=99),
  "Econ. satisfaction" = data3 |>
    feols(q25_5 ~ ingroup + stimsize + vs_pidbase | grupper, weights = ~weight))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS,
                'Controls', 'Yes', 'Yes', 'Yes', 'Yes',
                'Clustered SEs', 'Yes', 'Yes', 'No', 'No',
                'In-group FEs', 'Yes', 'Yes', 'Yes', 'Yes',
                'Number of groups', '22', '22', '6', '6')

attr(rows, 'position') <- c(5, 6, 7, 8)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(maintable, 
             output = "outputs/table_4.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "Relative in-group decline",
                          "C" = "Relative in-group decline",
                          "ingroup1" = "Relative in-group improvement"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline and Improvement",
             add_rows = rows,
             threeparttable = TRUE,
             notes = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment (in-group performance vs out-group performance information (ref.); with national benchmark) with in-group fixed effects and design controls. Standard errors clustered at the in-group level in Experiment 1. * p<.05, ** p<.01, *** p<.001.") |> 
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1, "Experiment 3" = 1)) |>
  kable_styling(font_size = 10.5, full_width = FALSE)

### Table 5 ----

pocket <- list(
  "Without national benchmark" = data1 |>
    feols(q12 ~ own_abs + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "With national benchmark" = data1 |>
    feols(q12 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1)
)

rows <- tribble(~term, ~OLS, ~OLS,
                'Clustered SEs', 'Yes', 'Yes',
                'In-group FEs', 'Yes', 'Yes',
                'Number of groups', '22', '22')

attr(rows, 'position') <- c(3, 4, 5)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(pocket, 
             output = "outputs/table_5.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "In-group decline",
                          "own_abs1" = "In-group decline"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline on Prospective Pocketbook Evaluations",
             add_rows = rows,
             threeparttable = TRUE,
             notes = "Regression of prospective pocketbook evaluations on treatment (in-group decline vs out-group decline information (ref.); with and without national benchmark). Model specifications are identical to those in Table 4. Standard errors clustered at the in-group level. * p<.05, ** p<.01, *** p<.001.") |> 
  kable_styling(full_width = FALSE)

## Appendix ----
### Table A1 ----

voting <- list(
  "Region (grp.-level)" = panel |> 
    feols(ptvConW ~ self_region_std + ego_std + socio_std | id + wave, vcov = ~id),
  "Class (grp.-level)" = panel |> 
    feols(ptvConW ~ self_class_std + ego_std + socio_std | id + wave, vcov = ~id),
  "Region (ind.-level)" = panel |> 
    feols(ptvConW ~ regionEconW_std + ego_std + socio_std | id + wave, vcov = ~id),
  "Local community (ind.-level)" = panel |> 
    feols(ptvConW ~ localEconW_std + ego_std + socio_std | id + wave, vcov = ~id),
  "Class (ind.-level)" = panel |> 
    feols(ptvConW ~ classEcon_std + ego_std + socio_std | id + wave, vcov = ~id)
)

total_n <- as.character(sapply(voting, function(x) x$nobs))
individual_fes <- as.character(sapply(voting, function(x) x$nparams))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS,
                'TWFE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                'Clustered SEs', 'Individual', 'Individual', 'Individual', 'Individual', 'Individual',
                'Wave N', '18', '18', '4', '4', '4',
                'Total N', total_n[1], total_n[2], total_n[3], total_n[4], total_n[5],
                'Individual FEs', individual_fes[1], individual_fes[2], individual_fes[3], individual_fes[4], individual_fes[5])

modelsummary(voting,
             output = "outputs/table_A1.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("self_region_std" = "Group performance",
                          "regionEconW_std" = "Group performance",
                          "localEconW_std" = "Group performance",
                          "self_class_std" = "Group performance",
                          "classEcon_std" = "Group performance",
                          "ego_std" = "Egotropic evaluations",
                          "socio_std" = "Sociotropic evaluations"),
             gof_map = NA,
             escape = T,
             title = "Relationship Between Group Performance and Incumbent Vote Intention, Unpooled Models by Group Type",
             add_rows = rows) |> 
  footnote(general = "Estimates from two-way fixed effects (TWFE) models of self-reported likelihood of voting for the incumbent party. Models split by group type. All independent variables are standardized. * p<.05, ** p<.01, *** p<.001.", #The dependent variable, incumbent vote intention, is on a 10-point scale. All independent variables are standardized. Standard errors are clustered at the individual level (in parentheses).
           threeparttable = TRUE) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), 
                font_size = 10) |>
  column_spec(1:1, width = "2.8cm") |>
  column_spec(2:6, width = "2.3cm")

### Table B1 ----

coefficients_df <- data.frame(group_name = character(),
                              measure = character(),
                              outcome = character(),
                              yi = numeric(),
                              sei = numeric(),
                              stringsAsFactors = FALSE)

#### Vote (group-level) ----
region_obj_vot <- panel |> 
  feols(ptvConW ~ self_region_std + ego_std + socio_std + cutsTooFarNationalW + leftRightW | id + wave, vcov = ~id)
variable_name <- "self_region_std"
group_name <- "region"
measure <- "objective"
outcome <- "vote"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "cutsTooFarNationalW"
group_name <- "cuts"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "leftRightW"
group_name <- "lr"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

class_obj_vot <- panel |> 
  feols(ptvConW ~ self_class_std + ego_std + socio_std + cutsTooFarNationalW + leftRightW | id + wave, vcov = ~id)
variable_name <- "self_class_std"
group_name <- "class"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(class_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "cutsTooFarNationalW"
group_name <- "cuts"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "leftRightW"
group_name <- "lr"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

#### Vote (individual-level) ----
region_subj_vot <- panel |> 
  feols(ptvConW ~ regionEconW_std + ego_std + socio_std + leftRightW | id + wave, vcov = ~id)
variable_name <- "regionEconW_std"
group_name <- "region"
measure <- "subjective"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(region_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "leftRightW"
group_name <- "lr"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

local_subj_vot <- panel |> 
  feols(ptvConW ~ localEconW_std + ego_std + socio_std + leftRightW | id + wave, vcov = ~id)
variable_name <- "localEconW_std"
group_name <- "local"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(local_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "leftRightW"
group_name <- "lr"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

class_subj_vot <- panel |> 
  feols(ptvConW ~ classEcon_std + ego_std + socio_std + leftRightW | id + wave, vcov = ~id)
variable_name <- "classEcon_std"
group_name <- "class"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "socio_std"
group_name <- "nation"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "ego_std"
group_name <- "self"
coefficients_df <- add_coefficients(class_subj_vot, variable_name, group_name, measure, outcome, coefficients_df)
variable_name <- "leftRightW"
group_name <- "lr"
coefficients_df <- add_coefficients(region_obj_vot, variable_name, group_name, measure, outcome, coefficients_df)

#### Combining estimates ----
vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote"))
vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote"))
socio_vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote" & group_name=="nation"))
socio_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="nation"))
ego_vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote" & group_name=="self"))
ego_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="self"))
lr_vote_subj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="subjective" & outcome=="vote" & group_name=="lr"))
lr_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="lr"))
cuts_vote_obj <- rma(yi, sei=sei, data=coefficients_df, test="z", method="EE", subset=(measure=="objective" & outcome=="vote" & group_name=="cuts"))

get_estimates(vote_obj)
get_estimates(vote_subj)
get_estimates(cuts_vote_obj)

socioegolist <- list(
  "socio_vote_obj" = socio_vote_obj,
  "socio_vote_subj" = socio_vote_subj,
  "ego_vote_obj" = ego_vote_obj,
  "ego_vote_subj" = ego_vote_subj,
  "lr_vote_obj" = lr_vote_obj,
  "lr_vote_subj" = lr_vote_subj,
  "cuts_vote_obj" = cuts_vote_obj)
socioego <- modelsummary(socioegolist, output = "modelsummary_list")

socio_vote_obj_estimate <- socioego[["socio_vote_obj"]][["tidy"]][["estimate"]]
socio_vote_obj_se <- socioego[["socio_vote_obj"]][["tidy"]][["std.error"]]
socio_vote_obj_pval <- socioego[["socio_vote_obj"]][["tidy"]][["p.value"]]

socio_vote_subj_estimate <- socioego[["socio_vote_subj"]][["tidy"]][["estimate"]]
socio_vote_subj_se <- socioego[["socio_vote_subj"]][["tidy"]][["std.error"]]
socio_vote_subj_pval <- socioego[["socio_vote_subj"]][["tidy"]][["p.value"]]

ego_vote_obj_estimate <- socioego[["ego_vote_obj"]][["tidy"]][["estimate"]]
ego_vote_obj_se <- socioego[["ego_vote_obj"]][["tidy"]][["std.error"]]
ego_vote_obj_pval <- socioego[["ego_vote_obj"]][["tidy"]][["p.value"]]

ego_vote_subj_estimate <- socioego[["ego_vote_subj"]][["tidy"]][["estimate"]]
ego_vote_subj_se <- socioego[["ego_vote_subj"]][["tidy"]][["std.error"]]
ego_vote_subj_pval <- socioego[["ego_vote_subj"]][["tidy"]][["p.value"]]

lr_vote_obj_estimate <- socioego[["lr_vote_obj"]][["tidy"]][["estimate"]]
lr_vote_obj_se <- socioego[["lr_vote_obj"]][["tidy"]][["std.error"]]
lr_vote_obj_pval <- socioego[["lr_vote_obj"]][["tidy"]][["p.value"]]

lr_vote_subj_estimate <- socioego[["lr_vote_subj"]][["tidy"]][["estimate"]]
lr_vote_subj_se <- socioego[["lr_vote_subj"]][["tidy"]][["std.error"]]
lr_vote_subj_pval <- socioego[["lr_vote_subj"]][["tidy"]][["p.value"]]

cuts_vote_obj_estimate <- socioego[["cuts_vote_obj"]][["tidy"]][["estimate"]]
cuts_vote_obj_se <- socioego[["cuts_vote_obj"]][["tidy"]][["std.error"]]
cuts_vote_obj_pval <- socioego[["cuts_vote_obj"]][["tidy"]][["p.value"]]

#### Table output ----

main_observational <- list(
  "(1)" = vote_obj,
  "(2)" = vote_subj
)

total_n <- as.character(sapply(main_observational, function(x) x$nobs))
individual_fes <- as.character(sapply(main_observational, function(x) x$nparams))

rows <- tribble(~term, ~OLS, ~OLS, 
                'TWFE', '\\checkmark', '\\checkmark', 
                'Clustered SEs', '\\checkmark', '\\checkmark', 
                'Wave N', '8', '4', 
                'Individual FEs', '49294', '17839',
)

attr(rows, 'position') <- c(7,8,9,10)

main_observational_tab <- modelsummary(main_observational, output = "modelsummary_list")

main_observational_tab[["(1)"]][["tidy"]][["term"]] <- paste('obj')
main_observational_tab[["(2)"]][["tidy"]][["term"]] <- paste('subj')

main_observational_tab[["(1)"]][["glance"]][["nobs"]] <- paste('354304')
main_observational_tab[["(2)"]][["glance"]][["nobs"]] <- paste('22352')

additional_terms <- c("socio_std", "ego_std", "cuts_ideoctrl_std", "lr_ideoctrl_std")

tidy_section <- main_observational_tab[["(1)"]][["tidy"]]
new_rows <- data.frame(
  term = rep(additional_terms, each = 1),
  type = rep("summary", length(additional_terms)),
  estimate = c(socio_vote_obj_estimate, ego_vote_obj_estimate, cuts_vote_obj_estimate, lr_vote_obj_estimate),
  std.error = c(socio_vote_obj_se, ego_vote_obj_se, cuts_vote_obj_se, lr_vote_obj_se),
  statistic = rep(NA, length(additional_terms)),
  p.value = c(socio_vote_obj_pval, ego_vote_obj_pval, cuts_vote_obj_pval, lr_vote_obj_pval)
)

tidy_section <- select(tidy_section, -conf.low, -conf.high)
updated_tidy_section <- rbind(tidy_section, new_rows)
main_observational_tab[["(1)"]][["tidy"]] <- updated_tidy_section

tidy_section <- main_observational_tab[["(2)"]][["tidy"]]
new_rows <- data.frame(
  term = rep(additional_terms, each = 1),
  type = rep("summary", length(additional_terms)),
  estimate = c(socio_vote_subj_estimate, ego_vote_subj_estimate, NA, lr_vote_subj_estimate),
  std.error = c(socio_vote_subj_se, ego_vote_subj_se, NA, lr_vote_subj_se),
  statistic = rep(NA, length(additional_terms)),
  p.value = c(socio_vote_subj_pval, ego_vote_subj_pval, NA, lr_vote_subj_pval)
)
tidy_section <- select(tidy_section, -conf.low, -conf.high)
updated_tidy_section <- rbind(tidy_section, new_rows)
main_observational_tab[["(2)"]][["tidy"]] <- updated_tidy_section

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Total N",         0)

modelsummary(main_observational_tab,
             output = "outputs/table_B1.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate} ({std.error}){stars}",
             stars = c("+" = 0.1, "**" = 0.01, "*" = 0.05),
             statistic = NULL,
             coef_map = c("obj" = "Group performance (grp.-level)",
                          "subj" = "Group performance (ind.-level)",
                          "cuts_ideoctrl_std" = "Austerity too far",
                          "lr_ideoctrl_std" = "Left-right placement",
                          "ego_std" = "Egotropic evaluations",
                          "socio_std" = "Sociotropic evaluations"),
             gof_map = gm,
             title = "Relationship Between Group Performance and Incumbent Vote Intention, With Ideology Controls",
             add_rows = rows) |> 
  add_header_above(c(" ", "Group-based retrospection (pooled estimates)" = 2)) |> 
  footnote(general = "Note: Estimates from two-way fixed effects models. The dependent variable, incumbent vote intention, is on a 10-point scale. † p<.10; * p<.05; ** p<.01.", 
           threeparttable = TRUE) |>
  kable_styling(font_size = 10.5, full_width = FALSE)

### Table B2 ----
# create copies of variables to distinguish them in coef_map
panel$regionEconW_std_ctrl <- panel$regionEconW_std
panel$localEconW_std_ctrl <- panel$localEconW_std
panel$classEcon_std_ctrl <- panel$classEcon_std

votewithgroupcontrols <- list(
  "No controls" = panel |> 
    feols(ptvConW ~ regionEconW_std + ego_std + socio_std | id + wave, vcov = ~id),
  "With controls" = panel |> 
    feols(ptvConW ~ regionEconW_std + ego_std + socio_std + classEcon_std_ctrl | id + wave, vcov = ~id),
  "No controls" = panel |> 
    feols(ptvConW ~ localEconW_std + ego_std + socio_std | id + wave, vcov = ~id),
  "With controls" = panel |> 
    feols(ptvConW ~ localEconW_std + ego_std + socio_std + classEcon_std_ctrl | id + wave, vcov = ~id),
  "No controls" = panel |> 
    feols(ptvConW ~ classEcon_std + ego_std + socio_std | id + wave, vcov = ~id),
  "With controls" = panel |> 
    feols(ptvConW ~ classEcon_std + ego_std + socio_std + regionEconW_std_ctrl + localEconW_std_ctrl | id + wave, vcov = ~id)
)

#get_model_info("ptvConW ~ regionEconW_std + ego_std + socio_std | id + wave")
#get_model_info("ptvConW ~ regionEconW_std + ego_std + socio_std + classEcon_std_ctrl | id + wave")
#get_model_info("ptvConW ~ localEconW_std + ego_std + socio_std | id + wave")
#get_model_info("ptvConW ~ localEconW_std + ego_std + socio_std + classEcon_std_ctrl | id + wave")
#get_model_info("ptvConW ~ classEcon_std + ego_std + socio_std | id + wave")
#get_model_info("ptvConW ~ classEcon_std + ego_std + socio_std + regionEconW_std_ctrl + localEconW_std_ctrl | id + wave")

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS,
                'TWFE', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes',
                'Clustered SEs', 'Individual', 'Individual', 'Individual', 'Individual', 'Individual', 'Individual',
                'Wave N', '4', '4', '4', '4', '4', '4',
                'Total N', '40023','21312','39557','21086','22352','20551',
                'Individual FEs', '33325','19510','33010','19310','20400','18859')

modelsummary(votewithgroupcontrols,
             output = "outputs/table_B2.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("regionEconW_std" = "Group performance (grp.-level)",
                          "localEconW_std" = "Group performance (grp.-level)",
                          "classEcon_std" = "Group performance (grp.-level)",
                          "regionEconW_std_ctrl" = "Region performance (grp.-level)",
                          "localEconW_std_ctrl" = "Local performance (grp.-level)",
                          "classEcon_std_ctrl" = "Class performance (grp.-level)",
                          "ego_std" = "Egotropic evaluations",
                          "socio_std" = "Sociotropic evaluations"),
             gof_map = NA,
             escape = T,
             title = "Relationship Between Group Performance and Incumbent Vote Intention, With Controls for Other Group Perceptions",
             add_rows = rows) |> 
  footnote(general = "Estimates from two-way fixed effects (TWFE) models. The dependent variable, incumbent vote intention, is on a 10-point scale. + p<.10 * p<.05, ** p<.01, *** p<.001.", threeparttable = TRUE) |> #All independent variables are standardized. Standard errors are clustered at the individual level (in parentheses).
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 10) |>
  column_spec(1:1, width = "2.8cm") |>
  column_spec(2:6, width = "1.9cm") |>
  add_header_above(c(" ", "Region (grp.-level)" = 2, "Local community (grp.-level)" = 2, "Class (grp.-level)" = 2)) |>
  kable_styling(full_width = FALSE)

### Table B3 ----

voting <- list(
  "Bivariate TWFE" = panel |> 
    feols(ptvConW ~ unemployment_rate | id + wave, vcov = ~id),
  "W. controls" = panel |> 
    feols(ptvConW ~ unemployment_rate + socio_std + ego_std | id + wave, vcov = ~id),
  "Lagged IV" = panel |> 
    feols(ptvConW ~ l(unemployment_rate, 1) + socio_std + ego_std | id + wave, panel.id = ~id + wave, vcov = ~id)
)

rows <- tribble(~term, ~OLS, ~OLS, ~OLS,
                'TWFE', 'Yes', 'Yes', 'Yes',
                'Clustered SEs', 'Yes', 'Yes', 'Yes',
                'Wave N', '21', '18', '17',
                'Individual FEs', '90706', '87808', '85300')

#voting[["1"]][["fixef_sizes"]][["id"]]
#voting[["1"]][["fixef_sizes"]][["wave"]]

attr(rows, 'position') <- c(9,10,11,12)

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Total N",         0)

modelsummary(voting,
             output = "outputs/table_B3.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("unemployment_rate" = "Regional unemployment",
                          "l(unemployment_rate, 1)" = "Regional unemployment (lag)",
                          "ego_std" = "Egotropic evaluations",
                          "socio_std" = "Sociotropic evaluations"),
             gof_map = gm,
             escape = T,
             title = "Relationship Between Regional Unemployment and Incumbent Vote Intention",
             add_rows = rows) |> 
  #add_header_above(c(" ", "Group-based retrospection (pooled estimates)" = 2)) |> 
  footnote(general = "Estimates from two-way fixed effects (TWFE) models. The dependent variable, incumbent vote intention, is on a 10-point scale. + p<.10 * p<.05, ** p<.01, *** p<.001.", #Regional unemployment is the current (or lagged) monthly unemployment rate in the respondent's region. Other independent variables are standardized. Standard errors are clustered at the individual level (in parentheses).
           threeparttable = TRUE) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 10, full_width = FALSE)


### Table D1 ----

manipcheck <- list(
  "Relative decline treatment" = data1 |> 
    filter(q14 < 4) |> 
    feols(q14 ~ own_rel + intensity + treattype),
  "Absolute decline treatment" = data1 |> 
    filter(q14 < 4) |> 
    feols(q14 ~ own_abs + intensity + treattype),
  "Relative decline treatment" = data2 |>
    filter(manipcheck<99) |>
    feols(manipcheck ~ C + intensity)
)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(manipcheck,
             output = "outputs/table_D1.tex",
             fmt = 2, # "%.3f"
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "Treatment",
                          "own_abs1" = "Treatment",
                          "C" = "Treatment",
                          "intensity" = "Treatment intensity"),
             gof_map = gof_map,
             escape = T,
             title = "Manipulation Checks") |> 
  footnote(general = "The dependent variable is perceived economic situation of the group they got negative information about in the stimulus (3-point scale). The reference is the average perception for control subjects receiving no group-level information. + p<.10 * p<.05, ** p<.01, *** p<.001. ", threeparttable = TRUE) |> #Standard errors clustered at the stimulus-group level (in parentheses).
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1)) |>
  kable_styling(font_size = 11, latex_options = c("hold_position", "scale_down")) |>
  column_spec(1:1, width = "3cm") |>
  column_spec(2:4, width = "3cm")

# Calculating the effect of going from minimum to maximum intensity in Experiment 1
modelsummary(manipcheck,
             output = "default",
             fmt = 4, # "%.3f"
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "Treatment",
                          "own_abs1" = "Treatment",
                          "C" = "Treatment",
                          "intensity" = "Treatment intensity"),
             gof_map = gof_map,
             escape = T,
             title = "Manipulation Checks") |> 
  footnote(general = "The dependent variable is perceived economic situation of the group they got negative information about in the stimulus (3-point scale). The reference is the average perception for control subjects receiving no group-level information. + p<.10 * p<.05, ** p<.01, *** p<.001. ", threeparttable = TRUE) |> #Standard errors clustered at the stimulus-group level (in parentheses).
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1)) |>
  kable_styling(font_size = 11, latex_options = c("hold_position", "scale_down")) |>
  column_spec(1:1, width = "3cm") |>
  column_spec(2:4, width = "3cm")

coef <- -0.0051
yscalesteps <- 2
pctpoints <- coef/yscalesteps*100
xscalesteps <- 93-3 # as per table J1
pctpoints*xscalesteps

### Table H1 ----

maintable <- list(
  "Econ. satisfaction" = data1 |> 
    feols(q8 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Govt. approval" = data1 |> 
    feols(q9 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Econ. satisfaction" = data2 |>
    feols(EXP2 ~ C*govtecon.pre.m + C*intensity.m + C*socio.pre.m + socio.pre.d + FV22govt | gruppe, subset = data2$byland!=99 & data2$unggam!=99),
  "Econ. satisfaction" = data3 |>
    feols(q25_5 ~ ingroup + stimsize + vs_pidbase | grupper, weights = ~weight))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS,
                'Clustered SEs', 'Yes', 'Yes', 'No', 'No',
                'In-group FEs', 'Yes', 'Yes', 'Yes', 'Yes',
                'No. of groups', '22', '22', '6', '6')

attr(rows, 'position') <- c(23, 24, 25)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(maintable,
             output = "outputs/table_H1.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "In-group decline",
                          "C" = "In-group decline",
                          "ingroup1" = "In-group improvement",
                          "intensity" = "Treatment intensity",
                          "intensity.m" = "Treatment intensity",
                          "stimsize" = "Treatment intensity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "socio.pre.m" = "Sociotropic eval's",
                          "redFT1" = "Gov't vote intention",
                          "FV22govt" = "Gov't vote intention",
                          "vs_pidbase2" = "Party ID: Rep.",
                          "vs_pidbase3" = "Party ID: Indep.",
                          "vs_pidbase4" = "Party ID: other"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline and Improvement (Full Results)",
             add_rows = rows) |> 
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1, "Experiment 3" = 1)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' and 'In-group improvement' variables show the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |> #'Treatment intensity' is a continuous indicator for the statistic included in each stimulus. The 'stimulus' coefficients for Experiment 1 encode which type of information is included in the stimulus (ref. is high financial insecurity). For party ID, ref. is the Democratic party. For gov't vote intention, this is the majority 'red bloc' parties for Experiment 1 and the three parties in the centrist coalition for Experiment 2. Standard errors (in parentheses) clustered at the in-group level in Experiment 1, and not clustered in Experiment 2 and 3.
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 10.5) |>
  landscape()

# Converting coefficients to percentage points
modelsummary(maintable,
             output = "default",
             fmt = 4,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "In-group decline",
                          "C" = "In-group decline",
                          "ingroup1" = "In-group improvement",
                          "intensity" = "Treatment intensity",
                          "intensity.m" = "Treatment intensity",
                          "stimsize" = "Treatment intensity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "socio.pre.m" = "Sociotropic eval's",
                          "redFT" = "Gov't vote intention",
                          "FV22govt" = "Gov't vote intention",
                          "vs_pidbase2" = "Party ID: Rep.",
                          "vs_pidbase3" = "Party ID: Indep.",
                          "vs_pidbase4" = "Party ID: other"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline and Improvement (Full Results)",
             add_rows = rows) |> 
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1, "Experiment 3" = 1)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' and 'In-group improvement' variables show the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |> #'Treatment intensity' is a continuous indicator for the statistic included in each stimulus. The 'stimulus' coefficients for Experiment 1 encode which type of information is included in the stimulus (ref. is high financial insecurity). For party ID, ref. is the Democratic party. For gov't vote intention, this is the majority 'red bloc' parties for Experiment 1 and the three parties in the centrist coalition for Experiment 2. Standard errors (in parentheses) clustered at the in-group level in Experiment 1, and not clustered in Experiment 2 and 3.
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 10.5) |>
  landscape()

# convert coefficient from experiment 1 to percentage points
coef <- -0.1187
yscalesteps <- 4
coef/yscalesteps*100 # percentage points

# convert coefficient from experiment 1 to percentage points
coef <- 0.1903
yscalesteps <- 4
coef/yscalesteps*100 # percentage points

### Table H2 ----

maintable <- list(
  "Bivariate" = data1 |> 
    feols(q8 ~ own_rel | q1, vcov = ~q1, subset = data1$q1!=98),
  "No FEs" = data1 |> 
    feols(q8 ~ own_rel + intensity + treattype + q6 + q7 + redFT, vcov = ~q1, subset = data1$q1!=98),  
  "Bivariate" = data1 |> 
    feols(q9 ~ own_rel | q1, vcov = ~q1, subset = data1$q1!=98),
  "No FEs" = data1 |> 
    feols(q9 ~ own_rel + intensity + treattype + q6 + q7 + redFT, vcov = ~q1, subset = data1$q1!=98),
  "Bivariate" = data2 |>
    feols(EXP2 ~ C | gruppe, subset = data2$byland!=99 & data2$unggam!=99),
  "No FEs" = data2 |>
    feols(EXP2 ~ C*govtecon.pre.m + C*intensity.m + C*socio.pre.m + socio.pre.d + FV22govt, subset = data2$byland!=99 & data2$unggam!=99),
  "Bivariate" = data3 |>
    feols(q25_5 ~ ingroup| grupper, weights = ~weight),
  "No FEs" = data3 |>
    feols(q25_5 ~ ingroup + stimsize + vs_pidbase, weights = ~weight))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS,
                'Clustered SEs', 'Yes', 'Yes', 'Yes', 'Yes', 'No', 'No', 'No', 'No', 
                'In-group FEs', 'Yes', 'No', 'Yes', 'No', 'Yes', 'No', 'Yes', 'No',
                'No. of groups', '22', '22', '22', '22', '6', '6', '6', '6')

attr(rows, 'position') <- c(23, 24, 25)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(maintable, 
             output = "outputs/table_H2.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "In-group decline",
                          "C" = "In-group decline",
                          "ingroup1" = "In-group improvement",
                          "intensity" = "Treatment intensity",
                          "intensity.m" = "Treatment intensity",
                          "stimsize" = "Treatment intensity",
                          "treattype1" = "Stimulus: high financial insecurity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "socio.pre.m" = "Sociotropic eval's",
                          "redFT" = "Gov't vote intention",
                          "FV22govt" = "Gov't vote intention",
                          "vs_pidbase2" = "Party ID: Rep.",
                          "vs_pidbase3" = "Party ID: Indep.",
                          "vs_pidbase4" = "Party ID: other"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline and Improvement Across Alternative Model Specifications",
             add_rows = rows) |>
  add_header_above(c(" ", "Economic satisfaction" = 2, "Government approval" = 2, "Economic satisfaction" = 2, "Economic satisfaction" = 2)) |>
  add_header_above(c(" ", "Experiment 1" = 4, "Experiment 2" = 2, "Experiment 3" = 2)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' and 'In-group improvement' variables show the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |> 
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) |>
  landscape()

### Table H3 ----

purectrl <- list(
  "Economic satisfaction" = data1 |> 
    feols(q8 ~ treated + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$treat %in% c(1,4) & data1$q1!=98),
  "Government approval" = data1 |> 
    feols(q9 ~ treated + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$treat %in% c(1,4) & data1$q1!=98)
)

rows <- tribble(~term, ~OLS, ~OLS,
                'In-group clustered SEs', 'Yes', 'Yes',
                'In-group fixed effects', 'Yes', 'Yes',
                'No. of groups', '22', '22')

attr(rows, 'position') <- c(15,16,17,18)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(purectrl, 
             output = "outputs/table_H3.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("treated1" = "In-group decline",
                          "intensity" = "Treatment intensity",
                          "intensity.m" = "Treatment intensity",
                          "stimsize" = "Treatment intensity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "socio.pre.m" = "Sociotropic eval's",
                          "redFT" = "Gov't vote intention",
                          "FV22govt" = "Gov't vote intention",
                          "vs_pidbase2" = "Party ID: Rep.",
                          "vs_pidbase3" = "Party ID: Indep.",
                          "vs_pidbase4" = "Party ID: other"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline, With No-Information Control Scenario",
             add_rows = rows) |>
  add_header_above(c(" ", "In-group decline" = 2)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' variable shows the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |> #'Treatment intensity' is a continuous indicator for the statistic included in each stimulus. The 'stimulus' coefficients encode which type of information is included in the stimulus (ref. is high financial insecurity). For gov't vote intention, this is the majority 'red bloc' parties. Standard errors (in parentheses) clustered at the in-group level.
  kable_styling(latex_options = c("hold_position", "scale_down"), full_width = FALSE)

### Table H4 ----

data1 <- data1 |>
  mutate(treated_veryoutgroups1 = case_when(
    treat==4 ~ 1,
    treat %in% c(1,2,3) ~ NA_real_,
    treat==5 & GRUPPE_TYPE==1 ~ 0,
    treat==5 & GRUPPE_TYPE==2 ~ 0,
    treat==5 & q1==5 & TREATMENT %in% c(7,8,9,13,14,15,16) ~ 0,
    treat==5 & q1 %in% c(7,8,9) & TREATMENT %in% c(5,11,12) ~ 0,
    treat==5 & q1==10 & TREATMENT==5 ~ 0,
    treat==5 & q1 %in% c(11,12,13,14,15,16) & TREATMENT %in% c(5,11,12) ~ 0,
    treat==5 & q1 %in% c(17,18) & TREATMENT %in% c(21,22) ~ 0,
    treat==5 & q1 %in% c(21,22) & TREATMENT %in% c(17,18) ~ 0,
    TRUE ~ NA_real_
  ))

data2 <- data2 |>
  mutate(treated_veryoutgroups2 = case_when(
    C==1 ~ 1,
    C==0 & gruppe %in% c(1,3,4,6) & EXP2.grp %in% c(1,3,4,6) ~ 0,
    TRUE ~ NA_real_
  ))

data3 <- data3 |>
  mutate(treated_veryoutgroups3 = case_when(
    ingroup==1 ~ 1,
    ingroup==0 & grupper==1 & q25_ran %in% c(5,6) ~ 0,
    ingroup==0 & grupper==4 & q25_ran %in% c(2,3) ~ 0,
    ingroup==0 & grupper==3 & q25_ran %in% c(4) ~ 0,
    ingroup==0 & grupper==6 & q25_ran %in% c(1) ~ 0,
    ingroup==0 & grupper==2 & q25_ran %in% c(4) ~ 0,
    ingroup==0 & grupper==5 & q25_ran %in% c(1) ~ 0,
    TRUE ~ NA_real_
  ))

veryoutgroups <- list(
  "Econ. satisfaction" = data1 |> 
    feols(q8 ~ treated_veryoutgroups1 + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Gov't approval" = data1 |> 
    feols(q9 ~ treated_veryoutgroups1 + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1, subset = data1$q1!=98),
  "Econ. satisfaction" = data2 |>
    feols(EXP2 ~ treated_veryoutgroups2*govtecon.pre.m + treated_veryoutgroups2*intensity.m + treated_veryoutgroups2*socio.pre.m + socio.pre.d + FV22govt | gruppe, subset = data2$byland!=99 & data2$unggam!=99),
  "Econ. satisfaction" = data3 |>
    feols(q25_5 ~ treated_veryoutgroups3 + stimsize + vs_pidbase | grupper, weights = ~weight))

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS,
                'Controls', 'Yes', 'Yes', 'Yes', 'Yes',
                'Clustered SEs', 'Yes', 'Yes', 'No', 'No',
                'In-group FEs', 'Yes', 'Yes', 'Yes', 'Yes',
                'No. of groups', '22', '22', '6', '6',
                'Treated/untreated N', '680/447', '676/455', '980/254', '309/506')

attr(rows, 'position') <- c(5, 6, 7, 8, 9)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(veryoutgroups, 
             output = "outputs/table_H4.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("treated_veryoutgroups1" = "Rel. in-group decline",
                          "treated_veryoutgroups2" = "Rel. in-group decline",
                          "treated_veryoutgroups3" = "Rel. in-group improvement"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline and Improvement, with Control Stimuli Restricted to Most Dissimilar Out-Groups",
             add_rows = rows) |>
  add_header_above(c(" ", "Experiment 1" = 2, "Experiment 2" = 1, "Experiment 3" = 1)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' and 'In-group improvement' variables show the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |> #Main regression models where 'control' stimuli are restricted to the most dissimilar out-groups.
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 10.5, full_width = FALSE)

### Table H5 ----

absrel <- list(
  "1" = data1 |>
    feols(q8 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "2" = data1 |>
    feols(q9 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "3" = data1 |>
    feols(q8 ~ own_abs + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "4" = data1 |>
    feols(q9 ~ own_abs + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "5" = data1 |>
    filter(treat!=1) |>
    feols(q8 ~ own + macro + own*macro + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "6" = data1 |>
    filter(treat!=1) |>
    feols(q9 ~ own + macro + own*macro + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1)
)

rows <- tribble(~term, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, ~OLS, 
                'Clustered SEs', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 
                'In-group FEs', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 
                'No. of groups', '22', '22', '22', '22', '22', '22')

attr(rows, 'position') <- c(19, 20, 21)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(absrel, 
             output = "outputs/table_H5.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "Treatment effect",
                          "own_abs1" = "Treatment effect",
                          "own1" = "Treatment effect",
                          "macro1" = "National benchmark",
                          "own1:macro1" = "Treatment X national benchmark",
                          "intensity" = "Treatment intensity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "redFT" = "Govt. vote intention"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of Absolute and Relative In-group Decline",
             add_rows = rows) |> 
  add_header_above(c(" ", "Econ. satisfaction" = 1, "Govt. approval" = 1, "Econ. satisfaction" = 1, "Govt. approval" = 1, "Econ. satisfaction" = 1, "Govt. approval" = 1)) |> #'Treatment intensity' is a continuous indicator for the statistic included in each stimulus. The 'stimulus' coefficients for Experiment 1 encode which type of information is included in the stimulus (ref. is high financial insecurity). For govternment vote intention, this is the parties in the 'red bloc' (as the Social Democrats were in power). Standard errors (in parentheses) clustered at in-group level. 
  add_header_above(c(" ", "With national benchmark" = 2, "Without national benchmark" = 2, "Interaction model" = 2)) |>
  footnote(general = "Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' and 'In-group improvement' variables show the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). Models 1 and 2 show effects the effects of treatment including the national benchmark (identical to 'relative in-group decline' effects in Table 4). Models 3 and 4 show the same effects for the treatment conditions without the national benchmark. Models 5 and 6 compare these two effects, by interacting two variables: one indicating getting in-group vs out-group information, and one indicating getting the national benchmark vs not. * p<.05, ** p<.01, *** p<.001. ",
           threeparttable = TRUE) |>
  kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) |>
  landscape()

### Table J1 ----

pocket <- list(
  "Without national benchmark" = data1 |>
    feols(q12 ~ own_abs + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1),
  "With national benchmark" = data1 |>
    feols(q12 ~ own_rel + intensity + treattype + q6 + q7 + redFT | q1, vcov = ~q1)
)

rows <- tribble(~term, ~OLS, ~OLS,
                'Clustered SEs', 'Yes', 'Yes',
                'In-group FEs', 'Yes', 'Yes',
                'No. of groups', '22', '22')

attr(rows, 'position') <- c(15, 16, 17)

gof_map <- tribble(
  ~raw,         ~clean,  ~fmt,  ~omit,
  "nobs", "N",     0,  FALSE
)

modelsummary(pocket, 
             output = "outputs/table_J1.tex",
             fmt = 2,
             booktabs = TRUE,
             estimate = "{estimate}{stars}",
             stars = c('+' = .1, '*' = .05, '**' = 0.01),
             statistic = 'std.error',
             coef_map = c("own_rel1" = "In-group decline",
                          "own_abs1" = "In-group decline",
                          "intensity" = "Treatment intensity",
                          "treattype2" = "Stimulus: high job insecurity",
                          "treattype4" = "Stimulus: low job security",
                          "treattype3" = "Stimulus: low financial security",
                          "q6" = "Sociotropic eval's",
                          "q7" = "Egotropic eval's",
                          "redFT" = "Govt. vote intention"),
             gof_map = gof_map,
             escape = T,
             title = "Effects of In-Group Decline on Prospective Pocketbook Evaluations",
             add_rows = rows) |>
  footnote(general = "Estimates from linear regression models of prospective pocketbook evaluations (5-point scale) on treatment (in-group performance vs out-group performance information (ref.); with and without national benchmark). Estimates from linear regression models of satisfaction with the current state of the economy and approval of the incumbent's handling of the economy (5-point scales) on treatment with in-group fixed effects and design controls. The coefficients on the 'In-group decline' variable shows the gap between those getting in-group stimuli vs those getting out-group stimuli (ref.). * p<.05, ** p<.01, *** p<.001.",
           threeparttable = TRUE) |>
  kable_styling(full_width = FALSE, latex_options = c("hold_position", "scale_down"))
