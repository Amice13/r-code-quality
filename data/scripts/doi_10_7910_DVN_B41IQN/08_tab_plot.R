### ------------------------------------------------------------
# Thesis Survey Experiment
# 08 Tables and Plots
# Friedrich Püttmann
# July 2024
### ------------------------------------------------------------

## install packages --------------------------------------------

install.packages("flextable")
install.packages("ggplot2")
install.packages("modelsummary")
install.packages("officer")
install.packages("tidyverse")

## load packages -----------------------------------------------

library(flextable)
library(ggplot2)
library(modelsummary)
library(officer)
library(tidyverse)

## set defaults ------------------------------------------------

# flextable
# https://davidgohel.github.io/flextable/reference/set_flextable_defaults.html
set_flextable_defaults(
  font.family = "Arial",
  font.size = 10,
  font.color = "black",
  border.color = "black",
  border.width = 0.75,
  background.color = "white"
)

## load dataset ------------------------------------------------

data_exp <- read_rds("03_data_clean.rds")


# create crosstable --------------------------------------------

# calculate table
# https://modelsummary.com/vignettes/datasummary.html#datasummary_crosstab
crosstab <- datasummary_crosstab(
  treat ~ age,
  statistic =  ~ Percent("col"),
  data = data_exp,
  sparse_header = TRUE,
  output = "flextable" # export to flextable
)
crosstab

# adapt table
# https://davidgohel.github.io/flextable/index.html
crosstab <- delete_columns(crosstab, j = 2)
crosstab <- align(crosstab, align = c("left", "right", "right"), part = "all")
crosstab <- hline_top(
  crosstab, border = fp_border(color = "black", width = 1.5), part = "header"
)
crosstab <- hline_bottom(
  crosstab, border = fp_border(color = "black", width = 1.5), part = "body"
)
crosstab <- width(
  crosstab, j = c(1, 2, 3), width = c(5, 2.5, 2.5), unit = "cm"
)
crosstab

# export table to word
save_as_docx(crosstab, path = "crosstab.docx")


## create summary statistics -----------------------------------

# https://modelsummary.com/vignettes/datasummary.html#datasummary


# create regression table --------------------------------------

# estimate model - Example
res_logit_bas <- glm(
  try_stayaway ~
  head_cov + gender + age + eth_self + language + vote_today_group + edu_group
  + job_group + syr_presence + syr_encounter + Soru805 + Soru1308,
  family = binomial(link = "logit"),
  data = data_exp
)
summary(res_logit_bas)


# define coefficients ----------------------------------------------------------
# https://modelsummary.com/vignettes/modelsummary.html

  # Religious identity by lifestyle ----------------------------
coef_nam <- c(
  'lifestylemodern' = 'Modern',
  'lifestylereligious conservative' = 'Religious conservative',
  '(Intercept)' = 'Constant'
)
coef_nam

  # Religious identity by religious practice -------------------
coef_nam <- c(
  'rel_pracNon-believer' = 'Non-believer',
  'rel_pracNo practice' = 'No practice',
  'rel_pracSome practice' = 'Some practice',
  'rel_pracPractice' = 'Practice',
  'rel_pracPious practice' = 'Pious practice',
  '(Intercept)' = 'Constant'
)
coef_nam

  # Religious identity by head covering -----------------------
coef_nam <- c(
  'head_covNone' = 'No covering',
  'head_covHeadscarf' = 'Traditional headscarf',
  'head_covTurban' = 'Turban',
  'head_covChador/Niqab' = 'Chador/Niqab',
  'head_covInterviewee single male' = 'Interviewee single male',
  '(Intercept)' = 'Constant'
)
coef_nam

  # Ethnic identity as self-identified ----------------------------
coef_nam <- c(
  'eth_selfTurkish' = 'Turkish',
  'eth_selfKurdish' = 'Kurdish',
  'eth_selfZaza' = 'Zaza',
  'eth_selfArab' = 'Arab',
  'eth_selfOther'= 'Other',
  '(Intercept)' = 'Constant'
)
coef_nam

  # Ethnic identity as language at home ---------------------------
coef_nam <- c(
  'languageTurkish' = 'Turkish',
  'languageKurdish' = 'Kurdish',
  'languageZaza' = 'Zaza',
  'languageArabic' = 'Arabic',
  'languageOther'= 'Other',
  '(Intercept)' = 'Constant'
)
coef_nam


# define summary statistics ---------------------------------------------------
# https://modelsummary.com/vignettes/modelsummary.html
sum_stat <- tribble(
  ~raw,      ~clean,             ~fmt,
  "nobs",    "Observations",     0,
  "aic",     "AIC",              1,
  "bic",     "BIC",              1,
  "logLik",  "Log-likelihood",   3,
  "rmse",    "RMSE",             2
)
sum_stat


# define row for controls -----------------------------------------------------
# https://modelsummary.com/vignettes/modelsummary.html

  # Lifestyle ------------------------------
row_contr <- tribble(
  ~term,         ~"(1)",
  "Controls",    "\u2713",
)
attr(row_contr, 'position') <- 4 
row_contr

  # Religious practice ---------------------
row_contr <- tribble(
  ~term,         ~"(1)",
  "Controls",    "\u2713",
)
attr(row_contr, 'position') <- 10 
row_contr

  # Head covering -------------------------
row_contr <- tribble(
  ~term,         ~"(1)",
  "Controls",    "\u2713",
)
attr(row_contr, 'position') <- 10 
row_contr

  # Ethnicity self-identified ------------
row_contr <- tribble(
  ~term,         ~"(1)",
  "Controls",    "\u2713",
)
attr(row_contr, 'position') <- 10 
row_contr

  # Ethnicity language ---------------
row_contr <- tribble(
  ~term,         ~"(1)",
  "Controls",    "\u2713",
)
attr(row_contr, 'position') <- 10 
row_contr


# create table ----------------------------------------------------------------
# https://modelsummary.com/vignettes/modelsummary.html
reg_tab <- modelsummary(
  res_logit_bas, # name model
  stars = c('*' = .05, '**' = .01, '***' = 0.001), # define significance stars
  coef_map = coef_nam, # apply coefficient definition
  gof_map = sum_stat, # adapt summary statistics
  include_reference = TRUE, # include reference category
  add_rows = row_contr, # add row for controls
  output = "flextable" # export to flextable
)
reg_tab


# adapt table ------------------------------------------------------------------
# https://davidgohel.github.io/flextable/index.html

  # Lifestyle ------------------------------------------
reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(6, 7), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab

  # Religious practice ----------------------------------
reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(12, 13), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab

  # Head covering ------------------------------------
reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(12, 13), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab

  # Self-identified ethnicity -----------------------------------
reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(12, 13), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab

  # Language at home -----------------------------------
reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(12, 13), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab


# export table to word ---------------------------------
save_as_docx(reg_tab, path = "reg_res.docx")


# create coefficient plot --------------------------------------

# define coefficients
# https://modelsummary.com/vignettes/modelplot.html
coef_nam <- c(
  'head_covInterviewee single male' = 'Single male',
  'head_covChador/Niqab' = 'Chador/Niqab',
  'head_covTurban' = 'Turban',
  'head_covHeadscarf' = 'Headscarf'
)
coef_nam

# create plot
# https://modelsummary.com/vignettes/modelplot.html
# https://ggplot2.tidyverse.org/
modelplot(res_logit_bas,
  coef_map = coef_nam
) +
  labs(x = "Point estimates and 95% confidence interval",
       y = "Lifestyle")




## Tables for Chapter 8, Section 3.1 ---------------------------

# Staying away from Syrians - lifestyle
res_logit_bas <- glm(try_stayaway ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Talking to Syrians - lifestyle
res_logit_bas <- glm(syr_talking_ever ~ lifestyle + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Having a Syrian friend - lifestyle
res_logit_bas <- glm(syr_friend ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Socialising with Syrians - lifestyle
res_logit_bas <- glm(syr_socialising_ever ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

##

# Staying away from Syrians - religious practice
res_logit_bas <- glm(try_stayaway ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Talking to Syrians - religious practice
res_logit_bas <- glm(syr_talking_ever ~ rel_prac + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Having a Syrian friend - religious practice
res_logit_bas <- glm(syr_friend ~ rel_prac+ gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Socialising with Syrians - lifestyle
res_logit_bas <- glm(syr_socialising_ever ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

## 

# Staying away from Syrians - head covering
res_logit_bas <- glm(try_stayaway ~ head_cov + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Talking to Syrians - head covering
res_logit_bas <- glm(syr_talking_ever ~ head_cov + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Having a Syrian friend - head covering
res_logit_bas <- glm(syr_friend ~ head_cov + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Socialising with Syrians - head covering
res_logit_bas <- glm(syr_socialising_ever ~ head_cov + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)


## Tables for Chapter 8, Section 3.3 ---------------------------

# Staying away from Syrians - self-identified ethnicity
res_logit_bas <- glm(try_stayaway ~ eth_self + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Talking to Syrians - self-identified ethnicity
res_logit_bas <- glm(syr_talking_ever ~ eth_self + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Socialising with Syrians - self-identified ethnicity
res_logit_bas <- glm(syr_socialising_ever ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Having a Syrian friend - self-identified ethnicity
res_logit_bas <- glm(syr_friend ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

##

# Staying away from Syrians - language at home
res_logit_bas <- glm(try_stayaway ~ language + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Talking to Syrians - language at home
res_logit_bas <- glm(syr_talking_ever ~ language + gender + age + eth_self + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Socialising with Syrians - language at home
res_logit_bas <- glm(syr_socialising_ever ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Having a Syrian friend - language at home
res_logit_bas <- glm(syr_friend ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)




## Tables for Chapter 8, Section 3.2 ---------------------------

# Acceptance in the same country - lifestyle
res_logit_bas <- glm(Soru901 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same city - lifestyle
res_logit_bas <- glm(Soru902 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same neighbourhood/workplace - lifestyle
res_logit_bas <- glm(Soru903 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same apartment building/friends' circle - lifestyle
res_logit_bas <- glm(Soru904 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same home/family - lifestyle
res_logit_bas <- glm(Soru905 ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Cultural similarity (any) - lifestyle
res_logit_bas <- glm(cultsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Religious similarity (any) - lifestyle
res_logit_bas <- glm(relsim_any ~ lifestyle + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

##

# Cultural similarity (any) - religious practice
res_logit_bas <- glm(cultsim_any ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Religious similarity (any) - religious practice
res_logit_bas <- glm(relsim_any ~ rel_prac + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

##

# Cultural similarity (any) - head covering
res_logit_bas <- glm(cultsim_any ~ head_cov + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Religious similarity (any) - head covering
res_logit_bas <- glm(relsim_any ~ head_cov + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)






## Tables for Chapter 8, Section 3.4 ---------------------------

# Acceptance in the same country - self-identified ethnicity
res_logit_bas <- glm(Soru901 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same city - self-identified ethnicity
res_logit_bas <- glm(Soru902 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same neighbourhood - self-identified ethnicity
res_logit_bas <- glm(Soru903 ~ eth_self + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same apartment building/friends' circle - self-identified ethnicity
res_logit_bas <- glm(Soru904 ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same home/family - self-identified ethnicity
res_logit_bas <- glm(Soru905 ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Cultural similarity (any) - self-identified ethnicity
res_logit_bas <- glm(cultsim_any ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Religious similarity (any) - self-identified ethnicity
res_logit_bas <- glm(relsim_any ~ eth_self + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)


##

# Acceptance in the same country - language 
res_logit_bas <- glm(Soru901 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same city - language 
res_logit_bas <- glm(Soru902 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same neighbourhood - language
res_logit_bas <- glm(Soru903 ~ language + gender + age + lifestyle + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same apartment building/friends' circle - language
res_logit_bas <- glm(Soru904 ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Acceptance in the same home/family - language
res_logit_bas <- glm(Soru905 ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Cultural similarity (any) - language
res_logit_bas <- glm(cultsim_any ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)

# Religious similarity (any) - language
res_logit_bas <- glm(relsim_any ~ language + gender + age + language + vote_today_group + edu_group + job_group + syr_presence + syr_encounter + Soru805 + sect + syr_con2, family = binomial(link = "logit"), data = data_exp)
summary(res_logit_bas)




## Tables for Chapter 8, Section 3.6 (Results from the experiment) -------------

# Syrians part of Turkish society today? 

res_ols <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(partsoc_today ~ treat, data = .)
summary(res_ols)

# Should the Syrians be part of Turkish society?

res_ols <- data_exp  %>%
  filter(lifestyle == "modern") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(lifestyle == "religious conservative") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Turkish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Kurdish") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(lifestyle == "modern" & eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)

res_ols <- data_exp  %>%
  filter(lifestyle == "religious conservative" & eth_self == "Arab") %>%
  lm(partsoc_should ~ treat, data = .)
summary(res_ols)



# tables for the experiment (everything included) --------------------

sum_stat <- tribble(
  ~raw,                 ~clean,                   ~fmt,
  "nobs",               "Observations",           0,   # Number of observations
  "r.squared",          "R-squared",              3,   # R-squared
  "adj.r.squared",      "Adjusted R-squared",     3,   # Adjusted R-squared
  "sigma",              "Residual Std. Error",    3,   # Residual Standard Error
  "fstatistic",         "F-statistic",            3,   # F-statistic
  "p.value",            "p-value (F-statistic)",  3    # p-value of the F-statistic
)

coef_nam <- c("treatyes" = "Treatment", 
              "(Intercept)" = "Constant")

reg_tab <- modelsummary(
  res_ols, # name model
  stars = c('*' = .05, '**' = .01, '***' = 0.001), # define significance stars
  coef_map = coef_nam, # apply coefficient definition
  gof_map = sum_stat, # apply the goodness-of-fit (GoF) statistics definition
  output = "flextable", # export to flextable
  coef_order = c("(Intercept)", "treatyes") # Specify the order directly
)

reg_tab <- delete_part(reg_tab, part = "header")
reg_tab <- align(reg_tab, align = c("left", "right"), part = "all")
reg_tab <- hline_top(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "body"
)
reg_tab <- hline_bottom(
  reg_tab, border = fp_border(color = "black", width = 1.5), part = "footer"
)
reg_tab <- hline(
  reg_tab, i = c(4, 5), border = fp_border(color = "black", width = 0.75)
)
reg_tab <- width(reg_tab, j = c(1, 2), width = c(5, 2.5), unit = "cm")
reg_tab

save_as_docx(reg_tab, path = "reg_res.docx")

