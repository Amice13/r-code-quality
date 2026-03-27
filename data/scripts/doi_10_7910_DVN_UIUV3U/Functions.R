# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# Functions.R
# Analysis of Non Voting Participation among Rural Americans
# Core Functions for Data Analysis
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *****************************************************************
# PACKAGES ####
# *****************************************************************

library(tibble)      # For data table functions
library(dplyr)       # For %>%, filter(), select(), mutate()
library(haven)       # For read_dta()
library(srvyr)       # For as_survey()
library(survey)      # For survey summary statistics
library(tidyr)       # For pivot_*() functions
library(broom)       # For tidy()
library(purrr)       # For map()
library(ggplot2)     # For plots
library(stargazer)   # For regression table exports
library(dotwhisker)  # For dot whisker plots in raw output
library(xtable)      # For table exports
library(readr)       # For read_csv()
library(dataverse)   # For get_dataframe_by_name()

# *****************************************************************
# FUNCTIONS ####
# *****************************************************************

# Below are a set of functions that are written by the authors 
#  or modified from existing packages (Packages are noted and 
#  credited accordingly) that are used in the paper. This Script
#  serves as the central hub for locating all the custom functions
#  that are not included in the packages previously loaded above.

## Cleaning Data ####

# Functions in this section address data cleaning. Since the process
#  requires using many variables that have a similar coding structure,
#  the functions help streamline the amount of typing needed for this
#  process.

# This is the general clean function to recode values that the CES
#  labels as missing (typically 8 and 9) as NA. Cross ref codebook 
#  before use.
clean_CCES <- function(var){
  dplyr::recode(
    var, 
    `8` = NaN, `9` = NaN, 
    .default = var)
} 

# This function is to dummy code variables so that 0 is no and
#  1 is yes. Often, no values (oppose, in some cases) are denoted
#  as a 2. This changes that.
code_Oppose <- function(var){
  dplyr::recode(
    var, 
    `2` = 0, 
    .default = var)
}

## Organizing Regression Results ####

# The functions in this section assist with cleaning the 
#  regression output and generating odds ratios rather than
#  logit estimates (default for glm).

# This function generates odds ratios and their respective
#  standard errors
get.or.se <- function(model) {
  broom::tidy(model) %>%
    mutate(or = exp(estimate),
           var.diag = diag(vcov(model)),
           or.se = sqrt(or^2 * var.diag)) 
}

# The following function generates predicted probabilities 
#   and provides significance labels using stars based
#   on the p-values.
get_probability <- function(df){
  tidy(df) %>% 
    mutate(
      odds = exp(estimate),
      var.diag = diag(vcov(df)),
      odds_sd = sqrt(odds^2 * var.diag), 
      #prob = odds/(1 + odds),
      #prob_sd = odds_sd/(1 + odds_sd),
      p.value = round(p.value, 2),
      signif = case_when(
        p.value <= 0.001 ~ "***",
        p.value > 0.001 & p.value <= 0.05 ~ "**",
        p.value > 0.05 & p.value <= 0.1 ~ "*",
        TRUE ~ ""
      )
    )
}

# Since the default output in the above function does not
#   give us the total number of respondents and the 
#   default regression summary does not provide this 
#   information either, the following function gives us
#   the number of observations used to estimate the results
#   in each model and formats it for export 
get_n <- function(model){
  data.frame(
    value = length(model$residuals)
  ) %>% 
    add_column(
      term = "N",
      variable = "") %>% 
    relocate(value, .after = last_col()) %>% 
    mutate(value = as.character(value))
}

# To get the R^2 value for the regression, we turn to
#  Alex Tahk's psci package that has functions to help
#  generate these estimates  for survey logit regression
#  models. At the time of the writing of the code, 
#  the authors had difficulties downloading the package
#  so the relevant functions used from this package are 
#  posted below.
pR2Work <- function(llh,llhNull,n=NULL){
  if (is.null(n))
    n <- nobs(llh)
  McFadden <- 1 - llh/llhNull
  G2 <- -2*(llhNull-llh)
  r2ML <- 1 - exp(-G2/n)
  r2ML.max <- 1 - exp(llhNull*2/n)
  r2CU <- r2ML/r2ML.max
  out <- c(llh=llh,
           llhNull=llhNull,
           G2=G2,
           McFadden=McFadden,
           r2ML=r2ML,
           r2CU=r2CU)
  out      
}

pR2.default <- function(object,...){
  llh <- logLik(object)
  cat("fitting null model for pseudo-r2\n")
  objectNull <- update(object, ~ 1, data=model.frame(object))
  llhNull <- logLik(objectNull)
  pR2Work(llh,llhNull)
}

# The following function actually generates the Pseudo R^2
#   values and is from the jtools package. For the same 
#   reason as above, the authors had issues with installing
#   the original package so the same code is reposted here.
svypR2 <- function(object) {
  
  llh <- suppressWarnings(logLik(object))
  fam <- family(object)$family
  link <- family(object)$link
  
  if (fam == "quasibinomial") {
    fam <- binomial(link = link)
  } else if (fam == "quasipoisson") {
    fam <- poisson(link = link)
  } else {
    fam <- family(object)
  }
  if (is.null(attr(terms(formula(object)),"offset"))) {
    objectNull <- suppressWarnings(update(
      object, ~ 1, 
      design = object$survey.design,
      family = fam))
  } else {
    offs <- model.offset(model.frame(object))
    frame <- object$survey.design
    frame$variables$jtools_offs <- offs
    objectNull <- suppressWarnings(update(
      object, ~ 1 + offset(jtools_offs),
      design = frame, family = fam))
  }
  llhNull <- logLik(objectNull)
  n <- dim(object$model)[1]
  pR2Work(llh, llhNull, n)
}

# This function is written by the authors and leverages the
#   functions discussed above to get an R^2 value for each 
#   regression and format it for use with the table export 
get_r2 <- function(model){
  out <- data.frame(value = svypR2(model)) %>% 
    rownames_to_column() %>% 
    filter(rowname == "McFadden") %>% 
    mutate(
      term = case_when(
        rowname == "McFadden" ~ "Pseudo $R^2$"
      ),
      value = round(value, 2)
    ) %>% 
    select(-rowname) %>% 
    add_column(
      variable = ""
    ) %>% 
    relocate(value, .after = last_col()) %>% 
    mutate(value = as.character(value))
}

## Generate Output ####

# The functions in this section allow us to generage outputs
#   that come in handy for the paper. Since the steps that are
#   represented by each of these functions can be quite 
#   repeetitive, the following are written to help streamline 
#   the process.

# This functions generates a rough sketch of a dot-whisker 
#   plot based on odds ratios. It takes a data frame that is
#   prepared using get_probability()
prob_plot <- function(df){
  df %>% 
    ggplot(aes(x = term, y = odds))+
    geom_pointrange(aes(ymin=odds-odds_sd, ymax=odds+odds_sd))+
    coord_flip()+
    xlab("Variables")+
    ylab("Odds Ratios")+
    theme_bw()
}

# This function allows us to rename the predictors for the regressions
#   given that we have a tidy data frame (df)
# This is for use with the 2020 analyses.
CES_Predictors <- function(df){
  df %>% 
    mutate(
      term = case_when(
        term == "factor(residence)Suburb" ~ "Suburb",
        term == "factor(residence)Town" ~ "Small Town",
        term == "factor(residence)Rural" ~ "Rural area",
        term == "pid7" ~ "Party ID (7-category)",
        term == "ideo5" ~ "Ideology (5-category)", 
        term == "rr_work" ~ "Racial Resentment \n Work without favors",
        term == "rr_slavery" ~ "Racial Resentment \n Impact of Slavery",
        term == "RUCAx2" ~ "RUCA - Micropolitan",
        term == "RUCAx3" ~ "RUCA - Small Town",
        term == "RUCAx4" ~ "RUCA - Most Rural",
        term == "educ" ~ "Education",
        term == "income" ~ "Income",
        term == "FEMALETRUE" ~ "Female",
        term == "AGE25 - 44" ~ "Age: 25 - 44",
        term == "AGE45 - 64" ~ "Age: 45 - 64",
        term == "AGE65+" ~ "Age: 65+",
        term == "factor(race)2" ~ "Black",
        term == "factor(race)3" ~ "Hispanic",
        term == "factor(race)4" ~ "Asian",
        term == "factor(race)5" ~ "Native American",
        term == "factor(race)6" ~ "Middle Eastern",
        term == "factor(race)7" ~ "Two or more races",
        term == "factor(race)NaN" ~ "Other race",
        term == "factor(CHURCH)2" ~ "Church: Once a week", 
        term == "factor(CHURCH)3" ~ "Church: Once or twice a month",
        term == "factor(CHURCH)4" ~ "Church: A few times a year", 
        term == "factor(CHURCH)5" ~ "Church: Seldom", 
        term == "factor(CHURCH)6" ~ "Church: Never", 
        term == "factor(CHURCH)7" ~ "Church: Don't know",
        TRUE ~ term
      ),
      term = factor(term, levels = term)
    )
}

# Since the predictors for 2018 are slightly different than 2020,
#   this function gives us 2018 regression predictors
CES_Predictors_18 <- function(df){
  df %>% 
    mutate(
      term = case_when(
        term == "factor(residence)Suburb" ~ "Suburb",
        term == "factor(residence)Town" ~ "Small Town",
        term == "factor(residence)Rural" ~ "Rural area",
        term == "pid7" ~ "Party ID (7-category)",
        term == "ideo5" ~ "Ideology (5-category)", 
        term == "rr_work" ~ "Racial Resentment \n Work without favors",
        term == "rr_slavery" ~ "Racial Resentment \n Impact of Slavery",
        term == "rr_less" ~ "Racial Resentment \n Blacks got less",
        term == "rr_harder" ~ "Racial Resentment \n Try hard enough",
        term == "RUCAx2" ~ "RUCA - Micropolitan",
        term == "RUCAx3" ~ "RUCA - Small Town",
        term == "RUCAx4" ~ "RUCA - Rural Area",
        term == "educ" ~ "Education",
        term == "income" ~ "Income",
        term == "FEMALETRUE" ~ "Female",
        term == "AGE25 - 44" ~ "Age: 25 - 44",
        term == "AGE45 - 64" ~ "Age: 45 - 64",
        term == "AGE65+" ~ "Age: 65+",
        term == "factor(race)2" ~ "Black",
        term == "factor(race)3" ~ "Hispanic",
        term == "factor(race)4" ~ "Asian",
        term == "factor(race)5" ~ "Native American",
        term == "factor(race)6" ~ "Middle Eastern",
        term == "factor(race)7" ~ "Two or more races",
        term == "factor(race)NaN" ~ "Other race",
        term == "factor(CHURCH)2" ~ "Church: Once a week", 
        term == "factor(CHURCH)3" ~ "Church: Once or twice a month",
        term == "factor(CHURCH)4" ~ "Church: A few times a year", 
        term == "factor(CHURCH)5" ~ "Church: Seldom", 
        term == "factor(CHURCH)6" ~ "Church: Never", 
        term == "factor(CHURCH)7" ~ "Church: Don't know",
        TRUE ~ term
      ),
      term = factor(term, levels = term)
    )
}

# For 2020: This function gets us the odds ratios, renames
#   the predictors and formats a data frame to export requirement
#   and appends N and R^2 values to the end.
prob_tab <- function(df){
  regression <- get_probability(df) %>% 
    CES_Predictors() %>% 
    select(term, odds, odds_sd, signif) %>% 
    mutate(
      outcome = paste0(
        round(odds, 3), signif),
      out_SE = paste0("(", round(odds_sd, 3), ")")
    ) %>% 
    select(term, outcome, out_SE) %>% 
    reshape2::melt(id = "term") %>% 
    arrange(term)
  n <- get_n(df)
  r2 <- suppressWarnings(get_r2(df))
  outcome <- bind_rows(regression, n, r2)
  return(outcome)
}

# For 2016: Same as above but for 2018 regressions.
prob_tab18 <- function(df){
  regression <- get_probability(df) %>% 
    CES_Predictors_18() %>% 
    select(term, odds, odds_sd, signif) %>% 
    mutate(
      outcome = paste0(
        round(odds, 3), signif),
      out_SE = paste0("(", round(odds_sd, 3), ")")
    ) %>% 
    select(term, outcome, out_SE) %>% 
    reshape2::melt(id = "term") %>% 
    arrange(term) 
  n <- get_n(df)
  r2 <- suppressWarnings(get_r2(df))
  outcome <- bind_rows(regression, n, r2)
  return(outcome)
}


## Chi-Squared Tests ####

# The functions in this section provide functions to complete survey 
#   weighted chi squared analyses for multiple variables at once. 
#   These functions are useful to replicate the test results 
#   in the Appendix.

# For use with residence -- runs survey weighted chi-squared tests
#   on self-identified place of residence variabe
chi_squ_svy <- function(data, columns, ...) {
  model <- lapply(as.list(columns), function(x) {
    svychisq(as.formula(paste0("~residence+", names(data)[x])), 
             data = data, statistic = c( "Chisq"), ...)
  })
  return(model)
}

# For use with RUCA -- survey weighted chi-squared tests using the 
#   RUCA measure.
chi_squ_svy_ruca <- function(data, columns, ...) {
  model <- lapply(as.list(columns), function(x) {
    svychisq(as.formula(paste0("~RUCAx+", names(data)[x])), 
             data = data, statistic = c( "Chisq"), ...)
  })
  return(model)
}

# Vector of political behaviors used to quickly label the 
#   results. See the DESCRIPTIVE code files for information 
#   on use.
labels_chi <- c(
  "Voted", "Attended a Meeting",
  "Put up a political sign", "Work in Campaign",
  "Attended a Protest",  "Contact Elected Officials",
  "Donate Money", "Donate Blood", "Behavior: None of the Above", 
  "Posted Comment about Politics", "Followed a Political Event",
  "Forwarded Media about Politics", "Posted Media about Politics",
  "Read Story/Watched Video about Politics"
)
