###################
###################
## this is the main file from which all scripts can be executed 
## (they can also of course be executed individually)
## the main functionality of this script is to get
## exact coefficients, standard errors, and p-values for
## the main independent variables of interest
###################
###################

# clear environment
rm(list = ls())

library(tidyverse)
library(fixest)
library(lfe)
library(plm)
library(ordbetareg)
library(purrr)
library(texreg)
library(haven)

# list of scripts to source
script_files <- c("01-fe-models-reproduction-in-r.R",
                  "02-fe-models-add-year-fe.R",
                  "03-fe-models-add-clustered-se.R",
                  "04-fe-models-add-year-fe-clustered-se.R",
                  "05-fe-models-reproduction-scaled-vars.R",
                  "06-fe-models-scaled-vars-add-year-fe.R",
                  "07-fe-models-plm-fe.R",
                  "08-fe-models-plm-re.R")

# function to modify texreg objects to replace standard errors with p-values
replace_se_with_p_values <- function(models) {
  modified_models <- lapply(models, function(model) {
    tr <- texreg::extract(model)
    # p_values <- coef(summary(model))[, "Pr(>|t|)"]
    if ("Pr(>|t|)" %in% colnames(coef(summary(model)))) {
      p_values <- coef(summary(model))[, "Pr(>|t|)"]
    } else if ("Pr(>|z|)" %in% colnames(coef(summary(model)))) {
      p_values <- coef(summary(model))[, "Pr(>|z|)"]
    } else {
      stop("Neither 'Pr(>|t|)' nor 'Pr(>|z|)' found in the summary.")
    }
    tr@se <- p_values
    return(tr)
  })
  return(modified_models)
}


## these functions are not really in use anymore but remained in the script for future reference
## should someone want to have a dataframe which includes exact coefficients, p-values, and
## standard errors for the main independent variables
# function to extract p-values
safe_extract_p_value <- function(model, var) {
  tryCatch({
    s <- summary(model)
    pvalue <- s$coefficients[var, 4]
    return(pvalue)
  }, error = function(e) {
    return(NA)
  })
}

# function to extract coefficients
safe_extract_coefficient <- function(model, var) {
  tryCatch({
    s <- summary(model)
    coefficient <- s$coefficients[var, 1]
    return(coefficient)
  }, error = function(e) {
    return(NA)
  })
}

# function to extract standard errors
safe_extract_se <- function(model, var) {
  tryCatch({
    s <- summary(model)
    se <- s$coefficients[var, 2]
    return(se)
  }, error = function(e) {
    return(NA)
  })
}

# extract p-values, coefficients, and standard errors
extract_values <- function(models, iv_vars, model_lengths) {
  p_values <- mapply(safe_extract_p_value, models, iv_vars)
  coefficients <- mapply(safe_extract_coefficient, models, iv_vars)
  ses <- mapply(safe_extract_se, models, iv_vars)
  
  # generate corresponding model numbers to match regression tables based on lengths
  model_numbers <- unlist(sapply(model_lengths, function(len) rep(1:5, length.out = len)))
  
  data.frame(
    Model = paste0("Model ", model_numbers),
    IV_Variable = iv_vars,
    P_Value = p_values,
    Coefficient = coefficients,
    SE = ses,
    stringsAsFactors = FALSE
  )
}


results_list <- list()


for (script in script_files) {
  
  source(script)
  
  # accumulate models sourced from the script
  models2 <- list(tab2.1, tab2.2, tab2.3, tab2.4, tab2.5)
  models4 <- list(tab4.1, tab4.2, tab4.3, tab4.4, tab4.5)
  models5 <- list(tab5.1, tab5.2, tab5.3, tab5.4, tab5.5)
  models6 <- list(tab6.1, tab6.2, tab6.3, tab6.4, tab6.5)
  models8 <- list(tab8.1, tab8.2, tab8.3, tab8.4, tab8.5)
  models9 <- list(tab9.1, tab9.2, tab9.3, tab9.4, tab9.5)
  
  # combine all models into a single list
  all_models <- list(models2, models4, models5, models6, models8, models9)
  
  # combine main independent variables into a list
  iv_vars <- list(iv2.1, iv4.1, iv5.1, iv6.1, iv8.1, iv9.1)
  
  # define the lengths of the models and main independent variable counts
  model_lengths <- c(5, 5, 5, 5, 10, 20)
  
  # flatten models list
  flattened_models <- unlist(all_models, recursive = FALSE)
  
  # expand flattened models to match the length of expanded main independent variables
  expand_flattened_models <- function(models, lengths) {
    expanded_models <- unlist(mapply(function(model_set, length) {
      rep(model_set, each = length / length(model_set))
    }, models, lengths, SIMPLIFY = FALSE), recursive = FALSE)
    return(expanded_models)
  }
  
  expanded_flattened_models <- expand_flattened_models(all_models, model_lengths)
  
  # expand main independent variables
  expand_iv_vars <- function(iv_vars, model_lengths) {
    expanded_vars <- unlist(mapply(function(vars, length) {
      rep(vars, each = length / length(vars))
    }, iv_vars, model_lengths, SIMPLIFY = FALSE))
    return(expanded_vars)
  }
  
  expanded_iv_vars <- expand_iv_vars(iv_vars, model_lengths)
  
  values_list <- extract_values(expanded_flattened_models, expanded_iv_vars, model_lengths)
  values_list$Script <- script
  results_list[[script]] <- values_list
  
}

# combine all results into one dataframe
combined_results <- bind_rows(results_list)

# quick check
head(combined_results)


## pvalues dataframe
pvals <- combined_results %>%
  ## uncomment this if you only want the script number rather than the whole name
  # mutate(Script = str_extract_all(Script, "[[:digit:]]+", simplify = TRUE)) %>%
  select(Model, IV_Variable, Script, P_Value) %>%
  pivot_wider(names_from = Script, values_from = P_Value)

pvals$IV_Variable[pvals$IV_Variable == "ipema_any_demo_assist_dum_2l"] <- "Democracy mandate"
pvals$IV_Variable[pvals$IV_Variable == "itotal_compound_K_2l"] <- "# of uniformed personnel"
pvals$IV_Variable[pvals$IV_Variable == "iactual_civilian_total_K_2l"] <- "# of civilian personnel"
pvals$IV_Variable[pvals$IV_Variable == "iany_demo_all_max_dum_2l"] <- "Any dem. activities"
pvals$IV_Variable[pvals$IV_Variable == "iany_demo_engage_max_dum_2l"] <- "Any dem. eng. with host state"
pvals$IV_Variable[pvals$IV_Variable == "iany_demo_bypass_max_dum_2l"] <- "Any dem. byp. of host state"
pvals$IV_Variable[pvals$IV_Variable == "idemo_all_max_dum_2l"] <- "Any dem. institution act's"
pvals$IV_Variable[pvals$IV_Variable == "ielections_all_max_dum_2l"] <- "Any election act's"
pvals$IV_Variable[pvals$IV_Variable == "iparties_all_max_dum_2l"] <- "Any pol. party act's"
pvals$IV_Variable[pvals$IV_Variable == "ivoters_all_max_dum_2l"] <- "Any voter act's"


## coefficients dataframe
coefs <- combined_results %>%
  ## uncomment this if you only want the script number rather than the whole name
  # mutate(Script = str_extract_all(Script, "[[:digit:]]+", simplify = TRUE)) %>%
  select(Model, IV_Variable, Script, Coefficient) %>%
  pivot_wider(names_from = Script, values_from = Coefficient)

coefs$IV_Variable[coefs$IV_Variable == "ipema_any_demo_assist_dum_2l"] <- "Democracy mandate"
coefs$IV_Variable[coefs$IV_Variable == "itotal_compound_K_2l"] <- "# of uniformed personnel"
coefs$IV_Variable[coefs$IV_Variable == "iactual_civilian_total_K_2l"] <- "# of civilian personnel"
coefs$IV_Variable[coefs$IV_Variable == "iany_demo_all_max_dum_2l"] <- "Any dem. activities"
coefs$IV_Variable[coefs$IV_Variable == "iany_demo_engage_max_dum_2l"] <- "Any dem. eng. with host state"
coefs$IV_Variable[coefs$IV_Variable == "iany_demo_bypass_max_dum_2l"] <- "Any dem. byp. of host state"
coefs$IV_Variable[coefs$IV_Variable == "idemo_all_max_dum_2l"] <- "Any dem. institution act's"
coefs$IV_Variable[coefs$IV_Variable == "ielections_all_max_dum_2l"] <- "Any election act's"
coefs$IV_Variable[coefs$IV_Variable == "iparties_all_max_dum_2l"] <- "Any pol. party act's"
coefs$IV_Variable[coefs$IV_Variable == "ivoters_all_max_dum_2l"] <- "Any voter act's"


## standard errors dataframe
ses <- combined_results %>%
  ## uncomment this if you only want the script number rather than the whole name
  # mutate(Script = str_extract_all(Script, "[[:digit:]]+", simplify = TRUE)) %>%
  select(Model, IV_Variable, Script, SE) %>%
  pivot_wider(names_from = Script, values_from = SE)

ses$IV_Variable[ses$IV_Variable == "ipema_any_demo_assist_dum_2l"] <- "Democracy mandate"
ses$IV_Variable[ses$IV_Variable == "itotal_compound_K_2l"] <- "# of uniformed personnel"
ses$IV_Variable[ses$IV_Variable == "iactual_civilian_total_K_2l"] <- "# of civilian personnel"
ses$IV_Variable[ses$IV_Variable == "iany_demo_all_max_dum_2l"] <- "Any dem. activities"
ses$IV_Variable[ses$IV_Variable == "iany_demo_engage_max_dum_2l"] <- "Any dem. eng. with host state"
ses$IV_Variable[ses$IV_Variable == "iany_demo_bypass_max_dum_2l"] <- "Any dem. byp. of host state"
ses$IV_Variable[ses$IV_Variable == "idemo_all_max_dum_2l"] <- "Any dem. institution act's"
ses$IV_Variable[ses$IV_Variable == "ielections_all_max_dum_2l"] <- "Any election act's"
ses$IV_Variable[ses$IV_Variable == "iparties_all_max_dum_2l"] <- "Any pol. party act's"
ses$IV_Variable[ses$IV_Variable == "ivoters_all_max_dum_2l"] <- "Any voter act's"


## ordered beta regression models
## caution: these two scripts take a long time when models are estimated
## we provided the resulting models as rds files in this replication package
## which are currently used to create the tables
## should you want to rerun the models, uncomment the code in scripts 09 and 10
source("09-ordbetareg-fe-regressions.R")
source("10-ordbetareg-twfe-regressions.R")

## instrumental variable models
source("11-iv-reproduction-in-r.R")
source("12-iv-reproduction-clustered-se.R")
