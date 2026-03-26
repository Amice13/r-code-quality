#### Aggregate analysis PT ############

### This code produces the and saves results of the pre-trends models reported in section A.2 of the supplementary materials

####### GENERAL INSTRUCTIONS #####

## There are two options for models using spatially adjusted standard errors, depending on data access.
## Where flagged, you can either run the original code (if you have the data), or skip to the following chunk and read in a pre-loaded version of the results.
## As a baseline, the original code is ##ed out, so that the code automatically runs using datasets immediately available in the dataverse.
## All other code runs the same. If using full data, name the coordinates as "longitude" and "latitude" to ensure it matches the code.

## Clear environment

rm(list=ls())


# Load packages

library(ggplot2)
library(dplyr)
library(purrr)
library(fixest)
library(stringr)
library(tidyr)
library(broom)

#### Difference-in-differences analyses and saving results as tables/data.frame ####

## Load data and make subsets
ABcombined_rural <- readRDS("ABcombined.rds") %>% filter(urbrur == "Rural")

####### Full event study estimates (for supplementary materials) ####

#### Baseline 
pt_base <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code)
  
  # Return summary
  summary(model)
}

pt_base_bin <- function(outcome, dataset) {

  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside, bin.rel = \"bin::2\") + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code)
  
  # Return summary
  summary(model)
}

#### Country trends 
pt_trends <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code)
  
  summary(model)
}

pt_trends_bin <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside, bin.rel = \"bin::2\") + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code)
  
  summary(model)
}

#### Conley SEs 
pt_conley <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 conley(50, distance="spherical"))

  summary(model)
}

pt_conley_bin <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside, bin.rel = \"bin::2\") + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 conley(50, distance="spherical"))
  
  summary(model)
}

# List of dependent variables
dependent_vars <- c("new_mean_trust", "new_mean_trust_2", "new_mean_trust_bin",
                    "trust_pres", "trust_pres_bin", 
                    "trust_police", "trust_police_bin",
                    "trust_ec", "trust_ec_bin",
                    "trust_courts", "trust_courts_bin")

# List of datasets
datasets <- list(
  dataset1 = ABcombined_rural) ## Pt dataset

# Run the function for each dependent variable and each dataset and store the results in a list

pt_base_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- pt_base(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

pt_base_list_bin <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- pt_base_bin(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

pt_trends_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- pt_trends(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

pt_trends_list_bin <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- pt_trends_bin(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})


#### Note: below code requires sensitive data to run. If not, skip to next chunk to read in pre-saved lists

#pt_conley_list <- lapply(names(datasets), function(ds_name) {
  #dataset <- datasets[[ds_name]]
  #lapply(dependent_vars, function(var) {
    #result <- pt_conley(var, dataset)
    #list(dataset = ds_name, variable = var, result = result)
  #})
#})

#pt_conley_list_bin <- lapply(names(datasets), function(ds_name) {
  #dataset <- datasets[[ds_name]]
  #lapply(dependent_vars, function(var) {
    #result <- pt_conley_bin(var, dataset)
    #list(dataset = ds_name, variable = var, result = result)
  #})
#})

#saveRDS(pt_conley_list, "pt_conley_list.rds")
#saveRDS(pt_conley_list_bin, "pt_conley_list_bin.rds")


## Read lists of spatial results in below:
pt_conley_list <- readRDS("pt_conley_list.rds")
pt_conley_list_bin <- readRDS("pt_conley_list_bin.rds")

# Convert list to a data frame to make coefficient plots

tidy_model_result <- function(model_result) {
  broom::tidy(model_result$result, conf.int = TRUE) %>%
    dplyr::mutate(dataset = model_result$dataset, dependent_var = model_result$variable)
}

# Pull model output into a dataframe

indices <- 1

results_pt <- map_df(indices, function(i) {
  bind_rows(
    map_df(pt_base_list[[i]], tidy_model_result, .id = "model"),
    map_df(pt_base_list_bin[[i]], tidy_model_result, .id = "model"),
    map_df(pt_trends_list[[i]], tidy_model_result, .id = "model"),
    map_df(pt_trends_list_bin[[i]], tidy_model_result, .id = "model"),
    map_df(pt_conley_list[[i]], tidy_model_result, .id = "model"),
    map_df(pt_conley_list_bin[[i]], tidy_model_result, .id = "model")
  )}) %>%
  filter(str_detect(term, "time_to_inside") & 
           as.numeric(str_extract(term, "(?<=::)-?\\d+")) < 0) %>% ## Filter for pre-period estimates
  mutate(spec = rep(c("Baseline","+ Country trends",
                      "+ Conley 50km"), each=242), 
         outcome = rep(c(rep(c("Mean trust", "Mean trust (ex Pres)", "Mean trust (binary)", 
                               "President", "President (binary)",
                               "Police", "Police (binary)",
                               "Election\nCommission", "Election\nCommission (binary)", 
                               "Courts", "Courts (binary)"), each = 14),
                         rep(c("Mean trust", "Mean trust (ex Pres)", "Mean trust (binary)", 
                               "President", "President (binary)",
                               "Police", "Police (binary)",
                               "Election\nCommission", "Election\nCommission (binary)", 
                               "Courts", "Courts (binary)"), each = 8)) , 3), 
         term_numeric = as.numeric(sub("time_to_inside::", "", term)),
         binned = rep(c(rep("All years", 154), rep("2 year bin", 88)), 3))

## Seperate into two

results_pt_full <- results_pt %>% filter(binned == "All years")
results_pt_bin <- results_pt %>% filter(binned == "2 year bin")


####### Save cleaned results as data.frames #######

save(results_pt_full, results_pt_bin, 
     file = "pt_results.RData")


