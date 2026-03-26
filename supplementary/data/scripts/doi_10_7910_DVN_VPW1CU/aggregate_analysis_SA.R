#### Aggregate analysis (Sun Abraham) ############

### This code produces the and saves results of the Sun-Abraham difference-in-difference models used in the article
### Some of these are loaded and used to produce figure 5 in the main text, the code for which is in figure_5.R
### In the supplementary materials, they are loaded and used to produce the SA tables in section A.3


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
ABcombined <- readRDS("ABcombined.rds")

## Urban-rural split
ABcombined_rural <- ABcombined %>% filter(urbrur == "Rural")
ABcombined_urban <- ABcombined %>% filter(urbrur == "Urban")

## Filter for above/below median distance (rural only)
AB_dist_above <- ABcombined_rural %>% filter(distance > median(distance))
AB_dist_below <- ABcombined_rural %>% filter(distance <= median(distance))

## Urban rural opinion gap (rural only)
AB_trust_above <- ABcombined_rural %>% filter(trust_gap > median(trust_gap))
AB_trust_below <- ABcombined_rural %>% filter(trust_gap <= median(trust_gap))

## Urban rural poverty gap (rural only)
AB_wealthgap_above <- ABcombined_rural %>% filter(wealth_basket_gap > median(wealth_basket_gap))
AB_wealthgap_below <- ABcombined_rural %>% filter(wealth_basket_gap <= median(wealth_basket_gap))

## Alt control group
data_alt_control <- ABcombined_rural %>% 
  filter(round < 5) %>% ## Remove last two rounds where coverage more saturated
  mutate(first_inside = ifelse(first_inside > 2007, 10000, first_inside), 
         time_to_inside = ifelse(first_inside > 2007, -10000, time_to_inside))
## Shift those entering after round 4 into the never treated group

## Only most precise geocodes 
ABcombined_rural_geo <- ABcombined_rural %>% 
  filter(precision_code == "1")


##### Sun Abraham specifications (SA) ####

#### Baseline 
sa_base <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code) %>% summary(agg="ATT")
  
  summary(model)
}

#### Country trends 
sa_trends <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code) %>% summary(agg="ATT")

  summary(model)
}

#### Conley SEs 
sa_conley <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ sunab(first_inside, time_to_inside) + 
                       news_radio + news_tv + news_paper + election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name[year] + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 conley(50, distance="spherical")) %>% summary(agg="ATT")
  
  summary(model)
}


### List of dependent variables (same as above)

dependent_vars <- c("new_mean_trust", "new_mean_trust_2", "new_mean_trust_bin",
                    "trust_pres", "trust_police", "trust_ec", "trust_courts", 
                    "trust_pres_bin", "trust_police_bin", "trust_ec_bin", "trust_courts_bin")

### List of datasets

datasets <- list(
  dataset1 = ABcombined_rural, ## rural
  dataset2 = ABcombined_urban, ## urban
  dataset3 = AB_dist_above, ## rural above median distance
  dataset4 = AB_dist_below, ## rural below median distance
  dataset5 = AB_trust_above, ## above median trust gap
  dataset6 = AB_trust_below, ## below median trust gap
  dataset7 = AB_wealthgap_above, ## above median wealth gap
  dataset8 = AB_wealthgap_below, ## below median wealth gap
  dataset9 = data_alt_control, ## Alternative control group
  dataset10 = ABcombined_rural_geo) ## Subset of most precisely geocoded EAs

# Run the function for each DV and dataset, and store the results in a list

sa_base_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- sa_base(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

sa_trends_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- sa_trends(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

#### If have access to data, run the code below ###
#### If do not have access, go to next chunk to read in pre-loaded results ###

#sa_conley_list <- lapply(names(datasets), function(ds_name) {
  #dataset <- datasets[[ds_name]]
  #lapply(dependent_vars, function(var) {
    #result <- sa_conley(var, dataset)
    #list(dataset = ds_name, variable = var, result = result)
  #})
#})

### Save results as a series of smaller data files to upload to dataverse separately

#for (i in seq_along(sa_conley_list)) {
  #dataset_name <- names(datasets)[i]
  #saveRDS(sa_conley_list[[i]], file = paste0("sa_conley_", dataset_name, ".rds"))
#}


## Read back in full set of smaller data files and combine into single list

gc() ## free up unused memory

sa_conley_dataset1 <- readRDS("sa_conley_dataset1.rds")
sa_conley_dataset2 <- readRDS("sa_conley_dataset2.rds")
sa_conley_dataset3 <- readRDS("sa_conley_dataset3.rds")
sa_conley_dataset4 <- readRDS("sa_conley_dataset4.rds")
sa_conley_dataset5 <- readRDS("sa_conley_dataset5.rds")
sa_conley_dataset6 <- readRDS("sa_conley_dataset6.rds")
sa_conley_dataset7 <- readRDS("sa_conley_dataset7.rds")
sa_conley_dataset8 <- readRDS("sa_conley_dataset8.rds")
sa_conley_dataset9 <- readRDS("sa_conley_dataset9.rds")
sa_conley_dataset10 <- readRDS("sa_conley_dataset10.rds")

sa_conley_list <- list(
  dataset1 = sa_conley_dataset1,
  dataset2 = sa_conley_dataset2,
  dataset3 = sa_conley_dataset3,
  dataset4 = sa_conley_dataset4,
  dataset5 = sa_conley_dataset5,
  dataset6 = sa_conley_dataset6,
  dataset7 = sa_conley_dataset7,
  dataset8 = sa_conley_dataset8,
  dataset9 = sa_conley_dataset9,
  dataset10 = sa_conley_dataset10
)

## Remove individual components to save memory, as no longer needed

rm(sa_conley_dataset1, sa_conley_dataset2, sa_conley_dataset3, 
   sa_conley_dataset4, sa_conley_dataset5, sa_conley_dataset6, 
   sa_conley_dataset7, sa_conley_dataset8, sa_conley_dataset9, 
   sa_conley_dataset10)

gc() ## free up unused memory

### Convert list to a data frame to use in coefficient plots ###

tidy_model_result <- function(model_result) {
  broom::tidy(model_result$result, conf.int = TRUE) %>%
    dplyr::mutate(dataset = model_result$dataset, dependent_var = model_result$variable)
}

indices <- 1:10

results_sa <- map_df(indices, function(i) {
  bind_rows(
    map_df(sa_base_list[[i]], tidy_model_result, .id = "model"),
    map_df(sa_trends_list[[i]], tidy_model_result, .id = "model"),
    map_df(sa_conley_list[[i]], tidy_model_result, .id = "model")
  )}) %>%
  filter(term == "ATT") %>%
  mutate(spec = rep(rep(c("Baseline","+ Country trends",
                          "+ Conley 50km"), each=11), 10), 
         outcome = rep(c("Mean trust", "Mean trust (ex. Pres)", "Mean trust (Binary)", 
                         "President", "Police", "Election\nCommission", "Courts", 
                         "President (binary)", "Police (binary)", 
                         "Election\nCommission (binary)", "Courts (binary)"), 30), 
         type = rep(c(rep("Mean Trust", 3), 
                      rep("Components", 4), 
                      rep("Components (binary)", 4)), 30), 
         dataset = 
           rep(c("ABcombined_rural", "ABcombined_urban", 
                 "AB_dist_above", "AB_dist_below",
                 "AB_trust_above", "AB_trust_below", 
                 "AB_wealthgap_above", "AB_wealthgap_below", 
                 "data_alt_control", "ABcombined_rural_geo"), each = 33))

### Save data.frame version of results, to use making figures later

saveRDS(results_sa, file = "results_sa.rds")


##### SA tables for Supplementary Materials ########

### Set dictionary

setFixest_dict(c(new_mean_trust = "Mean", 
                 new_mean_trust_2 = "ex. Pres", 
                 new_mean_trust_bin = "Binary", 
                 inside = "Enter coverage",
                 trust_pres = "President", 
                 trust_police = "Police",
                 trust_courts = "Courts",
                 trust_ec = "EC", 
                 trust_pres_bin = "Binary", 
                 trust_police_bin = "Binary",
                 trust_courts_bin = "Binary",
                 trust_ec_bin = "Binary", 
                 attend_community = "Community meeting", 
                 raise_issue = "Raise issue", 
                 protest = "Protest",
                 country_name = "Country",
                 year = "Year",
                 round = "Survey round",
                 ea_code = "Enumeration area",
                 ATT = "Aggregated ATT"))

### Convert results to new format for tables

sa_base_flat_list <- unlist(sa_base_list, recursive = FALSE) %>%
  lapply(function(x) x$result)

sa_trends_flat_list <- unlist(sa_trends_list, recursive = FALSE) %>%
  lapply(function(x) x$result)

sa_conley_flat_list <- unlist(sa_conley_list, recursive = FALSE) %>%
  lapply(function(x) x$result)


### Create latex tables

#### Main results

tab_SM3 <- etable(sa_base_flat_list[1:3],
                 sa_trends_flat_list[1:3],
                 sa_conley_flat_list[1:3],
                 title = "Mobile coverage and political trust in rural Africa (Sun Abraham 2020) - Mean trust",
                 headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                 keep = c("ATT"), 
                 signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                 digits = 3,
                 digits.stats = 3,
                 fontsize = "tiny",
                 convergence = FALSE,
                 tex=T)


tab_SM5 <- etable(sa_base_flat_list[4], sa_base_flat_list[8],
                 sa_trends_flat_list[4], sa_trends_flat_list[8],
                 sa_conley_flat_list[4], sa_conley_flat_list[8],
                 sa_base_flat_list[5], sa_base_flat_list[9],
                 sa_trends_flat_list[5], sa_trends_flat_list[9],
                 sa_conley_flat_list[5], sa_conley_flat_list[9],
                 title = "Mobile coverage and political trust in rural Africa (Sun Abraham 2020) - President and Police",
                 headers = list("^:_:Specification" = 
                                  list("Baseline" = 2, "Trends" = 2, "Conley SEs" = 2,
                                       "Baseline" = 2, "Trends" = 2, "Conley SEs" = 2)),
                 keep = c("ATT"), 
                 signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                 digits = 3,
                 digits.stats = 3,
                 fontsize = "tiny",
                 convergence = FALSE,
                 tex=T)


tab_SM7 <- etable(sa_base_flat_list[6], sa_base_flat_list[10],
                 sa_trends_flat_list[6], sa_trends_flat_list[10],
                 sa_conley_flat_list[6], sa_conley_flat_list[10],
                 sa_base_flat_list[7], sa_base_flat_list[11],
                 sa_trends_flat_list[7], sa_trends_flat_list[11],
                 sa_conley_flat_list[7], sa_conley_flat_list[11],
                 title = "Mobile coverage and political trust in rural Africa (Sun Abraham 2020) - Electoral Commission and Courts",
                 headers = list("^:_:Specification" = 
                                  list("Baseline" = 2, "Trends" = 2, "Conley SEs" = 2,
                                       "Baseline" = 2, "Trends" = 2, "Conley SEs" = 2)),
                 keep = c("ATT"), 
                 signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                 label="tab:SAtab3",
                 digits = 3,
                 digits.stats = 3,
                 fontsize = "tiny",
                 convergence = FALSE,
                 tex=T)

## Heterogeneity

tab_SM10 <- etable(sa_base_flat_list[23],
                          sa_base_flat_list[24],
                          sa_base_flat_list[25],
                          sa_trends_flat_list[23],
                          sa_trends_flat_list[24],
                          sa_trends_flat_list[25],
                          sa_conley_flat_list[23],
                          sa_conley_flat_list[24],
                          sa_conley_flat_list[25],
                          title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, for units below median distance from country capital)",
                          headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                          keep = c("ATT"), 
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=T)

tab_SM11 <- etable(sa_base_flat_list[34],
                          sa_base_flat_list[35],
                          sa_base_flat_list[36],
                          sa_trends_flat_list[34],
                          sa_trends_flat_list[35],
                          sa_trends_flat_list[36],
                          sa_conley_flat_list[34],
                          sa_conley_flat_list[35],
                          sa_conley_flat_list[36],
                          title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, for units below median distance from country capital)",
                          headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                          keep = c("ATT"), 
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          label = "tab:SAtab2_distance",
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=T)

tab_SM14 <- etable(sa_base_flat_list[45],
                          sa_base_flat_list[46],
                          sa_base_flat_list[47],
                          sa_trends_flat_list[45],
                          sa_trends_flat_list[46],
                          sa_trends_flat_list[47],
                          sa_conley_flat_list[45],
                          sa_conley_flat_list[46],
                          sa_conley_flat_list[47],
                          title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, for countries with above median urban-rural difference in mean political trust)",
                          headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                          keep = c("Aggregated ATT"), 
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=T)

tab_SM15 <- etable(sa_base_flat_list[56],
                          sa_base_flat_list[57],
                          sa_base_flat_list[58],
                          sa_trends_flat_list[56],
                          sa_trends_flat_list[57],
                          sa_trends_flat_list[58],
                          sa_conley_flat_list[56],
                          sa_conley_flat_list[57],
                          sa_conley_flat_list[58],
                          title = "Mobile coverage and political trust in rural Africa (Sun Abraham 2020, for countries with below median urban-rural difference in mean political trust)",
                          headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                          keep = c("Aggregated ATT"), 
                          signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                          digits = 3,
                          digits.stats = 3,
                          fontsize = "tiny",
                          convergence = FALSE,
                          tex=T)

tab_SM18 <- etable(sa_base_flat_list[67],
                           sa_base_flat_list[68],
                           sa_base_flat_list[69],
                           sa_trends_flat_list[67],
                           sa_trends_flat_list[68],
                           sa_trends_flat_list[69],
                           sa_conley_flat_list[67],
                           sa_conley_flat_list[68],
                           sa_conley_flat_list[69],
                           title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, for countries with above median urban-rural difference in wealth)",
                           headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                           keep = c("Aggregated ATT"), 
                           signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                           digits = 3,
                           digits.stats = 3,
                           fontsize = "tiny",
                           convergence = FALSE,
                           tex=T)

tab_SM19 <- etable(sa_base_flat_list[78],
                           sa_base_flat_list[79],
                           sa_base_flat_list[80],
                           sa_trends_flat_list[78],
                           sa_trends_flat_list[79],
                           sa_trends_flat_list[80],
                           sa_conley_flat_list[78],
                           sa_conley_flat_list[79],
                           sa_conley_flat_list[80],
                           title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, for countries with below median urban-rural difference in wealth)",
                           headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                           keep = c("Aggregated ATT"), 
                           signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                           label = "tab:SAtab2_wealthgap",
                           digits = 3,
                           digits.stats = 3,
                           fontsize = "tiny",
                           convergence = FALSE,
                           tex=T)

## Additional results

tab_SM21 <- etable(sa_base_flat_list[89:91],
                     sa_trends_flat_list[89:91],
                     sa_conley_flat_list[89:91],
                     title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, alt control group)",
                     headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                     keep = c("ATT"), 
                     signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                     digits = 3,
                     digits.stats = 3,
                     fontsize = "tiny",
                     convergence = FALSE,
                     tex=T)

tab_SM23 <- etable(sa_base_flat_list[100:102],
                     sa_trends_flat_list[100:102],
                     sa_conley_flat_list[100:102],
                     title = "Mobile coverage and mean political trust in rural Africa (Sun Abraham 2020, precision code 1 only)",
                     headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                     keep = c("ATT"), 
                     signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                     digits = 3,
                     digits.stats = 3,
                     fontsize = "tiny",
                     convergence = FALSE,
                     tex=T)

### Save table objects to read back in when running supplementary materials rmd file

save(tab_SM3, tab_SM5, tab_SM7, 
     tab_SM10, tab_SM11,
     tab_SM14, tab_SM15,
     tab_SM18, tab_SM19,
     tab_SM21, tab_SM23, file = "did_tables_SA.RData")


