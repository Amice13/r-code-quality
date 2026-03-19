#### Aggregate analysis (TWFE) ############

### This code produces the and saves results of the TWFE aggregate difference-in-difference models used in the article
### Some of these are loaded and used to produce figure 5 in the main text, the code for which is in figure_5.R
### In the supplementary materials, they are loaded and used to produce the TWFE tables in section A.3

####### GENERAL INSTRUCTIONS #####

## There are two options for models using spatially adjusted standard errors, depending on data access.
## Where flagged, you can either run the original code (if you have the data), or skip to the following chunk and read in a pre-loaded version of the results.
## As a baseline, the original code is ##ed out, so that the code automatically runs using datasets immediately available in the dataverse.
## All other code runs the same. If using full data, name the coordinates as "longitude" and "latitude" to ensure it matches the code.

## Clear environment

rm(list=ls())

## Install packages (uncomment if do not have these installed)
## Note not all are required for this R file, but come up in others in the repo
#install.packages(c("dplyr", "ggplot2", "purrr", "fixest",
                   #"stringr", "tidyr", "broom", "gridExtra", "sf",
                   #"lubridate", "kableExtra", "mosaic",
                   #"knitr", "rmarkdown", "systemfonts", "farver"))
#install.packages("Rtool42") ## Only if using Windows (original analysis conducted on Mac)

# Load full set of packages (note: not all required for this R file, but come up in others in the repo)

library(dplyr)
library(ggplot2)
library(purrr)
library(fixest)
library(stringr)
library(tidyr)
library(broom)
library(gridExtra)
library(sf)
library(lubridate)
library(kableExtra)
library(mosaic)
library(knitr)
library(rmarkdown)
library(systemfonts)
library(farver)

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


###### RUN TWFE MODEL SPECIFICATIONS AND CLEAN RESULTS ######

#### Baseline function
twfe_base <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ inside + ever_inside + 
                       news_radio + news_tv + news_paper + 
                       election_year + nightlights_present +
                       cash_income + food + water + medical + age + gender + education | 
                       country_name + year")
  formula <- as.formula(formula_str)
  
  model <- feols(formula, 
                 data = dataset, 
                 vcov = ~ea_code)
  
  summary(model)
}

#### Country trends function
twfe_trends <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ inside + ever_inside + 
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

#### Conley SEs function
twfe_conley <- function(outcome, dataset) {
  
  formula_str <- paste(outcome, "~ inside + ever_inside + 
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


### List of dependent variables
dependent_vars <- c("new_mean_trust", "new_mean_trust_2", "new_mean_trust_bin",
                    "trust_pres", "trust_police", "trust_ec", "trust_courts", 
                    "trust_pres_bin", "trust_police_bin", "trust_ec_bin", "trust_courts_bin")

### List of datasets
datasets <- list(
  dataset1 = ABcombined_rural, ## rural
  dataset2 = ABcombined_urban, ## urban
  dataset3 = AB_dist_above, ## rural above median distance
  dataset4 = AB_dist_below, ## rural below or equal median distance
  dataset5 = AB_trust_above, ## above median trust gap
  dataset6 = AB_trust_below, ## below or equal median trust gap
  dataset7 = AB_wealthgap_above, ## above median wealth gap
  dataset8 = AB_wealthgap_below, ## below or equal median wealth gap
  dataset9 = data_alt_control, ## Alternative control group
  dataset10 = ABcombined_rural_geo) ## Subset of most precisely geocoded EAs

# Run the function for each DV and dataset and store the results in a list

twfe_base_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- twfe_base(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

twfe_trends_list <- lapply(names(datasets), function(ds_name) {
  dataset <- datasets[[ds_name]]
  lapply(dependent_vars, function(var) {
    result <- twfe_trends(var, dataset)
    list(dataset = ds_name, variable = var, result = result)
  })
})

##### If have access to data, run the code below ###
###### If do not have access, go to next chunk to read in pre-loaded results ###

#twfe_conley_list <- lapply(names(datasets), function(ds_name) {
  #dataset <- datasets[[ds_name]]
  #lapply(dependent_vars, function(var) {
    #result <- twfe_conley(var, dataset)
    #list(dataset = ds_name, variable = var, result = result)
  #})
#})

#saveRDS(twfe_conley_list, file = "twfe_conley_list.rds") ## Save result, which is pre-loaded below

#### If do not have access to the data, use below code to read in list of pre-loaded results ###

twfe_conley_list <- readRDS("twfe_conley_list.rds")

### Convert list to a data frame to use in coefficient plots ###

### General function to tidy model results
tidy_model_result <- function(model_result) {
  broom::tidy(model_result$result, conf.int = TRUE) %>%
    dplyr::mutate(dataset = model_result$dataset, dependent_var = model_result$variable)
}

### Convert list-based results into a dataframe

indices <- 1:10

results_twfe <- map_df(indices, function(i) {
  bind_rows(
    map_df(twfe_base_list[[i]], tidy_model_result, .id = "model"),
    map_df(twfe_trends_list[[i]], tidy_model_result, .id = "model"),
    map_df(twfe_conley_list[[i]], tidy_model_result, .id = "model")
  )}) %>%
  filter(term == "inside") %>%
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

saveRDS(results_twfe, file = "results_twfe.rds")

##### TWFE tables for Supplementary Materials ########

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

twfe_base_flat_list <- unlist(twfe_base_list, recursive = FALSE) %>%
  lapply(function(x) x$result)

twfe_trends_flat_list <- unlist(twfe_trends_list, recursive = FALSE) %>%
  lapply(function(x) x$result)

twfe_conley_flat_list <- unlist(twfe_conley_list, recursive = FALSE) %>%
  lapply(function(x) x$result)

### Create latex tables

#### Main results

tab_SM2 <- etable(twfe_base_flat_list[1:3],
                   twfe_trends_flat_list[1:3],
                   twfe_conley_flat_list[1:3],
                   title = "Mobile coverage and political trust in rural Africa (TWFE) - mean trust",
                   headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                   keep = c("Enter coverage"), 
                   signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                   digits = 3,
                   digits.stats = 3,
                   label = "tab:TWFEtab1",
                   fontsize = "tiny",
                   convergence = FALSE,
                   tex=T)


tab_SM4 <- etable(twfe_base_flat_list[4], twfe_base_flat_list[8],
                   twfe_trends_flat_list[4], twfe_trends_flat_list[8],
                   twfe_conley_flat_list[4], twfe_conley_flat_list[8],
                   twfe_base_flat_list[5], twfe_base_flat_list[9],
                   twfe_trends_flat_list[5], twfe_trends_flat_list[9],
                   twfe_conley_flat_list[5], twfe_conley_flat_list[9],
                   title = "Mobile coverage and political trust in rural Africa (TWFE) - President and Police",
                   headers = list("^:_:Specification" = 
                                    list("Baseline" = 2, "Trends" = 2, "Conley SEs" = 2,
                                         "Baseline" = 2, "Trends" = 2, "Conley SEs" = 2)),
                   keep = c("Enter coverage"), 
                   signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                   digits = 3,
                   digits.stats = 3,
                   fontsize = "tiny",
                   convergence = FALSE,
                   tex=T)

tab_SM6 <- etable(twfe_base_flat_list[6], twfe_base_flat_list[10],
                   twfe_trends_flat_list[6], twfe_trends_flat_list[10],
                   twfe_conley_flat_list[6], twfe_conley_flat_list[10],
                   twfe_base_flat_list[7], twfe_base_flat_list[11],
                   twfe_trends_flat_list[7], twfe_trends_flat_list[11],
                   twfe_conley_flat_list[7], twfe_conley_flat_list[11],
                   title = "Mobile coverage and political trust in rural Africa (TWFE)- Electoral Commission and Courts",
                   headers = list("^:_:Specification" = 
                                    list("Baseline" = 2, "Trends" = 2, "Conley SEs" = 2,
                                         "Baseline" = 2, "Trends" = 2, "Conley SEs" = 2)),
                   keep = c("Enter coverage"), 
                   signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                   digits = 3,
                   digits.stats = 3,
                   fontsize = "tiny",
                   convergence = FALSE,
                   tex=T)

## Heterogeneity

tab_SM8 <- etable(twfe_base_flat_list[23],
                            twfe_base_flat_list[24],
                            twfe_base_flat_list[25],
                            twfe_trends_flat_list[23],
                            twfe_trends_flat_list[24],
                            twfe_trends_flat_list[25],
                            twfe_conley_flat_list[23],
                            twfe_conley_flat_list[24],
                            twfe_conley_flat_list[25],
                            title = "Mobile coverage and mean political trust in rural Africa (TWFE, for units above median distance from country capital)",
                            headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                            keep = c("Enter coverage"), 
                            signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                            label = "tab:TWFEtab1_distance",
                            digits = 3,
                            digits.stats = 3,
                            fontsize = "tiny",
                            convergence = FALSE,
                            tex=T)

tab_SM9 <- etable(twfe_base_flat_list[34],
                            twfe_base_flat_list[35],
                            twfe_base_flat_list[36],
                            twfe_trends_flat_list[34],
                            twfe_trends_flat_list[35],
                            twfe_trends_flat_list[36],
                            twfe_conley_flat_list[34],
                            twfe_conley_flat_list[35],
                            twfe_conley_flat_list[36],
                            title = "Mobile coverage and mean political trust in rural Africa (TWFE, for units below median distance from country capital)",
                            headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                            keep = c("Enter coverage"), 
                            signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                            digits = 3,
                            digits.stats = 3,
                            fontsize = "tiny",
                            convergence = FALSE,
                            tex=T)


tab_SM12 <- etable(twfe_base_flat_list[45],
                            twfe_base_flat_list[46],
                            twfe_base_flat_list[47],
                            twfe_trends_flat_list[45],
                            twfe_trends_flat_list[46],
                            twfe_trends_flat_list[47],
                            twfe_conley_flat_list[45],
                            twfe_conley_flat_list[46],
                            twfe_conley_flat_list[47],
                            title = "Mobile coverage and mean political trust in rural Africa (TWFE, for countries with above median urban-rural difference in mean political trust)",
                            headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                            keep = c("Enter coverage"), 
                            signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                            digits = 3,
                            digits.stats = 3,
                            fontsize = "tiny",
                            convergence = FALSE,
                            tex=T)


tab_SM13 <- etable(twfe_base_flat_list[56],
                            twfe_base_flat_list[57],
                            twfe_base_flat_list[58],
                            twfe_trends_flat_list[56],
                            twfe_trends_flat_list[57],
                            twfe_trends_flat_list[58],
                            twfe_conley_flat_list[56],
                            twfe_conley_flat_list[57],
                            twfe_conley_flat_list[58],
                            title = "Mobile coverage and mean political trust in rural Africa (TWFE, for countries with below median urban-rural difference in mean political trust)",
                            headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                            keep = c("Enter coverage"), 
                            signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                            digits = 3,
                            digits.stats = 3,
                            fontsize = "tiny",
                            convergence = FALSE,
                            tex=T)

tab_SM16 <- etable(twfe_base_flat_list[67],
                             twfe_base_flat_list[68],
                             twfe_base_flat_list[69],
                             twfe_trends_flat_list[67],
                             twfe_trends_flat_list[68],
                             twfe_trends_flat_list[69],
                             twfe_conley_flat_list[67],
                             twfe_conley_flat_list[68],
                             twfe_conley_flat_list[69],
                             title = "Mobile coverage and mean political trust in rural Africa (TWFE, for countries with above median urban-rural difference in wealth)",
                             headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                             keep = c("Enter coverage"), 
                             signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                             digits = 3,
                             digits.stats = 3,
                             fontsize = "tiny",
                             convergence = FALSE,
                             tex=T)


tab_SM17 <- etable(twfe_base_flat_list[78],
                             twfe_base_flat_list[79],
                             twfe_base_flat_list[80],
                             twfe_trends_flat_list[78],
                             twfe_trends_flat_list[79],
                             twfe_trends_flat_list[80],
                             twfe_conley_flat_list[78],
                             twfe_conley_flat_list[79],
                             twfe_conley_flat_list[80],
                             title = "Mobile coverage and mean political trust in rural Africa (TWFE , for countries with below median urban-rural difference in wealth)",
                             headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                             keep = c("Enter coverage"), 
                             signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                             digits = 3,
                             digits.stats = 3,
                             fontsize = "tiny",
                             convergence = FALSE,
                             tex=T)

## Additional results

tab_SM20 <- etable(twfe_base_flat_list[89:91],
                       twfe_trends_flat_list[89:91],
                       twfe_conley_flat_list[89:91],
                       title = "Mobile coverage and mean political trust in rural Africa (TWFE, alt control group)",
                       headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                       keep = c("Enter coverage"), 
                       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                       digits = 3,
                       digits.stats = 3,
                       fontsize = "tiny",
                       convergence = FALSE,
                       tex=T)


tab_SM22 <- etable(twfe_base_flat_list[100:102],
                       twfe_trends_flat_list[100:102],
                       twfe_conley_flat_list[100:102],
                       title = "Mobile coverage and mean political trust in rural Africa (TWFE, precision code 1 only)",
                       headers = list("^:_:Specification" = list("Baseline" = 3, "Trends" = 3, "Conley SEs" = 3)),
                       keep = c("Enter coverage"), 
                       signifCode = c("**" = 0.01, "*" = 0.05, "\\ensuremath{^{\\dagger}}" = 0.1),
                       digits = 3,
                       digits.stats = 3,
                       fontsize = "tiny",
                       convergence = FALSE,
                       tex=T)

### Save table objects to read back in when running supplementary materials rmd file

save(tab_SM2, tab_SM4, tab_SM6, 
     tab_SM8, tab_SM9,
     tab_SM12, tab_SM13,
     tab_SM16, tab_SM17,
     tab_SM20, tab_SM22,
     file = "did_tables_TWFE.RData")

## Code to check computing environment // package versions
sessionInfo()
