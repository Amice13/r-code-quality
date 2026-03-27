#!/usr/bin/Rscript
##########################################################################################
# Social Media and Political Agenda Setting
##########################################################################################
# Description:
# Producing the Vector Autoregression Model (VAR) used in the Paper including the simulation 
# of 60 days ahead. 

# This script relies on the VAR analysis used by Barbará et al (2019).
# In detail we used their replication code made available through Harvard Dataverse and 
# used their VAR model and adopted it to work with our data and adjusted it where needed 
# to fit our data best. 
##########################################################################################
# Contents
##########################################################################################
# 1) Dependencies
# 2) Data Import
# 3) Data processing
# 4) Model Maker
# 5) Calculating ONE-TIME and PERMANENT 10-POINT EFFECTS
# 6) Main 
##########################################################################################
# 1) Dependencies
##########################################################################################
library(tidyr)
library(dplyr)
library(vars)
library(boot)
library(rio)
library(forecast)
library(urca)
library(showtext)
##########################################################################################
# 2) Data Import
##########################################################################################
rm(list = ls())
# - set dir
args = commandArgs()

scriptName = args[substr(args,1,7) == '--file=']

if (length(scriptName) == 0) {
  scriptName <- rstudioapi::getSourceEditorContext()$path
} else {
  scriptName <- substr(scriptName, 8, nchar(scriptName))
}

pathName = substr(
  scriptName, 
  1, 
  nchar(scriptName) - nchar(strsplit(scriptName, '.*[/|\\]')[[1]][2])
)

setwd(pathName)
parent_path <- getwd()

# - load fonts used in plots
font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")

# - define a global seed (used in all scripts)
set.seed(2019)

# - load data
db <- readRDS("../data/main_data_for_paper_all_topics_18_19.RDS")

# - define after how many days we want to evaluate irfs
days_set <- seq(1,9, by = 2)
##########################################################################################
# 3) Data processing
##########################################################################################
# - creating a list with the variables of interest. For the new iteration
variables <- c("Media_SMD", "Gov_PR", "Media_TW", "Org_PR", "Gov_TW", "Org_TW", "Party_PR", "Party_TW", "Politician_TW")

# - creating list of interesting topics for model configuration
mod_conf <- list(c("Environment_Energy"), 
                 c("GenderIssues_Discrimination"),
                 c("Immigration_Asylum"),
                 c("EU_Europa"),
                 c("Environment_Energy", "GenderIssues_Discrimination", "Immigration_Asylum", "EU_Europa")) 

mod_name <- c("Environment_Energy", "Gender_Issues", "Immigration_Asylum", "EU_Europa", "All_Four")

# - selecting only the variables of interest
db <- db[, c("pubDateTime", "selectsclass", variables)]

# - logit transform all series
for (v in variables) {
  # - pulling the series-agenda for that group
  x <- db[,v]
  # - for some groups the last couple observations for each issues are NA,
  # -  making these a 0 
  x[which(is.na(x)),] <- 0.01
  # - applying the non-linear transformation
  logit_x <- log(x / (1-x))
  # If a topic makes up 100 % on a day transform the inf value to 4.59512 which is equal to 99 % (see Barbera et al.)
  logit_x[mapply(is.infinite, logit_x)] <- 4.59512
  db[,v] <- logit_x
}

# - creating a list with the variables of interest
variables_single <- c("Media_SMD", "Gov_PR", "Org_PR", "Media_TW", "Gov_TW", "Org_TW", "Party_PR", "Party_TW", "Politician_TW")
variables_all <- c("Media_SMD", "Gov_PR", "Org_PR", "Media_TW", "Gov_TW", "Org_TW", "Party_PR", "Party_TW", "Politician_TW", "selectsclass")
##########################################################################################
# 4) Model Maker
##########################################################################################
##########################################################################################
# 4.1) Calculate Models with lag = number of covariates
##########################################################################################
var_model_merged <- list()
var_irfs_cum_merged <- list()

for(j in 1:length(mod_conf)){
  
  configuration_j <- mod_conf[[j]]
  if(j == 5){configuration_j <- unlist(configuration_j)}
  maindb <- db %>% filter(selectsclass %in% configuration_j)
  maindb$pubDateTime <- as.numeric(maindb$pubDateTime)
  
  # - a formula object that will facilitate transforming the topic variable from
  # - factor to dummies
  maindb$selectsclass <- as.factor(maindb$selectsclass)
  topic_levels <- levels(maindb$selectsclass)
  
  variables <- variables_single
  if(j == 5){variables <- variables_all}
  
  maindb$selectsclass <- as.character(maindb$selectsclass)
  mformula <- formula(paste0("~ ", 
                             paste0(c(variables), collapse = " + ")))
  model_data <- model.matrix(mformula, maindb[, variables])
  model_data <- model_data[, 2:ncol(model_data)] # removing intercept
  
  # - splitting the covariates of interest from the issue dummy variables
  X_endogenous <- model_data[, which(!grepl("selectsclass", colnames(model_data)))]
  X_exogenous <- model_data[, which(grepl("selectsclass", colnames(model_data)))]
  
  if(length(X_exogenous) == 0){
    # - estimating the model: 1 lag order
    var_model_merged_tmp <- VAR(y = X_endogenous, p = 3)
    
    var_irfs_cum_merged_tmp <- irf(var_model_merged_tmp, n.ahead = 60, cumulative = TRUE, runs = 1000)
  } else {
    # - estimating the model: 1 lag order
    var_model_merged_tmp <- VAR(y = X_endogenous, p = 3, exogen = X_exogenous)
    
    var_irfs_cum_merged_tmp <- irf(var_model_merged_tmp, n.ahead = 60, cumulative = TRUE, runs = 1000)
  }
  #Save different Models in List of Lists
  if(j == 1){
    var_model_merged <- list(var_model_merged_tmp)
    var_irfs_cum_merged <- list(var_irfs_cum_merged_tmp)
  } else {
    var_model_merged[[j]] <- var_model_merged_tmp
    var_irfs_cum_merged[[j]] <- var_irfs_cum_merged_tmp
  }
}

# - save models for later usage
saveRDS(var_model_merged, "../var/var_model_all_small.RDS")
saveRDS(var_irfs_cum_merged, "../var/var_irfs_main_all_small.RDS")
##########################################################################################
# 5) Calculating ONE-TIME and PERMANENT 10-POINT EFFECTS
##########################################################################################
var_irfs <- var_irfs_cum_merged
irf_data <- list()

# Loop through all configurations
for(k in 1:length(var_irfs)){
  # Get Model K:
  var_irfs_k <- var_irfs[[k]]
  
  variables <- names(var_irfs_k$irf)
  
  # - a list with the elements of interest from the IRF object
  elements_to_pull <- c("irf", "Upper", "Lower")
  
  # - initializing and filling a dataset with the IRF info
  irf_data_tmp <- NULL
  for (el in elements_to_pull) {
    new_irf_info <- var_irfs_k[el][[1]]
    for (out in variables) {
      new_irf_var_data <- as.data.frame(new_irf_info[out][[1]])
      # - take inverse logit to transform the effects to percentage point changes
      new_irf_var_data_transf <- as.data.frame(
        sapply(1:ncol(new_irf_var_data), function(j)
          inv.logit(new_irf_var_data[,j]) - 0.5))
      colnames(new_irf_var_data_transf) <- colnames(new_irf_var_data)
      new_irf_var_data_long <- new_irf_var_data_transf %>%
        tidyr::gather(cov, value)
      new_irf_var_data_long$out <- out
      new_irf_var_data_long$day <- rep(1:nrow(new_irf_var_data), 
                                       length(unique(new_irf_var_data_long$cov)))
      new_irf_var_data_long$e_type <- el
      irf_data_tmp <- rbind(irf_data_tmp, new_irf_var_data_long)
    }
  }
  
  # - give easier labels to the estimate types (e.g. Lower --> lwr)
  irf_data_tmp$e_type <- recode(irf_data_tmp$e_type,
                                `irf` = "pe",
                                `Lower` = "lwr", 
                                `Upper` = "upr")
  if(k == 1){
    irf_data <- list(irf_data_tmp)
  } else {
    irf_data[[k]] <- irf_data_tmp
  }
  
}
##########################################################################################
# 6)  MAIN
##########################################################################################
for(l in 1:length(irf_data)){
  new_irf_data <- NULL
  # - a vector with the name of the variables
  variables <- unique(irf_data[[l]]$cov)
  
  # - deciding the number of days to simulate
  DAYS <- 60
  
  tmpirf <- irf_data[[l]]
  
  irf_data_tmp <- tmpirf %>%
    filter(day <= (DAYS + 1))
  
  # - iterating through covariates
  for (covariate in variables) {
    # -iterating through outcomes
    for (outcome in variables) {
      # - skipping when covariate and response are the same
      if (covariate != outcome) {
        # - initializing a cummulative-shocks matrix for this scenario: two 3-dim 
        # - matrix, one matrix for the covariate and one matrix for the response,
        # - and one dimension for the point estimate and the two other dimensions
        # - for the lower and upper bounds of the estimate
        cov_mat <- array(0, dim = c(DAYS, DAYS, 3))
        out_mat <- array(0, dim = c(DAYS, DAYS, 3))
        
        # - pull the full 15-day IRFs for the endogenous covariate
        cov_resp <- irf_data_tmp %>%
          filter(cov == covariate, out == covariate) %>%
          # - remove 1st row: it's part of the set-up (repsonse at day 0)
          filter(day != 1) %>%
          mutate(day = day -1)
        
        # - pull the full 15-day IRFs for the particular outcome variable
        out_resp <- irf_data_tmp %>%
          filter(cov == covariate, out == outcome) %>%
          # - remove 1st row: it's part of the set-up (repsonse at day 0)
          filter(day != 1) %>%
          mutate(day = day -1)
        
        # - transforming the 15-day IRFs for the covariate and outcome to a wide
        # - 3-column format (one column per estimate type: pe, lwr, upr)
        or_cov_resp <- cov_resp %>%
          dplyr::select(day, value, e_type) %>%
          tidyr::spread(e_type, value) %>%
          dplyr::select(-day)
        
        or_out_resp <- out_resp %>%
          dplyr::select(day, value, e_type) %>%
          tidyr::spread(e_type, value) %>%
          dplyr::select(-day)
        
        # - fill up the first rows of the scenario matrices with the original 
        # - 1-day shock responses
        cov_mat[1,,1:3] <- or_cov_resp %>%
          as.matrix()
        out_mat[1,,1:3] <- or_out_resp %>%
          as.matrix()
        
        for (i in 2:DAYS) {
          # - iterating through the rest of the 15 days, beyond the 1st one
          # - chekcing first how much attention the covariate group is predicted 
          # - to pay to the issue in day i-1
          cov_att_pe <- sum(cov_mat[,(i-1),2])
          
          # - calculating how big a new shock needs to be in order for the 
          # - covariate group to keep its attention to 100%
          cov_new_shock <- 1 - cov_att_pe
          
          # - re-scaling the original 100 percentage point shock to the new shock
          cov_new_resp <- or_cov_resp[1:(DAYS-(i-1)),] * cov_new_shock
          out_new_resp <- or_out_resp[1:(DAYS-(i-1)),] * cov_new_shock
          
          # - adding the response to this new shock to the scenario matrices
          cov_mat[i,i:DAYS,1:3] <- cov_new_resp %>%
            as.matrix()
          out_mat[i,i:DAYS,1:3] <- out_new_resp %>%
            as.matrix()
        }
        # - saving the output for this cov --> out 
        new_rows <- rbind(
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,1]),
            out = outcome,
            day = 1:DAYS,
            e_type = "lwr",
            data_type = "structural"),
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,2]),
            out = outcome,
            day = 1:DAYS,
            e_type = "pe",
            data_type = "structural"),
          data.frame(
            cov = covariate,
            value = colSums(out_mat[,,3]),
            out = outcome,
            day = 1:DAYS,
            e_type = "upr",
            data_type = "structural")
        )
        new_irf_data <- rbind(new_irf_data, new_rows)
      }
    }
  }
  
  # - merging this new type of IRFs with the "regular" 1-time-shock IRFs
  tmpirf$data_type <- "one_time_shock"
  tmpirf <- tmpirf %>%
    # - correct the original data for the fact that day 1 is just pre-setting
    filter(day != 1) %>% 
    mutate(day = day -1)
  
  all_irf_data <- rbind(tmpirf, new_irf_data)
  
  # - removing from the dataset cases in which covariate and outcome are the same
  all_irf_data <- all_irf_data %>%
    filter(cov != out)
  
  # - a wide version of the dataset, with a separate column for each estimate type
  all_irf_data_wide <- all_irf_data %>%
    tidyr::spread(e_type, value)
  
  # - simulate one-time and structural shocks of 10 instead of 100 percentage pts.
  # - Present the results in 0-100 scale instead of 0-1
  all_irf_data_wide <- all_irf_data %>%
    mutate(value = (value / 10) * 100) %>%
    tidyr::spread(e_type, value)
  
  # - better labels for the data type
  all_irf_data_wide$data_type <- recode(
    all_irf_data_wide$data_type,
    `one_time_shock` =  "Effect of a one time 10 percentage point attention increase at day 0",
    `structural` = "Effect of a structural 10 percentage point attention increase at day 0")
  
  
  if(l == 1){
    all_irf_data_wide_all <- list(all_irf_data_wide)
  } else {
    all_irf_data_wide_all[[l]] <- all_irf_data_wide
  }
}

saveRDS(all_irf_data_wide_all,
        "../var/onetime-structural-shock-irfs-results_all_small.RDS")
##########################################################################################









