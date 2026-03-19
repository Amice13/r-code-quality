#####################################################################################################
###### SETUP
#####################################################################################################

## set working directory to folder containing downloaded replication repository
## UNCOMMENT the line below and insert path for your working directory
# setwd("YOUR WORKING DIRECTORY HERE") 

## source base script for functions, paths, etc.
source("base.R")

## load packages
library(ggthemes)
library(lmtest)
library(clubSandwich)
library(spatialreg)
library(tidyverse)

## RHS variables for step-wise model iterations
# see Tables e4.1 and e4.2 in supplement
scratch$rhs <- list(
        "base" = scratch$x[1:3],
        "pov" = scratch$x[1:7],
        "race" = scratch$x[c(1:3, 8:10)],
        "full" = scratch$x
)

df_s <- df %>%
        mutate(across(all_of(scratch$x[-1]), ~ as.numeric(scale(.)))) %>%
        dplyr::select(-contains("moe"))

## convert W to listw object for spatial regression functions
lW <- spdep::mat2listw(W, style = "W")

#####################################################################################################
###### STANDARD LINEAR MODELS (SLMs)
###### See Equation e4.1 in supplement
#####################################################################################################

## estimate SLMs for each combination of outcome and exposure variables
## returns a list with object names that indicate variables used
slm <- map(scratch$y, function(y) {
        mods <- map(scratch$rhs, function(x) {
                f <- as.formula(paste0(y, " ~ ", paste(x, collapse = " + ")))
                mod <- eval(bquote(lm(.(f), data = df, weights = df$n_15o_est)))
                return(mod)
        })
        names(mods) <- paste(substr(y, nchar(y) - 2, nchar(y)), names(scratch$rhs), sep = "_")
        return(mods)
}) %>%
        unlist(recursive = FALSE)

## residual diagnostic tests for spatial effects in SLMs
# see Section e4.2 in supplement 
tests <- resid_spatial_tests(slm, lW, 9999)

## compute robust SEs clustered by state for SLMs
# see section e4.2 in supplement
# returns a list with object names that correspond to list of models above
slm_robust <- map(slm, function(m) {
        # cluster-robust sandwich estimator with Stata default adjustment
        vcm <- clubSandwich::vcovCR(m, cluster = df$state, type = "CR1S")
        # table w/ hypothesis tests & p-values
        tt <- lmtest::coeftest(m, vcov = vcm)
        # table w/ 95% CIs
        ci <- lmtest::coefci(m, vcov = vcm)
        rob <- bind_cols(tibble(variable = names(m$coefficients)), as_tibble(cbind(tt, ci)))
        return(set_names(rob, c("variable", "beta", "se", "test", "p", "ci_lo", "ci_hi")))
        return(rob)
}) %>%
        set_names(paste0(names(slm), "_slm"))

#####################################################################################################
###### SPATIAL ERROR MODELS (SEMs)
###### See Equation e4.2 in supplement
#####################################################################################################

## estimate SEMs for each combination of outcome and exposure variables
# returns a list with object names that indicate variables used
sem <- map(scratch$y, function(y) {
        mods <- map(scratch$rhs, ~ run_sem(y, ., data = df, listw = lW, weights = df$n_15o_est))
        names(mods) <- paste(substr(y, nchar(y) - 2, nchar(y)), names(scratch$rhs), sep = "_")
        return(mods)
}) %>%
        unlist(recursive = FALSE)

## compute robust SEs clusterd by state for SEMs
# see section e4.2 in supplement
# returns a list with object names that correspond to list of models above
sem_robust <- map(sem, function(m) {
        ## first, recover the "target model"
        # this is the model that estimates the SEM coefficients after lambda is estimated by ML
        # `tary` and `tarX` are the outcome vector and exposure matrix, weighted by lambda and W
        # see the examples section of the documentation for spatialreg::LR.Sarlm
        tm <- lm(m$tary ~ m$tarX - 1, weights = m$weights)
        # cluster-robust sandwich estimator with Stata default adjustment
        vcm <- clubSandwich::vcovCR(tm, cluster = df$state, type = "CR1S", df = Inf)
        # table w/ hypothesis tests & p-values
        tt <- lmtest::coeftest(tm, vcov = vcm, df = Inf)
        # table w/ 95% CIs
        ci <- lmtest::coefci(tm, vcov = vcm, df = Inf)
        rob <- bind_cols(tibble(variable = names(m$coefficients)), as_tibble(cbind(tt, ci)))
        return(set_names(rob, c("variable", "beta", "se", "test", "p", "ci_lo", "ci_hi")))
}) %>% 
        set_names(paste0(names(sem), "_sem"))

#####################################################################################################
###### SPATIAL DURBIN ERROR MODELS (SDEMs)
#####################################################################################################

## estimate SEMs for each combination of outcome and exposure variables
# returns a list with object names that indicate variables used
sdem <- map(scratch$y, function(y) {
        mods <- map(scratch$rhs, 
                    ~ run_sem(y, ., data = df, listw = lW, weights = df$n_15o_est, Durbin = TRUE))
        names(mods) <- paste(substr(y, nchar(y) - 2, nchar(y)), names(scratch$rhs), sep = "_")
        return(mods)
}) %>%
        unlist(recursive = FALSE)

sdem_robust <- map(sdem, function(m) {
        ## first, recover the "target model"
        # this is the model that estimates the SEM coefficients after lambda is estimated by ML
        # `tary` and `tarX` are the outcome vector and exposure matrix, weighted by lambda and W
        # see the examples section of the documentation for spatialreg::LR.Sarlm
        tm <- lm(m$tary ~ m$tarX - 1, weights = m$weights)
        # cluster-robust sandwich estimator with Stata default adjustment
        vcm <- clubSandwich::vcovCR(tm, cluster = df$state, type = "CR1S", df = Inf)
        # table w/ hypothesis tests & p-values
        tt <- lmtest::coeftest(tm, vcov = vcm, df = Inf)
        # table w/ 95% CIs
        ci <- lmtest::coefci(tm, vcov = vcm, df = Inf)
        rob <- bind_cols(tibble(variable = names(m$coefficients)), as_tibble(cbind(tt, ci)))
        return(set_names(rob, c("variable", "beta", "se", "test", "p", "ci_lo", "ci_hi")))
}) %>% 
        set_names(paste0(names(sdem), "_sdem"))

#####################################################################################################
###### COMPARE SLM/SEM/SDEM COEFFICIENTS
#####################################################################################################

## Shape/filter data for plotting
cmpr_data <- bind_rows(c(slm_robust, sem_robust, sdem_robust), .id = "model") %>%
        separate(col = "model", into = c("y", "x", "spec"), sep = "_") %>% 
        filter(variable %in% scratch$x[-1] & x == "full") %>%
        mutate(vf = factor(variable, levels = scratch$x[-1]),
               yn = -as.numeric(vf),
               ynf = as.factor(yn),
               y = factor(case_when(y == "mar" ~ "March",
                                    y == "apr" ~ "April",
                                    y == "dif" ~ "Difference"), 
                          levels = c("March", "April", "Difference")),
               spec = factor(case_when(spec == "slm" ~ "Standard linear model (SLM)",
                                       spec == "sem" ~ "Spatial error model (SEM)",
                                       spec == "sdem" ~ "Spatial Durbin error model (SDEM)"), 
                             levels = c("Standard linear model (SLM)", 
                                        "Spatial error model (SEM)", 
                                        "Spatial Durbin error model (SDEM)"))) 
## compare SLM vs. SEM vs. SDEM
cmpr3 <- cmpr_plot(cmpr_data, "vertical")
## compare SLM vs. SEM only
cmpr2 <- cmpr_data %>%
        filter(spec != "Spatial Durbin error model (SDEM)") %>%
        cmpr_plot("horizontal")


