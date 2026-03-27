#################################################################################
### load needed packages
#################################################################################

library(readxl)
library(rgeoda)
library(tidyverse)

#################################################################################
### define functions
#################################################################################

## function to run LISA (Local Moran's I) analysis
get_lmi <- function(
          df, # data containing columns of group counts and an ID column
          W, # spatial weights matrix object of class `Weight` 
          id_col,# name of ID column
          drop_col = NULL, # columns to ignore (not calculate LISA)
          iter, # number of iterations for permutation significance test
          p, # p-value for significance treshold
          n_threads = parallel::detectCores() - 1L # number CPU cores to use
                                                   # in permutation tests
) {
     ids <- df[[id_col]]
     dplyr::select(df, -(any_of(c(id_col, drop_col)))) %>%
          map_dfr(function(d) {
               d <- tibble(x = d)
               # LISA analysis
               lmi1 <- local_moran(w = W, df = d, permutations = iter, 
                                   permutation_method = "complete", 
                                   significance_cutoff = p, 
                                   cpu_threads = n_threads, seed = 69)
               # extract test statistics, p-values, and cluster types
               lmi2 <- tibble(lmi = lisa_values(lmi1),
                              p = lisa_pvalues(lmi1),
                              clus = lisa_clusters(lmi1))
               # get text labels for cluster types
               lmi3 <- lisa_labels(lmi1)
               # match cluster types to text labels
               lmi2$clus <- tolower(map_chr(lmi2$clus, ~ lmi3[. + 1]))
               lmi2[[id_col]] <- ids
               return(lmi2)
          }, .id = "abb") %>%
          dplyr::select(all_of(c(id_col, "abb", "lmi", "p", "clus")))
}

#################################################################################
### prep data
#################################################################################

## labels for groups of interest
cw <- read_excel("data/glossaries.xlsx", 
                 sheet = "Groups (LISA Analyses)", 
                 col_types = c("text", rep("logical", 4)))

## list of tracts used in LISA analysis;
## excludes tracts w/ very low populations
lt <- read_csv("data/clean/lisa_tracts.csv", col_types = "cd")

## get tract-level group counts for groups of interest
source("scripts/get_groups_tract.R")

## get spatial polygons for tracts of interest
source("scripts/get_polygons.R")
pgn <- get_polygons(lt$ct_id, 2019L, 32611, FALSE, FALSE) %>%
     right_join(lt)

## prep tract-level group counts for LISA analysis
ctw <- ct %>%
     # long-to-wide (groups in columns)
     pivot_wider(id_cols = "ct_id", names_from = "abb", values_from = "est") %>%
     right_join(lt) %>%
     # normalize for census tract area
     mutate(across(-c(ct_id, area), ~ . / area)) 

## get spatial weights (second-order rook adjacency weighting)
sw <- rook_weights(pgn, 2, TRUE)

rm(lt, ct, pgn, get_polygons)
gc()

#################################################################################
### run analysis
#################################################################################

## LISA analysis w/ 9,999 iterations and alpha level 99.9
lmi <- get_lmi(ctw, sw, "ct_id", "area", 9999, 0.001)

## determine population centers from LISA analysis
pc <- lmi %>%
     dplyr::select(ct_id, abb, clus) %>%
     left_join(cw) %>%
     # define "high-high" clusters only as population centers
     mutate(clus = if_else(clus == "high-high", TRUE, FALSE),
            across(latino:white, ~ . * clus)) %>%
     group_by(ct_id) %>%
     # if tract is a population center for any of the four main ethnoracial
     # groups' subgroups, it is a population center for the main group
     summarize(across(latino:white, ~ sum(.) > 0)) 