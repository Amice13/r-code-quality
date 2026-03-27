#################################################################################
### load needed packages
#################################################################################

library(haven)
library(readxl)
library(spdep)
library(tidyverse)
library(seg)
library(OasisR)

#################################################################################
### define functions
#################################################################################

## standardize a matrix by row (normalize such that all rows sum to 1)
row_stand <- function(m) {
     t(apply(m, 1, function(x) {
          d1 <- x / sum(x)
          # return 0 for all row elements when rows sum to 0
          ifelse(is.nan(d1), 0, d1)
     }))
}

## get binary adjacency matrix
get_adj_mat <- function(
          df, # dataframe with id column `ct_id`
          nbs # two-column dataframe of tract-neighbor pairs
) { 
     ids <- sort(unique(df$ct_id))
     # identify counties represented in data
     counties <- tibble(county = unique(substr(ids, 1, 5)))
     # identify neighbors
     adj <- tibble(ct_id = ids) %>%
          left_join(nbs, by = "ct_id") %>%
          mutate(nb = 1L, county = substr(nid, 1, 5)) %>%
          # exclude neighbors in counties not included above
          semi_join(counties, by = "county") %>%
          dplyr::select(-county)
     # make square matrix
     expand_grid(ct_id = ids, nid = ids) %>%
          left_join(adj, by = c("ct_id", "nid")) %>%
          mutate(nb = if_else(is.na(nb), 0L, nb)) %>%
          pivot_wider(names_from = "nid", values_from = "nb") %>%
          column_to_rownames("ct_id") %>%
          as.matrix()
}

## get group counts in wide form (one row per areal unit, groups in columns)
## with identifiers as row-names (not a column)
get_group_mat <- function(
          df, # tidy dataframe containing group counts
          as.mat = FALSE # return square matrix? default: 2-column dataframe
) { 
     out <- df %>%
          dplyr::select(ct_id, est, abb) %>%
          drop_na("est") %>%
          pivot_wider(values_from = "est", names_from = "abb") %>%
          arrange(ct_id) %>%
          column_to_rownames("ct_id")
     if (as.mat) {
          out <- as.matrix(out)
     }
     out
}

## compute Moran's I (clustering index)
get_I <- function(
          x, # numeric vector of group population counts
          lw, # listw object of spatial weights
          n_sim # number of iterations for permutation significance test
) {
     m <- spdep::moran.mc(x, lw, n_sim, zero.policy = TRUE)
     tibble(
          index = "clustering",
          value = m$statistic,
          p = m$p.value
     )
}

## spatial dissimilarity index
get_D <- function(
          m, # matrix/dataframe of group counts from get_group_mat
          pgn # object of class Spatial containing polygons
) {
     cmb <- combn(colnames(m), 2)
     map2_dfr(cmb[1, ], cmb[2, ], function(.x, .y) {
          sds <- spseg(pgn,
                       as.matrix(dplyr::select(m, all_of(c(
                            .x, .y
                       )))),
                       negative.rm = FALSE)
          tibble(g1 = .x,
                 g2 = .y,
                 value = sds@d)
     }) %>%
          mutate(index = "evenness")
}

## spatial exposure/isolation index
get_P <- function(
          group_mat, # matrix/dataframe of group counts from get_group_mat;
                     # columns must sum to total population
          pgn, # object of class Spatial containing polygons
          yr # deprecated
) {
     OasisR::spatinteract(group_mat, pgn) %>%
          as.table() %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          set_names(c("g1", "g2", "value")) %>%
          mutate(index = "exposure")
}

## get concentration profiles 
get_cp <- function(
          df,  # matrix/dataframe of group counts from get_group_mat;
               # columns must sum to total population
          n_thr, # number of thresholds for graph output
          group = seq_along(df) # columns in data to analyze
) {
     m <- as.matrix(df)
     map_dfr(group, function(a) {
          cp <- seg::conprof(m, a, n_thr, FALSE)
          tibble(
               plot_x = list(cp$x),
               plot_y = list(cp$y),
               value = cp$d,
               g1 = colnames(m)[a],
               index = "concentration"
          )
     })
}

## compute all four indices
get_indices <- function(
          df, # dataframe to be run through get_group_mat function
          adj_nbs, # adjacency crosswalk
          yr, # vintage for TIGER/LINE shapefiles
          n_sim = 1e3L, # number of iterations for significance tests
          n_thr = 999L # number of thresholds for concentration profiles
) {
     # dataframe with groups of interest plus total population in columns
     df1 <- get_group_mat(df)
     # same as above without total population column
     df2 <- dplyr::select(df1, -pop)
     # replace total population with  population of"all other" groups
     df3 <- rowwise(df1) %>%
          mutate(other = pop - sum(c_across(-pop))) %>%
          ungroup() %>%
          dplyr::select(-pop)
     # polygons
     pgn <- get_polygons(rownames(df2), yr = yr)
     # adjacency matrix
     am <- get_adj_mat(rownames_to_column(df2, "ct_id"), adj_nbs)
     # row-standardized adjacency matrix
     am_rs <- mat2listw(row_stand(am), style = "W")
     # concentration 
     cp <- get_cp(df3, n_thr, 1:(length(df3)-1))
     # clustering
     mi <- map_dfr(df2, ~ get_I(., am_rs, n_sim), .id = "g1")
     # evenness
     d <- get_D(df2, pgn)
     # isolation/exposure
     ie <- get_P(df3, pgn)
     # combine
     out <- bind_rows(cp, mi, d, ie) %>%
          dplyr::select(index, g1, g2, value, p, plot_x, plot_y)
     return(out)
}

## function get spatial polygons for tracts of interest
source("scripts/get_polygons.R")

#################################################################################
### prep data
#################################################################################

## define groups of interest
gr_an <- read_excel("data/glossaries.xlsx", 
                    sheet = "Groups (Segregation Analyses)")

## crosswalk for groups of interest plus column for total population
cw <- tibble(abb = "pop", lab = "Total population") %>%
     bind_rows(gr_an) %>%
     mutate(abb = factor(abb, levels = abb))

## get tract-level group counts for groups of interest
source("scripts/get_groups_tract.R")

## adjacency crosswalk for census tracts for use in get_adj_mat function
adj <- read_dta("data/raw/nlist_2010.dta") %>%
     set_names(c("ct_id", "nid")) %>%
     mutate(across(.fns = ~ str_pad(., 11, "left", "0"))) 

#################################################################################
### calculate indices
#################################################################################

segregation_indices <- ct %>%
     # run analyses by metro area
     group_by(cbsa_name) %>%
     group_modify(~ get_indices(., adj, yr = 2019L)) %>%
     # get indices in standard-deviation units (z-scores)
     group_by(index, g1, g2) %>%
     mutate(z = as.numeric(scale(value))) %>%
     ungroup()