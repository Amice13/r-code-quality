#################################################################################
### prep data
#################################################################################

## read in tract-level group counts
source("scripts/get_groups_tract.R")

## read in CDC Social Vulnerability Index Data for California, 2018
ses_tract <- read_csv("data/raw/California.csv", 
                      col_types = cols(FIPS = col_character())) %>%
     # keep tract ID and SES component of index, which we use to measure SES
     dplyr::select(ct_id = FIPS, ses = SPL_THEME1) %>%
     # values of -999 are NAs; recode
     mutate(ses = if_else(ses == -999, NA_real_, ses))

#################################################################################
### SES calculations
#################################################################################

## calculate group populations by tract SES quartile
ses_quartiles <- ct %>%
     # filter to LA Metro only
     filter(cbsa_name == "Los Angeles CA") %>%
     left_join(svi) %>%
     # exclude tracts missing SES values
     drop_na(ses) %>%
     # create SES quartiles (1 = highest; 4 = lowest)
     mutate(ses4 = as.factor(ntile(ses, 4))) %>%
     # aggregate by group & quartile
     group_by(abb, ses4) %>%
     summarize(est = sum(est)) %>%
     # get proportion of each group in each quartile
     group_by(abb) %>%
     mutate(pr_grp_est = est / sum(est)) %>%
     ungroup() %>%
     drop_na() %>%
     arrange(abb)