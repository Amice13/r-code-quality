# Libraries ---------------------------------------------------------------
# Handle relative filepaths on everyone's machines
library(here)

# Data manipulation
library(tidyverse)


# Helpers -----------------------------------------------------------------
# Center and scale columns based on mean and variance of a reference group
standardize <- function(x, based_on = x, restricted_to) {
  mu    <- mean(based_on[restricted_to], na.rm = T)
  sigma <- sd(based_on[restricted_to], na.rm = T)
  
  (x - mu) / sigma
}

mean_cols <- function(...) { rowMeans(data.frame(...), na.rm = FALSE) } #MK: used to be true and added NAs as 0

# Load Data ---------------------------------------------------------------
message("Loading data...")
launch_raw <- read_rds("temp/launch_1_scr.rds")


# Standardize -------------------------------------------------------------
message("Standardizing...")
rating_cols <- c("vid_prime_useful", "vid_prime_trustworthy",
                 "vid_1_useful", "vid_1_trustworthy",
                 "vid_1_follow_recs", "vid_1_will_share",
                 "vid_2_useful", "vid_2_trustworthy",
                 "vid_2_follow_recs", "vid_2_will_share",
                 "vid_3_useful", "vid_3_trustworthy",
                 "vid_3_follow_recs", "vid_3_will_share")

link_cols <- c("link_exercise", "link_state_hotline", "link_testing",
               "link_mgh_resources", "link_mgh_app")

safety_cols <- c("safety_mask_in_often", "safety_mask_out_often",
                 "safety_hands_often", "safety_distance_often")

std_cols <- c(rating_cols, link_cols, safety_cols)

launch_std <- launch_raw %>% 
  mutate(across(all_of(std_cols), 
                standardize, 
                restricted_to = pure_placebo == 1,
                .names = "{col}_std"),
         know_3_practices_delta = know_3_practices_followup - know_3_practices,
         know_4_symptoms_delta  = know_4_symptoms_followup - know_4_symptoms,
         link_index = mean_cols(link_exercise_std,
                                link_state_hotline_std,
                                link_testing_std,
                                link_mgh_resources_std,
                                link_mgh_app_std),
         link_index = ifelse(is.na(links_total), NA, link_index), #recode to be consistent with sum/index
         links_total = ifelse(is.na(link_index), NA, links_total), #recode to be consistent with sum/index
         safety_mask_in_often_followup_std = standardize(safety_mask_in_often_followup,
                                                         based_on      = safety_mask_in_often,
                                                         restricted_to = pure_placebo == 1),
         safety_mask_out_often_followup_std = standardize(safety_mask_out_often_followup,
                                                          based_on      = safety_mask_out_often,
                                                          restricted_to = pure_placebo == 1),
         safety_hands_often_followup_std = standardize(safety_hands_often_followup,
                                                       based_on      = safety_hands_often,
                                                       restricted_to = pure_placebo == 1),
         safety_distance_often_followup_std = standardize(safety_distance_often_followup,
                                                          based_on      = safety_distance_often,
                                                          restricted_to = pure_placebo == 1),
         safety_index = mean_cols(safety_mask_in_often_std,
                                  safety_mask_out_often_std,
                                  safety_hands_often_std,
                                  safety_distance_often_std),
         safety_index_followup = mean_cols(safety_mask_in_often_followup_std,
                                           safety_mask_out_often_followup_std,
                                           safety_hands_often_followup_std,
                                           safety_distance_often_followup_std),
         safety_index_followup = ifelse(is.na(safety_total), NA, safety_index_followup), #recode to be consistent with sum/index
         safety_total = ifelse(is.na(safety_index_followup), NA, safety_total), #recode to be consistent with sum/index
         safety_index_delta = safety_index_followup - safety_index,
         behavior_index = standardize(wtp_masks,
                                      based_on      = wtp_masks,
                                      restricted_to = pure_placebo == 1)/3
         + safety_index_followup / 3 + link_index /3,
         behavior_index_nofollow = standardize(wtp_masks,
                                               based_on      = wtp_masks,
                                               restricted_to = pure_placebo == 1)/2
         + link_index /2)



# Write -------------------------------------------------------------------
message("Writing...")
write_rds(launch_std, "temp/launch_1_std.rds")


# Done --------------------------------------------------------------------
message("Done.")
