# Seed --------------------------------------------------------------------
set.seed(596298)


# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(glue)


# Helpers -----------------------------------------------------------------
# Reorders a vector
reindex <- function(x, new_index) { x[new_index] }


# Load Data ---------------------------------------------------------------
message("Loading data...")
launch <- read_rds("temp/launch_1.rds")


# Process Data ------------------------------------------------------------
message("Processing data...")
vid_idx <- c("prime", "1", "2", "3")

tmt_cols <- c("concord_ama", "concord_doc", "concord_both", "concord_any",
              "racism_ama", "racism_doc", "racism_any",
              "covid_any", "covid_only",
              "pure_placebo",
              "covid_any__racism_doc",
              "covid_any__racism_ama",
              "covid_any__racism_any",
              "covid_any__concord_doc",
              "covid_any__concord_any",
              "covid_any__racism_ama__concord_any",
              glue("vid_{vid_idx}"),
              glue("vid_{vid_idx}_id"),
              glue("vid_{vid_idx}_race"),
              glue("vid_{vid_idx}_sex"),
              glue("vid_{vid_idx}_name"))


launch_scr <- launch

# Write -------------------------------------------------------------------
message("Saving")
write_rds(launch_scr, "temp/launch_1_scr.rds")


# Done --------------------------------------------------------------------
message("Done.")

