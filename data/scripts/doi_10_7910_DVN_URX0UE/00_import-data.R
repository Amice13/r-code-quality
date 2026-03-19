# Libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(cyphr)


# Load Data ---------------------------------------------------------------
message("Loading data...")



launch_raw    <- read_csv("./Data/launch_1_raw.csv")
recontact_raw <- read_csv("./Data/recontact_1_raw.csv")


# Process Data ------------------------------------------------------------
message("Processing data...")
# Columns Qualtrics hides from us, to drop:
censored_cols <- c("IPAddress", "RecipientLastName", "RecipientFirstName",
                   "RecipientEmail", "ExternalReference", "LocationLatitude",
                   "LocationLongitude")

recontact <- recontact_raw %>% 
  filter(str_detect(irb_consent, "Yes, I would like to")) %>% 
  select(-all_of(censored_cols)) %>% 
  rename_all(paste0, "_followup")



# Join Survey Waves -------------------------------------------------------
# Add followup responses to original responses
launch <- launch_raw %>% 
  filter(str_detect(irb_consent, "Yes, I would like to")) %>% 
  select(-all_of(censored_cols)) %>% 
  left_join(recontact, by = c("rid" = "OriginalRID_followup")) 



# Done --------------------------------------------------------------------
message("Done.")
