####################################################
# Steven R. Gehrke
# Northern Arizona University
# 21-0405: Starship Robots
# Task: Video Data Cleaning
# Created: 05.16.2022
# Updated: 05.18.2022
####################################################

####################
### SET-UP

# Load libraries
# install.packages(c("tidyverse"))
packages <- c("tidyverse")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
dat_dir <- "H:/_projects/21-0405_SidewalkDeliveryRobots/"

# Import raw conflict, exposure, and site characteristics data
dat_con <- read_csv(paste(dat_dir, "conflict-review_master_v2.csv", sep="_dat/_video/_tabular/"))
dat_exp <- read_csv(paste(dat_dir, "exposure-review_master_v2.csv", sep="_dat/_video/_tabular/"))
dat_box <- read_csv(paste(dat_dir, "bounding-boxes_cdp.csv", sep="_dat/_video/_tabular/"))

####################
### DATA PREP

# CONFLICT: Remove NA fields and intentional conflicts
dat_con <- dat_con[!is.na(dat_con$conflict_id),c(1:17)]
dat_con <- dat_con[dat_con$intentional==0,]

# EXPOSURE: Remove duplicate columns
# head(dat_con); head(dat_exp)
dat_exp2 <- dat_exp[,c(1,10:15)]

# Merge data sets
dat_con2 <- merge(dat_con,dat_exp2,by=c("exposure_id"), all.x=T)
dat_con3 <- merge(dat_con2,dat_box,by=c("site"), all.x=T)

####################
### DATA CLEANING

# Remove extra column headers
colnames(dat_con3)
dat <- dat_con3[,c("conflict_id","day","user1_type","user2_type","conflict_dir","pet","pet_secs","pet_cat","evasive_m1","evasive_m2","intentional","notes",
                   "exposure_id","time_start","time_end","user_robot","user_walk","user_bike","user_other",
                   "site","tot_mupath","tot_blane","tot_swalk","intx_legs","lat_clear")]

# Remove unnecessary data sets
rm(dat_con2,dat_con3,dat_exp2)

# Export data set
write_csv(dat, paste(dat_dir, "dat_pet_clean.csv", sep="_dat/_video/_tabular/"))

####################
### END OF SCRIPT
