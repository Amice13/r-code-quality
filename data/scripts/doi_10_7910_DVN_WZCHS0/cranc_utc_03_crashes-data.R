####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Data Set (Study 2: Crashes)
# Created: 07.10.2023
# Updated: 07.12.2023
####################################################

####################
### SET-UP
####################

# Load libraries
# install.packages(c("tidyverse","foreign"))
packages <- c("tidyverse","foreign")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
# dat_dir <- "H:/_projects/22-0338_BicyclingAccessibility/"
dat_dir <- "F:/22-0338_BicyclingAccessibility/"

####################
### MODEL DATA SET
####################

# Import IBC employment data set
dat_ibc <- read_csv(paste(dat_dir, "dat_access_ibc_e.csv", sep="_data/_tabular/_outputs/"))
colnames(dat_ibc)

# Reduce accessibility outcomes: 15 and 30 mins
dat_ibc <- dat_ibc[,c(1,3,8:9,14:15)]
colnames(dat_ibc) <- c("From","mpo","e_10_15","e_10_15_lts","e_25_30","e_25_30_lts") # correct data, incorrect column headings in raw data set

####################
### CYCLIST CRASHES
####################

# Import and clean crash data sets

# CYMPO
dat_cympo <- read_csv(paste(dat_dir, "ibc_cympo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_cympo$From <- str_remove_all(dat_cympo$From," ")
dat_cympo <- dat_cympo %>% mutate_if(is.numeric, as.numeric); str(dat_cympo)
# dat_cympo[is.na(dat_cympo)] <- 0; dat_cympo <- dat_cympo %>% arrange(From)
dat_cympo_15 <- dat_cympo[dat_cympo$mins=="15",]; dat_cympo_30 <- dat_cympo[dat_cympo$mins=="30",]
# FMPO
dat_fmpo <- read_csv(paste(dat_dir, "ibc_fmpo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_fmpo$From <- str_remove_all(dat_fmpo$From," ")
dat_fmpo <- dat_fmpo %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo)
# dat_fmpo[is.na(dat_fmpo)] <- 0; dat_fmpo <- dat_fmpo %>% arrange(From)
dat_fmpo_15 <- dat_fmpo[dat_fmpo$mins=="15",]; dat_fmpo_30 <- dat_fmpo[dat_fmpo$mins=="30",]
# LHMPO
dat_lhmpo <- read_csv(paste(dat_dir, "ibc_lhmpo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_lhmpo$From <- str_remove_all(dat_lhmpo$From," ")
dat_lhmpo <- dat_lhmpo %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo)
# dat_lhmpo[is.na(dat_lhmpo)] <- 0; dat_lhmpo <- dat_lhmpo %>% arrange(From)
dat_lhmpo_15 <- dat_lhmpo[dat_lhmpo$mins=="15",]; dat_lhmpo_30 <- dat_lhmpo[dat_lhmpo$mins=="30",]
# MAG
dat_mag <- read_csv(paste(dat_dir, "ibc_mag_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_mag$From <- str_remove_all(dat_mag$From," ")
dat_mag <- dat_mag %>% mutate_if(is.numeric, as.numeric); str(dat_mag)
# dat_mag[is.na(dat_mag)] <- 0; dat_mag <- dat_mag %>% arrange(From)
dat_mag_15 <- dat_mag[dat_mag$mins=="15",]; dat_mag_30 <- dat_mag[dat_mag$mins=="30",]
# PAG
dat_pag <- read_csv(paste(dat_dir, "ibc_pag_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_pag$From <- str_remove_all(dat_pag$From," ")
dat_pag <- dat_pag %>% mutate_if(is.numeric, as.numeric); str(dat_pag)
# dat_pag[is.na(dat_pag)] <- 0; dat_pag <- dat_pag %>% arrange(From)
dat_pag_15 <- dat_pag[dat_pag$mins=="15",]; dat_pag_30 <- dat_pag[dat_pag$mins=="30",]
# SCMPO
dat_scmpo <- read_csv(paste(dat_dir, "ibc_scmpo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_scmpo$From <- str_remove_all(dat_scmpo$From," ")
dat_scmpo <- dat_scmpo %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo)
# dat_scmpo[is.na(dat_scmpo)] <- 0; dat_scmpo <- dat_scmpo %>% arrange(From)
dat_scmpo_15 <- dat_scmpo[dat_scmpo$mins=="15",]; dat_scmpo_30 <- dat_scmpo[dat_scmpo$mins=="30",]
# SVMPO
dat_svmpo <- read_csv(paste(dat_dir, "ibc_svmpo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_svmpo$From <- str_remove_all(dat_svmpo$From," ")
dat_svmpo <- dat_svmpo %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo)
# dat_svmpo[is.na(dat_svmpo)] <- 0; dat_svmpo <- dat_svmpo %>% arrange(From)
dat_svmpo_15 <- dat_svmpo[dat_svmpo$mins=="15",]; dat_svmpo_30 <- dat_svmpo[dat_svmpo$mins=="30",]
# YMPO
dat_ympo <- read_csv(paste(dat_dir, "ibc_ympo_crash_analysis.csv", sep="_data/_tabular/_crashes/"))
dat_ympo$From <- str_remove_all(dat_ympo$From," ")
dat_ympo <- dat_ympo %>% mutate_if(is.numeric, as.numeric); str(dat_ympo)
# dat_ympo[is.na(dat_ympo)] <- 0; dat_ympo <- dat_ympo %>% arrange(From)
dat_ympo_15 <- dat_ympo[dat_ympo$mins=="15",]; dat_ympo_30 <- dat_ympo[dat_ympo$mins=="30",]

# Merge crash data sets
dat_15 <- Reduce(rbind,list(dat_cympo_15,dat_fmpo_15,dat_lhmpo_15,dat_mag_15,dat_pag_15,dat_scmpo_15,dat_svmpo_15,dat_ympo_15))
dat_15 <- Reduce(function(x, y) merge(x, y, by="From", all.x=T), list(dat_ibc,dat_15))
dat_15 <- dat_15 %>% arrange(From)
dat_30 <- Reduce(rbind,list(dat_cympo_30,dat_fmpo_30,dat_lhmpo_30,dat_mag_30,dat_pag_30,dat_scmpo_30,dat_svmpo_30,dat_ympo_30))
dat_30 <- Reduce(function(x, y) merge(x, y, by="From", all.x=T), list(dat_ibc,dat_30))
dat_30 <- dat_30 %>% arrange(From)
rm(dat_cympo_15,dat_fmpo_15,dat_lhmpo_15,dat_mag_15,dat_pag_15,dat_scmpo_15,dat_svmpo_15,dat_ympo_15)
rm(dat_cympo_30,dat_fmpo_30,dat_lhmpo_30,dat_mag_30,dat_pag_30,dat_scmpo_30,dat_svmpo_30,dat_ympo_30)
rm(dat_cympo,dat_fmpo,dat_lhmpo,dat_mag,dat_pag,dat_scmpo,dat_svmpo,dat_ympo)

# Export crash data sets
write_csv(dat_15, (file=paste(dat_dir, "crashes_ibc_15.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_30, (file=paste(dat_dir, "crashes_ibc_30.csv", sep="_data/_tabular/_outputs/")))

####################
### GRID CONTEXT
####################

# Import and merge ACS/Census data sets
# dat_acs <- read_csv(paste(dat_dir, "grids_tracts_acs.csv", sep="_data/_tabular/_inputs/"))
# grid_id <- dat_15[,c("From","mpo")]; colnames(grid_id) <- c("GRID_ID2","mpo")
# dat_acs <- merge(grid_id,dat_acs,by="GRID_ID2")

dat_acs <- read_csv(paste(dat_dir, "grids_tracts_acs.csv", sep="_data/_tabular/_inputs/"))
colnames(dat_acs)[4] <- c("From")

# Reduce and rename columns
colnames(dat_acs)
dat_acs_2 <- dat_acs[, c(4,8:12,18:19,22:26,32:39,51:52,53:57,20:21,27:31)]
colnames(dat_acs_2) <- c("From","age_00_19","age_20_34","age_35_44","age_45_64","age_65_up","sex_m","sex_f","edu_less","edu_hs","edu_some","edu_assc","edu_bach",
                         "rac_whi","rac_baa","rac_asi","rac_aian","rac_nhpi","rac_some","rac_two","rac_hisp","emp_lab","emp_out",
                         "inc_000_024","inc_025_049","inc_050_099","inc_100_149","inc_150_up","ten_own","ten_rnt","veh_0","veh_1","veh_2","veh_3","veh_4")
dat_acs_2[is.na(dat_acs_2)] <- 0
colnames(dat_acs_2)

# Create variable percentages
dat_acs_2$page_00_19 <- NA
dat_acs_2$page_00_19 <- dat_acs_2$age_00_19/(dat_acs_2$age_00_19+dat_acs_2$age_20_34+dat_acs_2$age_35_44+dat_acs_2$age_45_64+dat_acs_2$age_65_up)
dat_acs_2$page_20_34 <- NA
dat_acs_2$page_20_34 <- dat_acs_2$age_20_34/(dat_acs_2$age_00_19+dat_acs_2$age_20_34+dat_acs_2$age_35_44+dat_acs_2$age_45_64+dat_acs_2$age_65_up)
dat_acs_2$page_35_44 <- NA
dat_acs_2$page_35_44 <- dat_acs_2$age_35_44/(dat_acs_2$age_00_19+dat_acs_2$age_20_34+dat_acs_2$age_35_44+dat_acs_2$age_45_64+dat_acs_2$age_65_up)
dat_acs_2$page_45_64 <- NA
dat_acs_2$page_45_64 <- dat_acs_2$age_45_64/(dat_acs_2$age_00_19+dat_acs_2$age_20_34+dat_acs_2$age_35_44+dat_acs_2$age_45_64+dat_acs_2$age_65_up)
dat_acs_2$page_65_up <- NA
dat_acs_2$page_65_up <- dat_acs_2$age_65_up/(dat_acs_2$age_00_19+dat_acs_2$age_20_34+dat_acs_2$age_35_44+dat_acs_2$age_45_64+dat_acs_2$age_65_up)
dat_acs_2$psex_m <- NA
dat_acs_2$psex_m <- dat_acs_2$sex_m/(dat_acs_2$sex_m+dat_acs_2$sex_f)
dat_acs_2$psex_f <- NA
dat_acs_2$psex_f <- dat_acs_2$sex_f/(dat_acs_2$sex_m+dat_acs_2$sex_f)
dat_acs_2$pedu_1 <- NA
dat_acs_2$pedu_1 <- (dat_acs_2$edu_less+dat_acs_2$edu_hs)/(dat_acs_2$edu_less+dat_acs_2$edu_hs+dat_acs_2$edu_some+dat_acs_2$edu_assc+dat_acs_2$edu_bach)
dat_acs_2$pedu_2 <- NA
dat_acs_2$pedu_2 <- (dat_acs_2$edu_some+dat_acs_2$edu_assc)/(dat_acs_2$edu_less+dat_acs_2$edu_hs+dat_acs_2$edu_some+dat_acs_2$edu_assc+dat_acs_2$edu_bach)
dat_acs_2$pedu_3 <- NA
dat_acs_2$pedu_3 <- (dat_acs_2$edu_bach)/(dat_acs_2$edu_less+dat_acs_2$edu_hs+dat_acs_2$edu_some+dat_acs_2$edu_assc+dat_acs_2$edu_bach)
dat_acs_2$prac_whi <- NA
dat_acs_2$prac_whi <- dat_acs_2$rac_whi/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$prac_baa <- NA
dat_acs_2$prac_baa <- dat_acs_2$rac_baa/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$prac_asi <- NA
dat_acs_2$prac_asi <- dat_acs_2$rac_asi/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$prac_aian <- NA
dat_acs_2$prac_aian <- dat_acs_2$rac_aian/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$prac_hisp <- NA
dat_acs_2$prac_hisp <- dat_acs_2$rac_hisp/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$prac_ocat <- NA
dat_acs_2$prac_ocat <- (dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two)/
  (dat_acs_2$rac_whi+dat_acs_2$rac_baa+dat_acs_2$rac_asi+dat_acs_2$rac_aian+dat_acs_2$rac_nhpi+dat_acs_2$rac_some+dat_acs_2$rac_two+dat_acs_2$rac_hisp)
dat_acs_2$pemp_lab <- NA
dat_acs_2$pemp_lab <- dat_acs_2$emp_lab/(dat_acs_2$emp_lab+dat_acs_2$emp_out)
dat_acs_2$pemp_out <- NA
dat_acs_2$pemp_out <- dat_acs_2$emp_out/(dat_acs_2$emp_lab+dat_acs_2$emp_out)
dat_acs_2$pinc_000_024 <- NA
dat_acs_2$pinc_000_024 <- dat_acs_2$inc_000_024/(dat_acs_2$inc_000_024+dat_acs_2$inc_025_049+dat_acs_2$inc_050_099+dat_acs_2$inc_100_149+dat_acs_2$inc_150_up)
dat_acs_2$pinc_025_049 <- NA
dat_acs_2$pinc_025_049 <- dat_acs_2$inc_025_049/(dat_acs_2$inc_000_024+dat_acs_2$inc_025_049+dat_acs_2$inc_050_099+dat_acs_2$inc_100_149+dat_acs_2$inc_150_up)
dat_acs_2$pinc_050_099 <- NA
dat_acs_2$pinc_050_099 <- dat_acs_2$inc_050_099/(dat_acs_2$inc_000_024+dat_acs_2$inc_025_049+dat_acs_2$inc_050_099+dat_acs_2$inc_100_149+dat_acs_2$inc_150_up)
dat_acs_2$pinc_100_149 <- NA
dat_acs_2$pinc_100_149 <- dat_acs_2$inc_100_149/(dat_acs_2$inc_000_024+dat_acs_2$inc_025_049+dat_acs_2$inc_050_099+dat_acs_2$inc_100_149+dat_acs_2$inc_150_up)
dat_acs_2$pinc_150_up <- NA
dat_acs_2$pinc_150_up <- dat_acs_2$inc_150_up/(dat_acs_2$inc_000_024+dat_acs_2$inc_025_049+dat_acs_2$inc_050_099+dat_acs_2$inc_100_149+dat_acs_2$inc_150_up)
dat_acs_2$pten_own <- NA
dat_acs_2$pten_own <- dat_acs_2$ten_own/(dat_acs_2$ten_own+dat_acs_2$ten_rnt)
dat_acs_2$pten_rnt <- NA
dat_acs_2$pten_rnt <- dat_acs_2$ten_rnt/(dat_acs_2$ten_own+dat_acs_2$ten_rnt)
dat_acs_2$pveh_0 <- NA
dat_acs_2$pveh_0 <- dat_acs_2$veh_0/(dat_acs_2$veh_0+dat_acs_2$veh_1+dat_acs_2$veh_2+dat_acs_2$veh_3+dat_acs_2$veh_4)
dat_acs_2$pveh_1 <- NA
dat_acs_2$pveh_1 <- dat_acs_2$veh_1/(dat_acs_2$veh_0+dat_acs_2$veh_1+dat_acs_2$veh_2+dat_acs_2$veh_3+dat_acs_2$veh_4)
dat_acs_2$pveh_2 <- NA
dat_acs_2$pveh_2 <- dat_acs_2$veh_2/(dat_acs_2$veh_0+dat_acs_2$veh_1+dat_acs_2$veh_2+dat_acs_2$veh_3+dat_acs_2$veh_4)
dat_acs_2$pveh_3 <- NA
dat_acs_2$pveh_3 <- dat_acs_2$veh_3/(dat_acs_2$veh_0+dat_acs_2$veh_1+dat_acs_2$veh_2+dat_acs_2$veh_3+dat_acs_2$veh_4)
dat_acs_2$pveh_4 <- NA
dat_acs_2$pveh_4 <- dat_acs_2$veh_4/(dat_acs_2$veh_0+dat_acs_2$veh_1+dat_acs_2$veh_2+dat_acs_2$veh_3+dat_acs_2$veh_4)

# Convert NaNs to NAs
dat_acs_2 <- dat_acs_2 %>% mutate_all(function(x) ifelse(is.nan(x), NA, x))
rm(dat_acs)

# Export acs data set
write_csv(dat_acs_2, (file=paste(dat_dir, "context_acs_grids.csv", sep="_data/_tabular/_outputs/")))

####################
### MODEL DATA SET
####################

# Merge and export data sets
dat_15_ibc <- Reduce(function(x, y) merge(x, y, by="From"), list(dat_15,dat_acs_2))
colnames(dat_15_ibc)
dat_15_ibc2 <- dat_15_ibc[!is.na(dat_15_ibc$e_10_15),]
write_csv(dat_15_ibc, (file=paste(dat_dir, "dat_crashes_ibc_15_e.csv", sep="_data/_tabular/_outputs/")))
dat_30_ibc <- Reduce(function(x, y) merge(x, y, by="From"), list(dat_30,dat_acs_2))
colnames(dat_30_ibc)
dat_30_ibc2 <- dat_30_ibc[!is.na(dat_30_ibc$e_25_30),]
write_csv(dat_30_ibc, (file=paste(dat_dir, "dat_crashes_ibc_30_e.csv", sep="_data/_tabular/_outputs/")))

# Crash data set (n=320,219) != Access data set (n=322,189)
table(is.na(dat_15_ibc2$s_pre_age_1)); table(is.na(dat_30_ibc2$s_pre_age_1))

####################
### END OF SCRIPT
