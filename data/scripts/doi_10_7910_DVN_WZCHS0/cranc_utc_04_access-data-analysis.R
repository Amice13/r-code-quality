####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Data Analysis (Study 1: Access)
# Created: 07.13.2023
# Updated: 07.13.2023
####################################################

####################
### DATA SET-UP
####################

# Load libraries
# install.packages(c("tidyverse","foreign"))
packages <- c("tidyverse","foreign")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
dat_dir <- "F:/22-0338_BicyclingAccessibility/"
dat_dir2 <- "H:/_projects/22-0338_BicyclingAccessibility/"

# Import ACS data

# Import accessibility data sets
dat_ibc_e <- read_csv(paste(dat_dir, "dat_access_ibc_e.csv", sep="_data/_tabular/_outputs/"))
dat_eac_e <- read_csv(paste(dat_dir, "dat_access_eac_e.csv", sep="_data/_tabular/_outputs/"))
dat_saf_e <- read_csv(paste(dat_dir, "dat_access_saf_e.csv", sep="_data/_tabular/_outputs/"))
dat_ibc_m <- read_csv(paste(dat_dir, "dat_access_ibc_m.csv", sep="_data/_tabular/_outputs/"))
dat_eac_m <- read_csv(paste(dat_dir, "dat_access_eac_m.csv", sep="_data/_tabular/_outputs/"))
dat_saf_m <- read_csv(paste(dat_dir, "dat_access_saf_m.csv", sep="_data/_tabular/_outputs/"))
dat_ibc_s <- read_csv(paste(dat_dir, "dat_access_ibc_s.csv", sep="_data/_tabular/_outputs/"))
dat_eac_s <- read_csv(paste(dat_dir, "dat_access_eac_s.csv", sep="_data/_tabular/_outputs/"))
dat_saf_s <- read_csv(paste(dat_dir, "dat_access_saf_s.csv", sep="_data/_tabular/_outputs/"))

# Import neighborhood data sets
dat_acs <- read_csv(paste(dat_dir, "context_acs_grids.csv", sep="_data/_tabular/_outputs/"))

####################
### DESC STATISTICS
####################

# Merge ACS data to access data set
dat_ibc_e2 <- Reduce(function(x, y) merge(x, y, by="From", all.x=T), list(dat_ibc_e,dat_acs))
table(is.na(dat_ibc_e$n)); table(is.na(dat_ibc_e2$n))

# Summarize variables by MPO
colnames(dat_ibc_e2)
dat_ibc_e2 %>% group_by(mpo) %>% summarise(n=n(),psex_m=mean(psex_m,na.rm=T),psex_f=mean(psex_f,na.rm=T))
dat_ibc_e2 %>% group_by(mpo) %>% summarise(page_00_19=mean(page_00_19,na.rm=T),page_20_34=mean(page_20_34,na.rm=T),page_35_44=mean(page_35_44,na.rm=T),
                                           page_45_64=mean(page_45_64,na.rm=T),page_65_up=mean(page_65_up,na.rm=T))
dat_ibc_e2 %>% group_by(mpo) %>% summarise(pedu_1_m=mean(pedu_1,na.rm=T),pedu_2=mean(pedu_2,na.rm=T),pedu_3=mean(pedu_3,na.rm=T))
dat_ibc_e2 %>% group_by(mpo) %>% summarise(prac_aian=mean(prac_aian,na.rm=T),prac_asi=mean(prac_asi,na.rm=T),prac_baa=mean(prac_baa,na.rm=T),
                                           prac_hisp=mean(prac_hisp,na.rm=T),prac_whi=mean(prac_whi,na.rm=T))
dat_ibc_e2 %>% group_by(mpo) %>% summarise(pinc_000_024=mean(pinc_000_024,na.rm=T),pinc_025_049=mean(pinc_025_049,na.rm=T),pinc_050_099=mean(pinc_050_099,na.rm=T),
                                           pinc_100_149=mean(pinc_100_149,na.rm=T),pinc_150_up=mean(pinc_150_up,na.rm=T))

####################
### WEIGHTED MEAN
####################

# Multiply accessibility value by variable category shares
dat_ibc_e2$asex_m <- NA; dat_ibc_e2$asex_m <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$psex_m)
dat_ibc_e2$asex_f <- NA; dat_ibc_e2$asex_f <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$psex_f)
dat_ibc_e2$aage_00_19 <- NA; dat_ibc_e2$aage_00_19 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$page_00_19)
dat_ibc_e2$aage_20_34 <- NA; dat_ibc_e2$aage_20_34 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$page_20_34)
dat_ibc_e2$aage_35_44 <- NA; dat_ibc_e2$aage_35_44 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$page_35_44)
dat_ibc_e2$aage_45_64 <- NA; dat_ibc_e2$aage_45_64 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$page_45_64)
dat_ibc_e2$aage_65_up <- NA; dat_ibc_e2$aage_65_up <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$page_65_up)
dat_ibc_e2$aedu_1 <- NA; dat_ibc_e2$aedu_1 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pedu_1)
dat_ibc_e2$aedu_2 <- NA; dat_ibc_e2$aedu_2 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pedu_2)
dat_ibc_e2$aedu_3 <- NA; dat_ibc_e2$aedu_3 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pedu_3)
dat_ibc_e2$arac_aian <- NA; dat_ibc_e2$arac_aian <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$prac_aian)
dat_ibc_e2$arac_asi <- NA; dat_ibc_e2$arac_asi <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$prac_asi)
dat_ibc_e2$arac_baa <- NA; dat_ibc_e2$arac_baa <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$prac_baa)
dat_ibc_e2$arac_hisp <- NA; dat_ibc_e2$arac_hisp <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$prac_hisp)
dat_ibc_e2$arac_whi <- NA; dat_ibc_e2$arac_whi <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$prac_whi)
dat_ibc_e2$ainc_000_024 <- NA; dat_ibc_e2$ainc_000_024 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pinc_000_024)
dat_ibc_e2$ainc_025_049 <- NA; dat_ibc_e2$ainc_025_049 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pinc_025_049)
dat_ibc_e2$ainc_050_099 <- NA; dat_ibc_e2$ainc_050_099 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pinc_050_099)
dat_ibc_e2$ainc_100_149 <- NA; dat_ibc_e2$ainc_100_149 <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pinc_100_149)
dat_ibc_e2$ainc_150_up <- NA; dat_ibc_e2$ainc_150_up <- (dat_ibc_e2$s_10_15_lt*dat_ibc_e2$pinc_150_up)

# Calculate weighted averages of accessibility by variable category
dat_ibc_e2 %>% group_by(mpo) %>% summarise(e_10_15_lts=mean(s_10_15_lt,na.rm=T))
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(asex_m=sum(asex_m,na.rm=T),psex_m=sum(psex_m,na.rm=T)); temp$wa <- NA; temp$wa <- temp$asex_m/temp$psex_m
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(asex_f=sum(asex_f,na.rm=T),psex_f=sum(psex_f,na.rm=T)); temp$wa <- NA; temp$wa <- temp$asex_f/temp$psex_f
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aage_00_19=sum(aage_00_19,na.rm=T),page_00_19=sum(page_00_19,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aage_00_19/temp$page_00_19
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aage_20_34=sum(aage_20_34,na.rm=T),page_20_34=sum(page_20_34,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aage_20_34/temp$page_20_34
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aage_35_44=sum(aage_35_44,na.rm=T),page_35_44=sum(page_35_44,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aage_35_44/temp$page_35_44
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aage_45_64=sum(aage_45_64,na.rm=T),page_45_64=sum(page_45_64,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aage_45_64/temp$page_45_64
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aage_65_up=sum(aage_65_up,na.rm=T),page_65_up=sum(page_65_up,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aage_65_up/temp$page_65_up
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aedu_1=sum(aedu_1,na.rm=T),pedu_1=sum(pedu_1,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aedu_1/temp$pedu_1
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aedu_2=sum(aedu_2,na.rm=T),pedu_2=sum(pedu_2,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aedu_2/temp$pedu_2
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(aedu_3=sum(aedu_3,na.rm=T),pedu_3=sum(pedu_3,na.rm=T)); temp$wa <- NA; temp$wa <- temp$aedu_3/temp$pedu_3
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(arac_aian=sum(arac_aian,na.rm=T),prac_aian=sum(prac_aian,na.rm=T)); temp$wa <- NA; temp$wa <- temp$arac_aian/temp$prac_aian
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(arac_asi=sum(arac_asi,na.rm=T),prac_asi=sum(prac_asi,na.rm=T)); temp$wa <- NA; temp$wa <- temp$arac_asi/temp$prac_asi
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(arac_baa=sum(arac_baa,na.rm=T),prac_baa=sum(prac_baa,na.rm=T)); temp$wa <- NA; temp$wa <- temp$arac_baa/temp$prac_baa
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(arac_hisp=sum(arac_hisp,na.rm=T),prac_hisp=sum(prac_hisp,na.rm=T)); temp$wa <- NA; temp$wa <- temp$arac_hisp/temp$prac_hisp
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(arac_whi=sum(arac_whi,na.rm=T),prac_whi=sum(prac_whi,na.rm=T)); temp$wa <- NA; temp$wa <- temp$arac_whi/temp$prac_whi
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(ainc_000_024=sum(ainc_000_024,na.rm=T),pinc_000_024=sum(pinc_000_024,na.rm=T))
temp$wa <- NA; temp$wa <- temp$ainc_000_024/temp$pinc_000_024
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(ainc_025_049=sum(ainc_025_049,na.rm=T),pinc_025_049=sum(pinc_025_049,na.rm=T))
temp$wa <- NA; temp$wa <- temp$ainc_025_049/temp$pinc_025_049
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(ainc_050_099=sum(ainc_050_099,na.rm=T),pinc_050_099=sum(pinc_050_099,na.rm=T))
temp$wa <- NA; temp$wa <- temp$ainc_050_099/temp$pinc_050_099
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(ainc_100_149=sum(ainc_100_149,na.rm=T),pinc_100_149=sum(pinc_100_149,na.rm=T))
temp$wa <- NA; temp$wa <- temp$ainc_100_149/temp$pinc_100_149
temp <- dat_ibc_e2 %>% group_by(mpo) %>% summarise(ainc_150_up=sum(ainc_150_up,na.rm=T),pinc_150_up=sum(pinc_150_up,na.rm=T))
temp$wa <- NA; temp$wa <- temp$ainc_150_up/temp$pinc_150_up

####################
### END OF SCRIPT
