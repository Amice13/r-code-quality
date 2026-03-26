####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Data Set (Study 1: Access)
# Created: 07.12.2023
# Updated: 07.12.2023
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

# Import base data set
dat_base <- read.dbf(paste(dat_dir2, "grid_1_4mi_az_mpo.dbf", sep="_data/_spatial/boundaries/"))
colnames(dat_base) <- c("From","mpo")

### EMPLOYMENT
# Import and combine IBC data sets
dat_ibc_cympo <- read.dbf(paste(dat_dir, "access_ibc_employment_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_fmpo <- read.dbf(paste(dat_dir, "access_ibc_employment_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_lhmpo <- read.dbf(paste(dat_dir, "access_ibc_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_mag <- read.dbf(paste(dat_dir, "access_ibc_employment_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_pag <- read.dbf(paste(dat_dir, "access_ibc_employment_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_scmpo <- read.dbf(paste(dat_dir, "access_ibc_employment_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_svmpo <- read.dbf(paste(dat_dir, "access_ibc_employment_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_ympo <- read.dbf(paste(dat_dir, "access_ibc_employment_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_e <- Reduce(rbind,list(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo))
dat_ibc_e <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_ibc_e))
rm(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo)
# Import and combine EAC data sets
dat_eac_cympo <- read.dbf(paste(dat_dir, "access_eac_employment_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_fmpo <- read.dbf(paste(dat_dir, "access_eac_employment_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_lhmpo <- read.dbf(paste(dat_dir, "access_eac_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_mag <- read.dbf(paste(dat_dir, "access_eac_employment_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_pag <- read.dbf(paste(dat_dir, "access_eac_employment_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_scmpo <- read.dbf(paste(dat_dir, "access_eac_employment_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_svmpo <- read.dbf(paste(dat_dir, "access_eac_employment_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_ympo <- read.dbf(paste(dat_dir, "access_eac_employment_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_e <- Reduce(rbind,list(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo))
dat_eac_e <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_eac_e))
rm(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo)
# Import and combine SAF data sets
dat_saf_cympo <- read.dbf(paste(dat_dir, "access_saf_employment_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_fmpo <- read.dbf(paste(dat_dir, "access_saf_employment_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_lhmpo <- read.dbf(paste(dat_dir, "access_saf_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_mag <- read.dbf(paste(dat_dir, "access_saf_employment_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_pag <- read.dbf(paste(dat_dir, "access_saf_employment_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_scmpo <- read.dbf(paste(dat_dir, "access_saf_employment_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_svmpo <- read.dbf(paste(dat_dir, "access_saf_employment_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_ympo <- read.dbf(paste(dat_dir, "access_saf_employment_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_e <- Reduce(rbind,list(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo))
dat_saf_e <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_saf_e))
rm(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo)

### MARKETS
# Import and combine IBC data sets
dat_ibc_cympo <- read.dbf(paste(dat_dir, "access_ibc_markets_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_fmpo <- read.dbf(paste(dat_dir, "access_ibc_markets_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_lhmpo <- read.dbf(paste(dat_dir, "access_ibc_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_mag <- read.dbf(paste(dat_dir, "access_ibc_markets_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_pag <- read.dbf(paste(dat_dir, "access_ibc_markets_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_scmpo <- read.dbf(paste(dat_dir, "access_ibc_markets_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_svmpo <- read.dbf(paste(dat_dir, "access_ibc_markets_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_ympo <- read.dbf(paste(dat_dir, "access_ibc_markets_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_m <- Reduce(rbind,list(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo))
dat_ibc_m <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_ibc_m))
rm(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo)
# Import and combine EAC data sets
dat_eac_cympo <- read.dbf(paste(dat_dir, "access_eac_markets_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_fmpo <- read.dbf(paste(dat_dir, "access_eac_markets_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_lhmpo <- read.dbf(paste(dat_dir, "access_eac_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_mag <- read.dbf(paste(dat_dir, "access_eac_markets_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_pag <- read.dbf(paste(dat_dir, "access_eac_markets_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_scmpo <- read.dbf(paste(dat_dir, "access_eac_markets_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_svmpo <- read.dbf(paste(dat_dir, "access_eac_markets_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_ympo <- read.dbf(paste(dat_dir, "access_eac_markets_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_m <- Reduce(rbind,list(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo))
dat_eac_m <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_eac_m))
rm(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo)
# Import and combine SAF data sets
dat_saf_cympo <- read.dbf(paste(dat_dir, "access_saf_markets_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_fmpo <- read.dbf(paste(dat_dir, "access_saf_markets_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_lhmpo <- read.dbf(paste(dat_dir, "access_saf_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_mag <- read.dbf(paste(dat_dir, "access_saf_markets_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_pag <- read.dbf(paste(dat_dir, "access_saf_markets_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_scmpo <- read.dbf(paste(dat_dir, "access_saf_markets_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_svmpo <- read.dbf(paste(dat_dir, "access_saf_markets_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_ympo <- read.dbf(paste(dat_dir, "access_saf_markets_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_m <- Reduce(rbind,list(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo))
dat_saf_m <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_saf_m))
rm(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo)

### SCHOOLS
# Import and combine IBC data sets
dat_ibc_cympo <- read.dbf(paste(dat_dir, "access_ibc_schools_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_fmpo <- read.dbf(paste(dat_dir, "access_ibc_schools_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_lhmpo <- read.dbf(paste(dat_dir, "access_ibc_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_mag <- read.dbf(paste(dat_dir, "access_ibc_schools_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_pag <- read.dbf(paste(dat_dir, "access_ibc_schools_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_scmpo <- read.dbf(paste(dat_dir, "access_ibc_schools_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_svmpo <- read.dbf(paste(dat_dir, "access_ibc_schools_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_ympo <- read.dbf(paste(dat_dir, "access_ibc_schools_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_ibc_s <- Reduce(rbind,list(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo))
dat_ibc_s <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_ibc_s))
rm(dat_ibc_cympo,dat_ibc_fmpo,dat_ibc_lhmpo,dat_ibc_mag,dat_ibc_pag,dat_ibc_scmpo,dat_ibc_svmpo,dat_ibc_ympo)
# Import and combine EAC data sets
dat_eac_cympo <- read.dbf(paste(dat_dir, "access_eac_schools_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_fmpo <- read.dbf(paste(dat_dir, "access_eac_schools_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_lhmpo <- read.dbf(paste(dat_dir, "access_eac_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_mag <- read.dbf(paste(dat_dir, "access_eac_schools_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_pag <- read.dbf(paste(dat_dir, "access_eac_schools_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_scmpo <- read.dbf(paste(dat_dir, "access_eac_schools_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_svmpo <- read.dbf(paste(dat_dir, "access_eac_schools_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_ympo <- read.dbf(paste(dat_dir, "access_eac_schools_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_eac_s <- Reduce(rbind,list(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo))
dat_eac_s <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_eac_s))
rm(dat_eac_cympo,dat_eac_fmpo,dat_eac_lhmpo,dat_eac_mag,dat_eac_pag,dat_eac_scmpo,dat_eac_svmpo,dat_eac_ympo)
# Import and combine SAF data sets
dat_saf_cympo <- read.dbf(paste(dat_dir, "access_saf_schools_cympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_fmpo <- read.dbf(paste(dat_dir, "access_saf_schools_fmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_lhmpo <- read.dbf(paste(dat_dir, "access_saf_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_mag <- read.dbf(paste(dat_dir, "access_saf_schools_mag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_pag <- read.dbf(paste(dat_dir, "access_saf_schools_pag.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_scmpo <- read.dbf(paste(dat_dir, "access_saf_schools_scmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_svmpo <- read.dbf(paste(dat_dir, "access_saf_schools_svmpo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_ympo <- read.dbf(paste(dat_dir, "access_saf_schools_ympo.dbf", sep="_data/_tabular/_outputs/"))
dat_saf_s <- Reduce(rbind,list(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo))
dat_saf_s <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(dat_base,dat_saf_s))
rm(dat_saf_cympo,dat_saf_fmpo,dat_saf_lhmpo,dat_saf_mag,dat_saf_pag,dat_saf_scmpo,dat_saf_svmpo,dat_saf_ympo)

# Remove duplicated rows
dat_ibc_e <- dat_ibc_e[!duplicated(dat_ibc_e$From),]
dat_ibc_m <- dat_ibc_m[!duplicated(dat_ibc_m$From),]
dat_ibc_s <- dat_ibc_s[!duplicated(dat_ibc_s$From),]
dat_eac_e <- dat_eac_e[!duplicated(dat_eac_e$From),]
dat_eac_m <- dat_eac_m[!duplicated(dat_eac_m$From),]
dat_eac_s <- dat_eac_s[!duplicated(dat_eac_s$From),]
dat_saf_e <- dat_saf_e[!duplicated(dat_saf_e$From),]
dat_saf_m <- dat_saf_m[!duplicated(dat_saf_m$From),]
dat_saf_s <- dat_saf_s[!duplicated(dat_saf_s$From),]
rm(dat_base)

# Ensure data uniformity and export data sets
dat_ibc_e2 <- na.omit(dat_ibc_e); dat_eac_e2 <- na.omit(dat_eac_e); dat_saf_e2 <- na.omit(dat_saf_e)
dat_ibc_m2 <- na.omit(dat_ibc_m); dat_eac_m2 <- na.omit(dat_eac_m); dat_saf_m2 <- na.omit(dat_saf_m)
dat_ibc_s2 <- na.omit(dat_ibc_s); dat_eac_s2 <- na.omit(dat_eac_s); dat_saf_s2 <- na.omit(dat_saf_s)
dat_ibc_e3 <- dat_ibc_e2[,c(1:2)]; dat_eac_e3 <- dat_eac_e2[,c(1:2)]; dat_saf_e3 <- dat_saf_e2[,c(1:2)]
dat_ibc_m3 <- dat_ibc_m2[,c(1:2)]; dat_eac_m3 <- dat_eac_m2[,c(1:2)]; dat_saf_m3 <- dat_saf_m2[,c(1:2)]
dat_ibc_s3 <- dat_ibc_s2[,c(1:2)]; dat_eac_s3 <- dat_eac_s2[,c(1:2)]; dat_saf_s3 <- dat_saf_s2[,c(1:2)]
temp <- Reduce(rbind,list(dat_ibc_e3,dat_ibc_m3,dat_ibc_s3,dat_eac_e3,dat_eac_m3,dat_eac_s3,dat_saf_e3,dat_saf_m3,dat_saf_s3))
temp <- temp %>% group_by(From) %>% summarise(n = n()); temp <- temp[temp$n==9,]
rm(dat_ibc_e2,dat_ibc_m2,dat_ibc_s2,dat_eac_e2,dat_eac_m2,dat_eac_s2,dat_saf_e2,dat_saf_m2,dat_saf_s2)
rm(dat_ibc_e3,dat_ibc_m3,dat_ibc_s3,dat_eac_e3,dat_eac_m3,dat_eac_s3,dat_saf_e3,dat_saf_m3,dat_saf_s3)

# Create uniform data sets
dat_ibc_e2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_ibc_e))
dat_eac_e2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_eac_e))
dat_saf_e2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_saf_e))
dat_ibc_m2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_ibc_m))
dat_eac_m2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_eac_m))
dat_saf_m2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_saf_m))
dat_ibc_s2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_ibc_s))
dat_eac_s2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_eac_s))
dat_saf_s2 <- Reduce(function(x, y) merge(x, y, by="From", all=T), list(temp,dat_saf_s))
rm(temp)

# Export uniform data sets
write_csv(dat_ibc_e2, (file=paste(dat_dir, "dat_access_ibc_e.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_eac_e2, (file=paste(dat_dir, "dat_access_eac_e.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_saf_e2, (file=paste(dat_dir, "dat_access_saf_e.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_ibc_m2, (file=paste(dat_dir, "dat_access_ibc_m.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_eac_m2, (file=paste(dat_dir, "dat_access_eac_m.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_saf_m2, (file=paste(dat_dir, "dat_access_saf_m.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_ibc_s2, (file=paste(dat_dir, "dat_access_ibc_s.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_eac_s2, (file=paste(dat_dir, "dat_access_eac_s.csv", sep="_data/_tabular/_outputs/")))
write_csv(dat_saf_s2, (file=paste(dat_dir, "dat_access_saf_s.csv", sep="_data/_tabular/_outputs/")))

####################
### END OF SCRIPT
