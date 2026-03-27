####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Cumulative Opportunities with LTS Impedance
#       Employment (IBC/EAC/SAF)
# Created: 06.29.2023
# Updated: 06.29.2023
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
### CYMPO: Prescott

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_cympo_e <- read_csv(paste(dat_dir, "ibc_cympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e <- dat_cympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e)
dat_cympo_e[is.na(dat_cympo_e)] <- 0; dat_cympo_e <- dat_cympo_e %>% arrange(From)
dat_cympo_e_lts <- read_csv(paste(dat_dir, "ibc_cympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e_lts <- dat_cympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e_lts)
dat_cympo_e_lts[is.na(dat_cympo_e_lts)] <- 0; dat_cympo_e_lts <- dat_cympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_e <- as.data.frame(dat_cympo_e_lts)[1]
out_cympo_e$s_00_05 <- as.numeric(0)
out_cympo_e$s_00_05 <- dat_cympo_e$employment_00_05_mins
out_cympo_e$s_00_05_lts <- as.numeric(0)
out_cympo_e$s_00_05_lts <- dat_cympo_e$employment_00_05_mins * (dat_cympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_cympo_e$s_05_10 <- as.numeric(0)
out_cympo_e$s_05_10 <- dat_cympo_e$employment_05_10_mins
out_cympo_e$s_05_10_lts <- as.numeric(0)
out_cympo_e$s_05_10_lts <- dat_cympo_e$employment_05_10_mins * (dat_cympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_cympo_e$s_10_15 <- as.numeric(0)
out_cympo_e$s_10_15 <- dat_cympo_e$employment_10_15_mins
out_cympo_e$s_10_15_lts <- as.numeric(0)
out_cympo_e$s_10_15_lts <- dat_cympo_e$employment_10_15_mins * (dat_cympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_cympo_e$s_15_20 <- as.numeric(0)
out_cympo_e$s_15_20 <- dat_cympo_e$employment_15_20_mins
out_cympo_e$s_15_20_lts <- as.numeric(0)
out_cympo_e$s_15_20_lts <- dat_cympo_e$employment_15_20_mins * (dat_cympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_cympo_e$s_20_25 <- as.numeric(0)
out_cympo_e$s_20_25 <- dat_cympo_e$employment_20_25_mins
out_cympo_e$s_20_25_lts <- as.numeric(0)
out_cympo_e$s_20_25_lts <- dat_cympo_e$employment_20_25_mins * (dat_cympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_cympo_e$s_25_30 <- as.numeric(0)
out_cympo_e$s_25_30 <- dat_cympo_e$employment_25_30_mins
out_cympo_e$s_25_30_lts <- as.numeric(0)
out_cympo_e$s_25_30_lts <- dat_cympo_e$employment_25_30_mins * (dat_cympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_cympo_e$s_30_35 <- as.numeric(0)
out_cympo_e$s_30_35 <- dat_cympo_e$employment_30_35_mins
out_cympo_e$s_30_35_lts <- as.numeric(0)
out_cympo_e$s_30_35_lts <- dat_cympo_e$employment_30_35_mins * (dat_cympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_cympo_e$s_35_40 <- as.numeric(0)
out_cympo_e$s_35_40 <- dat_cympo_e$employment_35_40_mins
out_cympo_e$s_35_40_lts <- as.numeric(0)
out_cympo_e$s_35_40_lts <- dat_cympo_e$employment_35_40_mins * (dat_cympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_cympo_e$s_40_45 <- as.numeric(0)
out_cympo_e$s_40_45 <- dat_cympo_e$employment_40_45_mins
out_cympo_e$s_40_45_lts <- as.numeric(0)
out_cympo_e$s_40_45_lts <- dat_cympo_e$employment_40_45_mins * (dat_cympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_cympo_e$s_45_50 <- as.numeric(0)
out_cympo_e$s_45_50 <- dat_cympo_e$employment_45_50_mins
out_cympo_e$s_45_50_lts <- as.numeric(0)
out_cympo_e$s_45_50_lts <- dat_cympo_e$employment_45_50_mins * (dat_cympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_cympo_e$s_50_55 <- as.numeric(0)
out_cympo_e$s_50_55 <- dat_cympo_e$employment_50_55_mins
out_cympo_e$s_50_55_lts <- as.numeric(0)
out_cympo_e$s_50_55_lts <- dat_cympo_e$employment_50_55_mins * (dat_cympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_cympo_e$s_55_60 <- as.numeric(0)
out_cympo_e$s_55_60 <- dat_cympo_e$employment_55_60_mins
out_cympo_e$s_55_60_lts <- as.numeric(0)
out_cympo_e$s_55_60_lts <- dat_cympo_e$employment_55_60_mins * (dat_cympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_cympo_e)

# Export accessibility output 
write.dbf(out_cympo_e, (file=paste(dat_dir, "access_ibc_employment_cympo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_cympo_e <- read_csv(paste(dat_dir, "eac_cympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e <- dat_cympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e)
dat_cympo_e[is.na(dat_cympo_e)] <- 0; dat_cympo_e <- dat_cympo_e %>% arrange(From)
dat_cympo_e_lts <- read_csv(paste(dat_dir, "eac_cympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e_lts <- dat_cympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e_lts)
dat_cympo_e_lts[is.na(dat_cympo_e_lts)] <- 0; dat_cympo_e_lts <- dat_cympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_e <- as.data.frame(dat_cympo_e_lts)[1]
out_cympo_e$s_00_05 <- as.numeric(0)
out_cympo_e$s_00_05 <- dat_cympo_e$employment_00_05_mins
out_cympo_e$s_00_05_lts <- as.numeric(0)
out_cympo_e$s_00_05_lts <- dat_cympo_e$employment_00_05_mins * (dat_cympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_cympo_e$s_05_10 <- as.numeric(0)
out_cympo_e$s_05_10 <- dat_cympo_e$employment_05_10_mins
out_cympo_e$s_05_10_lts <- as.numeric(0)
out_cympo_e$s_05_10_lts <- dat_cympo_e$employment_05_10_mins * (dat_cympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_cympo_e$s_10_15 <- as.numeric(0)
out_cympo_e$s_10_15 <- dat_cympo_e$employment_10_15_mins
out_cympo_e$s_10_15_lts <- as.numeric(0)
out_cympo_e$s_10_15_lts <- dat_cympo_e$employment_10_15_mins * (dat_cympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_cympo_e$s_15_20 <- as.numeric(0)
out_cympo_e$s_15_20 <- dat_cympo_e$employment_15_20_mins
out_cympo_e$s_15_20_lts <- as.numeric(0)
out_cympo_e$s_15_20_lts <- dat_cympo_e$employment_15_20_mins * (dat_cympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_cympo_e$s_20_25 <- as.numeric(0)
out_cympo_e$s_20_25 <- dat_cympo_e$employment_20_25_mins
out_cympo_e$s_20_25_lts <- as.numeric(0)
out_cympo_e$s_20_25_lts <- dat_cympo_e$employment_20_25_mins * (dat_cympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_cympo_e$s_25_30 <- as.numeric(0)
out_cympo_e$s_25_30 <- dat_cympo_e$employment_25_30_mins
out_cympo_e$s_25_30_lts <- as.numeric(0)
out_cympo_e$s_25_30_lts <- dat_cympo_e$employment_25_30_mins * (dat_cympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_cympo_e$s_30_35 <- as.numeric(0)
out_cympo_e$s_30_35 <- dat_cympo_e$employment_30_35_mins
out_cympo_e$s_30_35_lts <- as.numeric(0)
out_cympo_e$s_30_35_lts <- dat_cympo_e$employment_30_35_mins * (dat_cympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_cympo_e$s_35_40 <- as.numeric(0)
out_cympo_e$s_35_40 <- dat_cympo_e$employment_35_40_mins
out_cympo_e$s_35_40_lts <- as.numeric(0)
out_cympo_e$s_35_40_lts <- dat_cympo_e$employment_35_40_mins * (dat_cympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_cympo_e$s_40_45 <- as.numeric(0)
out_cympo_e$s_40_45 <- dat_cympo_e$employment_40_45_mins
out_cympo_e$s_40_45_lts <- as.numeric(0)
out_cympo_e$s_40_45_lts <- dat_cympo_e$employment_40_45_mins * (dat_cympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_cympo_e$s_45_50 <- as.numeric(0)
out_cympo_e$s_45_50 <- dat_cympo_e$employment_45_50_mins
out_cympo_e$s_45_50_lts <- as.numeric(0)
out_cympo_e$s_45_50_lts <- dat_cympo_e$employment_45_50_mins * (dat_cympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_cympo_e$s_50_55 <- as.numeric(0)
out_cympo_e$s_50_55 <- dat_cympo_e$employment_50_55_mins
out_cympo_e$s_50_55_lts <- as.numeric(0)
out_cympo_e$s_50_55_lts <- dat_cympo_e$employment_50_55_mins * (dat_cympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_cympo_e$s_55_60 <- as.numeric(0)
out_cympo_e$s_55_60 <- dat_cympo_e$employment_55_60_mins
out_cympo_e$s_55_60_lts <- as.numeric(0)
out_cympo_e$s_55_60_lts <- dat_cympo_e$employment_55_60_mins * (dat_cympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_cympo_e)

# Export accessibility output 
write.dbf(out_cympo_e, (file=paste(dat_dir, "access_eac_employment_cympo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_cympo_e <- read_csv(paste(dat_dir, "saf_cympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e <- dat_cympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e)
dat_cympo_e[is.na(dat_cympo_e)] <- 0; dat_cympo_e <- dat_cympo_e %>% arrange(From)
dat_cympo_e_lts <- read_csv(paste(dat_dir, "saf_cympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_cympo_e_lts <- dat_cympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_e_lts)
dat_cympo_e_lts[is.na(dat_cympo_e_lts)] <- 0; dat_cympo_e_lts <- dat_cympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_e <- as.data.frame(dat_cympo_e_lts)[1]
out_cympo_e$s_00_05 <- as.numeric(0)
out_cympo_e$s_00_05 <- dat_cympo_e$employment_00_05_mins
out_cympo_e$s_00_05_lts <- as.numeric(0)
out_cympo_e$s_00_05_lts <- dat_cympo_e$employment_00_05_mins * (dat_cympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_cympo_e$s_05_10 <- as.numeric(0)
out_cympo_e$s_05_10 <- dat_cympo_e$employment_05_10_mins
out_cympo_e$s_05_10_lts <- as.numeric(0)
out_cympo_e$s_05_10_lts <- dat_cympo_e$employment_05_10_mins * (dat_cympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_cympo_e$s_10_15 <- as.numeric(0)
out_cympo_e$s_10_15 <- dat_cympo_e$employment_10_15_mins
out_cympo_e$s_10_15_lts <- as.numeric(0)
out_cympo_e$s_10_15_lts <- dat_cympo_e$employment_10_15_mins * (dat_cympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_cympo_e$s_15_20 <- as.numeric(0)
out_cympo_e$s_15_20 <- dat_cympo_e$employment_15_20_mins
out_cympo_e$s_15_20_lts <- as.numeric(0)
out_cympo_e$s_15_20_lts <- dat_cympo_e$employment_15_20_mins * (dat_cympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_cympo_e$s_20_25 <- as.numeric(0)
out_cympo_e$s_20_25 <- dat_cympo_e$employment_20_25_mins
out_cympo_e$s_20_25_lts <- as.numeric(0)
out_cympo_e$s_20_25_lts <- dat_cympo_e$employment_20_25_mins * (dat_cympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_cympo_e$s_25_30 <- as.numeric(0)
out_cympo_e$s_25_30 <- dat_cympo_e$employment_25_30_mins
out_cympo_e$s_25_30_lts <- as.numeric(0)
out_cympo_e$s_25_30_lts <- dat_cympo_e$employment_25_30_mins * (dat_cympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_cympo_e$s_30_35 <- as.numeric(0)
out_cympo_e$s_30_35 <- dat_cympo_e$employment_30_35_mins
out_cympo_e$s_30_35_lts <- as.numeric(0)
out_cympo_e$s_30_35_lts <- dat_cympo_e$employment_30_35_mins * (dat_cympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_cympo_e$s_35_40 <- as.numeric(0)
out_cympo_e$s_35_40 <- dat_cympo_e$employment_35_40_mins
out_cympo_e$s_35_40_lts <- as.numeric(0)
out_cympo_e$s_35_40_lts <- dat_cympo_e$employment_35_40_mins * (dat_cympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_cympo_e$s_40_45 <- as.numeric(0)
out_cympo_e$s_40_45 <- dat_cympo_e$employment_40_45_mins
out_cympo_e$s_40_45_lts <- as.numeric(0)
out_cympo_e$s_40_45_lts <- dat_cympo_e$employment_40_45_mins * (dat_cympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_cympo_e$s_45_50 <- as.numeric(0)
out_cympo_e$s_45_50 <- dat_cympo_e$employment_45_50_mins
out_cympo_e$s_45_50_lts <- as.numeric(0)
out_cympo_e$s_45_50_lts <- dat_cympo_e$employment_45_50_mins * (dat_cympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_cympo_e$s_50_55 <- as.numeric(0)
out_cympo_e$s_50_55 <- dat_cympo_e$employment_50_55_mins
out_cympo_e$s_50_55_lts <- as.numeric(0)
out_cympo_e$s_50_55_lts <- dat_cympo_e$employment_50_55_mins * (dat_cympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_cympo_e$s_55_60 <- as.numeric(0)
out_cympo_e$s_55_60 <- dat_cympo_e$employment_55_60_mins
out_cympo_e$s_55_60_lts <- as.numeric(0)
out_cympo_e$s_55_60_lts <- dat_cympo_e$employment_55_60_mins * (dat_cympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_cympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_cympo_e)

# Export accessibility output 
write.dbf(out_cympo_e, (file=paste(dat_dir, "access_saf_employment_cympo.dbf", sep="_data/_tabular/_outputs/")))

####################
### FMPO/MetroPlan: Flagstaff

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_fmpo_e <- read_csv(paste(dat_dir, "ibc_fmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e <- dat_fmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e)
dat_fmpo_e[is.na(dat_fmpo_e)] <- 0; dat_fmpo_e <- dat_fmpo_e %>% arrange(From)
dat_fmpo_e_lts <- read_csv(paste(dat_dir, "ibc_fmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e_lts <- dat_fmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e_lts)
dat_fmpo_e_lts[is.na(dat_fmpo_e_lts)] <- 0; dat_fmpo_e_lts <- dat_fmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_e <- as.data.frame(dat_fmpo_e_lts)[1]
out_fmpo_e$s_00_05 <- as.numeric(0)
out_fmpo_e$s_00_05 <- dat_fmpo_e$employment_00_05_mins
out_fmpo_e$s_00_05_lts <- as.numeric(0)
out_fmpo_e$s_00_05_lts <- dat_fmpo_e$employment_00_05_mins * (dat_fmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_fmpo_e$s_05_10 <- as.numeric(0)
out_fmpo_e$s_05_10 <- dat_fmpo_e$employment_05_10_mins
out_fmpo_e$s_05_10_lts <- as.numeric(0)
out_fmpo_e$s_05_10_lts <- dat_fmpo_e$employment_05_10_mins * (dat_fmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_fmpo_e$s_10_15 <- as.numeric(0)
out_fmpo_e$s_10_15 <- dat_fmpo_e$employment_10_15_mins
out_fmpo_e$s_10_15_lts <- as.numeric(0)
out_fmpo_e$s_10_15_lts <- dat_fmpo_e$employment_10_15_mins * (dat_fmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_fmpo_e$s_15_20 <- as.numeric(0)
out_fmpo_e$s_15_20 <- dat_fmpo_e$employment_15_20_mins
out_fmpo_e$s_15_20_lts <- as.numeric(0)
out_fmpo_e$s_15_20_lts <- dat_fmpo_e$employment_15_20_mins * (dat_fmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_fmpo_e$s_20_25 <- as.numeric(0)
out_fmpo_e$s_20_25 <- dat_fmpo_e$employment_20_25_mins
out_fmpo_e$s_20_25_lts <- as.numeric(0)
out_fmpo_e$s_20_25_lts <- dat_fmpo_e$employment_20_25_mins * (dat_fmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_fmpo_e$s_25_30 <- as.numeric(0)
out_fmpo_e$s_25_30 <- dat_fmpo_e$employment_25_30_mins
out_fmpo_e$s_25_30_lts <- as.numeric(0)
out_fmpo_e$s_25_30_lts <- dat_fmpo_e$employment_25_30_mins * (dat_fmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_fmpo_e$s_30_35 <- as.numeric(0)
out_fmpo_e$s_30_35 <- dat_fmpo_e$employment_30_35_mins
out_fmpo_e$s_30_35_lts <- as.numeric(0)
out_fmpo_e$s_30_35_lts <- dat_fmpo_e$employment_30_35_mins * (dat_fmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_fmpo_e$s_35_40 <- as.numeric(0)
out_fmpo_e$s_35_40 <- dat_fmpo_e$employment_35_40_mins
out_fmpo_e$s_35_40_lts <- as.numeric(0)
out_fmpo_e$s_35_40_lts <- dat_fmpo_e$employment_35_40_mins * (dat_fmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_fmpo_e$s_40_45 <- as.numeric(0)
out_fmpo_e$s_40_45 <- dat_fmpo_e$employment_40_45_mins
out_fmpo_e$s_40_45_lts <- as.numeric(0)
out_fmpo_e$s_40_45_lts <- dat_fmpo_e$employment_40_45_mins * (dat_fmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_fmpo_e$s_45_50 <- as.numeric(0)
out_fmpo_e$s_45_50 <- dat_fmpo_e$employment_45_50_mins
out_fmpo_e$s_45_50_lts <- as.numeric(0)
out_fmpo_e$s_45_50_lts <- dat_fmpo_e$employment_45_50_mins * (dat_fmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_fmpo_e$s_50_55 <- as.numeric(0)
out_fmpo_e$s_50_55 <- dat_fmpo_e$employment_50_55_mins
out_fmpo_e$s_50_55_lts <- as.numeric(0)
out_fmpo_e$s_50_55_lts <- dat_fmpo_e$employment_50_55_mins * (dat_fmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_fmpo_e$s_55_60 <- as.numeric(0)
out_fmpo_e$s_55_60 <- dat_fmpo_e$employment_55_60_mins
out_fmpo_e$s_55_60_lts <- as.numeric(0)
out_fmpo_e$s_55_60_lts <- dat_fmpo_e$employment_55_60_mins * (dat_fmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_fmpo_e)

# Export accessibility output 
write.dbf(out_fmpo_e, (file=paste(dat_dir, "access_ibc_employment_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_fmpo_e <- read_csv(paste(dat_dir, "eac_fmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e <- dat_fmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e)
dat_fmpo_e[is.na(dat_fmpo_e)] <- 0; dat_fmpo_e <- dat_fmpo_e %>% arrange(From)
dat_fmpo_e_lts <- read_csv(paste(dat_dir, "eac_fmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e_lts <- dat_fmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e_lts)
dat_fmpo_e_lts[is.na(dat_fmpo_e_lts)] <- 0; dat_fmpo_e_lts <- dat_fmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_e <- as.data.frame(dat_fmpo_e_lts)[1]
out_fmpo_e$s_00_05 <- as.numeric(0)
out_fmpo_e$s_00_05 <- dat_fmpo_e$employment_00_05_mins
out_fmpo_e$s_00_05_lts <- as.numeric(0)
out_fmpo_e$s_00_05_lts <- dat_fmpo_e$employment_00_05_mins * (dat_fmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_fmpo_e$s_05_10 <- as.numeric(0)
out_fmpo_e$s_05_10 <- dat_fmpo_e$employment_05_10_mins
out_fmpo_e$s_05_10_lts <- as.numeric(0)
out_fmpo_e$s_05_10_lts <- dat_fmpo_e$employment_05_10_mins * (dat_fmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_fmpo_e$s_10_15 <- as.numeric(0)
out_fmpo_e$s_10_15 <- dat_fmpo_e$employment_10_15_mins
out_fmpo_e$s_10_15_lts <- as.numeric(0)
out_fmpo_e$s_10_15_lts <- dat_fmpo_e$employment_10_15_mins * (dat_fmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_fmpo_e$s_15_20 <- as.numeric(0)
out_fmpo_e$s_15_20 <- dat_fmpo_e$employment_15_20_mins
out_fmpo_e$s_15_20_lts <- as.numeric(0)
out_fmpo_e$s_15_20_lts <- dat_fmpo_e$employment_15_20_mins * (dat_fmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_fmpo_e$s_20_25 <- as.numeric(0)
out_fmpo_e$s_20_25 <- dat_fmpo_e$employment_20_25_mins
out_fmpo_e$s_20_25_lts <- as.numeric(0)
out_fmpo_e$s_20_25_lts <- dat_fmpo_e$employment_20_25_mins * (dat_fmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_fmpo_e$s_25_30 <- as.numeric(0)
out_fmpo_e$s_25_30 <- dat_fmpo_e$employment_25_30_mins
out_fmpo_e$s_25_30_lts <- as.numeric(0)
out_fmpo_e$s_25_30_lts <- dat_fmpo_e$employment_25_30_mins * (dat_fmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_fmpo_e$s_30_35 <- as.numeric(0)
out_fmpo_e$s_30_35 <- dat_fmpo_e$employment_30_35_mins
out_fmpo_e$s_30_35_lts <- as.numeric(0)
out_fmpo_e$s_30_35_lts <- dat_fmpo_e$employment_30_35_mins * (dat_fmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_fmpo_e$s_35_40 <- as.numeric(0)
out_fmpo_e$s_35_40 <- dat_fmpo_e$employment_35_40_mins
out_fmpo_e$s_35_40_lts <- as.numeric(0)
out_fmpo_e$s_35_40_lts <- dat_fmpo_e$employment_35_40_mins * (dat_fmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_fmpo_e$s_40_45 <- as.numeric(0)
out_fmpo_e$s_40_45 <- dat_fmpo_e$employment_40_45_mins
out_fmpo_e$s_40_45_lts <- as.numeric(0)
out_fmpo_e$s_40_45_lts <- dat_fmpo_e$employment_40_45_mins * (dat_fmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_fmpo_e$s_45_50 <- as.numeric(0)
out_fmpo_e$s_45_50 <- dat_fmpo_e$employment_45_50_mins
out_fmpo_e$s_45_50_lts <- as.numeric(0)
out_fmpo_e$s_45_50_lts <- dat_fmpo_e$employment_45_50_mins * (dat_fmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_fmpo_e$s_50_55 <- as.numeric(0)
out_fmpo_e$s_50_55 <- dat_fmpo_e$employment_50_55_mins
out_fmpo_e$s_50_55_lts <- as.numeric(0)
out_fmpo_e$s_50_55_lts <- dat_fmpo_e$employment_50_55_mins * (dat_fmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_fmpo_e$s_55_60 <- as.numeric(0)
out_fmpo_e$s_55_60 <- dat_fmpo_e$employment_55_60_mins
out_fmpo_e$s_55_60_lts <- as.numeric(0)
out_fmpo_e$s_55_60_lts <- dat_fmpo_e$employment_55_60_mins * (dat_fmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_fmpo_e)

# Export accessibility output 
write.dbf(out_fmpo_e, (file=paste(dat_dir, "access_eac_employment_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_fmpo_e <- read_csv(paste(dat_dir, "saf_fmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e <- dat_fmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e)
dat_fmpo_e[is.na(dat_fmpo_e)] <- 0; dat_fmpo_e <- dat_fmpo_e %>% arrange(From)
dat_fmpo_e_lts <- read_csv(paste(dat_dir, "saf_fmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_fmpo_e_lts <- dat_fmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_e_lts)
dat_fmpo_e_lts[is.na(dat_fmpo_e_lts)] <- 0; dat_fmpo_e_lts <- dat_fmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_e <- as.data.frame(dat_fmpo_e_lts)[1]
out_fmpo_e$s_00_05 <- as.numeric(0)
out_fmpo_e$s_00_05 <- dat_fmpo_e$employment_00_05_mins
out_fmpo_e$s_00_05_lts <- as.numeric(0)
out_fmpo_e$s_00_05_lts <- dat_fmpo_e$employment_00_05_mins * (dat_fmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_fmpo_e$s_05_10 <- as.numeric(0)
out_fmpo_e$s_05_10 <- dat_fmpo_e$employment_05_10_mins
out_fmpo_e$s_05_10_lts <- as.numeric(0)
out_fmpo_e$s_05_10_lts <- dat_fmpo_e$employment_05_10_mins * (dat_fmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_fmpo_e$s_10_15 <- as.numeric(0)
out_fmpo_e$s_10_15 <- dat_fmpo_e$employment_10_15_mins
out_fmpo_e$s_10_15_lts <- as.numeric(0)
out_fmpo_e$s_10_15_lts <- dat_fmpo_e$employment_10_15_mins * (dat_fmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_fmpo_e$s_15_20 <- as.numeric(0)
out_fmpo_e$s_15_20 <- dat_fmpo_e$employment_15_20_mins
out_fmpo_e$s_15_20_lts <- as.numeric(0)
out_fmpo_e$s_15_20_lts <- dat_fmpo_e$employment_15_20_mins * (dat_fmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_fmpo_e$s_20_25 <- as.numeric(0)
out_fmpo_e$s_20_25 <- dat_fmpo_e$employment_20_25_mins
out_fmpo_e$s_20_25_lts <- as.numeric(0)
out_fmpo_e$s_20_25_lts <- dat_fmpo_e$employment_20_25_mins * (dat_fmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_fmpo_e$s_25_30 <- as.numeric(0)
out_fmpo_e$s_25_30 <- dat_fmpo_e$employment_25_30_mins
out_fmpo_e$s_25_30_lts <- as.numeric(0)
out_fmpo_e$s_25_30_lts <- dat_fmpo_e$employment_25_30_mins * (dat_fmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_fmpo_e$s_30_35 <- as.numeric(0)
out_fmpo_e$s_30_35 <- dat_fmpo_e$employment_30_35_mins
out_fmpo_e$s_30_35_lts <- as.numeric(0)
out_fmpo_e$s_30_35_lts <- dat_fmpo_e$employment_30_35_mins * (dat_fmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_fmpo_e$s_35_40 <- as.numeric(0)
out_fmpo_e$s_35_40 <- dat_fmpo_e$employment_35_40_mins
out_fmpo_e$s_35_40_lts <- as.numeric(0)
out_fmpo_e$s_35_40_lts <- dat_fmpo_e$employment_35_40_mins * (dat_fmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_fmpo_e$s_40_45 <- as.numeric(0)
out_fmpo_e$s_40_45 <- dat_fmpo_e$employment_40_45_mins
out_fmpo_e$s_40_45_lts <- as.numeric(0)
out_fmpo_e$s_40_45_lts <- dat_fmpo_e$employment_40_45_mins * (dat_fmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_fmpo_e$s_45_50 <- as.numeric(0)
out_fmpo_e$s_45_50 <- dat_fmpo_e$employment_45_50_mins
out_fmpo_e$s_45_50_lts <- as.numeric(0)
out_fmpo_e$s_45_50_lts <- dat_fmpo_e$employment_45_50_mins * (dat_fmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_fmpo_e$s_50_55 <- as.numeric(0)
out_fmpo_e$s_50_55 <- dat_fmpo_e$employment_50_55_mins
out_fmpo_e$s_50_55_lts <- as.numeric(0)
out_fmpo_e$s_50_55_lts <- dat_fmpo_e$employment_50_55_mins * (dat_fmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_fmpo_e$s_55_60 <- as.numeric(0)
out_fmpo_e$s_55_60 <- dat_fmpo_e$employment_55_60_mins
out_fmpo_e$s_55_60_lts <- as.numeric(0)
out_fmpo_e$s_55_60_lts <- dat_fmpo_e$employment_55_60_mins * (dat_fmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_fmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_fmpo_e)

# Export accessibility output 
write.dbf(out_fmpo_e, (file=paste(dat_dir, "access_saf_employment_fmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### LHMPO: Lake Havasu

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_lhmpo_e <- read_csv(paste(dat_dir, "ibc_lhmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e <- dat_lhmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e)
dat_lhmpo_e[is.na(dat_lhmpo_e)] <- 0; dat_lhmpo_e <- dat_lhmpo_e %>% arrange(From)
dat_lhmpo_e_lts <- read_csv(paste(dat_dir, "ibc_lhmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e_lts)
dat_lhmpo_e_lts[is.na(dat_lhmpo_e_lts)] <- 0; dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_e <- as.data.frame(dat_lhmpo_e_lts)[1]
out_lhmpo_e$s_00_05 <- as.numeric(0)
out_lhmpo_e$s_00_05 <- dat_lhmpo_e$employment_00_05_mins
out_lhmpo_e$s_00_05_lts <- as.numeric(0)
out_lhmpo_e$s_00_05_lts <- dat_lhmpo_e$employment_00_05_mins * (dat_lhmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_lhmpo_e$s_05_10 <- as.numeric(0)
out_lhmpo_e$s_05_10 <- dat_lhmpo_e$employment_05_10_mins
out_lhmpo_e$s_05_10_lts <- as.numeric(0)
out_lhmpo_e$s_05_10_lts <- dat_lhmpo_e$employment_05_10_mins * (dat_lhmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_lhmpo_e$s_10_15 <- as.numeric(0)
out_lhmpo_e$s_10_15 <- dat_lhmpo_e$employment_10_15_mins
out_lhmpo_e$s_10_15_lts <- as.numeric(0)
out_lhmpo_e$s_10_15_lts <- dat_lhmpo_e$employment_10_15_mins * (dat_lhmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_lhmpo_e$s_15_20 <- as.numeric(0)
out_lhmpo_e$s_15_20 <- dat_lhmpo_e$employment_15_20_mins
out_lhmpo_e$s_15_20_lts <- as.numeric(0)
out_lhmpo_e$s_15_20_lts <- dat_lhmpo_e$employment_15_20_mins * (dat_lhmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_lhmpo_e$s_20_25 <- as.numeric(0)
out_lhmpo_e$s_20_25 <- dat_lhmpo_e$employment_20_25_mins
out_lhmpo_e$s_20_25_lts <- as.numeric(0)
out_lhmpo_e$s_20_25_lts <- dat_lhmpo_e$employment_20_25_mins * (dat_lhmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_lhmpo_e$s_25_30 <- as.numeric(0)
out_lhmpo_e$s_25_30 <- dat_lhmpo_e$employment_25_30_mins
out_lhmpo_e$s_25_30_lts <- as.numeric(0)
out_lhmpo_e$s_25_30_lts <- dat_lhmpo_e$employment_25_30_mins * (dat_lhmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_lhmpo_e$s_30_35 <- as.numeric(0)
out_lhmpo_e$s_30_35 <- dat_lhmpo_e$employment_30_35_mins
out_lhmpo_e$s_30_35_lts <- as.numeric(0)
out_lhmpo_e$s_30_35_lts <- dat_lhmpo_e$employment_30_35_mins * (dat_lhmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_lhmpo_e$s_35_40 <- as.numeric(0)
out_lhmpo_e$s_35_40 <- dat_lhmpo_e$employment_35_40_mins
out_lhmpo_e$s_35_40_lts <- as.numeric(0)
out_lhmpo_e$s_35_40_lts <- dat_lhmpo_e$employment_35_40_mins * (dat_lhmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_lhmpo_e$s_40_45 <- as.numeric(0)
out_lhmpo_e$s_40_45 <- dat_lhmpo_e$employment_40_45_mins
out_lhmpo_e$s_40_45_lts <- as.numeric(0)
out_lhmpo_e$s_40_45_lts <- dat_lhmpo_e$employment_40_45_mins * (dat_lhmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_lhmpo_e$s_45_50 <- as.numeric(0)
out_lhmpo_e$s_45_50 <- dat_lhmpo_e$employment_45_50_mins
out_lhmpo_e$s_45_50_lts <- as.numeric(0)
out_lhmpo_e$s_45_50_lts <- dat_lhmpo_e$employment_45_50_mins * (dat_lhmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_lhmpo_e$s_50_55 <- as.numeric(0)
out_lhmpo_e$s_50_55 <- dat_lhmpo_e$employment_50_55_mins
out_lhmpo_e$s_50_55_lts <- as.numeric(0)
out_lhmpo_e$s_50_55_lts <- dat_lhmpo_e$employment_50_55_mins * (dat_lhmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_lhmpo_e$s_55_60 <- as.numeric(0)
out_lhmpo_e$s_55_60 <- dat_lhmpo_e$employment_55_60_mins
out_lhmpo_e$s_55_60_lts <- as.numeric(0)
out_lhmpo_e$s_55_60_lts <- dat_lhmpo_e$employment_55_60_mins * (dat_lhmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_lhmpo_e)

# Export accessibility output 
write.dbf(out_lhmpo_e, (file=paste(dat_dir, "access_ibc_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_lhmpo_e <- read_csv(paste(dat_dir, "eac_lhmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e <- dat_lhmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e)
dat_lhmpo_e[is.na(dat_lhmpo_e)] <- 0; dat_lhmpo_e <- dat_lhmpo_e %>% arrange(From)
dat_lhmpo_e_lts <- read_csv(paste(dat_dir, "eac_lhmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e_lts)
dat_lhmpo_e_lts[is.na(dat_lhmpo_e_lts)] <- 0; dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_e <- as.data.frame(dat_lhmpo_e_lts)[1]
out_lhmpo_e$s_00_05 <- as.numeric(0)
out_lhmpo_e$s_00_05 <- dat_lhmpo_e$employment_00_05_mins
out_lhmpo_e$s_00_05_lts <- as.numeric(0)
out_lhmpo_e$s_00_05_lts <- dat_lhmpo_e$employment_00_05_mins * (dat_lhmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_lhmpo_e$s_05_10 <- as.numeric(0)
out_lhmpo_e$s_05_10 <- dat_lhmpo_e$employment_05_10_mins
out_lhmpo_e$s_05_10_lts <- as.numeric(0)
out_lhmpo_e$s_05_10_lts <- dat_lhmpo_e$employment_05_10_mins * (dat_lhmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_lhmpo_e$s_10_15 <- as.numeric(0)
out_lhmpo_e$s_10_15 <- dat_lhmpo_e$employment_10_15_mins
out_lhmpo_e$s_10_15_lts <- as.numeric(0)
out_lhmpo_e$s_10_15_lts <- dat_lhmpo_e$employment_10_15_mins * (dat_lhmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_lhmpo_e$s_15_20 <- as.numeric(0)
out_lhmpo_e$s_15_20 <- dat_lhmpo_e$employment_15_20_mins
out_lhmpo_e$s_15_20_lts <- as.numeric(0)
out_lhmpo_e$s_15_20_lts <- dat_lhmpo_e$employment_15_20_mins * (dat_lhmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_lhmpo_e$s_20_25 <- as.numeric(0)
out_lhmpo_e$s_20_25 <- dat_lhmpo_e$employment_20_25_mins
out_lhmpo_e$s_20_25_lts <- as.numeric(0)
out_lhmpo_e$s_20_25_lts <- dat_lhmpo_e$employment_20_25_mins * (dat_lhmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_lhmpo_e$s_25_30 <- as.numeric(0)
out_lhmpo_e$s_25_30 <- dat_lhmpo_e$employment_25_30_mins
out_lhmpo_e$s_25_30_lts <- as.numeric(0)
out_lhmpo_e$s_25_30_lts <- dat_lhmpo_e$employment_25_30_mins * (dat_lhmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_lhmpo_e$s_30_35 <- as.numeric(0)
out_lhmpo_e$s_30_35 <- dat_lhmpo_e$employment_30_35_mins
out_lhmpo_e$s_30_35_lts <- as.numeric(0)
out_lhmpo_e$s_30_35_lts <- dat_lhmpo_e$employment_30_35_mins * (dat_lhmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_lhmpo_e$s_35_40 <- as.numeric(0)
out_lhmpo_e$s_35_40 <- dat_lhmpo_e$employment_35_40_mins
out_lhmpo_e$s_35_40_lts <- as.numeric(0)
out_lhmpo_e$s_35_40_lts <- dat_lhmpo_e$employment_35_40_mins * (dat_lhmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_lhmpo_e$s_40_45 <- as.numeric(0)
out_lhmpo_e$s_40_45 <- dat_lhmpo_e$employment_40_45_mins
out_lhmpo_e$s_40_45_lts <- as.numeric(0)
out_lhmpo_e$s_40_45_lts <- dat_lhmpo_e$employment_40_45_mins * (dat_lhmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_lhmpo_e$s_45_50 <- as.numeric(0)
out_lhmpo_e$s_45_50 <- dat_lhmpo_e$employment_45_50_mins
out_lhmpo_e$s_45_50_lts <- as.numeric(0)
out_lhmpo_e$s_45_50_lts <- dat_lhmpo_e$employment_45_50_mins * (dat_lhmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_lhmpo_e$s_50_55 <- as.numeric(0)
out_lhmpo_e$s_50_55 <- dat_lhmpo_e$employment_50_55_mins
out_lhmpo_e$s_50_55_lts <- as.numeric(0)
out_lhmpo_e$s_50_55_lts <- dat_lhmpo_e$employment_50_55_mins * (dat_lhmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_lhmpo_e$s_55_60 <- as.numeric(0)
out_lhmpo_e$s_55_60 <- dat_lhmpo_e$employment_55_60_mins
out_lhmpo_e$s_55_60_lts <- as.numeric(0)
out_lhmpo_e$s_55_60_lts <- dat_lhmpo_e$employment_55_60_mins * (dat_lhmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_lhmpo_e)

# Export accessibility output 
write.dbf(out_lhmpo_e, (file=paste(dat_dir, "access_eac_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_lhmpo_e <- read_csv(paste(dat_dir, "saf_lhmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e <- dat_lhmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e)
dat_lhmpo_e[is.na(dat_lhmpo_e)] <- 0; dat_lhmpo_e <- dat_lhmpo_e %>% arrange(From)
dat_lhmpo_e_lts <- read_csv(paste(dat_dir, "saf_lhmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_e_lts)
dat_lhmpo_e_lts[is.na(dat_lhmpo_e_lts)] <- 0; dat_lhmpo_e_lts <- dat_lhmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_e <- as.data.frame(dat_lhmpo_e_lts)[1]
out_lhmpo_e$s_00_05 <- as.numeric(0)
out_lhmpo_e$s_00_05 <- dat_lhmpo_e$employment_00_05_mins
out_lhmpo_e$s_00_05_lts <- as.numeric(0)
out_lhmpo_e$s_00_05_lts <- dat_lhmpo_e$employment_00_05_mins * (dat_lhmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_lhmpo_e$s_05_10 <- as.numeric(0)
out_lhmpo_e$s_05_10 <- dat_lhmpo_e$employment_05_10_mins
out_lhmpo_e$s_05_10_lts <- as.numeric(0)
out_lhmpo_e$s_05_10_lts <- dat_lhmpo_e$employment_05_10_mins * (dat_lhmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_lhmpo_e$s_10_15 <- as.numeric(0)
out_lhmpo_e$s_10_15 <- dat_lhmpo_e$employment_10_15_mins
out_lhmpo_e$s_10_15_lts <- as.numeric(0)
out_lhmpo_e$s_10_15_lts <- dat_lhmpo_e$employment_10_15_mins * (dat_lhmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_lhmpo_e$s_15_20 <- as.numeric(0)
out_lhmpo_e$s_15_20 <- dat_lhmpo_e$employment_15_20_mins
out_lhmpo_e$s_15_20_lts <- as.numeric(0)
out_lhmpo_e$s_15_20_lts <- dat_lhmpo_e$employment_15_20_mins * (dat_lhmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_lhmpo_e$s_20_25 <- as.numeric(0)
out_lhmpo_e$s_20_25 <- dat_lhmpo_e$employment_20_25_mins
out_lhmpo_e$s_20_25_lts <- as.numeric(0)
out_lhmpo_e$s_20_25_lts <- dat_lhmpo_e$employment_20_25_mins * (dat_lhmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_lhmpo_e$s_25_30 <- as.numeric(0)
out_lhmpo_e$s_25_30 <- dat_lhmpo_e$employment_25_30_mins
out_lhmpo_e$s_25_30_lts <- as.numeric(0)
out_lhmpo_e$s_25_30_lts <- dat_lhmpo_e$employment_25_30_mins * (dat_lhmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_lhmpo_e$s_30_35 <- as.numeric(0)
out_lhmpo_e$s_30_35 <- dat_lhmpo_e$employment_30_35_mins
out_lhmpo_e$s_30_35_lts <- as.numeric(0)
out_lhmpo_e$s_30_35_lts <- dat_lhmpo_e$employment_30_35_mins * (dat_lhmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_lhmpo_e$s_35_40 <- as.numeric(0)
out_lhmpo_e$s_35_40 <- dat_lhmpo_e$employment_35_40_mins
out_lhmpo_e$s_35_40_lts <- as.numeric(0)
out_lhmpo_e$s_35_40_lts <- dat_lhmpo_e$employment_35_40_mins * (dat_lhmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_lhmpo_e$s_40_45 <- as.numeric(0)
out_lhmpo_e$s_40_45 <- dat_lhmpo_e$employment_40_45_mins
out_lhmpo_e$s_40_45_lts <- as.numeric(0)
out_lhmpo_e$s_40_45_lts <- dat_lhmpo_e$employment_40_45_mins * (dat_lhmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_lhmpo_e$s_45_50 <- as.numeric(0)
out_lhmpo_e$s_45_50 <- dat_lhmpo_e$employment_45_50_mins
out_lhmpo_e$s_45_50_lts <- as.numeric(0)
out_lhmpo_e$s_45_50_lts <- dat_lhmpo_e$employment_45_50_mins * (dat_lhmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_lhmpo_e$s_50_55 <- as.numeric(0)
out_lhmpo_e$s_50_55 <- dat_lhmpo_e$employment_50_55_mins
out_lhmpo_e$s_50_55_lts <- as.numeric(0)
out_lhmpo_e$s_50_55_lts <- dat_lhmpo_e$employment_50_55_mins * (dat_lhmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_lhmpo_e$s_55_60 <- as.numeric(0)
out_lhmpo_e$s_55_60 <- dat_lhmpo_e$employment_55_60_mins
out_lhmpo_e$s_55_60_lts <- as.numeric(0)
out_lhmpo_e$s_55_60_lts <- dat_lhmpo_e$employment_55_60_mins * (dat_lhmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_lhmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_lhmpo_e)

# Export accessibility output 
write.dbf(out_lhmpo_e, (file=paste(dat_dir, "access_saf_employment_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### MAG: Phoenix

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_mag_e <- read_csv(paste(dat_dir, "ibc_mag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e <- dat_mag_e %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e)
dat_mag_e[is.na(dat_mag_e)] <- 0; dat_mag_e <- dat_mag_e %>% arrange(From)
dat_mag_e_lts <- read_csv(paste(dat_dir, "ibc_mag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e_lts <- dat_mag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e_lts)
dat_mag_e_lts[is.na(dat_mag_e_lts)] <- 0; dat_mag_e_lts <- dat_mag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_e <- as.data.frame(dat_mag_e_lts)[1]
out_mag_e$s_00_05 <- as.numeric(0)
out_mag_e$s_00_05 <- dat_mag_e$employment_00_05_mins
out_mag_e$s_00_05_lts <- as.numeric(0)
out_mag_e$s_00_05_lts <- dat_mag_e$employment_00_05_mins * (dat_mag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_mag_e_lts$`employment_00_05_mins_lts2%`/100)
out_mag_e$s_05_10 <- as.numeric(0)
out_mag_e$s_05_10 <- dat_mag_e$employment_05_10_mins
out_mag_e$s_05_10_lts <- as.numeric(0)
out_mag_e$s_05_10_lts <- dat_mag_e$employment_05_10_mins * (dat_mag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_mag_e_lts$`employment_05_10_mins_lts2%`/100)
out_mag_e$s_10_15 <- as.numeric(0)
out_mag_e$s_10_15 <- dat_mag_e$employment_10_15_mins
out_mag_e$s_10_15_lts <- as.numeric(0)
out_mag_e$s_10_15_lts <- dat_mag_e$employment_10_15_mins * (dat_mag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_mag_e_lts$`employment_10_15_mins_lts2%`/100)
out_mag_e$s_15_20 <- as.numeric(0)
out_mag_e$s_15_20 <- dat_mag_e$employment_15_20_mins
out_mag_e$s_15_20_lts <- as.numeric(0)
out_mag_e$s_15_20_lts <- dat_mag_e$employment_15_20_mins * (dat_mag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_mag_e_lts$`employment_15_20_mins_lts2%`/100)
out_mag_e$s_20_25 <- as.numeric(0)
out_mag_e$s_20_25 <- dat_mag_e$employment_20_25_mins
out_mag_e$s_20_25_lts <- as.numeric(0)
out_mag_e$s_20_25_lts <- dat_mag_e$employment_20_25_mins * (dat_mag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_mag_e_lts$`employment_20_25_mins_lts2%`/100)
out_mag_e$s_25_30 <- as.numeric(0)
out_mag_e$s_25_30 <- dat_mag_e$employment_25_30_mins
out_mag_e$s_25_30_lts <- as.numeric(0)
out_mag_e$s_25_30_lts <- dat_mag_e$employment_25_30_mins * (dat_mag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_mag_e_lts$`employment_25_30_mins_lts2%`/100)
out_mag_e$s_30_35 <- as.numeric(0)
out_mag_e$s_30_35 <- dat_mag_e$employment_30_35_mins
out_mag_e$s_30_35_lts <- as.numeric(0)
out_mag_e$s_30_35_lts <- dat_mag_e$employment_30_35_mins * (dat_mag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_mag_e_lts$`employment_30_35_mins_lts2%`/100)
out_mag_e$s_35_40 <- as.numeric(0)
out_mag_e$s_35_40 <- dat_mag_e$employment_35_40_mins
out_mag_e$s_35_40_lts <- as.numeric(0)
out_mag_e$s_35_40_lts <- dat_mag_e$employment_35_40_mins * (dat_mag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_mag_e_lts$`employment_35_40_mins_lts2%`/100)
out_mag_e$s_40_45 <- as.numeric(0)
out_mag_e$s_40_45 <- dat_mag_e$employment_40_45_mins
out_mag_e$s_40_45_lts <- as.numeric(0)
out_mag_e$s_40_45_lts <- dat_mag_e$employment_40_45_mins * (dat_mag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_mag_e_lts$`employment_40_45_mins_lts2%`/100)
out_mag_e$s_45_50 <- as.numeric(0)
out_mag_e$s_45_50 <- dat_mag_e$employment_45_50_mins
out_mag_e$s_45_50_lts <- as.numeric(0)
out_mag_e$s_45_50_lts <- dat_mag_e$employment_45_50_mins * (dat_mag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_mag_e_lts$`employment_45_50_mins_lts2%`/100)
out_mag_e$s_50_55 <- as.numeric(0)
out_mag_e$s_50_55 <- dat_mag_e$employment_50_55_mins
out_mag_e$s_50_55_lts <- as.numeric(0)
out_mag_e$s_50_55_lts <- dat_mag_e$employment_50_55_mins * (dat_mag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_mag_e_lts$`employment_50_55_mins_lts2%`/100)
out_mag_e$s_55_60 <- as.numeric(0)
out_mag_e$s_55_60 <- dat_mag_e$employment_55_60_mins
out_mag_e$s_55_60_lts <- as.numeric(0)
out_mag_e$s_55_60_lts <- dat_mag_e$employment_55_60_mins * (dat_mag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_mag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_mag_e)

# Export accessibility output 
write.dbf(out_mag_e, (file=paste(dat_dir, "access_ibc_employment_mag.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_mag_e <- read_csv(paste(dat_dir, "eac_mag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e <- dat_mag_e %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e)
dat_mag_e[is.na(dat_mag_e)] <- 0; dat_mag_e <- dat_mag_e %>% arrange(From)
dat_mag_e_lts <- read_csv(paste(dat_dir, "eac_mag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e_lts <- dat_mag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e_lts)
dat_mag_e_lts[is.na(dat_mag_e_lts)] <- 0; dat_mag_e_lts <- dat_mag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_e <- as.data.frame(dat_mag_e_lts)[1]
out_mag_e$s_00_05 <- as.numeric(0)
out_mag_e$s_00_05 <- dat_mag_e$employment_00_05_mins
out_mag_e$s_00_05_lts <- as.numeric(0)
out_mag_e$s_00_05_lts <- dat_mag_e$employment_00_05_mins * (dat_mag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_mag_e_lts$`employment_00_05_mins_lts2%`/100)
out_mag_e$s_05_10 <- as.numeric(0)
out_mag_e$s_05_10 <- dat_mag_e$employment_05_10_mins
out_mag_e$s_05_10_lts <- as.numeric(0)
out_mag_e$s_05_10_lts <- dat_mag_e$employment_05_10_mins * (dat_mag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_mag_e_lts$`employment_05_10_mins_lts2%`/100)
out_mag_e$s_10_15 <- as.numeric(0)
out_mag_e$s_10_15 <- dat_mag_e$employment_10_15_mins
out_mag_e$s_10_15_lts <- as.numeric(0)
out_mag_e$s_10_15_lts <- dat_mag_e$employment_10_15_mins * (dat_mag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_mag_e_lts$`employment_10_15_mins_lts2%`/100)
out_mag_e$s_15_20 <- as.numeric(0)
out_mag_e$s_15_20 <- dat_mag_e$employment_15_20_mins
out_mag_e$s_15_20_lts <- as.numeric(0)
out_mag_e$s_15_20_lts <- dat_mag_e$employment_15_20_mins * (dat_mag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_mag_e_lts$`employment_15_20_mins_lts2%`/100)
out_mag_e$s_20_25 <- as.numeric(0)
out_mag_e$s_20_25 <- dat_mag_e$employment_20_25_mins
out_mag_e$s_20_25_lts <- as.numeric(0)
out_mag_e$s_20_25_lts <- dat_mag_e$employment_20_25_mins * (dat_mag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_mag_e_lts$`employment_20_25_mins_lts2%`/100)
out_mag_e$s_25_30 <- as.numeric(0)
out_mag_e$s_25_30 <- dat_mag_e$employment_25_30_mins
out_mag_e$s_25_30_lts <- as.numeric(0)
out_mag_e$s_25_30_lts <- dat_mag_e$employment_25_30_mins * (dat_mag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_mag_e_lts$`employment_25_30_mins_lts2%`/100)
out_mag_e$s_30_35 <- as.numeric(0)
out_mag_e$s_30_35 <- dat_mag_e$employment_30_35_mins
out_mag_e$s_30_35_lts <- as.numeric(0)
out_mag_e$s_30_35_lts <- dat_mag_e$employment_30_35_mins * (dat_mag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_mag_e_lts$`employment_30_35_mins_lts2%`/100)
out_mag_e$s_35_40 <- as.numeric(0)
out_mag_e$s_35_40 <- dat_mag_e$employment_35_40_mins
out_mag_e$s_35_40_lts <- as.numeric(0)
out_mag_e$s_35_40_lts <- dat_mag_e$employment_35_40_mins * (dat_mag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_mag_e_lts$`employment_35_40_mins_lts2%`/100)
out_mag_e$s_40_45 <- as.numeric(0)
out_mag_e$s_40_45 <- dat_mag_e$employment_40_45_mins
out_mag_e$s_40_45_lts <- as.numeric(0)
out_mag_e$s_40_45_lts <- dat_mag_e$employment_40_45_mins * (dat_mag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_mag_e_lts$`employment_40_45_mins_lts2%`/100)
out_mag_e$s_45_50 <- as.numeric(0)
out_mag_e$s_45_50 <- dat_mag_e$employment_45_50_mins
out_mag_e$s_45_50_lts <- as.numeric(0)
out_mag_e$s_45_50_lts <- dat_mag_e$employment_45_50_mins * (dat_mag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_mag_e_lts$`employment_45_50_mins_lts2%`/100)
out_mag_e$s_50_55 <- as.numeric(0)
out_mag_e$s_50_55 <- dat_mag_e$employment_50_55_mins
out_mag_e$s_50_55_lts <- as.numeric(0)
out_mag_e$s_50_55_lts <- dat_mag_e$employment_50_55_mins * (dat_mag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_mag_e_lts$`employment_50_55_mins_lts2%`/100)
out_mag_e$s_55_60 <- as.numeric(0)
out_mag_e$s_55_60 <- dat_mag_e$employment_55_60_mins
out_mag_e$s_55_60_lts <- as.numeric(0)
out_mag_e$s_55_60_lts <- dat_mag_e$employment_55_60_mins * (dat_mag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_mag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_mag_e)

# Export accessibility output 
write.dbf(out_mag_e, (file=paste(dat_dir, "access_eac_employment_mag.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_mag_e <- read_csv(paste(dat_dir, "saf_mag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e <- dat_mag_e %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e)
dat_mag_e[is.na(dat_mag_e)] <- 0; dat_mag_e <- dat_mag_e %>% arrange(From)
dat_mag_e_lts <- read_csv(paste(dat_dir, "saf_mag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_mag_e_lts <- dat_mag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_e_lts)
dat_mag_e_lts[is.na(dat_mag_e_lts)] <- 0; dat_mag_e_lts <- dat_mag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_e <- as.data.frame(dat_mag_e_lts)[1]
out_mag_e$s_00_05 <- as.numeric(0)
out_mag_e$s_00_05 <- dat_mag_e$employment_00_05_mins
out_mag_e$s_00_05_lts <- as.numeric(0)
out_mag_e$s_00_05_lts <- dat_mag_e$employment_00_05_mins * (dat_mag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_mag_e_lts$`employment_00_05_mins_lts2%`/100)
out_mag_e$s_05_10 <- as.numeric(0)
out_mag_e$s_05_10 <- dat_mag_e$employment_05_10_mins
out_mag_e$s_05_10_lts <- as.numeric(0)
out_mag_e$s_05_10_lts <- dat_mag_e$employment_05_10_mins * (dat_mag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_mag_e_lts$`employment_05_10_mins_lts2%`/100)
out_mag_e$s_10_15 <- as.numeric(0)
out_mag_e$s_10_15 <- dat_mag_e$employment_10_15_mins
out_mag_e$s_10_15_lts <- as.numeric(0)
out_mag_e$s_10_15_lts <- dat_mag_e$employment_10_15_mins * (dat_mag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_mag_e_lts$`employment_10_15_mins_lts2%`/100)
out_mag_e$s_15_20 <- as.numeric(0)
out_mag_e$s_15_20 <- dat_mag_e$employment_15_20_mins
out_mag_e$s_15_20_lts <- as.numeric(0)
out_mag_e$s_15_20_lts <- dat_mag_e$employment_15_20_mins * (dat_mag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_mag_e_lts$`employment_15_20_mins_lts2%`/100)
out_mag_e$s_20_25 <- as.numeric(0)
out_mag_e$s_20_25 <- dat_mag_e$employment_20_25_mins
out_mag_e$s_20_25_lts <- as.numeric(0)
out_mag_e$s_20_25_lts <- dat_mag_e$employment_20_25_mins * (dat_mag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_mag_e_lts$`employment_20_25_mins_lts2%`/100)
out_mag_e$s_25_30 <- as.numeric(0)
out_mag_e$s_25_30 <- dat_mag_e$employment_25_30_mins
out_mag_e$s_25_30_lts <- as.numeric(0)
out_mag_e$s_25_30_lts <- dat_mag_e$employment_25_30_mins * (dat_mag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_mag_e_lts$`employment_25_30_mins_lts2%`/100)
out_mag_e$s_30_35 <- as.numeric(0)
out_mag_e$s_30_35 <- dat_mag_e$employment_30_35_mins
out_mag_e$s_30_35_lts <- as.numeric(0)
out_mag_e$s_30_35_lts <- dat_mag_e$employment_30_35_mins * (dat_mag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_mag_e_lts$`employment_30_35_mins_lts2%`/100)
out_mag_e$s_35_40 <- as.numeric(0)
out_mag_e$s_35_40 <- dat_mag_e$employment_35_40_mins
out_mag_e$s_35_40_lts <- as.numeric(0)
out_mag_e$s_35_40_lts <- dat_mag_e$employment_35_40_mins * (dat_mag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_mag_e_lts$`employment_35_40_mins_lts2%`/100)
out_mag_e$s_40_45 <- as.numeric(0)
out_mag_e$s_40_45 <- dat_mag_e$employment_40_45_mins
out_mag_e$s_40_45_lts <- as.numeric(0)
out_mag_e$s_40_45_lts <- dat_mag_e$employment_40_45_mins * (dat_mag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_mag_e_lts$`employment_40_45_mins_lts2%`/100)
out_mag_e$s_45_50 <- as.numeric(0)
out_mag_e$s_45_50 <- dat_mag_e$employment_45_50_mins
out_mag_e$s_45_50_lts <- as.numeric(0)
out_mag_e$s_45_50_lts <- dat_mag_e$employment_45_50_mins * (dat_mag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_mag_e_lts$`employment_45_50_mins_lts2%`/100)
out_mag_e$s_50_55 <- as.numeric(0)
out_mag_e$s_50_55 <- dat_mag_e$employment_50_55_mins
out_mag_e$s_50_55_lts <- as.numeric(0)
out_mag_e$s_50_55_lts <- dat_mag_e$employment_50_55_mins * (dat_mag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_mag_e_lts$`employment_50_55_mins_lts2%`/100)
out_mag_e$s_55_60 <- as.numeric(0)
out_mag_e$s_55_60 <- dat_mag_e$employment_55_60_mins
out_mag_e$s_55_60_lts <- as.numeric(0)
out_mag_e$s_55_60_lts <- dat_mag_e$employment_55_60_mins * (dat_mag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_mag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_mag_e)

# Export accessibility output 
write.dbf(out_mag_e, (file=paste(dat_dir, "access_saf_employment_mag.dbf", sep="_data/_tabular/_outputs/")))

####################
### PAG: Tucson

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_pag_e <- read_csv(paste(dat_dir, "ibc_pag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e <- dat_pag_e %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e)
dat_pag_e[is.na(dat_pag_e)] <- 0; dat_pag_e <- dat_pag_e %>% arrange(From)
dat_pag_e_lts <- read_csv(paste(dat_dir, "ibc_pag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e_lts <- dat_pag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e_lts)
dat_pag_e_lts[is.na(dat_pag_e_lts)] <- 0; dat_pag_e_lts <- dat_pag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_e <- as.data.frame(dat_pag_e_lts)[1]
out_pag_e$s_00_05 <- as.numeric(0)
out_pag_e$s_00_05 <- dat_pag_e$employment_00_05_mins
out_pag_e$s_00_05_lts <- as.numeric(0)
out_pag_e$s_00_05_lts <- dat_pag_e$employment_00_05_mins * (dat_pag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_pag_e_lts$`employment_00_05_mins_lts2%`/100)
out_pag_e$s_05_10 <- as.numeric(0)
out_pag_e$s_05_10 <- dat_pag_e$employment_05_10_mins
out_pag_e$s_05_10_lts <- as.numeric(0)
out_pag_e$s_05_10_lts <- dat_pag_e$employment_05_10_mins * (dat_pag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_pag_e_lts$`employment_05_10_mins_lts2%`/100)
out_pag_e$s_10_15 <- as.numeric(0)
out_pag_e$s_10_15 <- dat_pag_e$employment_10_15_mins
out_pag_e$s_10_15_lts <- as.numeric(0)
out_pag_e$s_10_15_lts <- dat_pag_e$employment_10_15_mins * (dat_pag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_pag_e_lts$`employment_10_15_mins_lts2%`/100)
out_pag_e$s_15_20 <- as.numeric(0)
out_pag_e$s_15_20 <- dat_pag_e$employment_15_20_mins
out_pag_e$s_15_20_lts <- as.numeric(0)
out_pag_e$s_15_20_lts <- dat_pag_e$employment_15_20_mins * (dat_pag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_pag_e_lts$`employment_15_20_mins_lts2%`/100)
out_pag_e$s_20_25 <- as.numeric(0)
out_pag_e$s_20_25 <- dat_pag_e$employment_20_25_mins
out_pag_e$s_20_25_lts <- as.numeric(0)
out_pag_e$s_20_25_lts <- dat_pag_e$employment_20_25_mins * (dat_pag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_pag_e_lts$`employment_20_25_mins_lts2%`/100)
out_pag_e$s_25_30 <- as.numeric(0)
out_pag_e$s_25_30 <- dat_pag_e$employment_25_30_mins
out_pag_e$s_25_30_lts <- as.numeric(0)
out_pag_e$s_25_30_lts <- dat_pag_e$employment_25_30_mins * (dat_pag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_pag_e_lts$`employment_25_30_mins_lts2%`/100)
out_pag_e$s_30_35 <- as.numeric(0)
out_pag_e$s_30_35 <- dat_pag_e$employment_30_35_mins
out_pag_e$s_30_35_lts <- as.numeric(0)
out_pag_e$s_30_35_lts <- dat_pag_e$employment_30_35_mins * (dat_pag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_pag_e_lts$`employment_30_35_mins_lts2%`/100)
out_pag_e$s_35_40 <- as.numeric(0)
out_pag_e$s_35_40 <- dat_pag_e$employment_35_40_mins
out_pag_e$s_35_40_lts <- as.numeric(0)
out_pag_e$s_35_40_lts <- dat_pag_e$employment_35_40_mins * (dat_pag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_pag_e_lts$`employment_35_40_mins_lts2%`/100)
out_pag_e$s_40_45 <- as.numeric(0)
out_pag_e$s_40_45 <- dat_pag_e$employment_40_45_mins
out_pag_e$s_40_45_lts <- as.numeric(0)
out_pag_e$s_40_45_lts <- dat_pag_e$employment_40_45_mins * (dat_pag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_pag_e_lts$`employment_40_45_mins_lts2%`/100)
out_pag_e$s_45_50 <- as.numeric(0)
out_pag_e$s_45_50 <- dat_pag_e$employment_45_50_mins
out_pag_e$s_45_50_lts <- as.numeric(0)
out_pag_e$s_45_50_lts <- dat_pag_e$employment_45_50_mins * (dat_pag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_pag_e_lts$`employment_45_50_mins_lts2%`/100)
out_pag_e$s_50_55 <- as.numeric(0)
out_pag_e$s_50_55 <- dat_pag_e$employment_50_55_mins
out_pag_e$s_50_55_lts <- as.numeric(0)
out_pag_e$s_50_55_lts <- dat_pag_e$employment_50_55_mins * (dat_pag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_pag_e_lts$`employment_50_55_mins_lts2%`/100)
out_pag_e$s_55_60 <- as.numeric(0)
out_pag_e$s_55_60 <- dat_pag_e$employment_55_60_mins
out_pag_e$s_55_60_lts <- as.numeric(0)
out_pag_e$s_55_60_lts <- dat_pag_e$employment_55_60_mins * (dat_pag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_pag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_pag_e)

# Export accessibility output 
write.dbf(out_pag_e, (file=paste(dat_dir, "access_ibc_employment_pag.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_pag_e <- read_csv(paste(dat_dir, "eac_pag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e <- dat_pag_e %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e)
dat_pag_e[is.na(dat_pag_e)] <- 0; dat_pag_e <- dat_pag_e %>% arrange(From)
dat_pag_e_lts <- read_csv(paste(dat_dir, "eac_pag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e_lts <- dat_pag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e_lts)
dat_pag_e_lts[is.na(dat_pag_e_lts)] <- 0; dat_pag_e_lts <- dat_pag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_e <- as.data.frame(dat_pag_e_lts)[1]
out_pag_e$s_00_05 <- as.numeric(0)
out_pag_e$s_00_05 <- dat_pag_e$employment_00_05_mins
out_pag_e$s_00_05_lts <- as.numeric(0)
out_pag_e$s_00_05_lts <- dat_pag_e$employment_00_05_mins * (dat_pag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_pag_e_lts$`employment_00_05_mins_lts2%`/100)
out_pag_e$s_05_10 <- as.numeric(0)
out_pag_e$s_05_10 <- dat_pag_e$employment_05_10_mins
out_pag_e$s_05_10_lts <- as.numeric(0)
out_pag_e$s_05_10_lts <- dat_pag_e$employment_05_10_mins * (dat_pag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_pag_e_lts$`employment_05_10_mins_lts2%`/100)
out_pag_e$s_10_15 <- as.numeric(0)
out_pag_e$s_10_15 <- dat_pag_e$employment_10_15_mins
out_pag_e$s_10_15_lts <- as.numeric(0)
out_pag_e$s_10_15_lts <- dat_pag_e$employment_10_15_mins * (dat_pag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_pag_e_lts$`employment_10_15_mins_lts2%`/100)
out_pag_e$s_15_20 <- as.numeric(0)
out_pag_e$s_15_20 <- dat_pag_e$employment_15_20_mins
out_pag_e$s_15_20_lts <- as.numeric(0)
out_pag_e$s_15_20_lts <- dat_pag_e$employment_15_20_mins * (dat_pag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_pag_e_lts$`employment_15_20_mins_lts2%`/100)
out_pag_e$s_20_25 <- as.numeric(0)
out_pag_e$s_20_25 <- dat_pag_e$employment_20_25_mins
out_pag_e$s_20_25_lts <- as.numeric(0)
out_pag_e$s_20_25_lts <- dat_pag_e$employment_20_25_mins * (dat_pag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_pag_e_lts$`employment_20_25_mins_lts2%`/100)
out_pag_e$s_25_30 <- as.numeric(0)
out_pag_e$s_25_30 <- dat_pag_e$employment_25_30_mins
out_pag_e$s_25_30_lts <- as.numeric(0)
out_pag_e$s_25_30_lts <- dat_pag_e$employment_25_30_mins * (dat_pag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_pag_e_lts$`employment_25_30_mins_lts2%`/100)
out_pag_e$s_30_35 <- as.numeric(0)
out_pag_e$s_30_35 <- dat_pag_e$employment_30_35_mins
out_pag_e$s_30_35_lts <- as.numeric(0)
out_pag_e$s_30_35_lts <- dat_pag_e$employment_30_35_mins * (dat_pag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_pag_e_lts$`employment_30_35_mins_lts2%`/100)
out_pag_e$s_35_40 <- as.numeric(0)
out_pag_e$s_35_40 <- dat_pag_e$employment_35_40_mins
out_pag_e$s_35_40_lts <- as.numeric(0)
out_pag_e$s_35_40_lts <- dat_pag_e$employment_35_40_mins * (dat_pag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_pag_e_lts$`employment_35_40_mins_lts2%`/100)
out_pag_e$s_40_45 <- as.numeric(0)
out_pag_e$s_40_45 <- dat_pag_e$employment_40_45_mins
out_pag_e$s_40_45_lts <- as.numeric(0)
out_pag_e$s_40_45_lts <- dat_pag_e$employment_40_45_mins * (dat_pag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_pag_e_lts$`employment_40_45_mins_lts2%`/100)
out_pag_e$s_45_50 <- as.numeric(0)
out_pag_e$s_45_50 <- dat_pag_e$employment_45_50_mins
out_pag_e$s_45_50_lts <- as.numeric(0)
out_pag_e$s_45_50_lts <- dat_pag_e$employment_45_50_mins * (dat_pag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_pag_e_lts$`employment_45_50_mins_lts2%`/100)
out_pag_e$s_50_55 <- as.numeric(0)
out_pag_e$s_50_55 <- dat_pag_e$employment_50_55_mins
out_pag_e$s_50_55_lts <- as.numeric(0)
out_pag_e$s_50_55_lts <- dat_pag_e$employment_50_55_mins * (dat_pag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_pag_e_lts$`employment_50_55_mins_lts2%`/100)
out_pag_e$s_55_60 <- as.numeric(0)
out_pag_e$s_55_60 <- dat_pag_e$employment_55_60_mins
out_pag_e$s_55_60_lts <- as.numeric(0)
out_pag_e$s_55_60_lts <- dat_pag_e$employment_55_60_mins * (dat_pag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_pag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_pag_e)

# Export accessibility output 
write.dbf(out_pag_e, (file=paste(dat_dir, "access_eac_employment_pag.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_pag_e <- read_csv(paste(dat_dir, "saf_pag_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e <- dat_pag_e %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e)
dat_pag_e[is.na(dat_pag_e)] <- 0; dat_pag_e <- dat_pag_e %>% arrange(From)
dat_pag_e_lts <- read_csv(paste(dat_dir, "saf_pag_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_pag_e_lts <- dat_pag_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_e_lts)
dat_pag_e_lts[is.na(dat_pag_e_lts)] <- 0; dat_pag_e_lts <- dat_pag_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_e <- as.data.frame(dat_pag_e_lts)[1]
out_pag_e$s_00_05 <- as.numeric(0)
out_pag_e$s_00_05 <- dat_pag_e$employment_00_05_mins
out_pag_e$s_00_05_lts <- as.numeric(0)
out_pag_e$s_00_05_lts <- dat_pag_e$employment_00_05_mins * (dat_pag_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_pag_e_lts$`employment_00_05_mins_lts2%`/100)
out_pag_e$s_05_10 <- as.numeric(0)
out_pag_e$s_05_10 <- dat_pag_e$employment_05_10_mins
out_pag_e$s_05_10_lts <- as.numeric(0)
out_pag_e$s_05_10_lts <- dat_pag_e$employment_05_10_mins * (dat_pag_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_pag_e_lts$`employment_05_10_mins_lts2%`/100)
out_pag_e$s_10_15 <- as.numeric(0)
out_pag_e$s_10_15 <- dat_pag_e$employment_10_15_mins
out_pag_e$s_10_15_lts <- as.numeric(0)
out_pag_e$s_10_15_lts <- dat_pag_e$employment_10_15_mins * (dat_pag_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_pag_e_lts$`employment_10_15_mins_lts2%`/100)
out_pag_e$s_15_20 <- as.numeric(0)
out_pag_e$s_15_20 <- dat_pag_e$employment_15_20_mins
out_pag_e$s_15_20_lts <- as.numeric(0)
out_pag_e$s_15_20_lts <- dat_pag_e$employment_15_20_mins * (dat_pag_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_pag_e_lts$`employment_15_20_mins_lts2%`/100)
out_pag_e$s_20_25 <- as.numeric(0)
out_pag_e$s_20_25 <- dat_pag_e$employment_20_25_mins
out_pag_e$s_20_25_lts <- as.numeric(0)
out_pag_e$s_20_25_lts <- dat_pag_e$employment_20_25_mins * (dat_pag_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_pag_e_lts$`employment_20_25_mins_lts2%`/100)
out_pag_e$s_25_30 <- as.numeric(0)
out_pag_e$s_25_30 <- dat_pag_e$employment_25_30_mins
out_pag_e$s_25_30_lts <- as.numeric(0)
out_pag_e$s_25_30_lts <- dat_pag_e$employment_25_30_mins * (dat_pag_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_pag_e_lts$`employment_25_30_mins_lts2%`/100)
out_pag_e$s_30_35 <- as.numeric(0)
out_pag_e$s_30_35 <- dat_pag_e$employment_30_35_mins
out_pag_e$s_30_35_lts <- as.numeric(0)
out_pag_e$s_30_35_lts <- dat_pag_e$employment_30_35_mins * (dat_pag_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_pag_e_lts$`employment_30_35_mins_lts2%`/100)
out_pag_e$s_35_40 <- as.numeric(0)
out_pag_e$s_35_40 <- dat_pag_e$employment_35_40_mins
out_pag_e$s_35_40_lts <- as.numeric(0)
out_pag_e$s_35_40_lts <- dat_pag_e$employment_35_40_mins * (dat_pag_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_pag_e_lts$`employment_35_40_mins_lts2%`/100)
out_pag_e$s_40_45 <- as.numeric(0)
out_pag_e$s_40_45 <- dat_pag_e$employment_40_45_mins
out_pag_e$s_40_45_lts <- as.numeric(0)
out_pag_e$s_40_45_lts <- dat_pag_e$employment_40_45_mins * (dat_pag_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_pag_e_lts$`employment_40_45_mins_lts2%`/100)
out_pag_e$s_45_50 <- as.numeric(0)
out_pag_e$s_45_50 <- dat_pag_e$employment_45_50_mins
out_pag_e$s_45_50_lts <- as.numeric(0)
out_pag_e$s_45_50_lts <- dat_pag_e$employment_45_50_mins * (dat_pag_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_pag_e_lts$`employment_45_50_mins_lts2%`/100)
out_pag_e$s_50_55 <- as.numeric(0)
out_pag_e$s_50_55 <- dat_pag_e$employment_50_55_mins
out_pag_e$s_50_55_lts <- as.numeric(0)
out_pag_e$s_50_55_lts <- dat_pag_e$employment_50_55_mins * (dat_pag_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_pag_e_lts$`employment_50_55_mins_lts2%`/100)
out_pag_e$s_55_60 <- as.numeric(0)
out_pag_e$s_55_60 <- dat_pag_e$employment_55_60_mins
out_pag_e$s_55_60_lts <- as.numeric(0)
out_pag_e$s_55_60_lts <- dat_pag_e$employment_55_60_mins * (dat_pag_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_pag_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_pag_e)

# Export accessibility output 
write.dbf(out_pag_e, (file=paste(dat_dir, "access_saf_employment_pag.dbf", sep="_data/_tabular/_outputs/")))

####################
### SCMPO: Casa Grande

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_scmpo_e <- read_csv(paste(dat_dir, "ibc_scmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e <- dat_scmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e)
dat_scmpo_e[is.na(dat_scmpo_e)] <- 0; dat_scmpo_e <- dat_scmpo_e %>% arrange(From)
dat_scmpo_e_lts <- read_csv(paste(dat_dir, "ibc_scmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e_lts <- dat_scmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e_lts)
dat_scmpo_e_lts[is.na(dat_scmpo_e_lts)] <- 0; dat_scmpo_e_lts <- dat_scmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_e <- as.data.frame(dat_scmpo_e_lts)[1]
out_scmpo_e$s_00_05 <- as.numeric(0)
out_scmpo_e$s_00_05 <- dat_scmpo_e$employment_00_05_mins
out_scmpo_e$s_00_05_lts <- as.numeric(0)
out_scmpo_e$s_00_05_lts <- dat_scmpo_e$employment_00_05_mins * (dat_scmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_scmpo_e$s_05_10 <- as.numeric(0)
out_scmpo_e$s_05_10 <- dat_scmpo_e$employment_05_10_mins
out_scmpo_e$s_05_10_lts <- as.numeric(0)
out_scmpo_e$s_05_10_lts <- dat_scmpo_e$employment_05_10_mins * (dat_scmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_scmpo_e$s_10_15 <- as.numeric(0)
out_scmpo_e$s_10_15 <- dat_scmpo_e$employment_10_15_mins
out_scmpo_e$s_10_15_lts <- as.numeric(0)
out_scmpo_e$s_10_15_lts <- dat_scmpo_e$employment_10_15_mins * (dat_scmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_scmpo_e$s_15_20 <- as.numeric(0)
out_scmpo_e$s_15_20 <- dat_scmpo_e$employment_15_20_mins
out_scmpo_e$s_15_20_lts <- as.numeric(0)
out_scmpo_e$s_15_20_lts <- dat_scmpo_e$employment_15_20_mins * (dat_scmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_scmpo_e$s_20_25 <- as.numeric(0)
out_scmpo_e$s_20_25 <- dat_scmpo_e$employment_20_25_mins
out_scmpo_e$s_20_25_lts <- as.numeric(0)
out_scmpo_e$s_20_25_lts <- dat_scmpo_e$employment_20_25_mins * (dat_scmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_scmpo_e$s_25_30 <- as.numeric(0)
out_scmpo_e$s_25_30 <- dat_scmpo_e$employment_25_30_mins
out_scmpo_e$s_25_30_lts <- as.numeric(0)
out_scmpo_e$s_25_30_lts <- dat_scmpo_e$employment_25_30_mins * (dat_scmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_scmpo_e$s_30_35 <- as.numeric(0)
out_scmpo_e$s_30_35 <- dat_scmpo_e$employment_30_35_mins
out_scmpo_e$s_30_35_lts <- as.numeric(0)
out_scmpo_e$s_30_35_lts <- dat_scmpo_e$employment_30_35_mins * (dat_scmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_scmpo_e$s_35_40 <- as.numeric(0)
out_scmpo_e$s_35_40 <- dat_scmpo_e$employment_35_40_mins
out_scmpo_e$s_35_40_lts <- as.numeric(0)
out_scmpo_e$s_35_40_lts <- dat_scmpo_e$employment_35_40_mins * (dat_scmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_scmpo_e$s_40_45 <- as.numeric(0)
out_scmpo_e$s_40_45 <- dat_scmpo_e$employment_40_45_mins
out_scmpo_e$s_40_45_lts <- as.numeric(0)
out_scmpo_e$s_40_45_lts <- dat_scmpo_e$employment_40_45_mins * (dat_scmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_scmpo_e$s_45_50 <- as.numeric(0)
out_scmpo_e$s_45_50 <- dat_scmpo_e$employment_45_50_mins
out_scmpo_e$s_45_50_lts <- as.numeric(0)
out_scmpo_e$s_45_50_lts <- dat_scmpo_e$employment_45_50_mins * (dat_scmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_scmpo_e$s_50_55 <- as.numeric(0)
out_scmpo_e$s_50_55 <- dat_scmpo_e$employment_50_55_mins
out_scmpo_e$s_50_55_lts <- as.numeric(0)
out_scmpo_e$s_50_55_lts <- dat_scmpo_e$employment_50_55_mins * (dat_scmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_scmpo_e$s_55_60 <- as.numeric(0)
out_scmpo_e$s_55_60 <- dat_scmpo_e$employment_55_60_mins
out_scmpo_e$s_55_60_lts <- as.numeric(0)
out_scmpo_e$s_55_60_lts <- dat_scmpo_e$employment_55_60_mins * (dat_scmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_scmpo_e)

# Export accessibility output 
write.dbf(out_scmpo_e, (file=paste(dat_dir, "access_ibc_employment_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_scmpo_e <- read_csv(paste(dat_dir, "eac_scmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e <- dat_scmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e)
dat_scmpo_e[is.na(dat_scmpo_e)] <- 0; dat_scmpo_e <- dat_scmpo_e %>% arrange(From)
dat_scmpo_e_lts <- read_csv(paste(dat_dir, "eac_scmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e_lts <- dat_scmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e_lts)
dat_scmpo_e_lts[is.na(dat_scmpo_e_lts)] <- 0; dat_scmpo_e_lts <- dat_scmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_e <- as.data.frame(dat_scmpo_e_lts)[1]
out_scmpo_e$s_00_05 <- as.numeric(0)
out_scmpo_e$s_00_05 <- dat_scmpo_e$employment_00_05_mins
out_scmpo_e$s_00_05_lts <- as.numeric(0)
out_scmpo_e$s_00_05_lts <- dat_scmpo_e$employment_00_05_mins * (dat_scmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_scmpo_e$s_05_10 <- as.numeric(0)
out_scmpo_e$s_05_10 <- dat_scmpo_e$employment_05_10_mins
out_scmpo_e$s_05_10_lts <- as.numeric(0)
out_scmpo_e$s_05_10_lts <- dat_scmpo_e$employment_05_10_mins * (dat_scmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_scmpo_e$s_10_15 <- as.numeric(0)
out_scmpo_e$s_10_15 <- dat_scmpo_e$employment_10_15_mins
out_scmpo_e$s_10_15_lts <- as.numeric(0)
out_scmpo_e$s_10_15_lts <- dat_scmpo_e$employment_10_15_mins * (dat_scmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_scmpo_e$s_15_20 <- as.numeric(0)
out_scmpo_e$s_15_20 <- dat_scmpo_e$employment_15_20_mins
out_scmpo_e$s_15_20_lts <- as.numeric(0)
out_scmpo_e$s_15_20_lts <- dat_scmpo_e$employment_15_20_mins * (dat_scmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_scmpo_e$s_20_25 <- as.numeric(0)
out_scmpo_e$s_20_25 <- dat_scmpo_e$employment_20_25_mins
out_scmpo_e$s_20_25_lts <- as.numeric(0)
out_scmpo_e$s_20_25_lts <- dat_scmpo_e$employment_20_25_mins * (dat_scmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_scmpo_e$s_25_30 <- as.numeric(0)
out_scmpo_e$s_25_30 <- dat_scmpo_e$employment_25_30_mins
out_scmpo_e$s_25_30_lts <- as.numeric(0)
out_scmpo_e$s_25_30_lts <- dat_scmpo_e$employment_25_30_mins * (dat_scmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_scmpo_e$s_30_35 <- as.numeric(0)
out_scmpo_e$s_30_35 <- dat_scmpo_e$employment_30_35_mins
out_scmpo_e$s_30_35_lts <- as.numeric(0)
out_scmpo_e$s_30_35_lts <- dat_scmpo_e$employment_30_35_mins * (dat_scmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_scmpo_e$s_35_40 <- as.numeric(0)
out_scmpo_e$s_35_40 <- dat_scmpo_e$employment_35_40_mins
out_scmpo_e$s_35_40_lts <- as.numeric(0)
out_scmpo_e$s_35_40_lts <- dat_scmpo_e$employment_35_40_mins * (dat_scmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_scmpo_e$s_40_45 <- as.numeric(0)
out_scmpo_e$s_40_45 <- dat_scmpo_e$employment_40_45_mins
out_scmpo_e$s_40_45_lts <- as.numeric(0)
out_scmpo_e$s_40_45_lts <- dat_scmpo_e$employment_40_45_mins * (dat_scmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_scmpo_e$s_45_50 <- as.numeric(0)
out_scmpo_e$s_45_50 <- dat_scmpo_e$employment_45_50_mins
out_scmpo_e$s_45_50_lts <- as.numeric(0)
out_scmpo_e$s_45_50_lts <- dat_scmpo_e$employment_45_50_mins * (dat_scmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_scmpo_e$s_50_55 <- as.numeric(0)
out_scmpo_e$s_50_55 <- dat_scmpo_e$employment_50_55_mins
out_scmpo_e$s_50_55_lts <- as.numeric(0)
out_scmpo_e$s_50_55_lts <- dat_scmpo_e$employment_50_55_mins * (dat_scmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_scmpo_e$s_55_60 <- as.numeric(0)
out_scmpo_e$s_55_60 <- dat_scmpo_e$employment_55_60_mins
out_scmpo_e$s_55_60_lts <- as.numeric(0)
out_scmpo_e$s_55_60_lts <- dat_scmpo_e$employment_55_60_mins * (dat_scmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_scmpo_e)

# Export accessibility output 
write.dbf(out_scmpo_e, (file=paste(dat_dir, "access_eac_employment_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_scmpo_e <- read_csv(paste(dat_dir, "saf_scmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e <- dat_scmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e)
dat_scmpo_e[is.na(dat_scmpo_e)] <- 0; dat_scmpo_e <- dat_scmpo_e %>% arrange(From)
dat_scmpo_e_lts <- read_csv(paste(dat_dir, "saf_scmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_scmpo_e_lts <- dat_scmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_e_lts)
dat_scmpo_e_lts[is.na(dat_scmpo_e_lts)] <- 0; dat_scmpo_e_lts <- dat_scmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_e <- as.data.frame(dat_scmpo_e_lts)[1]
out_scmpo_e$s_00_05 <- as.numeric(0)
out_scmpo_e$s_00_05 <- dat_scmpo_e$employment_00_05_mins
out_scmpo_e$s_00_05_lts <- as.numeric(0)
out_scmpo_e$s_00_05_lts <- dat_scmpo_e$employment_00_05_mins * (dat_scmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_scmpo_e$s_05_10 <- as.numeric(0)
out_scmpo_e$s_05_10 <- dat_scmpo_e$employment_05_10_mins
out_scmpo_e$s_05_10_lts <- as.numeric(0)
out_scmpo_e$s_05_10_lts <- dat_scmpo_e$employment_05_10_mins * (dat_scmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_scmpo_e$s_10_15 <- as.numeric(0)
out_scmpo_e$s_10_15 <- dat_scmpo_e$employment_10_15_mins
out_scmpo_e$s_10_15_lts <- as.numeric(0)
out_scmpo_e$s_10_15_lts <- dat_scmpo_e$employment_10_15_mins * (dat_scmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_scmpo_e$s_15_20 <- as.numeric(0)
out_scmpo_e$s_15_20 <- dat_scmpo_e$employment_15_20_mins
out_scmpo_e$s_15_20_lts <- as.numeric(0)
out_scmpo_e$s_15_20_lts <- dat_scmpo_e$employment_15_20_mins * (dat_scmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_scmpo_e$s_20_25 <- as.numeric(0)
out_scmpo_e$s_20_25 <- dat_scmpo_e$employment_20_25_mins
out_scmpo_e$s_20_25_lts <- as.numeric(0)
out_scmpo_e$s_20_25_lts <- dat_scmpo_e$employment_20_25_mins * (dat_scmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_scmpo_e$s_25_30 <- as.numeric(0)
out_scmpo_e$s_25_30 <- dat_scmpo_e$employment_25_30_mins
out_scmpo_e$s_25_30_lts <- as.numeric(0)
out_scmpo_e$s_25_30_lts <- dat_scmpo_e$employment_25_30_mins * (dat_scmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_scmpo_e$s_30_35 <- as.numeric(0)
out_scmpo_e$s_30_35 <- dat_scmpo_e$employment_30_35_mins
out_scmpo_e$s_30_35_lts <- as.numeric(0)
out_scmpo_e$s_30_35_lts <- dat_scmpo_e$employment_30_35_mins * (dat_scmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_scmpo_e$s_35_40 <- as.numeric(0)
out_scmpo_e$s_35_40 <- dat_scmpo_e$employment_35_40_mins
out_scmpo_e$s_35_40_lts <- as.numeric(0)
out_scmpo_e$s_35_40_lts <- dat_scmpo_e$employment_35_40_mins * (dat_scmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_scmpo_e$s_40_45 <- as.numeric(0)
out_scmpo_e$s_40_45 <- dat_scmpo_e$employment_40_45_mins
out_scmpo_e$s_40_45_lts <- as.numeric(0)
out_scmpo_e$s_40_45_lts <- dat_scmpo_e$employment_40_45_mins * (dat_scmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_scmpo_e$s_45_50 <- as.numeric(0)
out_scmpo_e$s_45_50 <- dat_scmpo_e$employment_45_50_mins
out_scmpo_e$s_45_50_lts <- as.numeric(0)
out_scmpo_e$s_45_50_lts <- dat_scmpo_e$employment_45_50_mins * (dat_scmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_scmpo_e$s_50_55 <- as.numeric(0)
out_scmpo_e$s_50_55 <- dat_scmpo_e$employment_50_55_mins
out_scmpo_e$s_50_55_lts <- as.numeric(0)
out_scmpo_e$s_50_55_lts <- dat_scmpo_e$employment_50_55_mins * (dat_scmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_scmpo_e$s_55_60 <- as.numeric(0)
out_scmpo_e$s_55_60 <- dat_scmpo_e$employment_55_60_mins
out_scmpo_e$s_55_60_lts <- as.numeric(0)
out_scmpo_e$s_55_60_lts <- dat_scmpo_e$employment_55_60_mins * (dat_scmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_scmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_scmpo_e)

# Export accessibility output 
write.dbf(out_scmpo_e, (file=paste(dat_dir, "access_saf_employment_scmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### SVMPO: Sierra Vista

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_svmpo_e <- read_csv(paste(dat_dir, "ibc_svmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e <- dat_svmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e)
dat_svmpo_e[is.na(dat_svmpo_e)] <- 0; dat_svmpo_e <- dat_svmpo_e %>% arrange(From)
dat_svmpo_e_lts <- read_csv(paste(dat_dir, "ibc_svmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e_lts <- dat_svmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e_lts)
dat_svmpo_e_lts[is.na(dat_svmpo_e_lts)] <- 0; dat_svmpo_e_lts <- dat_svmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_e <- as.data.frame(dat_svmpo_e_lts)[1]
out_svmpo_e$s_00_05 <- as.numeric(0)
out_svmpo_e$s_00_05 <- dat_svmpo_e$employment_00_05_mins
out_svmpo_e$s_00_05_lts <- as.numeric(0)
out_svmpo_e$s_00_05_lts <- dat_svmpo_e$employment_00_05_mins * (dat_svmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_svmpo_e$s_05_10 <- as.numeric(0)
out_svmpo_e$s_05_10 <- dat_svmpo_e$employment_05_10_mins
out_svmpo_e$s_05_10_lts <- as.numeric(0)
out_svmpo_e$s_05_10_lts <- dat_svmpo_e$employment_05_10_mins * (dat_svmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_svmpo_e$s_10_15 <- as.numeric(0)
out_svmpo_e$s_10_15 <- dat_svmpo_e$employment_10_15_mins
out_svmpo_e$s_10_15_lts <- as.numeric(0)
out_svmpo_e$s_10_15_lts <- dat_svmpo_e$employment_10_15_mins * (dat_svmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_svmpo_e$s_15_20 <- as.numeric(0)
out_svmpo_e$s_15_20 <- dat_svmpo_e$employment_15_20_mins
out_svmpo_e$s_15_20_lts <- as.numeric(0)
out_svmpo_e$s_15_20_lts <- dat_svmpo_e$employment_15_20_mins * (dat_svmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_svmpo_e$s_20_25 <- as.numeric(0)
out_svmpo_e$s_20_25 <- dat_svmpo_e$employment_20_25_mins
out_svmpo_e$s_20_25_lts <- as.numeric(0)
out_svmpo_e$s_20_25_lts <- dat_svmpo_e$employment_20_25_mins * (dat_svmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_svmpo_e$s_25_30 <- as.numeric(0)
out_svmpo_e$s_25_30 <- dat_svmpo_e$employment_25_30_mins
out_svmpo_e$s_25_30_lts <- as.numeric(0)
out_svmpo_e$s_25_30_lts <- dat_svmpo_e$employment_25_30_mins * (dat_svmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_svmpo_e$s_30_35 <- as.numeric(0)
out_svmpo_e$s_30_35 <- dat_svmpo_e$employment_30_35_mins
out_svmpo_e$s_30_35_lts <- as.numeric(0)
out_svmpo_e$s_30_35_lts <- dat_svmpo_e$employment_30_35_mins * (dat_svmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_svmpo_e$s_35_40 <- as.numeric(0)
out_svmpo_e$s_35_40 <- dat_svmpo_e$employment_35_40_mins
out_svmpo_e$s_35_40_lts <- as.numeric(0)
out_svmpo_e$s_35_40_lts <- dat_svmpo_e$employment_35_40_mins * (dat_svmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_svmpo_e$s_40_45 <- as.numeric(0)
out_svmpo_e$s_40_45 <- dat_svmpo_e$employment_40_45_mins
out_svmpo_e$s_40_45_lts <- as.numeric(0)
out_svmpo_e$s_40_45_lts <- dat_svmpo_e$employment_40_45_mins * (dat_svmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_svmpo_e$s_45_50 <- as.numeric(0)
out_svmpo_e$s_45_50 <- dat_svmpo_e$employment_45_50_mins
out_svmpo_e$s_45_50_lts <- as.numeric(0)
out_svmpo_e$s_45_50_lts <- dat_svmpo_e$employment_45_50_mins * (dat_svmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_svmpo_e$s_50_55 <- as.numeric(0)
out_svmpo_e$s_50_55 <- dat_svmpo_e$employment_50_55_mins
out_svmpo_e$s_50_55_lts <- as.numeric(0)
out_svmpo_e$s_50_55_lts <- dat_svmpo_e$employment_50_55_mins * (dat_svmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_svmpo_e$s_55_60 <- as.numeric(0)
out_svmpo_e$s_55_60 <- dat_svmpo_e$employment_55_60_mins
out_svmpo_e$s_55_60_lts <- as.numeric(0)
out_svmpo_e$s_55_60_lts <- dat_svmpo_e$employment_55_60_mins * (dat_svmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_svmpo_e)

# Export accessibility output 
write.dbf(out_svmpo_e, (file=paste(dat_dir, "access_ibc_employment_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_svmpo_e <- read_csv(paste(dat_dir, "eac_svmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e <- dat_svmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e)
dat_svmpo_e[is.na(dat_svmpo_e)] <- 0; dat_svmpo_e <- dat_svmpo_e %>% arrange(From)
dat_svmpo_e_lts <- read_csv(paste(dat_dir, "eac_svmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e_lts <- dat_svmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e_lts)
dat_svmpo_e_lts[is.na(dat_svmpo_e_lts)] <- 0; dat_svmpo_e_lts <- dat_svmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_e <- as.data.frame(dat_svmpo_e_lts)[1]
out_svmpo_e$s_00_05 <- as.numeric(0)
out_svmpo_e$s_00_05 <- dat_svmpo_e$employment_00_05_mins
out_svmpo_e$s_00_05_lts <- as.numeric(0)
out_svmpo_e$s_00_05_lts <- dat_svmpo_e$employment_00_05_mins * (dat_svmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_svmpo_e$s_05_10 <- as.numeric(0)
out_svmpo_e$s_05_10 <- dat_svmpo_e$employment_05_10_mins
out_svmpo_e$s_05_10_lts <- as.numeric(0)
out_svmpo_e$s_05_10_lts <- dat_svmpo_e$employment_05_10_mins * (dat_svmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_svmpo_e$s_10_15 <- as.numeric(0)
out_svmpo_e$s_10_15 <- dat_svmpo_e$employment_10_15_mins
out_svmpo_e$s_10_15_lts <- as.numeric(0)
out_svmpo_e$s_10_15_lts <- dat_svmpo_e$employment_10_15_mins * (dat_svmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_svmpo_e$s_15_20 <- as.numeric(0)
out_svmpo_e$s_15_20 <- dat_svmpo_e$employment_15_20_mins
out_svmpo_e$s_15_20_lts <- as.numeric(0)
out_svmpo_e$s_15_20_lts <- dat_svmpo_e$employment_15_20_mins * (dat_svmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_svmpo_e$s_20_25 <- as.numeric(0)
out_svmpo_e$s_20_25 <- dat_svmpo_e$employment_20_25_mins
out_svmpo_e$s_20_25_lts <- as.numeric(0)
out_svmpo_e$s_20_25_lts <- dat_svmpo_e$employment_20_25_mins * (dat_svmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_svmpo_e$s_25_30 <- as.numeric(0)
out_svmpo_e$s_25_30 <- dat_svmpo_e$employment_25_30_mins
out_svmpo_e$s_25_30_lts <- as.numeric(0)
out_svmpo_e$s_25_30_lts <- dat_svmpo_e$employment_25_30_mins * (dat_svmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_svmpo_e$s_30_35 <- as.numeric(0)
out_svmpo_e$s_30_35 <- dat_svmpo_e$employment_30_35_mins
out_svmpo_e$s_30_35_lts <- as.numeric(0)
out_svmpo_e$s_30_35_lts <- dat_svmpo_e$employment_30_35_mins * (dat_svmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_svmpo_e$s_35_40 <- as.numeric(0)
out_svmpo_e$s_35_40 <- dat_svmpo_e$employment_35_40_mins
out_svmpo_e$s_35_40_lts <- as.numeric(0)
out_svmpo_e$s_35_40_lts <- dat_svmpo_e$employment_35_40_mins * (dat_svmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_svmpo_e$s_40_45 <- as.numeric(0)
out_svmpo_e$s_40_45 <- dat_svmpo_e$employment_40_45_mins
out_svmpo_e$s_40_45_lts <- as.numeric(0)
out_svmpo_e$s_40_45_lts <- dat_svmpo_e$employment_40_45_mins * (dat_svmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_svmpo_e$s_45_50 <- as.numeric(0)
out_svmpo_e$s_45_50 <- dat_svmpo_e$employment_45_50_mins
out_svmpo_e$s_45_50_lts <- as.numeric(0)
out_svmpo_e$s_45_50_lts <- dat_svmpo_e$employment_45_50_mins * (dat_svmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_svmpo_e$s_50_55 <- as.numeric(0)
out_svmpo_e$s_50_55 <- dat_svmpo_e$employment_50_55_mins
out_svmpo_e$s_50_55_lts <- as.numeric(0)
out_svmpo_e$s_50_55_lts <- dat_svmpo_e$employment_50_55_mins * (dat_svmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_svmpo_e$s_55_60 <- as.numeric(0)
out_svmpo_e$s_55_60 <- dat_svmpo_e$employment_55_60_mins
out_svmpo_e$s_55_60_lts <- as.numeric(0)
out_svmpo_e$s_55_60_lts <- dat_svmpo_e$employment_55_60_mins * (dat_svmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_svmpo_e)

# Export accessibility output 
write.dbf(out_svmpo_e, (file=paste(dat_dir, "access_eac_employment_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_svmpo_e <- read_csv(paste(dat_dir, "saf_svmpo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e <- dat_svmpo_e %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e)
dat_svmpo_e[is.na(dat_svmpo_e)] <- 0; dat_svmpo_e <- dat_svmpo_e %>% arrange(From)
dat_svmpo_e_lts <- read_csv(paste(dat_dir, "saf_svmpo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_svmpo_e_lts <- dat_svmpo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_e_lts)
dat_svmpo_e_lts[is.na(dat_svmpo_e_lts)] <- 0; dat_svmpo_e_lts <- dat_svmpo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_e <- as.data.frame(dat_svmpo_e_lts)[1]
out_svmpo_e$s_00_05 <- as.numeric(0)
out_svmpo_e$s_00_05 <- dat_svmpo_e$employment_00_05_mins
out_svmpo_e$s_00_05_lts <- as.numeric(0)
out_svmpo_e$s_00_05_lts <- dat_svmpo_e$employment_00_05_mins * (dat_svmpo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_00_05_mins_lts2%`/100)
out_svmpo_e$s_05_10 <- as.numeric(0)
out_svmpo_e$s_05_10 <- dat_svmpo_e$employment_05_10_mins
out_svmpo_e$s_05_10_lts <- as.numeric(0)
out_svmpo_e$s_05_10_lts <- dat_svmpo_e$employment_05_10_mins * (dat_svmpo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_05_10_mins_lts2%`/100)
out_svmpo_e$s_10_15 <- as.numeric(0)
out_svmpo_e$s_10_15 <- dat_svmpo_e$employment_10_15_mins
out_svmpo_e$s_10_15_lts <- as.numeric(0)
out_svmpo_e$s_10_15_lts <- dat_svmpo_e$employment_10_15_mins * (dat_svmpo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_10_15_mins_lts2%`/100)
out_svmpo_e$s_15_20 <- as.numeric(0)
out_svmpo_e$s_15_20 <- dat_svmpo_e$employment_15_20_mins
out_svmpo_e$s_15_20_lts <- as.numeric(0)
out_svmpo_e$s_15_20_lts <- dat_svmpo_e$employment_15_20_mins * (dat_svmpo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_15_20_mins_lts2%`/100)
out_svmpo_e$s_20_25 <- as.numeric(0)
out_svmpo_e$s_20_25 <- dat_svmpo_e$employment_20_25_mins
out_svmpo_e$s_20_25_lts <- as.numeric(0)
out_svmpo_e$s_20_25_lts <- dat_svmpo_e$employment_20_25_mins * (dat_svmpo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_20_25_mins_lts2%`/100)
out_svmpo_e$s_25_30 <- as.numeric(0)
out_svmpo_e$s_25_30 <- dat_svmpo_e$employment_25_30_mins
out_svmpo_e$s_25_30_lts <- as.numeric(0)
out_svmpo_e$s_25_30_lts <- dat_svmpo_e$employment_25_30_mins * (dat_svmpo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_25_30_mins_lts2%`/100)
out_svmpo_e$s_30_35 <- as.numeric(0)
out_svmpo_e$s_30_35 <- dat_svmpo_e$employment_30_35_mins
out_svmpo_e$s_30_35_lts <- as.numeric(0)
out_svmpo_e$s_30_35_lts <- dat_svmpo_e$employment_30_35_mins * (dat_svmpo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_30_35_mins_lts2%`/100)
out_svmpo_e$s_35_40 <- as.numeric(0)
out_svmpo_e$s_35_40 <- dat_svmpo_e$employment_35_40_mins
out_svmpo_e$s_35_40_lts <- as.numeric(0)
out_svmpo_e$s_35_40_lts <- dat_svmpo_e$employment_35_40_mins * (dat_svmpo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_35_40_mins_lts2%`/100)
out_svmpo_e$s_40_45 <- as.numeric(0)
out_svmpo_e$s_40_45 <- dat_svmpo_e$employment_40_45_mins
out_svmpo_e$s_40_45_lts <- as.numeric(0)
out_svmpo_e$s_40_45_lts <- dat_svmpo_e$employment_40_45_mins * (dat_svmpo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_40_45_mins_lts2%`/100)
out_svmpo_e$s_45_50 <- as.numeric(0)
out_svmpo_e$s_45_50 <- dat_svmpo_e$employment_45_50_mins
out_svmpo_e$s_45_50_lts <- as.numeric(0)
out_svmpo_e$s_45_50_lts <- dat_svmpo_e$employment_45_50_mins * (dat_svmpo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_45_50_mins_lts2%`/100)
out_svmpo_e$s_50_55 <- as.numeric(0)
out_svmpo_e$s_50_55 <- dat_svmpo_e$employment_50_55_mins
out_svmpo_e$s_50_55_lts <- as.numeric(0)
out_svmpo_e$s_50_55_lts <- dat_svmpo_e$employment_50_55_mins * (dat_svmpo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_50_55_mins_lts2%`/100)
out_svmpo_e$s_55_60 <- as.numeric(0)
out_svmpo_e$s_55_60 <- dat_svmpo_e$employment_55_60_mins
out_svmpo_e$s_55_60_lts <- as.numeric(0)
out_svmpo_e$s_55_60_lts <- dat_svmpo_e$employment_55_60_mins * (dat_svmpo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_svmpo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_svmpo_e)

# Export accessibility output 
write.dbf(out_svmpo_e, (file=paste(dat_dir, "access_saf_employment_svmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### YMPO: Yuma

### INTERESTED BUT CONCERNED

### Employment

# Import and clean cranc output data
dat_ympo_e <- read_csv(paste(dat_dir, "ibc_ympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e <- dat_ympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e)
dat_ympo_e[is.na(dat_ympo_e)] <- 0; dat_ympo_e <- dat_ympo_e %>% arrange(From)
dat_ympo_e_lts <- read_csv(paste(dat_dir, "ibc_ympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e_lts <- dat_ympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e_lts)
dat_ympo_e_lts[is.na(dat_ympo_e_lts)] <- 0; dat_ympo_e_lts <- dat_ympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_e <- as.data.frame(dat_ympo_e_lts)[1]
out_ympo_e$s_00_05 <- as.numeric(0)
out_ympo_e$s_00_05 <- dat_ympo_e$employment_00_05_mins
out_ympo_e$s_00_05_lts <- as.numeric(0)
out_ympo_e$s_00_05_lts <- dat_ympo_e$employment_00_05_mins * (dat_ympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_ympo_e$s_05_10 <- as.numeric(0)
out_ympo_e$s_05_10 <- dat_ympo_e$employment_05_10_mins
out_ympo_e$s_05_10_lts <- as.numeric(0)
out_ympo_e$s_05_10_lts <- dat_ympo_e$employment_05_10_mins * (dat_ympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_ympo_e$s_10_15 <- as.numeric(0)
out_ympo_e$s_10_15 <- dat_ympo_e$employment_10_15_mins
out_ympo_e$s_10_15_lts <- as.numeric(0)
out_ympo_e$s_10_15_lts <- dat_ympo_e$employment_10_15_mins * (dat_ympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_ympo_e$s_15_20 <- as.numeric(0)
out_ympo_e$s_15_20 <- dat_ympo_e$employment_15_20_mins
out_ympo_e$s_15_20_lts <- as.numeric(0)
out_ympo_e$s_15_20_lts <- dat_ympo_e$employment_15_20_mins * (dat_ympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_ympo_e$s_20_25 <- as.numeric(0)
out_ympo_e$s_20_25 <- dat_ympo_e$employment_20_25_mins
out_ympo_e$s_20_25_lts <- as.numeric(0)
out_ympo_e$s_20_25_lts <- dat_ympo_e$employment_20_25_mins * (dat_ympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_ympo_e$s_25_30 <- as.numeric(0)
out_ympo_e$s_25_30 <- dat_ympo_e$employment_25_30_mins
out_ympo_e$s_25_30_lts <- as.numeric(0)
out_ympo_e$s_25_30_lts <- dat_ympo_e$employment_25_30_mins * (dat_ympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_ympo_e$s_30_35 <- as.numeric(0)
out_ympo_e$s_30_35 <- dat_ympo_e$employment_30_35_mins
out_ympo_e$s_30_35_lts <- as.numeric(0)
out_ympo_e$s_30_35_lts <- dat_ympo_e$employment_30_35_mins * (dat_ympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_ympo_e$s_35_40 <- as.numeric(0)
out_ympo_e$s_35_40 <- dat_ympo_e$employment_35_40_mins
out_ympo_e$s_35_40_lts <- as.numeric(0)
out_ympo_e$s_35_40_lts <- dat_ympo_e$employment_35_40_mins * (dat_ympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_ympo_e$s_40_45 <- as.numeric(0)
out_ympo_e$s_40_45 <- dat_ympo_e$employment_40_45_mins
out_ympo_e$s_40_45_lts <- as.numeric(0)
out_ympo_e$s_40_45_lts <- dat_ympo_e$employment_40_45_mins * (dat_ympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_ympo_e$s_45_50 <- as.numeric(0)
out_ympo_e$s_45_50 <- dat_ympo_e$employment_45_50_mins
out_ympo_e$s_45_50_lts <- as.numeric(0)
out_ympo_e$s_45_50_lts <- dat_ympo_e$employment_45_50_mins * (dat_ympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_ympo_e$s_50_55 <- as.numeric(0)
out_ympo_e$s_50_55 <- dat_ympo_e$employment_50_55_mins
out_ympo_e$s_50_55_lts <- as.numeric(0)
out_ympo_e$s_50_55_lts <- dat_ympo_e$employment_50_55_mins * (dat_ympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_ympo_e$s_55_60 <- as.numeric(0)
out_ympo_e$s_55_60 <- dat_ympo_e$employment_55_60_mins
out_ympo_e$s_55_60_lts <- as.numeric(0)
out_ympo_e$s_55_60_lts <- dat_ympo_e$employment_55_60_mins * (dat_ympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_ympo_e)

# Export accessibility output 
write.dbf(out_ympo_e, (file=paste(dat_dir, "access_ibc_employment_ympo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Employment

# Import and clean cranc output data
dat_ympo_e <- read_csv(paste(dat_dir, "eac_ympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e <- dat_ympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e)
dat_ympo_e[is.na(dat_ympo_e)] <- 0; dat_ympo_e <- dat_ympo_e %>% arrange(From)
dat_ympo_e_lts <- read_csv(paste(dat_dir, "eac_ympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e_lts <- dat_ympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e_lts)
dat_ympo_e_lts[is.na(dat_ympo_e_lts)] <- 0; dat_ympo_e_lts <- dat_ympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_e <- as.data.frame(dat_ympo_e_lts)[1]
out_ympo_e$s_00_05 <- as.numeric(0)
out_ympo_e$s_00_05 <- dat_ympo_e$employment_00_05_mins
out_ympo_e$s_00_05_lts <- as.numeric(0)
out_ympo_e$s_00_05_lts <- dat_ympo_e$employment_00_05_mins * (dat_ympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_ympo_e$s_05_10 <- as.numeric(0)
out_ympo_e$s_05_10 <- dat_ympo_e$employment_05_10_mins
out_ympo_e$s_05_10_lts <- as.numeric(0)
out_ympo_e$s_05_10_lts <- dat_ympo_e$employment_05_10_mins * (dat_ympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_ympo_e$s_10_15 <- as.numeric(0)
out_ympo_e$s_10_15 <- dat_ympo_e$employment_10_15_mins
out_ympo_e$s_10_15_lts <- as.numeric(0)
out_ympo_e$s_10_15_lts <- dat_ympo_e$employment_10_15_mins * (dat_ympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_ympo_e$s_15_20 <- as.numeric(0)
out_ympo_e$s_15_20 <- dat_ympo_e$employment_15_20_mins
out_ympo_e$s_15_20_lts <- as.numeric(0)
out_ympo_e$s_15_20_lts <- dat_ympo_e$employment_15_20_mins * (dat_ympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_ympo_e$s_20_25 <- as.numeric(0)
out_ympo_e$s_20_25 <- dat_ympo_e$employment_20_25_mins
out_ympo_e$s_20_25_lts <- as.numeric(0)
out_ympo_e$s_20_25_lts <- dat_ympo_e$employment_20_25_mins * (dat_ympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_ympo_e$s_25_30 <- as.numeric(0)
out_ympo_e$s_25_30 <- dat_ympo_e$employment_25_30_mins
out_ympo_e$s_25_30_lts <- as.numeric(0)
out_ympo_e$s_25_30_lts <- dat_ympo_e$employment_25_30_mins * (dat_ympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_ympo_e$s_30_35 <- as.numeric(0)
out_ympo_e$s_30_35 <- dat_ympo_e$employment_30_35_mins
out_ympo_e$s_30_35_lts <- as.numeric(0)
out_ympo_e$s_30_35_lts <- dat_ympo_e$employment_30_35_mins * (dat_ympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_ympo_e$s_35_40 <- as.numeric(0)
out_ympo_e$s_35_40 <- dat_ympo_e$employment_35_40_mins
out_ympo_e$s_35_40_lts <- as.numeric(0)
out_ympo_e$s_35_40_lts <- dat_ympo_e$employment_35_40_mins * (dat_ympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_ympo_e$s_40_45 <- as.numeric(0)
out_ympo_e$s_40_45 <- dat_ympo_e$employment_40_45_mins
out_ympo_e$s_40_45_lts <- as.numeric(0)
out_ympo_e$s_40_45_lts <- dat_ympo_e$employment_40_45_mins * (dat_ympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_ympo_e$s_45_50 <- as.numeric(0)
out_ympo_e$s_45_50 <- dat_ympo_e$employment_45_50_mins
out_ympo_e$s_45_50_lts <- as.numeric(0)
out_ympo_e$s_45_50_lts <- dat_ympo_e$employment_45_50_mins * (dat_ympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_ympo_e$s_50_55 <- as.numeric(0)
out_ympo_e$s_50_55 <- dat_ympo_e$employment_50_55_mins
out_ympo_e$s_50_55_lts <- as.numeric(0)
out_ympo_e$s_50_55_lts <- dat_ympo_e$employment_50_55_mins * (dat_ympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_ympo_e$s_55_60 <- as.numeric(0)
out_ympo_e$s_55_60 <- dat_ympo_e$employment_55_60_mins
out_ympo_e$s_55_60_lts <- as.numeric(0)
out_ympo_e$s_55_60_lts <- dat_ympo_e$employment_55_60_mins * (dat_ympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_ympo_e)

# Export accessibility output 
write.dbf(out_ympo_e, (file=paste(dat_dir, "access_eac_employment_ympo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Employment

# Import and clean cranc output data
dat_ympo_e <- read_csv(paste(dat_dir, "saf_ympo_employment_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e <- dat_ympo_e %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e)
dat_ympo_e[is.na(dat_ympo_e)] <- 0; dat_ympo_e <- dat_ympo_e %>% arrange(From)
dat_ympo_e_lts <- read_csv(paste(dat_dir, "saf_ympo_employment_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_employment/"))
dat_ympo_e_lts <- dat_ympo_e_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_e_lts)
dat_ympo_e_lts[is.na(dat_ympo_e_lts)] <- 0; dat_ympo_e_lts <- dat_ympo_e_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_e <- as.data.frame(dat_ympo_e_lts)[1]
out_ympo_e$s_00_05 <- as.numeric(0)
out_ympo_e$s_00_05 <- dat_ympo_e$employment_00_05_mins
out_ympo_e$s_00_05_lts <- as.numeric(0)
out_ympo_e$s_00_05_lts <- dat_ympo_e$employment_00_05_mins * (dat_ympo_e_lts$`employment_00_05_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_00_05_mins_lts2%`/100)
out_ympo_e$s_05_10 <- as.numeric(0)
out_ympo_e$s_05_10 <- dat_ympo_e$employment_05_10_mins
out_ympo_e$s_05_10_lts <- as.numeric(0)
out_ympo_e$s_05_10_lts <- dat_ympo_e$employment_05_10_mins * (dat_ympo_e_lts$`employment_05_10_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_05_10_mins_lts2%`/100)
out_ympo_e$s_10_15 <- as.numeric(0)
out_ympo_e$s_10_15 <- dat_ympo_e$employment_10_15_mins
out_ympo_e$s_10_15_lts <- as.numeric(0)
out_ympo_e$s_10_15_lts <- dat_ympo_e$employment_10_15_mins * (dat_ympo_e_lts$`employment_10_15_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_10_15_mins_lts2%`/100)
out_ympo_e$s_15_20 <- as.numeric(0)
out_ympo_e$s_15_20 <- dat_ympo_e$employment_15_20_mins
out_ympo_e$s_15_20_lts <- as.numeric(0)
out_ympo_e$s_15_20_lts <- dat_ympo_e$employment_15_20_mins * (dat_ympo_e_lts$`employment_15_20_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_15_20_mins_lts2%`/100)
out_ympo_e$s_20_25 <- as.numeric(0)
out_ympo_e$s_20_25 <- dat_ympo_e$employment_20_25_mins
out_ympo_e$s_20_25_lts <- as.numeric(0)
out_ympo_e$s_20_25_lts <- dat_ympo_e$employment_20_25_mins * (dat_ympo_e_lts$`employment_20_25_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_20_25_mins_lts2%`/100)
out_ympo_e$s_25_30 <- as.numeric(0)
out_ympo_e$s_25_30 <- dat_ympo_e$employment_25_30_mins
out_ympo_e$s_25_30_lts <- as.numeric(0)
out_ympo_e$s_25_30_lts <- dat_ympo_e$employment_25_30_mins * (dat_ympo_e_lts$`employment_25_30_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_25_30_mins_lts2%`/100)
out_ympo_e$s_30_35 <- as.numeric(0)
out_ympo_e$s_30_35 <- dat_ympo_e$employment_30_35_mins
out_ympo_e$s_30_35_lts <- as.numeric(0)
out_ympo_e$s_30_35_lts <- dat_ympo_e$employment_30_35_mins * (dat_ympo_e_lts$`employment_30_35_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_30_35_mins_lts2%`/100)
out_ympo_e$s_35_40 <- as.numeric(0)
out_ympo_e$s_35_40 <- dat_ympo_e$employment_35_40_mins
out_ympo_e$s_35_40_lts <- as.numeric(0)
out_ympo_e$s_35_40_lts <- dat_ympo_e$employment_35_40_mins * (dat_ympo_e_lts$`employment_35_40_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_35_40_mins_lts2%`/100)
out_ympo_e$s_40_45 <- as.numeric(0)
out_ympo_e$s_40_45 <- dat_ympo_e$employment_40_45_mins
out_ympo_e$s_40_45_lts <- as.numeric(0)
out_ympo_e$s_40_45_lts <- dat_ympo_e$employment_40_45_mins * (dat_ympo_e_lts$`employment_40_45_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_40_45_mins_lts2%`/100)
out_ympo_e$s_45_50 <- as.numeric(0)
out_ympo_e$s_45_50 <- dat_ympo_e$employment_45_50_mins
out_ympo_e$s_45_50_lts <- as.numeric(0)
out_ympo_e$s_45_50_lts <- dat_ympo_e$employment_45_50_mins * (dat_ympo_e_lts$`employment_45_50_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_45_50_mins_lts2%`/100)
out_ympo_e$s_50_55 <- as.numeric(0)
out_ympo_e$s_50_55 <- dat_ympo_e$employment_50_55_mins
out_ympo_e$s_50_55_lts <- as.numeric(0)
out_ympo_e$s_50_55_lts <- dat_ympo_e$employment_50_55_mins * (dat_ympo_e_lts$`employment_50_55_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_50_55_mins_lts2%`/100)
out_ympo_e$s_55_60 <- as.numeric(0)
out_ympo_e$s_55_60 <- dat_ympo_e$employment_55_60_mins
out_ympo_e$s_55_60_lts <- as.numeric(0)
out_ympo_e$s_55_60_lts <- dat_ympo_e$employment_55_60_mins * (dat_ympo_e_lts$`employment_55_60_mins_lts1%`/100)+(dat_ympo_e_lts$`employment_55_60_mins_lts2%`/100)
str(out_ympo_e)

# Export accessibility output 
write.dbf(out_ympo_e, (file=paste(dat_dir, "access_saf_employment_ympo.dbf", sep="_data/_tabular/_outputs/")))

####################
### END OF SCRIPT
