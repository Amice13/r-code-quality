####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Cumulative Opportunities with LTS Impedance
#       Schools and Grocery Stores (IBC/EAC/SAF)
# Created: 05.18.2023
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

### Schools

# Import and clean cranc output data
dat_cympo_s <- read_csv(paste(dat_dir, "ibc_cympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s <- dat_cympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s)
dat_cympo_s[is.na(dat_cympo_s)] <- 0; dat_cympo_s <- dat_cympo_s %>% arrange(From)
dat_cympo_s_lts <- read_csv(paste(dat_dir, "ibc_cympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s_lts <- dat_cympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s_lts)
dat_cympo_s_lts[is.na(dat_cympo_s_lts)] <- 0; dat_cympo_s_lts <- dat_cympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_s <- as.data.frame(dat_cympo_s_lts)[1]
out_cympo_s$s_00_05 <- as.numeric(0)
out_cympo_s$s_00_05 <- dat_cympo_s$school_00_05_mins
out_cympo_s$s_00_05_lts <- as.numeric(0)
out_cympo_s$s_00_05_lts <- dat_cympo_s$school_00_05_mins * (dat_cympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_cympo_s_lts$`school_00_05_mins_lts2%`/100)
out_cympo_s$s_05_10 <- as.numeric(0)
out_cympo_s$s_05_10 <- dat_cympo_s$school_05_10_mins
out_cympo_s$s_05_10_lts <- as.numeric(0)
out_cympo_s$s_05_10_lts <- dat_cympo_s$school_05_10_mins * (dat_cympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_cympo_s_lts$`school_05_10_mins_lts2%`/100)
out_cympo_s$s_10_15 <- as.numeric(0)
out_cympo_s$s_10_15 <- dat_cympo_s$school_10_15_mins
out_cympo_s$s_10_15_lts <- as.numeric(0)
out_cympo_s$s_10_15_lts <- dat_cympo_s$school_10_15_mins * (dat_cympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_cympo_s_lts$`school_10_15_mins_lts2%`/100)
out_cympo_s$s_15_20 <- as.numeric(0)
out_cympo_s$s_15_20 <- dat_cympo_s$school_15_20_mins
out_cympo_s$s_15_20_lts <- as.numeric(0)
out_cympo_s$s_15_20_lts <- dat_cympo_s$school_15_20_mins * (dat_cympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_cympo_s_lts$`school_15_20_mins_lts2%`/100)
out_cympo_s$s_20_25 <- as.numeric(0)
out_cympo_s$s_20_25 <- dat_cympo_s$school_20_25_mins
out_cympo_s$s_20_25_lts <- as.numeric(0)
out_cympo_s$s_20_25_lts <- dat_cympo_s$school_20_25_mins * (dat_cympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_cympo_s_lts$`school_20_25_mins_lts2%`/100)
out_cympo_s$s_25_30 <- as.numeric(0)
out_cympo_s$s_25_30 <- dat_cympo_s$school_25_30_mins
out_cympo_s$s_25_30_lts <- as.numeric(0)
out_cympo_s$s_25_30_lts <- dat_cympo_s$school_25_30_mins * (dat_cympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_cympo_s_lts$`school_25_30_mins_lts2%`/100)
out_cympo_s$s_30_35 <- as.numeric(0)
out_cympo_s$s_30_35 <- dat_cympo_s$school_30_35_mins
out_cympo_s$s_30_35_lts <- as.numeric(0)
out_cympo_s$s_30_35_lts <- dat_cympo_s$school_30_35_mins * (dat_cympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_cympo_s_lts$`school_30_35_mins_lts2%`/100)
out_cympo_s$s_35_40 <- as.numeric(0)
out_cympo_s$s_35_40 <- dat_cympo_s$school_35_40_mins
out_cympo_s$s_35_40_lts <- as.numeric(0)
out_cympo_s$s_35_40_lts <- dat_cympo_s$school_35_40_mins * (dat_cympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_cympo_s_lts$`school_35_40_mins_lts2%`/100)
out_cympo_s$s_40_45 <- as.numeric(0)
out_cympo_s$s_40_45 <- dat_cympo_s$school_40_45_mins
out_cympo_s$s_40_45_lts <- as.numeric(0)
out_cympo_s$s_40_45_lts <- dat_cympo_s$school_40_45_mins * (dat_cympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_cympo_s_lts$`school_40_45_mins_lts2%`/100)
out_cympo_s$s_45_50 <- as.numeric(0)
out_cympo_s$s_45_50 <- dat_cympo_s$school_45_50_mins
out_cympo_s$s_45_50_lts <- as.numeric(0)
out_cympo_s$s_45_50_lts <- dat_cympo_s$school_45_50_mins * (dat_cympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_cympo_s_lts$`school_45_50_mins_lts2%`/100)
out_cympo_s$s_50_55 <- as.numeric(0)
out_cympo_s$s_50_55 <- dat_cympo_s$school_50_55_mins
out_cympo_s$s_50_55_lts <- as.numeric(0)
out_cympo_s$s_50_55_lts <- dat_cympo_s$school_50_55_mins * (dat_cympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_cympo_s_lts$`school_50_55_mins_lts2%`/100)
out_cympo_s$s_55_60 <- as.numeric(0)
out_cympo_s$s_55_60 <- dat_cympo_s$school_55_60_mins
out_cympo_s$s_55_60_lts <- as.numeric(0)
out_cympo_s$s_55_60_lts <- dat_cympo_s$school_55_60_mins * (dat_cympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_cympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_cympo_s)

# Export accessibility output 
write.dbf(out_cympo_s, (file=paste(dat_dir, "access_ibc_schools_cympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_cympo_m <- read_csv(paste(dat_dir, "ibc_cympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m <- dat_cympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m)
dat_cympo_m[is.na(dat_cympo_m)] <- 0; dat_cympo_m <- dat_cympo_m %>% arrange(From)
dat_cympo_m_lts <- read_csv(paste(dat_dir, "ibc_cympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m_lts <- dat_cympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m_lts)
dat_cympo_m_lts[is.na(dat_cympo_m_lts)] <- 0; dat_cympo_m_lts <- dat_cympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_m <- as.data.frame(dat_cympo_m_lts)[1]
out_cympo_m$m_00_05 <- as.numeric(0)
out_cympo_m$m_00_05 <- dat_cympo_m$grocery_00_05_mins
out_cympo_m$m_00_05_lts <- as.numeric(0)
out_cympo_m$m_00_05_lts <- dat_cympo_m$grocery_00_05_mins * (dat_cympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_cympo_m$m_05_10 <- as.numeric(0)
out_cympo_m$m_05_10 <- dat_cympo_m$grocery_05_10_mins
out_cympo_m$m_05_10_lts <- as.numeric(0)
out_cympo_m$m_05_10_lts <- dat_cympo_m$grocery_05_10_mins * (dat_cympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_cympo_m$m_10_15 <- as.numeric(0)
out_cympo_m$m_10_15 <- dat_cympo_m$grocery_10_15_mins
out_cympo_m$m_10_15_lts <- as.numeric(0)
out_cympo_m$m_10_15_lts <- dat_cympo_m$grocery_10_15_mins * (dat_cympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_cympo_m$m_15_20 <- as.numeric(0)
out_cympo_m$m_15_20 <- dat_cympo_m$grocery_15_20_mins
out_cympo_m$m_15_20_lts <- as.numeric(0)
out_cympo_m$m_15_20_lts <- dat_cympo_m$grocery_15_20_mins * (dat_cympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_cympo_m$m_20_25 <- as.numeric(0)
out_cympo_m$m_20_25 <- dat_cympo_m$grocery_20_25_mins
out_cympo_m$m_20_25_lts <- as.numeric(0)
out_cympo_m$m_20_25_lts <- dat_cympo_m$grocery_20_25_mins * (dat_cympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_cympo_m$m_25_30 <- as.numeric(0)
out_cympo_m$m_25_30 <- dat_cympo_m$grocery_25_30_mins
out_cympo_m$m_25_30_lts <- as.numeric(0)
out_cympo_m$m_25_30_lts <- dat_cympo_m$grocery_25_30_mins * (dat_cympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_cympo_m$m_30_35 <- as.numeric(0)
out_cympo_m$m_30_35 <- dat_cympo_m$grocery_30_35_mins
out_cympo_m$m_30_35_lts <- as.numeric(0)
out_cympo_m$m_30_35_lts <- dat_cympo_m$grocery_30_35_mins * (dat_cympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_cympo_m$m_35_40 <- as.numeric(0)
out_cympo_m$m_35_40 <- dat_cympo_m$grocery_35_40_mins
out_cympo_m$m_35_40_lts <- as.numeric(0)
out_cympo_m$m_35_40_lts <- dat_cympo_m$grocery_35_40_mins * (dat_cympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_cympo_m$m_40_45 <- as.numeric(0)
out_cympo_m$m_40_45 <- dat_cympo_m$grocery_40_45_mins
out_cympo_m$m_40_45_lts <- as.numeric(0)
out_cympo_m$m_40_45_lts <- dat_cympo_m$grocery_40_45_mins * (dat_cympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_cympo_m$m_45_50 <- as.numeric(0)
out_cympo_m$m_45_50 <- dat_cympo_m$grocery_45_50_mins
out_cympo_m$m_45_50_lts <- as.numeric(0)
out_cympo_m$m_45_50_lts <- dat_cympo_m$grocery_45_50_mins * (dat_cympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_cympo_m$m_50_55 <- as.numeric(0)
out_cympo_m$m_50_55 <- dat_cympo_m$grocery_50_55_mins
out_cympo_m$m_50_55_lts <- as.numeric(0)
out_cympo_m$m_50_55_lts <- dat_cympo_m$grocery_50_55_mins * (dat_cympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_cympo_m$m_55_60 <- as.numeric(0)
out_cympo_m$m_55_60 <- dat_cympo_m$grocery_55_60_mins
out_cympo_m$m_55_60_lts <- as.numeric(0)
out_cympo_m$m_55_60_lts <- dat_cympo_m$grocery_55_60_mins * (dat_cympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_cympo_m)

# Export accessibility output 
write.dbf(out_cympo_m, (file=paste(dat_dir, "access_ibc_markets_cympo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_cympo_s <- read_csv(paste(dat_dir, "eac_cympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s <- dat_cympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s)
dat_cympo_s[is.na(dat_cympo_s)] <- 0; dat_cympo_s <- dat_cympo_s %>% arrange(From)
dat_cympo_s_lts <- read_csv(paste(dat_dir, "eac_cympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s_lts <- dat_cympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s_lts)
dat_cympo_s_lts[is.na(dat_cympo_s_lts)] <- 0; dat_cympo_s_lts <- dat_cympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_s <- as.data.frame(dat_cympo_s_lts)[1]
out_cympo_s$s_00_05 <- as.numeric(0)
out_cympo_s$s_00_05 <- dat_cympo_s$school_00_05_mins
out_cympo_s$s_00_05_lts <- as.numeric(0)
out_cympo_s$s_00_05_lts <- dat_cympo_s$school_00_05_mins * (dat_cympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_cympo_s_lts$`school_00_05_mins_lts2%`/100)
out_cympo_s$s_05_10 <- as.numeric(0)
out_cympo_s$s_05_10 <- dat_cympo_s$school_05_10_mins
out_cympo_s$s_05_10_lts <- as.numeric(0)
out_cympo_s$s_05_10_lts <- dat_cympo_s$school_05_10_mins * (dat_cympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_cympo_s_lts$`school_05_10_mins_lts2%`/100)
out_cympo_s$s_10_15 <- as.numeric(0)
out_cympo_s$s_10_15 <- dat_cympo_s$school_10_15_mins
out_cympo_s$s_10_15_lts <- as.numeric(0)
out_cympo_s$s_10_15_lts <- dat_cympo_s$school_10_15_mins * (dat_cympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_cympo_s_lts$`school_10_15_mins_lts2%`/100)
out_cympo_s$s_15_20 <- as.numeric(0)
out_cympo_s$s_15_20 <- dat_cympo_s$school_15_20_mins
out_cympo_s$s_15_20_lts <- as.numeric(0)
out_cympo_s$s_15_20_lts <- dat_cympo_s$school_15_20_mins * (dat_cympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_cympo_s_lts$`school_15_20_mins_lts2%`/100)
out_cympo_s$s_20_25 <- as.numeric(0)
out_cympo_s$s_20_25 <- dat_cympo_s$school_20_25_mins
out_cympo_s$s_20_25_lts <- as.numeric(0)
out_cympo_s$s_20_25_lts <- dat_cympo_s$school_20_25_mins * (dat_cympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_cympo_s_lts$`school_20_25_mins_lts2%`/100)
out_cympo_s$s_25_30 <- as.numeric(0)
out_cympo_s$s_25_30 <- dat_cympo_s$school_25_30_mins
out_cympo_s$s_25_30_lts <- as.numeric(0)
out_cympo_s$s_25_30_lts <- dat_cympo_s$school_25_30_mins * (dat_cympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_cympo_s_lts$`school_25_30_mins_lts2%`/100)
out_cympo_s$s_30_35 <- as.numeric(0)
out_cympo_s$s_30_35 <- dat_cympo_s$school_30_35_mins
out_cympo_s$s_30_35_lts <- as.numeric(0)
out_cympo_s$s_30_35_lts <- dat_cympo_s$school_30_35_mins * (dat_cympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_cympo_s_lts$`school_30_35_mins_lts2%`/100)
out_cympo_s$s_35_40 <- as.numeric(0)
out_cympo_s$s_35_40 <- dat_cympo_s$school_35_40_mins
out_cympo_s$s_35_40_lts <- as.numeric(0)
out_cympo_s$s_35_40_lts <- dat_cympo_s$school_35_40_mins * (dat_cympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_cympo_s_lts$`school_35_40_mins_lts2%`/100)
out_cympo_s$s_40_45 <- as.numeric(0)
out_cympo_s$s_40_45 <- dat_cympo_s$school_40_45_mins
out_cympo_s$s_40_45_lts <- as.numeric(0)
out_cympo_s$s_40_45_lts <- dat_cympo_s$school_40_45_mins * (dat_cympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_cympo_s_lts$`school_40_45_mins_lts2%`/100)
out_cympo_s$s_45_50 <- as.numeric(0)
out_cympo_s$s_45_50 <- dat_cympo_s$school_45_50_mins
out_cympo_s$s_45_50_lts <- as.numeric(0)
out_cympo_s$s_45_50_lts <- dat_cympo_s$school_45_50_mins * (dat_cympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_cympo_s_lts$`school_45_50_mins_lts2%`/100)
out_cympo_s$s_50_55 <- as.numeric(0)
out_cympo_s$s_50_55 <- dat_cympo_s$school_50_55_mins
out_cympo_s$s_50_55_lts <- as.numeric(0)
out_cympo_s$s_50_55_lts <- dat_cympo_s$school_50_55_mins * (dat_cympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_cympo_s_lts$`school_50_55_mins_lts2%`/100)
out_cympo_s$s_55_60 <- as.numeric(0)
out_cympo_s$s_55_60 <- dat_cympo_s$school_55_60_mins
out_cympo_s$s_55_60_lts <- as.numeric(0)
out_cympo_s$s_55_60_lts <- dat_cympo_s$school_55_60_mins * (dat_cympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_cympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_cympo_s)

# Export accessibility output 
write.dbf(out_cympo_s, (file=paste(dat_dir, "access_eac_schools_cympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_cympo_m <- read_csv(paste(dat_dir, "eac_cympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m <- dat_cympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m)
dat_cympo_m[is.na(dat_cympo_m)] <- 0; dat_cympo_m <- dat_cympo_m %>% arrange(From)
dat_cympo_m_lts <- read_csv(paste(dat_dir, "eac_cympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m_lts <- dat_cympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m_lts)
dat_cympo_m_lts[is.na(dat_cympo_m_lts)] <- 0; dat_cympo_m_lts <- dat_cympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_m <- as.data.frame(dat_cympo_m_lts)[1]
out_cympo_m$m_00_05 <- as.numeric(0)
out_cympo_m$m_00_05 <- dat_cympo_m$grocery_00_05_mins
out_cympo_m$m_00_05_lts <- as.numeric(0)
out_cympo_m$m_00_05_lts <- dat_cympo_m$grocery_00_05_mins * (dat_cympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_cympo_m$m_05_10 <- as.numeric(0)
out_cympo_m$m_05_10 <- dat_cympo_m$grocery_05_10_mins
out_cympo_m$m_05_10_lts <- as.numeric(0)
out_cympo_m$m_05_10_lts <- dat_cympo_m$grocery_05_10_mins * (dat_cympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_cympo_m$m_10_15 <- as.numeric(0)
out_cympo_m$m_10_15 <- dat_cympo_m$grocery_10_15_mins
out_cympo_m$m_10_15_lts <- as.numeric(0)
out_cympo_m$m_10_15_lts <- dat_cympo_m$grocery_10_15_mins * (dat_cympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_cympo_m$m_15_20 <- as.numeric(0)
out_cympo_m$m_15_20 <- dat_cympo_m$grocery_15_20_mins
out_cympo_m$m_15_20_lts <- as.numeric(0)
out_cympo_m$m_15_20_lts <- dat_cympo_m$grocery_15_20_mins * (dat_cympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_cympo_m$m_20_25 <- as.numeric(0)
out_cympo_m$m_20_25 <- dat_cympo_m$grocery_20_25_mins
out_cympo_m$m_20_25_lts <- as.numeric(0)
out_cympo_m$m_20_25_lts <- dat_cympo_m$grocery_20_25_mins * (dat_cympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_cympo_m$m_25_30 <- as.numeric(0)
out_cympo_m$m_25_30 <- dat_cympo_m$grocery_25_30_mins
out_cympo_m$m_25_30_lts <- as.numeric(0)
out_cympo_m$m_25_30_lts <- dat_cympo_m$grocery_25_30_mins * (dat_cympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_cympo_m$m_30_35 <- as.numeric(0)
out_cympo_m$m_30_35 <- dat_cympo_m$grocery_30_35_mins
out_cympo_m$m_30_35_lts <- as.numeric(0)
out_cympo_m$m_30_35_lts <- dat_cympo_m$grocery_30_35_mins * (dat_cympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_cympo_m$m_35_40 <- as.numeric(0)
out_cympo_m$m_35_40 <- dat_cympo_m$grocery_35_40_mins
out_cympo_m$m_35_40_lts <- as.numeric(0)
out_cympo_m$m_35_40_lts <- dat_cympo_m$grocery_35_40_mins * (dat_cympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_cympo_m$m_40_45 <- as.numeric(0)
out_cympo_m$m_40_45 <- dat_cympo_m$grocery_40_45_mins
out_cympo_m$m_40_45_lts <- as.numeric(0)
out_cympo_m$m_40_45_lts <- dat_cympo_m$grocery_40_45_mins * (dat_cympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_cympo_m$m_45_50 <- as.numeric(0)
out_cympo_m$m_45_50 <- dat_cympo_m$grocery_45_50_mins
out_cympo_m$m_45_50_lts <- as.numeric(0)
out_cympo_m$m_45_50_lts <- dat_cympo_m$grocery_45_50_mins * (dat_cympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_cympo_m$m_50_55 <- as.numeric(0)
out_cympo_m$m_50_55 <- dat_cympo_m$grocery_50_55_mins
out_cympo_m$m_50_55_lts <- as.numeric(0)
out_cympo_m$m_50_55_lts <- dat_cympo_m$grocery_50_55_mins * (dat_cympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_cympo_m$m_55_60 <- as.numeric(0)
out_cympo_m$m_55_60 <- dat_cympo_m$grocery_55_60_mins
out_cympo_m$m_55_60_lts <- as.numeric(0)
out_cympo_m$m_55_60_lts <- dat_cympo_m$grocery_55_60_mins * (dat_cympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_cympo_m)

# Export accessibility output 
write.dbf(out_cympo_m, (file=paste(dat_dir, "access_eac_markets_cympo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_cympo_s <- read_csv(paste(dat_dir, "saf_cympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s <- dat_cympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s)
dat_cympo_s[is.na(dat_cympo_s)] <- 0; dat_cympo_s <- dat_cympo_s %>% arrange(From)
dat_cympo_s_lts <- read_csv(paste(dat_dir, "saf_cympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_cympo_s_lts <- dat_cympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_s_lts)
dat_cympo_s_lts[is.na(dat_cympo_s_lts)] <- 0; dat_cympo_s_lts <- dat_cympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_s <- as.data.frame(dat_cympo_s_lts)[1]
out_cympo_s$s_00_05 <- as.numeric(0)
out_cympo_s$s_00_05 <- dat_cympo_s$school_00_05_mins
out_cympo_s$s_00_05_lts <- as.numeric(0)
out_cympo_s$s_00_05_lts <- dat_cympo_s$school_00_05_mins * (dat_cympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_cympo_s_lts$`school_00_05_mins_lts2%`/100)
out_cympo_s$s_05_10 <- as.numeric(0)
out_cympo_s$s_05_10 <- dat_cympo_s$school_05_10_mins
out_cympo_s$s_05_10_lts <- as.numeric(0)
out_cympo_s$s_05_10_lts <- dat_cympo_s$school_05_10_mins * (dat_cympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_cympo_s_lts$`school_05_10_mins_lts2%`/100)
out_cympo_s$s_10_15 <- as.numeric(0)
out_cympo_s$s_10_15 <- dat_cympo_s$school_10_15_mins
out_cympo_s$s_10_15_lts <- as.numeric(0)
out_cympo_s$s_10_15_lts <- dat_cympo_s$school_10_15_mins * (dat_cympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_cympo_s_lts$`school_10_15_mins_lts2%`/100)
out_cympo_s$s_15_20 <- as.numeric(0)
out_cympo_s$s_15_20 <- dat_cympo_s$school_15_20_mins
out_cympo_s$s_15_20_lts <- as.numeric(0)
out_cympo_s$s_15_20_lts <- dat_cympo_s$school_15_20_mins * (dat_cympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_cympo_s_lts$`school_15_20_mins_lts2%`/100)
out_cympo_s$s_20_25 <- as.numeric(0)
out_cympo_s$s_20_25 <- dat_cympo_s$school_20_25_mins
out_cympo_s$s_20_25_lts <- as.numeric(0)
out_cympo_s$s_20_25_lts <- dat_cympo_s$school_20_25_mins * (dat_cympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_cympo_s_lts$`school_20_25_mins_lts2%`/100)
out_cympo_s$s_25_30 <- as.numeric(0)
out_cympo_s$s_25_30 <- dat_cympo_s$school_25_30_mins
out_cympo_s$s_25_30_lts <- as.numeric(0)
out_cympo_s$s_25_30_lts <- dat_cympo_s$school_25_30_mins * (dat_cympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_cympo_s_lts$`school_25_30_mins_lts2%`/100)
out_cympo_s$s_30_35 <- as.numeric(0)
out_cympo_s$s_30_35 <- dat_cympo_s$school_30_35_mins
out_cympo_s$s_30_35_lts <- as.numeric(0)
out_cympo_s$s_30_35_lts <- dat_cympo_s$school_30_35_mins * (dat_cympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_cympo_s_lts$`school_30_35_mins_lts2%`/100)
out_cympo_s$s_35_40 <- as.numeric(0)
out_cympo_s$s_35_40 <- dat_cympo_s$school_35_40_mins
out_cympo_s$s_35_40_lts <- as.numeric(0)
out_cympo_s$s_35_40_lts <- dat_cympo_s$school_35_40_mins * (dat_cympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_cympo_s_lts$`school_35_40_mins_lts2%`/100)
out_cympo_s$s_40_45 <- as.numeric(0)
out_cympo_s$s_40_45 <- dat_cympo_s$school_40_45_mins
out_cympo_s$s_40_45_lts <- as.numeric(0)
out_cympo_s$s_40_45_lts <- dat_cympo_s$school_40_45_mins * (dat_cympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_cympo_s_lts$`school_40_45_mins_lts2%`/100)
out_cympo_s$s_45_50 <- as.numeric(0)
out_cympo_s$s_45_50 <- dat_cympo_s$school_45_50_mins
out_cympo_s$s_45_50_lts <- as.numeric(0)
out_cympo_s$s_45_50_lts <- dat_cympo_s$school_45_50_mins * (dat_cympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_cympo_s_lts$`school_45_50_mins_lts2%`/100)
out_cympo_s$s_50_55 <- as.numeric(0)
out_cympo_s$s_50_55 <- dat_cympo_s$school_50_55_mins
out_cympo_s$s_50_55_lts <- as.numeric(0)
out_cympo_s$s_50_55_lts <- dat_cympo_s$school_50_55_mins * (dat_cympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_cympo_s_lts$`school_50_55_mins_lts2%`/100)
out_cympo_s$s_55_60 <- as.numeric(0)
out_cympo_s$s_55_60 <- dat_cympo_s$school_55_60_mins
out_cympo_s$s_55_60_lts <- as.numeric(0)
out_cympo_s$s_55_60_lts <- dat_cympo_s$school_55_60_mins * (dat_cympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_cympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_cympo_s)

# Export accessibility output 
write.dbf(out_cympo_s, (file=paste(dat_dir, "access_saf_schools_cympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_cympo_m <- read_csv(paste(dat_dir, "saf_cympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m <- dat_cympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m)
dat_cympo_m[is.na(dat_cympo_m)] <- 0; dat_cympo_m <- dat_cympo_m %>% arrange(From)
dat_cympo_m_lts <- read_csv(paste(dat_dir, "saf_cympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_cympo_m_lts <- dat_cympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_cympo_m_lts)
dat_cympo_m_lts[is.na(dat_cympo_m_lts)] <- 0; dat_cympo_m_lts <- dat_cympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_cympo_m <- as.data.frame(dat_cympo_m_lts)[1]
out_cympo_m$m_00_05 <- as.numeric(0)
out_cympo_m$m_00_05 <- dat_cympo_m$grocery_00_05_mins
out_cympo_m$m_00_05_lts <- as.numeric(0)
out_cympo_m$m_00_05_lts <- dat_cympo_m$grocery_00_05_mins * (dat_cympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_cympo_m$m_05_10 <- as.numeric(0)
out_cympo_m$m_05_10 <- dat_cympo_m$grocery_05_10_mins
out_cympo_m$m_05_10_lts <- as.numeric(0)
out_cympo_m$m_05_10_lts <- dat_cympo_m$grocery_05_10_mins * (dat_cympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_cympo_m$m_10_15 <- as.numeric(0)
out_cympo_m$m_10_15 <- dat_cympo_m$grocery_10_15_mins
out_cympo_m$m_10_15_lts <- as.numeric(0)
out_cympo_m$m_10_15_lts <- dat_cympo_m$grocery_10_15_mins * (dat_cympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_cympo_m$m_15_20 <- as.numeric(0)
out_cympo_m$m_15_20 <- dat_cympo_m$grocery_15_20_mins
out_cympo_m$m_15_20_lts <- as.numeric(0)
out_cympo_m$m_15_20_lts <- dat_cympo_m$grocery_15_20_mins * (dat_cympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_cympo_m$m_20_25 <- as.numeric(0)
out_cympo_m$m_20_25 <- dat_cympo_m$grocery_20_25_mins
out_cympo_m$m_20_25_lts <- as.numeric(0)
out_cympo_m$m_20_25_lts <- dat_cympo_m$grocery_20_25_mins * (dat_cympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_cympo_m$m_25_30 <- as.numeric(0)
out_cympo_m$m_25_30 <- dat_cympo_m$grocery_25_30_mins
out_cympo_m$m_25_30_lts <- as.numeric(0)
out_cympo_m$m_25_30_lts <- dat_cympo_m$grocery_25_30_mins * (dat_cympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_cympo_m$m_30_35 <- as.numeric(0)
out_cympo_m$m_30_35 <- dat_cympo_m$grocery_30_35_mins
out_cympo_m$m_30_35_lts <- as.numeric(0)
out_cympo_m$m_30_35_lts <- dat_cympo_m$grocery_30_35_mins * (dat_cympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_cympo_m$m_35_40 <- as.numeric(0)
out_cympo_m$m_35_40 <- dat_cympo_m$grocery_35_40_mins
out_cympo_m$m_35_40_lts <- as.numeric(0)
out_cympo_m$m_35_40_lts <- dat_cympo_m$grocery_35_40_mins * (dat_cympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_cympo_m$m_40_45 <- as.numeric(0)
out_cympo_m$m_40_45 <- dat_cympo_m$grocery_40_45_mins
out_cympo_m$m_40_45_lts <- as.numeric(0)
out_cympo_m$m_40_45_lts <- dat_cympo_m$grocery_40_45_mins * (dat_cympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_cympo_m$m_45_50 <- as.numeric(0)
out_cympo_m$m_45_50 <- dat_cympo_m$grocery_45_50_mins
out_cympo_m$m_45_50_lts <- as.numeric(0)
out_cympo_m$m_45_50_lts <- dat_cympo_m$grocery_45_50_mins * (dat_cympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_cympo_m$m_50_55 <- as.numeric(0)
out_cympo_m$m_50_55 <- dat_cympo_m$grocery_50_55_mins
out_cympo_m$m_50_55_lts <- as.numeric(0)
out_cympo_m$m_50_55_lts <- dat_cympo_m$grocery_50_55_mins * (dat_cympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_cympo_m$m_55_60 <- as.numeric(0)
out_cympo_m$m_55_60 <- dat_cympo_m$grocery_55_60_mins
out_cympo_m$m_55_60_lts <- as.numeric(0)
out_cympo_m$m_55_60_lts <- dat_cympo_m$grocery_55_60_mins * (dat_cympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_cympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_cympo_m)

# Export accessibility output 
write.dbf(out_cympo_m, (file=paste(dat_dir, "access_saf_markets_cympo.dbf", sep="_data/_tabular/_outputs/")))

####################
### FMPO/MetroPlan: Flagstaff

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_fmpo_s <- read_csv(paste(dat_dir, "ibc_fmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s <- dat_fmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s)
dat_fmpo_s[is.na(dat_fmpo_s)] <- 0; dat_fmpo_s <- dat_fmpo_s %>% arrange(From)
dat_fmpo_s_lts <- read_csv(paste(dat_dir, "ibc_fmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s_lts <- dat_fmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s_lts)
dat_fmpo_s_lts[is.na(dat_fmpo_s_lts)] <- 0; dat_fmpo_s_lts <- dat_fmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_s <- as.data.frame(dat_fmpo_s_lts)[1]
out_fmpo_s$s_00_05 <- as.numeric(0)
out_fmpo_s$s_00_05 <- dat_fmpo_s$school_00_05_mins
out_fmpo_s$s_00_05_lts <- as.numeric(0)
out_fmpo_s$s_00_05_lts <- dat_fmpo_s$school_00_05_mins * (dat_fmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_fmpo_s$s_05_10 <- as.numeric(0)
out_fmpo_s$s_05_10 <- dat_fmpo_s$school_05_10_mins
out_fmpo_s$s_05_10_lts <- as.numeric(0)
out_fmpo_s$s_05_10_lts <- dat_fmpo_s$school_05_10_mins * (dat_fmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_fmpo_s$s_10_15 <- as.numeric(0)
out_fmpo_s$s_10_15 <- dat_fmpo_s$school_10_15_mins
out_fmpo_s$s_10_15_lts <- as.numeric(0)
out_fmpo_s$s_10_15_lts <- dat_fmpo_s$school_10_15_mins * (dat_fmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_fmpo_s$s_15_20 <- as.numeric(0)
out_fmpo_s$s_15_20 <- dat_fmpo_s$school_15_20_mins
out_fmpo_s$s_15_20_lts <- as.numeric(0)
out_fmpo_s$s_15_20_lts <- dat_fmpo_s$school_15_20_mins * (dat_fmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_fmpo_s$s_20_25 <- as.numeric(0)
out_fmpo_s$s_20_25 <- dat_fmpo_s$school_20_25_mins
out_fmpo_s$s_20_25_lts <- as.numeric(0)
out_fmpo_s$s_20_25_lts <- dat_fmpo_s$school_20_25_mins * (dat_fmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_fmpo_s$s_25_30 <- as.numeric(0)
out_fmpo_s$s_25_30 <- dat_fmpo_s$school_25_30_mins
out_fmpo_s$s_25_30_lts <- as.numeric(0)
out_fmpo_s$s_25_30_lts <- dat_fmpo_s$school_25_30_mins * (dat_fmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_fmpo_s$s_30_35 <- as.numeric(0)
out_fmpo_s$s_30_35 <- dat_fmpo_s$school_30_35_mins
out_fmpo_s$s_30_35_lts <- as.numeric(0)
out_fmpo_s$s_30_35_lts <- dat_fmpo_s$school_30_35_mins * (dat_fmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_fmpo_s$s_35_40 <- as.numeric(0)
out_fmpo_s$s_35_40 <- dat_fmpo_s$school_35_40_mins
out_fmpo_s$s_35_40_lts <- as.numeric(0)
out_fmpo_s$s_35_40_lts <- dat_fmpo_s$school_35_40_mins * (dat_fmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_fmpo_s$s_40_45 <- as.numeric(0)
out_fmpo_s$s_40_45 <- dat_fmpo_s$school_40_45_mins
out_fmpo_s$s_40_45_lts <- as.numeric(0)
out_fmpo_s$s_40_45_lts <- dat_fmpo_s$school_40_45_mins * (dat_fmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_fmpo_s$s_45_50 <- as.numeric(0)
out_fmpo_s$s_45_50 <- dat_fmpo_s$school_45_50_mins
out_fmpo_s$s_45_50_lts <- as.numeric(0)
out_fmpo_s$s_45_50_lts <- dat_fmpo_s$school_45_50_mins * (dat_fmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_fmpo_s$s_50_55 <- as.numeric(0)
out_fmpo_s$s_50_55 <- dat_fmpo_s$school_50_55_mins
out_fmpo_s$s_50_55_lts <- as.numeric(0)
out_fmpo_s$s_50_55_lts <- dat_fmpo_s$school_50_55_mins * (dat_fmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_fmpo_s$s_55_60 <- as.numeric(0)
out_fmpo_s$s_55_60 <- dat_fmpo_s$school_55_60_mins
out_fmpo_s$s_55_60_lts <- as.numeric(0)
out_fmpo_s$s_55_60_lts <- dat_fmpo_s$school_55_60_mins * (dat_fmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_fmpo_s)

# Export accessibility output 
write.dbf(out_fmpo_s, (file=paste(dat_dir, "access_ibc_schools_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_fmpo_m <- read_csv(paste(dat_dir, "ibc_fmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m <- dat_fmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m)
dat_fmpo_m[is.na(dat_fmpo_m)] <- 0; dat_fmpo_m <- dat_fmpo_m %>% arrange(From)
dat_fmpo_m_lts <- read_csv(paste(dat_dir, "ibc_fmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m_lts <- dat_fmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m_lts)
dat_fmpo_m_lts[is.na(dat_fmpo_m_lts)] <- 0; dat_fmpo_m_lts <- dat_fmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_m <- as.data.frame(dat_fmpo_m_lts)[1]
out_fmpo_m$m_00_05 <- as.numeric(0)
out_fmpo_m$m_00_05 <- dat_fmpo_m$grocery_00_05_mins
out_fmpo_m$m_00_05_lts <- as.numeric(0)
out_fmpo_m$m_00_05_lts <- dat_fmpo_m$grocery_00_05_mins * (dat_fmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_fmpo_m$m_05_10 <- as.numeric(0)
out_fmpo_m$m_05_10 <- dat_fmpo_m$grocery_05_10_mins
out_fmpo_m$m_05_10_lts <- as.numeric(0)
out_fmpo_m$m_05_10_lts <- dat_fmpo_m$grocery_05_10_mins * (dat_fmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_fmpo_m$m_10_15 <- as.numeric(0)
out_fmpo_m$m_10_15 <- dat_fmpo_m$grocery_10_15_mins
out_fmpo_m$m_10_15_lts <- as.numeric(0)
out_fmpo_m$m_10_15_lts <- dat_fmpo_m$grocery_10_15_mins * (dat_fmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_fmpo_m$m_15_20 <- as.numeric(0)
out_fmpo_m$m_15_20 <- dat_fmpo_m$grocery_15_20_mins
out_fmpo_m$m_15_20_lts <- as.numeric(0)
out_fmpo_m$m_15_20_lts <- dat_fmpo_m$grocery_15_20_mins * (dat_fmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_fmpo_m$m_20_25 <- as.numeric(0)
out_fmpo_m$m_20_25 <- dat_fmpo_m$grocery_20_25_mins
out_fmpo_m$m_20_25_lts <- as.numeric(0)
out_fmpo_m$m_20_25_lts <- dat_fmpo_m$grocery_20_25_mins * (dat_fmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_fmpo_m$m_25_30 <- as.numeric(0)
out_fmpo_m$m_25_30 <- dat_fmpo_m$grocery_25_30_mins
out_fmpo_m$m_25_30_lts <- as.numeric(0)
out_fmpo_m$m_25_30_lts <- dat_fmpo_m$grocery_25_30_mins * (dat_fmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_fmpo_m$m_30_35 <- as.numeric(0)
out_fmpo_m$m_30_35 <- dat_fmpo_m$grocery_30_35_mins
out_fmpo_m$m_30_35_lts <- as.numeric(0)
out_fmpo_m$m_30_35_lts <- dat_fmpo_m$grocery_30_35_mins * (dat_fmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_fmpo_m$m_35_40 <- as.numeric(0)
out_fmpo_m$m_35_40 <- dat_fmpo_m$grocery_35_40_mins
out_fmpo_m$m_35_40_lts <- as.numeric(0)
out_fmpo_m$m_35_40_lts <- dat_fmpo_m$grocery_35_40_mins * (dat_fmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_fmpo_m$m_40_45 <- as.numeric(0)
out_fmpo_m$m_40_45 <- dat_fmpo_m$grocery_40_45_mins
out_fmpo_m$m_40_45_lts <- as.numeric(0)
out_fmpo_m$m_40_45_lts <- dat_fmpo_m$grocery_40_45_mins * (dat_fmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_fmpo_m$m_45_50 <- as.numeric(0)
out_fmpo_m$m_45_50 <- dat_fmpo_m$grocery_45_50_mins
out_fmpo_m$m_45_50_lts <- as.numeric(0)
out_fmpo_m$m_45_50_lts <- dat_fmpo_m$grocery_45_50_mins * (dat_fmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_fmpo_m$m_50_55 <- as.numeric(0)
out_fmpo_m$m_50_55 <- dat_fmpo_m$grocery_50_55_mins
out_fmpo_m$m_50_55_lts <- as.numeric(0)
out_fmpo_m$m_50_55_lts <- dat_fmpo_m$grocery_50_55_mins * (dat_fmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_fmpo_m$m_55_60 <- as.numeric(0)
out_fmpo_m$m_55_60 <- dat_fmpo_m$grocery_55_60_mins
out_fmpo_m$m_55_60_lts <- as.numeric(0)
out_fmpo_m$m_55_60_lts <- dat_fmpo_m$grocery_55_60_mins * (dat_fmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_fmpo_m)

# Export accessibility output 
write.dbf(out_fmpo_m, (file=paste(dat_dir, "access_ibc_markets_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_fmpo_s <- read_csv(paste(dat_dir, "eac_fmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s <- dat_fmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s)
dat_fmpo_s[is.na(dat_fmpo_s)] <- 0; dat_fmpo_s <- dat_fmpo_s %>% arrange(From)
dat_fmpo_s_lts <- read_csv(paste(dat_dir, "eac_fmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s_lts <- dat_fmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s_lts)
dat_fmpo_s_lts[is.na(dat_fmpo_s_lts)] <- 0; dat_fmpo_s_lts <- dat_fmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_s <- as.data.frame(dat_fmpo_s_lts)[1]
out_fmpo_s$s_00_05 <- as.numeric(0)
out_fmpo_s$s_00_05 <- dat_fmpo_s$school_00_05_mins
out_fmpo_s$s_00_05_lts <- as.numeric(0)
out_fmpo_s$s_00_05_lts <- dat_fmpo_s$school_00_05_mins * (dat_fmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_fmpo_s$s_05_10 <- as.numeric(0)
out_fmpo_s$s_05_10 <- dat_fmpo_s$school_05_10_mins
out_fmpo_s$s_05_10_lts <- as.numeric(0)
out_fmpo_s$s_05_10_lts <- dat_fmpo_s$school_05_10_mins * (dat_fmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_fmpo_s$s_10_15 <- as.numeric(0)
out_fmpo_s$s_10_15 <- dat_fmpo_s$school_10_15_mins
out_fmpo_s$s_10_15_lts <- as.numeric(0)
out_fmpo_s$s_10_15_lts <- dat_fmpo_s$school_10_15_mins * (dat_fmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_fmpo_s$s_15_20 <- as.numeric(0)
out_fmpo_s$s_15_20 <- dat_fmpo_s$school_15_20_mins
out_fmpo_s$s_15_20_lts <- as.numeric(0)
out_fmpo_s$s_15_20_lts <- dat_fmpo_s$school_15_20_mins * (dat_fmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_fmpo_s$s_20_25 <- as.numeric(0)
out_fmpo_s$s_20_25 <- dat_fmpo_s$school_20_25_mins
out_fmpo_s$s_20_25_lts <- as.numeric(0)
out_fmpo_s$s_20_25_lts <- dat_fmpo_s$school_20_25_mins * (dat_fmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_fmpo_s$s_25_30 <- as.numeric(0)
out_fmpo_s$s_25_30 <- dat_fmpo_s$school_25_30_mins
out_fmpo_s$s_25_30_lts <- as.numeric(0)
out_fmpo_s$s_25_30_lts <- dat_fmpo_s$school_25_30_mins * (dat_fmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_fmpo_s$s_30_35 <- as.numeric(0)
out_fmpo_s$s_30_35 <- dat_fmpo_s$school_30_35_mins
out_fmpo_s$s_30_35_lts <- as.numeric(0)
out_fmpo_s$s_30_35_lts <- dat_fmpo_s$school_30_35_mins * (dat_fmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_fmpo_s$s_35_40 <- as.numeric(0)
out_fmpo_s$s_35_40 <- dat_fmpo_s$school_35_40_mins
out_fmpo_s$s_35_40_lts <- as.numeric(0)
out_fmpo_s$s_35_40_lts <- dat_fmpo_s$school_35_40_mins * (dat_fmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_fmpo_s$s_40_45 <- as.numeric(0)
out_fmpo_s$s_40_45 <- dat_fmpo_s$school_40_45_mins
out_fmpo_s$s_40_45_lts <- as.numeric(0)
out_fmpo_s$s_40_45_lts <- dat_fmpo_s$school_40_45_mins * (dat_fmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_fmpo_s$s_45_50 <- as.numeric(0)
out_fmpo_s$s_45_50 <- dat_fmpo_s$school_45_50_mins
out_fmpo_s$s_45_50_lts <- as.numeric(0)
out_fmpo_s$s_45_50_lts <- dat_fmpo_s$school_45_50_mins * (dat_fmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_fmpo_s$s_50_55 <- as.numeric(0)
out_fmpo_s$s_50_55 <- dat_fmpo_s$school_50_55_mins
out_fmpo_s$s_50_55_lts <- as.numeric(0)
out_fmpo_s$s_50_55_lts <- dat_fmpo_s$school_50_55_mins * (dat_fmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_fmpo_s$s_55_60 <- as.numeric(0)
out_fmpo_s$s_55_60 <- dat_fmpo_s$school_55_60_mins
out_fmpo_s$s_55_60_lts <- as.numeric(0)
out_fmpo_s$s_55_60_lts <- dat_fmpo_s$school_55_60_mins * (dat_fmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_fmpo_s)

# Export accessibility output 
write.dbf(out_fmpo_s, (file=paste(dat_dir, "access_eac_schools_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_fmpo_m <- read_csv(paste(dat_dir, "eac_fmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m <- dat_fmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m)
dat_fmpo_m[is.na(dat_fmpo_m)] <- 0; dat_fmpo_m <- dat_fmpo_m %>% arrange(From)
dat_fmpo_m_lts <- read_csv(paste(dat_dir, "eac_fmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m_lts <- dat_fmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m_lts)
dat_fmpo_m_lts[is.na(dat_fmpo_m_lts)] <- 0; dat_fmpo_m_lts <- dat_fmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_m <- as.data.frame(dat_fmpo_m_lts)[1]
out_fmpo_m$m_00_05 <- as.numeric(0)
out_fmpo_m$m_00_05 <- dat_fmpo_m$grocery_00_05_mins
out_fmpo_m$m_00_05_lts <- as.numeric(0)
out_fmpo_m$m_00_05_lts <- dat_fmpo_m$grocery_00_05_mins * (dat_fmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_fmpo_m$m_05_10 <- as.numeric(0)
out_fmpo_m$m_05_10 <- dat_fmpo_m$grocery_05_10_mins
out_fmpo_m$m_05_10_lts <- as.numeric(0)
out_fmpo_m$m_05_10_lts <- dat_fmpo_m$grocery_05_10_mins * (dat_fmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_fmpo_m$m_10_15 <- as.numeric(0)
out_fmpo_m$m_10_15 <- dat_fmpo_m$grocery_10_15_mins
out_fmpo_m$m_10_15_lts <- as.numeric(0)
out_fmpo_m$m_10_15_lts <- dat_fmpo_m$grocery_10_15_mins * (dat_fmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_fmpo_m$m_15_20 <- as.numeric(0)
out_fmpo_m$m_15_20 <- dat_fmpo_m$grocery_15_20_mins
out_fmpo_m$m_15_20_lts <- as.numeric(0)
out_fmpo_m$m_15_20_lts <- dat_fmpo_m$grocery_15_20_mins * (dat_fmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_fmpo_m$m_20_25 <- as.numeric(0)
out_fmpo_m$m_20_25 <- dat_fmpo_m$grocery_20_25_mins
out_fmpo_m$m_20_25_lts <- as.numeric(0)
out_fmpo_m$m_20_25_lts <- dat_fmpo_m$grocery_20_25_mins * (dat_fmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_fmpo_m$m_25_30 <- as.numeric(0)
out_fmpo_m$m_25_30 <- dat_fmpo_m$grocery_25_30_mins
out_fmpo_m$m_25_30_lts <- as.numeric(0)
out_fmpo_m$m_25_30_lts <- dat_fmpo_m$grocery_25_30_mins * (dat_fmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_fmpo_m$m_30_35 <- as.numeric(0)
out_fmpo_m$m_30_35 <- dat_fmpo_m$grocery_30_35_mins
out_fmpo_m$m_30_35_lts <- as.numeric(0)
out_fmpo_m$m_30_35_lts <- dat_fmpo_m$grocery_30_35_mins * (dat_fmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_fmpo_m$m_35_40 <- as.numeric(0)
out_fmpo_m$m_35_40 <- dat_fmpo_m$grocery_35_40_mins
out_fmpo_m$m_35_40_lts <- as.numeric(0)
out_fmpo_m$m_35_40_lts <- dat_fmpo_m$grocery_35_40_mins * (dat_fmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_fmpo_m$m_40_45 <- as.numeric(0)
out_fmpo_m$m_40_45 <- dat_fmpo_m$grocery_40_45_mins
out_fmpo_m$m_40_45_lts <- as.numeric(0)
out_fmpo_m$m_40_45_lts <- dat_fmpo_m$grocery_40_45_mins * (dat_fmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_fmpo_m$m_45_50 <- as.numeric(0)
out_fmpo_m$m_45_50 <- dat_fmpo_m$grocery_45_50_mins
out_fmpo_m$m_45_50_lts <- as.numeric(0)
out_fmpo_m$m_45_50_lts <- dat_fmpo_m$grocery_45_50_mins * (dat_fmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_fmpo_m$m_50_55 <- as.numeric(0)
out_fmpo_m$m_50_55 <- dat_fmpo_m$grocery_50_55_mins
out_fmpo_m$m_50_55_lts <- as.numeric(0)
out_fmpo_m$m_50_55_lts <- dat_fmpo_m$grocery_50_55_mins * (dat_fmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_fmpo_m$m_55_60 <- as.numeric(0)
out_fmpo_m$m_55_60 <- dat_fmpo_m$grocery_55_60_mins
out_fmpo_m$m_55_60_lts <- as.numeric(0)
out_fmpo_m$m_55_60_lts <- dat_fmpo_m$grocery_55_60_mins * (dat_fmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_fmpo_m)

# Export accessibility output 
write.dbf(out_fmpo_m, (file=paste(dat_dir, "access_eac_markets_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_fmpo_s <- read_csv(paste(dat_dir, "saf_fmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s <- dat_fmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s)
dat_fmpo_s[is.na(dat_fmpo_s)] <- 0; dat_fmpo_s <- dat_fmpo_s %>% arrange(From)
dat_fmpo_s_lts <- read_csv(paste(dat_dir, "saf_fmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_fmpo_s_lts <- dat_fmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_s_lts)
dat_fmpo_s_lts[is.na(dat_fmpo_s_lts)] <- 0; dat_fmpo_s_lts <- dat_fmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_s <- as.data.frame(dat_fmpo_s_lts)[1]
out_fmpo_s$s_00_05 <- as.numeric(0)
out_fmpo_s$s_00_05 <- dat_fmpo_s$school_00_05_mins
out_fmpo_s$s_00_05_lts <- as.numeric(0)
out_fmpo_s$s_00_05_lts <- dat_fmpo_s$school_00_05_mins * (dat_fmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_fmpo_s$s_05_10 <- as.numeric(0)
out_fmpo_s$s_05_10 <- dat_fmpo_s$school_05_10_mins
out_fmpo_s$s_05_10_lts <- as.numeric(0)
out_fmpo_s$s_05_10_lts <- dat_fmpo_s$school_05_10_mins * (dat_fmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_fmpo_s$s_10_15 <- as.numeric(0)
out_fmpo_s$s_10_15 <- dat_fmpo_s$school_10_15_mins
out_fmpo_s$s_10_15_lts <- as.numeric(0)
out_fmpo_s$s_10_15_lts <- dat_fmpo_s$school_10_15_mins * (dat_fmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_fmpo_s$s_15_20 <- as.numeric(0)
out_fmpo_s$s_15_20 <- dat_fmpo_s$school_15_20_mins
out_fmpo_s$s_15_20_lts <- as.numeric(0)
out_fmpo_s$s_15_20_lts <- dat_fmpo_s$school_15_20_mins * (dat_fmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_fmpo_s$s_20_25 <- as.numeric(0)
out_fmpo_s$s_20_25 <- dat_fmpo_s$school_20_25_mins
out_fmpo_s$s_20_25_lts <- as.numeric(0)
out_fmpo_s$s_20_25_lts <- dat_fmpo_s$school_20_25_mins * (dat_fmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_fmpo_s$s_25_30 <- as.numeric(0)
out_fmpo_s$s_25_30 <- dat_fmpo_s$school_25_30_mins
out_fmpo_s$s_25_30_lts <- as.numeric(0)
out_fmpo_s$s_25_30_lts <- dat_fmpo_s$school_25_30_mins * (dat_fmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_fmpo_s$s_30_35 <- as.numeric(0)
out_fmpo_s$s_30_35 <- dat_fmpo_s$school_30_35_mins
out_fmpo_s$s_30_35_lts <- as.numeric(0)
out_fmpo_s$s_30_35_lts <- dat_fmpo_s$school_30_35_mins * (dat_fmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_fmpo_s$s_35_40 <- as.numeric(0)
out_fmpo_s$s_35_40 <- dat_fmpo_s$school_35_40_mins
out_fmpo_s$s_35_40_lts <- as.numeric(0)
out_fmpo_s$s_35_40_lts <- dat_fmpo_s$school_35_40_mins * (dat_fmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_fmpo_s$s_40_45 <- as.numeric(0)
out_fmpo_s$s_40_45 <- dat_fmpo_s$school_40_45_mins
out_fmpo_s$s_40_45_lts <- as.numeric(0)
out_fmpo_s$s_40_45_lts <- dat_fmpo_s$school_40_45_mins * (dat_fmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_fmpo_s$s_45_50 <- as.numeric(0)
out_fmpo_s$s_45_50 <- dat_fmpo_s$school_45_50_mins
out_fmpo_s$s_45_50_lts <- as.numeric(0)
out_fmpo_s$s_45_50_lts <- dat_fmpo_s$school_45_50_mins * (dat_fmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_fmpo_s$s_50_55 <- as.numeric(0)
out_fmpo_s$s_50_55 <- dat_fmpo_s$school_50_55_mins
out_fmpo_s$s_50_55_lts <- as.numeric(0)
out_fmpo_s$s_50_55_lts <- dat_fmpo_s$school_50_55_mins * (dat_fmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_fmpo_s$s_55_60 <- as.numeric(0)
out_fmpo_s$s_55_60 <- dat_fmpo_s$school_55_60_mins
out_fmpo_s$s_55_60_lts <- as.numeric(0)
out_fmpo_s$s_55_60_lts <- dat_fmpo_s$school_55_60_mins * (dat_fmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_fmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_fmpo_s)

# Export accessibility output 
write.dbf(out_fmpo_s, (file=paste(dat_dir, "access_saf_schools_fmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_fmpo_m <- read_csv(paste(dat_dir, "saf_fmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m <- dat_fmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m)
dat_fmpo_m[is.na(dat_fmpo_m)] <- 0; dat_fmpo_m <- dat_fmpo_m %>% arrange(From)
dat_fmpo_m_lts <- read_csv(paste(dat_dir, "saf_fmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_fmpo_m_lts <- dat_fmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_fmpo_m_lts)
dat_fmpo_m_lts[is.na(dat_fmpo_m_lts)] <- 0; dat_fmpo_m_lts <- dat_fmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_fmpo_m <- as.data.frame(dat_fmpo_m_lts)[1]
out_fmpo_m$m_00_05 <- as.numeric(0)
out_fmpo_m$m_00_05 <- dat_fmpo_m$grocery_00_05_mins
out_fmpo_m$m_00_05_lts <- as.numeric(0)
out_fmpo_m$m_00_05_lts <- dat_fmpo_m$grocery_00_05_mins * (dat_fmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_fmpo_m$m_05_10 <- as.numeric(0)
out_fmpo_m$m_05_10 <- dat_fmpo_m$grocery_05_10_mins
out_fmpo_m$m_05_10_lts <- as.numeric(0)
out_fmpo_m$m_05_10_lts <- dat_fmpo_m$grocery_05_10_mins * (dat_fmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_fmpo_m$m_10_15 <- as.numeric(0)
out_fmpo_m$m_10_15 <- dat_fmpo_m$grocery_10_15_mins
out_fmpo_m$m_10_15_lts <- as.numeric(0)
out_fmpo_m$m_10_15_lts <- dat_fmpo_m$grocery_10_15_mins * (dat_fmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_fmpo_m$m_15_20 <- as.numeric(0)
out_fmpo_m$m_15_20 <- dat_fmpo_m$grocery_15_20_mins
out_fmpo_m$m_15_20_lts <- as.numeric(0)
out_fmpo_m$m_15_20_lts <- dat_fmpo_m$grocery_15_20_mins * (dat_fmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_fmpo_m$m_20_25 <- as.numeric(0)
out_fmpo_m$m_20_25 <- dat_fmpo_m$grocery_20_25_mins
out_fmpo_m$m_20_25_lts <- as.numeric(0)
out_fmpo_m$m_20_25_lts <- dat_fmpo_m$grocery_20_25_mins * (dat_fmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_fmpo_m$m_25_30 <- as.numeric(0)
out_fmpo_m$m_25_30 <- dat_fmpo_m$grocery_25_30_mins
out_fmpo_m$m_25_30_lts <- as.numeric(0)
out_fmpo_m$m_25_30_lts <- dat_fmpo_m$grocery_25_30_mins * (dat_fmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_fmpo_m$m_30_35 <- as.numeric(0)
out_fmpo_m$m_30_35 <- dat_fmpo_m$grocery_30_35_mins
out_fmpo_m$m_30_35_lts <- as.numeric(0)
out_fmpo_m$m_30_35_lts <- dat_fmpo_m$grocery_30_35_mins * (dat_fmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_fmpo_m$m_35_40 <- as.numeric(0)
out_fmpo_m$m_35_40 <- dat_fmpo_m$grocery_35_40_mins
out_fmpo_m$m_35_40_lts <- as.numeric(0)
out_fmpo_m$m_35_40_lts <- dat_fmpo_m$grocery_35_40_mins * (dat_fmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_fmpo_m$m_40_45 <- as.numeric(0)
out_fmpo_m$m_40_45 <- dat_fmpo_m$grocery_40_45_mins
out_fmpo_m$m_40_45_lts <- as.numeric(0)
out_fmpo_m$m_40_45_lts <- dat_fmpo_m$grocery_40_45_mins * (dat_fmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_fmpo_m$m_45_50 <- as.numeric(0)
out_fmpo_m$m_45_50 <- dat_fmpo_m$grocery_45_50_mins
out_fmpo_m$m_45_50_lts <- as.numeric(0)
out_fmpo_m$m_45_50_lts <- dat_fmpo_m$grocery_45_50_mins * (dat_fmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_fmpo_m$m_50_55 <- as.numeric(0)
out_fmpo_m$m_50_55 <- dat_fmpo_m$grocery_50_55_mins
out_fmpo_m$m_50_55_lts <- as.numeric(0)
out_fmpo_m$m_50_55_lts <- dat_fmpo_m$grocery_50_55_mins * (dat_fmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_fmpo_m$m_55_60 <- as.numeric(0)
out_fmpo_m$m_55_60 <- dat_fmpo_m$grocery_55_60_mins
out_fmpo_m$m_55_60_lts <- as.numeric(0)
out_fmpo_m$m_55_60_lts <- dat_fmpo_m$grocery_55_60_mins * (dat_fmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_fmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_fmpo_m)

# Export accessibility output 
write.dbf(out_fmpo_m, (file=paste(dat_dir, "access_saf_markets_fmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### LHMPO: Lake Havasu

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_lhmpo_s <- read_csv(paste(dat_dir, "ibc_lhmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s <- dat_lhmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s)
dat_lhmpo_s[is.na(dat_lhmpo_s)] <- 0; dat_lhmpo_s <- dat_lhmpo_s %>% arrange(From)
dat_lhmpo_s_lts <- read_csv(paste(dat_dir, "ibc_lhmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s_lts)
dat_lhmpo_s_lts[is.na(dat_lhmpo_s_lts)] <- 0; dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_s <- as.data.frame(dat_lhmpo_s_lts)[1]
out_lhmpo_s$s_00_05 <- as.numeric(0)
out_lhmpo_s$s_00_05 <- dat_lhmpo_s$school_00_05_mins
out_lhmpo_s$s_00_05_lts <- as.numeric(0)
out_lhmpo_s$s_00_05_lts <- dat_lhmpo_s$school_00_05_mins * (dat_lhmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_lhmpo_s$s_05_10 <- as.numeric(0)
out_lhmpo_s$s_05_10 <- dat_lhmpo_s$school_05_10_mins
out_lhmpo_s$s_05_10_lts <- as.numeric(0)
out_lhmpo_s$s_05_10_lts <- dat_lhmpo_s$school_05_10_mins * (dat_lhmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_lhmpo_s$s_10_15 <- as.numeric(0)
out_lhmpo_s$s_10_15 <- dat_lhmpo_s$school_10_15_mins
out_lhmpo_s$s_10_15_lts <- as.numeric(0)
out_lhmpo_s$s_10_15_lts <- dat_lhmpo_s$school_10_15_mins * (dat_lhmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_lhmpo_s$s_15_20 <- as.numeric(0)
out_lhmpo_s$s_15_20 <- dat_lhmpo_s$school_15_20_mins
out_lhmpo_s$s_15_20_lts <- as.numeric(0)
out_lhmpo_s$s_15_20_lts <- dat_lhmpo_s$school_15_20_mins * (dat_lhmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_lhmpo_s$s_20_25 <- as.numeric(0)
out_lhmpo_s$s_20_25 <- dat_lhmpo_s$school_20_25_mins
out_lhmpo_s$s_20_25_lts <- as.numeric(0)
out_lhmpo_s$s_20_25_lts <- dat_lhmpo_s$school_20_25_mins * (dat_lhmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_lhmpo_s$s_25_30 <- as.numeric(0)
out_lhmpo_s$s_25_30 <- dat_lhmpo_s$school_25_30_mins
out_lhmpo_s$s_25_30_lts <- as.numeric(0)
out_lhmpo_s$s_25_30_lts <- dat_lhmpo_s$school_25_30_mins * (dat_lhmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_lhmpo_s$s_30_35 <- as.numeric(0)
out_lhmpo_s$s_30_35 <- dat_lhmpo_s$school_30_35_mins
out_lhmpo_s$s_30_35_lts <- as.numeric(0)
out_lhmpo_s$s_30_35_lts <- dat_lhmpo_s$school_30_35_mins * (dat_lhmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_lhmpo_s$s_35_40 <- as.numeric(0)
out_lhmpo_s$s_35_40 <- dat_lhmpo_s$school_35_40_mins
out_lhmpo_s$s_35_40_lts <- as.numeric(0)
out_lhmpo_s$s_35_40_lts <- dat_lhmpo_s$school_35_40_mins * (dat_lhmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_lhmpo_s$s_40_45 <- as.numeric(0)
out_lhmpo_s$s_40_45 <- dat_lhmpo_s$school_40_45_mins
out_lhmpo_s$s_40_45_lts <- as.numeric(0)
out_lhmpo_s$s_40_45_lts <- dat_lhmpo_s$school_40_45_mins * (dat_lhmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_lhmpo_s$s_45_50 <- as.numeric(0)
out_lhmpo_s$s_45_50 <- dat_lhmpo_s$school_45_50_mins
out_lhmpo_s$s_45_50_lts <- as.numeric(0)
out_lhmpo_s$s_45_50_lts <- dat_lhmpo_s$school_45_50_mins * (dat_lhmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_lhmpo_s$s_50_55 <- as.numeric(0)
out_lhmpo_s$s_50_55 <- dat_lhmpo_s$school_50_55_mins
out_lhmpo_s$s_50_55_lts <- as.numeric(0)
out_lhmpo_s$s_50_55_lts <- dat_lhmpo_s$school_50_55_mins * (dat_lhmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_lhmpo_s$s_55_60 <- as.numeric(0)
out_lhmpo_s$s_55_60 <- dat_lhmpo_s$school_55_60_mins
out_lhmpo_s$s_55_60_lts <- as.numeric(0)
out_lhmpo_s$s_55_60_lts <- dat_lhmpo_s$school_55_60_mins * (dat_lhmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_lhmpo_s)

# Export accessibility output 
write.dbf(out_lhmpo_s, (file=paste(dat_dir, "access_ibc_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_lhmpo_m <- read_csv(paste(dat_dir, "ibc_lhmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m <- dat_lhmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m)
dat_lhmpo_m[is.na(dat_lhmpo_m)] <- 0; dat_lhmpo_m <- dat_lhmpo_m %>% arrange(From)
dat_lhmpo_m_lts <- read_csv(paste(dat_dir, "ibc_lhmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m_lts)
dat_lhmpo_m_lts[is.na(dat_lhmpo_m_lts)] <- 0; dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_m <- as.data.frame(dat_lhmpo_m_lts)[1]
out_lhmpo_m$m_00_05 <- as.numeric(0)
out_lhmpo_m$m_00_05 <- dat_lhmpo_m$grocery_00_05_mins
out_lhmpo_m$m_00_05_lts <- as.numeric(0)
out_lhmpo_m$m_00_05_lts <- dat_lhmpo_m$grocery_00_05_mins * (dat_lhmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_lhmpo_m$m_05_10 <- as.numeric(0)
out_lhmpo_m$m_05_10 <- dat_lhmpo_m$grocery_05_10_mins
out_lhmpo_m$m_05_10_lts <- as.numeric(0)
out_lhmpo_m$m_05_10_lts <- dat_lhmpo_m$grocery_05_10_mins * (dat_lhmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_lhmpo_m$m_10_15 <- as.numeric(0)
out_lhmpo_m$m_10_15 <- dat_lhmpo_m$grocery_10_15_mins
out_lhmpo_m$m_10_15_lts <- as.numeric(0)
out_lhmpo_m$m_10_15_lts <- dat_lhmpo_m$grocery_10_15_mins * (dat_lhmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_lhmpo_m$m_15_20 <- as.numeric(0)
out_lhmpo_m$m_15_20 <- dat_lhmpo_m$grocery_15_20_mins
out_lhmpo_m$m_15_20_lts <- as.numeric(0)
out_lhmpo_m$m_15_20_lts <- dat_lhmpo_m$grocery_15_20_mins * (dat_lhmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_lhmpo_m$m_20_25 <- as.numeric(0)
out_lhmpo_m$m_20_25 <- dat_lhmpo_m$grocery_20_25_mins
out_lhmpo_m$m_20_25_lts <- as.numeric(0)
out_lhmpo_m$m_20_25_lts <- dat_lhmpo_m$grocery_20_25_mins * (dat_lhmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_lhmpo_m$m_25_30 <- as.numeric(0)
out_lhmpo_m$m_25_30 <- dat_lhmpo_m$grocery_25_30_mins
out_lhmpo_m$m_25_30_lts <- as.numeric(0)
out_lhmpo_m$m_25_30_lts <- dat_lhmpo_m$grocery_25_30_mins * (dat_lhmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_lhmpo_m$m_30_35 <- as.numeric(0)
out_lhmpo_m$m_30_35 <- dat_lhmpo_m$grocery_30_35_mins
out_lhmpo_m$m_30_35_lts <- as.numeric(0)
out_lhmpo_m$m_30_35_lts <- dat_lhmpo_m$grocery_30_35_mins * (dat_lhmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_lhmpo_m$m_35_40 <- as.numeric(0)
out_lhmpo_m$m_35_40 <- dat_lhmpo_m$grocery_35_40_mins
out_lhmpo_m$m_35_40_lts <- as.numeric(0)
out_lhmpo_m$m_35_40_lts <- dat_lhmpo_m$grocery_35_40_mins * (dat_lhmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_lhmpo_m$m_40_45 <- as.numeric(0)
out_lhmpo_m$m_40_45 <- dat_lhmpo_m$grocery_40_45_mins
out_lhmpo_m$m_40_45_lts <- as.numeric(0)
out_lhmpo_m$m_40_45_lts <- dat_lhmpo_m$grocery_40_45_mins * (dat_lhmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_lhmpo_m$m_45_50 <- as.numeric(0)
out_lhmpo_m$m_45_50 <- dat_lhmpo_m$grocery_45_50_mins
out_lhmpo_m$m_45_50_lts <- as.numeric(0)
out_lhmpo_m$m_45_50_lts <- dat_lhmpo_m$grocery_45_50_mins * (dat_lhmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_lhmpo_m$m_50_55 <- as.numeric(0)
out_lhmpo_m$m_50_55 <- dat_lhmpo_m$grocery_50_55_mins
out_lhmpo_m$m_50_55_lts <- as.numeric(0)
out_lhmpo_m$m_50_55_lts <- dat_lhmpo_m$grocery_50_55_mins * (dat_lhmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_lhmpo_m$m_55_60 <- as.numeric(0)
out_lhmpo_m$m_55_60 <- dat_lhmpo_m$grocery_55_60_mins
out_lhmpo_m$m_55_60_lts <- as.numeric(0)
out_lhmpo_m$m_55_60_lts <- dat_lhmpo_m$grocery_55_60_mins * (dat_lhmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_lhmpo_m)

# Export accessibility output 
write.dbf(out_lhmpo_m, (file=paste(dat_dir, "access_ibc_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_lhmpo_s <- read_csv(paste(dat_dir, "eac_lhmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s <- dat_lhmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s)
dat_lhmpo_s[is.na(dat_lhmpo_s)] <- 0; dat_lhmpo_s <- dat_lhmpo_s %>% arrange(From)
dat_lhmpo_s_lts <- read_csv(paste(dat_dir, "eac_lhmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s_lts)
dat_lhmpo_s_lts[is.na(dat_lhmpo_s_lts)] <- 0; dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_s <- as.data.frame(dat_lhmpo_s_lts)[1]
out_lhmpo_s$s_00_05 <- as.numeric(0)
out_lhmpo_s$s_00_05 <- dat_lhmpo_s$school_00_05_mins
out_lhmpo_s$s_00_05_lts <- as.numeric(0)
out_lhmpo_s$s_00_05_lts <- dat_lhmpo_s$school_00_05_mins * (dat_lhmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_lhmpo_s$s_05_10 <- as.numeric(0)
out_lhmpo_s$s_05_10 <- dat_lhmpo_s$school_05_10_mins
out_lhmpo_s$s_05_10_lts <- as.numeric(0)
out_lhmpo_s$s_05_10_lts <- dat_lhmpo_s$school_05_10_mins * (dat_lhmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_lhmpo_s$s_10_15 <- as.numeric(0)
out_lhmpo_s$s_10_15 <- dat_lhmpo_s$school_10_15_mins
out_lhmpo_s$s_10_15_lts <- as.numeric(0)
out_lhmpo_s$s_10_15_lts <- dat_lhmpo_s$school_10_15_mins * (dat_lhmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_lhmpo_s$s_15_20 <- as.numeric(0)
out_lhmpo_s$s_15_20 <- dat_lhmpo_s$school_15_20_mins
out_lhmpo_s$s_15_20_lts <- as.numeric(0)
out_lhmpo_s$s_15_20_lts <- dat_lhmpo_s$school_15_20_mins * (dat_lhmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_lhmpo_s$s_20_25 <- as.numeric(0)
out_lhmpo_s$s_20_25 <- dat_lhmpo_s$school_20_25_mins
out_lhmpo_s$s_20_25_lts <- as.numeric(0)
out_lhmpo_s$s_20_25_lts <- dat_lhmpo_s$school_20_25_mins * (dat_lhmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_lhmpo_s$s_25_30 <- as.numeric(0)
out_lhmpo_s$s_25_30 <- dat_lhmpo_s$school_25_30_mins
out_lhmpo_s$s_25_30_lts <- as.numeric(0)
out_lhmpo_s$s_25_30_lts <- dat_lhmpo_s$school_25_30_mins * (dat_lhmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_lhmpo_s$s_30_35 <- as.numeric(0)
out_lhmpo_s$s_30_35 <- dat_lhmpo_s$school_30_35_mins
out_lhmpo_s$s_30_35_lts <- as.numeric(0)
out_lhmpo_s$s_30_35_lts <- dat_lhmpo_s$school_30_35_mins * (dat_lhmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_lhmpo_s$s_35_40 <- as.numeric(0)
out_lhmpo_s$s_35_40 <- dat_lhmpo_s$school_35_40_mins
out_lhmpo_s$s_35_40_lts <- as.numeric(0)
out_lhmpo_s$s_35_40_lts <- dat_lhmpo_s$school_35_40_mins * (dat_lhmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_lhmpo_s$s_40_45 <- as.numeric(0)
out_lhmpo_s$s_40_45 <- dat_lhmpo_s$school_40_45_mins
out_lhmpo_s$s_40_45_lts <- as.numeric(0)
out_lhmpo_s$s_40_45_lts <- dat_lhmpo_s$school_40_45_mins * (dat_lhmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_lhmpo_s$s_45_50 <- as.numeric(0)
out_lhmpo_s$s_45_50 <- dat_lhmpo_s$school_45_50_mins
out_lhmpo_s$s_45_50_lts <- as.numeric(0)
out_lhmpo_s$s_45_50_lts <- dat_lhmpo_s$school_45_50_mins * (dat_lhmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_lhmpo_s$s_50_55 <- as.numeric(0)
out_lhmpo_s$s_50_55 <- dat_lhmpo_s$school_50_55_mins
out_lhmpo_s$s_50_55_lts <- as.numeric(0)
out_lhmpo_s$s_50_55_lts <- dat_lhmpo_s$school_50_55_mins * (dat_lhmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_lhmpo_s$s_55_60 <- as.numeric(0)
out_lhmpo_s$s_55_60 <- dat_lhmpo_s$school_55_60_mins
out_lhmpo_s$s_55_60_lts <- as.numeric(0)
out_lhmpo_s$s_55_60_lts <- dat_lhmpo_s$school_55_60_mins * (dat_lhmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_lhmpo_s)

# Export accessibility output 
write.dbf(out_lhmpo_s, (file=paste(dat_dir, "access_eac_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_lhmpo_m <- read_csv(paste(dat_dir, "eac_lhmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m <- dat_lhmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m)
dat_lhmpo_m[is.na(dat_lhmpo_m)] <- 0; dat_lhmpo_m <- dat_lhmpo_m %>% arrange(From)
dat_lhmpo_m_lts <- read_csv(paste(dat_dir, "eac_lhmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m_lts)
dat_lhmpo_m_lts[is.na(dat_lhmpo_m_lts)] <- 0; dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_m <- as.data.frame(dat_lhmpo_m_lts)[1]
out_lhmpo_m$m_00_05 <- as.numeric(0)
out_lhmpo_m$m_00_05 <- dat_lhmpo_m$grocery_00_05_mins
out_lhmpo_m$m_00_05_lts <- as.numeric(0)
out_lhmpo_m$m_00_05_lts <- dat_lhmpo_m$grocery_00_05_mins * (dat_lhmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_lhmpo_m$m_05_10 <- as.numeric(0)
out_lhmpo_m$m_05_10 <- dat_lhmpo_m$grocery_05_10_mins
out_lhmpo_m$m_05_10_lts <- as.numeric(0)
out_lhmpo_m$m_05_10_lts <- dat_lhmpo_m$grocery_05_10_mins * (dat_lhmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_lhmpo_m$m_10_15 <- as.numeric(0)
out_lhmpo_m$m_10_15 <- dat_lhmpo_m$grocery_10_15_mins
out_lhmpo_m$m_10_15_lts <- as.numeric(0)
out_lhmpo_m$m_10_15_lts <- dat_lhmpo_m$grocery_10_15_mins * (dat_lhmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_lhmpo_m$m_15_20 <- as.numeric(0)
out_lhmpo_m$m_15_20 <- dat_lhmpo_m$grocery_15_20_mins
out_lhmpo_m$m_15_20_lts <- as.numeric(0)
out_lhmpo_m$m_15_20_lts <- dat_lhmpo_m$grocery_15_20_mins * (dat_lhmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_lhmpo_m$m_20_25 <- as.numeric(0)
out_lhmpo_m$m_20_25 <- dat_lhmpo_m$grocery_20_25_mins
out_lhmpo_m$m_20_25_lts <- as.numeric(0)
out_lhmpo_m$m_20_25_lts <- dat_lhmpo_m$grocery_20_25_mins * (dat_lhmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_lhmpo_m$m_25_30 <- as.numeric(0)
out_lhmpo_m$m_25_30 <- dat_lhmpo_m$grocery_25_30_mins
out_lhmpo_m$m_25_30_lts <- as.numeric(0)
out_lhmpo_m$m_25_30_lts <- dat_lhmpo_m$grocery_25_30_mins * (dat_lhmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_lhmpo_m$m_30_35 <- as.numeric(0)
out_lhmpo_m$m_30_35 <- dat_lhmpo_m$grocery_30_35_mins
out_lhmpo_m$m_30_35_lts <- as.numeric(0)
out_lhmpo_m$m_30_35_lts <- dat_lhmpo_m$grocery_30_35_mins * (dat_lhmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_lhmpo_m$m_35_40 <- as.numeric(0)
out_lhmpo_m$m_35_40 <- dat_lhmpo_m$grocery_35_40_mins
out_lhmpo_m$m_35_40_lts <- as.numeric(0)
out_lhmpo_m$m_35_40_lts <- dat_lhmpo_m$grocery_35_40_mins * (dat_lhmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_lhmpo_m$m_40_45 <- as.numeric(0)
out_lhmpo_m$m_40_45 <- dat_lhmpo_m$grocery_40_45_mins
out_lhmpo_m$m_40_45_lts <- as.numeric(0)
out_lhmpo_m$m_40_45_lts <- dat_lhmpo_m$grocery_40_45_mins * (dat_lhmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_lhmpo_m$m_45_50 <- as.numeric(0)
out_lhmpo_m$m_45_50 <- dat_lhmpo_m$grocery_45_50_mins
out_lhmpo_m$m_45_50_lts <- as.numeric(0)
out_lhmpo_m$m_45_50_lts <- dat_lhmpo_m$grocery_45_50_mins * (dat_lhmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_lhmpo_m$m_50_55 <- as.numeric(0)
out_lhmpo_m$m_50_55 <- dat_lhmpo_m$grocery_50_55_mins
out_lhmpo_m$m_50_55_lts <- as.numeric(0)
out_lhmpo_m$m_50_55_lts <- dat_lhmpo_m$grocery_50_55_mins * (dat_lhmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_lhmpo_m$m_55_60 <- as.numeric(0)
out_lhmpo_m$m_55_60 <- dat_lhmpo_m$grocery_55_60_mins
out_lhmpo_m$m_55_60_lts <- as.numeric(0)
out_lhmpo_m$m_55_60_lts <- dat_lhmpo_m$grocery_55_60_mins * (dat_lhmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_lhmpo_m)

# Export accessibility output 
write.dbf(out_lhmpo_m, (file=paste(dat_dir, "access_eac_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_lhmpo_s <- read_csv(paste(dat_dir, "saf_lhmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s <- dat_lhmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s)
dat_lhmpo_s[is.na(dat_lhmpo_s)] <- 0; dat_lhmpo_s <- dat_lhmpo_s %>% arrange(From)
dat_lhmpo_s_lts <- read_csv(paste(dat_dir, "saf_lhmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_s_lts)
dat_lhmpo_s_lts[is.na(dat_lhmpo_s_lts)] <- 0; dat_lhmpo_s_lts <- dat_lhmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_s <- as.data.frame(dat_lhmpo_s_lts)[1]
out_lhmpo_s$s_00_05 <- as.numeric(0)
out_lhmpo_s$s_00_05 <- dat_lhmpo_s$school_00_05_mins
out_lhmpo_s$s_00_05_lts <- as.numeric(0)
out_lhmpo_s$s_00_05_lts <- dat_lhmpo_s$school_00_05_mins * (dat_lhmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_lhmpo_s$s_05_10 <- as.numeric(0)
out_lhmpo_s$s_05_10 <- dat_lhmpo_s$school_05_10_mins
out_lhmpo_s$s_05_10_lts <- as.numeric(0)
out_lhmpo_s$s_05_10_lts <- dat_lhmpo_s$school_05_10_mins * (dat_lhmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_lhmpo_s$s_10_15 <- as.numeric(0)
out_lhmpo_s$s_10_15 <- dat_lhmpo_s$school_10_15_mins
out_lhmpo_s$s_10_15_lts <- as.numeric(0)
out_lhmpo_s$s_10_15_lts <- dat_lhmpo_s$school_10_15_mins * (dat_lhmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_lhmpo_s$s_15_20 <- as.numeric(0)
out_lhmpo_s$s_15_20 <- dat_lhmpo_s$school_15_20_mins
out_lhmpo_s$s_15_20_lts <- as.numeric(0)
out_lhmpo_s$s_15_20_lts <- dat_lhmpo_s$school_15_20_mins * (dat_lhmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_lhmpo_s$s_20_25 <- as.numeric(0)
out_lhmpo_s$s_20_25 <- dat_lhmpo_s$school_20_25_mins
out_lhmpo_s$s_20_25_lts <- as.numeric(0)
out_lhmpo_s$s_20_25_lts <- dat_lhmpo_s$school_20_25_mins * (dat_lhmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_lhmpo_s$s_25_30 <- as.numeric(0)
out_lhmpo_s$s_25_30 <- dat_lhmpo_s$school_25_30_mins
out_lhmpo_s$s_25_30_lts <- as.numeric(0)
out_lhmpo_s$s_25_30_lts <- dat_lhmpo_s$school_25_30_mins * (dat_lhmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_lhmpo_s$s_30_35 <- as.numeric(0)
out_lhmpo_s$s_30_35 <- dat_lhmpo_s$school_30_35_mins
out_lhmpo_s$s_30_35_lts <- as.numeric(0)
out_lhmpo_s$s_30_35_lts <- dat_lhmpo_s$school_30_35_mins * (dat_lhmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_lhmpo_s$s_35_40 <- as.numeric(0)
out_lhmpo_s$s_35_40 <- dat_lhmpo_s$school_35_40_mins
out_lhmpo_s$s_35_40_lts <- as.numeric(0)
out_lhmpo_s$s_35_40_lts <- dat_lhmpo_s$school_35_40_mins * (dat_lhmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_lhmpo_s$s_40_45 <- as.numeric(0)
out_lhmpo_s$s_40_45 <- dat_lhmpo_s$school_40_45_mins
out_lhmpo_s$s_40_45_lts <- as.numeric(0)
out_lhmpo_s$s_40_45_lts <- dat_lhmpo_s$school_40_45_mins * (dat_lhmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_lhmpo_s$s_45_50 <- as.numeric(0)
out_lhmpo_s$s_45_50 <- dat_lhmpo_s$school_45_50_mins
out_lhmpo_s$s_45_50_lts <- as.numeric(0)
out_lhmpo_s$s_45_50_lts <- dat_lhmpo_s$school_45_50_mins * (dat_lhmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_lhmpo_s$s_50_55 <- as.numeric(0)
out_lhmpo_s$s_50_55 <- dat_lhmpo_s$school_50_55_mins
out_lhmpo_s$s_50_55_lts <- as.numeric(0)
out_lhmpo_s$s_50_55_lts <- dat_lhmpo_s$school_50_55_mins * (dat_lhmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_lhmpo_s$s_55_60 <- as.numeric(0)
out_lhmpo_s$s_55_60 <- dat_lhmpo_s$school_55_60_mins
out_lhmpo_s$s_55_60_lts <- as.numeric(0)
out_lhmpo_s$s_55_60_lts <- dat_lhmpo_s$school_55_60_mins * (dat_lhmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_lhmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_lhmpo_s)

# Export accessibility output 
write.dbf(out_lhmpo_s, (file=paste(dat_dir, "access_saf_schools_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_lhmpo_m <- read_csv(paste(dat_dir, "saf_lhmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m <- dat_lhmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m)
dat_lhmpo_m[is.na(dat_lhmpo_m)] <- 0; dat_lhmpo_m <- dat_lhmpo_m %>% arrange(From)
dat_lhmpo_m_lts <- read_csv(paste(dat_dir, "saf_lhmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_lhmpo_m_lts)
dat_lhmpo_m_lts[is.na(dat_lhmpo_m_lts)] <- 0; dat_lhmpo_m_lts <- dat_lhmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_lhmpo_m <- as.data.frame(dat_lhmpo_m_lts)[1]
out_lhmpo_m$m_00_05 <- as.numeric(0)
out_lhmpo_m$m_00_05 <- dat_lhmpo_m$grocery_00_05_mins
out_lhmpo_m$m_00_05_lts <- as.numeric(0)
out_lhmpo_m$m_00_05_lts <- dat_lhmpo_m$grocery_00_05_mins * (dat_lhmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_lhmpo_m$m_05_10 <- as.numeric(0)
out_lhmpo_m$m_05_10 <- dat_lhmpo_m$grocery_05_10_mins
out_lhmpo_m$m_05_10_lts <- as.numeric(0)
out_lhmpo_m$m_05_10_lts <- dat_lhmpo_m$grocery_05_10_mins * (dat_lhmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_lhmpo_m$m_10_15 <- as.numeric(0)
out_lhmpo_m$m_10_15 <- dat_lhmpo_m$grocery_10_15_mins
out_lhmpo_m$m_10_15_lts <- as.numeric(0)
out_lhmpo_m$m_10_15_lts <- dat_lhmpo_m$grocery_10_15_mins * (dat_lhmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_lhmpo_m$m_15_20 <- as.numeric(0)
out_lhmpo_m$m_15_20 <- dat_lhmpo_m$grocery_15_20_mins
out_lhmpo_m$m_15_20_lts <- as.numeric(0)
out_lhmpo_m$m_15_20_lts <- dat_lhmpo_m$grocery_15_20_mins * (dat_lhmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_lhmpo_m$m_20_25 <- as.numeric(0)
out_lhmpo_m$m_20_25 <- dat_lhmpo_m$grocery_20_25_mins
out_lhmpo_m$m_20_25_lts <- as.numeric(0)
out_lhmpo_m$m_20_25_lts <- dat_lhmpo_m$grocery_20_25_mins * (dat_lhmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_lhmpo_m$m_25_30 <- as.numeric(0)
out_lhmpo_m$m_25_30 <- dat_lhmpo_m$grocery_25_30_mins
out_lhmpo_m$m_25_30_lts <- as.numeric(0)
out_lhmpo_m$m_25_30_lts <- dat_lhmpo_m$grocery_25_30_mins * (dat_lhmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_lhmpo_m$m_30_35 <- as.numeric(0)
out_lhmpo_m$m_30_35 <- dat_lhmpo_m$grocery_30_35_mins
out_lhmpo_m$m_30_35_lts <- as.numeric(0)
out_lhmpo_m$m_30_35_lts <- dat_lhmpo_m$grocery_30_35_mins * (dat_lhmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_lhmpo_m$m_35_40 <- as.numeric(0)
out_lhmpo_m$m_35_40 <- dat_lhmpo_m$grocery_35_40_mins
out_lhmpo_m$m_35_40_lts <- as.numeric(0)
out_lhmpo_m$m_35_40_lts <- dat_lhmpo_m$grocery_35_40_mins * (dat_lhmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_lhmpo_m$m_40_45 <- as.numeric(0)
out_lhmpo_m$m_40_45 <- dat_lhmpo_m$grocery_40_45_mins
out_lhmpo_m$m_40_45_lts <- as.numeric(0)
out_lhmpo_m$m_40_45_lts <- dat_lhmpo_m$grocery_40_45_mins * (dat_lhmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_lhmpo_m$m_45_50 <- as.numeric(0)
out_lhmpo_m$m_45_50 <- dat_lhmpo_m$grocery_45_50_mins
out_lhmpo_m$m_45_50_lts <- as.numeric(0)
out_lhmpo_m$m_45_50_lts <- dat_lhmpo_m$grocery_45_50_mins * (dat_lhmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_lhmpo_m$m_50_55 <- as.numeric(0)
out_lhmpo_m$m_50_55 <- dat_lhmpo_m$grocery_50_55_mins
out_lhmpo_m$m_50_55_lts <- as.numeric(0)
out_lhmpo_m$m_50_55_lts <- dat_lhmpo_m$grocery_50_55_mins * (dat_lhmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_lhmpo_m$m_55_60 <- as.numeric(0)
out_lhmpo_m$m_55_60 <- dat_lhmpo_m$grocery_55_60_mins
out_lhmpo_m$m_55_60_lts <- as.numeric(0)
out_lhmpo_m$m_55_60_lts <- dat_lhmpo_m$grocery_55_60_mins * (dat_lhmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_lhmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_lhmpo_m)

# Export accessibility output 
write.dbf(out_lhmpo_m, (file=paste(dat_dir, "access_saf_markets_lhmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### MAG: Phoenix

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_mag_s <- read_csv(paste(dat_dir, "ibc_mag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s <- dat_mag_s %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s)
dat_mag_s[is.na(dat_mag_s)] <- 0; dat_mag_s <- dat_mag_s %>% arrange(From)
dat_mag_s_lts <- read_csv(paste(dat_dir, "ibc_mag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s_lts <- dat_mag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s_lts)
dat_mag_s_lts[is.na(dat_mag_s_lts)] <- 0; dat_mag_s_lts <- dat_mag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_s <- as.data.frame(dat_mag_s_lts)[1]
out_mag_s$s_00_05 <- as.numeric(0)
out_mag_s$s_00_05 <- dat_mag_s$school_00_05_mins
out_mag_s$s_00_05_lts <- as.numeric(0)
out_mag_s$s_00_05_lts <- dat_mag_s$school_00_05_mins * (dat_mag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_mag_s_lts$`school_00_05_mins_lts2%`/100)
out_mag_s$s_05_10 <- as.numeric(0)
out_mag_s$s_05_10 <- dat_mag_s$school_05_10_mins
out_mag_s$s_05_10_lts <- as.numeric(0)
out_mag_s$s_05_10_lts <- dat_mag_s$school_05_10_mins * (dat_mag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_mag_s_lts$`school_05_10_mins_lts2%`/100)
out_mag_s$s_10_15 <- as.numeric(0)
out_mag_s$s_10_15 <- dat_mag_s$school_10_15_mins
out_mag_s$s_10_15_lts <- as.numeric(0)
out_mag_s$s_10_15_lts <- dat_mag_s$school_10_15_mins * (dat_mag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_mag_s_lts$`school_10_15_mins_lts2%`/100)
out_mag_s$s_15_20 <- as.numeric(0)
out_mag_s$s_15_20 <- dat_mag_s$school_15_20_mins
out_mag_s$s_15_20_lts <- as.numeric(0)
out_mag_s$s_15_20_lts <- dat_mag_s$school_15_20_mins * (dat_mag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_mag_s_lts$`school_15_20_mins_lts2%`/100)
out_mag_s$s_20_25 <- as.numeric(0)
out_mag_s$s_20_25 <- dat_mag_s$school_20_25_mins
out_mag_s$s_20_25_lts <- as.numeric(0)
out_mag_s$s_20_25_lts <- dat_mag_s$school_20_25_mins * (dat_mag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_mag_s_lts$`school_20_25_mins_lts2%`/100)
out_mag_s$s_25_30 <- as.numeric(0)
out_mag_s$s_25_30 <- dat_mag_s$school_25_30_mins
out_mag_s$s_25_30_lts <- as.numeric(0)
out_mag_s$s_25_30_lts <- dat_mag_s$school_25_30_mins * (dat_mag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_mag_s_lts$`school_25_30_mins_lts2%`/100)
out_mag_s$s_30_35 <- as.numeric(0)
out_mag_s$s_30_35 <- dat_mag_s$school_30_35_mins
out_mag_s$s_30_35_lts <- as.numeric(0)
out_mag_s$s_30_35_lts <- dat_mag_s$school_30_35_mins * (dat_mag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_mag_s_lts$`school_30_35_mins_lts2%`/100)
out_mag_s$s_35_40 <- as.numeric(0)
out_mag_s$s_35_40 <- dat_mag_s$school_35_40_mins
out_mag_s$s_35_40_lts <- as.numeric(0)
out_mag_s$s_35_40_lts <- dat_mag_s$school_35_40_mins * (dat_mag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_mag_s_lts$`school_35_40_mins_lts2%`/100)
out_mag_s$s_40_45 <- as.numeric(0)
out_mag_s$s_40_45 <- dat_mag_s$school_40_45_mins
out_mag_s$s_40_45_lts <- as.numeric(0)
out_mag_s$s_40_45_lts <- dat_mag_s$school_40_45_mins * (dat_mag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_mag_s_lts$`school_40_45_mins_lts2%`/100)
out_mag_s$s_45_50 <- as.numeric(0)
out_mag_s$s_45_50 <- dat_mag_s$school_45_50_mins
out_mag_s$s_45_50_lts <- as.numeric(0)
out_mag_s$s_45_50_lts <- dat_mag_s$school_45_50_mins * (dat_mag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_mag_s_lts$`school_45_50_mins_lts2%`/100)
out_mag_s$s_50_55 <- as.numeric(0)
out_mag_s$s_50_55 <- dat_mag_s$school_50_55_mins
out_mag_s$s_50_55_lts <- as.numeric(0)
out_mag_s$s_50_55_lts <- dat_mag_s$school_50_55_mins * (dat_mag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_mag_s_lts$`school_50_55_mins_lts2%`/100)
out_mag_s$s_55_60 <- as.numeric(0)
out_mag_s$s_55_60 <- dat_mag_s$school_55_60_mins
out_mag_s$s_55_60_lts <- as.numeric(0)
out_mag_s$s_55_60_lts <- dat_mag_s$school_55_60_mins * (dat_mag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_mag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_mag_s)

# Export accessibility output 
write.dbf(out_mag_s, (file=paste(dat_dir, "access_ibc_schools_mag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_mag_m <- read_csv(paste(dat_dir, "ibc_mag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m <- dat_mag_m %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m)
dat_mag_m[is.na(dat_mag_m)] <- 0; dat_mag_m <- dat_mag_m %>% arrange(From)
dat_mag_m_lts <- read_csv(paste(dat_dir, "ibc_mag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m_lts <- dat_mag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m_lts)
dat_mag_m_lts[is.na(dat_mag_m_lts)] <- 0; dat_mag_m_lts <- dat_mag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_m <- as.data.frame(dat_mag_m_lts)[1]
out_mag_m$m_00_05 <- as.numeric(0)
out_mag_m$m_00_05 <- dat_mag_m$grocery_00_05_mins
out_mag_m$m_00_05_lts <- as.numeric(0)
out_mag_m$m_00_05_lts <- dat_mag_m$grocery_00_05_mins * (dat_mag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_mag_m$m_05_10 <- as.numeric(0)
out_mag_m$m_05_10 <- dat_mag_m$grocery_05_10_mins
out_mag_m$m_05_10_lts <- as.numeric(0)
out_mag_m$m_05_10_lts <- dat_mag_m$grocery_05_10_mins * (dat_mag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_mag_m$m_10_15 <- as.numeric(0)
out_mag_m$m_10_15 <- dat_mag_m$grocery_10_15_mins
out_mag_m$m_10_15_lts <- as.numeric(0)
out_mag_m$m_10_15_lts <- dat_mag_m$grocery_10_15_mins * (dat_mag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_mag_m$m_15_20 <- as.numeric(0)
out_mag_m$m_15_20 <- dat_mag_m$grocery_15_20_mins
out_mag_m$m_15_20_lts <- as.numeric(0)
out_mag_m$m_15_20_lts <- dat_mag_m$grocery_15_20_mins * (dat_mag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_mag_m$m_20_25 <- as.numeric(0)
out_mag_m$m_20_25 <- dat_mag_m$grocery_20_25_mins
out_mag_m$m_20_25_lts <- as.numeric(0)
out_mag_m$m_20_25_lts <- dat_mag_m$grocery_20_25_mins * (dat_mag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_mag_m$m_25_30 <- as.numeric(0)
out_mag_m$m_25_30 <- dat_mag_m$grocery_25_30_mins
out_mag_m$m_25_30_lts <- as.numeric(0)
out_mag_m$m_25_30_lts <- dat_mag_m$grocery_25_30_mins * (dat_mag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_mag_m$m_30_35 <- as.numeric(0)
out_mag_m$m_30_35 <- dat_mag_m$grocery_30_35_mins
out_mag_m$m_30_35_lts <- as.numeric(0)
out_mag_m$m_30_35_lts <- dat_mag_m$grocery_30_35_mins * (dat_mag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_mag_m$m_35_40 <- as.numeric(0)
out_mag_m$m_35_40 <- dat_mag_m$grocery_35_40_mins
out_mag_m$m_35_40_lts <- as.numeric(0)
out_mag_m$m_35_40_lts <- dat_mag_m$grocery_35_40_mins * (dat_mag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_mag_m$m_40_45 <- as.numeric(0)
out_mag_m$m_40_45 <- dat_mag_m$grocery_40_45_mins
out_mag_m$m_40_45_lts <- as.numeric(0)
out_mag_m$m_40_45_lts <- dat_mag_m$grocery_40_45_mins * (dat_mag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_mag_m$m_45_50 <- as.numeric(0)
out_mag_m$m_45_50 <- dat_mag_m$grocery_45_50_mins
out_mag_m$m_45_50_lts <- as.numeric(0)
out_mag_m$m_45_50_lts <- dat_mag_m$grocery_45_50_mins * (dat_mag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_mag_m$m_50_55 <- as.numeric(0)
out_mag_m$m_50_55 <- dat_mag_m$grocery_50_55_mins
out_mag_m$m_50_55_lts <- as.numeric(0)
out_mag_m$m_50_55_lts <- dat_mag_m$grocery_50_55_mins * (dat_mag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_mag_m$m_55_60 <- as.numeric(0)
out_mag_m$m_55_60 <- dat_mag_m$grocery_55_60_mins
out_mag_m$m_55_60_lts <- as.numeric(0)
out_mag_m$m_55_60_lts <- dat_mag_m$grocery_55_60_mins * (dat_mag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_mag_m)

# Export accessibility output 
write.dbf(out_mag_m, (file=paste(dat_dir, "access_ibc_markets_mag.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_mag_s <- read_csv(paste(dat_dir, "eac_mag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s <- dat_mag_s %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s)
dat_mag_s[is.na(dat_mag_s)] <- 0; dat_mag_s <- dat_mag_s %>% arrange(From)
dat_mag_s_lts <- read_csv(paste(dat_dir, "eac_mag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s_lts <- dat_mag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s_lts)
dat_mag_s_lts[is.na(dat_mag_s_lts)] <- 0; dat_mag_s_lts <- dat_mag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_s <- as.data.frame(dat_mag_s_lts)[1]
out_mag_s$s_00_05 <- as.numeric(0)
out_mag_s$s_00_05 <- dat_mag_s$school_00_05_mins
out_mag_s$s_00_05_lts <- as.numeric(0)
out_mag_s$s_00_05_lts <- dat_mag_s$school_00_05_mins * (dat_mag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_mag_s_lts$`school_00_05_mins_lts2%`/100)
out_mag_s$s_05_10 <- as.numeric(0)
out_mag_s$s_05_10 <- dat_mag_s$school_05_10_mins
out_mag_s$s_05_10_lts <- as.numeric(0)
out_mag_s$s_05_10_lts <- dat_mag_s$school_05_10_mins * (dat_mag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_mag_s_lts$`school_05_10_mins_lts2%`/100)
out_mag_s$s_10_15 <- as.numeric(0)
out_mag_s$s_10_15 <- dat_mag_s$school_10_15_mins
out_mag_s$s_10_15_lts <- as.numeric(0)
out_mag_s$s_10_15_lts <- dat_mag_s$school_10_15_mins * (dat_mag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_mag_s_lts$`school_10_15_mins_lts2%`/100)
out_mag_s$s_15_20 <- as.numeric(0)
out_mag_s$s_15_20 <- dat_mag_s$school_15_20_mins
out_mag_s$s_15_20_lts <- as.numeric(0)
out_mag_s$s_15_20_lts <- dat_mag_s$school_15_20_mins * (dat_mag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_mag_s_lts$`school_15_20_mins_lts2%`/100)
out_mag_s$s_20_25 <- as.numeric(0)
out_mag_s$s_20_25 <- dat_mag_s$school_20_25_mins
out_mag_s$s_20_25_lts <- as.numeric(0)
out_mag_s$s_20_25_lts <- dat_mag_s$school_20_25_mins * (dat_mag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_mag_s_lts$`school_20_25_mins_lts2%`/100)
out_mag_s$s_25_30 <- as.numeric(0)
out_mag_s$s_25_30 <- dat_mag_s$school_25_30_mins
out_mag_s$s_25_30_lts <- as.numeric(0)
out_mag_s$s_25_30_lts <- dat_mag_s$school_25_30_mins * (dat_mag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_mag_s_lts$`school_25_30_mins_lts2%`/100)
out_mag_s$s_30_35 <- as.numeric(0)
out_mag_s$s_30_35 <- dat_mag_s$school_30_35_mins
out_mag_s$s_30_35_lts <- as.numeric(0)
out_mag_s$s_30_35_lts <- dat_mag_s$school_30_35_mins * (dat_mag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_mag_s_lts$`school_30_35_mins_lts2%`/100)
out_mag_s$s_35_40 <- as.numeric(0)
out_mag_s$s_35_40 <- dat_mag_s$school_35_40_mins
out_mag_s$s_35_40_lts <- as.numeric(0)
out_mag_s$s_35_40_lts <- dat_mag_s$school_35_40_mins * (dat_mag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_mag_s_lts$`school_35_40_mins_lts2%`/100)
out_mag_s$s_40_45 <- as.numeric(0)
out_mag_s$s_40_45 <- dat_mag_s$school_40_45_mins
out_mag_s$s_40_45_lts <- as.numeric(0)
out_mag_s$s_40_45_lts <- dat_mag_s$school_40_45_mins * (dat_mag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_mag_s_lts$`school_40_45_mins_lts2%`/100)
out_mag_s$s_45_50 <- as.numeric(0)
out_mag_s$s_45_50 <- dat_mag_s$school_45_50_mins
out_mag_s$s_45_50_lts <- as.numeric(0)
out_mag_s$s_45_50_lts <- dat_mag_s$school_45_50_mins * (dat_mag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_mag_s_lts$`school_45_50_mins_lts2%`/100)
out_mag_s$s_50_55 <- as.numeric(0)
out_mag_s$s_50_55 <- dat_mag_s$school_50_55_mins
out_mag_s$s_50_55_lts <- as.numeric(0)
out_mag_s$s_50_55_lts <- dat_mag_s$school_50_55_mins * (dat_mag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_mag_s_lts$`school_50_55_mins_lts2%`/100)
out_mag_s$s_55_60 <- as.numeric(0)
out_mag_s$s_55_60 <- dat_mag_s$school_55_60_mins
out_mag_s$s_55_60_lts <- as.numeric(0)
out_mag_s$s_55_60_lts <- dat_mag_s$school_55_60_mins * (dat_mag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_mag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_mag_s)

# Export accessibility output 
write.dbf(out_mag_s, (file=paste(dat_dir, "access_eac_schools_mag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_mag_m <- read_csv(paste(dat_dir, "eac_mag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m <- dat_mag_m %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m)
dat_mag_m[is.na(dat_mag_m)] <- 0; dat_mag_m <- dat_mag_m %>% arrange(From)
dat_mag_m_lts <- read_csv(paste(dat_dir, "eac_mag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m_lts <- dat_mag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m_lts)
dat_mag_m_lts[is.na(dat_mag_m_lts)] <- 0; dat_mag_m_lts <- dat_mag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_m <- as.data.frame(dat_mag_m_lts)[1]
out_mag_m$m_00_05 <- as.numeric(0)
out_mag_m$m_00_05 <- dat_mag_m$grocery_00_05_mins
out_mag_m$m_00_05_lts <- as.numeric(0)
out_mag_m$m_00_05_lts <- dat_mag_m$grocery_00_05_mins * (dat_mag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_mag_m$m_05_10 <- as.numeric(0)
out_mag_m$m_05_10 <- dat_mag_m$grocery_05_10_mins
out_mag_m$m_05_10_lts <- as.numeric(0)
out_mag_m$m_05_10_lts <- dat_mag_m$grocery_05_10_mins * (dat_mag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_mag_m$m_10_15 <- as.numeric(0)
out_mag_m$m_10_15 <- dat_mag_m$grocery_10_15_mins
out_mag_m$m_10_15_lts <- as.numeric(0)
out_mag_m$m_10_15_lts <- dat_mag_m$grocery_10_15_mins * (dat_mag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_mag_m$m_15_20 <- as.numeric(0)
out_mag_m$m_15_20 <- dat_mag_m$grocery_15_20_mins
out_mag_m$m_15_20_lts <- as.numeric(0)
out_mag_m$m_15_20_lts <- dat_mag_m$grocery_15_20_mins * (dat_mag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_mag_m$m_20_25 <- as.numeric(0)
out_mag_m$m_20_25 <- dat_mag_m$grocery_20_25_mins
out_mag_m$m_20_25_lts <- as.numeric(0)
out_mag_m$m_20_25_lts <- dat_mag_m$grocery_20_25_mins * (dat_mag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_mag_m$m_25_30 <- as.numeric(0)
out_mag_m$m_25_30 <- dat_mag_m$grocery_25_30_mins
out_mag_m$m_25_30_lts <- as.numeric(0)
out_mag_m$m_25_30_lts <- dat_mag_m$grocery_25_30_mins * (dat_mag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_mag_m$m_30_35 <- as.numeric(0)
out_mag_m$m_30_35 <- dat_mag_m$grocery_30_35_mins
out_mag_m$m_30_35_lts <- as.numeric(0)
out_mag_m$m_30_35_lts <- dat_mag_m$grocery_30_35_mins * (dat_mag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_mag_m$m_35_40 <- as.numeric(0)
out_mag_m$m_35_40 <- dat_mag_m$grocery_35_40_mins
out_mag_m$m_35_40_lts <- as.numeric(0)
out_mag_m$m_35_40_lts <- dat_mag_m$grocery_35_40_mins * (dat_mag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_mag_m$m_40_45 <- as.numeric(0)
out_mag_m$m_40_45 <- dat_mag_m$grocery_40_45_mins
out_mag_m$m_40_45_lts <- as.numeric(0)
out_mag_m$m_40_45_lts <- dat_mag_m$grocery_40_45_mins * (dat_mag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_mag_m$m_45_50 <- as.numeric(0)
out_mag_m$m_45_50 <- dat_mag_m$grocery_45_50_mins
out_mag_m$m_45_50_lts <- as.numeric(0)
out_mag_m$m_45_50_lts <- dat_mag_m$grocery_45_50_mins * (dat_mag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_mag_m$m_50_55 <- as.numeric(0)
out_mag_m$m_50_55 <- dat_mag_m$grocery_50_55_mins
out_mag_m$m_50_55_lts <- as.numeric(0)
out_mag_m$m_50_55_lts <- dat_mag_m$grocery_50_55_mins * (dat_mag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_mag_m$m_55_60 <- as.numeric(0)
out_mag_m$m_55_60 <- dat_mag_m$grocery_55_60_mins
out_mag_m$m_55_60_lts <- as.numeric(0)
out_mag_m$m_55_60_lts <- dat_mag_m$grocery_55_60_mins * (dat_mag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_mag_m)

# Export accessibility output 
write.dbf(out_mag_m, (file=paste(dat_dir, "access_eac_markets_mag.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_mag_s <- read_csv(paste(dat_dir, "saf_mag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s <- dat_mag_s %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s)
dat_mag_s[is.na(dat_mag_s)] <- 0; dat_mag_s <- dat_mag_s %>% arrange(From)
dat_mag_s_lts <- read_csv(paste(dat_dir, "saf_mag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_mag_s_lts <- dat_mag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_s_lts)
dat_mag_s_lts[is.na(dat_mag_s_lts)] <- 0; dat_mag_s_lts <- dat_mag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_s <- as.data.frame(dat_mag_s_lts)[1]
out_mag_s$s_00_05 <- as.numeric(0)
out_mag_s$s_00_05 <- dat_mag_s$school_00_05_mins
out_mag_s$s_00_05_lts <- as.numeric(0)
out_mag_s$s_00_05_lts <- dat_mag_s$school_00_05_mins * (dat_mag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_mag_s_lts$`school_00_05_mins_lts2%`/100)
out_mag_s$s_05_10 <- as.numeric(0)
out_mag_s$s_05_10 <- dat_mag_s$school_05_10_mins
out_mag_s$s_05_10_lts <- as.numeric(0)
out_mag_s$s_05_10_lts <- dat_mag_s$school_05_10_mins * (dat_mag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_mag_s_lts$`school_05_10_mins_lts2%`/100)
out_mag_s$s_10_15 <- as.numeric(0)
out_mag_s$s_10_15 <- dat_mag_s$school_10_15_mins
out_mag_s$s_10_15_lts <- as.numeric(0)
out_mag_s$s_10_15_lts <- dat_mag_s$school_10_15_mins * (dat_mag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_mag_s_lts$`school_10_15_mins_lts2%`/100)
out_mag_s$s_15_20 <- as.numeric(0)
out_mag_s$s_15_20 <- dat_mag_s$school_15_20_mins
out_mag_s$s_15_20_lts <- as.numeric(0)
out_mag_s$s_15_20_lts <- dat_mag_s$school_15_20_mins * (dat_mag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_mag_s_lts$`school_15_20_mins_lts2%`/100)
out_mag_s$s_20_25 <- as.numeric(0)
out_mag_s$s_20_25 <- dat_mag_s$school_20_25_mins
out_mag_s$s_20_25_lts <- as.numeric(0)
out_mag_s$s_20_25_lts <- dat_mag_s$school_20_25_mins * (dat_mag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_mag_s_lts$`school_20_25_mins_lts2%`/100)
out_mag_s$s_25_30 <- as.numeric(0)
out_mag_s$s_25_30 <- dat_mag_s$school_25_30_mins
out_mag_s$s_25_30_lts <- as.numeric(0)
out_mag_s$s_25_30_lts <- dat_mag_s$school_25_30_mins * (dat_mag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_mag_s_lts$`school_25_30_mins_lts2%`/100)
out_mag_s$s_30_35 <- as.numeric(0)
out_mag_s$s_30_35 <- dat_mag_s$school_30_35_mins
out_mag_s$s_30_35_lts <- as.numeric(0)
out_mag_s$s_30_35_lts <- dat_mag_s$school_30_35_mins * (dat_mag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_mag_s_lts$`school_30_35_mins_lts2%`/100)
out_mag_s$s_35_40 <- as.numeric(0)
out_mag_s$s_35_40 <- dat_mag_s$school_35_40_mins
out_mag_s$s_35_40_lts <- as.numeric(0)
out_mag_s$s_35_40_lts <- dat_mag_s$school_35_40_mins * (dat_mag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_mag_s_lts$`school_35_40_mins_lts2%`/100)
out_mag_s$s_40_45 <- as.numeric(0)
out_mag_s$s_40_45 <- dat_mag_s$school_40_45_mins
out_mag_s$s_40_45_lts <- as.numeric(0)
out_mag_s$s_40_45_lts <- dat_mag_s$school_40_45_mins * (dat_mag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_mag_s_lts$`school_40_45_mins_lts2%`/100)
out_mag_s$s_45_50 <- as.numeric(0)
out_mag_s$s_45_50 <- dat_mag_s$school_45_50_mins
out_mag_s$s_45_50_lts <- as.numeric(0)
out_mag_s$s_45_50_lts <- dat_mag_s$school_45_50_mins * (dat_mag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_mag_s_lts$`school_45_50_mins_lts2%`/100)
out_mag_s$s_50_55 <- as.numeric(0)
out_mag_s$s_50_55 <- dat_mag_s$school_50_55_mins
out_mag_s$s_50_55_lts <- as.numeric(0)
out_mag_s$s_50_55_lts <- dat_mag_s$school_50_55_mins * (dat_mag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_mag_s_lts$`school_50_55_mins_lts2%`/100)
out_mag_s$s_55_60 <- as.numeric(0)
out_mag_s$s_55_60 <- dat_mag_s$school_55_60_mins
out_mag_s$s_55_60_lts <- as.numeric(0)
out_mag_s$s_55_60_lts <- dat_mag_s$school_55_60_mins * (dat_mag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_mag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_mag_s)

# Export accessibility output 
write.dbf(out_mag_s, (file=paste(dat_dir, "access_saf_schools_mag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_mag_m <- read_csv(paste(dat_dir, "saf_mag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m <- dat_mag_m %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m)
dat_mag_m[is.na(dat_mag_m)] <- 0; dat_mag_m <- dat_mag_m %>% arrange(From)
dat_mag_m_lts <- read_csv(paste(dat_dir, "saf_mag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_mag_m_lts <- dat_mag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_mag_m_lts)
dat_mag_m_lts[is.na(dat_mag_m_lts)] <- 0; dat_mag_m_lts <- dat_mag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_mag_m <- as.data.frame(dat_mag_m_lts)[1]
out_mag_m$m_00_05 <- as.numeric(0)
out_mag_m$m_00_05 <- dat_mag_m$grocery_00_05_mins
out_mag_m$m_00_05_lts <- as.numeric(0)
out_mag_m$m_00_05_lts <- dat_mag_m$grocery_00_05_mins * (dat_mag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_mag_m$m_05_10 <- as.numeric(0)
out_mag_m$m_05_10 <- dat_mag_m$grocery_05_10_mins
out_mag_m$m_05_10_lts <- as.numeric(0)
out_mag_m$m_05_10_lts <- dat_mag_m$grocery_05_10_mins * (dat_mag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_mag_m$m_10_15 <- as.numeric(0)
out_mag_m$m_10_15 <- dat_mag_m$grocery_10_15_mins
out_mag_m$m_10_15_lts <- as.numeric(0)
out_mag_m$m_10_15_lts <- dat_mag_m$grocery_10_15_mins * (dat_mag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_mag_m$m_15_20 <- as.numeric(0)
out_mag_m$m_15_20 <- dat_mag_m$grocery_15_20_mins
out_mag_m$m_15_20_lts <- as.numeric(0)
out_mag_m$m_15_20_lts <- dat_mag_m$grocery_15_20_mins * (dat_mag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_mag_m$m_20_25 <- as.numeric(0)
out_mag_m$m_20_25 <- dat_mag_m$grocery_20_25_mins
out_mag_m$m_20_25_lts <- as.numeric(0)
out_mag_m$m_20_25_lts <- dat_mag_m$grocery_20_25_mins * (dat_mag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_mag_m$m_25_30 <- as.numeric(0)
out_mag_m$m_25_30 <- dat_mag_m$grocery_25_30_mins
out_mag_m$m_25_30_lts <- as.numeric(0)
out_mag_m$m_25_30_lts <- dat_mag_m$grocery_25_30_mins * (dat_mag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_mag_m$m_30_35 <- as.numeric(0)
out_mag_m$m_30_35 <- dat_mag_m$grocery_30_35_mins
out_mag_m$m_30_35_lts <- as.numeric(0)
out_mag_m$m_30_35_lts <- dat_mag_m$grocery_30_35_mins * (dat_mag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_mag_m$m_35_40 <- as.numeric(0)
out_mag_m$m_35_40 <- dat_mag_m$grocery_35_40_mins
out_mag_m$m_35_40_lts <- as.numeric(0)
out_mag_m$m_35_40_lts <- dat_mag_m$grocery_35_40_mins * (dat_mag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_mag_m$m_40_45 <- as.numeric(0)
out_mag_m$m_40_45 <- dat_mag_m$grocery_40_45_mins
out_mag_m$m_40_45_lts <- as.numeric(0)
out_mag_m$m_40_45_lts <- dat_mag_m$grocery_40_45_mins * (dat_mag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_mag_m$m_45_50 <- as.numeric(0)
out_mag_m$m_45_50 <- dat_mag_m$grocery_45_50_mins
out_mag_m$m_45_50_lts <- as.numeric(0)
out_mag_m$m_45_50_lts <- dat_mag_m$grocery_45_50_mins * (dat_mag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_mag_m$m_50_55 <- as.numeric(0)
out_mag_m$m_50_55 <- dat_mag_m$grocery_50_55_mins
out_mag_m$m_50_55_lts <- as.numeric(0)
out_mag_m$m_50_55_lts <- dat_mag_m$grocery_50_55_mins * (dat_mag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_mag_m$m_55_60 <- as.numeric(0)
out_mag_m$m_55_60 <- dat_mag_m$grocery_55_60_mins
out_mag_m$m_55_60_lts <- as.numeric(0)
out_mag_m$m_55_60_lts <- dat_mag_m$grocery_55_60_mins * (dat_mag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_mag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_mag_m)

# Export accessibility output 
write.dbf(out_mag_m, (file=paste(dat_dir, "access_saf_markets_mag.dbf", sep="_data/_tabular/_outputs/")))

####################
### PAG: Tucson

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_pag_s <- read_csv(paste(dat_dir, "ibc_pag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s <- dat_pag_s %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s)
dat_pag_s[is.na(dat_pag_s)] <- 0; dat_pag_s <- dat_pag_s %>% arrange(From)
dat_pag_s_lts <- read_csv(paste(dat_dir, "ibc_pag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s_lts <- dat_pag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s_lts)
dat_pag_s_lts[is.na(dat_pag_s_lts)] <- 0; dat_pag_s_lts <- dat_pag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_s <- as.data.frame(dat_pag_s_lts)[1]
out_pag_s$s_00_05 <- as.numeric(0)
out_pag_s$s_00_05 <- dat_pag_s$school_00_05_mins
out_pag_s$s_00_05_lts <- as.numeric(0)
out_pag_s$s_00_05_lts <- dat_pag_s$school_00_05_mins * (dat_pag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_pag_s_lts$`school_00_05_mins_lts2%`/100)
out_pag_s$s_05_10 <- as.numeric(0)
out_pag_s$s_05_10 <- dat_pag_s$school_05_10_mins
out_pag_s$s_05_10_lts <- as.numeric(0)
out_pag_s$s_05_10_lts <- dat_pag_s$school_05_10_mins * (dat_pag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_pag_s_lts$`school_05_10_mins_lts2%`/100)
out_pag_s$s_10_15 <- as.numeric(0)
out_pag_s$s_10_15 <- dat_pag_s$school_10_15_mins
out_pag_s$s_10_15_lts <- as.numeric(0)
out_pag_s$s_10_15_lts <- dat_pag_s$school_10_15_mins * (dat_pag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_pag_s_lts$`school_10_15_mins_lts2%`/100)
out_pag_s$s_15_20 <- as.numeric(0)
out_pag_s$s_15_20 <- dat_pag_s$school_15_20_mins
out_pag_s$s_15_20_lts <- as.numeric(0)
out_pag_s$s_15_20_lts <- dat_pag_s$school_15_20_mins * (dat_pag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_pag_s_lts$`school_15_20_mins_lts2%`/100)
out_pag_s$s_20_25 <- as.numeric(0)
out_pag_s$s_20_25 <- dat_pag_s$school_20_25_mins
out_pag_s$s_20_25_lts <- as.numeric(0)
out_pag_s$s_20_25_lts <- dat_pag_s$school_20_25_mins * (dat_pag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_pag_s_lts$`school_20_25_mins_lts2%`/100)
out_pag_s$s_25_30 <- as.numeric(0)
out_pag_s$s_25_30 <- dat_pag_s$school_25_30_mins
out_pag_s$s_25_30_lts <- as.numeric(0)
out_pag_s$s_25_30_lts <- dat_pag_s$school_25_30_mins * (dat_pag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_pag_s_lts$`school_25_30_mins_lts2%`/100)
out_pag_s$s_30_35 <- as.numeric(0)
out_pag_s$s_30_35 <- dat_pag_s$school_30_35_mins
out_pag_s$s_30_35_lts <- as.numeric(0)
out_pag_s$s_30_35_lts <- dat_pag_s$school_30_35_mins * (dat_pag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_pag_s_lts$`school_30_35_mins_lts2%`/100)
out_pag_s$s_35_40 <- as.numeric(0)
out_pag_s$s_35_40 <- dat_pag_s$school_35_40_mins
out_pag_s$s_35_40_lts <- as.numeric(0)
out_pag_s$s_35_40_lts <- dat_pag_s$school_35_40_mins * (dat_pag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_pag_s_lts$`school_35_40_mins_lts2%`/100)
out_pag_s$s_40_45 <- as.numeric(0)
out_pag_s$s_40_45 <- dat_pag_s$school_40_45_mins
out_pag_s$s_40_45_lts <- as.numeric(0)
out_pag_s$s_40_45_lts <- dat_pag_s$school_40_45_mins * (dat_pag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_pag_s_lts$`school_40_45_mins_lts2%`/100)
out_pag_s$s_45_50 <- as.numeric(0)
out_pag_s$s_45_50 <- dat_pag_s$school_45_50_mins
out_pag_s$s_45_50_lts <- as.numeric(0)
out_pag_s$s_45_50_lts <- dat_pag_s$school_45_50_mins * (dat_pag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_pag_s_lts$`school_45_50_mins_lts2%`/100)
out_pag_s$s_50_55 <- as.numeric(0)
out_pag_s$s_50_55 <- dat_pag_s$school_50_55_mins
out_pag_s$s_50_55_lts <- as.numeric(0)
out_pag_s$s_50_55_lts <- dat_pag_s$school_50_55_mins * (dat_pag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_pag_s_lts$`school_50_55_mins_lts2%`/100)
out_pag_s$s_55_60 <- as.numeric(0)
out_pag_s$s_55_60 <- dat_pag_s$school_55_60_mins
out_pag_s$s_55_60_lts <- as.numeric(0)
out_pag_s$s_55_60_lts <- dat_pag_s$school_55_60_mins * (dat_pag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_pag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_pag_s)

# Export accessibility output 
write.dbf(out_pag_s, (file=paste(dat_dir, "access_ibc_schools_pag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_pag_m <- read_csv(paste(dat_dir, "ibc_pag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m <- dat_pag_m %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m)
dat_pag_m[is.na(dat_pag_m)] <- 0; dat_pag_m <- dat_pag_m %>% arrange(From)
dat_pag_m_lts <- read_csv(paste(dat_dir, "ibc_pag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m_lts <- dat_pag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m_lts)
dat_pag_m_lts[is.na(dat_pag_m_lts)] <- 0; dat_pag_m_lts <- dat_pag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_m <- as.data.frame(dat_pag_m_lts)[1]
out_pag_m$m_00_05 <- as.numeric(0)
out_pag_m$m_00_05 <- dat_pag_m$grocery_00_05_mins
out_pag_m$m_00_05_lts <- as.numeric(0)
out_pag_m$m_00_05_lts <- dat_pag_m$grocery_00_05_mins * (dat_pag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_pag_m$m_05_10 <- as.numeric(0)
out_pag_m$m_05_10 <- dat_pag_m$grocery_05_10_mins
out_pag_m$m_05_10_lts <- as.numeric(0)
out_pag_m$m_05_10_lts <- dat_pag_m$grocery_05_10_mins * (dat_pag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_pag_m$m_10_15 <- as.numeric(0)
out_pag_m$m_10_15 <- dat_pag_m$grocery_10_15_mins
out_pag_m$m_10_15_lts <- as.numeric(0)
out_pag_m$m_10_15_lts <- dat_pag_m$grocery_10_15_mins * (dat_pag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_pag_m$m_15_20 <- as.numeric(0)
out_pag_m$m_15_20 <- dat_pag_m$grocery_15_20_mins
out_pag_m$m_15_20_lts <- as.numeric(0)
out_pag_m$m_15_20_lts <- dat_pag_m$grocery_15_20_mins * (dat_pag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_pag_m$m_20_25 <- as.numeric(0)
out_pag_m$m_20_25 <- dat_pag_m$grocery_20_25_mins
out_pag_m$m_20_25_lts <- as.numeric(0)
out_pag_m$m_20_25_lts <- dat_pag_m$grocery_20_25_mins * (dat_pag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_pag_m$m_25_30 <- as.numeric(0)
out_pag_m$m_25_30 <- dat_pag_m$grocery_25_30_mins
out_pag_m$m_25_30_lts <- as.numeric(0)
out_pag_m$m_25_30_lts <- dat_pag_m$grocery_25_30_mins * (dat_pag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_pag_m$m_30_35 <- as.numeric(0)
out_pag_m$m_30_35 <- dat_pag_m$grocery_30_35_mins
out_pag_m$m_30_35_lts <- as.numeric(0)
out_pag_m$m_30_35_lts <- dat_pag_m$grocery_30_35_mins * (dat_pag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_pag_m$m_35_40 <- as.numeric(0)
out_pag_m$m_35_40 <- dat_pag_m$grocery_35_40_mins
out_pag_m$m_35_40_lts <- as.numeric(0)
out_pag_m$m_35_40_lts <- dat_pag_m$grocery_35_40_mins * (dat_pag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_pag_m$m_40_45 <- as.numeric(0)
out_pag_m$m_40_45 <- dat_pag_m$grocery_40_45_mins
out_pag_m$m_40_45_lts <- as.numeric(0)
out_pag_m$m_40_45_lts <- dat_pag_m$grocery_40_45_mins * (dat_pag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_pag_m$m_45_50 <- as.numeric(0)
out_pag_m$m_45_50 <- dat_pag_m$grocery_45_50_mins
out_pag_m$m_45_50_lts <- as.numeric(0)
out_pag_m$m_45_50_lts <- dat_pag_m$grocery_45_50_mins * (dat_pag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_pag_m$m_50_55 <- as.numeric(0)
out_pag_m$m_50_55 <- dat_pag_m$grocery_50_55_mins
out_pag_m$m_50_55_lts <- as.numeric(0)
out_pag_m$m_50_55_lts <- dat_pag_m$grocery_50_55_mins * (dat_pag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_pag_m$m_55_60 <- as.numeric(0)
out_pag_m$m_55_60 <- dat_pag_m$grocery_55_60_mins
out_pag_m$m_55_60_lts <- as.numeric(0)
out_pag_m$m_55_60_lts <- dat_pag_m$grocery_55_60_mins * (dat_pag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_pag_m)

# Export accessibility output 
write.dbf(out_pag_m, (file=paste(dat_dir, "access_ibc_markets_pag.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_pag_s <- read_csv(paste(dat_dir, "eac_pag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s <- dat_pag_s %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s)
dat_pag_s[is.na(dat_pag_s)] <- 0; dat_pag_s <- dat_pag_s %>% arrange(From)
dat_pag_s_lts <- read_csv(paste(dat_dir, "eac_pag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s_lts <- dat_pag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s_lts)
dat_pag_s_lts[is.na(dat_pag_s_lts)] <- 0; dat_pag_s_lts <- dat_pag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_s <- as.data.frame(dat_pag_s_lts)[1]
out_pag_s$s_00_05 <- as.numeric(0)
out_pag_s$s_00_05 <- dat_pag_s$school_00_05_mins
out_pag_s$s_00_05_lts <- as.numeric(0)
out_pag_s$s_00_05_lts <- dat_pag_s$school_00_05_mins * (dat_pag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_pag_s_lts$`school_00_05_mins_lts2%`/100)
out_pag_s$s_05_10 <- as.numeric(0)
out_pag_s$s_05_10 <- dat_pag_s$school_05_10_mins
out_pag_s$s_05_10_lts <- as.numeric(0)
out_pag_s$s_05_10_lts <- dat_pag_s$school_05_10_mins * (dat_pag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_pag_s_lts$`school_05_10_mins_lts2%`/100)
out_pag_s$s_10_15 <- as.numeric(0)
out_pag_s$s_10_15 <- dat_pag_s$school_10_15_mins
out_pag_s$s_10_15_lts <- as.numeric(0)
out_pag_s$s_10_15_lts <- dat_pag_s$school_10_15_mins * (dat_pag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_pag_s_lts$`school_10_15_mins_lts2%`/100)
out_pag_s$s_15_20 <- as.numeric(0)
out_pag_s$s_15_20 <- dat_pag_s$school_15_20_mins
out_pag_s$s_15_20_lts <- as.numeric(0)
out_pag_s$s_15_20_lts <- dat_pag_s$school_15_20_mins * (dat_pag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_pag_s_lts$`school_15_20_mins_lts2%`/100)
out_pag_s$s_20_25 <- as.numeric(0)
out_pag_s$s_20_25 <- dat_pag_s$school_20_25_mins
out_pag_s$s_20_25_lts <- as.numeric(0)
out_pag_s$s_20_25_lts <- dat_pag_s$school_20_25_mins * (dat_pag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_pag_s_lts$`school_20_25_mins_lts2%`/100)
out_pag_s$s_25_30 <- as.numeric(0)
out_pag_s$s_25_30 <- dat_pag_s$school_25_30_mins
out_pag_s$s_25_30_lts <- as.numeric(0)
out_pag_s$s_25_30_lts <- dat_pag_s$school_25_30_mins * (dat_pag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_pag_s_lts$`school_25_30_mins_lts2%`/100)
out_pag_s$s_30_35 <- as.numeric(0)
out_pag_s$s_30_35 <- dat_pag_s$school_30_35_mins
out_pag_s$s_30_35_lts <- as.numeric(0)
out_pag_s$s_30_35_lts <- dat_pag_s$school_30_35_mins * (dat_pag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_pag_s_lts$`school_30_35_mins_lts2%`/100)
out_pag_s$s_35_40 <- as.numeric(0)
out_pag_s$s_35_40 <- dat_pag_s$school_35_40_mins
out_pag_s$s_35_40_lts <- as.numeric(0)
out_pag_s$s_35_40_lts <- dat_pag_s$school_35_40_mins * (dat_pag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_pag_s_lts$`school_35_40_mins_lts2%`/100)
out_pag_s$s_40_45 <- as.numeric(0)
out_pag_s$s_40_45 <- dat_pag_s$school_40_45_mins
out_pag_s$s_40_45_lts <- as.numeric(0)
out_pag_s$s_40_45_lts <- dat_pag_s$school_40_45_mins * (dat_pag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_pag_s_lts$`school_40_45_mins_lts2%`/100)
out_pag_s$s_45_50 <- as.numeric(0)
out_pag_s$s_45_50 <- dat_pag_s$school_45_50_mins
out_pag_s$s_45_50_lts <- as.numeric(0)
out_pag_s$s_45_50_lts <- dat_pag_s$school_45_50_mins * (dat_pag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_pag_s_lts$`school_45_50_mins_lts2%`/100)
out_pag_s$s_50_55 <- as.numeric(0)
out_pag_s$s_50_55 <- dat_pag_s$school_50_55_mins
out_pag_s$s_50_55_lts <- as.numeric(0)
out_pag_s$s_50_55_lts <- dat_pag_s$school_50_55_mins * (dat_pag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_pag_s_lts$`school_50_55_mins_lts2%`/100)
out_pag_s$s_55_60 <- as.numeric(0)
out_pag_s$s_55_60 <- dat_pag_s$school_55_60_mins
out_pag_s$s_55_60_lts <- as.numeric(0)
out_pag_s$s_55_60_lts <- dat_pag_s$school_55_60_mins * (dat_pag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_pag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_pag_s)

# Export accessibility output 
write.dbf(out_pag_s, (file=paste(dat_dir, "access_eac_schools_pag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_pag_m <- read_csv(paste(dat_dir, "eac_pag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m <- dat_pag_m %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m)
dat_pag_m[is.na(dat_pag_m)] <- 0; dat_pag_m <- dat_pag_m %>% arrange(From)
dat_pag_m_lts <- read_csv(paste(dat_dir, "eac_pag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m_lts <- dat_pag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m_lts)
dat_pag_m_lts[is.na(dat_pag_m_lts)] <- 0; dat_pag_m_lts <- dat_pag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_m <- as.data.frame(dat_pag_m_lts)[1]
out_pag_m$m_00_05 <- as.numeric(0)
out_pag_m$m_00_05 <- dat_pag_m$grocery_00_05_mins
out_pag_m$m_00_05_lts <- as.numeric(0)
out_pag_m$m_00_05_lts <- dat_pag_m$grocery_00_05_mins * (dat_pag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_pag_m$m_05_10 <- as.numeric(0)
out_pag_m$m_05_10 <- dat_pag_m$grocery_05_10_mins
out_pag_m$m_05_10_lts <- as.numeric(0)
out_pag_m$m_05_10_lts <- dat_pag_m$grocery_05_10_mins * (dat_pag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_pag_m$m_10_15 <- as.numeric(0)
out_pag_m$m_10_15 <- dat_pag_m$grocery_10_15_mins
out_pag_m$m_10_15_lts <- as.numeric(0)
out_pag_m$m_10_15_lts <- dat_pag_m$grocery_10_15_mins * (dat_pag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_pag_m$m_15_20 <- as.numeric(0)
out_pag_m$m_15_20 <- dat_pag_m$grocery_15_20_mins
out_pag_m$m_15_20_lts <- as.numeric(0)
out_pag_m$m_15_20_lts <- dat_pag_m$grocery_15_20_mins * (dat_pag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_pag_m$m_20_25 <- as.numeric(0)
out_pag_m$m_20_25 <- dat_pag_m$grocery_20_25_mins
out_pag_m$m_20_25_lts <- as.numeric(0)
out_pag_m$m_20_25_lts <- dat_pag_m$grocery_20_25_mins * (dat_pag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_pag_m$m_25_30 <- as.numeric(0)
out_pag_m$m_25_30 <- dat_pag_m$grocery_25_30_mins
out_pag_m$m_25_30_lts <- as.numeric(0)
out_pag_m$m_25_30_lts <- dat_pag_m$grocery_25_30_mins * (dat_pag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_pag_m$m_30_35 <- as.numeric(0)
out_pag_m$m_30_35 <- dat_pag_m$grocery_30_35_mins
out_pag_m$m_30_35_lts <- as.numeric(0)
out_pag_m$m_30_35_lts <- dat_pag_m$grocery_30_35_mins * (dat_pag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_pag_m$m_35_40 <- as.numeric(0)
out_pag_m$m_35_40 <- dat_pag_m$grocery_35_40_mins
out_pag_m$m_35_40_lts <- as.numeric(0)
out_pag_m$m_35_40_lts <- dat_pag_m$grocery_35_40_mins * (dat_pag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_pag_m$m_40_45 <- as.numeric(0)
out_pag_m$m_40_45 <- dat_pag_m$grocery_40_45_mins
out_pag_m$m_40_45_lts <- as.numeric(0)
out_pag_m$m_40_45_lts <- dat_pag_m$grocery_40_45_mins * (dat_pag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_pag_m$m_45_50 <- as.numeric(0)
out_pag_m$m_45_50 <- dat_pag_m$grocery_45_50_mins
out_pag_m$m_45_50_lts <- as.numeric(0)
out_pag_m$m_45_50_lts <- dat_pag_m$grocery_45_50_mins * (dat_pag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_pag_m$m_50_55 <- as.numeric(0)
out_pag_m$m_50_55 <- dat_pag_m$grocery_50_55_mins
out_pag_m$m_50_55_lts <- as.numeric(0)
out_pag_m$m_50_55_lts <- dat_pag_m$grocery_50_55_mins * (dat_pag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_pag_m$m_55_60 <- as.numeric(0)
out_pag_m$m_55_60 <- dat_pag_m$grocery_55_60_mins
out_pag_m$m_55_60_lts <- as.numeric(0)
out_pag_m$m_55_60_lts <- dat_pag_m$grocery_55_60_mins * (dat_pag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_pag_m)

# Export accessibility output 
write.dbf(out_pag_m, (file=paste(dat_dir, "access_eac_markets_pag.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_pag_s <- read_csv(paste(dat_dir, "saf_pag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s <- dat_pag_s %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s)
dat_pag_s[is.na(dat_pag_s)] <- 0; dat_pag_s <- dat_pag_s %>% arrange(From)
dat_pag_s_lts <- read_csv(paste(dat_dir, "saf_pag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_pag_s_lts <- dat_pag_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_s_lts)
dat_pag_s_lts[is.na(dat_pag_s_lts)] <- 0; dat_pag_s_lts <- dat_pag_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_s <- as.data.frame(dat_pag_s_lts)[1]
out_pag_s$s_00_05 <- as.numeric(0)
out_pag_s$s_00_05 <- dat_pag_s$school_00_05_mins
out_pag_s$s_00_05_lts <- as.numeric(0)
out_pag_s$s_00_05_lts <- dat_pag_s$school_00_05_mins * (dat_pag_s_lts$`school_00_05_mins_lts1%`/100)+(dat_pag_s_lts$`school_00_05_mins_lts2%`/100)
out_pag_s$s_05_10 <- as.numeric(0)
out_pag_s$s_05_10 <- dat_pag_s$school_05_10_mins
out_pag_s$s_05_10_lts <- as.numeric(0)
out_pag_s$s_05_10_lts <- dat_pag_s$school_05_10_mins * (dat_pag_s_lts$`school_05_10_mins_lts1%`/100)+(dat_pag_s_lts$`school_05_10_mins_lts2%`/100)
out_pag_s$s_10_15 <- as.numeric(0)
out_pag_s$s_10_15 <- dat_pag_s$school_10_15_mins
out_pag_s$s_10_15_lts <- as.numeric(0)
out_pag_s$s_10_15_lts <- dat_pag_s$school_10_15_mins * (dat_pag_s_lts$`school_10_15_mins_lts1%`/100)+(dat_pag_s_lts$`school_10_15_mins_lts2%`/100)
out_pag_s$s_15_20 <- as.numeric(0)
out_pag_s$s_15_20 <- dat_pag_s$school_15_20_mins
out_pag_s$s_15_20_lts <- as.numeric(0)
out_pag_s$s_15_20_lts <- dat_pag_s$school_15_20_mins * (dat_pag_s_lts$`school_15_20_mins_lts1%`/100)+(dat_pag_s_lts$`school_15_20_mins_lts2%`/100)
out_pag_s$s_20_25 <- as.numeric(0)
out_pag_s$s_20_25 <- dat_pag_s$school_20_25_mins
out_pag_s$s_20_25_lts <- as.numeric(0)
out_pag_s$s_20_25_lts <- dat_pag_s$school_20_25_mins * (dat_pag_s_lts$`school_20_25_mins_lts1%`/100)+(dat_pag_s_lts$`school_20_25_mins_lts2%`/100)
out_pag_s$s_25_30 <- as.numeric(0)
out_pag_s$s_25_30 <- dat_pag_s$school_25_30_mins
out_pag_s$s_25_30_lts <- as.numeric(0)
out_pag_s$s_25_30_lts <- dat_pag_s$school_25_30_mins * (dat_pag_s_lts$`school_25_30_mins_lts1%`/100)+(dat_pag_s_lts$`school_25_30_mins_lts2%`/100)
out_pag_s$s_30_35 <- as.numeric(0)
out_pag_s$s_30_35 <- dat_pag_s$school_30_35_mins
out_pag_s$s_30_35_lts <- as.numeric(0)
out_pag_s$s_30_35_lts <- dat_pag_s$school_30_35_mins * (dat_pag_s_lts$`school_30_35_mins_lts1%`/100)+(dat_pag_s_lts$`school_30_35_mins_lts2%`/100)
out_pag_s$s_35_40 <- as.numeric(0)
out_pag_s$s_35_40 <- dat_pag_s$school_35_40_mins
out_pag_s$s_35_40_lts <- as.numeric(0)
out_pag_s$s_35_40_lts <- dat_pag_s$school_35_40_mins * (dat_pag_s_lts$`school_35_40_mins_lts1%`/100)+(dat_pag_s_lts$`school_35_40_mins_lts2%`/100)
out_pag_s$s_40_45 <- as.numeric(0)
out_pag_s$s_40_45 <- dat_pag_s$school_40_45_mins
out_pag_s$s_40_45_lts <- as.numeric(0)
out_pag_s$s_40_45_lts <- dat_pag_s$school_40_45_mins * (dat_pag_s_lts$`school_40_45_mins_lts1%`/100)+(dat_pag_s_lts$`school_40_45_mins_lts2%`/100)
out_pag_s$s_45_50 <- as.numeric(0)
out_pag_s$s_45_50 <- dat_pag_s$school_45_50_mins
out_pag_s$s_45_50_lts <- as.numeric(0)
out_pag_s$s_45_50_lts <- dat_pag_s$school_45_50_mins * (dat_pag_s_lts$`school_45_50_mins_lts1%`/100)+(dat_pag_s_lts$`school_45_50_mins_lts2%`/100)
out_pag_s$s_50_55 <- as.numeric(0)
out_pag_s$s_50_55 <- dat_pag_s$school_50_55_mins
out_pag_s$s_50_55_lts <- as.numeric(0)
out_pag_s$s_50_55_lts <- dat_pag_s$school_50_55_mins * (dat_pag_s_lts$`school_50_55_mins_lts1%`/100)+(dat_pag_s_lts$`school_50_55_mins_lts2%`/100)
out_pag_s$s_55_60 <- as.numeric(0)
out_pag_s$s_55_60 <- dat_pag_s$school_55_60_mins
out_pag_s$s_55_60_lts <- as.numeric(0)
out_pag_s$s_55_60_lts <- dat_pag_s$school_55_60_mins * (dat_pag_s_lts$`school_55_60_mins_lts1%`/100)+(dat_pag_s_lts$`school_55_60_mins_lts2%`/100)
str(out_pag_s)

# Export accessibility output 
write.dbf(out_pag_s, (file=paste(dat_dir, "access_saf_schools_pag.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_pag_m <- read_csv(paste(dat_dir, "saf_pag_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m <- dat_pag_m %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m)
dat_pag_m[is.na(dat_pag_m)] <- 0; dat_pag_m <- dat_pag_m %>% arrange(From)
dat_pag_m_lts <- read_csv(paste(dat_dir, "saf_pag_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_pag_m_lts <- dat_pag_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_pag_m_lts)
dat_pag_m_lts[is.na(dat_pag_m_lts)] <- 0; dat_pag_m_lts <- dat_pag_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_pag_m <- as.data.frame(dat_pag_m_lts)[1]
out_pag_m$m_00_05 <- as.numeric(0)
out_pag_m$m_00_05 <- dat_pag_m$grocery_00_05_mins
out_pag_m$m_00_05_lts <- as.numeric(0)
out_pag_m$m_00_05_lts <- dat_pag_m$grocery_00_05_mins * (dat_pag_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_00_05_mins_lts2%`/100)
out_pag_m$m_05_10 <- as.numeric(0)
out_pag_m$m_05_10 <- dat_pag_m$grocery_05_10_mins
out_pag_m$m_05_10_lts <- as.numeric(0)
out_pag_m$m_05_10_lts <- dat_pag_m$grocery_05_10_mins * (dat_pag_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_05_10_mins_lts2%`/100)
out_pag_m$m_10_15 <- as.numeric(0)
out_pag_m$m_10_15 <- dat_pag_m$grocery_10_15_mins
out_pag_m$m_10_15_lts <- as.numeric(0)
out_pag_m$m_10_15_lts <- dat_pag_m$grocery_10_15_mins * (dat_pag_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_10_15_mins_lts2%`/100)
out_pag_m$m_15_20 <- as.numeric(0)
out_pag_m$m_15_20 <- dat_pag_m$grocery_15_20_mins
out_pag_m$m_15_20_lts <- as.numeric(0)
out_pag_m$m_15_20_lts <- dat_pag_m$grocery_15_20_mins * (dat_pag_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_15_20_mins_lts2%`/100)
out_pag_m$m_20_25 <- as.numeric(0)
out_pag_m$m_20_25 <- dat_pag_m$grocery_20_25_mins
out_pag_m$m_20_25_lts <- as.numeric(0)
out_pag_m$m_20_25_lts <- dat_pag_m$grocery_20_25_mins * (dat_pag_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_20_25_mins_lts2%`/100)
out_pag_m$m_25_30 <- as.numeric(0)
out_pag_m$m_25_30 <- dat_pag_m$grocery_25_30_mins
out_pag_m$m_25_30_lts <- as.numeric(0)
out_pag_m$m_25_30_lts <- dat_pag_m$grocery_25_30_mins * (dat_pag_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_25_30_mins_lts2%`/100)
out_pag_m$m_30_35 <- as.numeric(0)
out_pag_m$m_30_35 <- dat_pag_m$grocery_30_35_mins
out_pag_m$m_30_35_lts <- as.numeric(0)
out_pag_m$m_30_35_lts <- dat_pag_m$grocery_30_35_mins * (dat_pag_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_30_35_mins_lts2%`/100)
out_pag_m$m_35_40 <- as.numeric(0)
out_pag_m$m_35_40 <- dat_pag_m$grocery_35_40_mins
out_pag_m$m_35_40_lts <- as.numeric(0)
out_pag_m$m_35_40_lts <- dat_pag_m$grocery_35_40_mins * (dat_pag_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_35_40_mins_lts2%`/100)
out_pag_m$m_40_45 <- as.numeric(0)
out_pag_m$m_40_45 <- dat_pag_m$grocery_40_45_mins
out_pag_m$m_40_45_lts <- as.numeric(0)
out_pag_m$m_40_45_lts <- dat_pag_m$grocery_40_45_mins * (dat_pag_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_40_45_mins_lts2%`/100)
out_pag_m$m_45_50 <- as.numeric(0)
out_pag_m$m_45_50 <- dat_pag_m$grocery_45_50_mins
out_pag_m$m_45_50_lts <- as.numeric(0)
out_pag_m$m_45_50_lts <- dat_pag_m$grocery_45_50_mins * (dat_pag_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_45_50_mins_lts2%`/100)
out_pag_m$m_50_55 <- as.numeric(0)
out_pag_m$m_50_55 <- dat_pag_m$grocery_50_55_mins
out_pag_m$m_50_55_lts <- as.numeric(0)
out_pag_m$m_50_55_lts <- dat_pag_m$grocery_50_55_mins * (dat_pag_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_50_55_mins_lts2%`/100)
out_pag_m$m_55_60 <- as.numeric(0)
out_pag_m$m_55_60 <- dat_pag_m$grocery_55_60_mins
out_pag_m$m_55_60_lts <- as.numeric(0)
out_pag_m$m_55_60_lts <- dat_pag_m$grocery_55_60_mins * (dat_pag_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_pag_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_pag_m)

# Export accessibility output 
write.dbf(out_pag_m, (file=paste(dat_dir, "access_saf_markets_pag.dbf", sep="_data/_tabular/_outputs/")))

####################
### SCMPO: Casa Grande

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_scmpo_s <- read_csv(paste(dat_dir, "ibc_scmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s <- dat_scmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s)
dat_scmpo_s[is.na(dat_scmpo_s)] <- 0; dat_scmpo_s <- dat_scmpo_s %>% arrange(From)
dat_scmpo_s_lts <- read_csv(paste(dat_dir, "ibc_scmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s_lts <- dat_scmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s_lts)
dat_scmpo_s_lts[is.na(dat_scmpo_s_lts)] <- 0; dat_scmpo_s_lts <- dat_scmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_s <- as.data.frame(dat_scmpo_s_lts)[1]
out_scmpo_s$s_00_05 <- as.numeric(0)
out_scmpo_s$s_00_05 <- dat_scmpo_s$school_00_05_mins
out_scmpo_s$s_00_05_lts <- as.numeric(0)
out_scmpo_s$s_00_05_lts <- dat_scmpo_s$school_00_05_mins * (dat_scmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_scmpo_s$s_05_10 <- as.numeric(0)
out_scmpo_s$s_05_10 <- dat_scmpo_s$school_05_10_mins
out_scmpo_s$s_05_10_lts <- as.numeric(0)
out_scmpo_s$s_05_10_lts <- dat_scmpo_s$school_05_10_mins * (dat_scmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_scmpo_s$s_10_15 <- as.numeric(0)
out_scmpo_s$s_10_15 <- dat_scmpo_s$school_10_15_mins
out_scmpo_s$s_10_15_lts <- as.numeric(0)
out_scmpo_s$s_10_15_lts <- dat_scmpo_s$school_10_15_mins * (dat_scmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_scmpo_s$s_15_20 <- as.numeric(0)
out_scmpo_s$s_15_20 <- dat_scmpo_s$school_15_20_mins
out_scmpo_s$s_15_20_lts <- as.numeric(0)
out_scmpo_s$s_15_20_lts <- dat_scmpo_s$school_15_20_mins * (dat_scmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_scmpo_s$s_20_25 <- as.numeric(0)
out_scmpo_s$s_20_25 <- dat_scmpo_s$school_20_25_mins
out_scmpo_s$s_20_25_lts <- as.numeric(0)
out_scmpo_s$s_20_25_lts <- dat_scmpo_s$school_20_25_mins * (dat_scmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_scmpo_s$s_25_30 <- as.numeric(0)
out_scmpo_s$s_25_30 <- dat_scmpo_s$school_25_30_mins
out_scmpo_s$s_25_30_lts <- as.numeric(0)
out_scmpo_s$s_25_30_lts <- dat_scmpo_s$school_25_30_mins * (dat_scmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_scmpo_s$s_30_35 <- as.numeric(0)
out_scmpo_s$s_30_35 <- dat_scmpo_s$school_30_35_mins
out_scmpo_s$s_30_35_lts <- as.numeric(0)
out_scmpo_s$s_30_35_lts <- dat_scmpo_s$school_30_35_mins * (dat_scmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_scmpo_s$s_35_40 <- as.numeric(0)
out_scmpo_s$s_35_40 <- dat_scmpo_s$school_35_40_mins
out_scmpo_s$s_35_40_lts <- as.numeric(0)
out_scmpo_s$s_35_40_lts <- dat_scmpo_s$school_35_40_mins * (dat_scmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_scmpo_s$s_40_45 <- as.numeric(0)
out_scmpo_s$s_40_45 <- dat_scmpo_s$school_40_45_mins
out_scmpo_s$s_40_45_lts <- as.numeric(0)
out_scmpo_s$s_40_45_lts <- dat_scmpo_s$school_40_45_mins * (dat_scmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_scmpo_s$s_45_50 <- as.numeric(0)
out_scmpo_s$s_45_50 <- dat_scmpo_s$school_45_50_mins
out_scmpo_s$s_45_50_lts <- as.numeric(0)
out_scmpo_s$s_45_50_lts <- dat_scmpo_s$school_45_50_mins * (dat_scmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_scmpo_s$s_50_55 <- as.numeric(0)
out_scmpo_s$s_50_55 <- dat_scmpo_s$school_50_55_mins
out_scmpo_s$s_50_55_lts <- as.numeric(0)
out_scmpo_s$s_50_55_lts <- dat_scmpo_s$school_50_55_mins * (dat_scmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_scmpo_s$s_55_60 <- as.numeric(0)
out_scmpo_s$s_55_60 <- dat_scmpo_s$school_55_60_mins
out_scmpo_s$s_55_60_lts <- as.numeric(0)
out_scmpo_s$s_55_60_lts <- dat_scmpo_s$school_55_60_mins * (dat_scmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_scmpo_s)

# Export accessibility output 
write.dbf(out_scmpo_s, (file=paste(dat_dir, "access_ibc_schools_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_scmpo_m <- read_csv(paste(dat_dir, "ibc_scmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m <- dat_scmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m)
dat_scmpo_m[is.na(dat_scmpo_m)] <- 0; dat_scmpo_m <- dat_scmpo_m %>% arrange(From)
dat_scmpo_m_lts <- read_csv(paste(dat_dir, "ibc_scmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m_lts <- dat_scmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m_lts)
dat_scmpo_m_lts[is.na(dat_scmpo_m_lts)] <- 0; dat_scmpo_m_lts <- dat_scmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_m <- as.data.frame(dat_scmpo_m_lts)[1]
out_scmpo_m$m_00_05 <- as.numeric(0)
out_scmpo_m$m_00_05 <- dat_scmpo_m$grocery_00_05_mins
out_scmpo_m$m_00_05_lts <- as.numeric(0)
out_scmpo_m$m_00_05_lts <- dat_scmpo_m$grocery_00_05_mins * (dat_scmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_scmpo_m$m_05_10 <- as.numeric(0)
out_scmpo_m$m_05_10 <- dat_scmpo_m$grocery_05_10_mins
out_scmpo_m$m_05_10_lts <- as.numeric(0)
out_scmpo_m$m_05_10_lts <- dat_scmpo_m$grocery_05_10_mins * (dat_scmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_scmpo_m$m_10_15 <- as.numeric(0)
out_scmpo_m$m_10_15 <- dat_scmpo_m$grocery_10_15_mins
out_scmpo_m$m_10_15_lts <- as.numeric(0)
out_scmpo_m$m_10_15_lts <- dat_scmpo_m$grocery_10_15_mins * (dat_scmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_scmpo_m$m_15_20 <- as.numeric(0)
out_scmpo_m$m_15_20 <- dat_scmpo_m$grocery_15_20_mins
out_scmpo_m$m_15_20_lts <- as.numeric(0)
out_scmpo_m$m_15_20_lts <- dat_scmpo_m$grocery_15_20_mins * (dat_scmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_scmpo_m$m_20_25 <- as.numeric(0)
out_scmpo_m$m_20_25 <- dat_scmpo_m$grocery_20_25_mins
out_scmpo_m$m_20_25_lts <- as.numeric(0)
out_scmpo_m$m_20_25_lts <- dat_scmpo_m$grocery_20_25_mins * (dat_scmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_scmpo_m$m_25_30 <- as.numeric(0)
out_scmpo_m$m_25_30 <- dat_scmpo_m$grocery_25_30_mins
out_scmpo_m$m_25_30_lts <- as.numeric(0)
out_scmpo_m$m_25_30_lts <- dat_scmpo_m$grocery_25_30_mins * (dat_scmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_scmpo_m$m_30_35 <- as.numeric(0)
out_scmpo_m$m_30_35 <- dat_scmpo_m$grocery_30_35_mins
out_scmpo_m$m_30_35_lts <- as.numeric(0)
out_scmpo_m$m_30_35_lts <- dat_scmpo_m$grocery_30_35_mins * (dat_scmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_scmpo_m$m_35_40 <- as.numeric(0)
out_scmpo_m$m_35_40 <- dat_scmpo_m$grocery_35_40_mins
out_scmpo_m$m_35_40_lts <- as.numeric(0)
out_scmpo_m$m_35_40_lts <- dat_scmpo_m$grocery_35_40_mins * (dat_scmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_scmpo_m$m_40_45 <- as.numeric(0)
out_scmpo_m$m_40_45 <- dat_scmpo_m$grocery_40_45_mins
out_scmpo_m$m_40_45_lts <- as.numeric(0)
out_scmpo_m$m_40_45_lts <- dat_scmpo_m$grocery_40_45_mins * (dat_scmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_scmpo_m$m_45_50 <- as.numeric(0)
out_scmpo_m$m_45_50 <- dat_scmpo_m$grocery_45_50_mins
out_scmpo_m$m_45_50_lts <- as.numeric(0)
out_scmpo_m$m_45_50_lts <- dat_scmpo_m$grocery_45_50_mins * (dat_scmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_scmpo_m$m_50_55 <- as.numeric(0)
out_scmpo_m$m_50_55 <- dat_scmpo_m$grocery_50_55_mins
out_scmpo_m$m_50_55_lts <- as.numeric(0)
out_scmpo_m$m_50_55_lts <- dat_scmpo_m$grocery_50_55_mins * (dat_scmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_scmpo_m$m_55_60 <- as.numeric(0)
out_scmpo_m$m_55_60 <- dat_scmpo_m$grocery_55_60_mins
out_scmpo_m$m_55_60_lts <- as.numeric(0)
out_scmpo_m$m_55_60_lts <- dat_scmpo_m$grocery_55_60_mins * (dat_scmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_scmpo_m)

# Export accessibility output 
write.dbf(out_scmpo_m, (file=paste(dat_dir, "access_ibc_markets_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_scmpo_s <- read_csv(paste(dat_dir, "eac_scmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s <- dat_scmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s)
dat_scmpo_s[is.na(dat_scmpo_s)] <- 0; dat_scmpo_s <- dat_scmpo_s %>% arrange(From)
dat_scmpo_s_lts <- read_csv(paste(dat_dir, "eac_scmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s_lts <- dat_scmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s_lts)
dat_scmpo_s_lts[is.na(dat_scmpo_s_lts)] <- 0; dat_scmpo_s_lts <- dat_scmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_s <- as.data.frame(dat_scmpo_s_lts)[1]
out_scmpo_s$s_00_05 <- as.numeric(0)
out_scmpo_s$s_00_05 <- dat_scmpo_s$school_00_05_mins
out_scmpo_s$s_00_05_lts <- as.numeric(0)
out_scmpo_s$s_00_05_lts <- dat_scmpo_s$school_00_05_mins * (dat_scmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_scmpo_s$s_05_10 <- as.numeric(0)
out_scmpo_s$s_05_10 <- dat_scmpo_s$school_05_10_mins
out_scmpo_s$s_05_10_lts <- as.numeric(0)
out_scmpo_s$s_05_10_lts <- dat_scmpo_s$school_05_10_mins * (dat_scmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_scmpo_s$s_10_15 <- as.numeric(0)
out_scmpo_s$s_10_15 <- dat_scmpo_s$school_10_15_mins
out_scmpo_s$s_10_15_lts <- as.numeric(0)
out_scmpo_s$s_10_15_lts <- dat_scmpo_s$school_10_15_mins * (dat_scmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_scmpo_s$s_15_20 <- as.numeric(0)
out_scmpo_s$s_15_20 <- dat_scmpo_s$school_15_20_mins
out_scmpo_s$s_15_20_lts <- as.numeric(0)
out_scmpo_s$s_15_20_lts <- dat_scmpo_s$school_15_20_mins * (dat_scmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_scmpo_s$s_20_25 <- as.numeric(0)
out_scmpo_s$s_20_25 <- dat_scmpo_s$school_20_25_mins
out_scmpo_s$s_20_25_lts <- as.numeric(0)
out_scmpo_s$s_20_25_lts <- dat_scmpo_s$school_20_25_mins * (dat_scmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_scmpo_s$s_25_30 <- as.numeric(0)
out_scmpo_s$s_25_30 <- dat_scmpo_s$school_25_30_mins
out_scmpo_s$s_25_30_lts <- as.numeric(0)
out_scmpo_s$s_25_30_lts <- dat_scmpo_s$school_25_30_mins * (dat_scmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_scmpo_s$s_30_35 <- as.numeric(0)
out_scmpo_s$s_30_35 <- dat_scmpo_s$school_30_35_mins
out_scmpo_s$s_30_35_lts <- as.numeric(0)
out_scmpo_s$s_30_35_lts <- dat_scmpo_s$school_30_35_mins * (dat_scmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_scmpo_s$s_35_40 <- as.numeric(0)
out_scmpo_s$s_35_40 <- dat_scmpo_s$school_35_40_mins
out_scmpo_s$s_35_40_lts <- as.numeric(0)
out_scmpo_s$s_35_40_lts <- dat_scmpo_s$school_35_40_mins * (dat_scmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_scmpo_s$s_40_45 <- as.numeric(0)
out_scmpo_s$s_40_45 <- dat_scmpo_s$school_40_45_mins
out_scmpo_s$s_40_45_lts <- as.numeric(0)
out_scmpo_s$s_40_45_lts <- dat_scmpo_s$school_40_45_mins * (dat_scmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_scmpo_s$s_45_50 <- as.numeric(0)
out_scmpo_s$s_45_50 <- dat_scmpo_s$school_45_50_mins
out_scmpo_s$s_45_50_lts <- as.numeric(0)
out_scmpo_s$s_45_50_lts <- dat_scmpo_s$school_45_50_mins * (dat_scmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_scmpo_s$s_50_55 <- as.numeric(0)
out_scmpo_s$s_50_55 <- dat_scmpo_s$school_50_55_mins
out_scmpo_s$s_50_55_lts <- as.numeric(0)
out_scmpo_s$s_50_55_lts <- dat_scmpo_s$school_50_55_mins * (dat_scmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_scmpo_s$s_55_60 <- as.numeric(0)
out_scmpo_s$s_55_60 <- dat_scmpo_s$school_55_60_mins
out_scmpo_s$s_55_60_lts <- as.numeric(0)
out_scmpo_s$s_55_60_lts <- dat_scmpo_s$school_55_60_mins * (dat_scmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_scmpo_s)

# Export accessibility output 
write.dbf(out_scmpo_s, (file=paste(dat_dir, "access_eac_schools_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_scmpo_m <- read_csv(paste(dat_dir, "eac_scmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m <- dat_scmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m)
dat_scmpo_m[is.na(dat_scmpo_m)] <- 0; dat_scmpo_m <- dat_scmpo_m %>% arrange(From)
dat_scmpo_m_lts <- read_csv(paste(dat_dir, "eac_scmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m_lts <- dat_scmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m_lts)
dat_scmpo_m_lts[is.na(dat_scmpo_m_lts)] <- 0; dat_scmpo_m_lts <- dat_scmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_m <- as.data.frame(dat_scmpo_m_lts)[1]
out_scmpo_m$m_00_05 <- as.numeric(0)
out_scmpo_m$m_00_05 <- dat_scmpo_m$grocery_00_05_mins
out_scmpo_m$m_00_05_lts <- as.numeric(0)
out_scmpo_m$m_00_05_lts <- dat_scmpo_m$grocery_00_05_mins * (dat_scmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_scmpo_m$m_05_10 <- as.numeric(0)
out_scmpo_m$m_05_10 <- dat_scmpo_m$grocery_05_10_mins
out_scmpo_m$m_05_10_lts <- as.numeric(0)
out_scmpo_m$m_05_10_lts <- dat_scmpo_m$grocery_05_10_mins * (dat_scmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_scmpo_m$m_10_15 <- as.numeric(0)
out_scmpo_m$m_10_15 <- dat_scmpo_m$grocery_10_15_mins
out_scmpo_m$m_10_15_lts <- as.numeric(0)
out_scmpo_m$m_10_15_lts <- dat_scmpo_m$grocery_10_15_mins * (dat_scmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_scmpo_m$m_15_20 <- as.numeric(0)
out_scmpo_m$m_15_20 <- dat_scmpo_m$grocery_15_20_mins
out_scmpo_m$m_15_20_lts <- as.numeric(0)
out_scmpo_m$m_15_20_lts <- dat_scmpo_m$grocery_15_20_mins * (dat_scmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_scmpo_m$m_20_25 <- as.numeric(0)
out_scmpo_m$m_20_25 <- dat_scmpo_m$grocery_20_25_mins
out_scmpo_m$m_20_25_lts <- as.numeric(0)
out_scmpo_m$m_20_25_lts <- dat_scmpo_m$grocery_20_25_mins * (dat_scmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_scmpo_m$m_25_30 <- as.numeric(0)
out_scmpo_m$m_25_30 <- dat_scmpo_m$grocery_25_30_mins
out_scmpo_m$m_25_30_lts <- as.numeric(0)
out_scmpo_m$m_25_30_lts <- dat_scmpo_m$grocery_25_30_mins * (dat_scmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_scmpo_m$m_30_35 <- as.numeric(0)
out_scmpo_m$m_30_35 <- dat_scmpo_m$grocery_30_35_mins
out_scmpo_m$m_30_35_lts <- as.numeric(0)
out_scmpo_m$m_30_35_lts <- dat_scmpo_m$grocery_30_35_mins * (dat_scmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_scmpo_m$m_35_40 <- as.numeric(0)
out_scmpo_m$m_35_40 <- dat_scmpo_m$grocery_35_40_mins
out_scmpo_m$m_35_40_lts <- as.numeric(0)
out_scmpo_m$m_35_40_lts <- dat_scmpo_m$grocery_35_40_mins * (dat_scmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_scmpo_m$m_40_45 <- as.numeric(0)
out_scmpo_m$m_40_45 <- dat_scmpo_m$grocery_40_45_mins
out_scmpo_m$m_40_45_lts <- as.numeric(0)
out_scmpo_m$m_40_45_lts <- dat_scmpo_m$grocery_40_45_mins * (dat_scmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_scmpo_m$m_45_50 <- as.numeric(0)
out_scmpo_m$m_45_50 <- dat_scmpo_m$grocery_45_50_mins
out_scmpo_m$m_45_50_lts <- as.numeric(0)
out_scmpo_m$m_45_50_lts <- dat_scmpo_m$grocery_45_50_mins * (dat_scmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_scmpo_m$m_50_55 <- as.numeric(0)
out_scmpo_m$m_50_55 <- dat_scmpo_m$grocery_50_55_mins
out_scmpo_m$m_50_55_lts <- as.numeric(0)
out_scmpo_m$m_50_55_lts <- dat_scmpo_m$grocery_50_55_mins * (dat_scmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_scmpo_m$m_55_60 <- as.numeric(0)
out_scmpo_m$m_55_60 <- dat_scmpo_m$grocery_55_60_mins
out_scmpo_m$m_55_60_lts <- as.numeric(0)
out_scmpo_m$m_55_60_lts <- dat_scmpo_m$grocery_55_60_mins * (dat_scmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_scmpo_m)

# Export accessibility output 
write.dbf(out_scmpo_m, (file=paste(dat_dir, "access_eac_markets_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_scmpo_s <- read_csv(paste(dat_dir, "saf_scmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s <- dat_scmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s)
dat_scmpo_s[is.na(dat_scmpo_s)] <- 0; dat_scmpo_s <- dat_scmpo_s %>% arrange(From)
dat_scmpo_s_lts <- read_csv(paste(dat_dir, "saf_scmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_scmpo_s_lts <- dat_scmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_s_lts)
dat_scmpo_s_lts[is.na(dat_scmpo_s_lts)] <- 0; dat_scmpo_s_lts <- dat_scmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_s <- as.data.frame(dat_scmpo_s_lts)[1]
out_scmpo_s$s_00_05 <- as.numeric(0)
out_scmpo_s$s_00_05 <- dat_scmpo_s$school_00_05_mins
out_scmpo_s$s_00_05_lts <- as.numeric(0)
out_scmpo_s$s_00_05_lts <- dat_scmpo_s$school_00_05_mins * (dat_scmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_scmpo_s$s_05_10 <- as.numeric(0)
out_scmpo_s$s_05_10 <- dat_scmpo_s$school_05_10_mins
out_scmpo_s$s_05_10_lts <- as.numeric(0)
out_scmpo_s$s_05_10_lts <- dat_scmpo_s$school_05_10_mins * (dat_scmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_scmpo_s$s_10_15 <- as.numeric(0)
out_scmpo_s$s_10_15 <- dat_scmpo_s$school_10_15_mins
out_scmpo_s$s_10_15_lts <- as.numeric(0)
out_scmpo_s$s_10_15_lts <- dat_scmpo_s$school_10_15_mins * (dat_scmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_scmpo_s$s_15_20 <- as.numeric(0)
out_scmpo_s$s_15_20 <- dat_scmpo_s$school_15_20_mins
out_scmpo_s$s_15_20_lts <- as.numeric(0)
out_scmpo_s$s_15_20_lts <- dat_scmpo_s$school_15_20_mins * (dat_scmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_scmpo_s$s_20_25 <- as.numeric(0)
out_scmpo_s$s_20_25 <- dat_scmpo_s$school_20_25_mins
out_scmpo_s$s_20_25_lts <- as.numeric(0)
out_scmpo_s$s_20_25_lts <- dat_scmpo_s$school_20_25_mins * (dat_scmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_scmpo_s$s_25_30 <- as.numeric(0)
out_scmpo_s$s_25_30 <- dat_scmpo_s$school_25_30_mins
out_scmpo_s$s_25_30_lts <- as.numeric(0)
out_scmpo_s$s_25_30_lts <- dat_scmpo_s$school_25_30_mins * (dat_scmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_scmpo_s$s_30_35 <- as.numeric(0)
out_scmpo_s$s_30_35 <- dat_scmpo_s$school_30_35_mins
out_scmpo_s$s_30_35_lts <- as.numeric(0)
out_scmpo_s$s_30_35_lts <- dat_scmpo_s$school_30_35_mins * (dat_scmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_scmpo_s$s_35_40 <- as.numeric(0)
out_scmpo_s$s_35_40 <- dat_scmpo_s$school_35_40_mins
out_scmpo_s$s_35_40_lts <- as.numeric(0)
out_scmpo_s$s_35_40_lts <- dat_scmpo_s$school_35_40_mins * (dat_scmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_scmpo_s$s_40_45 <- as.numeric(0)
out_scmpo_s$s_40_45 <- dat_scmpo_s$school_40_45_mins
out_scmpo_s$s_40_45_lts <- as.numeric(0)
out_scmpo_s$s_40_45_lts <- dat_scmpo_s$school_40_45_mins * (dat_scmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_scmpo_s$s_45_50 <- as.numeric(0)
out_scmpo_s$s_45_50 <- dat_scmpo_s$school_45_50_mins
out_scmpo_s$s_45_50_lts <- as.numeric(0)
out_scmpo_s$s_45_50_lts <- dat_scmpo_s$school_45_50_mins * (dat_scmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_scmpo_s$s_50_55 <- as.numeric(0)
out_scmpo_s$s_50_55 <- dat_scmpo_s$school_50_55_mins
out_scmpo_s$s_50_55_lts <- as.numeric(0)
out_scmpo_s$s_50_55_lts <- dat_scmpo_s$school_50_55_mins * (dat_scmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_scmpo_s$s_55_60 <- as.numeric(0)
out_scmpo_s$s_55_60 <- dat_scmpo_s$school_55_60_mins
out_scmpo_s$s_55_60_lts <- as.numeric(0)
out_scmpo_s$s_55_60_lts <- dat_scmpo_s$school_55_60_mins * (dat_scmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_scmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_scmpo_s)

# Export accessibility output 
write.dbf(out_scmpo_s, (file=paste(dat_dir, "access_saf_schools_scmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_scmpo_m <- read_csv(paste(dat_dir, "saf_scmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m <- dat_scmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m)
dat_scmpo_m[is.na(dat_scmpo_m)] <- 0; dat_scmpo_m <- dat_scmpo_m %>% arrange(From)
dat_scmpo_m_lts <- read_csv(paste(dat_dir, "saf_scmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_scmpo_m_lts <- dat_scmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_scmpo_m_lts)
dat_scmpo_m_lts[is.na(dat_scmpo_m_lts)] <- 0; dat_scmpo_m_lts <- dat_scmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_scmpo_m <- as.data.frame(dat_scmpo_m_lts)[1]
out_scmpo_m$m_00_05 <- as.numeric(0)
out_scmpo_m$m_00_05 <- dat_scmpo_m$grocery_00_05_mins
out_scmpo_m$m_00_05_lts <- as.numeric(0)
out_scmpo_m$m_00_05_lts <- dat_scmpo_m$grocery_00_05_mins * (dat_scmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_scmpo_m$m_05_10 <- as.numeric(0)
out_scmpo_m$m_05_10 <- dat_scmpo_m$grocery_05_10_mins
out_scmpo_m$m_05_10_lts <- as.numeric(0)
out_scmpo_m$m_05_10_lts <- dat_scmpo_m$grocery_05_10_mins * (dat_scmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_scmpo_m$m_10_15 <- as.numeric(0)
out_scmpo_m$m_10_15 <- dat_scmpo_m$grocery_10_15_mins
out_scmpo_m$m_10_15_lts <- as.numeric(0)
out_scmpo_m$m_10_15_lts <- dat_scmpo_m$grocery_10_15_mins * (dat_scmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_scmpo_m$m_15_20 <- as.numeric(0)
out_scmpo_m$m_15_20 <- dat_scmpo_m$grocery_15_20_mins
out_scmpo_m$m_15_20_lts <- as.numeric(0)
out_scmpo_m$m_15_20_lts <- dat_scmpo_m$grocery_15_20_mins * (dat_scmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_scmpo_m$m_20_25 <- as.numeric(0)
out_scmpo_m$m_20_25 <- dat_scmpo_m$grocery_20_25_mins
out_scmpo_m$m_20_25_lts <- as.numeric(0)
out_scmpo_m$m_20_25_lts <- dat_scmpo_m$grocery_20_25_mins * (dat_scmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_scmpo_m$m_25_30 <- as.numeric(0)
out_scmpo_m$m_25_30 <- dat_scmpo_m$grocery_25_30_mins
out_scmpo_m$m_25_30_lts <- as.numeric(0)
out_scmpo_m$m_25_30_lts <- dat_scmpo_m$grocery_25_30_mins * (dat_scmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_scmpo_m$m_30_35 <- as.numeric(0)
out_scmpo_m$m_30_35 <- dat_scmpo_m$grocery_30_35_mins
out_scmpo_m$m_30_35_lts <- as.numeric(0)
out_scmpo_m$m_30_35_lts <- dat_scmpo_m$grocery_30_35_mins * (dat_scmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_scmpo_m$m_35_40 <- as.numeric(0)
out_scmpo_m$m_35_40 <- dat_scmpo_m$grocery_35_40_mins
out_scmpo_m$m_35_40_lts <- as.numeric(0)
out_scmpo_m$m_35_40_lts <- dat_scmpo_m$grocery_35_40_mins * (dat_scmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_scmpo_m$m_40_45 <- as.numeric(0)
out_scmpo_m$m_40_45 <- dat_scmpo_m$grocery_40_45_mins
out_scmpo_m$m_40_45_lts <- as.numeric(0)
out_scmpo_m$m_40_45_lts <- dat_scmpo_m$grocery_40_45_mins * (dat_scmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_scmpo_m$m_45_50 <- as.numeric(0)
out_scmpo_m$m_45_50 <- dat_scmpo_m$grocery_45_50_mins
out_scmpo_m$m_45_50_lts <- as.numeric(0)
out_scmpo_m$m_45_50_lts <- dat_scmpo_m$grocery_45_50_mins * (dat_scmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_scmpo_m$m_50_55 <- as.numeric(0)
out_scmpo_m$m_50_55 <- dat_scmpo_m$grocery_50_55_mins
out_scmpo_m$m_50_55_lts <- as.numeric(0)
out_scmpo_m$m_50_55_lts <- dat_scmpo_m$grocery_50_55_mins * (dat_scmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_scmpo_m$m_55_60 <- as.numeric(0)
out_scmpo_m$m_55_60 <- dat_scmpo_m$grocery_55_60_mins
out_scmpo_m$m_55_60_lts <- as.numeric(0)
out_scmpo_m$m_55_60_lts <- dat_scmpo_m$grocery_55_60_mins * (dat_scmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_scmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_scmpo_m)

# Export accessibility output 
write.dbf(out_scmpo_m, (file=paste(dat_dir, "access_saf_markets_scmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### SVMPO: Sierra Vista

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_svmpo_s <- read_csv(paste(dat_dir, "ibc_svmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s <- dat_svmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s)
dat_svmpo_s[is.na(dat_svmpo_s)] <- 0; dat_svmpo_s <- dat_svmpo_s %>% arrange(From)
dat_svmpo_s_lts <- read_csv(paste(dat_dir, "ibc_svmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s_lts <- dat_svmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s_lts)
dat_svmpo_s_lts[is.na(dat_svmpo_s_lts)] <- 0; dat_svmpo_s_lts <- dat_svmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_s <- as.data.frame(dat_svmpo_s_lts)[1]
out_svmpo_s$s_00_05 <- as.numeric(0)
out_svmpo_s$s_00_05 <- dat_svmpo_s$school_00_05_mins
out_svmpo_s$s_00_05_lts <- as.numeric(0)
out_svmpo_s$s_00_05_lts <- dat_svmpo_s$school_00_05_mins * (dat_svmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_svmpo_s$s_05_10 <- as.numeric(0)
out_svmpo_s$s_05_10 <- dat_svmpo_s$school_05_10_mins
out_svmpo_s$s_05_10_lts <- as.numeric(0)
out_svmpo_s$s_05_10_lts <- dat_svmpo_s$school_05_10_mins * (dat_svmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_svmpo_s$s_10_15 <- as.numeric(0)
out_svmpo_s$s_10_15 <- dat_svmpo_s$school_10_15_mins
out_svmpo_s$s_10_15_lts <- as.numeric(0)
out_svmpo_s$s_10_15_lts <- dat_svmpo_s$school_10_15_mins * (dat_svmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_svmpo_s$s_15_20 <- as.numeric(0)
out_svmpo_s$s_15_20 <- dat_svmpo_s$school_15_20_mins
out_svmpo_s$s_15_20_lts <- as.numeric(0)
out_svmpo_s$s_15_20_lts <- dat_svmpo_s$school_15_20_mins * (dat_svmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_svmpo_s$s_20_25 <- as.numeric(0)
out_svmpo_s$s_20_25 <- dat_svmpo_s$school_20_25_mins
out_svmpo_s$s_20_25_lts <- as.numeric(0)
out_svmpo_s$s_20_25_lts <- dat_svmpo_s$school_20_25_mins * (dat_svmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_svmpo_s$s_25_30 <- as.numeric(0)
out_svmpo_s$s_25_30 <- dat_svmpo_s$school_25_30_mins
out_svmpo_s$s_25_30_lts <- as.numeric(0)
out_svmpo_s$s_25_30_lts <- dat_svmpo_s$school_25_30_mins * (dat_svmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_svmpo_s$s_30_35 <- as.numeric(0)
out_svmpo_s$s_30_35 <- dat_svmpo_s$school_30_35_mins
out_svmpo_s$s_30_35_lts <- as.numeric(0)
out_svmpo_s$s_30_35_lts <- dat_svmpo_s$school_30_35_mins * (dat_svmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_svmpo_s$s_35_40 <- as.numeric(0)
out_svmpo_s$s_35_40 <- dat_svmpo_s$school_35_40_mins
out_svmpo_s$s_35_40_lts <- as.numeric(0)
out_svmpo_s$s_35_40_lts <- dat_svmpo_s$school_35_40_mins * (dat_svmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_svmpo_s$s_40_45 <- as.numeric(0)
out_svmpo_s$s_40_45 <- dat_svmpo_s$school_40_45_mins
out_svmpo_s$s_40_45_lts <- as.numeric(0)
out_svmpo_s$s_40_45_lts <- dat_svmpo_s$school_40_45_mins * (dat_svmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_svmpo_s$s_45_50 <- as.numeric(0)
out_svmpo_s$s_45_50 <- dat_svmpo_s$school_45_50_mins
out_svmpo_s$s_45_50_lts <- as.numeric(0)
out_svmpo_s$s_45_50_lts <- dat_svmpo_s$school_45_50_mins * (dat_svmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_svmpo_s$s_50_55 <- as.numeric(0)
out_svmpo_s$s_50_55 <- dat_svmpo_s$school_50_55_mins
out_svmpo_s$s_50_55_lts <- as.numeric(0)
out_svmpo_s$s_50_55_lts <- dat_svmpo_s$school_50_55_mins * (dat_svmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_svmpo_s$s_55_60 <- as.numeric(0)
out_svmpo_s$s_55_60 <- dat_svmpo_s$school_55_60_mins
out_svmpo_s$s_55_60_lts <- as.numeric(0)
out_svmpo_s$s_55_60_lts <- dat_svmpo_s$school_55_60_mins * (dat_svmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_svmpo_s)

# Export accessibility output 
write.dbf(out_svmpo_s, (file=paste(dat_dir, "access_ibc_schools_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_svmpo_m <- read_csv(paste(dat_dir, "ibc_svmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m <- dat_svmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m)
dat_svmpo_m[is.na(dat_svmpo_m)] <- 0; dat_svmpo_m <- dat_svmpo_m %>% arrange(From)
dat_svmpo_m_lts <- read_csv(paste(dat_dir, "ibc_svmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m_lts <- dat_svmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m_lts)
dat_svmpo_m_lts[is.na(dat_svmpo_m_lts)] <- 0; dat_svmpo_m_lts <- dat_svmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_m <- as.data.frame(dat_svmpo_m_lts)[1]
out_svmpo_m$m_00_05 <- as.numeric(0)
out_svmpo_m$m_00_05 <- dat_svmpo_m$grocery_00_05_mins
out_svmpo_m$m_00_05_lts <- as.numeric(0)
out_svmpo_m$m_00_05_lts <- dat_svmpo_m$grocery_00_05_mins * (dat_svmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_svmpo_m$m_05_10 <- as.numeric(0)
out_svmpo_m$m_05_10 <- dat_svmpo_m$grocery_05_10_mins
out_svmpo_m$m_05_10_lts <- as.numeric(0)
out_svmpo_m$m_05_10_lts <- dat_svmpo_m$grocery_05_10_mins * (dat_svmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_svmpo_m$m_10_15 <- as.numeric(0)
out_svmpo_m$m_10_15 <- dat_svmpo_m$grocery_10_15_mins
out_svmpo_m$m_10_15_lts <- as.numeric(0)
out_svmpo_m$m_10_15_lts <- dat_svmpo_m$grocery_10_15_mins * (dat_svmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_svmpo_m$m_15_20 <- as.numeric(0)
out_svmpo_m$m_15_20 <- dat_svmpo_m$grocery_15_20_mins
out_svmpo_m$m_15_20_lts <- as.numeric(0)
out_svmpo_m$m_15_20_lts <- dat_svmpo_m$grocery_15_20_mins * (dat_svmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_svmpo_m$m_20_25 <- as.numeric(0)
out_svmpo_m$m_20_25 <- dat_svmpo_m$grocery_20_25_mins
out_svmpo_m$m_20_25_lts <- as.numeric(0)
out_svmpo_m$m_20_25_lts <- dat_svmpo_m$grocery_20_25_mins * (dat_svmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_svmpo_m$m_25_30 <- as.numeric(0)
out_svmpo_m$m_25_30 <- dat_svmpo_m$grocery_25_30_mins
out_svmpo_m$m_25_30_lts <- as.numeric(0)
out_svmpo_m$m_25_30_lts <- dat_svmpo_m$grocery_25_30_mins * (dat_svmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_svmpo_m$m_30_35 <- as.numeric(0)
out_svmpo_m$m_30_35 <- dat_svmpo_m$grocery_30_35_mins
out_svmpo_m$m_30_35_lts <- as.numeric(0)
out_svmpo_m$m_30_35_lts <- dat_svmpo_m$grocery_30_35_mins * (dat_svmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_svmpo_m$m_35_40 <- as.numeric(0)
out_svmpo_m$m_35_40 <- dat_svmpo_m$grocery_35_40_mins
out_svmpo_m$m_35_40_lts <- as.numeric(0)
out_svmpo_m$m_35_40_lts <- dat_svmpo_m$grocery_35_40_mins * (dat_svmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_svmpo_m$m_40_45 <- as.numeric(0)
out_svmpo_m$m_40_45 <- dat_svmpo_m$grocery_40_45_mins
out_svmpo_m$m_40_45_lts <- as.numeric(0)
out_svmpo_m$m_40_45_lts <- dat_svmpo_m$grocery_40_45_mins * (dat_svmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_svmpo_m$m_45_50 <- as.numeric(0)
out_svmpo_m$m_45_50 <- dat_svmpo_m$grocery_45_50_mins
out_svmpo_m$m_45_50_lts <- as.numeric(0)
out_svmpo_m$m_45_50_lts <- dat_svmpo_m$grocery_45_50_mins * (dat_svmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_svmpo_m$m_50_55 <- as.numeric(0)
out_svmpo_m$m_50_55 <- dat_svmpo_m$grocery_50_55_mins
out_svmpo_m$m_50_55_lts <- as.numeric(0)
out_svmpo_m$m_50_55_lts <- dat_svmpo_m$grocery_50_55_mins * (dat_svmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_svmpo_m$m_55_60 <- as.numeric(0)
out_svmpo_m$m_55_60 <- dat_svmpo_m$grocery_55_60_mins
out_svmpo_m$m_55_60_lts <- as.numeric(0)
out_svmpo_m$m_55_60_lts <- dat_svmpo_m$grocery_55_60_mins * (dat_svmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_svmpo_m)

# Export accessibility output 
write.dbf(out_svmpo_m, (file=paste(dat_dir, "access_ibc_markets_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_svmpo_s <- read_csv(paste(dat_dir, "eac_svmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s <- dat_svmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s)
dat_svmpo_s[is.na(dat_svmpo_s)] <- 0; dat_svmpo_s <- dat_svmpo_s %>% arrange(From)
dat_svmpo_s_lts <- read_csv(paste(dat_dir, "eac_svmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s_lts <- dat_svmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s_lts)
dat_svmpo_s_lts[is.na(dat_svmpo_s_lts)] <- 0; dat_svmpo_s_lts <- dat_svmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_s <- as.data.frame(dat_svmpo_s_lts)[1]
out_svmpo_s$s_00_05 <- as.numeric(0)
out_svmpo_s$s_00_05 <- dat_svmpo_s$school_00_05_mins
out_svmpo_s$s_00_05_lts <- as.numeric(0)
out_svmpo_s$s_00_05_lts <- dat_svmpo_s$school_00_05_mins * (dat_svmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_svmpo_s$s_05_10 <- as.numeric(0)
out_svmpo_s$s_05_10 <- dat_svmpo_s$school_05_10_mins
out_svmpo_s$s_05_10_lts <- as.numeric(0)
out_svmpo_s$s_05_10_lts <- dat_svmpo_s$school_05_10_mins * (dat_svmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_svmpo_s$s_10_15 <- as.numeric(0)
out_svmpo_s$s_10_15 <- dat_svmpo_s$school_10_15_mins
out_svmpo_s$s_10_15_lts <- as.numeric(0)
out_svmpo_s$s_10_15_lts <- dat_svmpo_s$school_10_15_mins * (dat_svmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_svmpo_s$s_15_20 <- as.numeric(0)
out_svmpo_s$s_15_20 <- dat_svmpo_s$school_15_20_mins
out_svmpo_s$s_15_20_lts <- as.numeric(0)
out_svmpo_s$s_15_20_lts <- dat_svmpo_s$school_15_20_mins * (dat_svmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_svmpo_s$s_20_25 <- as.numeric(0)
out_svmpo_s$s_20_25 <- dat_svmpo_s$school_20_25_mins
out_svmpo_s$s_20_25_lts <- as.numeric(0)
out_svmpo_s$s_20_25_lts <- dat_svmpo_s$school_20_25_mins * (dat_svmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_svmpo_s$s_25_30 <- as.numeric(0)
out_svmpo_s$s_25_30 <- dat_svmpo_s$school_25_30_mins
out_svmpo_s$s_25_30_lts <- as.numeric(0)
out_svmpo_s$s_25_30_lts <- dat_svmpo_s$school_25_30_mins * (dat_svmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_svmpo_s$s_30_35 <- as.numeric(0)
out_svmpo_s$s_30_35 <- dat_svmpo_s$school_30_35_mins
out_svmpo_s$s_30_35_lts <- as.numeric(0)
out_svmpo_s$s_30_35_lts <- dat_svmpo_s$school_30_35_mins * (dat_svmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_svmpo_s$s_35_40 <- as.numeric(0)
out_svmpo_s$s_35_40 <- dat_svmpo_s$school_35_40_mins
out_svmpo_s$s_35_40_lts <- as.numeric(0)
out_svmpo_s$s_35_40_lts <- dat_svmpo_s$school_35_40_mins * (dat_svmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_svmpo_s$s_40_45 <- as.numeric(0)
out_svmpo_s$s_40_45 <- dat_svmpo_s$school_40_45_mins
out_svmpo_s$s_40_45_lts <- as.numeric(0)
out_svmpo_s$s_40_45_lts <- dat_svmpo_s$school_40_45_mins * (dat_svmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_svmpo_s$s_45_50 <- as.numeric(0)
out_svmpo_s$s_45_50 <- dat_svmpo_s$school_45_50_mins
out_svmpo_s$s_45_50_lts <- as.numeric(0)
out_svmpo_s$s_45_50_lts <- dat_svmpo_s$school_45_50_mins * (dat_svmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_svmpo_s$s_50_55 <- as.numeric(0)
out_svmpo_s$s_50_55 <- dat_svmpo_s$school_50_55_mins
out_svmpo_s$s_50_55_lts <- as.numeric(0)
out_svmpo_s$s_50_55_lts <- dat_svmpo_s$school_50_55_mins * (dat_svmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_svmpo_s$s_55_60 <- as.numeric(0)
out_svmpo_s$s_55_60 <- dat_svmpo_s$school_55_60_mins
out_svmpo_s$s_55_60_lts <- as.numeric(0)
out_svmpo_s$s_55_60_lts <- dat_svmpo_s$school_55_60_mins * (dat_svmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_svmpo_s)

# Export accessibility output 
write.dbf(out_svmpo_s, (file=paste(dat_dir, "access_eac_schools_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_svmpo_m <- read_csv(paste(dat_dir, "eac_svmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m <- dat_svmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m)
dat_svmpo_m[is.na(dat_svmpo_m)] <- 0; dat_svmpo_m <- dat_svmpo_m %>% arrange(From)
dat_svmpo_m_lts <- read_csv(paste(dat_dir, "eac_svmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m_lts <- dat_svmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m_lts)
dat_svmpo_m_lts[is.na(dat_svmpo_m_lts)] <- 0; dat_svmpo_m_lts <- dat_svmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_m <- as.data.frame(dat_svmpo_m_lts)[1]
out_svmpo_m$m_00_05 <- as.numeric(0)
out_svmpo_m$m_00_05 <- dat_svmpo_m$grocery_00_05_mins
out_svmpo_m$m_00_05_lts <- as.numeric(0)
out_svmpo_m$m_00_05_lts <- dat_svmpo_m$grocery_00_05_mins * (dat_svmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_svmpo_m$m_05_10 <- as.numeric(0)
out_svmpo_m$m_05_10 <- dat_svmpo_m$grocery_05_10_mins
out_svmpo_m$m_05_10_lts <- as.numeric(0)
out_svmpo_m$m_05_10_lts <- dat_svmpo_m$grocery_05_10_mins * (dat_svmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_svmpo_m$m_10_15 <- as.numeric(0)
out_svmpo_m$m_10_15 <- dat_svmpo_m$grocery_10_15_mins
out_svmpo_m$m_10_15_lts <- as.numeric(0)
out_svmpo_m$m_10_15_lts <- dat_svmpo_m$grocery_10_15_mins * (dat_svmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_svmpo_m$m_15_20 <- as.numeric(0)
out_svmpo_m$m_15_20 <- dat_svmpo_m$grocery_15_20_mins
out_svmpo_m$m_15_20_lts <- as.numeric(0)
out_svmpo_m$m_15_20_lts <- dat_svmpo_m$grocery_15_20_mins * (dat_svmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_svmpo_m$m_20_25 <- as.numeric(0)
out_svmpo_m$m_20_25 <- dat_svmpo_m$grocery_20_25_mins
out_svmpo_m$m_20_25_lts <- as.numeric(0)
out_svmpo_m$m_20_25_lts <- dat_svmpo_m$grocery_20_25_mins * (dat_svmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_svmpo_m$m_25_30 <- as.numeric(0)
out_svmpo_m$m_25_30 <- dat_svmpo_m$grocery_25_30_mins
out_svmpo_m$m_25_30_lts <- as.numeric(0)
out_svmpo_m$m_25_30_lts <- dat_svmpo_m$grocery_25_30_mins * (dat_svmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_svmpo_m$m_30_35 <- as.numeric(0)
out_svmpo_m$m_30_35 <- dat_svmpo_m$grocery_30_35_mins
out_svmpo_m$m_30_35_lts <- as.numeric(0)
out_svmpo_m$m_30_35_lts <- dat_svmpo_m$grocery_30_35_mins * (dat_svmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_svmpo_m$m_35_40 <- as.numeric(0)
out_svmpo_m$m_35_40 <- dat_svmpo_m$grocery_35_40_mins
out_svmpo_m$m_35_40_lts <- as.numeric(0)
out_svmpo_m$m_35_40_lts <- dat_svmpo_m$grocery_35_40_mins * (dat_svmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_svmpo_m$m_40_45 <- as.numeric(0)
out_svmpo_m$m_40_45 <- dat_svmpo_m$grocery_40_45_mins
out_svmpo_m$m_40_45_lts <- as.numeric(0)
out_svmpo_m$m_40_45_lts <- dat_svmpo_m$grocery_40_45_mins * (dat_svmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_svmpo_m$m_45_50 <- as.numeric(0)
out_svmpo_m$m_45_50 <- dat_svmpo_m$grocery_45_50_mins
out_svmpo_m$m_45_50_lts <- as.numeric(0)
out_svmpo_m$m_45_50_lts <- dat_svmpo_m$grocery_45_50_mins * (dat_svmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_svmpo_m$m_50_55 <- as.numeric(0)
out_svmpo_m$m_50_55 <- dat_svmpo_m$grocery_50_55_mins
out_svmpo_m$m_50_55_lts <- as.numeric(0)
out_svmpo_m$m_50_55_lts <- dat_svmpo_m$grocery_50_55_mins * (dat_svmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_svmpo_m$m_55_60 <- as.numeric(0)
out_svmpo_m$m_55_60 <- dat_svmpo_m$grocery_55_60_mins
out_svmpo_m$m_55_60_lts <- as.numeric(0)
out_svmpo_m$m_55_60_lts <- dat_svmpo_m$grocery_55_60_mins * (dat_svmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_svmpo_m)

# Export accessibility output 
write.dbf(out_svmpo_m, (file=paste(dat_dir, "access_eac_markets_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_svmpo_s <- read_csv(paste(dat_dir, "saf_svmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s <- dat_svmpo_s %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s)
dat_svmpo_s[is.na(dat_svmpo_s)] <- 0; dat_svmpo_s <- dat_svmpo_s %>% arrange(From)
dat_svmpo_s_lts <- read_csv(paste(dat_dir, "saf_svmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_svmpo_s_lts <- dat_svmpo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_s_lts)
dat_svmpo_s_lts[is.na(dat_svmpo_s_lts)] <- 0; dat_svmpo_s_lts <- dat_svmpo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_s <- as.data.frame(dat_svmpo_s_lts)[1]
out_svmpo_s$s_00_05 <- as.numeric(0)
out_svmpo_s$s_00_05 <- dat_svmpo_s$school_00_05_mins
out_svmpo_s$s_00_05_lts <- as.numeric(0)
out_svmpo_s$s_00_05_lts <- dat_svmpo_s$school_00_05_mins * (dat_svmpo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_00_05_mins_lts2%`/100)
out_svmpo_s$s_05_10 <- as.numeric(0)
out_svmpo_s$s_05_10 <- dat_svmpo_s$school_05_10_mins
out_svmpo_s$s_05_10_lts <- as.numeric(0)
out_svmpo_s$s_05_10_lts <- dat_svmpo_s$school_05_10_mins * (dat_svmpo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_05_10_mins_lts2%`/100)
out_svmpo_s$s_10_15 <- as.numeric(0)
out_svmpo_s$s_10_15 <- dat_svmpo_s$school_10_15_mins
out_svmpo_s$s_10_15_lts <- as.numeric(0)
out_svmpo_s$s_10_15_lts <- dat_svmpo_s$school_10_15_mins * (dat_svmpo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_10_15_mins_lts2%`/100)
out_svmpo_s$s_15_20 <- as.numeric(0)
out_svmpo_s$s_15_20 <- dat_svmpo_s$school_15_20_mins
out_svmpo_s$s_15_20_lts <- as.numeric(0)
out_svmpo_s$s_15_20_lts <- dat_svmpo_s$school_15_20_mins * (dat_svmpo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_15_20_mins_lts2%`/100)
out_svmpo_s$s_20_25 <- as.numeric(0)
out_svmpo_s$s_20_25 <- dat_svmpo_s$school_20_25_mins
out_svmpo_s$s_20_25_lts <- as.numeric(0)
out_svmpo_s$s_20_25_lts <- dat_svmpo_s$school_20_25_mins * (dat_svmpo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_20_25_mins_lts2%`/100)
out_svmpo_s$s_25_30 <- as.numeric(0)
out_svmpo_s$s_25_30 <- dat_svmpo_s$school_25_30_mins
out_svmpo_s$s_25_30_lts <- as.numeric(0)
out_svmpo_s$s_25_30_lts <- dat_svmpo_s$school_25_30_mins * (dat_svmpo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_25_30_mins_lts2%`/100)
out_svmpo_s$s_30_35 <- as.numeric(0)
out_svmpo_s$s_30_35 <- dat_svmpo_s$school_30_35_mins
out_svmpo_s$s_30_35_lts <- as.numeric(0)
out_svmpo_s$s_30_35_lts <- dat_svmpo_s$school_30_35_mins * (dat_svmpo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_30_35_mins_lts2%`/100)
out_svmpo_s$s_35_40 <- as.numeric(0)
out_svmpo_s$s_35_40 <- dat_svmpo_s$school_35_40_mins
out_svmpo_s$s_35_40_lts <- as.numeric(0)
out_svmpo_s$s_35_40_lts <- dat_svmpo_s$school_35_40_mins * (dat_svmpo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_35_40_mins_lts2%`/100)
out_svmpo_s$s_40_45 <- as.numeric(0)
out_svmpo_s$s_40_45 <- dat_svmpo_s$school_40_45_mins
out_svmpo_s$s_40_45_lts <- as.numeric(0)
out_svmpo_s$s_40_45_lts <- dat_svmpo_s$school_40_45_mins * (dat_svmpo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_40_45_mins_lts2%`/100)
out_svmpo_s$s_45_50 <- as.numeric(0)
out_svmpo_s$s_45_50 <- dat_svmpo_s$school_45_50_mins
out_svmpo_s$s_45_50_lts <- as.numeric(0)
out_svmpo_s$s_45_50_lts <- dat_svmpo_s$school_45_50_mins * (dat_svmpo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_45_50_mins_lts2%`/100)
out_svmpo_s$s_50_55 <- as.numeric(0)
out_svmpo_s$s_50_55 <- dat_svmpo_s$school_50_55_mins
out_svmpo_s$s_50_55_lts <- as.numeric(0)
out_svmpo_s$s_50_55_lts <- dat_svmpo_s$school_50_55_mins * (dat_svmpo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_50_55_mins_lts2%`/100)
out_svmpo_s$s_55_60 <- as.numeric(0)
out_svmpo_s$s_55_60 <- dat_svmpo_s$school_55_60_mins
out_svmpo_s$s_55_60_lts <- as.numeric(0)
out_svmpo_s$s_55_60_lts <- dat_svmpo_s$school_55_60_mins * (dat_svmpo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_svmpo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_svmpo_s)

# Export accessibility output 
write.dbf(out_svmpo_s, (file=paste(dat_dir, "access_saf_schools_svmpo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_svmpo_m <- read_csv(paste(dat_dir, "saf_svmpo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m <- dat_svmpo_m %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m)
dat_svmpo_m[is.na(dat_svmpo_m)] <- 0; dat_svmpo_m <- dat_svmpo_m %>% arrange(From)
dat_svmpo_m_lts <- read_csv(paste(dat_dir, "saf_svmpo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_svmpo_m_lts <- dat_svmpo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_svmpo_m_lts)
dat_svmpo_m_lts[is.na(dat_svmpo_m_lts)] <- 0; dat_svmpo_m_lts <- dat_svmpo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_svmpo_m <- as.data.frame(dat_svmpo_m_lts)[1]
out_svmpo_m$m_00_05 <- as.numeric(0)
out_svmpo_m$m_00_05 <- dat_svmpo_m$grocery_00_05_mins
out_svmpo_m$m_00_05_lts <- as.numeric(0)
out_svmpo_m$m_00_05_lts <- dat_svmpo_m$grocery_00_05_mins * (dat_svmpo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_svmpo_m$m_05_10 <- as.numeric(0)
out_svmpo_m$m_05_10 <- dat_svmpo_m$grocery_05_10_mins
out_svmpo_m$m_05_10_lts <- as.numeric(0)
out_svmpo_m$m_05_10_lts <- dat_svmpo_m$grocery_05_10_mins * (dat_svmpo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_svmpo_m$m_10_15 <- as.numeric(0)
out_svmpo_m$m_10_15 <- dat_svmpo_m$grocery_10_15_mins
out_svmpo_m$m_10_15_lts <- as.numeric(0)
out_svmpo_m$m_10_15_lts <- dat_svmpo_m$grocery_10_15_mins * (dat_svmpo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_svmpo_m$m_15_20 <- as.numeric(0)
out_svmpo_m$m_15_20 <- dat_svmpo_m$grocery_15_20_mins
out_svmpo_m$m_15_20_lts <- as.numeric(0)
out_svmpo_m$m_15_20_lts <- dat_svmpo_m$grocery_15_20_mins * (dat_svmpo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_svmpo_m$m_20_25 <- as.numeric(0)
out_svmpo_m$m_20_25 <- dat_svmpo_m$grocery_20_25_mins
out_svmpo_m$m_20_25_lts <- as.numeric(0)
out_svmpo_m$m_20_25_lts <- dat_svmpo_m$grocery_20_25_mins * (dat_svmpo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_svmpo_m$m_25_30 <- as.numeric(0)
out_svmpo_m$m_25_30 <- dat_svmpo_m$grocery_25_30_mins
out_svmpo_m$m_25_30_lts <- as.numeric(0)
out_svmpo_m$m_25_30_lts <- dat_svmpo_m$grocery_25_30_mins * (dat_svmpo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_svmpo_m$m_30_35 <- as.numeric(0)
out_svmpo_m$m_30_35 <- dat_svmpo_m$grocery_30_35_mins
out_svmpo_m$m_30_35_lts <- as.numeric(0)
out_svmpo_m$m_30_35_lts <- dat_svmpo_m$grocery_30_35_mins * (dat_svmpo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_svmpo_m$m_35_40 <- as.numeric(0)
out_svmpo_m$m_35_40 <- dat_svmpo_m$grocery_35_40_mins
out_svmpo_m$m_35_40_lts <- as.numeric(0)
out_svmpo_m$m_35_40_lts <- dat_svmpo_m$grocery_35_40_mins * (dat_svmpo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_svmpo_m$m_40_45 <- as.numeric(0)
out_svmpo_m$m_40_45 <- dat_svmpo_m$grocery_40_45_mins
out_svmpo_m$m_40_45_lts <- as.numeric(0)
out_svmpo_m$m_40_45_lts <- dat_svmpo_m$grocery_40_45_mins * (dat_svmpo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_svmpo_m$m_45_50 <- as.numeric(0)
out_svmpo_m$m_45_50 <- dat_svmpo_m$grocery_45_50_mins
out_svmpo_m$m_45_50_lts <- as.numeric(0)
out_svmpo_m$m_45_50_lts <- dat_svmpo_m$grocery_45_50_mins * (dat_svmpo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_svmpo_m$m_50_55 <- as.numeric(0)
out_svmpo_m$m_50_55 <- dat_svmpo_m$grocery_50_55_mins
out_svmpo_m$m_50_55_lts <- as.numeric(0)
out_svmpo_m$m_50_55_lts <- dat_svmpo_m$grocery_50_55_mins * (dat_svmpo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_svmpo_m$m_55_60 <- as.numeric(0)
out_svmpo_m$m_55_60 <- dat_svmpo_m$grocery_55_60_mins
out_svmpo_m$m_55_60_lts <- as.numeric(0)
out_svmpo_m$m_55_60_lts <- dat_svmpo_m$grocery_55_60_mins * (dat_svmpo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_svmpo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_svmpo_m)

# Export accessibility output 
write.dbf(out_svmpo_m, (file=paste(dat_dir, "access_saf_markets_svmpo.dbf", sep="_data/_tabular/_outputs/")))

####################
### YMPO: Yuma

### INTERESTED BUT CONCERNED

### Schools

# Import and clean cranc output data
dat_ympo_s <- read_csv(paste(dat_dir, "ibc_ympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s <- dat_ympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s)
dat_ympo_s[is.na(dat_ympo_s)] <- 0; dat_ympo_s <- dat_ympo_s %>% arrange(From)
dat_ympo_s_lts <- read_csv(paste(dat_dir, "ibc_ympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s_lts <- dat_ympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s_lts)
dat_ympo_s_lts[is.na(dat_ympo_s_lts)] <- 0; dat_ympo_s_lts <- dat_ympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_s <- as.data.frame(dat_ympo_s_lts)[1]
out_ympo_s$s_00_05 <- as.numeric(0)
out_ympo_s$s_00_05 <- dat_ympo_s$school_00_05_mins
out_ympo_s$s_00_05_lts <- as.numeric(0)
out_ympo_s$s_00_05_lts <- dat_ympo_s$school_00_05_mins * (dat_ympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_ympo_s_lts$`school_00_05_mins_lts2%`/100)
out_ympo_s$s_05_10 <- as.numeric(0)
out_ympo_s$s_05_10 <- dat_ympo_s$school_05_10_mins
out_ympo_s$s_05_10_lts <- as.numeric(0)
out_ympo_s$s_05_10_lts <- dat_ympo_s$school_05_10_mins * (dat_ympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_ympo_s_lts$`school_05_10_mins_lts2%`/100)
out_ympo_s$s_10_15 <- as.numeric(0)
out_ympo_s$s_10_15 <- dat_ympo_s$school_10_15_mins
out_ympo_s$s_10_15_lts <- as.numeric(0)
out_ympo_s$s_10_15_lts <- dat_ympo_s$school_10_15_mins * (dat_ympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_ympo_s_lts$`school_10_15_mins_lts2%`/100)
out_ympo_s$s_15_20 <- as.numeric(0)
out_ympo_s$s_15_20 <- dat_ympo_s$school_15_20_mins
out_ympo_s$s_15_20_lts <- as.numeric(0)
out_ympo_s$s_15_20_lts <- dat_ympo_s$school_15_20_mins * (dat_ympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_ympo_s_lts$`school_15_20_mins_lts2%`/100)
out_ympo_s$s_20_25 <- as.numeric(0)
out_ympo_s$s_20_25 <- dat_ympo_s$school_20_25_mins
out_ympo_s$s_20_25_lts <- as.numeric(0)
out_ympo_s$s_20_25_lts <- dat_ympo_s$school_20_25_mins * (dat_ympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_ympo_s_lts$`school_20_25_mins_lts2%`/100)
out_ympo_s$s_25_30 <- as.numeric(0)
out_ympo_s$s_25_30 <- dat_ympo_s$school_25_30_mins
out_ympo_s$s_25_30_lts <- as.numeric(0)
out_ympo_s$s_25_30_lts <- dat_ympo_s$school_25_30_mins * (dat_ympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_ympo_s_lts$`school_25_30_mins_lts2%`/100)
out_ympo_s$s_30_35 <- as.numeric(0)
out_ympo_s$s_30_35 <- dat_ympo_s$school_30_35_mins
out_ympo_s$s_30_35_lts <- as.numeric(0)
out_ympo_s$s_30_35_lts <- dat_ympo_s$school_30_35_mins * (dat_ympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_ympo_s_lts$`school_30_35_mins_lts2%`/100)
out_ympo_s$s_35_40 <- as.numeric(0)
out_ympo_s$s_35_40 <- dat_ympo_s$school_35_40_mins
out_ympo_s$s_35_40_lts <- as.numeric(0)
out_ympo_s$s_35_40_lts <- dat_ympo_s$school_35_40_mins * (dat_ympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_ympo_s_lts$`school_35_40_mins_lts2%`/100)
out_ympo_s$s_40_45 <- as.numeric(0)
out_ympo_s$s_40_45 <- dat_ympo_s$school_40_45_mins
out_ympo_s$s_40_45_lts <- as.numeric(0)
out_ympo_s$s_40_45_lts <- dat_ympo_s$school_40_45_mins * (dat_ympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_ympo_s_lts$`school_40_45_mins_lts2%`/100)
out_ympo_s$s_45_50 <- as.numeric(0)
out_ympo_s$s_45_50 <- dat_ympo_s$school_45_50_mins
out_ympo_s$s_45_50_lts <- as.numeric(0)
out_ympo_s$s_45_50_lts <- dat_ympo_s$school_45_50_mins * (dat_ympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_ympo_s_lts$`school_45_50_mins_lts2%`/100)
out_ympo_s$s_50_55 <- as.numeric(0)
out_ympo_s$s_50_55 <- dat_ympo_s$school_50_55_mins
out_ympo_s$s_50_55_lts <- as.numeric(0)
out_ympo_s$s_50_55_lts <- dat_ympo_s$school_50_55_mins * (dat_ympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_ympo_s_lts$`school_50_55_mins_lts2%`/100)
out_ympo_s$s_55_60 <- as.numeric(0)
out_ympo_s$s_55_60 <- dat_ympo_s$school_55_60_mins
out_ympo_s$s_55_60_lts <- as.numeric(0)
out_ympo_s$s_55_60_lts <- dat_ympo_s$school_55_60_mins * (dat_ympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_ympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_ympo_s)

# Export accessibility output 
write.dbf(out_ympo_s, (file=paste(dat_dir, "access_ibc_schools_ympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_ympo_m <- read_csv(paste(dat_dir, "ibc_ympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m <- dat_ympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m)
dat_ympo_m[is.na(dat_ympo_m)] <- 0; dat_ympo_m <- dat_ympo_m %>% arrange(From)
dat_ympo_m_lts <- read_csv(paste(dat_dir, "ibc_ympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m_lts <- dat_ympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m_lts)
dat_ympo_m_lts[is.na(dat_ympo_m_lts)] <- 0; dat_ympo_m_lts <- dat_ympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_m <- as.data.frame(dat_ympo_m_lts)[1]
out_ympo_m$m_00_05 <- as.numeric(0)
out_ympo_m$m_00_05 <- dat_ympo_m$grocery_00_05_mins
out_ympo_m$m_00_05_lts <- as.numeric(0)
out_ympo_m$m_00_05_lts <- dat_ympo_m$grocery_00_05_mins * (dat_ympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_ympo_m$m_05_10 <- as.numeric(0)
out_ympo_m$m_05_10 <- dat_ympo_m$grocery_05_10_mins
out_ympo_m$m_05_10_lts <- as.numeric(0)
out_ympo_m$m_05_10_lts <- dat_ympo_m$grocery_05_10_mins * (dat_ympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_ympo_m$m_10_15 <- as.numeric(0)
out_ympo_m$m_10_15 <- dat_ympo_m$grocery_10_15_mins
out_ympo_m$m_10_15_lts <- as.numeric(0)
out_ympo_m$m_10_15_lts <- dat_ympo_m$grocery_10_15_mins * (dat_ympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_ympo_m$m_15_20 <- as.numeric(0)
out_ympo_m$m_15_20 <- dat_ympo_m$grocery_15_20_mins
out_ympo_m$m_15_20_lts <- as.numeric(0)
out_ympo_m$m_15_20_lts <- dat_ympo_m$grocery_15_20_mins * (dat_ympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_ympo_m$m_20_25 <- as.numeric(0)
out_ympo_m$m_20_25 <- dat_ympo_m$grocery_20_25_mins
out_ympo_m$m_20_25_lts <- as.numeric(0)
out_ympo_m$m_20_25_lts <- dat_ympo_m$grocery_20_25_mins * (dat_ympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_ympo_m$m_25_30 <- as.numeric(0)
out_ympo_m$m_25_30 <- dat_ympo_m$grocery_25_30_mins
out_ympo_m$m_25_30_lts <- as.numeric(0)
out_ympo_m$m_25_30_lts <- dat_ympo_m$grocery_25_30_mins * (dat_ympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_ympo_m$m_30_35 <- as.numeric(0)
out_ympo_m$m_30_35 <- dat_ympo_m$grocery_30_35_mins
out_ympo_m$m_30_35_lts <- as.numeric(0)
out_ympo_m$m_30_35_lts <- dat_ympo_m$grocery_30_35_mins * (dat_ympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_ympo_m$m_35_40 <- as.numeric(0)
out_ympo_m$m_35_40 <- dat_ympo_m$grocery_35_40_mins
out_ympo_m$m_35_40_lts <- as.numeric(0)
out_ympo_m$m_35_40_lts <- dat_ympo_m$grocery_35_40_mins * (dat_ympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_ympo_m$m_40_45 <- as.numeric(0)
out_ympo_m$m_40_45 <- dat_ympo_m$grocery_40_45_mins
out_ympo_m$m_40_45_lts <- as.numeric(0)
out_ympo_m$m_40_45_lts <- dat_ympo_m$grocery_40_45_mins * (dat_ympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_ympo_m$m_45_50 <- as.numeric(0)
out_ympo_m$m_45_50 <- dat_ympo_m$grocery_45_50_mins
out_ympo_m$m_45_50_lts <- as.numeric(0)
out_ympo_m$m_45_50_lts <- dat_ympo_m$grocery_45_50_mins * (dat_ympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_ympo_m$m_50_55 <- as.numeric(0)
out_ympo_m$m_50_55 <- dat_ympo_m$grocery_50_55_mins
out_ympo_m$m_50_55_lts <- as.numeric(0)
out_ympo_m$m_50_55_lts <- dat_ympo_m$grocery_50_55_mins * (dat_ympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_ympo_m$m_55_60 <- as.numeric(0)
out_ympo_m$m_55_60 <- dat_ympo_m$grocery_55_60_mins
out_ympo_m$m_55_60_lts <- as.numeric(0)
out_ympo_m$m_55_60_lts <- dat_ympo_m$grocery_55_60_mins * (dat_ympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_ympo_m)

# Export accessibility output 
write.dbf(out_ympo_m, (file=paste(dat_dir, "access_ibc_markets_ympo.dbf", sep="_data/_tabular/_outputs/")))

### ENTHUSED AND CONFIDENT

### Schools

# Import and clean cranc output data
dat_ympo_s <- read_csv(paste(dat_dir, "eac_ympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s <- dat_ympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s)
dat_ympo_s[is.na(dat_ympo_s)] <- 0; dat_ympo_s <- dat_ympo_s %>% arrange(From)
dat_ympo_s_lts <- read_csv(paste(dat_dir, "eac_ympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s_lts <- dat_ympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s_lts)
dat_ympo_s_lts[is.na(dat_ympo_s_lts)] <- 0; dat_ympo_s_lts <- dat_ympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_s <- as.data.frame(dat_ympo_s_lts)[1]
out_ympo_s$s_00_05 <- as.numeric(0)
out_ympo_s$s_00_05 <- dat_ympo_s$school_00_05_mins
out_ympo_s$s_00_05_lts <- as.numeric(0)
out_ympo_s$s_00_05_lts <- dat_ympo_s$school_00_05_mins * (dat_ympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_ympo_s_lts$`school_00_05_mins_lts2%`/100)
out_ympo_s$s_05_10 <- as.numeric(0)
out_ympo_s$s_05_10 <- dat_ympo_s$school_05_10_mins
out_ympo_s$s_05_10_lts <- as.numeric(0)
out_ympo_s$s_05_10_lts <- dat_ympo_s$school_05_10_mins * (dat_ympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_ympo_s_lts$`school_05_10_mins_lts2%`/100)
out_ympo_s$s_10_15 <- as.numeric(0)
out_ympo_s$s_10_15 <- dat_ympo_s$school_10_15_mins
out_ympo_s$s_10_15_lts <- as.numeric(0)
out_ympo_s$s_10_15_lts <- dat_ympo_s$school_10_15_mins * (dat_ympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_ympo_s_lts$`school_10_15_mins_lts2%`/100)
out_ympo_s$s_15_20 <- as.numeric(0)
out_ympo_s$s_15_20 <- dat_ympo_s$school_15_20_mins
out_ympo_s$s_15_20_lts <- as.numeric(0)
out_ympo_s$s_15_20_lts <- dat_ympo_s$school_15_20_mins * (dat_ympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_ympo_s_lts$`school_15_20_mins_lts2%`/100)
out_ympo_s$s_20_25 <- as.numeric(0)
out_ympo_s$s_20_25 <- dat_ympo_s$school_20_25_mins
out_ympo_s$s_20_25_lts <- as.numeric(0)
out_ympo_s$s_20_25_lts <- dat_ympo_s$school_20_25_mins * (dat_ympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_ympo_s_lts$`school_20_25_mins_lts2%`/100)
out_ympo_s$s_25_30 <- as.numeric(0)
out_ympo_s$s_25_30 <- dat_ympo_s$school_25_30_mins
out_ympo_s$s_25_30_lts <- as.numeric(0)
out_ympo_s$s_25_30_lts <- dat_ympo_s$school_25_30_mins * (dat_ympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_ympo_s_lts$`school_25_30_mins_lts2%`/100)
out_ympo_s$s_30_35 <- as.numeric(0)
out_ympo_s$s_30_35 <- dat_ympo_s$school_30_35_mins
out_ympo_s$s_30_35_lts <- as.numeric(0)
out_ympo_s$s_30_35_lts <- dat_ympo_s$school_30_35_mins * (dat_ympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_ympo_s_lts$`school_30_35_mins_lts2%`/100)
out_ympo_s$s_35_40 <- as.numeric(0)
out_ympo_s$s_35_40 <- dat_ympo_s$school_35_40_mins
out_ympo_s$s_35_40_lts <- as.numeric(0)
out_ympo_s$s_35_40_lts <- dat_ympo_s$school_35_40_mins * (dat_ympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_ympo_s_lts$`school_35_40_mins_lts2%`/100)
out_ympo_s$s_40_45 <- as.numeric(0)
out_ympo_s$s_40_45 <- dat_ympo_s$school_40_45_mins
out_ympo_s$s_40_45_lts <- as.numeric(0)
out_ympo_s$s_40_45_lts <- dat_ympo_s$school_40_45_mins * (dat_ympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_ympo_s_lts$`school_40_45_mins_lts2%`/100)
out_ympo_s$s_45_50 <- as.numeric(0)
out_ympo_s$s_45_50 <- dat_ympo_s$school_45_50_mins
out_ympo_s$s_45_50_lts <- as.numeric(0)
out_ympo_s$s_45_50_lts <- dat_ympo_s$school_45_50_mins * (dat_ympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_ympo_s_lts$`school_45_50_mins_lts2%`/100)
out_ympo_s$s_50_55 <- as.numeric(0)
out_ympo_s$s_50_55 <- dat_ympo_s$school_50_55_mins
out_ympo_s$s_50_55_lts <- as.numeric(0)
out_ympo_s$s_50_55_lts <- dat_ympo_s$school_50_55_mins * (dat_ympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_ympo_s_lts$`school_50_55_mins_lts2%`/100)
out_ympo_s$s_55_60 <- as.numeric(0)
out_ympo_s$s_55_60 <- dat_ympo_s$school_55_60_mins
out_ympo_s$s_55_60_lts <- as.numeric(0)
out_ympo_s$s_55_60_lts <- dat_ympo_s$school_55_60_mins * (dat_ympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_ympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_ympo_s)

# Export accessibility output 
write.dbf(out_ympo_s, (file=paste(dat_dir, "access_eac_schools_ympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_ympo_m <- read_csv(paste(dat_dir, "eac_ympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m <- dat_ympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m)
dat_ympo_m[is.na(dat_ympo_m)] <- 0; dat_ympo_m <- dat_ympo_m %>% arrange(From)
dat_ympo_m_lts <- read_csv(paste(dat_dir, "eac_ympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m_lts <- dat_ympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m_lts)
dat_ympo_m_lts[is.na(dat_ympo_m_lts)] <- 0; dat_ympo_m_lts <- dat_ympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_m <- as.data.frame(dat_ympo_m_lts)[1]
out_ympo_m$m_00_05 <- as.numeric(0)
out_ympo_m$m_00_05 <- dat_ympo_m$grocery_00_05_mins
out_ympo_m$m_00_05_lts <- as.numeric(0)
out_ympo_m$m_00_05_lts <- dat_ympo_m$grocery_00_05_mins * (dat_ympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_ympo_m$m_05_10 <- as.numeric(0)
out_ympo_m$m_05_10 <- dat_ympo_m$grocery_05_10_mins
out_ympo_m$m_05_10_lts <- as.numeric(0)
out_ympo_m$m_05_10_lts <- dat_ympo_m$grocery_05_10_mins * (dat_ympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_ympo_m$m_10_15 <- as.numeric(0)
out_ympo_m$m_10_15 <- dat_ympo_m$grocery_10_15_mins
out_ympo_m$m_10_15_lts <- as.numeric(0)
out_ympo_m$m_10_15_lts <- dat_ympo_m$grocery_10_15_mins * (dat_ympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_ympo_m$m_15_20 <- as.numeric(0)
out_ympo_m$m_15_20 <- dat_ympo_m$grocery_15_20_mins
out_ympo_m$m_15_20_lts <- as.numeric(0)
out_ympo_m$m_15_20_lts <- dat_ympo_m$grocery_15_20_mins * (dat_ympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_ympo_m$m_20_25 <- as.numeric(0)
out_ympo_m$m_20_25 <- dat_ympo_m$grocery_20_25_mins
out_ympo_m$m_20_25_lts <- as.numeric(0)
out_ympo_m$m_20_25_lts <- dat_ympo_m$grocery_20_25_mins * (dat_ympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_ympo_m$m_25_30 <- as.numeric(0)
out_ympo_m$m_25_30 <- dat_ympo_m$grocery_25_30_mins
out_ympo_m$m_25_30_lts <- as.numeric(0)
out_ympo_m$m_25_30_lts <- dat_ympo_m$grocery_25_30_mins * (dat_ympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_ympo_m$m_30_35 <- as.numeric(0)
out_ympo_m$m_30_35 <- dat_ympo_m$grocery_30_35_mins
out_ympo_m$m_30_35_lts <- as.numeric(0)
out_ympo_m$m_30_35_lts <- dat_ympo_m$grocery_30_35_mins * (dat_ympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_ympo_m$m_35_40 <- as.numeric(0)
out_ympo_m$m_35_40 <- dat_ympo_m$grocery_35_40_mins
out_ympo_m$m_35_40_lts <- as.numeric(0)
out_ympo_m$m_35_40_lts <- dat_ympo_m$grocery_35_40_mins * (dat_ympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_ympo_m$m_40_45 <- as.numeric(0)
out_ympo_m$m_40_45 <- dat_ympo_m$grocery_40_45_mins
out_ympo_m$m_40_45_lts <- as.numeric(0)
out_ympo_m$m_40_45_lts <- dat_ympo_m$grocery_40_45_mins * (dat_ympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_ympo_m$m_45_50 <- as.numeric(0)
out_ympo_m$m_45_50 <- dat_ympo_m$grocery_45_50_mins
out_ympo_m$m_45_50_lts <- as.numeric(0)
out_ympo_m$m_45_50_lts <- dat_ympo_m$grocery_45_50_mins * (dat_ympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_ympo_m$m_50_55 <- as.numeric(0)
out_ympo_m$m_50_55 <- dat_ympo_m$grocery_50_55_mins
out_ympo_m$m_50_55_lts <- as.numeric(0)
out_ympo_m$m_50_55_lts <- dat_ympo_m$grocery_50_55_mins * (dat_ympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_ympo_m$m_55_60 <- as.numeric(0)
out_ympo_m$m_55_60 <- dat_ympo_m$grocery_55_60_mins
out_ympo_m$m_55_60_lts <- as.numeric(0)
out_ympo_m$m_55_60_lts <- dat_ympo_m$grocery_55_60_mins * (dat_ympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_ympo_m)

# Export accessibility output 
write.dbf(out_ympo_m, (file=paste(dat_dir, "access_eac_markets_ympo.dbf", sep="_data/_tabular/_outputs/")))

### STRONG AND FEARLESS

### Schools

# Import and clean cranc output data
dat_ympo_s <- read_csv(paste(dat_dir, "saf_ympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s <- dat_ympo_s %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s)
dat_ympo_s[is.na(dat_ympo_s)] <- 0; dat_ympo_s <- dat_ympo_s %>% arrange(From)
dat_ympo_s_lts <- read_csv(paste(dat_dir, "saf_ympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
dat_ympo_s_lts <- dat_ympo_s_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_s_lts)
dat_ympo_s_lts[is.na(dat_ympo_s_lts)] <- 0; dat_ympo_s_lts <- dat_ympo_s_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_s <- as.data.frame(dat_ympo_s_lts)[1]
out_ympo_s$s_00_05 <- as.numeric(0)
out_ympo_s$s_00_05 <- dat_ympo_s$school_00_05_mins
out_ympo_s$s_00_05_lts <- as.numeric(0)
out_ympo_s$s_00_05_lts <- dat_ympo_s$school_00_05_mins * (dat_ympo_s_lts$`school_00_05_mins_lts1%`/100)+(dat_ympo_s_lts$`school_00_05_mins_lts2%`/100)
out_ympo_s$s_05_10 <- as.numeric(0)
out_ympo_s$s_05_10 <- dat_ympo_s$school_05_10_mins
out_ympo_s$s_05_10_lts <- as.numeric(0)
out_ympo_s$s_05_10_lts <- dat_ympo_s$school_05_10_mins * (dat_ympo_s_lts$`school_05_10_mins_lts1%`/100)+(dat_ympo_s_lts$`school_05_10_mins_lts2%`/100)
out_ympo_s$s_10_15 <- as.numeric(0)
out_ympo_s$s_10_15 <- dat_ympo_s$school_10_15_mins
out_ympo_s$s_10_15_lts <- as.numeric(0)
out_ympo_s$s_10_15_lts <- dat_ympo_s$school_10_15_mins * (dat_ympo_s_lts$`school_10_15_mins_lts1%`/100)+(dat_ympo_s_lts$`school_10_15_mins_lts2%`/100)
out_ympo_s$s_15_20 <- as.numeric(0)
out_ympo_s$s_15_20 <- dat_ympo_s$school_15_20_mins
out_ympo_s$s_15_20_lts <- as.numeric(0)
out_ympo_s$s_15_20_lts <- dat_ympo_s$school_15_20_mins * (dat_ympo_s_lts$`school_15_20_mins_lts1%`/100)+(dat_ympo_s_lts$`school_15_20_mins_lts2%`/100)
out_ympo_s$s_20_25 <- as.numeric(0)
out_ympo_s$s_20_25 <- dat_ympo_s$school_20_25_mins
out_ympo_s$s_20_25_lts <- as.numeric(0)
out_ympo_s$s_20_25_lts <- dat_ympo_s$school_20_25_mins * (dat_ympo_s_lts$`school_20_25_mins_lts1%`/100)+(dat_ympo_s_lts$`school_20_25_mins_lts2%`/100)
out_ympo_s$s_25_30 <- as.numeric(0)
out_ympo_s$s_25_30 <- dat_ympo_s$school_25_30_mins
out_ympo_s$s_25_30_lts <- as.numeric(0)
out_ympo_s$s_25_30_lts <- dat_ympo_s$school_25_30_mins * (dat_ympo_s_lts$`school_25_30_mins_lts1%`/100)+(dat_ympo_s_lts$`school_25_30_mins_lts2%`/100)
out_ympo_s$s_30_35 <- as.numeric(0)
out_ympo_s$s_30_35 <- dat_ympo_s$school_30_35_mins
out_ympo_s$s_30_35_lts <- as.numeric(0)
out_ympo_s$s_30_35_lts <- dat_ympo_s$school_30_35_mins * (dat_ympo_s_lts$`school_30_35_mins_lts1%`/100)+(dat_ympo_s_lts$`school_30_35_mins_lts2%`/100)
out_ympo_s$s_35_40 <- as.numeric(0)
out_ympo_s$s_35_40 <- dat_ympo_s$school_35_40_mins
out_ympo_s$s_35_40_lts <- as.numeric(0)
out_ympo_s$s_35_40_lts <- dat_ympo_s$school_35_40_mins * (dat_ympo_s_lts$`school_35_40_mins_lts1%`/100)+(dat_ympo_s_lts$`school_35_40_mins_lts2%`/100)
out_ympo_s$s_40_45 <- as.numeric(0)
out_ympo_s$s_40_45 <- dat_ympo_s$school_40_45_mins
out_ympo_s$s_40_45_lts <- as.numeric(0)
out_ympo_s$s_40_45_lts <- dat_ympo_s$school_40_45_mins * (dat_ympo_s_lts$`school_40_45_mins_lts1%`/100)+(dat_ympo_s_lts$`school_40_45_mins_lts2%`/100)
out_ympo_s$s_45_50 <- as.numeric(0)
out_ympo_s$s_45_50 <- dat_ympo_s$school_45_50_mins
out_ympo_s$s_45_50_lts <- as.numeric(0)
out_ympo_s$s_45_50_lts <- dat_ympo_s$school_45_50_mins * (dat_ympo_s_lts$`school_45_50_mins_lts1%`/100)+(dat_ympo_s_lts$`school_45_50_mins_lts2%`/100)
out_ympo_s$s_50_55 <- as.numeric(0)
out_ympo_s$s_50_55 <- dat_ympo_s$school_50_55_mins
out_ympo_s$s_50_55_lts <- as.numeric(0)
out_ympo_s$s_50_55_lts <- dat_ympo_s$school_50_55_mins * (dat_ympo_s_lts$`school_50_55_mins_lts1%`/100)+(dat_ympo_s_lts$`school_50_55_mins_lts2%`/100)
out_ympo_s$s_55_60 <- as.numeric(0)
out_ympo_s$s_55_60 <- dat_ympo_s$school_55_60_mins
out_ympo_s$s_55_60_lts <- as.numeric(0)
out_ympo_s$s_55_60_lts <- dat_ympo_s$school_55_60_mins * (dat_ympo_s_lts$`school_55_60_mins_lts1%`/100)+(dat_ympo_s_lts$`school_55_60_mins_lts2%`/100)
str(out_ympo_s)

# Export accessibility output 
write.dbf(out_ympo_s, (file=paste(dat_dir, "access_saf_schools_ympo.dbf", sep="_data/_tabular/_outputs/")))

### Grocery Markets

# Import and clean cranc output data
dat_ympo_m <- read_csv(paste(dat_dir, "saf_ympo_grocery_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m <- dat_ympo_m %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m)
dat_ympo_m[is.na(dat_ympo_m)] <- 0; dat_ympo_m <- dat_ympo_m %>% arrange(From)
dat_ympo_m_lts <- read_csv(paste(dat_dir, "saf_ympo_grocery_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_grocery/"))
dat_ympo_m_lts <- dat_ympo_m_lts %>% mutate_if(is.numeric, as.numeric); str(dat_ympo_m_lts)
dat_ympo_m_lts[is.na(dat_ympo_m_lts)] <- 0; dat_ympo_m_lts <- dat_ympo_m_lts %>% arrange(From)

# Calculate cumulative opportunities metric w/ and w/o LTS impedance factor
out_ympo_m <- as.data.frame(dat_ympo_m_lts)[1]
out_ympo_m$m_00_05 <- as.numeric(0)
out_ympo_m$m_00_05 <- dat_ympo_m$grocery_00_05_mins
out_ympo_m$m_00_05_lts <- as.numeric(0)
out_ympo_m$m_00_05_lts <- dat_ympo_m$grocery_00_05_mins * (dat_ympo_m_lts$`grocery_00_05_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_00_05_mins_lts2%`/100)
out_ympo_m$m_05_10 <- as.numeric(0)
out_ympo_m$m_05_10 <- dat_ympo_m$grocery_05_10_mins
out_ympo_m$m_05_10_lts <- as.numeric(0)
out_ympo_m$m_05_10_lts <- dat_ympo_m$grocery_05_10_mins * (dat_ympo_m_lts$`grocery_05_10_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_05_10_mins_lts2%`/100)
out_ympo_m$m_10_15 <- as.numeric(0)
out_ympo_m$m_10_15 <- dat_ympo_m$grocery_10_15_mins
out_ympo_m$m_10_15_lts <- as.numeric(0)
out_ympo_m$m_10_15_lts <- dat_ympo_m$grocery_10_15_mins * (dat_ympo_m_lts$`grocery_10_15_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_10_15_mins_lts2%`/100)
out_ympo_m$m_15_20 <- as.numeric(0)
out_ympo_m$m_15_20 <- dat_ympo_m$grocery_15_20_mins
out_ympo_m$m_15_20_lts <- as.numeric(0)
out_ympo_m$m_15_20_lts <- dat_ympo_m$grocery_15_20_mins * (dat_ympo_m_lts$`grocery_15_20_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_15_20_mins_lts2%`/100)
out_ympo_m$m_20_25 <- as.numeric(0)
out_ympo_m$m_20_25 <- dat_ympo_m$grocery_20_25_mins
out_ympo_m$m_20_25_lts <- as.numeric(0)
out_ympo_m$m_20_25_lts <- dat_ympo_m$grocery_20_25_mins * (dat_ympo_m_lts$`grocery_20_25_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_20_25_mins_lts2%`/100)
out_ympo_m$m_25_30 <- as.numeric(0)
out_ympo_m$m_25_30 <- dat_ympo_m$grocery_25_30_mins
out_ympo_m$m_25_30_lts <- as.numeric(0)
out_ympo_m$m_25_30_lts <- dat_ympo_m$grocery_25_30_mins * (dat_ympo_m_lts$`grocery_25_30_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_25_30_mins_lts2%`/100)
out_ympo_m$m_30_35 <- as.numeric(0)
out_ympo_m$m_30_35 <- dat_ympo_m$grocery_30_35_mins
out_ympo_m$m_30_35_lts <- as.numeric(0)
out_ympo_m$m_30_35_lts <- dat_ympo_m$grocery_30_35_mins * (dat_ympo_m_lts$`grocery_30_35_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_30_35_mins_lts2%`/100)
out_ympo_m$m_35_40 <- as.numeric(0)
out_ympo_m$m_35_40 <- dat_ympo_m$grocery_35_40_mins
out_ympo_m$m_35_40_lts <- as.numeric(0)
out_ympo_m$m_35_40_lts <- dat_ympo_m$grocery_35_40_mins * (dat_ympo_m_lts$`grocery_35_40_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_35_40_mins_lts2%`/100)
out_ympo_m$m_40_45 <- as.numeric(0)
out_ympo_m$m_40_45 <- dat_ympo_m$grocery_40_45_mins
out_ympo_m$m_40_45_lts <- as.numeric(0)
out_ympo_m$m_40_45_lts <- dat_ympo_m$grocery_40_45_mins * (dat_ympo_m_lts$`grocery_40_45_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_40_45_mins_lts2%`/100)
out_ympo_m$m_45_50 <- as.numeric(0)
out_ympo_m$m_45_50 <- dat_ympo_m$grocery_45_50_mins
out_ympo_m$m_45_50_lts <- as.numeric(0)
out_ympo_m$m_45_50_lts <- dat_ympo_m$grocery_45_50_mins * (dat_ympo_m_lts$`grocery_45_50_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_45_50_mins_lts2%`/100)
out_ympo_m$m_50_55 <- as.numeric(0)
out_ympo_m$m_50_55 <- dat_ympo_m$grocery_50_55_mins
out_ympo_m$m_50_55_lts <- as.numeric(0)
out_ympo_m$m_50_55_lts <- dat_ympo_m$grocery_50_55_mins * (dat_ympo_m_lts$`grocery_50_55_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_50_55_mins_lts2%`/100)
out_ympo_m$m_55_60 <- as.numeric(0)
out_ympo_m$m_55_60 <- dat_ympo_m$grocery_55_60_mins
out_ympo_m$m_55_60_lts <- as.numeric(0)
out_ympo_m$m_55_60_lts <- dat_ympo_m$grocery_55_60_mins * (dat_ympo_m_lts$`grocery_55_60_mins_lts1%`/100)+(dat_ympo_m_lts$`grocery_55_60_mins_lts2%`/100)
str(out_ympo_m)

# Export accessibility output 
write.dbf(out_ympo_m, (file=paste(dat_dir, "access_saf_markets_ympo.dbf", sep="_data/_tabular/_outputs/")))

####################
### END OF SCRIPT
