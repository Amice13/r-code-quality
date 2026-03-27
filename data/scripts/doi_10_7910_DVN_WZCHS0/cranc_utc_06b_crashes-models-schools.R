####################################################
# Steven R. Gehrke
# Northern Arizona University
# 22-0338: Bicycling Accessibility (CRANC 2.0)
# Task: Models (Study 2: High-Stress Cycling Access)
# Created: 08.09.2023
# Updated: 08.09.2023
####################################################

####################
### SET-UP
####################

# Load libraries
# install.packages(c("tidyverse","foreign","MASS"))
packages <- c("tidyverse","foreign","MASS")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
# dat_dir <- "H:/_projects/22-0338_BicyclingAccessibility/"
dat_dir <- "F:/22-0338_BicyclingAccessibility/"

####################
### DATA: SCHOOLS
####################

# Import and combine CRANC data sets: IBC cyclist and schools
ibc_sch_cympo <- read_csv(paste(dat_dir, "ibc_cympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_fmpo <- read_csv(paste(dat_dir, "ibc_fmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_lhmpo <- read_csv(paste(dat_dir, "ibc_lhmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_mag <- read_csv(paste(dat_dir, "ibc_mag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_pag <- read_csv(paste(dat_dir, "ibc_pag_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_scmpo <- read_csv(paste(dat_dir, "ibc_scmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_svmpo <- read_csv(paste(dat_dir, "ibc_svmpo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_ympo <- read_csv(paste(dat_dir, "ibc_ympo_school_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch <- Reduce(rbind,list(ibc_sch_cympo,ibc_sch_fmpo,ibc_sch_lhmpo,ibc_sch_mag,ibc_sch_pag,ibc_sch_scmpo,ibc_sch_svmpo,ibc_sch_ympo))
rm(ibc_sch_cympo,ibc_sch_fmpo,ibc_sch_lhmpo,ibc_sch_mag,ibc_sch_pag,ibc_sch_scmpo,ibc_sch_svmpo,ibc_sch_ympo)
ibc_sch <- ibc_sch[,c("From","school_10_15_mins","school_25_30_mins")]

# Import and combine CRANC data sets: IBC cyclist and schools and LTS
ibc_sch2_cympo <- read_csv(paste(dat_dir, "ibc_cympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_fmpo <- read_csv(paste(dat_dir, "ibc_fmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_lhmpo <- read_csv(paste(dat_dir, "ibc_lhmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_mag <- read_csv(paste(dat_dir, "ibc_mag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_pag <- read_csv(paste(dat_dir, "ibc_pag_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_scmpo <- read_csv(paste(dat_dir, "ibc_scmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_svmpo <- read_csv(paste(dat_dir, "ibc_svmpo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch2_ympo <- read_csv(paste(dat_dir, "ibc_ympo_school_lts_accessibility_analysis.csv", sep="_data/_tabular/_cranc/_school/"))
ibc_sch_lts <- Reduce(rbind,list(ibc_sch2_cympo,ibc_sch2_fmpo,ibc_sch2_lhmpo,ibc_sch2_mag,ibc_sch2_pag,ibc_sch2_scmpo,ibc_sch2_svmpo,ibc_sch2_ympo))
rm(ibc_sch2_cympo,ibc_sch2_fmpo,ibc_sch2_lhmpo,ibc_sch2_mag,ibc_sch2_pag,ibc_sch2_scmpo,ibc_sch2_svmpo,ibc_sch2_ympo)
ibc_sch_lts <- ibc_sch_lts[,c(1,18:25,42:49)]

# Merge CRANC data sets
ibc_sch_lts2 <- ibc_sch_lts[!is.na(ibc_sch_lts$school_10_15_mins_lts1_dist),]
ibc_sch_lts2$dist_15 <- NA
ibc_sch_lts2$dist_15 <- ibc_sch_lts2$school_10_15_mins_lts1_dist + ibc_sch_lts2$school_10_15_mins_lts2_dist +
  ibc_sch_lts2$school_10_15_mins_lts3_dist + ibc_sch_lts2$school_10_15_mins_lts4_dist
ibc_sch_lts2$dist_30 <- NA
ibc_sch_lts2$dist_30 <- ibc_sch_lts2$school_25_30_mins_lts1_dist + ibc_sch_lts2$school_25_30_mins_lts2_dist +
  ibc_sch_lts2$school_25_30_mins_lts3_dist + ibc_sch_lts2$school_25_30_mins_lts4_dist

# Rename columns in merged data sets
colnames(ibc_sch) <- c("From","sch_15_mins","sch_30_mins")
colnames(ibc_sch_lts2) <- c("From",
                            "dist_15_lts1","pct_15_lts1","dist_15_lts2","pct_15_lts2","dist_15_lts3","pct_15_lts3","dist_15_lts4","pct_15_lts4",
                            "dist_30_lts1","pct_30_lts1","dist_30_lts2","pct_30_lts2","dist_30_lts3","pct_30_lts3","dist_30_lts4","pct_30_lts4",
                            "dist_15","dist_30")

# Remove additional records w/o distances
ibc_sch_lts2 <- ibc_sch_lts2[ibc_sch_lts2$dist_15>0,]

# Create study sample w/ study 1 attributes
ibc_dat <- merge(ibc_sch_lts2,ibc_sch, by=c("From"))
ind_vars <- read_csv(paste(dat_dir, "dat_crashes_ibc_15_e.csv", sep="_data/_tabular/_outputs/")) # Note: employment access
ind_vars <- ind_vars[,c(1:2,8:95)]
ibc_dat2 <- merge(ibc_dat,ind_vars, by=c("From"))
rm(ind_vars,ibc_sch,ibc_sch_lts,ibc_sch_lts2,ibc_dat)

# Remove additional records w/o population
ibc_dat2$tot_pop <- NA; ibc_dat2$tot_pop <- ibc_dat2$sex_m +ibc_dat2$sex_f
# ibc_dat2 <- ibc_dat2[ibc_dat2$tot_pop>0,]
ibc_dat2 <- ibc_dat2[ibc_dat2$tot_pop>10,]

# Remove additional records w/o distance
ibc_dat2 <- ibc_dat2[ibc_dat2$dist_15>0,]

# Remove columns missing study 1 attributes
ibc_dat2 <- ibc_dat2[complete.cases(ibc_dat2),]

####################
### HSCA: school
####################

### High-stress cycling accessibility

# Low-stress facilities
ibc_dat2$dlts_12 <- NA; ibc_dat2$dlts_12 <- (ibc_dat2$dist_15_lts1 + ibc_dat2$dist_15_lts2)
ibc_dat2$plts_12 <- NA; ibc_dat2$plts_12 <- (ibc_dat2$pct_15_lts1 + ibc_dat2$pct_15_lts2)

# High-stress facilities
ibc_dat2$dlts_34 <- NA; ibc_dat2$dlts_34 <- (ibc_dat2$dist_15_lts3 + ibc_dat2$dist_15_lts4)
ibc_dat2$plts_34 <- NA; ibc_dat2$plts_34 <- (ibc_dat2$pct_15_lts3 + ibc_dat2$pct_15_lts4)

# Create HSCA metric
ibc_dat2$hsca_15 <- NA
ibc_dat2$hsca_15 <- (ibc_dat2$dlts_34) / ibc_dat2$dist_15
summary(ibc_dat2$hsca_15)

### Additional predictors

# Household vehicles
ibc_dat2$pveh_2p <- NA
ibc_dat2$pveh_2p <- ibc_dat2$pveh_2 + ibc_dat2$pveh_3 + ibc_dat2$pveh_4
ibc_dat2$pveh_2p[is.na(ibc_dat2$pveh_2p)] <- 0
summary(ibc_dat2$pveh_2p)

# Household income
ibc_dat2$pinc_100_up <- NA
ibc_dat2$pinc_100_up <- ibc_dat2$pinc_100_149 + ibc_dat2$pinc_150_up
ibc_dat2$pinc_100_up[is.na(ibc_dat2$pinc_100_up)] <- 0
summary(ibc_dat2$pinc_100_up)

### Export model data set
colnames(ibc_dat2)
ibc_dat3 <- ibc_dat2[,c(1:8,18,112:116)]
write_csv(ibc_dat3, (file=paste(dat_dir, "dat_hsca_ibc_15_s.csv", sep="_data/_tabular/_outputs/")))

### HSCA Models

# Base model
m0_hsca_ibc <- lm(hsca_15 ~ sch_15_mins + dist_15, data=ibc_dat2)
summary(m0_hsca_ibc)

# Full model
colnames(ibc_dat2)
m1_hsca_ibc <- lm(hsca_15 ~ sch_15_mins + dist_15 +
                    psex_f + page_00_19 + page_65_up +
                    pedu_1 +
                    prac_baa + prac_asi + prac_aian + prac_hisp + prac_ocat +
                    inc_050_099 + pinc_100_up +
                    pveh_1 + pveh_2p, data=ibc_dat2)
summary(m1_hsca_ibc)

# # Full model: Boot-strapped (Model fit)
# fit_func = function(data,ind){
#   model = lm(hsca_15~sch_15_mins+dist_15+psex_f+page_00_19+page_65_up+pedu_1+prac_baa+prac_asi+prac_aian+prac_hisp+prac_ocat+inc_050_099+pinc_100_up+pveh_1+pveh_2p,
#              data=data[ind,])
#   c(coef(model),rsq = summary(model)$r.squared)
# }
# m2_hsca_ibc <- boot::boot(ibc_dat2,R=999,statistic=fit_func)
# head(m2_hsca_ibc$t)
# boot::boot.ci(m2_hsca_ibc,index=5,type="perc")

# Expected HSCA
ibc_dat2$pred_hsca_15 <- NA; ibc_dat2$pred_hsca_15 <- predict(m1_hsca_ibc)
summary(ibc_dat2$pred_hsca_15); summary(ibc_dat2$hsca_15)

### Crash outcomes

# Crash frequency
ibc_dat2$pre_crash <- NA; ibc_dat2$pre_crash <- ibc_dat2$s_pre_inj_1 + ibc_dat2$s_pre_inj_2 + ibc_dat2$s_pre_inj_3 + ibc_dat2$s_pre_inj_4 + ibc_dat2$s_pre_inj_5
summary(ibc_dat2$pre_crash)
ibc_dat2$post_crash <- NA; ibc_dat2$post_crash <- ibc_dat2$s_post_inj_1 + ibc_dat2$s_post_inj_2 + ibc_dat2$s_post_inj_3 + ibc_dat2$s_post_inj_4 + ibc_dat2$s_post_inj_5
summary(ibc_dat2$post_crash)

# Injury severity
ibc_dat2$pre_inj_ka <- NA; ibc_dat2$pre_inj_ka <- ibc_dat2$s_pre_inj_1 + ibc_dat2$s_pre_inj_2
summary(ibc_dat2$pre_inj_ka)
ibc_dat2$pre_inj_bco <- NA; ibc_dat2$pre_inj_bco <- ibc_dat2$s_pre_inj_3 + ibc_dat2$s_pre_inj_4 + ibc_dat2$s_pre_inj_5
summary(ibc_dat2$pre_inj_bco)

### Additional predictors

# Reported age
ibc_dat2$p_pre_age_1 <- NA
ibc_dat2$p_pre_age_1 <- ibc_dat2$s_pre_age_1 /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_age_1[is.na(ibc_dat2$p_pre_age_1)] <- 0
summary(ibc_dat2$p_pre_age_1)
ibc_dat2$p_pre_age_2 <- NA
ibc_dat2$p_pre_age_2 <- ibc_dat2$s_pre_age_2 /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_age_2[is.na(ibc_dat2$p_pre_age_2)] <- 0
summary(ibc_dat2$p_pre_age_2)
ibc_dat2$p_pre_age_3 <- NA
ibc_dat2$p_pre_age_3 <- ibc_dat2$s_pre_age_3 /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_age_3[is.na(ibc_dat2$p_pre_age_3)] <- 0
summary(ibc_dat2$p_pre_age_3)
ibc_dat2$p_pre_age_4 <- NA
ibc_dat2$p_pre_age_4 <- ibc_dat2$s_pre_age_4 /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_age_4[is.na(ibc_dat2$p_pre_age_4)] <- 0
summary(ibc_dat2$p_pre_age_4)
ibc_dat2$p_pre_age_5 <- NA
ibc_dat2$p_pre_age_5 <- ibc_dat2$s_pre_age_5 /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_age_5[is.na(ibc_dat2$p_pre_age_5)] <- 0
summary(ibc_dat2$p_pre_age_5)

# Reported sex
ibc_dat2$p_pre_sex_m <- NA
ibc_dat2$p_pre_sex_m <- ibc_dat2$s_pre_sex_m /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_sex_m[is.na(ibc_dat2$p_pre_sex_m)] <- 0
summary(ibc_dat2$p_pre_sex_m)
ibc_dat2$p_pre_sex_f <- NA
ibc_dat2$p_pre_sex_f <- ibc_dat2$s_pre_sex_f /(ibc_dat2$pre_crash) 
ibc_dat2$p_pre_sex_f[is.na(ibc_dat2$p_pre_sex_f)] <- 0
summary(ibc_dat2$p_pre_sex_f)

### Crash Frequency Models

# Base model
m0_freq_ibc <- glm.nb(pre_crash ~ pred_hsca_15, data=ibc_dat2)
summary(m0_freq_ibc)

# Full model
m1_freq_ibc <- glm.nb(pre_crash ~ pred_hsca_15 +
                        p_pre_age_1 + p_pre_age_5 + p_pre_sex_f, data=ibc_dat2)
summary(m1_freq_ibc)

### Injury Severity Models

# Base model
m0_inj_ibc <- glm.nb(pre_inj_ka ~ pred_hsca_15, data=ibc_dat2)
summary(m0_inj_ibc)

# Full model
m1_inj_ibc <- glm.nb(pre_inj_ka ~ pred_hsca_15 +
                       p_pre_age_1 + p_pre_age_5 + p_pre_sex_f, data=ibc_dat2)
summary(m1_inj_ibc)

### Descriptive Statistics
sapply(ibc_dat2[], function(x) round(mean(x, trim=0.05),3))
summary(ibc_dat2)

####################
### END OF SCRIPT
