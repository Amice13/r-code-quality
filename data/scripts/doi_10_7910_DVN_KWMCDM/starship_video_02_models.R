####################################################
# Steven R. Gehrke
# Northern Arizona University
# 21-0405: Starship Robots
# Task: PET Modeling
# Created: 05.17.2022
# Updated: 06.27.2022
####################################################

### NOTES
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/

####################
### SET-UP

# Load libraries
# install.packages(c("tidyverse","MASS","lmtest"))
packages <- c("tidyverse","MASS","lmtest")
lapply(packages, require, character.only=TRUE); rm(packages)

# Set directory
dat_dir <- "H:/_projects/21-0405_SidewalkDeliveryRobots/"

# Import cleaned data set
dat <- read_csv(paste(dat_dir, "dat_pet_clean.csv", sep="_dat/_video/_tabular/"))

####################
### VARIABLE REVIEW + CREATION

## Outcome: PET Categories
table(dat$pet_cat); round(prop.table(table(dat$pet_cat)),2)
summary(dat$pet_secs); sd(dat$pet_secs)

## Conflict Characteristics

# First pathway user to conflict: Robot
dat$robot_first <- ifelse(dat$user1_type==1, 1, 0)
table(dat$robot_first); round(prop.table(table(dat$robot_first)),2)
summary(dat$robot_first); sd(dat$robot_first)

# Conflict direction: Second pathway user
table(dat$conflict_dir); round(prop.table(table(dat$conflict_dir)),2)
dat$condir_same <- 0; dat$condir_same[dat$conflict_dir==1] <- 1
summary(dat$condir_same); sd(dat$condir_same)
dat$condir_opp <- 0; dat$condir_opp[dat$conflict_dir==2] <- 1
summary(dat$condir_opp); sd(dat$condir_opp)
dat$condir_cross <- 0; dat$condir_cross[dat$conflict_dir==3] <- 1
summary(dat$condir_cross); sd(dat$condir_cross)

# Time of day
table(dat$time_start)
dat$hour <- as.numeric(gsub("\\:.*$", "", dat$time_start))
table(dat$hour)
dat$tod <- NA
dat$tod <- with(dat, ifelse(hour>=9 & hour<=10, 1,
                            ifelse(hour>=11 & hour<=13, 2, 3)))
table(dat$tod); round(prop.table(table(dat$tod)),2)
dat$tod_1 <- 0; dat$tod_1[dat$tod==1] <- 1
dat$tod_2 <- 0; dat$tod_2[dat$tod==2] <- 1
dat$tod_3 <- 0; dat$tod_3[dat$tod==3] <- 1
summary(dat$tod_1); sd(dat$tod_1)
summary(dat$tod_2); sd(dat$tod_2)
summary(dat$tod_3); sd(dat$tod_3)

# Evasive maneuver: First pathway user
table(dat$evasive_m1); round(prop.table(table(dat$evasive_m1)),2)
dat$em1_0 <- 0; dat$em1_0[dat$evasive_m1==0] <- 1
dat$em1_1 <- 0; dat$em1_1[dat$evasive_m1==1] <- 1
dat$em1_2 <- 0; dat$em1_2[dat$evasive_m1==2] <- 1
dat$em1_3 <- 0; dat$em1_3[dat$evasive_m1==3] <- 1
dat$em1_4 <- 0; dat$em1_4[dat$evasive_m1==4] <- 1
summary(dat$em1_0); sd(dat$em1_0)
summary(dat$em1_1); sd(dat$em1_1)
summary(dat$em1_2); sd(dat$em1_2)
summary(dat$em1_3); sd(dat$em1_3)
summary(dat$em1_4); sd(dat$em1_4)

# Evasive maneuver: Second pathway user
table(dat$evasive_m2); round(prop.table(table(dat$evasive_m2)),2)
dat$em2_0 <- 0; dat$em2_0[dat$evasive_m2==0] <- 1
dat$em2_1 <- 0; dat$em2_1[dat$evasive_m2==1] <- 1
dat$em2_2 <- 0; dat$em2_2[dat$evasive_m2==2] <- 1
dat$em2_3 <- 0; dat$em2_3[dat$evasive_m2==3] <- 1
dat$em2_4 <- 0; dat$em2_4[dat$evasive_m2==4] <- 1
summary(dat$em2_0); sd(dat$em2_0)
summary(dat$em2_1); sd(dat$em2_1)
summary(dat$em2_2); sd(dat$em2_2)
summary(dat$em2_3); sd(dat$em2_3)
summary(dat$em2_4); sd(dat$em2_4)

## Site Characteristics

# Pathway user exposure: Count and share
summary(dat$user_robot); sd(dat$user_robot)
dat$exp_pct_robot <- 0; dat$exp_pct_robot <- round(dat$user_robot/(dat$user_robot+dat$user_walk+dat$user_bike+dat$user_other),5)
summary(dat$exp_pct_robot); sd(dat$exp_pct_robot)
summary(dat$user_walk); sd(dat$user_walk)
dat$exp_pct_walk <- 0; dat$exp_pct_walk <- round(dat$user_walk/(dat$user_robot+dat$user_walk+dat$user_bike+dat$user_other),5)
summary(dat$exp_pct_walk); sd(dat$exp_pct_walk)
summary(dat$user_bike); sd(dat$user_bike)
dat$exp_pct_bike <- 0; dat$exp_pct_bike <- round(dat$user_bike/(dat$user_robot+dat$user_walk+dat$user_bike+dat$user_other),5)
summary(dat$exp_pct_bike); sd(dat$exp_pct_bike)

# Presence of bike lane
table(dat$tot_blane)
dat$blane <- NA; dat$blane <- ifelse(dat$tot_blane>0,1,0)
table(dat$blane); round(prop.table(table(dat$blane)),2)
summary(dat$blane); sd(dat$blane)

# Sidewalk width categories
summary(dat$tot_swalk)
dat$tot_swalk_ft <- 0; dat$tot_swalk_ft <- (dat$tot_swalk * 3.28084)
summary(dat$tot_swalk_ft); sd(dat$tot_swalk_ft)
dat$sw_ft_u10 <- NA; dat$sw_ft_u10 <- ifelse(dat$tot_swalk_ft<10,1,0)
table(dat$sw_ft_u10); round(prop.table(table(dat$sw_ft_u10)),2)
dat$sw_ft_10_20 <- NA; dat$sw_ft_10_20 <- ifelse(dat$tot_swalk_ft>=10 & dat$tot_swalk_ft<20,1,0)
table(dat$sw_ft_10_20); round(prop.table(table(dat$sw_ft_10_20)),2)
dat$sw_ft_20p <- NA; dat$sw_ft_20p <- ifelse(dat$tot_swalk_ft>=20,1,0)
table(dat$sw_ft_20p); round(prop.table(table(dat$sw_ft_20p)),2)
summary(dat$sw_ft_u10); sd(dat$sw_ft_u10)
summary(dat$sw_ft_10_20); sd(dat$sw_ft_10_20)
summary(dat$sw_ft_20p); sd(dat$sw_ft_20p)

# Presence of path barrier
table(dat$lat_clear)
dat$path_bar <- NA; dat$path_bar <- ifelse(dat$lat_clear==0,1,0)
table(dat$path_bar); round(prop.table(table(dat$path_bar)),2)
summary(dat$path_bar); sd(dat$path_bar)

# Number of path legs
table(dat$intx_legs); round(prop.table(table(dat$intx_legs)),2)
dat$intx_0 <- 0; dat$intx_0[dat$intx_legs==0] <- 1
dat$intx_1 <- 0; dat$intx_1[dat$intx_legs==1] <- 1
dat$intx_2 <- 0; dat$intx_2[dat$intx_legs==2] <- 1
dat$intx_3 <- 0; dat$intx_3[dat$intx_legs==3] <- 1
summary(dat$intx_0); sd(dat$intx_0)
summary(dat$intx_1); sd(dat$intx_1)
summary(dat$intx_2); sd(dat$intx_2)
summary(dat$intx_3); sd(dat$intx_3)


####################
### ORDERED LOGIT MODEL

# Clean data set for modeling
colnames(dat)
dat2 <- dat[,c("conflict_id","exposure_id","user1_type","user2_type","pet","pet_secs","pet_cat",
               "robot_first","condir_same","condir_opp","condir_cross","tod_1","tod_2","tod_3", "em1_0","em1_1","em1_2","em1_3","em1_4","em2_0","em2_1","em2_2","em2_3","em2_4",
               "user_robot","user_walk","user_bike","exp_pct_robot","exp_pct_walk","exp_pct_bike","blane","sw_ft_u10","sw_ft_10_20","sw_ft_20p","path_bar","intx_0","intx_1","intx_2","intx_3")]
head(dat2)
write_csv(dat2, paste(dat_dir, "dat_pet_model.csv" , sep="_dat/_video/_tabular/"))

# Examine correlation of independent variables with PET ordinal measure
cor_out <- as.data.frame(round(cor(dat2[,7:39], use="complete.obs", method="spearman"), 2))
cor_out$var <- row.names(cor_out)
cor_out <- cor_out[,c(34,1)]
cor_out$abs_pet_cat <- NA; cor_out$abs_pet_cat <- abs(cor_out$pet_cat)
write_csv(cor_out, paste(dat_dir, "starship_video_vars-spearman.csv" , sep="_analysis/_outputs/"))

# Examine p-values of largest associations (p<0.10)
cor.test(x=dat2$robot_first, y=dat2$pet_cat, method="spearman")
cor.test(x=dat2$em2_1, y=dat2$pet_cat, method="spearman")
cor.test(x=dat2$condir_opp, y=dat2$pet_cat, method="spearman")
cor.test(x=dat2$em1_4, y=dat2$pet_cat, method="spearman")

# MODEL 0: Constants only
dat2$pet_cat <- as.factor(dat2$pet_cat)
m0 <- polr(pet_cat ~ 1, data=dat2, Hess=TRUE)
logLik(m0)

# MODEL 1: Base model with correlated independent variables
m1 <- polr(pet_cat ~ robot_first + em2_1 + condir_opp + em1_4, data=dat2, Hess=TRUE)
summary(m1); confint(m1, level=0.90)

# MODEL 2: Remove non-significant predictors
m2 <- polr(pet_cat ~ robot_first + condir_opp + em1_4, data=dat2, Hess=TRUE)
summary(m2); confint(m2, level=0.90)

# MODEL 3: Iterative addition of predictors with marginal significance (p<0.10)
m3 <- polr(pet_cat ~ robot_first + condir_opp + em1_4, data=dat2, Hess=TRUE)
summary(m3); confint(m3, level=0.90)
# Note: No signficant variables found

# FINAL MODEL
m <- polr(pet_cat ~ robot_first + condir_opp + em1_4, data=dat2, Hess=TRUE)
summary(m); confint(m, level=0.95)
logLik(m)
lrtest(m,m0)

# CONFUSION MATRIX
m_pred <- predict(m, dat2)
table(dat2$pet_cat, m_pred)
mean (as.character(dat2$pet_cat) !=as.character(m_pred))


# Example conflicts based on model results
# m_ex <- dat2[dat2$robot_first==1 & dat2$condir_opp==0 & dat2$em1_4==1,]

####################
### END OF SCRIPT
