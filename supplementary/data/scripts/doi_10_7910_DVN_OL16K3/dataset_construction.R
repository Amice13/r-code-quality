################################################################################ 
############################ Create Pulwama Dataset ############################ 
################################################################################ 

rm(list=ls(all = TRUE))

# Set working Directory

# Install and load packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <-
  c("maptools",
    "geosphere",
    "dplyr",
    "spdplyr",
    "stringr",
    "gridExtra",
    "sp")
ipak(pkg = packages)

#########################################################
########## Construct Village-Level Data #################
#########################################################

## Load in Village Census + Distance Data
## Distance Data constructed from ML InfoMap Shapefile, 
# 'Village Boundaries of Uttar Pradesh, India, 2011'

villages <- read.csv("Village_Data.csv")

## Merge SHRUG Village-Level Covariates

# Load datasets
shrug_pc <- read.csv("shrug_pc11.csv")
shrug_ancillary <- read.csv("shrug_ancillary.csv")
shrug_nl <- read.csv("shrug_nl_wide.csv")

# Subset columns
shrug_pc <- shrug_pc[,c("shrid","pc11_vd_power_all","pc11_vd_tar_road")]
shrug_anc <- shrug_ancillary[,c("shrid","tdist_10","tdist_50","tdist_100","tdist_500")]
shrug_nl <- shrug_nl[,c("shrid","num_cells","total_light2013")]

# Merge SHRUG datasets
shrug <- merge(shrug_pc,shrug_ancillary,by="shrid",all=T)
shrug <- merge(shrug,shrug_nl,by="shrid",all=T)

# Merge with master
shrug$village_code <- str_sub(shrug$shrid, start= -6)

villages <- merge(villages, shrug, by = "village_code", all.x=T)

############################################################
########## Combine with Booth Level Data ###################
############################################################

########## Load Booth-Booth and Booth-Village Crosswalks ########## 
# Load 2009 Booth - 2014 Booth Crosswalk
booth_matches09 <- read.csv("Booth_Matches_09.csv")

# Load 2014 Booth - 2019 Booth Crosswalk
booth_matches <- read.csv("Booth_Matches_14.csv")

# Load 2014 Booth - 2011 Village Crosswalk
village_matches <- read.csv("Village_Matches.csv")

########## Recode Village Matches For Villages That Gain A Booth, 2009-2014 ########## 

# Merge Crosswalks
crosswalk09 <- merge(booth_matches09, village_matches, by=c("No_14", "AC"), all.x=T)
crosswalk <- merge(booth_matches, village_matches, by=c("No_14", "AC"), all.x=T)
crosswalk <- crosswalk[crosswalk$village_code %in% villages$village_code,
                       c("No_14", "No_19", "AC", "village_code")]
crosswalk09 <- crosswalk09[crosswalk09$village_code %in% villages$village_code,
                           c("No_14", "No_09", "AC", "village_code")]

# Identify village pairs (or trio) that gained booths from 2009 to 2014
is_duplicated <- function (x) duplicated (x) | duplicated (x, fromLast = TRUE)
village_pair_gained <- crosswalk09[!is_duplicated(crosswalk09[,c("No_09", "AC", "village_code")]) &
                                     is_duplicated(crosswalk09[,c("No_09", "AC")]),]

# Replace 2014 Booth-Village Match For Smaller Village(s)
# With 2014 Booth-Village Match From Larger Village
pop <- villages[,c("village_code", "TOT_POP")]
village_pair_gained <- merge(village_pair_gained, pop, by="village_code", all.x=T)

village_pair_gained <- village_pair_gained %>% 
  group_by(No_09, AC) %>% 
  mutate(village_code = village_code[which.max(TOT_POP)]) %>%
  ungroup() %>%
  select(-c(TOT_POP, No_09))

crosswalk <- merge(crosswalk, village_pair_gained, by=c("No_14","AC"), all.x=T)
crosswalk09 <- merge(crosswalk09, village_pair_gained, by=c("No_14","AC"), all.x=T)

crosswalk$village_code <- ifelse(!is.na(crosswalk$village_code.y),
                                 crosswalk$village_code.y,
                                 crosswalk$village_code.x)
crosswalk09$village_code <- ifelse(!is.na(crosswalk09$village_code.y),
                                   crosswalk09$village_code.y,
                                   crosswalk09$village_code.x)
crosswalk <- crosswalk[,c("No_14", "No_19", "AC", "village_code")]
crosswalk09 <- crosswalk09[,c("No_14", "No_09", "AC", "village_code")]

########## Recode Village Matches For Villages That Lose A Booth, 2014-2019 ########## 

# Identify village pairs (or trio) that consolidated booths
is_duplicated <- function (x) duplicated (x) | duplicated (x, fromLast = TRUE)
village_pair_lost <- crosswalk[!is_duplicated(crosswalk[,c("No_19", "AC", "village_code")]) &
                                 is_duplicated(crosswalk[,c("No_19", "AC")]),]

# Replace 2014 Booth-Village Match For Smaller Village(s)
# With 2014 Booth-Village Match From Larger Village
pop <- villages[,c("village_code", "TOT_POP")]
village_pair_lost <- merge(village_pair_lost, pop, by="village_code", all.x=T)

village_pair_lost <- village_pair_lost %>% 
  group_by(No_19, AC) %>% 
  mutate(village_code = village_code[which.max(TOT_POP)]) %>%
  ungroup() %>%
  select(-c(TOT_POP, No_19))

crosswalk <- merge(crosswalk, village_pair_lost, by=c("No_14","AC"), all.x=T)
crosswalk09 <- merge(crosswalk09, village_pair_lost, by=c("No_14","AC"), all.x=T)

crosswalk$village_code <- ifelse(!is.na(crosswalk$village_code.y),
                                 crosswalk$village_code.y,
                                 crosswalk$village_code.x)
crosswalk09$village_code <- ifelse(!is.na(crosswalk09$village_code.y),
                                   crosswalk09$village_code.y,
                                   crosswalk09$village_code.x)
crosswalk <- crosswalk[,c("No_14", "No_19", "AC", "village_code")]
crosswalk09 <- crosswalk09[,c("No_14", "No_09", "AC", "village_code")]

########## Merge 2014 Booth Data ########## 
crosswalk14 <- crosswalk %>%
  group_by(No_14, AC) %>%
  summarise(village_code = mean(village_code))

# 2014 Booth-Level Covariates
booth_covariates <- read.csv("uprolls2014.csv")
booth_covariates <- booth_covariates[,c("ac_id_09","booth_id_14","electors_14",
                                        "missing_percent_14", "age_avg_14",
                                        "women_percent_14","muslim_percent_14",
                                        "hindu_percent_14")]
colnames(booth_covariates)[1:3] <- c("AC","No_14","Draft_Electors_14")

crosswalk14_covs <- merge(crosswalk14, booth_covariates, 
                          by=c("No_14", "AC"), all.x=T)

aggregated_covariates <- crosswalk14_covs %>%
  group_by(village_code) %>%
  summarise(missing_percent_14 = weighted.mean(missing_percent_14,
                                               Draft_Electors_14),
            age_avg_14 = weighted.mean(age_avg_14,
                                       Draft_Electors_14),
            women_percent_14 = weighted.mean(women_percent_14,
                                             Draft_Electors_14),
            hindu_percent_14 = weighted.mean(hindu_percent_14,
                                             Draft_Electors_14),
            muslim_percent_14 = weighted.mean(muslim_percent_14,
                                              Draft_Electors_14),
            Draft_Electors_14 = sum(Draft_Electors_14))

aggregated_covariates$muslim_percent_14_nonmissing <- aggregated_covariates$muslim_percent_14/
  (100 - aggregated_covariates$missing_percent_14)*100

villages <- merge(villages, aggregated_covariates,
                  by = "village_code", all.x=T)

# 2014 Election Results
form20_2014 <- read.csv("uploksabha2014.csv")
form20_2014 <- form20_2014[,c("ac_id_09","booth_id_14","electors_14",
                              "turnout_14","male_votes_14","female_votes_14",
                              "votes_bsp_14","votes_bjp_14","votes_inc_14",
                              "votes_sp_14","votes_rld_14","votes_ad_14")]
colnames(form20_2014)[1:2] <- c("AC", "No_14")

crosswalk14_results <- merge(crosswalk14,form20_2014,
                             by=c("AC","No_14"),all.x=T)

crosswalk14_results$one <- 1
aggregated_results14 <- crosswalk14_results %>%
  group_by(village_code) %>%
  summarise_all(sum) %>%
  mutate(AC = AC/one) %>%
  select(-c(No_14, one))

villages <- merge(villages, aggregated_results14,
                  by = "village_code", all.x=T)

# Remove AC for megacities that span mutiple constituencies
aggregated_results14$AC[aggregated_results14$AC %% 1 != 0] <- NA

########## Merge 2019 Booth Data ########## 
crosswalk19 <- crosswalk %>%
  group_by(No_19, AC) %>%
  summarise(village_code = mean(village_code))

# 2019 Election Results
form20_2019 <- read.csv("results2019.csv")

crosswalk19_results <- merge(crosswalk19,form20_2019,
                             by=c("AC","No_19"),all.x=T)

aggregated_results19 <- crosswalk19_results %>%
  group_by(village_code) %>%
  summarise_all(sum) %>%
  select(-c(AC, No_19))

villages <- merge(villages, aggregated_results19,
                  by = "village_code", all.x=T)

########## Merge 2009 Booth Data ########## 
form20_2009a <- read.csv("uploksabha2009-a.csv")
form20_2009b <- read.csv("uploksabha2009-b.csv")
form20_2009c <- read.csv("uploksabha2009-c.csv")
form20_2009 <- as.data.frame(rbind(form20_2009a,form20_2009b,form20_2009c))

form20_2009 <- form20_2009[,c("ac_id_09","booth_id_09","electors_09",
                              "turnout_09","male_votes_09","female_votes_09",
                              "votes_bsp_09","votes_bjp_09","votes_inc_09",
                              "votes_vjp_09","votes_rashtriyalokdal_09",
                              "votes_ad_09")]

colnames(form20_2009)[10:11] <- c("votes_sp_09", "votes_rld_09")
colnames(form20_2009)[1:2] <- c("AC", "No_09")

crosswalk09_results <- merge(crosswalk09,form20_2009,
                             by=c("AC","No_09"),all.x=T)

aggregated_results09 <- crosswalk09_results %>%
  group_by(village_code) %>%
  summarise_all(sum) %>%
  select(-c(AC, No_09))

villages <- merge(villages, aggregated_results09,
                  by = "village_code", all.x=T)

########## Create Final Covariates ########

# PC, Reservation, Phase, Incumbency, and Contestation data
pc <- read.csv("AC_PC_Codes.csv")
pc <- subset(pc, ST_ABBR=="UP")
pc <- pc[,c("PC_CODE", "PC_NAME", "PC_TYPE",
            "AC_CODE", "AC_NAME", "AC_TYPE")]
colnames(pc)[c(1,4)] <- c("PC", "AC")
villages <- merge(villages, pc, by="AC", all.x=T)

phases <- read.csv("Phases.csv")
villages <- merge(villages, phases, by="PC", all.x=T)

incumbency <- read.csv("Incumbents_14.csv")
villages <- merge(villages, incumbency, by="PC", all.x=T)

incumbency_09 <- read.csv("Incumbents_09.csv")
villages <- merge(villages, incumbency_09, by="PC", all.x=T)

# Treatment variable
villages <- transform(villages, mindist = pmin(distance1, distance2, distance3, distance4,
                                                         distance5, distance6, distance7, distance8,
                                                         distance9, distance10, distance11, distance12)/1000)
villages$lndist <- log(villages$mindist+1)

# Election Variables
villages$bjp_share_19 <- villages$BJP_Votes_19/villages$Votes_19
villages$bjp_share_14 <- villages$votes_bjp_14/villages$turnout_14
villages$bjp_diff <- villages$bjp_share_19 - villages$bjp_share_14 
villages$bjp_share_09 <- villages$votes_bjp_09/villages$turnout_09
villages$bjp_diff_09 <- villages$bjp_share_14 - villages$bjp_share_09


villages$bsp_share_19 <- villages$BSP_Votes_19/villages$Votes_19
villages$bsp_share_14 <- villages$votes_bsp_14/villages$turnout_14
villages$bsp_diff <- villages$bsp_share_19 - villages$bsp_share_14 
villages$bsp_share_09 <- villages$votes_bsp_09/villages$turnout_09
villages$bsp_diff_09 <- villages$bsp_share_14 - villages$bsp_share_09

villages$sp_share_19 <- villages$SP_Votes_19/villages$Votes_19
villages$sp_share_14 <- villages$votes_sp_14/villages$turnout_14
villages$sp_diff <- villages$sp_share_19 - villages$sp_share_14 
villages$sp_share_09 <- villages$votes_sp_09/villages$turnout_09
villages$sp_diff_09 <- villages$sp_share_14 - villages$sp_share_09

villages$rld_share_19 <- villages$RLD_Votes_19/villages$Votes_19
villages$rld_share_14 <- villages$votes_rld_14/villages$turnout_14
villages$rld_diff <- villages$rld_share_19 - villages$rld_share_14 
villages$rld_share_09 <- villages$votes_rld_09/villages$turnout_09
villages$rld_diff_09 <- villages$rld_share_14 - villages$rld_share_09

villages$inc_share_19 <- villages$INC_Votes_19/villages$Votes_19
villages$inc_share_14 <- villages$votes_inc_14/villages$turnout_14
villages$inc_diff <- villages$inc_share_19 - villages$inc_share_14 
villages$inc_share_09 <- villages$votes_inc_09/villages$turnout_09
villages$inc_diff_09 <- villages$inc_share_14 - villages$inc_share_09

villages$ad_share_19 <- villages$AD_Votes_19/villages$Votes_19
villages$ad_share_14 <- villages$votes_ad_14/villages$turnout_14
villages$ad_diff <- villages$ad_share_19 - villages$ad_share_14 
villages$ad_share_09 <- villages$votes_ad_09/villages$turnout_09
villages$ad_diff_09 <- villages$ad_share_14 - villages$ad_share_09

villages$other_share_19 <- 1 - villages$ad_share_19 - villages$inc_share_19 - 
  villages$rld_share_19 - villages$bsp_share_19 - 
  villages$sp_share_19 - villages$bjp_share_19
villages$reporting_error <- as.numeric(villages$other_share_19<0)

villages$other_share_14 <- 1-rowSums(villages[,c("ad_share_14", "inc_share_14",
                                                           "rld_share_14","bsp_share_14",
                                                           "sp_share_14","bjp_share_14")],
                                          na.rm=TRUE)
villages$other_share_14[is.na(villages$AC)] <- NA


villages$other_share_09 <- 1 - villages$ad_share_09 - villages$inc_share_09 - 
  villages$rld_share_09 - villages$bsp_share_09 - 
  villages$sp_share_09 - villages$bjp_share_09

villages$other_diff <- villages$other_share_19 - villages$other_share_14 
villages$other_diff_09 <- villages$other_share_14 - villages$other_share_09 

villages$incumbent_share_19 <- ifelse(villages$Incumbent_Party=="BJP",
                                      villages$bjp_share_19,
                                      ifelse(villages$Incumbent_Party=="SP",
                                             villages$sp_share_19,
                                             villages$ad_share_19))

villages$incumbent_share_14 <- ifelse(villages$Incumbent_Party=="BJP",
                                      villages$bjp_share_14,
                                      ifelse(villages$Incumbent_Party=="SP",
                                             villages$sp_share_14,
                                             villages$ad_share_14))

villages$old_incumbent_share_09 <- ifelse(villages$Incumbent_Party_09=="BJP",
                                          villages$bjp_share_09,
                                          ifelse(villages$Incumbent_Party_09=="SP",
                                                 villages$sp_share_09,
                                                 ifelse(villages$Incumbent_Party_09=="RLD",
                                                        villages$rld_share_09,
                                                        ifelse(villages$Incumbent_Party_09=="BSP",
                                                               villages$bsp_share_09,villages$inc_share_09))))

villages$old_incumbent_share_14 <- ifelse(villages$Incumbent_Party_09=="BJP",
                                          villages$bjp_share_14,
                                          ifelse(villages$Incumbent_Party_09=="SP",
                                                 villages$sp_share_14,
                                                 ifelse(villages$Incumbent_Party_09=="RLD",
                                                        villages$rld_share_14,
                                                        ifelse(villages$Incumbent_Party_09=="BSP",
                                                               villages$bsp_share_14,villages$inc_share_14))))

villages$incumbent_diff <- villages$incumbent_share_19 - villages$incumbent_share_14
villages$old_incumbent_diff <- villages$old_incumbent_share_14 - villages$old_incumbent_share_09

villages$turnout_19 <- villages$Votes_19/villages$Electors_19
villages$turnout_19[villages$Electors_19==0] <- NA
villages$tr_14 <- villages$turnout_14/villages$electors_14
villages$tr_09 <- villages$turnout_09/villages$electors_09

villages$turnout_diff <- villages$turnout_19 - villages$tr_14
villages$turnout_diff_09 <- villages$tr_14 - villages$tr_09

villages$opp_share_14 <- ifelse(villages$SP_Contests==1, villages$sp_share_14,
                                ifelse(villages$BSP_Contests==1,
                                       villages$bsp_share_14,villages$rld_share_14))

villages$mgb_share_14 <- villages$bsp_share_14 + villages$sp_share_14 + villages$rld_share_14
villages$mgb_share_14[is.na(villages$rld_share_14)] <- villages$bsp_share_14[is.na(villages$rld_share_14)] +
  villages$sp_share_14[is.na(villages$rld_share_14)] 

villages$nda_share_14 <- villages$bjp_share_14
villages$nda_share_14[is.na(villages$bjp_share_14)] <- villages$ad_share_14[is.na(villages$bjp_share_14)]

villages$mgb_share_19 <- villages$bsp_share_19 + villages$sp_share_19 + villages$rld_share_19
villages$mgb_share_19[is.na(villages$rld_share_19)] <- villages$bsp_share_19[is.na(villages$rld_share_19)] +
  villages$sp_share_19[is.na(villages$rld_share_19)] 
villages$mgb_share_19[is.na(villages$bsp_share_19)] <- villages$sp_share_19[is.na(villages$bsp_share_19)] +
  villages$rld_share_19[is.na(villages$bsp_share_19)] 

villages$nda_share_19 <- villages$bjp_share_19
villages$nda_share_19[is.na(villages$bjp_share_19)] <- villages$ad_share_19[is.na(villages$bjp_share_19)]

villages$incumbent_coalition_share_14 <- ifelse(villages$Incumbent_Party=="BJP" |
                                                  villages$Incumbent_Party=="AD", 
                                                villages$nda_share_14, villages$mgb_share_14)
villages$incumbent_coalition_share_19 <- ifelse(villages$Incumbent_Party=="BJP" |
                                                  villages$Incumbent_Party=="AD", 
                                                villages$nda_share_19, villages$mgb_share_19)

villages$nonincumbent_coalition_share_14 <- ifelse(villages$Incumbent_Party=="BJP" |
                                                     villages$Incumbent_Party=="AD", 
                                                   villages$mgb_share_14, villages$nda_share_14)
villages$nonincumbent_coalition_share_19 <- ifelse(villages$Incumbent_Party=="BJP" |
                                                     villages$Incumbent_Party=="AD", 
                                                   villages$mgb_share_19, villages$nda_share_19)

# Village Covariates
villages$proportion_working <- villages$TOT_W/villages$TOT_POP
villages$log_population <- log(villages$TOT_POP)
villages$proportion_male <- villages$M_POP/villages$TOT_POP
villages$proportion_sc <- villages$TOT_SC/villages$TOT_POP
villages$proportion_st <- villages$TOT_ST/villages$TOT_POP
villages$proportion_literate <- villages$TOT_LIT/villages$TOT_POP
villages$proportion_cultivators <- villages$TOT_CULT/villages$TOT_POP
villages$proportion_aglaborers <- villages$TOT_AGLB/villages$TOT_POP
villages$proportion_marginalworkers <- villages$TOT_MRW/villages$TOT_POP
villages$proportion_L6education <- villages$TOT_L6/villages$TOT_POP

villages$Town <- as.numeric(villages$LEVEL=="Town") 
villages$Big_Town <- as.numeric(villages$TOT_POP>50000)
villages$log_towndistance10k <- log(villages$tdist_10+1)
villages$log_towndistance50k <- log(villages$tdist_50+1)
villages$log_towndistance100k <- log(villages$tdist_100+1)
villages$log_towndistance500k <- log(villages$tdist_500+1)

villages$village_nonelec <- as.numeric(villages$pc11_vd_power_all==1)
villages$village_nonelec[villages$Town==1] <- 0
villages$village_nonpaved <- as.numeric(villages$pc11_vd_tar_road==1)
villages$village_nonpaved[villages$Town==1] <- 0

# Final variable additions
villages$nda_diff <- villages$nda_share_19 - villages$nda_share_14

villages$proportion_literate[villages$TOT_LIT==-9999] <- NA
villages$proximity <- - 1 * villages$lndist

villages$log_votes_14 <- log(villages$votes_bjp_14+1)
villages$log_votes_19 <- log(villages$BJP_Votes_19+1)
villages$log_votes_14_opp <- log(villages$opp_share_14*villages$turnout_14+1)
villages$log_turnout <- log(villages$turnout_14+1)

villages$log_nightlight_intensity <- log(villages$total_light2013/villages$num_cells + 1)

# Radius Data
radius <- paste("radius", 1:12, sep="")

distances <- paste("distance", 1:12, sep="")
villages[radius] <- as.numeric(villages[distances]<=20000)

## Keep only the variables used in analysis: comment out this code to utilize other variables in dataset
analysis_vars <- c("AC", "age_avg_14", "bjp_diff", "bjp_diff_09",
"bjp_share_09", "bjp_share_14", "BSP_Contests", "bsp_diff", "bsp_share_14",
"DISTRICT", "id", "INC_Contests", "inc_diff", "inc_share_14", "incumbent_diff",
"Incumbent_Party", "Incumbent_Reruns", "incumbent_share_14", "lat", "lndist",
"log_nightlight_intensity", "log_population", "log_towndistance100k", "log_towndistance10k",
"log_towndistance500k", "log_towndistance50k", "log_turnout", "log_votes_14",
"log_votes_14_opp", "log_votes_19", "long", "mindist", "MoV_Last", "muslim_percent_14",
"nda_diff", "nda_share_14", "opp_share_14", "PC", "Phase", "proportion_aglaborers",
"proportion_cultivators", "proportion_literate", "proportion_male",
"proportion_marginalworkers", "proportion_sc", "proportion_working", "proximity",
"radius1", "radius10", "radius11", "radius12", "radius2", "radius3",
"radius4", "radius5", "radius6", "radius7", "radius8", "radius9",
"distance1", "distance10", "distance11", "distance12", "distance2", "distance3",
"distance4", "distance5", "distance6", "distance7", "distance8", "distance9",
"reporting_error", "SP_Contests", "sp_diff", "sp_share_14", "SUB_DIST",
"Town", "tr_14", "turnout_19", "village_code", "village_nonelec",
"village_nonpaved", "Votes_19", "women_percent_14")

villages <- villages[,analysis_vars]


write.csv(villages, "analysis_dataset.csv", row.names=F)
